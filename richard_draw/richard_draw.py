#!/usr/bin/python3

# Copyright (C) 2021 Emmanuel Stamou
#
# This file is part of RichardDraw
#
# qdraw is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# MaRTIn is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RichardDraw.  If not, see <https://www.gnu.org/licenses/>.

class DiagramObject(object):
    def __init__(s, xml_dia, 
                    field_names={}, 
                    field_styles={}, 
                    decorated_vertex_names={}, 
                    decorated_vertex_styles={}, 
                    artificial_legs=[],
                    artificial_propagators=[],
                    vertex_styles={}
                    ):

        s.field_names             = field_names
        s.field_styles            = field_styles
        s.decorated_vertex_names  = decorated_vertex_names  
        s.decorated_vertex_styles = decorated_vertex_styles 
        s.artificial_legs         = artificial_legs         
        s.artificial_propagators  = artificial_propagators
        s.vertex_styles           = vertex_styles

        s.dianum,   = xml_dia.xpath('diadata/dianum/text()')
        s.sym,      = xml_dia.xpath('diadata/signsym/text()')
        s.num_props = int(xml_dia.xpath('diadata/numprops/text()')[0])
        s.num_verts = int(xml_dia.xpath('diadata/numverts/text()')[0])
        s.num_incom = int(xml_dia.xpath('diadata/numin/text()')[0])
        s.num_outgo = int(xml_dia.xpath('diadata/numout/text()')[0])

        s.incom = []
        s.incom_artificial = []
        for leg in xml_dia.find('incoming'):
            leg_obj = LegObject(leg, field_names=s.field_names, field_styles=s.field_styles, artificial_legs=s.artificial_legs)
            #print(leg_obj)
            if leg_obj.is_artificial:
                s.incom_artificial.append(leg_obj)
            else:
                s.incom.append(leg_obj)

        s.outgo = []
        s.outgo_artificial = []
        for leg in xml_dia.find('outgoing'):
            leg_obj = LegObject(leg, field_names=s.field_names, field_styles=s.field_styles, artificial_legs=s.artificial_legs)
            #print(leg_obj)
            if leg_obj.is_artificial:
                s.outgo_artificial.append(leg_obj)
            else:
                s.outgo.append(leg_obj)
        
        s.props_not_artificial = []
        s.props_artificial = []
        s.props_not_tadpole = []
        s.props_tadpole = []
        for xprop in xml_dia.find('propagators'):
            prop = PropagatorObject(xprop, field_names=s.field_names, field_styles=s.field_styles, artificial_propagators=s.artificial_propagators)
            #print(prop)
            if prop.is_artificial:
                s.props_artificial.append(prop)
            else:
                s.props_not_artificial.append(prop)
            if prop.is_tadpole:
                s.props_tadpole.append(prop)
            else:
                s.props_not_tadpole.append(prop)

        s.props = s.props_not_artificial + s.props_artificial

        s.bundles = {}
        for prop in s.props_not_tadpole:
            v1 = min([prop.from_vertex, prop.to_vertex])
            v2 = max([prop.from_vertex, prop.to_vertex])
            if (v1, v2) not in s.bundles:
                s.bundles[(v1, v2)] = [prop]
            else:
                s.bundles[(v1, v2)].append(prop)
        
        s.verts_not_decorated = []
        s.verts_decorated = []
        for vert in xml_dia.find('vertices'):
            vert_obj = VertexObject(vert, decorated_vertex_names=s.decorated_vertex_names, decorated_vertex_styles=s.decorated_vertex_styles)
            #print(vert_obj)
            if vert_obj.is_decorated:
                s.verts_decorated.append(vert_obj)
            else:
                s.verts_not_decorated.append(vert_obj)
        s.verts = s.verts_not_decorated + s.verts_decorated

    def __str__(s):
        txt_i = [str(i) for i in s.incom] + [str(i) for i in s.incom_artificial]
        txt_o = [str(o) for o in s.outgo] + [str(o) for o in s.outgo_artificial]
        txt_p = [str(p) for p in s.props]
        txt_v = [str(v) for v in s.verts]

        txt = 'Diagram(number:      ' + str(s.dianum)               + '\n\t' + \
                      'symmetry:    ' + str(s.sym)                  + '\n\t' + \
                      'incoming:    # = ' + str(s.num_incom)        + '\n\t' + \
                      '             ' + '\n\t\t     '.join(txt_i)   + '\n\t' + \
                      'outgoing:    # = ' + str(s.num_outgo)        + '\n\t' + \
                      '             ' + '\n\t\t     '.join(txt_o)   + '\n\t' + \
                      'Propagators: # = ' + str(s.num_props)        + '\n\t' + \
                      '             ' + '\n\t\t     '.join(txt_p)   + '\n\t' + \
                      'Vertices:    # = ' + str(s.num_verts)        + '\n\t' + \
                      '             ' + '\n\t\t     '.join(txt_v)   + '\n\t)'
        return txt
    
    def draw_tikz(s, size='small', 
                     draw_artificial_legs=False,
                     draw_artificial_propagators=False):
        if size == 'large':
            width = '0.45'
        else:
            width = '0.32'

        txt = r'''
\begin{minipage}[]{%s\textwidth}
\begin{center}
\begin{tikzpicture}
\tikzfeynmanset{}
\begin{feynman}
\diagram[%s]{
''' % (width, size)

        for leg in s.incom:
            txt = txt + '   ' + leg.draw_tikz_incoming() + '\n'
        for leg in s.outgo:
            txt = txt + '   ' + leg.draw_tikz_outgoing() + '\n'
        
        if draw_artificial_legs:
            for leg in s.incom_artificial:
                txt = txt + '   ' + leg.draw_tikz_incoming() + '\n'
            for leg in s.outgo_artificial:
                txt = txt + '   ' + leg.draw_tikz_outgoing() + '\n'

        for tad in s.props_tadpole:
            txt = txt + '   ' + tad.draw_tikz(artificial=tad.is_artificial) + '\n'

        for bun_props in s.bundles.values():
            num_bprop = len(bun_props)

            if num_bprop == 1:
                def_list = [[0, 0, 0]]
            elif num_bprop == 2:
                def_list = [[90, 90, 1], [-90, -90, 1]]
            elif num_bprop == 3:
                def_list = [[90, 90, 1.5], [0, 0, 0], [-90, -90, 1.5]]
            elif num_bprop == 4:
                def_list = [[90, 90, 1.5], [30, 150, 1], [-30, -150, 1], [-90, -90, 1.5]]
            elif num_bprop == 5:
                def_list = [[90, 90, 2], [30, 150, 1.5], [0, 0, 0], [-30, -150, 1.5], [-90, -90, 2]]
            elif num_bprop == 6:
                def_list = [[90, 90, 2], [60, 120, 1.5], [30, 150, 1], [-30, -150, 1], [-60, -120, 1.5], [-90, -90, 2]]
            else:
                raise Exception('Implement angle for more than 6 propagators.')

            i = 0
            for prop in bun_props:
                if prop.from_vertex < prop.to_vertex:
                    direction = +1
                elif prop.from_vertex > prop.to_vertex:
                    direction = -1
                else:
                    raise Exception('There should be no tadpole here.')

                a_out = direction * def_list[i][0]
                a_in  = direction * def_list[i][1]
                loose = def_list[i][2]
                i = i + 1

                txt = txt + '   ' + prop.draw_tikz(angle_out=a_out, angle_in=a_in, looseness=loose, artificial=prop.is_artificial) + '\n'

        txt = txt + '};\n'
        
        for vdec in s.verts_decorated:
                txt = txt + vdec.draw_tikz(vertex_styles=vertex_styles) + '\n'

        txt = txt + r'''\end{feynman}
\end{tikzpicture}\\
\# $%s$ \\
(symmetry: $%s$)
\end{center}
\end{minipage}''' % (s.dianum, s.sym)
        return txt



class LegObject(object):
    def __init__(s, xml_leg, field_names={}, field_styles={}, artificial_legs={}):
        import re
        s.index      = int(xml_leg.xpath('index/text()')[0])
        s.field,     = xml_leg.xpath('field/text()')
        s.mom,       = xml_leg.xpath('mom/text()')

        if s.field in field_names:
            s.name = field_names[s.field]
        else:
            s.name = s.field

        if s.field in field_styles:
            s.style = field_styles[s.field]
        else:
            s.style = 'plain'

        if xml_leg.find('to') is not None:
            s.status = 'incoming'
            s.to_vertex = int(xml_leg.xpath('to/text()')[0])

        if xml_leg.find('from') is not None:
            s.status = 'outgoing'
            s.from_vertex = int(xml_leg.xpath('from/text()')[0])

        if s.mom == '0':
            s.mom = 'p0'
        s.mom = '$' + s.mom + '$'
        for ff in re.findall(r'([a-z,A-Z])([0-9][0-9]*)', s.mom):
            mom_a = ff[0]
            mom_n = ff[1]
            s.mom = s.mom.replace(mom_a + mom_n, mom_a + '_{' + mom_n + '}')
        
        if s.field in artificial_legs:
            s.is_artificial = True
        else:
            s.is_artificial = False

    def draw_tikz_incoming(s):
        v_in  = 'i' + str(abs(s.index))
        v_out = 'v' + str(s.to_vertex)
        f_name  = s.name
        f_style = s.style
        txt = v_in + ' [particle=' + f_name + '] -- [' + f_style + '] ' + v_out + ','
        txt = v_in + ' [particle=' + f_name + '] -- [' + f_style + \
                     r", momentum'={[arrow style=gray, label distance=-1mm, arrow distance=1.5mm] {\tiny " + s.mom + '}}] '\
                   + v_out + ','
        return txt

    def draw_tikz_outgoing(s):
        v_in  = 'v' + str(s.from_vertex)
        v_out = 'o' + str(abs(s.index))
        f_name  = s.name
        f_style = s.style
        txt = v_in + ' -- [' + f_style + '] ' + v_out + ' [particle=' + f_name + '], '
        txt = v_in + ' -- [' + f_style +  \
                     r", momentum'={[arrow style=gray, label distance=-1mm, arrow distance=1.5mm] {\tiny " + s.mom + '}}] '\
                   + v_out + ' [particle=' + f_name + '], '
        return txt

    def __str__(s):
        if s.status == 'incoming':
            return 'Leg(' + s.field + ', [' + s.name + '], ' + s.style + ', ' + s.mom \
                          + ', vertex, -> ' + str(s.to_vertex) \
                          + ', index, -> ' + str(s.index) \
                          + ', artificial: ' + str(s.is_artificial) + ')'
        if s.status == 'outgoing':
            return 'Leg(' + s.field + ', [' + s.name + '], ' + s.style + ', ' + s.mom \
                          + ', vertex, ' + str(s.from_vertex) \
                          + ' ->, index, ' + str(s.index) + ' ->' \
                          + ', artificial: ' + str(s.is_artificial) + ')'


class PropagatorObject(object):
    def __init__(s, xml_prop, field_names={}, field_styles={}, artificial_propagators={}):
        import re
        s.index       = int(xml_prop.xpath('index/text()')[0])
        s.field,      = xml_prop.xpath('field/text()')
        s.antifield,  = xml_prop.xpath('antifield/text()')
        s.mom,        = xml_prop.xpath('mom/text()')
        s.from_index  = int(xml_prop.xpath('from_index/text()')[0])
        s.to_index    = int(xml_prop.xpath('to_index/text()')[0])
        s.from_vertex = int(xml_prop.xpath('from/text()')[0])
        s.to_vertex   = int(xml_prop.xpath('to/text()')[0])

        if s.field in field_names:
            s.name = field_names[s.field]
        else:
            s.name = s.field

        if s.antifield in field_names:
            s.antiname = field_names[s.antifield]
        else:
            s.antiname = s.antifield

        if s.field in field_styles:
            s.style = field_styles[s.field]
        elif s.antifield in field_styles:
            s.style = field_styles[s.antifield]
        else:
            s.style = 'plain'

        if s.mom == '0':
            s.mom = 'p0'

        s.mom = '$' + s.mom + '$'
        for ff in re.findall(r'([a-z,A-Z])([0-9][0-9]*)', s.mom):
            mom_a = ff[0]
            mom_n = ff[1]
            s.mom = s.mom.replace(mom_a + mom_n, mom_a + '_{' + mom_n + '}')

        if s.field in artificial_propagators:
            s.is_artificial = True
        else:
            s.is_artificial = False

        if s.from_vertex == s.to_vertex:
            s.is_tadpole = True
        else:
            s.is_tadpole = False

    def draw_tikz(s, angle_out=None, angle_in=None, looseness=None, artificial=False):
        v_in  = 'v' + str(s.from_vertex)
        v_out = 'v' + str(s.to_vertex)
        if looseness is None:
            looseness = 1
        if angle_out == 0:
            angle_out = None
        if angle_in == 0:
            angle_in = None

        if not artificial:
            txt_label = 'edge label=' + s.name + ', '
            txt_style = s.style
        else:
            txt_label = ''
            txt_style = 'double distance=1.5pt, gray'

        if v_in != v_out:
            if angle_out is None and angle_in is None:
                txt = v_in + ' -- [' + txt_label + txt_style + '] ' + v_out + ','
                #txt = v_in + ' -- [edge label=' + f_name + ', ' + f_style + \
                #             r", momentum'={[arrow style=gray, label distance=-1mm, arrow distance=1.5mm] {\tiny "+ s.mom + '}}]' + v_out + ','
            else:
                txt = v_in + ' -- [' + txt_label + txt_style \
                           + ', out=' + str(angle_out) \
                           + ', in='  + str(angle_in) \
                           + ', looseness = ' + str(looseness) \
                           + ', relative=true] ' + v_out + ','
        else:
            v_tad = 't' + str(s.from_vertex)
            txt =  v_in  + ' -- [half right, ' + txt_label + txt_style + '] ' + v_tad + ',\n   '\
                 + v_tad + ' -- [half right, '             + txt_style + '] ' + v_out + ','

        return txt

    def __str__(s):
        return 'Propagator(' + str(s.index) + ', ' + s.mom + ', '\
                             + s.field + ' -> ' + s.antifield + ', '\
                             + '[' + s.name + ' -> ' + s.antiname + '], '\
                             + s.style + ', ' \
                             + 'vertex: ' + str(s.from_vertex) + ' -> ' + str(s.to_vertex) + ', '\
                             + 'index: '  + str(s.from_index)  + ' -> ' + str(s.to_index) + ', '\
                             + 'artificial: ' + str(s.is_artificial) + ', '\
                             + 'tadpole: ' + str(s.is_tadpole) + ')'


class VertexObject(object):
    def __init__(s, xml_vertex, decorated_vertex_names={}, decorated_vertex_styles={}):

        s.vfunct,       = xml_vertex.xpath('vfunct/text()')
        s.index         = int(xml_vertex.xpath('index/text()')[0])
        s.field_momenta = xml_vertex.xpath('momenta/text()')[0].split(',')
        s.field_indices = xml_vertex.xpath('fields/text()')[0].split(',')
        s.field_types   = xml_vertex.xpath('type/text()')[0].split(',')
       
        if s.vfunct in decorated_vertex_names:
            s.name = decorated_vertex_names[s.vfunct]
        else:
            s.name = None
        
        if s.name is None:
            s.has_name = False
        else:
            s.has_name = True
        
        if s.vfunct in decorated_vertex_styles:
            s.style = decorated_vertex_styles[s.vfunct]
        else:
            s.style = None

        if s.style is None:
            s.has_style = False
        else:
            s.has_style = True

        if s.has_name or s.has_style:
            s.is_decorated = True
        else:
            s.is_decorated = False

    def __str__(s):
        if s.vfunct == s.name:
            txt_name = s.vfunct
        else:
            txt_name = s.vfunct + ' (' + str(s.name) +')'
        return 'Vertex(' + str(s.index) + ', ' + txt_name + ',' \
                + ' [' + ','.join(s.field_indices) + '],'\
                + ' [' + ','.join(s.field_types) + '],'\
                + ' [' + ','.join(s.field_momenta) + ', ' \
                + 'style: ' + str(s.style) + ')'
    
    def draw_tikz(s, vertex_styles={}):

        if s.has_name:
            txt_label = 'label={' + s.name + '}'
        else:
            txt_label = ''

        if s.has_style:
            if s.style in vertex_styles:
                txt_style = vertex_styles[s.style] + ', '
            else:
                print('Warning: could not find a style for the vertex', s.vfunct)
                txt_style = 'draw=black, shape=squared circle, inner sep=2pt' + ', '


        return r'\draw (v' + str(s.index) + r') node [' + txt_style + txt_label + '];'


def create_tikz_tex(xml_dias, out_file=None, qgraf_input=None, 
                    size='small',
                    total_number_of_diagrams=None,
                    draw_artificial_legs=False,
                    draw_artificial_propagators=False,
                    field_names={},
                    field_styles={},
                    artificial_legs=[],
                    artificial_propagators=[],
                    decorated_vertex_names={},
                    decorated_vertex_styles={},
                    vertex_styles={}):

    import sys
    txt_pro = r'''\documentclass[11pt,a4paper,DIV20]{scrartcl}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{graphicx}
\usepackage{multicol}
\usepackage[compat=1.1.0]{tikz-feynman}
\usetikzlibrary{shapes}

\title{Diagrams (total \# %s)\\[-0.5em]}
\author{\ttfamily{Richard\_Draw.py}}
\date{}

\begin{document}
\maketitle
\centering
''' % (total_number_of_diagrams)

    if qgraf_input is not None:
        txt_input = \
          r'\begin{verbatim}' + '\n' \
        + '----------------------------------------------------' \
        + qgraf_input \
        + '----------------------------------------------------' + '\n'\
        + r'\end{verbatim}'
    else:
        txt_input = ''

    txt_epi = r'''\end{document}''' + '\n'

    if out_file is None:
        fl = sys.stdout
    else:
        fl = open(out_file, 'w')

    fl.write(txt_pro)
    fl.write(txt_input)
    for xml_dia in xml_dias:
        dia = DiagramObject(xml_dia,
                            field_names=field_names,
                            field_styles=field_styles,
                            artificial_legs=artificial_legs,
                            artificial_propagators=artificial_propagators,
                            decorated_vertex_names=decorated_vertex_names,
                            decorated_vertex_styles=decorated_vertex_styles,
                            vertex_styles=vertex_styles)
        fl.write(dia.draw_tikz(size=size, 
                               draw_artificial_legs=draw_artificial_legs,
                               draw_artificial_propagators=draw_artificial_propagators
                               ) + '\n%')
    fl.write('\n' + txt_epi)

    if out_file is not None:
        fl.close()


#######################################################
#######################################################

if __name__ == '__main__':
    import lxml.etree as ET
    import argparse

    parser = argparse.ArgumentParser(
             description='''
                         richard_draw is a python3 program that takes the output of
                         QGRAF and generates tex code to be compiled with lualatex.
                         The diagrams must be generated by QGRAF with the richard_draw.sty
                         that accompanies richard_draw.py.''',
             epilog='''
                    ''',
             formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('diagrams_dat',
             type=str,
             metavar='diagram.dat',
             help='''The path to a diagram.dat file of QGRAF.''')

    parser.add_argument('-n', '--dia-num',
             metavar="DIANUM",
             type=str,
             default='*',
             help='''The number of a single diagram to draw. Default is to draw them all.''')

    parser.add_argument('-o', '--output-file',
             metavar="TEX-FILE",
             type=str,
             default=None,
             help='''The output file (tex file) to be compiled with lualatex.''')

    parser.add_argument('-s', '--style-file',
             metavar="JSON-FILE",
             type=str,
             action='append',
             default=None,
             help='''The optional style file (a json file) with latex names and styles for the fields and vertices 
                     of the model.''')

    parser.add_argument('-d', '--dia-size',
             metavar='SIZE',
             type=str,
             default='small',
             help='''The size of the feynman diagram (small/medium/large).''')

    args = parser.parse_args()

################################

    print('Processing file:', args.diagrams_dat)
    
    field_names             = {}
    field_styles            = {}
    artificial_legs         = []
    artificial_propagators  = []
    decorated_vertex_names  = {}
    decorated_vertex_styles = {}
    vertex_styles           = {
                               'default effective':              'draw=black, shape=crossed circle, inner sep=2pt, fill=white!70!red',
                               'default counterterm propagator': 'draw=red, shape=cross out, outer sep=2pt, inner sep=0pt, thick',
                               'default counterterm vertex':     'draw=red, shape=cross out, outer sep=2pt, inner sep=0pt, thick',
                              }


    if args.style_file is not None:
        import json
        for sfile in args.style_file:
            print('Load latex names and styles from: ' + sfile)
            with open(sfile, 'r') as fl:
                full_dict = json.load(fl)

            #print(full_dict.keys())

            if 'fields' in full_dict:
                for field in full_dict['fields']:
                    field_names[field]  = full_dict['fields'][field][0]
                    field_styles[field] = full_dict['fields'][field][1]
            
            if 'artificial legs' in full_dict:
                for leg in full_dict['artificial legs']:
                    artificial_legs.append(leg)
            
            if 'artificial propagators' in full_dict:
                for prop in full_dict['artificial propagators']:
                    artificial_propagators.append(prop)

            if 'decorated vertices' in full_dict:
                for vertex in full_dict['decorated vertices']:
                    if full_dict['decorated vertices'][vertex][0] == '':
                        decorated_vertex_names[vertex] = None
                    else:
                        decorated_vertex_names[vertex] = full_dict['decorated vertices'][vertex][0]
                    if full_dict['decorated vertices'][vertex][1] == "":
                        decorated_vertex_styles[vertex] = None
                    else:
                        decorated_vertex_styles[vertex] = full_dict['decorated vertices'][vertex][1]
            
            if 'vertex styles' in full_dict:
                for vstyle in full_dict['vertex styles']:
                    vertex_styles[vstyle] = None if "" else full_dict['vertex styles'][vstyle]

        print("  Field names")
        print(" ", field_names)
        print("  Field styles")
        print(" ", field_styles)
        print("  Artificial legs")
        print(" ", artificial_legs)
        print("  Artificial propagators")
        print(" ", artificial_propagators)
        print("  Decorated vertex names")
        print(" ", decorated_vertex_names)
        print("  Decorated vertex styles")
        print(" ", decorated_vertex_styles)
        print("  Self defined vertex styles")
        print(" ", vertex_styles)

    ########################

    qtree = ET.parse(args.diagrams_dat).getroot()

    total_number_of_diagrams, = qtree.xpath('totaldia/text()')
    qgraf_version,            = qtree.xpath('version/text()')
    qgraf_input,              = qtree.xpath('input/text()')
    tex_file = args.output_file
    selector = args.dia_num
    dia_size = args.dia_size

    input_list = list(filter(lambda x: x != '', qgraf_input.split('\n')))
    num_tot_lines = len(input_list)
    num_max_lines = 12
    if num_tot_lines > num_max_lines:
        qgraf_input = '\n'.join([input_list[i] for i in range(num_max_lines)])
        qgraf_input = '\n' + qgraf_input + '\n ...\n + ' + str(num_tot_lines - num_max_lines) + ' more lines.\n ...\n'

    print('The total number of diagrams is:', total_number_of_diagrams)
    print('QGRAF version:', qgraf_version)
    print('QGRAF input:\n',
          '---------------------------------------------',
          qgraf_input,
          '---------------------------------------------')
    print('Output file:', tex_file)
    if selector == '*':
        print('Drawing all diagrams.')
    else:
        print('Drawing diagram ' + selector)

    xml_dias = qtree.xpath(f'./diagrams/dia[{selector}]')

    #dia = DiagramObject(xml_dias[0],
    #                    field_names=field_names,
    #                    field_styles=field_styles,
    #                    artificial_legs=artificial_legs,
    #                    artificial_propagators=artificial_propagators,
    #                    decorated_vertex_names=decorated_vertex_names,
    #                    decorated_vertex_styles=decorated_vertex_styles,
    #                    vertex_styles=vertex_styles)
    #
    #print(dia.draw_tikz(draw_artificial_legs=True))
    #raise 1

    create_tikz_tex(xml_dias,
                    out_file=tex_file,
                    qgraf_input=qgraf_input,
                    size=dia_size,
                    total_number_of_diagrams=total_number_of_diagrams,
                    field_names=field_names,
                    field_styles=field_styles,
                    artificial_legs=artificial_legs,
                    artificial_propagators=artificial_propagators,
                    decorated_vertex_names=decorated_vertex_names,
                    decorated_vertex_styles=decorated_vertex_styles,
                    vertex_styles=vertex_styles,
                    draw_artificial_legs=False,
                    draw_artificial_propagators=True
                    )
