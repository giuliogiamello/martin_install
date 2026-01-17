#!/usr/bin/env python3

# Copyright (C) 2009-2020 Joachim Brod, Emmanuel Stamou
#
# This file is part of MaRTIn.
#
# MaRTIn is free software: you can redistribute it and/or modify
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
# along with MaRTIn.  If not, see <https://www.gnu.org/licenses/>.
#
# For further details see the AUTHORS file in the main MaRTIn directory.

import os
import sys
import shutil
import re
import argparse
import subprocess
import configparser

fmodel_dir = {}
qmodel_dir = {}
rdraw_dir  = {}
prc_dir    = {}
res_dir    = {}

config_file = '~/.martin.conf'


####################################################
# Write an example config file (internal)
####################################################

write_config = True
write_config = False
if write_config:
    config = configparser.ConfigParser(empty_lines_in_values=False)

    config['Default Paths'] = {
             'fmodel_dir'  : '~/dev/2loopForm_user_default/models/form',
             'qmodel_dir'  : '~/dev/2loopForm_user_default/models/qgraf',
             'rdraw_dir'   : '~/dev/2loopForm_user_default/models/rdraw',
             'prc_dir'     : '~/dev/2loopForm_user_default/prc',
             'res_dir'     : '~/dev/2loopForm_results'
             }

    config['FORM and QGRAF'] = {
            'qgraf_bin'    : '~/src/qgraf_3.4.2/qgraf',
            'form_bin'     : '~/src/form_install/bin/form',
            'rdraw_bin'    : '~/dev/richard_draw/richard_draw.py',
            'form_options' : '-M -Z'
            }

    config['Make Options'] = {
             'jobs'  : 3
             }

    with open(os.path.expanduser(config_file), 'w') as configfile:
        config.write(configfile)


####################################################
# Read default configuration from config file
####################################################

config = configparser.ConfigParser(empty_lines_in_values=False)
config.read(os.path.expanduser(config_file))

if not config.has_section('Default Paths'):
    config.add_section('Default Paths')

if not config.has_section('FORM and QGRAF'):
    config.add_section('FORM and QGRAF')

if not config.has_section('Make Options'):
    config.add_section('Make Options')

default_dirs = config['Default Paths']
default_bins = config['FORM and QGRAF']
default_opts = config['Make Options']

jobs_default       = default_opts.getint('jobs', 1)

fmodel_dir['config'] = default_dirs.get('fmodel_dir', None)
qmodel_dir['config'] = default_dirs.get('qmodel_dir', None)
rdraw_dir['config']  = default_dirs.get('rdraw_dir',  None)
prc_dir['config']    = default_dirs.get('prc_dir',    None)
res_dir['config']    = default_dirs.get('res_dir',    None)

qgraf_bin_config = default_bins.get('qgraf_bin',    'qgraf')
form_bin_config  = default_bins.get('form_bin',     'form')
form_opt_config  = default_bins.get('form_options', '-M -Z')
rdraw_bin_config = default_bins.get('rdraw_bin',   'you_must_specify_this/richard_draw.py')

####################################################
####################################################

parser = argparse.ArgumentParser(
        description='''
                    MaRTIn is the python wrapper that invokes "make" to
                    compute diagrams via the 2loopForm bundle of programs.
                    Apart from standard default values of arguments (see below)
                    the program also reads off default values from the config
                    file "''' + config_file + '" in case it exists.',
             epilog='''
                    ''',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('loop_dat',
                    type=str, metavar='loop.dat',
                    help='''The path to a loop.dat file with the FORM and QGRAF input
                            to be used for the computation.''')

parser.add_argument('make_targets', type=str, nargs='*',
                    help='''Targets and options to be passed to "make".
                            No argument computes all diagrams. "pdf" creates a pdf with
                            the diagrams, "sum" computes the sum of all diagrams.
                            "clean", "clean-sum", "clean-dia",  "clean-res", "clean-str",
                            and "clean-gra" all clean (part of) the result folder. In order
                            to pass optional keyword arguments to "make", i.e., arguments
                            with dashes, add them to the "make_target" list but include a
                            "--" before. For example: "martin.py loop.dat -- -dr --trace dia1 dia2".''')

parser.add_argument('-n', '--dry-run', action='store_true',
                    help='''Just print what will be executed without executing "make".''')

parser.add_argument('-j', '--jobs', metavar="N", type=int, default=jobs_default,
                    help='''Specifies the number of jobs (commands) that make is allowed to
                            run simultaneously.''')

parser.add_argument('-r', '--res-dir', metavar="DIR", type=str, default=False,
                    help='''The directory in which the results will be written.
                            If it is not explicity provided, but res_dir is set in
                            the config file, then the loop.dat path is used to
                            append the proper paths to that res_dir. If also res_dir
                            is not set in the config file then the result folder
                            is automatically inferred from the loop.dat path
                            loop_path(problems->results).''')

parser.add_argument('-p', '--prc-dir', metavar="DIR", type=str, default=False,
                    help='''The path to the user prc directory.
                            All the following paths will be passed on to "make" (and FORM)
                            as long as they exist:
                            prc_dir(explicitly set), prc_dir(from loop_path), prc_dir(set in config file).
                            If files with the same name exist in the different directories FORM
                            will use the one appearing first in the directory order above.''')

parser.add_argument('-f', '--fmodel-dir', metavar='DIR', type=str, default=False,
                    help='''The path to the directories with FORM models.
                            The names of the FORM models are extracted from the loop.dat.
                            The corresponding files are then searched for in the following
                            directories as long as those exist:
                            fmodel_dir(explicitly set), fmodel_dir(from loop_path), fmodel_dir(set in config file).
                            The directories are listed in order of importance.''')

parser.add_argument('-q', '--qmodel-dir', metavar='DIR', type=str, default=False,
                    help='''The path to the directories with QGRAF models.
                            The names of the QGRAF models are extracted from the loop.dat.
                            The corresponding files are then searched for in the following
                            directories as long as those exist:
                            qmodel_dir(explicitly set), qmodel_dir(from loop_path), qmodel_dir(set in config file).
                            The directories are listed in order of importance.''')

parser.add_argument('-d', '--rdraw-dir', metavar='DIR', type=str, default=False,
                    help='''The path to the directories with JSON files for richard_draw.py
                            The names of the JSON files models are extracted from the QGRAF models in the loop.dat.
                            The corresponding files are then searched for in the following
                            directories as long as those exist:
                            rdraw_dir(explicitly set), rdraw_dir(from loop_path), rdraw_dir(set in config file).
                            The directories are listed in order of importance.''')

parser.add_argument('--qgraf-bin', metavar='QGRAF', type=str, default=qgraf_bin_config,
                    help='''The path or name of the QGRAF binary to use.''')
parser.add_argument('--form-bin', metavar='FORM', type=str, default=form_bin_config,
                    help='''The path or name of the FORM binary to use.''')
parser.add_argument('--rdraw-bin', metavar='RDRAW', type=str, default=rdraw_bin_config,
                    help='''The path or name of the richard_draw.py binary to use.''')
parser.add_argument('--form-opt', metavar='FORMOPT', type=str, default=form_opt_config,
                    help='''Options passed to FORM.''')

args = parser.parse_args()


####################################################
####################################################

print('''
-----------------------------------------------------------------------
Massive Recursive Tensor Integration

            M a R T I n

Copyright (C) 2009-2024 J. Brod, L. Huedepohl, E. Stamou , T. Steudtner
MaRTIn is licensed under the GNU General Public License Version 3.
-----------------------------------------------------------------------
''')

####################################################
####################################################

dry_run = args.dry_run
if dry_run:
    print('DRY RUN: "make" will not actually be executed.\n')

jobs = args.jobs
qgraf_bin = os.path.expanduser(args.qgraf_bin)
form_bin = os.path.expanduser(args.form_bin)
form_opt = args.form_opt
rdraw_bin = os.path.expanduser(args.rdraw_bin)

res_dir['explicit']    = args.res_dir    if args.res_dir    else None
prc_dir['explicit']    = args.prc_dir    if args.prc_dir    else None
fmodel_dir['explicit'] = args.fmodel_dir if args.fmodel_dir else None
qmodel_dir['explicit'] = args.qmodel_dir if args.qmodel_dir else None
rdraw_dir['explicit']  = args.rdraw_dir  if args.rdraw_dir  else None


####################################################
# The config file
####################################################
if os.path.isfile(os.path.expanduser(config_file)):
    print('Extracted default configuration from config file: "' + config_file + '"\n')


####################################################
# The path to the makefile of 2loopForm
####################################################
martin_path = os.path.dirname(os.path.realpath(__file__))

make_path = os.path.join(martin_path, 'code', 'Makefile')

if not os.path.isfile(make_path):
    raise Exception('Could not find the Makefile.')


####################################################
# GIT commit
####################################################
if os.path.isdir(os.path.join(martin_path, ".git")) and shutil.which('git') is not None:
    proc = subprocess.Popen(
            ['git', 'rev-parse', '--short', 'HEAD'],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=martin_path)
    out, err = proc.communicate()
    git_commit = out.decode(encoding='utf8').replace('\n','')
else:
    git_commit = ''


####################################################
# The loop.dat file
####################################################

# The path to the loop.dat (we allow the loop.dat to be a symlink)
loop_path = os.path.abspath(args.loop_dat)

if not os.path.isfile(loop_path):
    raise Exception('The loop.dat file provided does not exist.')

PRO_dir = os.path.dirname(loop_path)

# The problem suffix of the loop.dat (can be empty)
m_suffix = re.match('loop(.*)\.dat', os.path.basename(loop_path))
if m_suffix:
    suffix, = m_suffix.groups()
else:
    raise Exception('The name of the loop.dat-file really needs to be "loop####.dat".')


####################################################
# Paths extracted from the loop.dat file.
####################################################
#
# In the standard usage of MaRTIn the loop.dat is
# nested somewhere within the "problems" directory that
# lives in the user directory with a spesific tree structure,
# namely:
#        user_dir/
#        user_dir/problems/      (the loop.dat is somewhere here)
#        user_dir/results/       (may not be present, generated by make)
#        user_dir/prc/
#        user_dir/models/
#        user_dir/models/form/
#        user_dir/models/qgraf/
#
# We extact the path to the user_dir by searching for "problems" in
# the absolute path of the loop.dat. We will assume that the "problems"
# closest to the loop.dat file may be the user_dir and will stop searching
# higher up on the tree. So if the user has the briliant idea of creating
# a "problems" inside a "problems" folder we will not find the user_dir.
# We can live with that, it's the user own fault.

#print('Inferring (some) paths from the loop.dat path.\n')

res_dir['loop']    = None
prc_dir['loop']    = None
fmodel_dir['loop'] = None
qmodel_dir['loop'] = None
rdraw_dir['loop']  = None

re_problem1 = re.match('.*\/problems\/', loop_path)
re_problem2 = re.finditer('(?=(\/problems\/.*))', loop_path)
if re_problem1 and re_problem2:
    pro_list = []
    for mpro in re_problem2:
        pro, = mpro.groups()
        pro_list.append(pro.strip('/'))

    pro = pro_list[-1]
    mstem = re.match(r'problems/(.*)/loop.*.dat', pro)
    pro_stem, = mstem.groups()
    user_dir = loop_path.replace('/'+pro, '')

    res_dir['loop']    = os.path.join(user_dir, 'results', pro_stem)
    prc_dir['loop']    = os.path.join(user_dir, 'prc')
    fmodel_dir['loop'] = os.path.join(user_dir, 'models/form')
    qmodel_dir['loop'] = os.path.join(user_dir, 'models/qgraf')
    rdraw_dir['loop']  = os.path.join(user_dir, 'models/rdraw')


####################################################
# Avoid unwanted "make" complication by providing absolute paths
####################################################

for dic in [prc_dir, fmodel_dir, qmodel_dir, rdraw_dir]:

    if dic['explicit'] is not None and not os.path.isdir(os.path.expanduser(dic['explicit'])):
        # explicity given directories must exist
        raise Exception('The explicitly given directory "' + dic['explicit'] + '" does not actually exist.')

    for case in ['config', 'explicit', 'loop']:
        if dic[case] is not None and os.path.isdir(os.path.expanduser(dic[case])):
            dic[case] = os.path.abspath(os.path.expanduser(dic[case]))
        else:
            dic[case] = None

if res_dir['config'] is not None:
    try:
        res_dir['config'] = os.path.join(res_dir['config'], pro_stem)
    except:
        res_dir['config'] = os.path.join(res_dir['config'], 'undefined')

for case in ['config', 'explicit', 'loop']:
    if res_dir[case] is not None:
        res_dir[case] = os.path.abspath(os.path.expanduser(res_dir[case]))
    else:
        res_dir[case] = None


####################################################
# Decide on the res_dir
####################################################

res_dir_list = list(filter(lambda t: t[0] is not None,
                           [[res_dir['explicit'], '[explicit argument]'],
                            [res_dir['config'],   '[from config file]'],
                            [res_dir['loop'],     '[inferred from loop.dat]']]))

try:
    RES_dir = res_dir_list[0][0]
    res_dir_origin = res_dir_list[0][1]
except:
    raise Exception('Failed to find res_dir')


####################################################
# Collect all prc_dir's
####################################################

prc_dir_choices = list(filter(lambda t: t[0] is not None,
                           [[prc_dir['explicit'], '[explicit argument]'],
                            [prc_dir['loop'],     '[inferred from loop.dat]'],
                            [prc_dir['config'],   '[from config file]']]))

prc_dirs        = [t[0] for t in prc_dir_choices]
prc_dirs_origin = [t[1] for t in prc_dir_choices]


####################################################
# Collect fmodel_dir's and find the models.
####################################################

fmodel_dir_choices = list(filter(lambda t: t[0] is not None,
                           [[fmodel_dir['explicit'], '[explicit argument]'],
                            [fmodel_dir['loop'],     '[inferred from loop.dat]'],
                            [fmodel_dir['config'],   '[from config file]']]))

if len(fmodel_dir_choices) == 0:
    raise Exception('Failed to find a directory with FORM models.')

fmodels = []
with open(loop_path, 'r') as fl:
    for line in fl:
        mmodel = re.match(r'^\s*#define\s*MODEL[0-9][0-9]*\s*[\',"](.*)[\',"]', line)
        if mmodel:
            model, = mmodel.groups()
            fmodels.append('model_'+model)

if len(fmodels) == 0:
    raise Exception('Failed to find a FORM model in the loop.dat.')

fmodel_paths  = []
fmodel_origin = []
for fmodel in fmodels:
    found_model = False
    for model_dir, case in fmodel_dir_choices:
        fmodel_path = os.path.join(model_dir, fmodel)
        if os.path.isfile(fmodel_path):
            fmodel_paths.append(fmodel_path)
            fmodel_origin.append(case)
            found_model = True
            break

    if not found_model:
        raise Exception('Could not find the FORM model: ' + fmodel)


####################################################
# Collect qmodel_dir's and find the models.
####################################################

qmodel_dir_choices = list(filter(lambda t: t[0] is not None,
                           [[qmodel_dir['explicit'], '[explicit argument]'],
                            [qmodel_dir['loop'],     '[inferred from loop.dat]'],
                            [qmodel_dir['config'],   '[from config file]']]))

if len(qmodel_dir_choices) == 0:
    raise Exception('Failed to find a directory with QGRAF models.')

qmodels = []
with open(loop_path, 'r') as fl:
    for line in fl:
        mmodel = re.match(r'^\s*model\s*=\s*[\',"](.*)[\',"]\s*;', line)
        if mmodel:
            model, = mmodel.groups()
            qmodels.append(model)

if len(qmodels) == 0:
    raise Exception('Failed to find a QGRAF model in the loop.dat')

qmodel_paths  = []
qmodel_origin = []
for qmodel in qmodels:
    found_model = False
    for model_dir, case in qmodel_dir_choices:
        qmodel_path = os.path.join(model_dir, qmodel)
        if os.path.isfile(qmodel_path):
            qmodel_paths.append(qmodel_path)
            qmodel_origin.append(case)
            found_model = True
            break

    if not found_model:
        raise Exception('Could not find the QGRAF model: ' + qmodel)


####################################################
# Collect rdraw_dir's and find the JSON files
####################################################

rdraw_dir_choices = list(filter(lambda t: t[0] is not None,
                           [[rdraw_dir['explicit'], '[explicit argument]'],
                            [rdraw_dir['loop'],     '[inferred from loop.dat]'],
                            [rdraw_dir['config'],   '[from config file]']]))

jsons = list(set([x.replace('.prop.lag', '.lag').replace('.vrtx.lag', '.lag').replace('.lag', '.json') for x in qmodels]))

json_paths  = []
json_origin = []
for json in jsons:
    found_model = False
    for model_dir, case in rdraw_dir_choices:
        json_path = os.path.join(model_dir, json)
        if os.path.isfile(json_path):
            json_paths.append(json_path)
            json_origin.append(case)
            found_model = True
            break


####################################################
# Summary
####################################################
print('Computing using the input of the loop.dat:')
print('   ' + loop_path + '\n')
print('The problem suffix of the loop.dat is:')
print('   suffix = "'+suffix+'"' + '\n')
print('The problem directory is:')
print('   ' + PRO_dir + '\n')
print('The results directory is:')
print('   {0: <24} : {1}\n'.format(res_dir_origin, RES_dir))
if len(prc_dirs) != 0:
    print('The user_prc directories are (in order of importance):')
    for path, case in zip(prc_dirs, prc_dirs_origin):
        print('   {0: <24} : {1}'.format(case, path))
    print('')
else:
    print('There are no user_prc directories.\n')
print('The FORM models are:')
for path, case in zip(fmodel_paths, fmodel_origin):
    print('   {0: <24} : {1}'.format(case, path))
print('')
print('The QGRAF models are:')
for path, case in zip(qmodel_paths, qmodel_origin):
    print('   {0: <24} : {1}'.format(case, path))
print('')
if len(json_paths) != 0:
    print('The JSON files for richard_draw.py are:')
    for path, case in zip(json_paths, json_origin):
        print('   {0: <24} : {1}'.format(case, path))
else:
    print('No JSON files for richard_draw.py found.')
print('')
if git_commit != '':
    print('Computing based on GIT commit: "' + git_commit +
          '" (Modulo local/uncommited modifications)\n')


####################################################
# Call make
####################################################

make_targets = args.make_targets

make_command_list =[
                 'make',
                 '-f', make_path,
                 '--jobs=' + str(jobs),
                 'QGRAF=' + qgraf_bin,
                 'FORM=' + form_bin,
                 'FORMOPT=' + form_opt,
                 'RDRAW=' + rdraw_bin,
                 'GIT=' + git_commit,
                 'PROBLEM_SUFFIX=' + suffix,
                 'PROBLEM_DIRECTORY=' + PRO_dir,
                 'RESULTS_DIRECTORY=' + RES_dir,
                 'USERPRC_DIRECTORIES=' + ' '.join(prc_dirs),
                 'FMODELS=' + ' '.join(fmodel_paths),
                 'QMODELS=' + ' '.join(qmodel_paths),
                 'RJSONS='  + ' '.join(json_paths),
                 *make_targets
                 ]

if dry_run:
    print('Running "make" (DRY RUN)\n')
    print_command_list =[]
    for com in make_command_list:
        wmatch = re.match(r'(.*=)(.*\s.*)', com)
        if wmatch:
            c1, c2 = wmatch.groups()
            print_command_list.append(c1 + '"' + c2 + '"')
        else:
            print_command_list.append(com)

    print_command_list = ['echo', *print_command_list]

    sys.stdout.flush()
    subprocess.call(print_command_list)

else:
    print('Running "make"...\n')
    sys.stdout.flush()
    subprocess.call(make_command_list)

print('\nMaRTIn finished.')
