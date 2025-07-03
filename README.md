# martin_install

## Introduction

Installation guide for: 

- [MaRTIn - arxiv.org/2401.04033](https://arxiv.org/abs/2401.04033) 
- [Emmanuel Stamou / MaRTIn · GitLab](https://gitlab.com/manstam/martin)

Main dependencies: 

- [GitHub - form-dev/form](https://github.com/form-dev/form)
- [QGRAF's website](http://cefema-gt.tecnico.ulisboa.pt/~paulo/d.html)
- [Emmanuel Stamou / Richard Draw · GitLab](https://gitlab.com/manstam/richard_draw)

(Other needed packages: `gfortran`, python pkg `lxml`)

**NOTE:** I wrote this installation guide for myself, so if you will follow that pay attention to change the path's name with the correct one from your setup).

> Some sysinfo of my pc, just for info

- `neofetch --off`

```
jamal@giulio-pc 
--------------- 
OS: Ubuntu 24.04.2 LTS x86_64 
Host: VivoBook_ASUSLaptop X521IA_M533IA 1.0 
Kernel: 6.11.0-28-generic 
Uptime: 3 hours, 9 mins 
Packages: 2251 (dpkg), 7 (flatpak), 18 (snap) 
Shell: bash 5.2.21 
Resolution: 1920x1080 
DE: GNOME 46.0 
WM: Mutter 
WM Theme: Adwaita 
Theme: Yaru-blue-dark [GTK2/3] 
Icons: Yaru-blue [GTK2/3] 
Terminal: gnome-terminal 
CPU: AMD Ryzen 7 4700U with Radeon Graphics (8) @ 2.000GHz 
GPU: AMD ATI Radeon RX Vega 6 
Memory: 4622MiB / 15407MiB 
```

> Look in which folder I am (of course, your path is gonna be similar, but not exactly the same)

- `pwd`

```
/home/jamal
```

> This path is equivalent to `~/`

> Create `bsm` folder and go into it (not needed, I use it to have all the dependencies in the same folder)

- `mkdir bsm`
- `cd bsm`

## form

> Clone the repo, go into the folder and compile it.
> For more details about the compilation: [form/INSTALL at master · form-dev/form · GitHub](https://github.com/form-dev/form/blob/master/INSTALL)

- `git clone https://github.com/form-dev/form.git`
- `cd form`
- `mkdir form_build`
- `autoreconf -i`
- `./configure --prefix=~/bsm/form_build`
- `make`
- `make install`
- ~/bsm/form_build/bin/form`

> Create link to the `form` executable

- `sudo ln -s ~/bsm/form_build/bin/form /usr/bin/form`

> Test `form` works correctly, the output should be like:

- `form`

```
FORM 5.0.0-beta.1 (Jun 23 2025, v5.0.0-beta.1-195-g21f29a8)
No filename specified in call of FORM
  0.00 sec out of 0.00 sec
```

## qgraf

> You need `gfortran` in order to compile `qgraf`, so if you don't have it, install with:

- `sudo apt install gfortran` 

> The current ‘**stable**’ versions are 3.4 and 3.6, while 4.0.5 is a ‘development version’ (nearly ready for actual use) that includes an implementation of the previously announced interface.

- (http://cefema-gt.tecnico.ulisboa.pt/~paulo/d.html) and download `qgraf-3.6.10.tgz`
- `tar -xvzf qgraf-3.6.10.tgz -C qgraf-3.6.10`
- `mv qgraf-3.6.10/ ~/bsm/`
- `cd bsm/qgraf-3.6.10/`
- `gfortran qgraf-3.6.10.f08 -o qgraf`

> Create link to the `qgraf` executable

- `sudo ln -s ~/bsm/qgraf-3.6.10/qgraf /usr/bin/qgraf`

> Test `qgraf` works correctly

- `qgraf`

```
 --------------------------------------------------------------
                          qgraf-3.6.10
 --------------------------------------------------------------

 output = 'diagrams.dat' ;
 style = '../../richard_draw.sty' ;
 model = '../standardmodel.lag' ;
 in = fd[q1];
 out = fd[q2];
 loops = 1 ;
 loop_momentum = p ;
 options = onshell,tadpole;
 true = iprop[a,g,G0,0,0];
 true = iprop[h,1,1];
 true = iprop[ft,1,1];

 --------------------------------------------------------------

  ============================================================
   error: output-file already exists
  ============================================================

```

> Test: run the file `qgraf.dat` using your installed `qgraf` version (everything is good if you get 34098 connected diagrams), the output should be like:

- `qgraf qgraf.dat`

```
 --------------------------------------------------------------
                          qgraf-3.6.10
 --------------------------------------------------------------

  config= info ;
  output= '' ;
  style= 'form.sty' ;
  model= qed3 // 'qedx';
  in= e_minus ;
  out= e_minus, photon ;
  loops= 5 ;
  loop_momentum= ;
  options= onepi, floop ;

 --------------------------------------------------------------

   #loops    v-degrees        #diagrams

      5
               3^11     ....     34098


        total =  34098 connected diagrams
```

## richard_draw

> Go back into your `bsm` folder and clone `richard_draw`

- `cd ~/bsm/`
- `git clone https://gitlab.com/manstam/richard_draw.git`

> Choose one of the `richard_draw/example_suite/test_NAME` folders to test `richard_draw`. Here I choose `test_tadpoles`

- `cd richard_draw/example_suite/test_tadpoles`

> Check what is inside that folder (thre should be only a file named `qgraf.dat`)

- `ls`

```
qgraf.dat
```

---

> Maybe you don't need to install the python pkg `lxml`, so skip this step and install it only if required. I needed it, so:

> With the following command you are going to install the package system-wide and, doing this, you accept the risk of broken some other python package 

- `pip install --upgrade lxml --break-system-packages`

> So, if you prefer, you can create a virtual enviroment, activate it and install the package in the venv

- `python -m venv NAME_venv`
- `source NAME_venv/bin/activate`
- `pip install lxml`
- `pip install --upgrade lxml`

> In order to "turn off" and exit the virtual enviroment

- `deactivate`

---

> Compile the `qgraf.dat` file with `qgraf`

- `qgraf qgraf.dat`

```
--------------------------------------------------------------
                          qgraf-3.6.10
--------------------------------------------------------------

 output = 'diagrams.dat' ;
 style = '../../richard_draw.sty' ;
 model = '../standardmodel.lag' ;
 in = fd[q1];
 out = fd[q2];
 loops = 1 ;
 loop_momentum = p ;
 options = onshell,tadpole;
 true = iprop[a,g,G0,0,0];
 true = iprop[h,1,1];
 true = iprop[ft,1,1];

 --------------------------------------------------------------

   #loops    v-degrees          #diagrams

      1
              -   4^1     ....     0   **
             3^2   -      ....     1


        total =  1 connected diagram
```

> This will produce as output a file named `diagrams.dat`

- `ls`

```
diagrams.dat  qgraf.dat
```

> Create link to the `richard_draw` executable

- `sudo ln -s ~/bsm/richard_draw/richard_draw.py /usr/bin/richard_draw`

> Test `richard_draw` works correctly

- `richard_draw -s ../standardmodel.json  -s ../effective_counter.json diagrams.dat -o diagrams.tex`

```
Processing file: diagrams.dat
Load latex names and styles from: ../standardmodel.json
Load latex names and styles from: ../effective_counter.json
  Field names
  {'fU': '$\\bar u$', 'fu': '$u$', 'fC': '$\\bar c$', 'fc': '$c$', 'fT': '$\\bar t$', 'ft': '$t$', 'fD': '$\\bar d$', 'fd': '$d$', 'fS': '$\\bar s$', 'fs': '$s$', 'fB': '$\\bar b$', 'fb': '$b$', 'fNue': '$\\bar\\nu_e$', 'fnue': '$\\nu_e$', 'fNumu': '$\\bar\\nu_\\mu$', 'fnumu': '$\\nu_\\mu$', 'fNutau': '$\\bar\\nu_\\tau$', 'fnutau': '$\\nu_\\tau$', 'fE': '$\\bar e$', 'fe': '$e$', 'fMu': '$\\bar\\mu$', 'fmu': '$\\mu$', 'fTau': '$\\bar\\tau$', 'ftau': '$\\tau$', 'h': '$h$', 'G0': '$G_0$', 'Gm': '$G^-$', 'Gp': '$G^+$', 'wp': '$W^+$', 'wm': '$W^-$', 'z': '$Z$', 'a': '$A$', 'g': '$g$', 'ug': '$u_g$', 'Ug': '$\\bar u_g$', 'ua': '$u_A$', 'Ua': '$\\bar u_\\gamma$', 'uz': '$u_Z$', 'Uz': '$\\bar u_Z$', 'uwp': '$u_+$', 'Uwp': '$\\bar u_+$', 'uwm': '$u_-$', 'Uwm': '$\\bar u_-$'}
  Field styles
  {'fU': 'anti fermion', 'fu': 'fermion', 'fC': 'anti fermion', 'fc': 'fermion', 'fT': 'anti fermion', 'ft': 'fermion', 'fD': 'anti fermion', 'fd': 'fermion', 'fS': 'anti fermion', 'fs': 'fermion', 'fB': 'anti fermion', 'fb': 'fermion', 'fNue': 'anti fermion', 'fnue': 'fermion', 'fNumu': 'anti fermion', 'fnumu': 'fermion', 'fNutau': 'anti fermion', 'fnutau': 'fermion', 'fE': 'anti fermion', 'fe': 'fermion', 'fMu': 'anti fermion', 'fmu': 'fermion', 'fTau': 'anti fermion', 'ftau': 'fermion', 'h': 'scalar', 'G0': 'scalar', 'Gm': 'charged scalar', 'Gp': 'anti charged scalar', 'wp': 'charged boson', 'wm': 'anti charged boson', 'z': 'boson', 'a': 'boson', 'g': 'gluon', 'ug': 'ghost', 'Ug': 'ghost', 'ua': 'ghost', 'Ua': 'ghost', 'uz': 'ghost', 'Uz': 'ghost', 'uwp': 'ghost', 'Uwp': 'ghost', 'uwm': 'ghost', 'Uwm': 'ghost'}
  Artificial legs
  ['effextQ', 'countt']
  Artificial propagators
  ['effQ1', 'effQ2']
  Decorated vertex names
  {'VQ3': '[red]$Q_3$', 'VQeff': '[red]$Q_3$', 'VCountbb': None, 'VCountbba': None}
  Decorated vertex styles
  {'VQ3': 'default effective', 'VQeff': 'default effective', 'VCountbb': 'default counterterm propagator', 'VCountbba': 'default counterterm vertex'}
  Self defined vertex styles
  {'default effective': 'draw=black, shape=crossed circle, inner sep=2pt, fill=white!70!red', 'default counterterm propagator': 'draw=red, shape=cross out, outer sep=2pt, inner sep=0pt, thick', 'default counterterm vertex': 'draw=red, shape=cross out, outer sep=2pt, inner sep=0pt, thick', 'own vertex': 'draw=blue, shape=circle, inner sep=5pt'}
The total number of diagrams is: 1
QGRAF version: qgraf-3.6.10
QGRAF input:
 --------------------------------------------- 
 output = 'diagrams.dat' ;
 style = '../../richard_draw.sty' ;
 model = '../standardmodel.lag' ;
 in = fd[q1];
 out = fd[q2];
 loops = 1 ;
 loop_momentum = p ;
 options = onshell,tadpole;
 true = iprop[a,g,G0,0,0];
 true = iprop[h,1,1];
 true = iprop[ft,1,1];
 ---------------------------------------------
Output file: diagrams.tex
Drawing all diagrams.
```

> This will produce as output a file named `diagrams.tex`

- `ls`

```
diagrams.dat  diagrams.tex  qgraf.dat
```

> Compile the `diagrams.tex` with `lualatex` compiler (if you don't have latex on your system, see below)

```
lualatex diagrams.tex
```

> This will produce as output the following files (look for `diagrams.pdf`)

- `ls`

```
diagrams.aux  diagrams.dat  diagrams.log  diagrams.pdf  diagrams.tex  qgraf.dat
```

---

### Install (full) latex on Ubuntu

```
sudo apt install texlive-full texlive-xetex texlive-luatex biber texlive-base texlive-latex-recommended texlive texlive-latex-extra
```

## MaRTIn

> Go back into your `bsm` folder and clone `martin`

- `cd ~/bsm/`
- `git clone https://gitlab.com/manstam/martin.git`

> Create link to the `martin` executable

- `sudo ln -s ~/bsm/martin/martin.py /usr/bin/martin`

> Set config file in `~/` and rename it

- `cp bsm/martin/user_template/template_martin.conf ~/`
- `mv template_martin.conf .martin.conf`

> Change the paths in `.martin.conf` with the right one from your setup.
> (Here i use `vim` to edit the file)

- `vim .martin.conf`

```
# An example for a ~/.martin.conf file to change the default
# behaviour of martin.py. Users may adapt it and move their
# version to ~/.martin.conf. All supported options sofar
# are listed in this template, but any of them can be omitted.
# For further usage details do and read "martin.py -h".

[Default Paths]
fmodel_dir = ~/bsm/martin/user_template/models/form
qmodel_dir = ~/bsm/martin/user_template/models/qgraf
prc_dir    = ~/bsm/martin/user_template/prc
rdraw_dir = ~/bsm/martin/user_template/models/rdraw

# it is possible but discouraged to change the default
# result directory. If you really need to uncommment the
# line below.
#res_dir = ~/dev/2loopForm_results

[FORM and QGRAF]
qgraf_bin = /usr/bin/qgraf
form_bin = /usr/bin/form
rdraw_bin = /home/jamal/bsm/richard_draw/richard_draw.py
form_options = -M -Z

[Make Options]
jobs = 3
```

> Copy the `~/bsm/martin/user_template/` folder to `~/` and rename it as you want and remove the `template_martin.conf` from the new folder

- `cd`
- `cp ~/bsm/martin/user_template/ ~/`
- `mv user_template/ jamal_martin`
- `cd jamal_martin/`
- `rm template_martin.conf`

---

> You can find my martin user folder here: [martin_install/README.md at main · giuliogiamello/martin_install](https://github.com/giuliogiamello/martin_install/blob/main/README.md)

---

> Test `martin` works correctly

- `cd ~/jamal_martin/problems/SM`
- `martin loop.1_uu.dat clean` (not sure if you need to run this if it it's your first time running `martin`, try to skip it)
- `martin loop.1_uu.dat`

```
-----------------------------------------------------------------------
Massive Recursive Tensor Integration

            M a R T I n

Copyright (C) 2009-2024 J. Brod, L. Huedepohl, E. Stamou , T. Steudtner
MaRTIn is licensed under the GNU General Public License Version 3.
-----------------------------------------------------------------------

Extracted default configuration from config file: "~/.martin.conf"

Computing using the input of the loop.dat:
   /home/jamal/jamal_martin/problems/SM/loop.1_uu.dat

The problem suffix of the loop.dat is:
   suffix = ".1_uu"

The problem directory is:
   /home/jamal/jamal_martin/problems/SM

The results directory is:
   [inferred from loop.dat] : /home/jamal/jamal_martin/results/SM

The user_prc directories are (in order of importance):
   [inferred from loop.dat] : /home/jamal/jamal_martin/prc
   [from config file]       : /home/jamal/bsm/martin/user_template/prc

The FORM models are:
   [inferred from loop.dat] : /home/jamal/jamal_martin/models/form/model_SM

The QGRAF models are:
   [inferred from loop.dat] : /home/jamal/jamal_martin/models/qgraf/sm.prop.lag
   [inferred from loop.dat] : /home/jamal/jamal_martin/models/qgraf/sm.vrtx.lag

The JSON files for richard_draw.py are:
   [inferred from loop.dat] : /home/jamal/jamal_martin/models/rdraw/sm.json

Computing based on GIT commit: "21b1b15" (Modulo local/uncommited modifications)

Running "make"...

Generating /home/jamal/jamal_martin/results/SM/qgraf.1_uu.dat ...
Generating /home/jamal/jamal_martin/results/SM/qmodel.1_uu.lag ...
Generating /home/jamal/jamal_martin/results/SM/qlist.1_uu.dat ...
Running QGRAF ...

 --------------------------------------------------------------
                          qgraf-3.6.10
 --------------------------------------------------------------

 output = 'qlist.1_uu.dat';
 style  = 'main.sty';
 model  = 'model.lag';
 in =  fu[q1];
 out = fu[q1];
 loops = 1 ;
 loop_momentum = p;
 options =;
 true = bridge[a, g, z, G0, h, 0, 0] ;
 true = iprop[g, 1, 1] ;

 --------------------------------------------------------------

   #loops    v-degrees          #diagrams

      1
              -   4^1     ....     0
             3^2   -      ....     1


        total =  1 connected diagram

Generating /home/jamal/jamal_martin/results/SM/make.1_uu.info ...
rm /home/jamal/jamal_martin/results/SM/qlist.1_uu.dat
Generating /home/jamal/jamal_martin/results/SM/form.1_uu.dat ...
Computing /home/jamal/jamal_martin/results/SM/form.1_uu/dia1.sav ...
FORM 5.0.0-beta.1 (Jun 23 2025, v5.0.0-beta.1-195-g21f29a8)  Run: Tue Jul  1 15:24:45 2025
    #-

   dia1 =

       + ep^-1 * (
          - 3/4*UbarSp(fu,su3col,j1,mom,q1)*DIRAC(1,one)*USp(fu,su3col,j1,mom,
         q1)*i_*pi^-1*alphas*Mup*CF
          - 1/4*UbarSp(fu,su3col,j1,mom,q1)*DIRAC(1,one)*USp(fu,su3col,j1,mom,
         q1)*i_*pi^-1*xiqg*alphas*Mup*CF
          + 1/4*UbarSp(fu,su3col,j1,mom,q1)*DIRAC(1,q1)*USp(fu,su3col,j1,mom,
         q1)*i_*pi^-1*xiqg*alphas*CF
          );

  0.30 sec out of 0.31 sec
Done computing /home/jamal/jamal_martin/results/SM/form.1_uu/dia1.sav.

MaRTIn finished.

```

- `martin loop.1_uu.dat rpdf`

```
Really long output...
Generated /home/jamal/jamal_martin/results/SM/rgraphs.1_uu.pdf
```

> Finally, you can open the `rgraphs.1_uu.pdf` saved in `~/jamal_martin/results/SM/`, with your pdf viewer

- `open ~/jamal_martin/results/SM/rgraphs.1_uu.pdf`

---

Giulio Giamello

