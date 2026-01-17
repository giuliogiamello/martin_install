#-

*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou,
*                         Tom Steudtner
*
* This file is part of MaRTIn.
*
* MaRTIn is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* MaRTIn is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with MaRTIn.  If not, see <https://www.gnu.org/licenses/>.
*
* For further details see the AUTHORS file in the main MaRTIn directory.
*******************************************************************

*******************************************************************
* Define the problem, the diagram and the output                  *
*******************************************************************
#include `PROBLEM' # MAIN


*******************************************************************
* Declarations and routines                                       *
*******************************************************************
off statistics;

#include maindeclare
#include maindeclare_folds

#include `PROBLEM' # USERDEF


.global

#include dirac_algebra_routines.h

*******************************************************************
* Load diagram                                                    *
*******************************************************************
g `DIA' =
#include `FILENAME' # `DIA'
;


*******************************************************************
* Calculate diagram                                               *
*******************************************************************

#call printMessage(Calculating with input from the loop.dat file:)
#call printMessage(~   `PROBLEM')
#call printMessage(Using the models:)
#do i = 1, `NM'
  #call printMessage(~   model_`MODEL`i'')
#enddo



*******************************************************************
* Applying FOLD TEST **********************************************
*******************************************************************
#include `PROBLEM' # TEST


*******************************************************************
* In the following, the integrand is generated from the output of *
* qgraf. The relevant Feynman rules are defined in the            *
* model_MODEL files.                                              *
*******************************************************************
#include gengraph

#call printMessage(gengraph done)

*******************************************************************
* Applying FOLD FOLD1 *********************************************
*******************************************************************
#include `PROBLEM' # FOLD1

*******************************************************************
* Now the expression should be complete.                          *
*                                                                 *
* Here starts the evaluation of the diagram.                      *                                                *
* - IRA/ momentum expansion of propagators                        *
* - traces                                                        *
* - Dirac algebra                                                 *
* - Tensorreduction                                               *
* - Integrals                                                     *
*******************************************************************


*******************************************************************
* Treat masses and momenta                                        *
*******************************************************************


* We can only do vacuum integrals (might change in the future).
* If IRA is not defined, an expansion in external momenta has to be performed.
* A Taylor expansion in small (or large) masses is optional.
* If IRA is defined, all this will be taken care of by IRA.

#ifdef `EXPDENO'
  #ifdef `IRA'
    exit "ERROR: you defined both EXPDENO and IRA. Choose one of the two.";
  #endif
#endif


*******************************************************************
* Just expand
*******************************************************************
#ifdef `EXPDENO'
  #call expandDeno(`EXPDENO')
#endif


*******************************************************************
* The infrared rearrangement                                      *
*******************************************************************
#ifdef `IRA'
  #call performira(`IRA')
#endif


*******************************************************************
* The scheme for the Dirac algebra computation and
* our Gam`LINE' objects
*******************************************************************

* if the variable DSCHEME was not defined by the user then use the
* least harmful scheme, i.e, NDR. It is the least harmful scheme because
* it will automatically completely stop if it encounters a G5 in a
* closed fermions line (aka in traces).
#ifndef `DSCHEME'
  #define DSCHEME "NDR"
#endif

#if ( (`$nol' > 0) || (`$ncl' > 0) )
  #call printMessage(The scheme for the Dirac algebra computation is: `DSCHEME')
#endif

* Generate our commuting Gam`LINE' objects of the form:
* Gam1("D-2ep", mu1, G5, "D", mu2, ..., "-2ep", mu3, ....)

* No PL and PR are allowed in them. Be careful if you defined your
* our gamma matrices objects, e.g., sigmas etc. Make sure that everything
* is expressed in terms of the objects treated by generateGamLINE.prc

#do LINE = 1,`$ncl'
   #call generateGam(`LINE')
   #call contractMetric
   #call contractGamMetric(`LINE')
#enddo

* Flag the number of G5's appearing in each fermion line.
* to keep track of what cases had to be evaluated and to assist
* tracing and commuting G5 around
#do LINE = 1,`$ncl'

  id Gam`LINE'(?x) = flagNumG5(`LINE',?x) * Gam`LINE'(?x);

  repeat id flagNumG5(`LINE',?x,mu0?,     ?y) = flagNumG5(`LINE',?x,?y);
  repeat id flagNumG5(`LINE',?x,aa?!{,G5},?y) = flagNumG5(`LINE',?x,?y);

  id flagNumG5(`LINE',?x) = flagNumG5(`LINE',nargs_(?x));

#enddo
.sort

*************************************************************
* write important information related to G5 in the info file
*************************************************************

#write <`INFORESULTFILE'> "number of open lines   = %s", `$nol'
#write <`INFORESULTFILE'> "number of closed lines = %s", {`$ncl' - `$nol'}

#ifdef `WRITEG5INFO'
  #if `$nol' != `$ncl'

    l NumOfG5sInTraces = `DIA';
    b flagNumG5;
    .sort

    skip `DIA';
    collect TEMP1, TEMP2;
    id TEMP1(?x) = 1;
    id TEMP2(?x) = 1;
    #do LINE=1,`$nol'
      id flagNumG5(`LINE',aa?) = 1;
    #enddo
    b flagNumG5;
    .sort

    skip `DIA';
    collect TEMP1;
    id TEMP1(?x) = 1;
    .sort

    drop NumOfG5sInTraces;
    #write <`INFORESULTFILE'> "Num_of_G5s_in_traces = ["
    #do i = NumOfG5sInTraces;
      #write <`INFORESULTFILE'> "\t%s," , "`i'"
    #enddo
    #write <`INFORESULTFILE'> "\t]"

  #endif
  .sort
#endif

*******************************************************************
* Perform traces                                                  *
*******************************************************************

* By default FORM's tracen routine sets the trace of the unit matrix to 4
* We need to change this value based on the dimension we are in
unittrace `DIMENSION';


#do i = {`$nol'+1},`$ncl'
  #call printMessage(Tracing line `i' in `DIMENSION'-2ep dimensions in the `DSCHEME' scheme)
  #call trace(`i',`DSCHEME',`DIMENSION')
#enddo

#do i = 1,`$ncl';
  #call contractGamMetric(`i')
#enddo


*******************************************************************
* Applying FOLD FOLD2 *********************************************
*******************************************************************
#include `PROBLEM' # FOLD2


*******************************************************************
* Dirac algebra
*******************************************************************
*
* Step 1: treat the epsilon tensors according to the selected scheme when
* contracted with gamma matrices. This in principle can be done both in d and
* in 4 dimensions so it is a tricky thing. In the LARIN scheme epsilon tensors
* are left alone.
*
* Step 2: move G5s to the right, this depends on the scheme.
*
* Step 3: contract adjacent momenta and indices and order the stuff in the
* lines

*** test that we dont have any illegal objects at this stage
#do LINE = 1,`$ncl'
if (
     (match(g5_(`LINE')))          ||
     (match(g6_(`LINE')))          ||
     (match(g7_(`LINE')))          ||
     (match(G5`LINE'))             ||
     (match(PL`LINE'))             ||
     (match(PR`LINE'))             ||
     (match(g_(`LINE',mu1?)))      ||
     (match(gam`LINE'(mu1?)))      ||
     (match(gamhat`LINE'(mu1?)))   ||
     (match(gamtilde`LINE'(mu1?))) ||
     (match(gi_(`LINE')))          ||
     (match(gi`LINE'))             ||
     (match(GamF`LINE'(?x)))
   );

  exit "Only GamLINE objects should be present here.";

endif;
#enddo

*** Epsilon tensors
#if `DSCHEME' == "NDRmisiak"
  if (match(e_(mu1?,mu2?,mu3?,mu4?))) exit "What shall we do with an epsilon tensor in NDRmisiak?";
#endif


#if ( ( `DIMENSION' == 4 ) && ( `DSCHEME' != "LARIN" ) && ( `DSCHEME' != "sNDR" ) )
    #call treatEps4d(1,`$nol')
#endif

#if ( `DSCHEME' == "sNDR" )
    #call treatEpsDd(1,`$nol')
#endif

*** G5
#if ( ( `DIMENSION' == 4 ) && ( `DSCHEME' == "LARIN" ) )
  #do i=1,`$nol'
    repeat;
*     Conventions e_0123=+1 like in Tracer
      id once Gam`i'(?x,G5,?y) = i_/24*e_(uu1,uu2,uu3,uu4)
                                      *Gam`i'(?x,[D-2ep],uu1,[D-2ep],uu2,[D-2ep],uu3,[D-2ep],uu4,?y);
      sum uu1,uu2,uu3,uu4;
    endrepeat;
  #enddo
.sort
#endif

*** do the diracalgebra
#include diracalgebra`DIMENSION'd

*** do sth about Misiaks traces
#if `DSCHEME' == "NDRmisiak"
  #include treat_TR5
  #include diracalgebra`DIMENSION'd
#endif


*******************************************************************
* Tensor reduction -- vacuum integrals                            *
*******************************************************************

#ifndef `GENERICLOOPFUNCTIONS'
  #include tensorreduction_vac
#endif

*******************************************************************
* Dirac algebra                                                   *
*******************************************************************

*** do the diracalgebra
#include diracalgebra`DIMENSION'd

*** do sth about Misiaks traces
#if `DSCHEME' == "NDRmisiak"
  #include treat_TR5
  #include diracalgebra`DIMENSION'd
#endif

*** Set flagNumG5 to 1.

id flagNumG5(?a) = 1 ;

*******************************************************************
* Introduce DIRAC notation and external spinors *******************
*******************************************************************
#include diracoperators


*******************************************************************
* Applying FOLD FOLD3 *********************************************
*******************************************************************

#include `PROBLEM' # FOLD3


*******************************************************************
* Replace d -> nom                                                *
*******************************************************************

#call expandep({`FINALEPLIM' + `LOOP'})

#call printMessage(Replace d -> ep and expand in epsilon)



*******************************************************************
*** Dont integrate but attempt to write integrals in sth useful
*******************************************************************
#ifdef `GENERICLOOPFUNCTIONS'

  #if `LOOP' > 1;
    exit "Have not implemented a reasonable general integral for loop>1";
  #endif

  #if `LOOP' == 1;
    #if `DIMENSION' == 4
      #include generic_1loop_integrals
    #else
      exit "not implemented";
    #endif
  #endif

#endif

*******************************************************************
*** Do the integration for vacuum integrals ***********************
*******************************************************************

#ifndef `GENERICLOOPFUNCTIONS'
  #if `LOOP' != 0
    #call printMessage(Performing integration for vanishing external momenta)
    #include integrate
    .sort

    #if  ( `DIMENSION' == 4 )
      multiply (1/16/pi^2)^{`LOOP'};
    #endif

    #if ( `DIMENSION' == 3 )
      multiply (1/8/pi/sqrt_(pi))^{`LOOP'};
    #endif
  #endif
#endif

*******************************************************************
* Applying FOLD FOLD4 *********************************************
*******************************************************************
#include `PROBLEM' # FOLD4


*******************************************************************
* Expand Gamma functions                                          *
*******************************************************************

#include insert_gamma_function

#call expandep({`FINALEPLIM' + `LOOP'})
#call printMessage(Expanded Gamma functions)

*******************************************************************
* Test                                                            *
*******************************************************************
if ((count(p1,1) != 0) || (count(p2,1) != 0) || (count(p3,1)))
exit "This is main.frm: leftover loop momenta after integration";


*******************************************************************
* Introduce external spinors and polarization vectors *************
*******************************************************************

#do j = 1, `NM'
   #include model_`MODEL`j'' # POLARIZATION
#enddo


#call printMessage(Introduced spinors and polarization vectors)

*******************************************************************
* Insert coupling constants                                       *
*******************************************************************

#do m = 1,2
  #do i = 1, `NM'

    #include model_`MODEL`i'' # INSERTCOUPLINGS
    
    #call printMessage(Inserting coupling constants for model_`MODEL`i'')
  
  #enddo
#enddo


*******************************************************************
* Applying FOLD FOLD5 *********************************************
*******************************************************************
#include `PROBLEM' # FOLD5


*******************************************************************
* stupid form
*******************************************************************

id   sqrt_(aa?) = aa^(+1/2);
id 1/sqrt_(aa?) = aa^(-1/2);

id (aa?)^xx1?*(aa?)^xx2?  = (aa)^(xx1+xx2);
id (aa?)^(1/2) = aa^0*sqrt_(aa);
id (aa?)^(3/2) = aa^1*sqrt_(aa);
id (aa?)^(5/2) = aa^2*sqrt_(aa);
id (aa?)^(7/2) = aa^3*sqrt_(aa);
id (aa?)^(9/2) = aa^4*sqrt_(aa);


repeat;
id 1/Delta(?x)     *Delta(?x)      = 1;
endrepeat;

repeat;
id 1/Delta(?x)     /Delta(?x)      = 1/Delta(?x)^(2);
id 1/Delta(?x)^xx1?/Delta(?x)      = 1/Delta(?x)^(xx1+1);
id 1/Delta(?x)^xx1?/Delta(?x)^xx2? = 1/Delta(?x)^(xx1+xx2);
endrepeat;

*******************************************************************
* Final epsilon discarded                                         *
*******************************************************************

#call printMessage(Discarding terms with power of epsilon > {`FINALEPLIM'})
if (count(ep,1) > {`FINALEPLIM'}) discard;

*******************************************************************
* Print  expression                                               *
*******************************************************************


#call printMessage(Final result for diagram `DIA' of `PROBLEM')

#ifdef `FINALPRINT'
  bracket ep;
  print +s;
  .sort
#endif


*******************************************************************
* Save FORM expression                                            *
*******************************************************************

.store

#call printMessage(Writing Form file `FORMRESULTFILE')
save `FORMRESULTFILE' `DIA';


*******************************************************************
* Save MATHEMATICA expression                                     *
*******************************************************************

g `DIA'math = `DIA';

* rewrite things in a Mathematica friendly way
#include format_mathematica

.sort

#call printMessage(Writing Mathematica file `MATHRESULTFILE')
#write <`MATHRESULTFILE'> "%s`DIA'\t%s\t%s%E%s\n%" ,"(* ","*)","{",`DIA'math,"}"


*******************************************************************
* Write info file                                                 *
*******************************************************************

#call printMessage(Writing Info file `INFORESULTFILE')
#write <`INFORESULTFILE'> "TIME  = `TIME_'"
#write <`INFORESULTFILE'> "TIMER = `TIMER_'"

.end
