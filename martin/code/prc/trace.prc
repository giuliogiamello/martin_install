#procedure trace(LINE,SCHEME,DIMENSION)

*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************

*******************************************************************
*** Trace Gam`LINE' depending on your choice of "SCHEME"
*******************************************************************

*** Check for objects that should not be present at this stage
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

  exit "Only Gam`LINE' objects should be present here.";

endif;

b Gam`LINE',flagNumG5;
.sort

collect TEMP1,TEMP2;

*******************************************************************
*******************************************************************

#if  `SCHEME' == "NDR"
* In NDR we are strict and no G5 should ever appear in traces.
  if (match(flagNumG5(`LINE',aa?pos_))) exit "In NDR no g5s should ever appear in a trace.";
  if (match(Gam`LINE'(?x,G5,?y)))       exit "In NDR no g5s should ever appear in a trace.";
#endif

#if  `SCHEME' == "LARIN"
* In Larin G5 is substituted in favour of an epsilon-tensor 
* (convention of e_0123=1 as in Tracer.m)
    repeat;
      id once Gam`LINE'(?x,G5,?y) = 
                i_/24*e_(uu1,uu2,uu3,uu4)*
                Gam`LINE'(?x,[D-2ep],uu1,[D-2ep],uu2,[D-2ep],uu3,[D-2ep],uu4,?y);
      sum uu1,uu2,uu3,uu4;
    endrepeat;
#endif

* In NDR (no G5's are present PERIOD) and LARIN (G5 substituded to e_ tensors)
* we trace in d-dimension with FORM's internal routines. Same for an even number
* of G5's in JEMhv (though that scheme is flawed) or sNDR

#if `SCHEME'=="NDR"
  #call convertGamtogFORM(`LINE')
  tracen,`LINE';
  .sort

#elseif `SCHEME'=="LARIN" 
  #call convertGamtogFORM(`LINE')
  tracen,`LINE';
  .sort

#elseif `SCHEME'=="JEMhv" || `SCHEME'=="sNDR"
  if (match(flagNumG5(`LINE',aa?even_)));
    #call convertGamtogFORM(`LINE')
    tracen,`LINE';
  endif;
  .sort
#endif

*******************************************************************
*** the case of odd number of G5s
*******************************************************************

*** Cyclicity *****************************************************
* Traces in our schemes are cyclic even when g5's are present.
* We will now implement this cyclicity to get a convenient ordering
* of the G5's in the trace. Afterwards most of the G5's will be at the
* left of the cyclic FORM function

id Gam`LINE'(?x) = GamF`LINE'(?x);
id Gam`LINE'     = GamF`LINE';
repeat;
  id GamF`LINE'(?x,            G5) = GamF`LINE'(?x) * dummyf(      G5i);
  id GamF`LINE'(?x, [D-2ep], mu1?) = GamF`LINE'(?x) * dummyf(DDIMi,mu1);
  id GamF`LINE'(?x, [D],     mu1?) = GamF`LINE'(?x) * dummyf(TILi, mu1);
  id GamF`LINE'(?x, [-2ep],  mu1?) = GamF`LINE'(?x) * dummyf(HATi, mu1);
  id GamF`LINE'                    = dummyf;
endrepeat;

repeat id dummyf(?x)*dummyf(?y) = dummyf(?x,?y);
id dummyf(?x) = TR5`LINE'(?x);
id dummyf     = TR5`LINE';

*******************************************************************
* Remove all but one G5
*******************************************************************

* Use G5*G5=1 to trivially remove G5's that are next to each other
* That is actually nice because in some cases it means that no HV
* relations will have to be used.

repeat id TR5`LINE'(G5i,G5i,?y) = TR5`LINE'(?y);


*** Remove all but one G5 in the schemes for which we implement HV relations
#if ( (`SCHEME' == "HV") || (`SCHEME' == "JEMhv") )
* use HV relations to reduce the number of G5's
  repeat;
    id once TR5`LINE'(G5i, ?a, DDIMi, mu1?, G5i, ?b) = -TR5`LINE'(G5i, ?a, G5i, TILi, mu1, ?b)
                                                       +TR5`LINE'(G5i, ?a, G5i, HATi, mu1, ?b);

    id TR5`LINE'(G5i,G5i,?a) = TR5`LINE'(?a);

    id once TR5`LINE'(G5i, ?a, HATi,  mu1?, G5i, ?b) = +TR5`LINE'(G5i, ?a, G5i, HATi, mu1, ?b);

    id TR5`LINE'(G5i,G5i,?a) = TR5`LINE'(?a);

    id once TR5`LINE'(G5i, ?a, TILi,  mu1?, G5i, ?b) = -TR5`LINE'(G5i, ?a, G5i, TILi, mu1, ?b);

    id TR5`LINE'(G5i,G5i,?a) = TR5`LINE'(?a);
  endrepeat;
.sort
#endif


*** Remove all but one G5 in favour of evanescent anticommutators of NDRmisiak
#if ( (`SCHEME' == "NDRmisiak") )

  if ( (match(TR5`LINE'(TILi,?a))) || (match(TR5`LINE'(HATi,?a))) );
    exit "In NDRmisiak no D or -2ep gammas should be present.";
  endif;

* Notation for new evanescent structures: (..., AnC5i, mu, ...) == (..., {gamma_mu, gamma_5}, ...)
  repeat;
    id once TR5`LINE'(G5i, ?a, DDIMi, mu1?, G5i, ?b) = TR5`LINE'(G5i, ?a, AnC5i, mu1, ?b)
                                                      -TR5`LINE'(G5i, ?a, G5i, DDIMi, mu1, ?b);
    id TR5`LINE'(G5i,G5i,?a) = TR5`LINE'(?a);

    id once TR5`LINE'(G5i, ?a, AnC5i, mu1?, G5i, ?b) = TR5`LINE'(G5i, ?a, G5i, AnC5i, mu1, ?b);

    id TR5`LINE'(G5i,G5i,?a) = TR5`LINE'(?a);
  endrepeat;

.sort
#endif


*** Remove all but one G5 in the schemes for which we implement NDR relations
#if ( `SCHEME' == "sNDR" )
* use NDR relations to reduce the number of G5's
  repeat;
    id once TR5`LINE'(G5i, ?a, DDIMi, mu1?, G5i, ?b) = -TR5`LINE'(G5i, ?a, G5i, DDIMi, mu1, ?b);

    id TR5`LINE'(G5i,G5i,?a) = TR5`LINE'(?a);

    id once TR5`LINE'(G5i, ?a, HATi,  mu1?, G5i, ?b) = -TR5`LINE'(G5i, ?a, G5i, HATi, mu1, ?b);

    id TR5`LINE'(G5i,G5i,?a) = TR5`LINE'(?a);

    id once TR5`LINE'(G5i, ?a, TILi,  mu1?, G5i, ?b) = -TR5`LINE'(G5i, ?a, G5i, TILi, mu1, ?b);

    id TR5`LINE'(G5i,G5i,?a) = TR5`LINE'(?a);
  endrepeat;
.sort
#endif


*** Express all indices in TR5`LINE' in terms of D-2ep dimensional indices
*** and then drop the label DDIMi, this is the standard form of TR5LINE
repeat;
  id once TR5`LINE'(HATi,mu1?,?y) = TR5`LINE'(DDIMi,tempmu,?y)*ddhat(tempmu,mu1);
  sum tempmu;
endrepeat;

repeat;
  id once TR5`LINE'(TILi,mu1?,?y) = TR5`LINE'(DDIMi,tempmu,?y)*ddtilde(tempmu,mu1);
  sum tempmu;
endrepeat;

repeat id TR5`LINE'(DDIMi,mu1?,?y) = TR5`LINE'(mu1,?y);

id ddtilde(mu1?,mu2?) = d_(mu1,mu2) - ddhat(mu1,mu2);

#call contractMetric
.sort

*******************************************************************
* Tracing what is left to trace
*******************************************************************

*** Traces for the HV pieces via the epsilon tensor
#if ( (`SCHEME' == "HV") || (`SCHEME' == "JEMhv") )

#if `DIMENSION' == "4"
* Substitute the expression for the g5
*  exit "check first the sign of the relation G5 = i/4! e_  g g g g";
  repeat; 
* (convention of e_0123=1 as in Tracer.m)
    id once TR5`LINE'(G5i,?x) = i_/24*e_(tempmu1,tempmu2,tempmu3,tempmu4)*TR5`LINE'(tempmu1,tempmu2,tempmu3,tempmu4,?x);
    sum tempmu1,tempmu2,tempmu3,tempmu4;
  endrepeat;
#else
  exit "What shall we do with the g5s in d=`DIMENSION' dimensions";
#endif

* rewrite in FORM objects
#call convertTR5togFORM(`LINE')

* let FORM trace in d dimensions
tracen, `LINE';

* simplify
#call contractMetric
id e_(mu1?,mu2?,mu3?,mu4?)*ddhat(mu1?,mu5?) = 0;

.sort
#endif


#if ( `SCHEME' == "sNDR" )

#if `DIMENSION' == "4"
* Substitute the expression for the g5 using the 'naive' epsilon tensor
  repeat; 
* (convention of e_0123=1 as in Tracer.m)
    id once TR5`LINE'(G5i,?x) = i_/24*e_(tempmu1,tempmu2,tempmu3,tempmu4)*TR5`LINE'(tempmu1,tempmu2,tempmu3,tempmu4,?x)*flagsNDR;
    sum tempmu1,tempmu2,tempmu3,tempmu4;
  endrepeat;
#else
  exit "What shall we do with the g5s in d=`DIMENSION' dimensions";
#endif

* rewrite in FORM objects
#call convertTR5togFORM(`LINE')

* let FORM trace in d dimensions
tracen, `LINE';

* simplify
#call contractMetric
id ddtilde(mu1?,mu2?) = d_(mu1,mu2) - ddhat(mu1,mu2);
.sort
#endif


*******************************************************************
*** bring back original term **************************************
*******************************************************************

id TEMP1(aa?) = aa;
id TEMP2(aa?) = aa;


*** Test **********************************************************
if (match(Gam`LINE'(?x))) exit "This is trace.prc. Trace operation failed";
if (match(GamF`LINE'(?x))) exit "This is trace.prc. Trace operation failed";

#if (`SCHEME' != "NDRmisiak")
  if (match(TR5`LINE'(?x))) exit "This is trace.prc. Trace operation failed";
#endif

.sort
#endprocedure

******************
* vim: syntax=form
******************
