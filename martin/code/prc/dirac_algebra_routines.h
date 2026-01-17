*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************

#procedure generateGamF(LINE)
****************************************************
* This procedure converts FORMs gamma matrices, G5's
* and PL/PRs into GamFs. There can be multiple
* GamFs because we do not join the GamFs that contain
* a G5 or Misiaks anticommutator AnC5
****************************************************

* remove PL and PR
id PL`LINE' = (gi_(`LINE') - G5`LINE')/2;
id PR`LINE' = (gi_(`LINE') + G5`LINE')/2;

* G5's
id g5_(`LINE') = GamF`LINE'(G5);
id G5`LINE'    = GamF`LINE'(G5);

* identities
id gi_(`LINE') = GamF`LINE';
id gi`LINE'    = GamF`LINE';

* Gamma matrices
* \gamma^mu
id g_(`LINE',mu0?)      = GamF`LINE'([D-2ep], mu0);
id gam`LINE'(mu0?)      = GamF`LINE'([D-2ep], mu0);
* \hat{\gamma}^mu
id gamhat`LINE'(mu0?)   = GamF`LINE'([-2ep],  mu0);
* \tilde{\gamma}^mu
id gamtilde`LINE'(mu0?) = GamF`LINE'([D],     mu0);
* {G5, \gamma^mu} this is for Misiak's scheme
id gamAnC5`LINE'(mu0?)  = GamF`LINE'(AnC5,    mu0);

* Remove redundant identity matrices
repeat;
id GamF`LINE'*    GamF`LINE'(?x) = GamF`LINE'(?x);
id GamF`LINE'(?x)*GamF`LINE'     = GamF`LINE'(?x);
id GamF`LINE'*    GamF`LINE'     = GamF`LINE';
endrepeat;

* Generate the GamFLINE
id GamF`LINE'(G5)        = dummyf(G5);
id GamF`LINE'(AnC5,mu1?) = dummyf(AnC5,mu1);

repeat id GamF`LINE'(?x) * GamF`LINE'(?y) = GamF`LINE'(?x,?y);

id dummyf(G5)        = GamF`LINE'(G5);
id dummyf(AnC5,mu1?) = GamF`LINE'(AnC5,mu1);

#endprocedure


#procedure generateGam(LINE)
****************************************************
* This procedure joins all GamF's from one dirac line to one Gam.
****************************************************
  #call generateGamF(`LINE')

  repeat id GamF`LINE'(?x) * GamF`LINE'(?y) = GamF`LINE'(?x,?y);

  id  GamF`LINE'(?x) = counter*GamF`LINE'(?x);
  if (count(counter,1) > 1) exit "This is procedure generateGam. Sth is splitting your GamFLINE.";
  id counter = 1;

  id GamF`LINE'(?x) = Gam`LINE'(?x);

#endprocedure


#procedure splitGam(LINE)
****************************************************
* This procedure split Gam's to GamF's
****************************************************

id Gam`LINE'(?x) = GamF`LINE'(?x);

repeat;
  id GamF`LINE'(?x, G5,        ?y) = GamF`LINE'(?x) * dummyf(G5)       * GamF`LINE'(?y);
  id GamF`LINE'(?x, AnC5, mu0? ?y) = GamF`LINE'(?x) * dummyf(AnC5,mu0) * GamF`LINE'(?y);
endrepeat;

id dummyf(G5)        = GamF`LINE'(G5);
id dummyf(AnC5,mu0?) = GamF`LINE'(AnC5,mu0);

repeat;
  id GamF`LINE'(?x) * GamF`LINE'     = GamF`LINE'(?x);
  id GamF`LINE'     * GamF`LINE'(?x) = GamF`LINE'(?x);
  id GamF`LINE'     * GamF`LINE'     = GamF`LINE';
endrepeat;

#endprocedure


#procedure splitfullGam(LINE)
****************************************************
* This procedure split Gam's to GamF's completely
****************************************************

#call splitGam(`LINE')

repeat;
  id GamF`LINE'(?x, [D-2ep], mu0?) = GamF`LINE'(?x) * dummyf([D-2ep], mu0);
  id GamF`LINE'(?x, [D],     mu0?) = GamF`LINE'(?x) * dummyf([D],     mu0);
  id GamF`LINE'(?x, [-2ep],  mu0?) = GamF`LINE'(?x) * dummyf([-2ep],  mu0);
endrepeat;

id dummyf([D-2ep], mu0?) = GamF`LINE'([D-2ep], mu0);
id dummyf([D],     mu0?) = GamF`LINE'([D],     mu0);
id dummyf([-2ep],  mu0?) = GamF`LINE'([-2ep],  mu0);

repeat;
  id GamF`LINE'(?x) * GamF`LINE'     = GamF`LINE'(?x);
  id GamF`LINE'     * GamF`LINE'(?x) = GamF`LINE'(?x);
  id GamF`LINE'     * GamF`LINE'     = GamF`LINE';
endrepeat;

#endprocedure


#procedure convertGamtogFORM(LINE)
****************************************************
* This procedure takes our Gam`LINE' or GamF`LINE'
* objects and converts them to FORM objects
****************************************************

if ( (match(Gam`LINE'(?x,[D],   ?y))) ||
     (match(Gam`LINE'(?x,[-2ep],?y))));
   exit "This is procedure convertGamtogFORM. Why do you have D or -2ep gammas here? FORM doesnt know about these objects";
endif;

id Gam`LINE'(?x) = GamF`LINE'(?x);

repeat;
  id GamF`LINE'(?x,G5)           = GamF`LINE'(?x) * g5_(`LINE');
  id GamF`LINE'(?x,[D-2ep],mu1?) = GamF`LINE'(?x) * g_(`LINE',mu1);
  id GamF`LINE'                  = gi_(`LINE');
endrepeat;

#endprocedure


#procedure convertTR5togFORM(LINE)
****************************************************
* here we convert the TR5LINES back to FORM objects*
****************************************************

if (
     (match(TR5`LINE'(?x,[D-2ep],?y))) ||
     (match(TR5`LINE'(?x,[D],    ?y))) ||
     (match(TR5`LINE'(?x,[-2ep], ?y))) ||
     (match(TR5`LINE'(?x,DDIMi,  ?y))) ||
     (match(TR5`LINE'(?x,TILi,   ?y))) ||
     (match(TR5`LINE'(?x,HATi,   ?y))) ||
     (match(TR5`LINE'(?x,AnC5i,  ?y))) ||
     (match(TR5`LINE'(?x,IDi,    ?y))) );
   exit "Bring TR5LINE to its standard form first.";
endif;

id TR5`LINE'(?x) = GamF`LINE'(?x);
id TR5`LINE'     = GamF`LINE';

repeat id GamF`LINE'(?x,G5i,?y) = GamF`LINE'(?x,G5,?y);

repeat;
  id GamF`LINE'(?x,G5)   = GamF`LINE'(?x) * g5_(`LINE');
  id GamF`LINE'(?x,mu1?) = GamF`LINE'(?x) * g_(`LINE',mu1);
  id GamF`LINE'          = gi_(`LINE');
endrepeat;

#endprocedure


#procedure convertTR5toGam(LINE)
****************************************************
* here we convert the TR5LINES back to our GamLINE *
****************************************************

id TR5`LINE'(?x) = Gam`LINE'(?x);
id TR5`LINE'     = Gam`LINE';

repeat id Gam`LINE'(?x, G5i,   ?y) = Gam`LINE'(?x, G5,   ?y);
repeat id Gam`LINE'(?x, AnC5i, ?y) = Gam`LINE'(?x, AnC5, ?y);

* cyclicity
id Gam`LINE'(mu1?, ?x, AnC5)    = Gam`LINE'(?x, AnC5, mu1);

repeat;
  id Gam`LINE'(mu1?, ?x)           = Gam`LINE'([D-2ep], mu1, ?x);
  id Gam`LINE'(?x, mu1?, mu2?, ?y) = Gam`LINE'(?x, mu1, [D-2ep], mu2, ?y);
  id Gam`LINE'(?x, G5,   mu2?, ?y) = Gam`LINE'(?x, G5,  [D-2ep], mu2, ?y);
endrepeat;

#endprocedure


#procedure convertGamtoTR5(LINE)
****************************************************
* here we convert the GamLINE to our TR5LINE
****************************************************

if (match(Gam`LINE'(?x,[D],   ?y))) exit "This is procedure convertGamtoTR5. TR5 object can only have D-2ep indices, G5s, and AnC5s";
if (match(Gam`LINE'(?x,[-2ep],?y))) exit "This is procedure convertGamtoTR5. TR5 object can only have D-2ep indices, G5s, and AnC5s";

repeat id Gam`LINE'(?x, G5,            ?y) = Gam`LINE'(?x, G5i,   ?y);
repeat id Gam`LINE'(?x, AnC5,          ?y) = Gam`LINE'(?x, AnC5i, ?y);
repeat id Gam`LINE'(?x, [D-2ep], mu1?, ?y) = Gam`LINE'(?x, mu1,   ?y);

id Gam`LINE'(?x) = TR5`LINE'(?x);

#endprocedure


#procedure convertGamTR5toTensor(LINE)
****************************************************
* here we convert the GamLINE and TR5LINE to tensors
****************************************************

repeat;
  id Gam`LINE'(?x,G5,     ?y) = Gam`LINE'(?x,G5i,   ?y);
  id Gam`LINE'(?x,AnC5,   ?y) = Gam`LINE'(?x,AnC5i, ?y);
  id Gam`LINE'(?x,[D-2ep],?y) = Gam`LINE'(?x,DDIMi, ?y);
  id Gam`LINE'(?x,[D],    ?y) = Gam`LINE'(?x,TILi,  ?y);
  id Gam`LINE'(?x,[-2ep], ?y) = Gam`LINE'(?x,HATi,  ?y);
endrepeat;

id Gam`LINE'(?x) = GamT`LINE'(?x);

repeat;
  id TR5`LINE'(?x,G5,  ?y) = TR5`LINE'(?x,G5i,  ?y);
  id TR5`LINE'(?x,AnC5,?y) = TR5`LINE'(?x,AnC5i,?y);
endrepeat;

id TR5`LINE'(?x) = TR5T`LINE'(?x);

#endprocedure


#procedure convertTensortoGamTR5(LINE)
****************************************************
* here we convert the tensors back to Gam and TR5
****************************************************

id GamT`LINE'(?x) = Gam`LINE'(?x);
repeat;
  id Gam`LINE'(?x,G5i,  ?y) = Gam`LINE'(?x,G5,     ?y);
  id Gam`LINE'(?x,AnC5i,?y) = Gam`LINE'(?x,AnC5,   ?y);
  id Gam`LINE'(?x,DDIMi,?y) = Gam`LINE'(?x,[D-2ep],?y);
  id Gam`LINE'(?x,TILi, ?y) = Gam`LINE'(?x,[D],    ?y);
  id Gam`LINE'(?x,HATi, ?y) = Gam`LINE'(?x,[-2ep], ?y);
endrepeat;

id TR5T`LINE'(?x) = TR5`LINE'(?x);
repeat;
  id TR5`LINE'(?x,G5i,  ?y) = TR5`LINE'(?x,G5,   ?y);
  id TR5`LINE'(?x,AnC5i,?y) = TR5`LINE'(?x,AnC5, ?y);
endrepeat;

#endprocedure

#procedure contractGamMetric(LINE)
****************************************************
* here we contract the gammafunctions with the     *
* the metric tensors.                              *
****************************************************

repeat;
* for the non-commuting GamFLINEs
  id GamF`LINE'(?x, [D-2ep], mu1?, ?y) * ddtilde(mu1?,mu2?) = GamF`LINE'(?x, [D],     mu2, ?y);
  id GamF`LINE'(?x, [D-2ep], mu1?, ?y) *   ddhat(mu1?,mu2?) = GamF`LINE'(?x, [-2ep],  mu2, ?y);
  id GamF`LINE'(?x, [D-2ep], mu1?, ?y) *      dd(mu1?,mu2?) = GamF`LINE'(?x, [D-2ep], mu2, ?y);

  id GamF`LINE'(?x, [D],     mu1?, ?y) * ddtilde(mu1?,mu2?) = GamF`LINE'(?x, [D], mu2, ?y);
  id GamF`LINE'(?x, [D],     mu1?, ?y) *   ddhat(mu1?,mu2?) = 0;
  id GamF`LINE'(?x, [D],     mu1?, ?y) *      dd(mu1?,mu2?) = GamF`LINE'(?x, [D], mu2, ?y);

  id GamF`LINE'(?x, [-2ep],  mu1?, ?y) * ddtilde(mu1?,mu2?) = 0;
  id GamF`LINE'(?x, [-2ep],  mu1?, ?y) *   ddhat(mu1?,mu2?) = GamF`LINE'(?x, [-2ep], mu2, ?y);
  id GamF`LINE'(?x, [-2ep],  mu1?, ?y) *      dd(mu1?,mu2?) = GamF`LINE'(?x, [-2ep], mu2, ?y);

* for the commuting GamLINEs
  id Gam`LINE'(?x, [D-2ep], mu1?, ?y) * ddtilde(mu1?,mu2?) = Gam`LINE'(?x, [D],     mu2, ?y);
  id Gam`LINE'(?x, [D-2ep], mu1?, ?y) *   ddhat(mu1?,mu2?) = Gam`LINE'(?x, [-2ep],  mu2, ?y);
  id Gam`LINE'(?x, [D-2ep], mu1?, ?y) *      dd(mu1?,mu2?) = Gam`LINE'(?x, [D-2ep], mu2, ?y);

  id Gam`LINE'(?x, [D],     mu1?, ?y) * ddtilde(mu1?,mu2?) = Gam`LINE'(?x, [D], mu2, ?y);
  id Gam`LINE'(?x, [D],     mu1?, ?y) *   ddhat(mu1?,mu2?) = 0;
  id Gam`LINE'(?x, [D],     mu1?, ?y) *      dd(mu1?,mu2?) = Gam`LINE'(?x, [D], mu2, ?y);

  id Gam`LINE'(?x, [-2ep],  mu1?, ?y) * ddtilde(mu1?,mu2?) = 0;
  id Gam`LINE'(?x, [-2ep],  mu1?, ?y) *   ddhat(mu1?,mu2?) = Gam`LINE'(?x, [-2ep], mu2, ?y);
  id Gam`LINE'(?x, [-2ep],  mu1?, ?y) *      dd(mu1?,mu2?) = Gam`LINE'(?x, [-2ep], mu2, ?y);
endrepeat;

#endprocedure


#procedure contractMetric
***************************************************
*** contract metric tensors in HV or NDR scheme ***
***************************************************
repeat;
  id dd(mu1?,mu1?)      = d_(mu1,mu1);
* this is a trick to avoid using the `DIMENSION' variable
  id ddtilde(mu1?,mu1?) = d_(mu1,mu1) - nom(0,-2);
  id ddhat(mu1?,mu1?) 	= nom(0,-2);

  id dd(mu1?,mu2?)*     dd(mu2?,mu3?) =      dd(mu1,mu3);
  id dd(mu1?,mu2?)*ddtilde(mu2?,mu3?) = ddtilde(mu1,mu3);
  id dd(mu1?,mu2?)*  ddhat(mu2?,mu3?) =   ddhat(mu1,mu3);

  id ddtilde(mu1?,mu2?)*ddtilde(mu2?,mu3?) = ddtilde(mu1,mu3);
  id ddtilde(mu1?,mu2?)*  ddhat(mu2?,mu3?) = 0;

  id ddhat(mu1?,mu2?)*ddhat(mu2?,mu3?) = ddhat(mu1,mu3);
endrepeat;
#endprocedure


#procedure contractGamCross(MAXLINE)
***************************************************
*** Generate hats and tildes from cross contractions in Gam's
***************************************************
#do m = 1,`MAXLINE'

  #do n = {`m'+1}, `MAXLINE'
    repeat id Gam`m'(?xm, [D-2ep], mu0?, ?ym) * Gam`n'(?xn, [-2ep],  mu0?, ?yn) =
              Gam`m'(?xm, [-2ep],  mu0,  ?ym) * Gam`n'(?xn, [-2ep],  mu0,  ?yn);

    repeat id Gam`n'(?xm, [D-2ep], mu0?, ?ym) * Gam`m'(?xn, [-2ep],  mu0?, ?yn) =
              Gam`n'(?xm, [-2ep],  mu0,  ?ym) * Gam`m'(?xn, [-2ep],  mu0,  ?yn);

    repeat id Gam`m'(?xm, [D-2ep], mu0?, ?ym) * Gam`n'(?xn, [D],     mu0?, ?yn) =
              Gam`m'(?xm, [D],     mu0,  ?ym) * Gam`n'(?xn, [D],     mu0,  ?yn);

    repeat id Gam`n'(?xm, [D-2ep], mu0?, ?ym) * Gam`m'(?xn, [D],     mu0?, ?yn) =
              Gam`n'(?xm, [D],     mu0,  ?ym) * Gam`m'(?xn, [D],     mu0,  ?yn);

    repeat id Gam`m'(?xm, [D],     mu0?, ?ym) * Gam`n'(?xn, [-2ep],  mu0?, ?yn) = 0;
    repeat id Gam`n'(?xm, [D],     mu0?, ?ym) * Gam`m'(?xn, [-2ep],  mu0?, ?yn) = 0;
  #enddo

  repeat id Gam`m'(?x, [D-2ep], mu0?, ?y, [-2ep],  mu0?, ?z) =
            Gam`m'(?x, [-2ep],  mu0,  ?y, [-2ep],  mu0,  ?z);

  repeat id Gam`m'(?x, [-2ep],  mu0?, ?y, [D-2ep], mu0?, ?z) =
            Gam`m'(?x, [-2ep],  mu0,  ?y, [-2ep],  mu0,  ?z);

  repeat id Gam`m'(?x, [D-2ep], mu0?, ?y, [D],     mu0?, ?z) =
            Gam`m'(?x, [D],     mu0,  ?y, [D],     mu0,  ?z);

  repeat id Gam`m'(?x, [D],     mu0?, ?y, [D-2ep], mu0?, ?z) =
            Gam`m'(?x, [D],     mu0,  ?y, [D],     mu0,  ?z);

  repeat id Gam`m'(?x, [D],     mu0?, ?y, [-2ep], mu0?, ?z) = 0;
  repeat id Gam`m'(?x, [-2ep],  mu0?, ?y, [D],    mu0?, ?z) = 0;

#enddo

#endprocedure


#procedure contractadjacent(LINE)
********************************************************
* This procedure accepts GamF objects (hopefully split
* already from G5 and AnC5) and contracts adjacent
* indices and momenta.
********************************************************

*** find and contract adjacent indices
#do m=1,1;
  #call contractMetric
  #call contractGamMetric(`LINE')

  id GamF`LINE'(?x,mu0?,?y,mu0?,?z) = GamF`LINE'(?x,tempmu,?y,tempmu,?z);
  
  repeat;

*   adjacent contractions with indices
    id GamF`LINE'(?x, [D-2ep], mu1?, [D-2ep], mu1?, ?y) = GamF`LINE'(?x,?y) *      d_(mu1,mu1);
    id GamF`LINE'(?x, [D-2ep], mu1?, [D],     mu1?, ?y) = GamF`LINE'(?x,?y) * ddtilde(mu1,mu1);
    id GamF`LINE'(?x, [D-2ep], mu1?, [-2ep],  mu1?, ?y) = GamF`LINE'(?x,?y) *   ddhat(mu1,mu1);

    id GamF`LINE'(?x, [D],     mu1?, [D-2ep], mu1?, ?y) = GamF`LINE'(?x,?y) * ddtilde(mu1,mu1);
    id GamF`LINE'(?x, [D],     mu1?, [D],     mu1?, ?y) = GamF`LINE'(?x,?y) * ddtilde(mu1,mu1);
    id GamF`LINE'(?x, [D],     mu1?, [-2ep],  mu1?, ?y) = 0;

    id GamF`LINE'(?x, [-2ep],  mu1?, [D-2ep], mu1?, ?y) = GamF`LINE'(?x,?y) *   ddhat(mu1,mu1);
    id GamF`LINE'(?x, [-2ep],  mu1?, [D],     mu1?, ?y) = 0;
    id GamF`LINE'(?x, [-2ep],  mu1?, [-2ep],  mu1?, ?y) = GamF`LINE'(?x,?y) *   ddhat(mu1,mu1);

*   adjacent contractions of momenta
    id GamF`LINE'(?x, [D-2ep], p?,   [D-2ep], p?,   ?y) = GamF`LINE'(?x,?y) *      d_(p,p);
    id GamF`LINE'(?x, [D],     p?,   [D],     p?,   ?y) = GamF`LINE'(?x,?y) * ddtilde(p,p);
    id GamF`LINE'(?x, [-2ep],  p?,   [-2ep],  p?,   ?y) = GamF`LINE'(?x,?y) *   ddhat(p,p);

*   bring adjacent index closer
    id GamF`LINE'(?x, [D-2ep], tempmu, [D-2ep], mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [D-2ep], mu1,    [D-2ep], tempmu, ?y, aa,  tempmu, ?z)
     + 2 * d_(tempmu,mu1) * GamF`LINE'(?x, ?y, aa, tempmu, ?z);

    id GamF`LINE'(?x, [D-2ep], tempmu, [D],     mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [D],     mu1,    [D-2ep], tempmu, ?y, aa,  tempmu, ?z)
     + 2 * ddtilde(tempmu,mu1) * GamF`LINE'(?x, ?y, aa, tempmu, ?z);

    id GamF`LINE'(?x, [D-2ep], tempmu, [-2ep],  mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [-2ep],  mu1,    [D-2ep], tempmu, ?y, aa,  tempmu, ?z)
     + 2 * ddhat(tempmu,mu1)   * GamF`LINE'(?x, ?y, aa, tempmu, ?z);

    id GamF`LINE'(?x, [D],     tempmu, [D-2ep], mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [D-2ep], mu1,    [D],     tempmu, ?y, aa,  tempmu, ?z)
     + 2 * ddtilde(tempmu,mu1) * GamF`LINE'(?x, ?y, aa, tempmu, ?z);

    id GamF`LINE'(?x, [D],     tempmu, [D],     mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [D],     mu1,    [D],     tempmu, ?y, aa,  tempmu, ?z)
     + 2 * ddtilde(tempmu,mu1) * GamF`LINE'(?x, ?y, aa, tempmu, ?z);

    id GamF`LINE'(?x, [D],     tempmu, [-2ep],  mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [-2ep],  mu1,    [D],     tempmu, ?y, aa,  tempmu, ?z);


    id GamF`LINE'(?x, [-2ep],  tempmu, [D-2ep], mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [D-2ep], mu1,    [-2ep],  tempmu, ?y, aa,  tempmu, ?z)
     + 2 * ddhat(tempmu,mu1)   * GamF`LINE'(?x, ?y, aa, tempmu, ?z);

    id GamF`LINE'(?x, [-2ep],  tempmu, [D],     mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [D],     mu1,    [-2ep],  tempmu, ?y, aa,  tempmu, ?z);

    id GamF`LINE'(?x, [-2ep],  tempmu, [-2ep],  mu1?,   ?y, aa?, tempmu, ?z) =
     - GamF`LINE'(?x, [-2ep],  mu1,    [-2ep],  tempmu, ?y, aa,  tempmu, ?z)
     + 2 * ddhat(tempmu,mu1)   * GamF`LINE'(?x, ?y, aa, tempmu, ?z);

  endrepeat;

  sum tempmu;

  if (match(GamF`LINE'(?x,mu0?,?y,mu0?,?z))) redefine m "0";
  .sort

#enddo

*** order again
#call orderGamF(`LINE')

*** find and contract adjacent momenta
#do m=1,1;
  #call contractMetric
  #call contractGamMetric(`LINE')

  multiply counter;
  id counter * GamF`LINE'(?x, [D-2ep], p?, ?y, [D-2ep], p?, ?z) = dummycf(p) * GamF`LINE'(?x, [D-2ep], p, ?y, [D-2ep], p, ?z);
  id counter * GamF`LINE'(?x, [D],     p?, ?y, [D],     p?, ?z) = dummycf(p) * GamF`LINE'(?x, [D],     p, ?y, [D],     p, ?z);
  id counter * GamF`LINE'(?x, [-2ep],  p?, ?y, [-2ep],  p?, ?z) = dummycf(p) * GamF`LINE'(?x, [-2ep],  p, ?y, [-2ep],  p, ?z);
  id counter * GamF`LINE'(?x, [D],     p?, ?y, [D-2ep], p?, ?z) = dummycf(p) * GamF`LINE'(?x, [D],     p, ?y, [D-2ep], p, ?z);
  id counter * GamF`LINE'(?x, [-2ep],  p?, ?y, [D-2ep], p?, ?z) = dummycf(p) * GamF`LINE'(?x, [-2ep],  p, ?y, [D-2ep], p, ?z);
  id counter * GamF`LINE'(?x, [-2ep],  p?, ?y, [D],     p?, ?z) = dummycf(p) * GamF`LINE'(?x, [-2ep],  p, ?y, [D],     p, ?z);
  id counter = 1;

  repeat;

*   adjacent contractions with indices
    id GamF`LINE'(?x, [D-2ep], mu1?, [D-2ep], mu1?, ?y) = GamF`LINE'(?x,?y) *      d_(mu1,mu1);
    id GamF`LINE'(?x, [D-2ep], mu1?, [D],     mu1?, ?y) = GamF`LINE'(?x,?y) * ddtilde(mu1,mu1);
    id GamF`LINE'(?x, [D-2ep], mu1?, [-2ep],  mu1?, ?y) = GamF`LINE'(?x,?y) *   ddhat(mu1,mu1);

    id GamF`LINE'(?x, [D],     mu1?, [D-2ep], mu1?, ?y) = GamF`LINE'(?x,?y) * ddtilde(mu1,mu1);
    id GamF`LINE'(?x, [D],     mu1?, [D],     mu1?, ?y) = GamF`LINE'(?x,?y) * ddtilde(mu1,mu1);
    id GamF`LINE'(?x, [D],     mu1?, [-2ep],  mu1?, ?y) = 0;

    id GamF`LINE'(?x, [-2ep],  mu1?, [D-2ep], mu1?, ?y) = GamF`LINE'(?x,?y) *   ddhat(mu1,mu1);
    id GamF`LINE'(?x, [-2ep],  mu1?, [D],     mu1?, ?y) = 0;
    id GamF`LINE'(?x, [-2ep],  mu1?, [-2ep],  mu1?, ?y) = GamF`LINE'(?x,?y) *   ddhat(mu1,mu1);

*   adjacent contractions of momenta
    id GamF`LINE'(?x, [D-2ep], p?,   [D-2ep], p?,   ?y) = GamF`LINE'(?x,?y) *      d_(p,p);
    id GamF`LINE'(?x, [D],     p?,   [D],     p?,   ?y) = GamF`LINE'(?x,?y) * ddtilde(p,p);
    id GamF`LINE'(?x, [-2ep],  p?,   [-2ep],  p?,   ?y) = GamF`LINE'(?x,?y) *   ddhat(p,p);

* JB: Do we really need these? Including them can lead to an infinite loop. Delete if you agree.
*    id GamF`LINE'(?x, [D],     p?,   [D-2ep], p?,   ?y) =
*     - GamF`LINE'(?x, [D-2ep], p,    [D],     p,    ?y) + 2 * GamF`LINE'(?x,?y) * ddtilde(p,p);
*    id GamF`LINE'(?x, [-2ep],  p?,   [D-2ep], p?,   ?y) =
*     - GamF`LINE'(?x, [D-2ep], p,    [-2ep],  p,    ?y) + 2 * GamF`LINE'(?x,?y) * ddhat(p,p);
*    id GamF`LINE'(?x, [-2ep],  p?,   [D],     p?,   ?y) =
*     - GamF`LINE'(?x, [D],     p,    [-2ep],  p,    ?y);

*   bring adjacent momenta closer
    id dummycf(p?) * GamF`LINE'(?x, [D-2ep], p?,  [D-2ep], mu1?, ?y, aa?, p?, ?z) =
     - dummycf(p)  * GamF`LINE'(?x, [D-2ep], mu1, [D-2ep], p,    ?y, aa,  p,  ?z)
     + 2 * d_(p,mu1) * GamF`LINE'(?x, ?y, aa, p, ?z);

    id dummycf(p?) * GamF`LINE'(?x, [D],     p?,  [D],     mu1?, ?y, aa?, p?, ?z) =
     - dummycf(p)  * GamF`LINE'(?x, [D],     mu1, [D],     p,    ?y, aa,  p,  ?z)
     + 2 * ddtilde(p,mu1) * GamF`LINE'(?x, ?y, aa, p, ?z);

    id dummycf(p?) * GamF`LINE'(?x, [-2ep],  p?,  [-2ep],  mu1?, ?y, aa?, p?, ?z) =
     - dummycf(p)  * GamF`LINE'(?x, [-2ep],  mu1, [-2ep],  p,    ?y, aa,  p,  ?z)
     + 2 * ddhat(p,mu1)   * GamF`LINE'(?x, ?y, aa, p, ?z);

    id dummycf(p?) * GamF`LINE'(?x, [D],     p?,  [D-2ep], mu1?, ?y, aa?, p?, ?z) =
     - dummycf(p)  * GamF`LINE'(?x, [D-2ep], mu1, [D],     p,    ?y, aa,  p,  ?z)
     + 2 * ddtilde(p,mu1) * GamF`LINE'(?x, ?y, aa, p, ?z);

    id dummycf(p?) * GamF`LINE'(?x, [-2ep],  p?,  [D-2ep], mu1?, ?y, aa?, p?, ?z) =
     - dummycf(p)  * GamF`LINE'(?x, [D-2ep], mu1, [-2ep],  p,    ?y, aa,  p,  ?z)
     + 2 * ddhat(p,mu1)   * GamF`LINE'(?x, ?y, aa, p, ?z);

    id dummycf(p?) * GamF`LINE'(?x, [-2ep],  p?,  [D],     mu1?, ?y, aa?, p?, ?z) =
     - dummycf(p)  * GamF`LINE'(?x, [D],     mu1, [-2ep],  p,    ?y, aa,  p,  ?z);

* JB: Do we really need these? Including them can lead to an infinite loop. Delete if you agree.
*    id dummycf(p?) * GamF`LINE'(?x, [D-2ep], p?,  [D],     mu1?, ?y, aa?, p?, ?z) =
*     - dummycf(p)  * GamF`LINE'(?x, [D],     mu1, [D-2ep], p,    ?y, aa,  p,  ?z)
*     + 2 * ddtilde(p,mu1) * GamF`LINE'(?x, ?y, aa, p, ?z);

*    id dummycf(p?) * GamF`LINE'(?x, [D-2ep], p?,  [-2ep],  mu1?, ?y, aa?, p?, ?z) =
*     - dummycf(p)  * GamF`LINE'(?x, [-2ep],  mu1, [D-2ep], p,    ?y, aa,  p,  ?z)
*     + 2 * ddhat(p,mu1)   * GamF`LINE'(?x, ?y, aa, p, ?z);

*    id dummycf(p?) * GamF`LINE'(?x, [D],     p?,  [-2ep],  mu1?, ?y, aa?, p?, ?z) =
*     - dummycf(p)  * GamF`LINE'(?x, [-2ep],  mu1, [D],     p,    ?y, aa,  p,  ?z);

  endrepeat;

  id dummycf(p?) = 1;

  if (
      (match(GamF`LINE'(?x, [D-2ep], p?, ?y, [D-2ep], p?, ?z))) ||
      (match(GamF`LINE'(?x, [D],     p?, ?y, [D],     p?, ?z))) ||
      (match(GamF`LINE'(?x, [-2ep],  p?, ?y, [-2ep],  p?, ?z))) ||
      (match(GamF`LINE'(?x, [D],     p?, ?y, [D-2ep], p?, ?z))) ||
      (match(GamF`LINE'(?x, [-2ep],  p?, ?y, [D-2ep], p?, ?z))) ||
      (match(GamF`LINE'(?x, [-2ep],  p?, ?y, [D],     p?, ?z))) ) redefine m "0";
  .sort

#enddo

#call contractMetric
#call contractGamMetric(`LINE')

#endprocedure


#procedure orderGamF(LINE)
*******************************************************************
* this procedure orders the GamF's as  GamFLINE([D-2ep],...,[D],...,[-2ep],...)
*******************************************************************
repeat;
  id GamF`LINE'(?x, [D],     uu1?, [D-2ep], uu2?, ?y) =
   - GamF`LINE'(?x, [D-2ep], uu2,  [D],     uu1,  ?y) + 2 * GamF`LINE'(?x,?y) * ddtilde(uu1,uu2);

  id GamF`LINE'(?x, [-2ep],  uu1?, [D-2ep], uu2?, ?y) =
   - GamF`LINE'(?x, [D-2ep], uu2,  [-2ep],  uu1,  ?y) + 2 * GamF`LINE'(?x,?y) * ddhat(uu1,uu2);

  id GamF`LINE'(?x, [-2ep],  uu1?, [D],    uu2?, ?y) =
   - GamF`LINE'(?x, [D],     uu2,  [-2ep], uu1,  ?y);
endrepeat;
#endprocedure

#procedure treatGams(MAXLINE)
*******************************************************************
*                                                                 *
* This program takes commuting GamLINE objects contracts adjacent *
* indices, momenta, and orders the rest. It does not do anything  *
* about G5s                                                       *
*                                                                 *
*******************************************************************

*.sort
*g asda = Gam1([D-2ep],p1,[D-2ep],p1,[D-2ep],mu2,[D-2ep],N2_?,[D-2ep],mu5)*Gam2([D-2ep],p1
*      ,[D-2ep],p1,[D-2ep],N3_?,[D-2ep],nu8,[D-2ep],mu5,[D],mu6,[-2ep],mu6)*e_(mu2,N2_?,N3_?,nu8);
*.sort

* convert all summed Lorentz indices to dummyindices
#do i=1,{`MAXLINE'-1}
#do j={`i'+1},`MAXLINE'
repeat;
  if (match(Gam`i'(?x,mu1?!dummyindices_$sumind,?y)*Gam`j'(?z,mu1?,?w))) sum $sumind;
endrepeat;
#enddo
#enddo

#do i=1,`MAXLINE'
repeat;
  if (match(Gam`i'(?x,mu1?!dummyindices_$sumind,?y,mu1?,?z))) sum $sumind;
endrepeat;
#enddo

#do i=1,`MAXLINE'
repeat;
  if (match(Gam`i'(?x,mu1?!dummyindices_$sumind,?y)*e_(mu1?,mu2?,mu3?,mu4?))) sum $sumind;
endrepeat;
#enddo


* convert all summed Lorentz indices to fixed indices
#do i=1,`MAXLINE'
  id Gam`i'(?x) = Gam`i'(?x) * dummycf(?x);
#enddo
id e_(uu1?,uu2?,uu3?,uu4?) = e_(uu1,uu2,uu3,uu4)*dummycf(uu1,uu2,uu3,uu4);

repeat id dummycf(?x,aa?,               ?y) = dummycf(?x,?y);
repeat id dummycf(?x,p?,                ?y) = dummycf(?x,?y);
repeat id dummycf(?x,mu0?!dummyindices_,?y) = dummycf(?x,?y);
id dummycf(?x) = dummycf(nargs_(?x));
repeat id dummycf(xx1?)*dummycf(xx2?) = dummycf(xx1+xx2);
id once dummycf(xx1?) = dummycf(xx1/2);

* substitute the dummycf's
#do i=1,`MAXLINE'
repeat;
  id once dummycf(xx1?) *Gam`i'(?x,mu1?dummyindices_,?y,mu1?,      ?z) =
          dummycf(xx1-1)*Gam`i'(?x,isumN[xx1],       ?y,isumN[xx1],?z);
endrepeat;
#enddo

#do i=1,`MAXLINE'
repeat;
  id once dummycf(xx1?) *Gam`i'(?x,mu1?dummyindices_,?y)*e_(mu1?,      mu2?,mu3?,mu4?) = 
          dummycf(xx1-1)*Gam`i'(?x,isumN[xx1],       ?y)*e_(isumN[xx1],mu2, mu3, mu4);
endrepeat;
#enddo

#do i=1,{`MAXLINE'-1}
#do j={`i'+1},`MAXLINE'
repeat;
  id once dummycf(xx1?) *Gam`i'(?x,mu1?dummyindices_,?y)*Gam`j'(?z,mu1?,      ?w) =
          dummycf(xx1-1)*Gam`i'(?x,isumN[xx1],       ?y)*Gam`j'(?z,isumN[xx1],?w);
endrepeat;
#enddo
#enddo

id dummycf(0)=1;
*b Gam1,Gam2,e_,dummycf;
*print[] +s;
.sort


*** Take care of cross contractions
#call contractGamCross(`MAXLINE')

*** Contract adjacent indices and momenta and bring to standard order
#do n=1,1
  #do LINE = 1,`MAXLINE'

*   split Gam to GamF
    #call splitGam(`LINE')

*   Contract adjacent indices and momenta in GamF
    #call contractadjacent(`LINE')

*   Contract with metric
    #do L = 1,`MAXLINE'
      #call contractGamMetric(`L')
      #call generateGam(`L')
    #enddo

*   Take care of cross contractions
    #call contractGamCross(`MAXLINE')

*   split Gam to GamF
    #call splitGam(`LINE')

*   bring to an order: [D-2ep],...,[D],...,[-2ep],...
    #call orderGamF(`LINE')

*   Contract with metric
    #do L = 1,`MAXLINE'
      #call contractGamMetric(`L')
      #call generateGam(`L')
    #enddo

*   Take care of cross contractions
    #call contractGamCross(`MAXLINE')

    .sort

*   Fully split Gam to GamF to use disorder to order
    #call splitfullGam(`LINE')

*   ONCE CHECKED REMOVE BELOW UP TO
    repeat id GamF`LINE'([D-2ep],mu1?) = dummynt(mu1);
    id once,disorder dummynt(mu2?) * dummynt(mu1?) =
                   - GamF`LINE'([D-2ep],mu1)  * GamF`LINE'([D-2ep],mu2)
                   + 2 * d_(mu1,mu2) * GamF`LINE';
    repeat id dummynt(mu1?) = GamF`LINE'([D-2ep],mu1);

    repeat id GamF`LINE'([D],mu1?) = dummynt(mu1);
    id once,disorder dummynt(mu2?) * dummynt(mu1?) =
                   - GamF`LINE'([D],mu1)  * GamF`LINE'([D],mu2)
                   + 2 * ddtilde(mu1,mu2) * GamF`LINE';
    repeat id dummynt(mu1?) = GamF`LINE'([D],mu1);

    repeat id GamF`LINE'([-2ep],mu1?) = dummynt(mu1);
    id once,disorder dummynt(mu2?) * dummynt(mu1?) =
                   - GamF`LINE'([-2ep],mu1)  * GamF`LINE'([-2ep],mu2)
                   + 2 * ddhat(mu1,mu2) * GamF`LINE';
    repeat id dummynt(mu1?) = GamF`LINE'([-2ep],mu1);
*   HERE AND THEN UNCOMMENT THE STUFF BELOW
    
*    id once,disorder GamF`LINE'([D-2ep],mu2?) * GamF`LINE'([D-2ep],mu1?) =
*                   - GamF`LINE'([D-2ep],mu1)  * GamF`LINE'([D-2ep],mu2)
*                   + 2 * d_(mu1,mu2) * GamF`LINE';
*
*    id once,disorder GamF`LINE'([D],mu2?) * GamF`LINE'([D],mu1?) =
*                   - GamF`LINE'([D],mu1)  * GamF`LINE'([D],mu2)
*                   + 2 * ddtilde(mu1,mu2) * GamF`LINE';
*
*    id once,disorder GamF`LINE'([-2ep],mu2?) * GamF`LINE'([-2ep],mu1?) =
*                   - GamF`LINE'([-2ep],mu1)  * GamF`LINE'([-2ep],mu2)
*                   + 2 * ddhat(mu1,mu2) * GamF`LINE';

*   Join back the GamF to Gam
    #call generateGam(`LINE')

*   Make sure everything is contracted
    #call contractMetric
    #do m = 1,`MAXLINE'
      #call contractGamMetric(`m')
    #enddo

*   Take care of cross contractions
    #call contractGamCross(`MAXLINE')

    .sort
  #enddo
  

* redo if after procedure more adjacent indices were generated or ordering not done
  #do LINE = 1,`MAXLINE'

*   indices
    if (match(Gam`LINE'(?x,mu0?,?y,mu0?,?z))) redefine n "0";

*   momenta
    if (match(Gam`LINE'(?x,[D-2ep],p?,?y,[D-2ep],p?,?z))) redefine n "0";
    if (match(Gam`LINE'(?x,[D],    p?,?y,[D],    p?,?z))) redefine n "0";
    if (match(Gam`LINE'(?x,[-2ep], p?,?y,[-2ep], p?,?z))) redefine n "0";
    if (match(Gam`LINE'(?x,[D],    p?,?y,[D-2ep],p?,?z))) redefine n "0";
    if (match(Gam`LINE'(?x,[-2ep], p?,?y,[D-2ep],p?,?z))) redefine n "0";
    if (match(Gam`LINE'(?x,[-2ep], p?,?y,[D],    p?,?z))) redefine n "0";

*   ordering
    #call splitfullGam(`LINE')

*   ONCE CHECKED REMOVE BELOW UP TO
    repeat id GamF`LINE'([D-2ep],mu1?) = dummynt(mu1);
    if (match(disorder, dummynt(mu2?) * dummynt(mu1?))) redefine n "0";
    repeat id dummynt(mu1?) = GamF`LINE'([D-2ep],mu1);
    
    repeat id GamF`LINE'([D],mu1?) = dummynt(mu1);
    if (match(disorder, dummynt(mu2?) * dummynt(mu1?))) redefine n "0";
    repeat id dummynt(mu1?) = GamF`LINE'([D],mu1);
    
    repeat id GamF`LINE'([-2ep],mu1?) = dummynt(mu1);
    if (match(disorder, dummynt(mu2?) * dummynt(mu1?))) redefine n "0";
    repeat id dummynt(mu1?) = GamF`LINE'([-2ep],mu1);
*   HERE AND THEN UNCOMMENT THE STUFF BELOW

*    if (match(disorder, GamF`LINE'([D-2ep],mu2?) * GamF`LINE'([D-2ep],mu1?))) redefine n "0";
*    if (match(disorder, GamF`LINE'([D],    mu2?) * GamF`LINE'([D],    mu1?))) redefine n "0";
*    if (match(disorder, GamF`LINE'([-2ep], mu2?) * GamF`LINE'([-2ep], mu1?))) redefine n "0";

    #call generateGam(`LINE')

  #enddo
  .sort

#enddo

sum isum1,...,isum100;
.sort


#endprocedure

******************
* vim: syntax=form
******************
