#procedure performira(POWER)

*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************

*******************************************************************
*
* The Infrared Rearrangement
* (Chetyrkin, Misiak, Muenz, hep-ph/9711266)
* is applied in this procedure.
*
* intp`i' is the integration momentum
* extq`i' is the external momentum
* mIRA is the auxiliary mass
*
* This procedure has to be changed in the three-loop case (but not
* by much)
*
*******************************************************************
#call printMessage(IRA dropping terms with more than `POWER' powers of external momenta)

*** scaleless propagators make no sense. Use 1PI diagrams
if (match(Deno(0,0))) exit "Deno(0,0) encountered: use one-light particle irreducible diagrams";

*** drop powers of external momenta
if ( count(q1,1,q2,1,q3,1,q4,1,q5,1,q6,1,q7,1,q8,1,q9,1,q10,1) > `POWER' ) discard;

*** bring Denos into more appropriate form
id Deno(?a,M?) = fakef1(?a)*fakef2(?a)*fakef3(M);

argument fakef1;
  id q?qext = 0;
endargument;
argument fakef2;
  id p?pint = 0;
endargument;

id fakef1(?a)*fakef2(?b)*fakef3(?c) = Deno(int,?a,ext,?b,mass,?c);

*** 1/q^2 propagators are probably a bug
if (match(Deno(int,0,ext,q?,mass,0))) exit "Deno(q,0) encountered: are you sure you are not screwing sth up?";

*** 1/(q^2-M^2) propagators in IRA are probably a bug.
* Of course there is no problem to expand them using expanddeno.prc
* if it's not a bug. However, let's do that if we actually find a case
* in which it makes sense

if (match(Deno(int,0,ext,q?,mass,M?))) exit "This is performira.prc. Deno(q,M) encountered: are you sure you are not screwing sth up?";
*id Deno(int, 0,  ext, q?, mass, M?) =    -1/M^2*sum_(xx1, 0, `POWER', (q.q/M^2)^xx1);

*** 1/M^2 happens in tadpole diagrams
id Deno(int,0,ext,0,mass,M?) = -1/M^2;
*id Deno(int,0,ext,0,mass,M?) = -1/mIRA^2;


*** add powers of pc to count the superficial degree of divergence
multiply pc^( `DIMENSION' * `LOOP' );

id p1 = pc*p1;
id p2 = pc*p2;

id Deno(int,p?,ext,?b,mass,?c) = Deno(int,p,ext,?b,mass,?c)*pc^(-2);

********************************
*** Now perform the IRA ********
********************************

#do k = 1, 1
  b Deno,pc,q1,q2,q3,q4,q5,q6,q7,q8,q9,q10;
  .sort
  Keep Brackets;

* the zero-external momentum case
  if (match(Deno(int,p1?,ext,0,mass,M?!{mIRA})));
     id Deno(int,p1?,ext,0,mass,M?!{mIRA}) =
        Deno(p1, mIRA) * (1 + Deno(int,p1,ext,0,mass,M)*M^2*pc^(-2));
  endif;

* the non-zero-external momentum case
  if (match(Deno(int,p1?,ext,q1?,mass,M?!{mIRA})));
     id Deno(int,p1?,ext,q1?,mass,M?!{mIRA}) =(
                    Deno(p1,mIRA)
                  - Deno(p1,mIRA)*Deno(int,p1,ext,q1,mass,M)* q1.q1*pc^(-2)
                  + Deno(p1,mIRA)*Deno(int,p1,ext,q1,mass,M)* M^2*pc^(-2)
                  - Deno(p1,mIRA)*Deno(int,p1,ext,q1,mass,M)*2*p1.q1*pc^(-1));
  endif;

  if ( count(q1,1,q2,1,q3,1,q4,1,q5,1,q6,1,q7,1,q8,1,q9,1,q10,1) > `POWER' ) discard;
  if ( (match(Deno(?a,ext,?b))) && (count(pc,1) < -2) ) discard;
  if (  match(Deno(?a,ext,?b))) redefine k "0";

  .sort

#enddo

*** Test and standard form ****************************************
id Deno(-p?pint, M?) = Deno(p,M);
id Deno(-p2+p1,  M?) = Deno(p1-p2,M);
id Deno(-p1-p2,  M?) = Deno(p1+p2,M);

id pc      = 1;
id pc^(-1) = 1;

if (match(Deno(?a,int,?b)))  exit "Sth failed in IRA";
if (match(Deno(?a,ext,?b)))  exit "Sth failed in IRA";
if (match(Deno(?a,mass,?b))) exit "Sth failed in IRA";

#call printMessage(Infrared rearrangement done)

#endprocedure
******************
* vim: syntax=form
******************
