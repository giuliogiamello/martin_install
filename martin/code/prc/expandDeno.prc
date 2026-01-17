#procedure expandDeno(POWER)

*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************
*************************************************************
* This Program expands the Denominators in the external     *
* momenta and drops terms with a power larger than POWER    *
*************************************************************

#call printMessage(Expanding propagators in external momenta dropping powers > `POWER')

*** scaleless propagators make no sense. Use 1PI diagrams
if (match(Deno(0,0))) exit "Deno(0,0) encountered: use one-light particle irreducible diagrams";

*** bring Denos into more appropriate form
id Deno(?a,M?) = fakef1(?a)*fakef2(?a)*fakef3(M);

argument fakef1;
  id q?qext = 0;
endargument;
argument fakef2;
  id p?pint = 0;
endargument;

id fakef1(?a)*fakef2(?b)*fakef3(?c) = Deno(int,?a,ext,?b,mass,?c);

*** expand up to order POWER ***

*** 1/q^2 propagators are probably a bug
if (match(Deno(int,0,ext,q?,mass,0))) exit "Deno(q,0) encountered: are you sure you are not screwing sth up?";
*id Deno(int, 0,  ext, q?, mass, 0)  = 1/q.q;

repeat;
id once Deno(int, 0,  ext, q?, mass, M?) = -1/M^2*sum_(xx1, 0, `POWER', (q.q/M^2)^xx1);

id      Deno(int, 0,  ext, 0,  mass, M?) = -1/M^2;

id      Deno(int, p?, ext, 0,  mass, M?) = Deno(p,M);

id once Deno(int, p?, ext, q?, mass, M?) = Deno(p,M)*sum_(xx1, 0, `POWER', ((-2*p.q - q.q)*Deno(p,M))^xx1);

*** drop powers of external momenta
if ( count(q1,1,q2,1,q3,1,q4,1,q5,1,q6,1,q7,1,q8,1,q9,1,q10,1) > `POWER' ) discard;

endrepeat;

.sort

id Deno(-p?pint, M?) = Deno(p,M);
id Deno(-p2+p1,  M?) = Deno(p1-p2,M);
id Deno(-p1-p2,  M?) = Deno(p1+p2,M);

*#message Expanded propagators in external momenta, dropped powers higher than `POWER'

#endprocedure

******************
* vim: syntax=form
******************
