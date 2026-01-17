#procedure diracalgebra3d(LINE)

*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************

exit "Check this before using";

* test for G5 PL PR
if ( match(g_(`LINE', 5))) exit "Have not implemented g_5 in 3d";
if ( match(g_(`LINE', 6))) exit "Have not implemented P_R in 3d";
if ( match(g_(`LINE', 7))) exit "Have not implemented P_L in 3d";

* the flag aa will become the identity later
id once g_(`LINE', uu1?) = aa * g_(`LINE', uu1);

* create own gamma matrices
id g_(`LINE', uu1?) = dummynt(uu1);
repeat id once dummynt(?x)*dummynt(uu1?)=dummynt(?x, uu1);

* perform the gamma algebra
repeat;
  id dummynt(?x, uu1?,  uu1?,  ?y) = nom(`DIMENSION',-2)*dummynt(?x, ?y);
  id dummynt(?x, v1?, v1?, ?y) =           v1.v1*dummynt(?x, ?y);

  id dummynt(?x, uu1?,  uu2?,  ?y, uu1?, ?z) =
                  2*dummynt(?x, ?y, uu2, ?z) - dummynt(?x, uu2, uu1, ?y, uu1, ?z);
endrepeat;

repeat;
  id dummynt(?x, uu1?,  uu1?,  ?y) = nom(`DIMENSION',-2)*dummynt(?x, ?y);
  id dummynt(?x, v1?, v1?, ?y) = v1.v1*dummynt(?x, ?y);
  id dummynt(?x, v1?, uu2?,  ?y, v1?, ?z) =
                  2*d_(v1, uu2)*dummynt(?x, ?y, v1, ?z) - dummynt(?x, uu2, v1, ?y, v1, ?z);
endrepeat;

* bring to standard form
id dummynt = 1;
id dummynt(?x) = g_(`LINE', ?x);
id aa = gi_(`LINE');

#endprocedure
******************
* vim: syntax=form
******************
