#procedure expandep(max)
*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************

*****************************************
* Expand nom and deno                   *
*****************************************

id deno(0,xx1?) = 1/xx1/ep;

id d = nom(`DIMENSION',-2);

id nom(xx1?,xx2?) = xx1 + ep * xx2;

id deno(xx1?,xx2?) = 1/xx1*sum_(sumind,0,`max',sign_(sumind)*(xx2/xx1*ep)^sumind);

#endprocedure

******************
* vim: syntax=form
******************
