#procedure treatEpsDd(i,j)

*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************

*******************************************************************
*** Treat epsilon tensors *****************************************
*******************************************************************

* Contract epsilon tensors with metric tensors and themselves

#include contractEpsMetricDd

* Contract epsilon tensors with the gammas
* We use the convention that the
* indices of the epsilon tensor are upstairs. 

#do LINE = `i',`j'
  if (match(Gam`LINE'(?x,[-2ep],mu1?,?y))) 
     exit "The whole point of d-dimensional eps is to not have [-2ep] Gammas";
  if (match(Gam`LINE'(?x,[D],   mu1?,?y))) 
     exit "The whole point of d-dimensional eps is to not have [D] Gammas";
#enddo

#do LINE = `i',`j'
* (convention of e_0123=1 as in Tracer.m)
  repeat id Gam`LINE'(?x,[D-2ep],mu4?,?y) * e_(mu1?,mu2?,mu3?,mu4?) =
               + i_* d_(mu1,mu2) * Gam`LINE'(?x, G5, [D-2ep], mu3, ?y)
               - i_* d_(mu1,mu3) * Gam`LINE'(?x, G5, [D-2ep], mu2, ?y)
               + i_* d_(mu2,mu3) * Gam`LINE'(?x, G5, [D-2ep], mu1, ?y)
               - i_* Gam`LINE'(?x, G5, [D-2ep], mu1, [D-2ep], mu2, [D-2ep], mu3, ?y);
#enddo

#call contractMetric

#do LINE = `i',`j'
  #call contractGamMetric(`LINE')
#enddo

.sort
#endprocedure

* vim: syntax=form
