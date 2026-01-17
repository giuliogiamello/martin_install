#procedure treatEps4d(i,j)

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
* our epsilon tensor sofar is purely 4d and so are its contractions

#include contractEpsMetric4d

* Contract epsilon tensors with the gammas
* We use the convention that the
* indices of the epsilon tensor are upstairs. 

#do LINE = `i',`j'
  repeat id Gam`LINE'(?x,[-2ep],mu4?,?y) * e_(mu1?,mu2?,mu3?,mu4?) = 0;
#enddo

#do LINE = `i',`j'
* (convention of e_0123=1 as in Tracer.m)
  repeat id Gam`LINE'(?x,[D-2ep],mu4?,?y) * e_(mu1?,mu2?,mu3?,mu4?) =
               + i_* ddtilde(mu1,mu2) * Gam`LINE'(?x, G5, [D], mu3, ?y)
               - i_* ddtilde(mu1,mu3) * Gam`LINE'(?x, G5, [D], mu2, ?y)
               + i_* ddtilde(mu2,mu3) * Gam`LINE'(?x, G5, [D], mu1, ?y)
               - i_* Gam`LINE'(?x, G5, [D], mu1, [D], mu2, [D], mu3, ?y);
#enddo

#do LINE = `i',`j'
* (convention of e_0123=1 as in Tracer.m)
  repeat id Gam`LINE'(?x,[D],mu4?,?y) * e_(mu1?,mu2?,mu3?,mu4?) =
               + i_* ddtilde(mu1,mu2) * Gam`LINE'(?x, G5, [D], mu3, ?y)
               - i_* ddtilde(mu1,mu3) * Gam`LINE'(?x, G5, [D], mu2, ?y)
               + i_* ddtilde(mu2,mu3) * Gam`LINE'(?x, G5, [D], mu1, ?y)
               - i_* Gam`LINE'(?x, G5, [D], mu1, [D], mu2, [D], mu3, ?y);
#enddo


* Express in terms of d and -2*ep dimensional objects
id ddtilde(mu1?,mu2?) = d_(mu1,mu2) - ddhat(mu1,mu2);

#call contractMetric

#do LINE = `i',`j'
  repeat id Gam`LINE'(?x,[D],mu1?,?y) = Gam`LINE'(?x,[D-2ep],mu1,?y) - Gam`LINE'(?x,[-2ep],mu1,?y);
  #call contractGamMetric(`LINE')
#enddo

.sort
#endprocedure

* vim: syntax=form
