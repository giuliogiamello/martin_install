#procedure trace3d(LINE)

*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************

exit "Check this before using, also trace.prc may be enough";

if ( match(g_(`LINE', 5))) exit "Have not implemented g_5 in 3d";
if ( match(g_(`LINE', 6))) exit "Have not implemented P_R in 3d";
if ( match(g_(`LINE', 7))) exit "Have not implemented P_L in 3d";

*.store
*g test1 = g_(`LINE',nu1);
*g test2 = g_(`LINE',nu1)*g_(`LINE',nu2);
*g test3 = g_(`LINE',nu1)*g_(`LINE',nu2)*g_(`LINE',nu3);
*g test4 = g_(`LINE',nu1)*g_(`LINE',nu2)*g_(`LINE',nu3)*g_(`LINE',nu4);
*g test5 = g_(`LINE',nu1)*g_(`LINE',nu2)*g_(`LINE',nu3)*g_(`LINE',nu4)*g_(`LINE',nu5);
*g test6 = g_(`LINE',nu1)*g_(`LINE',nu2)*g_(`LINE',nu3)*g_(`LINE',nu4)*g_(`LINE',nu5)*g_(`LINE',nu6);
*g test7 = g_(`LINE',nu1)*g_(`LINE',nu2)*g_(`LINE',nu3)*g_(`LINE',nu4)*g_(`LINE',nu5)*g_(`LINE',nu6)*g_(`LINE',nu7);
*.sort


id g_(`LINE', uu1?) = counter*g_(`LINE', uu1);
.sort

*******************************************************************
*** the case of odd number of gamma matrices
*******************************************************************

* the case of odd number of gammas is difficult and it is not
* clear what the write procedure is. The implementation that follows
* does the following in such cases. It takes two gamma matrices
* and exchanges it it for (gam_i gam_j -> g_ij -i eps[i j k] gam_j).
* So it takes a tr( 2n+1 ) -> tr( 2n-1 ) + trace( 2n ) eps
* the trace (2n) is then performed in d dimensions. How the epsilon
* tensors are contracted is not specified here. This procedure
* is simple and not particularly sofisticated, but it is unclear if
* the resulting order(ep) terms are correct so take care.

#do m = 1,1
   if ( count(counter,1) == 1 ) discard;
   if ( count(counter,1) != multipleof(2) );
      redefine m "0";

      id once g_(`LINE',uu1?)*g_(`LINE',uu2?) =
            + d_(uu1, uu2) / counter^2
            - i_ * e_(uu1, uu2, uu3) * g_(`LINE',uu3) / counter;

      sum uu3;

   endif;

   .sort
   #if ( `m' == 0 )
      #message Warning: encountered trace with odd number of gammas
   #endif
#enddo

*** the case of even number of gamma matrices **********************
if ( count(counter,1) == multipleof(2) );
  multiply 1/2;
  tracen,`LINE';
endif;
.sort

id counter = 1;
.sort
#endprocedure

******************
* vim: syntax=form
******************
