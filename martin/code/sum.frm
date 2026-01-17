#-
off statistics;

*******************************************************************
* Copyright (C) 2009-2020 Joachim Brod, Emmanuel Stamou
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS 
* file in the main MaRTIn directory.
*******************************************************************


#call printMessage(Summing the `NUMDIA' diagram(s) in `RESPATH')

* load diagrams
#do i = 1, `NUMDIA'
  load `RESPATH'/dia`i'.sav ;
#enddo

* the name of the expression
#call printMessage(The name of the expression is: diasum)

* define and sum
g diasum =
  #do i = 1, `NUMDIA'
    + dia`i'
  #enddo
  ;
.sort

*******************************************************************
* Save FORM expression                                            *
*******************************************************************

.store
off statistics;
#include maindeclare

#call printMessage(Writing Form file `FORMRESULTFILE')
save `FORMRESULTFILE' diasum;

*******************************************************************
* Save MATHEMATICA expression                                     *
*******************************************************************

* the following things need to be redeclared because they appear in
* the format_mathematica file.

g diaSUM = diasum;


#include format_mathematica ;

.sort

#call printMessage(Writing Mathematica file `MATHRESULTFILE')
#write <`MATHRESULTFILE'> "%sdiasum\t%s\t%s%E%s\n%" ,"(* ","*)","{",diaSUM,"}"

*print +s;
.end
