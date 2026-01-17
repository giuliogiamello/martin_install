#procedure printMessage(STR)

*******************************************************************
* Copyright (C) 2009-2023 Joachim Brod, Emmanuel Stamou, 
*                         Tom Steudtner
*
* This file is part of MaRTIn.
*
* MaRTIn is lisenced under GPLv3. For further details see the AUTHORS
* file in the main MaRTIn directory.
*******************************************************************


	#ifdef `PRINT'
		#message `DIA': `STR'
	#endif

#endprocedure