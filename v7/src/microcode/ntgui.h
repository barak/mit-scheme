/* -*-C-*-

$Id: ntgui.h,v 1.7 1997/01/01 22:57:26 cph Exp $

Copyright (c) 1993-97 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

#ifndef SCM_NTGUI_H
#define SCM_NTGUI_H

#define IDM_NEW            100
#define IDM_OPEN           101
#define IDM_SAVE           102
#define IDM_SAVEAS         103
#define IDM_PRINT          104
#define IDM_PRINTSETUP     105
#define IDM_EXIT           106
#define IDM_UNDO           200
#define IDM_CUT            201
#define IDM_COPY           202
#define IDM_PASTE          203
#define IDM_LINK           204
#define IDM_LINKS          205
#define IDM_HELPCONTENTS   300
#define IDM_HELPSEARCH     301
#define IDM_HELPHELP       302
#define IDM_ABOUT          303
#define IDM_EMERGENCYKILL  400

#ifndef CATATONIA_BLOCK_COUNTER
/* They must be contiguous, with counter being lower. */
# define CATATONIA_BLOCK_COUNTER 	0
# define CATATONIA_BLOCK_LIMIT   	(CATATONIA_BLOCK_COUNTER + 1)
# define CATATONIA_BLOCK_FLAG    	(CATATONIA_BLOCK_COUNTER + 2)
#endif

#define WM_CATATONIC (WM_USER)
#define WM_SCHEME_INTERRUPT (WM_USER + 1)

#endif /* SCM_NTGUI_H */
