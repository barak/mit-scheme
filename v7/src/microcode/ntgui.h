/* -*-C-*-

$Id: ntgui.h,v 1.9 2002/11/20 19:46:10 cph Exp $

Copyright (c) 1993-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

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
