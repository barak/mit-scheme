/* -*-C-*-

$Id: uxutil.h,v 1.4 2002/11/20 19:46:15 cph Exp $

Copyright (c) 1990, 1999 Massachusetts Institute of Technology

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

#ifndef SCM_UXUTIL_H
#define SCM_UXUTIL_H

#include "os.h"

extern CONST char * EXFUN (char_description, (unsigned char c, int long_p));
extern void EXFUN (userio_buffered_input, (void));
extern char EXFUN (userio_read_char, (void));
extern char EXFUN (userio_read_char_raw, (void));
extern char EXFUN
  (userio_choose_option,
   (CONST char * herald, CONST char * prompt, CONST char ** choices));
extern int EXFUN (userio_confirm, (CONST char * prompt));

#endif /* SCM_UXUTIL_H */
