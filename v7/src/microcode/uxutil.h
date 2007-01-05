/* -*-C-*-

$Id: uxutil.h,v 1.6 2007/01/05 15:33:08 cph Exp $

Copyright (c) 1990, 1999 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

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
