/* -*-C-*-

$Id: outf.h,v 1.6 2002/11/20 19:46:12 cph Exp $

Copyright (c) 1993, 1999, 2000 Massachusetts Institute of Technology

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

#ifndef SCM_OUTF_H
#define SCM_OUTF_H

#include <stdio.h>
#include "config.h"

typedef struct __outf_channel_type_placeholder *outf_channel;

extern void EXFUN (outf, (outf_channel chan, CONST char *format  DOTS));
extern void EXFUN (outf_console, (CONST char *format  DOTS));
extern void EXFUN (outf_error, (CONST char *format  DOTS));
extern void EXFUN (outf_fatal, (CONST char *format  DOTS));

extern void EXFUN (outf_flush, (outf_channel chan));
extern void EXFUN (outf_flush_console, (void));
extern void EXFUN (outf_flush_error, (void));
extern void EXFUN (outf_flush_fatal, (void));

#define  console_output ((outf_channel)-1)
#define  error_output ((outf_channel)-2)
#define  fatal_output ((outf_channel)-3)

#endif /* SCM_OUTF_H */
