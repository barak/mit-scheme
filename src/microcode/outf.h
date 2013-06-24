/* -*-C-*-

$Id: outf.h,v 1.5 2000/12/05 21:23:47 cph Exp $

Copyright (c) 1993, 1999, 2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
