/* -*-C-*-

$Id: outf.h,v 1.3 1993/06/28 02:28:34 cph Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

#ifndef SCM_OUTF_H
#define SCM_OUTF_H

#include <stdio.h>
#include "ansidecl.h"

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
