/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/unxutl/Attic/cf-dist.h,v 1.2 1989/07/24 21:08:15 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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

#define PROC_TYPE_UNKNOWN 0
#define PROC_TYPE_68000 1
#define PROC_TYPE_68020 2
#define PROC_TYPE_HPPA 3	/* HP Precision Architecture */
#define PROC_TYPE_VAX 4

#include "s.h"
#include "m.h"

#ifndef PROC_TYPE
#define PROC_TYPE PROC_TYPE_UNKNOWN
#endif

/* Define HAVE_X_WINDOWS if you want to use the X window system.  */
#define HAVE_X_WINDOWS

/* Some useful compilation options:
   -DCOMPILE_HISTORY		turns on history recording mechanism
   -DENABLE_DEBUGGING_TOOLS	turns on microcode debugging tools
   -DUSE_STACKLETS		selects heap-allocated stack
   -DCOMPILE_FUTURES		turns on parallel processing features
   -DFUTURE_LOGGING		???
   -DCOMPILE_STEPPER		enables single-stepper (currently broken)
   */

#define C_SWITCH_FEATURES -DCOMPILE_HISTORY
