/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/unxutl/Attic/cf-dist.h,v 1.10 1990/03/08 19:26:43 cph Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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
#define PROC_TYPE_MIPS 5
#define PROC_TYPE_NS32K 6
#define PROC_TYPE_HCX 7		/* Harris HCX */
#define PROC_TYPE_IBM032 8	/* IBM RT */
#define PROC_TYPE_SPARC 9

/* Define this macro to use a non-standard compiler.
   It must be defined before including the m/ and s/ files because
   they may be conditionalized on it. */
/* #define ALTERNATE_CC gcc */

/* Define this macro to use a non-standard assembler. */
/* #define ALTERNATE_AS gashp */

#include "s.h"
#include "m.h"

#ifndef PROC_TYPE
#define PROC_TYPE PROC_TYPE_UNKNOWN
#endif

/* Define HAVE_X_WINDOWS if you want to use the X window system.  */
#define HAVE_X_WINDOWS

/* Define HAVE_STARBASE_GRAPHICS if you want Starbase graphics support.
   This is specific to HP-UX. */
/* #define HAVE_STARBASE_GRAPHICS */
/* #define STARBASE_DEVICE_DRIVERS -ldd300h -ldd98700 -ldd98710 -ldd98556 */

/* Some useful compilation options:
   -DCOMPILE_HISTORY		turns on history recording mechanism
   -DENABLE_DEBUGGING_TOOLS	turns on microcode debugging tools
   -DUSE_STACKLETS		selects heap-allocated stack
   -DCOMPILE_FUTURES		turns on parallel processing features
   -DFUTURE_LOGGING		???
   -DCOMPILE_STEPPER		enables single-stepper (currently broken)
   -DENABLE_SUBPROCESSES	enables creation of and communication with
   				subprocesses
   */

#define C_SWITCH_FEATURES -DCOMPILE_HISTORY

/* The following two switches are mutually exclusive for most C compilers.
   An exception is the GNU C compiler. */

/* If defined, this prevents the C compiler from running its optimizer. */
/* #define SUPPRESS_C_OPTIMIZER */

/* If defined, this prevents the C compiler from
   generating debugging information. */
#define SUPPRESS_C_DEBUGGING
