/* -*-C-*-

Copyright (c) 1986 Massachusetts Institute of Technology

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

/* File: scheme.h
 *
 * General declarations for the SCode interpreter.  This
 * file is INCLUDED by others and contains declarations only.
 */

/* "fast" is a register declaration if we aren't debugging code */

#ifdef ENABLE_DEBUGGING_TOOLS
#define Consistency_Check	true
#define fast
#else
#define Consistency_Check	false
#define fast			register
#endif

#ifdef noquick
#define quick
#else
#define quick fast
#endif

#ifdef COMPILE_STEPPER
#define Microcode_Does_Stepping	true
#else
#define Microcode_Does_Stepping	false
#endif

#define forward		extern	/* For forward references */

#include "config.h"	/* Machine and OS configuration info */
#include "bkpt.h"	/* May shadow some defaults */
#include "object.h"	/* Scheme Object Representation */
#include "scode.h"	/* Scheme SCode Representation */
#include "sdata.h"	/* Scheme User Data Representation */
#include "gc.h"		/* Garbage Collector related macros */
#include "history.h"	/* History maintenance */
#include "interpret.h"	/* Macros for interpreter */
#include "stack.h"	/* Macros for stack (stacklet) manipulation */
#include "futures.h"	/* Support macros, etc. for FUTURE */
#include "types.h"	/* Type code numbers */
#include "errors.h"	/* Error code numbers */
#include "returns.h"	/* Return code numbers */
#include "const.h"	/* Various named constants */
#include "fixobj.h"	/* Format of fixed objects vector */
#ifdef RENAME
#include "rename.c"	/* Rename of identifiers for some compilers */
#endif
#include <setjmp.h>
#include <stdio.h>

#ifdef butterfly
#include "butterfly.h"
#endif

#include "default.h"	/* Defaults for various hooks. */
#include "extern.h"	/* External declarations */
