/* -*-C-*-

$Id: scheme.h,v 9.39 2000/12/05 21:23:48 cph Exp $

Copyright (c) 1987-2000 Massachusetts Institute of Technology

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

/* General declarations for the SCode interpreter.  This
   file is INCLUDED by others and contains declarations only. */

/* Certain debuggers cannot really deal with variables in registers.
   When debugging, NO_REGISTERS can be defined. */

#ifndef SCM_SCHEME_H
#define SCM_SCHEME_H

#ifdef NO_REGISTERS
#define fast
#else
#define fast			register
#endif

#ifdef ENABLE_DEBUGGING_TOOLS
#define Consistency_Check	true
#define ENABLE_PRIMITIVE_PROFILING
#else
#define Consistency_Check	false
#ifdef ENABLE_PRIMITIVE_PROFILING
#undef ENABLE_PRIMITIVE_PROFILING
#endif
#endif

#define forward		extern	/* For forward references */

#include "config.h"

#include <stdio.h>
#ifdef STDC_HEADERS
#  include <stdlib.h>
#endif

#include "dstack.h"	/* Dynamic stack support package */
#include "obstack.h"	/* Obstack package */
#include "types.h"	/* Type code numbers */
#include "const.h"	/* Various named constants */
#include "object.h"	/* Scheme object representation */
#include "intrpt.h"	/* Interrupt processing macros */
#include "critsec.h"	/* Critical sections */
#include "gc.h"		/* Memory management related macros */
#include "scode.h"	/* Scheme scode representation */
#include "sdata.h"	/* Scheme user data representation */
#include "futures.h"	/* Support macros, etc. for FUTURE */
#include "errors.h"	/* Error code numbers */
#include "returns.h"	/* Return code numbers */
#include "fixobj.h"	/* Format of fixed objects vector */
#include "stack.h"	/* Macros for stack (stacklet) manipulation */
#include "interp.h"	/* Macros for interpreter */

#ifdef butterfly
#include "butterfly.h"
#endif

#include "outf.h"	/* Formatted output for errors */
#include "bkpt.h"	/* Shadows some defaults */
#include "default.h"	/* Defaults for various hooks. */
#include "extern.h"	/* External declarations */
#include "bignum.h"	/* Bignum declarations */
#include "prim.h"	/* Declarations for primitives. */
#include "float.h"	/* Floating-point parameters */
#if (FLT_RADIX != 2)
#include "error: floating point radix not 2!  Arithmetic won't work."
#endif

#endif /* SCM_SCHEME_H */
