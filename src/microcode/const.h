/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

/* Named constants used throughout the interpreter */

#define PI 3.1415926535

/* Assorted sizes used in various places */

/* Maximum # of chars in a file name.  */
#ifdef MAXPATHLEN
#  define FILE_NAME_LENGTH MAXPATHLEN
#else
#  define FILE_NAME_LENGTH 1024
#endif

/* Interning hash table */
#define OBARRAY_SIZE 32771

/* Cells between constant and stack before overflow occurs.  */
#ifndef STACK_GUARD_SIZE
#  define STACK_GUARD_SIZE 4096
#endif

/* Some versions of stdio define this. */
#ifndef _NFILE
#  define _NFILE 15
#endif

#define FILE_CHANNELS _NFILE

#define MAX_LIST_PRINT 10

#define ILLEGAL_PRIMITIVE -1

/* Primitive flow control codes: directs computation after processing
   a primitive application.  */

#define PRIM_DONE			-1
#define PRIM_DO_EXPRESSION		-2
#define PRIM_APPLY			-3
#define PRIM_INTERRUPT			-4
#define PRIM_NO_TRAP_EVAL		-5
#define PRIM_NO_TRAP_APPLY		-6
#define PRIM_POP_RETURN			-7
#define PRIM_TOUCH			-8
#define PRIM_APPLY_INTERRUPT		-9
#define PRIM_APPLY_ERROR		-10
#define PRIM_NO_TRAP_POP_RETURN		-11
#define PRIM_RETURN_TO_C		-12
#define PRIM_ABORT_TO_C			-13

#define ABORT_NAME_TABLE						\
{									\
  /* -1 */	"DONE",							\
  /* -2 */	"DO-EXPRESSION",					\
  /* -3 */	"APPLY",						\
  /* -4 */	"INTERRUPT",						\
  /* -5 */	"NO-TRAP-EVAL",						\
  /* -6 */	"NO-TRAP_APPLY",					\
  /* -7 */	"POP-RETURN",						\
  /* -8 */	"TOUCH",						\
  /* -9 */	"APPLY-INTERRUPT",					\
  /* -10 */	"REENTER",						\
  /* -11 */	"NO-TRAP-POP-RETURN",					\
  /* -12 */	"RETURN-TO-C",						\
  /* -13 */	"ABORT-TO-C"						\
}

/* Some numbers of parameters which mean something special */

#define LEXPR_PRIMITIVE_ARITY		-1
#define UNKNOWN_PRIMITIVE_ARITY		-2

/* Error case detection for precomputed constants */
/* VMS preprocessor does not like line continuations in conditionals */

#define Are_The_Constants_Incompatible					\
((TC_NULL != 0x00) || (TC_CONSTANT != 0x08) ||				\
 (TC_FIXNUM != 0x1A) || (TC_BROKEN_HEART != 0x22) || 			\
 (TC_CHARACTER_STRING != 0x1E))

/* The values used above are in sdata.h and types.h,
   check for consistency if the check below fails. */

#if Are_The_Constants_Incompatible
#include "Error: const.h and types.h disagree"
#endif

/* These are the only entries in Registers[] needed by the microcode.
   All other entries are used only by the compiled code interface. */

#define REGBLOCK_MEMTOP			0
#define REGBLOCK_INT_MASK		1
#define REGBLOCK_VAL			2
#define REGBLOCK_ENV			3
#define REGBLOCK_CC_TEMP		4	/* For use by compiler */
#define REGBLOCK_EXPR			5
#define REGBLOCK_RETURN			6
#define REGBLOCK_LEXPR_ACTUALS		7
#define REGBLOCK_PRIMITIVE		8
#define REGBLOCK_CLOSURE_FREE		9	/* For use by compiler */
#define REGBLOCK_CLOSURE_SPACE		10	/* For use by compiler */
#define REGBLOCK_STACK_GUARD		11
#define REGBLOCK_INT_CODE		12
#define REGBLOCK_REFLECT_TO_INTERFACE	13	/* For use by compiler */

#define REGBLOCK_MINIMUM_LENGTH		14
