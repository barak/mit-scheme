/* -*-C-*-

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/const.h,v 9.34 1989/08/28 18:28:42 cph Exp $
 *
 * Named constants used throughout the interpreter
 *
 */

#if (CHAR_SIZE != 8)
#define MAX_CHAR		((1<<CHAR_SIZE)-1)
#else
#define MAX_CHAR		0xFF
#endif

#define PI			3.1415926535
#define STACK_FRAME_HEADER	1

/* Precomputed typed pointers */
#ifdef b32			/* 32 bit word */

#if (TYPE_CODE_LENGTH == 8)
#define SHARP_F			0x00000000
#define SHARP_T			0x08000000
#define UNSPECIFIC		0x08000001
#define FIXNUM_ZERO		0x1A000000
#define BROKEN_HEART_ZERO	0x22000000
#endif /* (TYPE_CODE_LENGTH == 8) */

#if (TYPE_CODE_LENGTH == 6)
#define SHARP_F			0x00000000
#define SHARP_T			0x20000000
#define UNSPECIFIC		0x20000001
#define FIXNUM_ZERO		0x68000000
#define BROKEN_HEART_ZERO	0x88000000
#endif /* (TYPE_CODE_LENGTH == 6) */

#endif /* b32 */

#ifndef SHARP_F			/* Safe version */
#define SHARP_F			Make_Non_Pointer(TC_NULL, 0)
#define SHARP_T			Make_Non_Pointer(TC_TRUE, 0)
#define UNSPECIFIC		Make_Non_Pointer(TC_TRUE, 1)
#define FIXNUM_ZERO		Make_Non_Pointer(TC_FIXNUM, 0)
#define BROKEN_HEART_ZERO	Make_Non_Pointer(TC_BROKEN_HEART, 0)
#endif /* SHARP_F */

#define EMPTY_LIST SHARP_F
#define NIL SHARP_F
#define TRUTH SHARP_T
#define NOT_THERE 		-1	/* Command line parser */

/* Assorted sizes used in various places */

#ifdef MAXPATHLEN
#define FILE_NAME_LENGTH	MAXPATHLEN
#else
#define FILE_NAME_LENGTH	1024   	/* Max. chars. in a file name */
#endif

#define OBARRAY_SIZE		3001	/* Interning hash table */

#ifndef STACK_GUARD_SIZE
#define STACK_GUARD_SIZE	4096	/* Cells between constant and
					   stack before overflow
					   occurs */
#endif

/* Some versions of stdio define this. */
#ifndef _NFILE
#define _NFILE		15
#endif

#define FILE_CHANNELS		_NFILE

#define MAX_LIST_PRINT		10

#define ILLEGAL_PRIMITIVE	-1

/* Last immediate reference trap. */
				    
#define TRAP_MAX_IMMEDIATE	9

/* For headers in pure / constant area */

#define END_OF_BLOCK		TC_FIXNUM
#define CONSTANT_PART		TC_TRUE
#define PURE_PART		TC_FALSE

/* Primitive flow control codes: directs computation after
 * processing a primitive application.
 */

#define PRIM_DONE			-1
#define PRIM_DO_EXPRESSION		-2
#define PRIM_APPLY			-3
#define PRIM_INTERRUPT			-4
#define PRIM_NO_TRAP_EVAL		-5
#define PRIM_NO_TRAP_APPLY		-6
#define PRIM_POP_RETURN			-7
#define PRIM_TOUCH			-8
#define PRIM_APPLY_INTERRUPT		-9
#define PRIM_REENTER			-10

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
  /* -10 */	"REENTER"						\
}

/* Some numbers of parameters which mean something special */

#define LEXPR_PRIMITIVE_ARITY		-1
#define UNKNOWN_PRIMITIVE_ARITY		-2

/* Error case detection for precomputed constants */
/* VMS preprocessor does not like line continuations in conditionals */

#define Are_The_Constants_Incompatible					\
((TC_NULL != 0x00) || (TC_TRUE != 0x08) ||				\
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
#define REGBLOCK_STACKGUARD		1
#define REGBLOCK_VAL			2
#define REGBLOCK_ENV			3
#define REGBLOCK_TEMP			4
#define REGBLOCK_EXPR			5
#define REGBLOCK_RETURN			6
#define REGBLOCK_LEXPR_ACTUALS		7
#define REGBLOCK_PRIMITIVE		8
#define REGBLOCK_MINIMUM_LENGTH		9

/* Codes specifying how to start scheme at boot time. */

#define BOOT_FASLOAD		0
#define BOOT_LOAD_BAND		1
#define BOOT_GET_WORK		2
