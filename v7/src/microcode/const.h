/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/const.h,v 9.21 1987/01/22 14:22:44 jinx Exp $
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
#ifndef b32			/* Safe version */

#define NIL			Make_Non_Pointer(TC_NULL, 0)
#define TRUTH			Make_Non_Pointer(TC_TRUE, 0)
#define UNASSIGNED_OBJECT 	Make_Non_Pointer(TC_UNASSIGNED, UNASSIGNED)
#define UNBOUND_OBJECT		Make_Non_Pointer(TC_UNASSIGNED, UNBOUND)
#define UNCOMPILED_VARIABLE	Make_Non_Pointer(UNCOMPILED_REF, 0)
#define FIXNUM_0		Make_Non_Pointer(TC_FIXNUM, 0)
#define LOCAL_REF_0		Make_Non_Pointer(LOCAL_REF, 0)
#define BROKEN_HEART_0		Make_Non_Pointer(TC_BROKEN_HEART, 0)
#define STRING_0		Make_Non_Pointer(TC_CHARACTER_STRING, 0)

#else				/* 32 bit word */
#define NIL			0x00000000
#define TRUTH			0x08000000
#define UNASSIGNED_OBJECT 	0x32000000
#define UNBOUND_OBJECT		0x32000001
#define UNCOMPILED_VARIABLE	0x08000000
#define FIXNUM_0		0x1A000000
#define LOCAL_REF_0		0x00000000
#define BROKEN_HEART_0		0x22000000
#define STRING_0		0x1E000000
#endif				/* b32 */

/* Some names for flag values */

#define SET_IT			0	/* Lookup */
#define CLEAR_IT		1
#define READ_IT			2
#define TEST_IT			3

#define FOUND_SLOT              1	/* Slot lookup */
#define NO_SLOT                 2
#define FOUND_UNBOUND           4

#define NOT_THERE 		-1	/* Command line parser */

/* Assorted sizes used in various places */

#ifdef MAXPATHLEN
#define FILE_NAME_LENGTH	MAXPATHLEN
#else
#define FILE_NAME_LENGTH	1024   	/* Max. chars. in a file name */
#endif

#define OBARRAY_SIZE		3001	/* Interning hash table */
#define STACK_GUARD_SIZE	4096	/* Cells between constant and
					   stack before overflow
					   occurs */
#define FILE_CHANNELS		15
#define MAX_LIST_PRINT		10

#define ILLEGAL_PRIMITIVE	-1

/* Hashing algorithm for interning */

#define MAX_HASH_CHARS		5
#define LENGTH_MULTIPLIER	5
#define SHIFT_AMOUNT		2

/* For looking up variable definitions */

#define UNCOMPILED_REF		TC_TRUE
#define GLOBAL_REF		TC_UNINTERNED_SYMBOL
#define FORMAL_REF		TC_FIXNUM
#define AUX_REF			TC_ENVIRONMENT
#define LOCAL_REF		TC_NULL
/* LOCAL_REF must be 0 in order for code in interpret.c to work fast */

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

/* Interrupt bits -- scanned from LSB (1) to MSB (16) */

#define INT_Stack_Overflow	1	/* Local interrupt */
#define INT_Global_GC		2
#define INT_GC			4	/* Local interrupt */
#define INT_Global_1		8
#define INT_Character		16	/* Local interrupt */
#define INT_Global_2		32
#define INT_Timer		64	/* Local interrupt */
#define INT_Global_3		128
#define INT_Global_Mask		\
  (INT_Global_GC | INT_Global_1 | INT_Global_2 | INT_Global_3)
#define Global_GC_Level		1
#define Global_1_Level		3
#define Global_2_Level		5
#define Global_3_Level		7
#define MAX_INTERRUPT_NUMBER	7

#define INT_Mask		((1<<(MAX_INTERRUPT_NUMBER+1))-1)

/* Error case detection for precomputed constants */
/* VMS preprocessor does not like line continuations in conditionals */

#define Are_The_Constants_Incompatible					\
((TC_NULL != 0x00) || (TC_TRUE != 0x08) || (TC_UNASSIGNED != 0x32) ||	\
 (UNASSIGNED != 0) || (UNBOUND != 1) || (UNCOMPILED_REF != 0x08) ||	\
 (TC_FIXNUM != 0x1A) || (TC_BROKEN_HEART != 0x22) || 			\
 (TC_CHARACTER_STRING != 0x1E) || (LOCAL_REF != 0x00))

/* The values used above are in sdata.h and types.h,
   check for consistency if the check below fails. */

#if Are_The_Constants_Incompatible
#include "Error: disagreement in const.h"
#endif 

/* These are the only entries in Registers[] needed by the microcode.
   All other entries are used only by the compiled code interface. */

#define REGBLOCK_MEMTOP 0
#define REGBLOCK_STACKGUARD 1
#define REGBLOCK_MINIMUM_LENGTH 2
