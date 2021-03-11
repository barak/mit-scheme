/* -*-C-*-

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* Named constants used throughout the interpreter */

#if (CHAR_BIT != 8)
#define MAX_CHAR		((1<<CHAR_BIT)-1)
#else
#define MAX_CHAR		0xFF
#endif

#define PI			3.1415926535
#define STACK_FRAME_HEADER	1

/* Precomputed typed pointers */
#ifdef b32			/* 32 bit word */

#if (TYPE_CODE_LENGTH == 8)
#include "error: update the constants here to reflect the general #defines"
#define SHARP_F			0x08000000
#define SHARP_T			0x08000001
#define UNSPECIFIC		0x08000002
#define EMPTY_LIST_VALUE        0x08000003
#define FIXNUM_ZERO		0x00000000
#define BROKEN_HEART_ZERO	0x11000000
#endif /* (TYPE_CODE_LENGTH == 8) */

#if (TYPE_CODE_LENGTH == 6)
#ifndef SHARP_F
#define SHARP_F			0x22000000
#endif
#define SHARP_T			0x23000000
#define UNSPECIFIC		0x23800000
#define EMPTY_LIST_VALUE        0x22800000
#define FIXNUM_ZERO		0x00000000
#define BROKEN_HEART_ZERO	0x44000000
#endif /* (TYPE_CODE_LENGTH == 6) */

#endif /* b32 */

#ifndef SHARP_T			/* Safe version */
/* The special constants are all of the form TC_CONSTANT | #b1xx
   to allows smaller datum values to be integer-like
   xxx values:  100=#F, 101=(), 110=#T, 111=unspecific
 */

#define SHARP_F		       MAKE_OBJECT (TC_CONSTANT, 0x4<<(DATUM_LENGTH-3))
#define SHARP_T		       MAKE_OBJECT (TC_CONSTANT, 0x6<<(DATUM_LENGTH-3))
#define UNSPECIFIC	       MAKE_OBJECT (TC_CONSTANT, 0x7<<(DATUM_LENGTH-3))
#define EMPTY_LIST_VALUE       MAKE_OBJECT (TC_CONSTANT, 0x5<<(DATUM_LENGTH-3))
#define FIXNUM_ZERO	       MAKE_OBJECT (TC_POSITIVE_FIXNUM, 0)
#define BROKEN_HEART_ZERO      MAKE_OBJECT (TC_BROKEN_HEART, 0)
#endif /* SHARP_F */

#define EMPTY_LIST              (Registers[REGBLOCK_EMPTY_LIST])

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

/*#define END_OF_BLOCK		TC_POSITIVE_FIXNUM*/
/*#define CONSTANT_PART		TC_TRUE*/
/*#define PURE_PART		TC_FALSE*/

#define END_OF_BLOCK		TC_POSITIVE_FIXNUM
#define CONSTANT_PART		TC_CONSTANT
#if (TC_CONSTANT==TC_FALSE)
#  define PURE_PART		TC_CHARACTER
#else
#  define PURE_PART             TC_FALSE
#endif

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
#define PRIM_NO_TRAP_POP_RETURN		-11

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
  /* -11 */	"NO-TRAP-POP-RETURN"					\
}

/* Some numbers of parameters which mean something special */

#define LEXPR_PRIMITIVE_ARITY		-1
#define UNKNOWN_PRIMITIVE_ARITY		-2

/* Error case detection for precomputed constants */
/* VMS preprocessor does not like line continuations in conditionals */

#define Are_The_Constants_Incompatible					\
( (TC_NULL != 0x05) || (TC_CONSTANT != 0x08) ||				\
  (TC_POSITIVE_FIXNUM != 0x00) ||                                       \
  (TC_NEGATIVE_FIXNUM != 0x3F) || (TC_BROKEN_HEART != 0x11))

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
#define REGBLOCK_COMPILER_TEMP		4	/* For use by compiler */
#define REGBLOCK_EXPR			5
#define REGBLOCK_RETURN			6
#define REGBLOCK_LEXPR_ACTUALS		7
#define REGBLOCK_PRIMITIVE		8
#define REGBLOCK_CLOSURE_FREE		9	/* For use by compiler */
#define REGBLOCK_CLOSURE_SPACE		10	/* For use by compiler */
#define REGBLOCK_STACK_GUARD		11
#define REGBLOCK_INT_CODE		12
#define REGBLOCK_REFLECT_TO_INTERFACE	13	/* For use by compiler */
#define REGBLOCK_EMPTY_LIST             14      /* allows compatbilty */

#define REGBLOCK_MINIMUM_LENGTH		15

/* Codes specifying how to start scheme at boot time. */

#define BOOT_FASLOAD		0
#define BOOT_LOAD_BAND		1
#define BOOT_GET_WORK		2
#define BOOT_EXECUTE		3
