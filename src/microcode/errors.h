/* -*-C-*-

$Id: errors.h,v 9.46 2003/02/14 18:28:18 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* Error and termination code declarations. */

#ifndef SCM_ERRORS_H
#define SCM_ERRORS_H

/* All error and termination codes must be positive
 * to allow primitives to return either an error code
 * or a primitive flow control value (see const.h)
 */

#define ERR_BAD_ERROR_CODE			0x00
#define ERR_UNBOUND_VARIABLE			0x01
#define ERR_UNASSIGNED_VARIABLE			0x02
#define ERR_INAPPLICABLE_OBJECT			0x03
#define ERR_IN_SYSTEM_CALL			0x04
#define ERR_WITH_ARGUMENT			0x05
#define ERR_BAD_FRAME				0x06
#define ERR_BROKEN_COMPILED_VARIABLE		0x07
#define ERR_UNDEFINED_USER_TYPE			0x08
#define ERR_UNDEFINED_PRIMITIVE			0x09
#define ERR_EXTERNAL_RETURN			0x0A
#define ERR_EXECUTE_MANIFEST_VECTOR		0x0B
#define ERR_WRONG_NUMBER_OF_ARGUMENTS		0x0C
#define ERR_ARG_1_WRONG_TYPE			0x0D
#define ERR_ARG_2_WRONG_TYPE			0x0E
#define ERR_ARG_3_WRONG_TYPE			0x0F
#define ERR_ARG_1_BAD_RANGE			0x10
#define ERR_ARG_2_BAD_RANGE			0x11
#define ERR_ARG_3_BAD_RANGE			0x12
#define ERR_MACRO_BINDING			0x13
/* #define ERR_FASDUMP_OVERFLOW			0x14 */
#define ERR_BAD_INTERRUPT_CODE			0x15 /* Not generated */
/* #define ERR_NO_ERRORS			0x16 */
#define ERR_FASL_FILE_TOO_BIG			0x17
#define ERR_FASL_FILE_BAD_DATA			0x18
#define ERR_IMPURIFY_OUT_OF_SPACE		0x19

/* The following do not exist in the 68000 version */
#define ERR_WRITE_INTO_PURE_SPACE		0x1A
/* #define ERR_LOSING_SPARE_HEAP		0x1B */
/* #define ERR_NO_HASH_TABLE			0x1C */
#define ERR_BAD_SET                             0x1D
#define ERR_ARG_1_FAILED_COERCION      		0x1E
#define ERR_ARG_2_FAILED_COERCION      		0x1F
#define ERR_OUT_OF_FILE_HANDLES			0x20
/* #define ERR_SHELL_DIED			0x21 */

/* Late additions to both 68000 and C world */
#define ERR_ARG_4_BAD_RANGE			0x22
#define ERR_ARG_5_BAD_RANGE			0x23
#define ERR_ARG_6_BAD_RANGE			0x24
#define ERR_ARG_7_BAD_RANGE			0x25
#define ERR_ARG_8_BAD_RANGE			0x26
#define ERR_ARG_9_BAD_RANGE			0x27
#define ERR_ARG_10_BAD_RANGE			0x28
#define ERR_ARG_4_WRONG_TYPE			0x29
#define ERR_ARG_5_WRONG_TYPE			0x2A
#define ERR_ARG_6_WRONG_TYPE			0x2B
#define ERR_ARG_7_WRONG_TYPE			0x2C
#define ERR_ARG_8_WRONG_TYPE			0x2D
#define ERR_ARG_9_WRONG_TYPE			0x2E
#define ERR_ARG_10_WRONG_TYPE			0x2F
#define ERR_INAPPLICABLE_CONTINUATION		0x30
#define ERR_COMPILED_CODE_ERROR			0x31
#define ERR_FLOATING_OVERFLOW			0x32
#define ERR_UNIMPLEMENTED_PRIMITIVE		0x33
#define ERR_ILLEGAL_REFERENCE_TRAP		0x34
#define ERR_BROKEN_VARIABLE_CACHE		0x35
#define ERR_WRONG_ARITY_PRIMITIVES		0x36
#define ERR_IO_ERROR				0x37
#define ERR_FASDUMP_ENVIRONMENT			0x38
#define ERR_FASLOAD_BAND			0x39
#define ERR_FASLOAD_COMPILED_MISMATCH		0x3A
#define ERR_UNKNOWN_PRIMITIVE_CONTINUATION	0x3B
#define ERR_ILLEGAL_CONTINUATION		0x3C
#define ERR_STACK_HAS_SLIPPED			0x3D
#define ERR_CANNOT_RECURSE			0x3E

/*
  If you add any error codes here, add them to
  the table below and to utabmd.scm as well.
 */

#define MAX_ERROR				0x3E

#define ERROR_NAME_TABLE						\
{									\
/* 0x00 */		"BAD-ERROR-CODE",				\
/* 0x01 */		"UNBOUND-VARIABLE",				\
/* 0x02 */		"UNASSIGNED-VARIABLE",				\
/* 0x03 */		"INAPPLICABLE-OBJECT",				\
/* 0x04 */		"OUT-OF-HASH-NUMBERS",				\
/* 0x05 */		"ENVIRONMENT-CHAIN-TOO-DEEP",			\
/* 0x06 */		"BAD-FRAME",					\
/* 0x07 */		"BROKEN-COMPILED-VARIABLE",			\
/* 0x08 */		"UNDEFINED-USER-TYPE",				\
/* 0x09 */		"UNDEFINED-PRIMITIVE",				\
/* 0x0A */		"EXTERNAL-RETURN",				\
/* 0x0B */		"EXECUTE-MANIFEST-VECTOR",			\
/* 0x0C */		"WRONG-NUMBER-OF-ARGUMENTS",			\
/* 0x0D */		"ARG-1-WRONG-TYPE",				\
/* 0x0E */		"ARG-2-WRONG-TYPE",				\
/* 0x0F */		"ARG-3-WRONG-TYPE",				\
/* 0x10 */		"ARG-1-BAD-RANGE",				\
/* 0x11 */		"ARG-2-BAD-RANGE",				\
/* 0x12 */		"ARG-3-BAD-RANGE",				\
/* 0x13 */		"BAD-COMBINATION",				\
/* 0x14 */		"FASDUMP-OVERFLOW",				\
/* 0x15 */		"BAD-INTERRUPT-CODE",				\
/* 0x16 */		"NO-ERRORS",					\
/* 0x17 */		"FASL-FILE-TOO-BIG",				\
/* 0x18 */		"FASL-FILE-BAD-DATA",				\
/* 0x19 */		"IMPURIFY-OUT-OF-SPACE",			\
/* 0x1A */		"WRITE-INTO-PURE-SPACE",			\
/* 0x1B */		"LOSING-SPARE-HEAP",				\
/* 0x1C */		"NO-HASH-TABLE",				\
/* 0x1D */		"BAD-SET",					\
/* 0x1E */		"ARG-1-FAILED-COERCION",			\
/* 0x1F */		"ARG-2-FAILED-COERCION",			\
/* 0x20 */		"OUT-OF-FILE-HANDLES",				\
/* 0x21 */		"SHELL-DIED",					\
/* 0x22 */		"ARG-4-BAD-RANGE",				\
/* 0x23 */		"ARG-5-BAD-RANGE",				\
/* 0x24 */		"ARG-6-BAD-RANGE",				\
/* 0x25 */		"ARG-7-BAD-RANGE",				\
/* 0x26 */		"ARG-8-BAD-RANGE",				\
/* 0x27 */		"ARG-9-BAD-RANGE",				\
/* 0x28 */		"ARG-10-BAD-RANGE",				\
/* 0x29 */		"ARG-4-WRONG-TYPE",				\
/* 0x2A */		"ARG-5-WRONG-TYPE",				\
/* 0x2B */		"ARG-6-WRONG-TYPE",				\
/* 0x2C */		"ARG-7-WRONG-TYPE",				\
/* 0x2D */		"ARG-8-WRONG-TYPE",				\
/* 0x2E */		"ARG-9-WRONG-TYPE",				\
/* 0x2F */		"ARG-10-WRONG-TYPE",				\
/* 0x30 */		"INAPPLICABLE-CONTINUATION",			\
/* 0x31 */		"COMPILED-CODE-ERROR",				\
/* 0x32 */		"FLOATING-OVERFLOW",				\
/* 0x33 */		"UNIMPLEMENTED-PRIMITIVE",			\
/* 0x34 */		"ILLEGAL-REFERENCE-TRAP",			\
/* 0x35 */		"BROKEN-VARIABLE-CACHE",			\
/* 0x36 */		"WRONG-ARITY-PRIMITIVES",			\
/* 0x37 */		"IO-ERROR",					\
/* 0x38 */		"FASDUMP-ENVIRONMENT",				\
/* 0x39 */		"FASLOAD-BAND",					\
/* 0x3A */		"FASLOAD-COMPILED-MISMATCH",			\
/* 0x3B */		"UNKNOWN-PRIMITIVE-CONTINUATION",		\
/* 0x3C */		"ILLEGAL-CONTINUATION",				\
/* 0x3D */		"STACK-HAS-SLIPPED",				\
/* 0x3E */		"CANNOT-RECURSE"				\
}

/* Termination codes: the interpreter halts on these */

#define TERM_HALT				0x00
#define TERM_DISK_RESTORE			0x01
#define TERM_BROKEN_HEART			0x02
#define TERM_NON_POINTER_RELOCATION		0x03
#define TERM_BAD_ROOT				0x04
#define TERM_NON_EXISTENT_CONTINUATION		0x05
#define TERM_BAD_STACK				0x06
#define TERM_STACK_OVERFLOW			0x07
#define TERM_STACK_ALLOCATION_FAILED		0x08
#define TERM_NO_ERROR_HANDLER			0x09
#define TERM_NO_INTERRUPT_HANDLER		0x0A
#define TERM_UNIMPLEMENTED_CONTINUATION		0x0B
#define TERM_EXIT				0x0C
#define TERM_BAD_PRIMITIVE_DURING_ERROR		0x0D
#define TERM_EOF				0x0E
#define TERM_BAD_PRIMITIVE			0x0F
#define TERM_TERM_HANDLER			0x10
#define TERM_END_OF_COMPUTATION			0x11
#define TERM_INVALID_TYPE_CODE                  0x12
#define TERM_COMPILER_DEATH			0x13
#define TERM_GC_OUT_OF_SPACE			0x14
#define TERM_NO_SPACE				0x15
#define TERM_SIGNAL				0x16
#define TERM_TOUCH				0x17
#define TERM_SAVE_AND_EXIT			0x18
#define TERM_TRAP				0x19
#define TERM_BAD_BACK_OUT			0x1a

/*
  If you add any termination codes here, add them to
  the tables below as well!
 */

#define MAX_TERMINATION				0x1a

#define TERM_NAME_TABLE							\
{									\
/* 0x00 */		"HALT",						\
/* 0x01 */		"DISK-RESTORE",					\
/* 0x02 */		"BROKEN-HEART",					\
/* 0x03 */		"NON-POINTER-RELOCATION",			\
/* 0x04 */		"BAD-ROOT",					\
/* 0x05 */		"NON-EXISTENT-CONTINUATION",			\
/* 0x06 */		"BAD-STACK",					\
/* 0x07 */		"STACK-OVERFLOW",				\
/* 0x08 */		"STACK-ALLOCATION-FAILED",			\
/* 0x09 */		"NO-ERROR-HANDLER",				\
/* 0x0A */		"NO-INTERRUPT-HANDLER",				\
/* 0x0B */		"UNIMPLEMENTED-CONTINUATION",			\
/* 0x0C */		"EXIT",						\
/* 0x0D */		"BAD-PRIMITIVE-DURING-ERROR",			\
/* 0x0E */		"EOF",						\
/* 0x0F */		"BAD-PRIMITIVE",				\
/* 0x10 */		"HANDLER",					\
/* 0x11 */		"END-OF-COMPUTATION",				\
/* 0x12 */		"INVALID-TYPE-CODE",				\
/* 0x13 */		"COMPILER-DEATH",				\
/* 0x14 */		"GC-OUT-OF-SPACE",				\
/* 0x15 */		"NO-SPACE",					\
/* 0x16 */		"SIGNAL",					\
/* 0x17 */		"TOUCH",					\
/* 0x18 */		"SAVE-AND-EXIT",				\
/* 0x19 */		"TERM_TRAP",					\
/* 0x1a */		"BAD_BACK_OUT"					\
}

#define TERM_MESSAGE_TABLE						\
{									\
/* 0x00 */		"Happy Happy Joy Joy",				\
/* 0x01 */		"Unrecoverable error while loading a band",	\
/* 0x02 */		"Broken heart encountered",			\
/* 0x03 */		"Non pointer relocation",			\
/* 0x04 */		"Cannot restore control state from band",	\
/* 0x05 */		"Nonexistent return code",			\
/* 0x06 */		"Control stack messed up",			\
/* 0x07 */		"Stack overflow: Maximum recursion depth exceeded", \
/* 0x08 */		"Not enough space for stack!",			\
/* 0x09 */		"No error handler",				\
/* 0x0A */		"No interrupt handler",				\
/* 0x0B */		"Unimplemented return code",			\
/* 0x0C */		"Inconsistency detected",			\
/* 0x0D */		"Error during unknown primitive",		\
/* 0x0E */		"End of input stream reached",			\
/* 0x0F */		"Bad primitive invoked",			\
/* 0x10 */		"Termination handler returned",			\
/* 0x11 */		"End of computation",				\
/* 0x12 */		"Unknown type encountered",			\
/* 0x13 */		"Mismatch between compiled code and compiled code support", \
/* 0x14 */		"Out of space after garbage collection",	\
/* 0x15 */		"Out of memory: Available memory exceeded",	\
/* 0x16 */		"Unhandled signal received",			\
/* 0x17 */		"Touch without futures support",		\
/* 0x18 */		"Halt requested by external source",		\
/* 0x19 */		"User requested termination after trap",	\
/* 0x1a */		"Backing out of non-primitive"			\
}

#endif /* SCM_ERRORS_H */
