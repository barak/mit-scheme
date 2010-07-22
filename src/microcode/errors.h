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

/* Error and termination code declarations. */

#ifndef SCM_ERRORS_H
#define SCM_ERRORS_H

/* All error and termination codes must be positive to allow
   primitives to return either an error code or a primitive flow
   control value (see const.h).  */

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
#define ERR_FASDUMP_OBJECT_TOO_LARGE		0x14
/* #define ERR_BAD_INTERRUPT_CODE		0x15 */
/* #define ERR_NO_ERRORS			0x16 */
#define ERR_FASL_FILE_TOO_BIG			0x17
#define ERR_FASL_FILE_BAD_DATA			0x18
/* #define ERR_IMPURIFY_OUT_OF_SPACE		0x19 */
/* #define ERR_WRITE_INTO_PURE_SPACE		0x1A */
/* #define ERR_LOSING_SPARE_HEAP		0x1B */
/* #define ERR_NO_HASH_TABLE			0x1C */
#define ERR_BAD_SET                             0x1D
#define ERR_ARG_1_FAILED_COERCION      		0x1E
#define ERR_ARG_2_FAILED_COERCION      		0x1F
#define ERR_OUT_OF_FILE_HANDLES			0x20
/* #define ERR_SHELL_DIED			0x21 */
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

/* If you add any error codes here, add them to the table below.  */

#define MAX_ERROR				0x3E

#define ERROR_NAME_TABLE						\
{									\
/* 0x00 */		"bad-error-code",				\
/* 0x01 */		"unbound-variable",				\
/* 0x02 */		"unassigned-variable",				\
/* 0x03 */		"undefined-procedure",				\
/* 0x04 */		"system-call",					\
/* 0x05 */		"error-with-argument",				\
/* 0x06 */		"bad-frame",					\
/* 0x07 */		"broken-compiled-variable",			\
/* 0x08 */		"undefined-user-type",				\
/* 0x09 */		"undefined-primitive-operation",		\
/* 0x0a */		"external-return",				\
/* 0x0b */		"execute-manifest-vector",			\
/* 0x0c */		"wrong-number-of-arguments",			\
/* 0x0d */		"wrong-type-argument-0",			\
/* 0x0e */		"wrong-type-argument-1",			\
/* 0x0f */		"wrong-type-argument-2",			\
/* 0x10 */		"bad-range-argument-0",				\
/* 0x11 */		"bad-range-argument-1",				\
/* 0x12 */		"bad-range-argument-2",				\
/* 0x13 */		"macro-binding",				\
/* 0x14 */		"fasdump-object-too-large",			\
/* 0x15 */		0,						\
/* 0x16 */		0,						\
/* 0x17 */		"fasl-file-too-big",				\
/* 0x18 */		"fasl-file-bad-data",				\
/* 0x19 */		0,						\
/* 0x1a */		0,						\
/* 0x1b */		0,						\
/* 0x1c */		0,						\
/* 0x1d */		"bad-assignment",				\
/* 0x1e */		"failed-arg-1-coercion",			\
/* 0x1f */		"failed-arg-2-coercion",			\
/* 0x20 */		"out-of-file-handles",				\
/* 0x21 */		0,						\
/* 0x22 */		"bad-range-argument-3",				\
/* 0x23 */		"bad-range-argument-4",				\
/* 0x24 */		"bad-range-argument-5",				\
/* 0x25 */		"bad-range-argument-6",				\
/* 0x26 */		"bad-range-argument-7",				\
/* 0x27 */		"bad-range-argument-8",				\
/* 0x28 */		"bad-range-argument-9",				\
/* 0x29 */		"wrong-type-argument-3",			\
/* 0x2a */		"wrong-type-argument-4",			\
/* 0x2b */		"wrong-type-argument-5",			\
/* 0x2c */		"wrong-type-argument-6",			\
/* 0x2d */		"wrong-type-argument-7",			\
/* 0x2e */		"wrong-type-argument-8",			\
/* 0x2f */		"wrong-type-argument-9",			\
/* 0x30 */		"inapplicable-continuation",			\
/* 0x31 */		"compiled-code-error",				\
/* 0x32 */		"floating-overflow",				\
/* 0x33 */		"unimplemented-primitive",			\
/* 0x34 */		"illegal-reference-trap",			\
/* 0x35 */		"broken-variable-cache",			\
/* 0x36 */		"wrong-arity-primitives",			\
/* 0x37 */		"io-error",					\
/* 0x38 */		"fasdump-environment",				\
/* 0x39 */		"fasload-band",					\
/* 0x3a */		"fasload-compiled-mismatch",			\
/* 0x3b */		"unknown-primitive-continuation",		\
/* 0x3c */		"illegal-continuation",				\
/* 0x3d */		"stack-has-slipped",				\
/* 0x3e */		"cannot-recurse"				\
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
/* #define TERM_				0x17 */
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
/* 0x00 */		"halt",						\
/* 0x01 */		"disk-restore",					\
/* 0x02 */		"broken-heart",					\
/* 0x03 */		"non-pointer-relocation",			\
/* 0x04 */		"bad-root",					\
/* 0x05 */		"non-existent-continuation",			\
/* 0x06 */		"bad-stack",					\
/* 0x07 */		"stack-overflow",				\
/* 0x08 */		"stack-allocation-failed",			\
/* 0x09 */		"no-error-handler",				\
/* 0x0a */		"no-interrupt-handler",				\
/* 0x0b */		"unimplemented-continuation",			\
/* 0x0c */		"exit",						\
/* 0x0d */		"bad-primitive-during-error",			\
/* 0x0e */		"eof",						\
/* 0x0f */		"bad-primitive",				\
/* 0x10 */		"termination-handler",				\
/* 0x11 */		"end-of-computation",				\
/* 0x12 */		"invalid-type-code",				\
/* 0x13 */		"compiler-death",				\
/* 0x14 */		"gc-out-of-space",				\
/* 0x15 */		"no-space",					\
/* 0x16 */		"signal",					\
/* 0x17 */		0,						\
/* 0x18 */		"save-and-exit",				\
/* 0x19 */		"trap",						\
/* 0x1a */		"bad-back-out"					\
}

#define TERM_MESSAGE_TABLE						\
{									\
/* 0x00 */		"Moriturus te saluto",				\
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
/* 0x17 */		0,						\
/* 0x18 */		"Halt requested by external source",		\
/* 0x19 */		"User requested termination after trap",	\
/* 0x1A */		"Backing out of non-primitive"			\
}

#endif /* SCM_ERRORS_H */
