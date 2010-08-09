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

/* Return codes.  These are placed in GET_RET when an
   interpreter operation needs to operate in several phases. */

#define RC_END_OF_COMPUTATION		0x00
#define RC_JOIN_STACKLETS		0x01
/* unused				0x02 */
#define RC_INTERNAL_APPLY		0x03
/* unused			 	0x04 */
#define RC_RESTORE_HISTORY 		0x05
#define RC_INVOKE_STACK_THREAD 		0x06
/* unused		 		0x07 */
#define RC_EXECUTE_ASSIGNMENT_FINISH	0x08
#define RC_EXECUTE_DEFINITION_FINISH	0x09
#define RC_EXECUTE_ACCESS_FINISH	0x0A
#define RC_EXECUTE_IN_PACKAGE_CONTINUE  0x0B
#define RC_SEQ_2_DO_2			0x0C
#define RC_SEQ_3_DO_2			0x0D
#define RC_SEQ_3_DO_3			0x0E
#define RC_CONDITIONAL_DECIDE		0x0F
#define RC_DISJUNCTION_DECIDE		0x10
#define RC_COMB_1_PROCEDURE		0x11
#define RC_COMB_APPLY_FUNCTION		0x12
#define RC_COMB_2_FIRST_OPERAND		0x13
#define RC_COMB_2_PROCEDURE		0x14
#define RC_COMB_SAVE_VALUE		0x15
#define RC_PCOMB1_APPLY			0x16
#define RC_PCOMB2_DO_1			0x17
#define RC_PCOMB2_APPLY			0x18
#define RC_PCOMB3_DO_2			0x19
#define RC_PCOMB3_DO_1			0x1A
#define RC_PCOMB3_APPLY			0x1B
#define RC_SNAP_NEED_THUNK		0x1C
#define RC_REENTER_COMPILED_CODE 	0x1D
/* unused				0x1E */
/* unused			 	0x1F */
#define RC_NORMAL_GC_DONE	 	0x20
/* unused	 			0x21 */
#define RC_PURIFY_GC_1			0x22
#define RC_PURIFY_GC_2			0x23
/* unused	 			0x24 through 0x28 */
#define RC_POP_FROM_COMPILED_CODE 	0x29
#define RC_RETURN_TRAP_POINT		0x2A
/* unused				0x2B */
/* unused				0x2C */
/* unused				0x2D */
#define RC_RESTORE_VALUE		0x2E
#define RC_RESTORE_DONT_COPY_HISTORY    0x2F
/* unused				0x30 through 0x3F */
#define RC_POP_RETURN_ERROR		0x40
#define RC_EVAL_ERROR			0x41
#define RC_STACK_MARKER			0x42
#define RC_COMP_INTERRUPT_RESTART	0x43
/* unused				0x44 */
#define RC_RESTORE_INT_MASK		0x45
#define RC_HALT				0x46
/* unused				0x47 */
#define RC_REPEAT_DISPATCH		0x48
#define RC_GC_CHECK			0x49
/* unused				0x4A through 0x52 */
#define RC_COMP_LOOKUP_TRAP_RESTART  	0x53
#define RC_COMP_ASSIGNMENT_TRAP_RESTART 0x54
/* unused				0x55 */
#define RC_COMP_OP_REF_TRAP_RESTART	0x56
#define RC_COMP_CACHE_REF_APPLY_RESTART 0x57
#define RC_COMP_SAFE_REF_TRAP_RESTART   0x58
#define RC_COMP_UNASSIGNED_TRAP_RESTART 0x59
/* unused				0x5A */
#define RC_COMP_LINK_CACHES_RESTART	0x5B
#define RC_HARDWARE_TRAP		0x5C
#define RC_INTERNAL_APPLY_VAL		0x5D
#define RC_COMP_ERROR_RESTART		0x5E
#define RC_PRIMITIVE_CONTINUE		0x5F

/* When adding return codes, add them to the table below as well! */

#define MAX_RETURN_CODE			0x5F

#define RETURN_NAME_TABLE						\
{									\
/* 0x00 */		"non-existent-continuation",			\
/* 0x01 */		"join-stacklets",				\
/* 0x02 */		0,						\
/* 0x03 */		"internal-apply",				\
/* 0x04 */		0,						\
/* 0x05 */		"restore-history",				\
/* 0x06 */		"invoke-stack-thread",				\
/* 0x07 */		0,						\
/* 0x08 */		"assignment-continue",				\
/* 0x09 */		"definition-continue",				\
/* 0x0a */		"access-continue",				\
/* 0x0b */		"in-package-continue",				\
/* 0x0c */		"sequence-2-second",				\
/* 0x0d */		"sequence-3-second",				\
/* 0x0e */		"sequence-3-third",				\
/* 0x0f */		"conditional-decide",				\
/* 0x10 */		"disjunction-decide",				\
/* 0x11 */		"combination-1-procedure",			\
/* 0x12 */		"combination-apply",				\
/* 0x13 */		"combination-2-first-operand",			\
/* 0x14 */		"combination-2-procedure",			\
/* 0x15 */		"combination-save-value",			\
/* 0x16 */		"primitive-combination-1-apply",		\
/* 0x17 */		"primitive-combination-2-first-operand",	\
/* 0x18 */		"primitive-combination-2-apply",		\
/* 0x19 */		"primitive-combination-3-second-operand",	\
/* 0x1a */		"primitive-combination-3-first-operand",	\
/* 0x1b */		"primitive-combination-3-apply",		\
/* 0x1c */		"force-snap-thunk",				\
/* 0x1d */		"reenter-compiled-code",			\
/* 0x1e */		0,						\
/* 0x1f */		0,						\
/* 0x20 */		"normal-garbage-collect-done",			\
/* 0x21 */		0,						\
/* 0x22 */		"purify-after-first-gc",			\
/* 0x23 */		"purify-after-second-gc",			\
/* 0x24 */		0,						\
/* 0x25 */		0,						\
/* 0x26 */		0,						\
/* 0x27 */		0,						\
/* 0x28 */		0,						\
/* 0x29 */		"pop-from-compiled-code",			\
/* 0x2a */		"return-trap-point",				\
/* 0x2b */		0,						\
/* 0x2c */		0,						\
/* 0x2d */		0,						\
/* 0x2e */		"restore-value",				\
/* 0x2f */		"restore-dont-copy-history",			\
/* 0x30 */		0,						\
/* 0x31 */		0,						\
/* 0x32 */		0,						\
/* 0x33 */		0,						\
/* 0x34 */		0,						\
/* 0x35 */		0,						\
/* 0x36 */		0,						\
/* 0x37 */		0,						\
/* 0x38 */		0,						\
/* 0x39 */		0,						\
/* 0x3a */		0,						\
/* 0x3b */		0,						\
/* 0x3c */		0,						\
/* 0x3d */		0,						\
/* 0x3e */		0,						\
/* 0x3f */		0,						\
/* 0x40 */		"pop-return-error",				\
/* 0x41 */		"eval-error",					\
/* 0x42 */		"stack-marker",					\
/* 0x43 */		"compiler-interrupt-restart",			\
/* 0x44 */		0,						\
/* 0x45 */		"restore-interrupt-mask",			\
/* 0x46 */		"halt",						\
/* 0x47 */		0,						\
/* 0x48 */		"repeat-dispatch",				\
/* 0x49 */		"gc-check",					\
/* 0x4a */		0,						\
/* 0x4b */		0,						\
/* 0x4c */		0,						\
/* 0x4d */		0,						\
/* 0x4e */		0,						\
/* 0x4f */		0,						\
/* 0x50 */		0,						\
/* 0x51 */		0,						\
/* 0x52 */		0,						\
/* 0x53 */		"compiler-reference-trap-restart",		\
/* 0x54 */		"compiler-assignment-trap-restart",		\
/* 0x55 */		0,						\
/* 0x56 */		"compiler-operator-lookup-trap-restart",	\
/* 0x57 */		"compiler-lookup-apply-trap-restart",		\
/* 0x58 */		"compiler-safe-reference-trap-restart",		\
/* 0x59 */		"compiler-unassigned?-trap-restart",		\
/* 0x5a */		0,						\
/* 0x5b */		"compiler-link-caches-restart",			\
/* 0x5c */		"hardware-trap",				\
/* 0x5d */		"internal-apply-val",				\
/* 0x5e */		"compiler-error-restart",			\
/* 0x5f */		"primitive-continue"				\
}
