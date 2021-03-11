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

/* Return codes.  These are placed in Return when an
   interpreter operation needs to operate in several phases. */

#define RC_END_OF_COMPUTATION		0x00
/* formerly RC_RESTORE_CONTROL_POINT	0x01 */
#define RC_JOIN_STACKLETS		0x01
#define RC_RESTORE_CONTINUATION		0x02 /* Used for 68000 */
#define RC_INTERNAL_APPLY		0x03
#define RC_BAD_INTERRUPT_CONTINUE 	0x04 /* Used for 68000 */
#define RC_RESTORE_HISTORY 		0x05
#define RC_INVOKE_STACK_THREAD 		0x06
#define RC_RESTART_EXECUTION 		0x07 /* Used for 68000 */
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
/* formerly RC_GET_CHAR_REPEAT		0x1E */
#define RC_COMP_REFERENCE_RESTART 	0x1F
#define RC_NORMAL_GC_DONE	 	0x20
#define RC_COMPLETE_GC_DONE 		0x21 /* Used for 68000 */
#define RC_PURIFY_GC_1			0x22
#define RC_PURIFY_GC_2			0x23
#define RC_AFTER_MEMORY_UPDATE 		0x24 /* Used for 68000 */
#define RC_RESTARTABLE_EXIT	 	0x25 /* Used for 68000 */
/* formerly RC_GET_CHAR 		0x26 */
/* formerly RC_GET_CHAR_IMMEDIATE	0x27 */
#define RC_COMP_ASSIGNMENT_RESTART 	0x28
#define RC_POP_FROM_COMPILED_CODE 	0x29
#define RC_RETURN_TRAP_POINT		0x2A
#define RC_RESTORE_STEPPER		0x2B /* Used for 68000 */
#define RC_RESTORE_TO_STATE_POINT	0x2C
#define RC_MOVE_TO_ADJACENT_POINT	0x2D
#define RC_RESTORE_VALUE		0x2E
#define RC_RESTORE_DONT_COPY_HISTORY    0x2F

/* The following are not used in the 68000 implementation */
#define RC_POP_RETURN_ERROR		0x40
#define RC_EVAL_ERROR			0x41
#define RC_STACK_MARKER			0x42
#define RC_COMP_INTERRUPT_RESTART	0x43
/* formerly RC_COMP_RECURSION_GC	0x44 */
#define RC_RESTORE_INT_MASK		0x45
#define RC_HALT				0x46
#define RC_FINISH_GLOBAL_INT		0x47	/* Multiprocessor */
#define RC_REPEAT_DISPATCH		0x48
#define RC_GC_CHECK			0x49
#define RC_RESTORE_FLUIDS		0x4A
#define RC_COMP_LOOKUP_APPLY_RESTART	0x4B
#define RC_COMP_ACCESS_RESTART		0x4C
#define RC_COMP_UNASSIGNED_P_RESTART	0x4D
#define RC_COMP_UNBOUND_P_RESTART	0x4E
#define RC_COMP_DEFINITION_RESTART	0x4F
/* formerly RC_COMP_LEXPR_INTERRUPT_RESTART 0x50 */
#define RC_COMP_SAFE_REFERENCE_RESTART  0x51
/* formerly RC_COMP_CACHE_LOOKUP_RESTART  	0x52 */
#define RC_COMP_LOOKUP_TRAP_RESTART  	0x53
#define RC_COMP_ASSIGNMENT_TRAP_RESTART 0x54
/* formerly RC_COMP_CACHE_OPERATOR_RESTART	0x55 */
#define RC_COMP_OP_REF_TRAP_RESTART	0x56
#define RC_COMP_CACHE_REF_APPLY_RESTART 0x57
#define RC_COMP_SAFE_REF_TRAP_RESTART   0x58
#define RC_COMP_UNASSIGNED_TRAP_RESTART 0x59
/* formerly RC_COMP_CACHE_ASSIGN_RESTART	0x5A */
#define RC_COMP_LINK_CACHES_RESTART	0x5B
#define RC_HARDWARE_TRAP		0x5C
#define RC_INTERNAL_APPLY_VAL		0x5D
#define RC_COMP_ERROR_RESTART		0x5E
#define RC_PRIMITIVE_CONTINUE		0x5F
#define RC_COMP_LINK_CACHES_CONTINUE	0x60

/* When adding return codes, add them to the table below as well! */

#define MAX_RETURN_CODE			0x60

#define RETURN_NAME_TABLE						\
{									\
/* 0x00 */		"END_OF_COMPUTATION",				\
/* 0x01 */		"JOIN_STACKLETS",				\
/* 0x02 */		"RESTORE_CONTINUATION",				\
/* 0x03 */		"INTERNAL_APPLY",				\
/* 0x04 */		"BAD_INTERRUPT_CONTINUE",			\
/* 0x05 */		"RESTORE_HISTORY",				\
/* 0x06 */		"INVOKE_STACK_THREAD",				\
/* 0x07 */		"RESTART_EXECUTION",				\
/* 0x08 */		"EXECUTE_ASSIGNMENT_FINISH",			\
/* 0x09 */		"EXECUTE_DEFINITION_FINISH",			\
/* 0x0A */		"EXECUTE_ACCESS_FINISH",			\
/* 0x0b */		"EXECUTE_IN_PACKAGE_CONTINUE",			\
/* 0x0C */		"SEQ_2_DO_2",					\
/* 0x0d */		"SEQ_3_DO_2",					\
/* 0x0E */		"SEQ_3_DO_3",					\
/* 0x0f */		"CONDITIONAL_DECIDE",				\
/* 0x10 */		"DISJUNCTION_DECIDE",				\
/* 0x11 */		"COMB_1_PROCEDURE",				\
/* 0x12 */		"COMB_APPLY_FUNCTION",				\
/* 0x13 */		"COMB_2_FIRST_OPERAND",				\
/* 0x14 */		"COMB_2_PROCEDURE",				\
/* 0x15 */		"COMB_SAVE_VALUE",				\
/* 0x16 */		"PCOMB1_APPLY",					\
/* 0x17 */		"PCOMB2_DO_1",					\
/* 0x18 */		"PCOMB2_APPLY",					\
/* 0x19 */		"PCOMB3_DO_2",					\
/* 0x1A */		"PCOMB3_DO_1",					\
/* 0x1B */		"PCOMB3_APPLY",					\
/* 0x1C */		"SNAP_NEED_THUNK",				\
/* 0x1D */		"REENTER_COMPILED_CODE",			\
/* 0x1E */		"",						\
/* 0x1F */		"COMP_REFERENCE_RESTART",			\
/* 0x20 */		"NORMAL_GC_DONE",				\
/* 0x21 */		"COMPLETE_GC_DONE",				\
/* 0x22 */		"PURIFY_GC_1",					\
/* 0x23 */		"PURIFY_GC_2",					\
/* 0x24 */		"AFTER_MEMORY_UPDATE",				\
/* 0x25 */		"RESTARTABLE_EXIT",				\
/* 0x26 */		"",						\
/* 0x27 */		"",						\
/* 0x28 */		"COMP_ASSIGNMENT_RESTART",			\
/* 0x29 */		"POP_FROM_COMPILED_CODE",			\
/* 0x2A */		"RETURN_TRAP_POINT",				\
/* 0x2B */		"RESTORE_STEPPER",				\
/* 0x2C */		"RESTORE_TO_STATE_POINT",			\
/* 0x2D */		"MOVE_TO_ADJACENT_POINT",			\
/* 0x2E */		"RESTORE_VALUE",				\
/* 0x2F */		"RESTORE_DONT_COPY_HISTORY",			\
/* 0x30 */		"",						\
/* 0x31 */		"",						\
/* 0x32 */		"",						\
/* 0x33 */		"",						\
/* 0x34 */		"",						\
/* 0x35 */		"",						\
/* 0x36 */		"",						\
/* 0x37 */		"",						\
/* 0x38 */		"",						\
/* 0x39 */		"",						\
/* 0x3A */		"",						\
/* 0x3B */		"",						\
/* 0x3C */		"",						\
/* 0x3D */		"",						\
/* 0x3E */		"",						\
/* 0x3F */		"",						\
/* 0x40 */		"POP_RETURN_ERROR",				\
/* 0x41 */		"EVAL_ERROR",					\
/* 0x42 */		"STACK_MARKER",					\
/* 0x43 */		"COMPILER_INTERRUPT_RESTART",			\
/* 0x44 */		"",						\
/* 0x45 */		"RESTORE_INT_MASK",				\
/* 0x46 */		"HALT",						\
/* 0x47 */		"FINISH_GLOBAL_INT",				\
/* 0x48 */		"REPEAT_DISPATCH",				\
/* 0x49 */		"GC_CHECK",					\
/* 0x4A */		"RESTORE_FLUIDS",				\
/* 0x4B */		"COMPILER_LOOKUP_APPLY_RESTART",		\
/* 0x4C */		"COMPILER_ACCESS_RESTART",			\
/* 0x4D */		"COMPILER_UNASSIGNED_P_RESTART",		\
/* 0x4E */		"COMPILER_UNBOUND_P_RESTART",			\
/* 0x4F */		"COMPILER_DEFINITION_RESTART",			\
/* 0x50 */		"",						\
/* 0x51 */		"COMPILER_SAFE_REFERENCE_RESTART",		\
/* 0x52 */		"",						\
/* 0x53 */		"COMPILER_LOOKUP_TRAP_RESTART",			\
/* 0x54 */		"COMPILER_ASSIGNMENT_TRAP_RESTART",		\
/* 0X55 */		"",						\
/* 0x56 */		"COMPILER_OPERATOR_REFERENCE_TRAP_RESTART",	\
/* 0x57 */		"COMPILER_CACHE_REFERENCE_APPLY_RESTART",	\
/* 0x58 */		"COMPILER_SAFE_REFERENCE_TRAP_RESTART",		\
/* 0x59 */		"COMPILER_UNASSIGNED_P_TRAP_RESTART",		\
/* 0x5A */		"",						\
/* 0x5B */		"COMPILER_LINK_CACHES_RESTART",			\
/* 0x5C */		"HARDWARE_TRAP",				\
/* 0x5D */		"INTERNAL_APPLY_VAL",				\
/* 0x5E */		"COMPILER_ERROR_RESTARRT",			\
/* 0x5F */		"PRIMITIVE_CONTINUE"				\
/* 0x60 */	  	"COMPILER_LINK_CACHES_CONTINUE",		\
}
