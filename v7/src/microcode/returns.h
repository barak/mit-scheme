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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/returns.h,v 9.29 1987/11/04 20:02:48 cph Rel $
 *
 * Return codes.  These are placed in Return when an
 * interpreter operation needs to operate in several
 * phases.  This must correspond with UTABMD.SCM
 *
 */

/* These names are also in storage.c.
 * Please maintain consistency.
 * Names should not exceed 31 characters.
 */

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
#define RC_REPEAT_PRIMITIVE		0x42
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
#define RC_COMP_LEXPR_INTERRUPT_RESTART 0x50
#define RC_COMP_SAFE_REFERENCE_RESTART  0x51
#define RC_COMP_CACHE_LOOKUP_RESTART  	0x52
#define RC_COMP_LOOKUP_TRAP_RESTART  	0x53
#define RC_COMP_ASSIGNMENT_TRAP_RESTART 0x54
#define RC_COMP_CACHE_OPERATOR_RESTART	0x55
#define RC_COMP_OP_REF_TRAP_RESTART	0x56
#define RC_COMP_CACHE_REF_APPLY_RESTART 0x57
#define RC_COMP_SAFE_REF_TRAP_RESTART   0x58
#define RC_COMP_UNASSIGNED_TRAP_RESTART 0x59
#define RC_COMP_CACHE_ASSIGN_RESTART	0x5A

#define MAX_RETURN_CODE			0x5A

/* When adding return codes, don't forget to update storage.c too. */
