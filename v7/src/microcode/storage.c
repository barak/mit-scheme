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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/storage.c,v 9.29 1987/04/21 15:00:46 cph Exp $

This file defines the storage for global variables for
the Scheme Interpreter. */

#include "scheme.h"
#include "gctype.c"

                         /*************/
                         /* REGISTERS */
                         /*************/

Pointer
 *Ext_History,		/* History register */
 *Free,			/* Next free word in storage */
 *MemTop,		/* Top of free space available */
 *Ext_Stack_Pointer,	/* Next available slot in control stack */
 *Stack_Top,		/* Top of control stack */
 *Stack_Guard,		/* Guard area at end of stack */
 *Free_Stacklets,	/* Free list of stacklets */
 *Constant_Space,	/* Bottom of constant+pure space */
 *Free_Constant,	/* Next free cell in constant+pure area */
 *Heap_Top,		/* Top of current heap */
 *Heap_Bottom,		/* Bottom of current heap */
 *Unused_Heap_Top,	/* Top of other heap */
 *Unused_Heap,		/* Bottom of other heap */
 *Local_Heap_Base,	/* Per-processor CONSing area */
 *Heap,			/* Bottom of entire heap */
  Current_State_Point = NIL, /* Used by dynamic winder */
  Fluid_Bindings = NIL,	/* Fluid bindings AList */
  return_to_interpreter, /* Return address/code left by interpreter
			    when calling compiled code */
 *last_return_code,	/* Address of the most recent return code in the stack.
			   This is only meaningful while in compiled code.
			   *** This must be changed when stacklets are used. ***
			 */
 Swap_Temp;		/* Used by Swap_Pointers in default.h */

long IntCode,		/* Interrupts requesting */
     IntEnb,		/* Interrupts enabled */
     Lookup_Offset,	/* Slot lookup result return */
     GC_Reserve = 4500,	/* Scheme pointer overflow space in heap */
     GC_Space_Needed = 0, /* Amount of space needed when GC triggered */
     /* Used to signal microcode errors from compiled code. */
     compiled_code_error_code;

Declare_Fixed_Objects();

FILE *(Channels[FILE_CHANNELS]), *File_Handle, *Photo_File_Handle;

int Saved_argc;
char **Saved_argv;
char *OS_Name, *OS_Variant;

Boolean Photo_Open = false; /* Photo file open */

Boolean Trapping;

Pointer Old_Return_Code, *Return_Hook_Address;

Pointer *Prev_Restore_History_Stacklet;
long Prev_Restore_History_Offset;

jmp_buf *Back_To_Eval; /* Buffer for set/longjmp */

long Heap_Size, Constant_Size, Stack_Size;
Pointer *Highest_Allocated_Address;

#ifndef Heap_In_Low_Memory
Pointer *Memory_Base;
#endif

                    /**********************/
                    /* DEBUGGING SWITCHES */
                    /**********************/

#ifdef ENABLE_DEBUGGING_TOOLS
Boolean Eval_Debug	= false;
Boolean Hex_Input_Debug	= false;
Boolean File_Load_Debug	= false;
Boolean Reloc_Debug	= false;	
Boolean Intern_Debug	= false;
Boolean Cont_Debug	= false;
Boolean Primitive_Debug	= false;
Boolean Lookup_Debug	= false;
Boolean Define_Debug	= false;
Boolean GC_Debug	= false;
Boolean Upgrade_Debug	= false;
Boolean Dump_Debug	= false;
Boolean Trace_On_Error	= false;
Boolean Bignum_Debug    = false;
Boolean Per_File	= true;
Boolean Fluids_Debug	= false;
More_Debug_Flag_Allocs();

int debug_slotno = 0;
int debug_nslots = 0;
int local_slotno = 0;
int local_nslots = 0;
/* MHWU
int debug_circle[debug_maxslots];
int local_circle[debug_maxslots];
*/
int debug_circle[100];
int local_circle[100];
#endif

		/****************************/
		/* Debugging Macro Messages */
		/****************************/

char *CONT_PRINT_RETURN_MESSAGE =   "Save_Cont, return code";
char *CONT_PRINT_EXPR_MESSAGE   =   "Save_Cont, expression";
char *RESTORE_CONT_RETURN_MESSAGE = "Restore_Cont, return code";
char *RESTORE_CONT_EXPR_MESSAGE =   "Restore_Cont, expression";

static char No_Name[] = "";

char *Return_Names[] = {
/* 0x00 */		"END_OF_COMPUTATION",
/* 0x01 */		"JOIN_STACKLETS",
/* 0x02 */		"RESTORE_CONTINUATION",
/* 0x03 */		"INTERNAL_APPLY",
/* 0x04 */		"BAD_INTERRUPT_CONTINUE",
/* 0x05 */		"RESTORE_HISTORY",
/* 0x06 */		"INVOKE_STACK_THREAD",
/* 0x07 */		"RESTART_EXECUTION",
/* 0x08 */		"EXECUTE_ASSIGNMENT_FINISH",
/* 0x09 */		"EXECUTE_DEFINITION_FINISH",
/* 0x0A */		"EXECUTE_ACCESS_FINISH",
/* 0x0b */		"EXECUTE_IN_PACKAGE_CONTINUE",
/* 0x0C */		"SEQ_2_DO_2",
/* 0x0d */		"SEQ_3_DO_2",
/* 0x0E */		"SEQ_3_DO_3",
/* 0x0f */		"CONDITIONAL_DECIDE",
/* 0x10 */		"DISJUNCTION_DECIDE",
/* 0x11 */		"COMB_1_PROCEDURE",
/* 0x12 */		"COMB_APPLY_FUNCTION",
/* 0x13 */		"COMB_2_FIRST_OPERAND",
/* 0x14 */		"COMB_2_PROCEDURE",
/* 0x15 */		"COMB_SAVE_VALUE",
/* 0x16 */		"PCOMB1_APPLY",
/* 0x17 */		"PCOMB2_DO_1",
/* 0x18 */		"PCOMB2_APPLY",
/* 0x19 */		"PCOMB3_DO_2",
/* 0x1A */		"PCOMB3_DO_1",
/* 0x1B */		"PCOMB3_APPLY",
/* 0x1C */		"SNAP_NEED_THUNK",
/* 0x1D */		No_Name,
/* 0x1E */		No_Name,
/* 0x1F */		No_Name,
/* 0x20 */		"NORMAL_GC_DONE",
/* 0x21 */		"COMPLETE_GC_DONE",
/* 0x22 */		"PURIFY_GC_1",
/* 0x23 */		"PURIFY_GC_2",
/* 0x24 */		"AFTER_MEMORY_UPDATE",
/* 0x25 */		"RESTARTABLE_EXIT",
/* 0x26 */		No_Name,
/* 0x27 */		No_Name,

/* 0x28 */		No_Name,
/* 0x29 */		No_Name,
/* 0x2A */		"RETURN_TRAP_POINT",
/* 0x2B */		"RESTORE_STEPPER",
/* 0x2C */		"RESTORE_TO_STATE_POINT",
/* 0x2D */		"MOVE_TO_ADJACENT_POINT",
/* 0x2E */		"RESTORE_VALUE",
/* 0x2F */		"RESTORE_DONT_COPY_HISTORY",
/* 0x30 */		No_Name,
/* 0x31 */		No_Name,
/* 0x32 */		No_Name,
/* 0x33 */		No_Name,
/* 0x34 */		No_Name,
/* 0x35 */		No_Name,
/* 0x36 */		No_Name,
/* 0x37 */		No_Name,
/* 0x38 */		No_Name,
/* 0x39 */		No_Name,
/* 0x3A */		No_Name,
/* 0x3B */		No_Name,
/* 0x3C */		No_Name,
/* 0x3D */		No_Name,
/* 0x3E */		No_Name,
/* 0x3F */		No_Name,
/* 0x40 */		"POP_RETURN_ERROR",
/* 0x41 */		"EVAL_ERROR",
/* 0x42 */		"REPEAT_PRIMITIVE",
/* 0x43 */		"COMPILER_INTERRUPT_RESTART",
/* 0x44 */		No_Name,
/* 0x45 */		"RESTORE_INT_MASK",
/* 0x46 */		"HALT",
/* 0x47 */		"FINISH_GLOBAL_INT",
/* 0x48 */		"REPEAT_DISPATCH",
/* 0x49 */		"GC_CHECK",
/* 0x4A */		"RESTORE_FLUIDS",
/* 0x4B */		"COMPILER_LOOKUP_APPLY_RESTART",
/* 0x4C */		"COMPILER_ACCESS_RESTART",
/* 0x4D */		"COMPILER_UNASSIGNED_P_RESTART",
/* 0x4E */		"COMPILER_UNBOUND_P_RESTART",
/* 0x4F */		"COMPILER_DEFINITION_RESTART",
/* 0x50 */		"COMPILER_LEXPR_GC_RESTART"
};

#if (MAX_RETURN_CODE != 0x50)
/* Cause an error */
#include "Returns.h and storage.c are inconsistent -- Names Table"
#endif

long MAX_RETURN = MAX_RETURN_CODE;
