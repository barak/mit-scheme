/* -*-C-*-

$Id: storage.c,v 9.59 2002/11/20 19:46:14 cph Exp $

Copyright (c) 1987-2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* This file defines the storage for the interpreter's global variables. */

#include "scheme.h"
#include "gctype.c"

                         /*************/
                         /* REGISTERS */
                         /*************/

SCHEME_OBJECT
  * MemTop,		/* Top of free space available */
  * Free,		/* Next free word in heap */
  * Heap_Top,		/* Top of current heap */
  * Heap_Bottom,	/* Bottom of current heap */
  * Unused_Heap_Top,	/* Top of unused heap */
  * Unused_Heap_Bottom,	/* Bottom of unused heap */
  * Stack_Guard,	/* Guard area at end of stack */
  * sp_register,	/* Next available slot in control stack */
  * Stack_Bottom,	/* Bottom of control stack */
  * Stack_Top,		/* Top of control stack */
  * Free_Constant,	/* Next free word in constant space */
  * Constant_Space,	/* Bottom of constant+pure space */
  * Constant_Top,	/* Top of constant+pure space */
  * Local_Heap_Base,	/* Per-processor CONSing area */
  * Free_Stacklets,	/* Free list of stacklets */
  * history_register,	/* History register */
  Current_State_Point,	/* Dynamic state point */
  Fluid_Bindings,	/* Fluid bindings AList */
  * last_return_code;	/* Address of the most recent return code in the stack.
			   This is only meaningful while in compiled code.
			   *** This must be changed when stacklets are used. */

long
  temp_long,		/* temporary for sign extension */
  GC_Reserve,		/* Scheme pointer overflow space in heap */
  GC_Space_Needed;	/* Amount of space needed when GC triggered */

Declare_Fixed_Objects ();

Boolean Trapping;

SCHEME_OBJECT Old_Return_Code;
SCHEME_OBJECT * Return_Hook_Address;

SCHEME_OBJECT * Prev_Restore_History_Stacklet;
long Prev_Restore_History_Offset;

long Heap_Size;
long Constant_Size;
long Stack_Size;
SCHEME_OBJECT * Lowest_Allocated_Address, * Highest_Allocated_Address;
#ifndef HEAP_IN_LOW_MEMORY
SCHEME_OBJECT * memory_base;
#endif

                    /**********************/
                    /* DEBUGGING SWITCHES */
                    /**********************/

#ifdef ENABLE_DEBUGGING_FLAGS

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
Boolean Per_File	= false;
Boolean Fluids_Debug	= false;
More_Debug_Flag_Allocs();

int debug_slotno = 0;
int debug_nslots = 0;
int local_slotno = 0;
int local_nslots = 0;

#if FALSE /* MHWU */
int debug_circle[debug_maxslots];
int local_circle[debug_maxslots];
#endif /* false */

int debug_circle[100];
int local_circle[100];
#endif /* ENABLE_DEBUGGING_FLAGS */

		/****************************/
		/* Debugging Macro Messages */
		/****************************/

char *CONT_PRINT_RETURN_MESSAGE =   "Save_Cont, return code";
char *CONT_PRINT_EXPR_MESSAGE   =   "Save_Cont, expression";
char *RESTORE_CONT_RETURN_MESSAGE = "Restore_Cont, return code";
char *RESTORE_CONT_EXPR_MESSAGE =   "Restore_Cont, expression";

/* Interpreter code name and message tables */

long MAX_RETURN = MAX_RETURN_CODE;

extern char *Return_Names[];
char *Return_Names[] = RETURN_NAME_TABLE;	/* in returns.h */

extern char *Type_Names[];
char *Type_Names[] = TYPE_NAME_TABLE;		/* in types.h */

extern char *Abort_Names[];
char *Abort_Names[] = ABORT_NAME_TABLE;		/* in const.h */

extern char *Error_Names[];
char *Error_Names[] = ERROR_NAME_TABLE;		/* in errors.h */

extern char *Term_Names[];
char *Term_Names[] = TERM_NAME_TABLE;		/* in errors.h */

extern char *Term_Messages[];
char *Term_Messages[] = TERM_MESSAGE_TABLE;	/* in errors.h */
