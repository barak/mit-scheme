/*	EMACS should recognize -*- C -*- code by itself.	*/

/****************************************************************
*                                                               *
*                         Copyright (c) 1986                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: DEFAULT.H
 *
 * This file contains default definitions for some hooks which 
 * various machines require.  These machines define these hooks
 * in CONFIG.H and this file defines them only if they remain 
 * undefined.
 *
 */

/* Compiler bug fixes. */

#ifndef And2
#define And2(x, y)    ((x) && (y))
#define And3(x, y, z) ((x) && (y) && (z))
#define Or2(x, y)     ((x) || (y))
#define Or3(x, y, z)  ((x) || (y) || (z))
#endif

#ifndef Fetch
/* These definitions allow a true multi-processor with shared memory
   but no atomic longword operations (Butterfly and Concert,
   for example) to supply their own atomic operators in config.h.
*/
#define Fetch(P)                (P) 
#define Store(P, S)             (P) = (S)
#endif

#ifndef Get_Fixed_Obj_Slot
#define Get_Fixed_Obj_Slot(N)	Fast_User_Vector_Ref(Fixed_Objects, N)
#define Set_Fixed_Obj_Slot(N,S)	Fast_User_Vector_Set(Fixed_Objects, N, S)
#define Update_FObj_Slot(N, S)  Set_Fixed_Obj_Slot(N, S)
#define Declare_Fixed_Objects()	Pointer Fixed_Objects;
#define Valid_Fixed_Obj_Vector()				\
  (Type_Code(Fixed_Objects) == TC_VECTOR)
#define Save_Fixed_Obj(Save_FO)					\
  Save_FO = Fixed_Objects;					\
  Fixed_Objects = NIL;
#define Restore_Fixed_Obj(Save_FO)				\
  Fixed_Objects = Save_FO
#endif


/* Atomic swapping hook.  Used extensively. */

#ifndef Swap_Pointers
extern Pointer Swap_Temp;
#define Swap_Pointers(P, S)			\
(Swap_Temp = *(P), *(P) = (S), Swap_Temp) 
#endif

#ifndef Set_Pure_Top
#ifndef USE_STACKLETS
#define Set_Pure_Top()	 			\
  Align_Float(Free_Constant);			\
  Set_Stack_Guard(Free_Constant+STACK_GUARD_SIZE)
#define Test_Pure_Space_Top(New_Top)		\
  ((New_Top+STACK_GUARD_SIZE) <= Stack_Pointer)
#define Absolute_Stack_Base	Free_Constant

#ifndef Initialize_Stack
#define Initialize_Stack()						\
  Stack_Top = Highest_Allocated_Address;				\
  Stack_Pointer = Stack_Top;						\
  Set_Stack_Guard(Free_Constant + STACK_GUARD_SIZE)
#endif

#else	/* Stacklets in use */

#define Set_Pure_Top()	Align_Float(Free_Constant)
#define Test_Pure_Space_Top(New_Top)		\
   (New_Top <= Highest_Allocated_Address)
#endif
#endif

/* Character IO hooks.  Used extensively. */

#ifndef OS_Put_C
#define	OS_Put_C		putc
#endif

#ifndef OS_Get_C
#define OS_Get_C		getc
#endif

/* Used in BOOT.C */

#ifndef term_type
#define term_type void
#endif

#ifndef Command_Line_Hook
#define Command_Line_Hook()
#endif

#ifndef Exit_Scheme_Declarations
#define Exit_Scheme_Declarations
#endif

#ifndef Init_Exit_Scheme
#define Init_Exit_Scheme()
#endif

#ifndef Exit_Scheme
#define Exit_Scheme exit
#endif

/* Used in various places. */

#ifndef Init_Fixed_Objects
#define Init_Fixed_Objects()				\
  Default_Init_Fixed_Objects(Fixed_Objects)
#endif

#ifndef Set_Fixed_Obj_Hook
#define Set_Fixed_Obj_Hook(New_Vector)			\
  Fixed_Objects = New_Vector
#endif

#ifndef Entry_Hook
#define Entry_Hook()
#endif

#ifndef Exit_Hook
#define Exit_Hook()
#endif

#ifndef Sys_Clock
#define Sys_Clock()	System_Clock()
#endif

/* Used in DEBUG.C */

#ifndef Back_Trace_Entry_Hook
#define Back_Trace_Entry_Hook()
#endif

#ifndef Back_Trace_Exit_Hook
#define Back_Trace_Exit_Hook()
#endif

#ifndef More_Debug_Flag_Cases
#define More_Debug_Flag_Cases()
#endif

#ifndef Set_Flag_Hook
#define Set_Flag_Hook()
#endif

#ifndef More_Debug_Flag_Names
#define More_Debug_Flag_Names()
#endif

#ifndef LAST_SWITCH
#define LAST_SWITCH		LAST_NORMAL_SWITCH
#endif

#ifndef debug_getdec
#define debug_getdec normal_debug_getdec
#endif

/* Used in EXTERN.H */

#ifndef More_Debug_Flag_Externs
#define More_Debug_Flag_Externs()
#endif

/* Used in FASDUMP.C */

#ifndef Band_Dump_Permitted
#define Band_Dump_Permitted()
#endif

#ifndef Band_Load_Hook
#define Band_Load_Hook()
#endif

#ifndef Fasdump_Exit_Hook
#define Fasdump_Exit_Hook()
#endif

#ifndef Fasdump_Free_Calc
#define Fasdump_Free_Calc(NewFree, NewMemtop, ignored)	\
  NewFree = Unused_Heap;				\
  NewMemTop = Unused_Heap_Top
#endif

/* Used in FASLOAD.C */

#ifndef Open_File_Hook
#define Open_File_Hook(ignore)
#endif

#ifndef Close_File_Hook
#define Close_File_Hook()
#endif

/* Used in FLONUM.H and GENERIC.C */

#ifndef double_into_fixnum
#define double_into_fixnum(what, target)				\
  target = Make_Non_Pointer(TC_FIXNUM, ((long) (what)))
#endif

/* Used in INTERPRET.C */

/* Primitive calling code. */

#ifndef ENABLE_DEBUGGING_TOOLS
#define Apply_Primitive(N)	(*(Primitive_Table[N]))()
#else
extern Pointer Apply_Primitive();
#endif

#ifndef Metering_Apply_Primitive
#define Metering_Apply_Primitive(Loc, N)				\
Loc = Apply_Primitive(N)
#endif

#ifndef Eval_Ucode_Hook()
#define Eval_Ucode_Hook()
#endif

#ifndef Pop_Return_Ucode_Hook()
#define Pop_Return_Ucode_Hook()
#endif

#ifndef Apply_Ucode_Hook()
#define Apply_Ucode_Hook()
#endif

#ifndef End_GC_Hook
#define End_GC_Hook()
#endif

/* Used in STORAGE.C */

#ifndef More_Debug_Flag_Allocs
#define More_Debug_Flag_Allocs()
#endif

/* Used in UTILS.C */

#ifndef Global_Interrupt_Hook
#define Global_Interrupt_Hook()
#endif

#ifndef Error_Exit_Hook
#define Error_Exit_Hook()
#endif

/* Used in LOOKUP.C */

/* Permit caching of incrementally defined variables */
#ifndef Allow_Aux_Compilation
#define Allow_Aux_Compilation	true
#endif

/* This is how we support future numbering for external metering */
#ifndef New_Future_Number
#define New_Future_Number() NIL
#else
Pointer Get_New_Future_Number();
#endif
