/* -*-C-*-

$Id: default.h,v 9.40 1993/10/14 19:19:55 gjr Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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

/* This file contains default definitions for some hooks which
   various machines require.  These machines define these hooks
   in CONFIG.H and this file defines them only if they remain
   undefined. */

/* Compiler bug fixes. */

#ifndef And2
#define And2(x, y)    ((x) && (y))
#define And3(x, y, z) ((x) && (y) && (z))
#define Or2(x, y)     ((x) || (y))
#define Or3(x, y, z)  ((x) || (y) || (z))
#endif

#ifndef MEMORY_FETCH
/* These definitions allow a true multi-processor with shared memory
   but no atomic longword operations (Butterfly and Concert,
   for example) to supply their own atomic operators in config.h. */
#define MEMORY_FETCH(locative) (locative)
#define MEMORY_STORE(locative, object) (locative) = (object)
#endif

#ifndef Get_Fixed_Obj_Slot
#define Get_Fixed_Obj_Slot(N)	FAST_VECTOR_REF (Fixed_Objects, N)
#define Set_Fixed_Obj_Slot(N,S)	FAST_VECTOR_SET (Fixed_Objects, N, S)
#define Update_FObj_Slot(N, S)  Set_Fixed_Obj_Slot(N, S)
#define Declare_Fixed_Objects()	SCHEME_OBJECT Fixed_Objects
#define Valid_Fixed_Obj_Vector() (VECTOR_P (Fixed_Objects))
#define Save_Fixed_Obj(Save_FO)					\
  Save_FO = Fixed_Objects;					\
  Fixed_Objects = SHARP_F;
#define Restore_Fixed_Obj(Save_FO)				\
  Fixed_Objects = Save_FO
#endif

/* Atomic swapping hook.  Used extensively. */

#ifndef SWAP_POINTERS
#define SWAP_POINTERS(locative, object, target) do			\
{									\
  (target) = (* (locative));						\
  (* (locative)) = (object);						\
} while (0)
#endif

#ifndef INITIALIZE_STACK
#define INITIALIZE_STACK() do						\
{									\
  Stack_Pointer = Stack_Top;						\
  SET_STACK_GUARD (Stack_Bottom + STACK_GUARD_SIZE);			\
  * Stack_Bottom							\
    = (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Stack_Bottom));		\
} while (0)
#endif

#ifndef STACK_ALLOCATION_SIZE
#define STACK_ALLOCATION_SIZE(Stack_Blocks) (Stack_Blocks)
#endif

#ifndef STACK_OVERFLOWED_P
#define STACK_OVERFLOWED_P()						\
  ((* Stack_Bottom) != (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Stack_Bottom)))
#endif

#ifndef STACK_SANITY_CHECK
#define STACK_SANITY_CHECK(name) do					\
{									\
  extern void EXFUN (stack_death, (CONST char *));			\
									\
  if (STACK_OVERFLOWED_P ())						\
    stack_death (name);							\
    /*NOTREACHED */							\
} while (0)
#endif

#ifndef SET_CONSTANT_TOP
#define SET_CONSTANT_TOP() do						\
{									\
  ALIGN_FLOAT (Free_Constant);						\
  SEAL_CONSTANT_SPACE ();						\
} while (0)
#endif

#ifndef TEST_CONSTANT_TOP
#define TEST_CONSTANT_TOP(New_Top) ((New_Top) <= Constant_Top)
#endif

#ifndef CONSTANT_AREA_END
#define CONSTANT_AREA_END()	Free_Constant
#endif

#ifndef CONSTANT_AREA_START
#define CONSTANT_AREA_START()	Stack_Pointer
#endif CONSTANT_AREA_START

#ifndef SEAL_CONSTANT_SPACE
#define SEAL_CONSTANT_SPACE() do					\
{									\
  * Free_Constant =							\
    (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Free_Constant));		\
} while (0)
#endif

/* Used in debug.c */

#ifndef Back_Trace_Entry_Hook
#define Back_Trace_Entry_Hook()
#endif

#ifndef Back_Trace_Exit_Hook
#define Back_Trace_Exit_Hook()
#endif

/* Used in extern.h */

#ifndef More_Debug_Flag_Externs
#define More_Debug_Flag_Externs()
#endif

/* Used in fasdump.c */

#ifndef Band_Dump_Permitted
#define Band_Dump_Permitted()
#endif

#ifndef Band_Load_Hook
#define Band_Load_Hook()
#endif

#ifndef Band_Dump_Exit_Hook
#define Band_Dump_Exit_Hook()
#endif

#ifndef Fasdump_Exit_Hook
#define Fasdump_Exit_Hook()
#endif

#ifndef Fasdump_Free_Calc
#define Fasdump_Free_Calc(NewFree, NewMemtop, ignored) do		\
{									\
  NewFree = Unused_Heap_Bottom;						\
  NewMemTop = Unused_Heap_Top;						\
} while (0)
#endif

/* Used in interpret.c */

#ifndef Eval_Ucode_Hook
#define Eval_Ucode_Hook()
#endif

#ifndef Pop_Return_Ucode_Hook
#define Pop_Return_Ucode_Hook()
#endif

#ifndef Apply_Ucode_Hook
#define Apply_Ucode_Hook()
#endif

#ifndef End_GC_Hook
#define End_GC_Hook()
#endif

/* Used in storage.c */

#ifndef More_Debug_Flag_Allocs
#define More_Debug_Flag_Allocs()
#endif

/* Used in utils.c */

#ifndef Global_Interrupt_Hook
#define Global_Interrupt_Hook()
#endif

#ifndef Error_Exit_Hook
#define Error_Exit_Hook()
#endif

/* Common Lisp Hooks */

#ifndef SITE_EXPRESSION_DISPATCH_HOOK
#define SITE_EXPRESSION_DISPATCH_HOOK()
#endif

#ifndef SITE_RETURN_DISPATCH_HOOK
#define SITE_RETURN_DISPATCH_HOOK()
#endif

#ifndef FASLOAD_RELOCATE_HOOK
#define FASLOAD_RELOCATE_HOOK(heap_low, heap_high, constant_low, constant_high)
#endif
