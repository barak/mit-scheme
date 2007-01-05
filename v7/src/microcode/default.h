/* -*-C-*-

$Id: default.h,v 9.48 2007/01/05 15:33:06 cph Exp $

Copyright (c) 1988-2000, 2002 Massachusetts Institute of Technology

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
  sp_register = Stack_Top;						\
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
#define CONSTANT_AREA_END() Free_Constant
#endif

#ifndef CONSTANT_AREA_START
#define CONSTANT_AREA_START() sp_register
#endif /* CONSTANT_AREA_START */

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
#define Fasdump_Free_Calc(NewFree, NewMemtop) do			\
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
