/* -*-C-*-

$Id: stack.h,v 9.42 2002/07/02 20:50:53 cph Exp $

Copyright (c) 1987-1999, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*/

/* This file contains macros for manipulating stacks and stacklets. */

#ifndef STACK_RESET
# define STACK_RESET() do {} while (0)
#endif /* STACK_RESET */

#ifdef USE_STACKLETS

/*
  Stack is made up of linked small parts, each in the heap
 */

#define INITIALIZE_STACK() do						\
{									\
  if (GC_Check(Default_Stacklet_Size))					\
    Microcode_Termination(TERM_STACK_ALLOCATION_FAILED);		\
  SET_STACK_GUARD (Free + STACKLET_HEADER_SIZE);			\
  *Free =								\
    (MAKE_OBJECT (TC_MANIFEST_VECTOR, (Default_Stacklet_Size - 1)));	\
  Free += Default_Stacklet_Size;					\
  sp_register = Free;							\
  Free_Stacklets = NULL;						\
  Prev_Restore_History_Stacklet = NULL;					\
  Prev_Restore_History_Offset = 0;					\
} while (0)

/* This is a lie, but OK in the context in which it is used. */

#define STACK_OVERFLOWED_P()	FALSE

#define Internal_Will_Push(N)						\
{									\
  if ((sp_register - (N)) < Stack_Guard)				\
  {									\
    Allocate_New_Stacklet((N));						\
  }									\
}

/* No space required independent of the heap for the stacklets */

#define STACK_ALLOCATION_SIZE(Stack_Blocks)	0

#define Current_Stacklet	(Stack_Guard - STACKLET_HEADER_SIZE)

/* Make the unused portion of the old stacklet invisible to garbage
 * collection. This also allows the stack pointer to be reconstructed.
 */

#define Internal_Terminate_Old_Stacklet()				\
{									\
  Current_Stacklet[STACKLET_REUSE_FLAG] = SHARP_T;			\
  Current_Stacklet[STACKLET_UNUSED_LENGTH] =				\
    MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (sp_register - Stack_Guard));	\
}

#ifdef ENABLE_DEBUGGING_TOOLS

#define Terminate_Old_Stacklet()					\
{									\
  if (sp_register < Stack_Guard)					\
  {									\
    outf_fatal ("\nsp_register: 0x%lx, Guard: 0x%lx\n",			\
                ((long) sp_register), ((long) Stack_Guard));		\
    Microcode_Termination(TERM_EXIT);					\
  }									\
  Internal_Terminate_Old_Stacklet();					\
}

#else /* not ENABLE_DEBUGGING_TOOLS */

#define Terminate_Old_Stacklet()	Internal_Terminate_Old_Stacklet()

#endif /* ENABLE_DEBUGGING_TOOLS */

/* Used by garbage collector to detect the end of constant space */

#define CONSTANT_AREA_START()	Constant_Space

#define Get_Current_Stacklet()						\
  (MAKE_POINTER_OBJECT (TC_CONTROL_POINT, Current_Stacklet))

#define Previous_Stack_Pointer(Where)					\
  (MEMORY_LOC								\
   (Where,								\
    (STACKLET_HEADER_SIZE +						\
     (OBJECT_DATUM (MEMORY_REF (Where, STACKLET_UNUSED_LENGTH))))))

#define Set_Current_Stacklet(Where)					\
{									\
  SCHEME_OBJECT Our_Where;						\
									\
  Our_Where = (Where);							\
  SET_STACK_GUARD (MEMORY_LOC (Our_Where, STACKLET_HEADER_SIZE));	\
  sp_register = Previous_Stack_Pointer(Our_Where);			\
}

#define STACKLET_SLACK	(STACKLET_HEADER_SIZE + CONTINUATION_SIZE)

#define Default_Stacklet_Size 	(Stack_Size + STACKLET_SLACK)

#define New_Stacklet_Size(N)						\
 (STACKLET_SLACK + Stack_Size * (((N) + Stack_Size - 1) / Stack_Size))

#define Get_End_Of_Stacklet()						\
  (&(Current_Stacklet[1 + OBJECT_DATUM (Current_Stacklet[STACKLET_LENGTH])]))

#define Apply_Stacklet_Backout()					\
Will_Push((2 * CONTINUATION_SIZE) + (STACK_ENV_EXTRA_SLOTS + 2));	\
  exp_register = SHARP_F;						\
  Store_Return(RC_END_OF_COMPUTATION);					\
  Save_Cont();								\
  STACK_PUSH (val_register);						\
  STACK_PUSH (Previous_Stacklet);					\
  STACK_PUSH (STACK_FRAME_HEADER + 1);					\
  Store_Return(RC_INTERNAL_APPLY);					\
  Save_Cont();								\
Pushed()

#define Join_Stacklet_Backout()		Apply_Stacklet_Backout()

/* This depends on the fact that Within_Control_Point is going to
 * push an apply frame immediately after Return_To_Previous_Stacklet
 * "returns".  This apply will cause the GC, then the 2nd argument to
 * Within_Control_Point will be invoked, and finally the control point
 * will be entered.
 */

#define Within_Stacklet_Backout()					\
{									\
  SCHEME_OBJECT Old_Expression;						\
									\
  Old_Expression = exp_register;					\
  exp_register = Previous_Stacklet;					\
  Store_Return(RC_JOIN_STACKLETS);					\
  Save_Cont();								\
  exp_register = Old_Expression;					\
}

/* Our_Throw is used in chaining from one stacklet to another.  In
 * order to improve efficiency, the entire stack is copied neither on
 * catch or throw, but is instead copied one stacklet at a time as
 * needed.  The need to copy a stacklet is signified by the object in
 * the STACKLET_REUSE_FLAG of a stacklet.  If this object is #F, the
 * stacklet is copied when it is "returned into", and the word is set
 * to #F in the stacklet into which the copied one will return. When a
 * stacklet is returned from, it is no longer needed for anything so it
 * can be deallocated.  A free list of deallocate stacklets is kept in
 * order to improve the efficiencty of their use.
 */

#define Our_Throw(From_Pop_Return, Stacklet)				\
{									\
  SCHEME_OBJECT Previous_Stacklet;					\
  SCHEME_OBJECT *Stacklet_Top;						\
									\
  Previous_Stacklet = (Stacklet);					\
  Stacklet_Top = Current_Stacklet;					\
  Stacklet_Top[STACKLET_FREE_LIST_LINK] =				\
    ((SCHEME_OBJECT) Free_Stacklets);					\
  Free_Stacklets = Stacklet_Top;					\
  if (!(From_Pop_Return))						\
  {									\
    Prev_Restore_History_Stacklet = NULL;				\
    Prev_Restore_History_Offset = 0;					\
  }									\
  if ((MEMORY_REF (Previous_Stacklet, STACKLET_REUSE_FLAG)) == SHARP_F)	\
  {									\
    /* We need to copy the stacklet into which we are			\
       returning.							\
     */									\
									\
    if (GC_Check(VECTOR_LENGTH (Previous_Stacklet) + 1))		\
    {									\
      /* We don't have enough space to copy the stacklet. */		\
									\
      Free_Stacklets =							\
	((SCHEME_OBJECT *) Free_Stacklets[STACKLET_FREE_LIST_LINK]);	\
      sp_register = Get_End_Of_Stacklet();				\
      Prev_Restore_History_Stacklet = NULL;				\
      Prev_Restore_History_Offset = 0

      /* Backout code inserted here by macro user */

#define Our_Throw_Part_2()						\
      Request_GC(VECTOR_LENGTH (Previous_Stacklet) + 1);		\
    }									\
    else								\
    {									\
      /* There is space available to copy the stacklet. */		\
									\
      long Unused_Length;						\
      fast Used_Length;							\
      fast SCHEME_OBJECT *Old_Stacklet_Top, *temp;			\
      SCHEME_OBJECT *First_Continuation;				\
									\
      Old_Stacklet_Top = OBJECT_ADDRESS (Previous_Stacklet);		\
      First_Continuation =						\
        MEMORY_LOC (Previous_Stacklet,					\
		    ((1 + VECTOR_LENGTH (Previous_Stacklet)) -		\
		     CONTINUATION_SIZE));				\
      if (Old_Stacklet_Top == Prev_Restore_History_Stacklet)		\
        Prev_Restore_History_Stacklet = NULL;				\
      if (First_Continuation[CONTINUATION_RETURN_CODE] ==		\
	  MAKE_OBJECT (TC_RETURN_CODE, RC_JOIN_STACKLETS))		\
      {									\
	SCHEME_OBJECT Older_Stacklet;					\
									\
	Older_Stacklet = First_Continuation[CONTINUATION_EXPRESSION];	\
	MEMORY_SET (Older_Stacklet, STACKLET_REUSE_FLAG, SHARP_F);	\
      }									\
      temp = Free;							\
      SET_STACK_GUARD (& (temp[STACKLET_HEADER_SIZE]));			\
      temp[STACKLET_LENGTH] = Old_Stacklet_Top[STACKLET_LENGTH];	\
      Unused_Length =							\
	OBJECT_DATUM (Old_Stacklet_Top[STACKLET_UNUSED_LENGTH]) +	\
        STACKLET_HEADER_SIZE;						\
      temp += Unused_Length;						\
      sp_register = temp;						\
      Used_Length =							\
        (OBJECT_DATUM (Old_Stacklet_Top[STACKLET_LENGTH]) -		\
         Unused_Length) + 1;						\
      Old_Stacklet_Top += Unused_Length;				\
      while (--Used_Length >= 0)					\
	*temp++ = *Old_Stacklet_Top++;					\
      Free = temp;							\
    }									\
  }									\
  else									\
  {									\
    /* No need to copy the stacklet we are going into */		\
									\
    if (OBJECT_ADDRESS (Previous_Stacklet)==				\
        Prev_Restore_History_Stacklet)					\
      Prev_Restore_History_Stacklet = NULL;				\
    Set_Current_Stacklet(Previous_Stacklet);				\
  }									\
}

#else /* not USE_STACKLETS */

/*
  Full size stack in a statically allocated area
 */

#define Stack_Check(P) do						\
{									\
  if ((P) <= Stack_Guard)						\
    {									\
      extern void EXFUN (stack_death, (CONST char *));			\
      if (STACK_OVERFLOWED_P ())					\
	stack_death ("Stack_Check");					\
      REQUEST_INTERRUPT (INT_Stack_Overflow);				\
    }									\
} while (0)

#define Internal_Will_Push(N)	Stack_Check(sp_register - (N))

#define Terminate_Old_Stacklet()

#define Get_Current_Stacklet() SHARP_F

#define Set_Current_Stacklet(Where) {}

#define Previous_Stack_Pointer(Where)					\
  (MEMORY_LOC								\
   (Where,								\
    (STACKLET_HEADER_SIZE +						\
     (OBJECT_DATUM (MEMORY_REF (Where, STACKLET_UNUSED_LENGTH))))))

/* Never allocate more space */
#define New_Stacklet_Size(N)	0

#define Get_End_Of_Stacklet()	Stack_Top

/* Not needed in this version */

#define Join_Stacklet_Backout()
#define Apply_Stacklet_Backout()
#define Within_Stacklet_Backout()

/* This piece of code KNOWS which way the stack grows.
   The assumption is that successive pushes modify decreasing addresses.
 */

/* Clear the stack and replace it with a copy of the contents of the
   control point. Also disables the history collection mechanism,
   since the saved history would be incorrect on the new stack. */

#define Our_Throw(From_Pop_Return, P) do				\
{									\
  SCHEME_OBJECT Control_Point;						\
  fast SCHEME_OBJECT *To_Where, *From_Where;				\
  fast long len, valid, invalid;					\
									\
  Control_Point = (P);							\
  if ((Consistency_Check)						\
      && (OBJECT_TYPE (Control_Point) != TC_CONTROL_POINT))		\
    Microcode_Termination (TERM_BAD_STACK);				\
  len = VECTOR_LENGTH (Control_Point);					\
  invalid = ((OBJECT_DATUM (MEMORY_REF (Control_Point,			\
					STACKLET_UNUSED_LENGTH))) +	\
	     STACKLET_HEADER_SIZE);					\
  valid = ((len + 1) - invalid);					\
  CLEAR_INTERRUPT(INT_Stack_Overflow);					\
  To_Where = (Stack_Top - valid);					\
  From_Where = MEMORY_LOC (Control_Point, invalid);			\
  Stack_Check (To_Where);						\
  sp_register = To_Where;						\
  while (--valid >= 0)							\
    *To_Where++ = *From_Where++;					\
  if (Consistency_Check)						\
  {									\
    if ((To_Where != Stack_Top) ||					\
	(From_Where !=							\
	 MEMORY_LOC (Control_Point, (1 + len))))			\
      Microcode_Termination (TERM_BAD_STACK);				\
  }									\
  STACK_RESET ();							\
  if (!(From_Pop_Return))						\
  {									\
    Prev_Restore_History_Stacklet = NULL;				\
    Prev_Restore_History_Offset = 0;					\
    if ((!Valid_Fixed_Obj_Vector ()) ||					\
	(Get_Fixed_Obj_Slot (Dummy_History) == SHARP_F))		\
      history_register = Make_Dummy_History ();				\
    else								\
      history_register							\
	= OBJECT_ADDRESS (Get_Fixed_Obj_Slot (Dummy_History));		\
  }									\
  else if (Prev_Restore_History_Stacklet ==				\
	   OBJECT_ADDRESS (Control_Point))				\
    Prev_Restore_History_Stacklet = NULL;				\
} while (0)

#define Our_Throw_Part_2()

#endif /* USE_STACKLETS */
