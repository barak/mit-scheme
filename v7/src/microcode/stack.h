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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/stack.h,v 9.23 1987/10/09 16:14:01 jinx Rel $ */

/* This file contains macros for manipulating stacks and stacklets. */

#ifdef USE_STACKLETS

/*
  Stack is made up of linked small parts, each in the heap
 */

#define Initialize_Stack()						\
{									\
  if (GC_Check(Default_Stacklet_Size))					\
  {									\
    Microcode_Termination(TERM_STACK_ALLOCATION_FAILED);		\
  }									\
  Stack_Guard = (Free + STACKLET_HEADER_SIZE);				\
  *Free = Make_Non_Pointer(TC_MANIFEST_VECTOR,				\
			   (Default_Stacklet_Size - 1));		\
  Free += Default_Stacklet_Size;					\
  Stack_Pointer = Free;							\
  Free_Stacklets = NULL;						\
  Prev_Restore_History_Stacklet = NULL;					\
  Prev_Restore_History_Offset = 0;					\
}

#define Internal_Will_Push(N)						\
{									\
  if ((Stack_Pointer - (N)) < Stack_Guard)				\
  {									\
    Export_Registers();							\
    Allocate_New_Stacklet((N));						\
    Import_Registers();							\
  }									\
}

/* No space required independent of the heap for the stacklets */

#define Stack_Allocation_Size(Stack_Blocks)	0

#define Current_Stacklet	(Stack_Guard - STACKLET_HEADER_SIZE)

/* Make the unused portion of the old stacklet invisible to garbage
 * collection. This also allows the stack pointer to be reconstructed.
 */

#define Internal_Terminate_Old_Stacklet()				\
{									\
  Current_Stacklet[STACKLET_REUSE_FLAG] = TRUTH;			\
  Current_Stacklet[STACKLET_UNUSED_LENGTH] =				\
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR,				\
		     (Stack_Pointer - Stack_Guard));			\
}

#ifdef ENABLE_DEBUGGING_TOOLS

#define Terminate_Old_Stacklet()					\
{									\
  if (Stack_Pointer < Stack_Guard)					\
  {									\
    fprintf(stderr, "\nStack_Pointer: 0x%x, Guard: 0x%x\n",		\
           Stack_Pointer, Stack_Guard);					\
    Microcode_Termination(TERM_EXIT);					\
  }									\
  Internal_Terminate_Old_Stacklet();					\
}

#else

#define Terminate_Old_Stacklet()	Internal_Terminate_Old_Stacklet()

#endif

/* Used by garbage collector to detect the end of constant space */

#define Terminate_Constant_Space(Where)					\
  *Free_Constant = Make_Pointer(TC_BROKEN_HEART, Free_Constant);	\
  Where = Free_Constant

#define Get_Current_Stacklet()						\
  Make_Pointer(TC_CONTROL_POINT, Current_Stacklet)	

#define Previous_Stack_Pointer(Where)					\
  (Nth_Vector_Loc(Where,						\
		 (STACKLET_HEADER_SIZE +				\
                  Get_Integer(Vector_Ref(Where,				\
                                         STACKLET_UNUSED_LENGTH)))))

#define Set_Current_Stacklet(Where)					\
{									\
  Pointer Our_Where;							\
									\
  Our_Where = (Where);							\
  Stack_Guard = Nth_Vector_Loc(Our_Where, STACKLET_HEADER_SIZE);	\
  Stack_Pointer = Previous_Stack_Pointer(Our_Where);			\
}

#define STACKLET_SLACK	(STACKLET_HEADER_SIZE + CONTINUATION_SIZE)

#define Default_Stacklet_Size 	(Stack_Size + STACKLET_SLACK)

#define New_Stacklet_Size(N)						\
 (STACKLET_SLACK + Stack_Size * (((N) + Stack_Size - 1) / Stack_Size))

#define Get_End_Of_Stacklet()						\
  (&(Current_Stacklet[1 + Get_Integer(Current_Stacklet[STACKLET_LENGTH])]))

#define Apply_Stacklet_Backout()					\
Will_Push((2 * CONTINUATION_SIZE) + (STACK_ENV_EXTRA_SLOTS + 2));	\
  Store_Expression(NIL);						\
  Store_Return(RC_END_OF_COMPUTATION);					\
  Save_Cont();								\
  Push(Val);								\
  Push(Previous_Stacklet);						\
  Push(STACK_FRAME_HEADER + 1);						\
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
  Pointer Old_Expression;						\
									\
  Old_Expression = Fetch_Expression();					\
  Store_Expression(Previous_Stacklet);					\
  Store_Return(RC_JOIN_STACKLETS);					\
  Save_Cont();								\
  Store_Expression(Old_Expression);					\
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
  Pointer Previous_Stacklet;						\
  Pointer *Stacklet_Top;						\
									\
  Previous_Stacklet = (Stacklet);					\
  Stacklet_Top = Current_Stacklet;					\
  Stacklet_Top[STACKLET_FREE_LIST_LINK] =				\
    ((Pointer) Free_Stacklets);						\
  Free_Stacklets = Stacklet_Top;					\
  if (!(From_Pop_Return))						\
  {									\
    Prev_Restore_History_Stacklet = NULL;				\
    Prev_Restore_History_Offset = 0;					\
  }									\
  if ((Vector_Ref(Previous_Stacklet, STACKLET_REUSE_FLAG)) == NIL)	\
  {									\
    /* We need to copy the stacklet into which we are			\
       returning.							\
     */									\
									\
    if (GC_Check(Vector_Length(Previous_Stacklet) + 1))			\
    {									\
      /* We don't have enough space to copy the stacklet. */		\
									\
      Free_Stacklets =							\
	((Pointer *) Free_Stacklets[STACKLET_FREE_LIST_LINK]);		\
      Stack_Pointer = Get_End_Of_Stacklet();				\
      Prev_Restore_History_Stacklet = NULL;				\
      Prev_Restore_History_Offset = 0

      /* Backout code inserted here by macro user */

#define Our_Throw_Part_2()						\
      Request_GC(Vector_Length(Previous_Stacklet) + 1);			\
    }									\
    else								\
    {									\
      /* There is space available to copy the stacklet. */		\
									\
      long Unused_Length;						\
      fast Used_Length;							\
      fast Pointer *Old_Stacklet_Top, *temp;				\
      Pointer *First_Continuation;					\
									\
      Old_Stacklet_Top = Get_Pointer(Previous_Stacklet);		\
      First_Continuation =						\
        Nth_Vector_Loc(Previous_Stacklet,				\
		       ((1 + Vector_Length(Previous_Stacklet)) -	\
                        CONTINUATION_SIZE));				\
      if (Old_Stacklet_Top == Prev_Restore_History_Stacklet)		\
      {									\
        Prev_Restore_History_Stacklet = NULL;				\
      }									\
      if (First_Continuation[CONTINUATION_RETURN_CODE] ==		\
	  Make_Non_Pointer(TC_RETURN_CODE, RC_JOIN_STACKLETS))		\
      {									\
	Pointer Older_Stacklet;						\
									\
	Older_Stacklet = First_Continuation[CONTINUATION_EXPRESSION];	\
	Vector_Set(Older_Stacklet, STACKLET_REUSE_FLAG, NIL);		\
      }									\
									\
      temp = Free;							\
      Stack_Guard = &(temp[STACKLET_HEADER_SIZE]);			\
      temp[STACKLET_LENGTH] = Old_Stacklet_Top[STACKLET_LENGTH];	\
      Unused_Length =							\
	Get_Integer(Old_Stacklet_Top[STACKLET_UNUSED_LENGTH]) +		\
        STACKLET_HEADER_SIZE;						\
      temp += Unused_Length;						\
      Stack_Pointer = temp;						\
      Used_Length =							\
        (Get_Integer(Old_Stacklet_Top[STACKLET_LENGTH]) -		\
         Unused_Length) + 1;						\
      Old_Stacklet_Top += Unused_Length;				\
      while (--Used_Length >= 0)					\
      {									\
	*temp++ = *Old_Stacklet_Top++;					\
      }									\
      Free = temp;							\
    }									\
  }									\
  else									\
  {									\
    /* No need to copy the stacklet we are going into */		\
									\
    if (Get_Pointer(Previous_Stacklet)==				\
        Prev_Restore_History_Stacklet)					\
    {									\
      Prev_Restore_History_Stacklet = NULL;				\
    }									\
    Set_Current_Stacklet(Previous_Stacklet);				\
  }									\
}
			  
#else /* not USE_STACKLETS */

/*
  Full size stack in a statically allocated area
 */

#define Stack_Check(P)							\
do									\
{									\
  if ((P) <= Stack_Guard)						\
    {									\
      if ((P) <= Absolute_Stack_Base)					\
      {									\
	Microcode_Termination (TERM_STACK_OVERFLOW);			\
      }									\
      Request_Interrupt (INT_Stack_Overflow);				\
    }									\
} while (0)

#define Internal_Will_Push(N)	Stack_Check(Stack_Pointer - (N))

#define Stack_Allocation_Size(Stack_Blocks) (Stack_Blocks)

#define Terminate_Old_Stacklet()

/* Used by garbage collector to detect the end of constant space, and to
   skip over the gap between constant space and the stack. */

#define Terminate_Constant_Space(Where)					\
{									\
  *Free_Constant =							\
    Make_Non_Pointer (TC_MANIFEST_NM_VECTOR,				\
		      ((Stack_Pointer - Free_Constant) - 1));		\
  *Stack_Top = Make_Pointer (TC_BROKEN_HEART, Stack_Top);		\
  Where = Stack_Top;							\
}

#define Get_Current_Stacklet()	NIL

#define Set_Current_Stacklet(Where) {}

#define Previous_Stack_Pointer(Where)					\
(Nth_Vector_Loc (Where,							\
		 (STACKLET_HEADER_SIZE +				\
		  Get_Integer (Vector_Ref (Where,			\
					   STACKLET_UNUSED_LENGTH)))))

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

#define Our_Throw(From_Pop_Return, P)					\
{									\
  Pointer Control_Point;						\
  fast Pointer *To_Where, *From_Where;					\
  fast long len, valid, invalid;					\
									\
  Control_Point = (P);							\
  if (Consistency_Check)						\
  {									\
    if (OBJECT_TYPE(Control_Point) != TC_CONTROL_POINT)			\
    {									\
      Microcode_Termination (TERM_BAD_STACK);				\
    }									\
  }									\
  len = Vector_Length (Control_Point);					\
  invalid = ((Get_Integer (Vector_Ref (Control_Point,			\
				     STACKLET_UNUSED_LENGTH))) +	\
	     STACKLET_HEADER_SIZE);					\
  valid = ((len + 1) - invalid);					\
  IntCode &= (~ INT_Stack_Overflow);					\
  To_Where = (Stack_Top - valid);					\
  From_Where = Nth_Vector_Loc (Control_Point, invalid);			\
  Stack_Check (To_Where);						\
  Stack_Pointer = To_Where;						\
  while (--valid >= 0)							\
  {									\
    *To_Where++ = *From_Where++;					\
  }									\
									\
  if (Consistency_Check)						\
  {									\
    if ((To_Where != Stack_Top) ||					\
	(From_Where !=							\
	 Nth_Vector_Loc (Control_Point, (1 + len))))			\
    {									\
      Microcode_Termination (TERM_BAD_STACK);				\
    }									\
  }									\
  if (!(From_Pop_Return))						\
  {									\
    Prev_Restore_History_Stacklet = NULL;				\
    Prev_Restore_History_Offset = 0;					\
    if ((!Valid_Fixed_Obj_Vector ()) ||					\
	(Get_Fixed_Obj_Slot (Dummy_History) == NIL))			\
    {									\
      History = Make_Dummy_History ();					\
    }									\
    else								\
    {									\
      History = Get_Pointer (Get_Fixed_Obj_Slot (Dummy_History));	\
    }									\
  }									\
  else if (Prev_Restore_History_Stacklet ==				\
	   Get_Pointer (Control_Point))					\
  {									\
    Prev_Restore_History_Stacklet = NULL;				\
  }									\
}

#define Our_Throw_Part_2()

#endif /* USE_STACKLETS */
