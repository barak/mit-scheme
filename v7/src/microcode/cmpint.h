/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpint.h,v 10.2 1988/08/15 20:43:41 cph Rel $
 *
 * Macros for the interface between compiled code and interpreted code.
 *
 */

/* Stack Gap Operations: */

/* With_Stack_Gap opens a gap Gap_Size wide Gap_Position cells above the
 * top of the stack.  Code must push Gap_Size objects.  It executes Code
 * with the stack pointer placed so that these objects will fill the gap.
 */

#define With_Stack_Gap(Gap_Size, Gap_Position, Code)			\
{									\
  Pointer *Saved_Destination;						\
  fast Pointer *Destination;						\
  fast long size_to_move;						\
									\
  size_to_move = (Gap_Position);					\
  Destination = Simulate_Pushing(Gap_Size);				\
  Saved_Destination = Destination;					\
  while (--size_to_move >= 0)						\
  {									\
    Pop_Into(Destination, Pop());					\
  }									\
  Code;									\
  Stack_Pointer = Saved_Destination;					\
}

/* Close_Stack_Gap closes a gap Gap_Size wide Gap_Position cells above the 
 * top of the stack.  The contents of the gap are lost.
 */

#define Close_Stack_Gap(Gap_Size, Gap_Position, extra_code)		\
{									\
  fast long size_to_move;						\
  fast Pointer *Source;							\
									\
  size_to_move = (Gap_Position);					\
  Source = Simulate_Popping(size_to_move);				\
  Stack_Pointer = Simulate_Popping((Gap_Size) + size_to_move);		\
  extra_code;								\
  while (--size_to_move >= 0)						\
  {									\
    Push(Push_From(Source));						\
  }									\
}

/* Going from interpreted code to compiled code */

/* Tail recursion is handled as follows:
   if the return code is `reenter_compiled_code', it is discarded,
   and the two contiguous interpreter segments on the stack are
   merged.
 */

/* Apply interface:
   calling a compiled procedure with a frame nslots long.
 */

#define apply_compiled_setup(nslots)					\
{									\
  long frame_size;							\
									\
  frame_size = (nslots);						\
  if (Stack_Ref(frame_size + CONTINUATION_RETURN_CODE) ==		\
      (Make_Non_Pointer(TC_RETURN_CODE, RC_REENTER_COMPILED_CODE)))	\
  {									\
    /* Merge compiled code segments on the stack. */			\
    Close_Stack_Gap (CONTINUATION_SIZE,					\
		     frame_size,					\
		   {							\
		     long segment_size;					\
									\
		     segment_size =					\
		       OBJECT_DATUM(Stack_Ref(CONTINUATION_EXPRESSION -	\
					      CONTINUATION_SIZE));	\
		     last_return_code = Simulate_Popping(segment_size);	\
		   });							\
    /* Undo the subproblem rotation. */					\
    Compiler_End_Subproblem();						\
  }									\
  else									\
  {									\
    /* Make a new compiled code segment which includes this frame. */	\
    /* History need not be hacked here. */				\
    With_Stack_Gap(1,							\
		   frame_size,						\
		 {							\
		   last_return_code = &Top_Of_Stack();			\
		   Push(return_to_interpreter);				\
		 });							\
  }									\
}

/* Eval interface:
   executing a compiled expression.
 */

#define execute_compiled_setup()					\
{									\
  if (Stack_Ref(CONTINUATION_RETURN_CODE) ==				\
      (Make_Non_Pointer(TC_RETURN_CODE, RC_REENTER_COMPILED_CODE)))	\
  {									\
    /* Merge compiled code segments on the stack. */			\
    long segment_size;							\
									\
    Restore_Cont();							\
    segment_size = OBJECT_DATUM(Fetch_Expression());			\
    last_return_code = Simulate_Popping(segment_size);			\
    /* Undo the subproblem rotation. */					\
    Compiler_End_Subproblem();						\
  }									\
    else								\
  {									\
    /* Make a new compiled code segment on the stack. */		\
    /* History need not be hacked here. */				\
    last_return_code = &Top_Of_Stack();					\
    Push(return_to_interpreter);					\
  }									\
}

/* Pop return interface:
   Returning to compiled code from the interpreter.
 */
   
#define compiled_code_restart()						\
{									\
  long segment_size;							\
									\
  segment_size = Datum(Fetch_Expression());				\
  last_return_code = Simulate_Popping(segment_size);			\
  /* Undo the subproblem rotation. */					\
  Compiler_End_Subproblem();						\
}

/* Going from compiled code to interpreted code */

/* Tail recursion is handled in the following way:
   if the return address is `return_to_interpreter', it is discarded,
   and the two contiguous interpreter segments on the stack are
   merged.
 */

/* Apply interface:
   calling an interpreted procedure (or unsafe primitive)
   with a frame nslots long.
 */

#define compiler_apply_procedure(nslots)				\
{									\
  long frame_size;							\
									\
  frame_size = (nslots);						\
  if (Stack_Ref( frame_size) == return_to_interpreter)			\
  {									\
    Close_Stack_Gap(1, frame_size, {});					\
    /* Set up the current rib. */					\
    Compiler_New_Reduction();						\
  }									\
  else									\
    { /* Make a new interpreter segment which includes this frame. */	\
      With_Stack_Gap(CONTINUATION_SIZE,					\
		     frame_size,					\
		   {							\
		     long segment_size;					\
									\
		     segment_size = Stack_Distance(last_return_code);	\
		     Store_Expression(Make_Unsigned_Fixnum(segment_size)); \
		     Store_Return(RC_REENTER_COMPILED_CODE);		\
		     Save_Cont();					\
		   });							\
      /* Rotate history to a new subproblem. */				\
      Compiler_New_Subproblem();					\
    }									\
}

/* Pop Return interface:
   returning to the interpreter from compiled code.
   Nothing needs to be done at this time.
 */

#define compiled_code_done()

/* Various handlers for backing out of compiled code. */

/* Backing out of apply. */

#define apply_compiled_backout()					\
{									\
  compiler_apply_procedure(STACK_ENV_EXTRA_SLOTS +			\
			   Get_Integer( Stack_Ref( STACK_ENV_HEADER)));	\
}

/* Backing out of eval. */

#define execute_compiled_backout()					\
{									\
  if (Top_Of_Stack() == return_to_interpreter)				\
  {									\
    Simulate_Popping(1);						\
    /* Set up the current rib. */					\
    Compiler_New_Reduction();						\
  }									\
  else									\
  {									\
    long segment_size;							\
									\
    segment_size = Stack_Distance(last_return_code);			\
    Store_Expression(Make_Unsigned_Fixnum(segment_size));		\
    Store_Return(RC_REENTER_COMPILED_CODE);				\
    Save_Cont();							\
    /* Rotate history to a new subproblem. */				\
    Compiler_New_Subproblem();						\
  }									\
}

/* Backing out because of special errors or interrupts.
   The microcode has already setup a return code with a NIL.
   No tail recursion in this case.
   ***
       Is the history manipulation correct?
       Does Microcode_Error do something special?
   ***
 */

#define compiled_error_backout()					\
{									\
  long segment_size;							\
									\
  Restore_Cont();							\
  segment_size = Stack_Distance(last_return_code);			\
  Store_Expression(Make_Unsigned_Fixnum(segment_size));			\
  /* The Store_Return is a NOP, the Save_Cont is done by the code	\
     that follows.							\
   */									\
  /* Store_Return(Datum(Fetch_Return())); */				\
  /* Save_Cont(); */							\
  Compiler_New_Subproblem();						\
}
