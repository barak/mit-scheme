/* -*-C-*-

$Id: cmpint.h,v 10.8 2002/07/02 18:37:58 cph Exp $

Copyright (c) 1987-1990, 1999, 2000, 2002 Massachusetts Institute of Technology

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

/* Macros for the interface between compiled code and interpreted code. */

/* Stack Gap Operations: */

/* With_Stack_Gap opens a gap Gap_Size wide Gap_Position cells above the
 * top of the stack.  Code must push Gap_Size objects.  It executes Code
 * with the stack pointer placed so that these objects will fill the gap.
 */

#define With_Stack_Gap(Gap_Size, Gap_Position, Code)			\
{									\
  long size_to_move = (Gap_Position);					\
  SCHEME_OBJECT * Destination = (STACK_LOC (- (Gap_Size)));		\
  SCHEME_OBJECT * Saved_Destination = Destination;			\
  while ((--size_to_move) >= 0)						\
    (STACK_LOCATIVE_POP (Destination)) = (STACK_POP ());		\
  Code;									\
  sp_register = Saved_Destination;					\
}

/* Close_Stack_Gap closes a gap Gap_Size wide Gap_Position cells above the
 * top of the stack.  The contents of the gap are lost.
 */

#define Close_Stack_Gap(Gap_Size, Gap_Position, extra_code)		\
{									\
  long size_to_move;							\
  SCHEME_OBJECT *Source;						\
									\
  size_to_move = (Gap_Position);					\
  Source = (STACK_LOC (size_to_move));					\
  sp_register = (STACK_LOC ((Gap_Size) + size_to_move));		\
  extra_code;								\
  while (--size_to_move >= 0)						\
  {									\
    STACK_PUSH (STACK_LOCATIVE_PUSH (Source));				\
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
  if (STACK_REF(frame_size + CONTINUATION_RETURN_CODE) ==		\
      (MAKE_OBJECT (TC_RETURN_CODE, RC_REENTER_COMPILED_CODE)))		\
  {									\
    /* Merge compiled code segments on the stack. */			\
    Close_Stack_Gap (CONTINUATION_SIZE,					\
		     frame_size,					\
		   {							\
		     long segment_size =				\
		       (OBJECT_DATUM					\
			(STACK_REF					\
			 (CONTINUATION_EXPRESSION -			\
			  CONTINUATION_SIZE)));				\
		     last_return_code = (STACK_LOC (segment_size));	\
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
		   last_return_code = (STACK_LOC (0));			\
		   STACK_PUSH (return_to_interpreter);			\
		 });							\
  }									\
}

/* Eval interface:
   executing a compiled expression.
 */

#define execute_compiled_setup()					\
{									\
  if (STACK_REF(CONTINUATION_RETURN_CODE) ==				\
      (MAKE_OBJECT (TC_RETURN_CODE, RC_REENTER_COMPILED_CODE)))		\
  {									\
    /* Merge compiled code segments on the stack. */			\
    long segment_size;							\
									\
    Restore_Cont();							\
    segment_size = OBJECT_DATUM (Fetch_Expression());			\
    last_return_code = (STACK_LOC (segment_size));			\
    /* Undo the subproblem rotation. */					\
    Compiler_End_Subproblem();						\
  }									\
    else								\
  {									\
    /* Make a new compiled code segment on the stack. */		\
    /* History need not be hacked here. */				\
    last_return_code = (STACK_LOC (0));					\
    STACK_PUSH (return_to_interpreter);					\
  }									\
}

/* Pop return interface:
   Returning to compiled code from the interpreter.
 */

#define compiled_code_restart()						\
{									\
  long segment_size = OBJECT_DATUM (Fetch_Expression());		\
  last_return_code = (STACK_LOC (segment_size));			\
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
  long frame_size = (nslots);						\
  if ((STACK_REF (frame_size)) == return_to_interpreter)		\
  {									\
    Close_Stack_Gap(1, frame_size, {});					\
    /* Set up the current rib. */					\
    Compiler_New_Reduction ();						\
  }									\
  else									\
    { /* Make a new interpreter segment which includes this frame. */	\
      With_Stack_Gap							\
	(CONTINUATION_SIZE,						\
	 frame_size,							\
	 {								\
	   long segment_size =						\
	     (STACK_LOCATIVE_DIFFERENCE					\
	      (last_return_code, (STACK_LOC (0))));			\
	   Store_Expression (LONG_TO_UNSIGNED_FIXNUM (segment_size));	\
	   Store_Return (RC_REENTER_COMPILED_CODE);			\
	   Save_Cont ();						\
	 });								\
      /* Rotate history to a new subproblem. */				\
      Compiler_New_Subproblem ();					\
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
			   OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER)));\
}

/* Backing out of eval. */

#define execute_compiled_backout()					\
{									\
  if ((STACK_REF (0)) == return_to_interpreter)				\
  {									\
    /* Set up the current rib. */					\
    Compiler_New_Reduction ();						\
  }									\
  else									\
  {									\
    long segment_size =							\
      (STACK_LOCATIVE_DIFFERENCE (last_return_code, (STACK_LOC (0))));	\
    Store_Expression (LONG_TO_UNSIGNED_FIXNUM (segment_size));		\
    Store_Return (RC_REENTER_COMPILED_CODE);				\
    Save_Cont ();							\
    /* Rotate history to a new subproblem. */				\
    Compiler_New_Subproblem ();						\
  }									\
}

/* Backing out because of special errors or interrupts.
   The microcode has already setup a return code with a #F.
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
  segment_size =							\
    (STACK_LOCATIVE_DIFFERENCE (last_return_code, (STACK_LOC (0))));	\
  Store_Expression (LONG_TO_UNSIGNED_FIXNUM (segment_size));		\
  /* The Store_Return is a NOP, the Save_Cont is done by the code	\
     that follows. */							\
  /* Store_Return (OBJECT_DATUM (Fetch_Return ())); */			\
  /* Save_Cont (); */							\
  Compiler_New_Subproblem ();						\
}

extern long EXFUN (apply_compiled_procedure, (void));
extern long EXFUN (comp_access_restart, (void));
extern long EXFUN (comp_assignment_restart, (void));
extern long EXFUN (comp_assignment_trap_restart, (void));
extern long EXFUN (comp_cache_lookup_apply_restart, (void));
extern long EXFUN (comp_definition_restart, (void));
extern long EXFUN (comp_error_restart, (void));
extern long EXFUN (comp_interrupt_restart, (void));
extern long EXFUN (comp_link_caches_restart, (void));
extern long EXFUN (comp_lookup_apply_restart, (void));
extern long EXFUN (comp_lookup_trap_restart, (void));
extern long EXFUN (comp_op_lookup_trap_restart, (void));
extern long EXFUN (comp_reference_restart, (void));
extern long EXFUN (comp_safe_lookup_trap_restart, (void));
extern long EXFUN (comp_safe_reference_restart, (void));
extern long EXFUN (comp_unassigned_p_restart, (void));
extern long EXFUN (comp_unassigned_p_trap_restart, (void));
extern long EXFUN (comp_unbound_p_restart, (void));
extern long EXFUN (enter_compiled_expression, (void));
extern long EXFUN (return_to_compiled_code, (void));

extern SCHEME_OBJECT * EXFUN
  (compiled_entry_to_block_address, (SCHEME_OBJECT));

extern void EXFUN (compiled_entry_type, (SCHEME_OBJECT, long *));
