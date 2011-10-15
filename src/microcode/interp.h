/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* Definitions used by the interpreter and some utilities. */

#ifndef SCM_INTERP_H
#define SCM_INTERP_H 1

#include "object.h"
#include "stack.h"

/* Note: SAVE_CONT must match the definitions in sdata.h */

#define SAVE_CONT() do							\
{									\
  PUSH_EXP ();								\
  PUSH_RET ();								\
} while (0)

#define RESTORE_CONT() do						\
{									\
  POP_RET ();								\
  POP_EXP ();								\
} while (0)

#define CONT_RC(offset) (OBJECT_DATUM (CONT_RET (offset)))
#define CONT_RET(offset) (STACK_REF ((offset) + CONTINUATION_RETURN_CODE))
#define CONT_EXP(offset) (STACK_REF ((offset) + CONTINUATION_EXPRESSION))

#define PUSH_APPLY_FRAME_HEADER(n_args)					\
  STACK_PUSH (MAKE_OBJECT (0, ((n_args) + 1)))

#define POP_APPLY_FRAME_HEADER() APPLY_FRAME_HEADER_N_ARGS (STACK_POP ())
#define APPLY_FRAME_HEADER_N_ARGS(header) ((OBJECT_DATUM (header)) - 1)
#define APPLY_FRAME_SIZE() (OBJECT_DATUM (STACK_REF (0)))
#define APPLY_FRAME_N_ARGS() (APPLY_FRAME_HEADER_N_ARGS (STACK_REF (0)))
#define APPLY_FRAME_PROCEDURE() (STACK_REF (1))
#define APPLY_FRAME_ARGS() (STACK_LOC (2))

#define CHECK_RETURN_CODE(code, offset)					\
  ((CONT_RET (offset)) == (MAKE_RETURN_CODE (code)))

/* Saving history is required for C_call_scheme to work correctly
   because the recursive call to Interpret() can rotate the history.  */

#define APPLY_PRIMITIVE_FROM_INTERPRETER(primitive) do			\
{									\
  SCHEME_OBJECT * APFI_saved_history = history_register;		\
  PRIMITIVE_APPLY (primitive);						\
  history_register = APFI_saved_history;				\
} while (0)

/* Stack manipulation */

#ifdef ENABLE_DEBUGGING_TOOLS

#define Will_Push(N)							\
{									\
  SCHEME_OBJECT * Will_Push_Limit;					\
									\
  STACK_CHECK (N);							\
  Will_Push_Limit = (STACK_LOC (- (N)))

#define Pushed()							\
  if (STACK_LOCATIVE_LESS_P (stack_pointer, Will_Push_Limit))		\
    {									\
      Stack_Death ();							\
    }									\
}

#else

#define Will_Push(N) STACK_CHECK (N)
#define Pushed()

#endif

/* N in Will_Eventually_Push is the maximum contiguous (single return
   code) amount that this operation may take.  On the average case it
   may use less.  M in Finished_Eventual_Pushing is the amount not yet
   pushed.  */

#define Will_Eventually_Push(N) STACK_CHECK (N)
#define Finished_Eventual_Pushing(M)

/* Primitive utility macros */

#ifndef ENABLE_DEBUGGING_TOOLS
#  define PRIMITIVE_APPLY PRIMITIVE_APPLY_INTERNAL
#else
   extern void primitive_apply_internal (SCHEME_OBJECT);
#  define PRIMITIVE_APPLY primitive_apply_internal
#endif

#define PRIMITIVE_APPLY_INTERNAL(primitive) do				\
{									\
  void * PRIMITIVE_APPLY_INTERNAL_position = dstack_position;		\
  SET_PRIMITIVE (primitive);						\
  Free_primitive = Free;						\
  SET_VAL								\
    ((* (Primitive_Procedure_Table [PRIMITIVE_NUMBER (primitive)]))	\
     ());								\
  /* If the primitive failed to unwind the dynamic stack, lose. */	\
  if (PRIMITIVE_APPLY_INTERNAL_position != dstack_position)		\
    {									\
      outf_fatal ("\nPrimitive slipped the dynamic stack: %s\n",	\
		  (PRIMITIVE_NAME (primitive)));			\
      Microcode_Termination (TERM_EXIT);				\
    }									\
  Free_primitive = 0;							\
  SET_PRIMITIVE (SHARP_F);						\
} while (0)

#define POP_PRIMITIVE_FRAME(arity) (stack_pointer = (STACK_LOC (arity)))

typedef struct interpreter_state_s * interpreter_state_t;

struct interpreter_state_s
{
  interpreter_state_t previous_state;
  unsigned int nesting_level;
  void * dstack_position;
  jmp_buf catch_env;
  int throw_argument;
};

#define interpreter_catch_dstack_position interpreter_state->dstack_position
#define interpreter_catch_env interpreter_state->catch_env
#define interpreter_throw_argument interpreter_state->throw_argument
#define NULL_INTERPRETER_STATE ((interpreter_state_t) 0)

extern void abort_to_interpreter (int) NORETURN;
extern int abort_to_interpreter_argument (void);

extern interpreter_state_t interpreter_state;
extern long prim_apply_error_code;
extern void bind_interpreter_state (interpreter_state_t);
extern void unbind_interpreter_state (interpreter_state_t);

#endif /* not SCM_INTERP_H */
