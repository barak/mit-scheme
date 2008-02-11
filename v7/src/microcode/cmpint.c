/* -*-C-*-

$Id: cmpint.c,v 1.112 2008/02/11 21:07:21 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* Compiled-code interface */

/* Some of the cmpintmd/FOO.h files use this macro to alter their
   behavior when included here.  */
#define IN_CMPINT_C 1

#include "scheme.h"
#include "prims.h"
#include "lookup.h"
#include "trap.h"
#include "history.h"
#include "cmpgc.h"

/* Two special classes of procedures are used in this file:

   Scheme interface entries.  These procedures are called from C and
   ultimately invoke 'ENTER_SCHEME' to enter compiled code, or return
   a status code.

   Scheme interface utilities.  These procedures are called from the
   Scheme interface and perform tasks that the compiler does not code
   inline.  They are referenced from compiled Scheme code by index,
   and the assembly language interface fetches them from an array.
   They are defined with 'SCHEME_UTILITY_n' for some 'n', and
   ultimately invoke either 'RETURN_TO_SCHEME' (in the normal case) or
   'RETURN_TO_C' (in the error case).  */

typedef long cache_handler_t (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);

typedef struct
{
  SCHEME_OBJECT * block_address;
  SCHEME_OBJECT * scan;
  unsigned long n_sections;
  insn_t * return_address;
  unsigned long n_linked_sections;
  SCHEME_OBJECT * scan0;
  linkage_section_type_t type;
  unsigned long n_entries;
  unsigned long n_linked_entries;
} link_cc_state_t;

/* Ways to bypass the interpreter */
typedef enum
{
  REFLECT_CODE_INTERNAL_APPLY,
  REFLECT_CODE_RESTORE_INTERRUPT_MASK,
  REFLECT_CODE_STACK_MARKER,
  REFLECT_CODE_CC_BKPT
} reflect_code_t;

#define PUSH_REFLECTION(code) do					\
{									\
  STACK_PUSH (ULONG_TO_FIXNUM (code));					\
  STACK_PUSH (reflect_to_interface);					\
} while (false)

typedef enum
{
  TRAMPOLINE_K_RETURN_TO_INTERPRETER,
  TRAMPOLINE_K_APPLY,
  TRAMPOLINE_K_ARITY,		/* unused */
  TRAMPOLINE_K_ENTITY,		/* unused */
  TRAMPOLINE_K_INTERPRETED,	/* unused */
  TRAMPOLINE_K_LEXPR_PRIMITIVE,
  TRAMPOLINE_K_PRIMITIVE,
  TRAMPOLINE_K_LOOKUP,
  TRAMPOLINE_K_1_0,
  TRAMPOLINE_K_2_1,
  TRAMPOLINE_K_2_0,
  TRAMPOLINE_K_3_2,
  TRAMPOLINE_K_3_1,
  TRAMPOLINE_K_3_0,
  TRAMPOLINE_K_4_3,
  TRAMPOLINE_K_4_2,
  TRAMPOLINE_K_4_1,
  TRAMPOLINE_K_4_0,
  TRAMPOLINE_K_REFLECT_TO_INTERFACE = 0x3A
} trampoline_type_t;

#define TC_TRAMPOLINE_HEADER TC_FIXNUM
#define TRAMPOLINE_TABLE_SIZE 4

static trampoline_type_t
trampoline_arity_table [TRAMPOLINE_TABLE_SIZE * TRAMPOLINE_TABLE_SIZE] =
{
  TRAMPOLINE_K_1_0,		/* 1_0 */
  TRAMPOLINE_K_APPLY,		/* 1_1 should not get here */
  TRAMPOLINE_K_APPLY,		/* 1_2 should not get here */
  TRAMPOLINE_K_APPLY,		/* 1_3 should not get here */
  TRAMPOLINE_K_2_0,		/* 2_0 */
  TRAMPOLINE_K_2_1,		/* 2_1 */
  TRAMPOLINE_K_APPLY,		/* 2_2 should not get here */
  TRAMPOLINE_K_APPLY,		/* 2_3 should not get here */
  TRAMPOLINE_K_3_0,		/* 3_0 */
  TRAMPOLINE_K_3_1,		/* 3_1 */
  TRAMPOLINE_K_3_2,		/* 3_2 */
  TRAMPOLINE_K_APPLY,		/* 3_3 should not get here */
  TRAMPOLINE_K_4_0,		/* 4_0 */
  TRAMPOLINE_K_4_1,		/* 4_1 */
  TRAMPOLINE_K_4_2,		/* 4_2 */
  TRAMPOLINE_K_4_3		/* 4_3 */
};

cc_arch_t compiler_processor_type;
unsigned int compiler_interface_version;

SCHEME_OBJECT compiler_utilities;
SCHEME_OBJECT return_to_interpreter;
SCHEME_OBJECT reflect_to_interface;

static bool linking_cc_block_p = 0;

static SCHEME_OBJECT make_compiler_utilities (void);
static void open_stack_gap (unsigned long, unsigned long);
static void close_stack_gap (unsigned long, unsigned long);
static void recover_from_apply_error (SCHEME_OBJECT, unsigned long);
static void setup_compiled_invocation_from_primitive 
  (SCHEME_OBJECT, unsigned long);
static long link_remaining_sections (link_cc_state_t *);
static void start_linking_cc_block (void);
static void end_linking_cc_block (link_cc_state_t *);
static void abort_linking_cc_block (void *);
static void update_cache_after_link (link_cc_state_t *);
static void start_linking_section (link_cc_state_t *);
static long link_section (link_cc_state_t *);
static bool link_section_handler
  (linkage_section_type_t, cache_handler_t **, bool *);
static void back_out_of_link_section (link_cc_state_t *);
static void restore_link_cc_state (link_cc_state_t *);
static long setup_compiled_invocation (SCHEME_OBJECT, unsigned long);
static long setup_lexpr_invocation
  (SCHEME_OBJECT, unsigned long, unsigned long);
static bool open_gap (unsigned long, unsigned long);
static bool cc_block_address_closure_p (SCHEME_OBJECT *);
static void write_uuo_link (SCHEME_OBJECT, SCHEME_OBJECT *);
static long make_fake_uuo_link (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
static long make_trampoline
  (SCHEME_OBJECT *, cc_entry_type_t *, trampoline_type_t, unsigned int, ...);
static void make_trampoline_headers
  (unsigned long, unsigned long,
   SCHEME_OBJECT *, SCHEME_OBJECT *, unsigned long *);
static bool fill_trampoline
  (SCHEME_OBJECT *, unsigned long, cc_entry_type_t *, trampoline_type_t);
static long make_redirection_trampoline
  (SCHEME_OBJECT *, trampoline_type_t, SCHEME_OBJECT);
static long make_apply_trampoline
  (SCHEME_OBJECT *, trampoline_type_t, SCHEME_OBJECT, unsigned long);

#ifndef UTILITY_RESULT_DEFINED
#ifdef CMPINT_USE_STRUCS

#ifdef C_FUNC_PTR_IS_CLOSURE
   typedef insn_t * c_func_t;
#else
   typedef void c_func_t (void);
/* From trunk, but may not be needed: */
#  if 0
#    ifdef __OPEN_WATCOM_14__
#      define REFENTRY(name) ((void *) name)
#    else
#      define REFENTRY(name) ((c_func_t *) name)
#    endif
#    define VARENTRY(name) c_func_t * name
#    define EXTENTRY(name) extern c_func_t ASM_ENTRY_POINT (name)
#  endif
#endif

#define RETURN_TO_C(code) do						\
{									\
  (DSU_result->interface_dispatch) = interface_to_C;			\
  ((DSU_result->extra) . code_to_interpreter) = (code);			\
  return;								\
} while (false)

#define RETURN_TO_SCHEME(ep) do						\
{									\
  (DSU_result->interface_dispatch) = interface_to_scheme;		\
  ((DSU_result->extra) . entry_point) = (ep);				\
  return;								\
} while (false)

extern c_func_t ASM_ENTRY_POINT (interface_to_C);
extern c_func_t ASM_ENTRY_POINT (interface_to_scheme);

#define ENTER_SCHEME(ep) return (C_to_interface (ep))
extern long ASM_ENTRY_POINT (C_to_interface) (insn_t *);

#else /* !CMPINT_USE_STRUCS */

#define RETURN_TO_C(code) do						\
{									\
  (*DSU_result) = interface_to_C_hook;					\
  C_return_value = (code);						\
  return;								\
} while (false)

#define RETURN_TO_SCHEME(ep) do						\
{									\
  (*DSU_result) = (ep);							\
  return;								\
} while (false)

#define ENTER_SCHEME(ep) do						\
{									\
  C_to_interface (ep);							\
  return (C_return_value);						\
} while (false)

extern utility_result_t interface_to_C_hook;
extern void ASM_ENTRY_POINT (C_to_interface) (insn_t *);
long C_return_value;

#endif /* !CMPINT_USE_STRUCS */
#endif /* !UTILITY_RESULT_DEFINED */

#define JUMP_TO_CC_ENTRY(entry) ENTER_SCHEME (CC_ENTRY_ADDRESS (entry))

#ifndef COMPILER_REGBLOCK_N_FIXED
#  define COMPILER_REGBLOCK_N_FIXED REGBLOCK_MINIMUM_LENGTH
#endif

#ifndef COMPILER_REGBLOCK_N_TEMPS
#  define COMPILER_REGBLOCK_N_TEMPS 0
#endif

#ifndef COMPILER_TEMP_SIZE
#  define COMPILER_TEMP_SIZE ((sizeof (double)) / (sizeof (SCHEME_OBJECT)))
#endif

#ifndef COMPILER_REGBLOCK_EXTRA_SIZE
#  define COMPILER_REGBLOCK_EXTRA_SIZE 0
#endif

#if (REGBLOCK_MINIMUM_LENGTH > COMPILER_REGBLOCK_N_FIXED)
#  include "ERROR: cmpint.c and const.h disagree on REGBLOCK_MINIMUM_LENGTH!"
#endif

#define REGBLOCK_LENGTH							\
  (COMPILER_REGBLOCK_N_FIXED						\
   + (COMPILER_REGBLOCK_N_TEMPS * COMPILER_TEMP_SIZE)			\
   + COMPILER_REGBLOCK_EXTRA_SIZE)

#ifndef REGBLOCK_ALLOCATED_BY_INTERFACE
  SCHEME_OBJECT Registers [REGBLOCK_LENGTH];
#endif

#ifndef ASM_RESET_HOOK
#  define ASM_RESET_HOOK() do {} while (false)
#endif

#define SAVE_LAST_RETURN_CODE(code) do					\
{									\
  {									\
    long SLRC_offset							\
      = (STACK_LOCATIVE_DIFFERENCE (stack_pointer, last_return_code));	\
    assert (SLRC_offset > 0);						\
    STACK_PUSH (LONG_TO_FIXNUM (SLRC_offset));				\
  }									\
  PUSH_RC (code);							\
  COMPILER_NEW_SUBPROBLEM ();						\
} while (false)

#define RESTORE_LAST_RETURN_CODE() do					\
{									\
  last_return_code = (STACK_LOC (FIXNUM_TO_ULONG (GET_EXP)));		\
  CHECK_LAST_RETURN_CODE ();						\
  COMPILER_END_SUBPROBLEM ();						\
} while (false)

#define CHECK_LAST_RETURN_CODE() do					\
{									\
  assert								\
    (RETURN_CODE_P							\
     (STACK_LOCATIVE_REFERENCE (last_return_code,			\
				CONTINUATION_RETURN_CODE)));		\
} while (false)

/* Initialization */

void
compiler_initialize (bool fasl_p)
{
  /* Called when scheme started.  */
  SET_PRIMITIVE (SHARP_F);
  compiler_processor_type = COMPILER_PROCESSOR_TYPE;
  compiler_interface_version = COMPILER_INTERFACE_VERSION;
  if (fasl_p)
    compiler_reset (make_compiler_utilities ());
  else
    {
      /* Delay until after band-load, when compiler_reset will be invoked. */
      compiler_utilities = SHARP_F;
      return_to_interpreter = SHARP_F;
#ifdef CC_ARCH_INITIALIZE
      CC_ARCH_INITIALIZE ();
#endif
    }
}

#define COMPILER_UTILITIES_HEADERS(h1, h2, n)				\
  make_trampoline_headers (2, 2, (h1), (h2), (n))

static SCHEME_OBJECT
make_compiler_utilities (void)
{
  SCHEME_OBJECT h1;
  SCHEME_OBJECT h2;
  unsigned long n_words;
  SCHEME_OBJECT * block;

  COMPILER_UTILITIES_HEADERS ((&h1), (&h2), (&n_words));
  if (GC_NEEDED_P (n_words))
    {
      outf_fatal ("Can't allocate compiler_utilities.\n");
      Microcode_Termination (TERM_NO_SPACE);
    }
  h1 = (OBJECT_NEW_TYPE (TC_MANIFEST_VECTOR, h1));

  block = Free;
  Free += n_words;
  (block[0]) = h1;
  (block[1]) = h2;

  {
    cc_entry_type_t cet;
    make_cc_entry_type ((&cet), CET_RETURN_TO_INTERPRETER);
    if ((fill_trampoline (block, 0, (&cet), TRAMPOLINE_K_RETURN_TO_INTERPRETER))
	||
	(fill_trampoline (block, 1, (&cet), TRAMPOLINE_K_REFLECT_TO_INTERFACE)))
      {
	outf_fatal ("\nError in make_compiler_utilities\n");
	Microcode_Termination (TERM_COMPILER_DEATH);
	/*NOTREACHED*/
      }
  }

  /* These entries are no longer used, but are provided for
     compatibility with the previous structure.  */
  {
    SCHEME_OBJECT * store = (trampoline_storage (block));
    (store[0]) = ((trampoline_entry_addr (block, 0)) - ((insn_t *) block));
    (store[1]) = ((trampoline_entry_addr (block, 1)) - ((insn_t *) block));
  }

  block = (copy_to_constant_space (block, n_words));
  return (MAKE_CC_BLOCK (block));
}

void
compiler_reset (SCHEME_OBJECT new_block)
{
  /* Called after a disk restore */
  SCHEME_OBJECT h1;
  SCHEME_OBJECT h2;
  unsigned long n_words;
  SCHEME_OBJECT * nbp;

  COMPILER_UTILITIES_HEADERS ((&h1), (&h2), (&n_words));
  h1 = (OBJECT_NEW_TYPE (TC_MANIFEST_VECTOR, h1));
  if (! ((CC_BLOCK_P (new_block))
	 && ((MEMORY_REF (new_block, 0)) == h1)
	 && ((MEMORY_REF (new_block, 1)) == h2)))
    {
      outf_fatal ("\nThe world image being restored is incompatible"
		  " with this microcode.\n");
      Microcode_Termination (TERM_COMPILER_DEATH);
      /*NOTREACHED*/
    }

  nbp = (OBJECT_ADDRESS (new_block));
  compiler_utilities = new_block;
  return_to_interpreter = (MAKE_CC_ENTRY (trampoline_entry_addr (nbp, 0)));
  reflect_to_interface = (MAKE_CC_ENTRY (trampoline_entry_addr (nbp, 1)));
  SET_CLOSURE_FREE (0);
  SET_CLOSURE_SPACE (0);
  SET_REFLECTOR (reflect_to_interface);

  ASM_RESET_HOOK ();
}

/* Main compiled-code entry points */

#define DEFINE_SCHEME_ENTRY(pname) long pname (void)

DEFINE_SCHEME_ENTRY (enter_compiled_expression)
{
  SCHEME_OBJECT entry = GET_EXP;
  {
    cc_entry_type_t cet;
    if (read_cc_entry_type ((&cet), (CC_ENTRY_ADDRESS (entry))))
      return (ERR_COMPILED_CODE_ERROR);
    if (cet.marker != CET_EXPRESSION)
      {
	/* evaluate to self */
	SET_VAL (entry);
	return (PRIM_DONE);
      }
  }
  guarantee_cc_return (0);
  JUMP_TO_CC_ENTRY (entry);
}

DEFINE_SCHEME_ENTRY (apply_compiled_procedure)
{
  unsigned long n_args = (POP_APPLY_FRAME_HEADER ());
  SCHEME_OBJECT procedure = (STACK_POP ());
  long code = (setup_compiled_invocation (procedure, n_args));
  if (code != PRIM_DONE)
    return (code);
  JUMP_TO_CC_ENTRY (procedure);
}

DEFINE_SCHEME_ENTRY (return_to_compiled_code)
{
  RESTORE_LAST_RETURN_CODE ();
  {
    SCHEME_OBJECT cont = (STACK_POP ());
    {
      cc_entry_type_t cet;
      if ((read_cc_entry_type ((&cet), (CC_ENTRY_ADDRESS (cont))))
	  || (! ((cet.marker == CET_CONTINUATION)
		 || (cet.marker == CET_INTERNAL_CONTINUATION)
		 || (cet.marker == CET_RETURN_TO_INTERPRETER))))
	{
	  STACK_PUSH (cont);
	  SAVE_CONT ();
	  return (ERR_INAPPLICABLE_OBJECT);
	}
    }
    JUMP_TO_CC_ENTRY (cont);
  }
}

void
guarantee_cc_return (unsigned long offset)
{
  if (CC_ENTRY_P (STACK_REF (offset)))
    return;
  assert (RETURN_CODE_P (CONT_RET (offset)));
  if (CHECK_RETURN_CODE (RC_REENTER_COMPILED_CODE, offset))
    {
      unsigned long lrc = (FIXNUM_TO_ULONG (CONT_EXP (offset)));
      close_stack_gap (offset, CONTINUATION_SIZE);
      last_return_code = (STACK_LOC (offset + lrc));
      CHECK_LAST_RETURN_CODE ();
      COMPILER_END_SUBPROBLEM ();
    }
  else
    {
      last_return_code = (STACK_LOC (offset));
      CHECK_LAST_RETURN_CODE ();
      open_stack_gap (offset, 1);
      (STACK_REF (offset)) = return_to_interpreter;
    }
}

void
guarantee_interp_return (void)
{
  unsigned long offset = (1 + (APPLY_FRAME_SIZE ()));
  if (RETURN_CODE_P (CONT_RET (offset)))
    return;
  assert (CC_ENTRY_P (STACK_REF (offset)));
  if ((STACK_REF (offset)) == return_to_interpreter)
    {
      assert (RETURN_CODE_P (CONT_RET (offset + 1)));
      close_stack_gap (offset, 1);
      COMPILER_NEW_REDUCTION ();
    }
  else
    {
      open_stack_gap (offset, CONTINUATION_SIZE);
      {
	SCHEME_OBJECT * sp = stack_pointer;
	stack_pointer = (STACK_LOC (offset + CONTINUATION_SIZE));
	SAVE_LAST_RETURN_CODE (RC_REENTER_COMPILED_CODE);
	stack_pointer = sp;
      }
    }
}

static void
open_stack_gap (unsigned long offset, unsigned long n_words)
{
  SCHEME_OBJECT * scan_from = (STACK_LOC (0));
  SCHEME_OBJECT * scan_end = (STACK_LOC (offset));
  SCHEME_OBJECT * scan_to = (STACK_LOC (-n_words));
  while (scan_from != scan_end)
    (STACK_LOCATIVE_POP (scan_to)) = (STACK_LOCATIVE_POP (scan_from));
  stack_pointer = (STACK_LOC (-n_words));
}

static void
close_stack_gap (unsigned long offset, unsigned long n_words)
{
  SCHEME_OBJECT * scan_from = (STACK_LOC (offset));
  SCHEME_OBJECT * scan_end = (STACK_LOC (0));
  SCHEME_OBJECT * scan_to = (STACK_LOC (offset + n_words));
  while (scan_from != scan_end)
    (STACK_LOCATIVE_PUSH (scan_to)) = (STACK_LOCATIVE_PUSH (scan_from));
  stack_pointer = (STACK_LOC (n_words));
}

static void
recover_from_apply_error (SCHEME_OBJECT procedure, unsigned long n_args)
{
  STACK_PUSH (procedure);
  PUSH_APPLY_FRAME_HEADER (n_args);
  guarantee_interp_return ();
}

void
compiled_with_interrupt_mask (unsigned long old_mask,
			      SCHEME_OBJECT receiver,
			      unsigned long new_mask)
{
  STACK_PUSH (ULONG_TO_FIXNUM (old_mask));
  PUSH_REFLECTION (REFLECT_CODE_RESTORE_INTERRUPT_MASK);
  STACK_PUSH (ULONG_TO_FIXNUM (new_mask));
  setup_compiled_invocation_from_primitive (receiver, 1);
  /* Pun: receiver is being invoked as a return address.  */
  STACK_PUSH (receiver);
}

void
compiled_with_stack_marker (SCHEME_OBJECT thunk)
{
  PUSH_REFLECTION (REFLECT_CODE_STACK_MARKER);
  setup_compiled_invocation_from_primitive (thunk, 0);
  /* Pun: thunk is being invoked as a return address.  */
  STACK_PUSH (thunk);
}

static void
setup_compiled_invocation_from_primitive (SCHEME_OBJECT procedure,
					  unsigned long n_args)
{
  long code = (setup_compiled_invocation (procedure, n_args));
  if (code != PRIM_DONE)
    {
      if (code != PRIM_APPLY_INTERRUPT)
	PUSH_REFLECTION (REFLECT_CODE_INTERNAL_APPLY);
      PRIMITIVE_ABORT (code);
    }
}

/* SCHEME_UTILITY procedures

   Here's a mass of procedures that are called (via
   'scheme_to_interface', an assembly language hook) by compiled code
   to do various jobs.  */

#define DEFINE_SCHEME_UTILITY_0(pname)					\
void									\
ASM_ENTRY_POINT (pname)							\
     (utility_result_t * DSU_result,					\
      unsigned long ignore1,						\
      unsigned long ignore2,						\
      unsigned long ignore3,						\
      unsigned long ignore4)

#define DEFINE_SCHEME_UTILITY_1(pname, av1)				\
void									\
ASM_ENTRY_POINT (pname)							\
     (utility_result_t * DSU_result,					\
      unsigned long av1##_raw,						\
      unsigned long ignore2,						\
      unsigned long ignore3,						\
      unsigned long ignore4)

#define DEFINE_SCHEME_UTILITY_2(pname, av1, av2)			\
void									\
ASM_ENTRY_POINT (pname)							\
     (utility_result_t * DSU_result,					\
      unsigned long av1##_raw,						\
      unsigned long av2##_raw,						\
      unsigned long ignore3,						\
      unsigned long ignore4)

#define DEFINE_SCHEME_UTILITY_3(pname, av1, av2, av3)			\
void									\
ASM_ENTRY_POINT (pname)							\
     (utility_result_t * DSU_result,					\
      unsigned long av1##_raw,						\
      unsigned long av2##_raw,						\
      unsigned long av3##_raw,						\
      unsigned long ignore4)

#define DEFINE_SCHEME_UTILITY_4(pname, av1, av2, av3, av4)		\
void									\
ASM_ENTRY_POINT (pname)							\
     (utility_result_t * DSU_result,					\
      unsigned long av1##_raw,						\
      unsigned long av2##_raw,						\
      unsigned long av3##_raw,						\
      unsigned long av4##_raw)

#define DECLARE_UTILITY_ARG(at1, av1) at1 av1 = ((at1) av1##_raw)

#define INVOKE_RETURN_ADDRESS() do					\
{									\
  if (Free >= GET_MEMTOP)						\
    {									\
      compiler_interrupt_common (DSU_result, 0, GET_VAL);		\
      return;								\
    }									\
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (STACK_POP ()));			\
} while (false)

#define TAIL_CALL_1(pname, a1) do					\
{									\
  pname (DSU_result, ((unsigned long) (a1)), 0, 0, 0);			\
  return;								\
} while (false)

#define TAIL_CALL_2(pname, a1, a2) do					\
{									\
  pname (DSU_result,							\
	 ((unsigned long) (a1)),					\
	 ((unsigned long) (a2)),					\
	 0,								\
	 0);								\
  return;								\
} while (false)

DEFINE_SCHEME_UTILITY_2 (comutil_apply, procedure, frame_size)
{
  DECLARE_UTILITY_ARG (SCHEME_OBJECT, procedure);
  DECLARE_UTILITY_ARG (unsigned long, frame_size);

  while (1)
    switch (OBJECT_TYPE (procedure))
      {
      case TC_ENTITY:
	{
	  SCHEME_OBJECT data = (MEMORY_REF (procedure, ENTITY_DATA));
	  if ((VECTOR_P (data))
	      && ((VECTOR_LENGTH (data)) > frame_size)
	      && ((VECTOR_REF (data, 0))
		  == (VECTOR_REF (fixed_objects, ARITY_DISPATCHER_TAG)))
	      && ((VECTOR_REF (data, frame_size)) != SHARP_F))
	    {
	      procedure = (VECTOR_REF (data, frame_size));
	      break;
	    }
	}
	{
	  SCHEME_OBJECT operator = (MEMORY_REF (procedure, ENTITY_OPERATOR));
	  if (!CC_ENTRY_P (operator))
	    goto handle_in_interpreter;
	  STACK_PUSH (procedure);
	  procedure = operator;
	  frame_size += 1;
	}
	/* fall through */

      case TC_COMPILED_ENTRY:
	{
	  long code
	    = (setup_compiled_invocation (procedure, (frame_size - 1)));
	  if (code != PRIM_DONE)
	    RETURN_TO_C (code);
	}
	RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));

      case TC_PRIMITIVE:
	if (IMPLEMENTED_PRIMITIVE_P (procedure))
	  {
	    int arity = (PRIMITIVE_ARITY (procedure));
	    if (arity == (frame_size - 1))
	      TAIL_CALL_1 (comutil_primitive_apply, procedure);
	    else if (arity == LEXPR)
	      {
		SET_LEXPR_ACTUALS (frame_size - 1);
		TAIL_CALL_1 (comutil_primitive_lexpr_apply, procedure);
	      }
	    else
	      {
		recover_from_apply_error (procedure, (frame_size - 1));
		RETURN_TO_C (ERR_WRONG_NUMBER_OF_ARGUMENTS);
	      }
	  }
	/* fall through */

      handle_in_interpreter:
      default:
	{
	  recover_from_apply_error (procedure, (frame_size - 1));
	  RETURN_TO_C (PRIM_APPLY);
	}
      }
}

/* comutil_lexpr_apply is invoked to reformat the frame when compiled
   code calls a known lexpr.  The actual arguments are on the stack,
   and it is given the number of arguments and the real entry point of
   the procedure.  */

DEFINE_SCHEME_UTILITY_2 (comutil_lexpr_apply, address, n_args)
{
  DECLARE_UTILITY_ARG (insn_t *, address);
  DECLARE_UTILITY_ARG (unsigned long, n_args);
  cc_entry_type_t cet;

  if (! ((!read_cc_entry_type ((&cet), address))
	 && ((cet.marker) == CET_PROCEDURE)
	 && (cet.args.for_procedure.rest_p)
	 && (n_args >= (cet.args.for_procedure.n_required))))
    {
      recover_from_apply_error ((MAKE_CC_ENTRY (address)), n_args);
      RETURN_TO_C (ERR_COMPILED_CODE_ERROR);
    }
  {
    long code
      = (setup_lexpr_invocation ((MAKE_CC_ENTRY (address)),
				 n_args,
				 ((cet.args.for_procedure.n_required)
				  + (cet.args.for_procedure.n_optional))));
    if (code != PRIM_DONE)
      RETURN_TO_C (code);
  }
  RETURN_TO_SCHEME (address);
}

/* comutil_primitive_apply is used to invoked a C primitive.  Note
   that some C primitives (the so called interpreter hooks) will not
   return normally, but will "longjmp" to the interpreter instead.
   Thus the assembly language invoking this should have set up the
   appropriate locations in case this happens.  After invoking the
   primitive, it pops the arguments off the Scheme stack, and proceeds
   by invoking the continuation on top of the stack.  */

DEFINE_SCHEME_UTILITY_1 (comutil_primitive_apply, primitive)
{
  DECLARE_UTILITY_ARG (SCHEME_OBJECT, primitive);
  PRIMITIVE_APPLY (primitive);
  POP_PRIMITIVE_FRAME (PRIMITIVE_ARITY (primitive));
  INVOKE_RETURN_ADDRESS ();
}

/* comutil_primitive_lexpr_apply is like comutil_primitive_apply
   except that it is used to invoke primitives that take an arbitrary
   number of arguments.  The number of arguments is in the
   REGBLOCK_LEXPR_ACTUALS slot of the register block.  */

DEFINE_SCHEME_UTILITY_1 (comutil_primitive_lexpr_apply, primitive)
{
  DECLARE_UTILITY_ARG (SCHEME_OBJECT, primitive);
  PRIMITIVE_APPLY (primitive);
  POP_PRIMITIVE_FRAME (GET_LEXPR_ACTUALS);
  INVOKE_RETURN_ADDRESS ();
}

/* comutil_error is used by compiled code to signal an error.  It
   expects the arguments to the error procedure to be pushed on the
   stack, and is passed the number of arguments (+ 1).  */

DEFINE_SCHEME_UTILITY_1 (comutil_error, frame_size)
{
  DECLARE_UTILITY_ARG (unsigned long, frame_size);
  TAIL_CALL_2 (comutil_apply,
	       (VECTOR_REF (fixed_objects, CC_ERROR_PROCEDURE)),
	       frame_size);
}

/* comutil_link is used to initialize all the variable cache slots for
   a compiled code block.  It is called at load time, by the compiled
   code itself.  It assumes that the return address has been saved on
   the stack.  If an error occurs during linking, or an interrupt must
   be processed (because of the need to GC, etc.), it backs out and
   sets up a return code that will invoke comp_link_caches_restart
   when the error/interrupt processing is done.  */

DEFINE_SCHEME_UTILITY_4 (comutil_link,
			 return_addr,
			 block_addr,
			 constant_addr,
			 n_sections)
{
  DECLARE_UTILITY_ARG (insn_t *, return_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, block_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, constant_addr);
  DECLARE_UTILITY_ARG (unsigned long, n_sections);
  link_cc_state_t s;

  (s.return_address) = return_addr;
  (s.block_address) = block_addr;
  (s.scan) = constant_addr;
  (s.n_sections) = n_sections;
  (s.n_linked_sections) = 0;

  start_linking_cc_block ();
  {
    long result = (link_remaining_sections (&s));
    end_linking_cc_block (&s);
    if (result != PRIM_DONE)
      RETURN_TO_C (result);
  }
  RETURN_TO_SCHEME (s.return_address);
}

/* comp_link_caches_restart is used to continue the linking process
   started by comutil_link after the garbage collector has run.  */

DEFINE_SCHEME_ENTRY (comp_link_caches_restart)
{
  link_cc_state_t s;
  long result;

  restore_link_cc_state (&s);
  SET_ENV (cc_block_environment (MAKE_CC_BLOCK (s.block_address)));

  start_linking_cc_block ();

  result = (link_section (&s));
  if (result == PRIM_DONE)
    result = (link_remaining_sections (&s));
  
  end_linking_cc_block (&s);
  if (result != PRIM_DONE)
    return (result);

  ENTER_SCHEME (s.return_address);
}

static long
link_remaining_sections (link_cc_state_t * s)
{
  while ((s->n_linked_sections) < (s->n_sections))
    {
      start_linking_section (s);
      {
	long result = (link_section (s));
	if (result != PRIM_DONE)
	  return (result);
      }
    }
  return (PRIM_DONE);
}

static void
start_linking_cc_block (void)
{
  bool * ap = (dstack_alloc (sizeof (bool)));
  (*ap) = linking_cc_block_p;
  transaction_begin ();
  transaction_record_action (tat_always, abort_linking_cc_block, ap);
  linking_cc_block_p = 1;
}

static void
end_linking_cc_block (link_cc_state_t * s)
{
  transaction_commit ();
  update_cache_after_link (s);
}

static void
abort_linking_cc_block (void * ap)
{
  linking_cc_block_p = (* ((bool *) (ap)));
}

static void
update_cache_after_link (link_cc_state_t * s)
{
#if defined(FLUSH_I_CACHE_REGION) || defined(PUSH_D_CACHE_REGION)
  SCHEME_OBJECT * addr = (s->block_address);
  if ((cc_entry_address_to_block_address (s->return_address)) == addr)
#ifdef FLUSH_I_CACHE_REGION
    FLUSH_I_CACHE_REGION (addr, (CC_BLOCK_ADDR_LENGTH (addr)));
#else
    ;
#endif
  else
#ifdef PUSH_D_CACHE_REGION
    PUSH_D_CACHE_REGION (addr, (CC_BLOCK_ADDR_LENGTH (addr)));
#else
    ;
#endif
#endif
}

static void
start_linking_section (link_cc_state_t * s)
{
  (s->scan0) = (s->scan);
  (s->n_linked_entries) = 0;
  {
    SCHEME_OBJECT header = (*(s->scan)++);
    (s->type) = (linkage_section_type (header));
    (s->n_entries) = (linkage_section_count (header));
  }
}

static long
link_section (link_cc_state_t * s)
{
  SCHEME_OBJECT * scan1 = ((s->scan0) + 1);
  SCHEME_OBJECT * scan = (s->scan);
  SCHEME_OBJECT * block_address = (s->block_address);
  unsigned long n_linked = (s->n_linked_entries);
  unsigned long n_entries = (s->n_entries);
  cache_handler_t * handler;
  bool execute_p;
  unsigned long entry_size;
  long result = PRIM_DONE;
  DECLARE_RELOCATION_REFERENCE (ref);

  if (!link_section_handler ((s->type), (&handler), (&execute_p)))
    {
      result = ERR_COMPILED_CODE_ERROR;
      goto done;
    }

  if (execute_p)
    {
      /* Hair: START_OPERATOR_RELOCATION requires scan to be pointing
	 to the first word after the header.  Also, it might move scan
	 forward.  If we are just starting the link, just use scan as
	 the argument and let it be changed.  If we are restarting, we
	 need to use use a temporary variable that points to the right
	 place.  */
      if (n_linked == 0)
	START_OPERATOR_RELOCATION (scan, ref);
      else
	START_OPERATOR_RELOCATION (scan1, ref);
      entry_size = UUO_LINK_SIZE;
    }
  else
    entry_size = 1;

  while (n_linked < n_entries)
    {
      result = ((*handler) ((execute_p
			     ? (read_uuo_symbol (scan))
			     : (*scan)),
			    (MAKE_CC_BLOCK (block_address)),
			    (scan - block_address)));
      if (result != PRIM_DONE)
	break;
      scan += entry_size;
      n_linked += 1;
    }

 done:
  /* If we failed on the first entry, back scan up to where it was
     before START_OPERATOR_RELOCATION possibly changed it.  */
  (s->scan) = ((n_linked == 0) ? scan1 : scan);
  (s->n_linked_entries) = n_linked;
  (* (s->scan0)) = (make_linkage_section_marker ((s->type), n_linked));
  if (result == PRIM_DONE)
    (s->n_linked_sections) += 1;
  else
    back_out_of_link_section (s);
  return (result);
}

static bool
link_section_handler (linkage_section_type_t type,
		      cache_handler_t ** handler_r,
		      bool * execute_p_r)
{
  switch (type)
    {
    case LINKAGE_SECTION_TYPE_OPERATOR:
      (*handler_r) = compiler_cache_operator;
      (*execute_p_r) = true;
      return (true);

    case LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR:
      (*handler_r) = compiler_cache_global_operator;
      (*execute_p_r) = true;
      return (true);

    case LINKAGE_SECTION_TYPE_REFERENCE:
      (*handler_r) = compiler_cache_lookup;
      (*execute_p_r) = false;
      return (true);

    case LINKAGE_SECTION_TYPE_ASSIGNMENT:
      (*handler_r) = compiler_cache_assignment;
      (*execute_p_r) = false;
      return (true);

    default:
      return (false);
    }
}

static void
back_out_of_link_section (link_cc_state_t * s)
{
  /* Save enough state to restart.  */
  STACK_PUSH (MAKE_CC_ENTRY (s->return_address));
  STACK_PUSH (ULONG_TO_FIXNUM ((s->n_sections) - (s->n_linked_sections)));
  STACK_PUSH (ULONG_TO_FIXNUM ((s->scan0) - (s->block_address)));
  STACK_PUSH (ULONG_TO_FIXNUM ((s->scan) - (s->block_address)));
  STACK_PUSH (MAKE_CC_BLOCK (s->block_address));
  STACK_PUSH (ULONG_TO_FIXNUM ((s->n_entries) - (s->n_linked_entries)));
  STACK_PUSH (ULONG_TO_FIXNUM (s->n_entries));
  SAVE_LAST_RETURN_CODE (RC_COMP_LINK_CACHES_RESTART);
}

static void
restore_link_cc_state (link_cc_state_t * s)
{
  RESTORE_LAST_RETURN_CODE ();
  (s->n_entries) = (OBJECT_DATUM (STACK_POP ()));
  (s->n_linked_entries) = ((s->n_entries) - (OBJECT_DATUM (STACK_POP ())));
  (s->block_address) = (OBJECT_ADDRESS (STACK_POP ()));
  (s->scan) = ((s->block_address) + (OBJECT_DATUM (STACK_POP ())));
  (s->scan0) = ((s->block_address) + (OBJECT_DATUM (STACK_POP ())));
  (s->n_sections) = (OBJECT_DATUM (STACK_POP ()));
  (s->return_address) = (CC_ENTRY_ADDRESS (STACK_POP ()));

  (s->n_linked_sections) = 0;
  (s->type) = (linkage_section_type (* (s->scan0)));
}

/* Interrupt/GC from Scheme

   These procedures are called from compiled code at the start
   (respectively) of a procedure or continuation if an interrupt has
   been detected.  They must not be called unless there is an
   interrupt to be serviced.

   The code that handles RC_COMP_INTERRUPT_RESTART in "interp.c" will
   return control to comp_interrupt_restart (below).  This assumes
   that the Scheme stack contains a compiled code entry address (start
   of continuation, procedure, etc.).  The GET_EXP saved with the
   continuation is a piece of state that will be returned to
   GET_VAL and GET_ENV (both) upon return.  */

DEFINE_SCHEME_UTILITY_0 (comutil_interrupt_closure)
{
  compiler_interrupt_common (DSU_result, 0, SHARP_F);
}

DEFINE_SCHEME_UTILITY_2 (comutil_interrupt_dlink, entry_point, dlink)
{
  DECLARE_UTILITY_ARG (insn_t *, entry_point);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, dlink);
  compiler_interrupt_common (DSU_result,
			     entry_point,
			     (MAKE_CC_STACK_ENV (dlink)));
}

DEFINE_SCHEME_UTILITY_1 (comutil_interrupt_procedure, entry_point)
{
  DECLARE_UTILITY_ARG (insn_t *, entry_point);
  compiler_interrupt_common (DSU_result, entry_point, SHARP_F);
}

/* GET_VAL has live data, and there is no entry address on the stack */

DEFINE_SCHEME_UTILITY_1 (comutil_interrupt_continuation, return_addr)
{
  DECLARE_UTILITY_ARG (insn_t *, return_addr);
  compiler_interrupt_common (DSU_result, return_addr, GET_VAL);
}

/* GET_ENV has live data; no entry point on the stack */

DEFINE_SCHEME_UTILITY_1 (comutil_interrupt_ic_procedure, entry_point)
{
  DECLARE_UTILITY_ARG (insn_t *, entry_point);
  compiler_interrupt_common (DSU_result, entry_point, GET_ENV);
}

DEFINE_SCHEME_UTILITY_0 (comutil_interrupt_continuation_2)
{
  compiler_interrupt_common (DSU_result, 0, GET_VAL);
}

void
compiler_interrupt_common (utility_result_t * DSU_result,
			   insn_t * address,
			   SCHEME_OBJECT state)
{
  if (!FREE_OK_P (Free))
    REQUEST_GC (Free - heap_alloc_limit);
  STACK_CHECK (0);
  if (address != 0)
    STACK_PUSH (MAKE_CC_ENTRY (address));
  STACK_PUSH (state);
  SAVE_LAST_RETURN_CODE (RC_COMP_INTERRUPT_RESTART);
  RETURN_TO_C (PRIM_INTERRUPT);
}

DEFINE_SCHEME_ENTRY (comp_interrupt_restart)
{
  RESTORE_LAST_RETURN_CODE ();
  {
    SCHEME_OBJECT state = (STACK_POP ());
    SET_ENV (state);
    SET_VAL (state);
  }
  JUMP_TO_CC_ENTRY (STACK_POP ());
}

/* Other traps */

/* Assigning a variable that contains a trap.  */

DEFINE_SCHEME_UTILITY_3 (comutil_assignment_trap,
			 ret_addr, cache_addr, new_val)
{
  DECLARE_UTILITY_ARG (insn_t *, ret_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, cache_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT, new_val);
  SCHEME_OBJECT cache = (MAKE_POINTER_OBJECT (CACHE_TYPE, cache_addr));
  SCHEME_OBJECT old_val;
  long code = (compiler_assignment_trap (cache, new_val, (&old_val)));
  if (code != PRIM_DONE)
    {
      SCHEME_OBJECT sra = (MAKE_CC_ENTRY (ret_addr));
      SCHEME_OBJECT block = (cc_entry_to_block (sra));
      STACK_PUSH (sra);
      STACK_PUSH (new_val);
      STACK_PUSH (cc_block_environment (block));
      STACK_PUSH
	(compiler_var_error (cache, block, CACHE_REFERENCES_ASSIGNMENT));
      SAVE_LAST_RETURN_CODE (RC_COMP_ASSIGNMENT_TRAP_RESTART);
      RETURN_TO_C (code);
    }
  SET_VAL (old_val);
  RETURN_TO_SCHEME (ret_addr);
}

DEFINE_SCHEME_ENTRY (comp_assignment_trap_restart)
{
  RESTORE_LAST_RETURN_CODE ();
  {
    SCHEME_OBJECT name = (STACK_POP ());
    SCHEME_OBJECT environment = (STACK_POP ());
    SCHEME_OBJECT new_val = (STACK_POP ());
    SCHEME_OBJECT old_val;
    long code = (assign_variable (environment, name, new_val, (&old_val)));
    if (code != PRIM_DONE)
      {
	STACK_PUSH (new_val);
	STACK_PUSH (environment);
	STACK_PUSH (name);
	SAVE_LAST_RETURN_CODE (RC_COMP_ASSIGNMENT_TRAP_RESTART);
	return (code);
      }
    SET_VAL (old_val);
    JUMP_TO_CC_ENTRY (STACK_POP ());
  }
}

DEFINE_SCHEME_UTILITY_3 (comutil_cache_lookup_apply,
			 cache_addr, block_addr, frame_size)
{
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, cache_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, block_addr);
  DECLARE_UTILITY_ARG (unsigned long, frame_size);
  SCHEME_OBJECT cache = (MAKE_POINTER_OBJECT (CACHE_TYPE, cache_addr));
  SCHEME_OBJECT value;
  long code = (compiler_lookup_trap (cache, (&value)));
  if (code != PRIM_DONE)
    {
      SCHEME_OBJECT block = (MAKE_CC_BLOCK (block_addr));
      STACK_PUSH (block);
      STACK_PUSH (ULONG_TO_FIXNUM (frame_size));
      STACK_PUSH (cc_block_environment (block));
      STACK_PUSH
	(compiler_var_error (cache, block, CACHE_REFERENCES_OPERATOR));
      SAVE_LAST_RETURN_CODE (RC_COMP_CACHE_REF_APPLY_RESTART);
      RETURN_TO_C (code);
    }
  TAIL_CALL_2 (comutil_apply, value, frame_size);
}

DEFINE_SCHEME_ENTRY (comp_cache_lookup_apply_restart)
{
  RESTORE_LAST_RETURN_CODE ();
  {
    SCHEME_OBJECT name = (STACK_POP ());
    SCHEME_OBJECT environment = (STACK_POP ());
    SCHEME_OBJECT frame_size = (STACK_POP ());
    SCHEME_OBJECT block = (STACK_POP ());
    SCHEME_OBJECT value;
    {
      long code = (lookup_variable (environment, name, (&value)));
      if (code != PRIM_DONE)
	{
	  STACK_PUSH (block);
	  STACK_PUSH (frame_size);
	  STACK_PUSH (environment);
	  STACK_PUSH (name);
	  SAVE_LAST_RETURN_CODE (RC_COMP_CACHE_REF_APPLY_RESTART);
	  return (code);
	}
    }
    STACK_PUSH (value);
    PUSH_APPLY_FRAME_HEADER ((FIXNUM_TO_ULONG (frame_size)) - 1);
    if (CC_ENTRY_P (value))
      return (apply_compiled_procedure ());
    guarantee_interp_return ();
    return (PRIM_APPLY);
  }
}

/* Variable reference traps:
   Reference to a free variable that contains a reference trap.  */

DEFINE_SCHEME_UTILITY_2 (comutil_lookup_trap, ret_addr, cache_addr)
{
  DECLARE_UTILITY_ARG (insn_t *, ret_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, cache_addr);
  SCHEME_OBJECT cache = (MAKE_POINTER_OBJECT (CACHE_TYPE, cache_addr));
  SCHEME_OBJECT val;
  long code = (compiler_lookup_trap (cache, (&val)));
  if (code != PRIM_DONE)
    {
      SCHEME_OBJECT sra = (MAKE_CC_ENTRY (ret_addr));
      SCHEME_OBJECT block = (cc_entry_to_block (sra));
      STACK_PUSH (sra);
      STACK_PUSH (cc_block_environment (block));
      STACK_PUSH (compiler_var_error (cache, block, CACHE_REFERENCES_LOOKUP));
      SAVE_LAST_RETURN_CODE (RC_COMP_LOOKUP_TRAP_RESTART);
      RETURN_TO_C (code);
    }
  SET_VAL (val);
  RETURN_TO_SCHEME (ret_addr);
}

DEFINE_SCHEME_ENTRY (comp_lookup_trap_restart)
{
  RESTORE_LAST_RETURN_CODE ();
  {
    SCHEME_OBJECT name = GET_EXP;
    SCHEME_OBJECT environment = (STACK_POP ());
    SCHEME_OBJECT val;
    long code = (lookup_variable (environment, name, (&val)));
    if (code != PRIM_DONE)
      {
	STACK_PUSH (environment);
	STACK_PUSH (name);
	SAVE_LAST_RETURN_CODE (RC_COMP_LOOKUP_TRAP_RESTART);
	return (code);
      }
    SET_VAL (val);
    JUMP_TO_CC_ENTRY (STACK_POP ());
  }
}

DEFINE_SCHEME_UTILITY_2 (comutil_safe_lookup_trap, ret_addr, cache_addr)
{
  DECLARE_UTILITY_ARG (insn_t *, ret_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, cache_addr);
  SCHEME_OBJECT cache = (MAKE_POINTER_OBJECT (CACHE_TYPE, cache_addr));
  SCHEME_OBJECT val;
  long code = (compiler_safe_lookup_trap (cache, (&val)));
  if (code != PRIM_DONE)
    {
      SCHEME_OBJECT sra = (MAKE_CC_ENTRY (ret_addr));
      SCHEME_OBJECT block = (cc_entry_to_block (sra));
      STACK_PUSH (sra);
      STACK_PUSH (cc_block_environment (block));
      STACK_PUSH (compiler_var_error (cache, block, CACHE_REFERENCES_LOOKUP));
      SAVE_LAST_RETURN_CODE (RC_COMP_SAFE_REF_TRAP_RESTART);
      RETURN_TO_C (code);
    }
  SET_VAL (val);
  RETURN_TO_SCHEME (ret_addr);
}

DEFINE_SCHEME_ENTRY (comp_safe_lookup_trap_restart)
{
  RESTORE_LAST_RETURN_CODE ();
  {
    SCHEME_OBJECT name = GET_EXP;
    SCHEME_OBJECT environment = (STACK_POP ());
    SCHEME_OBJECT val;
    long code = (safe_lookup_variable (environment, name, (&val)));
    if (code != PRIM_DONE)
      {
	STACK_PUSH (environment);
	STACK_PUSH (name);
	SAVE_LAST_RETURN_CODE (RC_COMP_SAFE_REF_TRAP_RESTART);
	return (code);
      }
    SET_VAL (val);
    JUMP_TO_CC_ENTRY (STACK_POP ());
  }
}

DEFINE_SCHEME_UTILITY_2 (comutil_unassigned_p_trap, ret_addr, cache_addr)
{
  DECLARE_UTILITY_ARG (insn_t *, ret_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, cache_addr);
  SCHEME_OBJECT cache = (MAKE_POINTER_OBJECT (CACHE_TYPE, cache_addr));
  SCHEME_OBJECT val;
  long code = (compiler_unassigned_p_trap (cache, (&val)));
  if (code != PRIM_DONE)
    {
      SCHEME_OBJECT sra = (MAKE_CC_ENTRY (ret_addr));
      SCHEME_OBJECT block = (cc_entry_to_block (sra));
      STACK_PUSH (sra);
      STACK_PUSH (cc_block_environment (block));
      STACK_PUSH (compiler_var_error (cache, block, CACHE_REFERENCES_LOOKUP));
      SAVE_LAST_RETURN_CODE (RC_COMP_UNASSIGNED_TRAP_RESTART);
      RETURN_TO_C (code);
    }
  SET_VAL (val);
  RETURN_TO_SCHEME (ret_addr);
}

DEFINE_SCHEME_ENTRY (comp_unassigned_p_trap_restart)
{
  RESTORE_LAST_RETURN_CODE ();
  {
    SCHEME_OBJECT name = GET_EXP;
    SCHEME_OBJECT environment = (STACK_POP ());
    SCHEME_OBJECT val;
    long code = (variable_unassigned_p (environment, name, (&val)));
    if (code != PRIM_DONE)
      {
	STACK_PUSH (environment);
	STACK_PUSH (name);
	SAVE_LAST_RETURN_CODE (RC_COMP_UNASSIGNED_TRAP_RESTART);
	return (code);
      }
    SET_VAL (val);
    JUMP_TO_CC_ENTRY (STACK_POP ());
  }
}

/* Numeric routines

   Invoke the arithmetic primitive in the fixed objects vector.  The
   Scheme arguments are expected on the Scheme stack.  */

#define COMPILER_ARITH_PRIM(name, fobj_index, arity)			\
DEFINE_SCHEME_UTILITY_0 (name)						\
{									\
  TAIL_CALL_2								\
    (comutil_apply, (VECTOR_REF (fixed_objects, fobj_index)), (arity));	\
}

COMPILER_ARITH_PRIM (comutil_decrement, GENERIC_TRAMPOLINE_PREDECESSOR, 2)
COMPILER_ARITH_PRIM (comutil_divide, GENERIC_TRAMPOLINE_DIVIDE, 3)
COMPILER_ARITH_PRIM (comutil_equal, GENERIC_TRAMPOLINE_EQUAL_P, 3)
COMPILER_ARITH_PRIM (comutil_greater, GENERIC_TRAMPOLINE_GREATER_P, 3)
COMPILER_ARITH_PRIM (comutil_increment, GENERIC_TRAMPOLINE_SUCCESSOR, 2)
COMPILER_ARITH_PRIM (comutil_less, GENERIC_TRAMPOLINE_LESS_P, 3)
COMPILER_ARITH_PRIM (comutil_minus, GENERIC_TRAMPOLINE_SUBTRACT, 3)
COMPILER_ARITH_PRIM (comutil_modulo, GENERIC_TRAMPOLINE_MODULO, 3)
COMPILER_ARITH_PRIM (comutil_multiply, GENERIC_TRAMPOLINE_MULTIPLY, 3)
COMPILER_ARITH_PRIM (comutil_negative, GENERIC_TRAMPOLINE_NEGATIVE_P, 2)
COMPILER_ARITH_PRIM (comutil_plus, GENERIC_TRAMPOLINE_ADD, 3)
COMPILER_ARITH_PRIM (comutil_positive, GENERIC_TRAMPOLINE_POSITIVE_P, 2)
COMPILER_ARITH_PRIM (comutil_quotient, GENERIC_TRAMPOLINE_QUOTIENT, 3)
COMPILER_ARITH_PRIM (comutil_remainder, GENERIC_TRAMPOLINE_REMAINDER, 3)
COMPILER_ARITH_PRIM (comutil_zero, GENERIC_TRAMPOLINE_ZERO_P, 2)

DEFINE_SCHEME_UTILITY_2 (comutil_primitive_error, ret_addr, primitive)
{
  DECLARE_UTILITY_ARG (insn_t *, ret_addr);
  DECLARE_UTILITY_ARG (SCHEME_OBJECT, primitive);
  STACK_PUSH (MAKE_CC_ENTRY (ret_addr));
  STACK_PUSH (primitive);
  SAVE_LAST_RETURN_CODE (RC_COMP_ERROR_RESTART);
  RETURN_TO_C (ERR_COMPILED_CODE_ERROR);
}

DEFINE_SCHEME_ENTRY (comp_error_restart)
{
  RESTORE_LAST_RETURN_CODE ();
  (void) STACK_POP ();		/* primitive */
  JUMP_TO_CC_ENTRY (STACK_POP ());
}

void
apply_compiled_from_primitive (unsigned long n_args,
			       SCHEME_OBJECT procedure)
{
  while ((OBJECT_TYPE (procedure)) == TC_ENTITY)
    {
      {
	unsigned long frame_size = (n_args + 1);
	SCHEME_OBJECT data = (MEMORY_REF (procedure, ENTITY_DATA));
	if ((VECTOR_P (data))
	    && (frame_size < (VECTOR_LENGTH (data)))
	    && (CC_ENTRY_P (VECTOR_REF (data, frame_size)))
	    && ((VECTOR_REF (data, 0))
		== (VECTOR_REF (fixed_objects, ARITY_DISPATCHER_TAG))))
	  {
	    procedure = (VECTOR_REF (data, frame_size));
	    continue;
	  }
      }
      {
	SCHEME_OBJECT operator = (MEMORY_REF (procedure, ENTITY_OPERATOR));
	if (CC_ENTRY_P (operator))
	  {
	    STACK_PUSH (procedure);
	    n_args += 1;
	    procedure = operator;
	  }
      }
      break;
    }

  if (CC_ENTRY_P (procedure))
    {
      setup_compiled_invocation_from_primitive (procedure, n_args);
      STACK_PUSH (procedure);
    }
  else
    {
      STACK_PUSH (procedure);
      PUSH_APPLY_FRAME_HEADER (n_args);
      PUSH_REFLECTION (REFLECT_CODE_INTERNAL_APPLY);
    }
}

/* Adjust the stack frame for applying a compiled procedure.  Returns
   PRIM_DONE when successful, otherwise sets up the call frame for
   application by the interpreter and returns the appropriate code.  */

static long
setup_compiled_invocation (SCHEME_OBJECT procedure, unsigned long n_args)
{
  cc_entry_type_t cet;
  unsigned long n_min;
  unsigned long n_max;

  if (read_cc_entry_type ((&cet), (CC_ENTRY_ADDRESS (procedure))))
    {
      recover_from_apply_error (procedure, n_args);
      return (ERR_COMPILED_CODE_ERROR);
    }
  if ((cet.marker) != CET_PROCEDURE)
    {
      recover_from_apply_error (procedure, n_args);
      return (ERR_INAPPLICABLE_OBJECT);
    }
  n_min = (cet.args.for_procedure.n_required);
  if (n_args < n_min)
    {
      recover_from_apply_error (procedure, n_args);
      return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    }
  n_max = (n_min + (cet.args.for_procedure.n_optional));
  if (cet.args.for_procedure.rest_p)
    return (setup_lexpr_invocation (procedure, n_args, n_max));
  if (n_args == n_max)
    return (PRIM_DONE);
  if (n_args > n_max)
    {
      recover_from_apply_error (procedure, n_args);
      return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    }
  if (open_gap (n_args, n_max))
    {
      recover_from_apply_error (procedure, n_args);
      return (PRIM_APPLY_INTERRUPT);
    }
  return (PRIM_DONE);
}

static long
setup_lexpr_invocation (SCHEME_OBJECT procedure,
			unsigned long n_args,
			unsigned long n_max)
{
  if (n_args <= n_max)
    {
      if (open_gap (n_args, (n_max + 1)))
	{
	  recover_from_apply_error (procedure, n_args);
	  return (PRIM_APPLY_INTERRUPT);
	}
      (STACK_REF (n_max)) = EMPTY_LIST;
      return (PRIM_DONE);
    }
  {
    unsigned long n_words = ((n_args - n_max) * 2);
    if (GC_NEEDED_P (n_words))
      {
	REQUEST_GC (n_words);
	recover_from_apply_error (procedure, n_args);
	return (PRIM_APPLY_INTERRUPT);
      }
  }
  {
    SCHEME_OBJECT rest_arg = (MAKE_POINTER_OBJECT (TC_LIST, Free));
    SCHEME_OBJECT * p1 = (STACK_LOC (n_max));
    {
      unsigned long i;
      for (i = n_max; (i < n_args); i += 1)
	{
	  (Free[0]) = (STACK_LOCATIVE_POP (p1));
	  (Free[1]) = (MAKE_POINTER_OBJECT (TC_LIST, (Free + 2)));
	  Free += 2;
	}
    }
    (Free[-1]) = EMPTY_LIST;
    (STACK_LOCATIVE_PUSH (p1)) = rest_arg;
    {
      SCHEME_OBJECT * p2 = (STACK_LOC (n_max));
      unsigned long i;
      for (i = 0; (i < n_max); i += 1)
	(STACK_LOCATIVE_PUSH (p1)) = (STACK_LOCATIVE_PUSH (p2));
    }
    stack_pointer = p1;
  }
  return (PRIM_DONE);
}

static bool
open_gap (unsigned long n_args, unsigned long n_needed)
{
  unsigned long n_defaults = (n_needed - n_args);

  STACK_CHECK (n_defaults);
  if (PENDING_INTERRUPTS_P)
    return (true);

  open_stack_gap (n_args, n_defaults);
  {
    SCHEME_OBJECT * scan = (STACK_LOC (n_args));
    SCHEME_OBJECT * end = (STACK_LOC (n_needed));
    while (scan != end)
      (STACK_LOCATIVE_POP (scan)) = DEFAULT_OBJECT;
  }
  return (false);
}

void
make_compiled_procedure_type (cc_entry_type_t * cet,
			      unsigned int n_required,
			      unsigned int n_optional,
			      bool rest_p)
{
  (cet->marker) = CET_PROCEDURE;
  (cet->args.for_procedure.n_required) = n_required;
  (cet->args.for_procedure.n_optional) = n_optional;
  (cet->args.for_procedure.rest_p) = rest_p;
}

void
make_compiled_continuation_type (cc_entry_type_t * cet, unsigned long offset)
{
  (cet->marker) = CET_CONTINUATION;
  (cet->args.for_continuation.offset) = offset;
}

void
make_cc_entry_type (cc_entry_type_t * cet, cc_entry_type_marker_t marker)
{
  assert (! ((marker == CET_PROCEDURE) || (marker == CET_CONTINUATION)));
  (cet->marker) = marker;
  memset ((& (cet->args)), 0, (sizeof (cet->marker)));
}

SCHEME_OBJECT
cc_entry_to_block (SCHEME_OBJECT entry)
{
  return (MAKE_CC_BLOCK (cc_entry_to_block_address (entry)));
}

SCHEME_OBJECT *
cc_entry_to_block_address (SCHEME_OBJECT entry)
{
  return (cc_entry_address_to_block_address (CC_ENTRY_ADDRESS (entry)));
}

SCHEME_OBJECT *
cc_entry_address_to_block_address (insn_t * entry)
{
  insn_t * p = entry;
  while (1)
    {
      cc_entry_offset_t ceo;
      read_cc_entry_offset ((&ceo), p);
      p -= (ceo.offset);
      if (! (ceo.continued_p))
	{
	  assert ((((unsigned long) p) % (sizeof (SCHEME_OBJECT))) == 0);
	  assert (((SCHEME_OBJECT *) entry)
		  < (CC_BLOCK_ADDR_END ((SCHEME_OBJECT *) p)));
	  return ((SCHEME_OBJECT *) p);
	}
    }
}

int
plausible_cc_block_p (SCHEME_OBJECT * block)
{
  insn_t * zero = ((insn_t *) block);
  insn_t * entry = (((insn_t *) (block + 2)) + CC_ENTRY_HEADER_SIZE);
  {
    cc_entry_type_t cet;
    if ((read_cc_entry_type ((&cet), entry))
	|| ((cet.marker) != CET_EXPRESSION))
      {
	entry += CC_ENTRY_GC_TRAP_SIZE;
	if ((read_cc_entry_type ((&cet), entry))
	    || (! (((cet.marker) == CET_PROCEDURE)
		   || ((cet.marker) == CET_CONTINUATION))))
	  return (0);
      }
  }
  {
    cc_entry_offset_t ceo;
    if ((read_cc_entry_offset ((&ceo), entry))
	|| ((ceo.offset) != (entry - zero)))
      return (0);
  }
  {
    SCHEME_OBJECT * block_end = ((CC_BLOCK_ADDR_END (block)) - 1);
    return
      ((((HEAP_ADDRESS_P (block)) && (HEAP_ADDRESS_P (block_end)))
	|| ((ADDRESS_IN_CONSTANT_P (block))
	    && (ADDRESS_IN_CONSTANT_P (block_end))))
       && (ENVIRONMENT_P (*block_end)));
  }
}

linkage_section_type_t
linkage_section_type (SCHEME_OBJECT marker)
{
  unsigned long type = ((OBJECT_DATUM (marker)) >> 16);
  assert (type < N_LINKAGE_SECTION_TYPES);
  return ((linkage_section_type_t) type);
}

#ifndef UUO_WORDS_TO_COUNT
#  define UUO_WORDS_TO_COUNT(nw) ((nw) / UUO_LINK_SIZE)
#  define UUO_COUNT_TO_WORDS(nc) ((nc) * UUO_LINK_SIZE)
#endif

unsigned long
linkage_section_count (SCHEME_OBJECT marker)
{
  linkage_section_type_t type = (linkage_section_type (marker));
  unsigned long n_words = ((OBJECT_DATUM (marker)) & 0xFFFFUL);
  return (((type == LINKAGE_SECTION_TYPE_OPERATOR)
	   || (type == LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR))
	  ? (UUO_WORDS_TO_COUNT (n_words))
	  : n_words);
}

SCHEME_OBJECT
make_linkage_section_marker (linkage_section_type_t type, unsigned long count)
{
  unsigned long n_words;

  assert (type < N_LINKAGE_SECTION_TYPES);
  n_words
    = (((type == LINKAGE_SECTION_TYPE_OPERATOR)
	|| (type == LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR))
       ? (UUO_COUNT_TO_WORDS (count))
       : count);
  assert (n_words < 0x10000);
  return (MAKE_OBJECT (TC_LINKAGE_SECTION,
		       ((((unsigned long) (type)) << 16) | n_words)));
}

/* Procedures to destructure compiled entries and closures. */

/* Returns the debugging information attached to 'block'.  Usually
   this is a string that contains the filename where the debugging
   info is stored.  */

SCHEME_OBJECT
cc_block_debugging_info (SCHEME_OBJECT block)
{
  return (VECTOR_REF (block, ((VECTOR_LENGTH (block)) - 2)));
}

/* Returns the environment where 'block' was evaluated. */

SCHEME_OBJECT
cc_block_environment (SCHEME_OBJECT block)
{
  return (VECTOR_REF (block, ((VECTOR_LENGTH (block)) - 1)));
}

unsigned long
cc_entry_to_block_offset (SCHEME_OBJECT entry)
{
  return ((CC_ENTRY_ADDRESS (entry))
	  - ((insn_t *) (cc_entry_to_block_address (entry))));
}

bool
cc_block_closure_p (SCHEME_OBJECT block)
{
  return (cc_block_address_closure_p (OBJECT_ADDRESS (block)));
}

bool
cc_entry_closure_p (SCHEME_OBJECT entry)
{
  return (cc_block_address_closure_p (cc_entry_to_block_address (entry)));
}

static bool
cc_block_address_closure_p (SCHEME_OBJECT * block_addr)
{
  SCHEME_OBJECT header_word = (*block_addr);
  return (((OBJECT_TYPE (header_word)) == TC_MANIFEST_CLOSURE));
}

/* Return the entry point ultimately invoked by the compiled closure
   'entry'. */

SCHEME_OBJECT
cc_closure_to_entry (SCHEME_OBJECT entry)
{
  return (compiled_closure_entry_to_target (CC_ENTRY_ADDRESS (entry)));
}

void
declare_compiled_code_block (SCHEME_OBJECT block)
{
#ifdef PUSH_D_CACHE_REGION
  PUSH_D_CACHE_REGION ((OBJECT_ADDRESS (block)), (CC_BLOCK_END (block)));
#endif
}

void
write_variable_cache (SCHEME_OBJECT cache,
		      SCHEME_OBJECT block,
		      unsigned long offset)
{
  MEMORY_SET (block, offset, ((SCHEME_OBJECT) (OBJECT_ADDRESS (cache))));
}

/* Get a compiled procedure from a cached operator reference. */

SCHEME_OBJECT
read_uuo_link (SCHEME_OBJECT block, unsigned long offset)
{
  return
    (MAKE_CC_ENTRY (read_uuo_target_no_reloc (MEMORY_LOC (block, offset))));
}

static void
write_uuo_link (SCHEME_OBJECT target, SCHEME_OBJECT * cache_address)
{
  write_uuo_target ((CC_ENTRY_ADDRESS (target)), cache_address);
#ifdef FLUSH_I_CACHE_REGION
  if (!linking_cc_block_p)
    {
      /* The linker will flush the whole region afterwards. */
      FLUSH_I_CACHE_REGION (cache_address, UUO_LINK_SIZE);
    }
#endif
}

SCHEME_OBJECT *
compiled_closure_objects (SCHEME_OBJECT * block)
{
  insn_t * start = (compiled_closure_start (block));
  unsigned long count = (compiled_closure_count (block));

  /* Skip to end of entries.  */
  while (count > 0)
    {
      start = (compiled_closure_next (start));
      count -= 1;
    }

  /* Skip to first object.  */
  return (skip_compiled_closure_padding (start));
}

bool
decode_old_style_format_word (cc_entry_type_t * cet, uint16_t fw)
{
  uint16_t low = (fw & 0x00FF);
  uint16_t high = ((fw & 0xFF00) >> 8);
  bool rest_p = false;

  if (high < 0x80)
    {
      if ((high == 0x00)
	  || (low == 0x00)
	  || (low == 0x80))
	return (true);
      if (low > 0x80)
	{
	  low = (0xFF - low);
	  rest_p = true;
	}
      if (! (high <= low))
	return (true);
      make_compiled_procedure_type (cet, (high - 1), (low - high), rest_p);
      return (false);
    }
  if (low < 0x80)
    return (true);
  if (low < 0xE0)
    {
      make_compiled_continuation_type
	(cet,
	 (((low & 0x7F) << 7) | (high & 0x7F)));
      return (false);
    }
  if (high != 0xFF)
    return (true);
  switch (low)
    {
    case 0xFF:
      make_cc_entry_type (cet, CET_EXPRESSION);
      break;
    case 0xFE:
      make_cc_entry_type (cet, CET_INTERNAL_PROCEDURE);
      break;
    case 0xFD:
      make_cc_entry_type (cet, CET_TRAMPOLINE);
      break;
    case 0xFC:
      make_cc_entry_type (cet, CET_INTERNAL_CONTINUATION);
      break;
    case 0xFB:
      make_cc_entry_type (cet, CET_RETURN_TO_INTERPRETER);
      break;
    case 0xFA:
      make_cc_entry_type (cet, CET_CLOSURE);
      break;
    default:
      return (true);
    }
  return (false);
}

bool
encode_old_style_format_word (cc_entry_type_t * cet, uint16_t * fw_r)
{
  unsigned int low;
  unsigned int high;

  switch (cet->marker)
    {
    case CET_PROCEDURE:
      high = ((cet->args.for_procedure.n_required) + 1);
      low = (high + (cet->args.for_procedure.n_optional));
      if (! (low < 0x80))
	return (true);
      if (cet->args.for_procedure.rest_p)
	low = (0xFF - low);
      break;

    case CET_CONTINUATION:
      {
	unsigned long n = (cet->args.for_continuation.offset);
	if (! (n < 0x3000))
	  return (true);
	high = ((n & 0x7F) | 0x80);
	low = ((n >> 7) | 0x80);
      }
      break;

    case CET_EXPRESSION:
      low = 0xFF;
      high = 0xFF;
      break;

    case CET_INTERNAL_PROCEDURE:
      low = 0xFE;
      high = 0xFF;
      break;

    case CET_TRAMPOLINE:
      low = 0xFD;
      high = 0xFF;
      break;

    case CET_INTERNAL_CONTINUATION:
      low = 0xFC;
      high = 0xFF;
      break;

    case CET_RETURN_TO_INTERPRETER:
      low = 0xFB;
      high = 0xFF;
      break;

    case CET_CLOSURE:
      low = 0xFA;
      high = 0xFF;
      break;

    default:
      return (true);
    }
  (*fw_r) = ((high << 8) | low);
  return (false);
}

/* Trampolines

   When a free variable appears in operator position in compiled code,
   there must be a directly callable procedure in the corresponding
   UUO cell.  If, at link time, there is no appropriate value for the
   free variable, a fake compiled Scheme procedure that calls one of
   these procedures will be placed into the cell instead.

   The trampolines themselves are made by 'make_uuo_link',
   'make_fake_uuo_link', and 'coerce_to_compiled'.  The trampoline
   looks like a Scheme closure, containing some code that jumps to one
   of these procedures, and additional information to be used by the
   procedure.

   These procedures expect a single argument, the address of the
   information block where they can find the relevant data: typically
   the procedure to invoke and the number of arguments to invoke it
   with.  */

#define DEFINE_TRAMPOLINE(pname)					\
DEFINE_SCHEME_UTILITY_1 (pname, TRAMP_store)

#define INIT_TRAMPOLINE_1(av1)						\
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, TRAMP_store);			\
  SCHEME_OBJECT av1 = (TRAMP_store[0])

#define INIT_TRAMPOLINE_2(av1, av2)					\
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, TRAMP_store);			\
  SCHEME_OBJECT av1 = (TRAMP_store[0]);					\
  SCHEME_OBJECT av2 = (TRAMP_store[1])

#define INIT_TRAMPOLINE_3(av1, av2, av3)				\
  DECLARE_UTILITY_ARG (SCHEME_OBJECT *, TRAMP_store);			\
  SCHEME_OBJECT av1 = (TRAMP_store[0]);					\
  SCHEME_OBJECT av2 = (TRAMP_store[1]);					\
  SCHEME_OBJECT av3 = (TRAMP_store[2])

/* This is how compiled Scheme code normally returns back to the
   Scheme interpreter.  It is invoked by a trampoline, which passes
   the address of the (empty) trampoline storage block to it.  */

DEFINE_TRAMPOLINE (comutil_return_to_interpreter)
{
  RETURN_TO_C (PRIM_DONE);
}

DEFINE_TRAMPOLINE (comutil_reflect_to_interface)
{
  SCHEME_OBJECT code = (STACK_POP ());

  switch (OBJECT_DATUM (code))
    {
    case REFLECT_CODE_INTERNAL_APPLY:
      {
	unsigned long frame_size = (OBJECT_DATUM (STACK_POP ()));
	SCHEME_OBJECT procedure = (STACK_POP ());
	TAIL_CALL_2 (comutil_apply, procedure, frame_size);
      }

    case REFLECT_CODE_RESTORE_INTERRUPT_MASK:
      SET_INTERRUPT_MASK (OBJECT_DATUM (STACK_POP ()));
      INVOKE_RETURN_ADDRESS ();

    case REFLECT_CODE_STACK_MARKER:
      (void) STACK_POP ();	/* marker1 */
      (void) STACK_POP ();	/* marker2 */
      INVOKE_RETURN_ADDRESS ();

    case REFLECT_CODE_CC_BKPT:
      /* Attempt to process interrupts before really proceeding. */
      if (Free >= GET_MEMTOP)
	{
	  PUSH_REFLECTION (REFLECT_CODE_CC_BKPT);
	  compiler_interrupt_common (DSU_result, 0, SHARP_F);
	  return;
	}
      {
	insn_t * addr;
	long code = (do_bkpt_proceed (&addr));
	if (code != PRIM_DONE)
	  {
	    STACK_PUSH (code);
	    RETURN_TO_C (code);
	  }
	RETURN_TO_SCHEME (addr);
      }

    default:
      STACK_PUSH (code);
      RETURN_TO_C (ERR_EXTERNAL_RETURN);
    }
}

DEFINE_TRAMPOLINE (comutil_operator_apply_trap)
{
  INIT_TRAMPOLINE_2 (procedure, frame_header);
  TAIL_CALL_2 (comutil_apply, procedure, (OBJECT_DATUM (frame_header)));
}

DEFINE_TRAMPOLINE (comutil_operator_primitive_trap)
{
  INIT_TRAMPOLINE_1 (primitive);
  TAIL_CALL_1 (comutil_primitive_apply, primitive);
}

DEFINE_TRAMPOLINE (comutil_operator_lexpr_trap)
{
  INIT_TRAMPOLINE_2 (procedure, frame_header);
  SET_LEXPR_ACTUALS (APPLY_FRAME_HEADER_N_ARGS (frame_header));
  TAIL_CALL_1 (comutil_primitive_lexpr_apply, procedure);
}

/* ARITY mismatch handling

   These receive the entry point as an argument and must fill the
   Scheme stack with the missing default values.  They are invoked by
   TRAMPOLINE_K_n_m where n and m are the same as in the name of the
   procedure.  All the arguments are on the Scheme stack.  */

DEFINE_TRAMPOLINE (comutil_operator_1_0_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  STACK_PUSH (DEFAULT_OBJECT);
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_2_0_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  STACK_PUSH (DEFAULT_OBJECT);
  STACK_PUSH (DEFAULT_OBJECT);
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_2_1_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  {
    SCHEME_OBJECT a1 = (STACK_POP ());
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (a1);
  }
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_3_0_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  STACK_PUSH (DEFAULT_OBJECT);
  STACK_PUSH (DEFAULT_OBJECT);
  STACK_PUSH (DEFAULT_OBJECT);
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_3_1_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  {
    SCHEME_OBJECT a1 = (STACK_POP ());
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (a1);
  }
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_3_2_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  {
    SCHEME_OBJECT a1 = (STACK_POP ());
    SCHEME_OBJECT a2 = (STACK_POP ());
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (a2);
    STACK_PUSH (a1);
  }
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_4_0_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  STACK_PUSH (DEFAULT_OBJECT);
  STACK_PUSH (DEFAULT_OBJECT);
  STACK_PUSH (DEFAULT_OBJECT);
  STACK_PUSH (DEFAULT_OBJECT);
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_4_1_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  {
    SCHEME_OBJECT a1 = (STACK_POP ());
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (a1);
  }
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_4_2_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  {
    SCHEME_OBJECT a1 = (STACK_POP ());
    SCHEME_OBJECT a2 = (STACK_POP ());
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (a2);
    STACK_PUSH (a1);
  }
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

DEFINE_TRAMPOLINE (comutil_operator_4_3_trap)
{
  INIT_TRAMPOLINE_1 (procedure);
  {
    SCHEME_OBJECT a1 = (STACK_POP ());
    SCHEME_OBJECT a2 = (STACK_POP ());
    SCHEME_OBJECT a3 = (STACK_POP ());
    STACK_PUSH (DEFAULT_OBJECT);
    STACK_PUSH (a3);
    STACK_PUSH (a2);
    STACK_PUSH (a1);
  }
  RETURN_TO_SCHEME (CC_ENTRY_ADDRESS (procedure));
}

/* The linker either couldn't find a binding or the binding was
   unassigned.  This must report the correct name of the missing
   variable and the environment in which the lookup begins for the
   error cases.

   'cache' is the linker object corresponding to the operator variable
   (it contains the actual value cell, the name, and linker tables).
   'block' and 'offset' point to the cache cell in question.  */

DEFINE_TRAMPOLINE (comutil_operator_lookup_trap)
{
  INIT_TRAMPOLINE_3 (cache, block, offset);
  SCHEME_OBJECT * cache_addr = (MEMORY_LOC (block, (OBJECT_DATUM (offset))));
  unsigned long frame_size = (read_uuo_frame_size (cache_addr));
  SCHEME_OBJECT procedure;
  long code = (compiler_operator_reference_trap (cache, (&procedure)));
  if (code != PRIM_DONE)
    {
      STACK_PUSH (MAKE_CC_ENTRY (read_uuo_target_no_reloc (cache_addr)));
      /* Next three for debugger.  */
      STACK_PUSH (ULONG_TO_FIXNUM (frame_size));
      STACK_PUSH (cc_block_environment (block));
      STACK_PUSH
	(compiler_var_error (cache, block, CACHE_REFERENCES_OPERATOR));
      SAVE_LAST_RETURN_CODE (RC_COMP_OP_REF_TRAP_RESTART);
      RETURN_TO_C (code);
    }
  TAIL_CALL_2 (comutil_apply, procedure, frame_size);
}

/* Re-start after processing an error/interrupt encountered in the
   previous utility.  Extract the new trampoline or procedure (the
   user may have defined the missing variable) and invoke it.  */

DEFINE_SCHEME_ENTRY (comp_op_lookup_trap_restart)
{
  RESTORE_LAST_RETURN_CODE ();
  /* Discard debugger info.  */
  stack_pointer = (STACK_LOC (3));
  {
    SCHEME_OBJECT * store
      = (trampoline_storage (cc_entry_to_block_address (STACK_POP ())));
    SCHEME_OBJECT block = (store[1]);
    unsigned long offset = (OBJECT_DATUM (store[2]));
    ENTER_SCHEME (read_uuo_target_no_reloc (MEMORY_LOC (block, offset)));
  }
}

/* make_uuo_link is called by C and initializes a compiled procedure
   cache at a location given by a block and an offset.  */

long
make_uuo_link (SCHEME_OBJECT procedure,
	       SCHEME_OBJECT cache,
	       SCHEME_OBJECT block,
	       unsigned long offset)
{
  SCHEME_OBJECT * cache_address = (MEMORY_LOC (block, offset));
  unsigned long frame_size = (read_uuo_frame_size (cache_address));
  SCHEME_OBJECT orig_proc;
  trampoline_type_t kind;
  long result;
  SCHEME_OBJECT trampoline;

  if (REFERENCE_TRAP_P (procedure))
    return (make_fake_uuo_link (cache, block, offset));

  orig_proc = procedure;
 loop:
  switch (OBJECT_TYPE (procedure))
    {
    case TC_COMPILED_ENTRY:
      {
	insn_t * entry = (CC_ENTRY_ADDRESS (procedure));
	unsigned long nargs = (frame_size - 1);
	cc_entry_type_t cet;
	unsigned long nmin;
	unsigned long nmax;

	if ((read_cc_entry_type ((&cet), entry))
	    || ((cet.marker) != CET_PROCEDURE))
	  return (ERR_COMPILED_CODE_ERROR);
	nmin = (cet.args.for_procedure.n_required);
	nmax = (nmin + (cet.args.for_procedure.n_optional));
	if (cet.args.for_procedure.rest_p)
	  kind = TRAMPOLINE_K_APPLY;
	else if (nargs == nmax)
	  {
	    /* No defaulting is needed.  */
	    write_uuo_link (procedure, cache_address);
	    return (PRIM_DONE);
	  }
	else if ((nargs < nmax)
		 && (nargs >= nmin)
		 && (nmin < nmax)
		 && (nmax <= TRAMPOLINE_TABLE_SIZE))
	  {
	    /* We have optimized defaulting for this case.  */
	    kind
	      = (trampoline_arity_table
		 [(((nmax - 1) * TRAMPOLINE_TABLE_SIZE) + nargs)]);
	    assert (kind != TRAMPOLINE_K_APPLY);
	    frame_size = 0;
	  }
	else
	  /* Use unoptimized defaulting.  */
	  kind = TRAMPOLINE_K_APPLY;
	break;
      }

    case TC_ENTITY:
      {
	SCHEME_OBJECT data = (MEMORY_REF (procedure, ENTITY_DATA));
	if ((VECTOR_P (data))
	    && (frame_size < (VECTOR_LENGTH (data)))
	    && ((VECTOR_REF (data, frame_size)) != SHARP_F)
	    && ((VECTOR_REF (data, 0))
		== (VECTOR_REF (fixed_objects, ARITY_DISPATCHER_TAG))))
	  {
	    procedure = (VECTOR_REF (data, frame_size));
	    goto loop;
	  }
	kind = TRAMPOLINE_K_APPLY;
	break;
      }

    case TC_PRIMITIVE:
      {
	long arity = (PRIMITIVE_ARITY (procedure));
	if (arity == ((long) (frame_size - 1)))
	  {
	    kind = TRAMPOLINE_K_PRIMITIVE;
	    frame_size = 0;
	  }
	else if (arity == LEXPR_PRIMITIVE_ARITY)
	  kind = TRAMPOLINE_K_LEXPR_PRIMITIVE;
	else
	  kind = TRAMPOLINE_K_APPLY;
	break;
      }

    default:
      kind = TRAMPOLINE_K_APPLY;
      break;
    }
  result
    = ((frame_size == 0)
       ? (make_redirection_trampoline ((&trampoline), kind, procedure))
       : (make_apply_trampoline ((&trampoline), kind, procedure, frame_size)));
  if (result == PRIM_DONE)
    write_uuo_link (trampoline, cache_address);
  return (result);
}

static long
make_fake_uuo_link (SCHEME_OBJECT cache,
		    SCHEME_OBJECT block,
		    unsigned long offset)
{
  cc_entry_type_t cet;
  SCHEME_OBJECT trampoline;

  make_cc_entry_type ((&cet), CET_TRAMPOLINE);
  {
    long result = (make_trampoline ((&trampoline),
				    (&cet),
				    TRAMPOLINE_K_LOOKUP,
				    3,
				    cache,
				    block,
				    (ULONG_TO_FIXNUM (offset))));
    if (result != PRIM_DONE)
      return (result);
  }
  {
    SCHEME_OBJECT * cache_address = (MEMORY_LOC (block, offset));
    write_uuo_link (trampoline, cache_address);
  }
  return (PRIM_DONE);
}

long
coerce_to_compiled (SCHEME_OBJECT procedure,
		    unsigned int arity,
		    SCHEME_OBJECT * location)
{
  cc_entry_type_t cet;

  if (CC_ENTRY_P (procedure))
    {
      if (read_cc_entry_type ((&cet), (CC_ENTRY_ADDRESS (procedure))))
	return (ERR_COMPILED_CODE_ERROR);
      if ((cet.marker) == CET_PROCEDURE)
	{
	  (*location) = procedure;
	  return (PRIM_DONE);
	}
    }
  make_compiled_procedure_type ((&cet), arity, 0, false);
  return (make_trampoline (location,
			   (&cet),
			   TRAMPOLINE_K_APPLY,
			   2,
			   procedure,
			   (ULONG_TO_FIXNUM (arity + 1))));
}

static long
make_trampoline (SCHEME_OBJECT * slot,
		 cc_entry_type_t * cet,
		 trampoline_type_t kind,
		 unsigned int n_values,
		 ...)
{
  SCHEME_OBJECT h1;
  SCHEME_OBJECT h2;
  unsigned long n_words;
  SCHEME_OBJECT * block;

  make_trampoline_headers (1, n_values, (&h1), (&h2), (&n_words));
  if (GC_NEEDED_P (n_words))
    {
      REQUEST_GC (n_words);
      return (PRIM_INTERRUPT);
    }
  block = Free;
  Free += n_words;
  (block[0]) = h1;
  (block[1]) = h2;
  if (fill_trampoline (block, 0, cet, kind))
    return (ERR_COMPILED_CODE_ERROR);
  {
    SCHEME_OBJECT * p = (trampoline_storage (block));
    va_list ap;

    va_start (ap, n_values);
    while (n_values > 0)
      {
	(*p++) = (va_arg (ap, SCHEME_OBJECT));
	n_values -= 1;
      }
    va_end (ap);
  }
  (*slot) = (MAKE_CC_ENTRY (trampoline_entry_addr (block, 0)));
  return (PRIM_DONE);
}

static void
make_trampoline_headers (unsigned long n_entries, unsigned long n_store,
			 SCHEME_OBJECT * h1_r, SCHEME_OBJECT * h2_r,
			 unsigned long * n_words_r)
{
  unsigned long n1 = (trampoline_entry_size (n_entries));
  unsigned long n2 = (1 + n1 + n_store);
  (*h1_r) = (MAKE_OBJECT (TC_TRAMPOLINE_HEADER, n2));
  (*h2_r) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, n1));
  (*n_words_r) = (1 + n2);
}

static bool
fill_trampoline (SCHEME_OBJECT * block,
		 unsigned long index,
		 cc_entry_type_t * cet,
		 trampoline_type_t kind)
{
  insn_t * addr = (trampoline_entry_addr (block, index));
  if (write_cc_entry_type (cet, addr))
    return (true);
  {
    cc_entry_offset_t ceo;
    (ceo.offset) = (addr - ((insn_t *) block));
    (ceo.continued_p) = false;
    if (write_cc_entry_offset ((&ceo), addr))
      return (true);
  }
  return (store_trampoline_insns (addr, kind));
}

SCHEME_OBJECT *
trampoline_storage (SCHEME_OBJECT * block)
{
  return (block + 2 + (OBJECT_DATUM (block[1])));
}

static long
make_redirection_trampoline (SCHEME_OBJECT * slot,
			     trampoline_type_t kind,
			     SCHEME_OBJECT procedure)
{
  cc_entry_type_t cet;
  make_cc_entry_type ((&cet), CET_TRAMPOLINE);
  return (make_trampoline (slot, (&cet), kind, 1, procedure));
}

static long
make_apply_trampoline (SCHEME_OBJECT * slot,
		       trampoline_type_t kind,
		       SCHEME_OBJECT procedure,
		       unsigned long frame_size)
{
  cc_entry_type_t cet;
  make_cc_entry_type ((&cet), CET_TRAMPOLINE);
  return (make_trampoline (slot,
			   (&cet),
			   kind,
			   2,
			   procedure,
			   (ULONG_TO_FIXNUM (frame_size))));
}

/* Compiled-code breakpoints */

#ifdef HAVE_BKPT_SUPPORT

#define BKPT_PROCEED_FRAME_SIZE	3

SCHEME_OBJECT
bkpt_proceed (insn_t * ep, SCHEME_OBJECT handle, SCHEME_OBJECT state)
{
  if (! ((CC_ENTRY_P (STACK_REF (BKPT_PROCEED_FRAME_SIZE)))
	 && ((CC_ENTRY_ADDRESS (STACK_REF (BKPT_PROCEED_FRAME_SIZE))) == ep)))
    error_external_return ();
  PUSH_REFLECTION (REFLECT_CODE_CC_BKPT);
  stack_pointer = (STACK_LOC (-BKPT_PROCEED_FRAME_SIZE));
  return (SHARP_F);
}

#else /* not HAVE_BKPT_SUPPORT */

SCHEME_OBJECT
bkpt_install (insn_t * ep)
{
  return (SHARP_F);
}

SCHEME_OBJECT
bkpt_closure_install (insn_t * ep)
{
  return (SHARP_F);
}

void
bkpt_remove (insn_t * ep, SCHEME_OBJECT handle)
{
  error_external_return ();
}

bool
bkpt_p (insn_t * ep)
{
  return (false);
}

SCHEME_OBJECT
bkpt_proceed (insn_t * ep, SCHEME_OBJECT handle, SCHEME_OBJECT state)
{
  error_external_return ();
  return (UNSPECIFIC);
}

long
do_bkpt_proceed (insn_t ** addr_r)
{
  return (ERR_EXTERNAL_RETURN);
}

#endif /* not HAVE_BKPT_SUPPORT */

DEFINE_SCHEME_UTILITY_2 (comutil_compiled_code_bkpt, entry_addr, state)
{
  DECLARE_UTILITY_ARG (insn_t *, entry_addr);
  DECLARE_UTILITY_ARG (void *, state);
  SCHEME_OBJECT entry = (MAKE_CC_ENTRY (entry_addr));
  cc_entry_type_t cet;
  SCHEME_OBJECT to_save;
  SCHEME_OBJECT stack_ptr;

  /* Potential bug: This does not preserve the environment for IC
     procedures.  There is no way to tell that we have an IC procedure
     in our hands.  It is not safe to preserve it in general because
     the contents of the register may be stale (predate the last GC).
     However, the compiler no longer generates IC procedures, and will
     probably never do it again.  */

  if (read_cc_entry_type ((&cet), entry_addr))
    to_save = SHARP_F;
  else
    switch (cet.marker)
      {
	case CET_CONTINUATION:
	  to_save = GET_VAL;
	  break;

	case CET_INTERNAL_CONTINUATION:
	  to_save = (MAKE_CC_STACK_ENV ((SCHEME_OBJECT *) state));
	  break;

	case CET_RETURN_TO_INTERPRETER:
	  to_save = GET_VAL;
	  break;

	case CET_CLOSURE:
	  to_save = (MAKE_CC_ENTRY ((insn_t *) state));
	  break;

	default:
	  to_save = SHARP_F;
	  break;
	}

  STACK_PUSH (entry);
  stack_ptr = (MAKE_CC_STACK_ENV (stack_pointer));
  STACK_PUSH (to_save);
  STACK_PUSH (stack_ptr);
  STACK_PUSH (entry);
  TAIL_CALL_2 (comutil_apply,
	       (VECTOR_REF (fixed_objects, CC_BKPT_PROCEDURE)),
	       4);
}

DEFINE_SCHEME_UTILITY_1 (comutil_compiled_closure_bkpt, entry_addr)
{
  DECLARE_UTILITY_ARG (insn_t *, entry_addr);
  SCHEME_OBJECT entry = (MAKE_CC_ENTRY (entry_addr));
  SCHEME_OBJECT stack_ptr;

  STACK_PUSH (entry);
  stack_ptr = (MAKE_CC_STACK_ENV (stack_pointer));
  STACK_PUSH (SHARP_F);
  STACK_PUSH (stack_ptr);
  STACK_PUSH (entry);
  TAIL_CALL_2 (comutil_apply,
	       (VECTOR_REF (fixed_objects, CC_BKPT_PROCEDURE)),
	       4);
}

/* Utility table used by the assembly language interface to invoke the
   SCHEME_UTILITY procedures that appear in this file.

   Important: Do NOT reorder this table without changing the indices
   defined on the following page and the corresponding table in the
   compiler.  */

utility_proc_t * utility_table [] =
{
  comutil_return_to_interpreter,		/* 0x0 */
  comutil_operator_apply_trap,			/* 0x1 */
  comutil_operator_apply_trap,			/* 0x2 */
  comutil_operator_apply_trap,			/* 0x3 */
  comutil_operator_apply_trap,			/* 0x4 */
  comutil_operator_lexpr_trap,			/* 0x5 */
  comutil_operator_primitive_trap,		/* 0x6 */
  comutil_operator_lookup_trap,			/* 0x7 */
  comutil_operator_1_0_trap,			/* 0x8 */
  comutil_operator_2_1_trap,			/* 0x9 */
  comutil_operator_2_0_trap,			/* 0xa */
  comutil_operator_3_2_trap,			/* 0xb */
  comutil_operator_3_1_trap,			/* 0xc */
  comutil_operator_3_0_trap,			/* 0xd */
  comutil_operator_4_3_trap,			/* 0xe */
  comutil_operator_4_2_trap,			/* 0xf */
  comutil_operator_4_1_trap,			/* 0x10 */
  comutil_operator_4_0_trap,			/* 0x11 */
  comutil_primitive_apply,			/* 0x12 */
  comutil_primitive_lexpr_apply,		/* 0x13 */
  comutil_apply,				/* 0x14 */
  comutil_error,				/* 0x15 */
  comutil_lexpr_apply,				/* 0x16 */
  comutil_link,					/* 0x17 */
  comutil_interrupt_closure,			/* 0x18 */
  comutil_interrupt_dlink,			/* 0x19 */
  comutil_interrupt_procedure,			/* 0x1a */
  comutil_interrupt_continuation,		/* 0x1b */
  comutil_interrupt_ic_procedure,		/* 0x1c */
  comutil_assignment_trap,			/* 0x1d */
  comutil_cache_lookup_apply,			/* 0x1e */
  comutil_lookup_trap,				/* 0x1f */
  comutil_safe_lookup_trap,			/* 0x20 */
  comutil_unassigned_p_trap,			/* 0x21 */
  comutil_decrement,				/* 0x22 */
  comutil_divide,				/* 0x23 */
  comutil_equal,				/* 0x24 */
  comutil_greater,				/* 0x25 */
  comutil_increment,				/* 0x26 */
  comutil_less,					/* 0x27 */
  comutil_minus,				/* 0x28 */
  comutil_multiply,				/* 0x29 */
  comutil_negative,				/* 0x2a */
  comutil_plus,					/* 0x2b */
  comutil_positive,				/* 0x2c */
  comutil_zero,					/* 0x2d */
  0,						/* 0x2e */
  0,						/* 0x2f */
  0,						/* 0x30 */
  0,						/* 0x31 */
  0,						/* 0x32 */
  0,						/* 0x33 */
  0,						/* 0x34 */
  0,						/* 0x35 */
  comutil_primitive_error,			/* 0x36 */
  comutil_quotient,				/* 0x37 */
  comutil_remainder,				/* 0x38 */
  comutil_modulo,				/* 0x39 */
  comutil_reflect_to_interface,			/* 0x3a */
  comutil_interrupt_continuation_2,		/* 0x3b */
  comutil_compiled_code_bkpt,			/* 0x3c */
  comutil_compiled_closure_bkpt			/* 0x3d */
};

unsigned long max_trampoline
  = ((sizeof (utility_table)) / (sizeof (utility_proc_t *)));

/* Support for trap handling. */

const char *
utility_index_to_name (unsigned int index)
{
  return (0);
}

int
pc_to_utility_index (unsigned long pc)
{
  return (-1);
}

static unsigned int n_builtins = 0;
static unsigned int s_builtins = 0;
static unsigned long * builtins = 0;
static const char ** builtin_names = 0;

void
declare_builtin (unsigned long builtin, const char * name)
{
  if (n_builtins == s_builtins)
    {
      if (s_builtins == 0)
	{
	  s_builtins = 30;
	  builtins = (malloc (s_builtins * (sizeof (unsigned long))));
	  builtin_names = (malloc (s_builtins * (sizeof (char *))));
	}
      else
	{
	  s_builtins += s_builtins;
	  builtins
	    = (realloc (builtins, (s_builtins * (sizeof (unsigned long)))));
	  builtin_names
	    = (realloc (builtin_names, (s_builtins * (sizeof (char *)))));
	}
      if ((builtins == 0) || (builtin_names == 0))
	{
	  outf_fatal ("declare_builtin: malloc/realloc failed (size = %d).\n",
		      s_builtins);
	  termination_init_error ();
	}
    }
  {
    unsigned int low = 0;
    unsigned int high = n_builtins;
    while (1)
      {
	if (low < high)
	  {
	    unsigned int middle = ((low + high) / 2);
	    if (builtin < (builtins[middle]))
	      high = middle;
	    else if (builtin > (builtins[middle]))
	      low = (middle + 1);
	    else
	      {
		(builtin_names[middle]) = name;
		return;
	      }
	  }
	else
	  {
	    unsigned int scan = n_builtins;
	    while (low < scan)
	      {
		(builtins [scan]) = (builtins [scan - 1]);
		(builtin_names [scan]) = (builtin_names [scan - 1]);
		scan -= 1;
	      }
	    (builtins [low]) = builtin;
	    (builtin_names [low]) = name;
	    return;
	  }
      }
  }
}

const char *
builtin_index_to_name (unsigned int index)
{
  return ((index < n_builtins) ? (builtin_names[index]) : 0);
}

int
pc_to_builtin_index (unsigned long pc)
{
  if (! ((builtins != 0)
	 && (pc >= (builtins[0]))
	 && (pc < (builtins [(n_builtins - 1)]))))
    return (-1);
  {
    unsigned int low = 0;
    unsigned int high = (n_builtins - 1);
    while ((low + 1) < high)
      {
	unsigned int middle = ((low + high) / 2);
	if (pc < (builtins[middle]))
	  high = middle;
	else if (pc > (builtins[middle]))
	  low = middle;
	else
	  return (middle);
      }
    return ((pc == (builtins[high])) ? high : low);
  }
}

#ifdef __WIN32__
#include "ntscmlib.h"

extern unsigned long * win32_catatonia_block;

#ifndef REGBLOCK_LENGTH
#  define REGBLOCK_LENGTH REGBLOCK_MINIMUM_LENGTH
#endif

typedef struct register_storage
{
  /* The following must be allocated consecutively */
  unsigned long catatonia_block [3];
  void * Regstart [32];		/* Negative byte offsets from &Registers[0] */
  SCHEME_OBJECT Registers [REGBLOCK_LENGTH];
} REGMEM;

SCHEME_OBJECT * RegistersPtr = 0;
unsigned long * win32_catatonia_block = 0;
static REGMEM regmem;

void
win32_allocate_registers (void)
{
  win32_catatonia_block = (regmem.catatonia_block);
  Registers = (regmem.Registers);
  if (!win32_system_utilities.lock_memory_area ((&regmem), (sizeof (regmem))))
    {
      outf_error ("Unable to lock registers\n");
      outf_flush_error ();
    }
}

void
win32_deallocate_registers (void)
{
  win32_system_utilities.unlock_memory_area ((&regmem), (sizeof (regmem)));
}

#endif /* __WIN32__ */
