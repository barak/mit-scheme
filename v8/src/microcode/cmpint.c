/* -*-C-*-

Copyright (c) 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/cmpint.c,v 1.12 1989/11/06 17:31:23 jinx Exp $
 *
 * This file corresponds to
 * $COMPILER-Header: compiler.c,v 9.37 89/10/25 14:55:45 GMT jinx Exp $
 * $MC68020-Header: cmp68020.m4,v 9.93 89/10/26 07:49:23 GMT cph Exp $
 *
 * Compiled code interface.  Portable version.
 * This file requires a bit of assembly language described in cmpaux.m4
 * See also the files cmpint.h, cmpgc.h, and cmpint.txt .
 *
 */

/*
 * Procedures in this file belong to the following categories:
 *
 * Local C procedures.  These are local procedures called only by
 * other procedures in this file, and have been separated only for
 * modularity reasons.  They are tagged with the C keyword `static'.
 * They can return any C type.
 *
 * C utility procedures.  These procedures are called from C
 * primitives and other subsystems and never leave the C world.  They
 * constitute the compiled code data abstraction as far as other C
 * parts of the Scheme "microcode" are concerned.  They are tagged
 * with the noise word `C_UTILITY'.  They can return any C type.
 *
 * C interface entries.  These procedures are called from the
 * interpreter (written in C) and ultimately enter the Scheme compiled
 * code world by using the assembly language utility
 * `C_to_interface'.  They are tagged with the noise word
 * `C_TO_SCHEME'.  They MUST return a C long indicating what
 * the interpreter should do next.
 *
 * Scheme interface utilities.  These procedures are called from the
 * assembly language interface and return to it, and perform all the
 * tasks that the compiler does not code inline.  They are referenced
 * by compiled scheme code by index, and the assembly language
 * interface fetches them from an array.  They are tagged with the
 * noise word `SCHEME_UTILITY'.  They return a C structure (struct
 * utility_result) which describes whether computation should proceed
 * in the interpreter or in compiled code, and how.
 *
 */

/* Macro imports */

#include <setjmp.h>
#include <stdio.h>
#include "config.h"     /* SCHEME_OBJECT type and machine dependencies */
#include "types.h"      /* Needed by const.h */
#include "const.h"      /* REGBLOCK_MINIMUM_LENGTH and PRIM_... codes */
#include "object.h"     /* Making and destructuring Scheme objects */
#include "intrpt.h"	/* Interrupt processing macros */
#include "gc.h"		/* Request_GC, etc. */
#include "sdata.h"	/* ENTITY_OPERATOR */
#include "cmpgc.h"      /* Compiled code object relocation */
#include "errors.h"     /* Error codes and Termination codes */
#include "returns.h"	/* Return addresses in the interpreter */
#include "fixobj.h"	/* To find the error handlers */
#include "stack.h"	/* Stacks and stacklets */
#include "interp.h"     /* Interpreter state and primitive destructuring */
#include "default.h"    /* Metering_Apply_Primitive */
#include "extern.h"	/* External decls (missing Cont_Debug, etc.) */
#include "trap.h"       /* UNASSIGNED_OBJECT, TRAP_EXTENSION_TYPE */
#include "prims.h"      /* LEXPR */
#include "cmpint2.h"    /* Compiled code object destructuring */
#include "prim.h"	/* Primitive_Procedure_Table, etc. */

/* Make noise words invisible to the C compiler. */

#define C_UTILITY
#define C_TO_SCHEME
#define SCHEME_UTILITY

/* Structure returned by SCHEME_UTILITYs */

struct utility_result
{
  void (*interface_dispatch)();
  union additional_info
  {
    long                code_to_interpreter;
    instruction        *entry_point;
  } extra;
};

/* Some convenience macros */

#define RETURN_TO_C(code)                                               \
do {                                                                    \
  struct utility_result temp;                                           \
                                                                        \
  temp.interface_dispatch = ((void (*)()) interface_to_C);              \
  temp.extra.code_to_interpreter = (code);                              \
                                                                        \
  return (temp);                                                        \
} while (false)

#define RETURN_TO_SCHEME(ep)                                            \
do {                                                                    \
  struct utility_result temp;                                           \
                                                                        \
  temp.interface_dispatch = ((void (*)()) interface_to_scheme);         \
  temp.extra.entry_point = ((instruction *) (ep));			\
                                                                        \
  return (temp);                                                        \
} while (false)

#define RETURN_UNLESS_EXCEPTION(code, entry_point)                      \
{                                                                       \
  int return_code;                                                      \
                                                                        \
  return_code = (code);                                                 \
  if (return_code == PRIM_DONE)                                         \
  {                                                                     \
    RETURN_TO_SCHEME (entry_point);                                     \
  }                                                                     \
  else                                                                  \
  {                                                                     \
    RETURN_TO_C (return_code);                                          \
  }                                                                     \
}

#define ENTRY_TO_OBJECT(entry)						\
MAKE_POINTER_OBJECT(TC_COMPILED_ENTRY, ((SCHEME_OBJECT *) (entry)))

#define MAKE_CC_BLOCK(block_addr)					\
(MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block_addr))

/* Imports from the rest of the "microcode" */

extern term_type
  Microcode_Termination();

extern long
  compiler_cache_operator(),
  compiler_cache_lookup(),
  compiler_cache_assignment();

/* Imports from assembly language */

extern long
  C_to_interface();

extern void
  interface_to_C(),
  interface_to_scheme();

/* Exports to the rest of the "microcode" */

extern long
  compiler_interface_version,
  compiler_processor_type;

extern SCHEME_OBJECT
  Registers[],
  compiler_utilities,
  return_to_interpreter;

extern C_UTILITY long
  make_fake_uuo_link(),
  make_uuo_link(),
  compiled_block_closure_p(),
  compiled_entry_closure_p(),
  compiled_entry_to_block_offset(),
  coerce_to_compiled();

extern C_UTILITY SCHEME_OBJECT
  extract_uuo_link(),
  extract_variable_cache(),
  compiled_block_debugging_info(),
  compiled_block_environment(),
  compiled_closure_to_entry(),
  *compiled_entry_to_block_address(),
  compiled_entry_to_block();

extern C_UTILITY void
  compiler_initialize(),
  compiler_reset(),
  store_variable_cache(),
  compiled_entry_type();

extern C_TO_SCHEME long
  enter_compiled_expression(),
  apply_compiled_procedure(),
  return_to_compiled_code(),
  comp_link_caches_restart(),
  comp_op_lookup_trap_restart(),
  comp_interrupt_restart(),
  comp_assignment_trap_restart(),
  comp_cache_lookup_apply_restart(),
  comp_lookup_trap_restart(),
  comp_safe_lookup_trap_restart(),
  comp_unassigned_p_trap_restart(),
  comp_access_restart(),
  comp_reference_restart(),
  comp_safe_reference_restart(),
  comp_unassigned_p_restart(),
  comp_unbound_p_restart(),
  comp_assignment_restart(),
  comp_definition_restart(),
  comp_lookup_apply_restart();

extern SCHEME_UTILITY struct utility_result
  comutil_return_to_interpreter(),
  comutil_operator_apply_trap(),
  comutil_operator_arity_trap(),
  comutil_operator_entity_trap(),
  comutil_operator_interpreted_trap(),
  comutil_operator_lexpr_trap(),
  comutil_operator_primitive_trap(),
  comutil_operator_lookup_trap(),
  comutil_operator_1_0_trap(),
  comutil_operator_2_1_trap(),
  comutil_operator_2_0_trap(),
  comutil_operator_3_2_trap(),
  comutil_operator_3_1_trap(),
  comutil_operator_3_0_trap(),
  comutil_operator_4_3_trap(),
  comutil_operator_4_2_trap(),
  comutil_operator_4_1_trap(),
  comutil_operator_4_0_trap(),
  comutil_primitive_apply(),
  comutil_primitive_lexpr_apply(),
  comutil_apply(),
  comutil_error(),
  comutil_lexpr_apply(),
  comutil_link(),
  comutil_interrupt_closure(),
  comutil_interrupt_dlink(),
  comutil_interrupt_procedure(),
  comutil_interrupt_continuation(),
  comutil_interrupt_ic_procedure(),
  comutil_assignment_trap(),
  comutil_cache_lookup_apply(),
  comutil_lookup_trap(),
  comutil_safe_lookup_trap(),
  comutil_unassigned_p_trap(),
  comutil_decrement(),
  comutil_divide(),
  comutil_equal(),
  comutil_greater(),
  comutil_increment(),
  comutil_less(),
  comutil_minus(),
  comutil_multiply(),
  comutil_negative(),
  comutil_plus(),
  comutil_positive(),
  comutil_zero(),
  comutil_access(),
  comutil_reference(),
  comutil_safe_reference(),
  comutil_unassigned_p(),
  comutil_unbound_p(),
  comutil_assignment(),
  comutil_definition(),
  comutil_lookup_apply();

extern struct utility_result
  (*(utility_table[]))();

/*
  Utility table used by the assembly language interface to invoke
  the SCHEME_UTILITY procedures that appear in this file.

  Important: Do NOT reorder this table without changing the indices
  defined on the following page and the corresponding table in the
  compiler.
 */

struct utility_result
  (*(utility_table[]))() =
{
  comutil_return_to_interpreter,		/* 0x0 */
  comutil_operator_apply_trap,			/* 0x1 */
  comutil_operator_arity_trap,			/* 0x2 */
  comutil_operator_entity_trap,			/* 0x3 */
  comutil_operator_interpreted_trap,		/* 0x4 */
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
  comutil_access,				/* 0x2e */
  comutil_reference,				/* 0x2f */
  comutil_safe_reference,			/* 0x30 */
  comutil_unassigned_p,				/* 0x31 */
  comutil_unbound_p,				/* 0x32 */
  comutil_assignment,				/* 0x33 */
  comutil_definition,				/* 0x34 */
  comutil_lookup_apply				/* 0x35 */
  };

/* These definitions reflect the indices into the table above. */

#define TRAMPOLINE_K_RETURN			0x0
#define TRAMPOLINE_K_APPLY			0x1
#define TRAMPOLINE_K_ARITY			0x2
#define TRAMPOLINE_K_ENTITY			0x3
#define TRAMPOLINE_K_INTERPRETED		0x4
#define TRAMPOLINE_K_LEXPR_PRIMITIVE		0x5
#define TRAMPOLINE_K_PRIMITIVE			0x6
#define TRAMPOLINE_K_LOOKUP			0x7
#define TRAMPOLINE_K_1_0			0x8
#define TRAMPOLINE_K_2_1			0x9
#define TRAMPOLINE_K_2_0			0xa
#define TRAMPOLINE_K_3_2			0xb
#define TRAMPOLINE_K_3_1			0xc
#define TRAMPOLINE_K_3_0			0xd
#define TRAMPOLINE_K_4_3			0xe
#define TRAMPOLINE_K_4_2			0xf
#define TRAMPOLINE_K_4_1			0x10
#define TRAMPOLINE_K_4_0			0x11

#define TRAMPOLINE_K_OTHER			TRAMPOLINE_K_INTERPRETED

/* Main compiled code entry points.
   These are the primary entry points that the interpreter
   uses to execute compiled code.
   The other entry points are special purpose return
   points to compiled code invoked after the interpreter has been
   employed to take corrective action (interrupt, error, etc).
   They are coded adjacent to the place where the interpreter
   is invoked.
 */

C_TO_SCHEME long
enter_compiled_expression()
{
  SCHEME_OBJECT *compiled_entry_address;

  compiled_entry_address = (OBJECT_ADDRESS (Fetch_Expression ()));
  if ((COMPILED_ENTRY_FORMAT_WORD (compiled_entry_address)) !=
      (FORMAT_WORD_EXPR))
  {
    /* It self evaluates. */
    Val = (Fetch_Expression ());
    return (C_to_interface ((instruction *) (OBJECT_ADDRESS (STACK_POP ()))));
  }
  return (C_to_interface ((instruction *) compiled_entry_address));
}

C_TO_SCHEME long
apply_compiled_procedure()
{
  static long setup_compiled_invocation();
  SCHEME_OBJECT nactuals, procedure;
  instruction *procedure_entry;
  long result;

  nactuals = (STACK_POP ());
  procedure = (STACK_POP ());
  procedure_entry = ((instruction *) (OBJECT_ADDRESS (procedure)));
  result = setup_compiled_invocation ((OBJECT_DATUM (nactuals)),
                                      ((machine_word *) procedure_entry));
  if (result == PRIM_DONE)
  {
    /* Go into compiled code. */
    return (C_to_interface (procedure_entry));
  }
  else
  {
    STACK_PUSH (procedure);
    STACK_PUSH (nactuals);
    return (result);
  }
}

/* Note that this does not check that compiled_entry_address
   is a valid return address. -- Should it?
 */

C_TO_SCHEME long
return_to_compiled_code ()
{
  instruction *compiled_entry_address;

  compiled_entry_address =
    ((instruction *) (OBJECT_ADDRESS (STACK_POP ())));
  return (C_to_interface (compiled_entry_address));
}

/* NOTE: In the rest of this file, number of arguments (or minimum
   number of arguments, etc.) is always 1 greater than the number of
   arguments (it includes the procedure object).
 */

static long
setup_compiled_invocation (nactuals, compiled_entry_address)
     long nactuals;
     machine_word *compiled_entry_address;
{
  static long setup_lexpr_invocation();
  static SCHEME_OBJECT *open_gap();
  long nmin, nmax, delta;               /* all +1 */

  nmax = (COMPILED_ENTRY_MAXIMUM_ARITY (compiled_entry_address));
  if (nactuals == nmax)
  {
    /* Either the procedure takes exactly the number of arguments
       given, or it has optional arguments, no rest argument, and
       all the optional arguments have been provided.  Thus the
       frame is in the right format and we are done.
     */
    return (PRIM_DONE);
  }
  nmin = (COMPILED_ENTRY_MINIMUM_ARITY (compiled_entry_address));
  if (nmin < 0)
  {
    /* Not a procedure. */
    return (ERR_INAPPLICABLE_OBJECT);
  }
  if (nactuals < nmin)
  {
    /* Too few arguments. */
    return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  delta = (nactuals - nmax);
  if (delta <= 0)
  {
    /* The procedure takes optional arguments but no rest argument
       and not all the optional arguments have been provided.
       They must be defaulted.
     */
    ((void) (open_gap (nactuals, delta)));
    return (PRIM_DONE);
  }
  if (nmax > 0)
  {
    /* Too many arguments */
    return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  /* The procedure can take arbitrarily many arguments, ie.
     it is a lexpr.
   */
  return (setup_lexpr_invocation (nactuals, nmax));
}

/* Default some optional parameters, and return the location
   of the return address (one past the last actual argument location).
 */

static SCHEME_OBJECT *
open_gap (nactuals, delta)
     register long nactuals, delta;
{
  register SCHEME_OBJECT *gap_location, *source_location;

  /* Need to fill in optionals */

  gap_location = STACK_LOC (delta);
  source_location = STACK_LOC (0);
  Stack_Pointer = gap_location;
  nactuals -= 1;
  while ((--nactuals) > 0)
  {
    STACK_LOCATIVE_POP (gap_location) = STACK_LOCATIVE_POP (source_location);
  }
  delta = (- delta);
  while ((--delta) >= 0)
  {
    STACK_LOCATIVE_POP (source_location) = UNASSIGNED_OBJECT;
  }
  return (source_location);
}

/* Setup a rest argument as appropriate. */

static long
setup_lexpr_invocation (nactuals, nmax)
     register long nactuals, nmax;
{
  register long delta;

  /* nmax is negative! */

  delta = (nactuals + nmax);

  if (delta < 0)
  {
    /* Not enough arguments have been passed to allocate a list.
       The missing optional arguments must be defaulted, and the
       rest parameter needs to be set to the empty list.
     */

    SCHEME_OBJECT *last_loc;

    last_loc = open_gap (nactuals, delta);
    (STACK_LOCATIVE_PUSH (last_loc)) = EMPTY_LIST;
    return (PRIM_DONE);
  }
  else if (delta == 0)
  {
    /* The number of arguments passed matches exactly the number of
       formal paramters.  The last argument needs to be replaced by
       a list containing it, but there is no need to pop anything
       since the frame has the right size.
       This does not check for gc!
       The procedure should (and currently will) on entry.
     */

    register SCHEME_OBJECT temp, *gap_location, *local_free;

    local_free = Free;
    Free += 2;
    gap_location = STACK_LOC (nactuals - 2);
    temp = *gap_location;
    *gap_location = (MAKE_POINTER_OBJECT (TC_LIST, local_free));
    *local_free++ = temp;
    *local_free = EMPTY_LIST;
    return (PRIM_DONE);
  }

  else /* (delta > 0) */
  {
    /* The number of arguments passed is greater than the number of
       formal parameters named by the procedure.  Excess arguments
       need to be placed in a list passed at the last parameter
       location. The extra arguments must then be popped from the stack.
     */
    long list_size;
    register SCHEME_OBJECT *gap_location, *source_location;

    /* Allocate the list, and GC if necessary. */

    list_size = (2 * (delta + 1));
    if (GC_Check (list_size))
    {
      Request_GC (list_size);
      return (PRIM_APPLY_INTERRUPT);
    }
    gap_location = &Free[list_size];
    Free = gap_location;

    /* Place the arguments in the list, and link it. */

    source_location = (STACK_LOC (nactuals - 1));
    (*(--gap_location)) = EMPTY_LIST;

    while ((--delta) >= 0)
    {
      gap_location -= 2;
      (*(gap_location + 1)) = (STACK_LOCATIVE_PUSH (source_location));
      (*(gap_location)) = (MAKE_POINTER_OBJECT (TC_LIST, (gap_location + 1)));
    }

    (*(--gap_location)) = (STACK_LOCATIVE_PUSH (source_location));

    /* Place the list at the appropriate location in the stack. */

    STACK_LOCATIVE_REFERENCE (source_location, 0) =
      (MAKE_POINTER_OBJECT (TC_LIST, (gap_location)));

    /* Now move the arguments into their correct location in the stack
       popping any unneeded locations.
     */

    gap_location = (STACK_LOC (nactuals - 1));
    STACK_LOCATIVE_INCREMENT (source_location);

    /* Remember that nmax is originally negative! */

    for (nmax = ((-nmax) - 1); ((--nmax) >= 0); )
    {
      (STACK_LOCATIVE_PUSH (gap_location)) =
        (STACK_LOCATIVE_PUSH (source_location));
    }
    Stack_Pointer = gap_location;
    return (PRIM_DONE);
  }
}

/*
  SCHEME_UTILITYs

  Here's a mass of procedures that are called (via scheme_to_interface,
  an assembly language hook) by compiled code to do various jobs.
 */

/*
  This is how compiled Scheme code normally returns back to the
  Scheme interpreter.
  It is invoked by a trampoline, which passes the address of the
  trampoline storage block (empty) to it.
 */

SCHEME_UTILITY struct utility_result
comutil_return_to_interpreter (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  RETURN_TO_C (PRIM_DONE);
}

/*
  comutil_primitive_apply is used to invoked a C primitive.
  Note that some C primitives (the so called interpreter hooks)
  will not return normally, but will "longjmp" to the interpreter
  instead.  Thus the assembly language invoking this should have
  set up the appropriate locations in case this happens.
  After invoking the primitive, it pops the arguments off the
  Scheme stack, and proceeds by invoking the continuation on top
  of the stack.
 */

SCHEME_UTILITY struct utility_result
comutil_primitive_apply (primitive, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT primitive;
     long ignore_2, ignore_3, ignore_4;
{ 
  Metering_Apply_Primitive (Val, primitive);
  Pop_Primitive_Frame (PRIMITIVE_ARITY (primitive));
  RETURN_TO_SCHEME (OBJECT_ADDRESS (STACK_POP ()));
}

/*
  comutil_primitive_lexpr_apply is like comutil_primitive_apply
  except that it is used to invoke primitives that take
  an arbitrary number of arguments.
  The number of arguments is in the REGBLOCK_LEXPR_ACTUALS slot
  of the register block.
 */

SCHEME_UTILITY struct utility_result
comutil_primitive_lexpr_apply (primitive, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT primitive;
     long ignore_2, ignore_3, ignore_4;
{
  Metering_Apply_Primitive (Val, primitive);
  Pop_Primitive_Frame (((long) Regs[REGBLOCK_LEXPR_ACTUALS]));
  RETURN_TO_SCHEME (OBJECT_ADDRESS (STACK_POP ()));
}

/*
  comutil_apply is used by compiled code to invoke an unknown
  procedure.  It dispatches on its type to the correct place.  It
  expects the procedure to invoke, and the number of arguments (+ 1).
 */

SCHEME_UTILITY struct utility_result
comutil_apply (procedure, nactuals, ignore_3, ignore_4)
     SCHEME_OBJECT procedure;
     long nactuals, ignore_3, ignore_4;
{
  switch (OBJECT_TYPE (procedure))
  {
    case TC_COMPILED_ENTRY:
    callee_is_compiled:
    {
      instruction *entry_point;

      entry_point = ((instruction *) (OBJECT_ADDRESS (procedure)));
      RETURN_UNLESS_EXCEPTION
        ((setup_compiled_invocation (nactuals,
				     ((machine_word *) entry_point))),
         entry_point);
    }

    case TC_ENTITY:
    {
      SCHEME_OBJECT operator;

      operator = (MEMORY_REF (procedure, ENTITY_OPERATOR));
      if (!(COMPILED_CODE_ADDRESS_P (operator)))
      {
        goto callee_is_interpreted;
      }
      STACK_PUSH (procedure);           /* The entity itself */
      procedure = operator;
      nactuals += 1;
      goto callee_is_compiled;
    }

    case TC_PRIMITIVE:
    {
      /* This code depends on the fact that unimplemented
         primitives map into a "fake" primitive which accepts
         any number of arguments, thus the arity test will
         fail for unimplemented primitives.
       */

      long arity;

      arity = PRIMITIVE_ARITY (procedure);
      if (arity == (nactuals - 1))
      {
        return (comutil_primitive_apply (procedure, 0, 0, 0));
      }

      if (arity != LEXPR)
      {
        /* Wrong number of arguments. */
        STACK_PUSH (procedure);
        STACK_PUSH (nactuals);
        RETURN_TO_C (ERR_WRONG_NUMBER_OF_ARGUMENTS);
      }
      if (!(IMPLEMENTED_PRIMITIVE_P (procedure)))
      {
        /* Let the interpreter handle it. */
        goto callee_is_interpreted;
      }
      /* "Lexpr" primitive. */
      Regs[REGBLOCK_LEXPR_ACTUALS] = ((SCHEME_OBJECT) (nactuals - 1));
      return (comutil_primitive_lexpr_apply (procedure, 0, 0, 0));
    }

    callee_is_interpreted:
    default:
    {
      STACK_PUSH (procedure);
      STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
      RETURN_TO_C (PRIM_APPLY);
    }
  }
}

/*
  comutil_error is used by compiled code to signal an error.  It
  expects the arguments to the error procedure to be pushed on the
  stack, and is passed the number of arguments (+ 1).
*/

SCHEME_UTILITY struct utility_result
comutil_error (nactuals, ignore_2, ignore_3, ignore_4)
     long nactuals, ignore_2, ignore_3, ignore_4;
{
  SCHEME_OBJECT error_procedure;

  error_procedure = (Get_Fixed_Obj_Slot (Compiler_Err_Procedure));
  return (comutil_apply (error_procedure, nactuals, 0, 0));
}

/*
  comutil_lexpr_apply is invoked to reformat the frame when compiled
  code calls a known lexpr.  The actual arguments are on the stack,
  and it is given the number of arguments (WITHOUT counting the entry
  point being invoked), and the real entry point of the procedure.

  Important: This code assumes that it is always invoked with a valid
  number of arguments (the compiler checked it), and will not check.
 */

SCHEME_UTILITY struct utility_result
comutil_lexpr_apply (entry_address, nactuals, ignore_3, ignore_4)
     register instruction *entry_address;
     long nactuals;
     long ignore_3, ignore_4;
{
  RETURN_UNLESS_EXCEPTION
    ((setup_lexpr_invocation
      ((nactuals + 1),
       (COMPILED_ENTRY_MAXIMUM_ARITY (entry_address)))),
     entry_address);
}

/* Core of comutil_link and comp_link_caches_restart. */

static long
link_cc_block (block_address, offset, last_header_offset,
               sections, original_count, ret_add)
     register SCHEME_OBJECT *block_address;
     register long offset;
     long last_header_offset, sections, original_count;
     instruction *ret_add;
{
  Boolean execute_p;
  register long entry_size, count;
  SCHEME_OBJECT block;
  SCHEME_OBJECT header;
  long result, kind, total_count;
  long (*cache_handler)();

  block = (MAKE_CC_BLOCK (block_address));

  while ((--sections) >= 0)
  {
    header = (block_address[last_header_offset]);
    kind = (READ_LINKAGE_KIND (header));
    if (kind == OPERATOR_LINKAGE_KIND)
    {
      execute_p = true;
      entry_size = EXECUTE_CACHE_ENTRY_SIZE;
      cache_handler = compiler_cache_operator;
      count = (READ_OPERATOR_LINKAGE_COUNT (header));
    }
    else
    {
      execute_p = false;
      entry_size = 1;
      cache_handler = ((kind == REFERENCE_LINKAGE_KIND) ?
                       compiler_cache_lookup :
                       compiler_cache_assignment);
      count = (READ_CACHE_LINKAGE_COUNT (header));
    }

    /* This accomodates the re-entry case after a GC.
       It undoes the effects of the "smash header" code below.
     */

    if ((OBJECT_TYPE (header)) == TC_LINKAGE_SECTION)
    {
      count = (original_count - count);
      total_count = original_count;
    }
    else
    {
      total_count = count;
    }

    block_address[last_header_offset] =
      (MAKE_LINKAGE_SECTION_HEADER (kind, total_count));

    for (offset += 1; ((--count) >= 0); offset += entry_size)
    {
      SCHEME_OBJECT name;

      if (!execute_p)
      {
	name = (block_address[offset]);
      }
      else
      {
	EXTRACT_EXECUTE_CACHE_SYMBOL(name, &(block_address[offset]));
      }
      result = ((*cache_handler)(name, block, offset));

      if (result != PRIM_DONE)
      {
        /* Save enough state to continue.
	   Note that offset is decremented to compensate for it being
	   incremented by the for loop header.
	   Similary sections and count are incremented to compensate
	   for loop headers pre-decrementing.
	   count is saved although it's not needed for re-entry to
	   match the assembly language versions.
	 */

        STACK_PUSH (ENTRY_TO_OBJECT (ret_add));
        STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (sections + 1));
        STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (last_header_offset));
        STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (offset - 1));
        STACK_PUSH (block);
	STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (count + 1));

        Store_Expression (LONG_TO_UNSIGNED_FIXNUM (total_count));
        Store_Return (RC_COMP_LINK_CACHES_RESTART);
        Save_Cont ();

        /* Smash header for the garbage collector.
           It is smashed back on return.  See the comment above.
         */

        block_address[last_header_offset] =
          (MAKE_LINKAGE_SECTION_HEADER (kind, (total_count - (count + 1))));
        return (result);
      }
    }
    last_header_offset = offset;
  }
  return (PRIM_DONE);
}

/*
  comutil_link is used to initialize all the variable cache slots for
  a compiled code block.  It is called at load time, by the compiled
  code itself.  It assumes that the return address has been saved on
  the stack.
  If an error occurs during linking, or an interrupt must be processed
  (because of the need to GC, etc.), it backs out and sets up a return
  code that will invoke comp_link_caches_restart when the error/interrupt
  processing is done.
*/

SCHEME_UTILITY struct utility_result
comutil_link (ret_add, block_address, constant_address, sections)
     instruction *ret_add;
     SCHEME_OBJECT *block_address, *constant_address;
     long sections;
{
  long offset;

  offset = (constant_address - block_address);

  RETURN_UNLESS_EXCEPTION
    ((link_cc_block (block_address,
                     offset,
                     offset,
                     sections,
                     -1,
                     ret_add)),
     ret_add);
}

/*
  comp_link_caches_restart is used to continue the linking process
  started by comutil_link after the garbage collector has run.
  It expects the top of the stack to be as left by link_cc_block.
 */

C_TO_SCHEME long
comp_link_caches_restart ()
{
  SCHEME_OBJECT block;
  long original_count, offset, last_header_offset, sections, code;
  instruction *ret_add;

  original_count = (OBJECT_DATUM (Fetch_Expression ()));
  STACK_POP ();			/* Pop count, not needed */
  block = (STACK_POP ());
  offset = (OBJECT_DATUM (STACK_POP ()));
  last_header_offset = (OBJECT_DATUM (STACK_POP ()));
  sections = (OBJECT_DATUM (STACK_POP ()));
  ret_add = ((instruction *) (OBJECT_ADDRESS (STACK_POP ())));
  code = (link_cc_block ((OBJECT_ADDRESS (block)),
                         offset,
                         last_header_offset,
                         sections,
                         original_count,
                         ret_add));
  if (code == PRIM_DONE)
  {
    /* Return to the block being linked. */
    return (C_to_interface (ret_add));
  }
  else
  {
    /* Another GC or error.  We should be ready for back-out. */
    return (code);
  }
}

/* TRAMPOLINE code
   When a free variable appears in operator position in compiled code,
   there must be a directly callable procedure in the corresponding
   execute cache cell.  If, at link time, there is no appropriate
   value for the free variable, a fake compiled Scheme procedure that
   calls one of these procedures will be placed into the cell instead.

   The trampolines themselves are made by make_uuo_link,
   make_fake_uuo_link, and coerce_to_compiled.  The trampoline looks
   like a Scheme closure, containing some code to jump to one of
   these procedures and additional information to be used by the
   procedure.

   These procedures expect a single argument, the address of the
   information block where they can find the relevant data, typically
   the procedure to invoke and the number of arguments to invoke it
   with.
*/

SCHEME_UTILITY struct utility_result
comutil_operator_apply_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  /* Used by coerce_to_compiled.  TRAMPOLINE_K_APPLY */

  return (comutil_apply ((tramp_data[0]),
			 (OBJECT_DATUM (tramp_data[1])),
			 0, 0));
}

SCHEME_UTILITY struct utility_result
comutil_operator_arity_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  /* Linker saw an argument count mismatch. TRAMPOLINE_K_ARITY */

  return (comutil_apply ((tramp_data[0]),
			 (OBJECT_DATUM (tramp_data[1])),
			 0, 0));
}

SCHEME_UTILITY struct utility_result
comutil_operator_entity_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  /* Linker saw an entity to be applied. TRAMPOLINE_K_ENTITY */

  return (comutil_apply ((tramp_data[0]),
			 (OBJECT_DATUM (tramp_data[1])),
			 0, 0));
}

SCHEME_UTILITY struct utility_result
comutil_operator_interpreted_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  /* Linker saw an interpreted procedure or a procedure that it cannot
     link directly.  TRAMPOLINE_K_INTERPRETED
   */

  return (comutil_apply ((tramp_data[0]),
			 (OBJECT_DATUM (tramp_data[1])),
			 0, 0));
}

SCHEME_UTILITY struct utility_result
comutil_operator_lexpr_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  /* Linker saw a primitive of arbitrary number of arguments.
     TRAMPOLINE_K_LEXPR_PRIMITIVE
   */

  Regs[REGBLOCK_LEXPR_ACTUALS] =
    ((SCHEME_OBJECT) (OBJECT_DATUM (tramp_data[1])));
  return (comutil_primitive_lexpr_apply ((tramp_data[0]), 0, 0, 0));
}

SCHEME_UTILITY struct utility_result
comutil_operator_primitive_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  /* Linker saw a primitive of fixed matching arity. TRAMPOLINE_K_PRIMITIVE */

  return (comutil_primitive_apply ((tramp_data[0]), 0, 0, 0));
}

/* The linker either couldn't find a binding or the binding was
   unassigned, unbound, or a deep-bound (parallel processor) fluid.
   This must report the correct name of the missing variable and the
   environment in which the lookup begins for the error cases, or do
   the correct deep reference for fluids.

   "extension" is the linker object corresponding to the operator
   variable (it contains the actual value cell, the name, and linker
   tables). code_block and offset point to the cache cell in question.
   tramp_data contains extension, code_block, offset.  TRAMPOLINE_K_LOOKUP
*/

SCHEME_UTILITY struct utility_result
comutil_operator_lookup_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  extern long complr_operator_reference_trap();
  SCHEME_OBJECT true_operator, *cache_cell;
  long code, nargs;

  code = (complr_operator_reference_trap (&true_operator, (tramp_data[0])));
  cache_cell = (MEMORY_LOC ((tramp_data[1]),
			    (OBJECT_DATUM (tramp_data[2]))));
  EXTRACT_EXECUTE_CACHE_ARITY (nargs, cache_cell);
  if (code == PRIM_DONE)
  {
    return (comutil_apply (true_operator, nargs, 0, 0));
  }

  else /* Error or interrupt */
  {
    SCHEME_OBJECT *trampoline, environment, name;

    /* This could be done by bumpint tramp_data to the entry point.
       It would probably be better.
     */
    EXTRACT_EXECUTE_CACHE_ADDRESS (trampoline, cache_cell);
    environment = (compiled_block_environment (tramp_data[1]));
    name = (compiler_var_error ((tramp_data[0]), environment));

    STACK_PUSH(ENTRY_TO_OBJECT(trampoline));
    STACK_PUSH(LONG_TO_UNSIGNED_FIXNUM(nargs)); /* For debugger */
    STACK_PUSH(environment);    /* For debugger */
    Store_Expression(name);
    Store_Return(RC_COMP_OP_REF_TRAP_RESTART);
    Save_Cont();
    RETURN_TO_C(code);
  }
}

/*
  Re-start after processing an error/interrupt encountered in the previous
  utility.
  Extract the new trampoline or procedure (the user may have defined the
  missing variable) and invoke it.
 */

C_TO_SCHEME long
comp_op_lookup_trap_restart ()
{
  SCHEME_OBJECT *old_trampoline, code_block, new_procedure;
  long offset;

  /* Discard env. and nargs */

  Stack_Pointer = (Simulate_Popping (2));
  old_trampoline = (OBJECT_ADDRESS (STACK_POP ()));
  code_block = ((TRAMPOLINE_STORAGE (old_trampoline))[1]);
  offset = (OBJECT_DATUM ((TRAMPOLINE_STORAGE (old_trampoline))[2]));
  EXTRACT_EXECUTE_CACHE_ADDRESS (new_procedure,
				 (MEMORY_LOC (code_block, offset)));
  return (C_to_interface ((instruction *) (OBJECT_ADDRESS (new_procedure))));
}

/* ARITY Mismatch handling
   These receive the entry point as an argument and must fill the
   Scheme stack with the missing unassigned values.
   They are invoked by TRAMPOLINE_K_n_m where n and m are the same
   as in the name of the procedure.
   The single item of information in the trampoline data area is
   the real procedure to invoke.  All the arguments are on the
   Scheme stack.
 */

SCHEME_UTILITY struct utility_result
comutil_operator_1_0_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  STACK_PUSH (UNASSIGNED_OBJECT);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_2_1_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  SCHEME_OBJECT Top;

  Top = STACK_POP ();
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_2_0_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_3_2_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  SCHEME_OBJECT Top, Next;

  Top = STACK_POP ();
  Next = STACK_POP ();
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Next);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_3_1_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  SCHEME_OBJECT Top;

  Top = STACK_POP ();
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_3_0_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_4_3_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  SCHEME_OBJECT Top, Middle, Bottom;

  Top = STACK_POP ();
  Middle = STACK_POP ();
  Bottom = STACK_POP ();

  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Bottom);
  STACK_PUSH (Middle);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_4_2_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  SCHEME_OBJECT Top, Next;

  Top = STACK_POP ();
  Next = STACK_POP ();
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Next);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_4_1_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  SCHEME_OBJECT Top;

  Top = STACK_POP ();
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY struct utility_result
comutil_operator_4_0_trap (tramp_data, ignore_2, ignore_3, ignore_4)
     SCHEME_OBJECT *tramp_data;
     long ignore_2, ignore_3, ignore_4;
{
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

/* INTERRUPT/GC from Scheme
   The next four procedures are called from compiled code at the start
   (respectively) of a closure, continuation, interpreter compatible
   procedure, or ordinary (not closed) procedure if an interrupt has
   been detected.  They return to the interpreter if the interrupt is
   invalid after saving the state necessary to restart the compiled
   code.

   The code that handles RC_COMP_INTERRUPT_RESTART in interp.c will
   return control to comp_interrupt_restart (below).  This assumes
   that the Scheme stack contains a compiled code entry address (start
   of continuation, procedure, etc.).  The Expression register saved
   with the continuation is a piece of state that will be returned to
   Val and Env (both) upon return.
*/

#define GC_DESIRED_P()		(Free >= MemTop)

#define TEST_GC_NEEDED()						\
{									\
  if (GC_DESIRED_P())							\
  {									\
    Request_GC(Free-MemTop);						\
  }									\
}

/* Called with no arguments, closure at top of (Scheme) stack */

SCHEME_UTILITY struct utility_result
comutil_interrupt_closure (ignore_1, ignore_2, ignore_3, ignore_4)
     long ignore_1, ignore_2, ignore_3, ignore_4;
{
  TEST_GC_NEEDED();
  if ((PENDING_INTERRUPTS()) == 0)
  {
    SCHEME_OBJECT *entry_point;

    EXTRACT_CLOSURE_ENTRY_ADDRESS(entry_point,
				  (OBJECT_ADDRESS (STACK_REF (0))));
    RETURN_TO_SCHEME(((instruction *) entry_point) +
                     CLOSURE_SKIPPED_CHECK_OFFSET);
  }
  else
  {
    /* Return to interpreter to handle interrupt */
    
    Store_Expression (SHARP_F);
    Store_Return (RC_COMP_INTERRUPT_RESTART);
    Save_Cont ();
    RETURN_TO_C (PRIM_INTERRUPT);
  }
}

/* State is the live data; no entry point on the stack.
 */

static struct utility_result
compiler_interrupt_common (entry_point, state)
     instruction *entry_point;
     SCHEME_OBJECT state;
{
  TEST_GC_NEEDED();
  if ((PENDING_INTERRUPTS()) == 0)
  {
    RETURN_TO_SCHEME (entry_point + ENTRY_SKIPPED_CHECK_OFFSET);
  }
  else
  {
    STACK_PUSH (ENTRY_TO_OBJECT (entry_point));
    Store_Expression (state);
    Store_Return (RC_COMP_INTERRUPT_RESTART);
    Save_Cont ();
    RETURN_TO_C (PRIM_INTERRUPT);
  }
}

SCHEME_UTILITY struct utility_result
comutil_interrupt_dlink (entry_point, dlink, ignore_3, ignore_4)
     instruction *entry_point;
     SCHEME_OBJECT *dlink;
     long ignore_3, ignore_4;
{
  return
    (compiler_interrupt_common(entry_point,
			       MAKE_POINTER_OBJECT(TC_STACK_ENVIRONMENT,
						   dlink)));
}

SCHEME_UTILITY struct utility_result
comutil_interrupt_procedure (entry_point, ignore_2, ignore_3, ignore_4)
     instruction *entry_point;
     long ignore_2, ignore_3, ignore_4;
{
  return (compiler_interrupt_common(entry_point, SHARP_F));
}

/* Val has live data, and there is no entry address on the stack */

SCHEME_UTILITY struct utility_result
comutil_interrupt_continuation (return_address, ignore_2, ignore_3, ignore_4)
     instruction *return_address;
     long ignore_2, ignore_3, ignore_4;
{
  return (compiler_interrupt_common (return_address, Val));
}

/* Env has live data; no entry point on the stack */

SCHEME_UTILITY struct utility_result
comutil_interrupt_ic_procedure (entry_point, ignore_2, ignore_3, ignore_4)
     instruction *entry_point;
     long ignore_2, ignore_3, ignore_4;
{
  return (compiler_interrupt_common (entry_point, (Fetch_Env())));
}

C_TO_SCHEME long
comp_interrupt_restart ()
{
  Store_Env (Fetch_Expression());
  Val = (Fetch_Expression ());
  return (C_to_interface ((instruction *) (OBJECT_ADDRESS (STACK_POP ()))));
}

/* Other TRAPS */

/* Assigning a variable that has a trap in it (except unassigned) */

SCHEME_UTILITY struct utility_result
comutil_assignment_trap (return_address, extension_addr, value, ignore_4)
     instruction *return_address;
     SCHEME_OBJECT *extension_addr, value;
     long ignore_4;
{
  extern long compiler_assignment_trap();
  SCHEME_OBJECT extension;
  long code;

  extension = (MAKE_POINTER_OBJECT (TC_QUAD, extension_addr));
  code = (compiler_assignment_trap (extension, value));
  if (code == PRIM_DONE)
  {
    RETURN_TO_SCHEME (return_address);
  }
  else
  {
    SCHEME_OBJECT block, environment, name;

    STACK_PUSH(ENTRY_TO_OBJECT (return_address));
    STACK_PUSH (value);
    block = (compiled_entry_to_block (return_address));
    environment = (compiled_block_environment (block));
    STACK_PUSH (environment);
    name = (compiler_var_error (extension, environment));
    Store_Expression (name);
    Store_Return (RC_COMP_ASSIGNMENT_TRAP_RESTART);
    Save_Cont ();
    RETURN_TO_C (code);
  }
}

C_TO_SCHEME long
comp_assignment_trap_restart ()
{
  extern long Symbol_Lex_Set();
  SCHEME_OBJECT name, environment, value;
  long code;

  name = (Fetch_Expression ());
  environment = (STACK_POP ());
  value = (STACK_POP ());
  code = (Symbol_Lex_Set (environment, name, value));
  if (code == PRIM_DONE)
  {
    return (C_to_interface (OBJECT_ADDRESS (STACK_POP ())));
  }
  else
  {
    STACK_PUSH (value);
    STACK_PUSH (environment);
    Store_Expression (name);
    Store_Return (RC_COMP_ASSIGNMENT_TRAP_RESTART);
    Save_Cont ();
    return (code);
  }
}

SCHEME_UTILITY struct utility_result
comutil_cache_lookup_apply (extension_addr, block_address, nactuals, ignore_4)
     SCHEME_OBJECT *extension_addr, *block_address;
     long nactuals, ignore_4;
{
  extern long compiler_lookup_trap();
  SCHEME_OBJECT extension;
  long code;

  extension = (MAKE_POINTER_OBJECT (TC_QUAD, extension_addr));
  code = (compiler_lookup_trap (extension));
  if (code == PRIM_DONE)
  {
    return (comutil_apply (Val, nactuals, 0, 0));
  }
  else
  {
    SCHEME_OBJECT block, environment, name;

    block = (MAKE_CC_BLOCK (block_address));
    STACK_PUSH (block);
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
    environment = (compiled_block_environment (block));
    STACK_PUSH (environment);
    name = (compiler_var_error (extension, environment));
    Store_Expression (name);
    Store_Return (RC_COMP_CACHE_REF_APPLY_RESTART);
    Save_Cont ();
    RETURN_TO_C (code);
  }
}

C_TO_SCHEME long
comp_cache_lookup_apply_restart ()
{
  extern long Symbol_Lex_Ref();
  SCHEME_OBJECT name, environment, block;
  long code;

  name = (Fetch_Expression ());
  environment = (STACK_POP ());
  code = (Symbol_Lex_Ref (environment, name));
  if (code == PRIM_DONE)
  {
    /* Replace block with actual operator */
    (*(STACK_LOC (1))) = Val;
    if (COMPILED_CODE_ADDRESS_P (Val))
    {
      return (apply_compiled_procedure ());
    }
    else
    {
      return (PRIM_APPLY);
    }
  }
  else
  {
    STACK_PUSH (environment);
    Store_Expression (name);
    Store_Return (RC_COMP_CACHE_REF_APPLY_RESTART);
    Save_Cont ();
    return (code);
  }
}

/* Variable reference traps:
   Reference to a free variable that has a reference trap -- either a
   fluid or an error (unassigned / unbound)
 */

#define CMPLR_REF_TRAP(name, c_trap, ret_code, restart, c_lookup)	\
SCHEME_UTILITY struct utility_result					\
name (return_address, extension_addr, ignore_3, ignore_4)		\
     instruction *return_address;					\
     SCHEME_OBJECT *extension_addr;					\
     long ignore_3, ignore_4;						\
{									\
  extern long c_trap();							\
  long code;								\
  SCHEME_OBJECT extension;						\
									\
  extension = (MAKE_POINTER_OBJECT (TC_QUAD, extension_addr));		\
  code = c_trap (extension);						\
  if (code == PRIM_DONE)						\
  {									\
    RETURN_TO_SCHEME (return_address);					\
  }									\
  else									\
  {									\
    SCHEME_OBJECT block, environment, name;				\
									\
    STACK_PUSH (ENTRY_TO_OBJECT (return_address));			\
    block = (compiled_entry_to_block (return_address));			\
    environment = (compiled_block_environment (block));			\
    STACK_PUSH (environment);						\
    name = (compiler_var_error (extension, environment));		\
    Store_Expression (name);						\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    RETURN_TO_C (code);							\
  }									\
}									\
									\
C_TO_SCHEME long							\
restart ()								\
{									\
  extern long c_lookup();						\
  SCHEME_OBJECT name, environment;					\
  long code;								\
									\
  name = (Fetch_Expression ());						\
  environment = (STACK_POP ());						\
  code = (c_lookup (environment, name));				\
  if (code == PRIM_DONE)						\
  {									\
    return (C_to_interface (OBJECT_ADDRESS (STACK_POP ())));		\
  }									\
  else									\
  {									\
    STACK_PUSH (environment);						\
    Store_Expression (name);						\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    return (code);							\
  }									\
}

/* Actual traps */

CMPLR_REF_TRAP(comutil_lookup_trap,
               compiler_lookup_trap,
               RC_COMP_LOOKUP_TRAP_RESTART,
               comp_lookup_trap_restart,
               Symbol_Lex_Ref);

CMPLR_REF_TRAP(comutil_safe_lookup_trap,
               compiler_safe_lookup_trap,
               RC_COMP_SAFE_REF_TRAP_RESTART,
               comp_safe_lookup_trap_restart,
               safe_symbol_lex_ref);

CMPLR_REF_TRAP(comutil_unassigned_p_trap,
               compiler_unassigned_p_trap,
               RC_COMP_UNASSIGNED_TRAP_RESTART,
               comp_unassigned_p_trap_restart,
               Symbol_Lex_unassigned_p);

/* NUMERIC ROUTINES
   Invoke the arithmetic primitive in the fixed objects vector.
   The Scheme arguments are expected on the Scheme stack.
 */

#define COMPILER_ARITH_PRIM(name, fobj_index, arity)			\
SCHEME_UTILITY struct utility_result					\
name (ignore_1, ignore_2, ignore_3, ignore_4)				\
     long ignore_1, ignore_2, ignore_3, ignore_4;			\
{									\
  SCHEME_OBJECT handler;						\
									\
  handler = (Get_Fixed_Obj_Slot (fobj_index));				\
  return (comutil_apply (handler, (arity), 0, 0));			\
}

COMPILER_ARITH_PRIM (comutil_decrement, GENERIC_TRAMPOLINE_PREDECESSOR, 2);
COMPILER_ARITH_PRIM (comutil_divide, GENERIC_TRAMPOLINE_DIVIDE, 3);
COMPILER_ARITH_PRIM (comutil_equal, GENERIC_TRAMPOLINE_EQUAL_P, 3);
COMPILER_ARITH_PRIM (comutil_greater, GENERIC_TRAMPOLINE_GREATER_P, 3);
COMPILER_ARITH_PRIM (comutil_increment, GENERIC_TRAMPOLINE_SUCCESSOR, 2);
COMPILER_ARITH_PRIM (comutil_less, GENERIC_TRAMPOLINE_LESS_P, 3);
COMPILER_ARITH_PRIM (comutil_minus, GENERIC_TRAMPOLINE_SUBTRACT, 3);
COMPILER_ARITH_PRIM (comutil_multiply, GENERIC_TRAMPOLINE_MULTIPLY, 3);
COMPILER_ARITH_PRIM (comutil_negative, GENERIC_TRAMPOLINE_NEGATIVE_P, 2);
COMPILER_ARITH_PRIM (comutil_plus, GENERIC_TRAMPOLINE_ADD, 3);
COMPILER_ARITH_PRIM (comutil_positive, GENERIC_TRAMPOLINE_POSITIVE_P, 2);
COMPILER_ARITH_PRIM (comutil_zero, GENERIC_TRAMPOLINE_ZERO_P, 2);

/*
  Obsolete SCHEME_UTILITYs used to handle first class environments.
  They have been superseded by the variable caching code.
  They are here for completeness, and because the code in the compiler
  that uses them has not yet been spliced out, although it is switched
  off.
*/

#define CMPLR_REFERENCE(util_name, c_proc, ret_code, restart_name)	\
SCHEME_UTILITY struct utility_result					\
util_name (ret_add, environment, variable, ignore_4)			\
     instruction *ret_add;						\
     SCHEME_OBJECT environment, variable;				\
     long ignore_4;							\
{									\
  extern long c_proc();							\
  long code;								\
									\
  code = (c_proc (environment, variable));				\
  if (code == PRIM_DONE)						\
  {									\
    RETURN_TO_SCHEME (ret_add);						\
  }									\
  else									\
  {									\
    STACK_PUSH (ENTRY_TO_OBJECT (ret_add));				\
    STACK_PUSH (variable);						\
    Store_Expression (environment);					\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    RETURN_TO_C (code);							\
  }									\
}									\
									\
C_TO_SCHEME long							\
restart_name ()								\
{									\
  extern long c_proc();							\
  SCHEME_OBJECT environment, variable;					\
  long code;								\
									\
  environment = (Fetch_Expression ());					\
  variable = (STACK_POP ());						\
  code = (c_proc (environment, variable));				\
  if (code == PRIM_DONE)						\
  {									\
    Regs[REGBLOCK_ENV] = environment;					\
    return (C_to_interface (OBJECT_ADDRESS (STACK_POP ())));		\
  }									\
  else									\
  {									\
    STACK_PUSH (variable);						\
    Store_Expression (environment);					\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    return (code);							\
  }									\
}

#define CMPLR_ASSIGNMENT(util_name, c_proc, ret_code, restart_name)	\
SCHEME_UTILITY struct utility_result					\
util_name (ret_add, environment, variable, value)			\
     instruction *ret_add;						\
     SCHEME_OBJECT environment, variable, value;			\
{									\
  extern long c_proc();							\
  long code;								\
									\
  code = (c_proc (environment, variable, value));			\
  if (code == PRIM_DONE)						\
  {									\
    RETURN_TO_SCHEME (ret_add);						\
  }									\
  else									\
  {									\
    STACK_PUSH (ENTRY_TO_OBJECT (ret_add));				\
    STACK_PUSH (value);							\
    STACK_PUSH (variable);						\
    Store_Expression (environment);					\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    RETURN_TO_C (code);							\
  }									\
}									\
									\
C_TO_SCHEME long							\
restart_name ()								\
{									\
  extern long c_proc();							\
  SCHEME_OBJECT environment, variable, value;				\
  long code;								\
									\
  environment = (Fetch_Expression ());					\
  variable = (STACK_POP ());						\
  value = (STACK_POP ());						\
  code = (c_proc (environment, variable, value));			\
  if (code == PRIM_DONE)						\
  {									\
    Regs[REGBLOCK_ENV] = environment;					\
    return (C_to_interface (OBJECT_ADDRESS (STACK_POP ())));		\
  }									\
  else									\
  {									\
    STACK_PUSH (value);							\
    STACK_PUSH (variable);						\
    Store_Expression (environment);					\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    return (code);							\
  }									\
}

CMPLR_REFERENCE(comutil_access,
		Symbol_Lex_Ref,
		RC_COMP_ACCESS_RESTART,
		comp_access_restart);

CMPLR_REFERENCE(comutil_reference,
		Lex_Ref,
		RC_COMP_REFERENCE_RESTART,
		comp_reference_restart);

CMPLR_REFERENCE(comutil_safe_reference,
		safe_lex_ref,
		RC_COMP_SAFE_REFERENCE_RESTART,
		comp_safe_reference_restart);

CMPLR_REFERENCE(comutil_unassigned_p,
		Symbol_Lex_unassigned_p,
		RC_COMP_UNASSIGNED_P_RESTART,
		comp_unassigned_p_restart);

CMPLR_REFERENCE(comutil_unbound_p,
		Symbol_Lex_unbound_p,
		RC_COMP_UNBOUND_P_RESTART,
		comp_unbound_p_restart);

CMPLR_ASSIGNMENT(comutil_assignment,
		 Lex_Set,
		 RC_COMP_ASSIGNMENT_RESTART,
		 comp_assignment_restart);

CMPLR_ASSIGNMENT(comutil_definition,
		 Local_Set,
		 RC_COMP_DEFINITION_RESTART,
		 comp_definition_restart);

SCHEME_UTILITY struct utility_result
comutil_lookup_apply (environment, variable, nactuals, ignore_4)
     SCHEME_OBJECT environment, variable;
     long nactuals, ignore_4;
{
  extern long Lex_Ref();
  long code;

  code = (Lex_Ref (environment, variable));
  if (code == PRIM_DONE)
  {
    return (comutil_apply (Val, nactuals, 0, 0));
  }
  else
  {
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
    STACK_PUSH (variable);
    Store_Expression (environment);
    Store_Return (RC_COMP_LOOKUP_APPLY_RESTART);
    Save_Cont ();
    RETURN_TO_C (code);
  }
}

C_TO_SCHEME long
comp_lookup_apply_restart ()
{
  extern long Lex_Ref();
  SCHEME_OBJECT environment, variable;
  long code;

  environment = (Fetch_Expression ());
  variable = (STACK_POP ());
  code = (Lex_Ref (environment, variable));
  if (code == PRIM_DONE)
  {
    SCHEME_OBJECT nactuals;

    nactuals = (STACK_POP ());
    STACK_PUSH (Val);
    STACK_PUSH (nactuals);
    if (COMPILED_CODE_ADDRESS_P (Val))
    {
      return (apply_compiled_procedure ());
    }
    else
    {
      return (PRIM_APPLY);
    }
  }
  else
  {
    STACK_PUSH (variable);
    Store_Expression (environment);
    Store_Return (RC_COMP_LOOKUP_APPLY_RESTART);
    Save_Cont ();
    return (code);
  }
}

/* Procedures to destructure compiled entries and closures. */

/*
  Extract the debugging information attached to `block'.  Usually
  this is a string which contains the filename where the debugging
  info is stored.
 */

C_UTILITY SCHEME_OBJECT
compiled_block_debugging_info (block)
     SCHEME_OBJECT block;
{
  long length;

  length = (VECTOR_LENGTH (block));
  return (FAST_MEMORY_REF (block, (length - 1)));
}

/* Extract the environment where the `block' was "loaded". */

C_UTILITY SCHEME_OBJECT
compiled_block_environment (block)
     SCHEME_OBJECT block;
{
  long length;

  length = (VECTOR_LENGTH (block));
  return (FAST_MEMORY_REF (block, length));
}

/*
  Given `entry', a Scheme object representing a compiled code entry point,
  it returns the address of the block to which it belongs.
 */

C_UTILITY SCHEME_OBJECT *
compiled_entry_to_block_address (entry)
     SCHEME_OBJECT entry;
{
  SCHEME_OBJECT *block_address;

  Get_Compiled_Block (block_address, (OBJECT_ADDRESS (entry)));
  return (block_address);
}

C_UTILITY SCHEME_OBJECT
compiled_entry_to_block (entry)
     SCHEME_OBJECT entry;
{
  SCHEME_OBJECT *block_address;

  Get_Compiled_Block (block_address, (OBJECT_ADDRESS (entry)));
  return (MAKE_CC_BLOCK (block_address));
}

/* Returns the offset from the block to the entry point. */

C_UTILITY long
compiled_entry_to_block_offset (entry)
     SCHEME_OBJECT entry;
{
  SCHEME_OBJECT *entry_address, *block_address;

  entry_address = (OBJECT_ADDRESS (entry));
  Get_Compiled_Block (block_address, entry_address);
  return (((char *) entry_address) - ((char *) block_address));
}

/*
  Check whether the compiled code block whose address is `block_addr'
  is a compiled closure block.
 */

static long
block_address_closure_p (block_addr)
     SCHEME_OBJECT *block_addr;
{
  SCHEME_OBJECT header_word;

  header_word = (*block_addr);
  return (((OBJECT_TYPE (header_word)) == TC_MANIFEST_CLOSURE));
}

/*
  Check whether the compiled code block `block' is a compiled closure block.
 */

C_UTILITY long
compiled_block_closure_p (block)
     SCHEME_OBJECT block;
{
  return (block_address_closure_p (OBJECT_ADDRESS (block)));
}

/*
  Check whether the compiled entry point `entry' is a compiled closure.
 */

C_UTILITY long
compiled_entry_closure_p (entry)
     SCHEME_OBJECT entry;
{
  return (block_address_closure_p (compiled_entry_to_block_address (entry)));
}

/*
  Extract the entry point ultimately invoked by the compiled closure
  represented by `entry'.
 */

C_UTILITY SCHEME_OBJECT
compiled_closure_to_entry (entry)
     SCHEME_OBJECT entry;
{
  SCHEME_OBJECT *real_entry;

  EXTRACT_CLOSURE_ENTRY_ADDRESS (real_entry,
				 (OBJECT_ADDRESS (entry)));
  return (ENTRY_TO_OBJECT (real_entry));
}

/*
  Store the information for `entry' into `buffer'.
  This is used by the printer and debugging utilities.
 */

/* Kinds and subkinds of entries. */

#define KIND_PROCEDURE                          0
#define KIND_CONTINUATION                       1
#define KIND_EXPRESSION                         2
#define KIND_OTHER                              3
#define KIND_ILLEGAL                            4

/* Continuation subtypes */

#define CONTINUATION_NORMAL                     0
#define CONTINUATION_DYNAMIC_LINK               1
#define CONTINUATION_RETURN_TO_INTERPRETER      2

C_UTILITY void
compiled_entry_type (entry, buffer)
     SCHEME_OBJECT entry, *buffer;
{
  long kind, min_arity, max_arity, field1, field2;
  SCHEME_OBJECT *entry_address;

  entry_address = (OBJECT_ADDRESS (entry));
  max_arity = (COMPILED_ENTRY_FORMAT_HIGH (entry_address));
  min_arity = (COMPILED_ENTRY_FORMAT_LOW (entry_address));
  field1 = min_arity;
  field2 = max_arity;
  if (min_arity >= 0)
  {
    kind = KIND_PROCEDURE;
  }
  else if (max_arity >= 0)
  {
    kind = KIND_ILLEGAL;
  }
  else if ((((unsigned long) max_arity) & 0xff) < 0xe0)
  {
    /* Field2 is the offset to the next continuation */

    kind = KIND_CONTINUATION;
    field1 = CONTINUATION_NORMAL;
    field2 = (((((unsigned long) max_arity) & 0x3f) << 7) |
              (((unsigned long) min_arity) & 0x7f));
  }
  else if (min_arity != (-1))
  {
    kind = KIND_ILLEGAL;
  }

  else
  {
    switch (max_arity)
    {
      case FORMAT_BYTE_EXPR:
      {
        kind = KIND_EXPRESSION;
        break;
      }
      case FORMAT_BYTE_COMPLR:
      case FORMAT_BYTE_CMPINT:
      {
        kind = KIND_OTHER;
        break;
      }
      case FORMAT_BYTE_DLINK:
      {
        kind = KIND_CONTINUATION;
        field1 = CONTINUATION_DYNAMIC_LINK;
        field2 = -1;
        break;
      }
      case FORMAT_BYTE_RETURN:
      {
        kind = KIND_CONTINUATION;
        field1 = CONTINUATION_RETURN_TO_INTERPRETER;
        field2 = 0;
        break;
      }
      default:
      {
        kind = KIND_ILLEGAL;
        break;
      }
    }
  }
  buffer[0] = kind;
  buffer[1] = field1;
  buffer[2] = field2;
  return;
}

/* Destructuring free variable caches. */

C_UTILITY void
store_variable_cache (extension, block, offset)
     SCHEME_OBJECT extension, block;
     long offset;
{
  FAST_MEMORY_SET (block, offset,
                   ((SCHEME_OBJECT) (OBJECT_ADDRESS (extension))));
  return;
}

C_UTILITY SCHEME_OBJECT
extract_variable_cache (block, offset)
     SCHEME_OBJECT block;
     long offset;
{
  return (MAKE_POINTER_OBJECT (TRAP_EXTENSION_TYPE,
                               ((SCHEME_OBJECT *)
                                (FAST_MEMORY_REF (block, offset)))));
}

/* Get a compiled procedure from a cached operator reference. */

C_UTILITY SCHEME_OBJECT
extract_uuo_link (block, offset)
     SCHEME_OBJECT block;
     long offset;
{
  SCHEME_OBJECT *cache_address, *compiled_entry_address;

  cache_address = (MEMORY_LOC (block, offset));
  EXTRACT_EXECUTE_CACHE_ADDRESS (compiled_entry_address, cache_address);
  return ENTRY_TO_OBJECT(compiled_entry_address);
}

static void
store_uuo_link (entry, cache_address)
     SCHEME_OBJECT entry, *cache_address;
{
  SCHEME_OBJECT *entry_address;

  entry_address = (OBJECT_ADDRESS (entry));
  STORE_EXECUTE_CACHE_CODE (cache_address);
  STORE_EXECUTE_CACHE_ADDRESS (cache_address, entry_address);
  return;
}

/* This makes a fake compiled procedure which traps to kind handler when
   invoked.  WARNING: this won't work if instruction alignment is more
   restricted than simple longword alignment.
 */

#define TRAMPOLINE_SIZE	(TRAMPOLINE_ENTRY_SIZE + 2)

static long
make_trampoline (slot, format_word, kind, size, value1, value2, value3)
     SCHEME_OBJECT *slot;
     machine_word format_word;
     long kind, size;
     SCHEME_OBJECT value1, value2, value3;
{
  SCHEME_OBJECT *block, *local_free, *entry_point;

  if (GC_Check (TRAMPOLINE_SIZE + size))
  {
    Request_GC (TRAMPOLINE_SIZE + size);
    return (PRIM_INTERRUPT);
  }

  local_free = Free;
  Free += (TRAMPOLINE_SIZE + size);
  block = local_free;
  local_free[0] = (MAKE_OBJECT (TC_MANIFEST_VECTOR,
				((TRAMPOLINE_SIZE - 1) + size)));
  local_free[1] = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,
				TRAMPOLINE_ENTRY_SIZE));
  local_free += TRAMPOLINE_BLOCK_TO_ENTRY;
  entry_point = local_free;
  local_free = (TRAMPOLINE_STORAGE(entry_point));
  (COMPILED_ENTRY_FORMAT_WORD (entry_point)) = format_word;
  (COMPILED_ENTRY_OFFSET_WORD (entry_point)) =
    (MAKE_OFFSET_WORD (entry_point, block, false));
  STORE_TRAMPOLINE_ENTRY (entry_point, kind);

  if ((--size) >= 0)
  {
    *local_free++ = value1;
  }
  if ((--size) >= 0)
  {
    *local_free++ = value2;
  }
  if ((--size) >= 0)
  {
    *local_free++ = value3;
  }
  *slot = (ENTRY_TO_OBJECT (entry_point));
  return (PRIM_DONE);
}

/* Standard trampolines. */

static long
make_redirection_trampoline (slot, kind, procedure)
     SCHEME_OBJECT *slot;
     long kind;
     SCHEME_OBJECT procedure;
{
  return (make_trampoline (slot,
			   ((machine_word) FORMAT_WORD_CMPINT),
			   kind,
			   1,
			   procedure,
			   SHARP_F,
			   SHARP_F));
}

static long
make_apply_trampoline (slot, kind, procedure, nactuals)
     SCHEME_OBJECT *slot;
     long kind, nactuals;
     SCHEME_OBJECT procedure;
{
  return (make_trampoline (slot,
			   ((machine_word) FORMAT_WORD_CMPINT),
			   kind,
			   2,
			   procedure,
			   (LONG_TO_UNSIGNED_FIXNUM (nactuals)),
			   SHARP_F));
}

#define TRAMPOLINE_TABLE_SIZE   4

static long
trampoline_arity_table[TRAMPOLINE_TABLE_SIZE * TRAMPOLINE_TABLE_SIZE] =
{
  TRAMPOLINE_K_1_0,		/* 1_0 */
  TRAMPOLINE_K_ARITY,		/* 1_1 should not get here */
  TRAMPOLINE_K_ARITY,		/* 1_2 should not get here */
  TRAMPOLINE_K_ARITY,		/* 1_3 should not get here */
  TRAMPOLINE_K_2_0,		/* 2_0 */
  TRAMPOLINE_K_2_1,		/* 2_1 */
  TRAMPOLINE_K_ARITY,		/* 2_2 should not get here */
  TRAMPOLINE_K_ARITY,		/* 2_3 should not get here */
  TRAMPOLINE_K_3_0,		/* 3_0 */
  TRAMPOLINE_K_3_1,		/* 3_1 */
  TRAMPOLINE_K_3_2,		/* 3_2 */
  TRAMPOLINE_K_ARITY,		/* 3_3 should not get here */
  TRAMPOLINE_K_4_0,		/* 4_0 */
  TRAMPOLINE_K_4_1,		/* 4_1 */
  TRAMPOLINE_K_4_2,		/* 4_2 */
  TRAMPOLINE_K_4_3		/* 4_3 */
};

/*
  make_uuo_link is called by C and initializes a compiled procedure
  cache at a location given by a block and an offset.

  make_uuo_link checks its procedure argument, and:

  - If it is not a compiled procedure, an entity, or a primitive
  procedure with a matching number of arguments, it stores a fake
  compiled procedure which will invoke comutil_operator_interpreted_trap
  when invoked.

  - If its argument is an entity, it stores a fake compiled procedure
  which will invoke comutil_operator_entity_trap when invoked.

  - If its argument is a primitive, it stores a fake compiled procedure
  which will invoke comutil_operator_primitive_trap, or
  comutil_operator_lexpr_trap when invoked.

  - If its argument is a compiled procedure that expects more or
  less arguments than those provided, it stores a fake compiled
  procedure which will invoke comutil_operator_arity_trap, or one of
  its specialized versions when invoked.

  - Otherwise, the actual (compatible) operator is stored.
*/

C_UTILITY long
make_uuo_link (procedure, extension, block, offset)
     SCHEME_OBJECT procedure, extension, block;
     long offset;
{
  long kind, result, nactuals;
  SCHEME_OBJECT trampoline, *cache_address;

  cache_address = (MEMORY_LOC (block, offset));
  EXTRACT_EXECUTE_CACHE_ARITY (nactuals, cache_address);
  /* nactuals >= 0 */

  switch (OBJECT_TYPE (procedure))
  {
    case TC_COMPILED_ENTRY:
    {
      SCHEME_OBJECT *entry;
      long nmin, nmax;

      entry = (OBJECT_ADDRESS (procedure));
      nmax = (COMPILED_ENTRY_MAXIMUM_ARITY (entry));
      if (nactuals == nmax)
      {
        store_uuo_link (procedure, cache_address);
        return (PRIM_DONE);
      }
      nmin = (COMPILED_ENTRY_MINIMUM_ARITY (entry));

      if ((nmax > 1) && (nmin > 0) && (nmin <= nactuals) &&
          (nactuals <= TRAMPOLINE_TABLE_SIZE) &&
          (nmax <= (TRAMPOLINE_TABLE_SIZE + 1)))
      {
        kind = (trampoline_arity_table[((nmax - 2) * TRAMPOLINE_TABLE_SIZE) +
				       (nactuals - 1)]);
	/* Paranoia */
	if (kind != TRAMPOLINE_K_ARITY)
	{
	  nactuals = 0;
	  break;
	}
      }
      kind = TRAMPOLINE_K_ARITY;
      break;
    }

    case TC_ENTITY:
    {
      kind = TRAMPOLINE_K_ENTITY;
      break;
    }

    case TC_PRIMITIVE:
    {
      long arity;
      extern long primitive_to_arity ();

      arity = primitive_to_arity (procedure);
      if (arity == (nactuals - 1))
      {
	nactuals = 0;
        kind = TRAMPOLINE_K_PRIMITIVE;
      }
      else if (arity == LEXPR_PRIMITIVE_ARITY)
      {
        kind = TRAMPOLINE_K_LEXPR_PRIMITIVE;
      }
      else
      {
        kind = TRAMPOLINE_K_OTHER;
      }
      break;
    }

    case TC_PROCEDURE: /* and some others... */
    default:
    uuo_link_interpreted:
    {
      kind = TRAMPOLINE_K_INTERPRETED;
      break;
    }
  }
  if (nactuals == 0)
  {
    result = (make_redirection_trampoline (&trampoline, kind, procedure));
  }
  else
  {
    result = (make_apply_trampoline (&trampoline, kind, procedure, nactuals));
  }
  if (result != PRIM_DONE)
  {
    return (result);
  }
  store_uuo_link (trampoline, cache_address);
  return (PRIM_DONE);
}

C_UTILITY long
make_fake_uuo_link (extension, block, offset)
     SCHEME_OBJECT extension, block;
     long offset;
{
  long result;
  SCHEME_OBJECT trampoline, *cache_address;

  result = (make_trampoline (&trampoline,
			     ((machine_word) FORMAT_WORD_CMPINT),
			     TRAMPOLINE_K_LOOKUP,
			     3,
			     extension,
			     block,
			     (LONG_TO_UNSIGNED_FIXNUM (offset))));
  if (result != PRIM_DONE)
  {
    return (result);
  }
  cache_address = (MEMORY_LOC (block, offset));
  store_uuo_link (trampoline, cache_address);
  return (PRIM_DONE);
}

/* C_UTILITY long fake_uuo_link_p does not appear to be used anymore */

C_UTILITY long
coerce_to_compiled (procedure, arity, location)
     SCHEME_OBJECT procedure, *location;
     long arity;
{
  long frame_size;

  frame_size = (arity + 1);
  if ((!(COMPILED_CODE_ADDRESS_P (procedure))) ||
      ((COMPILED_ENTRY_MAXIMUM_ARITY (OBJECT_ADDRESS (procedure))) !=
       frame_size))
  {
    if (frame_size > FORMAT_BYTE_FRAMEMAX)
    {
      return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    }
    return (make_trampoline (location,
			     ((machine_word)
			      (MAKE_FORMAT_WORD (frame_size, frame_size))),
			     TRAMPOLINE_K_APPLY,
			     2,
			     procedure,
			     (LONG_TO_UNSIGNED_FIXNUM (frame_size)),
			     SHARP_F));
  }
  (*location) = procedure;
  return (PRIM_DONE);
}

/* Initialization */

#define COMPILER_INTERFACE_VERSION		2

#define COMPILER_REGBLOCK_N_FIXED		16
#define COMPILER_REGBLOCK_N_HOOKS		64
#define COMPILER_REGBLOCK_N_TEMPS		128

#if (REGBLOCK_MINIMUM_LENGTH > COMPILER_REGBLOCK_N_FIXED)
#include "error: cmpint.c and const.h disagree on REGBLOCK_MINIMUM_LENGTH!"
#endif

#define COMPILER_FIXED_SIZE	1	/* ((sizeof(long)) / (sizeof(long))) */

#ifndef COMPILER_HOOK_SIZE
#define COMPILER_HOOK_SIZE	(EXECUTE_CACHE_ENTRY_SIZE)
#endif

#ifndef COMPILER_TEMP_SIZE
#define COMPILER_TEMP_SIZE	((sizeof (double)) / (sizeof (long)))
#endif

#define REGBLOCK_LENGTH							\
((COMPILER_REGBLOCK_N_FIXED * COMPILER_FIXED_SIZE)		+	\
 (COMPILER_REGBLOCK_N_HOOKS * COMPILER_HOOK_SIZE)		+	\
 (COMPILER_REGBLOCK_N_TEMPS * COMPILER_TEMP_SIZE))

long
  compiler_processor_type,
  compiler_interface_version;

SCHEME_OBJECT
  compiler_utilities,
  return_to_interpreter;

#ifndef ASM_REGISTER_BLOCK
SCHEME_OBJECT
  Registers[REGBLOCK_LENGTH];
#endif

static void
compiler_reset_internal ()
{
  /* Other stuff can be placed here. */

#ifdef ASM_RESET_HOOK
  ASM_RESET_HOOK();
#endif

  return_to_interpreter =
    (ENTRY_TO_OBJECT ((SCHEME_OBJECT *)
		      ((OBJECT_ADDRESS (compiler_utilities)) +
		       TRAMPOLINE_BLOCK_TO_ENTRY)));

  return;
}

C_UTILITY void
compiler_reset (new_block)
     SCHEME_OBJECT new_block;
{
  /* Called after a disk restore */

  if ((OBJECT_TYPE (new_block)) != TC_COMPILED_CODE_BLOCK)
  {
    extern void compiler_reset_error ();

    compiler_reset_error ();
  }
  else
  {
    compiler_utilities = new_block;
    compiler_reset_internal ();
  }
  return;
}

C_UTILITY void
compiler_initialize (fasl_p)
     long fasl_p;
{
  /* Start-up of whole interpreter */

  long code;
  SCHEME_OBJECT trampoline, *block;

  compiler_processor_type = COMPILER_PROCESSOR_TYPE;
  compiler_interface_version = COMPILER_INTERFACE_VERSION;
  if (fasl_p)
  {
    extern SCHEME_OBJECT *copy_to_constant_space();

    code = (make_trampoline (&trampoline,
			     FORMAT_WORD_RETURN,
			     TRAMPOLINE_K_RETURN,
			     0, SHARP_F, SHARP_F, SHARP_F));
    if (code != PRIM_DONE)
    {
      fprintf (stderr,
	       "compiler_initialize: Not enough space!\n");
      Microcode_Termination (TERM_NO_SPACE);
    }
    block = (compiled_entry_to_block_address (trampoline));
    block = (copy_to_constant_space (block, (1 + (OBJECT_DATUM (block[0])))));
    compiler_utilities = (MAKE_CC_BLOCK (block));
    compiler_reset_internal ();
  }
  else
  {
    compiler_utilities = SHARP_F;
    return_to_interpreter = SHARP_F;
  }
  return;
}
