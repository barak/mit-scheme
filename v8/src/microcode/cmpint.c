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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/cmpint.c,v 1.1 1989/06/02 14:49:59 jinx Exp $
 *
 * This file corresponds to
 * $COMPILER-Header: compiler.c,v 9.35 88/10/26 20:02:13 GMT cph Exp $
 * $MC68020-Header: cmp68020.m4,v 9.86 89/04/19 02:24:19 GMT arthur Exp $
 *
 * Compiled code interface.  Portable version.
 * This file requires a bit of assembly language described in cmpaux.m4
 * See also the files cmpint.h, cmpgc.h, and cmpint.txt .
 *
 */

/*
 * Procedures in this file divide into the following categories:
 *
 * 0: local C procedures.  These are static procedures used only by
 * this file.  They are called by the other procedures in this file,
 * and have been separated only for modularity reasons.  They are
 * tagged with the C keyword `static'.
 *
 * 1: C interface entries.  These procedures are called from C and
 * ultimately enter the Scheme compiled code world by using the
 * assembly language utility `enter_compiled_code'.  They are tagged
 * with the noise word `C_TO_SCHEME'.
 *
 * 2: C utility procedures.  These procedures are called from C and
 * never leave the C world.  They constitute the compiled code data
 * abstraction as far as other C parts of the Scheme system are
 * concerned.  They are tagged with the noise word `C_UTILITY'.
 *
 * 3: Scheme interface utilities.  These procedures are called from
 * the assembly language interface and return to it.  They never leave
 * the Scheme compiled code world.  If an error occurs or an interrupt
 * must be processed, they return an exit code to the assembly language
 * code that calls them.  They are tagged with the noise word
 * `SCHEME_UTILITY'.
 *
 */

/* Make noise words invisible to the C compiler. */

#define C_UTILITY
#define C_TO_SCHEME
#define SCHEME_UTILITY

#include "config.h"	/* Pointer type declaration */
#include "object.h"	/* Making pointers */
#include "sdata.h"	/* Needed by const.h */
#include "types.h"	/* Needed by const.h */
#include "errors.h"	/* Error codes and Termination codes */
#include "const.h"	/* REGBLOCK_MINIMUM_LENGTH */
#include "returns.h"	/* RC_POP_FROM_COMPILED_CODE */
#include "trap.h"	/* UNASSIGNED_OBJECT */
#include "cmpint.h"

/* Exports */

extern long
  compiler_interface_version,
  compiler_processor_type;

extern Pointer
  Registers[],
  compiler_utilities,
  return_to_interpreter;

extern long
  enter_compiled_expression(), 
  apply_compiled_procedure(),
  return_to_compiled_code(),
  make_fake_uuo_link(),
  make_uuo_link(),
  compiled_block_manifest_closure_p(),
  compiled_entry_manifest_closure_p(),
  compiled_entry_to_block_offset();

extern Pointer
  extract_uuo_link(),
  extract_variable_cache(),
  compiled_block_debugging_info(),
  compiled_block_environment(),
  compiled_closure_to_entry(),
  *compiled_entry_to_block_address();

extern void
  store_variable_cache(),
  compiled_entry_type(),
  Microcode_Termination();

/* Imports from assembly language */

extern long
  enter_compiled_code();

C_TO_SCHEME long
enter_compiled_expression()
{
  Pointer compiled_entry_address;

  compiled_entry_address = (Get_Pointer(Fetch_Expression ()));
  if ((COMPILED_ENTRY_FORMAT_WORD (compiled_entry)) !=
      (EXPRESSION_FORMAT_WORD))
  {
    /* It self evaluates. */
    Val = (Fetch_Expression ());
    return (PRIM_DONE);
  }
  return (enter_compiled_code (compiled_entry_address));
}

C_TO_SCHEME long
apply_compiled_procedure()
{
  static long setup_compiled_application();
  Pointer nactuals, procedure;
  machine_word *procedure_entry;
  long result;

  nactuals = (Pop ());
  procedure = (Pop ());
  procedure_entry = ((machine_word *) (Get_Pointer(procedure)));
  result = setup_compiled_application ((OBJECT_DATUM (nactuals)),
				       procedure_entry);
  if (result == PRIM_DONE)
  {
    /* Go into compiled code. */
    return (enter_compiled_code (procedure_entry));
  }
  else
  {
    Push (procedure);
    Push (nactuals);
    return (result);
  }
}

C_TO_SCHEME long
return_to_compiled_code ()
{
  register Pointer *compiled_entry_address;

  compiled_entry_address = (Get_Pointer (Pop ()));
  /* *** No checking here? *** */
  return (enter_compiled_code (compiled_entry_address));
}

static long
setup_compiled_application (nactuals, compiled_entry_address)
     register long nactuals;
     register machine_word *compiled_entry_address;
{
  static long setup_lexpr_application();
  static Pointer *open_gap();
  register long nmin, nmax, delta;	/* all +1 */

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
    ((void) (open_gap(nactuals, delta)));
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
  return (setup_lexpr_application (nactuals, nmin, nmax));
}

/* Default some optional parameters, and return the location
   of the return address (one past the last actual argument location).
 */

static Pointer *
open_gap (nactuals, delta)
     register long nactuals, delta;
{
  register Pointer *gap_location, *source_location;

  /* Need to fill in optionals */

  gap_location = STACK_LOC(delta);
  source_location = STACK_LOC(0);
  Stack_Pointer = gap_location;
  while ((--nactuals) > 0)
  {
    STACK_LOCATIVE_POP(gap_location) = STACK_LOCATIVE_POP(source_location);
  }
  delta = (- delta);
  while ((--delta) >= 0)
  {
    STACK_LOCATIVE_POP(source_location) = UNASSIGNED_OBJECT;
  }
  return (source_location);
}

/* Setup a rest argument as appropriate. */

static long
setup_lexpr_application (nactuals, nmin, nmax)
     register long nactuals, nmin, nmax;
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

    Pointer *last_loc;

    last_loc = open_gap(nactuals, delta);
    (STACK_LOCATIVE_PUSH(last_loc)) = NIL;
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

    register Pointer temp, *gap_location;

    gap_location = STACK_LOC(nactuals - 2);
    temp = *gap_location;
    *gap_location = (Make_Pointer (TC_LIST, Free));
    *Free++ = temp;
    *Free++ = NIL;
    return (PRIM_DONE);
  }

  else /* (delta > 0) */
  {
    /* The number of arguments passed is greater than the number of
       formal parameters named by the procedure.  Excess arguments
       need to be placed in a list passed at the last parameter
       location. The extra arguments must then be popped from the stack.
     */
    register Pointer *gap_location, *source_location;

    /* Allocate the list, and GC if necessary. */

    gap_location = &Free[2 * (delta + 1)];
    if (GC_Check (gap_location - Free))
    {
      Request_GC (gap_location - Free);
      return (PRIM_APPLY_INTERRUPT);
    }

    /* Place the arguments in the list, and link it. */

    source_location = (STACK_LOC(nactuals - 1));
    (*(--gap_location)) = NIL;

    while ((--delta) >= 0)
    {
      gap_location -= 2;
      (*(gap_location + 1)) = (STACK_LOCATIVE_PUSH(source_location));
      (*(gap_location)) = (Make_Pointer(TC_LIST, (gap_location + 1)));
    }

    (*(--gap_location)) = (STACK_LOCATIVE_PUSH(source_location));

    /* Place the list at the appropriate location in the stack. */

    STACK_LOCATIVE_REFERENCE(source_location, 0) =
      (Make_Pointer(TC_LIST, (gap_location)));

    /* Now move the arguments into their correct location in the stack
       popping any unneeded locations.
     */

    gap_location = (STACK_LOC(nactuals - 1));
    STACK_LOCATIVE_INCREMENT(source_location);
    nmin -= 1;
    while ((--nmin) >= 0)
    {
      STACK_LOCATIVE_PUSH(gap_location) = STACK_LOCATIVE_PUSH(source_location);
    }
    Stack_Pointer = gap_location;
    return (PRIM_DONE);
  }
}

/*
  This entry point is invoked to reformat the frame when compiled code
  calls a known lexpr.
  Important: This assumes that it is always invoked with a valid
  number of arguments (the compiler checked it), and will not check.
 */

SCHEME_UTILITY long
invoke_lexpr (nactuals, compiled_entry_address)
     register long nactuals;
     register machine_word *compiled_entry_address;
{
  /* Use setup_lexpr_application */
/* *** HERE *** */
}

Pointer
  Registers[REGBLOCK_MINIMUM_LENGTH],
  compiler_utilities,
  return_to_interpreter;

long
  compiler_interface_version,
  compiler_processor_type;

/* Bad entry points. */

long
make_fake_uuo_link(extension, block, offset)
     Pointer extension, block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
make_uuo_link(value, extension, block, offset)
     Pointer value, extension, block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer
extract_uuo_link(block, offset)
     Pointer block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

void
store_variable_cache(extension, block, offset)
     Pointer extension, block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer
extract_variable_cache(block, offset)
     Pointer block;
     long offset;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer
compiled_block_debugging_info(block)
     Pointer block;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer
compiled_block_environment(block)
     Pointer block;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
compiled_block_manifest_closure_p(block)
     Pointer block;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer *
compiled_entry_to_block_address(entry)
     Pointer entry;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
compiled_entry_to_block_offset(entry)
     Pointer entry;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

void
compiled_entry_type(entry, buffer)
     Pointer entry, *buffer;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
compiled_entry_manifest_closure_p(entry)
     Pointer entry;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

Pointer
compiled_closure_to_entry(entry)
     Pointer entry;
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

#define losing_return_address(name)					\
extern long name();							\
long									\
name()									\
{									\
  Microcode_Termination (TERM_COMPILER_DEATH);				\
  /*NOTREACHED*/							\
}

losing_return_address (comp_interrupt_restart)
losing_return_address (comp_lookup_apply_restart)
losing_return_address (comp_reference_restart)
losing_return_address (comp_access_restart)
losing_return_address (comp_unassigned_p_restart)
losing_return_address (comp_unbound_p_restart)
losing_return_address (comp_assignment_restart)
losing_return_address (comp_definition_restart)
losing_return_address (comp_safe_reference_restart)
losing_return_address (comp_lookup_trap_restart)
losing_return_address (comp_assignment_trap_restart)
losing_return_address (comp_op_lookup_trap_restart)
losing_return_address (comp_cache_lookup_apply_restart)
losing_return_address (comp_safe_lookup_trap_restart)
losing_return_address (comp_unassigned_p_trap_restart)
losing_return_address (comp_link_caches_restart)

/* NOP entry points */

extern void
  compiler_reset(),
  compiler_initialize();

extern long
  coerce_to_compiled();

void
compiler_reset (new_block)
     Pointer new_block;
{
  extern void compiler_reset_error();

  if (new_block != NIL)
  {
    compiler_reset_error();
  }
  return;
}

void
compiler_initialize ()
{
  compiler_processor_type = 0;
  compiler_interface_version = 0;
  compiler_utilities = NIL;
  return_to_interpreter =
    (Make_Non_Pointer (TC_RETURN_CODE, RC_POP_FROM_COMPILED_CODE));
  return;
}

/* Identity procedure */

long
coerce_to_compiled(object, arity, location)
     Pointer object, *location;
     long arity;
{
  *location = object;
  return (PRIM_DONE);
}
