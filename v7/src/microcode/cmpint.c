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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpint.c,v 1.4 1989/06/13 08:21:36 jinx Exp $
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
 * Procedures in this file belong to the following categories:
 *
 * Local C procedures.  These are local procedures called only by
 * other procedures in this file, and have been separated only for
 * modularity reasons.  They are tagged with the C keyword `static'.
 *
 * C interface entries.  These procedures are called from the
 * interpreter (written in C) and ultimately enter the Scheme compiled
 * code world by using the assembly language utility
 * `enter_compiled_code'.  They are tagged with the noise word
 * `C_TO_SCHEME'.
 *
 * C utility procedures.  These procedures are called from C
 * primitives and other subsystems and never leave the C world.  They
 * constitute the compiled code data abstraction as far as other C
 * parts of the Scheme "microcode" are concerned.  They are tagged
 * with the noise word `C_UTILITY'.
 *
 * Scheme interface utilities.  These procedures are called from
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

/* Macro imports */

#include "config.h"	/* Pointer type declaration and machine dependencies */
#include "object.h"	/* Making and destructuring Scheme objects */
#include "sdata.h"	/* Needed by const.h */
#include "types.h"	/* Needed by const.h */
#include "errors.h"	/* Error codes and Termination codes */
#include "const.h"	/* REGBLOCK_MINIMUM_LENGTH and PRIM_... codes */
#include "trap.h"	/* UNASSIGNED_OBJECT, TRAP_EXTENSION_TYPE */
#include "interp.h"	/* Interpreter state and primitive destructuring */
#include "prims.h"	/* LEXPR */
#include "cmpint.h"	/* Compiled code object destructuring */
#include "cmpgc.h"	/* Compiled code object relocation */
#include "default.h"	/* Metering_Apply_Primitive */

/* Imports from the rest of the "microcode" */

extern term_type
  Microcode_Termination();

extern long
  compiler_cache_operator(),
  compiler_cache_lookup(),
  compiler_cache_assignment();

/* Exports to the rest of the "microcode" */

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
  compiled_entry_type();

/* Imports from assembly language */

extern long
  enter_compiled_code();

/* Exports to assembly language */

extern long
  comutil_error(),
  comutil_apply(),
  comutil_setup_lexpr(),
  comutil_link();

extern Pointer
  comutil_invoke_primitive();

/* Main compiled code entry points. */

C_TO_SCHEME long
enter_compiled_expression()
{
  Pointer compiled_entry_address;

  compiled_entry_address = (Get_Pointer(Fetch_Expression ()));
  if ((COMPILED_ENTRY_FORMAT_WORD (compiled_entry)) !=
      (FORMAT_WORD_EXPRESSION))
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
  static long setup_compiled_invocation();
  Pointer nactuals, procedure;
  machine_word *procedure_entry;
  long result;

  nactuals = (Pop ());
  procedure = (Pop ());
  procedure_entry = ((machine_word *) (Get_Pointer(procedure)));
  result = setup_compiled_invocation ((OBJECT_DATUM (nactuals)),
				      (procedure_entry));
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
  /* Note that this does not check that compiled_entry_address
     is a valid return address. -- Should it?
   */
  return (enter_compiled_code (compiled_entry_address));
}

/* NOTE: In the rest of this file, number of arguments (or minimum
   number of arguments, etc.) is always 1 greater than the number of
   arguments (it includes the procedure object).
 */

static long
setup_compiled_invocation (nactuals, compiled_entry_address)
     register long nactuals;
     register machine_word *compiled_entry_address;
{
  static long setup_lexpr_invocation();
  static Pointer *open_gap();
  register long nmin, nmax, delta;	/* all +1 */

  nmax = (COMPILED_ENTRY_MAXIMUM_ARITY(compiled_entry_address));
  if (nactuals == nmax)
  {
    /* Either the procedure takes exactly the number of arguments
       given, or it has optional arguments, no rest argument, and
       all the optional arguments have been provided.  Thus the
       frame is in the right format and we are done.
     */
    return (PRIM_DONE);
  }
  nmin = (COMPILED_ENTRY_MINIMUM_ARITY(compiled_entry_address));
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
  return (setup_lexpr_invocation (nactuals, nmax));
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

    register Pointer temp, *gap_location, *local_free;

    local_free = Free;
    Free += 2;
    gap_location = STACK_LOC(nactuals - 2);
    temp = *gap_location;
    *gap_location = (Make_Pointer (TC_LIST, local_free));
    *local_free++ = temp;
    *local_free = NIL;
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
    register Pointer *gap_location, *source_location;

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

    /* Remember that nmax is originally negative! */

    for (nmax = ((-nmax) - 1); ((--max) >= 0); )
    {
      STACK_LOCATIVE_PUSH(gap_location) = STACK_LOCATIVE_PUSH(source_location);
    }
    Stack_Pointer = gap_location;
    return (PRIM_DONE);
  }
}

/*
  comutil_apply is used by compiled code when calling unknown
  procedures. It expects the arguments to be pushed on
  the stack, and is given the number of arguments and the
  procedure object to invoke.  It returns the following codes:

  PRIM_DONE:
    The procedure being invoked is compiled, the frame is "ready to go",
    and the procedure's entry point is in the Val interpreter "register".

  PRIM_APPLY:
    The procedure being applied is a primitive, the primitive object is
    in the Val interpreter "register", and we are ready to go.

  PRIM_REENTER:
    The procedure being invoked needs to be applied by the interpreter.
    The frame has already been prepared.

  PRIM_APPLY_INTERRUPT:
    The procedure being invoked has a rest argument and the system needs
    to garbage collect before proceeding with the application.

  ERR_INAPPLICABLE_OBJECT:
    The object being invoked is not a procedure.

  ERR_WRONG_NUMBER_OF_ARGUMENTS:
    The procedure being invoked has been given the wrong number of arguments.
*/

SCHEME_UTILITY long
comutil_apply (nactuals, procedure)
     long nactuals;
     Pointer procedure;
{
  switch (OBJECT_TYPE(procedure))
  {
    callee_is_compiled:
    case TC_COMPILED_ENTRY:
    {
      machine_word *entry_point;

      entry_point = ((machine_word *) (Get_Pointer(procedure)));
      Val = ((Pointer) entry_point);
      return (setup_compiled_invocation (nactuals, entry_point));
    }

    case TC_ENTITY:
    {
      Pointer operator;

      operator = Vector_Ref(procedure, entity_operator);
      if ((OBJECT_TYPE(operator)) != TC_COMPILED_ENTRY)
	goto callee_is_interpreted;
      Push(procedure);		/* The entity itself */
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

      arity = PRIMITIVE_ARITY(procedure);
      if (arity == (nactuals - 1))
      {
	/* We are all set. */
	Val = procedure;
	return (PRIM_APPLY);
      }
      if (arity != LEXPR)
      {
	/* Wrong number of arguments. */
	Push(procedure);
	Push(nactuals);
	return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
      }
      if (!(IMPLEMENTED_PRIMITIVE_P(procedure)))
      {
	/* Let the interpreter handle it. */
	goto callee_is_interpreted;
      }
      /* "Lexpr" primitive. */
      Regs[REGBLOCK_LEXPR_ACTUALS] = ((Pointer) (nactuals - 1));
      Val = procedure;
      return (PRIM_APPLY);
    }
      
    callee_is_interpreted:
    default:
    {
      Push(procedure);
      Push(MAKE_UNSIGNED_FIXNUM(nactuals));
      return (PRIM_REENTER);
    }
  }
}

/*
  comutil_error is used by compiled code to signal an error.  It
  expects the arguments to the error procedure to be pushed on the
  stack, and is passed the number of arguments.
*/

SCHEME_UTILITY long
comutil_error (nactuals)
     long nactuals;
{
  Pointer error_procedure;

  error_procedure = (Get_Fixed_Obj_Slot(Compiler_Err_Procedure));
  return (comutil_apply (nactuals, error_procedure));
}

/*
  comutil_setup_lexpr is invoked to reformat the frame when compiled
  code calls a known lexpr.  The actual arguments are on the stack,
  and it is given the number of arguments (WITHOUT the entry point
  being invoked).

  Important: This code assumes that it is always invoked with a valid
  number of arguments (the compiler checked it), and will not check.
 */

SCHEME_UTILITY long
comutil_setup_lexpr (nactuals, compiled_entry_address)
     register long nactuals;
     register machine_word *compiled_entry_address;
{
  return (setup_lexpr_invocation
	  ((nactuals + 1),
	   (COMPILED_ENTRY_MAXIMUM_ARITY(compiled_entry_address))));
}
/*
  comutil_invoke_primitive is used to invoked a C primitive.
  It returns the value returned by the C primitive.
  Note that some C primitives (the so called interpreter hooks)
  will not return normally, but will "longjmp" to the interpreter
  instead.  Thus the assembly language invoking this should have
  set up the appropriate locations in case this happens.
 */

SCHEME_UTILITY Pointer
comutil_invoke_primitive (primitive)
     register Pointer primitive;
{
  Pointer result;

  Metering_Apply_Primitive(result, primitive);
  return (result);
}

/* Core of comutil_link and comutil_continue_linking. */

#define MAKE_LINKAGE_SECTION_HEADER(kind, count)			\ \
Make_Non_Pointer(TC_LINKAGE_SECTION,					\
		 (kind |						\
		  ((kind != OPERATOR_LINKAGE_KIND) ?			\
		   count :						\
		   (count * OPERATOR_LINK_ENTRY_SIZE))))

static long
link_cc_block (block_address, offset, last_header_offset,
	       sections, original_count)
     register Pointer block_address;
     register long offset;
     long last_header_offset, sections, original_count;
{
  register long entry_size, count;
  register Pointer block;
  Pointer header;
  long result, kind, total_count;
  long (*cache_handler)();

  block = Make_Pointer(TC_COMPILED_CODE_BLOCK, block_address);

  while ((--sections) >= 0)
  {
    header = (block_address[last_header_offset]);
    kind = (READ_LINKAGE_KIND(header));
    if (kind == OPERATOR_LINKAGE_KIND)
    {
      entry_size = OPERATOR_LINK_ENTRY_SIZE;
      cache_handler = compiler_cache_operator;
      count = (READ_OPERATOR_LINKAGE_COUNT(header)); 
    }
    else
    {
      entry_size = 1;
      cache_handler = ((kind == REFERENCE_LINKAGE_KIND) ?
		       compiler_cache_lookup :
		       compiler_cache_assignment);
      count = (READ_CACHE_LINKAGE_COUNT(header)); 
    }

    /* This accomodates the re-entry case after a GC.
       It undoes the effects of the "Smash header" code below.
     */

    total_count = ((OBJECT_TYPE(header) == TC_LINKAGE_SECTION) ?
		   original_count :
		   count);
    block_address[last_header_offset] =
      (MAKE_LINKAGE_SECTION_HEADER(kind, total_count));

    for (offset += 1; ((--count) >= 0); offset += entry_size)
    {
      result = ((*cache_handler)
		(block_address[offset],	/* symbol */
		 block,
		 offset));

      if (result != PRIM_DONE)
      {
	/* Save enough state to continue. */

	Push(MAKE_UNSIGNED_FIXNUM(sections + 1));
	Push(MAKE_UNSIGNED_FIXNUM(last_header_offset));
	Push(MAKE_UNSIGNED_FIXNUM(offset - 1));
	Push(block);
	Push(MAKE_UNSIGNED_FIXNUM(total_count));

	/* Smash header for the garbage collector.
	   It is smashed back on return.  See the comment above.
	 */

	block_address[last_header_offset] =
	  (MAKE_LINKAGE_SECTION_HEADER(kind, (total_count - (count + 1))));
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
  It returns PRIM_DONE if finished, or PRIM_INTERRUPT if the garbage
  collector must be run.  In the latter case, the stack is all set
  for reentry.
*/

SCHEME_UTILITY long
comutil_link (block_address, constant_address, sections)
     Pointer *block_address, *constant_address;
     long sections;
{
  long offset;

  offset = (constant_address - block_address);
  return (link_cc_block (block_address,
			 offset,
			 offset,
			 sections,
			 -1));
}

/*
  comutil_continue_linking is used to continue the linking process
  started by comutil_link after the garbage collector has run.
  It expects the top of the stack to be as left by comutil_link.
 */

SCHEME_UTILITY long
comutil_continue_linking ()
{
  Pointer block;
  long original_count, offset, last_header_offset, sections;

  original_count = (OBJECT_DATUM(Pop()));
  block = (Pop());
  offset = (OBJECT_DATUM(Pop()));
  last_header_offset = (OBJECT_DATUM(Pop()));
  sections = (OBJECT_DATUM(Pop()));
  return (link_cc_block ((Get_Pointer(block)),
			 last_header_offset,
			 offset,
			 sections,
			 original_count));
}

/* Procedures to destructure compiled entries and closures. */

/*
  Extract the debugging information attached to `block'.  Usually
  this is a string which contains the filename where the debugging
  info is stored.
*/

C_UTILITY Pointer
compiled_block_debugging_info(block)
     Pointer block;
{
  long length;

  length = Vector_Length(block);
  return (Fast_Vector_Ref(block, (length - 1)));
}

/* Extract the environment where the `block' was "loaded". */

C_UTILITY Pointer
compiled_block_environment(block)
     Pointer block;
{
  long length;

  length = Vector_Length(block);
  return (Fast_Vector_Ref(block, length));
}

/*
  Given `entry', a Scheme object representing a compiled code entry point,
  it returns the address of the block to which it belongs.
 */

C_UTILITY Pointer *
compiled_entry_to_block_address(entry)
     Pointer entry;
{
  Pointer *block_address;

  Get_Compiled_Block(block_address, (Get_Pointer(entry)));
  return (block_address);
}

/* Returns the offset from the block to the entry point. */

C_UTILITY long
compiled_entry_to_block_offset(entry)
     Pointer entry;
{
  Pointer *entry_address, block_address;

  entry_address = (Get_Pointer(entry));
  Get_Compiled_Block(block_address, entry_address);
  return (((char *) entry_address) - ((char *) block_address));
}

/*
  Check whether the compiled code block whose address is `block_addr'
  is a compiled closure block.
 */

static long
block_address_closure_p(block_addr)
     Pointer *block_addr;
{
  Pointer header_word;

  header_word = (*block_addr);
  return ((OBJECT_TYPE(header_word) == TC_MANIFEST_CLOSURE));
}

/*
  Check whether the compiled code block `block' is a compiled closure block.
 */

C_UTILITY long
compiled_block_manifest_closure_p(block)
     Pointer block;
{
  return (block_address_closure_p(Get_Pointer(block)));
}

/*
  Check whether the compiled procedure `entry' is a compiled closure.
 */

C_UTILITY long
compiled_entry_manifest_closure_p(entry)
     Pointer entry;
{
  return (block_address_closure_p(compiled_entry_to_block_address(entry));
}

/*
  Extract the entry point ultimately invoked by the compiled closure
  represented by `entry'.
 */

C_UTILITY Pointer
compiled_closure_to_entry(entry)
     Pointer entry;
{
  Pointer *real_entry, *block;

  Get_Compiled_Block(blck, Get_Pointer(entry));
  EXTRACT_COMPILED_CLOSURE_ENTRY_ADDRESS(real_entry, block);
  return (Make_Pointer(TC_COMPILED_ENTRY, real_entry));
}

/*
  Store the information for `entry' into `buffer'.
  This is used by the printer and debugging utilities.
 */

/* Kinds and subkinds of entries. */

#define KIND_PROCEDURE				0
#define KIND_CONTINUATION			1
#define KIND_EXPRESSION				2
#define KIND_OTHER				3
#define KIND_ILLEGAL				4

/* Continuation subtypes */

#define CONTINUATION_NORMAL			0
#define CONTINUATION_DYNAMIC_LINK		1
#define CONTINUATION_RETURN_TO_INTERPRETER	2

C_UTILITY void
compiled_entry_type(entry, buffer)
     Pointer entry, *buffer;
{
  long kind, min_arity, max_arity, field1, field2;
  Pointer *entry_address;

  entry_address = (Get_Pointer(entry));
  max_arity = (COMPILED_ENTRY_FORMAT_HIGH(entry_address));
  min_arity = (COMPILED_ENTRY_FORMAT_LOW(entry_address));
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
store_variable_cache(extension, block, offset)
     Pointer extension, block;
     long offset;
{
  Fast_Vector_Set(block, offset, ((Pointer) (Get_Pointer(extension))));
  return;
}

C_UTILITY Pointer
extract_variable_cache(block, offset)
     Pointer block;
     long offset;
{
  return (Make_Pointer(TRAP_EXTENSION_TYPE,
		       ((Pointer *) (Fast_Vector_Ref(block, offset)))));
}

/* Get a compiled procedure from a cached operator reference. */

C_UTILITY Pointer
extract_uuo_link(block, offset)
     Pointer block;
     long offset;
{
  Pointer *cache_address, *compiled_entry_address;

  cache_address = Nth_Vector_Loc(block, offset);
  EXTRACT_OPERATOR_LINK_ADDRESS(compiled_entry_address, cache_address);
  return (Make_Pointer(TC_COMPILED_ENTRY, compiled_entry_address));
}

static void
store_uuo_link(entry, cache_address)
     Pointer entry, *cache_address;
{
  Pointer *entry_address;

  entry_address = (Get_Pointer(entry));
  STORE_OPERATOR_LINK_INSTRUCTION(cache_address);
  STORE_OPERATOR_LINK_ADDRESS(cache_address, entry_address);
  return;
}

/* This makes a fake compiled procedure which traps to kind handler when
   invoked.
 */

static long
make_trampoline(slot, format_word, kind, size, value1, value2, value3)
     Pointer *slot;
     machine_word format_word;
     long kind, size;
     Pointer value1, value2, value3;
{
  Pointer *block, *local_free;

  if (GC_Check(TRAMPOLINE_SIZE + size))
  {
    Request_GC(TRAMPOLINE_SIZE + size);
    return (PRIM_INTERRUPT);
  }
  
  local_free = Free;
  Free += (TRAMPOLINE_SIZE + size);
  block = local_free;
  *local_free++ = (Make_Non_Pointer(TC_MAIFEST_VECTOR,
				    ((TRAMPOLINE_SIZE - 1) + size)));
  *local_free++ = (Make_Non_Pointer(TC_MANIFEST_NM_VECTOR,
				    (TRAMPOLINE_ENTRY_SIZE + 1)));
  local_free += 1;
  (COMPILED_ENTRY_FORMAT_WORD(local_free)) = format_word;
  (COMPILED_ENTRY_OFFSET_WORD(local_free)) =
    (MAKE_OFFSET_WORD(local_free, block, false));
  STORE_TRAMPOLINE_ENTRY(local_free, kind);
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
  *slot = (Make_Pointer(TC_COMPILED_ENTRY, block));
  return (PRIM_DONE);
}

static long
make_simple_trampoline(slot, kind, procedure)
     Pointer *slot;
     long kind;
     Pointer procedure;
{
  return (make_trampoline(slot,
			  ((machine_word) FORMAT_WORD_CMPINT), kind,
			  1, procedure, NIL, NIL));
}

#define TRAMPOLINE_TABLE_SIZE	4

static long 
trampoline_arity_table[TRAMPOLINE_TABLE_SIZE * TRAMPOLINE_TABLE_SIZE] =
{
  TRAMPOLINE_1_0,			/* 1_0 */
  TRAMPOLINE_ARITY,			/* 1_1 should not get here */
  TRAMPOLINE_ARITY,			/* 1_2 should not get here */
  TRAMPOLINE_ARITY,			/* 1_3 should not get here */
  TRAMPOLINE_2_0,			/* 2_0 */
  TRAMPOLINE_2_1,			/* 2_1 */
  TRAMPOLINE_ARITY,			/* 2_2 should not get here */
  TRAMPOLINE_ARITY,			/* 2_3 should not get here */
  TRAMPOLINE_3_0,			/* 3_0 */
  TRAMPOLINE_3_1,			/* 3_1 */
  TRAMPOLINE_3_2,			/* 3_2 */
  TRAMPOLINE_ARITY,			/* 3_3 should not get here */
  TRAMPOLINE_4_0,			/* 4_0 */
  TRAMPOLINE_4_1,			/* 4_1 */
  TRAMPOLINE_4_2,			/* 4_2 */
  TRAMPOLINE_4_3			/* 4_3 */
};

/*
  make_uuo_link is called by C and initializes a compiled procedure
  cache at a location given by a block and an offset.

  make_uuo_link checks its procedure argument, and:

  - If it is not a compiled procedure, an entity, or a primitive
  procedure with a matching number of arguments, it stores a fake
  compiled procedure which will invoke comentry_operator_interpreted_trap
  when invoked.

  - If its argument is an entity, it stores a fake compiled procedure
  which will invoke comentry_operator_entity_trap when invoked.

  - If its argument is a primitive, it stores a fake compiled procedure
  which will invoke comentry_operator_primitive_trap, or
  comentry_operator_lexpr_trap when invoked.

  - If its argument is a compiled procedure that expects more or
  less arguments than those provided, it stores a fake compiled
  procedure which will invoke comentry_operator_arity_trap, or one of
  its specialized versions when invoked.

  - Otherwise, the actual (compatible) operator is stored.
*/

C_UTILITY long
make_uuo_link(procedure, extension, block, offset)
     Pointer procedure, extension, block;
     long offset;
{
  long kind, result, nactuals;
  Pointer trampoline, *cache_address;
  
  cache_address = Nth_Vector_Loc(block, offset);
  EXTRACT_OPERATOR_LINK_ARITY(nactuals, cache_address);

  switch (OBJECT_TYPE(procedure))
  {
    case TC_COMPILED_ENTRY:
    {
      Pointer *entry;
      long nmin, nmax;
      
      entry = (Get_Pointer(procedure));
      nmax = (COMPILED_ENTRY_MAXIMUM_ARITY(entry));
      if (nactuals == nmax)
      {
	store_uuo_link(procedure, cache_address);
	return (PRIM_DONE);
      }
      nmin = (COMPILED_ENTRY_MINIMUM_ARITY(entry));

      if ((nmax > 0) && (nmin > 0) && (nmin <= nactuals) &&
	  (nactuals <= TRAMPOLINE_TABLE_SIZE) &&
	  (nmax <= (TRAMPOLINE_TABLE_SIZE + 1)))
      {
	kind = trampoline_arity_table[((nmax - 1) * TRAMPOLINE_TABLE_SIZE) +
				      nactuals];
      }
      else
      {
	kind = TRAMPOLINE_ARITY;
      }
      break;
    }

    case TC_ENTITY:
    {
      kind = TRAMPOLINE_ENTITY;
      break;
    }

    case TC_PRIMITIVE:
    {
      long arity;
      extern long primitive_to_arity();

      arity = primitive_to_arity(procedure);
      if (arity == (nactuals - 1))
      {
	kind = TRAMPOLINE_PRIMITIVE;
      }
      else if (arity == LEXPR_PRIMITIVE_ARITY)
      {
	kind = TRAMPOLINE_LEXPR_PRIMITIVE;
      }
      else
      {
	kind = TRAMPOLINE_INTERPRETED;
      }
      break;
    }
    
    default:
    uuo_link_interpreted:
    {
      kind = TRAMPOLINE_INTERPRETED;
      break;
    }
  }
  result = make_simple_trampoline(&trampoline, kind, procedure);
  if (result != PRIM_DONE)
  {
    return (result);
  }
  store_uuo_link(trampoline, cache_address);
  return (PRIM_DONE);
}

C_UTILITY long
make_fake_uuo_link(extension, block, offset)
     Pointer extension, block;
     long offset;
{
  Pointer trampoline, *cache_address;

  result = make_trampoline(&trampoline,
			   ((machine_word) FORMAT_WORD_CMPINT),
			   TRAMPOLINE_LOOKUP, 3,
			   extension, block,
			   MAKE_UNSIGNED_FIXNUM(offset));
  if (result != PRIM_DONE)
  {
    return (result);
  }
  cache_address = Nth_Vector_Loc(block, offset);
  store_uuo_link(trampoline, cache_address);
  return (PRIM_DONE);
}

C_UTILITY long
coerce_to_compiled(procedure, arity, location)
     Pointer procedure, *location;
     long arity;
{
  long frame_size;

  frame_size = (arity + 1)
  if ((OBJECT_TYPE(procedure) != TC_COMPILED_ENTRY) ||
      ((COMPILED_ENTRY_MAXIMUM_ARITY(Get_Pointer(procedure))) !=
       frame_size))
  {
    if (frame_size > FORMAT_BYTE_FRAMEMAX)
    {
      return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    }
    return (make_trampoline(location,
			    ((machine_word)
			     (MAKE_FORMAT_WORD(frame_size, frame_size))),
			    TRAMPOLINE_INVOKE, 1,
			    procedure, NIL, NIL));
  }
  *location = procedure;
  return (PRIM_DONE);
}

/* *** HERE *** */

/* Priorities:

   - uuo link manipulation
   - initialization and register block
   - error back outs
   - arithmetic
 */

Pointer
  Registers[REGBLOCK_MINIMUM_LENGTH],
  compiler_utilities,
  return_to_interpreter;

long
  compiler_interface_version,
  compiler_processor_type;

/* Missing entry points. */

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
