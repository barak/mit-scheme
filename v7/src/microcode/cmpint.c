/* -*-C-*-

$Id: cmpint.c,v 1.99 2002/11/20 19:46:07 cph Exp $

Copyright (c) 1989-2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/*
 *
 * Compiled code interface.  Portable version.
 * This file requires a bit of assembly language from cmpaux-md.m4
 * See also the files cmpint.txt, cmpgc.h, and cmpint-md.h .
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

#include "config.h"
#include <stdio.h>
#ifdef STDC_HEADERS
#  include <stdlib.h>
#endif
#include "dstack.h"	/* Dynamic-stack support */
#include "outf.h"	/* error reporting */
#include "types.h"      /* Needed by const.h */
#include "const.h"      /* REGBLOCK_MINIMUM_LENGTH and PRIM_... codes */
#include "object.h"     /* Making and destructuring Scheme objects */
#include "intrpt.h"	/* Interrupt processing macros */
#include "gc.h"		/* Request_GC, etc. */
#include "sdata.h"	/* ENTITY_OPERATOR */
#include "errors.h"     /* Error codes and Termination codes */
#include "returns.h"	/* Return addresses in the interpreter */
#include "fixobj.h"	/* To find the error handlers */
#include "stack.h"	/* Stacks and stacklets */
#include "interp.h"     /* Interpreter state and primitive destructuring */
#include "default.h"    /* various definitions */
#include "extern.h"	/* External decls (missing Cont_Debug, etc.) */
#include "trap.h"       /* UNASSIGNED_OBJECT, CACHE_TYPE */
#include "prims.h"      /* LEXPR */
#include "prim.h"	/* Primitive_Procedure_Table, etc. */

#define ENTRY_TO_OBJECT(entry)						\
  (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY, ((SCHEME_OBJECT *) (entry))))

#define IN_CMPINT_C
#include "cmpgc.h"      /* Compiled code object relocation */

#include "lookup.h"

#ifdef HAS_COMPILER_SUPPORT

/* ASM_ENTRY_POINT, EXFNX, and DEFNX are for OS/2.  The IBM C Set++/2
   compiler has several different external calling conventions.  The
   default calling convention is called _Optlink, uses a combination
   of registers and the stack, and is complicated.  The calling
   convention used for operating system interface procedures is called
   _System, uses only the stack, and is very similar to the calling
   conventions used with our DOS compilers.  So, in order to simplify
   the changes to the assembly language, we use _System conventions
   for calling C procedures from the assembly language file.

   Since _Optlink is the default, we must somehow cause the relevant
   procedures to be compiled using _System.  The easiest way to do
   this is to force the use of _System everywhere, but that's
   undesirable since _Optlink is generally more efficient.  Instead,
   we use the ASM_ENTRY_POINT wrapper to cause each of the relevant
   procedures to be tagged with the compiler's _System keyword.  The
   relevant procedures are all of the SCHEME_UTILITY procedures,
   C_to_interface, interface_to_C, and interface_to_scheme.  */

#ifndef ASM_ENTRY_POINT
#  define ASM_ENTRY_POINT(name) name
#endif

#ifdef STDC_HEADERS
#define EXFNX(name, proto) ASM_ENTRY_POINT (name) proto
#define DEFNX(name, arglist, args) ASM_ENTRY_POINT (name) (args)
#define DEFNX_VOID(name) ASM_ENTRY_POINT (name) (void)
#else
#define EXFNX(name, proto) ASM_ENTRY_POINT (name) ()
#define DEFNX(name, arglist, args) ASM_ENTRY_POINT (name) arglist args;
#define DEFNX_VOID(name) ASM_ENTRY_POINT (name) ()
#endif

/* Make noise words invisible to the C compiler. */

#define C_UTILITY
#define C_TO_SCHEME
#define SCHEME_UTILITY

/* For clarity */

typedef char instruction;

#ifndef CMPINT_USE_STRUCS

typedef instruction * utility_result;

/* Imports from assembly language */

extern void EXFNX (C_to_interface, (void *));
extern utility_result interface_to_C_hook;

extern long C_return_value;
long C_return_value;

/* Convenience macros */

#define RETURN_TO_C(code) do						\
{									\
  C_return_value = (code);						\
  return (interface_to_C_hook);						\
} while (false)

#define RETURN_TO_SCHEME(ep)	return ((utility_result) (ep))

#define ENTER_SCHEME(ep) do						\
{									\
  C_to_interface ((void *) (ep));					\
  return (C_return_value);						\
} while (false)

#else /* CMPINT_USE_STRUCS */

#ifdef C_FUNC_PTR_IS_CLOSURE
#  define REFENTRY(name) (name)
#  define VARENTRY(name) instruction *name
#  define EXTENTRY(name) extern instruction *name
#else
#  define REFENTRY(name) ((void (*)()) name)
#  define VARENTRY(name) void (*name)()
#  define EXTENTRY(name) extern void EXFNX (name, (void))
#endif

/* Structure returned by SCHEME_UTILITYs */

struct utility_result_s
{
  VARENTRY (interface_dispatch);
  union additional_info
  {
    long                code_to_interpreter;
    instruction        *entry_point;
  } extra;
};

typedef struct utility_result_s utility_result;

/* Imports from assembly language */

extern long EXFNX (C_to_interface, (void *));

EXTENTRY (interface_to_C);
EXTENTRY (interface_to_scheme);

/* Convenience macros */

#define RETURN_TO_C(code) do						\
{									\
  struct utility_result_s temp;						\
									\
  temp.interface_dispatch = (REFENTRY (interface_to_C));		\
  temp.extra.code_to_interpreter = (code);				\
									\
  return (temp);							\
} while (false)

#define RETURN_TO_SCHEME(ep) do						\
{									\
  struct utility_result_s temp;						\
									\
  temp.interface_dispatch = (REFENTRY (interface_to_scheme));		\
  temp.extra.entry_point = ((instruction *) (ep));			\
									\
  return (temp);							\
} while (false)

#define ENTER_SCHEME(ep)	return (C_to_interface ((void *) (ep)))

#endif /* CMPINT_USE_STRUCS */

/* utility table entries. */

typedef utility_result EXFUN
  ((*ASM_ENTRY_POINT(utility_table_entry)), (long, long, long, long));

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

#define MAKE_CC_BLOCK(block_addr)					\
  (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block_addr))

/* Imports from the rest of the "microcode" */

/* Exports to the rest of the "microcode" */

extern long
  compiler_interface_version,
  compiler_processor_type;

extern SCHEME_OBJECT
  compiler_utilities,
  return_to_interpreter;

extern C_UTILITY long
  EXFUN (make_fake_uuo_link,
	 (SCHEME_OBJECT extension, SCHEME_OBJECT block, long offset)),
  EXFUN (make_uuo_link,
	 (SCHEME_OBJECT value, SCHEME_OBJECT extension,
	  SCHEME_OBJECT block, long offset)),
  EXFUN (compiled_block_closure_p, (SCHEME_OBJECT block)),
  EXFUN (compiled_entry_closure_p, (SCHEME_OBJECT entry)),
  EXFUN (compiled_entry_to_block_offset, (SCHEME_OBJECT entry)),
  EXFUN (coerce_to_compiled,
	 (SCHEME_OBJECT object, long arity, SCHEME_OBJECT *location));

extern C_UTILITY SCHEME_OBJECT
  EXFUN (extract_uuo_link, (SCHEME_OBJECT block, long offset)),
  EXFUN (extract_variable_cache,
	 (SCHEME_OBJECT extension, long offset)),
  EXFUN (compiled_block_debugging_info, (SCHEME_OBJECT block)),
  EXFUN (compiled_block_environment, (SCHEME_OBJECT block)),
  EXFUN (compiled_closure_to_entry, (SCHEME_OBJECT entry)),
  EXFUN (compiled_entry_to_block, (SCHEME_OBJECT entry)),
  EXFUN (apply_compiled_from_primitive, (int)),
  EXFUN (compiled_with_interrupt_mask, (unsigned long,
					SCHEME_OBJECT,
					unsigned long)),
  EXFUN (compiled_with_stack_marker, (SCHEME_OBJECT)),
  * EXFUN (cons_c_code_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *));

extern C_UTILITY Boolean
  EXFUN (install_c_code_table, (SCHEME_OBJECT *, long));

extern C_UTILITY void
  EXFUN (compiler_initialize, (long fasl_p)),
  EXFUN (compiler_reset, (SCHEME_OBJECT new_block)),
  EXFUN (store_variable_cache,
	 (SCHEME_OBJECT extension, SCHEME_OBJECT block, long offset)),
  EXFUN (declare_compiled_code_block, (SCHEME_OBJECT block));  

extern utility_table_entry utility_table[];

static SCHEME_OBJECT reflect_to_interface;

/* Breakpoint stuff. */

extern C_UTILITY SCHEME_OBJECT EXFUN (bkpt_install, (PTR));
extern C_UTILITY SCHEME_OBJECT EXFUN (bkpt_closure_install, (PTR));
extern C_UTILITY Boolean EXFUN (bkpt_p, (PTR));
extern C_UTILITY SCHEME_OBJECT EXFUN
  (bkpt_proceed, (PTR, SCHEME_OBJECT, SCHEME_OBJECT));
extern C_UTILITY void EXFUN (bkpt_remove, (PTR, SCHEME_OBJECT));

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
#define TRAMPOLINE_K_REFLECT_TO_INTERFACE	0x3a

#define TRAMPOLINE_K_OTHER			TRAMPOLINE_K_INTERPRETED

/* Ways to bypass the interpreter */

#define REFLECT_CODE_INTERNAL_APPLY		0
#define REFLECT_CODE_RESTORE_INTERRUPT_MASK	1
#define REFLECT_CODE_STACK_MARKER		2
#define REFLECT_CODE_CC_BKPT			3

/* Markers for special entry points */

#ifndef FORMAT_BYTE_EXPR
#define FORMAT_BYTE_EXPR                	0xFF
#endif
#ifndef FORMAT_BYTE_COMPLR
#define FORMAT_BYTE_COMPLR              	0xFE
#endif
#ifndef FORMAT_BYTE_CMPINT
#define FORMAT_BYTE_CMPINT              	0xFD
#endif
#ifndef FORMAT_BYTE_DLINK
#define FORMAT_BYTE_DLINK               	0xFC
#endif
#ifndef FORMAT_BYTE_RETURN
#define FORMAT_BYTE_RETURN              	0xFB
#endif
#ifndef FORMAT_BYTE_CLOSURE
#define FORMAT_BYTE_CLOSURE			0xFA
#endif
#ifndef FORMAT_BYTE_FRAMEMAX
#define FORMAT_BYTE_FRAMEMAX            	0x7F
#endif

#ifndef FORMAT_WORD_EXPR
#define FORMAT_WORD_EXPR        (MAKE_FORMAT_WORD (0xFF, FORMAT_BYTE_EXPR))
#endif
#ifndef FORMAT_WORD_CMPINT
#define FORMAT_WORD_CMPINT      (MAKE_FORMAT_WORD (0xFF, FORMAT_BYTE_CMPINT))
#endif
#ifndef FORMAT_WORD_RETURN
#define FORMAT_WORD_RETURN      (MAKE_FORMAT_WORD (0xFF, FORMAT_BYTE_RETURN))
#endif

/* Utilities for application of compiled procedures. */

/* NOTE: In this file, the number of arguments (or minimum
   number of arguments, etc.) is always 1 greater than the number of
   arguments (it includes the procedure object).
 */

/* open_gap: Default some optional parameters, and return the location
   of the return address (one past the last actual argument location).
 */

static SCHEME_OBJECT *
DEFUN (open_gap,
       (nactuals, delta),
       register long nactuals AND register long delta)
{
  register SCHEME_OBJECT *gap_location, *source_location;

  /* Need to fill in optionals */

  gap_location = STACK_LOC (delta);
  source_location = STACK_LOC (0);
  sp_register = gap_location;
  while ((--nactuals) > 0)
  {
    STACK_LOCATIVE_POP (gap_location) = STACK_LOCATIVE_POP (source_location);
  }
  delta = (- delta);
  while ((--delta) >= 0)
  {
    STACK_LOCATIVE_POP (gap_location) = UNASSIGNED_OBJECT;
  }
  return (source_location);
}

/* setup_lexpr_invocation: Setup a rest argument as appropriate. */

static long
DEFUN (setup_lexpr_invocation,
       (nactuals, nmax, entry_address),
       register long nactuals AND register long nmax
       AND instruction * entry_address)
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
      STACK_PUSH (ENTRY_TO_OBJECT (entry_address));
      STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
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
    sp_register = gap_location;
    return (PRIM_DONE);
  }
}

/* setup_compiled_invocation: Prepare the application frame the way that
   the called procedure expects it (optional arguments and rest argument
   initialized.
 */

static long
DEFUN (setup_compiled_invocation,
       (nactuals, compiled_entry_address),
       long nactuals AND instruction * compiled_entry_address)
{
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
    STACK_PUSH (ENTRY_TO_OBJECT (compiled_entry_address));
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
    return (ERR_INAPPLICABLE_OBJECT);
  }
  if (nactuals < nmin)
  {
    /* Too few arguments. */
    STACK_PUSH (ENTRY_TO_OBJECT (compiled_entry_address));
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
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
    STACK_PUSH (ENTRY_TO_OBJECT (compiled_entry_address));
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
    return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
  }
  /* The procedure can take arbitrarily many arguments, ie.
     it is a lexpr.
   */
  return (setup_lexpr_invocation (nactuals, nmax, compiled_entry_address));
}

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
DEFUN_VOID (enter_compiled_expression)
{
  instruction * compiled_entry_address;

  compiled_entry_address =
    ((instruction *) (OBJECT_ADDRESS (exp_register)));
  if ((COMPILED_ENTRY_FORMAT_WORD (compiled_entry_address)) !=
      FORMAT_WORD_EXPR)
  {
    /* It self evaluates. */
    val_register = exp_register;
    ENTER_SCHEME (OBJECT_ADDRESS (STACK_POP ()));
  }
  ENTER_SCHEME (compiled_entry_address);
}

C_TO_SCHEME long
DEFUN_VOID (apply_compiled_procedure)
{
  SCHEME_OBJECT nactuals, procedure;
  instruction * procedure_entry;
  long result;

  nactuals = (STACK_POP ());
  procedure = (STACK_POP ());
  procedure_entry = ((instruction *) (OBJECT_ADDRESS (procedure)));
  result = setup_compiled_invocation ((OBJECT_DATUM (nactuals)),
                                      procedure_entry);
  if (result == PRIM_DONE)
    /* Go into compiled code. */
    ENTER_SCHEME (procedure_entry);
  else
    return (result);
}

/* Note that this does not check that compiled_entry_address
   is a valid return address. -- Should it?
 */

C_TO_SCHEME long
DEFUN_VOID (return_to_compiled_code)
{
  instruction *compiled_entry_address;

  compiled_entry_address =
    ((instruction *) (OBJECT_ADDRESS (STACK_POP ())));
  ENTER_SCHEME (compiled_entry_address);
}

C_UTILITY SCHEME_OBJECT
DEFUN (apply_compiled_from_primitive, (arity), int arity)
{
  SCHEME_OBJECT frame_size, procedure;
  long result;
  
  frame_size = (STACK_POP ());
  procedure = (STACK_POP ());

  switch (OBJECT_TYPE (procedure))
  {
    case TC_ENTITY:
    {
      SCHEME_OBJECT data, operator;
      unsigned long nactuals = (OBJECT_DATUM (frame_size));

      data = (MEMORY_REF (procedure, ENTITY_DATA));
      if ((VECTOR_P (data))
	  && (nactuals < (VECTOR_LENGTH (data)))
	  && (COMPILED_CODE_ADDRESS_P (VECTOR_REF (data, nactuals)))
	  && ((VECTOR_REF (data, 0))
	      == (Get_Fixed_Obj_Slot (ARITY_DISPATCHER_TAG))))
	procedure = (VECTOR_REF (data, nactuals));
      else
      {
	operator = (MEMORY_REF (procedure, ENTITY_OPERATOR));
	if (!COMPILED_CODE_ADDRESS_P (operator))
	  goto defer_application;
	STACK_PUSH (procedure);
	frame_size += 1;
	procedure = operator;
      }
      /* fall through */
    }

    case TC_COMPILED_ENTRY:
    {
      result = setup_compiled_invocation ((OBJECT_DATUM (frame_size)),
					  ((instruction *)
					   (OBJECT_ADDRESS (procedure))));
      if (result == PRIM_DONE)
      {
	STACK_PUSH (procedure);
	sp_register = (STACK_LOC (- arity));
	return (SHARP_F);
      }
      else
	break;
    }

    case TC_PRIMITIVE:
    /* For now, fall through */

    default:
defer_application:
      STACK_PUSH (procedure);
      STACK_PUSH (frame_size);
      break;
  }

  STACK_PUSH (FIXNUM_ZERO + REFLECT_CODE_INTERNAL_APPLY);
  STACK_PUSH (reflect_to_interface);
  sp_register = (STACK_LOC (- arity));
  return (SHARP_F);
}

C_UTILITY SCHEME_OBJECT
DEFUN (compiled_with_interrupt_mask, (old_mask, receiver, new_mask),
       unsigned long old_mask
       AND SCHEME_OBJECT receiver
       AND unsigned long new_mask)
{
  long result;

  STACK_PUSH (LONG_TO_FIXNUM (old_mask));
  STACK_PUSH (FIXNUM_ZERO + REFLECT_CODE_RESTORE_INTERRUPT_MASK);
  STACK_PUSH (reflect_to_interface);

  STACK_PUSH (LONG_TO_FIXNUM (new_mask));
  result = (setup_compiled_invocation (2,
				       ((instruction *)
					(OBJECT_ADDRESS (receiver)))));
  STACK_PUSH (receiver);

  if (result != PRIM_DONE)
  {
    STACK_PUSH (STACK_FRAME_HEADER + 1);
    STACK_PUSH (FIXNUM_ZERO + REFLECT_CODE_INTERNAL_APPLY);
    STACK_PUSH (reflect_to_interface);
  }

  sp_register = (STACK_LOC (- 2));
  return (SHARP_F);
}

C_UTILITY SCHEME_OBJECT
DEFUN (compiled_with_stack_marker, (thunk), SCHEME_OBJECT thunk)
{
  long result;

  STACK_PUSH (FIXNUM_ZERO + REFLECT_CODE_STACK_MARKER);
  STACK_PUSH (reflect_to_interface);

  result = (setup_compiled_invocation (1,
				       ((instruction *)
					(OBJECT_ADDRESS (thunk)))));
  STACK_PUSH (thunk);

  if (result != PRIM_DONE)
  {
    STACK_PUSH (STACK_FRAME_HEADER);
    STACK_PUSH (FIXNUM_ZERO + REFLECT_CODE_INTERNAL_APPLY);
    STACK_PUSH (reflect_to_interface);
  }

  sp_register = (STACK_LOC (- 3));
  return (SHARP_F);
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

SCHEME_UTILITY utility_result
DEFNX (comutil_return_to_interpreter,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  RETURN_TO_C (PRIM_DONE);
}

#if (COMPILER_PROCESSOR_TYPE != COMPILER_IA32_TYPE)

#define INVOKE_RETURN_ADDRESS()					\
  RETURN_TO_SCHEME (OBJECT_ADDRESS (STACK_POP ()))

#else /* COMPILER_IA32_TYPE */

static utility_result
  EXFUN (compiler_interrupt_common, (SCHEME_ADDR, SCHEME_OBJECT));

#define INVOKE_RETURN_ADDRESS() do					\
{									\
  if (((long) (ADDR_TO_SCHEME_ADDR (Free)))				\
      >= ((long) (Registers[REGBLOCK_MEMTOP])))				\
    return (compiler_interrupt_common (0, val_register));		\
  else									\
    RETURN_TO_SCHEME (OBJECT_ADDRESS (STACK_POP ()));			\
} while (0)

#endif /* COMPILER_IA32_TYPE */

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

SCHEME_UTILITY utility_result
DEFNX (comutil_primitive_apply,
       (primitive, ignore_2, ignore_3, ignore_4),
       SCHEME_OBJECT primitive
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{ 
  PRIMITIVE_APPLY (val_register, primitive);
  POP_PRIMITIVE_FRAME (PRIMITIVE_ARITY (primitive));
  INVOKE_RETURN_ADDRESS ();
}

/*
  comutil_primitive_lexpr_apply is like comutil_primitive_apply
  except that it is used to invoke primitives that take
  an arbitrary number of arguments.
  The number of arguments is in the REGBLOCK_LEXPR_ACTUALS slot
  of the register block.
 */

SCHEME_UTILITY utility_result
DEFNX (comutil_primitive_lexpr_apply,
       (primitive, ignore_2, ignore_3, ignore_4),
       SCHEME_OBJECT primitive
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  PRIMITIVE_APPLY (val_register, primitive);
  POP_PRIMITIVE_FRAME (((long) (Registers[REGBLOCK_LEXPR_ACTUALS])));
  INVOKE_RETURN_ADDRESS ();
}

/*
  comutil_apply is used by compiled code to invoke an unknown
  procedure.  It dispatches on its type to the correct place.  It
  expects the procedure to invoke, and the number of arguments (+ 1).
 */

SCHEME_UTILITY utility_result
DEFNX (comutil_apply,
       (procedure, nactuals, ignore_3, ignore_4),
       SCHEME_OBJECT procedure
       AND unsigned long nactuals
       AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT orig_proc = procedure;

loop:
  switch (OBJECT_TYPE (procedure))
  {
    case TC_COMPILED_ENTRY:
    callee_is_compiled:
    {
      instruction * entry_point;

      entry_point = ((instruction *) (OBJECT_ADDRESS (procedure)));
      RETURN_UNLESS_EXCEPTION
        ((setup_compiled_invocation (nactuals, entry_point)),
         entry_point);
    }

    case TC_ENTITY:
    {
      SCHEME_OBJECT data, operator;

      data = (MEMORY_REF (procedure, ENTITY_DATA));
      if ((VECTOR_P (data))
	  && (nactuals < (VECTOR_LENGTH (data)))
	  && ((VECTOR_REF (data, nactuals)) != SHARP_F)
	  && ((VECTOR_REF (data, 0))
	      == (Get_Fixed_Obj_Slot (ARITY_DISPATCHER_TAG))))
      {
	/* No loops allowed! */
	SCHEME_OBJECT nproc = (VECTOR_REF (data, nactuals));

	if ((procedure == orig_proc) && (nproc != procedure))
	{
	  procedure = nproc;
	  goto loop;
	}
	else
	  procedure = orig_proc;
      }

      operator = (MEMORY_REF (procedure, ENTITY_OPERATOR));
      if (!(COMPILED_CODE_ADDRESS_P (operator)))
        goto callee_is_interpreted;

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

      arity = (PRIMITIVE_ARITY (procedure));
      if (arity == ((long) (nactuals - 1)))
        return (comutil_primitive_apply (procedure, 0, 0, 0));

      if (arity != LEXPR)
      {
        /* Wrong number of arguments. */
        STACK_PUSH (procedure);
        STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
        RETURN_TO_C (ERR_WRONG_NUMBER_OF_ARGUMENTS);
      }
      if (!(IMPLEMENTED_PRIMITIVE_P (procedure)))
        /* Let the interpreter handle it. */
        goto callee_is_interpreted;

      /* "Lexpr" primitive. */
      (Registers[REGBLOCK_LEXPR_ACTUALS]) = ((SCHEME_OBJECT) (nactuals - 1));
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

SCHEME_UTILITY utility_result
DEFNX (comutil_error,
       (nactuals, ignore_2, ignore_3, ignore_4),
       long nactuals AND
       long ignore_2 AND long ignore_3 AND long ignore_4)
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

SCHEME_UTILITY utility_result
DEFNX (comutil_lexpr_apply,
       (entry_address_raw, nactuals, ignore_3, ignore_4),
       SCHEME_ADDR entry_address_raw AND long nactuals
       AND long ignore_3 AND long ignore_4)
{
  instruction * entry_address
    = ((instruction *) (SCHEME_ADDR_TO_ADDR (entry_address_raw)));
				 
  RETURN_UNLESS_EXCEPTION
    ((setup_lexpr_invocation
      ((nactuals + 1),
       (COMPILED_ENTRY_MAXIMUM_ARITY (entry_address)),
       entry_address)),
     entry_address);
}

static long
DEFUN (compiler_link_closure_pattern, (distance, block, offset),
       SCHEME_OBJECT distance AND
       SCHEME_OBJECT block AND
       unsigned long offset)
{
  long objdist = (FIXNUM_TO_LONG (distance));
  long nmv_length = (OBJECT_DATUM (MEMORY_REF (block, 1)));
  SCHEME_OBJECT * location = (MEMORY_LOC (block, offset));
  SCHEME_OBJECT * closptr = (location - objdist);
  SCHEME_OBJECT * end_closptr = (MEMORY_LOC (block, (2 + nmv_length)));
  SCHEME_OBJECT entry_offset, * area_end;
  char * word_ptr;
  long count;

  nmv_length -= (end_closptr - closptr);
  while (closptr < end_closptr)
  {
    while ((* closptr) == ((SCHEME_OBJECT) 0))
      closptr ++;
    closptr ++;
    count = (MANIFEST_CLOSURE_COUNT (closptr));
    word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (closptr));
    area_end = ((MANIFEST_CLOSURE_END (closptr, count)) - 1);
    while ((--count) >= 0)
    {
      closptr = ((SCHEME_OBJECT *) word_ptr);
      word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
      EXTRACT_CLOSURE_ENTRY_ADDRESS (entry_offset, closptr);
      entry_offset = ((SCHEME_OBJECT)
		      (((long) closptr) - ((long) entry_offset)));
      STORE_CLOSURE_ENTRY_ADDRESS (entry_offset, closptr);
    }
    closptr = &area_end[1];
  }

  MEMORY_SET (block, 1, (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, nmv_length)));
  return (PRIM_DONE);
}

static Boolean linking_cc_block_p = false;

static void
DEFUN (abort_link_cc_block, (ap), PTR ap)
{
  linking_cc_block_p = (* ((Boolean *) (ap)));
  return;
}

/* Core of comutil_link and comp_link_caches_restart. */

static long
DEFUN (link_cc_block,
       (block_address, offset, last_header_offset,
	sections, original_count, ret_add),
       SCHEME_OBJECT * block_address AND
       unsigned long offset AND
       unsigned long last_header_offset AND
       long sections AND
       long original_count AND
       instruction * ret_add)
{
  Boolean execute_p = false;
  register long entry_size, count;
  SCHEME_OBJECT block;
  SCHEME_OBJECT header;
  long result, kind, total_count;
  long EXFUN ((*cache_handler), (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long));

  transaction_begin ();
  {
    Boolean * ap = (dstack_alloc (sizeof (Boolean)));
    *ap = linking_cc_block_p;
    transaction_record_action (tat_abort, abort_link_cc_block, ap);
  }
  linking_cc_block_p = true;

  result = PRIM_DONE;
  block = (MAKE_CC_BLOCK (block_address));

  while ((--sections) >= 0)
  {
    SCHEME_OBJECT * scan = &(block_address[last_header_offset]);
    header = (*scan);

    kind = (READ_LINKAGE_KIND (header));
    switch (kind)
    {
      case OPERATOR_LINKAGE_KIND:
	cache_handler = compiler_cache_operator;

      handle_operator:
        execute_p = true;
	entry_size = EXECUTE_CACHE_ENTRY_SIZE;
	START_OPERATOR_RELOCATION (scan);
	count = (READ_OPERATOR_LINKAGE_COUNT (header));
	break;

      case GLOBAL_OPERATOR_LINKAGE_KIND:
	cache_handler = compiler_cache_global_operator;
	goto handle_operator;

      case ASSIGNMENT_LINKAGE_KIND:
	cache_handler = compiler_cache_assignment;
	goto handle_reference;

      case REFERENCE_LINKAGE_KIND:
	cache_handler = compiler_cache_lookup;
      handle_reference:
	execute_p = false;
	entry_size = 1;
	count = (READ_CACHE_LINKAGE_COUNT (header));
	break;

      case CLOSURE_PATTERN_LINKAGE_KIND:
	cache_handler = compiler_link_closure_pattern;
	/* Not really a reference, but the same format. */
	goto handle_reference;

      default:
	offset += 1;
	total_count = (READ_CACHE_LINKAGE_COUNT (header));
	count = (total_count - 1);
	result = ERR_COMPILED_CODE_ERROR;
	goto back_out;
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
      if (execute_p)
	offset += (FIRST_OPERATOR_LINKAGE_OFFSET - 1);
    }

    block_address[last_header_offset] =
      (MAKE_LINKAGE_SECTION_HEADER (kind, total_count));
    for (offset += 1; ((--count) >= 0); offset += entry_size)
    {
      SCHEME_OBJECT info;	/* A symbol or a fixnum */

      if (! execute_p)
	info = (block_address[offset]);
      else
	EXTRACT_EXECUTE_CACHE_SYMBOL (info, &(block_address[offset]));

      result = ((*cache_handler) (info, block, offset));
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

  back_out:
	if (execute_p)
	  END_OPERATOR_RELOCATION (&(block_address[offset]));
        STACK_PUSH (ENTRY_TO_OBJECT (ret_add));
        STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (sections + 1));
        STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (last_header_offset));
        STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (offset - 1));
        STACK_PUSH (block);
	STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (count + 1));
	STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (total_count));

        exp_register = SHARP_F;
        Store_Return (RC_COMP_LINK_CACHES_RESTART);
        Save_Cont ();

        /* Smash header for the garbage collector.
           It is smashed back on return.  See the comment above.
         */

        block_address[last_header_offset] =
          (MAKE_LINKAGE_SECTION_HEADER (kind, (total_count - (count + 1))));
	goto exit_proc;
      }
    }
    if (execute_p)
      END_OPERATOR_RELOCATION (&(block_address[offset - 1]));
    last_header_offset = offset;
  }

exit_proc:
  /* Rather than commit, since we want to undo */
  transaction_abort ();
#if defined(FLUSH_I_CACHE_REGION) || defined(PUSH_D_CACHE_REGION)
  {
    SCHEME_OBJECT * ret_add_block;
    unsigned long block_len = (((unsigned long) (* block_address)) + 1);
    
    Get_Compiled_Block (ret_add_block, ((SCHEME_OBJECT *) ret_add));
    if (ret_add_block == block_address)
      {
#ifdef FLUSH_I_CACHE_REGION
	FLUSH_I_CACHE_REGION (block_address, block_len);
#endif
      }
    else
      {
#ifdef PUSH_D_CACHE_REGION
	PUSH_D_CACHE_REGION (block_address, block_len);
#endif
      }
  }
#endif
  return (result);
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

SCHEME_UTILITY utility_result
DEFNX (comutil_link,
       (ret_add_raw, block_address_raw, constant_address_raw, sections),
       SCHEME_ADDR ret_add_raw
       AND SCHEME_ADDR block_address_raw
       AND SCHEME_ADDR constant_address_raw
       AND long sections)
{
  instruction * ret_add
    = ((instruction *) (SCHEME_ADDR_TO_ADDR (ret_add_raw)));
  SCHEME_OBJECT * block_address
    = (SCHEME_ADDR_TO_ADDR (block_address_raw));
  SCHEME_OBJECT * constant_address
    = (SCHEME_ADDR_TO_ADDR (constant_address_raw));
  unsigned long offset;

#ifdef AUTOCLOBBER_BUG
  block_address[OBJECT_DATUM (* block_address)] = env_register;
#endif

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
DEFUN_VOID (comp_link_caches_restart)
{
  SCHEME_OBJECT block, environment;
  long original_count, sections, code;
  unsigned long offset;
  unsigned long last_header_offset;
  instruction * ret_add;

  original_count = (OBJECT_DATUM (STACK_POP()));
  (void) STACK_POP ();		/* Loop count, for debugger */
  block = (STACK_POP ());
  environment = (compiled_block_environment (block));
  env_register = environment;
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
    /* Return to the block being linked. */
    ENTER_SCHEME (ret_add);
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

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_apply_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  /* Used by coerce_to_compiled.  TRAMPOLINE_K_APPLY */

  return (comutil_apply ((tramp_data[0]),
			 (OBJECT_DATUM (tramp_data[1])),
			 0, 0));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_arity_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  /* Linker saw an argument count mismatch. TRAMPOLINE_K_ARITY */

  return (comutil_apply ((tramp_data[0]),
			 (OBJECT_DATUM (tramp_data[1])),
			 0, 0));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_entity_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  /* Linker saw an entity to be applied. TRAMPOLINE_K_ENTITY */

  return (comutil_apply ((tramp_data[0]),
			 (OBJECT_DATUM (tramp_data[1])),
			 0, 0));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_interpreted_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  /* Linker saw an interpreted procedure or a procedure that it cannot
     link directly.  TRAMPOLINE_K_INTERPRETED
   */

  return (comutil_apply ((tramp_data[0]),
			 (OBJECT_DATUM (tramp_data[1])),
			 0, 0));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_lexpr_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  /* Linker saw a primitive of arbitrary number of arguments.
     TRAMPOLINE_K_LEXPR_PRIMITIVE
   */

  (Registers[REGBLOCK_LEXPR_ACTUALS]) =
    ((SCHEME_OBJECT) ((OBJECT_DATUM (tramp_data[1])) - 1));
  return (comutil_primitive_lexpr_apply ((tramp_data[0]), 0, 0, 0));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_primitive_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

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

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_lookup_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw AND
       long ignore_2 AND
       long ignore_3 AND
       long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));
  SCHEME_OBJECT cache = (tramp_data[0]);
  SCHEME_OBJECT block = (tramp_data[1]);
  unsigned long offset = (OBJECT_DATUM (tramp_data[2]));
  SCHEME_OBJECT true_operator;
  long code
    = (compiler_operator_reference_trap (cache, (&true_operator)));
  SCHEME_OBJECT * cache_cell = (MEMORY_LOC (block, offset));
  long nargs;

  EXTRACT_EXECUTE_CACHE_ARITY (nargs, cache_cell);
  if (code == PRIM_DONE)
    return (comutil_apply (true_operator, nargs, 0, 0));
  /* Error or interrupt */
  {
    SCHEME_OBJECT trampoline;

    /* This could be done by bumping tramp_data to the entry point.
       It would probably be better.  */
    EXTRACT_EXECUTE_CACHE_ADDRESS (trampoline, cache_cell);
    STACK_PUSH (ENTRY_TO_OBJECT (SCHEME_ADDR_TO_ADDR (trampoline)));
    /* Next three for debugger.  */
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nargs));
    STACK_PUSH (compiled_block_environment (block));
    STACK_PUSH (compiler_var_error (cache, block, CACHE_REFERENCES_OPERATOR));
    exp_register = SHARP_F;
    Store_Return (RC_COMP_OP_REF_TRAP_RESTART);
    Save_Cont ();
    RETURN_TO_C (code);
  }
}

/*
  Re-start after processing an error/interrupt encountered in the previous
  utility.
  Extract the new trampoline or procedure (the user may have defined the
  missing variable) and invoke it.
 */

C_TO_SCHEME long
DEFUN_VOID (comp_op_lookup_trap_restart)
{
  SCHEME_OBJECT * old_trampoline, code_block, new_procedure;
  long offset;

  /* Discard name, env. and nargs */

  sp_register = (STACK_LOC (3));
  old_trampoline = (OBJECT_ADDRESS (STACK_POP ()));
  code_block = ((TRAMPOLINE_STORAGE (old_trampoline))[1]);
  offset = (OBJECT_DATUM ((TRAMPOLINE_STORAGE (old_trampoline))[2]));
  EXTRACT_EXECUTE_CACHE_ADDRESS (new_procedure,
				 (MEMORY_LOC (code_block, offset)));
  ENTER_SCHEME (SCHEME_ADDR_TO_ADDR (new_procedure));
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

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_1_0_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  STACK_PUSH (UNASSIGNED_OBJECT);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_2_1_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT Top;
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  Top = (STACK_POP ());
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_2_0_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_3_2_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT Top, Next;
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  Top = (STACK_POP ());
  Next = (STACK_POP ());
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Next);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_3_1_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT Top;
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  Top = (STACK_POP ());
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_3_0_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_4_3_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT Top, Middle, Bottom;
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  Top = (STACK_POP ());
  Middle = (STACK_POP ());
  Bottom = (STACK_POP ());

  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Bottom);
  STACK_PUSH (Middle);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_4_2_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT Top, Next;
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  Top = (STACK_POP ());
  Next = (STACK_POP ());
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Next);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_4_1_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT Top;
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  Top = (STACK_POP ());
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (Top);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_operator_4_0_trap,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT * tramp_data = (SCHEME_ADDR_TO_ADDR (tramp_data_raw));

  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  STACK_PUSH (UNASSIGNED_OBJECT);
  RETURN_TO_SCHEME (OBJECT_ADDRESS (tramp_data[0]));
}

/* INTERRUPT/GC from Scheme

   These procedures are called from compiled code at the start
   (respectively) of a procedure or continuation if an interrupt has
   been detected.  They must not be called unless there is an
   interrupt to be serviced.

   The code that handles RC_COMP_INTERRUPT_RESTART in "interp.c" will
   return control to comp_interrupt_restart (below).  This assumes
   that the Scheme stack contains a compiled code entry address
   (start of continuation, procedure, etc.).  The exp_register
   saved with the continuation is a piece of state that will be
   returned to val_register and env_register (both) upon return.
 */

#define MAYBE_REQUEST_INTERRUPTS()					\
{									\
  if (Free >= MemTop)							\
    Request_GC (Free - MemTop);						\
  if (sp_register <= Stack_Guard)					\
    REQUEST_INTERRUPT (INT_Stack_Overflow);				\
}

static utility_result
DEFUN (compiler_interrupt_common, (entry_point_raw, state),
       SCHEME_ADDR entry_point_raw AND
       SCHEME_OBJECT state)
{
  MAYBE_REQUEST_INTERRUPTS ();
  if (entry_point_raw != ((SCHEME_ADDR) 0))
  {
    instruction * entry_point
      = ((instruction *) (SCHEME_ADDR_TO_ADDR (entry_point_raw)));
    STACK_PUSH (ENTRY_TO_OBJECT (entry_point));
  }
  STACK_PUSH (state);
  exp_register = SHARP_F;
  Store_Return (RC_COMP_INTERRUPT_RESTART);
  Save_Cont ();
  RETURN_TO_C (PRIM_INTERRUPT);
}

SCHEME_UTILITY utility_result
DEFNX (comutil_interrupt_closure, (ignore_1, ignore_2, ignore_3, ignore_4),
       long ignore_1 AND
       long ignore_2 AND
       long ignore_3 AND
       long ignore_4)
{
  return (compiler_interrupt_common (0, SHARP_F));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_interrupt_dlink,
       (entry_point_raw, dlink_raw, ignore_3, ignore_4),
       SCHEME_ADDR entry_point_raw AND
       SCHEME_ADDR dlink_raw AND
       long ignore_3 AND
       long ignore_4)
{
  SCHEME_OBJECT * dlink = (SCHEME_ADDR_TO_ADDR (dlink_raw));
  return
    (compiler_interrupt_common
     (entry_point_raw, (MAKE_POINTER_OBJECT (TC_STACK_ENVIRONMENT, dlink))));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_interrupt_procedure,
       (entry_point_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR entry_point_raw AND
       long ignore_2 AND
       long ignore_3 AND
       long ignore_4)
{
  return (compiler_interrupt_common (entry_point_raw, SHARP_F));
}

/* val_register has live data, and there is no entry address on the stack */

SCHEME_UTILITY utility_result
DEFNX (comutil_interrupt_continuation,
       (return_address_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR return_address_raw AND
       long ignore_2 AND
       long ignore_3 AND
       long ignore_4)
{
  return (compiler_interrupt_common (return_address_raw, val_register));
}

/* env_register has live data; no entry point on the stack */

SCHEME_UTILITY utility_result
DEFNX (comutil_interrupt_ic_procedure,
       (entry_point_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR entry_point_raw AND
       long ignore_2 AND
       long ignore_3 AND
       long ignore_4)
{
  return (compiler_interrupt_common (entry_point_raw, env_register));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_interrupt_continuation_2,
       (ignore_1, ignore_2, ignore_3, ignore_4),
       long ignore_1 AND
       long ignore_2 AND
       long ignore_3 AND
       long ignore_4)
{
  return (compiler_interrupt_common (0, val_register));
}

C_TO_SCHEME long
DEFUN_VOID (comp_interrupt_restart)
{
  SCHEME_OBJECT state = (STACK_POP ());
  env_register = state;
  val_register = state;
  ENTER_SCHEME (OBJECT_ADDRESS (STACK_POP ()));
}

/* Other TRAPS */

/* Assigning a variable that has a trap in it (except unassigned) */

SCHEME_UTILITY utility_result
DEFNX (comutil_assignment_trap,
       (return_address_raw, cache_addr_raw, value, ignore_4),
       SCHEME_ADDR return_address_raw AND
       SCHEME_ADDR cache_addr_raw AND
       SCHEME_OBJECT value AND
       long ignore_4)
{
  instruction * return_address
    = ((instruction *) (SCHEME_ADDR_TO_ADDR (return_address_raw)));
  SCHEME_OBJECT cache
    = (MAKE_POINTER_OBJECT
       (CACHE_TYPE, (SCHEME_ADDR_TO_ADDR (cache_addr_raw))));
  long code = (compiler_assignment_trap (cache, value, (&val_register)));
  if (code == PRIM_DONE)
    RETURN_TO_SCHEME (return_address);
  else
    {
      SCHEME_OBJECT sra = (ENTRY_TO_OBJECT (return_address));
      SCHEME_OBJECT block = (compiled_entry_to_block (sra));
      STACK_PUSH (sra);
      STACK_PUSH (value);
      STACK_PUSH (compiled_block_environment (block));
      STACK_PUSH
	(compiler_var_error (cache, block, CACHE_REFERENCES_ASSIGNMENT));
      exp_register = SHARP_F;
      Store_Return (RC_COMP_ASSIGNMENT_TRAP_RESTART);
      Save_Cont ();
      RETURN_TO_C (code);
    }
}

C_TO_SCHEME long
DEFUN_VOID (comp_assignment_trap_restart)
{
  SCHEME_OBJECT name = (STACK_POP ());
  SCHEME_OBJECT environment = (STACK_POP ());
  SCHEME_OBJECT value = (STACK_POP ());
  long code = (assign_variable (environment, name, value, (&val_register)));
  if (code == PRIM_DONE)
    ENTER_SCHEME (OBJECT_ADDRESS (STACK_POP ()));
  else
    {
      STACK_PUSH (value);
      STACK_PUSH (environment);
      STACK_PUSH (name);
      exp_register = SHARP_F;
      Store_Return (RC_COMP_ASSIGNMENT_TRAP_RESTART);
      Save_Cont ();
      return (code);
    }
}

SCHEME_UTILITY utility_result
DEFNX (comutil_cache_lookup_apply,
       (cache_addr_raw, block_address_raw, nactuals, ignore_4),
       SCHEME_ADDR cache_addr_raw AND
       SCHEME_ADDR block_address_raw AND
       long nactuals AND
       long ignore_4)
{
  SCHEME_OBJECT cache
    = (MAKE_POINTER_OBJECT
       (CACHE_TYPE, (SCHEME_ADDR_TO_ADDR (cache_addr_raw))));
  SCHEME_OBJECT value;
  long code = (compiler_lookup_trap (cache, (&value)));
  if (code == PRIM_DONE)
    return (comutil_apply (value, nactuals, 0, 0));
  {
    SCHEME_OBJECT block
      = (MAKE_CC_BLOCK (SCHEME_ADDR_TO_ADDR (block_address_raw)));
    STACK_PUSH (block);
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
    STACK_PUSH (compiled_block_environment (block));
    STACK_PUSH
      (compiler_var_error (cache, block, CACHE_REFERENCES_OPERATOR));
    exp_register = SHARP_F;
    Store_Return (RC_COMP_CACHE_REF_APPLY_RESTART);
    Save_Cont ();
    RETURN_TO_C (code);
  }
}

C_TO_SCHEME long
DEFUN_VOID (comp_cache_lookup_apply_restart)
{
  SCHEME_OBJECT name = (STACK_POP ());
  SCHEME_OBJECT environment = (STACK_POP ());
  SCHEME_OBJECT value;
  long code = (lookup_variable (environment, name, (&value)));
  if (code == PRIM_DONE)
    {
      /* Replace block with actual operator */
      (* (STACK_LOC (1))) = value;
      if (COMPILED_CODE_ADDRESS_P (value))
	return (apply_compiled_procedure ());
      else
	return (PRIM_APPLY);
    }
  else
    {
      STACK_PUSH (environment);
      STACK_PUSH (name);
      exp_register = SHARP_F;
      Store_Return (RC_COMP_CACHE_REF_APPLY_RESTART);
      Save_Cont ();
      return (code);
    }
}

/* Variable reference traps:
   Reference to a free variable that has a reference trap -- either a
   fluid or an error (unassigned / unbound).  */

#define CMPLR_REF_TRAP(name, c_trap, ret_code, restart, c_lookup)	\
SCHEME_UTILITY utility_result						\
DEFNX (name,								\
       (return_address_raw, cache_addr_raw, ignore_3, ignore_4),	\
       SCHEME_ADDR return_address_raw AND				\
       SCHEME_ADDR cache_addr_raw AND					\
       long ignore_3 AND						\
       long ignore_4)							\
{									\
  instruction * return_address						\
    = ((instruction *) (SCHEME_ADDR_TO_ADDR (return_address_raw)));	\
  SCHEME_OBJECT cache							\
    = (MAKE_POINTER_OBJECT						\
       (CACHE_TYPE, (SCHEME_ADDR_TO_ADDR (cache_addr_raw))));		\
  long code = (c_trap (cache, (&val_register)));			\
  if (code == PRIM_DONE)						\
    RETURN_TO_SCHEME (return_address);					\
  else									\
    {									\
      SCHEME_OBJECT sra = (ENTRY_TO_OBJECT (return_address));		\
      SCHEME_OBJECT block = (compiled_entry_to_block (sra));		\
      STACK_PUSH (sra);							\
      STACK_PUSH (compiled_block_environment (block));			\
      STACK_PUSH							\
	(compiler_var_error						\
	 (cache, block, CACHE_REFERENCES_LOOKUP));			\
      exp_register = SHARP_F;						\
      Store_Return (ret_code);						\
      Save_Cont ();							\
      RETURN_TO_C (code);						\
    }									\
}									\
									\
C_TO_SCHEME long							\
DEFUN_VOID (restart)							\
{									\
  SCHEME_OBJECT name = exp_register;					\
  SCHEME_OBJECT environment = (STACK_POP ());				\
  long code = (c_lookup (environment, name, (&val_register)));		\
  if (code == PRIM_DONE)						\
    ENTER_SCHEME (OBJECT_ADDRESS (STACK_POP ()));			\
  else									\
    {									\
      STACK_PUSH (environment);						\
      STACK_PUSH (name);						\
      exp_register = SHARP_F;						\
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
               lookup_variable)

CMPLR_REF_TRAP(comutil_safe_lookup_trap,
               compiler_safe_lookup_trap,
               RC_COMP_SAFE_REF_TRAP_RESTART,
               comp_safe_lookup_trap_restart,
               safe_lookup_variable)

CMPLR_REF_TRAP(comutil_unassigned_p_trap,
               compiler_unassigned_p_trap,
               RC_COMP_UNASSIGNED_TRAP_RESTART,
               comp_unassigned_p_trap_restart,
               variable_unassigned_p)


/* NUMERIC ROUTINES
   Invoke the arithmetic primitive in the fixed objects vector.
   The Scheme arguments are expected on the Scheme stack.
 */

#define COMPILER_ARITH_PRIM(name, fobj_index, arity)			\
SCHEME_UTILITY utility_result						\
DEFNX (name,								\
       (ignore_1, ignore_2, ignore_3, ignore_4),			\
       long ignore_1 AND long ignore_2					\
       AND long ignore_3 AND long ignore_4)				\
{									\
  SCHEME_OBJECT handler;						\
									\
  handler = (Get_Fixed_Obj_Slot (fobj_index));				\
  return (comutil_apply (handler, (arity), 0, 0));			\
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

/*
  Obsolete SCHEME_UTILITYs used to handle first class environments.
  They have been superseded by the variable caching code.
  They are here for completeness, and because the code in the compiler
  that uses them has not yet been spliced out, although it is switched
  off.
*/

#define CMPLR_REFERENCE(util_name, c_proc, ret_code, restart_name)	\
SCHEME_UTILITY utility_result						\
DEFNX (util_name,							\
       (ret_add_raw, environment, variable, ignore_4),			\
       SCHEME_ADDR ret_add_raw						\
       AND SCHEME_OBJECT environment AND SCHEME_OBJECT variable		\
       AND long ignore_4)						\
{									\
  instruction * ret_add							\
    = ((instruction *) (SCHEME_ADDR_TO_ADDR (ret_add_raw)));		\
  long code;								\
									\
  code = (c_proc (environment, variable, (&val_register)));		\
  if (code == PRIM_DONE)						\
  {									\
    RETURN_TO_SCHEME (ret_add);						\
  }									\
  else									\
  {									\
    STACK_PUSH (ENTRY_TO_OBJECT (ret_add));				\
    STACK_PUSH (variable);						\
    STACK_PUSH (environment);						\
    exp_register = SHARP_F;						\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    RETURN_TO_C (code);							\
  }									\
}									\
									\
C_TO_SCHEME long							\
DEFUN_VOID (restart_name)						\
{									\
  SCHEME_OBJECT environment, variable;					\
  long code;								\
									\
  environment = (STACK_POP ());						\
  variable = (STACK_POP ());						\
  code = (c_proc (environment, variable, (&val_register)));		\
  if (code == PRIM_DONE)						\
  {									\
    env_register = environment;						\
    ENTER_SCHEME (OBJECT_ADDRESS (STACK_POP ()));			\
  }									\
  else									\
  {									\
    STACK_PUSH (variable);						\
    STACK_PUSH (environment);						\
    exp_register = SHARP_F;						\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    return (code);							\
  }									\
}

#define CMPLR_ASSIGNMENT(util_name, c_proc, ret_code, restart_name)	\
SCHEME_UTILITY utility_result						\
DEFNX (util_name,							\
       (ret_add_raw, environment, variable, value),			\
       SCHEME_ADDR ret_add_raw						\
       AND SCHEME_OBJECT environment					\
       AND SCHEME_OBJECT variable					\
       AND SCHEME_OBJECT value)						\
{									\
  instruction * ret_add							\
    = ((instruction *) (SCHEME_ADDR_TO_ADDR (ret_add_raw)));		\
  long code;								\
									\
  code = (c_proc (environment, variable, value));			\
  if (code == PRIM_DONE)						\
    RETURN_TO_SCHEME (ret_add);						\
  else									\
  {									\
    STACK_PUSH (ENTRY_TO_OBJECT (ret_add));				\
    STACK_PUSH (value);							\
    STACK_PUSH (variable);						\
    STACK_PUSH (environment);						\
    exp_register = SHARP_F;						\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    RETURN_TO_C (code);							\
  }									\
}									\
									\
C_TO_SCHEME long							\
DEFUN_VOID (restart_name)						\
{									\
  SCHEME_OBJECT environment, variable, value;				\
  long code;								\
									\
  environment = exp_register;						\
  variable = (STACK_POP ());						\
  value = (STACK_POP ());						\
  code = (c_proc (environment, variable, value));			\
  if (code == PRIM_DONE)						\
  {									\
    env_register = environment;						\
    ENTER_SCHEME (OBJECT_ADDRESS (STACK_POP ()));			\
  }									\
  else									\
  {									\
    STACK_PUSH (value);							\
    STACK_PUSH (variable);						\
    STACK_PUSH (environment);						\
    exp_register = SHARP_F;						\
    Store_Return (ret_code);						\
    Save_Cont ();							\
    return (code);							\
  }									\
}

CMPLR_REFERENCE(comutil_access,
		lookup_variable,
		RC_COMP_ACCESS_RESTART,
		comp_access_restart)

CMPLR_REFERENCE(comutil_reference,
		lookup_variable,
		RC_COMP_REFERENCE_RESTART,
		comp_reference_restart)

CMPLR_REFERENCE(comutil_safe_reference,
		safe_lookup_variable,
		RC_COMP_SAFE_REFERENCE_RESTART,
		comp_safe_reference_restart)

CMPLR_REFERENCE(comutil_unassigned_p,
		variable_unassigned_p,
		RC_COMP_UNASSIGNED_P_RESTART,
		comp_unassigned_p_restart)

CMPLR_REFERENCE(comutil_unbound_p,
		variable_unbound_p,
		RC_COMP_UNBOUND_P_RESTART,
		comp_unbound_p_restart)

static long
compiler_assign_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
			  SCHEME_OBJECT value)
{
  return (assign_variable (environment, symbol, value, (&val_register)));
}

CMPLR_ASSIGNMENT(comutil_assignment,
		 compiler_assign_variable,
		 RC_COMP_ASSIGNMENT_RESTART,
		 comp_assignment_restart)

static long
compiler_define_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
			  SCHEME_OBJECT value)
{
  long result = (define_variable (environment, symbol, value));
  if (result == PRIM_DONE)
    val_register = symbol;
  return (result);
}

CMPLR_ASSIGNMENT(comutil_definition,
		 compiler_define_variable,
		 RC_COMP_DEFINITION_RESTART,
		 comp_definition_restart)

SCHEME_UTILITY utility_result
DEFNX (comutil_lookup_apply,
       (environment, variable, nactuals, ignore_4),
       SCHEME_OBJECT environment AND SCHEME_OBJECT variable
       AND long nactuals AND long ignore_4)
{
  long code = (lookup_variable (environment, variable, (&val_register)));
  if (code == PRIM_DONE)
    return (comutil_apply (val_register, nactuals, 0, 0));
  {
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nactuals));
    STACK_PUSH (variable);
    STACK_PUSH (environment);
    exp_register = SHARP_F;
    Store_Return (RC_COMP_LOOKUP_APPLY_RESTART);
    Save_Cont ();
    RETURN_TO_C (code);
  }
}

C_TO_SCHEME long
DEFUN_VOID (comp_lookup_apply_restart)
{
  SCHEME_OBJECT environment = (STACK_POP ());
  SCHEME_OBJECT variable = (STACK_POP ());
  SCHEME_OBJECT value;
  long code = (lookup_variable (environment, variable, (&value)));
  if (code == PRIM_DONE)
    {
      SCHEME_OBJECT nactuals = (STACK_POP ());
      STACK_PUSH (value);
      STACK_PUSH (nactuals);
      if (COMPILED_CODE_ADDRESS_P (value))
	return (apply_compiled_procedure ());
      else
	return (PRIM_APPLY);
    }
  else
    {
      STACK_PUSH (variable);
      STACK_PUSH (environment);
      exp_register = SHARP_F;
      Store_Return (RC_COMP_LOOKUP_APPLY_RESTART);
      Save_Cont ();
      return (code);
    }
}

SCHEME_UTILITY utility_result
DEFNX (comutil_primitive_error,
       (ret_add_raw, primitive, ignore_3, ignore_4),
       SCHEME_ADDR ret_add_raw
       AND SCHEME_OBJECT primitive
       AND long ignore_3 AND long ignore_4)
{
  instruction * ret_add =
    ((instruction *) (SCHEME_ADDR_TO_ADDR (ret_add_raw)));

  STACK_PUSH (ENTRY_TO_OBJECT (ret_add));
  STACK_PUSH (primitive);
  exp_register = SHARP_F;
  Store_Return (RC_COMP_ERROR_RESTART);
  Save_Cont ();
  RETURN_TO_C (ERR_COMPILED_CODE_ERROR);
}

C_TO_SCHEME long
DEFUN_VOID (comp_error_restart)
{
  instruction * ret_add;

  (void) STACK_POP ();		/* primitive */
  ret_add = ((instruction *) (OBJECT_ADDRESS (STACK_POP ())));
  ENTER_SCHEME (ret_add);
}

/* Procedures to destructure compiled entries and closures. */

/*
  Extract the debugging information attached to `block'.  Usually
  this is a string which contains the filename where the debugging
  info is stored.
 */

C_UTILITY SCHEME_OBJECT
DEFUN (compiled_block_debugging_info,
       (block),
       SCHEME_OBJECT block)
{
  long length;

  length = (VECTOR_LENGTH (block));
  return (FAST_MEMORY_REF (block, (length - 1)));
}

/* Extract the environment where the `block' was "loaded". */

C_UTILITY SCHEME_OBJECT
DEFUN (compiled_block_environment,
       (block),
       SCHEME_OBJECT block)
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
DEFUN (compiled_entry_to_block_address,
       (entry),
       SCHEME_OBJECT entry)
{
  SCHEME_OBJECT *block_address;

  Get_Compiled_Block (block_address, (OBJECT_ADDRESS (entry)));
  return (block_address);
}

C_UTILITY SCHEME_OBJECT
DEFUN (compiled_entry_to_block,
       (entry),
       SCHEME_OBJECT entry)
{
  SCHEME_OBJECT *block_address;

  Get_Compiled_Block (block_address, (OBJECT_ADDRESS (entry)));
  return (MAKE_CC_BLOCK (block_address));
}

/* Returns the offset from the block to the entry point. */

#ifndef CC_BLOCK_DISTANCE

#define CC_BLOCK_DISTANCE(block,entry)					\
  (((char *) (entry)) - ((char *) (block)))

#endif /* CC_BLOCK_DISTANCE */

C_UTILITY long
DEFUN (compiled_entry_to_block_offset,
       (entry),
       SCHEME_OBJECT entry)
{
  SCHEME_OBJECT *entry_address, *block_address;

  entry_address = (OBJECT_ADDRESS (entry));
  Get_Compiled_Block (block_address, entry_address);
  return (CC_BLOCK_DISTANCE (block_address, entry_address));
}

/*
  Check whether the compiled code block whose address is `block_addr'
  is a compiled closure block.
 */

static long
DEFUN (block_address_closure_p,
       (block_addr),
       SCHEME_OBJECT * block_addr)
{
  SCHEME_OBJECT header_word;

  header_word = (*block_addr);
  return (((OBJECT_TYPE (header_word)) == TC_MANIFEST_CLOSURE));
}

/*
  Check whether the compiled code block `block' is a compiled closure block.
 */

C_UTILITY long
DEFUN (compiled_block_closure_p,
       (block),
       SCHEME_OBJECT block)
{
  return (block_address_closure_p (OBJECT_ADDRESS (block)));
}

/*
  Check whether the compiled entry point `entry' is a compiled closure.
 */

C_UTILITY long
DEFUN (compiled_entry_closure_p,
       (entry),
       SCHEME_OBJECT entry)
{
  return (block_address_closure_p (compiled_entry_to_block_address (entry)));
}

/*
  Extract the entry point ultimately invoked by the compiled closure
  represented by `entry'.
 */

C_UTILITY SCHEME_OBJECT
DEFUN (compiled_closure_to_entry,
       (entry),
       SCHEME_OBJECT entry)
{
  SCHEME_OBJECT real_entry;

  EXTRACT_CLOSURE_ENTRY_ADDRESS (real_entry, (OBJECT_ADDRESS (entry)));
  return (ENTRY_TO_OBJECT (SCHEME_ADDR_TO_ADDR (real_entry)));
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

/* Other subtypes */

#define OTHER_CLOSURE				0
#define OTHER_RANDOM				1

C_UTILITY void
DEFUN (compiled_entry_type,
       (entry, buffer),
       SCHEME_OBJECT entry AND long * buffer)
{
  long kind, min_arity, max_arity, field1, field2;
  SCHEME_OBJECT * entry_address;

  entry_address = (OBJECT_ADDRESS (entry));
  max_arity = (COMPILED_ENTRY_MAXIMUM_ARITY (entry_address));
  min_arity = (COMPILED_ENTRY_MINIMUM_ARITY (entry_address));
  field1 = min_arity;
  field2 = max_arity;
  if (min_arity >= 0)
    kind = KIND_PROCEDURE;
  else if (max_arity >= 0)
    kind = KIND_ILLEGAL;
  else if ((((unsigned long) max_arity) & 0xff) < 0xe0)
  {
    /* Field2 is the offset to the next continuation */

    kind = KIND_CONTINUATION;
    field1 = CONTINUATION_NORMAL;
    field2 = (((((unsigned long) max_arity) & 0x3f) << 7)
	      | (((unsigned long) min_arity) & 0x7f));
  }
  else if (min_arity != -1)
    kind = KIND_ILLEGAL;

  else
  {
    switch (((unsigned long) max_arity) & 0xff)
    {
      case FORMAT_BYTE_EXPR:
      {
        kind = KIND_EXPRESSION;
        break;
      }
      case FORMAT_BYTE_CLOSURE:
      {
        kind = KIND_OTHER;
	field1 = OTHER_CLOSURE;
        break;
      }
      case FORMAT_BYTE_COMPLR:
      case FORMAT_BYTE_CMPINT:
      {
        kind = KIND_OTHER;
	field1 = OTHER_RANDOM;
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
        field2 = ((long) (entry != return_to_interpreter));
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
}

void
DEFUN (declare_compiled_code_block, (block), SCHEME_OBJECT block)
{
#ifdef PUSH_D_CACHE_REGION
  SCHEME_OBJECT * block_addr = (OBJECT_ADDRESS (block));
  PUSH_D_CACHE_REGION (block_addr, (1+ (OBJECT_DATUM (* block_addr))));
#endif
}

/* Destructuring free variable caches. */

C_UTILITY void
DEFUN (store_variable_cache,
       (extension, block, offset),
       SCHEME_OBJECT extension AND SCHEME_OBJECT block
       AND long offset)
{
  FAST_MEMORY_SET (block, offset,
                   ((SCHEME_OBJECT)
		    (ADDR_TO_SCHEME_ADDR (OBJECT_ADDRESS (extension)))));
}

C_UTILITY SCHEME_OBJECT
DEFUN (extract_variable_cache,
       (block, offset),
       SCHEME_OBJECT block AND long offset)
{
  return (MAKE_POINTER_OBJECT (CACHE_TYPE,
                               ((SCHEME_OBJECT *)
				(SCHEME_ADDR_TO_ADDR
				 (FAST_MEMORY_REF (block, offset))))));
}

/* Get a compiled procedure from a cached operator reference. */

C_UTILITY SCHEME_OBJECT
DEFUN (extract_uuo_link,
       (block, offset),
       SCHEME_OBJECT block AND long offset)
{
  SCHEME_OBJECT * cache_address, compiled_entry_address;

  cache_address = (MEMORY_LOC (block, offset));
  EXTRACT_EXECUTE_CACHE_ADDRESS (compiled_entry_address, cache_address);
  return (ENTRY_TO_OBJECT (SCHEME_ADDR_TO_ADDR (compiled_entry_address)));
}

static void
DEFUN (store_uuo_link,
       (entry, cache_address),
       SCHEME_OBJECT entry AND SCHEME_OBJECT * cache_address)
{
  SCHEME_OBJECT * entry_address;

  entry_address = (OBJECT_ADDRESS (entry));
  STORE_EXECUTE_CACHE_CODE (cache_address);
  STORE_EXECUTE_CACHE_ADDRESS (cache_address,
			       (ADDR_TO_SCHEME_ADDR (entry_address)));
#ifdef FLUSH_I_CACHE_REGION
  if (!linking_cc_block_p)
    {
      /* The linker will flush the whole region afterwards. */
      FLUSH_I_CACHE_REGION (cache_address, EXECUTE_CACHE_ENTRY_SIZE);
    }
#endif
}

/* This makes a fake compiled procedure which traps to kind handler when
   invoked.
 */

#define TRAMPOLINE_SIZE	(TRAMPOLINE_ENTRY_SIZE + 2)

/* Enabled so that the profiler can distinguish trampolines */

#if 1 || defined(AUTOCLOBBER_BUG)
#  define TC_TRAMPOLINE_HEADER	TC_FIXNUM
#else
#  define TC_TRAMPOLINE_HEADER	TC_MANIFEST_VECTOR
#endif

static void
DEFUN (fill_trampoline,
       (block, entry_point, fmt_word, kind),
       SCHEME_OBJECT * block
       AND instruction * entry_point
       AND format_word fmt_word
       AND long kind)
{
  (COMPILED_ENTRY_FORMAT_WORD (entry_point)) = fmt_word;
  (COMPILED_ENTRY_OFFSET_WORD (entry_point)) =
    (MAKE_OFFSET_WORD (entry_point, block, false));
  STORE_TRAMPOLINE_ENTRY (entry_point, kind);
  return;
}

static long
DEFUN (make_trampoline,
       (slot, fmt_word, kind, size, value1, value2, value3),
       SCHEME_OBJECT * slot
       AND format_word fmt_word
       AND long kind AND long size
       AND SCHEME_OBJECT value1 AND SCHEME_OBJECT value2
       AND SCHEME_OBJECT value3)
{
  instruction * entry_point;
  SCHEME_OBJECT * ptr;

  if (GC_Check (TRAMPOLINE_SIZE + size))
  {
    Request_GC (TRAMPOLINE_SIZE + size);
    return (PRIM_INTERRUPT);
  }

  ptr = Free;
  Free += (TRAMPOLINE_SIZE + size);
  ptr[0] = (MAKE_OBJECT (TC_TRAMPOLINE_HEADER,
				((TRAMPOLINE_SIZE - 1) + size)));
  ptr[1] = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,
			   TRAMPOLINE_ENTRY_SIZE));
  entry_point = ((instruction *) (TRAMPOLINE_ENTRY_POINT (ptr)));
  fill_trampoline (ptr, entry_point, fmt_word, kind);
  *slot = (ENTRY_TO_OBJECT (entry_point));
  ptr = (TRAMPOLINE_STORAGE (entry_point));
  if ((--size) >= 0)
    *ptr++ = value1;
  if ((--size) >= 0)
    *ptr++ = value2;
  if ((--size) >= 0)
    *ptr++ = value3;
  return (PRIM_DONE);
}

/* Standard trampolines. */

static long
DEFUN (make_redirection_trampoline,
       (slot, kind, procedure),
       SCHEME_OBJECT * slot AND long kind AND SCHEME_OBJECT procedure)
{
  return (make_trampoline (slot,
			   ((format_word) FORMAT_WORD_CMPINT),
			   kind,
			   1,
			   procedure,
			   SHARP_F,
			   SHARP_F));
}

static long
DEFUN (make_apply_trampoline,
       (slot, kind, procedure, nactuals),
       SCHEME_OBJECT * slot AND long kind
       AND SCHEME_OBJECT procedure AND long nactuals)
{
  return (make_trampoline (slot,
			   ((format_word) FORMAT_WORD_CMPINT),
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
DEFUN (make_uuo_link,
       (procedure, extension, block, offset),
       SCHEME_OBJECT procedure AND SCHEME_OBJECT extension
       AND SCHEME_OBJECT block AND long offset)
{
  long kind, result;
  unsigned long nactuals;
  SCHEME_OBJECT orig_proc, trampoline, *cache_address;

  cache_address = (MEMORY_LOC (block, offset));
  EXTRACT_EXECUTE_CACHE_ARITY (nactuals, cache_address);
  /* nactuals >= 0 */

  orig_proc = procedure;
loop:
  switch (OBJECT_TYPE (procedure))
  {
    case TC_COMPILED_ENTRY:
    {
      SCHEME_OBJECT * entry;
      long nmin, nmax;

      entry = (OBJECT_ADDRESS (procedure));
      nmax = (COMPILED_ENTRY_MAXIMUM_ARITY (entry));
      if (((long) nactuals) == nmax)
      {
        store_uuo_link (procedure, cache_address);
        return (PRIM_DONE);
      }
      nmin = (COMPILED_ENTRY_MINIMUM_ARITY (entry));
      if ((nmax > 1) && (nmin > 0) && (nmin <= ((long) nactuals)) &&
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
      SCHEME_OBJECT data;

      data = (MEMORY_REF (procedure, ENTITY_DATA));
      if ((VECTOR_P (data))
	  && (nactuals < (VECTOR_LENGTH (data)))
	  && ((VECTOR_REF (data, nactuals)) != SHARP_F)
	  && ((VECTOR_REF (data, 0))
	      == (Get_Fixed_Obj_Slot (ARITY_DISPATCHER_TAG))))
      {
	/* No loops allowed! */
	SCHEME_OBJECT nproc = (VECTOR_REF (data, nactuals));

	if ((procedure == orig_proc) && (nproc != procedure))
	{
	  procedure = nproc;
	  goto loop;
	}
	else
	  procedure = orig_proc;
      }
      kind = TRAMPOLINE_K_ENTITY;
      break;
    }

    case TC_PRIMITIVE:
    {
      long arity;

      arity = (PRIMITIVE_ARITY (procedure));
      if (arity == ((long) (nactuals - 1)))
      {
	nactuals = 0;
        kind = TRAMPOLINE_K_PRIMITIVE;
      }
      else if (arity == LEXPR_PRIMITIVE_ARITY)
        kind = TRAMPOLINE_K_LEXPR_PRIMITIVE;
      else
        kind = TRAMPOLINE_K_OTHER;
      break;
    }

    case TC_PROCEDURE: /* and some others... */
    default:
    /* uuo_link_interpreted: */
    {
      kind = TRAMPOLINE_K_INTERPRETED;
      break;
    }
  }
  if (nactuals == 0)
    result = (make_redirection_trampoline (&trampoline, kind, procedure));
  else
    result = (make_apply_trampoline (&trampoline, kind, procedure, nactuals));
  if (result != PRIM_DONE)
    return (result);
  store_uuo_link (trampoline, cache_address);
  return (PRIM_DONE);
}

C_UTILITY long
DEFUN (make_fake_uuo_link,
       (extension, block, offset),
       SCHEME_OBJECT extension AND SCHEME_OBJECT block AND long offset)
{
  long result;
  SCHEME_OBJECT trampoline, *cache_address;

  result = (make_trampoline (&trampoline,
			     ((format_word) FORMAT_WORD_CMPINT),
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
DEFUN (coerce_to_compiled,
       (procedure, arity, location),
       SCHEME_OBJECT procedure AND long arity AND SCHEME_OBJECT * location)
{
  long frame_size;

  frame_size = (arity + 1);
  if ((!(COMPILED_CODE_ADDRESS_P (procedure))) ||
      (((long) (COMPILED_ENTRY_MAXIMUM_ARITY (OBJECT_ADDRESS (procedure)))) !=
       frame_size))
  {
    if (frame_size > FORMAT_BYTE_FRAMEMAX)
      return (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    return (make_trampoline (location,
			     ((format_word)
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

#ifndef HAVE_BKPT_SUPPORT

C_UTILITY SCHEME_OBJECT
DEFUN (bkpt_install, (ep), PTR ep)
{
  return (SHARP_F);
}

C_UTILITY SCHEME_OBJECT
DEFUN (bkpt_closure_install, (ep), PTR ep)
{
  return (SHARP_F);
}

C_UTILITY void
DEFUN (bkpt_remove, (ep, handle), PTR ep AND SCHEME_OBJECT handle)
{
  error_external_return ();
}

C_UTILITY Boolean
DEFUN (bkpt_p, (ep), PTR ep)
{
  return (FALSE);
}

C_UTILITY SCHEME_OBJECT
DEFUN (bkpt_proceed, (ep, handle, state),
       PTR ep AND SCHEME_OBJECT handle AND SCHEME_OBJECT state)
{
  error_external_return ();
  return (UNSPECIFIC);
}

C_UTILITY PTR
DEFUN (do_bkpt_proceed, (value), unsigned long * value)
{
  * value = ((unsigned long) ERR_EXTERNAL_RETURN);
  return (FALSE);
}

#else /* HAVE_BKPT_SUPPORT */

#define BKPT_PROCEED_FRAME_SIZE	3

C_UTILITY SCHEME_OBJECT
DEFUN (bkpt_proceed, (ep, handle, state),
       PTR ep AND SCHEME_OBJECT handle AND SCHEME_OBJECT state)
{
  if ((! (COMPILED_CODE_ADDRESS_P (STACK_REF (BKPT_PROCEED_FRAME_SIZE))))
      || ((OBJECT_ADDRESS (STACK_REF (BKPT_PROCEED_FRAME_SIZE)))
	  != ((SCHEME_OBJECT *) ep)))
    error_external_return ();

  STACK_PUSH (FIXNUM_ZERO + REFLECT_CODE_CC_BKPT);
  STACK_PUSH (reflect_to_interface);
  sp_register = (STACK_LOC (- BKPT_PROCEED_FRAME_SIZE));
  return (SHARP_F);
}
#endif /* HAVE_BKPT_SUPPORT */

SCHEME_UTILITY utility_result
DEFNX (comutil_compiled_code_bkpt,
       (entry_point_raw, state_raw, ignore_3, ignore_4),
       SCHEME_ADDR entry_point_raw AND SCHEME_ADDR state_raw
       AND long ignore_3 AND long ignore_4)
{
  long type_info[3];
  instruction * entry_point_a
    = ((instruction *) (SCHEME_ADDR_TO_ADDR (entry_point_raw)));
  SCHEME_OBJECT entry_point = (ENTRY_TO_OBJECT (entry_point_a));
  SCHEME_OBJECT state;
  SCHEME_OBJECT stack_ptr;

  STACK_PUSH (entry_point);	/* return address */

  /* Potential bug: This does not preserve the environment for
     IC procedures.  There is no way to tell that we have
     an IC procedure in our hands.  It is not safe to preserve
     it in general because the contents of the register may
     be stale (predate the last GC).
     However, the compiler no longer generates IC procedures, and
     will probably never do it again.
   */

  compiled_entry_type (entry_point, &type_info[0]);
  if ((type_info[0] == KIND_OTHER) && (type_info[1] == OTHER_CLOSURE))
  {
    entry_point_a = ((instruction *) (SCHEME_ADDR_TO_ADDR (state_raw)));
    state = (ENTRY_TO_OBJECT (entry_point_a));
  }
  else if (type_info[0] != KIND_CONTINUATION)
    state = SHARP_F;
  else if (type_info[1] == CONTINUATION_DYNAMIC_LINK)
    state = (MAKE_POINTER_OBJECT
	     (TC_STACK_ENVIRONMENT, (SCHEME_ADDR_TO_ADDR (state_raw))));
  else
    state = val_register;

  stack_ptr = (MAKE_POINTER_OBJECT (TC_STACK_ENVIRONMENT, sp_register));
  STACK_PUSH (state);		/* state to preserve */
  STACK_PUSH (stack_ptr);	/* "Environment" pointer */
  STACK_PUSH (entry_point);	/* argument to handler */
  return (comutil_apply ((Get_Fixed_Obj_Slot (COMPILED_CODE_BKPT_HANDLER)),
			 4, ignore_3, ignore_4));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_compiled_closure_bkpt,
       (entry_point_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR entry_point_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  instruction * entry_point_a
    = ((instruction *) (SCHEME_ADDR_TO_ADDR (entry_point_raw)));
  SCHEME_OBJECT entry_point = (ENTRY_TO_OBJECT (entry_point_a));
  SCHEME_OBJECT stack_ptr;

  STACK_PUSH (entry_point);	/* return address */

  stack_ptr = (MAKE_POINTER_OBJECT (TC_STACK_ENVIRONMENT, sp_register));
  STACK_PUSH (SHARP_F);		/* state to preserve */
  STACK_PUSH (stack_ptr);	/* "Environment" pointer */
  STACK_PUSH (entry_point);	/* argument to handler */
  return (comutil_apply ((Get_Fixed_Obj_Slot (COMPILED_CODE_BKPT_HANDLER)),
			 4, ignore_3, ignore_4));
}

SCHEME_UTILITY utility_result
DEFNX (comutil_reflect_to_interface,
       (tramp_data_raw, ignore_2, ignore_3, ignore_4),
       SCHEME_ADDR tramp_data_raw
       AND long ignore_2 AND long ignore_3 AND long ignore_4)
{
  SCHEME_OBJECT code = (STACK_POP ());

  switch (OBJECT_DATUM (code))
  {
    case REFLECT_CODE_INTERNAL_APPLY:
    {
      long frame_size = (OBJECT_DATUM (STACK_POP ()));
      SCHEME_OBJECT procedure = (STACK_POP ());
      
      return (comutil_apply (procedure, frame_size, ignore_3, ignore_4));
    }

    case REFLECT_CODE_RESTORE_INTERRUPT_MASK:
    {
      SET_INTERRUPT_MASK (OBJECT_DATUM (STACK_POP ()));
      INVOKE_RETURN_ADDRESS ();
    }

    case REFLECT_CODE_STACK_MARKER:
    {
      (void) STACK_POP ();	/* marker1 */
      (void) STACK_POP ();	/* marker2 */
      INVOKE_RETURN_ADDRESS ();
    }

    case REFLECT_CODE_CC_BKPT:
    {
      unsigned long value;

      /* Attempt to process interrupts before really proceeding. */

      if (((long) (ADDR_TO_SCHEME_ADDR (Free)))
	  >= ((long) (Registers[REGBLOCK_MEMTOP])))
      {
	STACK_PUSH (FIXNUM_ZERO + REFLECT_CODE_CC_BKPT);
	STACK_PUSH (reflect_to_interface);
	return (compiler_interrupt_common (0, SHARP_F));
      }

      if (do_bkpt_proceed (& value))
	RETURN_TO_SCHEME (value);
      else
	RETURN_TO_C (value);
    }

    default:
      STACK_PUSH (code);
      RETURN_TO_C (ERR_EXTERNAL_RETURN);
  }
}

/*
  Utility table used by the assembly language interface to invoke
  the SCHEME_UTILITY procedures that appear in this file.

  Important: Do NOT reorder this table without changing the indices
  defined on the following page and the corresponding table in the
  compiler.

  In addition, this table must be declared before compiler_reset_internal.
 */

#define UTE(name) ((utility_table_entry) name)

utility_table_entry utility_table[] =
{
  UTE(comutil_return_to_interpreter),		/* 0x0 */
  UTE(comutil_operator_apply_trap),		/* 0x1 */
  UTE(comutil_operator_arity_trap),		/* 0x2 */
  UTE(comutil_operator_entity_trap),		/* 0x3 */
  UTE(comutil_operator_interpreted_trap),	/* 0x4 */
  UTE(comutil_operator_lexpr_trap),		/* 0x5 */
  UTE(comutil_operator_primitive_trap),		/* 0x6 */
  UTE(comutil_operator_lookup_trap),		/* 0x7 */
  UTE(comutil_operator_1_0_trap),		/* 0x8 */
  UTE(comutil_operator_2_1_trap),		/* 0x9 */
  UTE(comutil_operator_2_0_trap),		/* 0xa */
  UTE(comutil_operator_3_2_trap),		/* 0xb */
  UTE(comutil_operator_3_1_trap),		/* 0xc */
  UTE(comutil_operator_3_0_trap),		/* 0xd */
  UTE(comutil_operator_4_3_trap),		/* 0xe */
  UTE(comutil_operator_4_2_trap),		/* 0xf */
  UTE(comutil_operator_4_1_trap),		/* 0x10 */
  UTE(comutil_operator_4_0_trap),		/* 0x11 */
  UTE(comutil_primitive_apply),			/* 0x12 */
  UTE(comutil_primitive_lexpr_apply),		/* 0x13 */
  UTE(comutil_apply),				/* 0x14 */
  UTE(comutil_error),				/* 0x15 */
  UTE(comutil_lexpr_apply),			/* 0x16 */
  UTE(comutil_link),				/* 0x17 */
  UTE(comutil_interrupt_closure),		/* 0x18 */
  UTE(comutil_interrupt_dlink),			/* 0x19 */
  UTE(comutil_interrupt_procedure),		/* 0x1a */
  UTE(comutil_interrupt_continuation),		/* 0x1b */
  UTE(comutil_interrupt_ic_procedure),		/* 0x1c */
  UTE(comutil_assignment_trap),			/* 0x1d */
  UTE(comutil_cache_lookup_apply),		/* 0x1e */
  UTE(comutil_lookup_trap),			/* 0x1f */
  UTE(comutil_safe_lookup_trap),		/* 0x20 */
  UTE(comutil_unassigned_p_trap),		/* 0x21 */
  UTE(comutil_decrement),			/* 0x22 */
  UTE(comutil_divide),				/* 0x23 */
  UTE(comutil_equal),				/* 0x24 */
  UTE(comutil_greater),				/* 0x25 */
  UTE(comutil_increment),			/* 0x26 */
  UTE(comutil_less),				/* 0x27 */
  UTE(comutil_minus),				/* 0x28 */
  UTE(comutil_multiply),			/* 0x29 */
  UTE(comutil_negative),			/* 0x2a */
  UTE(comutil_plus),				/* 0x2b */
  UTE(comutil_positive),			/* 0x2c */
  UTE(comutil_zero),				/* 0x2d */
  UTE(comutil_access),				/* 0x2e */
  UTE(comutil_reference),			/* 0x2f */
  UTE(comutil_safe_reference),			/* 0x30 */
  UTE(comutil_unassigned_p),			/* 0x31 */
  UTE(comutil_unbound_p),			/* 0x32 */
  UTE(comutil_assignment),			/* 0x33 */
  UTE(comutil_definition),			/* 0x34 */
  UTE(comutil_lookup_apply),			/* 0x35 */
  UTE(comutil_primitive_error),			/* 0x36 */
  UTE(comutil_quotient),			/* 0x37 */
  UTE(comutil_remainder),			/* 0x38 */
  UTE(comutil_modulo),				/* 0x39 */
  UTE(comutil_reflect_to_interface),		/* 0x3a */
  UTE(comutil_interrupt_continuation_2),	/* 0x3b */
  UTE(comutil_compiled_code_bkpt),		/* 0x3c */
  UTE(comutil_compiled_closure_bkpt)		/* 0x3d */
  };

extern long MAX_TRAMPOLINE;
long MAX_TRAMPOLINE = ((sizeof (utility_table))
		       / (sizeof (utility_table_entry)));

/* Support for trap handling. */

static void
DEFUN_VOID (end_of_utils)
{
  return;
}

struct util_descriptor_s
{
  PTR pc;
  char * name;
};

#ifdef STDC_HEADERS
#  define UTLD(name)  { ((PTR) name), #name }
#else
/* Hope that this works. */
#  define UTLD(name)  { ((PTR) name), "name" }
#endif

static
struct util_descriptor_s utility_descriptor_table[] =
{
#ifdef DECLARE_CMPINTMD_UTILITIES
  DECLARE_CMPINTMD_UTILITIES(),
#endif /* DECLARE_CMPINTMD_UTILITIES */
  UTLD(C_to_interface),
  UTLD(open_gap),
  UTLD(setup_lexpr_invocation),
  UTLD(setup_compiled_invocation),
  UTLD(enter_compiled_expression),
  UTLD(apply_compiled_procedure),
  UTLD(return_to_compiled_code),
  UTLD(apply_compiled_from_primitive),
  UTLD(compiled_with_interrupt_mask),
  UTLD(compiled_with_stack_marker),
  UTLD(comutil_return_to_interpreter),
  UTLD(comutil_primitive_apply),
  UTLD(comutil_primitive_lexpr_apply),
  UTLD(comutil_apply),
  UTLD(comutil_error),
  UTLD(comutil_lexpr_apply),
  UTLD(abort_link_cc_block),
  UTLD(link_cc_block),
  UTLD(comutil_link),
  UTLD(comp_link_caches_restart),
  UTLD(comutil_operator_apply_trap),
  UTLD(comutil_operator_arity_trap),
  UTLD(comutil_operator_entity_trap),
  UTLD(comutil_operator_interpreted_trap),
  UTLD(comutil_operator_lexpr_trap),
  UTLD(comutil_operator_primitive_trap),
  UTLD(comutil_operator_lookup_trap),
  UTLD(comp_op_lookup_trap_restart),
  UTLD(comutil_operator_1_0_trap),
  UTLD(comutil_operator_2_1_trap),
  UTLD(comutil_operator_2_0_trap),
  UTLD(comutil_operator_3_2_trap),
  UTLD(comutil_operator_3_1_trap),
  UTLD(comutil_operator_3_0_trap),
  UTLD(comutil_operator_4_3_trap),
  UTLD(comutil_operator_4_2_trap),
  UTLD(comutil_operator_4_1_trap),
  UTLD(comutil_operator_4_0_trap),
  UTLD(compiler_interrupt_common),
  UTLD(comutil_interrupt_closure),
  UTLD(comutil_interrupt_dlink),
  UTLD(comutil_interrupt_procedure),
  UTLD(comutil_interrupt_continuation),
  UTLD(comutil_interrupt_ic_procedure),
  UTLD(comutil_interrupt_continuation_2),
  UTLD(comp_interrupt_restart),

  UTLD(comutil_assignment_trap),
  UTLD(comp_assignment_trap_restart),
  UTLD(comutil_cache_lookup_apply),
  UTLD(comp_cache_lookup_apply_restart),
  UTLD(comutil_lookup_trap),
  UTLD(comp_lookup_trap_restart),
  UTLD(comutil_safe_lookup_trap),
  UTLD(comp_safe_lookup_trap_restart),
  UTLD(comutil_unassigned_p_trap),
  UTLD(comp_unassigned_p_trap_restart),
  UTLD(comutil_decrement),
  UTLD(comutil_divide),
  UTLD(comutil_equal),
  UTLD(comutil_greater),
  UTLD(comutil_increment),
  UTLD(comutil_less),
  UTLD(comutil_minus),
  UTLD(comutil_modulo),
  UTLD(comutil_multiply),
  UTLD(comutil_negative),
  UTLD(comutil_plus),
  UTLD(comutil_positive),
  UTLD(comutil_quotient),
  UTLD(comutil_remainder),
  UTLD(comutil_zero),
  UTLD(comutil_access),
  UTLD(comp_access_restart),
  UTLD(comutil_reference),
  UTLD(comp_reference_restart),
  UTLD(comutil_safe_reference),
  UTLD(comp_safe_reference_restart),
  UTLD(comutil_unassigned_p),
  UTLD(comp_unassigned_p_restart),
  UTLD(comutil_unbound_p),
  UTLD(comp_unbound_p_restart),
  UTLD(comutil_assignment),
  UTLD(comp_assignment_restart),
  UTLD(comutil_definition),
  UTLD(comp_definition_restart),
  UTLD(comutil_lookup_apply),
  UTLD(comp_lookup_apply_restart),
  UTLD(comutil_primitive_error),
  UTLD(comp_error_restart),
  UTLD(compiled_block_debugging_info),
  UTLD(compiled_block_environment),
  UTLD(compiled_entry_to_block_address),
  UTLD(compiled_entry_to_block),
  UTLD(compiled_entry_to_block_offset),
  UTLD(block_address_closure_p),
  UTLD(compiled_block_closure_p),
  UTLD(compiled_entry_closure_p),
  UTLD(compiled_closure_to_entry),
  UTLD(compiled_entry_type),
  UTLD(declare_compiled_code_block),
  UTLD(store_variable_cache),
  UTLD(extract_variable_cache),
  UTLD(extract_uuo_link),
  UTLD(store_uuo_link),
  UTLD(fill_trampoline),
  UTLD(make_trampoline),
  UTLD(make_redirection_trampoline),
  UTLD(make_apply_trampoline),
  UTLD(make_uuo_link),
  UTLD(make_fake_uuo_link),
  UTLD(coerce_to_compiled),
#ifndef HAVE_BKPT_SUPPORT
  UTLD(bkpt_install),
  UTLD(bkpt_closure_install),
  UTLD(bkpt_remove),
  UTLD(bkpt_p),
  UTLD(do_bkpt_proceed),
#endif 
  UTLD(bkpt_proceed),
  UTLD(comutil_compiled_code_bkpt),
  UTLD(comutil_compiled_closure_bkpt),
  UTLD(comutil_reflect_to_interface),
  UTLD(end_of_utils)
};

extern char * EXFUN (utility_index_to_name, (int));
extern int EXFUN (pc_to_utility_index, (unsigned long));

#define UTIL_TABLE_PC_REF_REAL(index)					\
  ((unsigned long) (utility_descriptor_table[index].pc))

#ifndef UTIL_TABLE_PC_REF
#  define UTIL_TABLE_PC_REF(index)	(UTIL_TABLE_PC_REF_REAL (index))
#endif

static int last_util_table_index =
  (((sizeof (utility_descriptor_table)) / (sizeof (struct util_descriptor_s)))
   - 1);

char *
DEFUN (utility_index_to_name, (index), int index)
{
  if ((index < 0) || (index >= last_util_table_index))
    return ((char *) NULL);
  else
    return (utility_descriptor_table[index].name);
}

int
DEFUN (pc_to_utility_index, (pc), unsigned long pc)
{
  /* Binary search */

  extern int EXFUN (pc_to_builtin_index, (unsigned long));

  if ((pc < (UTIL_TABLE_PC_REF (0)))
      || (pc >= (UTIL_TABLE_PC_REF (last_util_table_index))))
    return (-1);
  else if (pc < (UTIL_TABLE_PC_REF (1)))
    return (((pc_to_builtin_index (pc)) == -1) ? 0 : -1);
  else
  {
    int low, high, middle;

    low = 0;
    high = last_util_table_index;
    while ((low + 1) < high)
    {
      middle = ((low + high) / 2);
      if (pc < (UTIL_TABLE_PC_REF (middle)))
	high = middle;
      else if (pc > (UTIL_TABLE_PC_REF (middle)))
	low = middle;
      else
	return (middle);
    }
    return ((pc == (UTIL_TABLE_PC_REF (high))) ? high : low);
  }
}

extern char * EXFUN (builtin_index_to_name, (int));
extern void EXFUN (declare_builtin, (unsigned long, char *));
extern int EXFUN (pc_to_builtin_index, (unsigned long));
extern unsigned long * builtins;

static int n_builtins = 0;
static int s_builtins = 0;
unsigned long * builtins = ((unsigned long *) NULL);
char ** builtin_names = ((char **) NULL);

void
DEFUN (declare_builtin, (builtin, name),
       unsigned long builtin AND char * name)
{
  if (n_builtins == s_builtins)
  {
    if (s_builtins == 0)
    {
      s_builtins = 30;
      builtins = ((unsigned long *)
		  (malloc (s_builtins * (sizeof (unsigned long)))));
      builtin_names = ((char **) (malloc (s_builtins * (sizeof (char *)))));
    }
    else
    {
      s_builtins += s_builtins;
      builtins = ((unsigned long *)
		  (realloc (builtins,
			    (s_builtins * (sizeof (unsigned long))))));
      builtin_names = ((char **)
		       (realloc (builtin_names,
				 (s_builtins * (sizeof (char *))))));
    }
    if ((builtins == ((unsigned long *) NULL))
	|| (builtin_names == ((char **) NULL)))
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

char *
DEFUN (builtin_index_to_name, (index), int index)
{
  if ((index < 0) || (index >= n_builtins))
    return ((char *) NULL);
  else
    return (builtin_names[index]);
}

int
DEFUN (pc_to_builtin_index, (pc), unsigned long pc)
{
  /* Binary search */

  if ((builtins == ((unsigned long *) NULL))
      || (pc < (builtins[0]))
      || (pc >= (builtins[n_builtins - 1])))
    return (-1);
  else
  {
    int low, high, middle;

    low = 0;
    high = (n_builtins - 1);
    while ((low + 1) < high)
    {
      middle = ((low + high) / 2);
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

/* Initialization */

#define COMPILER_INTERFACE_VERSION		3

#ifndef COMPILER_REGBLOCK_N_FIXED
#  define COMPILER_REGBLOCK_N_FIXED		16
#endif

#ifndef COMPILER_REGBLOCK_N_TEMPS
#  define COMPILER_REGBLOCK_N_TEMPS		256
#endif

#ifndef COMPILER_REGBLOCK_EXTRA_SIZE
#  define COMPILER_REGBLOCK_EXTRA_SIZE		0
#endif

#if (REGBLOCK_MINIMUM_LENGTH > COMPILER_REGBLOCK_N_FIXED)
#  include "ERROR: cmpint.c and const.h disagree on REGBLOCK_MINIMUM_LENGTH!"
#endif

/* ((sizeof(SCHEME_OBJECT)) / (sizeof(SCHEME_OBJECT))) */

#define COMPILER_FIXED_SIZE	1

#ifndef COMPILER_TEMP_SIZE
#  define COMPILER_TEMP_SIZE	((sizeof (double)) / (sizeof (SCHEME_OBJECT)))
#endif

#define REGBLOCK_LENGTH							\
  ((COMPILER_REGBLOCK_N_FIXED * COMPILER_FIXED_SIZE) +			\
   (COMPILER_REGBLOCK_N_TEMPS * COMPILER_TEMP_SIZE) +			\
   COMPILER_REGBLOCK_EXTRA_SIZE)

#ifndef ASM_RESET_HOOK
#  define ASM_RESET_HOOK() NOP()
#endif

long
  compiler_processor_type,
  compiler_interface_version;

SCHEME_OBJECT
  compiler_utilities,
  return_to_interpreter;

#if !defined(REGBLOCK_ALLOCATED_BY_INTERFACE) && !defined(__WIN32__)
SCHEME_OBJECT Registers [REGBLOCK_LENGTH];
#endif

static void
DEFUN_VOID (compiler_reset_internal)
{
  long len;
  SCHEME_OBJECT * block;

  /* Other stuff can be placed here. */

  block = (OBJECT_ADDRESS (compiler_utilities));
  len = (OBJECT_DATUM (block[0]));

  return_to_interpreter =
    (ENTRY_TO_OBJECT (((char *) block)
		      + ((unsigned long) (block [len - 1]))));

  reflect_to_interface =
    (ENTRY_TO_OBJECT (((char *) block)
		      + ((unsigned long) (block [len]))));

  (Registers[REGBLOCK_CLOSURE_FREE]) = ((SCHEME_OBJECT) NULL);
  (Registers[REGBLOCK_CLOSURE_SPACE]) = ((SCHEME_OBJECT) 0);
  (Registers[REGBLOCK_REFLECT_TO_INTERFACE]) = reflect_to_interface;

  ASM_RESET_HOOK();

  return;
}

#define COMPILER_UTILITIES_N_ENTRIES	2
#define COMPILER_UTILITIES_LENGTH					\
 ((COMPILER_UTILITIES_N_ENTRIES * (TRAMPOLINE_ENTRY_SIZE + 1)) + 2)

C_UTILITY void
DEFUN (compiler_initialize, (fasl_p), long fasl_p)
{
  /* Start-up of whole interpreter */

  (Registers[REGBLOCK_PRIMITIVE]) = SHARP_F;
  compiler_processor_type = COMPILER_PROCESSOR_TYPE;
  compiler_interface_version = COMPILER_INTERFACE_VERSION;
  if (fasl_p)
  {
    long len;
    instruction * tramp1, * tramp2;
    SCHEME_OBJECT * block;
    extern SCHEME_OBJECT * EXFUN (copy_to_constant_space,
				  (SCHEME_OBJECT *, long));

    len = COMPILER_UTILITIES_LENGTH;
    if (GC_Check (len))
    {
      outf_fatal ("compiler_initialize: Not enough space!\n");
      Microcode_Termination (TERM_NO_SPACE);
    }

    block = Free;
    Free += len;
    block[0] = (MAKE_OBJECT (TC_MANIFEST_VECTOR, (len - 1)));
    block[1] = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,
			     (COMPILER_UTILITIES_N_ENTRIES
			      * TRAMPOLINE_ENTRY_SIZE)));

    tramp1 = ((instruction *) (TRAMPOLINE_ENTRY_POINT (block)));
    fill_trampoline (block, tramp1,
		     ((format_word) FORMAT_WORD_RETURN),
		     TRAMPOLINE_K_RETURN);
    block[len - 2] = (((char *) tramp1) - ((char *) block));

    tramp2 = ((instruction *)
	      (((char *) tramp1)
	       + (TRAMPOLINE_ENTRY_SIZE * (sizeof (SCHEME_OBJECT)))));
    fill_trampoline (block, tramp2,
		     ((format_word) FORMAT_WORD_RETURN),
		     TRAMPOLINE_K_REFLECT_TO_INTERFACE);
    block[len - 1] = (((char *) tramp2) - ((char *) block));

    block = (copy_to_constant_space (block, len));
    compiler_utilities = (MAKE_CC_BLOCK (block));
    compiler_reset_internal ();
  }
  else
  {
    /* Delay until after band-load, when compiler_reset will be invoked. */
    compiler_utilities = SHARP_F;
    return_to_interpreter = SHARP_F;
#ifdef sonyrisc
    /* On the Sony NEWS 3250, this procedure initializes the
       floating-point CPU control register to enable the IEEE traps.
       This is normally executed by `compiler_reset' from LOAD-BAND,
       but the Sony operating system saves the control register in
       `setjmp' and restores it on `longjmp', so we must initialize
       the register before `setjmp' is called.  */
    interface_initialize ();
#endif
#ifdef __OS2__
    /* Same as for Sony.  */
    i386_interface_initialize ();
#endif
  }
  return;
}

C_UTILITY void
DEFUN (compiler_reset,
       (new_block),
       SCHEME_OBJECT new_block)
{
  /* Called after a disk restore */

  if ((OBJECT_TYPE (new_block)) != TC_COMPILED_CODE_BLOCK)
  {
    extern void EXFUN (compiler_reset_error, (void));

lose:
    compiler_reset_error ();
    return;
  }
  else if ((MEMORY_REF (new_block, 0))
	   != (MAKE_OBJECT (TC_MANIFEST_VECTOR,
			    (COMPILER_UTILITIES_LENGTH - 1))))
  {
    /* Backwards compatibility */
    if ((MEMORY_REF (new_block, 0))
	!= (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,
			 (COMPILER_UTILITIES_N_ENTRIES
			  * (TRAMPOLINE_ENTRY_SIZE + 1)))))
      goto lose;
  }
  else if ((MEMORY_REF (new_block, 1))
	   != (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,
			    (COMPILER_UTILITIES_N_ENTRIES
			     * TRAMPOLINE_ENTRY_SIZE))))
    goto lose;

  compiler_utilities = new_block;
  compiler_reset_internal ();
  return;
}

#ifndef NATIVE_CODE_IS_C

SCHEME_OBJECT *
DEFUN (cons_c_code_table, (start, limit, length),
       SCHEME_OBJECT * start
       AND SCHEME_OBJECT * limit
       AND long * length)
{
  * length = 0;
  return (start);
}

Boolean
DEFUN (install_c_code_table, (table, length),
       SCHEME_OBJECT * table AND long length)
{
  return (length == 0);
}

#endif /* NATIVE_CODE_IS_C */

#else	/* not HAS_COMPILER_SUPPORT */

/* Stubs for compiler utilities.
   All entries error out or kill the microcode.
 */

extern void EXFUN (Microcode_Termination, (int code));
extern void EXFUN (compiler_reset_error, (void));

extern long
  compiler_interface_version,
  compiler_processor_type;

extern SCHEME_OBJECT
  compiler_utilities,
  return_to_interpreter;

extern long
  EXFUN (enter_compiled_expression, (void)),
  EXFUN (apply_compiled_procedure, (void)),
  EXFUN (return_to_compiled_code, (void)),
  EXFUN (make_fake_uuo_link,
	 (SCHEME_OBJECT extension, SCHEME_OBJECT block, long offset)),
  EXFUN (make_uuo_link,
	 (SCHEME_OBJECT value, SCHEME_OBJECT extension,
	  SCHEME_OBJECT block, long offset)),
  EXFUN (compiled_block_closure_p, (SCHEME_OBJECT block)),
  EXFUN (compiled_entry_closure_p, (SCHEME_OBJECT entry)),
  EXFUN (compiled_entry_to_block_offset, (SCHEME_OBJECT entry)),
  EXFUN (coerce_to_compiled,
	 (SCHEME_OBJECT object, long arity, SCHEME_OBJECT *location));

extern SCHEME_OBJECT
  EXFUN (extract_uuo_link, (SCHEME_OBJECT block, long offset)),
  EXFUN (extract_variable_cache,
	 (SCHEME_OBJECT extension, long offset)),
  EXFUN (compiled_block_debugging_info, (SCHEME_OBJECT block)),
  EXFUN (compiled_block_environment, (SCHEME_OBJECT block)),
  EXFUN (compiled_closure_to_entry, (SCHEME_OBJECT entry)),
  * EXFUN (compiled_entry_to_block_address, (SCHEME_OBJECT entry)),
  EXFUN (compiled_entry_to_block, (SCHEME_OBJECT entry)),
  EXFUN (apply_compiled_from_primitive, (int)),
  EXFUN (compiled_with_interrupt_mask, (unsigned long,
					SCHEME_OBJECT,
					unsigned long)),
  EXFUN (compiled_with_stack_marker, (SCHEME_OBJECT)),
  * EXFUN (cons_c_code_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *));

extern Boolean
  EXFUN (install_c_code_table, (SCHEME_OBJECT *, long));

extern void
  EXFUN (compiler_reset, (SCHEME_OBJECT new_block)),
  EXFUN (compiler_initialize, (long fasl_p)),
  EXFUN (store_variable_cache,
	 (SCHEME_OBJECT extension, SCHEME_OBJECT block, long offset)),
  EXFUN (compiled_entry_type, (SCHEME_OBJECT entry, long *buffer)),
  EXFUN (declare_compiled_code_block, (SCHEME_OBJECT block));

/* Breakpoint stuff. */

extern SCHEME_OBJECT EXFUN (bkpt_install, (PTR));
extern SCHEME_OBJECT EXFUN (bkpt_closure_install, (PTR));
extern Boolean EXFUN (bkpt_p, (PTR));
extern SCHEME_OBJECT EXFUN (bkpt_proceed, (PTR, SCHEME_OBJECT, SCHEME_OBJECT));
extern void EXFUN (bkpt_remove, (PTR, SCHEME_OBJECT));

SCHEME_OBJECT
#ifndef __WIN32__
  Registers [REGBLOCK_MINIMUM_LENGTH],
#endif
  compiler_utilities,
  return_to_interpreter;

long
  compiler_interface_version,
  compiler_processor_type;

long
DEFUN_VOID (enter_compiled_expression)
{
  return (ERR_EXECUTE_MANIFEST_VECTOR);
}

long
DEFUN_VOID (apply_compiled_procedure)
{
  return (ERR_INAPPLICABLE_OBJECT);
}

long
DEFUN_VOID (return_to_compiled_code)
{
  return (ERR_INAPPLICABLE_CONTINUATION);
}

SCHEME_OBJECT
DEFUN (apply_compiled_from_primitive, (arity), int arity)
{
  signal_error_from_primitive (ERR_INAPPLICABLE_CONTINUATION);
  /*NOTREACHED*/
}

SCHEME_OBJECT
DEFUN (compiled_with_interrupt_mask, (old_mask, receiver, new_mask),
       unsigned long old_mask
       AND SCHEME_OBJECT receiver
       AND unsigned long new_mask)
{
  signal_error_from_primitive (ERR_INAPPLICABLE_CONTINUATION);
  /*NOTREACHED*/
}

SCHEME_OBJECT
DEFUN (compiled_with_stack_marker, (thunk), SCHEME_OBJECT thunk)
{
  signal_error_from_primitive (ERR_INAPPLICABLE_CONTINUATION);
  /*NOTREACHED*/
}

SCHEME_OBJECT *
DEFUN (cons_c_code_table, (start, limit, length),
       SCHEME_OBJECT * start
       AND SCHEME_OBJECT * limit
       AND long * length)
{
  * length = 0;
  return (start);
}

Boolean
DEFUN (install_c_code_table, (table, length),
       SCHEME_OBJECT * table AND long length)
{
  return (length == 0);
}

/* Bad entry points. */

long
DEFUN (make_fake_uuo_link,
       (extension, block, offset),
       SCHEME_OBJECT extension AND SCHEME_OBJECT block AND
       long offset)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
DEFUN (make_uuo_link,
       (value, extension, block, offset),
       SCHEME_OBJECT value AND SCHEME_OBJECT extension AND
       SCHEME_OBJECT block AND long offset)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

SCHEME_OBJECT
DEFUN (extract_uuo_link,
       (block, offset),
       SCHEME_OBJECT block AND long offset)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

void
DEFUN (store_variable_cache,
       (extension, block, offset),
       SCHEME_OBJECT extension AND SCHEME_OBJECT block AND
       long offset)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

SCHEME_OBJECT
DEFUN (extract_variable_cache,
       (block, offset),
       SCHEME_OBJECT block AND
       long offset)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

SCHEME_OBJECT
DEFUN (compiled_block_debugging_info,
       (block),
       SCHEME_OBJECT block)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

SCHEME_OBJECT
DEFUN (compiled_block_environment,
       (block),
       SCHEME_OBJECT block)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
DEFUN (compiled_block_closure_p,
       (block),
       SCHEME_OBJECT block)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

SCHEME_OBJECT *
DEFUN (compiled_entry_to_block_address,
       (entry),
       SCHEME_OBJECT entry)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
DEFUN (compiled_entry_to_block_offset,
       (entry),
       SCHEME_OBJECT entry)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

SCHEME_OBJECT
DEFUN (compiled_entry_to_block,
       (entry),
       SCHEME_OBJECT entry)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}


void
DEFUN (compiled_entry_type,
       (entry, buffer),
       SCHEME_OBJECT entry AND long *buffer)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

long
DEFUN (compiled_entry_closure_p,
       (entry),
       SCHEME_OBJECT entry)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

SCHEME_OBJECT
DEFUN (compiled_closure_to_entry, (entry), SCHEME_OBJECT entry)
{
  Microcode_Termination (TERM_COMPILER_DEATH);
  /*NOTREACHED*/
}

void
DEFUN (declare_compiled_code_block, (block), SCHEME_OBJECT block)
{
  return;
}

#define LOSING_RETURN_ADDRESS(name)					\
extern long EXFUN (name, (void));					\
long									\
DEFUN_VOID (name)							\
{									\
  Microcode_Termination (TERM_COMPILER_DEATH);				\
  /*NOTREACHED*/							\
}

LOSING_RETURN_ADDRESS (comp_interrupt_restart)
LOSING_RETURN_ADDRESS (comp_lookup_apply_restart)
LOSING_RETURN_ADDRESS (comp_reference_restart)
LOSING_RETURN_ADDRESS (comp_access_restart)
LOSING_RETURN_ADDRESS (comp_unassigned_p_restart)
LOSING_RETURN_ADDRESS (comp_unbound_p_restart)
LOSING_RETURN_ADDRESS (comp_assignment_restart)
LOSING_RETURN_ADDRESS (comp_definition_restart)
LOSING_RETURN_ADDRESS (comp_safe_reference_restart)
LOSING_RETURN_ADDRESS (comp_lookup_trap_restart)
LOSING_RETURN_ADDRESS (comp_assignment_trap_restart)
LOSING_RETURN_ADDRESS (comp_op_lookup_trap_restart)
LOSING_RETURN_ADDRESS (comp_cache_lookup_apply_restart)
LOSING_RETURN_ADDRESS (comp_safe_lookup_trap_restart)
LOSING_RETURN_ADDRESS (comp_unassigned_p_trap_restart)
LOSING_RETURN_ADDRESS (comp_link_caches_restart)
LOSING_RETURN_ADDRESS (comp_error_restart)

/* NOP entry points */

void
DEFUN (compiler_reset, (new_block), SCHEME_OBJECT new_block)
{
  extern void EXFUN (compiler_reset_error, (void));

  if (new_block != SHARP_F)
    compiler_reset_error ();
  return;
}

void
DEFUN (compiler_initialize, (fasl_p), long fasl_p)
{
  (Registers[REGBLOCK_PRIMITIVE]) = SHARP_F;
  compiler_processor_type = 0;
  compiler_interface_version = 0;
  compiler_utilities = SHARP_F;
  return_to_interpreter =
    (MAKE_OBJECT (TC_RETURN_CODE, RC_POP_FROM_COMPILED_CODE));
  return;
}

/* Identity procedure */

long
DEFUN (coerce_to_compiled,
       (object, arity, location),
       SCHEME_OBJECT object AND long arity AND SCHEME_OBJECT *location)
{
  *location = object;
  return (PRIM_DONE);
}

extern char * EXFUN (utility_index_to_name, (int));
extern void EXFUN (declare_builtin, (unsigned long));
extern char * EXFUN (builtin_index_to_name, (int));
extern int EXFUN (pc_to_utility_index, (unsigned long));
extern int EXFUN (pc_to_builtin_index, (unsigned long));

char *
DEFUN (utility_index_to_name, (index), int index)
{
  return ((char *) NULL);
}

void
DEFUN (declare_builtin, (builtin), unsigned long builtin)
{
  return;
}

char *
DEFUN (builtin_index_to_name, (index), int index)
{
  return ((char *) NULL);
}

int
DEFUN (pc_to_utility_index, (pc), unsigned long pc)
{
  return (-1);
}

int
DEFUN (pc_to_builtin_index, (pc), unsigned long pc)
{
  return (-1);
}

SCHEME_OBJECT
DEFUN (bkpt_install, (ep), PTR ep)
{
  return (SHARP_F);
}

SCHEME_OBJECT
DEFUN (bkpt_closure_install, (ep), PTR ep)
{
  return (SHARP_F);
}

void
DEFUN (bkpt_remove, (ep, handle), PTR ep AND SCHEME_OBJECT handle)
{
  error_external_return ();
}

Boolean
DEFUN (bkpt_p, (ep), PTR ep)
{
  return (SHARP_F);
}

SCHEME_OBJECT
DEFUN (bkpt_proceed, (ep, handle, state), 
       PTR ep AND SCHEME_OBJECT handle AND SCHEME_OBJECT state)
{
  error_external_return ();
}

#endif	/* HAS_COMPILER_SUPPORT */

#ifdef __WIN32__
#include "ntscmlib.h"

extern unsigned long * win32_catatonia_block;
extern void EXFUN (win32_allocate_registers, (void));
extern void EXFUN (win32_allocate_registers, (void));

#ifndef REGBLOCK_LENGTH
#  define REGBLOCK_LENGTH REGBLOCK_MINIMUM_LENGTH
#endif

typedef struct register_storage
{
  /* The following must be allocated consecutively */
  unsigned long catatonia_block[3];
#if (COMPILER_PROCESSOR_TYPE == COMPILER_IA32_TYPE)
  void * Regstart[32];	/* Negative byte offsets from &Registers[0] */
#endif
  SCHEME_OBJECT Registers [REGBLOCK_LENGTH];
} REGMEM;

SCHEME_OBJECT * RegistersPtr = ((SCHEME_OBJECT *) NULL);
unsigned long * win32_catatonia_block = ((unsigned long *) NULL);
static REGMEM regmem;

void
DEFUN_VOID (win32_allocate_registers)
{
  REGMEM * mem = & regmem;

  win32_catatonia_block = ((unsigned long *) &mem->catatonia_block[0]);
  RegistersPtr = mem->Registers;
  if (! (win32_system_utilities.lock_memory_area (mem, (sizeof (REGMEM)))))
  {
    outf_error ("Unable to lock registers\n");
    outf_flush_error ();
  }
  return;
}

void
DEFUN_VOID (win32_deallocate_registers)
{
  win32_system_utilities.unlock_memory_area (&regmem, (sizeof (REGMEM)));
  return;
}

#endif /* __WIN32__ */
