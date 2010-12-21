/* -*-C-*-

Copyright (C) 2006, 2007, 2008, 2009, 2010 Matthew Birkholz

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

/* Un*x primitives for an FFI. */

#include "scheme.h"
#include "prims.h"
#include "bignmint.h"
#include "history.h"
#include "pruxffi.h"
/* Using SCM instead of SCHEME_OBJECT here, hoping to ensure that
   these types always match. */

/* Alien Addresses */

#define HALF_WORD_SHIFT ((sizeof (void *) * CHAR_BIT) / 2UL)
#define HALF_WORD_MASK ((1UL << HALF_WORD_SHIFT) - 1UL)
#define ARG_RECORD(argument_number)					\
  ((RECORD_P (ARG_REF (argument_number)))				\
   ? (ARG_REF (argument_number))					\
   : ((error_wrong_type_arg (argument_number)), 0))

int
is_alien (SCM alien)
{
  if ((RECORD_P (alien)) && ((VECTOR_LENGTH (alien)) == 4))
    {
      SCM high = (VECTOR_REF (alien, 1));
      SCM low  = (VECTOR_REF (alien, 2));
      if ((UNSIGNED_FIXNUM_P (high)) && (UNSIGNED_FIXNUM_P (low)))
	return (1);
    }
  return (0);
}

void *
alien_address (SCM alien)
{
  unsigned long high = (FIXNUM_TO_ULONG (VECTOR_REF (alien, 1)));
  unsigned long low = (FIXNUM_TO_ULONG (VECTOR_REF (alien, 2)));
  return ((void *) ((high << HALF_WORD_SHIFT) + low));
}

void
set_alien_address (SCM alien, const void * ptr)
{
  unsigned long addr = ((unsigned long) ptr);
  VECTOR_SET (alien, 1, (ULONG_TO_FIXNUM (addr >> HALF_WORD_SHIFT)));
  VECTOR_SET (alien, 2, (ULONG_TO_FIXNUM (addr & HALF_WORD_MASK)));
}

SCM
arg_alien (int argn)
{
  SCM alien = (ARG_REF (argn));
  if (is_alien (alien))
    return (alien);
  error_wrong_type_arg (argn);
  return (0);
}

void *
arg_address (int argn)
{
  SCM alien = ARG_REF (argn);
  if (is_alien (alien))
    return (alien_address (alien));
  error_wrong_type_arg (argn);
  return (0);
}

#define ALIEN_ADDRESS_LOC(type)						\
  ((type *)(((char *) (arg_address (1))) + (UNSIGNED_FIXNUM_ARG (2))))

#define ALIEN_ADDRESS_REF(type) (* (ALIEN_ADDRESS_LOC (type)))

#define ALIEN_ADDRESS_SET(type, value) do				\
{									\
  (* (ALIEN_ADDRESS_LOC (type))) = (value);				\
} while (0)

#define C_PEEKER(type_to_object, type)					\
{									\
  PRIMITIVE_HEADER (2);							\
  PRIMITIVE_RETURN (type_to_object (ALIEN_ADDRESS_REF (type)));		\
}

/* Peek the Basic Types */

DEFINE_PRIMITIVE ("C-PEEK-CHAR", Prim_peek_char, 2, 2, 0)
  C_PEEKER (LONG_TO_FIXNUM, char)

DEFINE_PRIMITIVE ("C-PEEK-UCHAR", Prim_peek_uchar, 2, 2, 0)
  C_PEEKER (LONG_TO_FIXNUM, unsigned char)

DEFINE_PRIMITIVE ("C-PEEK-SHORT", Prim_peek_short, 2, 2, 0)
  C_PEEKER (LONG_TO_FIXNUM, short)

DEFINE_PRIMITIVE ("C-PEEK-USHORT", Prim_peek_ushort, 2, 2, 0)
  C_PEEKER (LONG_TO_FIXNUM, unsigned short)

DEFINE_PRIMITIVE ("C-PEEK-INT", Prim_peek_int, 2, 2, 0)
  C_PEEKER (long_to_integer, int)

DEFINE_PRIMITIVE ("C-PEEK-UINT", Prim_peek_uint, 2, 2, 0)
  C_PEEKER (ulong_to_integer, unsigned int)

DEFINE_PRIMITIVE ("C-PEEK-LONG", Prim_peek_long, 2, 2, 0)
  C_PEEKER (long_to_integer, long)

DEFINE_PRIMITIVE ("C-PEEK-ULONG", Prim_peek_ulong, 2, 2, 0)
  C_PEEKER (ulong_to_integer, unsigned long)

DEFINE_PRIMITIVE ("C-PEEK-FLOAT", Prim_peek_float, 2, 2, 0)
  C_PEEKER (double_to_flonum, float)

DEFINE_PRIMITIVE ("C-PEEK-DOUBLE", Prim_peek_double, 2, 2, 0)
  C_PEEKER (double_to_flonum, double)

DEFINE_PRIMITIVE ("C-PEEK-POINTER", Prim_peek_pointer, 3, 3, 0)
{
  /* Read the pointer at ALIEN+OFFSET and set ALIEN2 (perhaps the
     same as ALIEN) to point to the same address. */

  PRIMITIVE_HEADER (3);
  {
    SCM alien = (ARG_RECORD (3));
    set_alien_address (alien, (ALIEN_ADDRESS_REF (void *)));
    PRIMITIVE_RETURN (alien);
  }
}

DEFINE_PRIMITIVE ("C-PEEK-CSTRING", Prim_peek_cstring, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (char_pointer_to_string (ALIEN_ADDRESS_LOC (char)));
}

DEFINE_PRIMITIVE ("C-PEEK-CSTRING!", Prim_peek_cstring_bang, 2, 2, 0)
{
  /* Return a Scheme string containing the characters in a C string
     that starts at the address ALIEN+OFFSET.  Set ALIEN to the
     address of the C char after the string's null terminator. */

  PRIMITIVE_HEADER (2);
  {
    char * ptr = (ALIEN_ADDRESS_LOC (char));
    SCM string = (char_pointer_to_string (ptr));
    set_alien_address ((ARG_REF (1)), (ptr + strlen (ptr) + 1));
    PRIMITIVE_RETURN (string);
  }
}

DEFINE_PRIMITIVE ("C-PEEK-CSTRINGP", Prim_peek_cstringp, 2, 2, 0)
  C_PEEKER (char_pointer_to_string, char *)

DEFINE_PRIMITIVE ("C-PEEK-CSTRINGP!", Prim_peek_cstringp_bang, 2, 2, 0)
{
  /* Follow the pointer at the address ALIEN+OFFSET to a C string.
     Set ALIEN to the address of the char pointer after ALIEN+OFFSET.
     Copy the C string into the heap and return the new Scheme
     string. */

  PRIMITIVE_HEADER (2);
  {
    char ** ptr = (ALIEN_ADDRESS_LOC (char *));
    SCM string = char_pointer_to_string (*ptr);
    set_alien_address ((ARG_REF (1)), (ptr + 1)); /* No more aborts! */
    PRIMITIVE_RETURN (string);
  }
}

#define C_POKER(type, value_arg_ref)					\
{									\
  PRIMITIVE_HEADER (3);							\
  ALIEN_ADDRESS_SET (type, (value_arg_ref (3)));			\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

/* Poke the Basic Types */

DEFINE_PRIMITIVE ("C-POKE-CHAR", Prim_poke_char, 3, 3, 0)
  C_POKER (char, arg_integer)

DEFINE_PRIMITIVE ("C-POKE-UCHAR", Prim_poke_uchar, 3, 3, 0)
  C_POKER (unsigned char, arg_integer)

DEFINE_PRIMITIVE ("C-POKE-SHORT", Prim_poke_short, 3, 3, 0)
  C_POKER (short, arg_integer)

DEFINE_PRIMITIVE ("C-POKE-USHORT", Prim_poke_ushort, 3, 3, 0)
  C_POKER (unsigned short, arg_integer)

DEFINE_PRIMITIVE ("C-POKE-INT", Prim_poke_int, 3, 3, 0)
  C_POKER (int, arg_integer)

DEFINE_PRIMITIVE ("C-POKE-UINT", Prim_poke_uint, 3, 3, 0)
  C_POKER (unsigned int, arg_integer)

DEFINE_PRIMITIVE ("C-POKE-LONG", Prim_poke_long, 3, 3, 0)
  C_POKER (long, arg_integer)

DEFINE_PRIMITIVE ("C-POKE-ULONG", Prim_poke_ulong, 3, 3, 0)
  C_POKER (unsigned long, arg_ulong_integer)

DEFINE_PRIMITIVE ("C-POKE-FLOAT", Prim_poke_float, 3, 3, 0)
  C_POKER (float, arg_real_number)

DEFINE_PRIMITIVE ("C-POKE-DOUBLE", Prim_poke_double, 3, 3, 0)
  C_POKER (double, arg_real_number)

DEFINE_PRIMITIVE ("C-POKE-POINTER", Prim_poke_pointer, 3, 3, 0)
  C_POKER (void *, arg_pointer)

DEFINE_PRIMITIVE ("C-POKE-POINTER!", Prim_poke_pointer_bang, 3, 3, 0)
{
  /* Set the pointer at address ALIEN+OFFSET to ADDRESS (an alien,
     string, xstring or 0 for NULL).  Set ALIEN to the address of the
     pointer after ALIEN+OFFSET. */

  PRIMITIVE_HEADER (3);
  {
    void ** ptr = (ALIEN_ADDRESS_LOC (void *));
    (*ptr) = (arg_pointer (3));
    set_alien_address ((ARG_REF (1)), (ptr + 1));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("C-POKE-STRING", Prim_poke_string, 3, 3, 0)
{
  /* Copy into the C string at address ALIEN+OFFSET the Scheme STRING.
     Assume STRING fits.  Null terminate the C string. */

  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, STRING_P);
  {
    SCM string = (ARG_REF (3));
    strncpy ((ALIEN_ADDRESS_LOC (char)),
	     (STRING_POINTER (string)),
	     ((STRING_LENGTH (string)) + 1));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("C-POKE-STRING!", Prim_poke_string_bang, 3, 3, 0)
{
  /* Copy into the C string at address ALIEN+OFFSET the Scheme STRING.
     Assume STRING fits.  Null terminate the C string.  Set ALIEN to
     the address of the C char following the NULL terminator. */

  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, STRING_P);
  {
    char * ptr = (ALIEN_ADDRESS_LOC (char));
    SCM string = (ARG_REF (3));
    unsigned long n_chars = ((STRING_LENGTH (string)) + 1);
    strncpy (ptr, (STRING_POINTER (string)), n_chars);
    set_alien_address ((ARG_REF (1)), (ptr + n_chars));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Malloc/Free. */

DEFINE_PRIMITIVE ("C-MALLOC", Prim_c_malloc, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  set_alien_address ((arg_alien (1)), (malloc (arg_ulong_integer (2))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("C-FREE", Prim_c_free, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    void * addr = (arg_address (1));
    if (addr != NULL)
      free (addr);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* The CStack */

char *
cstack_top (void)
{
  return (ffi_obstack.next_free);
}

void
cstack_push (void * addr, int bytes)
{
  obstack_grow ((&ffi_obstack), addr, bytes);
}

char *
cstack_lpop (char * tos, int bytes)
{
  tos = tos - bytes;
  if (tos < ffi_obstack.object_base)
    {
      outf_error ("\ninternal error: C stack exhausted\n");
      outf_error ("\tCould not pop %d bytes.\n", bytes);
      outf_flush_error ();
      signal_error_from_primitive (ERR_EXTERNAL_RETURN);
    }
  return (tos);
}

void
cstack_pop (char * tos)
{
  if (tos < ffi_obstack.object_base)
    {
      outf_error ("\ninternal error: C stack over-popped.\n");
      outf_flush_error ();
      signal_error_from_primitive (ERR_EXTERNAL_RETURN);
    }
  (&ffi_obstack)->next_free = tos;
}

/* Number CStack frames, to detect slips. */
int cstack_depth = 0;

/* Callouts */

DEFINE_PRIMITIVE ("C-CALL", Prim_c_call, 1, LEXPR, 0)
{
  /* All the smarts are in the trampolines. */

  PRIMITIVE_HEADER (LEXPR);
  canonicalize_primitive_context ();
  {
    CalloutTrampOut tramp;

    tramp = (CalloutTrampOut) arg_alien_entry (1);
    tramp ();
    /* NOTREACHED */
    outf_error ("\ninternal error: Callout part1 trampoline returned.\n");
    outf_flush_error ();
    signal_error_from_primitive (ERR_EXTERNAL_RETURN);
    /* really NOTREACHED */
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

static SCM c_call_continue = SHARP_F;

void
callout_seal (CalloutTrampIn tramp)
{
  /* Used in a callout part1 trampoline.  Arrange for subsequent
     aborts to start part2.

     Seal the CStack, substitute the C-CALL-CONTINUE primitive for
     the C-CALL primitive, and back out.  The tramp can then execute
     the toolkit function safely, even if there is a callback. */

  if (c_call_continue == SHARP_F)
    {
      c_call_continue
	= find_primitive_cname ("C-CALL-CONTINUE",
				false, false, LEXPR_PRIMITIVE_ARITY);
      if (c_call_continue == SHARP_F)
	{
	  outf_error ("\nNo C-CALL-CONTINUE primitive!\n");
	  outf_flush_error ();
	  signal_error_from_primitive (ERR_EXTERNAL_RETURN);
	}
    }
  cstack_depth += 1;
  CSTACK_PUSH (int, cstack_depth);
  CSTACK_PUSH (CalloutTrampIn, tramp);

  /* Back out of C-CALL-CONTINUE. */
  SET_PRIMITIVE (c_call_continue);
  back_out_of_primitive ();
  /* Ready for Interpret(1). */
}

void
callout_unseal (CalloutTrampIn expected)
{
  /* Used by a callout part1 trampoline to strip the CStack's frame
     header (tramp, depth) before pushing return values. */

  char * tos;
  CalloutTrampIn found;
  int depth;

  tos = cstack_top ();
  CSTACK_LPOP (CalloutTrampIn, found, tos);
  CSTACK_LPOP (int, depth, tos);
  if (found != expected || depth != cstack_depth)
    {
      outf_error ("\ninternal error: slipped in 1st part of callout\n");
      outf_flush_error ();
      signal_error_from_primitive (ERR_EXTERNAL_RETURN);
    }
  cstack_pop (tos);
}

void
callout_continue (CalloutTrampIn tramp)
{
  /* Re-seal the CStack frame over the C results (again, pushing the
     cstack_depth and callout-part2) and abort.  Restart as
     C-CALL-CONTINUE and run callout-part2. */

  CSTACK_PUSH (int, cstack_depth);
  CSTACK_PUSH (CalloutTrampIn, tramp);

  PRIMITIVE_ABORT (PRIM_POP_RETURN);
  /* NOTREACHED */
}

DEFINE_PRIMITIVE ("C-CALL-CONTINUE", Prim_c_call_continue, 1, LEXPR, 0)
{
  /* (Re)Run the callout trampoline part 2 (CalloutTrampIn). */

  PRIMITIVE_HEADER (LEXPR);
  {
    char * tos;
    CalloutTrampIn tramp;
    int depth;
    SCM val;

    tos = cstack_top ();
    CSTACK_LPOP (CalloutTrampIn, tramp, tos);
    CSTACK_LPOP (int, depth, tos);
    if (depth != cstack_depth)
      {
	outf_error ("\ninternal error: slipped in 2nd part of callout\n");
	outf_flush_error ();
	signal_error_from_primitive (ERR_EXTERNAL_RETURN);
      }
    val = tramp ();
    PRIMITIVE_RETURN (val);
  }
}

char *
callout_lunseal (CalloutTrampIn expected)
{
  /* Used by a callout part2 trampoline to strip the CStack's frame
     header (tramp, depth) before lpopping return value(s). */

  char * tos;
  CalloutTrampIn found;
  int depth;

  tos = cstack_top ();
  CSTACK_LPOP (CalloutTrampIn, found, tos);
  CSTACK_LPOP (int, depth, tos);
  if (depth != cstack_depth || found != expected)
    {
      outf_error ("\ninternal error: slipped in 1st part of callout\n");
      outf_flush_error ();
      signal_error_from_primitive (ERR_EXTERNAL_RETURN);
    }
  return (tos);
}

void
callout_pop (char * tos)
{
  /* Used by a callout part2 trampoline just before returning. */

  cstack_depth -= 1;
  cstack_pop (tos);
}

/* Callbacks */

static SCM run_callback = SHARP_F;
static SCM return_to_c = SHARP_F;

void
callback_run_kernel (long callback_id, CallbackKernel kernel)
{
  /* Used by callback trampolines.

     Expect the args on the CStack.  Push a couple primitive apply
     frames on the Scheme stack and seal the CStack.  Then call
     Interpret().  Cannot abort. */

  if (run_callback == SHARP_F)
    {
      run_callback = find_primitive_cname ("RUN-CALLBACK", false, false, 0);
      return_to_c = find_primitive_cname ("RETURN-TO-C", false, false, 0);
      if (run_callback == SHARP_F || return_to_c == SHARP_F)
	{
	  outf_error
	    ("\nWarning: punted callback #%ld.  Missing primitives!\n",
	     callback_id);
	  outf_flush_error ();
	  SET_VAL (FIXNUM_ZERO);
	  return;
	}
    }

  /* Need to push 2 each of prim+header+continuation. */
  if (! CAN_PUSH_P (2 * (1 + 1 + CONTINUATION_SIZE)))
    {
      outf_error
	("\nWarning: punted callback #%ld.  No room on stack!\n", callback_id);
      outf_flush_error ();
      SET_VAL (FIXNUM_ZERO);
      return;
    }

  cstack_depth += 1;
  CSTACK_PUSH (int, cstack_depth);
  CSTACK_PUSH (CallbackKernel, kernel);

  STACK_PUSH (return_to_c);
  PUSH_APPLY_FRAME_HEADER (0);
  SET_RC (RC_INTERNAL_APPLY);
  SAVE_CONT();
  STACK_PUSH (run_callback);
  PUSH_APPLY_FRAME_HEADER (0);
  SAVE_CONT();
  Interpret (1);
  cstack_depth -= 1;
}

DEFINE_PRIMITIVE ("RUN-CALLBACK", Prim_run_callback, 0, 0, 0)
{
  /* All the smarts are in the kernel. */

  PRIMITIVE_HEADER (0);
  { 
    char * tos;
    CallbackKernel kernel;
    int depth;

    tos = cstack_top ();
    CSTACK_LPOP (CallbackKernel, kernel, tos);
    CSTACK_LPOP (int, depth, tos);
    if (depth != cstack_depth)
      {
	outf_error ("\nWarning: C data stack slipped in run-callback!\n");
	outf_flush_error ();
	signal_error_from_primitive (ERR_EXTERNAL_RETURN);
      }

    kernel ();
    /* NOTREACHED */
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("RETURN-TO-C", Prim_return_to_c, 0, 0, 0)
{
  /* Callbacks are possible while stopped.  The PRIM_RETURN_TO_C abort
     expects this primitive to clean up its stack frame. */

  PRIMITIVE_HEADER (0);
  canonicalize_primitive_context ();
  {
    SCM primitive;
    long nargs;

    primitive = GET_PRIMITIVE;
    assert (PRIMITIVE_P (primitive));
    nargs = (PRIMITIVE_N_ARGUMENTS (primitive));
    POP_PRIMITIVE_FRAME (nargs);
    SET_EXP (SHARP_F);
    PRIMITIVE_ABORT (PRIM_RETURN_TO_C);
    /* NOTREACHED */
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

char *
callback_lunseal (CallbackKernel expected)
{
  /* Used by a callback kernel to strip the CStack's frame header
     (kernel, depth) before lpopping arguments. */

  char * tos;
  CallbackKernel found;
  int depth;

  tos = cstack_top ();
  CSTACK_LPOP (CallbackKernel, found, tos);
  CSTACK_LPOP (int, depth, tos);
  if (depth != cstack_depth || found != expected)
    {
      outf_error ("\ninternal error: slipped in callback kernel\n");
      outf_flush_error ();
      signal_error_from_primitive (ERR_EXTERNAL_RETURN);
    }
  return (tos);
}

static SCM valid_callback_handler (void);
static SCM valid_callback_id (long id);

void
callback_run_handler (long callback_id, SCM arglist)
{
  /* Used by callback kernels, inside the interpreter.  Thus it MAY GC
     abort.

     Push a Scheme callback handler apply frame.  This leaves the
     interpreter ready to tail-call the Scheme procedure.  (The
     RUN-CALLBACK primitive apply frame is already gone.)  The
     trampoline should abort with PRIM_APPLY. */

  SCM handler, fixnum_id;

  handler = valid_callback_handler ();
  fixnum_id = valid_callback_id (callback_id);

  stop_history ();

  Will_Push (STACK_ENV_EXTRA_SLOTS + 3);
    STACK_PUSH (arglist);
    STACK_PUSH (fixnum_id);
    STACK_PUSH (handler);
    PUSH_APPLY_FRAME_HEADER (2);
  Pushed ();
}

static SCM
valid_callback_handler (void)
{
  /* Validate the Scheme callback handler procedure. */

  SCM handler;

  handler = (VECTOR_REF (fixed_objects, CALLBACK_HANDLER));
  if (! interpreter_applicable_p (handler))
    {
      outf_error ("\nWarning: bogus callback handler: 0x%x.\n",
		  ((unsigned int) handler));
      outf_flush_error ();
      Do_Micro_Error (ERR_INAPPLICABLE_OBJECT, true);
      abort_to_interpreter (PRIM_APPLY);
      /* NOTREACHED */
    }
  return (handler);
}

static SCM
valid_callback_id (long id)
{
  /* Validate the callback ID and convert to a fixnum. */

  if (ULONG_TO_FIXNUM_P (id))
    return (ULONG_TO_FIXNUM (id));
  signal_error_from_primitive (ERR_ARG_1_BAD_RANGE);
  /* NOTREACHED */
  return (FIXNUM_ZERO);
}

void
callback_return (char * tos)
{
  cstack_pop (tos);
  PRIMITIVE_ABORT (PRIM_APPLY);
}

/* Converters */

long
arg_long (int argn)
{
  return (arg_integer (argn));
}

unsigned long
arg_ulong (int argn)
{
  return (arg_ulong_integer (argn));
}

double
arg_double (int argn)
{
  /* Convert the object to a double.  Like arg_real_number. */

  return (arg_real_number (argn));
}

void *
arg_alien_entry (int argn)
{
  /* Expect an alien-function.  Return its address. */

  SCM alienf = VECTOR_ARG (argn);
  int length = VECTOR_LENGTH (alienf);
  if (length < 3)
    error_wrong_type_arg (argn);
  return (alien_address (alienf));
}

void *
arg_pointer (int argn)
{
  /* Accept an alien, string, xstring handle (positive integer),
     or zero (for a NULL pointer). */

  SCM arg = ARG_REF (argn);
  if (integer_zero_p (arg))
    return ((void *)0);
  if (STRING_P (arg))
    return ((void *) (STRING_POINTER (arg)));
  if ((INTEGER_P (arg)) && (integer_to_ulong_p (arg)))
    {
      unsigned char * result = lookup_external_string (arg, NULL);
      if (result == 0)
	error_wrong_type_arg (argn);
      return ((void *) result);
    }
  if (is_alien (arg))
    return (alien_address (arg));

  error_wrong_type_arg (argn);
  /*NOTREACHED*/
  return ((void *)0);
}

SCM
long_to_scm (const long i)
{
  return (long_to_integer (i));
}

SCM
ulong_to_scm (const unsigned long i)
{
  return (ulong_to_integer (i));
}

SCM
double_to_scm (const double d)
{
  return (double_to_flonum (d));
}

SCM
pointer_to_scm (const void * p)
{
  /* Return a pointer from a callout.  Expect the first real argument
     (the 2nd) to be either #F or an alien. */

  SCM arg = ARG_REF (2);
  if (arg == SHARP_F)
    return (UNSPECIFIC);
  if (is_alien (arg))
    {
      set_alien_address (arg, p);
      return (arg);
    }

  error_wrong_type_arg (2);
  /* NOTREACHED */
  return (SHARP_F);
}

SCM
cons_alien (const void * addr)
{
  /* Construct an alien.  Used by callback kernels to construct
     arguments for the Scheme callback-handler, or part2 of callouts
     returning a new alien.  Note that these should be fixed up on the
     Scheme side with the record type. */

  SCM alien;
  Primitive_GC_If_Needed (5);
  alien = (MAKE_POINTER_OBJECT (TC_RECORD, Free));
  (*Free++) = MAKE_OBJECT (TC_MANIFEST_VECTOR, 4);
  (*Free++) = SHARP_F;
  (*Free++) = FIXNUM_ZERO;
  (*Free++) = FIXNUM_ZERO;
  (*Free++) = SHARP_F;
  set_alien_address (alien, addr);
  return (alien);
}

long
long_value (void)
{
  /* Convert VAL to a long.  Accept integers AND characters.  Like
     arg_integer otherwise. */

  SCM value = GET_VAL;
  if (CHARACTER_P (value))
    return (CHAR_TO_ASCII (value));
  if (! (INTEGER_P (value)))
    {
      /* error_wrong_type_arg (1); Not inside the interpreter here. */
      outf_error ("\nWarning: Callback did not return an integer!\n");
      outf_flush_error ();
      return (0);
    }
  if (! (integer_to_long_p (value)))
    {
      /* error_bad_range_arg (1); */
      outf_error
	("\nWarning: Callback returned an integer larger than a C long!\n");
      outf_flush_error ();
      return (0);
    }
  return (integer_to_long (value));
}

unsigned long
ulong_value (void)
{
  /* Convert VAL to an unsigned long.  Accept integers AND characters.
     Like arg_integer otherwise. */

  SCM value = GET_VAL;
  if (CHARACTER_P (value))
    return (CHAR_TO_ASCII (value));
  if (! (INTEGER_P (value)))
    {
      /* error_wrong_type_arg (1); Not inside the interpreter here. */
      outf_error ("\nWarning: Callback did not return an integer!\n");
      outf_flush_error ();
      return (0);
    }
  if (! (integer_to_ulong_p (value)))
    {
      /* error_bad_range_arg (1); */
      outf_error
	("\nWarning: "
	 "Callback returned an integer larger than a C unsigned long!\n");
      outf_flush_error ();
      return (0);
    }
  return (integer_to_ulong (value));
}

double
double_value (void)
{
  /* Convert VAL to a double.  Like arg_real_number. */

  SCM value = GET_VAL;

  if (! REAL_P (value))
    {
      /* error_wrong_type_arg (1); Not inside the interpreter here. */
      outf_error ("\nWarning: Callback did not return a real.\n");
      outf_flush_error ();
      return (0.0);
    }
  if (! (real_number_to_double_p (value)))
    {
      /* error_bad_range_arg (1); */
      outf_error
	("\nWarning: Callback returned a real larger than a C double!\n");
      outf_flush_error ();
      return (0.0);
    }
  return (real_number_to_double (value));
}

void *
pointer_value (void)
{
  SCM value = GET_VAL;

  if (integer_zero_p (value))
    return (NULL);
  /* NOT allowing a Scheme string (heap pointer!) into the toolkit. */
  if ((INTEGER_P (value)) && (integer_to_ulong_p (value)))
    {
      unsigned char * result = lookup_external_string (value, NULL);
      if (result == 0)
	{
	  outf_error ("\nWarning: Callback returned a bogus xstring.\n");
	  outf_flush_error ();
	  return (NULL);
	}
      return ((void *) result);
    }
  if (is_alien (value))
    return (alien_address (value));

  outf_error ("\nWarning: Callback did not return a pointer.\n");
  outf_flush_error ();
  return (NULL);
}

/* Utilities */

void
check_number_of_args (int num)
{
  if (GET_LEXPR_ACTUALS < num)
    {
      signal_error_from_primitive (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    }
}

SCM
unspecific (void)
{
  return (UNSPECIFIC);
}

SCM
empty_list (void)
{
  return (EMPTY_LIST);
}

DEFINE_PRIMITIVE ("OUTF-CONSOLE", Prim_outf_console, 1, 1, 0)
{
  /* To avoid the normal i/o system when debugging a callback. */

  PRIMITIVE_HEADER (1);
  { 
    SCM arg = ARG_REF (1);
    if (STRING_P (arg))
      {
	char * string = ((char *) STRING_LOC (arg, 0));
	outf_console ("%s", string);
	outf_flush_console ();
      }
    else
      {
	error_wrong_type_arg (1);
      }
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}
