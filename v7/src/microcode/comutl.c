/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/comutl.c,v 1.20 1991/03/06 22:57:31 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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

/* Compiled Code Utilities */

#include "scheme.h"
#include "prims.h"

extern SCHEME_OBJECT
  *compiled_entry_to_block_address();

extern long
  compiled_entry_to_block_offset(),
  coerce_to_compiled();

extern void
  compiled_entry_type();

DEFINE_PRIMITIVE ("COMPILED-CODE-ADDRESS->BLOCK", Prim_comp_code_address_block, 1, 1,
  "Given a compiled code address, return its compiled code block.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  PRIMITIVE_RETURN
    (MAKE_POINTER_OBJECT
     (TC_COMPILED_CODE_BLOCK,
      (compiled_entry_to_block_address (ARG_REF (1)))));
}

DEFINE_PRIMITIVE ("COMPILED-CODE-ADDRESS->OFFSET", Prim_comp_code_address_offset, 1, 1,
  "Given a compiled code address, return its offset into its block.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  PRIMITIVE_RETURN
    (LONG_TO_FIXNUM (compiled_entry_to_block_offset (ARG_REF (1))));
}

#ifndef USE_STACKLETS

DEFINE_PRIMITIVE ("STACK-TOP-ADDRESS", Prim_stack_top_address, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer ((long) (ADDRESS_TO_DATUM (Stack_Top))));
}

DEFINE_PRIMITIVE ("STACK-ADDRESS-OFFSET", Prim_stack_address_offset, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STACK_ADDRESS_P);
  PRIMITIVE_RETURN
    (long_to_integer
     (STACK_LOCATIVE_DIFFERENCE ((Stack_Top),
				 (OBJECT_ADDRESS (ARG_REF (1))))));
}

#endif /* USE_STACKLETS */

DEFINE_PRIMITIVE ("COMPILED-ENTRY-KIND", Prim_compiled_entry_type, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  {
    long results [3];
    compiled_entry_type ((ARG_REF (1)), results);
    PRIMITIVE_RETURN
      (hunk3_cons ((LONG_TO_FIXNUM (results [0])),
		   (LONG_TO_FIXNUM (results [1])),
		   (LONG_TO_FIXNUM (results [2]))));
  }
}

DEFINE_PRIMITIVE ("COERCE-TO-COMPILED-PROCEDURE", Prim_coerce_to_closure, 2, 2, 0)
{
  SCHEME_OBJECT temp;
  long result;
  PRIMITIVE_HEADER(2);
  result = (coerce_to_compiled ((ARG_REF (1)), (arg_integer (2)), &temp));
  switch(result)
  {
    case PRIM_DONE:
      PRIMITIVE_RETURN(temp);

    case PRIM_INTERRUPT:
      Primitive_GC(10);
      /*NOTREACHED*/

    default:
      error_bad_range_arg (2);
      /*NOTREACHED*/
  }
}

DEFINE_PRIMITIVE ("COMPILED-CLOSURE->ENTRY", Prim_compiled_closure_to_entry, 1, 1,
  "Given a compiled closure, return the entry point which it invokes.")
{
  SCHEME_OBJECT entry_type [3];
  SCHEME_OBJECT closure;
  extern void compiled_entry_type ();
  extern long compiled_entry_closure_p ();
  extern SCHEME_OBJECT compiled_closure_to_entry ();
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  closure = (ARG_REF (1));
  compiled_entry_type (closure, (& (entry_type [0])));
  if (! (((entry_type [0]) == 0) && (compiled_entry_closure_p (closure))))
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (compiled_closure_to_entry (closure));
}
