/* -*-C-*-

$Id: comutl.c,v 1.32 2002/11/20 19:46:07 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* Compiled Code Utilities */

#include "scheme.h"
#include "prims.h"

extern SCHEME_OBJECT
  * EXFUN (compiled_entry_to_block_address, (SCHEME_OBJECT));

extern long
  EXFUN (compiled_entry_to_block_offset, (SCHEME_OBJECT)),
  EXFUN (coerce_to_compiled, (SCHEME_OBJECT, long, SCHEME_OBJECT *));

extern void EXFUN (compiled_entry_type, (SCHEME_OBJECT, long *));

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
      return (0);
  }
}

DEFINE_PRIMITIVE ("COMPILED-CLOSURE->ENTRY", Prim_compiled_closure_to_entry, 1, 1,
  "Given a compiled closure, return the entry point which it invokes.")
{
  long entry_type [3];
  SCHEME_OBJECT closure;
  extern long EXFUN (compiled_entry_closure_p, (SCHEME_OBJECT));
  extern SCHEME_OBJECT EXFUN (compiled_closure_to_entry, (SCHEME_OBJECT));
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  closure = (ARG_REF (1));
  compiled_entry_type (closure, (& (entry_type [0])));
  if (! (((entry_type [0]) == 0) && (compiled_entry_closure_p (closure))))
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (compiled_closure_to_entry (closure));
}

DEFINE_PRIMITIVE ("UTILITY-INDEX->NAME", Prim_utility_index_to_name, 1, 1,
  "Given an integer, return the name of the corresponding compiled code utility.")
{
  extern char * EXFUN (utility_index_to_name, (int));
  char * result;
  PRIMITIVE_HEADER (1);

  result = (utility_index_to_name (arg_integer (1)));
  if (result == ((char *) NULL))
    PRIMITIVE_RETURN (SHARP_F);
  else
    PRIMITIVE_RETURN (char_pointer_to_string ((unsigned char *) result));
}

DEFINE_PRIMITIVE ("BUILTIN-INDEX->NAME", Prim_builtin_index_to_name, 1, 1,
  "Given an integer, return the name of the corresponding compiled code utility.")
{
  extern char * EXFUN (builtin_index_to_name, (int));
  char * result;
  PRIMITIVE_HEADER (1);

  result = (builtin_index_to_name (arg_integer (1)));
  if (result == ((char *) NULL))
    PRIMITIVE_RETURN (SHARP_F);
  else
    PRIMITIVE_RETURN (char_pointer_to_string ((unsigned char *) result));
}

/* This is only meaningful for the C back end. */

DEFINE_PRIMITIVE ("INITIALIZE-C-COMPILED-BLOCK",
		  Prim_initialize_C_compiled_block, 1, 1,
  "Given the tag of a compiled object, return the object.")
{
#ifdef NATIVE_CODE_IS_C
  extern SCHEME_OBJECT * EXFUN (initialize_C_compiled_block, (int, char *));
  SCHEME_OBJECT * block, val;
  
  block = (initialize_C_compiled_block (1, (STRING_ARG (1))));
  val = ((block == ((SCHEME_OBJECT *) NULL))
	 ? SHARP_F
	 : (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY, block)));
  PRIMITIVE_RETURN (val);
#else
  PRIMITIVE_RETURN (SHARP_F);
#endif
}

DEFINE_PRIMITIVE ("DECLARE-COMPILED-CODE-BLOCK",
		  Prim_declare_compiled_code_block, 1, 1,
  "Ensure cache coherence for a compiled-code block newly constructed.")
{
  extern void EXFUN (declare_compiled_code_block, (SCHEME_OBJECT));
  SCHEME_OBJECT new_cc_block;
  PRIMITIVE_HEADER (1);

  new_cc_block = (ARG_REF (1));
  if ((OBJECT_TYPE (new_cc_block)) != TC_COMPILED_CODE_BLOCK)
    error_wrong_type_arg (1);
  declare_compiled_code_block (new_cc_block);
  PRIMITIVE_RETURN (SHARP_T);
}

extern SCHEME_OBJECT EXFUN (bkpt_install, (PTR));
extern SCHEME_OBJECT EXFUN (bkpt_closure_install, (PTR));
extern Boolean EXFUN (bkpt_p, (PTR));
extern SCHEME_OBJECT EXFUN (bkpt_proceed, (PTR, SCHEME_OBJECT, SCHEME_OBJECT));
extern void EXFUN (bkpt_remove, (PTR, SCHEME_OBJECT));

DEFINE_PRIMITIVE ("BKPT/INSTALL", Prim_install_bkpt, 1, 1,
		  "(compiled-entry-object)\n\
Install a breakpoint trap in a compiled code object.\n\
Returns false or a handled needed by REMOVE-BKPT and ONE-STEP-PROCEED.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);

  {
    SCHEME_OBJECT * entry = (OBJECT_ADDRESS (ARG_REF (1)));
    SCHEME_OBJECT * block;

    if (bkpt_p ((PTR) entry))
      error_bad_range_arg (1);

    block = (compiled_entry_to_block_address (ARG_REF (1)));
    if ((OBJECT_TYPE (block[0])) == TC_MANIFEST_CLOSURE)
      PRIMITIVE_RETURN (bkpt_closure_install ((PTR) entry));
    else
      PRIMITIVE_RETURN (bkpt_install ((PTR) entry));
  }
}

DEFINE_PRIMITIVE ("BKPT/REMOVE", Prim_remove_bkpt, 2, 2,
		  "(compiled-entry-object handle)\n\
Remove a breakpoint trap installed by INSTALL-BKPT.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  CHECK_ARG (2, NON_MARKED_VECTOR_P);

  {
    SCHEME_OBJECT * entry = (OBJECT_ADDRESS (ARG_REF (1)));
    SCHEME_OBJECT handle = (ARG_REF (2));

    if (! (bkpt_p ((PTR) entry)))
      error_bad_range_arg (1);
    bkpt_remove (((PTR) entry), handle);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("BKPT?", Prim_bkpt_p, 1, 1,
		  "(compiled-entry-object)\n\
True if there is a breakpoint trap in compiled-entry-object.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);

  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT
		    (bkpt_p ((PTR) (OBJECT_ADDRESS (ARG_REF (1))))));
}

DEFINE_PRIMITIVE ("BKPT/PROCEED", Prim_bkpt_proceed, 3, 3,
		  "(compiled-entry-object handle state)\n\
Proceed the computation from the current breakpoint.")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  CHECK_ARG (2, NON_MARKED_VECTOR_P);

  PRIMITIVE_RETURN (bkpt_proceed (((PTR) (OBJECT_ADDRESS (ARG_REF (1)))),
				  (ARG_REF (2)),
				  (ARG_REF (3))));
}
