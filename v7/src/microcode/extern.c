/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/extern.c,v 9.30 1990/02/13 16:00:06 cph Rel $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "prims.h"

/* Mapping between the internal and external representations of
   primitives and return addresses.  */

DEFINE_PRIMITIVE ("MAP-CODE-TO-MACHINE-ADDRESS", Prim_map_code_to_address, 2, 2,
  "For return codes and primitives, this returns the internal\n\
representation of the return address or primitive address given the\n\
external representation.\n\
\n\
This accepts two arguments, TYPE-CODE and VALUE-CODE.  TYPE-CODE is\n\
the microcode type of the object to be returned; it must be either a\n\
return address or primitive procedure type.  VALUE-CODE is the index\n\
number (i.e. external representation) of the desired result.")
{
  long tc, number;
  PRIMITIVE_HEADER (2);
  tc = (arg_nonnegative_integer (1));
  number = (arg_nonnegative_integer (2));
  switch (tc)
  {
    case TC_RETURN_CODE:
      if (number > MAX_RETURN_CODE)
	error_bad_range_arg (2);
      PRIMITIVE_RETURN (MAKE_OBJECT (tc, number));

    case TC_PRIMITIVE:
      if (number >= (NUMBER_OF_PRIMITIVES ()))
	error_bad_range_arg (2);
      PRIMITIVE_RETURN
	((number > MAX_PRIMITIVE)
	 ? (MAKE_PRIMITIVE_OBJECT (number, (MAX_PRIMITIVE + 1)))
	 : (MAKE_PRIMITIVE_OBJECT (0, number)));

    default:
      error_bad_range_arg (1);
  }
  /* NOTREACHED */
}

DEFINE_PRIMITIVE ("MAP-MACHINE-ADDRESS-TO-CODE", Prim_map_address_to_code, 2, 2,
  "This is the inverse operation of `map-code-to-machine-address'.  Given\n\
a machine ADDRESS and a TYPE-CODE (either return code or primitive\n\
procedure), it finds the number for the external representation for\n\
the internal address.")
{
  fast long tc;
  fast SCHEME_OBJECT address;
  PRIMITIVE_HEADER (2);
  tc = (arg_nonnegative_integer (1));
  address = (ARG_REF (2));
  if ((OBJECT_TYPE (address)) != tc)
    error_wrong_type_arg (2);
  switch (tc)
    {
    case TC_RETURN_CODE:
      {
	fast long number = (OBJECT_DATUM (address));
	if (number > MAX_RETURN_CODE)
	  error_bad_range_arg (2);
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (number));
      }

    case TC_PRIMITIVE:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (PRIMITIVE_NUMBER (address)));

    default:
      error_bad_range_arg (1);
    }
  /* NOTREACHED */
}

DEFINE_PRIMITIVE ("PRIMITIVE-PROCEDURE-ARITY", Prim_primitive_procedure_arity, 1, 1,
  "Given a primitive procedure, returns the number of arguments it requires.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PRIMITIVE_P);
  {
    fast SCHEME_OBJECT primitive = (ARG_REF (1));
    extern long primitive_to_arity ();
    if ((PRIMITIVE_NUMBER (primitive)) >= (NUMBER_OF_PRIMITIVES ()))
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (LONG_TO_FIXNUM (primitive_to_arity (primitive)));
  }
}

DEFINE_PRIMITIVE ("PRIMITIVE-PROCEDURE-DOCUMENTATION", Prim_primitive_procedure_doc, 1, 1,
  "Given a primitive procedure, return its documentation string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PRIMITIVE_P);
  {
    fast SCHEME_OBJECT primitive = (ARG_REF (1));
    if ((PRIMITIVE_NUMBER (primitive)) >= (NUMBER_OF_PRIMITIVES ()))
      error_bad_range_arg (1);
    {
      extern char * primitive_to_documentation ();
      fast char * answer = (primitive_to_documentation (primitive));
      PRIMITIVE_RETURN
	((answer == ((char *) 0))
	 ? SHARP_F
	 : (char_pointer_to_string (answer)));
    }
  }
}

DEFINE_PRIMITIVE ("GET-PRIMITIVE-COUNTS", Prim_get_primitive_counts, 0, 0,
  "Return a pair of integers which are the number of primitive procedures.\n\
The car is the count of defined primitives;\n\
the cdr is the count of undefined primitives that are referenced.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    (cons ((LONG_TO_UNSIGNED_FIXNUM (NUMBER_OF_DEFINED_PRIMITIVES ())),
	   (LONG_TO_UNSIGNED_FIXNUM (NUMBER_OF_UNDEFINED_PRIMITIVES ()))));
}

DEFINE_PRIMITIVE ("GET-PRIMITIVE-NAME", Prim_get_primitive_name, 1, 1,
  "Return the (string) name of PRIMITIVE-PROCEDURE.")
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT primitive = (ARG_REF (1));
    if (! ((PRIMITIVE_P (primitive)) || (FIXNUM_P (primitive))))
      error_wrong_type_arg (1);
    {
      fast long number = (PRIMITIVE_NUMBER (primitive));
      extern SCHEME_OBJECT primitive_name ();
      if ((number < 0) || (number >= NUMBER_OF_PRIMITIVES()))
	error_bad_range_arg (1);
      PRIMITIVE_RETURN (primitive_name (number));
    }
  }
}

DEFINE_PRIMITIVE ("GET-PRIMITIVE-ADDRESS", Prim_get_primitive_address, 2, 2,
  "Given a symbol NAME, return the primitive object of that name.\n\
ARITY is the number of arguments which the primitive should expect.\n\
If ARITY is #F, #F is returned if the primitive is not implemented,\n\
even if the name already exists.\n\
If ARITY is an integer, a primitive object will always be returned,\n\
whether the corresponding primitive is implemented or not.")
{
  fast SCHEME_OBJECT name;
  fast SCHEME_OBJECT arity_arg;
  extern SCHEME_OBJECT find_primitive ();
  Boolean intern_p, allow_p;
  long arity;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, SYMBOL_P);
  name = (ARG_REF (1));
  TOUCH_IN_PRIMITIVE ((ARG_REF (2)), arity_arg);
  if (arity_arg == SHARP_F)
    {
      allow_p = false;
      intern_p = false;
      arity = UNKNOWN_PRIMITIVE_ARITY;
    }
  else if (arity_arg == SHARP_T)
    {
      allow_p = true;
      intern_p = false;
      arity = UNKNOWN_PRIMITIVE_ARITY;
    }
  else
    {
      CHECK_ARG(2, FIXNUM_P);
      allow_p = true;
      intern_p = true;
      arity = (FIXNUM_TO_LONG (arity_arg));
    }
  PRIMITIVE_RETURN
    (find_primitive
     ((FAST_MEMORY_REF (name, SYMBOL_NAME)), intern_p, allow_p, arity));
}
