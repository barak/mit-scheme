/* -*-C-*-

$Id: extern.c,v 9.44 2007/04/22 16:31:22 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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
      if (number > (NUMBER_OF_PRIMITIVES ()))
	error_bad_range_arg (2);
      PRIMITIVE_RETURN (MAKE_PRIMITIVE_OBJECT (number));

    default:
      error_bad_range_arg (1);
  }
  /*NOTREACHED*/
  return (0);
}

DEFINE_PRIMITIVE ("MAP-MACHINE-ADDRESS-TO-CODE", Prim_map_address_to_code, 2, 2,
  "This is the inverse operation of `map-code-to-machine-address'.  Given\n\
a machine ADDRESS and a TYPE-CODE (either return code or primitive\n\
procedure), it finds the number for the external representation for\n\
the internal address.")
{
  SCHEME_OBJECT tc;
  SCHEME_OBJECT address;
  PRIMITIVE_HEADER (2);
  tc = (arg_nonnegative_integer (1));
  address = (ARG_REF (2));
  if ((OBJECT_TYPE (address)) != tc)
    error_wrong_type_arg (2);
  switch (tc)
    {
    case TC_RETURN_CODE:
      {
	long number = (OBJECT_DATUM (address));
	if (number > MAX_RETURN_CODE)
	  error_bad_range_arg (2);
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (number));
      }

    case TC_PRIMITIVE:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (PRIMITIVE_NUMBER (address)));

    default:
      error_bad_range_arg (1);
    }
  /*NOTREACHED*/
  return (0);
}

DEFINE_PRIMITIVE ("PRIMITIVE-PROCEDURE-ARITY", Prim_primitive_procedure_arity, 1, 1,
  "Given a primitive procedure, returns the number of arguments it requires.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PRIMITIVE_P);
  {
    SCHEME_OBJECT primitive = (ARG_REF (1));
    if ((PRIMITIVE_NUMBER (primitive))
	> ((unsigned long) (NUMBER_OF_PRIMITIVES ())))
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (LONG_TO_FIXNUM (PRIMITIVE_ARITY (primitive)));
  }
}

DEFINE_PRIMITIVE ("PRIMITIVE-PROCEDURE-DOCUMENTATION",
		  Prim_primitive_procedure_doc, 1, 1,
  "Given a primitive procedure, return its documentation string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PRIMITIVE_P);
  {
    SCHEME_OBJECT primitive = (ARG_REF (1));
    if ((PRIMITIVE_NUMBER (primitive))
	> ((unsigned long) (NUMBER_OF_PRIMITIVES ())))
      error_bad_range_arg (1);
    {
      const char * answer = (PRIMITIVE_DOCUMENTATION (primitive));
      PRIMITIVE_RETURN
	((answer == 0)
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
    (cons ((LONG_TO_UNSIGNED_FIXNUM ((NUMBER_OF_PRIMITIVES ()))),
	   (LONG_TO_UNSIGNED_FIXNUM (0))));
}

DEFINE_PRIMITIVE ("GET-PRIMITIVE-NAME", Prim_get_primitive_name, 1, 1,
  "Return the (string) name of PRIMITIVE-PROCEDURE.")
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT primitive = (ARG_REF (1));
    if (! ((PRIMITIVE_P (primitive)) || (FIXNUM_P (primitive))))
      error_wrong_type_arg (1);
    {
      long number = (PRIMITIVE_NUMBER (primitive));
      if ((number < 0) || (number > (NUMBER_OF_PRIMITIVES ())))
	error_bad_range_arg (1);
      PRIMITIVE_RETURN (char_pointer_to_string (PRIMITIVE_NAME (primitive)));
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
  SCHEME_OBJECT name;
  SCHEME_OBJECT arity_arg;
  bool intern_p, allow_p;
  long arity;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, SYMBOL_P);
  name = (ARG_REF (1));
  arity_arg = (ARG_REF (2));
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
     ((MEMORY_REF (name, SYMBOL_NAME)), intern_p, allow_p, arity));
}
