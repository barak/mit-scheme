/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/extern.c,v 9.26 1988/08/15 20:45:38 cph Exp $ */

#include "scheme.h"
#include "prims.h"

/* Mapping between the internal and external representations of
   primitives and return addresses.
 */

/* (MAP-CODE-TO-MACHINE-ADDRESS TYPE-CODE VALUE-CODE) For return codes
   and primitives, this returns the internal representation of the
   return address or primitive address given the external
   representation.  Currently in CScheme these two are the same for
   return codes, but for primitives there are two parts to the code.
   In the 68000 assembly version the internal representation is an
   actual address in memory.
*/

DEFINE_PRIMITIVE ("MAP-CODE-TO-MACHINE-ADDRESS", Prim_map_code_to_address, 2, 2, 0)
{
  Pointer result;
  long tc, number;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  tc = Get_Integer(Arg1);
  number = Get_Integer(Arg2);
  switch (tc)
  {
    case TC_RETURN_CODE:
      if (number > MAX_RETURN_CODE)
      {
	Primitive_Error(ERR_ARG_2_BAD_RANGE);
      }
      result = (Make_Non_Pointer(tc, number));
      break;

    case TC_PRIMITIVE:
      if (number >= NUMBER_OF_PRIMITIVES())
      {
	Primitive_Error(ERR_ARG_2_BAD_RANGE);
      }
      if (number > MAX_PRIMITIVE)
      {
	result = MAKE_PRIMITIVE_OBJECT(number, (MAX_PRIMITIVE + 1));
      }
      else
      {
	result = MAKE_PRIMITIVE_OBJECT(0, number);
      }
      break;

    default: Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  PRIMITIVE_RETURN(result);
}

/* (MAP-MACHINE-ADDRESS-TO-CODE TYPE-CODE ADDRESS)
   This is the inverse operation for MAP_CODE_TO_ADDRESS.
   Given a machine ADDRESS and a TYPE-CODE (either return code or
   primitive) it finds the number for the external representation
   for the internal address.
*/

DEFINE_PRIMITIVE ("MAP-MACHINE-ADDRESS-TO-CODE", Prim_map_address_to_code, 2, 2, 0)
{
  long tc, number;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  tc = Get_Integer(Arg1);
  Arg_2_Type(tc);
  switch (tc)
  { case TC_RETURN_CODE:
      number = Get_Integer(Arg2);
      if (number > MAX_RETURN_CODE)
      {
        Primitive_Error(ERR_ARG_2_BAD_RANGE);
      }
      break;

    case TC_PRIMITIVE:
      number = PRIMITIVE_NUMBER(Arg2);
      break;

    default: 
      Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  PRIMITIVE_RETURN(MAKE_UNSIGNED_FIXNUM(number));
}

DEFINE_PRIMITIVE ("PRIMITIVE-PROCEDURE-ARITY", Prim_map_prim_address_to_arity, 1, 1, 
  "Given the internal representation of PRIMITIVE (in CScheme the
internal and external representations are the same), return the
number of arguments it requires.")
{
  extern long primitive_to_arity();
  long answer;
  Primitive_1_Arg();

  Arg_1_Type(TC_PRIMITIVE);

  if (PRIMITIVE_NUMBER(Arg1) >= NUMBER_OF_PRIMITIVES())
  {
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  answer = primitive_to_arity(Arg1);
  PRIMITIVE_RETURN(MAKE_SIGNED_FIXNUM(answer));
}

DEFINE_PRIMITIVE ("PRIMITIVE-PROCEDURE-DOCUMENTATION", Prim_map_prim_address_to_documentation, 1, 1, 
  "Given the internal representation of PRIMITIVE (in CScheme the
internal and external representations are the same), return the
number of arguments it requires.")
{
  extern char * primitive_to_documentation ();
  char * answer;
  Primitive_1_Arg ();

  Arg_1_Type (TC_PRIMITIVE);

  if ((PRIMITIVE_NUMBER (Arg1)) >= (NUMBER_OF_PRIMITIVES ()))
    error_bad_range_arg (1);
  answer = (primitive_to_documentation (Arg1));
  PRIMITIVE_RETURN
    ((answer == ((char *) 0))
     ? SHARP_F
     : (C_String_To_Scheme_String (answer)));
}

/* (GET-PRIMITIVE-COUNTS)
   Returns a CONS of the number of primitives defined in this
   interpreter and the number of primitives referenced but not
   defined.
*/

DEFINE_PRIMITIVE ("GET-PRIMITIVE-COUNTS", Prim_get_primitive_counts, 0, 0, 0)
{
  Primitive_0_Args();

  *Free++ = MAKE_UNSIGNED_FIXNUM(NUMBER_OF_DEFINED_PRIMITIVES());
  *Free++ = MAKE_UNSIGNED_FIXNUM(NUMBER_OF_UNDEFINED_PRIMITIVES());
  PRIMITIVE_RETURN(Make_Pointer(TC_LIST, Free - 2));
}

/* (GET-PRIMITIVE-NAME n)
   Given a number, return the string for the name of the corresponding
   primitive procedure.  It causes an error if the number is out of range.
*/

DEFINE_PRIMITIVE ("GET-PRIMITIVE-NAME", Prim_get_primitive_name, 1, 1, 0)
{
  extern Pointer primitive_name();
  long Number, TC;
  Primitive_1_Arg();

  TC = OBJECT_TYPE(Arg1);
  if ((TC != TC_FIXNUM) && (TC != TC_PRIMITIVE))
  {
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  Number = PRIMITIVE_NUMBER(Arg1);
  if ((Number < 0) || (Number >= NUMBER_OF_PRIMITIVES()))
  {
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  PRIMITIVE_RETURN(primitive_name(Number));
}

/* (GET-PRIMITIVE-ADDRESS name arity)
   Given a symbol (name), return the primitive object corresponding
   to this name.  
   arity is the number of arguments which the primitive should expect.
   If arity is false, NIL is returned if the primitive is not
   implemented even if the name alredy exists.
   If arity is an integer, a primitive object will always be returned,
   whether the corresponding primitive is implemented or not.
*/

DEFINE_PRIMITIVE ("GET-PRIMITIVE-ADDRESS", Prim_get_primitive_address, 2, 2, 0)
{
  extern Pointer find_primitive();
  Boolean intern_p, allow_p;
  long arity;
  Primitive_2_Args();

  Arg_1_Type(TC_INTERNED_SYMBOL);
  Touch_In_Primitive(Arg2, Arg2);
  if (Arg2 == NIL)
  {
    allow_p = false;
    intern_p = false;
    arity = UNKNOWN_PRIMITIVE_ARITY;
  }
  else if (Arg2 == SHARP_T)
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
    Sign_Extend(Arg2, arity);
  }
  PRIMITIVE_RETURN(find_primitive(Fast_Vector_Ref(Arg1, SYMBOL_NAME),
				  intern_p, allow_p, arity));
}
