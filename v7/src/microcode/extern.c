/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/extern.c,v 9.23 1987/11/17 08:09:28 jinx Exp $ */

#include "scheme.h"
#include "primitive.h"

/* Mapping between the internal and external representations of
   primitives and return addresses.
 */

/* (MAP-CODE-TO-MACHINE-ADDRESS TYPE-CODE VALUE-CODE)
   For return codes and primitives, this returns the internal
   representation of the return address or primitive address given
   the external representation.  Currently in CScheme these two are
   the same.  In the 68000 assembly version the internal
   representation is an actual address in memory.
*/
Built_In_Primitive(Prim_Map_Code_To_Address, 2,
		   "MAP-CODE-TO-MACHINE-ADDRESS", 0x93)
Define_Primitive(Prim_Map_Code_To_Address, 2,
		   "MAP-CODE-TO-MACHINE-ADDRESS")
{
  long Code, Offset;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  Code = Get_Integer(Arg1);
  Offset = Get_Integer(Arg2);
  switch (Code)
  {
    case TC_RETURN_CODE:
      if (Offset > MAX_RETURN_CODE)
      {
	Primitive_Error(ERR_ARG_2_BAD_RANGE);
      }
      break;

    case TC_PRIMITIVE:
      if (Offset >= NUMBER_OF_PRIMITIVES())
      {
	Primitive_Error(ERR_ARG_2_BAD_RANGE);
      }
      break;

    default: Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  return (Make_Non_Pointer(Code, Offset));
}

/* (MAP-MACHINE-ADDRESS-TO-CODE TYPE-CODE ADDRESS)
   This is the inverse operation for MAP_CODE_TO_ADDRESS.
   Given a machine ADDRESS and a TYPE-CODE (either return code or
   primitive) it finds the number for the external representation
   for the internal address.
*/
Built_In_Primitive(Prim_Map_Address_To_Code, 2,
		   "MAP-MACHINE-ADDRESS-TO-CODE", 0x90)
Define_Primitive(Prim_Map_Address_To_Code, 2,
		   "MAP-MACHINE-ADDRESS-TO-CODE")
{
  long Code, Offset;
  Primitive_2_Args();

  Arg_1_Type(TC_FIXNUM);
  Code = Get_Integer(Arg1);
  Arg_2_Type(Code);
  Offset = Get_Integer(Arg2);
  switch (Code)
  { case TC_RETURN_CODE:
      if (Offset > MAX_RETURN_CODE)
      {
        Primitive_Error(ERR_ARG_2_BAD_RANGE);
      }
      break;

    case TC_PRIMITIVE:
      if (Offset > NUMBER_OF_PRIMITIVES())
      {
        Primitive_Error(ERR_ARG_2_BAD_RANGE);
      }
      break;

    default: 
      Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  return (MAKE_UNSIGNED_FIXNUM(Offset));
}

/* (PRIMITIVE-PROCEDURE-ARITY INTERNAL-PRIMITIVE)
   Given the internal representation of a primitive (in CScheme the
   internal and external representations are the same), return the
   number of arguments it requires.
*/
Built_In_Primitive(Prim_Map_Prim_Address_To_Arity, 1,
		 "PRIMITIVE-PROCEDURE-ARITY", 0x96)
Define_Primitive(Prim_Map_Prim_Address_To_Arity, 1,
		 "PRIMITIVE-PROCEDURE-ARITY")
{
  extern long primitive_to_arity();
  long Prim_Num, answer;
  Primitive_1_Arg();

  Arg_1_Type(TC_PRIMITIVE);
  Prim_Num = Get_Integer(Arg1);

  if (Prim_Num >= NUMBER_OF_PRIMITIVES())
  {
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  answer = primitive_to_arity(Prim_Num);
  return (MAKE_SIGNED_FIXNUM(answer));
}

/* (GET-PRIMITIVE-COUNTS)
   Returns a CONS of the number of primitives defined in this
   interpreter and the number of primitives referenced but not
   defined.
*/

Built_In_Primitive(Prim_Get_Primitive_Counts, 0, "GET-PRIMITIVE-COUNTS", 0x101)
Define_Primitive(Prim_Get_Primitive_Counts, 0, "GET-PRIMITIVE-COUNTS")
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

Built_In_Primitive(Prim_Get_Primitive_Name, 1, "GET-PRIMITIVE-NAME", 0x102)
Define_Primitive(Prim_Get_Primitive_Name, 1, "GET-PRIMITIVE-NAME")
{
  extern Pointer primitive_name();
  long Number, TC;
  Primitive_1_Arg();

  TC = Type_Code(Arg1);
  if ((TC != TC_FIXNUM) && (TC != TC_PRIMITIVE))
  {
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  }
  Range_Check(Number, Arg1, 0, (NUMBER_OF_PRIMITIVES() - 1),
              ERR_ARG_1_BAD_RANGE);
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

Built_In_Primitive(Prim_Get_Primitive_Address, 2, "GET-PRIMITIVE-ADDRESS", 0x103)
Define_Primitive(Prim_Get_Primitive_Address, 2, "GET-PRIMITIVE-ADDRESS")
{
  extern Pointer find_primitive();
  Boolean intern_p, check_p;
  long arity;
  Primitive_2_Args();

  Arg_1_Type(TC_INTERNED_SYMBOL);
  Touch_In_Primitive(Arg2, Arg2);
  if (Arg2 == NIL)
  {
    check_p = false;
    intern_p = false;
    arity = 0;
  }
  else
  {
    CHECK_ARG(2, FIXNUM_P);
    check_p = true;
    intern_p = true;
    Sign_Extend(Arg2, arity);
  }
  PRIMITIVE_RETURN(find_primitive(Fast_Vector_Ref(Arg1, SYMBOL_NAME),
				  intern_p, arity, check_p));
}
