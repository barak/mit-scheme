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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/extern.c,v 9.22 1987/04/16 02:21:18 jinx Rel $ */

#include "scheme.h"
#include "primitive.h"

/* (GET-EXTERNAL-COUNTS)
   Returns a CONS of the number of external primitives defined in this
   interpreter and the number of external primitives referenced but
   not defined.
*/

Built_In_Primitive(Prim_Get_External_Count, 0, "GET-EXTERNAL-COUNTS", 0x101)
{
  Primitive_0_Args();

  *Free++ = Make_Unsigned_Fixnum(MAX_EXTERNAL_PRIMITIVE + 1);
  *Free++ = Make_Unsigned_Fixnum(NUndefined());
  return Make_Pointer(TC_LIST, Free - 2);
}

/* (GET-EXTERNAL-NAME n)
   Given a number, return the string for the name of the corresponding
   external primitive.  An error if the number is out of range.
   External primitives start at 0.
*/

Built_In_Primitive(Prim_Get_Ext_Name, 1, "GET-EXTERNAL-NAME", 0x102)
{
  extern Pointer external_primitive_name();
  long Number, TC;
  Primitive_1_Arg();

  TC = Type_Code(Arg1);
  if ((TC != TC_FIXNUM) && (TC != TC_PRIMITIVE_EXTERNAL))
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Range_Check(Number, Arg1, 0, MAX_EXTERNAL_PRIMITIVE+NUndefined(),
              ERR_ARG_1_BAD_RANGE);
  if (Number <= MAX_EXTERNAL_PRIMITIVE)
    return external_primitive_name(Number);
  else return User_Vector_Ref(Undefined_Externals,
                              (Number - MAX_EXTERNAL_PRIMITIVE));
}

/* (GET-EXTERNAL-NUMBER name intern?)
   Given a symbol (name), return the external primitive object
   corresponding to this name.  
   If intern? is true, then an external object is created if one
   didn't exist before.
   If intern? is false, NIL is returned if the primitive is not
   implemented even if the name alredy exists.
   Otherwise, NIL is returned if the primitive does not exist and
   the name does not exist either.
*/

Built_In_Primitive(Prim_Get_Ext_Number, 2, "GET-EXTERNAL-NUMBER", 0x103)
{
  extern long make_external_primitive();
  Primitive_2_Args();

  Arg_1_Type(TC_INTERNED_SYMBOL);
  Touch_In_Primitive(Arg2, Arg2);
  return make_external_primitive(Arg1, Arg2);
}
