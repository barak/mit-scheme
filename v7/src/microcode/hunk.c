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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/hunk.c,v 9.25 1988/08/15 20:49:17 cph Rel $
 *
 * Support for Hunk3s (triples)
 */

#include "scheme.h"
#include "prims.h"

/* (HUNK3-CONS FIRST SECOND THIRD)
      Returns a triple consisting of the specified values.
*/
DEFINE_PRIMITIVE ("HUNK3-CONS", Prim_hunk3_cons, 3, 3, 0)
{
  Primitive_3_Args();

  Primitive_GC_If_Needed(3);
  *Free++ = Arg1;
  *Free++ = Arg2;
  *Free++ = Arg3;
  return Make_Pointer(TC_HUNK3, Free-3);
}

/* (HUNK3-CXR TRIPLE N)
      Returns the Nth item from the TRIPLE.  N must be 0, 1, or 2.
*/
DEFINE_PRIMITIVE ("HUNK3-CXR", Prim_hunk3_cxr, 2, 2, 0)
{
  long Offset;
  Primitive_2_Args();

  CHECK_ARG(1, HUNK3_P);
  CHECK_ARG(2, FIXNUM_P);
  Range_Check(Offset, Arg2, 0, 2, ERR_ARG_2_BAD_RANGE);
  return Vector_Ref(Arg1, Offset);
}

/* (HUNK3-SET-CXR! TRIPLE N VALUE)
      Stores VALUE in the Nth item of TRIPLE.  N must be 0, 1, or 2.
      Returns the previous contents.
*/
DEFINE_PRIMITIVE ("HUNK3-SET-CXR!", Prim_hunk3_set_cxr, 3, 3, 0)
{
  long Offset;
  Primitive_3_Args();

  CHECK_ARG(1, HUNK3_P);
  CHECK_ARG(2, FIXNUM_P);
  Range_Check(Offset, Arg2, 0, 2, ERR_ARG_2_BAD_RANGE);
  Side_Effect_Impurify(Arg1, Arg3);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, Offset), Arg3);
}

/* (SYSTEM-HUNK3-CXR0 GC-TRIPLE)
      Returns item 0 (the first item) from any object with a GC type
      of triple.  For example, this would access the operator slot of
      a COMBINATION_2_OPERAND SCode item.
*/
DEFINE_PRIMITIVE ("SYSTEM-HUNK3-CXR0", Prim_sys_h3_0, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_GC_Type(GC_Triple);
  return Vector_Ref(Arg1, 0);
}

/* (SYSTEM-HUNK3-CXR1 GC-TRIPLE)
      Returns item 1 (the second item) from any object with a GC type
      of triple.  For example, this would access the first operand
      slot of a COMBINATION_2_OPERAND SCode item.
*/
DEFINE_PRIMITIVE ("SYSTEM-HUNK3-CXR1", Prim_sys_h3_1, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_GC_Type(GC_Triple);
  return Vector_Ref(Arg1, 1);
}

/* (SYSTEM-HUNK3-CXR2 GC-TRIPLE)
      Returns item 2 (the third item) from any object with a GC type
      of triple.  For example, this would access the second operand
      slot of a COMBINATION_2_OPERAND SCode item.
*/
DEFINE_PRIMITIVE ("SYSTEM-HUNK3-CXR2", Prim_sys_h3_2, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_GC_Type(GC_Triple);
  return Vector_Ref(Arg1, 2);
}

/* (SYSTEM-HUNK3-SET-CXR0! GC-TRIPLE NEW-CONTENTS)
      Replaces item 0 (the first item) in any object with a GC type of
      triple with NEW-CONTENTS.  For example, this would modify the
      operator slot of a COMBINATION_2_OPERAND SCode item.  Returns
      the previous contents.
*/
DEFINE_PRIMITIVE ("SYSTEM-HUNK3-SET-CXR0!", Prim_sh3_set_0, 2, 2, 0)
{
  Primitive_2_Args();
  Arg_1_GC_Type(GC_Triple);

  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, 0), Arg2);
}

/* (SYSTEM-HUNK3-SET-CXR1! GC-TRIPLE NEW-CONTENTS)
      Replaces item 1 (the second item) in any object with a GC type
      of triple with NEW-CONTENTS.  For example, this would modify the
      first operand slot of a COMBINATION_2_OPERAND SCode item.
      Returns the previous contents.
*/
DEFINE_PRIMITIVE ("SYSTEM-HUNK3-SET-CXR1!", Prim_sh3_set_1, 2, 2, 0)
{
  Primitive_2_Args();
  Arg_1_GC_Type(GC_Triple);

  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, 1), Arg2);
}

/* (SYSTEM-HUNK3-SET-CXR2! GC-TRIPLE NEW-CONTENTS)
      Replaces item 2 (the third item) in any object with a GC type of
      triple with NEW-CONTENTS.  For example, this would modify the
      second operand slot of a COMBINATION_2_OPERAND SCode item.
      Returns the previous contents.
*/
DEFINE_PRIMITIVE ("SYSTEM-HUNK3-SET-CXR2!", Prim_sh3_set_2, 2, 2, 0)
{
  Primitive_2_Args();
  Arg_1_GC_Type(GC_Triple);

  Side_Effect_Impurify(Arg1, Arg2);
  return Swap_Pointers(Nth_Vector_Loc(Arg1, 2), Arg2);
}

