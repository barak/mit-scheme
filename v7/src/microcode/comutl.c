/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/comutl.c,v 1.11 1988/03/12 16:04:26 jinx Exp $

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

/* Compiled Code Utilities */

#include "scheme.h"
#include "primitive.h"

extern Pointer *compiled_entry_to_block_address();
extern long compiled_entry_to_block_offset();
extern void compiled_entry_type();

#define COMPILED_CODE_ADDRESS_P(object)			\
   ((OBJECT_TYPE (object)) == TC_COMPILED_ENTRY)

DEFINE_PRIMITIVE ("COMPILED-CODE-ADDRESS->BLOCK",
		  Prim_comp_code_address_block, 1)
{
  Pointer *address;
  Primitive_1_Arg ();

  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  address = compiled_entry_to_block_address(Arg1);
  PRIMITIVE_RETURN (Make_Pointer (TC_COMPILED_CODE_BLOCK, address));
}

DEFINE_PRIMITIVE ("COMPILED-CODE-ADDRESS->OFFSET",
		  Prim_comp_code_address_offset, 1)
{
  long offset;
  Primitive_1_Arg ();

  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  offset = compiled_entry_to_block_offset(Arg1);
  PRIMITIVE_RETURN (MAKE_SIGNED_FIXNUM (offset));
}

/*
  This number is eventually used to subtract from stack environment
  addresses, so it must correspond to how those are made.

  NOTE: this will have to be updated when the compiler is ported to the
  stacklet microcode.
 */

#define STACK_TOP_TO_DATUM() (((long) Stack_Top) & ADDRESS_MASK)

DEFINE_PRIMITIVE("STACK-TOP-ADDRESS", Prim_Stack_Top_Address, 0)
{
  Primitive_0_Args();

  PRIMITIVE_RETURN (MAKE_SIGNED_FIXNUM(STACK_TOP_TO_DATUM()));
}

DEFINE_PRIMITIVE("COMPILED-ENTRY-KIND", Prim_Compiled_Entry_Type, 1)
{
  fast Pointer *temp;
  Pointer result;
  PRIMITIVE_HEADER(1);

  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);

  Primitive_GC_If_Needed(3);
  temp = Free;
  Free = &temp[3];
  compiled_entry_type(ARG_REF(1), temp);
  temp[0] = MAKE_UNSIGNED_FIXNUM(((long) temp[0]));
  temp[1] = MAKE_SIGNED_FIXNUM(((long) temp[1]));
  temp[2] = MAKE_SIGNED_FIXNUM(((long) temp[2]));
  PRIMITIVE_RETURN (Make_Pointer(TC_HUNK3, temp));
}
