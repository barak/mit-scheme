/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/comutl.c,v 1.10 1987/12/09 22:35:43 jinx Rel $

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
#include "gccode.h"

extern Pointer *compiled_entry_to_block_address();
extern long compiled_entry_to_block_offset();

#define COMPILED_CODE_ADDRESS_P(object)					\
  (((OBJECT_TYPE (object)) == TC_COMPILED_EXPRESSION) ||		\
   ((OBJECT_TYPE (object)) == TC_RETURN_ADDRESS))

Pointer *
compiled_entry_to_block_address(ce)
     Pointer ce;
{
#ifdef Get_Compiled_Block

  Pointer *block;

  block = Get_Pointer(ce);
  Get_Compiled_Block(block, block);
  return block;

#else

  error_external_return();
  /*NOTREACHED*/

#endif
}

long
compiled_entry_to_block_offset(ce)
     Pointer ce;
{
  Pointer *address;

  address = Get_Pointer(ce);
  return (((unsigned long) address) -
	  ((unsigned long) compiled_entry_to_block_address(address)));
}

DEFINE_PRIMITIVE ("COMPILED-CODE-ADDRESS->BLOCK",
		  Prim_comp_code_address_block, 1)
{
  Pointer *address;
  Primitive_1_Arg ();

  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  address = compiled_entry_to_block_address(Arg1);
  PRIMITIVE_RETURN (Make_Pointer (TC_COMPILED_CODE_BLOCK, address));
}

DEFINE_PRIMITIVE ("COMPILED-CODE-ADDRESS->OFFSET", Prim_comp_code_address_offset, 1)
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
