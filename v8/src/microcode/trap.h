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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/trap.h,v 9.38 1987/05/29 02:24:53 jinx Rel $ */

/* Kinds of traps:

   Note that for every trap there is a dangerous version.
   The danger bit is the bottom bit of the trap number,
   thus all dangerous traps are odd and viceversa.

   For efficiency, some traps are immediate, while some are
   pointer objects.  The type code is multiplexed, and the
   garbage collector handles it specially.
 */

/* The following are immediate traps: */

#define TRAP_UNASSIGNED				0
#define TRAP_UNASSIGNED_DANGEROUS		1
#define TRAP_UNBOUND				2
#define TRAP_UNBOUND_DANGEROUS			3
#define TRAP_ILLEGAL				4
#define TRAP_ILLEGAL_DANGEROUS			5

/* TRAP_MAX_IMMEDIATE is defined in const.h */

/* The following are not: */

#define TRAP_NOP				10
#define TRAP_DANGEROUS				11
#define TRAP_FLUID				12
#define TRAP_FLUID_DANGEROUS			13
#define TRAP_COMPILER_CACHED			14
#define TRAP_COMPILER_CACHED_DANGEROUS		15

#define TRAP_EXTENSION_TYPE			TC_QUAD

/* Trap utilities */

#define get_trap_kind(variable, what)					\
{									\
  variable = Datum(what);						\
  if (variable > TRAP_MAX_IMMEDIATE)					\
    variable = Datum(Vector_Ref(what, TRAP_TAG));			\
}

/* Common constants */

#ifndef b32
#define UNASSIGNED_OBJECT		Make_Non_Pointer(TC_REFERENCE_TRAP, TRAP_UNASSIGNED)
#define DANGEROUS_UNASSIGNED_OBJECT	Make_Non_Pointer(TC_REFERENCE_TRAP, TRAP_UNASSIGNED_DANGEROUS)
#define UNBOUND_OBJECT			Make_Non_Pointer(TC_REFERENCE_TRAP, TRAP_UNBOUND)
#define DANGEROUS_UNBOUND_OBJECT	Make_Non_Pointer(TC_REFERENCE_TRAP, TRAP_UNBOUND_DANGEROUS)
#define ILLEGAL_OBJECT			Make_Non_Pointer(TC_REFERENCE_TRAP, TRAP_ILLEGAL)
#define DANGEROUS_ILLEGAL_OBJECT	Make_Non_Pointer(TC_REFERENCE_TRAP, TRAP_ILLEGAL_DANGEROUS)
#else
#define UNASSIGNED_OBJECT		0x32000000
#define DANGEROUS_UNASSIGNED_OBJECT	0x32000001
#define UNBOUND_OBJECT			0x32000002
#define DANGEROUS_UNBOUND_OBJECT	0x32000003
#define ILLEGAL_OBJECT			0x32000004
#define DANGEROUS_ILLEGAL_OBJECT	0x32000005
#endif

#define NOP_OBJECT			Make_Unsigned_Fixnum(TRAP_NOP)
#define DANGEROUS_OBJECT		Make_Unsigned_Fixnum(TRAP_DANGEROUS)
#define REQUEST_RECACHE_OBJECT		DANGEROUS_ILLEGAL_OBJECT

#if (TC_REFERENCE_TRAP != 0x32)
#include "error: trap.h and types.h are inconsistent"
#endif

