/* -*-C-*-

$Id: trap.h,v 9.45 2000/12/05 21:23:48 cph Exp $

Copyright (c) 1987, 1988, 1989, 1999, 2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

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
#define TRAP_EXPENSIVE				6
#define TRAP_EXPENSIVE_DANGEROUS		7

/* TRAP_MAX_IMMEDIATE is defined in const.h */

/* The following are not: */

#define TRAP_NOP				10
#define TRAP_DANGEROUS				11
#define TRAP_FLUID				12
#define TRAP_FLUID_DANGEROUS			13
#define TRAP_COMPILER_CACHED			14
#define TRAP_COMPILER_CACHED_DANGEROUS		15

/* These MUST be distinct */

#define TRAP_EXTENSION_TYPE			TC_QUAD
#define TRAP_REFERENCES_TYPE			TC_HUNK3

/* Trap utilities */

#define get_trap_kind(variable, what)					\
{									\
  variable = OBJECT_DATUM (what);					\
  if (variable > TRAP_MAX_IMMEDIATE)					\
    variable = OBJECT_DATUM (MEMORY_REF (what, TRAP_TAG));		\
}

/* Common constants */

#if (SIZEOF_UNSIGNED_LONG == 4)	/* 32 bit objects */
#  if (TYPE_CODE_LENGTH == 8)
#    define UNASSIGNED_OBJECT		0x32000000
#    define DANGEROUS_UNASSIGNED_OBJECT	0x32000001
#    define UNBOUND_OBJECT		0x32000002
#    define DANGEROUS_UNBOUND_OBJECT	0x32000003
#    define ILLEGAL_OBJECT		0x32000004
#    define DANGEROUS_ILLEGAL_OBJECT	0x32000005
#    define EXPENSIVE_OBJECT		0x32000006
#    define DANGEROUS_EXPENSIVE_OBJECT	0x32000007
#  endif
#  if (TYPE_CODE_LENGTH == 6)
#    define UNASSIGNED_OBJECT		0xc8000000
#    define DANGEROUS_UNASSIGNED_OBJECT	0xc8000001
#    define UNBOUND_OBJECT		0xc8000002
#    define DANGEROUS_UNBOUND_OBJECT	0xc8000003
#    define ILLEGAL_OBJECT		0xc8000004
#    define DANGEROUS_ILLEGAL_OBJECT	0xc8000005
#    define EXPENSIVE_OBJECT		0xc8000006
#    define DANGEROUS_EXPENSIVE_OBJECT	0xc8000007
#  endif
#  if (TC_REFERENCE_TRAP != 0x32)
#    include "error: trap.h and types.h are inconsistent"
#  endif
#endif

#ifndef UNASSIGNED_OBJECT		/* Safe version */
#  define UNASSIGNED_OBJECT		MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_UNASSIGNED)
#  define DANGEROUS_UNASSIGNED_OBJECT	MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_UNASSIGNED_DANGEROUS)
#  define UNBOUND_OBJECT		MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_UNBOUND)
#  define DANGEROUS_UNBOUND_OBJECT	MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_UNBOUND_DANGEROUS)
#  define ILLEGAL_OBJECT		MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_ILLEGAL)
#  define DANGEROUS_ILLEGAL_OBJECT	MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_ILLEGAL_DANGEROUS)
#  define EXPENSIVE_OBJECT		MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_EXPENSIVE)
#  define DANGEROUS_EXPENSIVE_OBJECT	MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_EXPENSIVE_DANGEROUS)
#endif

#define NOP_OBJECT (LONG_TO_UNSIGNED_FIXNUM (TRAP_NOP))
#define DANGEROUS_OBJECT (LONG_TO_UNSIGNED_FIXNUM (TRAP_DANGEROUS))
#define REQUEST_RECACHE_OBJECT DANGEROUS_ILLEGAL_OBJECT
#define EXPENSIVE_ASSIGNMENT_OBJECT EXPENSIVE_OBJECT
