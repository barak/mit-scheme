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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/types.h,v 9.22 1987/04/03 00:21:38 jinx Exp $
 *
 * Type code definitions, numerical order
 *
 */

#define TC_NULL	                	0x00
#define TC_LIST				0x01
#define TC_CHARACTER			0x02
#define	TC_SCODE_QUOTE                 	0x03
#define TC_PCOMB2			0x04
#define TC_UNINTERNED_SYMBOL		0x05
#define TC_BIG_FLONUM			0x06
#define TC_COMBINATION_1		0x07
#define TC_TRUE				0x08
#define TC_EXTENDED_PROCEDURE		0x09
#define TC_VECTOR			0x0A
#define TC_RETURN_CODE 			0x0B
#define TC_COMBINATION_2		0x0C
#define TC_COMPILED_PROCEDURE		0x0D
#define TC_BIG_FIXNUM			0x0E
#define TC_PROCEDURE			0x0F
#define TC_PRIMITIVE_EXTERNAL		0x10
#define TC_DELAY			0x11
#define TC_ENVIRONMENT			0x12
#define TC_DELAYED			0x13
#define TC_EXTENDED_LAMBDA		0x14
#define TC_COMMENT			0x15
#define TC_NON_MARKED_VECTOR		0x16
#define TC_LAMBDA			0x17
#define TC_PRIMITIVE			0x18
#define TC_SEQUENCE_2			0x19

#define TC_FIXNUM			0x1A
#define TC_PCOMB1			0x1B
#define TC_CONTROL_POINT		0x1C
#define TC_INTERNED_SYMBOL		0x1D
#define TC_CHARACTER_STRING		0x1E
#define TC_ACCESS			0x1F
/* UNUSED				0x20 */ /* Used to be EXTENDED_FIXNUM. */
#define TC_DEFINITION			0x21
#define TC_BROKEN_HEART			0x22
#define TC_ASSIGNMENT			0x23
#define TC_HUNK3			0x24
#define TC_IN_PACKAGE			0x25
#define TC_COMBINATION			0x26
#define TC_MANIFEST_NM_VECTOR		0x27
#define TC_COMPILED_EXPRESSION		0x28
#define TC_LEXPR			0x29
#define TC_PCOMB3  			0x2A
#define TC_MANIFEST_SPECIAL_NM_VECTOR	0x2B
#define TC_VARIABLE			0x2C
#define TC_THE_ENVIRONMENT		0x2D
#define TC_FUTURE			0x2E
#define TC_VECTOR_1B			0x2F
#define TC_PCOMB0			0x30
#define TC_VECTOR_16B			0x31
#define TC_REFERENCE_TRAP		0x32 /* Used to be UNASSIGNED. */
#define TC_SEQUENCE_3			0x33
#define TC_CONDITIONAL			0x34
#define TC_DISJUNCTION			0x35
#define TC_CELL				0x36
#define TC_WEAK_CONS			0x37
#define TC_QUAD				0x38 /* Used to be TC_TRAP. */
#define TC_RETURN_ADDRESS		0x39
#define TC_COMPILER_LINK		0x3A
#define TC_STACK_ENVIRONMENT		0x3B
#define TC_COMPLEX			0x3C

/* If you add a new type, don't forget to update gccode.h and gctype.c */

/* Aliases */

#define TC_FALSE	        	TC_NULL
#define TC_MANIFEST_VECTOR		TC_NULL
#define GLOBAL_ENV			TC_NULL
#define TC_BIT_STRING			TC_VECTOR_1B
#define TC_VECTOR_8B			TC_CHARACTER_STRING
#define TC_ADDRESS			TC_FIXNUM
