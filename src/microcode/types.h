/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/* Type code definitions */

#define TC_NULL				0x00
#define TC_LIST				0x01
#define TC_CHARACTER			0x02
#define	TC_SCODE_QUOTE			0x03
#define TC_PCOMB2			0x04
#define TC_UNINTERNED_SYMBOL		0x05
#define TC_BIG_FLONUM			0x06
#define TC_COMBINATION_1		0x07
#define TC_CONSTANT			0x08
#define TC_EXTENDED_PROCEDURE		0x09
#define TC_VECTOR			0x0A
#define TC_RETURN_CODE			0x0B
#define TC_COMBINATION_2		0x0C
#define TC_MANIFEST_CLOSURE		0x0D
#define TC_BIG_FIXNUM			0x0E
#define TC_PROCEDURE			0x0F
#define TC_ENTITY			0x10
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
#define TC_HUNK3_A			0x20
#define TC_DEFINITION			0x21
#define TC_BROKEN_HEART			0x22
#define TC_ASSIGNMENT			0x23
#define TC_HUNK3_B			0x24
#define TC_IN_PACKAGE			0x25
#define TC_COMBINATION			0x26
#define TC_MANIFEST_NM_VECTOR		0x27
#define TC_COMPILED_ENTRY		0x28
#define TC_LEXPR			0x29
#define TC_PCOMB3			0x2A
/* #define TC_UNUSED			0x2B */
#define TC_VARIABLE			0x2C
#define TC_THE_ENVIRONMENT		0x2D
/* #define TC_UNUSED			0x2E */
#define TC_VECTOR_1B			0x2F
#define TC_PCOMB0			0x30
#define TC_VECTOR_16B			0x31
#define TC_REFERENCE_TRAP		0x32
#define TC_SEQUENCE_3			0x33
#define TC_CONDITIONAL			0x34
#define TC_DISJUNCTION			0x35
#define TC_CELL				0x36
#define TC_WEAK_CONS			0x37
#define TC_QUAD				0x38
#define TC_LINKAGE_SECTION		0x39
#define TC_RATNUM			0x3A
#define TC_STACK_ENVIRONMENT		0x3B
#define TC_COMPLEX			0x3C
#define TC_COMPILED_CODE_BLOCK		0x3D
#define TC_RECORD			0x3E
/* #define TC_UNUSED			0x3F */

/* If you add a new type, don't forget to update "gcloop.c"
   and TYPE_NAME_TABLE below. */

#define MIN_TYPE_CODE_LENGTH 6
#define TYPE_CODE_LIMIT (1 << MIN_TYPE_CODE_LENGTH)
#define LAST_TYPE_CODE (TYPE_CODE_LIMIT - 1)

#if defined (TYPE_CODE_LENGTH) && (TYPE_CODE_LENGTH < MIN_TYPE_CODE_LENGTH)
#  include ";; inconsistency: TYPE_CODE_LENGTH < MIN_TYPE_CODE_LENGTH"
#endif

#define TYPE_NAME_TABLE							\
{									\
  /* 0x00 */			"false",				\
  /* 0x01 */			"pair",					\
  /* 0x02 */			"character",				\
  /* 0x03 */			"quotation",				\
  /* 0x04 */			"primitive-combination-2",		\
  /* 0x05 */			"uninterned-symbol",			\
  /* 0x06 */			"flonum",				\
  /* 0x07 */			"combination-1",			\
  /* 0x08 */			"constant",				\
  /* 0x09 */			"extended-procedure",			\
  /* 0x0A */			"vector",				\
  /* 0x0B */			"return-code",				\
  /* 0x0C */			"combination-2",			\
  /* 0x0D */			"manifest-closure",			\
  /* 0x0E */			"bignum",				\
  /* 0x0F */			"procedure",				\
  /* 0x10 */			"entity",				\
  /* 0x11 */			"delay",				\
  /* 0x12 */			"environment",				\
  /* 0x13 */			"promise",				\
  /* 0x14 */			"extended-lambda",			\
  /* 0x15 */			"comment",				\
  /* 0x16 */			"non-marked-vector",			\
  /* 0x17 */			"lambda",				\
  /* 0x18 */			"primitive",				\
  /* 0x19 */			"sequence-2",				\
  /* 0x1A */			"fixnum",				\
  /* 0x1B */			"primitive-combination-1",		\
  /* 0x1C */			"control-point",			\
  /* 0x1D */			"interned-symbol",			\
  /* 0x1e */			"string",				\
  /* 0x1f */			"access",				\
  /* 0x20 */			"hunk3-a",				\
  /* 0x21 */			"definition",				\
  /* 0x22 */			"broken-heart",				\
  /* 0x23 */			"assignment",				\
  /* 0x24 */			"triple",				\
  /* 0x25 */			"in-package",				\
  /* 0x26 */			"combination",				\
  /* 0x27 */			"manifest-nm-vector",			\
  /* 0x28 */			"compiled-entry",			\
  /* 0x29 */			"lexpr",				\
  /* 0x2a */			"primitive-combination-3",		\
  /* 0x2b */			0,					\
  /* 0x2c */			"variable",				\
  /* 0x2d */			"the-environment",			\
  /* 0x2e */			0,					\
  /* 0x2f */			"vector-1b",				\
  /* 0x30 */			"primitive-combination-0",		\
  /* 0x31 */			"vector-16b",				\
  /* 0x32 */			"reference-trap",			\
  /* 0x33 */			"sequence-3",				\
  /* 0x34 */			"conditional",				\
  /* 0x35 */			"disjunction",				\
  /* 0x36 */			"cell",					\
  /* 0x37 */			"weak-cons",				\
  /* 0x38 */			"quad",					\
  /* 0x39 */			"linkage-section",			\
  /* 0x3a */			"ratnum",				\
  /* 0x3b */			"stack-environment",			\
  /* 0x3c */			"recnum",				\
  /* 0x3d */			"compiled-code-block",			\
  /* 0x3e */			"record",				\
  /* 0x3f */			0					\
}

/* Aliases */

#define TC_FALSE			TC_NULL
#define TC_MANIFEST_VECTOR		TC_NULL
#define TC_BIT_STRING			TC_VECTOR_1B
#define TC_VECTOR_8B			TC_CHARACTER_STRING
#define TC_HUNK3			TC_HUNK3_B

#define UNMARKED_HISTORY_TYPE		TC_HUNK3_A
#define MARKED_HISTORY_TYPE		TC_HUNK3_B
