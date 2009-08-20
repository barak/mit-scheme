/* -*-C-*-

$Id$

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* Type code definitions, alphabetical order */

/*      Name                                    Value */
#define TC_ACCESS                               0x2C
#define TC_ASSIGNMENT                           0x3C
#define TC_BIG_FIXNUM                           0x1
#define TC_BIG_FLONUM                           0x3E
#define TC_BROKEN_HEART                         0x11
#define TC_CELL                                 0xB
#define TC_CHARACTER                            0x7
#define TC_CHARACTER_STRING                     0x1A
#define TC_COMBINATION                          0x33
#define TC_COMBINATION_1                        0x34
#define TC_COMBINATION_2                        0x35
#define TC_COMMENT                              0x29
#define TC_COMPILED_CODE_BLOCK                  0x1D
#define TC_COMPILED_ENTRY                       0x10
#define TC_COMPLEX                              0x3
#define TC_CONDITIONAL                          0x31
#define TC_CONSTANT                             0x8
#define TC_CONTROL_POINT                        0x1F
#define TC_DEFINITION                           0x3B
#define TC_DELAY                                0x3A
#define TC_DELAYED                              0x26
#define TC_DISJUNCTION                          0x32
#define TC_ENTITY                               0x24
#define TC_ENVIRONMENT                          0x25
#define TC_EXTENDED_LAMBDA                      0x2E
#define TC_EXTENDED_PROCEDURE                   0x22
#define TC_FUTURE                               0x27
#define TC_HUNK3_A                              0x12
#define TC_HUNK3_B                              0x13
#define TC_INTERNED_SYMBOL                      0xF
#define TC_IN_PACKAGE                           0x28
#define TC_LAMBDA                               0x2D
#define TC_LEXPR                                0x23
#define TC_LINKAGE_SECTION                      0x1E
#define TC_LIST                                 0xC
#define TC_MANIFEST_CLOSURE                     0xA
#define TC_MANIFEST_NM_VECTOR                   0x6
#define TC_MANIFEST_SPECIAL_NM_VECTOR           0x15
#define TC_NEGATIVE_FIXNUM                      0x3F
#define TC_NON_MARKED_VECTOR                    0x16
#define TC_NULL                                 0x5
#define TC_PCOMB0                               0x36
#define TC_PCOMB1                               0x37
#define TC_PCOMB2                               0x38
#define TC_PCOMB3                               0x39
#define TC_POSITIVE_FIXNUM                      0x0
#define TC_PRIMITIVE                            0x9
#define TC_PROCEDURE                            0x21
#define TC_QUAD                                 0x14
#define TC_RATNUM                               0x2
#define TC_RECORD                               0x18
#define TC_REFERENCE_TRAP                       0x1C
#define TC_RETURN_CODE                          0x4
#define TC_SCODE_QUOTE                          0x2A
#define TC_SEQUENCE_2                           0x2F
#define TC_SEQUENCE_3                           0x30
#define TC_STACK_ENVIRONMENT                    0x20
#define TC_THE_ENVIRONMENT                      0x3D
#define TC_UNINTERNED_SYMBOL                    0xE
#define TC_VARIABLE                             0x2B
#define TC_VECTOR                               0x17
#define TC_VECTOR_16B                           0x1B
#define TC_VECTOR_1B                            0x19
#define TC_WEAK_CONS                            0xD

/* If you add a new type, don't forget to update gccode.h, gctype.c,
   and the type name table below. */

#define LAST_TYPE_CODE   0x3F
#define MIN_TYPE_CODE_LENGTH   6

#ifdef TYPE_CODE_LENGTH
#if (TYPE_CODE_LENGTH < MIN_TYPE_CODE_LENGTH)
#include ";; inconsistency between object.h and types.h: MIN_TYPE_CODE_LENGTH"
#endif
#endif

#define TYPE_NAME_TABLE                                                \
{ /* 8bit 6bit */\
  /* 0x00 0x00 */   "POSITIVE-FIXNUM",\
  /* 0x01 0x04 */   "BIGNUM",\
  /* 0x02 0x08 */   "RATNUM",\
  /* 0x03 0x0C */   "RECNUM",\
  /* 0x04 0x10 */   "RETURN-CODE",\
  /* 0x05 0x14 */   "NULL",\
  /* 0x06 0x18 */   "MANIFEST-NM-VECTOR",\
  /* 0x07 0x1C */   "CHARACTER",\
  /* 0x08 0x20 */   "CONSTANT",\
  /* 0x09 0x24 */   "PRIMITIVE",\
  /* 0x0A 0x28 */   "MANIFEST-CLOSURE",\
  /* 0x0B 0x2C */   "CELL",\
  /* 0x0C 0x30 */   "PAIR",\
  /* 0x0D 0x34 */   "WEAK-CONS",\
  /* 0x0E 0x38 */   "UNINTERNED-SYMBOL",\
  /* 0x0F 0x3C */   "INTERNED-SYMBOL",\
  /* 0x10 0x40 */   "COMPILED-ENTRY",\
  /* 0x11 0x44 */   "BROKEN-HEART",\
  /* 0x12 0x48 */   "HUNK3-A",\
  /* 0x13 0x4C */   "TRIPLE",\
  /* 0x14 0x50 */   "QUAD",\
  /* 0x15 0x54 */   "MANIFEST-SPECIAL-NM-VECTOR",\
  /* 0x16 0x58 */   "NON-MARKED-VECTOR",\
  /* 0x17 0x5C */   "VECTOR",\
  /* 0x18 0x60 */   "RECORD",\
  /* 0x19 0x64 */   "VECTOR-1B",\
  /* 0x1A 0x68 */   "STRING",\
  /* 0x1B 0x6C */   "VECTOR-16B",\
  /* 0x1C 0x70 */   "REFERENCE-TRAP",\
  /* 0x1D 0x74 */   "COMPILED-CODE-BLOCK",\
  /* 0x1E 0x78 */   "LINKAGE-SECTION",\
  /* 0x1F 0x7C */   "CONTROL-POINT",\
  /* 0x20 0x80 */   "STACK-ENVIRONMENT",\
  /* 0x21 0x84 */   "PROCEDURE",\
  /* 0x22 0x88 */   "EXTENDED-PROCEDURE",\
  /* 0x23 0x8C */   "LEXPR",\
  /* 0x24 0x90 */   "ENTITY",\
  /* 0x25 0x94 */   "ENVIRONMENT",\
  /* 0x26 0x98 */   "PROMISE",\
  /* 0x27 0x9C */   "FUTURE",\
  /* 0x28 0xA0 */   "IN-PACKAGE",\
  /* 0x29 0xA4 */   "COMMENT",\
  /* 0x2A 0xA8 */   "QUOTATION",\
  /* 0x2B 0xAC */   "VARIABLE",\
  /* 0x2C 0xB0 */   "ACCESS",\
  /* 0x2D 0xB4 */   "LAMBDA",\
  /* 0x2E 0xB8 */   "EXTENDED-LAMBDA",\
  /* 0x2F 0xBC */   "SEQUENCE-2",\
  /* 0x30 0xC0 */   "SEQUENCE-3",\
  /* 0x31 0xC4 */   "CONDITIONAL",\
  /* 0x32 0xC8 */   "DISJUNCTION",\
  /* 0x33 0xCC */   "COMBINATION",\
  /* 0x34 0xD0 */   "COMBINATION-1",\
  /* 0x35 0xD4 */   "COMBINATION-2",\
  /* 0x36 0xD8 */   "PRIMITIVE-COMBINATION-0",\
  /* 0x37 0xDC */   "PRIMITIVE-COMBINATION-1",\
  /* 0x38 0xE0 */   "PRIMITIVE-COMBINATION-2",\
  /* 0x39 0xE4 */   "PRIMITIVE-COMBINATION-3",\
  /* 0x3A 0xE8 */   "DELAY",\
  /* 0x3B 0xEC */   "DEFINITION",\
  /* 0x3C 0xF0 */   "ASSIGNMENT",\
  /* 0x3D 0xF4 */   "THE-ENVIRONMENT",\
  /* 0x3E 0xF8 */   "FLONUM",\
  /* 0x3F 0xFC */   "NEGATIVE-FIXNUM"\
  }

/* Flags and aliases */

/* Type code 0x10 (used to be TC_PRIMITIVE_EXTERNAL) has been reused. */

#define PRIMITIVE_EXTERNAL_REUSED

/* Aliases */

#define TC_FALSE                        TC_NULL
#define TC_MANIFEST_VECTOR              TC_NULL
#define TC_BIT_STRING                   TC_VECTOR_1B
#define TC_VECTOR_8B                    TC_CHARACTER_STRING
#define TC_HUNK3                        TC_HUNK3_B

#ifndef TC_NEGATIVE_FIXNUM
#define TC_NEGATIVE_FIXNUM              TC_POSITIVE_FIXNUM
#define case_TC_FIXNUMs case TC_POSITIVE_FIXNUM
#else
#define case_TC_FIXNUMs case TC_POSITIVE_FIXNUM: case TC_NEGATIVE_FIXNUM
#endif

#define UNMARKED_HISTORY_TYPE           TC_HUNK3_A
#define MARKED_HISTORY_TYPE             TC_HUNK3_B
