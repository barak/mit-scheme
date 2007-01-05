/* -*-C-*-

$Id: gctype.c,v 9.38 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* This file contains the table which maps between Types and GC Types.  */

#include "config.h"		/* for definition of TYPE_CODE_LENGTH */

	    /*********************************/
	    /* Mapping GC_Type to Type_Codes */
	    /*********************************/

int GC_Type_Map[MAX_TYPE_CODE + 1] = {
    GC_Non_Pointer,		/* TC_NULL,etc */
    GC_Pair,			/* TC_LIST */
    GC_Non_Pointer,		/* TC_CHARACTER */
    GC_Pair,		   	/* TC_SCODE_QUOTE */
    GC_Triple,		        /* TC_PCOMB2 */
    GC_Pair,			/* TC_UNINTERNED_SYMBOL */
    GC_Vector,			/* TC_BIG_FLONUM */
    GC_Pair,			/* TC_COMBINATION_1 */
    GC_Non_Pointer,		/* TC_CONSTANT */
    GC_Pair,			/* TC_EXTENDED_PROCEDURE */
    GC_Vector,			/* TC_VECTOR */
    GC_Non_Pointer,		/* TC_RETURN_CODE */
    GC_Triple,			/* TC_COMBINATION_2 */
    GC_Special,			/* TC_MANIFEST_CLOSURE */
    GC_Vector,			/* TC_BIG_FIXNUM */
    GC_Pair,			/* TC_PROCEDURE */
    GC_Pair,			/* TC_ENTITY */
    GC_Pair,			/* TC_DELAY */
    GC_Vector,			/* TC_ENVIRONMENT */
    GC_Pair,			/* TC_DELAYED */
    GC_Triple,			/* TC_EXTENDED_LAMBDA */
    GC_Pair,			/* TC_COMMENT */
    GC_Vector,			/* TC_NON_MARKED_VECTOR */
    GC_Pair,			/* TC_LAMBDA */
    GC_Non_Pointer,		/* TC_PRIMITIVE */
    GC_Pair,			/* TC_SEQUENCE_2 */
    GC_Non_Pointer,		/* TC_FIXNUM */
    GC_Pair,			/* TC_PCOMB1 */
    GC_Vector,			/* TC_CONTROL_POINT */
    GC_Pair,			/* TC_INTERNED_SYMBOL */
    GC_Vector,			/* TC_CHARACTER_STRING,TC_VECTOR_8B */
    GC_Pair,			/* TC_ACCESS */
    GC_Triple,			/* TC_HUNK3_A */
    GC_Pair,			/* TC_DEFINITION */
    GC_Special,			/* TC_BROKEN_HEART */
    GC_Pair,			/* TC_ASSIGNMENT */
    GC_Triple,			/* TC_HUNK3_B */
    GC_Pair,			/* TC_IN_PACKAGE */

/* GC_Type_Map continues on next page */

/* GC_Type_Map continued */

    GC_Vector,			/* TC_COMBINATION */
    GC_Special,			/* TC_MANIFEST_NM_VECTOR */
    GC_Compiled,		/* TC_COMPILED_ENTRY */
    GC_Pair,			/* TC_LEXPR */
    GC_Vector,			/* TC_PCOMB3 */
    GC_Special,			/* TC_MANIFEST_SPECIAL_NM_VECTOR */
    GC_Triple,			/* TC_VARIABLE */
    GC_Non_Pointer,		/* TC_THE_ENVIRONMENT */
    GC_Vector,			/* TC_FUTURE */
    GC_Vector,			/* TC_VECTOR_1B,TC_BIT_STRING */
    GC_Non_Pointer,		/* TC_PCOMB0 */
    GC_Vector,			/* TC_VECTOR_16B */
    GC_Special,			/* TC_REFERENCE_TRAP */
    GC_Triple,			/* TC_SEQUENCE_3 */
    GC_Triple,			/* TC_CONDITIONAL */
    GC_Pair,			/* TC_DISJUNCTION */
    GC_Cell,			/* TC_CELL */
    GC_Pair,			/* TC_WEAK_CONS */
    GC_Quadruple,		/* TC_QUAD */
    GC_Special,			/* TC_LINKAGE_SECTION */
    GC_Pair,			/* TC_RATNUM */
    GC_Non_Pointer,		/* TC_STACK_ENVIRONMENT */
    GC_Pair,			/* TC_COMPLEX */
    GC_Vector,			/* TC_COMPILED_CODE_BLOCK */
    GC_Vector,			/* TC_RECORD */

#if (TYPE_CODE_LENGTH == 6)

    GC_Undefined			/* 0x3F */

#else /* (TYPE_CODE_LENGTH != 6) */

    GC_Undefined,			/* 0x3F */
    GC_Undefined,			/* 0x40 */
    GC_Undefined,			/* 0x41 */
    GC_Undefined,			/* 0x42 */
    GC_Undefined,			/* 0x43 */
    GC_Undefined,			/* 0x44 */
    GC_Undefined,			/* 0x45 */
    GC_Undefined,			/* 0x46 */
    GC_Undefined,			/* 0x47 */
    GC_Undefined,			/* 0x48 */
    GC_Undefined,			/* 0x49 */
    GC_Undefined,			/* 0x4A */
    GC_Undefined,			/* 0x4B */
    GC_Undefined,			/* 0x4C */
    GC_Undefined,			/* 0x4D */
    GC_Undefined,			/* 0x4E */
    GC_Undefined,			/* 0x4F */
    GC_Undefined,			/* 0x50 */
    GC_Undefined,			/* 0x51 */
    GC_Undefined,			/* 0x52 */
    GC_Undefined,			/* 0x53 */
    GC_Undefined,			/* 0x54 */

/* GC_Type_Map continues on next page */

/* GC_Type_Map continued */

    GC_Undefined,			/* 0x55 */
    GC_Undefined,			/* 0x56 */
    GC_Undefined,			/* 0x57 */
    GC_Undefined,			/* 0x58 */
    GC_Undefined,			/* 0x59 */
    GC_Undefined,			/* 0x5A */
    GC_Undefined,			/* 0x5B */
    GC_Undefined,			/* 0x5C */
    GC_Undefined,			/* 0x5D */
    GC_Undefined,			/* 0x5E */
    GC_Undefined,			/* 0x5F */
    GC_Undefined,			/* 0x60 */
    GC_Undefined,			/* 0x61 */
    GC_Undefined,			/* 0x62 */
    GC_Undefined,			/* 0x63 */
    GC_Undefined,			/* 0x64 */
    GC_Undefined,			/* 0x65 */
    GC_Undefined,			/* 0x66 */
    GC_Undefined,			/* 0x67 */
    GC_Undefined,			/* 0x68 */
    GC_Undefined,			/* 0x69 */
    GC_Undefined,			/* 0x6A */
    GC_Undefined,			/* 0x6B */
    GC_Undefined,			/* 0x6C */
    GC_Undefined,			/* 0x6D */
    GC_Undefined,			/* 0x6E */
    GC_Undefined,			/* 0x6F */
    GC_Undefined,			/* 0x70 */
    GC_Undefined,			/* 0x71 */
    GC_Undefined,			/* 0x72 */
    GC_Undefined,			/* 0x73 */
    GC_Undefined,			/* 0x74 */
    GC_Undefined,			/* 0x75 */
    GC_Undefined,			/* 0x76 */
    GC_Undefined,			/* 0x77 */
    GC_Undefined,			/* 0x78 */
    GC_Undefined,			/* 0x79 */
    GC_Undefined,			/* 0x7A */
    GC_Undefined,			/* 0x7B */
    GC_Undefined,			/* 0x7C */
    GC_Undefined,			/* 0x7D */
    GC_Undefined,			/* 0x7E */
    GC_Undefined,			/* 0x7F */

    GC_Undefined,			/* 0x80 */
    GC_Undefined,			/* 0x81 */
    GC_Undefined,			/* 0x82 */
    GC_Undefined,			/* 0x83 */
    GC_Undefined,			/* 0x84 */
    GC_Undefined,			/* 0x85 */
    GC_Undefined,			/* 0x86 */
    GC_Undefined,			/* 0x87 */
    GC_Undefined,			/* 0x88 */
    GC_Undefined,			/* 0x89 */
    GC_Undefined,			/* 0x8A */
    GC_Undefined,			/* 0x8B */
    GC_Undefined,			/* 0x8C */
    GC_Undefined,			/* 0x8D */
    GC_Undefined,			/* 0x8E */
    GC_Undefined,			/* 0x8F */
    GC_Undefined,			/* 0x90 */
    GC_Undefined,			/* 0x91 */
    GC_Undefined,			/* 0x92 */
    GC_Undefined,			/* 0x93 */
    GC_Undefined,			/* 0x94 */
    GC_Undefined,			/* 0x95 */
    GC_Undefined,			/* 0x96 */
    GC_Undefined,			/* 0x97 */
    GC_Undefined,			/* 0x98 */
    GC_Undefined,			/* 0x99 */
    GC_Undefined,			/* 0x9A */
    GC_Undefined,			/* 0x9B */
    GC_Undefined,			/* 0x9C */
    GC_Undefined,			/* 0x9D */
    GC_Undefined,			/* 0x9E */
    GC_Undefined,			/* 0x9F */
    GC_Undefined,			/* 0xA0 */
    GC_Undefined,			/* 0xA1 */
    GC_Undefined,			/* 0xA2 */
    GC_Undefined,			/* 0xA3 */
    GC_Undefined,			/* 0xA4 */
    GC_Undefined,			/* 0xA5 */
    GC_Undefined,			/* 0xA6 */
    GC_Undefined,			/* 0xA7 */
    GC_Undefined,			/* 0xA8 */
    GC_Undefined,			/* 0xA9 */
    GC_Undefined,			/* 0xAA */
    GC_Undefined,			/* 0xAB */
    GC_Undefined,			/* 0xAC */
    GC_Undefined,			/* 0xAD */
    GC_Undefined,			/* 0xAE */
    GC_Undefined,			/* 0xAF */

    GC_Undefined,			/* 0xB0 */
    GC_Undefined,			/* 0xB1 */
    GC_Undefined,			/* 0xB2 */
    GC_Undefined,			/* 0xB3 */
    GC_Undefined,			/* 0xB4 */
    GC_Undefined,			/* 0xB5 */
    GC_Undefined,			/* 0xB6 */
    GC_Undefined,			/* 0xB7 */
    GC_Undefined,			/* 0xB8 */
    GC_Undefined,			/* 0xB9 */
    GC_Undefined,			/* 0xBA */
    GC_Undefined,			/* 0xBB */
    GC_Undefined,			/* 0xBC */
    GC_Undefined,			/* 0xBD */
    GC_Undefined,			/* 0xBE */
    GC_Undefined,			/* 0xBF */
    GC_Undefined,			/* 0xC0 */
    GC_Undefined,			/* 0xC1 */
    GC_Undefined,			/* 0xC2 */
    GC_Undefined,			/* 0xC3 */
    GC_Undefined,			/* 0xC4 */
    GC_Undefined,			/* 0xC5 */
    GC_Undefined,			/* 0xC6 */
    GC_Undefined,			/* 0xC7 */
    GC_Undefined,			/* 0xC8 */
    GC_Undefined,			/* 0xC9 */
    GC_Undefined,			/* 0xCA */
    GC_Undefined,			/* 0xCB */
    GC_Undefined,			/* 0xCC */
    GC_Undefined,			/* 0xCD */
    GC_Undefined,			/* 0xCE */
    GC_Undefined,			/* 0xCF */
    GC_Undefined,			/* 0xD0 */
    GC_Undefined,			/* 0xD1 */
    GC_Undefined,			/* 0xD2 */
    GC_Undefined,			/* 0xD3 */
    GC_Undefined,			/* 0xD4 */
    GC_Undefined,			/* 0xD5 */
    GC_Undefined,			/* 0xD6 */
    GC_Undefined,			/* 0xD7 */
    GC_Undefined,			/* 0xD8 */
    GC_Undefined,			/* 0xD9 */
    GC_Undefined,			/* 0xDA */
    GC_Undefined,			/* 0xDB */
    GC_Undefined,			/* 0xDC */
    GC_Undefined,			/* 0xDD */
    GC_Undefined,			/* 0xDE */
    GC_Undefined,			/* 0xDF */

    GC_Undefined,			/* 0xE0 */
    GC_Undefined,			/* 0xE1 */
    GC_Undefined,			/* 0xE2 */
    GC_Undefined,			/* 0xE3 */
    GC_Undefined,			/* 0xE4 */
    GC_Undefined,			/* 0xE5 */
    GC_Undefined,			/* 0xE6 */
    GC_Undefined,			/* 0xE7 */
    GC_Undefined,			/* 0xE8 */
    GC_Undefined,			/* 0xE9 */
    GC_Undefined,			/* 0xEA */
    GC_Undefined,			/* 0xEB */
    GC_Undefined,			/* 0xEC */
    GC_Undefined,			/* 0xED */
    GC_Undefined,			/* 0xEE */
    GC_Undefined,			/* 0xEF */
    GC_Undefined,			/* 0xF0 */
    GC_Undefined,			/* 0xF1 */
    GC_Undefined,			/* 0xF2 */
    GC_Undefined,			/* 0xF3 */
    GC_Undefined,			/* 0xF4 */
    GC_Undefined,			/* 0xF5 */
    GC_Undefined,			/* 0xF6 */
    GC_Undefined,			/* 0xF7 */
    GC_Undefined,			/* 0xF8 */
    GC_Undefined,			/* 0xF9 */
    GC_Undefined,			/* 0xFA */
    GC_Undefined,			/* 0xFB */
    GC_Undefined,			/* 0xFC */
    GC_Undefined,			/* 0xFD */
    GC_Undefined,			/* 0xFE */
    GC_Undefined			/* last */
#endif /* (TYPE_CODE_LENGTH != 6) */

    };

#if (TYPE_CODE_LENGTH == 6)

#if (MAX_TYPE_CODE != 0x3F)
#include "gctype.c and object.h inconsistent -- GC_Type_Map"
#endif

#else /* (TYPE_CODE_LENGTH != 6) */

#if (MAX_TYPE_CODE != 0xFF)
#include "gctype.c and object.h inconsistent -- GC_Type_Map"
#endif

#endif /* (TYPE_CODE_LENGTH == 6) */
