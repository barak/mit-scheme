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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/fasl.h,v 9.22 1987/03/12 14:51:36 jinx Exp $

   Contains information relating to the format of FASL files.
   Some information is contained in CONFIG.H.
*/

/* FASL Version */

#define FASL_FILE_MARKER	0XFAFAFAFA
#define FASL_FORMAT_ADDED_STACK	1
#define FASL_FORMAT_VERSION	1
#define FASL_SUBVERSION		5

/* The FASL file has a header which begins as follows: */

#define FASL_HEADER_LENGTH	50	/* Scheme objects in header */
#define FASL_OLD_LENGTH		8	/* Size of header earlier */
#define FASL_Offset_Marker	0	/* Marker to indicate FASL format */
#define FASL_Offset_Heap_Count	1	/* Count of objects in heap */
#define FASL_Offset_Heap_Base	2	/* Address of heap when dumped */
#define FASL_Offset_Dumped_Obj	3	/* Where dumped object was */
#define FASL_Offset_Const_Count	4	/* Count of objects in const. area */
#define FASL_Offset_Const_Base	5	/* Address of const. area at dump */
#define FASL_Offset_Version	6	/* FASL format version info. */ 
#define FASL_Offset_Stack_Top	7	/* Top of stack when dumped */
#define FASL_Offset_Ext_Loc	8	/* Where ext. prims. vector is */

#define FASL_Offset_First_Free	9	/* Used to clear header */

/* Version information encoding */

#define MACHINE_TYPE_LENGTH (POINTER_LENGTH/2)
#define MACHINE_TYPE_MASK ((1<<MACHINE_TYPE_LENGTH)-1)
#define The_Machine_Type(P) ((P) & MACHINE_TYPE_MASK)
#define SUB_VERSION_LENGTH (MACHINE_TYPE_LENGTH-TYPE_CODE_LENGTH)
#define SUB_VERSION_MASK ((1<<SUB_VERSION_LENGTH)-1)
#define The_Sub_Version(P) (((P) >> MACHINE_TYPE_LENGTH) & SUB_VERSION_MASK)
#define The_Version(P) Type_Code(P)
#define Make_Version(V, S, M)					\
  Make_Non_Pointer((V), (((S) << MACHINE_TYPE_LENGTH) | (M)))

#define WRITE_FLAG		"w"
#define OPEN_FLAG		"r"

/* "Memorable" FASL sub-versions -- ones where we modified something
   and want to remain backwards compatible.
*/

#define FASL_LONG_HEADER	3
#define FASL_DENSE_TYPES	4
#define FASL_PADDED_STRINGS	5
#define FASL_OLDEST_SUPPORTED	5

#if 0
/* Old Type Codes -- used for conversion purposes
   This is no longer possible, because some were re-used
   without changing the fasl file version.
*/

#define OLD_TC_CHARACTER			0x40
#define OLD_TC_PCOMB2				0x44
#define OLD_TC_VECTOR				0x46
#define OLD_TC_RETURN_CODE			0x48
#define OLD_TC_COMPILED_PROCEDURE		0x49
#define OLD_TC_ENVIRONMENT			0x4E
#define OLD_TC_FIXNUM				0x50
#define OLD_TC_CONTROL_POINT			0x56
#define OLD_TC_BROKEN_HEART			0x58
#define OLD_TC_COMBINATION			0x5E
#define OLD_TC_MANIFEST_NM_VECTOR		0x60
#define OLD_TC_PCOMB3				0x66
#define OLD_TC_SPECIAL_NM_VECTOR 		0x68
#define OLD_TC_THE_ENVIRONMENT			0x70
#define OLD_TC_VECTOR_1B			0x76
#define OLD_TC_BIT_STRING			0x76
#define OLD_TC_PCOMB0				0x78
#define OLD_TC_VECTOR_16B			0x7E
#define OLD_TC_UNASSIGNED			0x38
#define OLD_TC_SEQUENCE_3			0x3C

#endif 0
