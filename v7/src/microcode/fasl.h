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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fasl.h,v 9.23 1987/04/03 00:12:15 jinx Exp $

   Contains information relating to the format of FASL files.
   Some information is contained in CONFIG.H.
*/

/* FASL Version */

#define FASL_FILE_MARKER	0XFAFAFAFA

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

/* "Memorable" FASL versions -- ones where we modified something
   and want to remain backwards compatible.
*/

/* Versions. */

#define FASL_FORMAT_ADDED_STACK	1

/* Subversions of highest numbered version. */

#define FASL_LONG_HEADER	3
#define FASL_DENSE_TYPES	4
#define FASL_PADDED_STRINGS	5
#define FASL_REFERENCE_TRAP	6

/* Current parameters. */

#define FASL_FORMAT_VERSION	FASL_FORMAT_ADDED_STACK
#define FASL_SUBVERSION		FASL_REFERENCE_TRAP
#define FASL_OLDEST_SUPPORTED	FASL_PADDED_STRINGS
