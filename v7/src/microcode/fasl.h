/* -*-C-*-

$Id: fasl.h,v 9.39 2002/11/20 19:46:08 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* Contains information relating to the format of FASL files.
   The machine/opsys information is contained in config.h
   The processor and compiled code version information is
   contained in the appropriate cmp* file, or compiler.c */

/* FASL Version */

#define FASL_FILE_MARKER	0xFAFAFAFA

/* The FASL file has a header which begins as follows: */

#define FASL_HEADER_LENGTH	50	/* Scheme objects in header */

#define FASL_Offset_Marker	0	/* Marker to indicate FASL format */
#define FASL_Offset_Heap_Count	1	/* Count of objects in heap */
#define FASL_Offset_Heap_Base	2	/* Address of heap when dumped */
#define FASL_Offset_Dumped_Obj	3	/* Where dumped object was */
#define FASL_Offset_Const_Count	4	/* Count of objects in const. area */
#define FASL_Offset_Const_Base	5	/* Address of const. area at dump */
#define FASL_Offset_Version	6	/* FASL format version info. */
#define FASL_Offset_Stack_Top	7	/* Top of stack when dumped */
#define FASL_Offset_Prim_Length 8	/* Number of entries in primitive table */
#define FASL_Offset_Prim_Size	9	/* Size of primitive table in SCHEME_OBJECTs */
#define FASL_Offset_Ci_Version	10	/* Version number for compiled code interface */
#define FASL_Offset_Ut_Base	11	/* Address of the utilities vector */
#define FASL_Offset_Check_Sum	12	/* Header and data checksum. */
#define FASL_Offset_C_Length	13	/* Number of entries in the C code table */
#define FASL_Offset_C_Size	14	/* Size of C code table in SCHEME_OBJECTs */
#define FASL_Offset_Mem_Base	15	/* Base address when not HEAP_IN_LOW_MEMORY */

#define FASL_Offset_First_Free	16	/* Used to clear header */

/* Aliases for backwards compatibility. */

/* Where ext. prims. vector is */
#define FASL_Offset_Ext_Loc	FASL_Offset_Prim_Length

/* Version information encoding */

#define ONE			((SCHEME_OBJECT) 1)

#define MACHINE_TYPE_LENGTH	(OBJECT_LENGTH / 2)
#define MACHINE_TYPE_MASK	((ONE << MACHINE_TYPE_LENGTH) - 1)
#define The_Machine_Type(P)	((P) & MACHINE_TYPE_MASK)
#define SUBVERSION_LENGTH	(MACHINE_TYPE_LENGTH - TYPE_CODE_LENGTH)
#define SUBVERSION_MASK		((ONE << SUBVERSION_LENGTH) - 1)
#define The_Sub_Version(P)	(((P) >> MACHINE_TYPE_LENGTH) & SUBVERSION_MASK)
#define The_Version(P)		(OBJECT_TYPE (P))

#define Make_Version(V, S, M)						\
  MAKE_OBJECT ((V), ((((unsigned long) (S)) << MACHINE_TYPE_LENGTH)	\
		     | (M)))						\

#define CI_MASK			((ONE << (DATUM_LENGTH / 2)) - 1)
#define CI_VERSION(P)		(((P) >> (DATUM_LENGTH / 2)) & CI_MASK)
#define CI_PROCESSOR(P)		((P) & CI_MASK)
#define CI_BAND_P(P)		((OBJECT_TYPE (P)) == TC_CONSTANT)

#define MAKE_CI_VERSION(Band_p, Version, Processor_Type)		\
  MAKE_OBJECT (((Band_p) ? TC_CONSTANT : TC_NULL),				\
	       ((((unsigned long) (Version)) << (DATUM_LENGTH / 2))	\
		| (Processor_Type)))

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
#define FASL_MERGED_PRIMITIVES	7
#define FASL_INTERFACE_VERSION	8
#define FASL_NEW_BIGNUMS	9
#define FASL_C_CODE		10

/* Current parameters.  Always used on output. */

#define FASL_FORMAT_VERSION	FASL_FORMAT_ADDED_STACK
#define FASL_SUBVERSION		FASL_C_CODE

/*
  The definitions below correspond to the ones above.  They usually
  have the same values.  They differ when the format is changing: A
  system can be built which reads the old format, but dumps the new one.
 */

#ifndef FASL_READ_VERSION
#define FASL_READ_VERSION	FASL_FORMAT_ADDED_STACK
#endif

#ifndef FASL_READ_SUBVERSION
#define FASL_READ_SUBVERSION	FASL_NEW_BIGNUMS
#endif

/* These are for Bintopsb.
   They are the values of the oldest supported formats.
 */

#define FASL_OLDEST_VERSION	FASL_FORMAT_ADDED_STACK
#define FASL_OLDEST_SUBVERSION	FASL_PADDED_STRINGS
