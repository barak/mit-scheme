/* -*-C-*-

$Id: load.c,v 9.42 2006/06/03 08:00:15 ihtfisp Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* This file contains common code for reading internal
   format binary files. */

#include "outf.h"	/* Formatted output for errors */

#include "fasl.h"

#define FASL_FILE_FINE			0
#define FASL_FILE_TOO_SHORT		1
#define FASL_FILE_NOT_FASL		2
#define FASL_FILE_BAD_MACHINE		3
#define FASL_FILE_BAD_VERSION		4
#define FASL_FILE_BAD_SUBVERSION	5
#define FASL_FILE_BAD_PROCESSOR		6
#define FASL_FILE_BAD_INTERFACE		7

#ifndef BYTE_INVERSION

#define NORMALIZE_HEADER(header, size, base, count)
#define NORMALIZE_REGION(region, size)

#else /* BYTE_INVERSION */

void
  EXFUN (Byte_Invert_Region, (long *, long)),
  EXFUN (Byte_Invert_Header, (long *, long, long, long));

#define NORMALIZE_HEADER Byte_Invert_Header
#define NORMALIZE_REGION Byte_Invert_Region

#endif /* BYTE_INVERSION */

/* Static storage for some shared variables */

static Boolean
  band_p;

static long
  Machine_Type, Version, Sub_Version,
  Dumped_Object, Dumped_Stack_Top,
  Heap_Base, Heap_Count,
  Const_Base, Const_Count,
  Dumped_Heap_Top, Dumped_Constant_Top,
  Primitive_Table_Size, Primitive_Table_Length,
  C_Code_Table_Size, C_Code_Table_Length,
  dumped_processor_type, dumped_interface_version,
  dumped_memory_base;

static unsigned long dumped_checksum;

#ifndef INHIBIT_CHECKSUMS
static unsigned long computed_checksum;
#endif /* INHIBIT_CHECKSUMS */


static SCHEME_OBJECT
  Ext_Prim_Vector,
  dumped_utilities;

void
DEFUN_VOID (print_fasl_information)
{
  printf ("FASL File Information:\n\n");
  printf ("Machine = %ld; Version = %ld; Subversion = %ld\n",
	  Machine_Type, Version, Sub_Version);
  if ((dumped_processor_type != 0) || (dumped_interface_version != 0))
    printf ("Compiled code interface version = %ld; Processor type = %ld\n",
	    dumped_interface_version, dumped_processor_type);
  if (band_p)
    printf ("The file contains a dumped image (band).\n");

  printf ("\nRelocation Information:\n\n");
  printf ("Heap Count = %ld; Heap Base = 0x%lx; Heap Top = 0x%lx\n",
	  Heap_Count, Heap_Base, Dumped_Heap_Top);
  printf ("Const Count = %ld; Const Base = 0x%lx; Const Top = 0x%lx\n",
	  Const_Count, Const_Base, Dumped_Constant_Top);
  printf ("Stack Top = 0x%lx\n", Dumped_Stack_Top);

  printf ("\nDumped Objects:\n\n");
  printf ("Dumped object at 0x%lx (as read from file)\n", Dumped_Object);
  printf ("Compiled code utilities vector = 0x%lx\n", dumped_utilities);
  if (Ext_Prim_Vector != SHARP_F)
    printf ("External primitives vector = 0x%lx\n", Ext_Prim_Vector);
  else
    printf ("Length of primitive table = %ld\n", Primitive_Table_Length);
  printf ("Length of C table = %ld\n", C_Code_Table_Length);
  printf ("Checksum = 0x%lx\n", dumped_checksum);
  return;
}

long
DEFUN (initialize_variables_from_fasl_header, (buffer),
       SCHEME_OBJECT * buffer)
{
  SCHEME_OBJECT Pointer_Heap_Base, Pointer_Const_Base;

  if (buffer[FASL_Offset_Marker] != FASL_FILE_MARKER)
    return (FASL_FILE_NOT_FASL);

  NORMALIZE_HEADER (buffer,
		    (sizeof(buffer) / sizeof(SCHEME_OBJECT)),
		    buffer[FASL_Offset_Heap_Base],
		    buffer[FASL_Offset_Heap_Count]);
  Heap_Count = OBJECT_DATUM (buffer[FASL_Offset_Heap_Count]);
  Pointer_Heap_Base = buffer[FASL_Offset_Heap_Base];
  Heap_Base = OBJECT_DATUM (Pointer_Heap_Base);
  Dumped_Object = OBJECT_DATUM (buffer[FASL_Offset_Dumped_Obj]);
  Const_Count = OBJECT_DATUM (buffer[FASL_Offset_Const_Count]);
  Pointer_Const_Base = buffer[FASL_Offset_Const_Base];
  Const_Base = OBJECT_DATUM (Pointer_Const_Base);
  Version = The_Version(buffer[FASL_Offset_Version]);
  Sub_Version = The_Sub_Version(buffer[FASL_Offset_Version]);
  Machine_Type = The_Machine_Type(buffer[FASL_Offset_Version]);
  Dumped_Stack_Top = OBJECT_DATUM (buffer[FASL_Offset_Stack_Top]);
  Dumped_Heap_Top =
    ADDRESS_TO_DATUM (MEMORY_LOC (Pointer_Heap_Base, Heap_Count));
  Dumped_Constant_Top =
    ADDRESS_TO_DATUM (MEMORY_LOC (Pointer_Const_Base, Const_Count));

  if (Version == FASL_FORMAT_ADDED_STACK && Sub_Version < FASL_MERGED_PRIMITIVES)
  {
    Primitive_Table_Length = 0;
    Primitive_Table_Size = 0;
    Ext_Prim_Vector =
      (OBJECT_NEW_TYPE (TC_CELL, (buffer [FASL_Offset_Ext_Loc])));
  }
  else
  {
    Primitive_Table_Length = (OBJECT_DATUM (buffer[FASL_Offset_Prim_Length]));
    Primitive_Table_Size = (OBJECT_DATUM (buffer[FASL_Offset_Prim_Size]));
    Ext_Prim_Vector = SHARP_F;
  }

  if (Version == FASL_FORMAT_ADDED_STACK && Sub_Version < FASL_INTERFACE_VERSION)
  {
    /* This may be all wrong, but... */
    band_p = false;
    dumped_processor_type = 0;
    dumped_interface_version = 0;
    dumped_utilities = SHARP_F;
  }
  else
  {
    SCHEME_OBJECT temp = buffer[FASL_Offset_Ci_Version];

    band_p = (CI_BAND_P (temp));
    dumped_processor_type = (CI_PROCESSOR (temp));
    dumped_interface_version = (CI_VERSION (temp));
    dumped_utilities = buffer[FASL_Offset_Ut_Base];
  }

  if (Version == FASL_FORMAT_ADDED_STACK && Sub_Version < FASL_C_CODE)
  {
    C_Code_Table_Length = 0;
    C_Code_Table_Size = 0;
  }
  else
  {
    C_Code_Table_Length = (OBJECT_DATUM (buffer[FASL_Offset_C_Length]));
    C_Code_Table_Size = (OBJECT_DATUM (buffer[FASL_Offset_C_Size]));
  }
  dumped_memory_base = ((long) buffer[FASL_Offset_Mem_Base]);

#ifndef INHIBIT_FASL_VERSION_CHECK
  /* The error messages here should be handled by the runtime system! */

  if ((Version != FASL_READ_VERSION) ||
#ifndef BYTE_INVERSION
      (Machine_Type != FASL_INTERNAL_FORMAT) ||
#endif
      (Sub_Version < FASL_READ_SUBVERSION) ||
      (Sub_Version > FASL_SUBVERSION))
  {
    outf_error ("\nread_file:\n");
    outf_error ("FASL File: Version %4d Subversion %4d Machine Type %4d.\n",
		Version, Sub_Version , Machine_Type);
    outf_error ("Expected:  Version %4d Subversion %4d Machine Type %4d.\n",
		FASL_READ_VERSION, FASL_READ_SUBVERSION, FASL_INTERNAL_FORMAT);

    return ((Machine_Type != FASL_INTERNAL_FORMAT)	?
	    FASL_FILE_BAD_MACHINE			:
	    ((Version != FASL_READ_VERSION)		?
	     FASL_FILE_BAD_VERSION			:
	     FASL_FILE_BAD_SUBVERSION));
  }

#endif /* INHIBIT_FASL_VERSION_CHECK */

#ifndef INHIBIT_COMPILED_VERSION_CHECK

  /* Is the compiled code "loadable" here? */

  {
    extern long compiler_processor_type, compiler_interface_version;

    if (((dumped_processor_type != 0) &&
	(dumped_processor_type != compiler_processor_type)) ||
	((dumped_interface_version != 0) &&
	 (dumped_interface_version != compiler_interface_version)))
    {
      outf_error ("\nread_file:\n");
      outf_error ("FASL File: compiled code interface %4d; processor %4d.\n",
		  dumped_interface_version, dumped_processor_type);
      outf_error ("Expected:  compiled code interface %4d; processor %4d.\n",
		  compiler_interface_version, compiler_processor_type);
      return (((dumped_processor_type != 0) &&
	       (dumped_processor_type != compiler_processor_type))	?
	      FASL_FILE_BAD_PROCESSOR					:
	      FASL_FILE_BAD_INTERFACE);
    }
  }

#endif /* INHIBIT_COMPILED_VERSION_CHECK */

  dumped_checksum = (buffer [FASL_Offset_Check_Sum]);

#ifndef INHIBIT_CHECKSUMS

  {
    extern unsigned long
      EXFUN (checksum_area, (unsigned long *, long, unsigned long));

    computed_checksum =
      (checksum_area (((unsigned long *) &buffer[0]),
		      ((long) (FASL_HEADER_LENGTH)),
		      ((unsigned long) 0)));
  }

#endif /* INHIBIT_CHECKSUMS */

  return (FASL_FILE_FINE);
}

long
DEFUN_VOID (Read_Header)
{
  SCHEME_OBJECT header[FASL_HEADER_LENGTH];

  if ((Load_Data (FASL_HEADER_LENGTH, header)) !=
      FASL_HEADER_LENGTH)
    return (FASL_FILE_TOO_SHORT);
  return (initialize_variables_from_fasl_header (&header[0]));
}

#ifdef HEAP_IN_LOW_MEMORY

#define SCHEME_ADDR_TO_OLD_DATUM(addr)					\
  (ADDRESS_TO_DATUM (SCHEME_ADDR_TO_ADDR ((SCHEME_OBJECT *) (addr))))

#else /* not HEAP_IN_LOW_MEMORY */

#define SCHEME_ADDR_TO_OLD_DATUM(addr)					\
  (((SCHEME_OBJECT *) (addr)) - ((SCHEME_OBJECT *) dumped_memory_base))

#endif /* HEAP_IN_LOW_MEMORY */

#ifdef BYTE_INVERSION

static Boolean Byte_Invert_Fasl_Files;

void
DEFUN (Byte_Invert_Header, (Header, Headsize, Test1, Test2),
       long * Header
       AND long Headsize
       AND long Test1
       AND long Test2)
{
  Byte_Invert_Fasl_Files = false;

  if ((Test1 & 0xff) == TC_BROKEN_HEART &&
      (Test2 & 0xff) == TC_BROKEN_HEART &&
      (OBJECT_TYPE (Test1) != TC_BROKEN_HEART ||
       OBJECT_TYPE (Test2) != TC_BROKEN_HEART))
  {
    Byte_Invert_Fasl_Files = true;
    Byte_Invert_Region(Header, Headsize);
  }
  return;
}

void
DEFUN (Byte_Invert_Region, (Region, Size),
       long * Region
       AND long Size)
{
  register long word, size;

  if (Byte_Invert_Fasl_Files)
  {
    for (size = Size; size > 0; size--, Region++)
    {
      word = (*Region);
      *Region = (((word>>24)&0xff) | ((word>>8)&0xff00) |
		 ((word<<8)&0xff0000) | ((word<<24)&0xff000000));
    }
  }
  return;
}

#endif /* BYTE_INVERSION */
