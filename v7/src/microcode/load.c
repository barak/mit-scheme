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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/load.c,v 9.26 1988/02/10 15:43:53 jinx Rel $
 *
 * This file contains common code for reading internal
 * format binary files.
 *
 */

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

void Byte_Invert_Region(), Byte_Invert_Header();

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
  dumped_processor_type, dumped_interface_version;

static Pointer
  Ext_Prim_Vector,
  dumped_utilities;

void
print_fasl_information()
{
  printf("FASL File Information:\n\n");
  printf("Machine = %ld; Version = %ld; Subversion = %ld\n",
	 Machine_Type, Version, Sub_Version);
  if ((dumped_processor_type != 0) || (dumped_interface_version != 0))
  {
    printf("Compiled code interface version = %ld; Processor type = %ld\n",
	   dumped_interface_version, dumped_processor_type);
  }
  if (band_p)
  {
    printf("The file contains a dumped image (band).\n");
  }

  printf("\nRelocation Information:\n\n");
  printf("Heap Count = %ld; Heap Base = 0x%lx; Heap Top = 0x%lx\n",
	 Heap_Count, Heap_Base, Dumped_Heap_Top);
  printf("Const Count = %ld; Const Base = 0x%lx; Const Top = 0x%lx\n",
	 Const_Count, Const_Base, Dumped_Constant_Top);
  printf("Stack Top = 0x%lx\n", Dumped_Stack_Top);

  printf("\nDumped Objects:\n\n");
  printf("Dumped object at 0x%lx (as read from file)\n", Dumped_Object); 
  printf("Compiled code utilities vector = 0x%lx\n", dumped_utilities);
  if (Ext_Prim_Vector != NIL)
  {
    printf("External primitives vector = 0x%lx\n", Ext_Prim_Vector);
  }
  else
  {
    printf("Length of primitive table = %ld\n", Primitive_Table_Length);
  }
  return;
}

long
Read_Header()
{
  Pointer Buffer[FASL_HEADER_LENGTH];
  Pointer Pointer_Heap_Base, Pointer_Const_Base;

  if (Load_Data(FASL_HEADER_LENGTH, ((char *) Buffer)) !=
      FASL_HEADER_LENGTH)
  {
    return (FASL_FILE_TOO_SHORT);
  }
  if (Buffer[FASL_Offset_Marker] != FASL_FILE_MARKER)
  {
    return (FASL_FILE_NOT_FASL);
  }
  NORMALIZE_HEADER(Buffer,
		   (sizeof(Buffer) / sizeof(Pointer)),
		   Buffer[FASL_Offset_Heap_Base],
		   Buffer[FASL_Offset_Heap_Count]);
  Heap_Count = Get_Integer(Buffer[FASL_Offset_Heap_Count]);
  Pointer_Heap_Base = Buffer[FASL_Offset_Heap_Base];
  Heap_Base = Datum(Pointer_Heap_Base);
  Dumped_Object = Datum(Buffer[FASL_Offset_Dumped_Obj]);
  Const_Count = Get_Integer(Buffer[FASL_Offset_Const_Count]);
  Pointer_Const_Base = Buffer[FASL_Offset_Const_Base];
  Const_Base = Datum(Pointer_Const_Base);
  Version = The_Version(Buffer[FASL_Offset_Version]);
  Sub_Version = The_Sub_Version(Buffer[FASL_Offset_Version]);
  Machine_Type = The_Machine_Type(Buffer[FASL_Offset_Version]);
  Dumped_Stack_Top = Get_Integer(Buffer[FASL_Offset_Stack_Top]);
  Dumped_Heap_Top =
    C_To_Scheme(Nth_Vector_Loc(Pointer_Heap_Base, Heap_Count));
  Dumped_Constant_Top =
    C_To_Scheme(Nth_Vector_Loc(Pointer_Const_Base, Const_Count));

  if (Sub_Version < FASL_MERGED_PRIMITIVES)
  {
    Primitive_Table_Length = 0;
    Primitive_Table_Size = 0;
    Ext_Prim_Vector =
      Make_Non_Pointer(TC_CELL, Datum(Buffer[FASL_Offset_Ext_Loc]));
  }
  else
  {
    Primitive_Table_Length = Get_Integer(Buffer[FASL_Offset_Prim_Length]);
    Primitive_Table_Size = Get_Integer(Buffer[FASL_Offset_Prim_Size]);
    Ext_Prim_Vector = NIL;
  }

  if (Sub_Version < FASL_INTERFACE_VERSION)
  {
    /* This may be all wrong, but... */
    band_p = false;
    dumped_processor_type = 0;
    dumped_interface_version = 0;
    dumped_utilities = NIL;
  }
  else
  {
    Pointer temp;

    temp = Buffer[FASL_Offset_Ci_Version];

    band_p = CI_BAND_P(temp);
    dumped_processor_type = CI_PROCESSOR(temp);
    dumped_interface_version = CI_VERSION(temp);
    dumped_utilities = Buffer[FASL_Offset_Ut_Base];
  }

#ifndef INHIBIT_FASL_VERSION_CHECK

  /* The error messages here should be handled by the runtime system! */

  if ((Version != FASL_READ_VERSION) ||
#ifndef BYTE_INVERSION
      (Machine_Type != FASL_INTERNAL_FORMAT) ||
#endif
      (Sub_Version < FASL_READ_SUBVERSION) ||
      (Sub_Version > FASL_SUBVERSION))
  {
    fprintf(stderr, "\nread_file:\n");
    fprintf(stderr,
	    "FASL File: Version %4d Subversion %4d Machine Type %4d.\n",
	    Version, Sub_Version , Machine_Type);
    fprintf(stderr,
	    "Expected:  Version %4d Subversion %4d Machine Type %4d.\n",
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
      fprintf(stderr, "\nread_file:\n");
      fprintf(stderr,
	      "FASL File: compiled code interface %4d; processor %4d.\n",
	      dumped_interface_version, dumped_processor_type);
      fprintf(stderr,
	      "Expected:  compiled code interface %4d; processor %4d.\n",
	      compiler_interface_version, compiler_processor_type);
      return (((dumped_processor_type != 0) &&
	       (dumped_processor_type != compiler_processor_type))	?
	      FASL_FILE_BAD_PROCESSOR					:
	      FASL_FILE_BAD_INTERFACE);
    }
  }

#endif /* INHIBIT_COMPILED_VERSION_CHECK */

  return (FASL_FILE_FINE);
}

#ifdef BYTE_INVERSION

static Boolean Byte_Invert_Fasl_Files;

void
Byte_Invert_Header(Header, Headsize, Test1, Test2)
     long *Header, Headsize, Test1, Test2;
{
  Byte_Invert_Fasl_Files = false;

  if ((Test1 & 0xff) == TC_BROKEN_HEART &&
      (Test2 & 0xff) == TC_BROKEN_HEART &&
      (Type_Code(Test1) != TC_BROKEN_HEART ||
       Type_Code(Test2) != TC_BROKEN_HEART))
  {
    Byte_Invert_Fasl_Files = true;
    Byte_Invert_Region(Header, Headsize);
  }
  return;
}

void
Byte_Invert_Region(Region, Size)
     long *Region, Size;
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

