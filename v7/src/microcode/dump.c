/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dump.c,v 9.35 1992/02/03 23:25:45 jinx Exp $

Copyright (c) 1987-92 Massachusetts Institute of Technology

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

/* This file contains common code for dumping internal format binary files. */

extern SCHEME_OBJECT compiler_utilities;
extern long compiler_interface_version, compiler_processor_type;

void
DEFUN (prepare_dump_header,
       (Buffer, Dumped_Object,
	Heap_Count, Heap_Relocation,
	Constant_Count, Constant_Relocation,
	table_length, table_size,
	cc_code_p, band_p),
       SCHEME_OBJECT *Buffer AND
       SCHEME_OBJECT *Dumped_Object AND
       long Heap_Count AND
       SCHEME_OBJECT *Heap_Relocation AND
       long Constant_Count AND
       SCHEME_OBJECT *Constant_Relocation AND
       long table_length AND
       long table_size AND
       Boolean cc_code_p AND
       Boolean band_p)
{
  long i;

#ifdef DEBUG

#ifndef HEAP_IN_LOW_MEMORY
  fprintf(stderr, "\nmemory_base = 0x%lx\n", ((long) memory_base));
#endif /* HEAP_IN_LOW_MEMORY */

  fprintf(stderr, "\nHeap_Relocation=0x%lx, dumped as 0x%lx\n",
	  ((long) Heap_Relocation),
	  ((long) (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Heap_Relocation))));
  fprintf(stderr, "\nDumped object=0x%lx, dumped as 0x%lx\n",
	  ((long) Dumped_Object),
	  ((long) (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Dumped_Object))));
#endif /* DEBUG */

  Buffer[FASL_Offset_Marker] = FASL_FILE_MARKER;
  Buffer[FASL_Offset_Heap_Count] =
    MAKE_OBJECT (TC_BROKEN_HEART, Heap_Count);
  Buffer[FASL_Offset_Heap_Base] =
    MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Heap_Relocation);
  Buffer[FASL_Offset_Dumped_Obj] =
    MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Dumped_Object);
  Buffer[FASL_Offset_Const_Count] =
    MAKE_OBJECT (TC_BROKEN_HEART, Constant_Count);
  Buffer[FASL_Offset_Const_Base] =
    MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Constant_Relocation);
  Buffer[FASL_Offset_Version] =
    Make_Version(FASL_FORMAT_VERSION,
		 FASL_SUBVERSION, FASL_INTERNAL_FORMAT);
  Buffer[FASL_Offset_Stack_Top] =
#ifdef USE_STACKLETS
    MAKE_OBJECT (TC_BROKEN_HEART, 0);	/* Nothing in stack area */
#else
    MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Stack_Top);
#endif /* USE_STACKLETS */

  Buffer[FASL_Offset_Prim_Length] =
    MAKE_OBJECT (TC_BROKEN_HEART, table_length);
  Buffer[FASL_Offset_Prim_Size] =
    MAKE_OBJECT (TC_BROKEN_HEART, table_size);

  if (cc_code_p)
  {
    Buffer[FASL_Offset_Ci_Version] =
      MAKE_CI_VERSION(band_p,
		      compiler_interface_version,
		      compiler_processor_type);
    Buffer[FASL_Offset_Ut_Base] = compiler_utilities;
  }
  else
  {
    /* If there is no compiled code in the file,
       flag it as if dumped without compiler support, so
       it can be loaded anywhere.
     */
    Buffer[FASL_Offset_Ci_Version] = MAKE_CI_VERSION(band_p, 0, 0);
    Buffer[FASL_Offset_Ut_Base] = SHARP_F;
  }

  Buffer[FASL_Offset_Check_Sum] = SHARP_F;
  for (i = FASL_Offset_First_Free; i < FASL_HEADER_LENGTH; i++)
  {
    Buffer[i] = SHARP_F;
  }
  return;
}

extern unsigned long
  EXFUN (checksum_area, (unsigned long *, long, unsigned long));

Boolean
DEFUN (Write_File,
       (Dumped_Object, Heap_Count, Heap_Relocation,
	Constant_Count, Constant_Relocation,
	table_start, table_length, table_size,
	cc_code_p, band_p),
       SCHEME_OBJECT *Dumped_Object
       AND long Heap_Count
       AND SCHEME_OBJECT *Heap_Relocation
       AND long Constant_Count
       AND SCHEME_OBJECT *Constant_Relocation
       AND SCHEME_OBJECT *table_start
       AND long table_length
       AND long table_size
       AND Boolean cc_code_p
       AND Boolean band_p)
{
  SCHEME_OBJECT Buffer[FASL_HEADER_LENGTH];
  unsigned long checksum;

  prepare_dump_header (Buffer, Dumped_Object,
		       Heap_Count, Heap_Relocation,
		       Constant_Count, Constant_Relocation,
		       table_length, table_size, cc_code_p, band_p);

  /* This is not done in prepare_dump_header because it doesn't
     work when prepare_dump_header is invoked from bchdmp.
     The areas don't really have these values.
     For the time being, bchdmp does not dump checksums.
   */

  checksum = (checksum_area (((unsigned long *) (&Buffer[0])),
			     ((long) FASL_Offset_Check_Sum),
			     ((unsigned long) 0L)));
  checksum = (checksum_area (((unsigned long *)
			      (&Buffer[FASL_Offset_Check_Sum + 1])),
			     ((long) ((FASL_HEADER_LENGTH - 1) -
				      FASL_Offset_Check_Sum)),
			     checksum));
  checksum = (checksum_area (((unsigned long *) Heap_Relocation),
			     Heap_Count,
			     checksum));
  checksum = (checksum_area (((unsigned long *) Constant_Relocation),
			     Constant_Count,
			     checksum));
  checksum = (checksum_area (((unsigned long *) table_start),
			     table_size,
			     checksum));
  Buffer[FASL_Offset_Check_Sum] = checksum;

  if ((Write_Data (FASL_HEADER_LENGTH, Buffer)) !=
      FASL_HEADER_LENGTH)
  {
    return (false);
  }
  if (Heap_Count != 0)
  {
    if ((Write_Data (Heap_Count, Heap_Relocation)) !=
	Heap_Count)
    {
      return (false);
    }
  }
  if (Constant_Count != 0)
  {
    if ((Write_Data (Constant_Count, Constant_Relocation)) !=
	Constant_Count)
    {
      return (false);
    }
  }
  if (table_size != 0)
  {
    if ((Write_Data (table_size, table_start)) !=
	table_size)
    {
      return (false);
    }
  }
  return (true);
}

unsigned long
DEFUN (checksum_area, (start, count, initial_value),
       register unsigned long * start
       AND register long count
       AND unsigned long initial_value)
{
  register unsigned long value;

  value = initial_value;
  while ((--count) >= 0)
  {
    value = (value ^ (*start++));
  }
  return (value);
}

