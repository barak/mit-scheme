/* -*-C-*-

$Id: dump.c,v 9.41 2002/11/20 19:46:08 cph Exp $

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

/* This file contains common code for dumping internal format binary files. */

#ifndef PSBMAP_H_INCLUDED
extern long
  compiler_interface_version,
  compiler_processor_type;

extern SCHEME_OBJECT
  compiler_utilities;
#endif /* PSBMAP_H_INCLUDED */

void
DEFUN (prepare_dump_header, (Buffer, Dumped_Object,
			     Heap_Count, Heap_Relocation,
			     Constant_Count, Constant_Relocation,
			     prim_table_length, prim_table_size,
			     c_table_length, c_table_size,
			     cc_code_p, band_p),
       SCHEME_OBJECT * Buffer
       AND SCHEME_OBJECT * Dumped_Object
       AND long Heap_Count
       AND SCHEME_OBJECT * Heap_Relocation
       AND long Constant_Count
       AND SCHEME_OBJECT * Constant_Relocation
       AND long prim_table_length
       AND long prim_table_size
       AND long c_table_length
       AND long c_table_size
       AND Boolean cc_code_p
       AND Boolean band_p)
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
    MAKE_OBJECT (TC_BROKEN_HEART, prim_table_length);
  Buffer[FASL_Offset_Prim_Size] =
    MAKE_OBJECT (TC_BROKEN_HEART, prim_table_size);

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
    Buffer[FASL_Offset_Ci_Version] = (MAKE_CI_VERSION (band_p, 0, 0));
    Buffer[FASL_Offset_Ut_Base] = SHARP_F;
  }

  Buffer[FASL_Offset_C_Length] =
    MAKE_OBJECT (TC_BROKEN_HEART, c_table_length);
  Buffer[FASL_Offset_C_Size] =
    MAKE_OBJECT (TC_BROKEN_HEART, c_table_size);

#ifdef HEAP_IN_LOW_MEMORY
  Buffer[FASL_Offset_Mem_Base] = ((SCHEME_OBJECT) 0);
#else /* not HEAP_IN_LOW_MEMORY */
  Buffer[FASL_Offset_Mem_Base] = ((SCHEME_OBJECT) memory_base);
#endif /* HEAP_IN_LOW_MEMORY */

  Buffer[FASL_Offset_Check_Sum] = SHARP_F;
  for (i = FASL_Offset_First_Free; i < FASL_HEADER_LENGTH; i++)
    Buffer[i] = SHARP_F;
  return;
}

extern unsigned long
  EXFUN (checksum_area, (unsigned long *, long, unsigned long));

Boolean
DEFUN (Write_File, (Dumped_Object, Heap_Count, Heap_Relocation,
		    Constant_Count, Constant_Relocation,
		    prim_table_start, prim_table_length, prim_table_size,
		    c_table_start, c_table_length, c_table_size,
		    cc_code_p, band_p),
       SCHEME_OBJECT * Dumped_Object
       AND long Heap_Count
       AND SCHEME_OBJECT * Heap_Relocation
       AND long Constant_Count
       AND SCHEME_OBJECT * Constant_Relocation
       AND SCHEME_OBJECT * prim_table_start
       AND long prim_table_length
       AND long prim_table_size
       AND SCHEME_OBJECT * c_table_start
       AND long c_table_length
       AND long c_table_size
       AND Boolean cc_code_p
       AND Boolean band_p)
{
  SCHEME_OBJECT Buffer[FASL_HEADER_LENGTH];
  unsigned long checksum;

  prepare_dump_header (Buffer, Dumped_Object,
		       Heap_Count, Heap_Relocation,
		       Constant_Count, Constant_Relocation,
		       prim_table_length, prim_table_size,
		       c_table_length, c_table_size,
		       cc_code_p, band_p);

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
  checksum = (checksum_area (((unsigned long *) prim_table_start),
			     prim_table_size,
			     checksum));
  checksum = (checksum_area (((unsigned long *) c_table_start),
			     c_table_size,
			     checksum));

  Buffer[FASL_Offset_Check_Sum] = checksum;

  if ((Write_Data (FASL_HEADER_LENGTH, Buffer))
      != FASL_HEADER_LENGTH)
    return (false);

  if ((Heap_Count != 0)
      && ((Write_Data (Heap_Count, Heap_Relocation))
	  != Heap_Count))
    return (false);

  if ((Constant_Count != 0)
      && ((Write_Data (Constant_Count, Constant_Relocation))
	  != Constant_Count))
    return (false);

  if ((prim_table_size != 0)
      && ((Write_Data (prim_table_size, prim_table_start))
	  != prim_table_size))
      return (false);

  if ((c_table_size != 0)
      && ((Write_Data (c_table_size, c_table_start))
	  != c_table_size))
      return (false);

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
    value = (value ^ (*start++));
  return (value);
}

