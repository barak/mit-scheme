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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dump.c,v 9.22 1987/04/03 00:11:11 jinx Exp $
 *
 * This file contains common code for dumping internal format binary files.
 */

#include "fasl.h"

Write_File(Heap_Count, Heap_Relocation, Dumped_Object,
           Constant_Count, Constant_Relocation, Prim_Exts)
Pointer *Heap_Relocation, *Dumped_Object,
        *Constant_Relocation, *Prim_Exts;
long Heap_Count, Constant_Count;
{ Pointer Buffer[FASL_HEADER_LENGTH];
  long i;

#ifdef DEBUG
#ifndef Heap_In_Low_Memory
  fprintf(stderr, "\nMemory_Base = 0x%x\n", Memory_Base);
#endif
  fprintf(stderr, "\nHeap_Relocation=0x%x, dumped as 0x%x\n",
	  Heap_Relocation, Make_Pointer(TC_BROKEN_HEART, Heap_Relocation));
  fprintf(stderr, "\nDumped object=0x%x, dumped as 0x%x\n",
	  Dumped_Object, Make_Pointer(TC_BROKEN_HEART, Dumped_Object));
#endif
  Buffer[FASL_Offset_Marker] = FASL_FILE_MARKER;
  Buffer[FASL_Offset_Heap_Count] =
    Make_Non_Pointer(TC_BROKEN_HEART, Heap_Count);
  Buffer[FASL_Offset_Heap_Base] =
    Make_Pointer(TC_BROKEN_HEART, Heap_Relocation);
  Buffer[FASL_Offset_Dumped_Obj] =
    Make_Pointer(TC_BROKEN_HEART, Dumped_Object);
  Buffer[FASL_Offset_Const_Count] =
    Make_Non_Pointer(TC_BROKEN_HEART, Constant_Count);
  Buffer[FASL_Offset_Const_Base] =
    Make_Pointer(TC_BROKEN_HEART, Constant_Relocation);
  Buffer[FASL_Offset_Version] =
    Make_Version(FASL_FORMAT_VERSION,
		 FASL_SUBVERSION, FASL_INTERNAL_FORMAT);
  Buffer[FASL_Offset_Stack_Top] =
#ifdef USE_STACKLETS
    Make_Pointer(TC_BROKEN_HEART, 0);	/* Nothing in stack area */
#else
    Make_Pointer(TC_BROKEN_HEART, Stack_Top);
#endif
  Buffer[FASL_Offset_Ext_Loc] = 
    Make_Pointer(TC_BROKEN_HEART, Prim_Exts);
  for (i=FASL_Offset_First_Free; i < FASL_HEADER_LENGTH; i++)
    Buffer[i] = NIL;
  Write_Data(FASL_HEADER_LENGTH, (char *) Buffer);
  if (Heap_Count != 0) Write_Data(Heap_Count, (char *) Heap_Relocation);
  if (Constant_Count != 0)
     Write_Data(Constant_Count, (char *) Constant_Relocation);
}
