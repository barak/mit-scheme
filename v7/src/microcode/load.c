/* Emacs, -*-C-*-an't you guess? */

/****************************************************************
*                                                               *
*                         Copyright (c) 1986                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: LOAD.C
 *
 * This file contains common code for reading internal
 * format binary files.
 *
 */

#include "fasl.h"

/* Static storage for some shared variables */

long Heap_Count, Const_Count,
     Version, Sub_Version, Machine_Type, Ext_Prim_Count,
     Heap_Base, Const_Base, Dumped_Object,
     Dumped_Heap_Top, Dumped_Constant_Top, Dumped_Stack_Top;
Pointer Ext_Prim_Vector;
Boolean Found_Ext_Prims;

Boolean Read_Header()
{ Pointer Buffer[FASL_HEADER_LENGTH];
  Pointer Pointer_Heap_Base, Pointer_Const_Base;
  Load_Data(FASL_OLD_LENGTH, (char *) Buffer);
  if (Buffer[FASL_Offset_Marker] != FASL_FILE_MARKER) return false;
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
  if (Sub_Version >= FASL_LONG_HEADER)
  { Load_Data(FASL_HEADER_LENGTH-FASL_OLD_LENGTH,
	      (char *) &(Buffer[FASL_OLD_LENGTH]));
    Ext_Prim_Vector =
      Make_Non_Pointer(TC_CELL, Datum(Buffer[FASL_Offset_Ext_Loc]));
  }
  else Ext_Prim_Vector = NIL;
  if (Reloc_or_Load_Debug)
  { printf("\nHeap_Count = %d; Heap_Base = %x; Dumped_Heap_Top = %x\n",
           Heap_Count, Heap_Base, Dumped_Heap_Top);
    printf("C_Count = %d; C_Base = %x, Dumped_C_Top = %x\n",
           Const_Count, Const_Base, Dumped_Constant_Top);
    printf("Dumped_S_Top = %x, Ext_Prim_Vector = 0x%08x\n",
	   Dumped_Stack_Top, Ext_Prim_Vector);
    printf("Dumped Object (as read from file) = %x\n", Dumped_Object); 
  }
  return true;
}
