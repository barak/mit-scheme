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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fasdump.c,v 9.29 1987/06/18 21:15:11 jinx Rel $

   This file contains code for fasdump and dump-band.
*/

#include "scheme.h"
#include "primitive.h"
#define In_Fasdump
#include "gccode.h"
#include "trap.h"
#include "lookup.h"
#include "dump.c"

extern Pointer Make_Prim_Exts();

/* Some statics used freely in this file */
Pointer *NewFree, *NewMemTop, *Fixup, *Orig_New_Free;

/* FASDUMP:

   Hair squared! ... in order to dump an object it must be traced (as
   in a garbage collection), but with some significant differences.
   First, the copy must have the global value cell of symbols set to
   UNBOUND and variables uncompiled.  Second, and worse, all the
   broken hearts created during the process must be restored to their
   original values.  This last is done by growing the copy of the
   object in the bottom of spare heap, keeping track of the locations
   of broken hearts and original contents at the top of the spare
   heap.

   FASDUMP is called with three arguments:
   Argument 1: Object to dump.
   Argument 2: File name.
   Argument 3: Flag.
               where the flag is #!true for a dump into constant
               space at reload time, () for a dump into heap.

   Currently flag is ignored.	       
*/

/* 
   Copy of GCLoop, except (a) copies out of constant space into the
   object to be dumped; (b) changes symbols and variables as
   described; (c) keeps track of broken hearts and their original
   contents (e) To_Pointer is now NewFree.
*/

#define Dump_Pointer(Code)					\
Old = Get_Pointer(Temp);					\
Code

#define Setup_Pointer_for_Dump(Extra_Code)			\
Dump_Pointer(Fasdump_Setup_Pointer(Extra_Code, Normal_BH(false, continue)))

/* Dump_Mode is currently a fossil.  It should be resurrected. */

/* Should be big enough for the largest fixed size object (a Quad) 
   and 2 for the Fixup.
 */

#define FASDUMP_FIX_BUFFER 10

Boolean DumpLoop(Scan, Dump_Mode)
fast Pointer *Scan;
int Dump_Mode;
{ fast Pointer *To, *Old, Temp, New_Address, *Fixes;

  To = NewFree;
  Fixes = Fixup;

  for ( ; Scan != To; Scan++)
  { Temp = *Scan;

    Switch_by_GC_Type(Temp)
    { case TC_BROKEN_HEART:
        if (Datum(Temp) != 0)
	{ fprintf(stderr, "\nDump: Broken heart in scan.\n");
	  Microcode_Termination(TERM_BROKEN_HEART);
	}
	break;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += Get_Integer(Temp);
	break;

	/* This should really be case_Fasdump_Non_Pointer,
	   and PRIMITIVE_EXTERNAL should be handled specially
	 */
      case_Non_Pointer:
	break;

      case_compiled_entry_point:
	Dump_Pointer(Fasdump_Setup_Pointer(Transport_Compiled(),
					   Compiled_BH(false, continue)));

      case_Cell:
	Setup_Pointer_for_Dump(Transport_Cell());

      case TC_REFERENCE_TRAP:
	if (Datum(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* Fall through. */
      case TC_WEAK_CONS:
      case_Fasdump_Pair:
	Setup_Pointer_for_Dump(Transport_Pair());

      case TC_INTERNED_SYMBOL:
	Setup_Pointer_for_Dump(Fasdump_Symbol(Make_Broken_Heart(0)));

      case TC_UNINTERNED_SYMBOL:
	Setup_Pointer_for_Dump(Fasdump_Symbol(UNBOUND_OBJECT));

      case_Triple:
	Setup_Pointer_for_Dump(Transport_Triple());

      case TC_VARIABLE:
	Setup_Pointer_for_Dump(Fasdump_Variable());

/* DumpLoop continues on the next page */

/* DumpLoop, continued */

      case_Quadruple:
	Setup_Pointer_for_Dump(Transport_Quadruple());

#ifdef FLOATING_ALIGNMENT
      case TC_BIG_FLONUM:
	Setup_Pointer_for_Dump(Transport_Flonum());
#else
      case TC_BIG_FLONUM:
	/* Fall through */
#endif
      case_Vector:
	Setup_Pointer_for_Dump(Transport_Vector());

      case TC_FUTURE:
	Setup_Pointer_for_Dump(Transport_Future());

      default:
	fprintf(stderr,
		"DumpLoop: Bad type code = 0x%02x\n",
		Type_Code(Temp));
	Invalid_Type_Code();

      }	/* Switch_by_GC_Type */
  } /* For loop */
  NewFree = To;
  Fixup = Fixes;
  return true;
} /* DumpLoop */

Boolean
Fasdump_Exit()
{
  Boolean result;
  fast Pointer *Fixes;

  Fixes = Fixup;
  result = Close_Dump_File();
  while (Fixes != NewMemTop)
  {
    fast Pointer *Fix_Address;

    Fix_Address = Get_Pointer(*Fixes++); /* Where it goes. */
    *Fix_Address = *Fixes++;             /* Put it there. */
  }
  Fixup = Fixes;
  Fasdump_Exit_Hook();
  return result;
}

/* (PRIMITIVE-FASDUMP object-to-dump file-name flag)
   Dump an object into a file so that it can be loaded using
   BINARY-FASLOAD.  A spare heap is required for this operation.
   The first argument is the object to be dumped.  The second is
   the filename and the third a flag.  The flag, if #!TRUE, means
   that the object is to be dumped for reloading into constant
   space.  This is currently disabled. If the flag is NIL, it means
   that it will be reloaded into the heap.  The primitive returns
   #!TRUE or NIL indicating whether it successfully dumped the
   object (it can fail on an object that is too large).

   The code for dumping pure is severely broken and conditionalized out.
*/
Built_In_Primitive(Prim_Prim_Fasdump, 3, "PRIMITIVE-FASDUMP", 0x56)
{
  Pointer Object, File_Name, Flag, *New_Object,
          *Addr_Of_New_Object, Prim_Exts;
  long Pure_Length, Length;
  Boolean result;
  Primitive_3_Args();

  Object = Arg1;
  File_Name = Arg2;
  Flag = Arg3;
  if (Type_Code(File_Name) != TC_CHARACTER_STRING)
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (!Open_Dump_File(File_Name, WRITE_FLAG))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
#if false
  if ((Flag != NIL) && (Flag != TRUTH))
#else
  if (Flag != NIL)
#endif
    Primitive_Error(ERR_ARG_3_WRONG_TYPE);

  Fasdump_Free_Calc(NewFree, NewMemTop, Orig_New_Free);
  Fixup = NewMemTop;
  Prim_Exts = Make_Prim_Exts();
  New_Object = NewFree;
  *NewFree++ = Object;
  *NewFree++ = Prim_Exts;

#if false
  if (Flag == TRUTH)
  {
    if (!DumpLoop(New_Object, PURE_COPY))
    {
      Fasdump_Exit();
      PRIMITIVE_RETURN(NIL);
    }
    /* Can't align.
       Align_Float(NewFree);
     */
    Pure_Length = (NewFree-New_Object) + 1;
    *NewFree++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
    *NewFree++ = Make_Non_Pointer(CONSTANT_PART, Pure_Length);
    if (!DumpLoop(New_Object, CONSTANT_COPY))
    {
      Fasdump_Exit();
      PRIMITIVE_RETURN(NIL);
    }
    Length =  ((NewFree - New_Object) + 2);
    *NewFree++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
    *NewFree++ = Make_Non_Pointer(END_OF_BLOCK, (Length - 1));
    Addr_Of_New_Object = Get_Pointer(New_Object[0]);
    Prim_Exts = New_Object[1];
    New_Object[0] = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR,
                                     Pure_Length);
    New_Object[1] = Make_Non_Pointer(PURE_PART, (Length - 1));
    result = Write_File(0, 0x000000, Addr_Of_New_Object,
			Length, New_Object, Prim_Exts);
  }
  else		/* Dumping for reload into heap */
#endif
  {
    if (!DumpLoop(New_Object, NORMAL_GC))
    {
      Fasdump_Exit();
      PRIMITIVE_RETURN(NIL);
    }
    /* Aligning might screw up some of the counters.
       Align_Float(NewFree);
     */
    Length = (NewFree - New_Object);
    result = Write_File(Length, New_Object, New_Object,
			0, Constant_Space, (New_Object + 1));
  }
  result = (result && Fasdump_Exit());
  PRIMITIVE_RETURN(result ? TRUTH : NIL);
}

/* (DUMP-BAND PROCEDURE FILE-NAME)
   Saves all of the heap and pure space on FILE-NAME.  When the
   file is loaded back using BAND_LOAD, PROCEDURE is called with an
   argument of NIL.
*/
Built_In_Primitive(Prim_Band_Dump, 2, "DUMP-BAND", 0xB7)
{
  extern Pointer compiler_utilities;
  Pointer Combination, Ext_Prims;
  long Arg1Type;
  Boolean result;
  Primitive_2_Args();

  Band_Dump_Permitted();
  Arg1Type = Type_Code(Arg1);
  if ((Arg1Type != TC_CONTROL_POINT) &&
      (Arg1Type != TC_PRIMITIVE) &&
      (Arg1Type != TC_PRIMITIVE_EXTERNAL) &&
      (Arg1Type != TC_EXTENDED_PROCEDURE)) Arg_1_Type(TC_PROCEDURE);
  Arg_2_Type(TC_CHARACTER_STRING);
  if (!Open_Dump_File(Arg2, WRITE_FLAG))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  /* Free cannot be saved around this code since Make_Prim_Exts will
     intern the undefined externals and potentially allocate space.
   */
  Ext_Prims = Make_Prim_Exts();
  Combination = Make_Pointer(TC_COMBINATION_1, Free);
  Free[COMB_1_FN] = Arg1;
  Free[COMB_1_ARG_1] = NIL;
  Free += 2;
  *Free++ = Combination;
  *Free++ = compiler_utilities;
  *Free = Make_Pointer(TC_LIST, Free-2);
  Free++;  /* Some compilers are TOO clever about this and increment Free
	      before calculating Free-2! */
  *Free++ = Ext_Prims;
  /* Aligning here confuses some of the counts computed.
     Align_Float(Free);
   */
  result = Write_File(((long) (Free - Heap_Bottom)), Heap_Bottom, (Free - 2),
		      ((long) (Free_Constant - Constant_Space)),
		      Constant_Space, (Free - 1));
  result = (result && Close_Dump_File());
  Band_Dump_Exit_Hook();
  PRIMITIVE_RETURN(result ? TRUTH : NIL);
}
