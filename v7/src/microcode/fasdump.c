/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fasdump.c,v 9.45 1989/11/26 17:38:39 jinx Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* This file contains code for fasdump and dump-band. */

#include "scheme.h"
#include "prims.h"
#define In_Fasdump
#include "gccode.h"
#include "trap.h"
#include "lookup.h"
#include "fasl.h"
#include "dump.c"

extern SCHEME_OBJECT
  dump_renumber_primitive(),
  *initialize_primitive_table(),
  *cons_primitive_table(),
  *cons_whole_primitive_table();

/* Some statics used freely in this file */

static SCHEME_OBJECT *NewFree, *NewMemTop, *Fixup, *Orig_New_Free;
static Boolean compiled_code_present_p;

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

#define Setup_Pointer_for_Dump(Extra_Code)				\
Dump_Pointer(Fasdump_Setup_Pointer(Extra_Code, Normal_BH(false, continue)))

#define Dump_Pointer(Code)						\
Old = OBJECT_ADDRESS (Temp);						\
Code

#define Dump_Compiled_Entry(label)					\
{									\
  Dump_Pointer(Fasdump_Setup_Pointer(Transport_Compiled(),		\
				     Compiled_BH(false, goto label)));	\
}

/* Dump_Mode is currently a fossil.  It should be resurrected. */

/* Should be big enough for the largest fixed size object (a Quad)
   and 2 for the Fixup.
 */

#define	NORMAL_GC	0
#define PURE_COPY	1
#define CONSTANT_COPY	2

#define FASDUMP_FIX_BUFFER 10

long
DumpLoop(Scan, Dump_Mode)
     fast SCHEME_OBJECT *Scan;
     int Dump_Mode;
{
  fast SCHEME_OBJECT *To, *Old, Temp, New_Address, *Fixes;
  long result;

  To = NewFree;
  Fixes = Fixup;

  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;

    Switch_by_GC_Type(Temp)
    {
      case TC_PRIMITIVE:
      case TC_PCOMB0:
        *Scan = dump_renumber_primitive(*Scan);
	break;

      case TC_BROKEN_HEART:
        if (OBJECT_DATUM (Temp) != 0)
	{
	  sprintf(gc_death_message_buffer,
		  "dumploop: broken heart (0x%lx) in scan",
		  Temp);
	  gc_death(TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	  /*NOTREACHED*/
	}
	break;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += OBJECT_DATUM (Temp);
	break;

      /* Compiled code relocation. */

      case_compiled_entry_point:
	compiled_code_present_p = true;
	Dump_Compiled_Entry(after_entry);
      after_entry:
	*Scan = Temp;
	break;

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char *word_ptr;
	SCHEME_OBJECT *area_end;

	compiled_code_present_p = true;
	Scan += 1;
	count = (MANIFEST_CLOSURE_COUNT (Scan));
	word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	area_end = (MANIFEST_CLOSURE_END (Scan, count));

	while ((--count) >= 0)
	{
	  Scan = ((SCHEME_OBJECT *) (word_ptr));
	  word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	  EXTRACT_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	  Dump_Compiled_Entry (after_closure);
	after_closure:
	  STORE_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	}
	Scan = area_end;
	break;
      }

      case TC_LINKAGE_SECTION:
      {
	compiled_code_present_p = true;
	if (READ_LINKAGE_KIND(Temp) != OPERATOR_LINKAGE_KIND)
	{
	  /* Assumes that all others are objects of type TC_QUAD without
	     their type codes.
	   */

	  fast long count;

	  Scan++;
	  for (count = READ_CACHE_LINKAGE_COUNT(Temp);
	       --count >= 0;
	       Scan += 1)
	  {
	    Temp = *Scan;
	    Setup_Pointer_for_Dump(Transport_Quadruple());
	  }
	  Scan -= 1;
	  break;
	}
	else
	{
	  fast long count;
	  fast char *word_ptr;
	  SCHEME_OBJECT *end_scan;

	  count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	  word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	  end_scan = (END_OPERATOR_LINKAGE_AREA (Scan, count));

	  while(--count >= 0)
	  {
	    Scan = ((SCHEME_OBJECT *) (word_ptr));
	    word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	    EXTRACT_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	    Dump_Compiled_Entry (after_operator);
	  after_operator:
	    STORE_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	  }
	  Scan = end_scan;
	  break;
	}
      }

      case_Cell:
	Setup_Pointer_for_Dump(Transport_Cell());
	break;

      case TC_REFERENCE_TRAP:
	if (OBJECT_DATUM (Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* Fall through. */

      case TC_WEAK_CONS:
      case_Fasdump_Pair:
	Setup_Pointer_for_Dump(Transport_Pair());
	break;

      case TC_INTERNED_SYMBOL:
	Setup_Pointer_for_Dump (Fasdump_Symbol (BROKEN_HEART_ZERO));
	break;

      case TC_UNINTERNED_SYMBOL:
	Setup_Pointer_for_Dump (Fasdump_Symbol (UNBOUND_OBJECT));
	break;

      case_Triple:
	Setup_Pointer_for_Dump(Transport_Triple());
	break;

      case TC_VARIABLE:
	Setup_Pointer_for_Dump(Fasdump_Variable());
	break;

      case_Quadruple:
	Setup_Pointer_for_Dump(Transport_Quadruple());
	break;

      case TC_BIG_FLONUM:
	Setup_Pointer_for_Dump({
	  Transport_Flonum();
	  break;
	});

      case TC_COMPILED_CODE_BLOCK:
      case_Purify_Vector:
	Setup_Pointer_for_Dump(Transport_Vector());
	break;

      case TC_ENVIRONMENT:
	/* Make fasdump fail */
	result = ERR_FASDUMP_ENVIRONMENT;
	goto exit_dumploop;

      case TC_FUTURE:
	Setup_Pointer_for_Dump(Transport_Future());
	break;

      default:
	GC_BAD_TYPE("dumploop");
	/* Fall Through */

      case TC_STACK_ENVIRONMENT:
      case_Fasload_Non_Pointer:
	break;
      }
  }
  result = PRIM_DONE;

exit_dumploop:
  NewFree = To;
  Fixup = Fixes;
  return (result);
}

#define DUMPLOOP(obj, code)						\
{									\
  long value;								\
									\
  value = DumpLoop(obj, code);						\
  if (value != PRIM_DONE)						\
  {									\
    PRIMITIVE_RETURN(Fasdump_Exit(value));				\
  }									\
}

#define FASDUMP_INTERRUPT()						\
{									\
  PRIMITIVE_RETURN(Fasdump_Exit(PRIM_INTERRUPT));			\
}

SCHEME_OBJECT
Fasdump_Exit(code)
     long code;
{
  Boolean result;
  fast SCHEME_OBJECT *Fixes;

  Fixes = Fixup;
  result = Close_Dump_File();
  while (Fixes != NewMemTop)
  {
    fast SCHEME_OBJECT *Fix_Address;

    Fix_Address = OBJECT_ADDRESS (*Fixes++); /* Where it goes. */
    *Fix_Address = *Fixes++;             /* Put it there. */
  }
  Fixup = Fixes;
  Fasdump_Exit_Hook();
  if (!result)
  {
    signal_error_from_primitive (ERR_IO_ERROR);
    /*NOTREACHED*/
  }
  if (code == PRIM_DONE)
  {
    return (SHARP_T);
  }
  else if (code == PRIM_INTERRUPT)
  {
    return (SHARP_F);
  }
  else
  {
    signal_error_from_primitive (code);
    /*NOTREACHED*/
  }
}

/* (PRIMITIVE-FASDUMP object-to-dump file-name flag)
   Dump an object into a file so that it can be loaded using
   BINARY-FASLOAD.  A spare heap is required for this operation.
   The first argument is the object to be dumped.  The second is
   the filename and the third a flag.  The flag, if #T, means
   that the object is to be dumped for reloading into constant
   space.  This is currently disabled. If the flag is #F, it means
   that it will be reloaded into the heap.  The primitive returns
   #T or #F indicating whether it successfully dumped the
   object (it can fail on an object that is too large).

   The code for dumping pure is severely broken and conditionalized out.
*/

DEFINE_PRIMITIVE ("PRIMITIVE-FASDUMP", Prim_prim_fasdump, 3, 3, 0)
{
  SCHEME_OBJECT Object, File_Name, Flag, *New_Object;
  SCHEME_OBJECT *table_start, *table_end;
  long Length, table_length;
  Boolean result;
  PRIMITIVE_HEADER (3);
  CHECK_ARG (2, STRING_P);
  compiled_code_present_p = false;
  Object = (ARG_REF (1));
  File_Name = (ARG_REF (2));
  Flag = (ARG_REF (3));
  if (! (Open_Dump_File (File_Name, WRITE_FLAG)))
    error_bad_range_arg (2);
#if false
  CHECK_ARG (3, BOOLEAN_P);
#else
  if (Flag != SHARP_F)
    error_wrong_type_arg (3);
#endif
  table_end = &Free[Space_Before_GC()];
  table_start = initialize_primitive_table(Free, table_end);
  if (table_start >= table_end)
    {
      Primitive_GC (table_start - Free);
    }
  Fasdump_Free_Calc(NewFree, NewMemTop, Orig_New_Free);
  Fixup = NewMemTop;
  New_Object = NewFree;
  *NewFree++ = Object;

#if false
  /* NOTE: This is wrong!

     Many things will break, among them:

     Symbols will not be interned correctly in the new system.

     The primitive dumping mechanism will break, since
     dump_renumber_primitive is not being invoked by
     either phase.

     The special entry point relocation code depends on the fact that
     fasdumped files (as opposed to bands) contain no constant space
     segment.  See fasload.c for further information.
*/

  if (Flag == SHARP_T)
  {
    SCHEME_OBJECT *Addr_Of_New_Object;

    *New_Free++ = SHARP_F;
    DUMPLOOP(New_Object, PURE_COPY);
#if false
    /* Can't align. */
    ALIGN_FLOAT (NewFree);
#endif
    Pure_Length = ((NewFree - New_Object) + 1);
    *NewFree++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
    *NewFree++ = MAKE_OBJECT (CONSTANT_PART, Pure_Length);
    DUMPLOOP(New_Object, CONSTANT_COPY);
    Length =  ((NewFree - New_Object) + 2);
    *NewFree++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
    *NewFree++ = MAKE_OBJECT (END_OF_BLOCK, (Length - 1));
    Addr_Of_New_Object = OBJECT_ADDRESS (New_Object[0]);
    New_Object[0] = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, Pure_Length);
    New_Object[1] = MAKE_OBJECT (PURE_PART, (Length - 1));
    table_start = NewFree;
    table_end = cons_primitive_table(NewFree, Fixup, &table_length);
    if (table_end >= Fixup)
    {
      FASDUMP_INTERRUPT();
    }
    result = Write_File(Addr_Of_New_Object, 0, 0,
			Length, New_Object,
			table_start, table_length,
			((long) (table_end - table_start)),
			compiled_code_present_p, false);
  }
  else
#endif /* Dumping for reload into heap */

  {
    DUMPLOOP(New_Object, NORMAL_GC);
#if false
    /* Aligning might screw up some of the counters. */
    ALIGN_FLOAT (NewFree);
#endif
    Length = (NewFree - New_Object);
    table_start = NewFree;
    table_end = cons_primitive_table(NewFree, Fixup, &table_length);
    if (table_end >= Fixup)
    {
      FASDUMP_INTERRUPT();
    }
    result = Write_File(New_Object,
			Length, New_Object,
			0, Constant_Space,
			table_start, table_length,
			((long) (table_end - table_start)),
			compiled_code_present_p, false);
  }

  PRIMITIVE_RETURN(Fasdump_Exit(result ? PRIM_DONE : PRIM_INTERRUPT));
}

/* (DUMP-BAND PROCEDURE FILE-NAME)
   Saves all of the heap and pure space on FILE-NAME.  When the
   file is loaded back using BAND_LOAD, PROCEDURE is called with an
   argument of #F.
*/

DEFINE_PRIMITIVE ("DUMP-BAND", Prim_band_dump, 2, 2, 0)
{
  SCHEME_OBJECT Combination, *table_start, *table_end, *saved_free;
  long table_length;
  Boolean result;
  PRIMITIVE_HEADER (2);
  Band_Dump_Permitted ();
  CHECK_ARG (1, INTERPRETER_APPLICABLE_P);
  CHECK_ARG (2, STRING_P);
  if (Unused_Heap < Heap_Bottom)
    {
      /* Cause the image to be in the low heap, to increase
	 the probability that no relocation is needed on reload. */
      Primitive_GC (0);
    }
  if (! (Open_Dump_File ((ARG_REF (2)), WRITE_FLAG)))
    error_bad_range_arg (2);
  Primitive_GC_If_Needed (5);
  saved_free = Free;
  Combination = MAKE_POINTER_OBJECT (TC_COMBINATION_1, Free);
  Free[COMB_1_FN] = (ARG_REF (1));
  Free[COMB_1_ARG_1] = SHARP_F;
  Free += 2;
  *Free++ = Combination;
  *Free++ = compiler_utilities;
  *Free = MAKE_POINTER_OBJECT (TC_LIST, (Free - 2));
  Free++;  /* Some compilers are TOO clever about this and increment Free
	      before calculating Free-2! */
  table_start = Free;
  table_end = cons_whole_primitive_table(Free, Heap_Top, &table_length);
  if (table_end >= Heap_Top)
  {
    result = false;
  }
  else
  {
#if false
  /* Aligning here confuses some of the counts computed. */
    ALIGN_FLOAT (Free);
#endif
    result = Write_File((Free - 1),
			((long) (Free - Heap_Bottom)), Heap_Bottom,
			((long) (Free_Constant - Constant_Space)),
			Constant_Space,
			table_start, table_length,
			((long) (table_end - table_start)),
			(compiler_utilities != SHARP_F), true);
  }
  /* The and is short-circuit, so it must be done in this order. */
  result = ((Close_Dump_File ()) && result);
  Band_Dump_Exit_Hook ();
  Free = saved_free;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (result));
}
