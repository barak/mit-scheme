/* -*-C-*-

$Id: fasdump.c,v 9.69 2007/01/05 15:33:06 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* This file contains code for fasdump and dump-band. */

#include "scheme.h"
#include "prims.h"
#include "osscheme.h"
#include "osio.h"
#include "osfile.h"
#include "osfs.h"
#define In_Fasdump
#include "gccode.h"
#include "trap.h"
#include "lookup.h"
#include "fasl.h"

static Tchannel dump_channel;

#define Write_Data(size, buffer)					\
  ((long)								\
   ((OS_channel_write_dump_file						\
     (dump_channel,							\
      ((char *) (buffer)),						\
      ((size) * (sizeof (SCHEME_OBJECT)))))				\
    / (sizeof (SCHEME_OBJECT))))

#include "dump.c"

extern SCHEME_OBJECT
  EXFUN (dump_renumber_primitive, (SCHEME_OBJECT)),
  * EXFUN (initialize_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *)),
  * EXFUN (cons_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *)),
  * EXFUN (cons_whole_primitive_table,
	   (SCHEME_OBJECT *, SCHEME_OBJECT *, long *)),
  * EXFUN (cons_c_code_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *));

/* Some statics used freely in this file */

static SCHEME_OBJECT *NewFree, *NewMemTop, *Fixup;
static Boolean compiled_code_present_p;
static CONST char * dump_file_name = ((char *) 0);

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
   Currently, flag is ignored.
*/

/*
   Copy of GCLoop, except (a) copies out of constant space into the
   object to be dumped; (b) changes symbols and variables as
   described; (c) keeps track of broken hearts and their original
   contents (e) To_Pointer is now NewFree.
*/

#define Setup_Pointer_for_Dump(Extra_Code)				\
  Dump_Pointer (Fasdump_Setup_Pointer (Extra_Code,			\
				       Normal_BH (false, continue)))

#define Dump_Pointer(Code)						\
  Old = (OBJECT_ADDRESS (Temp));					\
  Code

#define DUMP_RAW_POINTER(Code)						\
  Old = (SCHEME_ADDR_TO_ADDR (Temp));					\
  Code

/* This depends on the fact that the last word in a compiled code block
   contains the environment, and that To will be pointing to the word
   immediately after that!
 */

#define Fasdump_Transport_Compiled()					\
{									\
  Transport_Compiled ();						\
  if ((mode == 2) && ((OBJECT_TYPE (*(To - 1))) == TC_ENVIRONMENT))	\
    *(To - 1) = SHARP_F;						\
}

#define FASDUMP_TRANSPORT_RAW_COMPILED()				\
{									\
  TRANSPORT_RAW_COMPILED ();						\
  if ((mode == 2) && ((OBJECT_TYPE (*(To - 1))) == TC_ENVIRONMENT))	\
    *(To - 1) = SHARP_F;						\
}

#define Dump_Compiled_Entry(label)					\
{									\
  Dump_Pointer								\
    (Fasdump_Setup_Aligned (Fasdump_Transport_Compiled (),		\
			    Compiled_BH (false, goto label)));		\
}

#define DUMP_RAW_COMPILED_ENTRY(label)					\
{									\
  DUMP_RAW_POINTER							\
    (Fasdump_Setup_Aligned (FASDUMP_TRANSPORT_RAW_COMPILED (),		\
			    RAW_COMPILED_BH (false,			\
					     goto label)));		\
}

/* Should be big enough for the largest fixed size object (a Quad)
   and 2 for the Fixup.
 */

#define FASDUMP_FIX_BUFFER 10

long
DEFUN (DumpLoop, (Scan, mode), fast SCHEME_OBJECT * Scan AND int mode)
{
  fast SCHEME_OBJECT *To, *Old, Temp, New_Address, *Fixes;
  long result;
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  SCHEME_OBJECT object_referencing;
#endif

  To = NewFree;
  Fixes = Fixup;

  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;
#ifdef ENABLE_GC_DEBUGGING_TOOLS
    object_referencing = Temp;
#endif

    Switch_by_GC_Type (Temp)
    {
      case TC_PRIMITIVE:
      case TC_PCOMB0:
        * Scan = (dump_renumber_primitive (* Scan));
	break;

      case TC_BROKEN_HEART:
        if ((OBJECT_DATUM (Temp)) != 0)
	{
	  sprintf (gc_death_message_buffer,
		   "dumploop: broken heart (0x%lx) in scan",
		   ((long) Temp));
	  gc_death (TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	  /*NOTREACHED*/
	}
	break;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += (OBJECT_DATUM (Temp));
	break;

      /* Compiled code relocation. */

      case_compiled_entry_point:
	compiled_code_present_p = true;
	Dump_Compiled_Entry (after_entry);
      after_entry:
	* Scan = Temp;
	break;

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char * word_ptr;
	SCHEME_OBJECT * area_end;

	compiled_code_present_p = true;
	START_CLOSURE_RELOCATION (Scan);
	Scan += 1;
	count = (MANIFEST_CLOSURE_COUNT (Scan));
	word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	area_end = ((MANIFEST_CLOSURE_END (Scan, count)) - 1);

	while ((--count) >= 0)
	{
	  Scan = ((SCHEME_OBJECT *) (word_ptr));
	  word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	  EXTRACT_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	  DUMP_RAW_COMPILED_ENTRY (after_closure);
	after_closure:
	  STORE_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	}
	Scan = area_end;
	END_CLOSURE_RELOCATION (Scan);
	break;
      }

      case TC_LINKAGE_SECTION:
      {
	compiled_code_present_p = true;
	switch (READ_LINKAGE_KIND (Temp))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    /* Assumes that all others are objects of type TC_QUAD without
	       their type codes.
	     */

	    fast long count;

	    Scan++;
	    for (count = (READ_CACHE_LINKAGE_COUNT (Temp));
		 --count >= 0;
		 Scan += 1)
	    {
	      Temp = (* Scan);
	      DUMP_RAW_POINTER (Fasdump_Setup_Pointer
				(TRANSPORT_RAW_TRIPLE (),
				 RAW_BH (false, continue)));
	    }
	    Scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    fast long count;
	    fast char * word_ptr;
	    SCHEME_OBJECT * end_scan;

	    START_OPERATOR_RELOCATION (Scan);
	    count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	    end_scan = (END_OPERATOR_LINKAGE_AREA (Scan, count));

	    while (--count >= 0)
	    {
	      Scan = ((SCHEME_OBJECT *) (word_ptr));
	      word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	      EXTRACT_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	      DUMP_RAW_COMPILED_ENTRY (after_operator);
	    after_operator:
	      STORE_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	    }
	    Scan = end_scan;
	    END_OPERATOR_RELOCATION (Scan);
	    break;
	  }

	  case CLOSURE_PATTERN_LINKAGE_KIND:
	    Scan += (READ_CACHE_LINKAGE_COUNT (Temp));
	    break;

	  default:
	  {
	    gc_death (TERM_EXIT,
		      "fasdump: Unknown compiler linkage kind.",
		      Scan, Free);
	    /*NOTREACHED*/
	  }
	}
	break;
      }

      case_Cell:
	Setup_Pointer_for_Dump (Transport_Cell ());
	break;

      case TC_REFERENCE_TRAP:
	if ((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* Fall through. */

      case TC_WEAK_CONS:
      case_Fasdump_Pair:
	Setup_Pointer_for_Dump (Transport_Pair ());
	break;

      case TC_INTERNED_SYMBOL:
	Setup_Pointer_for_Dump (Fasdump_Symbol (BROKEN_HEART_ZERO));
	break;

      case TC_UNINTERNED_SYMBOL:
	Setup_Pointer_for_Dump (Fasdump_Symbol (UNBOUND_OBJECT));
	break;

      case_Triple:
	Setup_Pointer_for_Dump (Transport_Triple ());
	break;

      case TC_VARIABLE:
	Setup_Pointer_for_Dump (Fasdump_Variable ());
	break;

      case_Quadruple:
	Setup_Pointer_for_Dump (Transport_Quadruple ());
	break;

      case_Aligned_Vector:
	Dump_Pointer (Fasdump_Setup_Aligned (goto Move_Vector,
					     Normal_BH (false, continue)));
	break;

      case_Purify_Vector:
      process_vector:
	Setup_Pointer_for_Dump (Transport_Vector ());
	break;

      case TC_ENVIRONMENT:
	if (mode == 1)
	  goto process_vector;
	/* Make fasdump fail */
	result = ERR_FASDUMP_ENVIRONMENT;
	goto exit_dumploop;

      case TC_FUTURE:
	Setup_Pointer_for_Dump (Transport_Future ());
	break;

      default:
	GC_BAD_TYPE ("dumploop", Temp);
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

#define DUMPLOOP(obj, mode)						\
{									\
  long value;								\
									\
  value = (DumpLoop (obj, mode));					\
  if (value != PRIM_DONE)						\
  {									\
    PRIMITIVE_RETURN (Fasdump_Exit (value, false));			\
  }									\
}

#define FASDUMP_INTERRUPT()						\
{									\
  PRIMITIVE_RETURN (Fasdump_Exit (PRIM_INTERRUPT, false));		\
}

SCHEME_OBJECT
DEFUN (Fasdump_Exit, (code, close_p), long code AND Boolean close_p)
{
  Boolean result;
  fast SCHEME_OBJECT *Fixes;

  Fixes = Fixup;
  if (close_p)
    OS_channel_close_noerror (dump_channel);

  result = true;
  while (Fixes != NewMemTop)
  {
    fast SCHEME_OBJECT *Fix_Address;

    Fix_Address = (OBJECT_ADDRESS (*Fixes++)); /* Where it goes. */
    *Fix_Address = *Fixes++;             /* Put it there. */
  }
  Fixup = Fixes;
  if ((close_p) && ((!result) || (code != PRIM_DONE)))
    OS_file_remove (dump_file_name);

  dump_file_name = ((char *) 0);
  Fasdump_Exit_Hook ();
  if (!result)
  {
    signal_error_from_primitive (ERR_IO_ERROR);
    /*NOTREACHED*/
    return (0);
  }
  if (code == PRIM_DONE)
    return (SHARP_T);
  else if (code == PRIM_INTERRUPT)
    return (SHARP_F);
  else
  {
    signal_error_from_primitive (code);
    /*NOTREACHED*/
    return (0);
  }
}

/* (PRIMITIVE-FASDUMP object-to-dump filename-or-channel flag)

   Dump an object into a file so that it can be loaded using
   BINARY-FASLOAD.  A spare heap is required for this operation.  The
   first argument is the object to be dumped.  The second is the
   filename or channel.  The primitive returns #T or #F indicating
   whether it successfully dumped the object (it can fail on an object
   that is too large).  It should signal an error rather than return
   false, but ... some other time.

   The third argument, FLAG, specifies how to handle the dumping of
   environment objects:
   - SHARP_F means that it is an error to dump an object containing
   environment objects.
   - SHARP_T means that they should be dumped as if they were ordinary
   objects.
   - anything else means that the environment objects pointed at by
   compiled code blocks should be eliminated on the dumped copy,
   but other environments are not allowed.
*/

DEFINE_PRIMITIVE ("PRIMITIVE-FASDUMP", Prim_prim_fasdump, 3, 3, 0)
{
  Tchannel channel = NO_CHANNEL;
  Boolean arg_string_p;
  SCHEME_OBJECT Object, *New_Object, arg2, flag;
  SCHEME_OBJECT * prim_table_start, * prim_table_end;
  long Length, prim_table_length;
  Boolean result;
  PRIMITIVE_HEADER (3);

  Object = (ARG_REF (1));
  arg2 = (ARG_REF (2));
  arg_string_p = (STRING_P (arg2));
  if (!arg_string_p)
    channel = (arg_channel (2));
  flag = (ARG_REF (3));

  compiled_code_present_p = false;

  prim_table_end = &Free[(Space_Before_GC ())];
  prim_table_start = (initialize_primitive_table (Free, prim_table_end));
  if (prim_table_start >= prim_table_end)
    Primitive_GC (prim_table_start - Free);

  Fasdump_Free_Calc (NewFree, NewMemTop);
  Fixup = NewMemTop;
  ALIGN_FLOAT (NewFree);
  New_Object = NewFree;
  *NewFree++ = Object;

  if (arg_string_p)
  {
    /* This needs to be done before Fasdump_Exit is called.
       DUMPLOOP may do that.
       It should not be done if the primitive will not call
       Fasdump_Exit on its way out (ie. Primitive_GC above).
     */
    dump_file_name = ((CONST char *) (STRING_LOC (arg2, 0)));
  }

  DUMPLOOP (New_Object,
	    ((flag == SHARP_F) ? 0 : ((flag == SHARP_T) ? 1 : 2)));
  Length = (NewFree - New_Object);
  prim_table_start = NewFree;
  prim_table_end = (cons_primitive_table (NewFree, Fixup, &prim_table_length));
  if (prim_table_end >= Fixup)
    FASDUMP_INTERRUPT ();

#ifdef NATIVE_CODE_IS_C

  /* Cannot dump C compiled code. */

  if (compiled_code_present_p)
    PRIMITIVE_RETURN (Fasdump_Exit (ERR_COMPILED_CODE_ERROR, false));

#endif /* NATIVE_CODE_IS_C */

  if (arg_string_p)
  {
    channel = (OS_open_dump_file (dump_file_name));
    if (channel == NO_CHANNEL)
      PRIMITIVE_RETURN (Fasdump_Exit (ERR_ARG_2_BAD_RANGE, false));
  }

  dump_channel = channel;
  result = (Write_File (New_Object,
			Length, New_Object,
			0, Constant_Space,
			prim_table_start, prim_table_length,
			((long) (prim_table_end - prim_table_start)),
			prim_table_end, 0, 0,
			compiled_code_present_p, false));

  PRIMITIVE_RETURN (Fasdump_Exit ((result ? PRIM_DONE : PRIM_INTERRUPT),
				  arg_string_p));
}

/* (DUMP-BAND PROCEDURE FILE-NAME)
   Saves all of the heap and pure space on FILE-NAME.  When the
   file is loaded back using BAND_LOAD, PROCEDURE is called with an
   argument of #F.
*/

DEFINE_PRIMITIVE ("DUMP-BAND", Prim_band_dump, 2, 2, 0)
{
  SCHEME_OBJECT
    Combination, * saved_free,
    * prim_table_start, * prim_table_end,
    * c_table_start, * c_table_end;
  long
    prim_table_length,
    c_table_length;
  Boolean result = false;
  PRIMITIVE_HEADER (2);

  Band_Dump_Permitted ();
  CHECK_ARG (1, INTERPRETER_APPLICABLE_P);
  CHECK_ARG (2, STRING_P);
  if (Unused_Heap_Bottom < Heap_Bottom)
    /* Cause the image to be in the low heap, to increase
       the probability that no relocation is needed on reload. */
    Primitive_GC (0);
  Primitive_GC_If_Needed (5);
  saved_free = Free;
  Combination = (MAKE_POINTER_OBJECT (TC_COMBINATION_1, Free));
  Free[COMB_1_FN] = (ARG_REF (1));
  Free[COMB_1_ARG_1] = SHARP_F;
  Free += 2;
  (* Free++) = Combination;
  (* Free++) = compiler_utilities;
  (* Free) = (MAKE_POINTER_OBJECT (TC_LIST, (Free - 2)));
  Free ++;  /* Some compilers are TOO clever about this and increment Free
	      before calculating Free-2! */
  prim_table_start = Free;
  prim_table_end = (cons_whole_primitive_table (prim_table_start,
						Heap_Top,
						&prim_table_length));
  if (prim_table_end >= Heap_Top)
    goto done;

  c_table_start = prim_table_end;
  c_table_end = (cons_c_code_table (c_table_start, Heap_Top, &c_table_length));
  if (c_table_end >= Heap_Top)
    goto done;

  {
    SCHEME_OBJECT * faligned_heap, * faligned_constant;
    CONST char * filename = ((CONST char *) (STRING_LOC ((ARG_REF (2)), 0)));

    OS_file_remove_link (filename);
    dump_channel = (OS_open_dump_file (filename));
    if (dump_channel == NO_CHANNEL)
      error_bad_range_arg (2);

    for (faligned_heap = Heap_Bottom;
	 (! (FLOATING_ALIGNED_P (faligned_heap)));
	 faligned_heap += 1)
      ;
    
    for (faligned_constant = Constant_Space;
	 (! (FLOATING_ALIGNED_P (faligned_constant)));
	 faligned_constant += 1)
      ;

    result = (Write_File ((Free - 1),
			  ((long) (Free - faligned_heap)),
			  faligned_heap,
			  ((long) (Free_Constant - faligned_constant)),
			  faligned_constant,
			  prim_table_start, prim_table_length,
			  ((long) (prim_table_end - prim_table_start)),
			  c_table_start, c_table_length,
			  ((long) (c_table_end - c_table_start)),
			  (compiler_utilities != SHARP_F), true));
    OS_channel_close_noerror (dump_channel);
    if (! result)
      OS_file_remove (filename);
  }

done:
  Band_Dump_Exit_Hook ();
  Free = saved_free;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (result));
}
