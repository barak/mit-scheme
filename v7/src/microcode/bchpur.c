/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchpur.c,v 9.48 1989/12/06 05:49:09 jinx Exp $

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

/*
 * This file contains the code for primitives dealing with pure
 * and constant space.  Garbage collection to disk version.
 *
 * Poorly implemented:  If there is not enough space, instead of
 * undoing the changes, it crashes.
 * It should be changed to do the job in two passes like the
 * "normal" version.
 */

#include "scheme.h"
#include "prims.h"
#include "bchgcc.h"

#ifdef FLOATING_ALIGNMENT
/* This must be fixed. */
#include "error: bchpur does not handle floating alignment."
#endif

/* Purify modes */

#define	NORMAL_GC	0
#define PURE_COPY	1
#define CONSTANT_COPY	2

/* Some utility macros. */

#define relocate_indirect_setup()					\
{									\
  Old = OBJECT_ADDRESS (Temp);						\
  if (Old >= Low_Constant)						\
    continue;								\
  if (OBJECT_TYPE (*Old) == TC_BROKEN_HEART)				\
  {									\
    continue;								\
  }									\
  New_Address = (MAKE_BROKEN_HEART (To_Address));			\
}

#define relocate_indirect_end()						\
{									\
  *OBJECT_ADDRESS (Temp) = New_Address;					\
  continue;								\
}

/* A modified copy of GCLoop. */

SCHEME_OBJECT *
purifyloop(Scan, To_ptr, To_Address_ptr, purify_mode)
     fast SCHEME_OBJECT *Scan;
     SCHEME_OBJECT **To_ptr, **To_Address_ptr;
     int purify_mode;
{
  fast SCHEME_OBJECT *To, *Old, Temp, *Low_Constant, *To_Address, New_Address;

  To = *To_ptr;
  To_Address = *To_Address_ptr;
  Low_Constant = Constant_Space;

  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;
    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
        if (Scan != (OBJECT_ADDRESS (Temp)))
	{
	  sprintf(gc_death_message_buffer,
		  "purifyloop: broken heart (0x%lx) in scan",
		  Temp);
	  gc_death(TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	  /*NOTREACHED*/
	}
	if (Scan != scan_buffer_top)
	  goto end_purifyloop;
	/* The -1 is here because of the Scan++ in the for header. */
	Scan = dump_and_reload_scan_buffer(0, NULL) - 1;
	continue;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	/* Check whether this bumps over current buffer,
	   and if so we need a new bufferfull. */
	Scan += OBJECT_DATUM (Temp);
	if (Scan < scan_buffer_top)
	{
	  break;
	}
	else
	{
	  unsigned long overflow;

	  /* The + & -1 are here because of the Scan++ in the for header. */
	  overflow = (Scan - scan_buffer_top) + 1;
	  Scan = ((dump_and_reload_scan_buffer((overflow / GC_DISK_BUFFER_SIZE), NULL) +
		   (overflow % GC_DISK_BUFFER_SIZE)) - 1);
	  break;
	}

      case_compiled_entry_point:
	if (purify_mode == PURE_COPY)
	  break;
	relocate_compiled_entry(false);
	*Scan = Temp;
	break;

      case TC_LINKAGE_SECTION:
      {
	if (purify_mode == PURE_COPY)
	{
	  gc_death(TERM_COMPILER_DEATH,
		   "purifyloop: linkage section in pure area",
		   Scan, To);
	  /*NOTREACHED*/
	}
	if (READ_LINKAGE_KIND(Temp) != OPERATOR_LINKAGE_KIND)
	{
	  /* count typeless pointers to quads follow. */

	  fast long count;
	  long max_count, max_here;

	  Scan++;
	  max_here = (scan_buffer_top - Scan);
	  max_count = READ_CACHE_LINKAGE_COUNT(Temp);
	  while (max_count != 0)
	  {
	    count = ((max_count > max_here) ? max_here : max_count);
	    max_count -= count;
	    for ( ; --count >= 0; Scan += 1)
	    {
	      Temp = *Scan;
	      relocate_typeless_pointer(copy_quadruple(), 4);
	    }
	    if (max_count != 0)
	    {
	      /* We stopped because we needed to relocate too many. */
	      Scan = dump_and_reload_scan_buffer(0, NULL);
	      max_here = GC_DISK_BUFFER_SIZE;
	    }
	  }
	  /* The + & -1 are here because of the Scan++ in the for header. */
	  Scan -= 1;
	  break;
	}

	else
	{
	  /* Operator linkage */

	  fast long count;
	  fast char *word_ptr, *next_ptr;
	  long overflow;

	  count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	  word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	  overflow = ((END_OPERATOR_LINKAGE_AREA (Scan, count)) -
		      scan_buffer_top);

	  for (next_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	       (--count >= 0);
	       word_ptr = next_ptr,
	       next_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr)))
	  {
	    if (next_ptr > ((char *) scan_buffer_top))
	    {
	      extend_scan_buffer (((char *) next_ptr), To);
	      relocate_linked_operator (false);
	      next_ptr = ((char *)
			  (end_scan_buffer_extension ((char *) next_ptr)));
	      overflow -= GC_DISK_BUFFER_SIZE;
	    }
	    else
	    {
	      relocate_linked_operator (false);
	    }
	  }
	  Scan = (scan_buffer_top + overflow);
	  break;
	}
      }

      case TC_MANIFEST_CLOSURE:
      {
	if (purify_mode == PURE_COPY)
	{
	  gc_death(TERM_COMPILER_DEATH,
		   "purifyloop: manifest closure in pure area",
		   Scan, To);
	  /*NOTREACHED*/
	}
      }
      {
	fast long count;
	fast char *word_ptr;
	char *end_ptr;

	Scan += 1;
	/* Is there enough space to read the count? */
	if ((((char *) Scan) + (2 * (sizeof (format_word)))) >
	    ((char *) scan_buffer_top))
	{
	  long dw;
	  char *header_end;

	  header_end = (((char *) Scan) + (2 * (sizeof (format_word))));
	  extend_scan_buffer (((char *) header_end), To);
	  count = (MANIFEST_CLOSURE_COUNT (Scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	  dw = (word_ptr - header_end);
	  header_end = ((char *)
			(end_scan_buffer_extension ((char *) header_end)));
	  word_ptr = (header_end + dw);
	  Scan = ((SCHEME_OBJECT *)
		  (header_end - (2 * (sizeof (format_word)))));
	}
	else
	{
	  count = (MANIFEST_CLOSURE_COUNT (Scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	}
	end_ptr = ((char *) (MANIFEST_CLOSURE_END (Scan, count)));

	for ( ; ((--count) >= 0);
	     (word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr))))
	{
	  if ((CLOSURE_ENTRY_END(word_ptr)) > ((char *) scan_buffer_top))
	  {
	    char *entry_end;
	    long de, dw;

	    entry_end = (CLOSURE_ENTRY_END(word_ptr));
	    de = (end_ptr - entry_end);
	    dw = (entry_end - word_ptr);
	    extend_scan_buffer(((char *) entry_end), To);
	    relocate_manifest_closure (false);
	    entry_end = ((char *)
			 (end_scan_buffer_extension((char *) entry_end)));
	    word_ptr = (entry_end - dw);
	    end_ptr = (entry_end + de);
	  }
	  else
	  {
	    relocate_manifest_closure(false);
	  }
	}
	Scan = ((SCHEME_OBJECT *) (end_ptr));
	break;
      }

      case_Cell:
	relocate_normal_pointer(copy_cell(), 1);

      case TC_REFERENCE_TRAP:
	if (OBJECT_DATUM (Temp) <= TRAP_MAX_IMMEDIATE)
	  break; /* It is a non pointer. */
	goto purify_pair;

      case TC_INTERNED_SYMBOL:
      case TC_UNINTERNED_SYMBOL:
	if (purify_mode == PURE_COPY)
	{
	  Temp = MEMORY_REF (Temp, SYMBOL_NAME);
	  relocate_indirect_setup();
	  copy_vector(NULL);
	  relocate_indirect_end();
	}
	/* Fall through. */

      case_Fasdump_Pair:
      purify_pair:
	relocate_normal_pointer(copy_pair(), 2);

      case TC_WEAK_CONS:
	if (purify_mode == PURE_COPY)
	  break;
	else
	  relocate_normal_pointer(copy_weak_pair(), 2);

      case TC_VARIABLE:
      case_Triple:
	relocate_normal_pointer(copy_triple(), 3);

      case_Quadruple:
	relocate_normal_pointer(copy_quadruple(), 4);

      case TC_COMPILED_CODE_BLOCK:
      case TC_ENVIRONMENT:
	if (purify_mode == PURE_COPY)
	  break;
	/* Fall through */

      case TC_BIG_FLONUM:
      case_Purify_Vector:
	relocate_normal_setup();
      Move_Vector:
	copy_vector(NULL);
	relocate_normal_end();

      case TC_FUTURE:
	relocate_normal_setup();
	if (!(Future_Spliceable(Temp)))
	  goto Move_Vector;
	*Scan = Future_Value(Temp);
	Scan -= 1;
	continue;

      default:
	GC_BAD_TYPE("purifyloop");
	/* Fall Through */

      case_Non_Pointer:
	break;

      }
  }
end_purifyloop:
  *To_ptr = To;
  *To_Address_ptr = To_Address;
  return (Scan);
}

/* This is not paranoia!
   The two words in the header may overflow the free buffer.
 */

SCHEME_OBJECT *
purify_header_overflow(free_buffer)
     SCHEME_OBJECT *free_buffer;
{
  SCHEME_OBJECT *scan_buffer;
  long delta;

  delta = (free_buffer - free_buffer_top);
  free_buffer = dump_and_reset_free_buffer(delta, NULL);
  scan_buffer = dump_and_reload_scan_buffer(0, NULL);
  if ((scan_buffer + delta) != free_buffer)
  {
    gc_death(TERM_EXIT, "purify: scan and free do not meet at the end",
	     (scan_buffer + delta), free_buffer);
    /*NOTREACHED*/
  }
  return (free_buffer);
}

SCHEME_OBJECT
purify(object, flag)
     SCHEME_OBJECT object, flag;
{
  long length, pure_length;
  SCHEME_OBJECT value, *Result, *free_buffer, *block_start;

  Weak_Chain = EMPTY_LIST;
  free_buffer = initialize_free_buffer();
  block_start = Free_Constant;

  Free_Constant += 2;
  *free_buffer++ = SHARP_F;	/* Pure block header. */
  *free_buffer++ = object;
  if (free_buffer >= free_buffer_top)
  {
    free_buffer =
      dump_and_reset_free_buffer((free_buffer - free_buffer_top), NULL);
  }

  if (flag == SHARP_T)
  {
    Result = purifyloop(initialize_scan_buffer(),
			&free_buffer, &Free_Constant,
			PURE_COPY);
    if (Result != free_buffer)
    {
      gc_death(TERM_BROKEN_HEART, "purify: pure copy ended too early",
	       Result, free_buffer);
      /*NOTREACHED*/
    }
    pure_length = (Free_Constant - block_start) + 1;
  }
  else
  {
    pure_length = 3;
  }

  Free_Constant += 2;
  *free_buffer++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *free_buffer++ = MAKE_OBJECT (CONSTANT_PART, pure_length);
  if (free_buffer >= free_buffer_top)
  {
    free_buffer = purify_header_overflow(free_buffer);
  }

  if (flag == SHARP_T)
  {
    Result = purifyloop(initialize_scan_buffer(),
			&free_buffer, &Free_Constant,
			CONSTANT_COPY);
  }
  else
  {
    Result = GCLoop(initialize_scan_buffer(), &free_buffer, &Free_Constant);
  }
  if (Result != free_buffer)
  {
    gc_death(TERM_BROKEN_HEART, "purify: constant copy ended too early",
	     Result, free_buffer);
    /*NOTREACHED*/
  }

  Free_Constant += 2;
  length = (Free_Constant - block_start);
  *free_buffer++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *free_buffer++ = MAKE_OBJECT (END_OF_BLOCK, (length - 1));
  if (free_buffer >= free_buffer_top)
  {
    free_buffer = purify_header_overflow(free_buffer);
  }

  end_transport(NULL);

  if (!Test_Pure_Space_Top(Free_Constant))
  {
    gc_death(TERM_NO_SPACE, "purify: object too large", NULL, NULL);
    /*NOTREACHED*/
  }

  load_buffer(0, block_start,
	      (length * sizeof(SCHEME_OBJECT)),
	      "into constant space");
  *block_start++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, pure_length);
  *block_start = MAKE_OBJECT (PURE_PART, (length - 1));
  GC(Weak_Chain);
  Set_Pure_Top();
  return (SHARP_T);
}

/* Stub.  Not needed by this version.  Terminates Scheme if invoked. */

SCHEME_OBJECT
Purify_Pass_2(info)
     SCHEME_OBJECT info;
{
  gc_death(TERM_EXIT, "Purify_Pass_2 invoked", NULL, NULL);
  /*NOTREACHED*/
}

/* (PRIMITIVE-PURIFY OBJECT PURE? SAFETY-MARGIN)

   Copy an object from the heap into constant space.  It should only
   be used through the wrapper provided in the Scheme runtime system.

   To purify an object we just copy it into Pure Space in two
   parts with the appropriate headers and footers.  The actual
   copying is done by PurifyLoop above.

   Once the copy is complete we run a full GC which handles the
   broken hearts which now point into pure space.

   This primitive does not return normally.  It always escapes into
   the interpreter because some of its cached registers (eg. History)
   have changed.  */

DEFINE_PRIMITIVE ("PRIMITIVE-PURIFY", Prim_primitive_purify, 3, 3, 0)
{
  SCHEME_OBJECT object, daemon;
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (3);
  PRIMITIVE_CANONICALIZE_CONTEXT();
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  CHECK_ARG (2, BOOLEAN_P);
  GC_Reserve = (arg_nonnegative_integer (3));
  ENTER_CRITICAL_SECTION ("purify");
  {
    SCHEME_OBJECT purify_result;
    SCHEME_OBJECT words_free;

    purify_result = (purify (object, (ARG_REF (2))));
    words_free = (LONG_TO_UNSIGNED_FIXNUM (MemTop - Free));
    result = (MAKE_POINTER_OBJECT (TC_LIST, Free));
    (*Free++) = purify_result;
    (*Free++) = words_free;
  }
  Pop_Primitive_Frame(3);
  daemon = Get_Fixed_Obj_Slot(GC_Daemon);
  if (daemon == SHARP_F)
  {
    Val = result;
    EXIT_CRITICAL_SECTION ({});
    PRIMITIVE_ABORT(PRIM_POP_RETURN);
    /*NOTREACHED*/
  }
  RENAME_CRITICAL_SECTION ("purify daemon");
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 1));
  Store_Expression(result);
  Store_Return(RC_NORMAL_GC_DONE);
  Save_Cont();
  Push(daemon);
  Push(STACK_FRAME_HEADER);
 Pushed();
  PRIMITIVE_ABORT(PRIM_APPLY);
  /*NOTREACHED*/
}
