/* -*-C-*-

$Id: bchgcl.c,v 9.54 2001/12/16 06:01:32 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*/

/* This is the main GC loop for bchscheme.  */

#include "scheme.h"
#include "bchgcc.h"

#define MAYBE_DUMP_FREE(free)						\
{									\
  if (free >= free_buffer_top)						\
    DUMP_FREE (free);							\
}

#define DUMP_FREE(free)							\
  free = (dump_and_reset_free_buffer (free, 0))

#define MAYBE_DUMP_SCAN(scan)						\
{									\
  if (scan >= scan_buffer_top)						\
    DUMP_SCAN (scan);							\
}

#define DUMP_SCAN(scan)							\
  scan = (dump_and_reload_scan_buffer (scan, 0))

#define TRANSPORT_VECTOR(new_address, free, old_start, n_words)		\
{									\
  SCHEME_OBJECT * old_ptr = old_start;					\
  SCHEME_OBJECT * free_end = (free + n_words);				\
  if (free_end < free_buffer_top)					\
    while (free < free_end)						\
      (*free++) = (*old_ptr++);						\
  else									\
    {									\
      while (free < free_buffer_top)					\
	(*free++) = (*old_ptr++);					\
      free = (transport_vector_tail (free, free_end, old_ptr));		\
    }									\
}

static SCHEME_OBJECT *
DEFUN (transport_vector_tail, (free, free_end, tail),
       SCHEME_OBJECT * free AND
       SCHEME_OBJECT * free_end AND
       SCHEME_OBJECT * tail)
{
  unsigned long n_words = (free_end - free);
  DUMP_FREE (free);
  {
    unsigned long n_blocks = (n_words >> gc_buffer_shift);
    if (n_blocks > 0)
      {
	free = (dump_free_directly (tail, n_blocks, 0));
	tail += (n_blocks << gc_buffer_shift);
      }
  }
  {
    SCHEME_OBJECT * free_end = (free + (n_words & gc_buffer_mask));
    while (free < free_end)
      (*free++) = (*tail++);
  }
  return (free);
}

SCHEME_OBJECT *
DEFUN (gc_loop,
       (scan, free_ptr, new_address_ptr, low_heap, gc_mode,
	require_normal_end),
       SCHEME_OBJECT * scan AND
       SCHEME_OBJECT ** free_ptr AND
       SCHEME_OBJECT ** new_address_ptr AND
       SCHEME_OBJECT * low_heap AND
       gc_mode_t gc_mode AND
       int require_normal_end)
{
  SCHEME_OBJECT * free = (*free_ptr);
  SCHEME_OBJECT * new_address = (*new_address_ptr);
  while (scan != free)
    {
      SCHEME_OBJECT object;
      if (scan >= scan_buffer_top)
	{
	  if (scan == scan_buffer_top)
	    DUMP_SCAN (scan);
	  else
	    {
	      sprintf
		(gc_death_message_buffer,
		 "gc_loop: scan (0x%lx) > scan_buffer_top (0x%lx)",
		 ((unsigned long) scan),
		 ((unsigned long) scan_buffer_top));
	      gc_death (TERM_EXIT, gc_death_message_buffer, scan, free);
	      /*NOTREACHED*/
	    }
	}
      object = (*scan);
      switch (OBJECT_TYPE (object))
	{
	case TC_BROKEN_HEART:
	  if (gc_mode != NORMAL_GC)
	    goto end_gc_loop;
	  if (object == (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan)))
	    /* Does this ever happen?  */
	    goto end_gc_loop;
	  sprintf (gc_death_message_buffer,
		   "gc_loop: broken heart (0x%lx) in scan",
		   object);
	  gc_death (TERM_BROKEN_HEART, gc_death_message_buffer, scan, free);
	  /*NOTREACHED*/
	  break;

	case TC_CHARACTER:
	case TC_CONSTANT:
	case TC_FIXNUM:
	case TC_NULL:
	case TC_PCOMB0:
	case TC_PRIMITIVE:
	case TC_RETURN_CODE:
	case TC_STACK_ENVIRONMENT:
	case TC_THE_ENVIRONMENT:
	  scan += 1;
	  break;

	case TC_CELL:
	  if (gc_mode == CONSTANT_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		(*free++) = (old_start[0]);
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 1;
	      }
	  }
	  break;

	case TC_ACCESS:
	case TC_ASSIGNMENT:
	case TC_COMBINATION_1:
	case TC_COMMENT:
	case TC_COMPLEX:
	case TC_DEFINITION:
	case TC_DELAY:
	case TC_DELAYED:
	case TC_DISJUNCTION:
	case TC_ENTITY:
	case TC_EXTENDED_PROCEDURE:
	case TC_IN_PACKAGE:
	case TC_LAMBDA:
	case TC_LEXPR:
	case TC_LIST:
	case TC_PCOMB1:
	case TC_PROCEDURE:
	case TC_RATNUM:
	case TC_SCODE_QUOTE:
	case TC_SEQUENCE_2:
	transport_pair:
	  if (gc_mode == CONSTANT_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  goto really_transport_pair;

	case TC_INTERNED_SYMBOL:
	case TC_UNINTERNED_SYMBOL:
	  if (gc_mode == PURE_COPY)
	    {
	      SCHEME_OBJECT name = (MEMORY_REF (object, SYMBOL_NAME));
	      SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (name));
	      if ((old_start < low_heap)
		  || (BROKEN_HEART_P (*old_start)))
		scan += 1;
	      else
		{
		  unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		  TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		  (*scan++) = (OBJECT_NEW_ADDRESS (name, new_address));
		  (*old_start) = (MAKE_BROKEN_HEART (new_address));
		  new_address += n_words;
		}
	      break;
	    }
	really_transport_pair:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		(*free++) = (old_start[0]);
		(*free++) = (old_start[1]);
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 2;
	      }
	  }
	  break;

	case TC_COMBINATION_2:
	case TC_CONDITIONAL:
	case TC_EXTENDED_LAMBDA:
	case TC_HUNK3_A:
	case TC_HUNK3_B:
	case TC_PCOMB2:
	case TC_SEQUENCE_3:
	case TC_VARIABLE:
	  if (gc_mode == CONSTANT_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		(*free++) = (old_start[0]);
		(*free++) = (old_start[1]);
		(*free++) = (old_start[2]);
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 3;
	      }
	  }
	  break;

	case TC_QUAD:
	  if (gc_mode == CONSTANT_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		(*free++) = (old_start[0]);
		(*free++) = (old_start[1]);
		(*free++) = (old_start[2]);
		(*free++) = (old_start[3]);
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 4;
	      }
	  }
	  break;

	case TC_BIG_FIXNUM:
	case TC_CHARACTER_STRING:
	case TC_COMBINATION:
	case TC_CONTROL_POINT:
	case TC_NON_MARKED_VECTOR:
	case TC_PCOMB3:
	case TC_RECORD:
	case TC_VECTOR:
	case TC_VECTOR_16B:
	case TC_VECTOR_1B:
	  if (gc_mode == CONSTANT_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  goto transport_vector;

	case TC_ENVIRONMENT:
	  if (gc_mode == PURE_COPY)
	    {
	      scan += 1;
	      break;
	    }
	transport_vector:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += n_words;
	      }
	  }
	  break;

	case TC_BIG_FLONUM:
	  if (gc_mode == CONSTANT_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  goto transport_aligned_vector;

	case TC_COMPILED_CODE_BLOCK:
	  if (gc_mode == PURE_COPY)
	    {
	      scan += 1;
	      break;
	    }
	transport_aligned_vector:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		BCH_ALIGN_FLOAT (new_address, free);
		TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += n_words;
	      }
	  }
	  break;

	case TC_WEAK_CONS:
	  if (gc_mode == PURE_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		SCHEME_OBJECT weak_car = (old_start[0]);
		if (((OBJECT_TYPE (weak_car)) == TC_NULL)
		    || ((OBJECT_ADDRESS (weak_car)) < low_heap))
		  {
		    (*free++) = weak_car;
		    (*free++) = (old_start[1]);
		  }
		else if (weak_pair_stack_ptr > weak_pair_stack_limit)
		  {
		    (*--weak_pair_stack_ptr) = ((SCHEME_OBJECT) new_address);
		    (*--weak_pair_stack_ptr) = weak_car;
		    (*free++) = SHARP_F;
		    (*free++) = (old_start[1]);
		  }
		else
		  {
		    (*free++) = (OBJECT_NEW_TYPE (TC_NULL, weak_car));
		    (*free++) = (old_start[1]);
		    (old_start[1])
		      = (MAKE_OBJECT_FROM_OBJECTS (weak_car, Weak_Chain));
		    Weak_Chain = object;
		  }
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 2;
	      }
	  }
	  break;

	case TC_MANIFEST_NM_VECTOR:
	case TC_MANIFEST_SPECIAL_NM_VECTOR:
	  scan += (1 + (OBJECT_DATUM (object)));
	  MAYBE_DUMP_SCAN (scan);
	  break;

	case TC_REFERENCE_TRAP:
	  if ((OBJECT_DATUM (object)) > TRAP_MAX_IMMEDIATE)
	    goto transport_pair;
	  /* Otherwise it's a non-pointer.  */
	  scan += 1;
	  break;

	case TC_COMPILED_ENTRY:
	  if (gc_mode == PURE_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  {
	    SCHEME_OBJECT * old_start;
	    Get_Compiled_Block (old_start, (OBJECT_ADDRESS (object)));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++)
		= (RELOCATE_COMPILED (object,
				      (OBJECT_ADDRESS (*old_start)),
				      old_start));
	    else
	      {
		unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		BCH_ALIGN_FLOAT (new_address, free);
		TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		(*scan++)
		  = (RELOCATE_COMPILED (object, new_address, old_start));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += n_words;
	      }
	  }
	  break;

	case TC_LINKAGE_SECTION:
	  if (gc_mode == PURE_COPY)
	    {
	      gc_death (TERM_COMPILER_DEATH,
			"gc_loop: linkage section in pure area",
			scan, free);
	      /*NOTREACHED*/
	    }
	  switch (READ_LINKAGE_KIND (object))
	    {
	    case REFERENCE_LINKAGE_KIND:
	    case ASSIGNMENT_LINKAGE_KIND:
	      {
		/* `count' typeless pointers to hunk3s follow. */
		unsigned long count = (READ_CACHE_LINKAGE_COUNT (object));
		scan += 1;
		while (count > 0)
		  {
		    SCHEME_OBJECT * old_start;
		    MAYBE_DUMP_SCAN (scan);
		    old_start = (SCHEME_ADDR_TO_ADDR (*scan));
		    if (old_start < low_heap)
		      scan += 1;
		    else if (BROKEN_HEART_P (*old_start))
		      (*scan++)
			= (ADDR_TO_SCHEME_ADDR (OBJECT_ADDRESS (*old_start)));
		    else
		      {
			(*free++) = (old_start[0]);
			(*free++) = (old_start[1]);
			(*free++) = (old_start[2]);
			MAYBE_DUMP_FREE (free);
			(*scan++) = (ADDR_TO_SCHEME_ADDR (new_address));
			(*old_start) = (MAKE_BROKEN_HEART (new_address));
			new_address += 3;
		      }
		    count -= 1;
		  }
	      }
	      break;

	    case OPERATOR_LINKAGE_KIND:
	    case GLOBAL_OPERATOR_LINKAGE_KIND:
	      {
		unsigned long count = (READ_OPERATOR_LINKAGE_COUNT (object));
		char * entry = (FIRST_OPERATOR_LINKAGE_ENTRY (scan));
		long delta;

		{
		  int extend_p = (entry >= ((char *) scan_buffer_top));
		  long delta1 = (((char *) scan) - entry);
		  if (extend_p)
		    extend_scan_buffer (entry, free);
		  BCH_START_OPERATOR_RELOCATION (scan);
		  if (extend_p)
		    {
		      entry = (end_scan_buffer_extension (entry));
		      scan = ((SCHEME_OBJECT *) (entry + delta1));
		    }
		}

		/* END_OPERATOR_LINKAGE_AREA assumes that we will add
		   one to the result, so do that now.  */
		delta
		  = (((END_OPERATOR_LINKAGE_AREA (scan, count)) + 1)
		     - scan_buffer_top);

		/* The operator entries are copied sequentially, but
		   extra hair is required because the entry addresses
		   are encoded.  */
		while (count > 0)
		  {
		    char * next_entry = (NEXT_LINKAGE_OPERATOR_ENTRY (entry));
		    int extend_p = (next_entry >= ((char *) scan_buffer_top));
		    SCHEME_OBJECT esaddr;
		    SCHEME_OBJECT * old_start;

		    /* Guarantee that the scan buffer is large enough
		       to hold the entry.  */
		    if (extend_p)
		      extend_scan_buffer (next_entry, free);

		    /* Get the entry address.  */
		    BCH_EXTRACT_OPERATOR_LINKAGE_ADDRESS (esaddr, entry);

		    /* Get the code-block pointer for this entry.  */
		    Get_Compiled_Block
		      (old_start, (SCHEME_ADDR_TO_ADDR (esaddr)));

		    /* Copy the block.  */
		    if (old_start < low_heap)
		      ;
		    else if (BROKEN_HEART_P (*old_start))
		      {
			BCH_STORE_OPERATOR_LINKAGE_ADDRESS
			  ((RELOCATE_COMPILED_RAW_ADDRESS
			    (esaddr,
			     (OBJECT_ADDRESS (*old_start)),
			     old_start)),
			   entry);
		      }
		    else
		      {
			unsigned long n_words
			  = (1 + (OBJECT_DATUM (*old_start)));
			BCH_ALIGN_FLOAT (new_address, free);
			TRANSPORT_VECTOR
			  (new_address, free, old_start, n_words);
			BCH_STORE_OPERATOR_LINKAGE_ADDRESS
			  ((RELOCATE_COMPILED_RAW_ADDRESS
			    (esaddr, new_address, old_start)),
			   entry);
			(*old_start) = (MAKE_BROKEN_HEART (new_address));
			new_address += n_words;
		      }

		    if (extend_p)
		      {
			entry = (end_scan_buffer_extension (next_entry));
			delta -= gc_buffer_size;
		      }
		    else
		      entry = next_entry;

		    count -= 1;
		  }
		scan = (scan_buffer_top + delta);
		MAYBE_DUMP_SCAN (scan);
		BCH_END_OPERATOR_RELOCATION (scan);
	      }
	      break;

	    case CLOSURE_PATTERN_LINKAGE_KIND:
	      scan += (1 + (READ_CACHE_LINKAGE_COUNT (object)));
	      MAYBE_DUMP_SCAN (scan);
	      break;

	    default:
	      gc_death (TERM_EXIT, "gc_loop: Unknown compiler linkage kind.",
			scan, free);
	      /*NOTREACHED*/
	      break;
	    }
	  break;

	case TC_MANIFEST_CLOSURE:
	  if (gc_mode == PURE_COPY)
	    {
	      gc_death (TERM_COMPILER_DEATH,
			"gc_loop: manifest closure in pure area",
			scan, free);
	      /*NOTREACHED*/
	    }
	  {
	    unsigned long count;
	    char * entry;
	    char * closure_end;

	    {
	      unsigned long delta = (2 * (sizeof (format_word)));
	      char * count_end = (((char *) (scan + 1)) + delta);
	      int extend_p = (count_end >= ((char *) scan_buffer_top));

	      /* Guarantee that the scan buffer is large enough to
		 hold the count field.  */
	      if (extend_p)
		extend_scan_buffer (count_end, free);

	      BCH_START_CLOSURE_RELOCATION (scan);
	      count = (MANIFEST_CLOSURE_COUNT (scan + 1));
	      entry = (FIRST_MANIFEST_CLOSURE_ENTRY (scan + 1));

	      if (extend_p)
		{
		  long dw = (entry - count_end);
		  count_end = (end_scan_buffer_extension (count_end));
		  entry = (count_end + dw);
		}
	      scan = ((SCHEME_OBJECT *) (count_end - delta));
	    }

	    closure_end = ((char *) (MANIFEST_CLOSURE_END (scan, count)));

	    /* The closures are copied sequentially, but extra hair is
	       required because the code-entry pointers are encoded as
	       machine instructions.  */
	    while (count > 0)
	      {
		char * entry_end = (CLOSURE_ENTRY_END (entry));
		int extend_p = (entry_end >= ((char *) scan_buffer_top));
		SCHEME_OBJECT esaddr;
		SCHEME_OBJECT * old_start;
		long delta1 = (entry - entry_end);
		long delta2 = (closure_end - entry_end);

		/* If the closure overflows the scan buffer, extend
		   the buffer to the end of the closure.  */
		if (extend_p)
		  extend_scan_buffer (entry_end, free);

		/* Extract the code-entry pointer and convert it to a
		   C pointer.  */
		BCH_EXTRACT_CLOSURE_ENTRY_ADDRESS (esaddr, entry);
		Get_Compiled_Block (old_start, (SCHEME_ADDR_TO_ADDR (esaddr)));

		/* Copy the code entry.  Use machine-specific macro to
		   update the pointer. */
		if (old_start < low_heap)
		  ;
		else if (BROKEN_HEART_P (*old_start))
		  BCH_STORE_CLOSURE_ENTRY_ADDRESS
		    ((RELOCATE_COMPILED_RAW_ADDRESS
		      (esaddr, (OBJECT_ADDRESS (*old_start)), old_start)),
		     entry);
		else
		  {
		    unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		    BCH_ALIGN_FLOAT (new_address, free);
		    TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		    BCH_STORE_CLOSURE_ENTRY_ADDRESS
		      ((RELOCATE_COMPILED_RAW_ADDRESS
			(esaddr, new_address, old_start)),
		       entry);
		    (*old_start) = (MAKE_BROKEN_HEART (new_address));
		    new_address += n_words;
		  }

		if (extend_p)
		  {
		    entry_end = (end_scan_buffer_extension (entry_end));
		    entry = (entry_end + delta1);
		    closure_end = (entry_end + delta2);
		  }

		entry = (NEXT_MANIFEST_CLOSURE_ENTRY (entry));
		count -= 1;
	      }
	    scan = ((SCHEME_OBJECT *) closure_end);
	    MAYBE_DUMP_SCAN (scan);
	    BCH_END_CLOSURE_RELOCATION (scan);
	  }
	  break;

	case TC_FUTURE:
	  if (gc_mode == CONSTANT_COPY)
	    {
	      scan += 1;
	      break;
	    }
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (old_start < low_heap)
	      scan += 1;
	    else if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else if (Future_Spliceable (object))
	      (*scan) = (Future_Value (object));
	    else
	      {
		unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += n_words;
	      }
	  }
	  break;

	default:
	  GC_BAD_TYPE ("gc_loop", object);
	  scan += 1;
	  break;
	}
    }
 end_gc_loop:
  (*free_ptr) = free;
  (*new_address_ptr) = new_address;
  if (require_normal_end && (scan != free))
    {
      gc_death (TERM_BROKEN_HEART, "gc_loop ended too early", scan, free);
      /*NOTREACHED*/
    }
  return (scan);
}
