/* -*-C-*-

$Id: bchgcl.c,v 9.50 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* bchgcl, bchmmg, bchpur, and bchdmp can replace gcloop, memmag,
   purify, and fasdump, respectively, to provide garbage collection
   and related utilities to disk. */

#include "scheme.h"
#include "bchgcc.h"

SCHEME_OBJECT *
DEFUN (GCLoop, (Scan, To_ptr, To_Address_ptr),
       fast SCHEME_OBJECT * Scan AND
       SCHEME_OBJECT ** To_ptr AND
       SCHEME_OBJECT ** To_Address_ptr)
{
  fast SCHEME_OBJECT
    * To, * Old, Temp, * low_heap,
    * To_Address, New_Address;

  To = (* To_ptr);
  To_Address = (* To_Address_ptr);
  low_heap = Constant_Top;

  for ( ; Scan != To; Scan++)
  {
    Temp = (* Scan);
    Switch_by_GC_Type (Temp)
    {
      case TC_BROKEN_HEART:
        if (Temp != (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Scan)))
	{
	  sprintf (gc_death_message_buffer,
		   "gcloop: broken heart (0x%lx) in scan",
		   Temp);
	  gc_death (TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	  /*NOTREACHED*/
	}
	if (Scan != scan_buffer_top)
	  goto end_gcloop;
	/* The -1 is here because of the Scan++ in the for header. */
	Scan = ((dump_and_reload_scan_buffer (0, NULL)) - 1);
	continue;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	/* Check whether this bumps over current buffer,
	   and if so we need a new bufferfull. */
	Scan += (OBJECT_DATUM (Temp));
area_skipped:
	if (Scan < scan_buffer_top)
	  break;
	else
	{
	  unsigned long overflow;

	  /* The + & -1 are here because of the Scan++ in the for header. */
	  overflow = ((Scan - scan_buffer_top) + 1);
	  Scan = ((dump_and_reload_scan_buffer
		   ((overflow >> gc_buffer_shift), NULL)
		   + (overflow & gc_buffer_mask)) - 1);
	  break;
	}

      case_compiled_entry_point:
	relocate_compiled_entry (true);
	(* Scan) = Temp;
	break;

      case TC_LINKAGE_SECTION:
      {
	switch (READ_LINKAGE_KIND (Temp))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    /* count typeless pointers to quads follow. */

	    fast long count;
	    long max_count, max_here;

	    Scan++;
	    max_here = (scan_buffer_top - Scan);
	    max_count = (READ_CACHE_LINKAGE_COUNT (Temp));
	    while (max_count != 0)
	    {
	      count = ((max_count > max_here) ? max_here : max_count);
	      max_count -= count;
	      for ( ; --count >= 0; Scan += 1)
	      {
		Temp = (* Scan);
		relocate_typeless_pointer (copy_quadruple (), 4);
	      }
	      if (max_count != 0)
	      {
		/* We stopped because we needed to relocate too many. */
		Scan = (dump_and_reload_scan_buffer (0, NULL));
		max_here = gc_buffer_size;
	      }
	    }
	    /* The + & -1 are here because of the Scan++ in the for header. */
	    Scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    /* Operator linkage */

	    fast long count;
	    fast char * word_ptr, * next_ptr;
	    long overflow;

	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	    if (! (word_ptr > ((char *) scan_buffer_top)))
	      BCH_START_OPERATOR_RELOCATION (Scan);
	    else
	    {
	      overflow = (word_ptr - ((char *) Scan));
	      extend_scan_buffer (word_ptr, To);
	      BCH_START_OPERATOR_RELOCATION (Scan);
	      word_ptr = (end_scan_buffer_extension (word_ptr));
	      Scan = ((SCHEME_OBJECT *) (word_ptr - overflow));
	    }
	    
	    count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	    overflow = ((END_OPERATOR_LINKAGE_AREA (Scan, count)) -
			scan_buffer_top);

	    for (next_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
		 (--count >= 0);
		 word_ptr = next_ptr,
		 next_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr)))
	    {
	      if (! (next_ptr > ((char *) scan_buffer_top)))
		relocate_linked_operator (true);
	      else
	      {
		extend_scan_buffer (next_ptr, To);
		relocate_linked_operator (true);
		next_ptr = (end_scan_buffer_extension (next_ptr));
		overflow -= gc_buffer_size;
	      }
	    }
	    Scan = (scan_buffer_top + overflow);
	    BCH_END_OPERATOR_RELOCATION (Scan);
	    break;
	  }

	  case CLOSURE_PATTERN_LINKAGE_KIND:
	    Scan += (READ_CACHE_LINKAGE_COUNT (Temp));
	    goto area_skipped;

	  default:
	    gc_death (TERM_EXIT,
		      "GC: Unknown compiler linkage kind.",
		      Scan, Free);
	    /*NOTREACHED*/
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char * word_ptr;
	char * end_ptr;

	Scan += 1;

	/* Is there enough space to read the count? */

	end_ptr = (((char *) Scan) + (2 * (sizeof (format_word))));
	if (end_ptr > ((char *) scan_buffer_top))
	{
	  long dw;

	  extend_scan_buffer (end_ptr, To);
	  BCH_START_CLOSURE_RELOCATION (Scan - 1);
	  count = (MANIFEST_CLOSURE_COUNT (Scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	  dw = (word_ptr - end_ptr);
	  end_ptr = (end_scan_buffer_extension (end_ptr));
	  word_ptr = (end_ptr + dw);
	  Scan = ((SCHEME_OBJECT *) (end_ptr - (2 * (sizeof (format_word)))));
	}
	else
	{
	  BCH_START_CLOSURE_RELOCATION (Scan - 1);
	  count = (MANIFEST_CLOSURE_COUNT (Scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	}
	end_ptr = ((char *) (MANIFEST_CLOSURE_END (Scan, count)));

	for ( ; ((--count) >= 0);
	     (word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr))))
	{
	  if (! ((CLOSURE_ENTRY_END (word_ptr)) > ((char *) scan_buffer_top)))
	    relocate_manifest_closure (true);
	  else
	  {
	    char * entry_end;
	    long de, dw;

	    entry_end = (CLOSURE_ENTRY_END (word_ptr));
	    de = (end_ptr - entry_end);
	    dw = (entry_end - word_ptr);
	    extend_scan_buffer (entry_end, To);
	    relocate_manifest_closure (true);
	    entry_end = (end_scan_buffer_extension (entry_end));
	    word_ptr = (entry_end - dw);
	    end_ptr = (entry_end + de);
	  }
	}
	Scan = ((SCHEME_OBJECT *) (end_ptr));
	BCH_END_CLOSURE_RELOCATION (Scan);
	break;
      }

      case_Cell:
	relocate_normal_pointer (copy_cell(), 1);

      case TC_REFERENCE_TRAP:
	if ((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	  /* It is a non pointer. */
	  break;
	/* It is a pair, fall through. */
      case_Pair:
	relocate_normal_pointer (copy_pair (), 2);

      case TC_VARIABLE:
      case_Triple:
	relocate_normal_pointer (copy_triple (), 3);

      case_Quadruple:
	relocate_normal_pointer (copy_quadruple (), 4);

      case_Aligned_Vector:
	relocate_flonum_setup ();
	goto Move_Vector;

      case_Vector:
	relocate_normal_setup ();
      Move_Vector:
	copy_vector (NULL);
	relocate_normal_end ();

      case TC_FUTURE:
	relocate_normal_setup ();
	if (!(Future_Spliceable (Temp)))
	{
	  goto Move_Vector;
	}
	*Scan = (Future_Value (Temp));
	Scan -= 1;
	continue;

      case TC_WEAK_CONS:
	relocate_normal_pointer (copy_weak_pair (), 2);

      default:
	GC_BAD_TYPE ("gcloop");
	/* Fall Through */

      case_Non_Pointer:
	break;
      }
  }
end_gcloop:
  (* To_ptr) = To;
  (* To_Address_ptr) = To_Address;
  return (Scan);
}
