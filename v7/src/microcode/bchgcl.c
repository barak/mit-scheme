/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchgcl.c,v 9.40 1990/04/01 20:26:39 jinx Rel $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

/* bchgcl, bchmmg, bchpur, and bchdmp can replace gcloop, memmag,
   purify, and fasdump, respectively, to provide garbage collection
   and related utilities to disk. */

#include "scheme.h"
#include "bchgcc.h"

SCHEME_OBJECT *
GCLoop(Scan, To_ptr, To_Address_ptr)
     fast SCHEME_OBJECT *Scan;
     SCHEME_OBJECT **To_ptr, **To_Address_ptr;
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
		  "gcloop: broken heart (0x%lx) in scan",
		  Temp);
	  gc_death(TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	  /*NOTREACHED*/
	}
	if (Scan != scan_buffer_top)
	  goto end_gcloop;
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
	  Scan = ((dump_and_reload_scan_buffer
		   ((overflow / GC_DISK_BUFFER_SIZE), NULL) +
		   (overflow % GC_DISK_BUFFER_SIZE)) - 1);
	  break;
	}

      case_compiled_entry_point:
	relocate_compiled_entry(true);
	*Scan = Temp;
	break;

      case TC_LINKAGE_SECTION:
      {
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
	      extend_scan_buffer ((char *) next_ptr, To);
	      relocate_linked_operator (true);
	      next_ptr = ((char *)
			  (end_scan_buffer_extension ((char *) next_ptr)));
	      overflow -= GC_DISK_BUFFER_SIZE;
	    }
	    else
	    {
	      relocate_linked_operator (true);
	    }
	  }
	  Scan = (scan_buffer_top + overflow);
	  break;
	}
      }

      case TC_MANIFEST_CLOSURE:
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
	  if ((CLOSURE_ENTRY_END (word_ptr)) > ((char *) scan_buffer_top))
	  {
	    char *entry_end;
	    long de, dw;

	    entry_end = (CLOSURE_ENTRY_END (word_ptr));
	    de = (end_ptr - entry_end);
	    dw = (entry_end - word_ptr);
	    extend_scan_buffer (((char *) entry_end), To);
	    relocate_manifest_closure(true);
	    entry_end = ((char *)
			 (end_scan_buffer_extension ((char *) entry_end)));
	    word_ptr = (entry_end - dw);
	    end_ptr = (entry_end + de);
	  }
	  else
	  {
	    relocate_manifest_closure(true);
	  }
	}
	Scan = ((SCHEME_OBJECT *) (end_ptr));
	break;
      }

      case_Cell:
	relocate_normal_pointer(copy_cell(), 1);

      case TC_REFERENCE_TRAP:
	if (OBJECT_DATUM (Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* It is a pair, fall through. */
      case_Pair:
	relocate_normal_pointer(copy_pair(), 2);

      case TC_VARIABLE:
      case_Triple:
	relocate_normal_pointer(copy_triple(), 3);

      case_Quadruple:
	relocate_normal_pointer(copy_quadruple(), 4);

      case TC_BIG_FLONUM:
	relocate_flonum_setup();
	goto Move_Vector;

      case_Vector:
	relocate_normal_setup();
      Move_Vector:
	copy_vector(NULL);
	relocate_normal_end();

      case TC_FUTURE:
	relocate_normal_setup();
	if (!(Future_Spliceable(Temp)))
	{
	  goto Move_Vector;
	}
	*Scan = Future_Value(Temp);
	Scan -= 1;
	continue;

      case TC_WEAK_CONS:
	relocate_normal_pointer(copy_weak_pair(), 2);

      default:
	GC_BAD_TYPE("gcloop");
	/* Fall Through */

      case_Non_Pointer:
	break;
      }
  }
end_gcloop:
  *To_ptr = To;
  *To_Address_ptr = To_Address;
  return (Scan);
}
