/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchgcl.c,v 9.34 1988/08/15 20:36:15 cph Exp $ */

/* bchgcl, bchmmg, bchpur, and bchdmp can replace gcloop, memmag,
   purify, and fasdump, respectively, to provide garbage collection
   and related utilities to disk.
*/

#include "scheme.h"
#include "bchgcc.h"

#ifdef FLOATING_ALIGNMENT
/* This must be fixed. */
#include "error: bchgcl does not handle floating alignment."
#endif

Pointer *
GCLoop(Scan, To_ptr, To_Address_ptr)
     fast Pointer *Scan;
     Pointer **To_ptr, **To_Address_ptr;
{
  fast Pointer *To, *Old, Temp, *Low_Constant, *To_Address, New_Address;

  To = *To_ptr;
  To_Address = *To_Address_ptr;
  Low_Constant = Constant_Space;

  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;
    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
        if (Scan != (Get_Pointer(Temp)))
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
	Scan += Get_Integer(Temp);
	if (Scan < scan_buffer_top)
	  break;
	else
	{
	  unsigned long overflow;

	  /* The + & -1 are here because of the Scan++ in the for header. */
	  overflow = (Scan - scan_buffer_top) + 1;
	  Scan = ((dump_and_reload_scan_buffer((overflow / GC_DISK_BUFFER_SIZE), NULL) +
		   (overflow % GC_DISK_BUFFER_SIZE)) - 1);
	  break;
	}

      case_Non_Pointer:
	break;

      case_compiled_entry_point:
	relocate_compiled_entry(true);

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
	  fast machine_word *word_ptr, *next_ptr;
	  long overflow;

	  count = READ_OPERATOR_LINKAGE_COUNT(Temp);
	  word_ptr = FIRST_OPERATOR_LINKAGE_ENTRY(Scan);
	  overflow = ((END_OPERATOR_LINKAGE_AREA(Scan, count)) -
		      scan_buffer_top);

	  for (next_ptr = NEXT_LINKAGE_OPERATOR_ENTRY(word_ptr);
	       (--count >= 0);
	       word_ptr = next_ptr,
	       next_ptr = NEXT_LINKAGE_OPERATOR_ENTRY(word_ptr))
	  {
	    if (next_ptr > ((machine_word *) scan_buffer_top))
	    {
	      extend_scan_buffer((char *) next_ptr, To);
	      ONCE_ONLY(relocate_linked_operator(true));
	      next_ptr = ((machine_word *)
			  end_scan_buffer_extension((char *) next_ptr));
	      overflow -= GC_DISK_BUFFER_SIZE;
	    }
	    else
	    {
	      relocate_linked_operator(true);
	    }
	  }
	  Scan = scan_buffer_top + overflow;
	  break;
	}
      }

      case TC_MANIFEST_CLOSURE:
      {
	machine_word *start_ptr;
	fast machine_word *word_ptr, *next_ptr;

	Scan += 1;
	start_ptr = FIRST_MANIFEST_CLOSURE_ENTRY(Scan);
	
	for (word_ptr = start_ptr,
	     next_ptr = NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr);
	     true;
	     word_ptr = next_ptr,
	     next_ptr = NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr))
	{
	  if (!MANIFEST_CLOSURE_VALID_FITS_P(word_ptr, scan_buffer_top))
	  {
	    long dw, ds;

	    dw = (word_ptr - ((machine_word *) scan_buffer_top));
	    ds = (word_ptr - start_ptr);
	    word_ptr = (((machine_word *)
			 (dump_and_reload_scan_buffer(0, NULL))) +
			dw);
	    start_ptr = word_ptr - ds;
	    next_ptr = NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr);
	  }
	  if (!VALID_MANIFEST_CLOSURE_ENTRY(word_ptr))
	  {
	    break;
	  }
	  else if (next_ptr > ((machine_word *) scan_buffer_top))
	  {
	    long ds;

	    ds = (next_ptr - start_ptr);
	    extend_scan_buffer((char *) next_ptr, To);
	    ONCE_ONLY(relocate_manifest_closure(true));
	    next_ptr = ((machine_word *)
			end_scan_buffer_extension((char *) next_ptr));
	    start_ptr = next_ptr - ds;
	  }
	  else
	  {
	    relocate_manifest_closure(true);
	  }
	}
	Scan = MANIFEST_CLOSURE_END(word_ptr, start_ptr);
	break;
      }

      case_Cell:
	relocate_normal_pointer(copy_cell(), 1);

      case TC_REFERENCE_TRAP:
	if (OBJECT_DATUM(Temp) <= TRAP_MAX_IMMEDIATE)
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
	sprintf(gc_death_message_buffer,
		"gcloop: bad type code (0x%02x)",
		OBJECT_TYPE(Temp));
	gc_death(TERM_INVALID_TYPE_CODE, gc_death_message_buffer,
		 Scan, To);
	/*NOTREACHED*/
      }
  }
end_gcloop:
  *To_ptr = To;
  *To_Address_ptr = To_Address;
  return (Scan);
}
