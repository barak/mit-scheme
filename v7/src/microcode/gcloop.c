/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/gcloop.c,v 9.39 1992/02/18 17:30:10 jinx Exp $

Copyright (c) 1987-1992 Massachusetts Institute of Technology

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
 *
 * This file contains the code for the most primitive part
 * of garbage collection.
 *
 */

#include "scheme.h"
#include "gccode.h"

/* Exports */

extern SCHEME_OBJECT * EXFUN (GCLoop, (SCHEME_OBJECT *, SCHEME_OBJECT **));

#define GC_Pointer(Code)						\
{									\
  Old = OBJECT_ADDRESS (Temp);						\
  Code;									\
}

#define Setup_Pointer_for_GC(Extra_Code)				\
{									\
  GC_Pointer(Setup_Pointer(true, Extra_Code));				\
}

#ifdef ENABLE_GC_DEBUGGING_TOOLS

#ifndef GC_SCAN_HISTORY_SIZE
#define GC_SCAN_HISTORY_SIZE 1024
#endif

SCHEME_OBJECT
  * gc_scan_trap = ((SCHEME_OBJECT *) 0),
  * gc_free_trap = ((SCHEME_OBJECT *) 0),
  gc_trap = (MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_MAX_IMMEDIATE)),
  * (gc_scan_history [GC_SCAN_HISTORY_SIZE]),
  * (gc_to_history [GC_SCAN_HISTORY_SIZE]);

static int gc_scan_history_index;

#define INITIALIZE_GC_HISTORY()						\
{									\
  gc_scan_history_index = 0;						\
  {									\
    SCHEME_OBJECT ** scan = gc_scan_history;				\
    SCHEME_OBJECT ** end = (scan + GC_SCAN_HISTORY_SIZE);		\
    while (scan < end)							\
      (*scan++) = ((SCHEME_OBJECT *) 0);				\
  }									\
  {									\
    SCHEME_OBJECT ** scan = gc_to_history;				\
    SCHEME_OBJECT ** end = (scan + GC_SCAN_HISTORY_SIZE);		\
    while (scan < end)							\
      (*scan++) = ((SCHEME_OBJECT *) 0);				\
  }									\
}

#define HANDLE_GC_TRAP()						\
{									\
  (gc_scan_history [gc_scan_history_index]) = Scan;			\
  (gc_to_history [gc_scan_history_index]) = To;				\
  if ((++gc_scan_history_index) == GC_SCAN_HISTORY_SIZE)		\
    gc_scan_history_index = 0;						\
  if ((Temp == gc_trap)							\
      || ((gc_scan_trap != 0) && (Scan >= gc_scan_trap))		\
      || ((gc_free_trap != 0) && (To >= gc_free_trap)))			\
    {									\
      fprintf(stderr, "\nGCLoop: trap.\n");				\
      abort ();								\
    }									\
}

#else

#define INITIALIZE_GC_HISTORY()
#define HANDLE_GC_TRAP()

#endif

SCHEME_OBJECT *
DEFUN (GCLoop,
       (Scan, To_Pointer),
       fast SCHEME_OBJECT * Scan
       AND SCHEME_OBJECT ** To_Pointer)
{
  fast SCHEME_OBJECT *To, *Old, Temp, *Low_Constant, New_Address;

  INITIALIZE_GC_HISTORY ();
  To = *To_Pointer;
  Low_Constant = Constant_Space;
  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;
    HANDLE_GC_TRAP();

    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
        if (Scan == (OBJECT_ADDRESS (Temp)))
	{
	  *To_Pointer = To;
	  return (Scan);
	}
	sprintf(gc_death_message_buffer,
		"gcloop: broken heart (0x%lx) in scan",
		Temp);
	gc_death(TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	/*NOTREACHED*/

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += OBJECT_DATUM (Temp);
	break;

      /* Compiled code relocation. */

      case TC_LINKAGE_SECTION:
      {
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
	    for (count = READ_CACHE_LINKAGE_COUNT(Temp);
		 --count >= 0;
		 Scan += 1)
	    {
	      Temp = *Scan;
	      Setup_Pointer_for_GC(Transport_Quadruple());
	    }
	    Scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    fast long count;
	    fast char *word_ptr;
	    SCHEME_OBJECT *end_scan;

	    START_OPERATOR_RELOCATION (Scan);
	    count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	    end_scan = (END_OPERATOR_LINKAGE_AREA (Scan, count));

	    while(--count >= 0)
	    {
	      Scan = ((SCHEME_OBJECT *) word_ptr);
	      word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	      EXTRACT_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	      GC_Pointer(Setup_Internal(true,
					Transport_Compiled(),
					Compiled_BH(true,
						    goto next_operator)));
	      next_operator:
	      STORE_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	    }
	    Scan = end_scan;
	    END_OPERATOR_RELOCATION (Scan);
	    break;
	  }

	  default:
	  {
	    gc_death (TERM_EXIT,
		      "GC: Unknown compiler linkage kind.",
		      Scan, Free);
	    /*NOTREACHED*/
	  }
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char *word_ptr;
	SCHEME_OBJECT *area_end;

	START_CLOSURE_RELOCATION (Scan);
	Scan += 1;
	count = (MANIFEST_CLOSURE_COUNT (Scan));
	word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	area_end = (MANIFEST_CLOSURE_END (Scan, count));

	while ((--count) >= 0)
	{
	  Scan = ((SCHEME_OBJECT *) (word_ptr));
	  word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	  EXTRACT_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	  GC_Pointer(Setup_Internal(true,
				    Transport_Compiled(),
				    Compiled_BH(true, goto next_closure)));
	next_closure:
	  STORE_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	}

	Scan = area_end;
	END_CLOSURE_RELOCATION (Scan);
	break;
      }

      case_compiled_entry_point:
	GC_Pointer(Setup_Internal(true,
				  Transport_Compiled(),
				  Compiled_BH(true, goto after_entry)));
      after_entry:
	*Scan = Temp;
	break;

      case_Cell:
	Setup_Pointer_for_GC(Transport_Cell());
	break;

      case TC_REFERENCE_TRAP:
	if (OBJECT_DATUM (Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* Fall Through. */

      case_Pair:
	Setup_Pointer_for_GC(Transport_Pair());
	break;

      case TC_VARIABLE:
      case_Triple:
	Setup_Pointer_for_GC(Transport_Triple());
	break;

      case_Quadruple:
	Setup_Pointer_for_GC(Transport_Quadruple());
	break;

      case TC_BIG_FLONUM:
	Setup_Pointer_for_GC({
	  Transport_Flonum();
	  break;
	});

      case_Vector:
	Setup_Pointer_for_GC(Transport_Vector());
	break;

      case TC_FUTURE:
	Setup_Pointer_for_GC(Transport_Future());
	break;

      case TC_WEAK_CONS:
	Setup_Pointer_for_GC(Transport_Weak_Cons());
	break;

      default:
	GC_BAD_TYPE("gcloop");
	/* Fall Through */

      case_Non_Pointer:
	break;

      }	/* Switch_by_GC_Type */
  } /* For loop */

  *To_Pointer = To;
  return (To);

} /* GCLoop */
