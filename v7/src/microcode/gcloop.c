/* -*-C-*-

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/gcloop.c,v 9.31 1989/09/20 23:08:50 cph Exp $
 *
 * This file contains the code for the most primitive part
 * of garbage collection.
 *
 */

#include "scheme.h"
#include "gccode.h"

/* Exports */

extern SCHEME_OBJECT *GCLoop();

#define GC_Pointer(Code)						\
{									\
  Old = OBJECT_ADDRESS (Temp);						\
  Code;									\
}

#define Setup_Pointer_for_GC(Extra_Code)				\
{									\
  GC_Pointer(Setup_Pointer(true, Extra_Code));				\
}

#ifdef ENABLE_DEBUGGING_TOOLS

SCHEME_OBJECT
  *gc_scan_trap = NULL,
  *gc_free_trap = NULL,
  gc_trap = MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_MAX_IMMEDIATE);

#define HANDLE_GC_TRAP()						\
{									\
  if ((Temp == gc_trap) ||						\
      (Scan == gc_scan_trap) ||						\
      (To == gc_free_trap))						\
  {									\
    fprintf(stderr, "\nGCLoop: trap.\n");				\
  }									\
}

#else

#define HANDLE_GC_TRAP()

#endif

SCHEME_OBJECT *
GCLoop(Scan, To_Pointer)
     fast SCHEME_OBJECT *Scan;
     SCHEME_OBJECT **To_Pointer;
{
  fast SCHEME_OBJECT *To, *Old, Temp, *Low_Constant, New_Address;

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
	    Setup_Pointer_for_GC(Transport_Quadruple());
	  }
	  Scan -= 1;
	  break;
	}
	else
	{
	  fast long count;
	  fast machine_word *word_ptr;
	  SCHEME_OBJECT *end_scan;

	  count = READ_OPERATOR_LINKAGE_COUNT(Temp);
	  word_ptr = FIRST_OPERATOR_LINKAGE_ENTRY(Scan);
	  end_scan = END_OPERATOR_LINKAGE_AREA(Scan, count);

	  while(--count >= 0)
	  {
	    Scan = OPERATOR_LINKAGE_ENTRY_ADDRESS(word_ptr);
	    word_ptr = NEXT_LINKAGE_OPERATOR_ENTRY(word_ptr);
	    Temp = *Scan;
	    GC_Pointer(Setup_Internal(true,
				      Transport_Compiled(),
				      Compiled_BH(true, continue)));
	  }
	  Scan = end_scan;
	  break;
	}
      }

      case TC_MANIFEST_CLOSURE:
      {
	machine_word *start_ptr;
	fast machine_word *word_ptr;

	Scan += 1;
	word_ptr = FIRST_MANIFEST_CLOSURE_ENTRY(Scan);
	start_ptr = word_ptr;

	while (VALID_MANIFEST_CLOSURE_ENTRY(word_ptr))
	{
	  Scan = MANIFEST_CLOSURE_ENTRY_ADDRESS(word_ptr);
	  word_ptr = NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr);
	  Temp = *Scan;
	  GC_Pointer(Setup_Internal(true,
				    Transport_Compiled(),
				    Compiled_BH(true, continue)));
	}
	Scan = MANIFEST_CLOSURE_END(word_ptr, start_ptr);
	break;
      }

      case_compiled_entry_point:
	GC_Pointer(Setup_Internal(true,
				  Transport_Compiled(),
				  Compiled_BH(true, continue)));
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
