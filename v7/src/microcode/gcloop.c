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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/gcloop.c,v 9.23 1987/02/08 23:09:10 jinx Exp $
 *
 * This file contains the code for the most primitive part
 * of garbage collection.
 *
 */

#include "scheme.h"
#include "gccode.h"

/* Exports */

extern Pointer *GCLoop();

#define GC_Pointer(Code)					\
Old = Get_Pointer(Temp);					\
Code

#define Setup_Pointer_for_GC(Extra_Code)			\
GC_Pointer(Setup_Pointer(true, Extra_Code))

Pointer
*GCLoop(Scan, To_Pointer)
fast Pointer *Scan;
Pointer **To_Pointer;
{ fast Pointer *To, *Old, Temp, *Low_Constant, New_Address;

  To = *To_Pointer;
  Low_Constant = Constant_Space;
  if (GC_Debug)
  { fprintf(stderr, "Starting scan at 0x%08x\n", Scan);
    if (Low_Watch == ((Pointer *) NULL))
    { fprintf(stderr, "Enter low watch range and high watch range: ");
      scanf("%x %x", &Low_Watch, &High_Watch);
    }
  }

  for ( ; Scan != To; Scan++)
  { Temp = *Scan;

    if (GC_Debug)
    { In_Range = (((Scan >= Low_Watch) && (Scan <= High_Watch)) ||
		  ((Free >= Low_Watch) && (Free <= High_Watch)));
      if (In_Range)
	fprintf(stderr,  "0x%08x: %02x|%06x ... ",
	       Scan, Type_Code(Temp), Get_Integer(Temp));
    }

/* GCLoop continues on the next page */

/* GCLoop, continued */

    Switch_by_GC_Type(Temp)
    { case TC_BROKEN_HEART:
        if (Scan == (Get_Pointer(Temp)))
	{ *To_Pointer = To;
	  return Scan;
	}
        fprintf(stderr, "GC: Broken heart in scan.\n");
	Microcode_Termination(TERM_BROKEN_HEART);

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += Get_Integer(Temp);
	if (GC_Debug && In_Range)
	  fprintf(stderr,  "skipping %d cells.", Get_Integer(Temp));
	break;

      case_Non_Pointer:
	if (GC_Debug && In_Range) fprintf(stderr, "not a pointer.");
	break;

      case_compiled_entry_point:
	GC_Pointer(Setup_Internal(true,
				  Transport_Compiled(),
				  Compiled_BH(true, continue)));

      case_Cell:
	Setup_Pointer_for_GC(Transport_Cell());

      case_Pair:
	Setup_Pointer_for_GC(Transport_Pair());

      case_Triple:
	Setup_Pointer_for_GC(Transport_Triple());

      case TC_VARIABLE:
	Setup_Pointer_for_GC(Transport_Variable());	

/* GCLoop continues on the next page */

/* GCLoop, continued */

#ifdef QUADRUPLE
      case_Quadruple:
	Setup_Pointer_for_GC(Transport_Quadruple());
#endif

#ifdef FLOATING_ALIGNMENT
      case TC_BIG_FLONUM:
	Setup_Pointer_for_GC(Transport_Flonum());
#else
      case TC_BIG_FLONUM:
	/* Fall through */
#endif
      case_Vector:
	Setup_Pointer_for_GC(Transport_Vector());

      case TC_FUTURE:
	Setup_Pointer_for_GC(Transport_Future());

      case TC_WEAK_CONS:
	Setup_Pointer_for_GC(Transport_Weak_Cons());

      default:
	fprintf(stderr,
		"GCLoop: Bad type code = 0x%02x\n",
		Type_Code(Temp));
	Invalid_Type_Code();

      }	/* Switch_by_GC_Type */
    if (GC_Debug && In_Range) fprintf(stderr, "\n");
  } /* For loop */
  *To_Pointer = To;
  return To;
} /* GCLoop */
