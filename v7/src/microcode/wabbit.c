/* -*-C-*-

$Id: wabbit.c,v 1.10 2002/11/20 19:46:16 cph Exp $

Copyright (c) 1994-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* 
 *
 * What's opera, doc?!
 * This file contains the wabbit-hunting garbage collector,
 * by Ziggy and GJR.
 *
 */

#include "scheme.h"
#include "gccode.h"

extern SCHEME_OBJECT Weak_Chain;

extern SCHEME_OBJECT *
  EXFUN (wabbit_hunting_gcloop, (SCHEME_OBJECT *, SCHEME_OBJECT **));

extern void
  EXFUN (wabbit_season, (SCHEME_OBJECT));

extern void
  EXFUN (duck_season, (SCHEME_OBJECT));

extern void
  EXFUN (fix_weak_chain_and_hunt_wabbits, (void));

/* Wabbit hunting code */
/* Be wary, wary, quiet... */

#define TC_HEADLESS_REFERENCE		TC_NULL
#define TC_REFERENCE_TO_STACK		TC_STACK_ENVIRONMENT
#define TC_REFERENCE_TO_CONSTANT_SPACE	TC_CHARACTER

Boolean
  wabbit_holes_discarded_p,
  wabbit_holes_overwritten_p,
  wabbit_all_dead_p;  

SCHEME_OBJECT
  * wabbit_holes,
  * wabbit_holes_hi,
  * wabbit_lo_address,
  * wabbit_hi_address,
  * wabbit_of_Seville,
  * wabbit_buffer_lo,
  * wabbit_buffer_ptr,
  * wabbit_buffer_hi,
  * old_wabbit_buffer,
  * old_wabbit_buffer_end,
  * hares_lo,
  * hares_hi;

#define ELMER_FUDGE_FACTOR	 4	/* Size of QUAD */
#define ELMER_HUNG_FACTOR	20	/* 1 / (Sales tax in MA in 1994) */
#define RAJIV_SURATI_FACTOR     -20     /* -1 * ELMER_HUNG_FACTOR */

void EXFUN (kill_da_wabbit, (SCHEME_OBJECT *, SCHEME_OBJECT));
Boolean EXFUN (discard_wabbit_holes_p, (SCHEME_OBJECT *, SCHEME_OBJECT *));

/* We need not check wabbit_lo_address by construction:
   wabbit_lo_address is Free at the beginning of the GC, and
   all forwarded objects will point above that, except for
   the wabbit of Seville, a.k.a. the wabbit vector.
 */

#define WABBIT_P(addr)							\
  (((addr) < wabbit_hi_address)						\
   && ((addr) != wabbit_of_Seville))

#define HARE_P(addr)							\
  (((OBJECT_TYPE (* addr)) == TC_BROKEN_HEART)				\
   && ((OBJECT_ADDRESS (* addr)) >= old_wabbit_buffer)			\
   && ((OBJECT_ADDRESS (* addr)) < old_wabbit_buffer_end))

#define RECORD_WABBIT_HOLE(tag, address) do				\
{									\
  if ((wabbit_holes > (new_space_free + ELMER_FUDGE_FACTOR))		\
      || (discard_wabbit_holes_p (scan, new_space_free)))		\
    *--wabbit_holes = (MAKE_POINTER_OBJECT (tag, address));		\
} while (0)

#define KILL_DA_WABBIT(where, last_object) do				\
{									\
  if ((wabbit_buffer_ptr + 2) <= wabbit_buffer_hi)			\
    kill_da_wabbit (where, last_object);					\
  else									\
    wabbit_all_dead_p = false;						\
} while (0)

/* Oh, what have I done!  I've killed the little bunny wabbit... */

#define COPY_CELL()							\
{									\
  *new_space_free++ = *old_space_addr;					\
}

#define COPY_PAIR()							\
{									\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr;					\
}

#define COPY_TRIPLE()							\
{									\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr;					\
}

#define COPY_QUADRUPLE()						\
{									\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr;					\
}

#define COPY_VECTOR()							\
{									\
  long veclen = (1 + (OBJECT_DATUM (* old_space_addr)));		\
  SCHEME_OBJECT * vecend = (new_space_free + veclen);			\
									\
  if (vecend > wabbit_holes)						\
    discard_wabbit_holes_p (scan, new_space_free);			\
  while (new_space_free != vecend)					\
    *new_space_free++ = *old_space_addr++;				\
}

#define COPY_WEAK_PAIR()						\
{									\
  long car_tag = (OBJECT_TYPE (* old_space_addr));			\
  (*new_space_free++)							\
    = (OBJECT_NEW_TYPE (TC_NULL, (* old_space_addr)));			\
  *new_space_free++ = *++old_space_addr;				\
  * old_space_addr = (OBJECT_NEW_TYPE (car_tag, Weak_Chain));		\
  Weak_Chain = this_object;						\
}

#define RELOCATE_NORMAL_SETUP()						\
{									\
  old_space_addr = (OBJECT_ADDRESS (this_object));			\
  if (old_space_addr < low_heap)					\
  {									\
    if (HARE_P (old_space_addr))					\
      KILL_DA_WABBIT (scan, SHARP_F);					\
    continue;								\
  }									\
  if ((OBJECT_TYPE (* old_space_addr)) == TC_BROKEN_HEART)		\
  {									\
    new_space_addr = (OBJECT_ADDRESS (* old_space_addr));		\
    if (WABBIT_P (new_space_addr))					\
      KILL_DA_WABBIT (scan, SHARP_F);					\
    * scan = (MAKE_OBJECT_FROM_OBJECTS (this_object,			\
					(* old_space_addr)));		\
    continue;								\
  }									\
}

#define RELOCATE_NORMAL_END()						\
{									\
  (* (OBJECT_ADDRESS (this_object)))					\
    = (MAKE_BROKEN_HEART (new_space_addr));				\
  (* scan) = (MAKE_POINTER_OBJECT ((OBJECT_TYPE (this_object)),		\
				   new_space_addr));			\
  continue;								\
}

#define RELOCATE_NORMAL_POINTER(copy_code)				\
{									\
  RELOCATE_NORMAL_SETUP ();						\
  new_space_addr = new_space_free;					\
  copy_code;								\
  RECORD_WABBIT_HOLE ((OBJECT_TYPE (this_object)), new_space_addr);	\
  RELOCATE_NORMAL_END ();						\
}

#define RELOCATE_ALIGNED_POINTER(copy_code)				\
{									\
  RELOCATE_NORMAL_SETUP ();						\
  ALIGN_FLOAT (new_space_free);						\
  new_space_addr = new_space_free;					\
  copy_code;								\
  RECORD_WABBIT_HOLE ((OBJECT_TYPE (this_object)), new_space_addr);	\
  RELOCATE_NORMAL_END ();						\
}

#define RELOCATE_RAW_POINTER(tag, copy_code, last_object)		\
{									\
  old_space_addr = ((SCHEME_OBJECT *) this_object);			\
  if (old_space_addr < low_heap)					\
  {									\
    if (HARE_P (old_space_addr))					\
      KILL_DA_WABBIT (scan, last_object);				\
    continue;								\
  }									\
  if ((OBJECT_TYPE (* old_space_addr)) == TC_BROKEN_HEART)		\
  {									\
    new_space_addr = (OBJECT_ADDRESS (* old_space_addr));		\
    if (WABBIT_P (new_space_addr))					\
      KILL_DA_WABBIT (scan, last_object);				\
    * scan = ((SCHEME_OBJECT) new_space_addr);				\
    continue;								\
  }									\
  {									\
    SCHEME_OBJECT * saved_old_addr = old_space_addr;			\
									\
    new_space_addr = new_space_free;					\
    copy_code;								\
    RECORD_WABBIT_HOLE (tag, new_space_addr);				\
    (* saved_old_addr) = (MAKE_BROKEN_HEART (new_space_addr));		\
    (* scan) = ((SCHEME_OBJECT) new_space_addr);			\
    continue;								\
  }									\
}

#define RELOCATE_COMPILED_ENTRY(last_object)				\
{									\
  Get_Compiled_Block (old_space_addr,					\
		      ((SCHEME_OBJECT *) this_entry));			\
  if (old_space_addr < low_heap)					\
  {									\
    if (HARE_P (old_space_addr))					\
      KILL_DA_WABBIT (scan, last_object);				\
    new_entry = this_entry;						\
  }									\
  else if ((OBJECT_TYPE (* old_space_addr)) == TC_BROKEN_HEART)		\
  {									\
    new_space_addr = (OBJECT_ADDRESS (* old_space_addr));		\
    if (WABBIT_P (new_space_addr))					\
      KILL_DA_WABBIT (scan, last_object);				\
    new_entry =								\
      ((SCHEME_OBJECT)							\
       (RELOCATE_COMPILED_INTERNAL (this_entry,				\
				    new_space_addr,			\
				    old_space_addr)));			\
  }									\
  else									\
  {									\
    SCHEME_OBJECT * saved_old_addr = old_space_addr;			\
									\
    ALIGN_FLOAT (new_space_free);					\
    new_space_addr = new_space_free;					\
    new_entry =								\
      ((SCHEME_OBJECT)							\
       (RELOCATE_COMPILED_INTERNAL (this_entry,				\
				    new_space_addr,			\
				    old_space_addr)));			\
    COPY_VECTOR ();							\
    RECORD_WABBIT_HOLE (TC_COMPILED_CODE_BLOCK, new_space_addr);	\
    (* saved_old_addr) = (MAKE_BROKEN_HEART (new_space_addr));		\
  }									\
}

SCHEME_OBJECT *
DEFUN (wabbit_hunting_gcloop, (scan, new_space_free_loc),
       fast SCHEME_OBJECT * scan
       AND SCHEME_OBJECT ** new_space_free_loc)
{
  long last_nmv_length;
  fast SCHEME_OBJECT
    * new_space_free, * old_space_addr, this_object,
    * low_heap, * new_space_addr, this_entry, new_entry;
  SCHEME_OBJECT
    last_object, * last_object_end, * last_nmv, * last_hare, last_hare_head,
    magic_cookie, saved_cookie, * saved_addr;

  last_object = SHARP_F;
  last_object_end = 0;
  last_nmv = (scan - 2);	/* Make comparison fail until */
  last_nmv_length = 0;		/* an NMV is found. */
  last_hare = (scan - 2);	/* Same here */
  last_hare_head = SHARP_F;
  magic_cookie = SHARP_F;
  saved_cookie = SHARP_F;
  saved_addr = 0;
  new_space_free = * new_space_free_loc;
  low_heap = Constant_Top;
  for ( ; scan != new_space_free; scan++)
  {
    this_object = * scan;

repeat_dispatch:
    Switch_by_GC_Type (this_object)
    {
      case TC_BROKEN_HEART:
        old_space_addr = (OBJECT_ADDRESS (this_object));
        if (scan == old_space_addr)
	{
	  if (this_object == magic_cookie)
	  {
	    magic_cookie = SHARP_F;
	    last_hare = (scan - 1);
	    last_hare_head = scan[-1];
	    saved_addr[0] = scan[-1];
	    scan[-1] = (MAKE_BROKEN_HEART (saved_addr));
	    *scan = saved_cookie;
	    this_object = saved_cookie;
	    goto repeat_dispatch;
	  }
	  else
	  {
	    * new_space_free_loc = new_space_free;
	    return (scan);
	  }
	}
	else if ((old_space_addr < old_wabbit_buffer)
		 || (old_space_addr >= old_wabbit_buffer_end))
	{
	  sprintf (gc_death_message_buffer,
		   "wabbit_hunting_gcloop: broken heart (0x%lx) in scan",
		   this_object);
	  gc_death (TERM_BROKEN_HEART, gc_death_message_buffer,
		    scan, new_space_free);
	  /*NOTREACHED*/
	}
	else
        {
	  SCHEME_OBJECT old_head = old_space_addr[0];

	  switch (GC_Type_Map [(OBJECT_TYPE (old_head))])
	  {
	    default:
	    case GC_Non_Pointer:
	      last_hare = scan;
	      last_hare_head = old_head;
	      break;

	    case GC_Special:
	      if (((OBJECT_TYPE (old_head)) != TC_REFERENCE_TRAP)
		  || ((OBJECT_DATUM (old_head)) <= TRAP_MAX_IMMEDIATE))
	      {
		this_object = old_head;
		last_hare = scan;
		last_hare_head = old_head;
		goto repeat_dispatch;
	      }
	      /* fall through */

	    case GC_Cell:
	    case GC_Pair:
	    case GC_Triple:
	    case GC_Quadruple:
	    case GC_Vector:
	      if ((OBJECT_ADDRESS (old_head)) == scan)
	      {
		last_hare = scan;
		last_hare_head = old_head;
		KILL_DA_WABBIT (scan, old_head);
		break;
	      }
	      /* fall through */

	    case GC_Compiled:
	      saved_addr = old_space_addr;
	      saved_cookie = scan[1];
	      magic_cookie = (MAKE_BROKEN_HEART (scan + 1));
	      scan[1] = magic_cookie;
	      this_object = old_head;
	      *scan = old_head;
	      goto repeat_dispatch;
	  }
	}
	break;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	if ((last_nmv + (1 + last_nmv_length)) == scan)
	  last_object = SHARP_F;
	else if ((OBJECT_TYPE (scan[-1])) == TC_MANIFEST_VECTOR)
	{
	  last_object
	    = (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, (scan - 1)));
	  last_object_end = (scan + (OBJECT_DATUM (scan [-1])));
	}
	else if (((scan - 1) == last_hare)
		 && ((OBJECT_TYPE (last_hare_head)) == TC_MANIFEST_VECTOR))
	{
	  last_object
	    = (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, (scan - 1)));
	  last_object_end = (scan + (OBJECT_DATUM (last_hare_head)));
	}
	else
	  last_object = SHARP_F;

	last_nmv = scan;
	last_nmv_length = (OBJECT_DATUM (this_object));
	scan += last_nmv_length;
	break;

      /* Compiled code relocation. */

      case TC_LINKAGE_SECTION:
      {
	SCHEME_OBJECT saved_last_object, * saved_last_object_end;

	saved_last_object = last_object;
	saved_last_object_end = last_object_end;
	if ((last_object == SHARP_F) || (last_object_end < scan))
	{
	  last_object = (MAKE_POINTER_OBJECT (TC_HEADLESS_REFERENCE, scan));
	  last_object_end
	    = (scan + (1 + (READ_CACHE_LINKAGE_COUNT (this_object))));
	}

	switch (READ_LINKAGE_KIND (this_object))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    /* Assumes that all others are objects of type TC_QUAD without
	       their type codes.
	       */

	    fast long count;

	    scan++;
	    for (count = (READ_CACHE_LINKAGE_COUNT (this_object));
		 --count >= 0;
		 scan += 1)
	    {
	      this_object = (* scan);
	      RELOCATE_RAW_POINTER (TC_QUAD, COPY_QUADRUPLE (), last_object);
	    }
	    scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    fast long count;
	    fast char * word_ptr;
	    SCHEME_OBJECT * end_scan;

	    START_OPERATOR_RELOCATION (scan);
	    count = (READ_OPERATOR_LINKAGE_COUNT (this_object));
	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (scan));
	    end_scan = (END_OPERATOR_LINKAGE_AREA (scan, count));

	    while (--count >= 0)
	    {
	      scan = ((SCHEME_OBJECT *) word_ptr);
	      word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	      EXTRACT_OPERATOR_LINKAGE_ADDRESS (this_entry, scan);
	      RELOCATE_COMPILED_ENTRY (last_object);
	      STORE_OPERATOR_LINKAGE_ADDRESS (new_entry, scan);
	    }
	    scan = end_scan;
	    END_OPERATOR_RELOCATION (scan);
	    break;
	  }

	  case CLOSURE_PATTERN_LINKAGE_KIND:
	    scan += (READ_CACHE_LINKAGE_COUNT (this_object));
	    break;

	  default:
	  {
	    gc_death (TERM_EXIT,
		      "GC: Unknown compiler linkage kind.",
		      scan, Free);
	    /*NOTREACHED*/
	  }
	}
	last_object = saved_last_object;
	last_object_end = saved_last_object_end;
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char * word_ptr;
	SCHEME_OBJECT * area_end;
	SCHEME_OBJECT saved_last_object, * saved_last_object_end;

	saved_last_object = last_object;
	saved_last_object_end = last_object_end;
	if ((last_object == SHARP_F) || (last_object_end < scan))
	{
	  last_object = (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, scan));
	  last_object_end = (scan + (1 + (OBJECT_DATUM (this_object))));
	}
	START_CLOSURE_RELOCATION (scan);
	scan += 1;
	count = (MANIFEST_CLOSURE_COUNT (scan));
	word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (scan));
	area_end = ((MANIFEST_CLOSURE_END (scan, count)) - 1);

	while ((--count) >= 0)
	{
	  scan = ((SCHEME_OBJECT *) (word_ptr));
	  word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	  EXTRACT_CLOSURE_ENTRY_ADDRESS (this_entry, scan);
	  RELOCATE_COMPILED_ENTRY (last_object);
	  STORE_CLOSURE_ENTRY_ADDRESS (new_entry, scan);
	}

	scan = area_end;
	END_CLOSURE_RELOCATION (scan);
	last_object = saved_last_object;
	last_object_end = saved_last_object_end;
	break;
      }

      case_compiled_entry_point:
      {
	this_entry = ((SCHEME_OBJECT) (OBJECT_ADDRESS (this_object)));
	RELOCATE_COMPILED_ENTRY (SHARP_F);
	(* scan) = (MAKE_POINTER_OBJECT ((OBJECT_TYPE (this_object)),
					 ((SCHEME_OBJECT *) new_entry)));
	continue;
      }

      case_Cell:
	RELOCATE_NORMAL_POINTER (COPY_CELL ());
	break;

      case TC_REFERENCE_TRAP:
	if ((OBJECT_DATUM (this_object)) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* Fall Through. */

      case_Pair:
	RELOCATE_NORMAL_POINTER (COPY_PAIR ());
	break;

      case TC_VARIABLE:
      case_Triple:
	RELOCATE_NORMAL_POINTER (COPY_TRIPLE ());
	break;

      case_Quadruple:
	RELOCATE_NORMAL_POINTER (COPY_QUADRUPLE ());
	break;

      case_Aligned_Vector:
	RELOCATE_ALIGNED_POINTER (COPY_VECTOR ());
	break;

      case TC_FUTURE:
	if (Future_Spliceable (this_object))
	{
	  * scan = (Future_Value (this_object));
	  scan -= 1;
	  continue;
	}
	/* fall through */

      case_Vector:
	RELOCATE_NORMAL_POINTER (COPY_VECTOR ());
	break;

      case TC_WEAK_CONS:
	RELOCATE_NORMAL_POINTER (COPY_WEAK_PAIR ());
	break;

      default:
	sprintf (gc_death_message_buffer,
		 "wabbit_hunting_gcloop: bad type code (0x%02x)",
		 ((unsigned int) (OBJECT_TYPE (this_object))));
	gc_death (TERM_INVALID_TYPE_CODE,
		  gc_death_message_buffer,
		  scan, new_space_free);
	/*NOTREACHED*/

      case_Non_Pointer:
	break;

      }	/* Switch_by_GC_Type */
  } /* For loop */

  * new_space_free_loc = new_space_free;
  return (new_space_free);

} /* wabbit_hunting_gcloop */

void
DEFUN (wabbit_season, (wabbit_descriptor),
       SCHEME_OBJECT wabbit_descriptor)
{
  long n_wabbits, buf_len, ctr;
  SCHEME_OBJECT
    * result, * area, * saved_area,
    wabbit_buffer, wabbit_vector, * wabbit_vector_ptr;

  wabbit_vector = (VECTOR_REF (wabbit_descriptor, 1));
  wabbit_buffer = (VECTOR_REF (wabbit_descriptor, 2));
    
  buf_len = (VECTOR_LENGTH (wabbit_buffer));
  n_wabbits = (VECTOR_LENGTH (wabbit_vector));

  wabbit_all_dead_p = true;
  wabbit_holes_overwritten_p = false;
  wabbit_holes_discarded_p = false;
  wabbit_holes_hi = Heap_Top;
  wabbit_holes = wabbit_holes_hi;

  saved_area = area = Free;
  wabbit_lo_address = saved_area;
  wabbit_hi_address = saved_area;
  wabbit_of_Seville = saved_area;

  wabbit_vector_ptr = (MEMORY_LOC (wabbit_vector, 0));
  for (ctr = n_wabbits; ctr >= 0; ctr -= 1)
    *area++ = *wabbit_vector_ptr++;

  MEMORY_SET (wabbit_vector, 0, (MAKE_BROKEN_HEART (saved_area)));
  *area = (MAKE_BROKEN_HEART (area));
  Free = (area + 1);
  
  result = (wabbit_hunting_gcloop (saved_area, &Free));
  if (result != area)
  {
    outf_fatal ("\nwabbit_hunt Wabbit scan ended too early.\n");
    Microcode_Termination (TERM_BROKEN_HEART);
  }

  *area = SHARP_F;		/* Remove broken heart on Valentine's day */
  wabbit_lo_address = (area + 1);
  wabbit_hi_address = Free;

  if (BROKEN_HEART_P (MEMORY_REF (wabbit_buffer, 0)))
    /* One of the wabbits is the wabbit buffer itself! */
    wabbit_buffer_lo = (OBJECT_ADDRESS (MEMORY_REF (wabbit_buffer, 0)));
  else
  {
    wabbit_buffer_lo = Free;
    MEMORY_SET (wabbit_buffer, 0, (MAKE_BROKEN_HEART (wabbit_buffer_lo)));
    Free += (1 + buf_len);
  }
  wabbit_buffer_hi = (wabbit_buffer_lo + (1 + buf_len));
  * wabbit_buffer_lo = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, buf_len));
  wabbit_buffer_ptr = (wabbit_buffer_lo + 3);

  /* Check whether any wabbits are hares, and if so, mark them so. */

  old_wabbit_buffer = ((OBJECT_ADDRESS (wabbit_buffer)) + 3);
  old_wabbit_buffer[-1] = (MAKE_BROKEN_HEART (old_wabbit_buffer - 1));

  wabbit_vector_ptr = (MEMORY_LOC (wabbit_vector, 1));
  
  for (area = old_wabbit_buffer, ctr = n_wabbits; --ctr >= 0; )
  {
    SCHEME_OBJECT wabbit = *wabbit_vector_ptr++;
    SCHEME_OBJECT old_head;

    switch (GC_Type_Map [(OBJECT_TYPE (wabbit))])
    {
      case GC_Non_Pointer:
        /* Sucker -- should crash his scheme */
        break;

      case GC_Special:
	if (((OBJECT_TYPE (wabbit)) != TC_REFERENCE_TRAP)
	    || ((OBJECT_DATUM (wabbit)) <= TRAP_MAX_IMMEDIATE))
	  break;
	/* fall through */

      case GC_Cell:
      case GC_Pair:
      case GC_Triple:
      case GC_Quadruple:
      case GC_Vector:
	if ((OBJECT_ADDRESS (wabbit)) >= Constant_Top)
	  break;
	old_head = (MEMORY_REF (wabbit, 0));
	MEMORY_SET (wabbit, 0, (MAKE_BROKEN_HEART (area)));
	*area++ = old_head;
	*area++ = wabbit;
	break;

      case GC_Compiled:
      {
	SCHEME_OBJECT * block;

	if ((OBJECT_ADDRESS (wabbit)) >= Constant_Top)
	  break;

	Get_Compiled_Block (block, (OBJECT_ADDRESS (wabbit)));
	old_head = *block;
	*block = (MAKE_BROKEN_HEART (area));
	*area++ = old_head;
	*area++ = (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block));
	break;
      }

      default:
	/* Loser -- shouldn't happen */
	break;
    }
  }
  old_wabbit_buffer_end = area;

  result = (wabbit_hunting_gcloop (wabbit_lo_address, &Free));
  if (Free != result)
  {
    outf_fatal ("\nwabbit_hunt: heap scan ended too early.\n");
    Microcode_Termination (TERM_BROKEN_HEART);
  }
  return;
}

void
DEFUN (duck_season, (wabbit_descriptor),
       SCHEME_OBJECT wabbit_descriptor)
{
  SCHEME_OBJECT * ptr;

  /* Restore hares' heads */

  for (ptr = old_wabbit_buffer; ptr < old_wabbit_buffer_end; ptr += 2)
    MEMORY_SET (ptr[1], 0, ptr[0]);

  wabbit_buffer_lo[2] =
    (LONG_TO_UNSIGNED_FIXNUM (wabbit_buffer_ptr - (wabbit_buffer_lo + 1)));
  while (wabbit_buffer_ptr < wabbit_buffer_hi)
    *wabbit_buffer_ptr++ = SHARP_F;
  wabbit_buffer_lo[1] = (BOOLEAN_TO_OBJECT (wabbit_all_dead_p));
  wabbit_buffer_lo[0]
    = (MAKE_OBJECT (TC_MANIFEST_VECTOR,
		    (wabbit_buffer_hi - (wabbit_buffer_lo + 1))));

  if ((VECTOR_REF (wabbit_descriptor, 3)) == SHARP_T)
  {
    SCHEME_OBJECT * guaranteed_free = (Free + (GC_Reserve + 2));
    SCHEME_OBJECT * source, * dest, result;
    long len;

    if (guaranteed_free > wabbit_holes)
    {
      wabbit_holes_discarded_p = true;
      wabbit_holes = guaranteed_free;
    }
    dest = Free;
    result = (MAKE_POINTER_OBJECT (TC_VECTOR, dest));
    source = wabbit_holes;
    len = (wabbit_holes_hi - source);
    *dest++ = (MAKE_OBJECT (TC_MANIFEST_VECTOR, (len + 1)));
    *dest++ = (BOOLEAN_TO_OBJECT (! (wabbit_holes_discarded_p
				     || wabbit_holes_overwritten_p)));
    while (--len >= 0)
      *dest++ = *source++;
    Free = dest;
    VECTOR_SET (wabbit_descriptor, 3, result);
  }

  VECTOR_SET (wabbit_descriptor, 0, SHARP_T);
  return;
}

SCHEME_OBJECT *
DEFUN (hunt_wabbit, (where), SCHEME_OBJECT * where)
{
  SCHEME_OBJECT * ptr_lo, * ptr_hi, * ptr_mid, * hole;

  ptr_lo = wabbit_holes;
  ptr_hi = (wabbit_holes_hi - 1);

  while (ptr_lo < ptr_hi)
  {
    ptr_mid = (ptr_lo + ((ptr_hi - ptr_lo) / 2));
    hole = (OBJECT_ADDRESS (* ptr_mid));
    if (where < hole)
      ptr_lo = (ptr_mid + 1);
    else if (where > hole)
      ptr_hi = ptr_mid;
    else
    {
      ptr_hi = ptr_mid;
      ptr_lo = ptr_mid;
      break;
    }
  }
  return (ptr_lo);
}

Boolean
DEFUN (discard_wabbit_holes_p, (scan, free),
       SCHEME_OBJECT * scan AND SCHEME_OBJECT * free)
{
  SCHEME_OBJECT * hole, * new_hole;
  long keep_index;

  if (free > wabbit_holes)
  {
    wabbit_holes_overwritten_p = true;
    wabbit_holes = free;    
  }
  if (scan < Constant_Top)
    return (free < wabbit_holes);

  hole = ((hunt_wabbit (scan)) + 1);

  /* This guarantees that we don't get into quadratic copying:
     We discard only if the fraction of holes being discarded
     is at least 1/ELMER_HUNG_FACTOR of the total number of holes.
   */

  if ((ELMER_HUNG_FACTOR * (wabbit_holes_hi - hole))
      < (wabbit_holes_hi - wabbit_holes))
    return (free < wabbit_holes);

  keep_index = (hole - wabbit_holes);
  new_hole = wabbit_holes_hi;

  while (--keep_index >= 0)
    *--new_hole = *--hole;

  wabbit_holes = new_hole;
  wabbit_holes_discarded_p = true;
  return (free < wabbit_holes);
}

void
DEFUN (kill_da_wabbit, (where, current_object),
       SCHEME_OBJECT * where AND SCHEME_OBJECT current_object)
{
  SCHEME_OBJECT * hole, wabbit, * wabbit_addr;
  long offset, max_offset;

  /* With my sword and magic helmet... */

  if (where < Constant_Top)
  {
    SCHEME_OBJECT head;

    if (current_object != SHARP_F)
    {
      offset = (where - (OBJECT_ADDRESS (current_object)));
      head = current_object;
    }
    else
    {
      /* If we do cwcc before calling the special garbage collector,
	 there should be no references to the stack.
       */
      offset = 0;
      if (where < Stack_Top)
	head = (MAKE_POINTER_OBJECT (TC_REFERENCE_TO_STACK, where));
      else
	head = (MAKE_POINTER_OBJECT (TC_REFERENCE_TO_CONSTANT_SPACE, where));
    }

    *wabbit_buffer_ptr++ = head;
    *wabbit_buffer_ptr++ = (LONG_TO_UNSIGNED_FIXNUM (offset));
    return;
  }
  if (wabbit_holes >= wabbit_holes_hi)
    return;

  hole = (hunt_wabbit (where));
  wabbit = (* hole);
  wabbit_addr = (OBJECT_ADDRESS (wabbit));
  offset = (where - wabbit_addr);
  *wabbit_buffer_ptr++ = wabbit;
  *wabbit_buffer_ptr++ = (LONG_TO_UNSIGNED_FIXNUM (offset));

  if ((hole == wabbit_holes)
      && wabbit_holes_overwritten_p && (where != wabbit_addr))
  {
    switch (GC_Type_Map[(OBJECT_TYPE (wabbit))])
    {
      case GC_Pair:
        max_offset = 2;
	break;

      case GC_Triple:
        max_offset = 3;
	break;

      case GC_Quadruple:
        max_offset = 4;
	break;

      case GC_Vector:
	max_offset = (1 + (OBJECT_DATUM (* wabbit_addr)));
	break;
	
      case GC_Special:
        if ((OBJECT_TYPE (* hole)) == TC_REFERENCE_TRAP)
	{
	  max_offset = 2;
	  break;
	}
	/* fall through */

      case GC_Cell:	/* => (where == wabbit_addr), already tested */
      default:
	max_offset = -1;
    }
    if ((max_offset == -1) || (where > (wabbit_addr + max_offset)))
    {
      wabbit_buffer_ptr -= 2;
      wabbit_all_dead_p = false;
    }
  }
  return;
}

/* Alternate version of Fix_Weak_Chain that hunts wabbits. */

#ifndef EMPTY_WEAK_CHAIN
#define EMPTY_WEAK_CHAIN EMPTY_LIST
#endif

void
DEFUN_VOID (fix_weak_chain_and_hunt_wabbits)
{
  fast SCHEME_OBJECT
    * old_weak_pair, * scan, nulled_car, * new_space_addr,
    this_object, * old_space_addr, * low_heap;

  low_heap = Constant_Top;
  while (Weak_Chain != EMPTY_WEAK_CHAIN)
  {
    old_weak_pair = (OBJECT_ADDRESS (Weak_Chain));
    scan = (OBJECT_ADDRESS (*old_weak_pair++));
    Weak_Chain = * old_weak_pair;
    nulled_car = * scan;
    this_object = (MAKE_OBJECT_FROM_OBJECTS (Weak_Chain, nulled_car));
    Weak_Chain = (OBJECT_NEW_TYPE (TC_NULL, Weak_Chain));

    switch (GC_Type (this_object))
    {
      case GC_Non_Pointer:
        *scan = this_object;
	continue;

      case GC_Special:
	if ((OBJECT_TYPE (this_object)) != TC_REFERENCE_TRAP)
	{
	  /* No other special type makes sense here. */
	  goto fail;
	}
	if ((OBJECT_DATUM (this_object)) <= TRAP_MAX_IMMEDIATE)
	{
	  * scan = this_object;
	  continue;
	}
	/* Otherwise, it is a pointer.  Fall through */

      /* Normal pointer types, the broken heart is in the first word.
         Note that most special types are treated normally here.
       */
      case GC_Cell:
      case GC_Pair:
      case GC_Triple:
      case GC_Quadruple:
      case GC_Vector:
	* scan = this_object;	/* In case it points to constant space */
	RELOCATE_NORMAL_SETUP ();
	* scan = SHARP_F;
	continue;

      case GC_Compiled:
	* scan = this_object;
	old_space_addr = (OBJECT_ADDRESS (this_object));
	if (old_space_addr < low_heap)
	  continue;
	Get_Compiled_Block (old_space_addr, old_space_addr);
	if ((OBJECT_TYPE (* old_space_addr)) == TC_BROKEN_HEART)
	{
	  new_space_addr = (OBJECT_ADDRESS (* old_space_addr));
	  if (WABBIT_P (new_space_addr))
	    KILL_DA_WABBIT (scan, (MAKE_POINTER_OBJECT (TC_WEAK_CONS, scan)));
	    
	  * scan = (RELOCATE_COMPILED (this_object,
				       new_space_addr,
				       old_space_addr));
	  continue;
	}
	* scan = SHARP_F;
	continue;

      case GC_Undefined:
	outf_error
	  ("\nfix_weak_chain_and_hunt_wabbits: Clearing bad object 0x%08lx.\n",
	   this_object);
	* scan = SHARP_F;
	continue;

      default:			/* Non Marked Headers and Broken Hearts */
      fail:
        outf_fatal
	  ("\nfix_weak_chain_and_hunt_wabbits: Bad Object: 0x%08lx.\n",
	   this_object);
	* scan = SHARP_F;
	/*NOTREACHED*/
    }
  }
  return;
}

/* What did you expect from opera, a happy ending? */
