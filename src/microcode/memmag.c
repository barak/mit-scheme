/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

/* Memory management top level  */

#include "scheme.h"
#include "prims.h"
#include "history.h"
#include "gccode.h"
#include "osscheme.h"
#include "ostop.h"

#ifdef __WIN32__
   extern void win32_allocate_registers (void);
   extern void win32_deallocate_registers (void);
#  define ALLOCATE_REGISTERS win32_allocate_registers
#  define DEALLOCATE_REGISTERS win32_deallocate_registers

#  include "ntscmlib.h"

   extern BOOL win32_under_win32s_p (void);

   extern char * NT_allocate_heap (unsigned long, unsigned long *);
   extern void NT_release_heap (char *, unsigned long);
#  define WIN32_ALLOCATE_HEAP NT_allocate_heap
#  define WIN32_RELEASE_HEAP NT_release_heap

   static unsigned long scheme_heap_handle;
#endif

#ifndef HEAP_FREE
#  define HEAP_FREE free
#endif

#ifndef ALLOCATE_REGISTERS
#  define ALLOCATE_REGISTERS() do { } while (0)
#endif

#ifndef DEALLOCATE_REGISTERS
#  define DEALLOCATE_REGISTERS() do { } while (0)
#endif

#ifndef DEFAULT_HEAP_RESERVED
#  define DEFAULT_HEAP_RESERVED 4500
#endif

static unsigned long saved_heap_size;
static unsigned long saved_constant_size;
static unsigned long saved_stack_size;

static gc_tospace_allocator_t allocate_tospace;
static gc_abort_handler_t abort_gc NORETURN;
static gc_walk_proc_t save_tospace_copy;

static unsigned long compute_ephemeron_array_length (unsigned long);

/* Memory Allocation, sequential processor:

oo
   ------------------------------------------ <- fixed boundary (currently)
   |           Heap 2			    |
   |                                        |
   ------------------------------------------ <- boundary moved by purify
   |           Heap 1			    |
   |                                        |
   ------------------------------------------ <- boundary moved by purify
   |     Constant + Pure Space    /\        |
   |                              ||        |
   ------------------------------------------ <- fixed boundary (currently)
   |         Control Stack        ||        |
   |                              \/        |
   ------------------------------------------ <- fixed boundary (currently)
0

   Each area has a pointer to its starting address and a pointer to
   the next free cell (for the stack, it is a pointer to the last cell
   in use).  In addition, there is a pointer to the top of the
   useable area of the heap (the heap is subdivided into two areas for
   the purposes of GC, and this pointer indicates the top of the half
   currently in use).  */

void
setup_memory (unsigned long heap_size,
	      unsigned long stack_size,
	      unsigned long constant_size)
{
  ALLOCATE_REGISTERS ();

  /* Consistency check 1 */
  if ((heap_size == 0) || (stack_size == 0) || (constant_size == 0))
    {
      outf_fatal ("Configuration won't hold initial data.\n");
      outf_flush_fatal ();
      exit (1);
    }

  /* Allocate */
  ALLOCATE_HEAP_SPACE ((stack_size + heap_size + constant_size),
		       memory_block_start,
		       memory_block_end);

  /* Consistency check 2 */
  if (memory_block_start == 0)
    {
      outf_fatal ("Not enough memory for this configuration.\n");
      outf_flush_fatal ();
      exit (1);
    }

  /* Consistency check 3 */
  if ((ADDRESS_TO_DATUM (memory_block_end)) > DATUM_MASK)
    {
      outf_fatal ("Requested allocation is too large.\n");
      outf_fatal ("Try again with a smaller argument to '--heap'.\n");
      outf_flush_fatal ();
      reset_memory ();
      exit (1);
    }

  saved_stack_size = stack_size;
  saved_constant_size = constant_size;
  saved_heap_size = heap_size;
  reset_allocator_parameters (0, 0);
  initialize_gc (heap_size, (&heap_start), (&Free), allocate_tospace, abort_gc);
}

void
reset_memory (void)
{
  HEAP_FREE (memory_block_start);
  DEALLOCATE_REGISTERS ();
}

bool
allocations_ok_p (unsigned long n_constant,
		  unsigned long n_heap,
		  unsigned long n_reserved)
{
  return
    ((memory_block_start
      + saved_stack_size
      + n_constant + CONSTANT_SPACE_FUDGE
      + n_heap + ((n_reserved == 0) ? DEFAULT_HEAP_RESERVED : n_reserved))
     < memory_block_end);
}

void
reset_allocator_parameters (unsigned long n_constant, unsigned long reserved)
{
  heap_reserved = ((reserved == 0) ? DEFAULT_HEAP_RESERVED : reserved);
  gc_space_needed = 0;
  SET_STACK_LIMITS (memory_block_start, saved_stack_size);
  constant_start = (memory_block_start + saved_stack_size);
  constant_alloc_next = constant_start;
  constant_end = (constant_alloc_next + n_constant + CONSTANT_SPACE_FUDGE);
  heap_start = constant_end;
  Free = heap_start;
  heap_end = memory_block_end;
  
  RESET_HEAP_ALLOC_LIMIT ();
  INITIALIZE_STACK ();
  STACK_RESET ();
}

static void
allocate_tospace (unsigned long n_words,
		  SCHEME_OBJECT ** start_r, SCHEME_OBJECT ** end_r)
{
  if (n_words > 0)
    {
      SCHEME_OBJECT * p
	= (((*start_r) == 0)
	   ? (malloc (n_words * SIZEOF_SCHEME_OBJECT))
	   : (realloc ((*start_r), (n_words * SIZEOF_SCHEME_OBJECT))));
      if (p == 0)
	{
	  outf_fatal ("Unable to allocate temporary heap for GC.\n");
	  outf_flush_fatal ();
	  exit (1);
	}
      (*start_r) = p;
      (*end_r) = (p + n_words);
    }
  else if ((*start_r) != 0)
    {
      free (*start_r);
      (*start_r) = 0;
      (*end_r) = 0;
    }
}

static void
abort_gc (void)
{
  Microcode_Termination (TERM_EXIT);
}

bool
object_in_heap_p (SCHEME_OBJECT object)
{
  SCHEME_OBJECT * address = (get_object_address (object));
  return ((address != 0) && (ADDRESS_IN_HEAP_P (address)));
}

DEFINE_PRIMITIVE ("GARBAGE-COLLECT", Prim_garbage_collect, 1, 1,
		  "(SAFETY-MARGIN)\n\
Performs a garbage collection and returns the number of words\n\
available for further allocation.  Also sets the \"safety margin\",\n\
which is the number of reserved words at the top of the heap, to\n\
SAFETY-MARGIN, which must be a non-negative integer.  Finally, runs\n\
the primitive GC daemons before returning.")
{
  PRIMITIVE_HEADER (1);
  canonicalize_primitive_context ();

  STACK_CHECK_FATAL ("GC");
  if (Free > heap_end)
    {
      outf_fatal ("\nGC has been delayed too long!\n");
      outf_fatal
	("Free = %#lx; heap_alloc_limit = %#lx; heap_end = %#lx\n",
	 ((unsigned long) Free),
	 ((unsigned long) heap_alloc_limit),
	 ((unsigned long) heap_end));
      Microcode_Termination (TERM_NO_SPACE);
    }

  if ((ARG_HEAP_RESERVED (1)) < (heap_end - heap_start))
    {
      heap_reserved = (ARG_HEAP_RESERVED (1));
      heap_alloc_limit = (heap_end - heap_reserved);
    }
  POP_PRIMITIVE_FRAME (1);

  ENTER_CRITICAL_SECTION ("garbage collector");

  open_tospace (heap_start);
  initialize_weak_chain ();
  ephemeron_count = 0;

  std_gc_pt1 ();
  std_gc_pt2 ();

  Will_Push (CONTINUATION_SIZE);
  SET_RC (RC_NORMAL_GC_DONE);
  SET_EXP (ULONG_TO_FIXNUM ((HEAP_AVAILABLE > gc_space_needed)
			    ? (HEAP_AVAILABLE - gc_space_needed)
			    : 0));
  SAVE_CONT ();
  Pushed ();

  RENAME_CRITICAL_SECTION ("garbage collector daemon");
  {
    SCHEME_OBJECT daemon = (VECTOR_REF (fixed_objects, GC_DAEMON));
    if (daemon == SHARP_F)
      PRIMITIVE_ABORT (PRIM_POP_RETURN);

    Will_Push (2);
    STACK_PUSH (daemon);
    PUSH_APPLY_FRAME_HEADER (0);
    Pushed ();
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static SCHEME_OBJECT * saved_to;

void
std_gc_pt1 (void)
{
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  initialize_gc_object_references ();
#endif

  saved_to = (get_newspace_ptr ());
  add_to_tospace (fixed_objects);
  add_to_tospace
    (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history_register));

  current_gc_table = (std_gc_table ());
  gc_scan_oldspace (stack_pointer, stack_end);
  gc_scan_oldspace (constant_start, constant_alloc_next);
  gc_scan_tospace (saved_to, 0);

#ifdef ENABLE_GC_DEBUGGING_TOOLS
  finalize_gc_object_references ();
#endif
  update_weak_pointers ();
}

void
std_gc_pt2 (void)
{
  SCHEME_OBJECT * p = (get_newspace_ptr ());
  OS_free_pages (heap_start, heap_end);
  (void) save_tospace (save_tospace_copy, 0);
  Free = p;

  fixed_objects = (*saved_to++);
  history_register = (OBJECT_ADDRESS (*saved_to++));
  saved_to = 0;

  {
    unsigned long length
      = (compute_ephemeron_array_length
	 (ephemeron_count + n_ephemerons_requested));
    if (!HEAP_AVAILABLE_P
	((VECTOR_DATA + length) + (n_ephemerons_requested * EPHEMERON_SIZE)))
      {
	if (ephemeron_request_hard_p)
	  gc_space_needed += (VECTOR_DATA + length);
	length = (compute_ephemeron_array_length (ephemeron_count));
#ifdef ENABLE_GC_DEBUGGING_TOOLS
	/* This should never trigger, because we discard the previous
	   ephemeron array, which always has room for at least as many
	   ephemerons as are now live.  */
	if (!HEAP_AVAILABLE_P (VECTOR_DATA + length))
	  std_gc_death ("No room for ephemeron array");
#endif
      }
    ephemeron_array = (make_vector (length, SHARP_F, false));
    n_ephemerons_requested = 0;
    ephemeron_request_hard_p = false;
  }

  CC_TRANSPORT_END ();
  CLEAR_INTERRUPT (INT_GC);
}

static bool
save_tospace_copy (SCHEME_OBJECT * start, SCHEME_OBJECT * end, void * p)
{
  (void) memmove ((tospace_to_newspace (start)),
		  start,
		  ((end - start) * SIZEOF_SCHEME_OBJECT));
  return (true);
}

void
stack_death (const char * name)
{
  outf_fatal
    ("\n%s: The stack has overflowed and overwritten adjacent memory.\n",
     name);
  outf_fatal ("This was probably caused by a runaway recursion.\n");
  Microcode_Termination (TERM_STACK_OVERFLOW);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("GC-TRACE-REFERENCES", Prim_gc_trace_references, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT collector = (ARG_REF (2));
    if (! ((collector == SHARP_F)
	   || ((VECTOR_P (collector))
	       && ((VECTOR_LENGTH (collector)) >= 1))))
      error_wrong_type_arg (2);
#ifdef ENABLE_GC_DEBUGGING_TOOLS
    collect_gc_object_references ((ARG_REF (1)), collector);
#else
    error_external_return ();
#endif
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static unsigned long primes [] =
  {
    /* A list of primes that approximately doubles, up to near 2^32.
       If you have that many ephemerons, collisions in the ephemeron
       hash table are the least of your worries.  */
    11, 23, 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157,
    98317, 196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917,
    25165843, 50331653, 100663319, 201326611, 402653189, 805306457,
    1610612741,
  };

static unsigned long
compute_ephemeron_array_length (unsigned long n)
{
  unsigned int start = 0, end = ((sizeof primes) / (sizeof (*primes)));
  unsigned int index;

  if ((primes [end - 1]) < n)
    return (primes [end - 1]);

  do {
    index = (start + ((end - start) / 2));
    if ((primes [index]) < n)
      start = (index + 1);
    else if (n < (primes [index]))
      end = index;
    else
      return (primes [index]);
  } while (start < end);

  return (primes [start]);
}

static bool
ephemeron_array_big_enough_p (unsigned long n)
{
  return
    ((n == 0)
     || ((VECTOR_P (ephemeron_array))
	 && (n <= (VECTOR_LENGTH (ephemeron_array)))));
}

unsigned long
compute_extra_ephemeron_space (unsigned long n)
{
  if (ephemeron_array_big_enough_p (n))
    return (0);
  else
    return (VECTOR_DATA + (compute_ephemeron_array_length (n)));
}

void
guarantee_extra_ephemeron_space (unsigned long n)
{
  ephemeron_count = n;
  if (!ephemeron_array_big_enough_p (n))
    {
      unsigned long length = (compute_ephemeron_array_length (n));
      assert (HEAP_AVAILABLE_P (VECTOR_DATA + length));
      ephemeron_array = (make_vector (length, SHARP_F, false));
    }
}

static void
gc_if_needed_for_ephemeron (unsigned long extra_space)
{
  if (GC_NEEDED_P (EPHEMERON_SIZE + extra_space))
    {
      n_ephemerons_requested = 1;
      ephemeron_request_hard_p = true;
      Primitive_GC (EPHEMERON_SIZE);
    }
}

DEFINE_PRIMITIVE ("MAKE-EPHEMERON", Prim_make_ephemeron, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  ephemeron_count += 1;
  if (ephemeron_array_big_enough_p (ephemeron_count))
    gc_if_needed_for_ephemeron (0);
  else
    {
      unsigned long length
	= (compute_ephemeron_array_length (ephemeron_count));
      gc_if_needed_for_ephemeron (VECTOR_DATA + length);
      ephemeron_array = (make_vector (length, SHARP_F, false));
    }
  {
    SCHEME_OBJECT * addr = Free;
    (*Free++) = MARKED_EPHEMERON_MANIFEST;
    (*Free++) = (ARG_REF (1));	/* key */
    (*Free++) = (ARG_REF (2));	/* datum */
    (*Free++) = SHARP_F;	/* list */
    (*Free++) = SHARP_F;	/* queue */
    assert ((Free - addr) == EPHEMERON_SIZE);
    PRIMITIVE_RETURN (MAKE_POINTER_OBJECT (TC_EPHEMERON, addr));
  }
}
