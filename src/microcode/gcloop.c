/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* Garbage collector core

   This is a one-space copying garbage collector.  It's like a
   two-space collector, except that the heap is copied to temporary
   memory ("tospace"), then copied back at the end.  This design is
   more complex and slower than a two-space collector, but it has the
   advantage that tospace can be allocated anywhere in the virtual
   address space.  This matters because our tagging scheme limits the
   number of address bits we can use (26 on a 32-bit machine), and
   with this design tospace can be located outside of the addressable
   range, thus maximizing the usage of addressable memory.  This
   design is similar to that of the older "bchscheme" GC, except that
   bchscheme allocated its tospace in a file.

   Some terminology:

   "Fromspace" is the allocated portion of the heap that we copy
   objects from.

   "Tospace" is the temporary memory that we copy objects to.

   "Newspace" is the region of memory into which tospace will be
   copied after the GC is complete.  During the GC we copy objects
   into tospace, but update pointers to refer to locations in
   newspace.  Since there's a simple relationship between pointers in
   newspace and pointers in tospace, it's easy to convert between
   them.

   "Oldspace" is the addressable region of memory.  This includes
   fromspace, and also the stack and constant areas.  It is
   distinguished from fromspace because we can scan anywhere in
   oldspace, but we copy only from fromspace.

*/

#include "object.h"
#include "outf.h"
#include "gccode.h"

/* For ephemeron layout.  */
#include "sdata.h"

/* For memory advice.  */
#include "ostop.h"

static SCHEME_OBJECT ** p_fromspace_start;
static SCHEME_OBJECT ** p_fromspace_end;
static gc_tospace_allocator_t * gc_tospace_allocator;
static gc_abort_handler_t * gc_abort_handler NORETURN;

static SCHEME_OBJECT * tospace_start;
static SCHEME_OBJECT * tospace_next;
static SCHEME_OBJECT * tospace_end;
static SCHEME_OBJECT * newspace_start;
static SCHEME_OBJECT * newspace_next;
static SCHEME_OBJECT * newspace_end;

gc_table_t * current_gc_table;
static SCHEME_OBJECT * current_scan;
static SCHEME_OBJECT current_object;

#define ADDRESS_IN_FROMSPACE_P(addr)					\
  ((((void *) (addr)) >= ((void *) (*p_fromspace_start)))		\
   && (((void *) (addr)) < ((void *) (*p_fromspace_end))))

#define TOSPACE_TO_NEWSPACE(p) (((p) - tospace_start) + newspace_start)
#define NEWSPACE_TO_TOSPACE(p) (((p) - newspace_start) + tospace_start)

#define READ_TOSPACE(addr) (* (NEWSPACE_TO_TOSPACE (addr)))
#define WRITE_TOSPACE(addr, obj) ((* (NEWSPACE_TO_TOSPACE (addr))) = (obj))

#define CLOSE_TOSPACE() do						\
{									\
  tospace_next = 0;							\
  newspace_start = 0;							\
  newspace_next = 0;							\
  newspace_end = 0;							\
} while (false)

#define GUARANTEE_TOSPACE_OPEN() do					\
{									\
  if (tospace_next == 0)						\
    tospace_closed ();							\
} while (false)

#define GUARANTEE_TOSPACE_CLOSED() do					\
{									\
  if (tospace_next != 0)						\
    tospace_open ();							\
} while (false)

#ifndef READ_REFERENCE_ADDRESS
#  define READ_REFERENCE_ADDRESS(addr)					\
     (* ((SCHEME_OBJECT **) (addr)))
#  define WRITE_REFERENCE_ADDRESS(ref, addr)				\
     ((* ((SCHEME_OBJECT **) (addr))) = (ref))
#endif

/* The weak chain is a linked list of all the live weak pairs whose
   cars are not GC-invariant, described below.

   The ephemeron list is a linked list of all the live ephemerons whose
   cars are not GC-invariant.  The ephemeron queue is a queue of all
   the live ephemerons whose keys have been proven live but whose data
   slots have not yet been scanned.  The ephemeron hash table is a map
   from fromspace addresses to lists of ephemerons, in which an
   association between a fromspace address and a list of ephemerons
   indicates that if the object stored at that fromspace address is
   proven live, those ephemerons must not be broken, and consequently
   their data must be live too.  */

static SCHEME_OBJECT * weak_chain;
static SCHEME_OBJECT ephemeron_list = SHARP_F;
static SCHEME_OBJECT ephemeron_queue = SHARP_F;
static bool scanning_ephemerons_p = false;

extern SCHEME_OBJECT ephemeron_array;
extern unsigned long ephemeron_count;

static void queue_ephemerons_for_key (SCHEME_OBJECT *);
static SCHEME_OBJECT gc_transport_weak_pair (SCHEME_OBJECT);
static SCHEME_OBJECT gc_transport_ephemeron (SCHEME_OBJECT);

static void run_gc_loop (SCHEME_OBJECT * , SCHEME_OBJECT **);
static void tospace_closed (void) NORETURN;
static void tospace_open (void) NORETURN;

#ifdef ENABLE_GC_DEBUGGING_TOOLS
#  ifndef GC_SCAN_HISTORY_SIZE
#    define GC_SCAN_HISTORY_SIZE 1024
#  endif
#  define INITIALIZE_GC_HISTORY initialize_gc_history
#  define HANDLE_GC_TRAP handle_gc_trap
#  define CHECK_NEWSPACE_SYNC check_newspace_sync
#  define DEBUG_TRANSPORT_ONE_WORD debug_transport_one_word

   static unsigned int gc_scan_history_index;
   static SCHEME_OBJECT * gc_scan_history [GC_SCAN_HISTORY_SIZE];
   static SCHEME_OBJECT * gc_to_history [GC_SCAN_HISTORY_SIZE];

   static SCHEME_OBJECT gc_trap
     = (MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_MAX_IMMEDIATE));
   static SCHEME_OBJECT * gc_scan_trap = 0;
   static SCHEME_OBJECT * gc_to_trap = 0;

   static SCHEME_OBJECT gc_object_referenced = SHARP_F;
   static SCHEME_OBJECT gc_object_references = SHARP_F;
   static unsigned long gc_object_references_count;
   static SCHEME_OBJECT * gc_object_references_scan;
   static SCHEME_OBJECT * gc_object_references_end;

   static unsigned long weak_chain_length;

   static void initialize_gc_history (void);
   static void handle_gc_trap (SCHEME_OBJECT *, SCHEME_OBJECT);
   static void check_newspace_sync (void);
   static void debug_transport_one_word (SCHEME_OBJECT, SCHEME_OBJECT *);
#else
#  define INITIALIZE_GC_HISTORY() do {} while (false)
#  define HANDLE_GC_TRAP(scan, object) do {} while (false)
#  define CHECK_NEWSPACE_SYNC() do {} while (false)
#  define DEBUG_TRANSPORT_ONE_WORD(object, from) do {} while (false)
#endif

void
initialize_gc (unsigned long n_words,
	       SCHEME_OBJECT ** pf_start,
	       SCHEME_OBJECT ** pf_end,
	       gc_tospace_allocator_t * allocator,
	       gc_abort_handler_t * abort_handler NORETURN)
{
  p_fromspace_start = pf_start;
  p_fromspace_end = pf_end;
  gc_tospace_allocator = allocator;
  gc_abort_handler = abort_handler;
  CLOSE_TOSPACE ();
  tospace_start = 0;
  tospace_end = 0;
  (*gc_tospace_allocator) (n_words, (&tospace_start), (&tospace_end));
}

void
resize_tospace (unsigned long n_words)
{
  GUARANTEE_TOSPACE_CLOSED ();
  (*gc_tospace_allocator) (n_words, (&tospace_start), (&tospace_end));
}

void
open_tospace (SCHEME_OBJECT * start)
{
  GUARANTEE_TOSPACE_CLOSED ();
  tospace_next = tospace_start;
  newspace_start = start;
  newspace_next = start;
  newspace_end = (start + (tospace_end - tospace_start));
}

bool
save_tospace (gc_walk_proc_t * proc, void * ctx)
{
  bool ok;

  GUARANTEE_TOSPACE_OPEN ();
  CHECK_NEWSPACE_SYNC ();
  ok = (proc (tospace_start, tospace_next, ctx));
  OS_free_pages (tospace_start, tospace_end);
  CLOSE_TOSPACE ();
  return (ok);
}

void
discard_tospace (void)
{
  GUARANTEE_TOSPACE_OPEN ();
  CHECK_NEWSPACE_SYNC ();
  OS_free_pages (tospace_start, tospace_end);
  CLOSE_TOSPACE ();
}

bool
tospace_available_p (unsigned long n_words)
{
  GUARANTEE_TOSPACE_OPEN ();
  return ((tospace_end - tospace_next) >= n_words);
}

void
add_to_tospace (SCHEME_OBJECT object)
{
  GUARANTEE_TOSPACE_OPEN ();
  (*tospace_next++) = object;
  newspace_next += 1;
}

SCHEME_OBJECT
read_tospace (SCHEME_OBJECT * addr)
{
  GUARANTEE_TOSPACE_OPEN ();
  return (READ_TOSPACE (addr));
}

void
write_tospace (SCHEME_OBJECT * addr, SCHEME_OBJECT object)
{
  GUARANTEE_TOSPACE_OPEN ();
  WRITE_TOSPACE (addr, object);
}

void
increment_tospace_ptr (unsigned long n_words)
{
  GUARANTEE_TOSPACE_OPEN ();
  tospace_next += n_words;
  newspace_next += n_words;
}

SCHEME_OBJECT *
get_newspace_ptr (void)
{
  return (newspace_next);
}

void *
tospace_to_newspace (void * addr)
{
  return
    (((addr >= ((void *) tospace_start))
      && (addr <= ((void *) tospace_end)))
     ? ((((byte_t *) addr) - ((byte_t *) tospace_start))
	+ ((byte_t *) newspace_start))
     : addr);
}

void *
newspace_to_tospace (void * addr)
{
  return
    (((addr >= ((void *) newspace_start))
      && (addr <= ((void *) newspace_end)))
     ? ((((byte_t *) addr) - ((byte_t *) newspace_start))
	+ ((byte_t *) tospace_start))
     : addr);
}

#define SIMPLE_HANDLER(name)						\
  (GCT_ENTRY (table, i)) = name;					\
  break

void
initialize_gc_table (gc_table_t * table, bool transport_p)
{
  unsigned int i;
  for (i = 0; (i < N_TYPE_CODES); i += 1)
    switch (gc_type_map[i])
      {
      case GC_NON_POINTER: SIMPLE_HANDLER (gc_handle_non_pointer);
      case GC_CELL:        SIMPLE_HANDLER (gc_handle_cell);
      case GC_PAIR:        SIMPLE_HANDLER (gc_handle_pair);
      case GC_TRIPLE:      SIMPLE_HANDLER (gc_handle_triple);
      case GC_QUADRUPLE:   SIMPLE_HANDLER (gc_handle_quadruple);
      case GC_VECTOR:      SIMPLE_HANDLER (gc_handle_unaligned_vector);
      case GC_COMPILED:    SIMPLE_HANDLER (gc_handle_cc_entry);
      case GC_UNDEFINED:   SIMPLE_HANDLER (gc_handle_undefined);

      case GC_SPECIAL:
	switch (i)
	  {
	  case TC_BROKEN_HEART:
	    SIMPLE_HANDLER (gc_handle_broken_heart);

	  case TC_REFERENCE_TRAP:
	    SIMPLE_HANDLER (gc_handle_reference_trap);

	  case TC_LINKAGE_SECTION:
	    SIMPLE_HANDLER (gc_handle_linkage_section);

	  case TC_MANIFEST_CLOSURE:
	    SIMPLE_HANDLER (gc_handle_manifest_closure);

	  case TC_MANIFEST_NM_VECTOR:
	    SIMPLE_HANDLER (gc_handle_nmv);

	  default:
	    std_gc_death ("unknown GC special type: %#02x\n", i);
	    break;
	  }
	break;
      }
  (GCT_ENTRY (table, TC_WEAK_CONS)) = gc_handle_weak_pair;
  (GCT_ENTRY (table, TC_EPHEMERON)) = gc_handle_ephemeron;
  (GCT_ENTRY (table, TC_BIG_FLONUM)) = gc_handle_aligned_vector;
  (GCT_ENTRY (table, TC_COMPILED_CODE_BLOCK)) = gc_handle_aligned_vector;
  (GCT_TUPLE (table)) = gc_tuple;
  (GCT_VECTOR (table)) = gc_vector;
  (GCT_CC_ENTRY (table)) = gc_cc_entry;
  if (transport_p)
    {
      (GCT_PRECHECK_FROM (table)) = gc_precheck_from;
      (GCT_TRANSPORT_WORDS (table)) = gc_transport_words;
    }
  else
    {
      (GCT_PRECHECK_FROM (table)) = gc_precheck_from_no_transport;
      (GCT_TRANSPORT_WORDS (table)) = gc_no_transport_words;
    }
  (GCT_IGNORE_OBJECT_P (table)) = 0;
  (GCT_RAW_ADDRESS_TO_OBJECT (table)) = gc_raw_address_to_object;
  (GCT_OBJECT_TO_RAW_ADDRESS (table)) = gc_object_to_raw_address;
  (GCT_RAW_ADDRESS_TO_CC_ENTRY (table)) = gc_raw_address_to_cc_entry;
  (GCT_CC_ENTRY_TO_RAW_ADDRESS (table)) = gc_cc_entry_to_raw_address;
}

gc_table_t *
std_gc_table (void)
{
  static bool initialized_p = false;
  static gc_table_t table;
  if (!initialized_p)
    {
      initialize_gc_table ((&table), true);
      initialized_p = true;
    }
  return (&table);
}

void
gc_scan_oldspace (SCHEME_OBJECT * scan, SCHEME_OBJECT * end)
{
  OS_expect_sequential_access (scan, end);
  run_gc_loop (scan, (&end));
  /* FIXME: This doesn't actually revert the expectation for [scan,
     end).  However, Unix has no way to query the madvice, or to
     dynamically scope it, so this is the best we can do.  Fortunately,
     at the moment, none of the system uses any special madvice, so it
     doesn't matter for now.  */
  OS_expect_normal_access (scan, end);
}

void
gc_scan_tospace (SCHEME_OBJECT * scan, SCHEME_OBJECT * end)
{
  if (end == 0)
    run_gc_loop ((NEWSPACE_TO_TOSPACE (scan)), (&tospace_next));
  else
    {
      SCHEME_OBJECT * tend = (NEWSPACE_TO_TOSPACE (end));
      run_gc_loop ((NEWSPACE_TO_TOSPACE (scan)), (&tend));
    }
}

static void
run_gc_loop (SCHEME_OBJECT * scan, SCHEME_OBJECT ** pend)
{
  gc_ignore_object_p_t * ignore_object_p
    = (GCT_IGNORE_OBJECT_P (current_gc_table));
  INITIALIZE_GC_HISTORY ();
  while (scan < (*pend))
    {
      SCHEME_OBJECT object = (*scan);
      HANDLE_GC_TRAP (scan, object);
      if ((ignore_object_p != 0) && ((*ignore_object_p) (object)))
	scan += 1;
      else
	{
	  current_scan = scan;
	  current_object = object;
	  scan
	    = ((* (GCT_ENTRY (current_gc_table, (OBJECT_TYPE (object)))))
	       (scan, object));
	}
    }
}

DEFINE_GC_TUPLE_HANDLER (gc_tuple)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (tuple));
  SCHEME_OBJECT * new_address = (GC_PRECHECK_FROM (from));
  return
    (OBJECT_NEW_ADDRESS (tuple,
			 ((new_address != 0)
			  ? new_address
			  : (GC_TRANSPORT_WORDS (from, n_words, false)))));
}

DEFINE_GC_VECTOR_HANDLER (gc_vector)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (vector));
  SCHEME_OBJECT * new_address = (GC_PRECHECK_FROM (from));
  return
    (OBJECT_NEW_ADDRESS (vector,
			 ((new_address != 0)
			  ? new_address
			  : (GC_TRANSPORT_WORDS (from,
						 (1 + (OBJECT_DATUM (*from))),
						 align_p)))));
}

DEFINE_GC_OBJECT_HANDLER (gc_cc_entry)
{
#ifdef CC_SUPPORT_P
  SCHEME_OBJECT old_block = (cc_entry_to_block (object));
  SCHEME_OBJECT new_block = (GC_HANDLE_VECTOR (old_block, true));
  return (CC_ENTRY_NEW_BLOCK (object,
			      (OBJECT_ADDRESS (new_block)),
			      (OBJECT_ADDRESS (old_block))));
#else
  gc_no_cc_support ();
  return (object);
#endif
}

DEFINE_GC_PRECHECK_FROM (gc_precheck_from)
{
#if 0
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  if (!ADDRESS_IN_MEMORY_BLOCK_P (from))
    std_gc_death ("out of range pointer: %#lx", ((unsigned long) from));
#endif
#endif
  if (!ADDRESS_IN_FROMSPACE_P (from))
    return (from);
  if (BROKEN_HEART_P (*from))
    return (OBJECT_ADDRESS (*from));
  if (scanning_ephemerons_p)
    /* It would be nice if we had the new address, too; that way we
       could eliminate a post-processing loop over the list of all
       ephemerons.  However, the GC abstraction doesn't have a nice way
       to do that.  */
    queue_ephemerons_for_key (from);
  return (0);
}

DEFINE_GC_PRECHECK_FROM (gc_precheck_from_no_transport)
{
#if 0
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  if (!ADDRESS_IN_MEMORY_BLOCK_P (from))
    std_gc_death ("out of range pointer: %#lx", ((unsigned long) from));
#endif
#endif
  return (from);
}

DEFINE_GC_TRANSPORT_WORDS (gc_transport_words)
{
  SCHEME_OBJECT * from_start = from;
  SCHEME_OBJECT * from_end = (from_start + n_words);
  SCHEME_OBJECT * new_address;

  GUARANTEE_TOSPACE_OPEN ();
  if (align_p)
    while (!FLOATING_ALIGNED_P (newspace_next))
      {
	(*tospace_next++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));
	newspace_next += 1;
      }
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  if (tospace_next >= tospace_end)
    std_gc_death ("tospace completely filled");
  {
    SCHEME_OBJECT * end = (tospace_next + n_words);
    if (end > tospace_end)
      std_gc_death ("block overflows tospace: %#lx",
		    ((unsigned long) end));
  }
  if (n_words == 0)
    std_gc_death ("gc_transport_words: attempt to transfer zero words.");
  if (n_words > 0x10000)
    {
      outf_error ("\nWarning: copying large block: %lu\n", n_words);
      outf_flush_error ();
    }
#endif
  new_address = newspace_next;
  while (from < from_end)
    {
      DEBUG_TRANSPORT_ONE_WORD (current_object, from);
      (*tospace_next++) = (*from++);
      newspace_next += 1;
    }
  (*from_start) = (MAKE_BROKEN_HEART (new_address));
  return (new_address);
}

DEFINE_GC_TRANSPORT_WORDS (gc_no_transport_words)
{
  tospace_closed ();
  return (from);
}

DEFINE_GC_HANDLER (gc_handle_non_pointer)
{
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_cell)
{
  (*scan) = (GC_HANDLE_TUPLE (object, 1));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_pair)
{
  (*scan) = (GC_HANDLE_TUPLE (object, 2));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_triple)
{
  (*scan) = (GC_HANDLE_TUPLE (object, 3));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_quadruple)
{
  (*scan) = (GC_HANDLE_TUPLE (object, 4));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_weak_pair)
{
  SCHEME_OBJECT * new_address = (GC_PRECHECK_FROM (OBJECT_ADDRESS (object)));
  (*scan)
    = ((new_address != 0)
       ? (OBJECT_NEW_ADDRESS (object, new_address))
       : (gc_transport_weak_pair (object)));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_ephemeron)
{
  SCHEME_OBJECT * new_address = (GC_PRECHECK_FROM (OBJECT_ADDRESS (object)));
  (*scan)
    = ((new_address != 0)
       ? (OBJECT_NEW_ADDRESS (object, new_address))
       : (gc_transport_ephemeron (object)));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_cc_entry)
{
  (*scan) = (GC_HANDLE_CC_ENTRY (object));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_aligned_vector)
{
  (*scan) = (GC_HANDLE_VECTOR (object, true));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_unaligned_vector)
{
  (*scan) = (GC_HANDLE_VECTOR (object, false));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_broken_heart)
{
  std_gc_death ("broken heart in scan: %#lx", object);
  return (scan);
}

DEFINE_GC_HANDLER (gc_handle_nmv)
{
  return (scan + 1 + (OBJECT_DATUM (object)));
}

DEFINE_GC_HANDLER (gc_handle_reference_trap)
{
  (*scan) = (((OBJECT_DATUM (object)) <= TRAP_MAX_IMMEDIATE)
	     ? object
	     : (GC_HANDLE_TUPLE (object, 2)));
  return (scan + 1);
}

SCHEME_OBJECT
gc_raw_address_to_object (unsigned int type, SCHEME_OBJECT * address)
{
  return (MAKE_POINTER_OBJECT (type, address));
}

SCHEME_OBJECT *
gc_object_to_raw_address (SCHEME_OBJECT object)
{
  return (OBJECT_ADDRESS (object));
}

SCHEME_OBJECT
gc_raw_address_to_cc_entry (insn_t * address)
{
  return (MAKE_CC_ENTRY (address));
}

insn_t *
gc_cc_entry_to_raw_address (SCHEME_OBJECT entry)
{
  return (CC_ENTRY_ADDRESS (entry));
}

DEFINE_GC_HANDLER (gc_handle_linkage_section)
{
#ifdef CC_SUPPORT_P
  unsigned long count = (linkage_section_count (object));
  scan += 1;
  switch (linkage_section_type (object))
    {
    case LINKAGE_SECTION_TYPE_REFERENCE:
    case LINKAGE_SECTION_TYPE_ASSIGNMENT:
      while (count > 0)
	{
	  WRITE_REFERENCE_ADDRESS
	    ((GC_OBJECT_TO_RAW_ADDRESS
	      (GC_HANDLE_TUPLE
	       ((GC_RAW_ADDRESS_TO_OBJECT
		 (TC_HUNK3,
		  (READ_REFERENCE_ADDRESS (scan)))),
		3))),
	     scan);
	  scan += 1;
	  count -= 1;
	}
      break;

    case LINKAGE_SECTION_TYPE_OPERATOR:
    case LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR:
      {
	DECLARE_RELOCATION_REFERENCE (ref);
	START_OPERATOR_RELOCATION (scan, ref);
	while (count > 0)
	  {
	    write_uuo_target
	      ((GC_CC_ENTRY_TO_RAW_ADDRESS
		(GC_HANDLE_CC_ENTRY
		 (GC_RAW_ADDRESS_TO_CC_ENTRY
		  (READ_UUO_TARGET (scan, ref))))),
	       scan);
	    scan += UUO_LINK_SIZE;
	    count -= 1;
	  }
      }
      break;

    default:
      std_gc_death ("Unknown linkage-section type.");
      break;
    }
  return (scan);
#else
  gc_no_cc_support ();
  return (scan);
#endif
}

DEFINE_GC_HANDLER (gc_handle_manifest_closure)
{
#ifdef CC_SUPPORT_P
#ifdef EMBEDDED_CLOSURE_ADDRS_P
  DECLARE_RELOCATION_REFERENCE (ref);
  START_CLOSURE_RELOCATION (scan, ref);
  scan += 1;
  {
    insn_t * start = (compiled_closure_start (scan));
    unsigned long count = (compiled_closure_count (scan));
    while (count > 0)
      {
	write_compiled_closure_target
	  ((GC_CC_ENTRY_TO_RAW_ADDRESS
	    (GC_HANDLE_CC_ENTRY
	     (GC_RAW_ADDRESS_TO_CC_ENTRY
	      (READ_COMPILED_CLOSURE_TARGET (start, ref))))),
	   start);
	start = (compiled_closure_next (start));
	count -= 1;
      }
    scan = (skip_compiled_closure_padding (start));
  }
  return (scan);
#else
  return (compiled_closure_objects (scan + 1));
#endif
#else
  gc_no_cc_support ();
  return (scan);
#endif
}

DEFINE_GC_HANDLER (gc_handle_undefined)
{
  gc_bad_type (object);
  return (scan + 1);
}

/* Weak pairs are supported by adding an extra pass to the GC.  During
   the normal pass, a weak pair is transported to new space, but the
   car of the pair is marked as a non-pointer so it won't be traced.
   Then the original weak pair in old space is chained into a list.
   This work is performed by 'gc_transport_weak_pair'.

   At the end of this pass, we have a list of all of the old weak
   pairs.  Since each weak pair in old space has a broken-heart
   pointer to the corresponding weak pair in new space, we also have a
   list of all of the new weak pairs.

   The extra pass then traverses this list, restoring the original
   type of the object in the car of each pair.  Then, if the car is a
   pointer that hasn't been copied to new space, it is replaced by #F.
   This work is performed by 'update_weak_pointers'.

   Here is a diagram showing the layout of a weak pair immediately
   after it is transported to new space.  After the normal pass is
   complete, the only thing that will have changed is that the "old
   CDR object" will have been updated to point to new space, if it is
   a pointer object.


   weak_chain       old space           |         new space
       |      _______________________   |   _______________________
       |      |broken |     new     |   |   |      |              |
       +=====>|heart  |  location ======|==>| NULL | old CAR data |
	      |_______|_____________|   |   |______|______________|
	      |old car|   next in   |   |   |                     |
	      | type  |    chain    |   |   |   old CDR object    |
	      |_______|_____________|   |   |_____________________|

 */

static SCHEME_OBJECT *
weak_referent_address (SCHEME_OBJECT object)
{
  switch (gc_ptr_type (object))
    {
    case GC_POINTER_NORMAL:
      return (OBJECT_ADDRESS (object));

    case GC_POINTER_COMPILED:
#ifdef CC_SUPPORT_P
      return (cc_entry_address_to_block_address (CC_ENTRY_ADDRESS (object)));
#else
      gc_no_cc_support ();
#endif

    default:
      return (0);
    }
}

static SCHEME_OBJECT
weak_referent_forward (SCHEME_OBJECT object)
{
  SCHEME_OBJECT * addr;

  switch (gc_ptr_type (object))
    {
    case GC_POINTER_NORMAL:
      addr = (OBJECT_ADDRESS (object));
      if (BROKEN_HEART_P (*addr))
	return (MAKE_OBJECT_FROM_OBJECTS (object, (*addr)));
      return (SHARP_F);

    case GC_POINTER_COMPILED:
#ifdef CC_SUPPORT_P
      addr = (cc_entry_address_to_block_address (CC_ENTRY_ADDRESS (object)));
      if (BROKEN_HEART_P (*addr))
	return (CC_ENTRY_NEW_BLOCK (object, (OBJECT_ADDRESS (*addr)), addr));
#else
      gc_no_cc_support ();
#endif
      return (SHARP_F);

    case GC_POINTER_NOT:
    default:			/* suppress bogus GCC warning */
      std_gc_death ("Non-pointer cannot be a weak reference.");
      return (SHARP_F);
    }
}

static void
queue_ephemerons_for_key (SCHEME_OBJECT * addr)
{
  SCHEME_OBJECT ht = ephemeron_array;
  unsigned long index = (((unsigned long) addr) % (VECTOR_LENGTH (ht)));
  SCHEME_OBJECT * entry_loc = (VECTOR_LOC (ht, index));
  SCHEME_OBJECT entry;

#ifdef ENABLE_GC_DEBUGGING_TOOLS
  if (!scanning_ephemerons_p)
    std_gc_death ("queue_ephemerons_for_key while not scanning ephemerons");

  if (!ADDRESS_IN_FROMSPACE_P (addr))
    std_gc_death ("Queueing ephemerons for key with non-fromspace address.");
#endif

  while (EPHEMERON_P (entry = (*entry_loc)))
    {
      SCHEME_OBJECT * entry_addr = (OBJECT_ADDRESS (entry));
      SCHEME_OBJECT * next_loc
	= (NEWSPACE_TO_TOSPACE (entry_addr + EPHEMERON_NEXT));
      SCHEME_OBJECT * key_addr
	= (weak_referent_address (READ_TOSPACE (entry_addr + EPHEMERON_KEY)));
      if (addr == key_addr)
	{
	  (*entry_loc) = (*next_loc);
	  (*next_loc) = ephemeron_queue;
	  ephemeron_queue = entry;
	}
      entry_loc = next_loc;
    }
}

static SCHEME_OBJECT
gc_transport_weak_pair (SCHEME_OBJECT pair)
{
  SCHEME_OBJECT * old_addr = (OBJECT_ADDRESS (pair));
  SCHEME_OBJECT * new_addr = (GC_TRANSPORT_WORDS (old_addr, 2, false));
  SCHEME_OBJECT old_car = (READ_TOSPACE (new_addr));
  SCHEME_OBJECT * caddr = (weak_referent_address (old_car));

  if ((caddr != 0) && (ADDRESS_IN_FROMSPACE_P (caddr)))
    {
      WRITE_TOSPACE (new_addr, (OBJECT_NEW_TYPE (TC_NULL, old_car)));
      (old_addr[1])
	= ((weak_chain == 0)
	   ? (MAKE_OBJECT ((OBJECT_TYPE (old_car)), 0))
	   : (MAKE_POINTER_OBJECT ((OBJECT_TYPE (old_car)), weak_chain)));
      weak_chain = old_addr;
#ifdef ENABLE_GC_DEBUGGING_TOOLS
      weak_chain_length += 1;
#endif
    }
  return (OBJECT_NEW_ADDRESS (pair, new_addr));
}

static SCHEME_OBJECT
gc_transport_ephemeron (SCHEME_OBJECT old_ephemeron)
{
  SCHEME_OBJECT * old_addr = (OBJECT_ADDRESS (old_ephemeron));
  SCHEME_OBJECT * new_addr
    = (GC_TRANSPORT_WORDS (old_addr, EPHEMERON_SIZE, false));
  SCHEME_OBJECT new_ephemeron = (OBJECT_NEW_ADDRESS (old_ephemeron, new_addr));
  SCHEME_OBJECT old_key = (READ_TOSPACE (new_addr + EPHEMERON_KEY));
  SCHEME_OBJECT * old_key_addr = (weak_referent_address (old_key));
  SCHEME_OBJECT index;
  SCHEME_OBJECT ht = ephemeron_array;

  ephemeron_count += 1;

  /* If the key is GC-invariant or live, the ephemeron will not be
     broken, so leave a marked vector manifest to make the GC will scan
     its contents, including the datum.  */
  if ((old_key_addr == 0)
      || (!ADDRESS_IN_FROMSPACE_P (old_key_addr))
      || (SHARP_F != (weak_referent_forward (old_key))))
    {
      WRITE_TOSPACE (new_addr, MARKED_EPHEMERON_MANIFEST);
      return (new_ephemeron);
    }

  /* Write a manifest that makes the GC skip over the ephemeron.  */
  WRITE_TOSPACE (new_addr, UNMARKED_EPHEMERON_MANIFEST);

  /* Map its key back to it.  */
  index = (((unsigned long) old_key_addr) % (VECTOR_LENGTH (ht)));
  WRITE_TOSPACE ((new_addr + EPHEMERON_NEXT), (VECTOR_REF (ht, index)));
  VECTOR_SET (ht, index, new_ephemeron);

  /* Link it up in the ephemeron list.  */
  WRITE_TOSPACE ((new_addr + EPHEMERON_LIST), ephemeron_list);
  ephemeron_list = new_ephemeron;

  return (new_ephemeron);
}

static void
scan_newspace_addr (SCHEME_OBJECT * addr)
{
  gc_ignore_object_p_t * ignore_object_p
    = (GCT_IGNORE_OBJECT_P (current_gc_table));
  SCHEME_OBJECT * scan;
  SCHEME_OBJECT object;

  addr = (NEWSPACE_TO_TOSPACE (addr));
  scan = addr;

  INITIALIZE_GC_HISTORY ();
  object = (*scan);
  HANDLE_GC_TRAP (scan, object);
  if ((ignore_object_p != 0) && ((*ignore_object_p) (object)))
    return;

  current_scan = scan;
  current_object = object;
  scan = ((* (GCT_ENTRY (current_gc_table, (OBJECT_TYPE (object)))))
	  (scan, object));
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  if (scan != (addr + 1))
    std_gc_death ("scan_newspace_addr overflowed");
#endif
}

static void
scan_ephemerons (void)
{
  SCHEME_OBJECT ephemeron = ephemeron_list;
  SCHEME_OBJECT * saved_newspace_next = newspace_next;
  scanning_ephemerons_p = true;
  while (EPHEMERON_P (ephemeron))
    {
      SCHEME_OBJECT * ephemeron_addr = (OBJECT_ADDRESS (ephemeron));
      SCHEME_OBJECT old_key = (READ_TOSPACE (ephemeron_addr + EPHEMERON_KEY));
      ephemeron = (READ_TOSPACE (ephemeron_addr + EPHEMERON_LIST));
      /* It is tempting to scan the ephemeron's datum right here and
	 now, but we can't do that because it may already be in the
	 queue, and the assumption is that for each ephemeron in the
	 queue, its key has been proven live but its datum has not yet
	 been scanned.  It is tempting to link this up in the queue
	 right here and now, but we can't do that, because we must also
	 delete it from the hash table so that nothing else will put it
	 in the queue again.  */
      if (SHARP_F != (weak_referent_forward (old_key)))
	queue_ephemerons_for_key (weak_referent_address (old_key));
    }
  while (EPHEMERON_P (ephemeron = ephemeron_queue))
    {
      SCHEME_OBJECT * ephemeron_addr = (OBJECT_ADDRESS (ephemeron));
#ifdef ENABLE_GC_DEBUGGING_TOOLS
      {
	SCHEME_OBJECT key = (READ_TOSPACE (ephemeron_addr + EPHEMERON_KEY));
	if (! (weak_referent_forward (key)))
	  std_gc_death
	    ("Ephemeron queued whose key has not been forwarded: %lx", key);
      }
#endif
      ephemeron_queue = (READ_TOSPACE (ephemeron_addr + EPHEMERON_NEXT));
      saved_newspace_next = newspace_next;
      scan_newspace_addr (ephemeron_addr + EPHEMERON_DATUM);
      gc_scan_tospace (saved_newspace_next, 0);
    }
  scanning_ephemerons_p = false;
}

void
initialize_weak_chain (void)
{
  weak_chain = 0;
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  weak_chain_length = 0;
  if (ephemeron_list != SHARP_F) std_gc_death ("Bad ephemeron list.");
  if (ephemeron_queue != SHARP_F) std_gc_death ("Bad ephemeron queue.");
  if (scanning_ephemerons_p != SHARP_F) std_gc_death ("Bad ephemeron state.");
#endif
}

static void
update_ephemerons (void)
{
  SCHEME_OBJECT ephemeron = ephemeron_list;
  while (EPHEMERON_P (ephemeron))
    {
      SCHEME_OBJECT * ephemeron_addr = (OBJECT_ADDRESS (ephemeron));
      SCHEME_OBJECT * key_loc = (ephemeron_addr + EPHEMERON_KEY);
      SCHEME_OBJECT old_key = (READ_TOSPACE (key_loc));
      SCHEME_OBJECT new_key = (weak_referent_forward (old_key));
      WRITE_TOSPACE (ephemeron_addr, MARKED_EPHEMERON_MANIFEST);
      WRITE_TOSPACE (key_loc, new_key);
      /* Advance before we clobber the list pointer.  */
      ephemeron = (READ_TOSPACE (ephemeron_addr + EPHEMERON_LIST));
      WRITE_TOSPACE ((ephemeron_addr + EPHEMERON_LIST), SHARP_F);
      WRITE_TOSPACE ((ephemeron_addr + EPHEMERON_NEXT), SHARP_F);
      if (new_key == SHARP_F)
	WRITE_TOSPACE ((ephemeron_addr + EPHEMERON_DATUM), SHARP_F);
    }
  ephemeron_list = SHARP_F;
}

static void
update_weak_pairs (void)
{
#if 0
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  outf_console ("; **** Weak chain length = %lu\n", weak_chain_length);
  outf_flush_console ();
#endif
#endif
  while (weak_chain != 0)
    {
      SCHEME_OBJECT * new_addr = (OBJECT_ADDRESS (weak_chain[0]));
      SCHEME_OBJECT obj = (weak_chain[1]);
      SCHEME_OBJECT old_car
	= (OBJECT_NEW_TYPE ((OBJECT_TYPE (obj)),
			    (READ_TOSPACE (new_addr))));

      WRITE_TOSPACE (new_addr, (weak_referent_forward (old_car)));
      weak_chain = (((OBJECT_DATUM (obj)) == 0) ? 0 : (OBJECT_ADDRESS (obj)));
    }
}

void
update_weak_pointers (void)
{
  scan_ephemerons ();
  update_ephemerons ();
  update_weak_pairs ();
}

void
std_gc_death (const char * format, ...)
{
  va_list ap;

  va_start (ap, format);
  outf_fatal ("\n");
  voutf_fatal (format, ap);
  outf_fatal ("\n");
  if (current_scan != 0)
    {
      outf_fatal ("scan = 0x%lx", ((unsigned long) current_scan));
      if (tospace_next != 0)
	outf_fatal ("; to = 0x%lx", ((unsigned long) tospace_next));
      outf_fatal ("\n");
    }
  va_end (ap);
  if (gc_abort_handler != 0)
    (*gc_abort_handler) ();
  exit (1);
}

static void
tospace_closed (void)
{
  std_gc_death ("GC transport not allowed here");
}

static void
tospace_open (void)
{
  std_gc_death ("tospace is open, should be closed");
}

void
gc_no_cc_support (void)
{
  std_gc_death ("No compiled-code support.");
}

void
gc_bad_type (SCHEME_OBJECT object)
{
  std_gc_death ("bad type code: %#02lx %#lx",
		(OBJECT_TYPE (object)),
		object);
}

#ifdef ENABLE_GC_DEBUGGING_TOOLS

static void
initialize_gc_history (void)
{
  gc_scan_history_index = 0;
  memset (gc_scan_history, 0, (sizeof (gc_scan_history)));
  memset (gc_to_history, 0, (sizeof (gc_to_history)));
}

static void
handle_gc_trap (SCHEME_OBJECT * scan, SCHEME_OBJECT object)
{
  (gc_scan_history[gc_scan_history_index]) = scan;
  (gc_to_history[gc_scan_history_index]) = newspace_next;
  gc_scan_history_index += 1;
  if (gc_scan_history_index == GC_SCAN_HISTORY_SIZE)
    gc_scan_history_index = 0;
  if ((object == gc_trap)
      || ((gc_scan_trap != 0)
	  && (scan >= gc_scan_trap))
      || ((gc_to_trap != 0)
	  && (newspace_next != 0)
	  && (newspace_next >= gc_to_trap)))
    {
      outf_error ("\nhandle_gc_trap: trap.\n");
      abort ();
    }
}

static void
check_newspace_sync (void)
{
  if ((newspace_next - newspace_start)
      != (tospace_next - tospace_start))
    std_gc_death (
#ifdef HAVE_STDC_99
		  "mismatch between newspace and tospace ptrs: %td/%td",
#else
		  "mismatch between newspace and tospace ptrs: %ld/%ld",
#endif
		  (newspace_next - newspace_start),
		  (tospace_next - tospace_start));
}

void
collect_gc_object_references (SCHEME_OBJECT object, SCHEME_OBJECT collector)
{
  gc_object_referenced = object;
  gc_object_references = collector;
}

static void
debug_transport_one_word (SCHEME_OBJECT object, SCHEME_OBJECT * from)
{
  if ((gc_object_references != SHARP_F)
      && (gc_object_referenced == (*from)))
    {
      gc_object_references_count += 1;
      if (gc_object_references_scan < gc_object_references_end)
	(*gc_object_references_scan++) = object;
    }
}

void
initialize_gc_object_references (void)
{
  if (gc_object_references != SHARP_F)
    {
      /* Temporarily change to non-marked vector.  */
      MEMORY_SET
	(gc_object_references, 0,
	 (MAKE_OBJECT
	  (TC_MANIFEST_NM_VECTOR,
	   (OBJECT_DATUM (MEMORY_REF (gc_object_references, 0))))));
      gc_object_references_count = 0;
      gc_object_references_scan = (VECTOR_LOC (gc_object_references, 1));
      gc_object_references_end
	= (VECTOR_LOC (gc_object_references,
		       (VECTOR_LENGTH (gc_object_references))));
      /* Wipe the table.  */
      VECTOR_SET (gc_object_references, 0, FIXNUM_ZERO);
      {
	SCHEME_OBJECT * scan = gc_object_references_scan;
	while (scan < gc_object_references_end)
	  (*scan++) = SHARP_F;
      }
      (*tospace_next++) = gc_object_references;
      newspace_next += 1;
    }
}

void
finalize_gc_object_references (void)
{
  if (gc_object_references != SHARP_F)
    {
      SCHEME_OBJECT header = (MEMORY_REF (gc_object_references, 0));
      if (BROKEN_HEART_P (header))
	{
	  SCHEME_OBJECT * to_addr
	    = (NEWSPACE_TO_TOSPACE (OBJECT_ADDRESS (header)));
	  SCHEME_OBJECT * scan_to = to_addr;
	  SCHEME_OBJECT * scan_from = (VECTOR_LOC (gc_object_references, 0));

	  /* Change back to marked vector.  */
	  (*scan_to++)
	    = (MAKE_OBJECT (TC_MANIFEST_VECTOR, (OBJECT_DATUM (*to_addr))));

	  /* Store the count in the table.  */
	  VECTOR_SET (gc_object_references, 0,
		      (ULONG_TO_FIXNUM (gc_object_references_count)));

	  /* Make sure tospace copy is up to date.  */
	  while (scan_from < gc_object_references_scan)
	    (*scan_to++) = (*scan_from++);

	  /* No need to scan the vector's contents, since anything
	     here has already been transported.  */
	}
      gc_object_references = SHARP_F;
      gc_object_referenced = SHARP_F;
    }
}

#endif /* ENABLE_GC_DEBUGGING_TOOLS */

gc_type_t gc_type_map [N_TYPE_CODES] =
{
  GC_NON_POINTER,		/* TC_NULL,etc */
  GC_PAIR,			/* TC_LIST */
  GC_NON_POINTER,		/* TC_CHARACTER */
  GC_PAIR,		   	/* TC_SCODE_QUOTE */
  GC_TRIPLE,		        /* TC_PCOMB2 */
  GC_PAIR,			/* TC_UNINTERNED_SYMBOL */
  GC_VECTOR,			/* TC_BIG_FLONUM */
  GC_PAIR,			/* TC_COMBINATION_1 */
  GC_NON_POINTER,		/* TC_CONSTANT */
  GC_PAIR,			/* TC_EXTENDED_PROCEDURE */
  GC_VECTOR,			/* TC_VECTOR */
  GC_NON_POINTER,		/* TC_RETURN_CODE */
  GC_TRIPLE,			/* TC_COMBINATION_2 */
  GC_SPECIAL,			/* TC_MANIFEST_CLOSURE */
  GC_VECTOR,			/* TC_BIG_FIXNUM */
  GC_PAIR,			/* TC_PROCEDURE */
  GC_PAIR,			/* TC_ENTITY */
  GC_PAIR,			/* TC_DELAY */
  GC_VECTOR,			/* TC_ENVIRONMENT */
  GC_PAIR,			/* TC_DELAYED */
  GC_TRIPLE,			/* TC_EXTENDED_LAMBDA */
  GC_PAIR,			/* TC_COMMENT */
  GC_VECTOR,			/* TC_NON_MARKED_VECTOR */
  GC_PAIR,			/* TC_LAMBDA */
  GC_NON_POINTER,		/* TC_PRIMITIVE */
  GC_PAIR,			/* TC_SEQUENCE_2 */
  GC_NON_POINTER,		/* TC_FIXNUM */
  GC_PAIR,			/* TC_PCOMB1 */
  GC_VECTOR,			/* TC_CONTROL_POINT */
  GC_PAIR,			/* TC_INTERNED_SYMBOL */
  GC_VECTOR,			/* TC_CHARACTER_STRING,TC_VECTOR_8B */
  GC_PAIR,			/* TC_ACCESS */
  GC_TRIPLE,			/* TC_HUNK3_A */
  GC_PAIR,			/* TC_DEFINITION */
  GC_SPECIAL,			/* TC_BROKEN_HEART */
  GC_PAIR,			/* TC_ASSIGNMENT */
  GC_TRIPLE,			/* TC_HUNK3_B */
  GC_PAIR,			/* TC_IN_PACKAGE */
  GC_VECTOR,			/* TC_COMBINATION */
  GC_SPECIAL,			/* TC_MANIFEST_NM_VECTOR */
  GC_COMPILED,			/* TC_COMPILED_ENTRY */
  GC_PAIR,			/* TC_LEXPR */
  GC_VECTOR,			/* TC_PCOMB3 */
  GC_VECTOR,			/* TC_EPHEMERON */
  GC_TRIPLE,			/* TC_VARIABLE */
  GC_NON_POINTER,		/* TC_THE_ENVIRONMENT */
  GC_UNDEFINED,			/* 0x2E */
  GC_VECTOR,			/* TC_VECTOR_1B,TC_BIT_STRING */
  GC_NON_POINTER,		/* TC_PCOMB0 */
  GC_VECTOR,			/* TC_VECTOR_16B */
  GC_SPECIAL,			/* TC_REFERENCE_TRAP */
  GC_TRIPLE,			/* TC_SEQUENCE_3 */
  GC_TRIPLE,			/* TC_CONDITIONAL */
  GC_PAIR,			/* TC_DISJUNCTION */
  GC_CELL,			/* TC_CELL */
  GC_PAIR,			/* TC_WEAK_CONS */
  GC_QUADRUPLE,			/* TC_QUAD */
  GC_SPECIAL,			/* TC_LINKAGE_SECTION */
  GC_PAIR,			/* TC_RATNUM */
  GC_NON_POINTER,		/* TC_STACK_ENVIRONMENT */
  GC_PAIR,			/* TC_COMPLEX */
  GC_VECTOR,			/* TC_COMPILED_CODE_BLOCK */
  GC_VECTOR,			/* TC_RECORD */
  GC_UNDEFINED			/* 0x3F */
};

#if (N_TYPE_CODES != 0x40)
#  include "gcloop.c and object.h inconsistent -- gc_type_map"
#endif

gc_type_t
gc_type_code (unsigned int type_code)
{
  return (gc_type_map[type_code]);
}

gc_ptr_type_t
gc_ptr_type (SCHEME_OBJECT object)
{
  switch (GC_TYPE (object))
    {
    case GC_SPECIAL:
      return
	(((REFERENCE_TRAP_P (object))
	  && ((OBJECT_DATUM (object)) >= TRAP_MAX_IMMEDIATE))
	 ? GC_POINTER_NORMAL
	 : GC_POINTER_NOT);

    case GC_CELL:
    case GC_PAIR:
    case GC_TRIPLE:
    case GC_QUADRUPLE:
    case GC_VECTOR:
      return (GC_POINTER_NORMAL);

    case GC_COMPILED:
      return (GC_POINTER_COMPILED);
      break;

    default:
      return (GC_POINTER_NOT);
    }
}

SCHEME_OBJECT *
get_object_address (SCHEME_OBJECT object)
{
  switch (gc_ptr_type (object))
    {
    case GC_POINTER_NORMAL:
      return (OBJECT_ADDRESS (object));

    case GC_POINTER_COMPILED:
#ifdef CC_SUPPORT_P
      return (cc_entry_to_block_address (object));
#endif

    default:
      return (0);
    }
}
