/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchgcc.h,v 9.47 1992/06/04 12:42:30 jinx Exp $

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

#ifndef _BCHGCC_H_INCLUDED

#define _BCHGCC_H_INCLUDED

#include "oscond.h"
#include "gccode.h"

#ifdef _BSD
#  include <sys/file.h>
#else
#  ifndef F_GETFL
#    include <fcntl.h>
#  endif
#endif

#ifndef DOS386
#  include <sys/param.h>
#  define IO_PAGE_SIZE		4096
#endif

#ifndef BCH_START_CLOSURE_RELOCATION
#  define BCH_START_CLOSURE_RELOCATION(scan) do { } while (0)
#endif

#ifndef BCH_END_CLOSURE_RELOCATION
#  define BCH_END_CLOSURE_RELOCATION(scan) do { } while (0)
#endif

#ifndef BCH_EXTRACT_CLOSURE_ENTRY_ADDRESS
#  define BCH_EXTRACT_CLOSURE_ENTRY_ADDRESS EXTRACT_CLOSURE_ENTRY_ADDRESS
#endif

#ifndef BCH_STORE_CLOSURE_ENTRY_ADDRESS
#  define BCH_STORE_CLOSURE_ENTRY_ADDRESS STORE_CLOSURE_ENTRY_ADDRESS
#endif


#ifndef BCH_START_OPERATOR_RELOCATION
#  define BCH_START_OPERATOR_RELOCATION(scan) do { } while (0)
#endif

#ifndef BCH_END_OPERATOR_RELOCATION
#  define BCH_END_OPERATOR_RELOCATION(scan) do { } while (0)
#endif

#ifndef BCH_EXTRACT_OPERATOR_LINKAGE_ADDRESS
#  define BCH_EXTRACT_OPERATOR_LINKAGE_ADDRESS EXTRACT_OPERATOR_LINKAGE_ADDRESS
#endif

#ifndef BCH_STORE_OPERATOR_LINKAGE_ADDRESS
#  define BCH_STORE_OPERATOR_LINKAGE_ADDRESS STORE_OPERATOR_LINKAGE_ADDRESS
#endif


extern char * EXFUN (error_name, (int));

extern int EXFUN (retrying_file_operation,
		  (/* no prototype because (CONST char *) != (char *) */
		   int EXFUN((*), ()),
		   int, char *, long, long, char *, char *, long *,
		   int EXFUN((*), (char *, char *))));

extern int EXFUN (io_error_retry_p, (char *, char *));
extern int EXFUN (io_error_always_abort, (char *, char *));

#define GC_FILE_FLAGS		(O_RDWR | O_CREAT) /* O_SYNCIO removed */
#define GC_FILE_MASK		0644	/* Everyone reads, owner writes */

/* IO_PAGE_SIZE must be a power of 2! */

#ifdef DEV_BSIZE
#define IO_PAGE_SIZE DEV_BSIZE
#else
#define IO_PAGE_SIZE 8192
#endif

#define ALIGN_DOWN_TO_IO_PAGE(addr)					\
  (((unsigned long) (addr)) & (~(IO_PAGE_SIZE - 1)))

#define ALIGN_UP_TO_IO_PAGE(addr)					\
  (ALIGN_DOWN_TO_IO_PAGE (((unsigned long) (addr)) + (IO_PAGE_SIZE - 1)))

#define ALIGNED_TO_IO_PAGE_P(addr)					\
  (((unsigned long) (addr)) == (ALIGN_DOWN_TO_IO_PAGE (addr)))

extern long
  gc_file_end_position,
  gc_file_current_position,
  gc_file_start_position;

extern unsigned long
  gc_buffer_size,
  gc_buffer_bytes,
  gc_buffer_shift,
  gc_buffer_mask,
  gc_buffer_byte_shift;

extern char
  gc_death_message_buffer[];

extern SCHEME_OBJECT
  * scan_buffer_top,
  * scan_buffer_bottom,
  * free_buffer_top,
  * free_buffer_bottom,
  * weak_pair_stack_ptr,
  * weak_pair_stack_limit,
  * virtual_scan_pointer;

extern SCHEME_OBJECT
  * EXFUN (GCLoop, (SCHEME_OBJECT *, SCHEME_OBJECT **, SCHEME_OBJECT **)),
  * EXFUN (dump_and_reload_scan_buffer, (long, Boolean *)),
  * EXFUN (dump_and_reset_free_buffer, (long, Boolean *)),
  * EXFUN (dump_free_directly, (SCHEME_OBJECT *, long, Boolean *)),
  * EXFUN (initialize_free_buffer, (void)),
  * EXFUN (initialize_scan_buffer, (SCHEME_OBJECT *)),
  EXFUN (read_newspace_address, (SCHEME_OBJECT *));

extern void
  EXFUN (GC, (int)),
  EXFUN (end_transport, (Boolean *)),
  EXFUN (final_reload, (SCHEME_OBJECT *, unsigned long, char *)),
  EXFUN (extend_scan_buffer, (char *, SCHEME_OBJECT *)),
  EXFUN (gc_death, (long, char *, SCHEME_OBJECT *, SCHEME_OBJECT *)),
  EXFUN (restore_gc_file, (void)),
  EXFUN (initialize_weak_pair_transport, (SCHEME_OBJECT *));

extern char
  * EXFUN (end_scan_buffer_extension, (char *));

extern int
  EXFUN (swap_gc_file, (int));

/* Some utility macros */

/* These work even when scan/addr point to constant space
   because initialize_free_buffer (in bchmmg.c) cleverly initializes
   scan_buffer_bottom, scan_buffer_top, and virtual_scan_pointer
   so that the operations below do the right thing.

   These depend on (scan) and (addr) always pointing past the current
   Scan pointer!
 */

#define SCAN_POINTER_TO_NEWSPACE_ADDRESS(scan)				\
  (((char *) virtual_scan_pointer)					\
   + (((char *) (scan)) - ((char *) scan_buffer_bottom)))
      
#define READ_NEWSPACE_ADDRESS(loc, addr) do				\
{									\
  SCHEME_OBJECT * _addr, * _scaddr;					\
									\
  _addr = (addr);							\
  _scaddr = (scan_buffer_bottom + ((_addr) - virtual_scan_pointer));	\
									\
  if ((_scaddr >= scan_buffer_bottom) && (_scaddr < scan_buffer_top))	\
    (loc) = (* _scaddr);						\
  else if ((_addr >= Constant_Space) && (_addr < Free_Constant))	\
    (loc) = (* _addr);							\
  else									\
    (loc) = (read_newspace_address (_addr));				\
} while (0)

#define copy_weak_pair()						\
{									\
  SCHEME_OBJECT weak_car;						\
  long car_type;							\
									\
  weak_car = (*Old++);							\
  car_type = (OBJECT_TYPE (weak_car));					\
  if ((car_type == TC_NULL)						\
      || ((OBJECT_ADDRESS (weak_car)) >= Low_Constant))			\
  {									\
    *To++ = weak_car;							\
    *To++ = (*Old);							\
  }									\
  else if (weak_pair_stack_ptr > weak_pair_stack_limit)			\
  {									\
    *--weak_pair_stack_ptr = ((SCHEME_OBJECT) To_Address);		\
    *--weak_pair_stack_ptr = weak_car;					\
    *To++ = SHARP_F;							\
    *To++ = (*Old);							\
  }									\
  else									\
  {									\
    *To++ = (OBJECT_NEW_TYPE (TC_NULL, weak_car));			\
    *To++ = *Old;							\
    *Old = (OBJECT_NEW_TYPE (car_type, Weak_Chain));			\
    Weak_Chain = Temp;							\
  }									\
}

#define copy_cell()							\
{									\
  *To++ = *Old;								\
}

#define copy_pair()							\
{									\
  *To++ = *Old++;							\
  *To++ = *Old;								\
}

#define copy_triple()							\
{									\
  *To++ = *Old++;							\
  *To++ = *Old++;							\
  *To++ = *Old;								\
}

#define copy_quadruple()						\
{									\
  *To++ = *Old++;							\
  *To++ = *Old++;							\
  *To++ = *Old++;							\
  *To++ = *Old;								\
}

/* Transporting vectors is done in 3 parts:
   - Finish filling the current free buffer, dump it, and get a new one.
   - Dump the middle of the vector directly by bufferfulls.
   - Copy the end of the vector to the new buffer.
   The last piece of code is the only one executed when the vector does
   not overflow the current buffer.
*/

#define copy_vector(success)						\
{									\
  SCHEME_OBJECT * Saved_Scan = Scan;					\
  unsigned long real_length = (1 + (OBJECT_DATUM (*Old)));		\
									\
  To_Address += real_length;						\
  Scan = (To + real_length);						\
  if (Scan >= free_buffer_top)						\
  {									\
    unsigned long overflow;						\
									\
    overflow = (Scan - free_buffer_top);				\
    while (To != free_buffer_top)					\
      *To++ = *Old++;							\
    To = (dump_and_reset_free_buffer (0, success));			\
    real_length = (overflow >> gc_buffer_shift);			\
    if (real_length > 0)						\
      To = dump_free_directly (Old, real_length, success);		\
    Old += (real_length << gc_buffer_shift);				\
    Scan = To + (overflow & gc_buffer_mask);				\
  }									\
  while (To != Scan)							\
    *To++ = *Old++;							\
  Scan = Saved_Scan;							\
}

/* Utility macros. */

#define relocate_normal_setup()						\
{									\
  Old = (OBJECT_ADDRESS (Temp));					\
  if (Old >= Low_Constant)						\
    continue;								\
  if ((OBJECT_TYPE (*Old)) == TC_BROKEN_HEART)				\
  {									\
    *Scan = (MAKE_OBJECT_FROM_OBJECTS (Temp, *Old));			\
    continue;								\
  }									\
  New_Address = (MAKE_BROKEN_HEART (To_Address));			\
}

#define relocate_normal_transport(copy_code, length)			\
{									\
  copy_code;								\
  To_Address += (length);						\
  if (To >= free_buffer_top)						\
    To = (dump_and_reset_free_buffer ((To - free_buffer_top), NULL));	\
}

#define relocate_normal_end()						\
{									\
  *(OBJECT_ADDRESS (Temp)) = New_Address;				\
  *Scan = (MAKE_OBJECT_FROM_OBJECTS (Temp, New_Address));		\
  continue;								\
}

#define relocate_normal_pointer(copy_code, length)			\
{									\
  relocate_normal_setup ();						\
  relocate_normal_transport (copy_code, length);			\
  relocate_normal_end ();						\
}

#ifdef FLOATING_ALIGNMENT

#define FLOAT_ALIGN_FREE(free,free_ptr)					\
do {									\
  while ((((long) ((free) + 1)) & FLOATING_ALIGNMENT) != 0)		\
  {									\
    free += 1;								\
    *free_ptr++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));		\
  }									\
} while (0)

#define relocate_flonum_setup()						\
{									\
  relocate_normal_setup ();						\
  FLOAT_ALIGN_FREE (To_Address, To);					\
  New_Address = (MAKE_BROKEN_HEART (To_Address));			\
}

#else /* FLOATING_ALIGNMENT */

#define FLOAT_ALIGN_FREE(free,free_ptr)					\
do {									\
} while (0)

#define relocate_flonum_setup()	relocate_normal_setup()

#endif /* FLOATING_ALIGNMENT */

/* Typeless objects (implicit types). */

#define relocate_typeless_setup()					\
{									\
  Old = ((SCHEME_OBJECT *) Temp);					\
  if (Old >= Low_Constant)						\
    continue;								\
  if ((OBJECT_TYPE (*Old)) == TC_BROKEN_HEART)				\
  {									\
    *Scan = ((SCHEME_OBJECT) (OBJECT_ADDRESS (*Old)));			\
    continue;								\
  }									\
  New_Address = ((SCHEME_OBJECT) To_Address);				\
}

#define relocate_typeless_transport(copy_code, length)			\
{									\
  relocate_normal_transport (copy_code, length);			\
}

#define relocate_typeless_end()						\
{									\
  (* ((SCHEME_OBJECT *) Temp)) = (MAKE_BROKEN_HEART (New_Address));	\
  *Scan = New_Address;							\
  continue;								\
}

#define relocate_typeless_pointer(copy_code, length)			\
{									\
  relocate_typeless_setup ();						\
  relocate_typeless_transport (copy_code, length);			\
  relocate_typeless_end ();						\
}

/* The following macro uses do-while to trap the use of continue.
   On certain machines, the operator/closure need to be updated
   since the only addressing mode is pc-relative and the object
   containing the reference may not be at the same address as it was
   last time.
   In addition, we may be in the middle of a scan-buffer extension,
   which we need to finish.
 */

#define relocate_compiled_entry(in_gc_p) do				\
{									\
  Old = (OBJECT_ADDRESS (Temp));					\
  if (Old >= Low_Constant)						\
    continue;								\
  Compiled_BH (in_gc_p, continue);					\
  {									\
    SCHEME_OBJECT *Saved_Old = Old;					\
									\
    New_Address = (MAKE_BROKEN_HEART (To_Address));			\
    copy_vector (NULL);							\
    * Saved_Old = New_Address;						\
    Temp = (RELOCATE_COMPILED (Temp,					\
			       (OBJECT_ADDRESS (New_Address)),		\
			       Saved_Old));				\
    continue;								\
  }									\
} while (0)

#define relocate_linked_operator(in_gc_p)				\
{									\
  Scan = ((SCHEME_OBJECT *) (word_ptr));				\
  BCH_EXTRACT_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);			\
  relocate_compiled_entry (in_gc_p);					\
  BCH_STORE_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);			\
}

#define relocate_manifest_closure(in_gc_p)				\
{									\
  Scan = ((SCHEME_OBJECT *) (word_ptr));				\
  BCH_EXTRACT_CLOSURE_ENTRY_ADDRESS (Temp, Scan);			\
  relocate_compiled_entry (in_gc_p);					\
  BCH_STORE_CLOSURE_ENTRY_ADDRESS (Temp, Scan);				\
}

#endif /* _BCHGCC_H_INCLUDED */
