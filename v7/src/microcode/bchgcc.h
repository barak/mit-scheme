/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchgcc.h,v 9.38 1991/09/07 01:06:20 jinx Exp $

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

#include "oscond.h"
#include "gccode.h"
#ifdef _BSD
#include <sys/file.h>
#else
#include <fcntl.h>
#endif

/* This should be fixed.
   We need to change the definition of INITIAL_ALIGN_HEAP, and some
   uses.
 */

/* All of these are in objects (SCHEME_OBJECT), not bytes. */

#define GC_DISK_BUFFER_SIZE		16384			/* Used to be 1024 */
#define GC_EXTRA_BUFFER_SIZE		GC_DISK_BUFFER_SIZE	/* Complete next bufferfull */
#define GC_BUFFER_SPACE			(GC_DISK_BUFFER_SIZE + GC_EXTRA_BUFFER_SIZE)
#define GC_BUFFER_BYTES			(GC_DISK_BUFFER_SIZE * sizeof(SCHEME_OBJECT))
#define GC_BUFFER_OVERLAP_BYTES		(GC_EXTRA_BUFFER_SIZE * sizeof(SCHEME_OBJECT))
#define GC_BUFFER_REMAINDER_BYTES	(GC_BUFFER_BYTES - GC_BUFFER_OVERLAP_BYTES)
#define GC_FUDGE_SIZE			GC_EXTRA_BUFFER_SIZE

#define GC_BUFFER_BLOCK(size)						\
  (GC_DISK_BUFFER_SIZE							\
   * (((size) + (GC_DISK_BUFFER_SIZE - 1)) / GC_DISK_BUFFER_SIZE))

/* These assume that GC_BUFFER_BYTES is a power of 2! */

#define ALIGN_DOWN_TO_GC_BUFFER(addr)					\
  (((unsigned long) (addr)) & (~(GC_BUFFER_BYTES - 1)))

#define ALIGN_UP_TO_GC_BUFFER(addr)					\
  (ALIGN_DOWN_TO_GC_BUFFER (((unsigned long) (addr)) + (GC_BUFFER_BYTES - 1)))

#define ALIGNED_TO_GC_BUFFER_P(addr)					\
  (((unsigned long) (addr)) == (ALIGN_DOWN_TO_GC_BUFFER (addr)))

#define GC_FILE_FLAGS		(O_RDWR | O_CREAT) /* O_SYNCIO removed */
#define GC_FILE_MASK		0644	/* Everyone reads, owner writes */
#define GC_DEFAULT_FILE_NAME	"/tmp/GCXXXXXX"

extern char
  gc_death_message_buffer[];

extern int
  gc_file;

extern SCHEME_OBJECT
  *scan_buffer_top,
  *scan_buffer_bottom,
  *free_buffer_top,
  *free_buffer_bottom;

extern SCHEME_OBJECT
  * EXFUN (GCLoop, (SCHEME_OBJECT *, SCHEME_OBJECT **, SCHEME_OBJECT **)),
  * EXFUN (dump_and_reload_scan_buffer, (long, Boolean *)),
  * EXFUN (dump_and_reset_free_buffer, (long, Boolean *)),
  * EXFUN (initialize_free_buffer, (void)),
  * EXFUN (initialize_scan_buffer, (void));

extern void
  EXFUN (GC, (SCHEME_OBJECT)),
  EXFUN (end_transport, (Boolean *)),
  EXFUN (dump_free_directly, (SCHEME_OBJECT *, long, Boolean *)),
  EXFUN (load_buffer, (long, SCHEME_OBJECT *, long, char *)),
  EXFUN (extend_scan_buffer, (char *, SCHEME_OBJECT *)),
  EXFUN (gc_death, (long, char *, SCHEME_OBJECT *, SCHEME_OBJECT *));

extern char
  * EXFUN (end_scan_buffer_extension, (char *));

/* Some utility macros */

#define copy_cell()							\
{									\
  *To++ = *Old;								\
}

#define copy_pair()							\
{									\
  *To++ = *Old++;							\
  *To++ = *Old;								\
}

#define copy_weak_pair()						\
{									\
  long Car_Type;							\
									\
  Car_Type = OBJECT_TYPE (*Old);					\
  *To++ = (OBJECT_NEW_TYPE (TC_NULL, *Old));				\
  Old += 1;								\
  *To++ = *Old;								\
  *Old = (OBJECT_NEW_TYPE (Car_Type, Weak_Chain));			\
  Weak_Chain = Temp;							\
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
  SCHEME_OBJECT *Saved_Scan = Scan;					\
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
    real_length = (overflow / GC_DISK_BUFFER_SIZE);			\
    if (real_length > 0)						\
      dump_free_directly (Old, real_length, success);			\
    Old += (real_length * GC_DISK_BUFFER_SIZE);				\
    Scan = To + (overflow % GC_DISK_BUFFER_SIZE);			\
  }									\
  while (To != Scan)							\
    *To++ = *Old++;							\
  Scan = Saved_Scan;							\
}

/* Utility macros. */

#define relocate_normal_setup()						\
{									\
  Old = OBJECT_ADDRESS (Temp);						\
  if (Old >= Low_Constant)						\
    continue;								\
  if (OBJECT_TYPE (*Old) == TC_BROKEN_HEART)				\
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
  {									\
    To = dump_and_reset_free_buffer((To - free_buffer_top), NULL);	\
  }									\
}

#define relocate_normal_end()						\
{									\
  *OBJECT_ADDRESS (Temp) = New_Address;					\
  *Scan = (MAKE_OBJECT_FROM_OBJECTS (Temp, New_Address));		\
  continue;								\
}

#define relocate_normal_pointer(copy_code, length)			\
{									\
  relocate_normal_setup();						\
  relocate_normal_transport(copy_code, length);				\
  relocate_normal_end();						\
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
  relocate_normal_setup();						\
  FLOAT_ALIGN_FREE(To_Address, To);					\
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
  if (OBJECT_TYPE (*Old) == TC_BROKEN_HEART)				\
  {									\
    *Scan = ((SCHEME_OBJECT) OBJECT_ADDRESS (*Old));			\
    continue;								\
  }									\
  New_Address = ((SCHEME_OBJECT) To_Address);				\
}

#define relocate_typeless_transport(copy_code, length)			\
{									\
  relocate_normal_transport(copy_code, length);				\
}

#define relocate_typeless_end()						\
{									\
  (* ((SCHEME_OBJECT *) Temp)) = (MAKE_BROKEN_HEART (New_Address));	\
  *Scan = New_Address;							\
  continue;								\
}

#define relocate_typeless_pointer(copy_code, length)			\
{									\
  relocate_typeless_setup();						\
  relocate_typeless_transport(copy_code, length);			\
  relocate_typeless_end();						\
}

#define relocate_compiled_entry(in_gc_p)				\
do {									\
  Old = OBJECT_ADDRESS (Temp);						\
  if (Old >= Low_Constant)						\
    continue;								\
  Compiled_BH(in_gc_p, continue);					\
  {									\
    SCHEME_OBJECT *Saved_Old = Old;					\
									\
    New_Address = (MAKE_BROKEN_HEART (To_Address));			\
    copy_vector(NULL);							\
    *Saved_Old = New_Address;						\
    Temp = RELOCATE_COMPILED(Temp,					\
			     OBJECT_ADDRESS (New_Address),		\
			     Saved_Old);				\
    continue;								\
  }									\
} while (0)

#define relocate_linked_operator(in_gc_p)				\
{									\
  Scan = ((SCHEME_OBJECT *) (word_ptr));				\
  EXTRACT_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);			\
  relocate_compiled_entry(in_gc_p);					\
  STORE_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);				\
}

#define relocate_manifest_closure(in_gc_p)				\
{									\
  Scan = ((SCHEME_OBJECT *) (word_ptr));				\
  EXTRACT_CLOSURE_ENTRY_ADDRESS (Temp, Scan);				\
  relocate_compiled_entry(in_gc_p);					\
  STORE_CLOSURE_ENTRY_ADDRESS (Temp, Scan);				\
}
