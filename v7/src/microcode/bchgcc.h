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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchgcc.h,v 9.33 1988/08/15 20:36:07 cph Rel $ */

#include "gccode.h"
#ifdef bsd
#include <sys/file.h>
#else
#include <fcntl.h>
#endif

/* All of these are in objects (Pointer), not bytes. */

#define GC_EXTRA_BUFFER_SIZE		512
#define GC_DISK_BUFFER_SIZE		1024
#define GC_BUFFER_SPACE			(GC_DISK_BUFFER_SIZE + GC_EXTRA_BUFFER_SIZE)
#define GC_BUFFER_BYTES			(GC_DISK_BUFFER_SIZE * sizeof(Pointer))
#define GC_BUFFER_OVERLAP_BYTES		(GC_EXTRA_BUFFER_SIZE * sizeof(Pointer))
#define GC_BUFFER_REMAINDER_BYTES	(GC_BUFFER_BYTES - GC_BUFFER_OVERLAP_BYTES)			 

#define GC_FILE_FLAGS		(O_RDWR | O_CREAT) /* O_SYNCIO removed */
#define GC_FILE_MASK		0644	/* Everyone reads, owner writes */
#define GC_DEFAULT_FILE_NAME	"/tmp/GCXXXXXX"

extern Pointer *scan_buffer_top, *scan_buffer_bottom;
extern Pointer *free_buffer_top, *free_buffer_bottom;
extern Pointer *dump_and_reload_scan_buffer();
extern Pointer *dump_and_reset_free_buffer();
extern void    dump_free_directly(), load_buffer();

extern void    extend_scan_buffer();
extern char    *end_scan_buffer_extension();

extern Pointer *GCLoop();
extern Pointer *initialize_free_buffer(), *initialize_scan_buffer();
extern void    end_transport(), GC();
extern int     gc_file;

extern void gc_death();
extern char gc_death_message_buffer[];

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
  Car_Type = OBJECT_TYPE(*Old);						\
  *To++ = Make_New_Pointer(TC_NULL, *Old);				\
  Old += 1;								\
  *To++ = *Old;								\
  *Old = Make_New_Pointer(Car_Type, Weak_Chain);			\
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
  Pointer *Saved_Scan = Scan;						\
  unsigned long real_length = 1 + Get_Integer(*Old);			\
									\
  To_Address += real_length;						\
  Scan = To + real_length;						\
  if (Scan >= free_buffer_top)						\
  {									\
    unsigned long overflow;						\
									\
    overflow = Scan - free_buffer_top;					\
    while (To != free_buffer_top)					\
      *To++ = *Old++;							\
    To = dump_and_reset_free_buffer(0, success);			\
    real_length = (overflow / GC_DISK_BUFFER_SIZE);			\
    if (real_length > 0)						\
    {									\
      dump_free_directly(Old, real_length, success);			\
    }									\
    Old += (real_length * GC_DISK_BUFFER_SIZE);				\
    Scan = To + (overflow % GC_DISK_BUFFER_SIZE);			\
  }									\
  while (To != Scan)							\
  {									\
    *To++ = *Old++;							\
  }									\
  Scan = Saved_Scan;							\
}

/* Utility macros. */

#define relocate_normal_setup()						\
{									\
  Old = Get_Pointer(Temp);						\
  if (Old >= Low_Constant)						\
    continue;								\
  if (OBJECT_TYPE(*Old) == TC_BROKEN_HEART)				\
  {									\
    *Scan = Make_New_Pointer(OBJECT_TYPE(Temp), *Old);			\
    continue;								\
  }									\
  New_Address = Make_Broken_Heart(C_To_Scheme(To_Address));		\
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
  *Get_Pointer(Temp) = New_Address;					\
  *Scan = Make_New_Pointer(OBJECT_TYPE(Temp), New_Address);		\
  continue;								\
}

#define relocate_normal_pointer(copy_code, length)			\
{									\
  relocate_normal_setup();						\
  relocate_normal_transport(copy_code, length);				\
  relocate_normal_end();						\
}

/* Typeless objects (implicit types). */

#define relocate_typeless_setup()					\
{									\
  Old = ((Pointer *) Temp);						\
  if (Old >= Low_Constant)						\
    continue;								\
  if (OBJECT_TYPE(*Old) == TC_BROKEN_HEART)				\
  {									\
    *Scan = ((Pointer) Get_Pointer(*Old));				\
    continue;								\
  }									\
  New_Address = ((Pointer) To_Address);					\
}

#define relocate_typeless_transport(copy_code, length)			\
{									\
  relocate_normal_transport(copy_code, length);				\
}

#define relocate_typeless_end()						\
{									\
  *((Pointer *) Temp) = Make_Broken_Heart(C_To_Scheme(New_Address));	\
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
{									\
  Old = Get_Pointer(Temp);						\
  if (Old >= Low_Constant)						\
    continue;								\
  Compiled_BH(in_gc_p, continue);					\
  {									\
    Pointer *Saved_Old = Old;						\
									\
    New_Address = Make_Broken_Heart(C_To_Scheme(To_Address));		\
    copy_vector(NULL);							\
    *Saved_Old = New_Address;						\
    *Scan = Relocate_Compiled(Temp,					\
			      Get_Pointer(New_Address),			\
			      Saved_Old);				\
    continue;								\
  }									\
}

#define relocate_linked_operator(in_gc_p)				\
{									\
  Scan = OPERATOR_LINKAGE_ENTRY_ADDRESS(word_ptr);			\
  Temp = *Scan;								\
  relocate_compiled_entry(in_gc_p);					\
}

#define relocate_manifest_closure(in_gc_p)				\
{									\
  Scan = MANIFEST_CLOSURE_ENTRY_ADDRESS(word_ptr);			\
  Temp = *Scan;								\
  relocate_compiled_entry(in_gc_p);					\
}

#define ONCE_ONLY(stmt)							\
do									\
{									\
  stmt;									\
} while (false)
