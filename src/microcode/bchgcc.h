/* -*-C-*-

$Id: bchgcc.h,v 9.67 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

#ifndef SCM_BCHGCC_H
#define SCM_BCHGCC_H

#include "config.h"
#include "gccode.h"

#ifdef HAVE_SYS_FILE_H
#  include <sys/file.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif

#ifdef __WIN32__
#  define IO_PAGE_SIZE		4096
#endif
#ifdef __OS2__
#  define IO_PAGE_SIZE		4096
#endif
#ifndef IO_PAGE_SIZE
#  include <sys/param.h>
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

typedef ssize_t EXFUN (file_operation_t, (int, char *, int));

extern int EXFUN (retrying_file_operation,
		  (file_operation_t *,
		   int, char *, long, long, char *, char *, long *,
		   int EXFUN ((*), (char *, char *))));

extern int EXFUN (io_error_retry_p, (char *, char *));
extern int EXFUN (io_error_always_abort, (char *, char *));

extern char * EXFUN (make_gc_file_name, (CONST char *));
extern int EXFUN (allocate_gc_file, (char *));
extern void EXFUN (protect_gc_file_name, (CONST char *));

struct saved_scan_state
{
  SCHEME_OBJECT * virtual_scan_pointer;
  unsigned long scan_position;
  unsigned long scan_offset;
};

extern void EXFUN
  (save_scan_state, (struct saved_scan_state * state, SCHEME_OBJECT * scan));
extern SCHEME_OBJECT * EXFUN
  (restore_scan_state, (struct saved_scan_state * state));
extern void EXFUN
  (set_fixed_scan_area, (SCHEME_OBJECT * bottom, SCHEME_OBJECT * top));

#ifndef O_BINARY
#  define O_BINARY 0
#endif

#define GC_FILE_FLAGS		(O_RDWR | O_CREAT | O_BINARY) /* O_SYNCIO removed */
#define GC_FILE_MASK		0644	/* Everyone reads, owner writes */

/* IO_PAGE_SIZE must be a power of 2! */

#ifndef IO_PAGE_SIZE
#  ifdef DEV_BSIZE
#    define IO_PAGE_SIZE DEV_BSIZE
#  else
#    define IO_PAGE_SIZE 8192
#  endif
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

typedef enum { NORMAL_GC, PURE_COPY, CONSTANT_COPY } gc_mode_t;

extern SCHEME_OBJECT * EXFUN
  (gc_loop, (SCHEME_OBJECT *, SCHEME_OBJECT **, SCHEME_OBJECT **,
	     SCHEME_OBJECT *, gc_mode_t, int));

extern SCHEME_OBJECT
  * EXFUN (dump_and_reload_scan_buffer, (SCHEME_OBJECT *, Boolean *)),
  * EXFUN (dump_and_reset_free_buffer, (SCHEME_OBJECT *, Boolean *)),
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
  EXFUN (initialize_weak_pair_transport, (SCHEME_OBJECT *)),
  EXFUN (fix_weak_chain_1, (SCHEME_OBJECT *)),
  EXFUN (fix_weak_chain_2, (void)),
  EXFUN (GC_end_root_relocation, (SCHEME_OBJECT *, SCHEME_OBJECT *));

extern long
  EXFUN (GC_relocate_root, (SCHEME_OBJECT **));

extern char
  * EXFUN (end_scan_buffer_extension, (char *));

extern int
  EXFUN (swap_gc_file, (int));

extern Boolean EXFUN (update_allocator_parameters, (SCHEME_OBJECT *));
extern void EXFUN (reset_allocator_parameters, (void));

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

#ifdef FLOATING_ALIGNMENT

#define BCH_ALIGN_FLOAT(address, pointer)				\
{									\
  while (!FLOATING_ALIGNED_P (address))					\
    {									\
      (address) += 1;							\
      (* ((pointer)++)) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));	\
    }									\
}

#define BCH_ALIGN_FLOAT_ADDRESS(address)				\
{									\
  while (!FLOATING_ALIGNED_P (address))					\
    (address) += 1;							\
}

#else
#define BCH_ALIGN_FLOAT(address, pointer)
#define BCH_ALIGN_FLOAT_ADDRESS(address)
#endif

#endif /* SCM_BCHGCC_H */
