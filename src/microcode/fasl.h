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

/* Contains information relating to the format of FASL files.
   The machine/opsys information is contained in config.h
   The processor and compiled code version information is
   contained in the appropriate cmp* file, or compiler.c.  */

#ifndef SCM_FASL_H
#define SCM_FASL_H 1

#include "object.h"
#include "cmpint.h"

#if (SIZEOF_UNSIGNED_LONG == 4)
#  define FASL_FILE_MARKER 0xFAFAFAFAUL
#else
#  if (SIZEOF_UNSIGNED_LONG == 8)
#    define FASL_FILE_MARKER 0xFAFAFAFAFAFAFAFAUL
#  endif
#endif

/* The FASL file has a header which begins as follows: */

#define FASL_HEADER_LENGTH	50	/* Scheme objects in header */

#define FASL_OFFSET_MARKER	0	/* Marker to indicate FASL format */
#define FASL_OFFSET_HEAP_SIZE	1	/* # of words in heap */
#define FASL_OFFSET_HEAP_BASE	2	/* Address of heap when dumped */
#define FASL_OFFSET_DUMPED_OBJ	3	/* Where dumped object was */
#define FASL_OFFSET_CONST_SIZE	4	/* # of words in constant area */
#define FASL_OFFSET_CONST_BASE	5	/* Address of const. area at dump */
#define FASL_OFFSET_VERSION	6	/* FASL format version info. */
#define FASL_OFFSET_STACK_START	7	/* value of stack_start when dumped */
#define FASL_OFFSET_PRIM_LENGTH 8	/* # of entries in primitive table */
#define FASL_OFFSET_PRIM_SIZE	9	/* # of words in primitive table */
#define FASL_OFFSET_CI_VERSION	10	/* Version of comp. code interface */
#define FASL_OFFSET_UT_BASE	11	/* Address of the utilities vector */

#if 0
#define FASL_OFFSET_CHECK_SUM	12	/* Header and data checksum. */
#endif

#define FASL_OFFSET_C_LENGTH	13	/* # of entries in the C code table */
#define FASL_OFFSET_C_SIZE	14	/* # of words in the C code table */
#define FASL_OFFSET_MEM_BASE	15	/* Saved value of memory_base */
#define FASL_OFFSET_STACK_SIZE	16	/* # of words in stack area */
#define FASL_OFFSET_HEAP_RSVD   17	/* value of heap_reserved */

/* Version information encoding */

#define FASL_ARCH_LENGTH (OBJECT_LENGTH / 2)
#define FASL_ARCH(P) ((P) & ((1UL << FASL_ARCH_LENGTH) - 1))

#define FASL_VERSION_LENGTH (FASL_ARCH_LENGTH - TYPE_CODE_LENGTH)
#define FASL_VERSION(P)						\
  (((P) >> FASL_ARCH_LENGTH) & ((1UL << FASL_VERSION_LENGTH) - 1))

/* The '1' here is for upwards compatibility.  */
#define MAKE_FASL_VERSION(s, a)					\
  (MAKE_OBJECT (1, ((((unsigned long) (s)) << FASL_ARCH_LENGTH) | (a))))

#define CI_VERSION(P) (((P) >> HALF_DATUM_LENGTH) & HALF_DATUM_MASK)
#define CI_PROCESSOR(P) ((cc_arch_t) ((P) & HALF_DATUM_MASK))
#define CI_BAND_P(P) ((OBJECT_TYPE (P)) == TC_CONSTANT)

#define MAKE_CI_VERSION(b, v, a)					\
  (MAKE_OBJECT (((b) ? TC_CONSTANT : TC_NULL),				\
		((((unsigned long) (v)) << HALF_DATUM_LENGTH)		\
		 | ((unsigned long) (a)))))

typedef enum
{
  FASL_VERSION_NONE,
  FASL_VERSION_LONG_HEADER = 3,
  FASL_VERSION_DENSE_TYPES,
  FASL_VERSION_PADDED_STRINGS,
  FASL_VERSION_REFERENCE_TRAP,
  FASL_VERSION_MERGED_PRIMITIVES,
  FASL_VERSION_INTERFACE_VERSION,
  FASL_VERSION_NEW_BIGNUMS,
  FASL_VERSION_C_CODE,
  FASL_VERSION_STACK_END
} fasl_version_t;

#define OLDEST_INPUT_FASL_VERSION FASL_VERSION_C_CODE
#define NEWEST_INPUT_FASL_VERSION FASL_VERSION_STACK_END

#if 0
/* Temporarily disabled for testing.  */
#define OUTPUT_FASL_VERSION FASL_VERSION_STACK_END
#else
#define OUTPUT_FASL_VERSION FASL_VERSION_C_CODE
#endif

typedef struct
{
  fasl_version_t version;
  fasl_arch_t arch;
  unsigned int cc_version;
  cc_arch_t cc_arch;
  bool band_p;
  SCHEME_OBJECT * memory_base;
  SCHEME_OBJECT * root_pointer;
  SCHEME_OBJECT * heap_start;
  SCHEME_OBJECT * heap_end;
  unsigned long heap_reserved;
  SCHEME_OBJECT * constant_start;
  SCHEME_OBJECT * constant_end;
  SCHEME_OBJECT * stack_start;
  SCHEME_OBJECT * stack_end;
  unsigned long n_primitives;
  unsigned long primitive_table_size;
  unsigned long n_c_code_blocks;
  unsigned long c_code_table_size;
  SCHEME_OBJECT utilities_vector;
  SCHEME_OBJECT * utilities_start;
  SCHEME_OBJECT * utilities_end;
} fasl_header_t;

#define FASLHDR_VERSION(h) ((h)->version)
#define FASLHDR_ARCH(h) ((h)->arch)
#define FASLHDR_CC_VERSION(h) ((h)->cc_version)
#define FASLHDR_CC_ARCH(h) ((h)->cc_arch)
#define FASLHDR_BAND_P(h) ((h)->band_p)
#define FASLHDR_MEMORY_BASE(h) ((h)->memory_base)
#define FASLHDR_ROOT_POINTER(h) ((h)->root_pointer)
#define FASLHDR_HEAP_START(h) ((h)->heap_start)
#define FASLHDR_HEAP_END(h) ((h)->heap_end)
#define FASLHDR_HEAP_RESERVED(h) ((h)->heap_reserved)
#define FASLHDR_CONSTANT_START(h) ((h)->constant_start)
#define FASLHDR_CONSTANT_END(h) ((h)->constant_end)
#define FASLHDR_STACK_START(h) ((h)->stack_start)
#define FASLHDR_STACK_END(h) ((h)->stack_end)
#define FASLHDR_N_PRIMITIVES(h) ((h)->n_primitives)
#define FASLHDR_PRIMITIVE_TABLE_SIZE(h) ((h)->primitive_table_size)
#define FASLHDR_N_C_CODE_BLOCKS(h) ((h)->n_c_code_blocks)
#define FASLHDR_C_CODE_TABLE_SIZE(h) ((h)->c_code_table_size)
#define FASLHDR_UTILITIES_VECTOR(h) ((h)->utilities_vector)
#define FASLHDR_UTILITIES_START(h) ((h)->utilities_start)
#define __FASLHDR_UTILITIES_END(h) ((h)->utilities_end)

#define FASLHDR_UTILITIES_END(h) (faslhdr_utilities_end (h))

#define FASLHDR_HEAP_SIZE(h)						\
  ((unsigned long)							\
   ((FASLHDR_HEAP_END (h)) - (FASLHDR_HEAP_START (h))))

#define FASLHDR_CONSTANT_SIZE(h)					\
  ((unsigned long)							\
   ((FASLHDR_CONSTANT_END (h)) - (FASLHDR_CONSTANT_START (h))))

#define FASLHDR_STACK_SIZE(h)						\
  ((unsigned long)							\
   ((FASLHDR_STACK_END (h)) - (FASLHDR_STACK_START (h))))

typedef enum
{
  FASL_FILE_FINE,
  FASL_FILE_TOO_SHORT,
  FASL_FILE_NOT_FASL,
  FASL_FILE_BAD_MACHINE,
  FASL_FILE_BAD_VERSION,
  FASL_FILE_BAD_SUBVERSION,	/* unused */
  FASL_FILE_BAD_PROCESSOR,
  FASL_FILE_BAD_INTERFACE
} fasl_read_status_t;

typedef FILE * fasl_file_handle_t;

extern bool open_fasl_output_file (const char *, fasl_file_handle_t *);
extern bool close_fasl_output_file (fasl_file_handle_t);
extern bool write_fasl_header (fasl_header_t *, fasl_file_handle_t);
extern bool write_to_fasl_file (const void *, size_t, fasl_file_handle_t);
extern bool open_fasl_input_file (const char *, fasl_file_handle_t *);
extern bool close_fasl_input_file (fasl_file_handle_t);
extern bool read_fasl_header (fasl_header_t *, fasl_file_handle_t);
extern bool read_from_fasl_file (void *, size_t, fasl_file_handle_t);
extern SCHEME_OBJECT * fasl_object_address (SCHEME_OBJECT, fasl_header_t *);
extern insn_t * fasl_cc_address (SCHEME_OBJECT, fasl_header_t *);
extern SCHEME_OBJECT fasl_raw_address_to_object
  (unsigned int, SCHEME_OBJECT *, fasl_header_t *);
extern SCHEME_OBJECT fasl_raw_address_to_cc_entry (insn_t *, fasl_header_t *);
extern SCHEME_OBJECT * faslhdr_utilities_end (fasl_header_t *);
extern fasl_read_status_t check_fasl_version (fasl_header_t *);
extern fasl_read_status_t check_fasl_cc_version
  (fasl_header_t *, unsigned long, unsigned long);

#endif /* not SCM_FASL_H */
