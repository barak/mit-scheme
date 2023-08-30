/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

/* Description of the user data objects.  This should parallel the
   file SDATA.SCM in the runtime system.  */

#ifndef SCM_SDATA_H
#define SCM_SDATA_H 1

#include "object.h"
#include "extern.h"
#include "stack.h"
#include "trap.h"

// Vectors

#define vector_header memory_ref_0
#define set_vector_header memory_set_0
#define vector_ref_0 memory_ref_1
#define vector_set_0 memory_set_1
#define vector_loc_0 memory_loc_1
#define vector_ref_1 memory_ref_2
#define vector_set_1 memory_set_2
#define vector_loc_1 memory_loc_2
#define vector_ref_2 memory_ref_3
#define vector_set_2 memory_set_3
#define vector_loc_2 memory_loc_3

static inline SCHEME_OBJECT
make_vector_header (unsigned long length)
{
  return make_object (TC_MANIFEST_VECTOR, length);
}

static inline SCHEME_OBJECT
vector_length (SCHEME_OBJECT v)
{
  return object_datum (vector_header (v));
}

static inline void
set_vector_length (SCHEME_OBJECT v, unsigned long new_length)
{
  set_vector_header (v, object_new_datum (vector_header (v), new_length));
}

static inline SCHEME_OBJECT
vector_ref (SCHEME_OBJECT v, unsigned long index)
{
  return memory_ref (v, index + 1);
}

static inline void
vector_set (SCHEME_OBJECT v, unsigned long index, SCHEME_OBJECT object)
{
  memory_set (v, index + 1, object);
}

static inline SCHEME_OBJECT*
vector_loc (SCHEME_OBJECT v, unsigned long index)
{
  return memory_loc (v, index + 1);
}

// Non-marked vectors

static inline SCHEME_OBJECT
make_nmv_header (unsigned long length)
{
  return make_object (TC_MANIFEST_NM_VECTOR, length);
}

#define nm_vector_subheader memory_ref_1
#define set_nm_vector_subheader memory_set_1
#define nm_vector_data memory_loc_2

static inline unsigned long
nm_vector_data_length (SCHEME_OBJECT nmv)
{
  return vector_length (nmv) - 1;
}

static inline void
set_nm_vector_data_length (SCHEME_OBJECT nmv, unsigned long nwords)
{
  set_vector_length (nmv, make_nmv_header (nwords + 1));
}

// Bytevectors

static inline uint8_t*
bytevector_data (SCHEME_OBJECT bv)
{
  return (uint8_t*) nm_vector_data (bv);
}

static inline uint8_t*
bytevector_loc (SCHEME_OBJECT bv, unsigned long index)
{
  return bytevector_data (bv) + index;
}

static inline uint8_t
bytevector_ref (SCHEME_OBJECT bv, unsigned long index)
{
  return *bytevector_loc (bv, index);
}

static inline void
bytevector_set (SCHEME_OBJECT bv, unsigned long index, uint8_t b)
{
  *bytevector_loc (bv, index) = b;
}

static inline unsigned long
bytevector_length_to_gc_length (unsigned long nbytes)
{
  // Add 1 for subheader
  return 1 + BYTES_TO_WORDS (nbytes + 1);
}

static inline SCHEME_OBJECT
make_bytevector_subheader (unsigned long nbytes)
{
  return make_object (0, nbytes);
}

static inline unsigned long
bytevector_length (SCHEME_OBJECT bv)
{
  return object_datum (nm_vector_subheader (bv));
}

static inline void
set_bytevector_length (SCHEME_OBJECT bv, unsigned long nbytes)
{
  set_nm_vector_subheader (bv, make_bytevector_subheader (nbytes));
  bytevector_set (bv, nbytes, 0x00);
}

// Unicode strings

#define USTRING_FLAG_NFC (0x1)
#define USTRING_FLAG_NFC_SET (0x2)
#define USTRING_FLAG_NFD (0x4)
#define USTRING_FLAGS_ALL (0x7)

static inline uint8_t*
ustring_data (SCHEME_OBJECT string)
{
  return (uint8_t*) nm_vector_data (string);
}

static inline uint8_t
ustring_bytes_per_cp (SCHEME_OBJECT string)
{
  return object_type (nm_vector_subheader (string)) & 0x3;
}

static inline uint8_t
ustring_flags (SCHEME_OBJECT string)
{
  return (object_type (nm_vector_subheader (string)) >> 2) & USTRING_FLAGS_ALL;
}

static inline unsigned long
ustring_length_to_gc_length (unsigned long length, uint8_t bytes_per_cp)
{
  switch (bytes_per_cp)
    {
    case 1:
      // Add 1 word for subheader, 1 byte for trailing 0
      return 1 + BYTES_TO_WORDS (length + 1);
    case 2:
      // Add 1 word for subheader
      return 1 + BYTES_TO_WORDS (length * 2);
    default:
      // Add 1 word for subheader
      return 1 + BYTES_TO_WORDS (length * 3);
    }
}

static inline SCHEME_OBJECT
make_ustring_subheader (uint8_t bytes_per_cp, uint8_t flags, unsigned long ncps)
{
  return make_object ((flags & USTRING_FLAGS_ALL) << 2 | (bytes_per_cp & 0x3),
                      ncps);
}

static inline unsigned long
ustring_length (SCHEME_OBJECT string)
{
  return object_datum (nm_vector_subheader (string));
}

// Legacy strings

/* Legacy strings are laid out exactly the same way as bytevectors,
   except that they have a zero byte at the end that isn't included in
   the string's length. */

#define set_legacy_string_header set_vector_header

static inline char*
legacy_string_data (SCHEME_OBJECT string)
{
  return (char*) nm_vector_data (string);
}

static inline unsigned char*
legacy_string_loc (SCHEME_OBJECT string, unsigned long index)
{
  return ((unsigned char*) legacy_string_data (string)) + index;
}

static inline unsigned char
legacy_string_ref (SCHEME_OBJECT string, unsigned long index)
{
  return *legacy_string_loc (string, index);
}

static inline void
legacy_string_set (SCHEME_OBJECT string, unsigned long index, unsigned char c)
{
  *legacy_string_loc (string, index) = c;
}

static inline unsigned long
legacy_string_length_to_gc_length (unsigned long nchars)
{
  // Add 1 for subheader
  return 1 + BYTES_TO_WORDS (nchars + 1);
}

static inline SCHEME_OBJECT
make_legacy_string_subheader (unsigned long nchars)
{
  return make_object (0, nchars);
}

static inline unsigned long
legacy_string_length (SCHEME_OBJECT string)
{
  return object_datum (nm_vector_subheader (string));
}

static inline void
set_legacy_string_length (SCHEME_OBJECT string, unsigned long nchars)
{
  set_nm_vector_subheader (string, make_legacy_string_subheader (nchars));
  legacy_string_set (string, nchars, '\0');
}

static inline unsigned long
legacy_string_max_length (SCHEME_OBJECT string)
{
  return (nm_vector_data_length (string) * sizeof (SCHEME_OBJECT)) - 1;
}

static inline void
set_legacy_string_max_length (SCHEME_OBJECT string, unsigned long nchars)
{
  set_nm_vector_data_length (string,
                             legacy_string_length_to_gc_length (nchars));
}

#define cell_contents memory_ref_0
#define set_cell_contents memory_set_0

// Delayed objects

#define delayed_snapped memory_ref_0
#define delayed_value memory_ref_1
#define delayed_env memory_ref_0
#define delayed_proc memory_ref_1

static inline SCHEME_OBJECT
make_delayed (SCHEME_OBJECT proc, SCHEME_OBJECT env)
{
  SCHEME_OBJECT delayed = make_pointer_object (TC_DELAYED, Free);
  *Free++ = proc;
  *Free++ = env;
  return delayed;
}

static inline SCHEME_OBJECT
snap_delayed (SCHEME_OBJECT delayed, SCHEME_OBJECT val)
{
  // Don't snap thunk twice; evaluation of the thunk's body might have snapped
  // it already.
  if (memory_ref (delayed, 0) == SHARP_T)
    return memory_ref (delayed, 1);
  memory_set (delayed, 0, SHARP_T);
  memory_set (delayed, 1, val);
  return val;
}

// Entities

#define entity_operator memory_ref_0
#define entity_data memory_ref_1

// Procedures

static inline SCHEME_OBJECT
make_procedure (SCHEME_OBJECT lambda, SCHEME_OBJECT env)
{
  SCHEME_OBJECT proc = make_pointer_object (TC_PROCEDURE, Free);
  *Free++ = lambda;
  *Free++ = env;
  return proc;
}

static inline SCHEME_OBJECT
make_extended_procedure (SCHEME_OBJECT lambda, SCHEME_OBJECT env)
{
  SCHEME_OBJECT proc = make_pointer_object (TC_EXTENDED_PROCEDURE, Free);
  *Free++ = lambda;
  *Free++ = env;
  return proc;
}

#define proc_lambda memory_ref_0
#define proc_environment memory_ref_1

// Environments

#define env_header memory_ref_0
#define env_proc memory_ref_1
#define set_env_extension memory_set_1
#define env_vals memory_loc_2

static inline SCHEME_OBJECT
env_parent (SCHEME_OBJECT env)
{
  return proc_environment (env_proc (env));
}

static inline SCHEME_OBJECT*
env_val_cell (SCHEME_OBJECT env, unsigned long index)
{
  return env_vals (env) + index;
}

// Frame extensions

#define frame_extension_p VECTOR_P
#define frame_extension_parent vector_ref_0
#define set_frame_extension_parent vector_set_0
#define frame_extension_proc vector_ref_1
#define set_frame_extension_proc vector_set_1

#define FRAME_EXTENSION_MIN_SIZE 4

static inline unsigned long
frame_extension_length (SCHEME_OBJECT ext)
{
  return FIXNUM_TO_ULONG (vector_ref (ext, 2));
}

static inline void
set_frame_extension_length (SCHEME_OBJECT ext, unsigned long n)
{
  vector_set (ext, 2, ULONG_TO_FIXNUM (n));
}

static inline unsigned long
frame_extension_max_length (SCHEME_OBJECT ext)
{
  return vector_length (ext) - 3;
}

static inline SCHEME_OBJECT*
frame_extension_bindings (SCHEME_OBJECT ext)
{
  return vector_loc (ext, 3);
}

static inline bool
extended_frame_p (SCHEME_OBJECT frame)
{
  return frame_extension_p (env_proc (frame));
}

static inline SCHEME_OBJECT
extended_frame_proc (SCHEME_OBJECT frame)
{
  return frame_extension_proc (env_proc (frame));
}

static inline SCHEME_OBJECT*
extended_frame_bindings (SCHEME_OBJECT frame)
{
  return frame_extension_bindings (env_proc (frame));
}

static inline unsigned long
extended_frame_length (SCHEME_OBJECT frame)
{
  return frame_extension_length (env_proc (frame));
}

static inline unsigned long
extended_frame_max_length (SCHEME_OBJECT frame)
{
  return frame_extension_max_length (env_proc (frame));
}

static inline void
set_extended_frame_length (SCHEME_OBJECT frame, unsigned long n)
{
  return set_frame_extension_length (env_proc (frame), n);
}

#define hunk3_ref_0 memory_ref_0
#define hunk3_set_0 memory_set_0
#define hunk3_loc_0 memory_loc_0
#define hunk3_ref_1 memory_ref_1
#define hunk3_set_1 memory_set_1
#define hunk3_loc_1 memory_loc_1
#define hunk3_ref_2 memory_ref_2
#define hunk3_set_2 memory_set_2
#define hunk3_loc_2 memory_loc_2

// Symbols

#define symbol_name memory_ref_0
#define set_symbol_name memory_set_0
#define symbol_global_value memory_ref_1
#define set_symbol_global_value memory_set_1
#define symbol_global_value_cell memory_loc_1

// For GC:
#define SYMBOL_GLOBAL_VALUE 1

// Pairs

#define pair_car memory_ref_0
#define pair_cdr memory_ref_1
#define set_pair_car memory_set_0
#define set_pair_cdr memory_set_1
#define pair_car_loc memory_loc_0
#define pair_cdr_loc memory_loc_1

#define PAIR_CAR pair_car
#define PAIR_CDR pair_cdr
#define SET_PAIR_CAR set_pair_car
#define SET_PAIR_CDR set_pair_cdr
#define PAIR_CAR_LOC pair_car_loc
#define PAIR_CDR_LOC pair_cdr_loc

// For GC:
#define CONS_CDR 1

// Pointer reference traps

#define ptr_ref_trap_tag memory_ref_0
#define ptr_ref_trap_cache memory_ref_1
#define set_ptr_ref_trap_cache memory_set_1

#define PTR_REF_TRAP_SIZE 2

static inline SCHEME_OBJECT
make_ptr_ref_trap (unsigned long tag, SCHEME_OBJECT cache)
{
  SCHEME_OBJECT trap = make_pointer_object (TC_REFERENCE_TRAP, Free);
  *Free++ = ULONG_TO_FIXNUM (tag);
  *Free++ = cache;
  return trap;
}

// Reference-trap caches

enum cache_ref_kind
{
  LOOKUP_CACHE,
  ASSIGNMENT_CACHE,
  OPERATOR_CACHE
};

#define cache_value hunk3_ref_0
#define set_cache_value hunk3_set_0
#define cache_clone hunk3_ref_1
#define set_cache_clone hunk3_set_1
#define cache_refs hunk3_ref_2
#define set_cache_refs hunk3_set_2

#define CACHE_SIZE 3
#define CACHE_REFS_SIZE 3

static inline SCHEME_OBJECT
make_cache (SCHEME_OBJECT value, SCHEME_OBJECT clone, SCHEME_OBJECT refs)
{
  SCHEME_OBJECT cache = make_pointer_object (CACHE_TYPE, Free);
  *Free++ = value;
  *Free++ = clone;
  *Free++ = refs;
  return cache;
}

static inline bool
cache_clone_p (SCHEME_OBJECT cache)
{
  return cache_value (cache) == EXPENSIVE_OBJECT;
}

static inline SCHEME_OBJECT
make_cache_clone (SCHEME_OBJECT cache)
{
  return make_cache (EXPENSIVE_OBJECT, cache, cache_refs (cache));
}

static inline SCHEME_OBJECT
make_cache_refs (void)
{
  SCHEME_OBJECT refs = make_pointer_object (CACHE_REFERENCES_TYPE, Free);
  *Free++ = EMPTY_LIST;
  *Free++ = EMPTY_LIST;
  *Free++ = EMPTY_LIST;
  return refs;
}

static inline SCHEME_OBJECT*
cache_lookup_refs (SCHEME_OBJECT cache)
{
  return hunk3_loc_0 (cache_refs (cache));
}

static inline SCHEME_OBJECT*
cache_assignment_refs (SCHEME_OBJECT cache)
{
  return hunk3_loc_1 (cache_refs (cache));
}

static inline SCHEME_OBJECT*
cache_operator_refs (SCHEME_OBJECT cache)
{
  return hunk3_loc_2 (cache_refs (cache));
}

static inline SCHEME_OBJECT*
cache_kind_refs (SCHEME_OBJECT cache, enum cache_ref_kind kind)
{
  switch (kind)
    {
    case LOOKUP_CACHE: return cache_lookup_refs (cache);
    case ASSIGNMENT_CACHE: return cache_assignment_refs (cache);
    case OPERATOR_CACHE: return cache_operator_refs (cache);
    default: return 0;
    }
}

#define cache_ref_block pair_car
#define set_cache_ref_block set_pair_car

static inline unsigned long
cache_ref_offset (SCHEME_OBJECT ref)
{
  return object_datum (pair_cdr (ref));
}

static inline void
set_cache_ref_offset (SCHEME_OBJECT ref, unsigned long offset)
{
  set_pair_cdr (ref, ULONG_TO_FIXNUM (offset));
}

// Ephemerons

#define EPHEMERON_KEY		1
#define EPHEMERON_DATUM		2
#define EPHEMERON_LIST		3
#define EPHEMERON_NEXT		4

#define EPHEMERON_SIZE		5

#define MARKED_EPHEMERON_MANIFEST				\
  (make_vector_header ((EPHEMERON_SIZE - 1)))

#define UNMARKED_EPHEMERON_MANIFEST				\
  (make_nmv_header ((EPHEMERON_SIZE - 1)))

#endif /* not SCM_SDATA_H */
