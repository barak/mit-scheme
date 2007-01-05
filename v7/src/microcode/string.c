/* -*-C-*-

$Id: string.c,v 9.51 2007/01/05 21:19:25 cph Exp $

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

/* String primitives. */

#include "scheme.h"
#include "prims.h"

#ifndef STDC_HEADERS
#  ifdef HAVE_MALLOC_H
#    include <malloc.h>
#  else
     extern PTR EXFUN (malloc, (size_t));
     extern PTR EXFUN (realloc, (PTR, size_t));
#  endif
#endif

SCHEME_OBJECT
DEFUN (allocate_string, (nbytes), unsigned long nbytes)
{
  SCHEME_OBJECT result
    = (allocate_non_marked_vector
       (TC_CHARACTER_STRING,
	(STRING_LENGTH_TO_GC_LENGTH (nbytes)),
	1));
  SET_STRING_LENGTH (result, nbytes);
  return (result);
}

SCHEME_OBJECT
DEFUN (allocate_string_no_gc, (nbytes), unsigned long nbytes)
{
  SCHEME_OBJECT result
    = (allocate_non_marked_vector
       (TC_CHARACTER_STRING,
	(STRING_LENGTH_TO_GC_LENGTH (nbytes)),
	0));
  SET_STRING_LENGTH (result, nbytes);
  return (result);
}

SCHEME_OBJECT
DEFUN (memory_to_string, (nbytes, data),
       unsigned long nbytes AND
       CONST unsigned char * data)
{
  SCHEME_OBJECT result = (allocate_string (nbytes));
  unsigned char * scan_result = (STRING_LOC (result, 0));
  unsigned char * end_result = (scan_result + nbytes);
  while (scan_result < end_result)
    (*scan_result++) = (*data++);
  return (result);
}

SCHEME_OBJECT
DEFUN (memory_to_string_no_gc, (nbytes, data),
       unsigned long nbytes AND
       CONST unsigned char * data)
{
  SCHEME_OBJECT result = (allocate_string_no_gc (nbytes));
  unsigned char * scan_result = (STRING_LOC (result, 0));
  unsigned char * end_result = (scan_result + nbytes);
  while (scan_result < end_result)
    (*scan_result++) = (*data++);
  return (result);
}

SCHEME_OBJECT
DEFUN (char_pointer_to_string, (char_pointer),
       CONST unsigned char * char_pointer)
{
  CONST unsigned char * scan = char_pointer;
  if (scan == 0)
    scan += 1;
  else
    while ((*scan++) != '\0')
      ;
  return (memory_to_string (((scan - 1) - char_pointer), char_pointer));
}

SCHEME_OBJECT
DEFUN (char_pointer_to_string_no_gc, (char_pointer),
       CONST unsigned char * char_pointer)
{
  CONST unsigned char * scan = char_pointer;
  if (scan == 0)
    scan += 1;
  else
    while ((*scan++) != '\0')
      ;
  return (memory_to_string_no_gc (((scan - 1) - char_pointer), char_pointer));
}

/* Currently the strings used in symbols have type codes in the length
   field.  They should be changed to have just longwords there. */

DEFINE_PRIMITIVE ("STRING-ALLOCATE", Prim_string_allocate, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (allocate_string (arg_nonnegative_integer (1)));
}

DEFINE_PRIMITIVE ("STRING?", Prim_string_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (STRING_P (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("STRING-LENGTH", Prim_string_length, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (STRING_LENGTH (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("STRING-MAXIMUM-LENGTH", Prim_string_maximum_length, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM (MAXIMUM_STRING_LENGTH (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("SET-STRING-LENGTH!", Prim_set_string_length, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    SET_STRING_LENGTH
      (string,
       (arg_index_integer (2, ((MAXIMUM_STRING_LENGTH (string)) + 1))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SET-STRING-MAXIMUM-LENGTH!", Prim_set_string_maximum_length, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    long length
      = (arg_index_integer (2, ((MAXIMUM_STRING_LENGTH (string)) + 1)));
    if (length < (STRING_LENGTH (string)))
      SET_STRING_LENGTH (string, length);
    MEMORY_SET
      (string,
       STRING_HEADER,
       (MAKE_OBJECT
	(TC_MANIFEST_NM_VECTOR, ((BYTES_TO_WORDS (length + 1)) + 1))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define STRING_REF_BODY(process_result)					\
{									\
  PRIMITIVE_HEADER (2);							\
  CHECK_ARG (1, STRING_P);						\
  {									\
    SCHEME_OBJECT string = (ARG_REF (1));				\
    PRIMITIVE_RETURN							\
      (process_result							\
       (STRING_REF							\
	(string, (arg_index_integer (2, (STRING_LENGTH (string)))))));	\
  }									\
}

DEFINE_PRIMITIVE ("STRING-REF", Prim_string_ref, 2, 2, 0)
     STRING_REF_BODY (ASCII_TO_CHAR)

DEFINE_PRIMITIVE ("VECTOR-8B-REF", Prim_vec_8b_ref, 2, 2, 0)
     STRING_REF_BODY (LONG_TO_UNSIGNED_FIXNUM)

#define STRING_SET_BODY(get_ascii)					\
{									\
  PRIMITIVE_HEADER (3);							\
  CHECK_ARG (1, STRING_P);						\
  {									\
    SCHEME_OBJECT string = (ARG_REF (1));				\
    STRING_SET								\
      (string,								\
       (arg_index_integer (2, (STRING_LENGTH (string)))),		\
       ((unsigned char) (get_ascii (3))));				\
  }									\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

DEFINE_PRIMITIVE ("STRING-SET!", Prim_string_set, 3, 3, 0)
     STRING_SET_BODY (arg_ascii_char)

DEFINE_PRIMITIVE ("VECTOR-8B-SET!", Prim_vec_8b_set, 3, 3, 0)
     STRING_SET_BODY (arg_ascii_integer)

#define SUBSTRING_MOVE_PREFIX()						\
  unsigned char *ptr1, *ptr2;						\
  unsigned long len1, len2;						\
  unsigned long start1, end1, start2, end2, length;			\
  unsigned char *scan1, *scan2, *limit;					\
  PRIMITIVE_HEADER (5);							\
  ptr1 = (arg_extended_string (1, (&len1)));				\
  end1 = (arg_ulong_index_integer (3, (len1 + 1)));			\
  start1 = (arg_ulong_index_integer (2, (end1 + 1)));			\
  ptr2 = (arg_extended_string (4, (&len2)));				\
  start2 = (arg_ulong_index_integer (5, (len2 + 1)));			\
  length = (end1 - start1);						\
  end2 = (start2 + length);						\
  if (end2 > len2)							\
    error_bad_range_arg (5)

DEFINE_PRIMITIVE ("SUBSTRING-MOVE-RIGHT!", Prim_substring_move_right, 5, 5, 0)
{
  SUBSTRING_MOVE_PREFIX ();
  scan1 = (ptr1 + end1);
  scan2 = (ptr2 + end2);
  limit = (scan1 - length);
  while (scan1 > limit)
    (*--scan2) = (*--scan1);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SUBSTRING-MOVE-LEFT!", Prim_substring_move_left, 5, 5, 0)
{
  SUBSTRING_MOVE_PREFIX ();
  scan1 = (ptr1 + start1);
  scan2 = (ptr2 + start2);
  limit = (scan1 + length);
  while (scan1 < limit)
    (*scan2++) = (*scan1++);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define SUBSTRING_MODIFIER(char_map)					\
{									\
  SCHEME_OBJECT string;							\
  long start, end;							\
  long length;								\
  unsigned char *scan, temp;						\
  PRIMITIVE_HEADER (3);							\
  CHECK_ARG (1, STRING_P);						\
  string = (ARG_REF (1));						\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  if (end > (STRING_LENGTH (string)))					\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  length = (end - start);						\
  scan = (STRING_LOC (string, start));					\
  while ((length--) > 0)						\
    {									\
      temp = (*scan);							\
      (*scan++) = ((unsigned char) (char_map (temp)));			\
    }									\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

DEFINE_PRIMITIVE ("SUBSTRING-UPCASE!", Prim_substring_upcase, 3, 3, 0)
     SUBSTRING_MODIFIER (char_upcase)

DEFINE_PRIMITIVE ("SUBSTRING-DOWNCASE!", Prim_substring_downcase, 3, 3, 0)
     SUBSTRING_MODIFIER (char_downcase)

#define VECTOR_8B_SUBSTRING_PREFIX()					\
  long start, end, ascii;						\
  unsigned char *string_start, *scan, *limit;				\
  PRIMITIVE_HEADER (4);							\
  CHECK_ARG (1, STRING_P);						\
  string_start = (STRING_LOC ((ARG_REF (1)), 0));			\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  ascii = (arg_ascii_integer (4));					\
  if (end > (STRING_LENGTH (ARG_REF (1))))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2)

#define VECTOR_8B_SUBSTRING_PREFIX_FORWARD()				\
  VECTOR_8B_SUBSTRING_PREFIX ();					\
  scan = (string_start + start);					\
  limit = (string_start + end);

#define VECTOR_8B_SUBSTRING_PREFIX_BACKWARD()				\
  VECTOR_8B_SUBSTRING_PREFIX ();					\
  scan = (string_start + end);						\
  limit = (string_start + start);

DEFINE_PRIMITIVE ("VECTOR-8B-FILL!", Prim_vec_8b_fill, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_FORWARD ();
  while (scan < limit)
    (*scan++) = ((unsigned char) ascii);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-NEXT-CHAR", Prim_vec_8b_find_next_char, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_FORWARD ();
  while (scan < limit)
    if ((*scan++) == ascii)
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan - 1) - string_start));
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-PREVIOUS-CHAR", Prim_vec_8b_find_prev_char, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_BACKWARD ();
  while (scan > limit)
    if ((*--scan) == ascii)
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan - string_start));
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-NEXT-CHAR-CI", Prim_vec_8b_find_next_char_ci, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_FORWARD ();
  {
    unsigned char char1 = ((unsigned char) (char_upcase (ascii)));
    while (scan < limit)
      if ((char_upcase (*scan++)) == char1)
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan - 1) - string_start));
  }
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("VECTOR-8B-FIND-PREVIOUS-CHAR-CI", Prim_vec_8b_find_prev_char_ci, 4, 4, 0)
{
  VECTOR_8B_SUBSTRING_PREFIX_BACKWARD ();
  {
    unsigned char char1 = ((unsigned char) (char_upcase (ascii)));
    while (scan > limit)
      if ((char_upcase (*--scan)) == char1)
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan - string_start));
  }
  PRIMITIVE_RETURN (SHARP_F);
}

#define SUBSTR_FIND_CHAR_IN_SET_PREFIX()				\
  long start, end;							\
  unsigned char *char_set, *string_start, *scan, *limit;		\
  PRIMITIVE_HEADER (4);							\
  CHECK_ARG (1, STRING_P);						\
  string_start = (STRING_LOC ((ARG_REF (1)), 0));			\
  start = (arg_nonnegative_integer (2));				\
  end = (arg_nonnegative_integer (3));					\
  CHECK_ARG (4, STRING_P);						\
  char_set = (STRING_LOC ((ARG_REF (4)), 0));				\
  if (end > (STRING_LENGTH (ARG_REF (1))))				\
    error_bad_range_arg (3);						\
  if (start > end)							\
    error_bad_range_arg (2);						\
  if ((STRING_LENGTH (ARG_REF (4))) != MAX_ASCII)			\
    error_bad_range_arg (4)

DEFINE_PRIMITIVE ("SUBSTRING-FIND-NEXT-CHAR-IN-SET", Prim_find_next_char_in_set, 4, 4, 0)
{
  SUBSTR_FIND_CHAR_IN_SET_PREFIX ();
  scan = (string_start + start);
  limit = (string_start + end);
  while (scan < limit)
    if ((char_set [*scan++]) != '\0')
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan - 1) - string_start));
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET", Prim_find_prev_char_in_set, 4, 4, 0)
{
  SUBSTR_FIND_CHAR_IN_SET_PREFIX ();
  scan = (string_start + end);
  limit = (string_start + start);
  while (scan > limit)
    if ((char_set [*--scan]) != '\0')
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan - string_start));
  PRIMITIVE_RETURN (SHARP_F);
}

#define SUBSTRING_COMPARE_PREFIX()				\
  long start1, end1, start2, end2;				\
  unsigned char *string1_start, *string2_start;			\
  PRIMITIVE_HEADER (6);						\
  CHECK_ARG (1, STRING_P);					\
  string1_start = (STRING_LOC ((ARG_REF (1)), 0));		\
  start1 = (arg_nonnegative_integer (2));			\
  end1 = (arg_nonnegative_integer (3));				\
  CHECK_ARG (4, STRING_P);					\
  string2_start = (STRING_LOC ((ARG_REF (4)), 0));		\
  start2 = (arg_nonnegative_integer (5));			\
  end2 = (arg_nonnegative_integer (6));				\
  if (end1 > (STRING_LENGTH (ARG_REF (1))))			\
    error_bad_range_arg (3);					\
  if (start1 > end1)						\
    error_bad_range_arg (2);					\
  if (end2 > (STRING_LENGTH (ARG_REF (4))))			\
    error_bad_range_arg (6);					\
  if (start2 > end2)						\
    error_bad_range_arg (5)

#define SUBSTRING_EQUAL_PREFIX()				\
  unsigned char *scan1, *scan2, *limit;				\
  SUBSTRING_COMPARE_PREFIX ();					\
  if ((end1 - start1) != (end2 - start2))			\
    PRIMITIVE_RETURN (SHARP_F);					\
  scan1 = (string1_start + start1);				\
  limit = (string1_start + end1);				\
  scan2 = (string2_start + start2)

DEFINE_PRIMITIVE ("SUBSTRING=?", Prim_substring_equal, 6, 6, 0)
{
  SUBSTRING_EQUAL_PREFIX ();
  while (scan1 < limit)
    if ((*scan1++) != (*scan2++))
      PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("SUBSTRING-CI=?", Prim_substring_ci_equal, 6, 6, 0)
{
  SUBSTRING_EQUAL_PREFIX ();
  while (scan1 < limit)
    if ((char_upcase (*scan1++)) != (char_upcase (*scan2++)))
      PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("SUBSTRING<?", Prim_substring_less, 6, 6, 0)
{
  SUBSTRING_COMPARE_PREFIX ();
  {
    unsigned char * scan1 = (string1_start + start1);
    unsigned char * scan2 = (string2_start + start2);
    long length1 = (end1 - start1);
    long length2 = (end2 - start2);
    unsigned char * limit =
      (scan1 + ((length1 < length2) ? length1 : length2));
    while (scan1 < limit)
      if ((*scan1++) != (*scan2++))
	PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((scan1 [-1]) < (scan2 [-1])));
    PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (length1 < length2));
  }
}

static long
DEFUN (substring_length_min, (start1, end1, start2, end2),
       long start1
       AND long end1
       AND long start2
       AND long end2)
{
  long length1 = (end1 - start1);
  long length2 = (end2 - start2);
  return ((length1 < length2) ? length1 : length2);
}

#define SUBSTRING_MATCH_PREFIX()					\
  unsigned char *scan1, *scan2, *limit;					\
  long length;								\
  unsigned char *scan1_start;						\
  SUBSTRING_COMPARE_PREFIX ();						\
  length = (substring_length_min (start1, end1, start2, end2))

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-FORWARD", Prim_match_forward, 6, 6, 0)
{
  SUBSTRING_MATCH_PREFIX ();
  scan1 = (string1_start + start1);
  scan2 = (string2_start + start2);
  limit = (scan1 + length);
  scan1_start = scan1;
  while (scan1 < limit)
    if ((*scan1++) != (*scan2++))
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan1 - 1) - scan1_start));
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-FORWARD-CI", Prim_match_forward_ci, 6, 6, 0)
{
  SUBSTRING_MATCH_PREFIX ();
  scan1 = (string1_start + start1);
  scan2 = (string2_start + start2);
  limit = (scan1 + length);
  scan1_start = scan1;
  while (scan1 < limit)
    if ((char_upcase (*scan1++)) != (char_upcase (*scan2++)))
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((scan1 - 1) - scan1_start));
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-BACKWARD", Prim_match_backward, 6, 6, 0)
{
  SUBSTRING_MATCH_PREFIX ();
  scan1 = (string1_start + end1);
  scan2 = (string2_start + end2);
  limit = (scan1 - length);
  scan1_start = scan1;
  while (scan1 > limit)
    if ((*--scan1) != (*--scan2))
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan1_start - (scan1 + 1)));
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (length));
}

DEFINE_PRIMITIVE ("SUBSTRING-MATCH-BACKWARD-CI", Prim_match_backward_ci, 6, 6, 0)
{
  SUBSTRING_MATCH_PREFIX ();
  scan1 = (string1_start + end1);
  scan2 = (string2_start + end2);
  limit = (scan1 - length);
  scan1_start = scan1;
  while (scan1 > limit)
    if ((char_upcase (*--scan1)) != (char_upcase (*--scan2)))
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (scan1_start - (scan1 + 1)));
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (length));
}

/* External strings */

/* An external string is just a chunk of memory allocated using malloc
   outside of Scheme's address space.  It is represented to Scheme as
   an integer -- the address of the memory.  Each external string is
   registered in a hash table when it is allocated so that we can
   validate the incoming integers.  */

typedef struct ht_record_s ht_record_t;
struct ht_record_s
{
  ht_record_t * next;
  unsigned long n_bytes;
};

#define HT_RECORD_PTR(record) ((PTR) ((record) + 1))
#define HT_RECORD_KEY(record) ((unsigned long) ((record) + 1))
#define HT_RECORD_NEXT(record) ((record) -> next)
#define HT_RECORD_N_BYTES(record) ((record) -> n_bytes)

typedef struct
{
  unsigned long n_records;
  unsigned long n_buckets;
  ht_record_t ** buckets;
} hash_table_t;

#define HT_N_RECORDS(table) ((table) -> n_records)
#define HT_N_BUCKETS(table) ((table) -> n_buckets)
#define HT_BUCKET_INDEX(table, key) ((key) % (HT_N_BUCKETS (table)))
#define HT_BUCKETS(table) ((table) -> buckets)
#define HT_BUCKET_REF(table, index) ((HT_BUCKETS (table)) [(index)])
#define HT_SHRINK_POINT(table) ((((HT_N_BUCKETS (table)) + 1) / 2) - 1)

static hash_table_t * EXFUN (make_hash_table, (void));
static void EXFUN (ht_resize, (hash_table_t *, unsigned long));
static void EXFUN (zero_ht_buckets, (hash_table_t *));
static ht_record_t * EXFUN (ht_records_list, (hash_table_t *));
static ht_record_t * EXFUN (ht_lookup, (hash_table_t *, unsigned long));
static unsigned long EXFUN (ht_insert, (hash_table_t *, ht_record_t *));
static ht_record_t * EXFUN (ht_delete, (hash_table_t *, unsigned long));

static hash_table_t * external_strings = 0;

DEFINE_PRIMITIVE ("ALLOCATE-EXTERNAL-STRING", Prim_alloc_external_string, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    unsigned long n_bytes = (arg_ulong_integer (1));
    ht_record_t * result = (malloc (n_bytes + 1 + (sizeof (ht_record_t))));
    if (result == 0)
      error_bad_range_arg (1);
    if (external_strings == 0)
      external_strings = (make_hash_table ());
    (HT_RECORD_N_BYTES (result)) = n_bytes;
    /* Guarantee zero termination in case used as C string.  */
    (((char *) (HT_RECORD_PTR (result))) [n_bytes]) = '\0';
    PRIMITIVE_RETURN (ulong_to_integer (ht_insert (external_strings, result)));
  }
}

DEFINE_PRIMITIVE ("EXTERNAL-STRING?", Prim_external_string_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT x = (ARG_REF (1));
    if ((INTEGER_P (x)) && (integer_to_ulong_p (x)))
      {
	ht_record_t * record;
	if (external_strings == 0)
	  external_strings = (make_hash_table ());
	record = (ht_lookup (external_strings, (integer_to_ulong (x))));
	PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (record != 0));
      }
    else
      PRIMITIVE_RETURN (SHARP_F);
  }
}

DEFINE_PRIMITIVE ("DEALLOCATE-EXTERNAL-STRING", Prim_dealloc_external_string, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    unsigned long n = (arg_ulong_integer (1));
    ht_record_t * record;
    if (external_strings == 0)
      external_strings = (make_hash_table ());
    record = (ht_delete (external_strings, n));
    if (record == 0)
      error_wrong_type_arg (1);
    free (record);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("EXTENDED-STRING-LENGTH", Prim_extended_string_length, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    unsigned long len;
    arg_extended_string (1, (&len));
    PRIMITIVE_RETURN (ulong_to_integer (len));
  }
}

PTR
DEFUN (arg_extended_string, (n), unsigned int n AND unsigned long * lp)
{
  SCHEME_OBJECT object = (ARG_REF (n));
  if (STRING_P (object))
    {
      if (lp != 0)
	(*lp) = (STRING_LENGTH (object));
      return (STRING_LOC (object, 0));
    }
  else if ((INTEGER_P (object)) && (integer_to_ulong_p (object)))
    {
      ht_record_t * record;
      if (external_strings == 0)
	external_strings = (make_hash_table ());
      record = (ht_lookup (external_strings, (integer_to_ulong (object))));
      if (record == 0)
	error_wrong_type_arg (n);
      if (lp != 0)
	(*lp) = (HT_RECORD_N_BYTES (record));
      return (HT_RECORD_PTR (record));
    }
  else
    {
      error_wrong_type_arg (n);
      return (0);
    }
}

#define HT_MIN_EXPT 4
#define HT_MAX_EXPT 24

#define EXPT_TO_N(e) ((1 << (e)) - 1)

static hash_table_t *
DEFUN_VOID (make_hash_table)
{
  unsigned long n = (EXPT_TO_N (HT_MIN_EXPT));
  hash_table_t * table = (malloc (sizeof (hash_table_t)));
  if (table == 0)
    abort ();
  (HT_N_RECORDS (table)) = 0;
  (HT_N_BUCKETS (table)) = n;
  (HT_BUCKETS (table)) = (malloc (n * (sizeof (ht_record_t *))));
  if ((HT_BUCKETS (table)) == 0)
    abort ();
  zero_ht_buckets (table);
  return (table);
}

static void
DEFUN (ht_resize, (table, new_n_buckets),
       hash_table_t * table AND
       unsigned long new_n_buckets)
{
  ht_record_t ** new_buckets
    = (malloc (new_n_buckets * (sizeof (ht_record_t *))));
  if (new_buckets != 0)
    {
      ht_record_t * records = (ht_records_list (table));
      (HT_BUCKETS (table)) = new_buckets;
      (HT_N_BUCKETS (table)) = new_n_buckets;
      (HT_N_RECORDS (table)) = 0;
      zero_ht_buckets (table);
      while (records != 0)
	{
	  ht_record_t * next = (HT_RECORD_NEXT (records));
	  ht_insert (table, records);
	  records = next;
	}
    }
}

static void
DEFUN (zero_ht_buckets, (table), hash_table_t * table)
{
  ht_record_t ** scan = (HT_BUCKETS (table));
  ht_record_t ** end = (scan + (HT_N_BUCKETS (table)));
  while (scan < end)
    (*scan++) = 0;
}

static ht_record_t *
DEFUN (ht_records_list, (table), hash_table_t * table)
{
  ht_record_t ** scan_buckets = (HT_BUCKETS (table));
  ht_record_t ** end_buckets = (scan_buckets + (HT_N_BUCKETS (table)));
  ht_record_t * result = 0;
  while (scan_buckets < end_buckets)
    {
      ht_record_t * scan = (*scan_buckets);
      while (scan != 0)
	{
	  ht_record_t * next = (HT_RECORD_NEXT (scan));
	  (HT_RECORD_NEXT (scan)) = result;
	  result = scan;
	  scan = next;
	}
      (*scan_buckets++) = 0;
    }
  return (result);
}

static ht_record_t *
DEFUN (ht_lookup, (table, key),
       hash_table_t * table AND
       unsigned long key)
{
  unsigned long index = (HT_BUCKET_INDEX (table, key));
  ht_record_t * record = (HT_BUCKET_REF (table, index));
  while (record != 0)
    {
      if ((HT_RECORD_KEY (record)) == key)
	return (record);
      record = (HT_RECORD_NEXT (record));
    }
  return (0);
}

static unsigned long
DEFUN (ht_insert, (table, record),
       hash_table_t * table AND
       ht_record_t * record)
{
  unsigned long index = (HT_BUCKET_INDEX (table, (HT_RECORD_KEY (record))));
  ht_record_t * scan = (HT_BUCKET_REF (table, index));
  (HT_RECORD_NEXT (record)) = 0;
  if (scan == 0)
    (HT_BUCKET_REF (table, index)) = record;
  else
    {
      while ((HT_RECORD_NEXT (scan)) != 0)
	scan = (HT_RECORD_NEXT (scan));
      (HT_RECORD_NEXT (scan)) = record;
    }
  (HT_N_RECORDS (table)) += 1;
  if (((HT_N_RECORDS (table)) >= (HT_N_BUCKETS (table)))
      && ((HT_N_BUCKETS (table)) < (EXPT_TO_N (HT_MAX_EXPT))))
    {
      unsigned int e = HT_MIN_EXPT;
      while (e <= HT_MAX_EXPT)
	{
	  unsigned long n = (EXPT_TO_N (e));
	  if (n > (HT_N_BUCKETS (table)))
	    {
	      ht_resize (table, n);
	      break;
	    }
	  e += 1;
	}
    }
  return (HT_RECORD_KEY (record));
}

static ht_record_t *
DEFUN (ht_delete, (table, key),
       hash_table_t * table AND
       unsigned long key)
{
  unsigned long index = (HT_BUCKET_INDEX (table, key));
  ht_record_t * scan = (HT_BUCKET_REF (table, index));
  ht_record_t * prev = 0;
  while (1)
    {
      if (scan == 0)
	return (0);
      if ((HT_RECORD_KEY (scan)) == key)
	break;
      prev = scan;
      scan = (HT_RECORD_NEXT (scan));
    }
  if (prev == 0)
    (HT_BUCKET_REF (table, index)) = (HT_RECORD_NEXT (scan));
  else
    (HT_RECORD_NEXT (prev)) = (HT_RECORD_NEXT (scan));
  (HT_N_RECORDS (table)) -= 1;
  if (((HT_N_RECORDS (table)) < (HT_SHRINK_POINT (table)))
      && ((HT_N_BUCKETS (table)) > (EXPT_TO_N (HT_MIN_EXPT))))
    {
      unsigned int e = HT_MAX_EXPT;
      while (e >= HT_MIN_EXPT)
	{
	  unsigned long n = (EXPT_TO_N (e));
	  if (n < (HT_N_BUCKETS (table)))
	    {
	      ht_resize (table, n);
	      break;
	    }
	  e -= 1;
	}
    }
  return (scan);
}
