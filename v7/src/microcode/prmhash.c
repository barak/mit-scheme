/* -*-C-*-

$Id: prmhash.c,v 11.3 2001/03/09 16:13:02 cph Exp $

Copyright (c) 2000-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Interface to mhash library */

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "os.h"
#include <mhash.h>

#define UNARY_OPERATION(name, get_arg, cvt_val)				\
{									\
  PRIMITIVE_HEADER (1);							\
  PRIMITIVE_RETURN (cvt_val (name (get_arg (1))));			\
}

static SCHEME_OBJECT
cp2s (char * cp)
{
  if (cp == 0)
    return (SHARP_F);
  else
    {
      SCHEME_OBJECT s = (char_pointer_to_string (cp));
      free (cp);
      return (s);
    }
}

typedef struct
{
  MHASH context;
  hashid id;
} context_entry;

static size_t context_table_length = 0;
static context_entry * context_table = 0;

static size_t
search_context_table (MHASH context)
{
  size_t i;
  for (i = 0; (i < context_table_length); i += 1)
    if (((context_table[i]) . context) == context)
      break;
  return (i);
}

static size_t
allocate_context_entry (void)
{
  size_t i = (search_context_table (0));
  if (i < context_table_length)
    return (i);
  if (i == 0)
    {
      context_table_length = 256;
      context_table
	= (OS_malloc ((sizeof (context_entry)) * context_table_length));
    }
  else
    {
      context_table_length *= 2;
      context_table
	= (OS_realloc (context_table,
		       ((sizeof (context_entry)) * context_table_length)));
    }
  {
    size_t j;
    for (j = i; (j < context_table_length); j += 1)
      ((context_table[j]) . context) = 0;
  }
  return (i);
}

static SCHEME_OBJECT
store_context (MHASH context, hashid id)
{
  if (context == MHASH_FAILED)
    return (SHARP_F);
  {
    size_t i = (allocate_context_entry ());
    ((context_table[i]) . context) = context;
    ((context_table[i]) . id) = id;
    return (ulong_to_integer (i));
  }
}

static void
forget_context (size_t index)
{
  ((context_table[index]) . context) = 0;
}

static size_t
arg_context_index (unsigned int arg)
{
  unsigned long n = (arg_ulong_index_integer (arg, context_table_length));
  if (((context_table[n]) . context) == 0)
    error_bad_range_arg (arg);
  return (n);
}

static MHASH
arg_context (unsigned int arg)
{
  return ((context_table [arg_context_index (arg)]) . context);
}

static size_t hashid_count;
static hashid * hashid_map = 0;

static void
initialize_hashid_map (void)
{
  if (hashid_map == 0)
    {
      size_t i = 0;
      size_t j = 0;
      hashid_count = (mhash_count ());
      hashid_map = (OS_malloc ((sizeof (hashid)) * hashid_count));
      while (j < hashid_count)
	{
	  if ((mhash_get_block_size (i)) != 0)
	    (hashid_map[j++]) = ((hashid) i);
	  i += 1;
	}
    }
}

static hashid
arg_hashid (unsigned int arg)
{
  initialize_hashid_map ();
  return (hashid_map [arg_ulong_index_integer (arg, hashid_count)]);
}

DEFINE_PRIMITIVE ("MHASH_COUNT", Prim_mhash_count, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  initialize_hashid_map ();
  PRIMITIVE_RETURN (ulong_to_integer (hashid_count));
}

DEFINE_PRIMITIVE ("MHASH_GET_BLOCK_SIZE", Prim_mhash_get_block_size, 1, 1, 0)
  UNARY_OPERATION (mhash_get_block_size, arg_hashid, ulong_to_integer)
DEFINE_PRIMITIVE ("MHASH_GET_HASH_PBLOCK", Prim_mhash_get_hash_pblock, 1, 1, 0)
  UNARY_OPERATION (mhash_get_hash_pblock, arg_hashid, ulong_to_integer)
DEFINE_PRIMITIVE ("MHASH_GET_HASH_NAME", Prim_mhash_get_hash_name, 1, 1, 0)
  UNARY_OPERATION (mhash_get_hash_name, arg_hashid, cp2s)

DEFINE_PRIMITIVE ("MHASH_INIT", Prim_mhash_init, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    hashid id = (arg_hashid (1));
    PRIMITIVE_RETURN (store_context ((mhash_init (id)), id));
  }
}

DEFINE_PRIMITIVE ("MHASH_HMAC_INIT", Prim_mhash_hmac_init, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (2, STRING_P);
  {
    hashid id = (arg_hashid (1));
    SCHEME_OBJECT key = (ARG_REF (2));
    PRIMITIVE_RETURN
      (store_context ((mhash_hmac_init (id,
					(STRING_LOC (key, 0)),
					(STRING_LENGTH (key)),
					(arg_ulong_integer (3)))),
		      id));
  }
}

DEFINE_PRIMITIVE ("MHASH", Prim_mhash, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (2));
    unsigned long l = (STRING_LENGTH (string));
    unsigned long start = (arg_ulong_index_integer (3, l));
    unsigned long end = (arg_integer_in_range (4, start, (l + 1)));
    mhash ((arg_context (1)), (STRING_LOC (string, start)), (end - start));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("MHASH_END", Prim_mhash_end, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    size_t index = (arg_context_index (1));
    MHASH context = ((context_table[index]) . context);
    hashid id = ((context_table[index]) . id);
    size_t block_size = (mhash_get_block_size (id));
    /* Must allocate string _before_ calling mhash_end.  */
    SCHEME_OBJECT sd = (allocate_string (block_size));
    void * digest = (mhash_end (context));
    forget_context (index);
    memcpy ((STRING_LOC (sd, 0)), digest, block_size);
    free (digest);
    PRIMITIVE_RETURN (sd);
  }
}

DEFINE_PRIMITIVE ("MHASH_HMAC_END", Prim_mhash_hmac_end, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    size_t index = (arg_context_index (1));
    MHASH context = ((context_table[index]) . context);
    hashid id = ((context_table[index]) . id);
    size_t block_size = (mhash_get_block_size (id));
    /* Must allocate string _before_ calling mhash_hmac_end.  */
    SCHEME_OBJECT sd = (allocate_string (block_size));
    void * digest = (mhash_hmac_end (context));
    forget_context (index);
    memcpy ((STRING_LOC (sd, 0)), digest, block_size);
    free (digest);
    PRIMITIVE_RETURN (sd);
  }
}

static size_t keygenid_count;
static keygenid * keygenid_map = 0;

static void
initialize_keygenid_map (void)
{
  if (keygenid_map == 0)
    {
      size_t i = 0;
      size_t j = 0;
      keygenid_count = (mhash_keygen_count ());
      keygenid_map = (OS_malloc ((sizeof (keygenid)) * keygenid_count));
      while (j < keygenid_count)
	{
	  char * name = (mhash_get_keygen_name (i));
	  if (name != 0)
	    {
	      (keygenid_map[j++]) = ((keygenid) i);
	      free (name);
	    }
	  i += 1;
	}
    }
}

static keygenid
arg_keygenid (unsigned int arg)
{
  initialize_keygenid_map ();
  return (keygenid_map [arg_ulong_index_integer (arg, keygenid_count)]);
}

DEFINE_PRIMITIVE ("MHASH_KEYGEN_COUNT", Prim_mhash_keygen_count, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  initialize_keygenid_map ();
  PRIMITIVE_RETURN (ulong_to_integer (keygenid_count));
}

DEFINE_PRIMITIVE ("MHASH_GET_KEYGEN_NAME", Prim_mhash_get_keygen_name, 1, 1, 0)
  UNARY_OPERATION (mhash_get_keygen_name, arg_keygenid, cp2s)
DEFINE_PRIMITIVE ("MHASH_KEYGEN_USES_SALT", Prim_mhash_keygen_uses_salt, 1, 1, 0)
  UNARY_OPERATION (mhash_keygen_uses_salt, arg_keygenid, BOOLEAN_TO_OBJECT)
DEFINE_PRIMITIVE ("MHASH_KEYGEN_USES_COUNT", Prim_mhash_keygen_uses_count, 1, 1, 0)
  UNARY_OPERATION (mhash_keygen_uses_count, arg_keygenid, BOOLEAN_TO_OBJECT)
DEFINE_PRIMITIVE ("MHASH_KEYGEN_USES_HASH_ALGORITHM", Prim_mhash_keygen_uses_hash_algorithm, 1, 1, 0)
  UNARY_OPERATION (mhash_keygen_uses_hash_algorithm, arg_keygenid, long_to_integer)
DEFINE_PRIMITIVE ("MHASH_GET_KEYGEN_SALT_SIZE", Prim_mhash_get_keygen_salt_size, 1, 1, 0)
  UNARY_OPERATION (mhash_get_keygen_salt_size, arg_keygenid, ulong_to_integer)
DEFINE_PRIMITIVE ("MHASH_GET_KEYGEN_MAX_KEY_SIZE", Prim_mhash_get_keygen_max_key_size, 1, 1, 0)
  UNARY_OPERATION (mhash_get_keygen_max_key_size, arg_keygenid, ulong_to_integer)

DEFINE_PRIMITIVE ("MHASH_KEYGEN", Prim_mhash_keygen, 4, 4, 0)
{
  /* keygen-id #(salt count hashid ...) keyword passphrase */
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, VECTOR_P);
  CHECK_ARG (3, STRING_P);
  CHECK_ARG (4, STRING_P);
  {
    keygenid id = (arg_keygenid (1));
    SCHEME_OBJECT parameters = (ARG_REF (2));
    SCHEME_OBJECT keyword = (ARG_REF (3));
    SCHEME_OBJECT passphrase = (ARG_REF (4));
    unsigned int n_algs = (mhash_keygen_uses_hash_algorithm (id));
    SCHEME_OBJECT salt;
    SCHEME_OBJECT count;
    KEYGEN cparms;
    {
      size_t max_key_size = (mhash_get_keygen_max_key_size (id));
      if ((max_key_size != 0) && ((STRING_LENGTH (keyword)) > max_key_size))
	error_bad_range_arg (4);
    }
    if ((VECTOR_LENGTH (parameters)) != (2 + n_algs))
      error_bad_range_arg (2);
    salt = (VECTOR_REF (parameters, 0));
    count = (VECTOR_REF (parameters, 1));
    if (mhash_keygen_uses_salt (id))
      {
	if (!STRING_P (salt))
	  error_bad_range_arg (2);
	{
	  size_t salt_size = (mhash_get_keygen_salt_size (id));
	  if ((salt_size != 0) && ((STRING_LENGTH (salt)) != salt_size))
	    error_bad_range_arg (2);
	}
	(cparms . salt) = (STRING_LOC (salt, 0));
	(cparms . salt_size) = (STRING_LENGTH (salt));
      }
    else if (salt != SHARP_F)
      error_bad_range_arg (2);
    if (mhash_keygen_uses_count (id))
      {
	if (!integer_to_ulong_p (count))
	  error_bad_range_arg (2);
	(cparms . count) = (integer_to_ulong (count));
      }
    else if (count != SHARP_F)
      error_bad_range_arg (2);
    {
      unsigned int i;
      initialize_hashid_map ();
      for (i = 0; (i < n_algs); i += 1)
	{
	  SCHEME_OBJECT a = (VECTOR_REF (parameters, (2 + i)));
	  if (!integer_to_ulong_p (a))
	    error_bad_range_arg (2);
	  {
	    unsigned long ia = (integer_to_ulong (a));
	    if (ia < hashid_count)
	      ((cparms . hash_algorithm) [i]) = (hashid_map[ia]);
	    else
	      error_bad_range_arg (2);
	  }
	}
    }
    PRIMITIVE_RETURN
      (BOOLEAN_TO_OBJECT
       ((mhash_keygen_ext (id, cparms,
			   (STRING_LOC (keyword, 0)),
			   (STRING_LENGTH (keyword)),
			   (STRING_LOC (passphrase, 0)),
			   (STRING_LENGTH (passphrase))))
	== 0));
  }
}

#ifdef COMPILE_AS_MODULE

char *
DEFUN_VOID (dload_initialize_file)
{
  declare_primitive
    ("MHASH_COUNT", Prim_mhash_count, 0, 0, 0);
  declare_primitive
    ("MHASH_GET_BLOCK_SIZE", Prim_mhash_get_block_size, 1, 1, 0);
  declare_primitive
    ("MHASH_GET_HASH_PBLOCK", Prim_mhash_get_hash_pblock, 1, 1, 0);
  declare_primitive
    ("MHASH_GET_HASH_NAME", Prim_mhash_get_hash_name, 1, 1, 0);
  declare_primitive
    ("MHASH_INIT", Prim_mhash_init, 1, 1, 0);
  declare_primitive
    ("MHASH_HMAC_INIT", Prim_mhash_hmac_init, 3, 3, 0);
  declare_primitive
    ("MHASH", Prim_mhash, 4, 4, 0);
  declare_primitive
    ("MHASH_END", Prim_mhash_end, 1, 1, 0);
  declare_primitive
    ("MHASH_HMAC_END", Prim_mhash_hmac_end, 1, 1, 0);
  declare_primitive
    ("MHASH_KEYGEN_COUNT", Prim_mhash_keygen_count, 0, 0, 0);
  declare_primitive
    ("MHASH_GET_KEYGEN_NAME", Prim_mhash_get_keygen_name, 1, 1, 0);
  declare_primitive
    ("MHASH_KEYGEN_USES_SALT", Prim_mhash_keygen_uses_salt, 1, 1, 0);
  declare_primitive
    ("MHASH_KEYGEN_USES_COUNT", Prim_mhash_keygen_uses_count, 1, 1, 0);
  declare_primitive
    ("MHASH_KEYGEN_USES_HASH_ALGORITHM", Prim_mhash_keygen_uses_hash_algorithm, 1, 1, 0);
  declare_primitive
    ("MHASH_GET_KEYGEN_SALT_SIZE", Prim_mhash_get_keygen_salt_size, 1, 1, 0);
  declare_primitive
    ("MHASH_GET_KEYGEN_MAX_KEY_SIZE", Prim_mhash_get_keygen_max_key_size, 1, 1, 0);
  declare_primitive
     ("MHASH_KEYGEN", Prim_mhash_keygen, 4, 4, 0);
  return "#prmd5";
}

#endif /* COMPILE_AS_MODULE */
