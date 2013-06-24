/* -*-C-*-

$Id: prmcrypt.c,v 1.5 2003/02/14 18:28:23 cph Exp $

Copyright (c) 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* Interface to mcrypt library */

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "os.h"
#include <mcrypt.h>

static SCHEME_OBJECT
cp2s (char * cp)
{
  if (cp == 0)
    return (SHARP_F);
  else
    {
      SCHEME_OBJECT s = (char_pointer_to_string (cp));
      mcrypt_free (cp);
      return (s);
    }
}

static size_t context_table_length = 0;
static MCRYPT * context_table = 0;

static size_t
search_context_table (MCRYPT context)
{
  size_t i;
  for (i = 0; (i < context_table_length); i += 1)
    if ((context_table[i]) == context)
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
	= (OS_malloc ((sizeof (MCRYPT)) * context_table_length));
    }
  else
    {
      context_table_length *= 2;
      context_table
	= (OS_realloc (context_table,
		       ((sizeof (MCRYPT)) * context_table_length)));
    }
  {
    size_t j;
    for (j = i; (j < context_table_length); j += 1)
      (context_table[j]) = 0;
  }
  return (i);
}

static SCHEME_OBJECT
store_context (MCRYPT context)
{
  if (context == MCRYPT_FAILED)
    return (SHARP_F);
  {
    size_t i = (allocate_context_entry ());
    (context_table[i]) = context;
    return (ulong_to_integer (i));
  }
}

static void
forget_context (size_t index)
{
  (context_table[index]) = 0;
}

static size_t
arg_context_index (unsigned int arg)
{
  unsigned long n = (arg_ulong_index_integer (arg, context_table_length));
  if ((context_table[n]) == 0)
    error_bad_range_arg (arg);
  return (n);
}

static MCRYPT
arg_context (unsigned int arg)
{
  return (context_table [arg_context_index (arg)]);
}

DEFINE_PRIMITIVE ("MCRYPT_MODULE_OPEN", Prim_mcrypt_module_open, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (store_context
     (mcrypt_module_open ((STRING_ARG (1)), 0, (STRING_ARG (2)), 0)));
}

DEFINE_PRIMITIVE ("MCRYPT_GENERIC_INIT", Prim_mcrypt_generic_init, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (2, STRING_P);
  PRIMITIVE_RETURN
    (long_to_integer
     (mcrypt_generic_init ((arg_context (1)),
			   (STRING_LOC ((ARG_REF (2)), 0)),
			   (STRING_LENGTH (ARG_REF (2))),
			   (STRING_ARG (3)))));
}

DEFINE_PRIMITIVE ("MCRYPT_GENERIC", Prim_mcrypt_generic, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (2));
    unsigned long l = (STRING_LENGTH (string));
    unsigned long start = (arg_ulong_index_integer (3, l));
    unsigned long end = (arg_integer_in_range (4, start, (l + 1)));
    PRIMITIVE_RETURN
      (long_to_integer
       (mcrypt_generic ((arg_context (1)),
			(STRING_LOC (string, start)),
			(end - start))));
  }
}

DEFINE_PRIMITIVE ("MDECRYPT_GENERIC", Prim_mdecrypt_generic, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (2));
    unsigned long l = (STRING_LENGTH (string));
    unsigned long start = (arg_ulong_index_integer (3, l));
    unsigned long end = (arg_integer_in_range (4, start, (l + 1)));
    PRIMITIVE_RETURN
      (long_to_integer
       (mdecrypt_generic ((arg_context (1)),
			  (STRING_LOC (string, start)),
			  (end - start))));
  }
}

DEFINE_PRIMITIVE ("MCRYPT_GENERIC_END", Prim_mcrypt_generic_end, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    size_t index = (arg_context_index (1));
    int result = (mcrypt_generic_end (context_table[index]));
    forget_context (index);
    PRIMITIVE_RETURN (long_to_integer (result));
  }
}

#define CONTEXT_OPERATION(name, cvt_val)				\
{									\
  PRIMITIVE_HEADER (1);							\
  PRIMITIVE_RETURN (cvt_val (name (arg_context (1))));			\
}

DEFINE_PRIMITIVE ("MCRYPT_ENC_SELF_TEST", Prim_mcrypt_enc_self_test, 1, 1, 0)
  CONTEXT_OPERATION (mcrypt_enc_self_test, long_to_integer)

DEFINE_PRIMITIVE ("MCRYPT_ENC_IS_BLOCK_ALGORITHM_MODE", Prim_mcrypt_enc_is_block_algorithm_mode, 1, 1, 0)
  CONTEXT_OPERATION (mcrypt_enc_is_block_algorithm_mode, BOOLEAN_TO_OBJECT)

DEFINE_PRIMITIVE ("MCRYPT_ENC_IS_BLOCK_ALGORITHM", Prim_mcrypt_enc_is_block_algorithm, 1, 1, 0)
  CONTEXT_OPERATION (mcrypt_enc_is_block_algorithm, BOOLEAN_TO_OBJECT)

DEFINE_PRIMITIVE ("MCRYPT_ENC_IS_BLOCK_MODE", Prim_mcrypt_enc_is_block_mode, 1, 1, 0)
  CONTEXT_OPERATION (mcrypt_enc_is_block_mode, BOOLEAN_TO_OBJECT)

DEFINE_PRIMITIVE ("MCRYPT_ENC_GET_KEY_SIZE", Prim_mcrypt_enc_get_key_size, 1, 1, 0)
  CONTEXT_OPERATION (mcrypt_enc_get_key_size, long_to_integer)

DEFINE_PRIMITIVE ("MCRYPT_ENC_GET_IV_SIZE", Prim_mcrypt_enc_get_iv_size, 1, 1, 0)
  CONTEXT_OPERATION (mcrypt_enc_get_iv_size, long_to_integer)

DEFINE_PRIMITIVE ("MCRYPT_ENC_GET_ALGORITHMS_NAME", Prim_mcrypt_enc_get_algorithms_name, 1, 1, 0)
  CONTEXT_OPERATION (mcrypt_enc_get_algorithms_name, cp2s)

DEFINE_PRIMITIVE ("MCRYPT_ENC_GET_MODES_NAME", Prim_mcrypt_enc_get_modes_name, 1, 1, 0)
  CONTEXT_OPERATION (mcrypt_enc_get_modes_name, cp2s)

#define MODULE_OPERATION(name, cvt_val)					\
{									\
  PRIMITIVE_HEADER (1);							\
  PRIMITIVE_RETURN (cvt_val (name ((STRING_ARG (1)), 0)));		\
}

DEFINE_PRIMITIVE ("MCRYPT_MODULE_SELF_TEST", Prim_mcrypt_module_self_test, 1, 1, 0)
  MODULE_OPERATION (mcrypt_module_self_test, long_to_integer)

DEFINE_PRIMITIVE ("MCRYPT_MODULE_IS_BLOCK_ALGORITHM_MODE", Prim_mcrypt_module_is_block_algorithm_mode, 1, 1, 0)
  MODULE_OPERATION (mcrypt_module_is_block_algorithm_mode, BOOLEAN_TO_OBJECT)

DEFINE_PRIMITIVE ("MCRYPT_MODULE_IS_BLOCK_ALGORITHM", Prim_mcrypt_module_is_block_algorithm, 1, 1, 0)
  MODULE_OPERATION (mcrypt_module_is_block_algorithm, BOOLEAN_TO_OBJECT)

DEFINE_PRIMITIVE ("MCRYPT_MODULE_IS_BLOCK_MODE", Prim_mcrypt_module_is_block_mode, 1, 1, 0)
  MODULE_OPERATION (mcrypt_module_is_block_mode, BOOLEAN_TO_OBJECT)

DEFINE_PRIMITIVE ("MCRYPT_MODULE_GET_ALGO_BLOCK_SIZE", Prim_mcrypt_module_get_algo_block_size, 1, 1, 0)
  MODULE_OPERATION (mcrypt_module_get_algo_block_size, long_to_integer)

DEFINE_PRIMITIVE ("MCRYPT_MODULE_GET_ALGO_KEY_SIZE", Prim_mcrypt_module_get_algo_key_size, 1, 1, 0)
  MODULE_OPERATION (mcrypt_module_get_algo_key_size, long_to_integer)

struct deallocate_list_arg
{
  char ** elements;
  int n_elements;
};

static void
DEFUN (deallocate_list, (environment), PTR environment)
{
  struct deallocate_list_arg * a = environment;
  if ((a -> elements) != 0)
    mcrypt_free_p ((a -> elements), (a -> n_elements));
}

#define LIST_ITEMS(name)						\
{									\
  PRIMITIVE_HEADER (0);							\
  {									\
    struct deallocate_list_arg a;					\
    (a . elements) = (name (0, (& (a . n_elements))));			\
    transaction_begin ();						\
    transaction_record_action (tat_always, deallocate_list, (&a));	\
    if ((a . n_elements) < 0)						\
      error_external_return ();						\
    {									\
      char ** scan = (a . elements);					\
      char ** end = (scan + (a . n_elements));				\
      SCHEME_OBJECT sa = (make_vector ((a . n_elements), SHARP_F, 1));	\
      SCHEME_OBJECT * scan_sa = (VECTOR_LOC (sa, 0));			\
      while (scan < end)						\
	(*scan_sa++) = (char_pointer_to_string (*scan++));		\
      transaction_commit ();						\
      PRIMITIVE_RETURN (sa);						\
    }									\
  }									\
}

DEFINE_PRIMITIVE ("MCRYPT_LIST_ALGORITHMS", Prim_mcrypt_list_algorithms, 0, 0, 0)
  LIST_ITEMS (mcrypt_list_algorithms)

DEFINE_PRIMITIVE ("MCRYPT_LIST_MODES", Prim_mcrypt_list_modes, 0, 0, 0)
  LIST_ITEMS (mcrypt_list_modes)

static void
DEFUN (deallocate_key_sizes, (environment), PTR environment)
{
  if (environment != 0)
    mcrypt_free (environment);
}

static SCHEME_OBJECT
convert_key_sizes (int * sizes, int n_sizes)
{
  transaction_begin ();
  transaction_record_action (tat_always, deallocate_key_sizes, sizes);
  if (n_sizes < 0)
    error_external_return ();
  if (n_sizes == 0)
    {
      transaction_commit ();
      return (SHARP_F);
    }
  {
    SCHEME_OBJECT sa = (make_vector (n_sizes, FIXNUM_ZERO, 1));
    SCHEME_OBJECT * scan_sa = (VECTOR_LOC (sa, 0));
    int * scan = sizes;
    int * end = (scan + n_sizes);
    while (scan < end)
      (*scan_sa++) = (long_to_integer (*scan++));
    transaction_commit ();
    return (sa);
  }
}

DEFINE_PRIMITIVE ("MCRYPT_ENC_GET_SUPPORTED_KEY_SIZES", Prim_mcrypt_enc_get_supported_key_sizes, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    int n_sizes;
    int * sizes
      = (mcrypt_enc_get_supported_key_sizes ((arg_context (1)), (&n_sizes)));
    PRIMITIVE_RETURN (convert_key_sizes (sizes, n_sizes));
  }
}

DEFINE_PRIMITIVE ("MCRYPT_MODULE_GET_ALGO_SUPPORTED_KEY_SIZES", Prim_mcrypt_module_get_algo_supported_key_sizes, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    int n_sizes;
    int * sizes
      = (mcrypt_module_get_algo_supported_key_sizes
	 ((STRING_ARG (1)), 0, (&n_sizes)));
    PRIMITIVE_RETURN (convert_key_sizes (sizes, n_sizes));
  }
}

#ifdef COMPILE_AS_MODULE

char *
DEFUN_VOID (dload_initialize_file)
{
  declare_primitive
    ("MCRYPT_MODULE_OPEN", Prim_mcrypt_module_open, 2, 2, 0);
  declare_primitive
    ("MCRYPT_GENERIC_INIT", Prim_mcrypt_generic_init, 3, 3, 0);
  declare_primitive
    ("MCRYPT_GENERIC", Prim_mcrypt_generic, 4, 4, 0);
  declare_primitive
    ("MDECRYPT_GENERIC", Prim_mdecrypt_generic, 4, 4, 0);
  declare_primitive
    ("MCRYPT_GENERIC_END", Prim_mcrypt_generic_end, 1, 1, 0);
  declare_primitive
    ("MCRYPT_ENC_SELF_TEST", Prim_mcrypt_enc_self_test, 1, 1, 0);
  declare_primitive
    ("MCRYPT_ENC_IS_BLOCK_ALGORITHM_MODE", Prim_mcrypt_enc_is_block_algorithm_mode, 1, 1, 0);
  declare_primitive
    ("MCRYPT_ENC_IS_BLOCK_ALGORITHM", Prim_mcrypt_enc_is_block_algorithm, 1, 1, 0);
  declare_primitive
    ("MCRYPT_ENC_IS_BLOCK_MODE", Prim_mcrypt_enc_is_block_mode, 1, 1, 0);
  declare_primitive
    ("MCRYPT_ENC_GET_KEY_SIZE", Prim_mcrypt_enc_get_key_size, 1, 1, 0);
  declare_primitive
    ("MCRYPT_ENC_GET_IV_SIZE", Prim_mcrypt_enc_get_iv_size, 1, 1, 0);
  declare_primitive
    ("MCRYPT_ENC_GET_ALGORITHMS_NAME", Prim_mcrypt_enc_get_algorithms_name, 1, 1, 0);
  declare_primitive
    ("MCRYPT_ENC_GET_MODES_NAME", Prim_mcrypt_enc_get_modes_name, 1, 1, 0);
  declare_primitive
    ("MCRYPT_MODULE_SELF_TEST", Prim_mcrypt_module_self_test, 1, 1, 0);
  declare_primitive
    ("MCRYPT_MODULE_IS_BLOCK_ALGORITHM_MODE", Prim_mcrypt_module_is_block_algorithm_mode, 1, 1, 0);
  declare_primitive
    ("MCRYPT_MODULE_IS_BLOCK_ALGORITHM", Prim_mcrypt_module_is_block_algorithm, 1, 1, 0);
  declare_primitive
    ("MCRYPT_MODULE_IS_BLOCK_MODE", Prim_mcrypt_module_is_block_mode, 1, 1, 0);
  declare_primitive
    ("MCRYPT_MODULE_GET_ALGO_BLOCK_SIZE", Prim_mcrypt_module_get_algo_block_size, 1, 1, 0);
  declare_primitive
    ("MCRYPT_MODULE_GET_ALGO_KEY_SIZE", Prim_mcrypt_module_get_algo_key_size, 1, 1, 0);
  declare_primitive
    ("MCRYPT_LIST_ALGORITHMS", Prim_mcrypt_list_algorithms, 0, 0, 0);
  declare_primitive
    ("MCRYPT_LIST_MODES", Prim_mcrypt_list_modes, 0, 0, 0);
  declare_primitive
    ("MCRYPT_ENC_GET_SUPPORTED_KEY_SIZES", Prim_mcrypt_enc_get_supported_key_sizes, 1, 1, 0);
  declare_primitive
     ("MCRYPT_MODULE_GET_ALGO_SUPPORTED_KEY_SIZES", Prim_mcrypt_module_get_algo_supported_key_sizes, 1, 1, 0);
  return "#prmcrypt";
}

#endif /* COMPILE_AS_MODULE */
