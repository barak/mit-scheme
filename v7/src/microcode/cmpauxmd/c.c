/* -*-C-*-

$Id: c.c,v 1.16 2006/09/16 11:19:09 gjr Exp $

Copyright (c) 1992-1999, 2002, 2006 Massachusetts Institute of Technology

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

#include <string.h>
#define LIARC_IN_MICROCODE
#include "liarc.h"
#include "prims.h"
#include "bignum.h"
#include "bitstr.h"
#include "avltree.h"

#ifdef BUG_GCC_LONG_CALLS

extern SCHEME_OBJECT EXFUN (memory_to_string,
			    (unsigned long, CONST unsigned char *));
extern SCHEME_OBJECT EXFUN (memory_to_symbol, (long, unsigned char *));
extern SCHEME_OBJECT EXFUN (make_vector, (long, SCHEME_OBJECT, Boolean));
extern SCHEME_OBJECT EXFUN (cons, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (double_to_flonum, (double));
extern SCHEME_OBJECT EXFUN (long_to_integer, (long));
extern SCHEME_OBJECT EXFUN (digit_string_to_integer,
			    (Boolean, unsigned long, unsigned char *));
extern SCHEME_OBJECT EXFUN (digit_string_to_bit_string,
			    (unsigned long, unsigned long, unsigned char *));
extern SCHEME_OBJECT EXFUN (make_primitive, (char *, int));
extern SCHEME_OBJECT EXFUN (memory_to_uninterned_symbol,
			    (unsigned long, unsigned char *));

SCHEME_OBJECT EXFUN ((* (constructor_kludge [11])), ()) =
{
  ((SCHEME_OBJECT EXFUN ((*), ())) memory_to_string),
  ((SCHEME_OBJECT EXFUN ((*), ())) memory_to_symbol),
  ((SCHEME_OBJECT EXFUN ((*), ())) make_vector),
  ((SCHEME_OBJECT EXFUN ((*), ())) cons),
  ((SCHEME_OBJECT EXFUN ((*), ())) rconsm),
  ((SCHEME_OBJECT EXFUN ((*), ())) double_to_flonum),
  ((SCHEME_OBJECT EXFUN ((*), ())) long_to_integer),
  ((SCHEME_OBJECT EXFUN ((*), ())) digit_string_to_integer),
  ((SCHEME_OBJECT EXFUN ((*), ())) digit_string_to_bit_string),
  ((SCHEME_OBJECT EXFUN ((*), ())) make_primitive),
  ((SCHEME_OBJECT EXFUN ((*), ())) memory_to_uninterned_symbol),
};

#endif /* BUG_GCC_LONG_CALLS */

extern char * interface_to_C_hook;
extern long C_return_value, MAX_TRAMPOLINE;
extern void EXFUN (C_to_interface, (PTR));
extern void EXFUN (interface_initialize, (void));
extern SCHEME_OBJECT EXFUN (initialize_C_compiled_block, (int, char *));
extern int EXFUN (initialize_compiled_code_blocks, (void));
extern void * scheme_hooks_low, * scheme_hooks_high;

#define TRAMPOLINE_FUDGE 20

typedef SCHEME_OBJECT * EXFUN ((* code_block),
			       (SCHEME_OBJECT *, entry_count_t));

typedef SCHEME_OBJECT * EXFUN ((* data_block), (entry_count_t));

typedef SCHEME_OBJECT EXFUN ((* data_generator), (void));

typedef void EXFUN ((* uninit_data), (void));

struct compiled_entry_s
{
  code_block code;		/* C handler for this entry point */
  entry_count_t dispatch;	/* Internal dispatch tag */
};

#define COMPILED_BLOCK_FLAG_DATA_ONLY		1

struct compiled_block_s
{
  char * name;
  union
  {
    uninit_data errgen;		/* When not initialized yet */
    data_block constructor;	/* Data handler for this compiled block */
    data_generator builder;	/* Data generator for data-only cc blocks */
  } data;
  entry_count_t nentries;	/* Number of entry points in this block */
  entry_count_t dispatch;	/* Base of dispatch for this block */
  unsigned flags;
};

int pc_zero_bits;
static SCHEME_OBJECT
  dummy_entry = ((SCHEME_OBJECT) -1L);
char *
  interface_to_C_hook = ((char *) & dummy_entry);
void
  * scheme_hooks_low = NULL,
  * scheme_hooks_high = NULL;

#define PSEUDO_STATIC

PSEUDO_STATIC long
  initial_entry_number = -1;
PSEUDO_STATIC entry_count_t
  max_compiled_entries = 0,
  compiled_entries_size = 0;
PSEUDO_STATIC struct compiled_entry_s *
  compiled_entries = ((struct compiled_entry_s *) NULL);

PSEUDO_STATIC entry_count_t
  max_compiled_blocks = 0,
  compiled_blocks_table_size = 0;
PSEUDO_STATIC struct compiled_block_s *
  compiled_blocks_table = ((struct compiled_block_s *) NULL);
PSEUDO_STATIC tree_node
  compiled_blocks_tree = ((tree_node) NULL);

SCHEME_OBJECT *
DEFUN (trampoline_procedure, (trampoline, dispatch),
       SCHEME_OBJECT * trampoline AND entry_count_t dispatch)
{
  return (invoke_utility (((int) (* ((unsigned long *) trampoline))),
			  ((long) (TRAMPOLINE_STORAGE (trampoline))),
			  0, 0, 0));
}

int
DEFUN_VOID (NO_SUBBLOCKS)
{
  return (0);
}

SCHEME_OBJECT *
DEFUN (no_data, (base_dispatch), entry_count_t base_dispatch)
{
  return ((SCHEME_OBJECT *) NULL);
}

void
DEFUN_VOID (uninitialized_data)
{
  /* Not yet assigned.  Cannot construct data. */
  error_external_return ();
  /*NOTREACHED*/
}

SCHEME_OBJECT *
DEFUN (unspecified_code, (entry, dispatch),
       SCHEME_OBJECT * entry AND entry_count_t dispatch)
{
  exp_register = ((SCHEME_OBJECT) entry);
  C_return_value = (ERR_EXECUTE_MANIFEST_VECTOR);
  return (&dummy_entry);
}

extern PTR EXFUN (malloc, (unsigned long));
extern PTR EXFUN (realloc, (PTR, unsigned long));

PTR
DEFUN (lrealloc, (ptr, size), PTR ptr AND unsigned long size)
{
  if (ptr == ((PTR) NULL))
    return (malloc (size));
  else
    return (realloc (ptr, size));
}

int
DEFUN (declare_trampoline_block, (nentries), entry_count_t nentries)
{
  int result;

  result = (declare_compiled_code ("#trampoline_code_block",
				   nentries,
				   NO_SUBBLOCKS,
				   trampoline_procedure));
#if 0
  /* trampoline block is special. */

  if (result != 0)
    return (result);

  result = (declare_compiled_data ("#trampoline_code_block",
				   NO_SUBBLOCKS,
				   no_data));
#endif
  return (result);
}

void
DEFUN_VOID (interface_initialize)
{
  int i, pow, del;
  
  for (i = 0, pow = 1, del = ((sizeof (SCHEME_OBJECT)) / (sizeof (char)));
       pow < del; i+= 1)
    pow = (pow << 1);
  
  if (pow != del)
  {
    /* Not a power of two -- ill-defined pc_zero_bits. */
    outf_fatal ("interface_initialize: bad (sizeof (SCHEME_OBJECT)).\n");
    Microcode_Termination (TERM_EXIT);
  }
  pc_zero_bits = i;  

  if (initial_entry_number == -1)
    initial_entry_number = (MAX_TRAMPOLINE + TRAMPOLINE_FUDGE);

  if (((declare_trampoline_block (initial_entry_number)) != 0)
      || (initialize_compiled_code_blocks ()) != 0)
  {
    if (Registers[REGBLOCK_PRIMITIVE] != SHARP_F)
      signal_error_from_primitive (ERR_FASLOAD_COMPILED_MISMATCH);
    else
    {
      outf_fatal ("interface_initialize: error initializing compiled code.\n");
      Microcode_Termination (TERM_EXIT);
    }
  }
  return;
}

entry_count_t
DEFUN (find_compiled_block, (name), char * name)
{
  tree_node node = (tree_lookup (compiled_blocks_tree, name));

  if (node == ((tree_node) NULL))
    return (max_compiled_blocks);
  else
    return (node->value);
}

int
DEFUN (declare_compiled_data,
       (name, decl_data, data_proc),
       char * name
       AND int EXFUN ((* decl_data), (void))
       AND SCHEME_OBJECT * EXFUN ((* data_proc), (entry_count_t)))
{
  entry_count_t slot = (find_compiled_block (name));

  if (slot == max_compiled_blocks)
    return (-1);
  
  if ((compiled_blocks_table[slot].data.errgen != uninitialized_data)
      && (compiled_blocks_table[slot].data.constructor != data_proc))
    return (-1);

  compiled_blocks_table[slot].flags &= (~ COMPILED_BLOCK_FLAG_DATA_ONLY);
  compiled_blocks_table[slot].data.constructor = data_proc;
  return (* decl_data) ();  
}

SCHEME_OBJECT
DEFUN (initialize_subblock, (name), char * name)
{
  SCHEME_OBJECT * ep, * block;
  entry_count_t slot = (find_compiled_block (name));

  if ((slot == max_compiled_blocks)
      || ((compiled_blocks_table[slot].flags & COMPILED_BLOCK_FLAG_DATA_ONLY)
	  != 0))
    error_external_return ();

  ep = ((* compiled_blocks_table[slot].data.constructor)
	(compiled_blocks_table[slot].dispatch));
  Get_Compiled_Block (block, ep);
  return (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block));
}

SCHEME_OBJECT
DEFUN (initialize_C_compiled_block, (argno, name),
       int argno AND char * name)
{
  SCHEME_OBJECT val;
  entry_count_t slot;

  slot = (find_compiled_block (name));
  if (slot == max_compiled_blocks)
    return (SHARP_F);

  if ((compiled_blocks_table[slot].flags & COMPILED_BLOCK_FLAG_DATA_ONLY) != 0)
    val = ((* compiled_blocks_table[slot].data.builder) ());
  else
  {
    SCHEME_OBJECT * block;

    block = ((* compiled_blocks_table[slot].data.constructor)
	     (compiled_blocks_table[slot].dispatch));
    val =  (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY, block));
  }
  return (val);
}

int
DEFUN (declare_compiled_code,
       (name, nentries, decl_code, code_proc),
       char * name
       AND entry_count_t nentries
       AND int EXFUN ((* decl_code), (void))
       AND code_block code_proc)
{
  entry_count_t slot = (find_compiled_block (name));

  if (slot != max_compiled_blocks)
  {
    code_block old_code;

    old_code = (compiled_entries[compiled_blocks_table[slot].dispatch].code);
    if (((old_code != unspecified_code)
	 && (old_code != code_proc)
	 && (code_proc != unspecified_code))
	|| (compiled_blocks_table[slot].nentries != nentries))
      return (-1);
    if (old_code == unspecified_code)
    {
      entry_count_t counter, limit;

      counter = compiled_blocks_table[slot].dispatch;
      limit = (counter + nentries);
      while (counter < limit)
	compiled_entries[counter++].code = code_proc;
    }
  }
  else
  {
    entry_count_t dispatch = max_compiled_entries;
    entry_count_t n_dispatch = (dispatch + nentries);
    entry_count_t block_index = max_compiled_blocks;

    if (n_dispatch < dispatch)
      /* Wrap around */
      return (-1);
    
    if (n_dispatch >= compiled_entries_size)
    {
      struct compiled_entry_s * new_entries;
      entry_count_t new_entries_size = ((compiled_entries_size == 0)
					? 100
					: ((compiled_entries_size * 3) / 2));
      if (new_entries_size <= n_dispatch)
	new_entries_size = (n_dispatch + 1);

      new_entries = ((struct compiled_entry_s *)
		     (lrealloc (compiled_entries,
				(new_entries_size
				 * (sizeof (struct compiled_entry_s))))));
      if (new_entries == ((struct compiled_entry_s *) NULL))
	return (-1);
      compiled_entries_size = new_entries_size;
      compiled_entries = new_entries;
    }

    if (block_index >= compiled_blocks_table_size)
    {
      struct compiled_block_s * new_blocks;
      entry_count_t new_blocks_size
	= ((compiled_blocks_table_size == 0)
	   ? 10
	   : ((compiled_blocks_table_size * 3) / 2));
      new_blocks = ((struct compiled_block_s *)
		    (lrealloc (compiled_blocks_table,
			       (new_blocks_size
				* (sizeof (struct compiled_block_s))))));
      if (new_blocks == ((struct compiled_block_s *) NULL))
	return (-1);
      compiled_blocks_table_size = new_blocks_size;
      compiled_blocks_table = new_blocks;
    }

    {
      tree_node new_tree;

      tree_error_message = ((char *) NULL);
      new_tree = (tree_insert (compiled_blocks_tree, name, block_index));
      if (tree_error_message != ((char *) NULL))
	return (-1);
      compiled_blocks_tree = new_tree;
    }

    max_compiled_entries = n_dispatch;
    max_compiled_blocks = (block_index + 1);
  
    compiled_blocks_table[block_index].name = name;
    compiled_blocks_table[block_index].flags = 0;
    compiled_blocks_table[block_index].data.errgen = uninitialized_data;
    compiled_blocks_table[block_index].nentries = nentries;
    compiled_blocks_table[block_index].dispatch = dispatch;

    for (block_index = dispatch; block_index < n_dispatch; block_index++)
    {
      compiled_entries[block_index].code = code_proc;
      compiled_entries[block_index].dispatch = dispatch;
    }
  }
  return (* decl_code) ();
}

int
DEFUN (declare_data_object,
       (name, data_proc),
       char * name
       AND SCHEME_OBJECT EXFUN ((* data_proc), (void)))
{
  entry_count_t slot;

  slot = (find_compiled_block (name));
  if (slot == max_compiled_blocks)
  {
    declare_compiled_code (name, 0, NO_SUBBLOCKS, unspecified_code);
    slot = (find_compiled_block (name));
    if (slot == max_compiled_blocks)
      return (-1);
  }
  
  if ((compiled_blocks_table[slot].data.errgen != uninitialized_data)
      && (compiled_blocks_table[slot].data.builder != data_proc))
    return (-1);

  compiled_blocks_table[slot].flags |= (COMPILED_BLOCK_FLAG_DATA_ONLY);
  compiled_blocks_table[slot].data.builder = data_proc;

  return (0);
}

int
DEFUN (declare_compiled_code_mult, (nslots, slots),
       unsigned nslots AND CONST struct liarc_code_S * slots)
{
  unsigned i;
  int res = 0;

  for (i = 0; (i < nslots); i++)
  {
    res = (declare_compiled_code (((char *) (slots[i].name)),
				  (slots[i].nentries),
				  NO_SUBBLOCKS,
				  (slots[i].code)));
    if (res != 0)
      break;
  }
  return (res);
}

int
DEFUN (declare_compiled_data_mult, (nslots, slots),
       unsigned nslots AND CONST struct liarc_data_S * slots)
{
  unsigned i;
  int res = 0;

  for (i = 0; (i < nslots); i++)
  {
    res = (declare_compiled_data (((char *) (slots[i].name)),
				  NO_SUBBLOCKS,
				  (slots[i].data)));
    if (res != 0)
      break;
  }
  return (res);
}

/* For now */

extern SCHEME_OBJECT
  * EXFUN (cons_c_code_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *));

extern Boolean
  EXFUN (install_c_code_table, (SCHEME_OBJECT *, long));

static SCHEME_OBJECT *
DEFUN (copy_c_code_block_information, (index, start, limit),
       long index AND SCHEME_OBJECT * start AND SCHEME_OBJECT * limit)
{
  long char_count;
  char * src, * dest;

  if (start < limit)
    *start++
      = (LONG_TO_UNSIGNED_FIXNUM (compiled_blocks_table[index].nentries));
  
  src = compiled_blocks_table[index].name;
  dest = ((char *) start);

  while ((dest < ((char *) limit)) && ((*dest++ = *src++) != '\0'))
    ;
  if (dest >= ((char *) limit))
    while (*src++ != '\0')
      dest += 1;
  
  char_count = (dest - ((char *) start));
  return (start + (BYTES_TO_WORDS (dest - ((char *) start))));
}

SCHEME_OBJECT *
DEFUN (cons_c_code_table, (start, limit, length),
       SCHEME_OBJECT * start AND SCHEME_OBJECT * limit AND long * length)
{
  long count;

  * length = max_compiled_blocks;

  if (start < limit)
    *start++ = (LONG_TO_FIXNUM (initial_entry_number));

  for (count = 0; ((count < max_compiled_blocks) && (start < limit)); count++)
    start = (copy_c_code_block_information (count, start, limit));

  return (start);
}

Boolean
DEFUN (install_c_code_table, (table, length),
       SCHEME_OBJECT * table AND long length)
{
  SCHEME_OBJECT the_fixnum;
  long count, dumped_initial_entry_number;

  the_fixnum = *table++;
  dumped_initial_entry_number = (FIXNUM_TO_LONG (the_fixnum));
  if (dumped_initial_entry_number < MAX_TRAMPOLINE)
    return (false);
  initial_entry_number = dumped_initial_entry_number;

  if (compiled_entries != ((struct compiled_entry_s *) NULL))
    free (compiled_entries);
  if (compiled_blocks_table != ((struct compiled_block_s *) NULL))
    free (compiled_blocks_table);
  if (compiled_blocks_tree != ((tree_node) NULL))
    tree_free (compiled_blocks_tree);
  
  max_compiled_entries = 0;
  compiled_entries_size = 0;
  compiled_entries = ((struct compiled_entry_s *) NULL);
  max_compiled_blocks = 0;
  compiled_blocks_table_size = 0;
  compiled_blocks_table = ((struct compiled_block_s *) NULL);
  compiled_blocks_tree = ((tree_node) NULL);
  
  if ((declare_trampoline_block (initial_entry_number)) != 0)
    return (false);

  for (count = 0; count < length; count++)
  {
    long nentries = (UNSIGNED_FIXNUM_TO_LONG (* table++));
    int nlen = (strlen ((char *) table));
    char * ncopy = ((char *) (malloc (nlen + 1)));

    if (ncopy == ((char *) NULL))
      return (false);
    strcpy (ncopy, ((char *) table));
    if ((declare_compiled_code (ncopy,
				nentries,
				NO_SUBBLOCKS,
				unspecified_code))
	!= 0)
      return (false);
    table += (BYTES_TO_WORDS (nlen + 1));
  }

  return (true);
}

#define C_COUNT_TRANSFERS
unsigned long c_to_interface_transfers = 0;

void
DEFUN (C_to_interface, (in_entry), PTR in_entry)
{
  SCHEME_OBJECT * entry = ((SCHEME_OBJECT *) in_entry);

  while (1)
  {
    unsigned long entry_index = (* ((unsigned long *) entry));

#ifdef C_COUNT_TRANSFERS
    c_to_interface_transfers += 1;
#endif /* C_COUNT_TRANSFERS */

    if (entry_index < ((unsigned long) max_compiled_entries))
      entry = ((* (compiled_entries[entry_index].code))
	       (entry, compiled_entries[entry_index].dispatch));
    else
    {
      if (entry != &dummy_entry)
      {
	exp_register = ((SCHEME_OBJECT) entry);
	C_return_value = (ERR_EXECUTE_MANIFEST_VECTOR);
      }
      return;
    }
  }
}

DEFINE_PRIMITIVE ("SWAP-C-COUNTER!", Prim_swap_c_counter, 1, 1,
		  "(new-value)\n\
Set the C transfer counter to new-value.  Return the old value.")
{
  unsigned long new_counter, old_counter;
  PRIMITIVE_HEADER (1);

  new_counter = (arg_integer (1));
  old_counter = c_to_interface_transfers;
  c_to_interface_transfers = new_counter;
  PRIMITIVE_RETURN (ulong_to_integer (old_counter));
}

typedef SCHEME_OBJECT * utility_result;

typedef void EXFUN
  ((* utility_table_entry), (utility_result *, long, long, long, long));

extern utility_table_entry utility_table[];

SCHEME_OBJECT *
DEFUN (invoke_utility, (code, arg1, arg2, arg3, arg4),
       int code AND long arg1 AND long arg2 AND long arg3 AND long arg4)
{
  utility_result res;

  (* utility_table[code]) ((& res), arg1, arg2, arg3, arg4);

  return ((SCHEME_OBJECT *) res);
}

int
DEFUN (multiply_with_overflow, (x, y, res), long x AND long y AND long * res)
{
  extern SCHEME_OBJECT EXFUN (Mul, (SCHEME_OBJECT, SCHEME_OBJECT));
  SCHEME_OBJECT ans;
  
  ans = (Mul ((LONG_TO_FIXNUM (x)), (LONG_TO_FIXNUM (y))));
  if (ans == SHARP_F)
  {
    /* Bogus... */
    (* res) = (x * y);
    return (1);
  }
  else
  {
    (* res) = (FIXNUM_TO_LONG (ans));
    return (0);
  }
}

static unsigned int
DEFUN (hex_digit_to_int, (h_digit), unsigned char h_digit)
{
  unsigned int digit = ((unsigned int) h_digit);

  return (((digit >= '0') && (digit <= '9'))
	  ? (digit - '0')
	  : (((digit >= 'A') && (digit <= 'F'))
	     ? ((digit - 'A') + 10)
	     : ((digit - 'a') + 10)));
}

SCHEME_OBJECT
DEFUN (digit_string_to_bit_string, (n_bits, n_digits, digits),
       unsigned long n_bits
       AND unsigned long n_digits
       AND unsigned char * digits)
{
  extern void EXFUN (clear_bit_string, (SCHEME_OBJECT));
  extern SCHEME_OBJECT EXFUN (allocate_bit_string, (long));
  extern void EXFUN (bit_string_set, (SCHEME_OBJECT, long, int));
  SCHEME_OBJECT result = (allocate_bit_string ((long) n_bits));
  unsigned int digit, mask;
  long i, posn;
  int j;

  posn = 0;
  clear_bit_string (result);

  for (i = 0; i < ((long) n_digits); i++)
  {
    digit = (hex_digit_to_int (*digits++));
    for (j = 0, mask = 1;
	 j < 4;
	 j++, mask = (mask << 1), posn++)
      if ((digit & mask) != 0)
	bit_string_set (result, posn, 1);
  }
  return (result);
}

/* This avoids consing the string and symbol if it already exists. */

SCHEME_OBJECT
DEFUN (memory_to_uninterned_symbol, (length, string),
       unsigned long length AND unsigned char * string)
{
  SCHEME_OBJECT name = (memory_to_string (length, string));
  SCHEME_OBJECT res = (CONS (name, UNBOUND_OBJECT));
  return (OBJECT_NEW_TYPE (TC_UNINTERNED_SYMBOL, res));
}

static unsigned int
DEFUN (digit_string_producer, (digit_ptr), PTR v_digit_ptr)
{
  char ** digit_ptr = ((char **) v_digit_ptr);
  char digit = ** digit_ptr;
  * digit_ptr = ((* digit_ptr) + 1);
  return (hex_digit_to_int (digit));
}

SCHEME_OBJECT
DEFUN (digit_string_to_integer, (negative_p, n_digits, digits),
       Boolean negative_p
       AND unsigned long n_digits
       AND unsigned char * digits)
{
  SCHEME_OBJECT bignum;
  unsigned char * digit = digits;
  extern SCHEME_OBJECT EXFUN (bignum_to_integer, (SCHEME_OBJECT));

  bignum = (digit_stream_to_bignum (((int) n_digits),
				    digit_string_producer,
				    ((PTR) & digit),
				    16,
				    ((int) negative_p)));

  return (bignum_to_integer (bignum));
}

#ifdef USE_STDARG

SCHEME_OBJECT
DEFUN (rconsm, (nargs, tail DOTS),
       int nargs AND SCHEME_OBJECT tail DOTS)
{
  va_list arg_ptr;
  va_start (arg_ptr, tail);

  {
    int i;
    SCHEME_OBJECT result = tail;

    for (i = 1; i < nargs; i++)
      result = (cons ((va_arg (arg_ptr, SCHEME_OBJECT)),
		      result));

    va_end (arg_ptr);
    return (result);
  }
}

#else /* not USE_STDARG */

SCHEME_OBJECT
rconsm (va_alist)
va_dcl
{
  va_list arg_ptr;
  int nargs;
  SCHEME_OBJECT tail;

  va_start (arg_ptr);
  nargs = (va_arg (arg_ptr, int));
  tail = (va_arg (arg_ptr, SCHEME_OBJECT));
  
  {
    int i;
    SCHEME_OBJECT result = tail;

    for (i = 1; i < nargs; i++)
      result = (cons ((va_arg (arg_ptr, SCHEME_OBJECT)),
		      result));

    va_end (arg_ptr);
    return (result);
  }
}

#endif /* USE_STDARG */
