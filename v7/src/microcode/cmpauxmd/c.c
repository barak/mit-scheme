/* -*-C-*-

$Id: c.c,v 1.22 2007/04/22 16:31:24 cph Exp $

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

#define LIARC_IN_MICROCODE
#include "liarc.h"
#include "prims.h"
#include "bignum.h"
#include "bitstr.h"
#include "avltree.h"

extern int initialize_compiled_code_blocks (void);

#ifdef BUG_GCC_LONG_CALLS

extern SCHEME_OBJECT memory_to_string (unsigned long, const void *);
extern SCHEME_OBJECT memory_to_symbol (long, const void *);
extern SCHEME_OBJECT make_vector (long, SCHEME_OBJECT, bool);
extern SCHEME_OBJECT cons (SCHEME_OBJECT, SCHEME_OBJECT);
extern SCHEME_OBJECT double_to_flonum (double);
extern SCHEME_OBJECT long_to_integer (long);
extern SCHEME_OBJECT digit_string_to_integer
  (bool, unsigned long, const char *);
extern SCHEME_OBJECT digit_string_to_bit_string
  (unsigned long, unsigned long, const char *);
extern SCHEME_OBJECT make_primitive (char *, int);
extern SCHEME_OBJECT memory_to_uninterned_symbol (unsigned long, const void *);

SCHEME_OBJECT (* (constructor_kludge [11])) () =
{
  ((SCHEME_OBJECT (*) ()) memory_to_string),
  ((SCHEME_OBJECT (*) ()) memory_to_symbol),
  ((SCHEME_OBJECT (*) ()) make_vector),
  ((SCHEME_OBJECT (*) ()) cons),
  ((SCHEME_OBJECT (*) ()) rconsm),
  ((SCHEME_OBJECT (*) ()) double_to_flonum),
  ((SCHEME_OBJECT (*) ()) long_to_integer),
  ((SCHEME_OBJECT (*) ()) digit_string_to_integer),
  ((SCHEME_OBJECT (*) ()) digit_string_to_bit_string),
  ((SCHEME_OBJECT (*) ()) make_primitive),
  ((SCHEME_OBJECT (*) ()) memory_to_uninterned_symbol),
};

#endif /* BUG_GCC_LONG_CALLS */

static SCHEME_OBJECT dummy_entry = ((SCHEME_OBJECT) -1L);
utility_result_t interface_to_C_hook = ((utility_result_t) (&dummy_entry));

#define TRAMPOLINE_FUDGE 20

typedef struct
{
  const char * name;
  liarc_code_proc_t * code_proc; /* C handler for this entry point */
  void * data_proc;		/* Data handler for this compiled block */
  entry_count_t first_entry;	/* Base of dispatch for this block */
  entry_count_t n_entries;	/* Number of entry points in this block */
  unsigned int flags;
} compiled_block_t;

static entry_count_t n_compiled_blocks = 0;
static entry_count_t compiled_blocks_table_size = 0;
static compiled_block_t * compiled_blocks = 0;
static tree_node compiled_blocks_tree = 0;

static long initial_entry_number = (-1);
static entry_count_t n_compiled_entries = 0;
static entry_count_t compiled_entries_size = 0;
static compiled_block_t ** compiled_entries = 0;

#define COMPILED_BLOCK_NAME(block) ((block) -> name)
#define COMPILED_BLOCK_CODE_PROC(block) ((block) -> code_proc)
#define _COMPILED_BLOCK_DATA_PROC(block) ((block) -> data_proc)
#define COMPILED_BLOCK_FIRST_ENTRY(block) ((block) -> first_entry)
#define COMPILED_BLOCK_N_ENTRIES(block) ((block) -> n_entries)
#define COMPILED_BLOCK_FLAGS(block) ((block) -> flags)

#define COMPILED_BLOCK_DATA_PROC(block)					\
  ((liarc_data_proc_t *) (_COMPILED_BLOCK_DATA_PROC (block)))

#define SET_COMPILED_BLOCK_DATA_PROC(block, proc) do			\
{									\
  _CBFS (block, _CBF_DATA_INIT);					\
  _CBFC (block, _CBF_DATA_ONLY);					\
  (_COMPILED_BLOCK_DATA_PROC (block)) = (proc);				\
} while (false)

#define COMPILED_BLOCK_OBJECT_PROC(block)				\
  ((liarc_object_proc_t *) (_COMPILED_BLOCK_DATA_PROC (block)))

#define SET_COMPILED_BLOCK_OBJECT_PROC(block, proc) do			\
{									\
  _CBFS (block, (_CBF_DATA_INIT | _CBF_DATA_ONLY));			\
  (_COMPILED_BLOCK_DATA_PROC (block)) = (proc);				\
} while (false)

#define _CBFT(block, flag) (((COMPILED_BLOCK_FLAGS (block)) & (flag)) != 0)
#define _CBFS(block, flag) ((COMPILED_BLOCK_FLAGS (block)) |= (flag))
#define _CBFC(block, flag) ((COMPILED_BLOCK_FLAGS (block)) &=~ (flag))

#define _CBF_DATA_ONLY 0x01
#define _CBF_DATA_INIT 0x02

#define COMPILED_BLOCK_DATA_ONLY_P(block) (_CBFT (block, _CBF_DATA_ONLY))
#define COMPILED_BLOCK_DATA_INIT_P(block) (_CBFT (block, _CBF_DATA_INIT))

static bool grow_compiled_blocks (void);
static bool grow_compiled_entries (entry_count_t);
static int declare_trampoline_block (entry_count_t);
static SCHEME_OBJECT * trampoline_procedure (SCHEME_OBJECT *, entry_count_t);
static compiled_block_t * find_compiled_block (const char *);
static SCHEME_OBJECT * unspecified_code (SCHEME_OBJECT *, entry_count_t);
static void * lrealloc (void *, size_t);
static unsigned int digit_string_producer (void *);
static unsigned int hex_digit_to_int (char);

long C_return_value;

long
C_to_interface (SCHEME_OBJECT * entry)
{
  while (entry != 0)
    {
      entry_count_t index = ((entry_count_t) (*entry));
      compiled_block_t * block;

      if (index >= n_compiled_entries)
	{
	  SET_EXP ((SCHEME_OBJECT) entry);
	  return (ERR_EXECUTE_MANIFEST_VECTOR);
	}
      block = (compiled_entries[index]);
      entry = ((* (COMPILED_BLOCK_CODE_PROC (block)))
	       (entry, (COMPILED_BLOCK_FIRST_ENTRY (block))));
    }
  return (C_return_value);
}

SCHEME_OBJECT *
invoke_utility (unsigned int code,
		unsigned long arg1, unsigned long arg2,
		unsigned long arg3, unsigned long arg4)
{
  SCHEME_OBJECT * res;
  (* (utility_table[code])) ((&res), arg1, arg2, arg3, arg4);
  return (res);
}

void
initialize_C_interface (void)
{
  if (initial_entry_number == (-1))
    /* TRAMPOLINE_FUDGE allows for future growth of max_trampoline.  */
    initial_entry_number = (max_trampoline + TRAMPOLINE_FUDGE);

  if (! (((declare_trampoline_block (initial_entry_number)) == 0)
	 && ((initialize_compiled_code_blocks ()) == 0)))
    {
      if (GET_PRIMITIVE != SHARP_F)
	signal_error_from_primitive (ERR_FASLOAD_COMPILED_MISMATCH);
      outf_fatal ("error initializing compiled code.\n");
      Microcode_Termination (TERM_EXIT);
    }
}

SCHEME_OBJECT
initialize_C_compiled_block (int argno, const char * name)
{
  compiled_block_t * block = (find_compiled_block (name));
  return
    ((block == 0)
     ? SHARP_F
     : (COMPILED_BLOCK_DATA_ONLY_P (block))
     ? ((* (COMPILED_BLOCK_OBJECT_PROC (block))) ())
     : (MAKE_CC_ENTRY ((* (COMPILED_BLOCK_DATA_PROC (block)))
		       (COMPILED_BLOCK_FIRST_ENTRY (block)))));
}

SCHEME_OBJECT
initialize_subblock (const char * name)
{
  compiled_block_t * block = (find_compiled_block (name));
  if ((block == 0) || (COMPILED_BLOCK_DATA_ONLY_P (block)))
    error_external_return ();

  return
    (MAKE_CC_BLOCK
     (cc_entry_address_to_block_address
      ((* (COMPILED_BLOCK_DATA_PROC (block)))
       (COMPILED_BLOCK_FIRST_ENTRY (block)))));
}

unsigned long
c_code_table_export_length (unsigned long * n_blocks_r)
{
  compiled_block_t * block = compiled_blocks;
  compiled_block_t * end = (block + n_compiled_blocks);
  unsigned long n = 1;

  while (block < end)
    {
      n += (1 + (BYTES_TO_WORDS ((strlen (COMPILED_BLOCK_NAME (block))) + 1)));
      block += 1;
    }
  (*n_blocks_r) = n_compiled_blocks;
  return (n);
}

void
export_c_code_table (SCHEME_OBJECT * start)
{
  compiled_block_t * block = compiled_blocks;
  compiled_block_t * end = (block + n_compiled_blocks);

  (*start++) = (LONG_TO_FIXNUM (initial_entry_number));
  while (block < end)
    {
      (*start++) = (LONG_TO_UNSIGNED_FIXNUM (COMPILED_BLOCK_N_ENTRIES (block)));
      strcpy (((char *) start), (COMPILED_BLOCK_NAME (block)));
      start += (BYTES_TO_WORDS ((strlen (COMPILED_BLOCK_NAME (block))) + 1));
      block += 1;
    }
}

bool
import_c_code_table (SCHEME_OBJECT * table, unsigned long n_blocks)
{
  long dumped_initial_entry_number = (FIXNUM_TO_LONG (*table++));
  unsigned long count;

  if (dumped_initial_entry_number < max_trampoline)
    return (false);
  initial_entry_number = dumped_initial_entry_number;

  if (compiled_entries != 0)
    free (compiled_entries);
  if (compiled_blocks != 0)
    free (compiled_blocks);
  if (compiled_blocks_tree != 0)
    tree_free (compiled_blocks_tree);
  
  n_compiled_blocks = 0;
  compiled_blocks_table_size = 0;
  compiled_blocks = 0;
  compiled_blocks_tree = 0;

  n_compiled_entries = 0;
  compiled_entries_size = 0;
  compiled_entries = 0;

  if ((declare_trampoline_block (initial_entry_number)) != 0)
    return (false);

  for (count = 0; (count < n_blocks); count += 1)
    {
      unsigned long n_entries = (FIXNUM_TO_ULONG (*table++));
      size_t nb = ((strlen ((const char *) table)) + 1);
      char * ncopy = (malloc (nb));

      if (ncopy == 0)
	return (false);
      strcpy (ncopy, ((const char *) table));
      if ((declare_compiled_code_ns (ncopy, n_entries, unspecified_code)) != 0)
	return (false);
      table += (BYTES_TO_WORDS (nb));
    }

  return (true);
}

int
declare_compiled_code_ns (const char * name,
			  entry_count_t n_block_entries,
			  liarc_code_proc_t * code_proc)
{
  compiled_block_t * block = (find_compiled_block (name));
  if (block == 0)
    {
      entry_count_t entries_start = n_compiled_entries;
      entry_count_t entries_end = (entries_start + n_block_entries);
      tree_node new_tree;

      if (! ((entries_start <= entries_end)
	     && ((n_compiled_blocks < compiled_blocks_table_size)
		 || (grow_compiled_blocks ()))
	     && ((entries_end < compiled_entries_size)
		 || (grow_compiled_entries (entries_end)))))
	return (-1);

      tree_error_message = 0;
      new_tree = (tree_insert (compiled_blocks_tree, name, n_compiled_blocks));
      if (tree_error_message != 0)
	return (-1);
      compiled_blocks_tree = new_tree;

      block = (compiled_blocks + (n_compiled_blocks++));
      (COMPILED_BLOCK_NAME (block)) = name;
      (COMPILED_BLOCK_CODE_PROC (block)) = code_proc;
      (_COMPILED_BLOCK_DATA_PROC (block)) = 0;
      (COMPILED_BLOCK_FIRST_ENTRY (block)) = entries_start;
      (COMPILED_BLOCK_N_ENTRIES (block)) = n_block_entries;
      (COMPILED_BLOCK_FLAGS (block)) = 0;

      while (n_compiled_entries < entries_end)
	(compiled_entries[n_compiled_entries++]) = block;
      return (0);
    }
  else if ((((COMPILED_BLOCK_CODE_PROC (block)) == unspecified_code)
	    || ((COMPILED_BLOCK_CODE_PROC (block)) == code_proc)
	    || (code_proc == unspecified_code))
	   && ((COMPILED_BLOCK_N_ENTRIES (block)) == n_block_entries))
    {
      (COMPILED_BLOCK_CODE_PROC (block)) = code_proc;
      return (0);
    }
  else
    return (-1);
}

static bool
grow_compiled_blocks (void)
{
  entry_count_t new_blocks_size
    = ((compiled_blocks_table_size == 0)
       ? 16
       : (compiled_blocks_table_size * 2));
  compiled_block_t * new_blocks
    = (lrealloc (compiled_blocks,
		 (new_blocks_size * (sizeof (compiled_block_t)))));
  if (new_blocks == 0)
    return (false);
  if (new_blocks != compiled_blocks)
    {
      compiled_block_t ** scan = compiled_entries;
      compiled_block_t ** end = (scan + n_compiled_entries);
      while (scan < end)
	{
	  (*scan) = (((*scan) - compiled_blocks) + new_blocks);
	  scan += 1;
	}
    }
  compiled_blocks_table_size = new_blocks_size;
  compiled_blocks = new_blocks;
  return (true);
}

static bool
grow_compiled_entries (entry_count_t entries_end)
{
  entry_count_t new_entries_size
    = ((compiled_entries_size == 0)
       ? 128
       : compiled_entries_size);
  compiled_block_t ** new_entries;

  while (new_entries_size <= entries_end)
    new_entries_size *= 2;
  new_entries
    = (lrealloc (compiled_entries,
		 (new_entries_size * (sizeof (compiled_block_t *)))));
  if (new_entries == 0)
    return (false);
  compiled_entries_size = new_entries_size;
  compiled_entries = new_entries;
  return (true);
}

int
declare_compiled_code (const char * name,
		       entry_count_t n_block_entries,
		       liarc_decl_code_t * decl_code,
		       liarc_code_proc_t * code_proc)
{
  int rc = (declare_compiled_code_ns (name, n_block_entries, code_proc));
  return ((rc == 0) ? ((*decl_code) ()) : rc);
}

int
declare_compiled_data_ns (const char * name, liarc_data_proc_t * data_proc)
{
  compiled_block_t * block = (find_compiled_block (name));
  if ((block == 0)
      || ((COMPILED_BLOCK_DATA_INIT_P (block))
	  && ((COMPILED_BLOCK_DATA_PROC (block)) != data_proc)))
    return (-1);
  SET_COMPILED_BLOCK_DATA_PROC (block, data_proc);
  return (0);
}

int
declare_compiled_data (const char * name,
		       liarc_decl_data_t * decl_data,
		       liarc_data_proc_t * data_proc)
{
  int rc = (declare_compiled_data_ns (name, data_proc));
  return ((rc == 0) ? ((*decl_data) ()) : rc);
}

int
declare_data_object (const char * name, liarc_object_proc_t * object_proc)
{
  compiled_block_t * block = (find_compiled_block (name));
  if (block == 0)
    {
      declare_compiled_code_ns (name, 0, unspecified_code);
      block = (find_compiled_block (name));
      if (block == 0)
	return (-1);
    }
  
  if ((COMPILED_BLOCK_DATA_INIT_P (block))
      && ((COMPILED_BLOCK_OBJECT_PROC (block)) != object_proc))
    return (-1);

  SET_COMPILED_BLOCK_OBJECT_PROC (block, object_proc);
  return (0);
}

int
declare_compiled_code_mult (unsigned int nslots,
			    const struct liarc_code_S * slots)
{
  unsigned int i = 0;
  while (i < nslots)
    {
      int res = (declare_compiled_code_ns (((char *) ((slots[i]) . name)),
					   ((slots[i]) . nentries),
					   ((slots[i]) . code)));
      if (res != 0)
	return (res);
      i += 1;
    }
  return (0);
}

int
declare_compiled_data_mult (unsigned int nslots,
			    const struct liarc_data_S * slots)
{
  unsigned int i = 0;
  while (i < nslots)
    {
      int res = (declare_compiled_data_ns (((char *) ((slots[i]) . name)),
					   ((slots[i]) . data)));
      if (res != 0)
	return (res);
      i += 1;
    }
  return (0);
}

static int
declare_trampoline_block (entry_count_t n_block_entries)
{
  return (declare_compiled_code_ns ("#trampoline_code_block",
				    n_block_entries,
				    trampoline_procedure));
}

bool
store_trampoline_insns (insn_t * entry, byte_t code)
{
  /* Trampoline entries are stored in the lowest part of the
     compiled_entries table.  That's why we reserve those above.  */
  (*entry) = code;
  return (false);
}

static SCHEME_OBJECT *
trampoline_procedure (SCHEME_OBJECT * trampoline, entry_count_t dispatch)
{
  return (invoke_utility (((unsigned int) (* ((insn_t *) trampoline))),
			  ((unsigned long)
			   (trampoline_storage
			    (cc_entry_address_to_block_address
			     ((insn_t *) trampoline)))),
			  0, 0, 0));
}

static compiled_block_t *
find_compiled_block (const char * name)
{
  tree_node node = (tree_lookup (compiled_blocks_tree, name));
  return ((node == 0) ? 0 : (compiled_blocks + (node->value)));
}

static SCHEME_OBJECT *
unspecified_code (SCHEME_OBJECT * entry, entry_count_t dispatch)
{
  SET_EXP ((SCHEME_OBJECT) entry);
  C_return_value = ERR_EXECUTE_MANIFEST_VECTOR;
  return (0);
}

static void *
lrealloc (void * ptr, size_t size)
{
  return ((ptr == 0) ? (malloc (size)) : (realloc (ptr, size)));
}

int
multiply_with_overflow (long x, long y, long * res)
{
  SCHEME_OBJECT ans = (Mul ((LONG_TO_FIXNUM (x)), (LONG_TO_FIXNUM (y))));
  if (ans == SHARP_F)
    {
      /* Bogus... */
      (*res) = (x * y);
      return (1);
    }
  else
    {
      (*res) = (FIXNUM_TO_LONG (ans));
      return (0);
    }
}

SCHEME_OBJECT
memory_to_uninterned_symbol (unsigned long length, const void * string)
{
  SCHEME_OBJECT name = (memory_to_string (length, string));
  SCHEME_OBJECT res = (CONS (name, UNBOUND_OBJECT));
  return (OBJECT_NEW_TYPE (TC_UNINTERNED_SYMBOL, res));
}

SCHEME_OBJECT
rconsm (unsigned int nargs, SCHEME_OBJECT tail, ...)
{
  SCHEME_OBJECT result;
  unsigned int i;
  va_list arg_ptr;
  va_start (arg_ptr, tail);

  result = tail;
  for (i = 1; (i < nargs); i += 1)
    result
      = (cons ((va_arg (arg_ptr, SCHEME_OBJECT)),
	       result));

  va_end (arg_ptr);
  return (result);
}

SCHEME_OBJECT
digit_string_to_bit_string (unsigned long n_bits,
			    unsigned long n_digits,
			    const char * digits)
{
  SCHEME_OBJECT result = (allocate_bit_string (n_bits));
  unsigned long posn = 0;
  unsigned long i;

  clear_bit_string (result);
  for (i = 0; (i < n_digits); i += 1)
    {
      unsigned int digit = (hex_digit_to_int (*digits++));
      unsigned int j = 0;
      unsigned int mask = 1;
      while (j < 4)
	{
	  if ((digit & mask) != 0)
	    bit_string_set (result, posn, 1);
	  j += 1;
	  mask <<= 1;
	  posn += 1;
	}
    }
  return (result);
}

SCHEME_OBJECT
digit_string_to_integer (bool negative_p,
			 unsigned long n_digits,
			 const char * digits)
{
  SCHEME_OBJECT bignum
    = (digit_stream_to_bignum (((int) n_digits),
			       digit_string_producer,
			       ((void *) (&digits)),
			       16,
			       ((int) negative_p)));

  return (bignum_to_integer (bignum));
}

static unsigned int
digit_string_producer (void * v_digit_ptr)
{
  const char ** digit_ptr = v_digit_ptr;
  char digit = (**digit_ptr);
  (*digit_ptr) = ((*digit_ptr) + 1);
  return (hex_digit_to_int (digit));
}

static unsigned int
hex_digit_to_int (char h_digit)
{
  unsigned int digit = ((unsigned int) h_digit);
  return (((digit >= '0') && (digit <= '9'))
	  ? (digit - '0')
	  : (((digit >= 'A') && (digit <= 'F'))
	     ? ((digit - 'A') + 10)
	     : ((digit - 'a') + 10)));
}
