/* -*-C-*-

$Id: c.c,v 1.10 1993/11/01 15:27:42 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

#include "liarc.h"
#include "prims.h"
#include "bignum.h"
#include "bitstr.h"

#ifdef BUG_GCC_LONG_CALLS

extern SCHEME_OBJECT EXFUN (memory_to_string, (long, unsigned char *));
extern SCHEME_OBJECT EXFUN (memory_to_symbol, (long, unsigned char *));
extern SCHEME_OBJECT EXFUN (make_vector, (long, SCHEME_OBJECT, Boolean));
extern SCHEME_OBJECT EXFUN (cons, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (double_to_flonum, (double));
extern SCHEME_OBJECT EXFUN (long_to_integer, (long));
extern SCHEME_OBJECT EXFUN (digit_string_to_integer, (Boolean, long, char *));
extern SCHEME_OBJECT EXFUN (digit_string_to_bit_string, (long, long, char *));
extern SCHEME_OBJECT EXFUN (make_primitive, (char *, int));

SCHEME_OBJECT EXFUN ((* (constructor_kludge [10])), ()) =
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
  ((SCHEME_OBJECT EXFUN ((*), ())) make_primitive)
};

#endif /* BUG_GCC_LONG_CALLS */

extern char * interface_to_C_hook;
extern void EXFUN (C_to_interface, (PTR));
extern void EXFUN (interface_initialize, (void));
extern SCHEME_OBJECT * EXFUN (initialize_C_compiled_block, (int, char *));
extern int EXFUN (initialize_compiled_code_blocks, (void));
extern void * scheme_hooks_low, * scheme_hooks_high;

#define TRAMPOLINE_FUDGE 20

typedef SCHEME_OBJECT * EXFUN ((* code_block),
			       (SCHEME_OBJECT *, unsigned long));

typedef SCHEME_OBJECT * EXFUN ((* data_block), (unsigned long));

struct compiled_entry_s
{
  code_block code;
  unsigned long dispatch;
};

struct compiled_block_s
{
  char * name;
  unsigned long dispatch;
  data_block constructor;
};

int pc_zero_bits;
char * interface_to_C_hook;
#define PSEUDO_STATIC /* static */
PSEUDO_STATIC struct compiled_block_s *
  compiled_blocks = ((struct compiled_block_s *) NULL);
PSEUDO_STATIC struct compiled_entry_s *
  compiled_entries = ((struct compiled_entry_s *) NULL);
PSEUDO_STATIC unsigned long
  max_compiled_entries = 0,
  compiled_entries_size = 0;
PSEUDO_STATIC unsigned long
  max_compiled_blocks = 0,
  compiled_blocks_size = 0;
static SCHEME_OBJECT
  dummy_entry = ((SCHEME_OBJECT) -1L);
void
  * scheme_hooks_low = NULL,
  * scheme_hooks_high = NULL;

SCHEME_OBJECT *
DEFUN (trampoline_procedure, (trampoline, dispatch),
       SCHEME_OBJECT * trampoline AND unsigned long dispatch)
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
DEFUN (no_data, (base_dispatch), unsigned long base_dispatch)
{
  return ((SCHEME_OBJECT *) NULL);
}

SCHEME_OBJECT *
DEFUN (uninitialized_data, (base_dispatch), unsigned long base_dispatch)
{
  /* Not yet assigned.  Cannot construct data. */
  error_external_return ();
}

PTR
DEFUN (lrealloc, (ptr, size),
       PTR ptr
       AND unsigned long size)
{
  extern PTR EXFUN (malloc, (unsigned long));
  extern PTR EXFUN (realloc, (PTR, unsigned long));

  if (ptr == ((PTR) NULL))
    return (malloc (size));
  else
    return (realloc (ptr, size));
}

void
DEFUN_VOID (interface_initialize)
{
  extern long MAX_TRAMPOLINE;
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

  if (compiled_entries != ((struct compiled_entry_s *) NULL))
    free (compiled_entries);
  if (compiled_blocks != ((struct compiled_block_s *) NULL))
    free (compiled_blocks);
  
  interface_to_C_hook = ((char *) &dummy_entry);
  max_compiled_entries = 0;
  compiled_entries_size = 0;
  compiled_entries = ((struct compiled_entry_s *) NULL);
  max_compiled_blocks = 0;
  compiled_blocks_size = 0;
  compiled_blocks = ((struct compiled_block_s *) NULL);
  
  if (((declare_compiled_code ("#trampoline_code_block",
			       (MAX_TRAMPOLINE + TRAMPOLINE_FUDGE),
			       NO_SUBBLOCKS,
			       trampoline_procedure))
       != 0)
#if 0
      /* trampoline block is special. */

      || ((declare_compiled_data ("#trampoline_code_block",
				  NO_SUBBLOCKS,
				  no_data))
	  != 0)
#endif
      || (initialize_compiled_code_blocks ()) != 0)
  {
    outf_fatal ("interface_initialize: error initializing compiled code.\n");
    Microcode_Termination (TERM_EXIT);
  }
  return;
}

int
DEFUN (declare_compiled_code,
       (name, nentries, decl_code, code_proc),
       char * name
       AND unsigned long nentries
       AND int EXFUN ((* decl_code), (void))
       AND SCHEME_OBJECT * EXFUN ((* code_proc),
				  (SCHEME_OBJECT *, unsigned long)))
{
  unsigned long dispatch = max_compiled_entries;
  unsigned long n_dispatch = (dispatch + nentries);
  unsigned long block_index = max_compiled_blocks;

  if (n_dispatch < dispatch)
    /* Wrap around */
    return (-1);
    
  if (n_dispatch >= compiled_entries_size)
  {
    struct compiled_entry_s * new_entries;
    unsigned long new_entries_size = ((compiled_entries_size == 0)
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

  if (block_index >= compiled_blocks_size)
  {
    struct compiled_block_s * new_blocks;
    unsigned long new_blocks_size = ((compiled_blocks_size == 0)
				     ? 10
				     : ((compiled_blocks_size * 3) / 2));
    new_blocks = ((struct compiled_block_s *)
		  (lrealloc (compiled_blocks,
			     (new_blocks_size
			      * (sizeof (struct compiled_block_s))))));
    if (new_blocks == ((struct compiled_block_s *) NULL))
      return (-1);
    compiled_blocks_size = new_blocks_size;
    compiled_blocks = new_blocks;
  }

  max_compiled_entries = n_dispatch;
  max_compiled_blocks = (block_index + 1);
  
  compiled_blocks[block_index].name = name;
  compiled_blocks[block_index].dispatch = dispatch;
  compiled_blocks[block_index].constructor = uninitialized_data;

  for (block_index = dispatch; block_index < n_dispatch; block_index++)
  {
    compiled_entries[block_index].code = code_proc;
    compiled_entries[block_index].dispatch = dispatch;
  }

  return (* decl_code) ();
}

/* For now this is a linear search.
   Not that it matters much, but we could easily
   make it binary.
 */

unsigned long
DEFUN (find_compiled_block, (name), char * name)
{
  unsigned long i;
  
  for (i = 1; i < max_compiled_blocks; i++)
    if ((strcmp (name, compiled_blocks[i].name)) == 0)
      return (i);
  return (0);
}

int
DEFUN (declare_compiled_data,
       (name, decl_data, data_proc),
       char * name
       AND int EXFUN ((* decl_data), (void))
       AND SCHEME_OBJECT * EXFUN ((* data_proc), (unsigned long)))
{
  unsigned long slot = (find_compiled_block (name));

  if (slot == 0)
    return (-1);
  
  if ((compiled_blocks[slot].constructor != uninitialized_data)
      && (compiled_blocks[slot].constructor != data_proc))
    return (-1);

  compiled_blocks[slot].constructor = data_proc;
  return (* decl_data) ();  
}

SCHEME_OBJECT
DEFUN (initialize_subblock, (name), char * name)
{
  SCHEME_OBJECT * ep, * block;
  unsigned long slot = (find_compiled_block (name));

  if (slot == 0)
    error_external_return ();

  ep = ((* compiled_blocks[slot].constructor)
	(compiled_blocks[slot].dispatch));
  Get_Compiled_Block (block, ep);
  return (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block));
}

SCHEME_OBJECT *
DEFUN (initialize_C_compiled_block, (argno, name),
       int argno AND char * name)
{
  unsigned long slot;

  slot = (find_compiled_block (name));
  if (slot == 0)
    return ((SCHEME_OBJECT *) NULL);

  return ((* compiled_blocks[slot].constructor)
	  (compiled_blocks[slot].dispatch));
}

#define C_COUNT_TRANSFERS
unsigned long c_to_interface_transfers = 0;

void
DEFUN (C_to_interface, (in_entry), PTR in_entry)
{
  extern long C_return_value;
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
	/* We need to export C_return_value before enabling this code. */
	Store_Expression ((SCHEME_OBJECT) entry);
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

typedef SCHEME_OBJECT * EXFUN
  ((* utility_table_entry), (long, long, long, long));

extern utility_table_entry utility_table[];

SCHEME_OBJECT *
DEFUN (invoke_utility, (code, arg1, arg2, arg3, arg4),
       int code AND long arg1 AND long arg2 AND long arg3 AND long arg4)
{
  return ((* utility_table[code]) (arg1, arg2, arg3, arg4));
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
DEFUN (hex_digit_to_int, (h_digit), char h_digit)
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
       long n_bits AND long n_digits AND char * digits)
{
  extern void EXFUN (clear_bit_string, (SCHEME_OBJECT));
  extern SCHEME_OBJECT EXFUN (allocate_bit_string, (long));
  extern void EXFUN (bit_string_set, (SCHEME_OBJECT, long, int));
  SCHEME_OBJECT result = (allocate_bit_string (n_bits));
  unsigned int digit, mask;
  long i, posn;
  int j;

  posn = 0;
  clear_bit_string (result);

  for (i = 0; i < n_digits; i++)
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
DEFUN (memory_to_symbol, (length, string),
       long length AND unsigned char * string)
{
  extern SCHEME_OBJECT EXFUN (find_symbol, (long, unsigned char *));
  extern SCHEME_OBJECT EXFUN (string_to_symbol, (SCHEME_OBJECT));
  SCHEME_OBJECT symbol;

  symbol = (find_symbol (length, string));
  if (symbol != SHARP_F)
    return (symbol);
  return (string_to_symbol (memory_to_string (length, string)));
}

static unsigned int
DEFUN (digit_string_producer, (digit_ptr), char ** digit_ptr)
{
  char digit = ** digit_ptr;
  * digit_ptr = ((* digit_ptr) + 1);
  return (hex_digit_to_int (digit));
}

SCHEME_OBJECT
DEFUN (digit_string_to_integer, (negative_p, n_digits, digits),
       Boolean negative_p AND long n_digits AND char * digits)
{
  char * digit = digits;

  return (digit_stream_to_bignum (((int) n_digits),
				  digit_string_producer,
				  ((PTR) & digit),
				  16,
				  ((int) negative_p)));
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
