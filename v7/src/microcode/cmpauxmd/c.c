/* -*-C-*-

$Id: c.c,v 1.1 1993/06/08 06:13:32 gjr Exp $

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
#include "bignum.h"
#include "bitstr.h"

extern void EXFUN (lose_big_1, (char *, char *));

#ifdef BUG_GCC_LONG_CALLS

extern SCHEME_OBJECT EXFUN (memory_to_string, (long, unsigned char *));
extern SCHEME_OBJECT EXFUN (memory_to_symbol, (long, unsigned char *));
extern SCHEME_OBJECT EXFUN (make_vector, (long, SCHEME_OBJECT, Boolean));
extern SCHEME_OBJECT EXFUN (cons, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (double_to_flonum, (double));
extern SCHEME_OBJECT EXFUN (long_to_integer, (long));
extern SCHEME_OBJECT EXFUN (digit_string_to_integer, (Boolean, long, char *));
extern SCHEME_OBJECT EXFUN (digit_string_to_bit_string, (long, long, char *));
extern SCHEME_OBJECT EXFUN (search_for_primitive,
			    (SCHEME_OBJECT, char *, Boolean, Boolean, int));

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
  ((SCHEME_OBJECT EXFUN ((*), ())) search_for_primitive)
};

#endif /* BUG_GCC_LONG_CALLS */

extern char * interface_to_C_hook;
extern void EXFUN (C_to_interface, (PTR));
extern void EXFUN (interface_initialize, (void));
extern SCHEME_OBJECT * EXFUN (initialize_C_compiled_block, (int, char *));
extern void EXFUN (initialize_compiled_code_blocks, (void));

typedef SCHEME_OBJECT * EXFUN ((* compiled_block), (SCHEME_OBJECT *));

int pc_zero_bits;
char * interface_to_C_hook;
static compiled_block * compiled_code_blocks;
static char ** compiled_block_names;
static int max_compiled_code_blocks, compiled_code_blocks_size;
static SCHEME_OBJECT dummy_entry = SHARP_F;

SCHEME_OBJECT *
DEFUN (trampoline_procedure, (trampoline), SCHEME_OBJECT * trampoline)
{
  return (invoke_utility ((LABEL_TAG (trampoline)),
			  ((long) (TRAMPOLINE_STORAGE (trampoline))),
			  0, 0, 0));
}

void
DEFUN_VOID (NO_SUBBLOCKS)
{
  return;
}

int
DEFUN (declare_compiled_code, (name, decl_proc, code_proc),
       char * name
       AND void EXFUN (decl_proc, (void))
       AND SCHEME_OBJECT * EXFUN (code_proc, (SCHEME_OBJECT *)))
{  
  int index;

  index = max_compiled_code_blocks;
  max_compiled_code_blocks += 1;
  if ((MAKE_LABEL_WORD (index, 0)) == dummy_entry)
    return (0);

  if (index >= compiled_code_blocks_size)
  {
    compiled_block * new_blocks;
    char ** new_names;
    compiled_code_blocks_size = ((compiled_code_blocks_size == 0)
				 ? 10
				 : (compiled_code_blocks_size * 2));
    new_blocks =
      ((compiled_block *)
       (realloc (compiled_code_blocks,
		 (compiled_code_blocks_size * (sizeof (compiled_block))))));
    
    new_names =
      ((char **)
       (realloc (compiled_block_names,
		 (compiled_code_blocks_size * (sizeof (char *))))));

    if ((new_blocks == ((compiled_block *) NULL))
	|| (new_names == ((char **) NULL)))
      return (0);
    compiled_code_blocks = new_blocks;
    compiled_block_names = new_names;
  }
  compiled_code_blocks[index] = (code_proc);
  compiled_block_names[index] = name;
  decl_proc ();
  return (index);
}

void
DEFUN_VOID (interface_initialize)
{
  int i, pow, del;
  
  for (i = 0, pow = 1, del = ((sizeof (SCHEME_OBJECT)) / (sizeof (char)));
       pow < del;
       i+= 1)
    pow = (pow << 1);
  
  if (pow != del)
    lose_big ("initialize_compiler: not a power of two");

  pc_zero_bits = i;  

  dummy_entry = (MAKE_LABEL_WORD (-1, 0));
  interface_to_C_hook = ((char *) &dummy_entry);
  max_compiled_code_blocks = 0;
  compiled_code_blocks_size = 0;
  compiled_code_blocks = ((compiled_block *) NULL);
  compiled_block_names = ((char **) NULL);
  (void) declare_compiled_code ("", NO_SUBBLOCKS, trampoline_procedure);

  initialize_compiled_code_blocks ();

  return;
}

/* For now this is a linear search.
   Not that it matters much, but we could easily
   make it binary.
 */

int
DEFUN (find_compiled_block, (name), char * name)
{
  int i;
  
  for (i = 1; i < max_compiled_code_blocks; i++)
  {
    if ((strcmp (name, compiled_block_names[i])) == 0)
      return (i);
  }
  return (0);
}

SCHEME_OBJECT
DEFUN (initialize_subblock, (name), char * name)
{
  SCHEME_OBJECT id, * ep, * block;
  int slot = (find_compiled_block (name));

  if (slot == 0)
    error_external_return ();

  id = (MAKE_LABEL_WORD (slot, 0));
  ep = ((* (compiled_code_blocks[slot])) (&id));
  Get_Compiled_Block (block, ep);
  return (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block));
}

SCHEME_OBJECT *
DEFUN (initialize_C_compiled_block, (argno, name),
       int argno AND char * name)
{
  int slot;
  SCHEME_OBJECT id;
  slot = (find_compiled_block (name));
  if (slot == 0)
    return ((SCHEME_OBJECT *) NULL);

  id = (MAKE_LABEL_WORD (slot, 0));
  return ((* (compiled_code_blocks[slot])) (&id));
}

void
DEFUN (C_to_interface, (entry), PTR in_entry)
{
  SCHEME_OBJECT * entry = ((SCHEME_OBJECT *) in_entry);
  while (1)
  {
    int proc_index;
    proc_index = (LABEL_PROCEDURE (entry));
    if (proc_index >= max_compiled_code_blocks)
    {
      if (entry != &dummy_entry)
#if 0
      {
	/* We need to export C_return_value before enabling this code. */
	Store_Expression ((SCHEME_OBJECT) entry);
	C_return_value = (ERR_EXECUTE_MANIFEST_VECTOR);
	return;
      }
#else
	lose_big ("C_to_interface: non-existent procedure");
#endif
      return;
    }
    else
      entry = ((* (compiled_code_blocks [proc_index])) (entry));
  }
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
    * res = (x * y);
    return (1);
  }
  else
  {
    * res = (FIXNUM_TO_LONG (ans));
    return (0);
  }
}

void
DEFUN (lose_big, (msg), char * msg)
{
  fprintf (stderr, "\nlose_big: %s.\n", msg);
  Microcode_Termination (TERM_EXIT);
  /*NOTREACHED*/
}

void
DEFUN (lose_big_1, (msg, arg), char * msg AND char * arg)
{
  fprintf (stderr, "\nlose_big: %s (%s).\n", msg, arg);
  Microcode_Termination (TERM_EXIT);
  /*NOTREACHED*/
}

void
DEFUN_VOID (error_band_already_built)
{
  lose_big ("Trying to initilize data with the wrong binary.");
  /*NOTREACHED*/  
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
DEFUN (hex_digit_to_int, (h_digit), char h_digit)
{
  unsigned int digit = ((unsigned int) h_digit);

  return (((digit >= '0') && (digit <= '9'))
	  ? (digit - '0')
	  : (((digit >= 'A') && (digit <= 'F'))
	     ? ((digit - 'A') + 10)
	     : ((digit - 'a') + 10)));
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
