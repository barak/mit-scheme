/* -*-C-*-

$Id: bchdmp.c,v 9.91 2002/11/20 19:46:06 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* bchgcl, bchmmg, bchpur, and bchdmp can replace gcloop, memmag,
   purify, and fasdump, respectively, to provide garbage collection
   and related utilities to disk. */

#include "scheme.h"
#include "prims.h"
#include "osfile.h"
#include "osfs.h"
#include "trap.h"
#include "lookup.h"		/* UNCOMPILED_VARIABLE */
#define In_Fasdump
#include "fasl.h"
#include "bchgcc.h"

extern int EXFUN (OS_channel_copy, (off_t, Tchannel, Tchannel));

extern SCHEME_OBJECT EXFUN
  (dump_renumber_primitive, (SCHEME_OBJECT));
extern SCHEME_OBJECT * EXFUN
  (initialize_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *));
extern SCHEME_OBJECT * EXFUN
  (cons_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *));
extern SCHEME_OBJECT * EXFUN
  (cons_whole_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *));

extern SCHEME_OBJECT compiler_utilities;
extern SCHEME_OBJECT * EXFUN
  (cons_c_code_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *));

#ifdef __unix__
#  include "ux.h"
#  include "uxio.h"
   static char FASDUMP_FILENAME[] = "fasdumpXXXXXX";
#endif

#ifdef __WIN32__
#  include "nt.h"
#  include "ntio.h"
   static char FASDUMP_FILENAME[] = "faXXXXXX";
#endif

#ifdef __OS2__
#  include "os2.h"
   static char FASDUMP_FILENAME[] = "faXXXXXX";
#  ifdef __EMX__
#    include <io.h>
#  endif
#  if defined(__IBMC__) || defined(__WATCOMC__)
#    include <io.h>
#    include <sys\stat.h>
#    include <fcntl.h>
#    ifndef F_OK
#      define F_OK 0
#      define X_OK 1
#      define W_OK 2
#      define R_OK 4
#    endif
#  endif
#endif

static Tchannel dump_channel;
static CONST char * dump_file_name;
static int real_gc_file;
static int dump_file;
static SCHEME_OBJECT * saved_free;
static SCHEME_OBJECT * fixup_buffer = 0;
static SCHEME_OBJECT * fixup_buffer_end;
static SCHEME_OBJECT * fixup;
static int fixup_count = 0;
static Boolean compiled_code_present_p;

#define Write_Data(size, buffer)					\
  ((OS_channel_write_dump_file						\
    (dump_channel,							\
     ((char *) (buffer)),						\
     ((size) * (sizeof (SCHEME_OBJECT)))))				\
   / (sizeof (SCHEME_OBJECT)))

#include "dump.c"

static SCHEME_OBJECT EXFUN (dump_to_file, (SCHEME_OBJECT, CONST char *));
static int EXFUN (fasdump_exit, (long length));
static int EXFUN (reset_fixes, (void));
static ssize_t EXFUN (eta_read, (int, char *, int));
static ssize_t EXFUN (eta_write, (int, char *, int));
static long EXFUN
  (dump_loop, (SCHEME_OBJECT *, SCHEME_OBJECT **, SCHEME_OBJECT **));

/* (PRIMITIVE-FASDUMP object-to-dump filename-or-channel flag)

   Dump an object into a file so that it can be loaded using
   BINARY-FASLOAD.  A spare heap is required for this operation.  The
   first argument is the object to be dumped.  The second is the
   filename or channel.  The third argument, FLAG, is currently
   ignored.  The primitive returns #T or #F indicating whether it
   successfully dumped the object (it can fail on an object that is
   too large).  It should signal an error rather than return false,
   but ... some other time.

   This version of fasdump can only handle files (actually lseek-able
   streams), since the header is written at the beginning of the
   output but its contents are only know after the rest of the output
   has been written.

   Thus, for arbitrary channels, a temporary file is allocated, and on
   completion, the file is copied to the channel.  */

DEFINE_PRIMITIVE ("PRIMITIVE-FASDUMP", Prim_prim_fasdump, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT root = (ARG_REF (1));
    if (STRING_P (ARG_REF (2)))
      PRIMITIVE_RETURN (dump_to_file (root, (STRING_ARG (2))));
    {
      Tchannel channel = (arg_channel (2));
      char * temp_name = (make_gc_file_name (FASDUMP_FILENAME));
      transaction_begin ();
      protect_gc_file_name (temp_name);
      if (!allocate_gc_file (temp_name))
	signal_error_from_primitive (ERR_EXTERNAL_RETURN);
      {
	SCHEME_OBJECT fasdump_result = (dump_to_file (root, temp_name));
	if (fasdump_result == SHARP_T)
	  {
	    Tchannel temp_channel = (OS_open_input_file (temp_name));
	    int copy_result
	      = (OS_channel_copy ((OS_file_length (temp_channel)),
				  temp_channel,
				  channel));
	    OS_channel_close (temp_channel);
	    OS_file_remove (temp_name);
	    transaction_commit ();
	    if (copy_result < 0)
	      signal_error_from_primitive (ERR_IO_ERROR);
	  }
	PRIMITIVE_RETURN (fasdump_result);
      }
    }
  }
}

/* (DUMP-BAND PROCEDURE FILE-NAME)
   Saves all of the heap and pure space on FILE-NAME.  When the
   file is loaded back using BAND_LOAD, PROCEDURE is called with an
   argument of #F.  */

DEFINE_PRIMITIVE ("DUMP-BAND", Prim_band_dump, 2, 2, 0)
{
  SCHEME_OBJECT * saved_free;
  SCHEME_OBJECT * prim_table_start;
  SCHEME_OBJECT * prim_table_end;
  SCHEME_OBJECT * c_table_start;
  SCHEME_OBJECT * c_table_end;
  long prim_table_length;
  long c_table_length;
  int result = 0;
  PRIMITIVE_HEADER (2);

  Band_Dump_Permitted ();
  CHECK_ARG (1, INTERPRETER_APPLICABLE_P);
  CHECK_ARG (2, STRING_P);
  if (Unused_Heap_Bottom < Heap_Bottom)
    /* Cause the image to be in the low heap, to increase
       the probability that no relocation is needed on reload. */
    Primitive_GC (0);
  Primitive_GC_If_Needed (5);

  saved_free = Free;

  {
    SCHEME_OBJECT Combination;
    Combination = (MAKE_POINTER_OBJECT (TC_COMBINATION_1, Free));
    (Free[COMB_1_FN]) = (ARG_REF (1));
    (Free[COMB_1_ARG_1]) = SHARP_F;
    Free += 2;
    {
      SCHEME_OBJECT p = (MAKE_POINTER_OBJECT (TC_LIST, Free));
      (*Free++) = Combination;
      (*Free++) = compiler_utilities;
      (*Free++) = p;
    }
  }

  prim_table_start = Free;
  prim_table_end
    = (cons_whole_primitive_table (prim_table_start, Heap_Top,
				   (&prim_table_length)));
  if (prim_table_end >= Heap_Top)
    goto done;

  c_table_start = prim_table_end;
  c_table_end
    = (cons_c_code_table (c_table_start, Heap_Top,
			  (&c_table_length)));
  if (c_table_end >= Heap_Top)
    goto done;

  {
    CONST char * filename = ((CONST char *) (STRING_LOC ((ARG_REF (2)), 0)));
    SCHEME_OBJECT * faligned_heap = Heap_Bottom;
    SCHEME_OBJECT * faligned_constant = Constant_Space;

    BCH_ALIGN_FLOAT_ADDRESS (faligned_heap);
    BCH_ALIGN_FLOAT_ADDRESS (faligned_constant);

    OS_file_remove_link (filename);
    dump_channel = (OS_open_dump_file (filename));
    if (dump_channel == NO_CHANNEL)
      error_bad_range_arg (2);

    result
      = (Write_File ((Free - 1),
		     ((long) (Free - faligned_heap)),
		     faligned_heap,
		     ((long) (Free_Constant - faligned_constant)),
		     faligned_constant,
		     prim_table_start,
		     prim_table_length,
		     ((long) (prim_table_end - prim_table_start)),
		     c_table_start,
		     c_table_length,
		     ((long) (c_table_end - c_table_start)),
		     (compiler_utilities != SHARP_F),
		     1));

    OS_channel_close_noerror (dump_channel);
    if (!result)
      OS_file_remove (filename);
  }

 done:
  Band_Dump_Exit_Hook ();
  Free = saved_free;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (result));
}

static SCHEME_OBJECT
DEFUN (dump_to_file, (root, fname),
       SCHEME_OBJECT root AND
       CONST char * fname)
{
  Boolean success = 1;
  long value;
  long length;
  long hlength;
  long tlength;
  long tsize;
  SCHEME_OBJECT * dumped_object;
  SCHEME_OBJECT * free_buffer;
  SCHEME_OBJECT * dummy;
  SCHEME_OBJECT * table_start;
  SCHEME_OBJECT * table_end;
  SCHEME_OBJECT * table_top;
  SCHEME_OBJECT header [FASL_HEADER_LENGTH];

  if (fixup_buffer == 0)
    {
      fixup_buffer = ((SCHEME_OBJECT *) (malloc (gc_buffer_bytes)));
      if (fixup_buffer == 0)
	error_system_call (errno, syscall_malloc);
      fixup_buffer_end = (fixup_buffer + gc_buffer_size);
    }

  dump_file_name = fname;
  dump_file = (open (dump_file_name, GC_FILE_FLAGS, 0666));
  if (dump_file < 0)
    error_bad_range_arg (2);

  compiled_code_present_p = 0;
  real_gc_file = (swap_gc_file (dump_file));
  saved_free = Free;
  fixup = fixup_buffer_end;
  fixup_count = -1;

  table_top = (& (saved_free [Space_Before_GC ()]));
  table_start = (initialize_primitive_table (saved_free, table_top));
  if (table_start >= table_top)
    {
      fasdump_exit (0);
      Primitive_GC (table_start - saved_free);
    }

  free_buffer = (initialize_free_buffer ());
  Free = 0;
  free_buffer += FASL_HEADER_LENGTH;

  dummy = free_buffer;
  BCH_ALIGN_FLOAT (Free, dummy);

  (*free_buffer++) = root;
  dumped_object = (Free++);

  value
    = dump_loop (((initialize_scan_buffer (0)) + FASL_HEADER_LENGTH),
		 (&free_buffer), (&Free));
  if (value != PRIM_DONE)
    {
      fasdump_exit (0);
      if (value == PRIM_INTERRUPT)
	return (SHARP_F);
      else
	signal_error_from_primitive (value);
    }
  end_transport (&success);
  if (!success)
    {
      fasdump_exit (0);
      return (SHARP_F);
    }

  length = (Free - dumped_object);

  table_end = (cons_primitive_table (table_start, table_top, &tlength));
  if (table_end >= table_top)
    {
      fasdump_exit (0);
      Primitive_GC (table_end - saved_free);
    }

#ifdef NATIVE_CODE_IS_C
  /* Cannot dump C compiled code. */
  if (compiled_code_present_p)
    {
      fasdump_exit (0);
      signal_error_from_primitive (ERR_COMPILED_CODE_ERROR);
    }
#endif

  tsize = (table_end - table_start);
  hlength = ((sizeof (SCHEME_OBJECT)) * tsize);
  if (((lseek (dump_file,
	       ((sizeof (SCHEME_OBJECT)) * (length + FASL_HEADER_LENGTH)),
	       0))
       == -1)
      || ((write (dump_file, ((char *) (&table_start[0])), hlength))
	  != hlength))
    {
      fasdump_exit (0);
      return (SHARP_F);
    }

  hlength = ((sizeof (SCHEME_OBJECT)) * FASL_HEADER_LENGTH);
  prepare_dump_header
    (header, dumped_object, length, dumped_object,
     0, Constant_Space, tlength, tsize, 0, 0,
     compiled_code_present_p, 0);
  if (((lseek (dump_file, 0, 0)) == -1)
      || ((write (dump_file, ((char *) &header[0]), hlength)) != hlength))
    {
      fasdump_exit (0);
      return (SHARP_F);
    }
  return
    (BOOLEAN_TO_OBJECT
     (fasdump_exit (((sizeof (SCHEME_OBJECT)) * (length + tsize)) + hlength)));
}

static int
DEFUN (fasdump_exit, (length), long length)
{
  SCHEME_OBJECT * fixes, * fix_address;
  int result;

  Free = saved_free;
  restore_gc_file ();

#ifdef HAVE_FTRUNCATE
  ftruncate (dump_file, length);
#endif
  result = ((close (dump_file)) == 0);
#if defined(HAVE_TRUNCATE) && !defined(HAVE_FTRUNCATE)
  truncate (dump_file_name, length);
#endif

  if (length == 0)
    unlink (dump_file_name);
  dump_file_name = 0;

  fixes = fixup;

 next_buffer:

  while (fixes != fixup_buffer_end)
    {
      fix_address = ((SCHEME_OBJECT *) (*fixes++));
      (*fix_address) = (*fixes++);
    }

  if (fixup_count >= 0)
    {
      if ((retrying_file_operation
	   (eta_read,
	    real_gc_file,
	    ((char *) fixup_buffer),
	    (gc_file_start_position + (fixup_count << gc_buffer_byte_shift)),
	    gc_buffer_bytes,
	    "read",
	    "the fixup buffer",
	    (&gc_file_current_position),
	    io_error_retry_p))
	  != ((long) gc_buffer_bytes))
	{
	  gc_death
	    (TERM_EXIT,
	     "fasdump: Could not read back the fasdump fixup information",
	     0, 0);
	  /*NOTREACHED*/
	}
      fixup_count -= 1;
      fixes = fixup_buffer;
      goto next_buffer;
    }

  fixup = fixes;
  Fasdump_Exit_Hook ();
  return (result);
}

static int
DEFUN_VOID (reset_fixes)
{
  long start;

  fixup_count += 1;
  start = (gc_file_start_position + (fixup_count << gc_buffer_byte_shift));

  if (((start + ((long) gc_buffer_bytes)) > gc_file_end_position)
      || ((retrying_file_operation
	   (eta_write,
	    real_gc_file,
	    ((char *) fixup_buffer),
	    start,
	    gc_buffer_bytes,
	    "write",
	    "the fixup buffer",
	    (&gc_file_current_position),
	    io_error_always_abort))
	  != ((long) gc_buffer_bytes)))
    return (0);
  fixup = fixup_buffer_end;
  return (1);
}

static ssize_t
DEFUN (eta_read, (fid, buffer, size),
       int fid AND
       char * buffer AND
       int size)
{
  return (read (fid, buffer, size));
}

static ssize_t
DEFUN (eta_write, (fid, buffer, size),
       int fid AND
       char * buffer AND
       int size)
{
  return (write (fid, buffer, size));
}

#define MAYBE_DUMP_FREE(free)						\
{									\
  if (free >= free_buffer_top)						\
    DUMP_FREE (free);							\
}

#define DUMP_FREE(free) do						\
{									\
  Boolean _s = 1;							\
  free = (dump_and_reset_free_buffer (free, (&_s)));			\
  if (!_s)								\
    return (PRIM_INTERRUPT);						\
} while (0)

#define MAYBE_DUMP_SCAN(scan)						\
{									\
  if (scan >= scan_buffer_top)						\
    DUMP_SCAN (scan);							\
}

#define DUMP_SCAN(scan) do						\
{									\
  Boolean _s = 1;							\
  scan = (dump_and_reload_scan_buffer (scan, (&_s)));			\
  if (!_s)								\
    return (PRIM_INTERRUPT);						\
} while (0)

#define PUSH_FIXUP_DATA(ptr)						\
{									\
  if ((fixup == fixup_buffer) && (!reset_fixes ()))			\
    return (PRIM_INTERRUPT);						\
  (*--fixup) = (* (ptr));						\
  (*--fixup) = ((SCHEME_OBJECT) ptr);					\
}

#define TRANSPORT_VECTOR(new_address, free, old_start, n_words)		\
{									\
  SCHEME_OBJECT * old_ptr = old_start;					\
  SCHEME_OBJECT * free_end = (free + n_words);				\
  if (free_end < free_buffer_top)					\
    while (free < free_end)						\
      (*free++) = (*old_ptr++);						\
  else									\
    {									\
      while (free < free_buffer_top)					\
	(*free++) = (*old_ptr++);					\
      free = (transport_vector_tail (free, free_end, old_ptr));		\
      if (free == 0)							\
	return (PRIM_INTERRUPT);					\
    }									\
}

static SCHEME_OBJECT *
DEFUN (transport_vector_tail, (free, free_end, tail),
       SCHEME_OBJECT * free AND
       SCHEME_OBJECT * free_end AND
       SCHEME_OBJECT * tail)
{
  unsigned long n_words = (free_end - free);
  {
    Boolean success = 1;
    free = (dump_and_reset_free_buffer (free, (&success)));
    if (!success)
      return (0);
  }
  {
    unsigned long n_blocks = (n_words >> gc_buffer_shift);
    if (n_blocks > 0)
      {
	Boolean success = 1;
	free = (dump_free_directly (tail, n_blocks, (&success)));
	if (!success)
	  return (0);
	tail += (n_blocks << gc_buffer_shift);
      }
  }
  {
    SCHEME_OBJECT * free_end = (free + (n_words & gc_buffer_mask));
    while (free < free_end)
      (*free++) = (*tail++);
  }
  return (free);
}

/* A copy of gc_loop, with minor modifications. */

static long
DEFUN (dump_loop, (scan, free_ptr, new_address_ptr),
       SCHEME_OBJECT * scan AND
       SCHEME_OBJECT ** free_ptr AND
       SCHEME_OBJECT ** new_address_ptr)
{
  SCHEME_OBJECT * free = (*free_ptr);
  SCHEME_OBJECT * new_address = (*new_address_ptr);
  while (scan != free)
    {
      SCHEME_OBJECT object;
      if (scan >= scan_buffer_top)
	{
	  if (scan == scan_buffer_top)
	    DUMP_SCAN (scan);
	  else
	    {
	      sprintf
		(gc_death_message_buffer,
		 "dump_loop: scan (0x%lx) > scan_buffer_top (0x%lx)",
		 ((unsigned long) scan),
		 ((unsigned long) scan_buffer_top));
	      gc_death (TERM_EXIT, gc_death_message_buffer, scan, free);
	      /*NOTREACHED*/
	    }
	}
      object = (*scan);
      switch (OBJECT_TYPE (object))
	{
	case TC_BROKEN_HEART:
	  if ((OBJECT_DATUM (object)) == 0)
	    {
	      scan += 1;
	      break;
	    }
	  if (object == (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan)))
	    /* Does this ever happen?  */
	    goto end_dump_loop;
	  sprintf (gc_death_message_buffer,
		   "dump_loop: broken heart (0x%lx) in scan",
		   object);
	  gc_death (TERM_BROKEN_HEART, gc_death_message_buffer, scan, free);
	  /*NOTREACHED*/
	  break;

	case TC_CHARACTER:
	case TC_CONSTANT:
	case TC_FIXNUM:
	case TC_NULL:
	case TC_RETURN_CODE:
	case TC_STACK_ENVIRONMENT:
	case TC_THE_ENVIRONMENT:
	  scan += 1;
	  break;

	case TC_PCOMB0:
	case TC_PRIMITIVE:
	  (*scan++) = (dump_renumber_primitive (object));
	  break;

	case TC_CELL:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		PUSH_FIXUP_DATA (old_start);
		(*free++) = (old_start[0]);
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 1;
	      }
	  }
	  break;

	case TC_ACCESS:
	case TC_ASSIGNMENT:
	case TC_COMBINATION_1:
	case TC_COMMENT:
	case TC_COMPLEX:
	case TC_DEFINITION:
	case TC_DELAY:
	case TC_DELAYED:
	case TC_DISJUNCTION:
	case TC_ENTITY:
	case TC_EXTENDED_PROCEDURE:
	case TC_INTERNED_SYMBOL:
	case TC_IN_PACKAGE:
	case TC_LAMBDA:
	case TC_LEXPR:
	case TC_LIST:
	case TC_PCOMB1:
	case TC_PROCEDURE:
	case TC_RATNUM:
	case TC_SCODE_QUOTE:
	case TC_SEQUENCE_2:
	case TC_UNINTERNED_SYMBOL:
	case TC_WEAK_CONS:
	transport_pair:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		PUSH_FIXUP_DATA (old_start);
		(*free++) = (old_start[0]);
		switch (OBJECT_TYPE (object))
		  {
		  case TC_INTERNED_SYMBOL:
		    (*free++) = BROKEN_HEART_ZERO;
		    break;
		  case TC_UNINTERNED_SYMBOL:
		    (*free++) = UNBOUND_OBJECT;
		    break;
		  default:
		    (*free++) = (old_start[1]);
		    break;
		  }
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 2;
	      }
	  }
	  break;

	case TC_COMBINATION_2:
	case TC_CONDITIONAL:
	case TC_EXTENDED_LAMBDA:
	case TC_HUNK3_A:
	case TC_HUNK3_B:
	case TC_PCOMB2:
	case TC_SEQUENCE_3:
	case TC_VARIABLE:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		PUSH_FIXUP_DATA (old_start);
		(*free++) = (old_start[0]);
		switch (OBJECT_TYPE (object))
		  {
		  case TC_VARIABLE:
		    (*free++) = UNCOMPILED_VARIABLE;
		    (*free++) = SHARP_F;
		    break;
		  default:
		    (*free++) = (old_start[1]);
		    (*free++) = (old_start[2]);
		    break;
		  }
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 3;
	      }
	  }
	  break;

	case TC_QUAD:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		PUSH_FIXUP_DATA (old_start);
		(*free++) = (old_start[0]);
		(*free++) = (old_start[1]);
		(*free++) = (old_start[2]);
		(*free++) = (old_start[3]);
		MAYBE_DUMP_FREE (free);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += 4;
	      }
	  }
	  break;

	case TC_BIG_FIXNUM:
	case TC_CHARACTER_STRING:
	case TC_COMBINATION:
	case TC_CONTROL_POINT:
	case TC_NON_MARKED_VECTOR:
	case TC_PCOMB3:
	case TC_RECORD:
	case TC_VECTOR:
	case TC_VECTOR_16B:
	case TC_VECTOR_1B:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		PUSH_FIXUP_DATA (old_start);
		TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += n_words;
	      }
	  }
	  break;

	case TC_BIG_FLONUM:
	case TC_COMPILED_CODE_BLOCK:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else
	      {
		unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		PUSH_FIXUP_DATA (old_start);
		BCH_ALIGN_FLOAT (new_address, free);
		TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += n_words;
	      }
	  }
	  break;

	case TC_MANIFEST_NM_VECTOR:
	case TC_MANIFEST_SPECIAL_NM_VECTOR:
	  scan += (1 + (OBJECT_DATUM (object)));
	  MAYBE_DUMP_SCAN (scan);
	  break;

	case TC_REFERENCE_TRAP:
	  if ((OBJECT_DATUM (object)) > TRAP_MAX_IMMEDIATE)
	    goto transport_pair;
	  /* Otherwise it's a non-pointer.  */
	  scan += 1;
	  break;

	case TC_COMPILED_ENTRY:
	  compiled_code_present_p = true;
	  {
	    SCHEME_OBJECT * old_start;
	    Get_Compiled_Block (old_start, (OBJECT_ADDRESS (object)));
	    if (BROKEN_HEART_P (*old_start))
	      (*scan++)
		= (RELOCATE_COMPILED (object,
				      (OBJECT_ADDRESS (*old_start)),
				      old_start));
	    else
	      {
		unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		PUSH_FIXUP_DATA (old_start);
		BCH_ALIGN_FLOAT (new_address, free);
		TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		(*scan++)
		  = (RELOCATE_COMPILED (object, new_address, old_start));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += n_words;
	      }
	  }
	  break;

	case TC_LINKAGE_SECTION:
	  switch (READ_LINKAGE_KIND (object))
	    {
	    case REFERENCE_LINKAGE_KIND:
	    case ASSIGNMENT_LINKAGE_KIND:
	      {
		/* `count' typeless pointers to hunk3s follow. */
		unsigned long count = (READ_CACHE_LINKAGE_COUNT (object));
		scan += 1;
		while (count > 0)
		  {
		    SCHEME_OBJECT * old_start;
		    MAYBE_DUMP_SCAN (scan);
		    old_start = (SCHEME_ADDR_TO_ADDR (*scan));
		    if (BROKEN_HEART_P (*old_start))
		      (*scan++)
			= (ADDR_TO_SCHEME_ADDR (OBJECT_ADDRESS (*old_start)));
		    else
		      {
			PUSH_FIXUP_DATA (old_start);
			(*free++) = (old_start[0]);
			(*free++) = (old_start[1]);
			(*free++) = (old_start[2]);
			MAYBE_DUMP_FREE (free);
			(*scan++) = (ADDR_TO_SCHEME_ADDR (new_address));
			(*old_start) = (MAKE_BROKEN_HEART (new_address));
			new_address += 3;
		      }
		    count -= 1;
		  }
	      }
	      break;

	    case OPERATOR_LINKAGE_KIND:
	    case GLOBAL_OPERATOR_LINKAGE_KIND:
	      {
		unsigned long count = (READ_OPERATOR_LINKAGE_COUNT (object));
		char * entry = (FIRST_OPERATOR_LINKAGE_ENTRY (scan));
		long delta;

		if (count > 0)
		  compiled_code_present_p = true;

		{
		  int extend_p = (entry >= ((char *) scan_buffer_top));
		  long delta1 = (((char *) scan) - entry);
		  if (extend_p)
		    extend_scan_buffer (entry, free);
		  BCH_START_OPERATOR_RELOCATION (scan);
		  if (extend_p)
		    {
		      entry = (end_scan_buffer_extension (entry));
		      scan = ((SCHEME_OBJECT *) (entry + delta1));
		    }
		}

		/* END_OPERATOR_LINKAGE_AREA assumes that we will add
		   one to the result, so do that now.  */
		delta
		  = (((END_OPERATOR_LINKAGE_AREA (scan, count)) + 1)
		     - scan_buffer_top);

		/* The operator entries are copied sequentially, but
		   extra hair is required because the entry addresses
		   are encoded.  */
		while (count > 0)
		  {
		    char * next_entry = (NEXT_LINKAGE_OPERATOR_ENTRY (entry));
		    int extend_p = (next_entry >= ((char *) scan_buffer_top));
		    SCHEME_OBJECT esaddr;
		    SCHEME_OBJECT * old_start;

		    /* Guarantee that the scan buffer is large enough
		       to hold the entry.  */
		    if (extend_p)
		      extend_scan_buffer (next_entry, free);

		    /* Get the entry address.  */
		    BCH_EXTRACT_OPERATOR_LINKAGE_ADDRESS (esaddr, entry);

		    /* Get the code-block pointer for this entry.  */
		    Get_Compiled_Block
		      (old_start, (SCHEME_ADDR_TO_ADDR (esaddr)));

		    /* Copy the block.  */
		    if (BROKEN_HEART_P (*old_start))
		      {
			BCH_STORE_OPERATOR_LINKAGE_ADDRESS
			  ((RELOCATE_COMPILED_RAW_ADDRESS
			    (esaddr,
			     (OBJECT_ADDRESS (*old_start)),
			     old_start)),
			   entry);
		      }
		    else
		      {
			unsigned long n_words
			  = (1 + (OBJECT_DATUM (*old_start)));
			PUSH_FIXUP_DATA (old_start);
			BCH_ALIGN_FLOAT (new_address, free);
			TRANSPORT_VECTOR
			  (new_address, free, old_start, n_words);
			BCH_STORE_OPERATOR_LINKAGE_ADDRESS
			  ((RELOCATE_COMPILED_RAW_ADDRESS
			    (esaddr, new_address, old_start)),
			   entry);
			(*old_start) = (MAKE_BROKEN_HEART (new_address));
			new_address += n_words;
		      }

		    if (extend_p)
		      {
			entry = (end_scan_buffer_extension (next_entry));
			delta -= gc_buffer_size;
		      }
		    else
		      entry = next_entry;

		    count -= 1;
		  }
		scan = (scan_buffer_top + delta);
		MAYBE_DUMP_SCAN (scan);
		BCH_END_OPERATOR_RELOCATION (scan);
	      }
	      break;

	    case CLOSURE_PATTERN_LINKAGE_KIND:
	      scan += (1 + (READ_CACHE_LINKAGE_COUNT (object)));
	      MAYBE_DUMP_SCAN (scan);
	      break;

	    default:
	      gc_death (TERM_EXIT, "dump_loop: Unknown compiler linkage kind.",
			scan, free);
	      /*NOTREACHED*/
	      scan += 1;
	      break;
	    }
	  break;

	case TC_MANIFEST_CLOSURE:
	  {
	    unsigned long count;
	    char * entry;
	    char * closure_end;

	    {
	      unsigned long delta = (2 * (sizeof (format_word)));
	      char * count_end = (((char *) (scan + 1)) + delta);
	      int extend_p = (count_end >= ((char *) scan_buffer_top));

	      /* Guarantee that the scan buffer is large enough to
		 hold the count field.  */
	      if (extend_p)
		extend_scan_buffer (count_end, free);

	      BCH_START_CLOSURE_RELOCATION (scan);
	      count = (MANIFEST_CLOSURE_COUNT (scan + 1));
	      entry = (FIRST_MANIFEST_CLOSURE_ENTRY (scan + 1));

	      if (extend_p)
		{
		  long dw = (entry - count_end);
		  count_end = (end_scan_buffer_extension (count_end));
		  entry = (count_end + dw);
		}
	      scan = ((SCHEME_OBJECT *) (count_end - delta));
	    }

	    if (count > 0)
	      compiled_code_present_p = true;

	    closure_end = ((char *) (MANIFEST_CLOSURE_END (scan, count)));

	    /* The closures are copied sequentially, but extra hair is
	       required because the code-entry pointers are encoded as
	       machine instructions.  */
	    while (count > 0)
	      {
		char * entry_end = (CLOSURE_ENTRY_END (entry));
		int extend_p = (entry_end >= ((char *) scan_buffer_top));
		SCHEME_OBJECT esaddr;
		SCHEME_OBJECT * old_start;
		long delta1 = (entry - entry_end);
		long delta2 = (closure_end - entry_end);

		/* If the closure overflows the scan buffer, extend
		   the buffer to the end of the closure.  */
		if (extend_p)
		  extend_scan_buffer (entry_end, free);

		/* Extract the code-entry pointer and convert it to a
		   C pointer.  */
		BCH_EXTRACT_CLOSURE_ENTRY_ADDRESS (esaddr, entry);
		Get_Compiled_Block (old_start, (SCHEME_ADDR_TO_ADDR (esaddr)));

		/* Copy the code entry.  Use machine-specific macro to
		   update the pointer. */
		if (BROKEN_HEART_P (*old_start))
		  BCH_STORE_CLOSURE_ENTRY_ADDRESS
		    ((RELOCATE_COMPILED_RAW_ADDRESS
		      (esaddr, (OBJECT_ADDRESS (*old_start)), old_start)),
		     entry);
		else
		  {
		    unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		    PUSH_FIXUP_DATA (old_start);
		    BCH_ALIGN_FLOAT (new_address, free);
		    TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		    BCH_STORE_CLOSURE_ENTRY_ADDRESS
		      ((RELOCATE_COMPILED_RAW_ADDRESS
			(esaddr, new_address, old_start)),
		       entry);
		    (*old_start) = (MAKE_BROKEN_HEART (new_address));
		    new_address += n_words;
		  }

		if (extend_p)
		  {
		    entry_end = (end_scan_buffer_extension (entry_end));
		    entry = (entry_end + delta1);
		    closure_end = (entry_end + delta2);
		  }

		entry = (NEXT_MANIFEST_CLOSURE_ENTRY (entry));
		count -= 1;
	      }
	    scan = ((SCHEME_OBJECT *) closure_end);
	    MAYBE_DUMP_SCAN (scan);
	    BCH_END_CLOSURE_RELOCATION (scan);
	  }
	  break;

	case TC_ENVIRONMENT:
	  /* Make fasdump fail */
	  return (ERR_FASDUMP_ENVIRONMENT);

	case TC_FUTURE:
	  {
	    SCHEME_OBJECT * old_start = (OBJECT_ADDRESS (object));
	    if (BROKEN_HEART_P (*old_start))
	      (*scan++) = (MAKE_OBJECT_FROM_OBJECTS (object, (*old_start)));
	    else if (Future_Spliceable (object))
	      (*scan) = (Future_Value (object));
	    else
	      {
		unsigned long n_words = (1 + (OBJECT_DATUM (*old_start)));
		PUSH_FIXUP_DATA (old_start);
		TRANSPORT_VECTOR (new_address, free, old_start, n_words);
		(*scan++) = (OBJECT_NEW_ADDRESS (object, new_address));
		(*old_start) = (MAKE_BROKEN_HEART (new_address));
		new_address += n_words;
	      }
	  }
	  break;

	default:
	  GC_BAD_TYPE ("dump_loop", object);
	  scan += 1;
	  break;
	}
    }

 end_dump_loop:
  (*free_ptr) = free;
  (*new_address_ptr) = new_address;
  return (PRIM_DONE);
}
