/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchdmp.c,v 9.59 1992/02/03 22:40:39 jinx Exp $

Copyright (c) 1987-1992 Massachusetts Institute of Technology

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

/* bchgcl, bchmmg, bchpur, and bchdmp can replace gcloop, memmag,
   purify, and fasdump, respectively, to provide garbage collection
   and related utilities to disk. */

#include "scheme.h"
#include "prims.h"
#include "uxio.h"
#include "osfile.h"
#include "trap.h"
#include "lookup.h"		/* UNCOMPILED_VARIABLE */
#define In_Fasdump
#include "bchgcc.h"
#include "fasl.h"
#include "ux.h"

static Tchannel dump_channel;

#define Write_Data(size, buffer)					\
  ((OS_channel_write_dump_file						\
    (dump_channel,							\
     ((char *) (buffer)),						\
     ((size) * (sizeof (SCHEME_OBJECT)))))				\
   / (sizeof (SCHEME_OBJECT)))

#include "dump.c"

extern SCHEME_OBJECT
  EXFUN (dump_renumber_primitive, (SCHEME_OBJECT)),
  * EXFUN (initialize_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *)),
  * EXFUN (cons_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *)),
  * EXFUN (cons_whole_primitive_table,
	   (SCHEME_OBJECT *, SCHEME_OBJECT *, long *));

static char *dump_file_name;
static int real_gc_file, dump_file;
static SCHEME_OBJECT *saved_free;
static SCHEME_OBJECT *fixup_buffer = ((SCHEME_OBJECT *) NULL);
static SCHEME_OBJECT *fixup_buffer_end;
static SCHEME_OBJECT *fixup;
static fixup_count = 0;
static Boolean compiled_code_present_p;

/* Utility macros. */

#define fasdump_remember_to_fix(location, contents)			\
{									\
  if ((fixup == fixup_buffer) && (!(reset_fixes ())))			\
  {									\
    return (PRIM_INTERRUPT);						\
  }									\
  *--fixup = contents;							\
  *--fixup = ((SCHEME_OBJECT) location);				\
}

#define fasdump_normal_setup()						\
{									\
  Old = (OBJECT_ADDRESS (Temp));					\
  if ((OBJECT_TYPE (*Old)) == TC_BROKEN_HEART)				\
  {									\
    *Scan = (MAKE_OBJECT_FROM_OBJECTS (Temp, *Old));			\
    continue;								\
  }									\
  New_Address = (MAKE_BROKEN_HEART (To_Address));			\
  fasdump_remember_to_fix (Old, *Old);					\
}

#ifdef FLOATING_ALIGNMENT

#define fasdump_flonum_setup()						\
{									\
  Old = (OBJECT_ADDRESS (Temp));					\
  if ((OBJECT_TYPE (*Old)) == TC_BROKEN_HEART)				\
  {									\
    *Scan = (MAKE_OBJECT_FROM_OBJECTS (Temp, *Old));			\
    continue;								\
  }									\
  FLOAT_ALIGN_FREE (To_Address, To);					\
  New_Address = (MAKE_BROKEN_HEART (To_Address));			\
  fasdump_remember_to_fix (Old, *Old);					\
}

#else /* FLOATING_ALIGNMENT */

#define fasdump_flonum_setup()	fasdump_normal_setup ()

#endif /* FLOATING_ALIGNMENT */

#define fasdump_transport_end(length)					\
{									\
  To_Address += (length);						\
  if (To >= free_buffer_top)						\
  {									\
    To = (dump_and_reset_free_buffer ((To - free_buffer_top),		\
				      &success));			\
    if (!success)							\
    {									\
      return (PRIM_INTERRUPT);						\
    }									\
  }									\
}

#define fasdump_normal_transport(copy_code, length)			\
{									\
  copy_code;								\
  fasdump_transport_end (length);					\
}

#define fasdump_normal_end()						\
{									\
  *(OBJECT_ADDRESS (Temp)) = New_Address;				\
  *Scan = (MAKE_OBJECT_FROM_OBJECTS (Temp, New_Address));		\
  continue;								\
}

#define fasdump_normal_pointer(copy_code, length)			\
{									\
  fasdump_normal_setup ();						\
  fasdump_normal_transport (copy_code, length);				\
  fasdump_normal_end ();						\
}

#define fasdump_typeless_setup()					\
{									\
  Old = ((SCHEME_OBJECT *) Temp);					\
  if (OBJECT_TYPE (*Old) == TC_BROKEN_HEART)				\
  {									\
    *Scan = ((SCHEME_OBJECT) OBJECT_ADDRESS (*Old));			\
    continue;								\
  }									\
  New_Address = ((SCHEME_OBJECT) To_Address);				\
  fasdump_remember_to_fix (Old, *Old);					\
}

#define fasdump_typeless_end()						\
{									\
  (* (OBJECT_ADDRESS (Temp))) = (MAKE_BROKEN_HEART (New_Address));	\
  *Scan = ((SCHEME_OBJECT) New_Address);				\
  continue;								\
}

#define fasdump_typeless_pointer(copy_code, length)			\
{									\
  fasdump_typeless_setup ();						\
  fasdump_normal_transport (copy_code, length);				\
  fasdump_typeless_end ();						\
}

#define fasdump_compiled_entry()					\
do {									\
  compiled_code_present_p = true;					\
  Old = OBJECT_ADDRESS (Temp);						\
  Compiled_BH (false, continue);					\
  {									\
    SCHEME_OBJECT *Saved_Old = Old;					\
									\
    fasdump_remember_to_fix (Old, *Old);				\
    New_Address = (MAKE_BROKEN_HEART (To_Address));			\
    copy_vector (&success);						\
    if (!success)							\
    {									\
      return (PRIM_INTERRUPT);						\
    }									\
    *Saved_Old = New_Address;						\
    Temp = RELOCATE_COMPILED (Temp, (OBJECT_ADDRESS (New_Address)),	\
			      Saved_Old);				\
    continue;								\
  }									\
} while (false)

#define fasdump_linked_operator()					\
{									\
  Scan = ((SCHEME_OBJECT *) (word_ptr));				\
  EXTRACT_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);			\
  fasdump_compiled_entry ();						\
  STORE_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);				\
}

#define fasdump_manifest_closure()					\
{									\
  Scan = ((SCHEME_OBJECT *) (word_ptr));				\
  EXTRACT_CLOSURE_ENTRY_ADDRESS (Temp, Scan);				\
  fasdump_compiled_entry ();						\
  STORE_CLOSURE_ENTRY_ADDRESS (Temp, Scan);				\
}

Boolean
DEFUN (fasdump_exit, (length), long length)
{
  fast SCHEME_OBJECT *fixes, *fix_address;
  Boolean result;

  Free = saved_free;
  restore_gc_file ();

#if TRUE
  {
    extern int EXFUN (ftruncate, (int, unsigned long));

    ftruncate (dump_file, length);
    result = ((close (dump_file)) == 0);
  }
#else
  {
    extern int EXFUN (truncate, (const char *, unsigned long));

    result = (close (dump_file) == 0);
    truncate (dump_file_name, length);
  }
#endif

  if (length == 0)
  {
    extern int EXFUN (unlink, (const char *));

    (void) (unlink (dump_file_name));
  }
  dump_file_name = ((char *) NULL);

  fixes = fixup;

next_buffer:

  while (fixes != fixup_buffer_end)
  {
    fix_address = ((SCHEME_OBJECT *) (*fixes++));	/* Where it goes. */
    *fix_address = *fixes++;				/* Put it there. */
  }

  if (fixup_count >= 0)
  {
    if ((retrying_file_operation
	 (read, real_gc_file, ((char *) fixup_buffer),
	  (gc_file_start_position + (fixup_count << gc_buffer_byte_shift)),
	  gc_buffer_bytes, "read", "the fixup buffer",
	  &gc_file_current_position, io_error_retry_p))
	!= gc_buffer_bytes)
    {
      gc_death (TERM_EXIT,
		"fasdump: Could not read back the fasdump fixup information",
		NULL, NULL);
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

Boolean
DEFUN_VOID (reset_fixes)
{
  long start;

  fixup_count += 1;
  start = (gc_file_start_position + (fixup_count << gc_buffer_byte_shift));

  if (((start + gc_buffer_bytes) > gc_file_end_position)
      || ((retrying_file_operation
	   (write, real_gc_file, ((char *) fixup_buffer),
	    start, gc_buffer_bytes, "write", "the fixup buffer",
	    &gc_file_current_position, io_error_always_abort))
	  != gc_buffer_bytes))
    return (false);
  fixup = fixup_buffer_end;
  return (true);
}

/* A copy of GCLoop, with minor modifications. */

long
DEFUN (dumploop, (Scan, To_ptr, To_Address_ptr),
       fast SCHEME_OBJECT *Scan AND
       SCHEME_OBJECT **To_ptr AND
       SCHEME_OBJECT **To_Address_ptr)
{
  fast SCHEME_OBJECT *To, *Old, Temp, *To_Address, New_Address;
  Boolean success;

  success = true;
  To = *To_ptr;
  To_Address = *To_Address_ptr;

  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;
    Switch_by_GC_Type (Temp)
    {
      case TC_BROKEN_HEART:
        if ((OBJECT_DATUM (Temp)) == 0)
	{
	  break;
	}
        if (Temp != (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, Scan)))
	{
	  sprintf (gc_death_message_buffer,
		   "purifyloop: broken heart (0x%lx) in scan",
		   Temp);
	  gc_death (TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	  /*NOTREACHED*/
	}
	if (Scan != scan_buffer_top)
	{
	  goto end_dumploop;
	}

	/* The -1 is here because of the Scan++ in the for header. */

	Scan = ((dump_and_reload_scan_buffer (0, &success)) - 1);
	if (!success)
	{
	  return (PRIM_INTERRUPT);
	}
	continue;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	/* Check whether this bumps over current buffer,
	   and if so we need a new bufferfull. */
	Scan += (OBJECT_DATUM (Temp));
	if (Scan < scan_buffer_top)
	{
	  break;
	}
	else
	{
	  unsigned long overflow;

	  /* The + & -1 are here because of the Scan++ in the for header. */
	  overflow = ((Scan - scan_buffer_top) + 1);
	  Scan = (((dump_and_reload_scan_buffer ((overflow >> gc_buffer_shift),
						 &success)) +
		   (overflow & gc_buffer_mask)) - 1);
	  if (!success)
	  {
	    return (PRIM_INTERRUPT);
	  }
	  break;
	}

      case TC_PRIMITIVE:
      case TC_PCOMB0:
	*Scan = (dump_renumber_primitive (*Scan));
	break;

      case_compiled_entry_point:
	fasdump_compiled_entry ();
	*Scan = Temp;
	break;

      case TC_LINKAGE_SECTION:
      {
	switch (READ_LINKAGE_KIND (Temp))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    /* count typeless pointers to quads follow. */

	    fast long count;
	    long max_count, max_here;

	    Scan++;
	    max_here = (scan_buffer_top - Scan);
	    max_count = (READ_CACHE_LINKAGE_COUNT (Temp));
	    while (max_count != 0)
	    {
	      count = ((max_count > max_here) ? max_here : max_count);
	      max_count -= count;
	      for ( ; --count >= 0; Scan += 1)
	      {
		Temp = *Scan;
		fasdump_typeless_pointer (copy_quadruple (), 4);
	      }
	      if (max_count != 0)
	      {
		/* We stopped because we needed to relocate too many. */
		Scan = (dump_and_reload_scan_buffer (0, NULL));
		max_here = gc_buffer_size;
	      }
	    }
	    /* The + & -1 are here because of the Scan++ in the for header. */
	    Scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    /* Operator linkage */

	    fast long count;
	    fast char *word_ptr, *next_ptr;
	    long overflow;

	    count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	    overflow = ((END_OPERATOR_LINKAGE_AREA (Scan, count)) -
			scan_buffer_top);

	    for (next_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
		 (--count >= 0);
		 word_ptr = next_ptr,
		 next_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr)))
	    {
	      if (next_ptr > ((char *) scan_buffer_top))
	      {
		extend_scan_buffer (((char *) next_ptr), To);
		fasdump_linked_operator ();
		next_ptr = ((char *)
			    (end_scan_buffer_extension ((char *) next_ptr)));
		overflow -= gc_buffer_size;
	      }
	      else
	      {
		fasdump_linked_operator ();
	      }
	    }
	    Scan = (scan_buffer_top + overflow);
	    break;
	  }

	  default:
	  {
	    gc_death (TERM_EXIT,
		      "fasdump: Unknown compiler linkage kind.",
		      Scan, Free);
	    /*NOTREACHED*/
	  }
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char *word_ptr;
	char *end_ptr;

	Scan += 1;
	/* Is there enough space to read the count? */
	if ((((char *) Scan) + (2 * (sizeof (format_word)))) >
	    ((char *) scan_buffer_top))
	{
	  long dw;
	  char *header_end;

	  header_end = (((char *) Scan) + (2 * (sizeof (format_word))));
	  extend_scan_buffer (((char *) header_end), To);
	  count = (MANIFEST_CLOSURE_COUNT (Scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	  dw = (word_ptr - header_end);
	  header_end = ((char *)
			(end_scan_buffer_extension ((char *) header_end)));
	  word_ptr = (header_end + dw);
	  Scan = ((SCHEME_OBJECT *)
		  (header_end - (2 * (sizeof (format_word)))));
	}
	else
	{
	  count = (MANIFEST_CLOSURE_COUNT (Scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	}
	end_ptr = ((char *) (MANIFEST_CLOSURE_END (Scan, count)));

	for ( ; ((--count) >= 0);
	     (word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr))))
	{
	  if ((CLOSURE_ENTRY_END (word_ptr)) > ((char *) scan_buffer_top))
	  {
	    char *entry_end;
	    long de, dw;

	    entry_end = (CLOSURE_ENTRY_END (word_ptr));
	    de = (end_ptr - entry_end);
	    dw = (entry_end - word_ptr);
	    extend_scan_buffer (((char *) entry_end), To);
	    fasdump_manifest_closure ();
	    entry_end = ((char *)
			 (end_scan_buffer_extension ((char *) entry_end)));
	    word_ptr = (entry_end - dw);
	    end_ptr = (entry_end + de);
	  }
	  else
	  {
	    fasdump_manifest_closure ();
	  }
	}
	Scan = ((SCHEME_OBJECT *) (end_ptr));
	break;
      }

      case_Cell:
	fasdump_normal_pointer (copy_cell (), 1);

      case TC_REFERENCE_TRAP:
	if ((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* It is a pair, fall through. */

      case TC_WEAK_CONS:
      case_Fasdump_Pair:
	fasdump_normal_pointer (copy_pair (), 2);

      case TC_INTERNED_SYMBOL:
      {
	fasdump_normal_setup ();
	*To++ = *Old;
	*To++ = BROKEN_HEART_ZERO;
	fasdump_transport_end (2);
	fasdump_normal_end ();
      }

      case TC_UNINTERNED_SYMBOL:
      {
	fasdump_normal_setup ();
	*To++ = *Old;
	*To++ = UNBOUND_OBJECT;
	fasdump_transport_end (2);
	fasdump_normal_end ();
      }

      case_Triple:
	fasdump_normal_pointer (copy_triple (), 3);

      case TC_VARIABLE:
      {
	fasdump_normal_setup ();
	*To++ = *Old;
	*To++ = UNCOMPILED_VARIABLE;
	*To++ = SHARP_F;
	fasdump_transport_end (3);
	fasdump_normal_end ();
      }

      case_Quadruple:
	fasdump_normal_pointer (copy_quadruple (), 4);

      case TC_BIG_FLONUM:
	fasdump_flonum_setup ();
	goto Move_Vector;

      case TC_COMPILED_CODE_BLOCK:
      case_Purify_Vector:
	fasdump_normal_setup ();
      Move_Vector:
	copy_vector (&success);
	if (!success)
	{
	  return (PRIM_INTERRUPT);
	}
	fasdump_normal_end ();

      case TC_ENVIRONMENT:
	/* Make fasdump fail */
	return (ERR_FASDUMP_ENVIRONMENT);

      case TC_FUTURE:
	fasdump_normal_setup ();
	if (!(Future_Spliceable (Temp)))
	{
	  goto Move_Vector;
	}
	*Scan = (Future_Value (Temp));
	Scan -= 1;
	continue;

      default:
	GC_BAD_TYPE ("dumploop");
	/* Fall Through */

      case TC_STACK_ENVIRONMENT:
      case_Fasload_Non_Pointer:
	break;

      }
  }

end_dumploop:

  *To_ptr = To;
  *To_Address_ptr = To_Address;
  return (PRIM_DONE);
}

static SCHEME_OBJECT
DEFUN (dump_to_file, (root, fname),
       SCHEME_OBJECT root AND
       char *fname)
{
  Boolean success;
  long value, length, hlength, tlength, tsize;
  SCHEME_OBJECT *dumped_object, *free_buffer, *dummy;
  SCHEME_OBJECT *table_start, *table_end, *table_top;
  SCHEME_OBJECT header[FASL_HEADER_LENGTH];

  if (fixup_buffer == ((SCHEME_OBJECT *) NULL))
  {
    fixup_buffer = ((SCHEME_OBJECT *) (malloc (gc_buffer_bytes)));
    if (fixup_buffer == ((SCHEME_OBJECT *) NULL))
      error_system_call (errno, syscall_malloc);
    fixup_buffer_end = (fixup_buffer + gc_buffer_size);
  }

  dump_file_name = fname;
  dump_file = (open (dump_file_name, GC_FILE_FLAGS, 0666));
  if (dump_file < 0)
    error_bad_range_arg (2);

  compiled_code_present_p = false;
  success = true;
  real_gc_file = (swap_gc_file (dump_file));
  saved_free = Free;
  fixup = fixup_buffer_end;
  fixup_count = -1;

  table_top = (&saved_free[Space_Before_GC ()]);
  table_start = (initialize_primitive_table (saved_free, table_top));
  if (table_start >= table_top)
  {
    fasdump_exit (0);
    Primitive_GC (table_start - saved_free);
  }

  free_buffer = (initialize_free_buffer ());
  Free = ((SCHEME_OBJECT *) NULL);
  free_buffer += FASL_HEADER_LENGTH;

  dummy = free_buffer;
  FLOAT_ALIGN_FREE (Free, dummy);

  *free_buffer++ = root;
  dumped_object = Free;
  Free += 1;

  value = dumploop (((initialize_scan_buffer ()) + FASL_HEADER_LENGTH),
		    &free_buffer, &Free);
  if (value != PRIM_DONE)
  {
    fasdump_exit (0);
    if (value == PRIM_INTERRUPT)
    {
      return (SHARP_F);
    }
    else
    {
      signal_error_from_primitive (value);
    }
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

  tsize = (table_end - table_start);
  hlength = ((sizeof (SCHEME_OBJECT)) * tsize);
  if (((lseek (dump_file,
	       ((sizeof (SCHEME_OBJECT)) * (length + FASL_HEADER_LENGTH)),
	       0))
       == -1)
      || ((write (dump_file, ((char *) &table_start[0]), hlength)) != hlength))
  {
    fasdump_exit (0);
    return (SHARP_F);
  }

  hlength = ((sizeof (SCHEME_OBJECT)) * FASL_HEADER_LENGTH);
  prepare_dump_header (header, dumped_object, length, dumped_object,
		       0, Constant_Space, tlength, tsize,
		       compiled_code_present_p, false);
  if (((lseek (dump_file, 0, 0)) == -1)
      || ((write (dump_file, ((char *) &header[0]), hlength)) != hlength))
  {
    fasdump_exit (0);
    return (SHARP_F);
  }
  return (fasdump_exit (((sizeof (SCHEME_OBJECT)) *
			 (length + tsize)) + hlength) ?
	  SHARP_T : SHARP_F);
}

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
   completion, the file is copied to the channel.

*/

DEFINE_PRIMITIVE ("PRIMITIVE-FASDUMP", Prim_prim_fasdump, 3, 3, 0)
{
  SCHEME_OBJECT root;
  PRIMITIVE_HEADER (3);

  root = (ARG_REF (1));

  if (STRING_P (ARG_REF (2)))
  {
    PRIMITIVE_RETURN (dump_to_file (root, (STRING_ARG (2))));
  }
  else
  {
    extern char * EXFUN (mktemp, (char *));
    extern int EXFUN (OS_channel_copy,
		      (off_t source_length,
		       Tchannel source_channel,
		       Tchannel destination_channel));

    int copy_result;
    SCHEME_OBJECT fasdump_result;
    Tchannel channel, temp_channel;
    char temp_name [19];
    {
      char * scan1 = "/tmp/fasdumpXXXXXX";
      char * scan2 = temp_name;
      while (1)
	if (((*scan2++) = (*scan1++)) == '\0')
	  break;
    }
    channel = (arg_channel (2));

    (void) mktemp (temp_name);
    fasdump_result = (dump_to_file (root, (temp_name)));
    if (fasdump_result != SHARP_T)
    {
      PRIMITIVE_RETURN (fasdump_result);
    }

    temp_channel = (OS_open_input_file (temp_name));
    copy_result = (OS_channel_copy ((OS_file_length (temp_channel)),
				    temp_channel,
				    channel));
    OS_channel_close (temp_channel);
    OS_file_remove (temp_name);
    if (copy_result < 0)
    {
      signal_error_from_primitive (ERR_IO_ERROR);
    }
    PRIMITIVE_RETURN (SHARP_T);
  }
}

/* (DUMP-BAND PROCEDURE FILE-NAME)
   Saves all of the heap and pure space on FILE-NAME.  When the
   file is loaded back using BAND_LOAD, PROCEDURE is called with an
   argument of #F.  */

DEFINE_PRIMITIVE ("DUMP-BAND", Prim_band_dump, 2, 2, 0)
{
  extern SCHEME_OBJECT compiler_utilities;
  SCHEME_OBJECT Combination, *table_start, *table_end, *saved_free;
  long table_length;
  Boolean result;
  PRIMITIVE_HEADER (2);

  Band_Dump_Permitted ();
  CHECK_ARG (1, INTERPRETER_APPLICABLE_P);
  CHECK_ARG (2, STRING_P);
  Primitive_GC_If_Needed (5);
  saved_free = Free;
  Combination = (MAKE_POINTER_OBJECT (TC_COMBINATION_1, Free));
  Free[COMB_1_FN] = (ARG_REF (1));
  Free[COMB_1_ARG_1] = SHARP_F;
  Free += 2;
  *Free++ = Combination;
  *Free++ = compiler_utilities;
  *Free = (MAKE_POINTER_OBJECT (TC_LIST, (Free - 2)));
  Free++;  /* Some compilers are TOO clever about this and increment Free
	      before calculating Free-2! */
  table_start = Free;
  table_end = (cons_whole_primitive_table (Free, Heap_Top, &table_length));
  if (table_end >= Heap_Top)
  {
    result = false;
  }
  else
  {
    CONST char * filename = ((CONST char *) (STRING_LOC ((ARG_REF (2)), 0)));
    dump_channel = (OS_open_dump_file (filename));
    if (dump_channel == NO_CHANNEL)
    {
      error_bad_range_arg (2);
    }
    result = (Write_File ((Free - 1),
			  ((long) (Free - Heap_Bottom)), Heap_Bottom,
			  ((long) (Free_Constant - Constant_Space)),
			  Constant_Space,
			  table_start, table_length,
			  ((long) (table_end - table_start)),
			  (compiler_utilities != SHARP_F), true));
    OS_channel_close_noerror (dump_channel);
    if (!result)
    {
      OS_file_remove (filename);
    }
  }
  Band_Dump_Exit_Hook ();
  Free = saved_free;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (result));
}
