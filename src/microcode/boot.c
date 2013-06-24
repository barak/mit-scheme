/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

/* This file contains `main' and associated startup code. */

#include "scheme.h"
#include "prims.h"
#include "option.h"
#include "ostop.h"

extern void init_exit_scheme (void);
extern void OS_announcement (void);
extern void initialize_fixed_objects_vector (void);
extern SCHEME_OBJECT Re_Enter_Interpreter (void);
extern SCHEME_OBJECT make_microcode_identification_vector (void);

#ifdef __WIN32__
   extern void NT_initialize_win32_system_utilities (void);
   extern void NT_initialize_fov (SCHEME_OBJECT);
   extern void win32_enter_interpreter (void (*) (void));
#  define HOOK_ENTER_INTERPRETER win32_enter_interpreter
#endif

#ifdef __OS2__
   extern void OS2_initialize_early (void);
   extern void OS2_enter_interpreter (void (*) (void));
#  define HOOK_ENTER_INTERPRETER OS2_enter_interpreter
#endif

#ifndef HOOK_ENTER_INTERPRETER
#  define HOOK_ENTER_INTERPRETER(func) func ()
#endif

static void start_scheme (void);
static void Enter_Interpreter (void);

const char * scheme_program_name;
const char * OS_Name;
const char * OS_Variant;
struct obstack scratch_obstack;
struct obstack ffi_obstack;
void * initial_C_stack_pointer;
static char * reload_saved_string;
static unsigned int reload_saved_string_length;

void *
obstack_chunk_alloc (size_t size)
{
  void * result = (malloc (size));
  if (result == 0)
    {
      outf_fatal ("\n%s: unable to allocate obstack chunk of %d bytes\n",
		  scheme_program_name, ((int) size));
      Microcode_Termination (TERM_EXIT);
    }
  return (result);
}

#define obstack_chunk_free free

/* Declare the outermost critical section. */
DECLARE_CRITICAL_SECTION ();

#define BLOCKS_TO_BYTES(n) ((n) * 1024)

/* Exit is done in a different way on some operating systems (eg. VMS)  */

#ifndef main_name
#define main_name main
#endif

int
main_name (int argc, const char ** argv)
{
  init_exit_scheme ();
  scheme_program_name = (argv[0]);
  initial_C_stack_pointer = ((void *) (&argc));

#ifdef __WIN32__
  NT_initialize_win32_system_utilities ();
#endif
#ifdef PREALLOCATE_HEAP_MEMORY
  PREALLOCATE_HEAP_MEMORY ();
#endif
#ifdef __OS2__
  OS2_initialize_early ();
#endif
  obstack_init (&scratch_obstack);
  obstack_init (&ffi_obstack);
  dstack_initialize ();
  transaction_initialize ();
  reload_saved_string = 0;
  reload_saved_string_length = 0;
  read_command_line_options (argc, argv);

  setup_memory ((BLOCKS_TO_BYTES (option_heap_size)),
		(BLOCKS_TO_BYTES (option_stack_size)),
		(BLOCKS_TO_BYTES (option_constant_size)));

  initialize_primitives ();
  compiler_initialize (option_fasl_file != 0);
  OS_initialize ();
  start_scheme ();
  termination_init_error ();
  return (0);
}

/* Boot Scheme */

#ifndef ENTRY_HOOK
#  define ENTRY_HOOK() do { } while (0)
#endif

static void
start_scheme (void)
{
  SCHEME_OBJECT expr;

  if (!option_batch_mode && !option_show_version && !option_show_help)
    {
      outf_console ("MIT/GNU Scheme running under %s\n", OS_Variant);
      OS_announcement ();
      outf_console ("\n");
      outf_flush_console ();
    }
  initialize_fixed_objects_vector ();

  if (option_fasl_file != 0)
    {
#ifdef CC_IS_C
      /* (SCODE-EVAL (INITIALIZE-C-COMPILED-BLOCK <file>) GLOBAL-ENV) */
      SCHEME_OBJECT prim1 = (make_primitive ("INITIALIZE-C-COMPILED-BLOCK", 1));
#else
      /* (SCODE-EVAL (BINARY-FASLOAD <file>) GLOBAL-ENV) */
      SCHEME_OBJECT prim1 = (make_primitive ("BINARY-FASLOAD", 1));
#endif
      SCHEME_OBJECT fn_object = (char_pointer_to_string (option_fasl_file));
      SCHEME_OBJECT prim2 = (make_primitive ("SCODE-EVAL", 2));
      SCHEME_OBJECT * inner_arg = Free;
      (*Free++) = prim1;
      (*Free++) = fn_object;
      expr = (MAKE_POINTER_OBJECT (TC_PCOMB2, Free));
      (*Free++) = prim2;
      (*Free++) = (MAKE_POINTER_OBJECT (TC_PCOMB1, inner_arg));
      (*Free++) = THE_GLOBAL_ENV;
    }
  else
    {
      /* (LOAD-BAND <file>) */
      SCHEME_OBJECT prim = (make_primitive ("LOAD-BAND", 1));
      SCHEME_OBJECT fn_object = (char_pointer_to_string (option_band_file));
      expr = (MAKE_POINTER_OBJECT (TC_PCOMB1, Free));
      (*Free++) = prim;
      (*Free++) = fn_object;
    }

  /* Setup registers */
  INITIALIZE_INTERRUPTS (0);
  SET_ENV (THE_GLOBAL_ENV);
  trapping = false;

  /* Give the interpreter something to chew on, and ... */
  Will_Push (CONTINUATION_SIZE);
  SET_RC (RC_END_OF_COMPUTATION);
  SET_EXP (SHARP_F);
  SAVE_CONT ();
  Pushed ();

  SET_EXP (expr);

  /* Go to it! */
  if (! ((SP_OK_P (stack_pointer)) && (Free <= heap_alloc_limit)))
    {
      outf_fatal ("Configuration won't hold initial data.\n");
      termination_init_error ();
    }
  ENTRY_HOOK ();
  Enter_Interpreter ();
}

static void
Do_Enter_Interpreter (void)
{
  Interpret (0);
  outf_fatal ("\nThe interpreter returned to top level!\n");
  Microcode_Termination (TERM_EXIT);
}

static void
Enter_Interpreter (void)
{
  HOOK_ENTER_INTERPRETER (Do_Enter_Interpreter);
}

/* This must be used with care, and only synchronously. */

SCHEME_OBJECT
Re_Enter_Interpreter (void)
{
  Interpret (0);
  return (GET_VAL);
}

/* Utility primitives. */

DEFINE_PRIMITIVE ("MICROCODE-IDENTIFY", Prim_microcode_identify, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (make_microcode_identification_vector ());
}

DEFINE_PRIMITIVE ("MICROCODE-LIBRARY-PATH", Prim_microcode_library_path, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    const char ** scan = option_library_path;
    const char ** end = option_library_path;
    while (1)
      {
	if ((*end) == 0)
	  break;
	end += 1;
      }
    {
      SCHEME_OBJECT result =
	(allocate_marked_vector (TC_VECTOR, (end - scan), true));
      SCHEME_OBJECT * scan_result = (VECTOR_LOC (result, 0));
      while (scan < end)
	(*scan_result++) = (char_pointer_to_string (*scan++));
      PRIMITIVE_RETURN (result);
    }
  }
}

static SCHEME_OBJECT
argv_to_object (int argc, const char ** argv)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, argc, 1));
  const char ** scan = argv;
  const char ** end = (scan + argc);
  SCHEME_OBJECT * scan_result = (VECTOR_LOC (result, 0));
  while (scan < end)
    (*scan_result++) = (char_pointer_to_string (*scan++));
  return (result);
}

DEFINE_PRIMITIVE ("GET-COMMAND-LINE", Prim_get_command_line, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (argv_to_object (option_saved_argc, option_saved_argv));
}

DEFINE_PRIMITIVE ("GET-UNUSED-COMMAND-LINE", Prim_get_unused_command_line, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  if (option_unused_argv == 0)
    PRIMITIVE_RETURN (SHARP_F);
  {
    SCHEME_OBJECT result =
      (argv_to_object (option_unused_argc, option_unused_argv));
    option_unused_argv = 0;
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("RELOAD-SAVE-STRING", Prim_reload_save_string, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  if (reload_saved_string != 0)
    {
      free (reload_saved_string);
      reload_saved_string = 0;
    }
  if ((ARG_REF (1)) != SHARP_F)
    {
      CHECK_ARG (1, STRING_P);
      {
	unsigned int length = (STRING_LENGTH (ARG_REF (1)));
	if (length > 0)
	  {
	    reload_saved_string = (OS_malloc (length));
	    reload_saved_string_length = length;
	    {
	      char * scan = (STRING_POINTER (ARG_REF (1)));
	      char * end = (scan + length);
	      char * scan_result = reload_saved_string;
	      while (scan < end)
		(*scan_result++) = (*scan++);
	    }
	  }
      }
    }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("RELOAD-RETRIEVE-STRING", Prim_reload_retrieve_string, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  if (reload_saved_string == 0)
    PRIMITIVE_RETURN (SHARP_F);
  {
    SCHEME_OBJECT result =
      (memory_to_string (reload_saved_string_length,
			 ((unsigned char *) reload_saved_string)));
    free (reload_saved_string);
    reload_saved_string = 0;
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("BATCH-MODE?", Prim_batch_mode_p, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (option_batch_mode));
}
