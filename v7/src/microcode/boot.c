/* -*-C-*-

$Id: boot.c,v 9.118 2005/01/01 05:43:57 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1994,1995,1996,1997 Massachusetts Institute of Technology
Copyright 2000,2001,2002,2003,2004,2005 Massachusetts Institute of Technology

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

/* This file contains `main' and associated startup code. */

#include "scheme.h"
#include "prims.h"
#include "option.h"
#ifndef islower
#include <ctype.h>
#endif
#include "ostop.h"
#include "ostty.h"

extern PTR EXFUN (malloc, (unsigned int size));
extern void EXFUN (free, (PTR ptr));
extern void EXFUN (init_exit_scheme, (void));
extern void EXFUN (Clear_Memory, (int, int, int));
extern void EXFUN (Setup_Memory, (int, int, int));
extern void EXFUN (compiler_initialize, (long fasl_p));
extern SCHEME_OBJECT EXFUN (make_primitive, (char *, int));
extern void EXFUN (OS_announcement, (void));

static void EXFUN (Start_Scheme, (int, CONST char *));
static void EXFUN (Enter_Interpreter, (void));

CONST char * scheme_program_name;
CONST char * OS_Name;
CONST char * OS_Variant;
struct obstack scratch_obstack;
PTR initial_C_stack_pointer;
static char * reload_saved_string;
static unsigned int reload_saved_string_length;

/* If true, this is an executable created by dump-world. */
Boolean scheme_dumped_p = false;

PTR
DEFUN (obstack_chunk_alloc, (size), unsigned int size)
{
  PTR result = (malloc (size));
  if (result == 0)
    {
      outf_fatal ("\n%s: unable to allocate obstack chunk of %d bytes\n",
	       scheme_program_name, size);
      Microcode_Termination (TERM_EXIT);
    }
  return (result);
}

#define obstack_chunk_free free

#ifndef INIT_FIXED_OBJECTS
#define INIT_FIXED_OBJECTS initialize_fixed_objects_vector
#endif

/* Declare the outermost critical section. */
DECLARE_CRITICAL_SECTION ();

#define BLOCKS_TO_BYTES(n) ((n) * 1024)

/* Exit is done in a different way on some operating systems (eg. VMS)  */

#ifndef main_name
#define main_name main
#endif

#define FILE_READABLE(filename) ((access ((filename), 4)) >= 0)

int
DEFUN (main_name, (argc, argv),
       int argc AND CONST char ** argv)
{
  init_exit_scheme ();
  scheme_program_name = (argv[0]);
  initial_C_stack_pointer = ((PTR) (&argc));

#ifdef __WIN32__
  {
    extern void NT_initialize_win32_system_utilities();
    NT_initialize_win32_system_utilities ();
  }
#endif
#ifdef PREALLOCATE_HEAP_MEMORY
  PREALLOCATE_HEAP_MEMORY ();
#endif
#ifdef __OS2__
  {
    extern void OS2_initialize_early (void);
    OS2_initialize_early ();
  }
#endif
  obstack_init (&scratch_obstack);
  dstack_initialize ();
  transaction_initialize ();
  reload_saved_string = 0;
  reload_saved_string_length = 0;
  read_command_line_options (argc, argv);

  if (scheme_dumped_p)
    {
      extern SCHEME_OBJECT compiler_utilities;
      extern void EXFUN (compiler_reset, (SCHEME_OBJECT));

      if (! ((Heap_Size == ((long) option_heap_size))
	     && (Stack_Size == ((long) option_stack_size))
	     && (Constant_Size == ((long) option_constant_size))))
	{
	  outf_error ("%s: warning: ignoring allocation parameters.\n",
		      scheme_program_name);
	  outf_flush_error ();
	}
      OS_reset ();
      compiler_reset (compiler_utilities);
      if (!option_band_specified)
	{
	  outf_console ("Scheme Microcode Version %s\n", PACKAGE_VERSION);
	  OS_initialize ();
	  Enter_Interpreter ();
	}
      else
	{
	  Clear_Memory ((BLOCKS_TO_BYTES (Heap_Size)),
			(BLOCKS_TO_BYTES (Stack_Size)),
			(BLOCKS_TO_BYTES (Constant_Size)));
	  /* We are reloading from scratch anyway. */
	  scheme_dumped_p = false;
	  if (option_fasl_file)
	    Start_Scheme (BOOT_FASLOAD, option_fasl_file);
	  else
	    Start_Scheme (BOOT_LOAD_BAND, option_band_file);
	}
    }
  else
    {
      extern void EXFUN (initialize_primitives, (void));

      Heap_Size = option_heap_size;
      Stack_Size = option_stack_size;
      Constant_Size = option_constant_size;
      Setup_Memory ((BLOCKS_TO_BYTES (Heap_Size)),
		    (BLOCKS_TO_BYTES (Stack_Size)),
		    (BLOCKS_TO_BYTES (Constant_Size)));

#ifdef EMPTY_LIST_VALUE
      /* EMPTY_LIST_VALUE is defined if it is teh true value for '() and
         EMPTY_LIST is a location used to store '() or #F
	 */
      if (option_empty_list_eq_false)
	EMPTY_LIST = SHARP_F;
      else
	EMPTY_LIST = EMPTY_LIST_VALUE;
#endif

      initialize_primitives ();
      if (! option_fasl_file)
	{
	  compiler_initialize (0);
	  Start_Scheme (BOOT_LOAD_BAND, option_band_file);
	}
#ifdef NATIVE_CODE_IS_C
      else if (! (FILE_READABLE (option_fasl_file)))
      {
	compiler_initialize (1);
	Start_Scheme (BOOT_EXECUTE, option_fasl_file);
      }
#endif /* NATIVE_CODE_IS_C */
      else
	{
	  compiler_initialize (1);
	  Start_Scheme (BOOT_FASLOAD, option_fasl_file);
	}
    }
  termination_init_error ();
  return (0);
}

static SCHEME_OBJECT
DEFUN (names_to_vector, (length, names),
       unsigned int length AND
       unsigned char ** names)
{
  SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, length, 1));
  unsigned int i;
  for (i = 0; (i < length); i += 1)
    {
      VECTOR_SET (v, i, (char_pointer_to_symbol (names [i])));
    }
  return (v);
}

static SCHEME_OBJECT
DEFUN_VOID (fixed_objects_syscall_names)
{
  unsigned int length;
  unsigned char ** names;
  extern void EXFUN (OS_syscall_names, (unsigned int *, unsigned char ***));
  OS_syscall_names ((&length), (&names));
  return (names_to_vector (length, names));
}

static SCHEME_OBJECT
DEFUN_VOID (fixed_objects_syserr_names)
{
  unsigned int length;
  unsigned char ** names;
  extern void EXFUN (OS_syserr_names, (unsigned int *, unsigned char ***));
  OS_syserr_names ((&length), (&names));
  return (names_to_vector (length, names));
}

void
DEFUN_VOID (initialize_fixed_objects_vector)
{
  extern SCHEME_OBJECT EXFUN (initialize_history, (void));
  extern SCHEME_OBJECT EXFUN (initialize_interrupt_handler_vector, (void));
  extern SCHEME_OBJECT EXFUN (initialize_interrupt_mask_vector, (void));

  /* Create the fixed objects vector,
     with 4 extra slots for expansion and debugging. */
  fast SCHEME_OBJECT fixed_objects_vector =
    (make_vector ((NFixed_Objects + 4), SHARP_F, false));
  Fixed_Objects = fixed_objects_vector;
  FAST_VECTOR_SET (fixed_objects_vector, Me_Myself, fixed_objects_vector);
  FAST_VECTOR_SET
    (fixed_objects_vector, Non_Object, (MAKE_OBJECT (TC_CONSTANT, 2)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     System_Interrupt_Vector,
     (initialize_interrupt_handler_vector ()));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     FIXOBJ_INTERRUPT_MASK_VECTOR,
     (initialize_interrupt_mask_vector ()));
  /* Error vector is not needed at boot time */
  FAST_VECTOR_SET (fixed_objects_vector, System_Error_Vector, SHARP_F);
  FAST_VECTOR_SET
    (fixed_objects_vector,
     OBArray,
     (make_vector (OBARRAY_SIZE, EMPTY_LIST, false)));
  FAST_VECTOR_SET
    (fixed_objects_vector, Dummy_History, (initialize_history ()));
  FAST_VECTOR_SET (fixed_objects_vector, State_Space_Tag, SHARP_T);
  FAST_VECTOR_SET (fixed_objects_vector, Bignum_One, (long_to_bignum (1)));
  FAST_VECTOR_SET (fixed_objects_vector, FIXOBJ_EDWIN_AUTO_SAVE, EMPTY_LIST);
  FAST_VECTOR_SET (fixed_objects_vector, FIXOBJ_FILES_TO_DELETE, EMPTY_LIST);
  FAST_VECTOR_SET
    (fixed_objects_vector,
     FIXOBJ_SYSTEM_CALL_NAMES,
     (fixed_objects_syscall_names ()));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     FIXOBJ_SYSTEM_CALL_ERRORS,
     (fixed_objects_syserr_names ()));

  (*Free++) = EMPTY_LIST;
  (*Free++) = EMPTY_LIST;
  FAST_VECTOR_SET
    (fixed_objects_vector,
     The_Work_Queue,
     (MAKE_POINTER_OBJECT (TC_LIST, (Free - 2))));

  FAST_VECTOR_SET
    (fixed_objects_vector,
     Utilities_Vector,
     (make_vector (0, SHARP_F, false)));

  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_ZERO_P,
     (make_primitive ("INTEGER-ZERO?", 1)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_POSITIVE_P,
     (make_primitive ("INTEGER-POSITIVE?", 1)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_NEGATIVE_P,
     (make_primitive ("INTEGER-NEGATIVE?", 1)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_SUCCESSOR,
     (make_primitive ("INTEGER-ADD-1", 1)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_PREDECESSOR,
     (make_primitive ("INTEGER-SUBTRACT-1", 1)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_EQUAL_P,
     (make_primitive ("INTEGER-EQUAL?", 2)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_LESS_P,
     (make_primitive ("INTEGER-LESS?", 2)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_GREATER_P,
     (make_primitive ("INTEGER-GREATER?", 2)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_ADD,
     (make_primitive ("INTEGER-ADD", 2)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_SUBTRACT,
     (make_primitive ("INTEGER-SUBTRACT", 2)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_MULTIPLY,
     (make_primitive ("INTEGER-MULTIPLY", 2)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_DIVIDE,
     SHARP_F);
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_QUOTIENT,
     SHARP_F);
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_REMAINDER,
     SHARP_F);
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_MODULO,
     SHARP_F);

  FAST_VECTOR_SET
    (fixed_objects_vector,
     ARITY_DISPATCHER_TAG,
     char_pointer_to_symbol("#[(microcode)arity-dispatcher-tag]"));

#ifdef __WIN32__
  {
    extern void EXFUN (NT_initialize_fov, (SCHEME_OBJECT));
    NT_initialize_fov (fixed_objects_vector);
  }
#endif
}

/* Boot Scheme */

#ifndef ENTRY_HOOK
#  define ENTRY_HOOK() do { } while (0)
#endif

static void
DEFUN (Start_Scheme, (Start_Prim, File_Name),
       int Start_Prim AND CONST char * File_Name)
{
  SCHEME_OBJECT FName;
  SCHEME_OBJECT expr = SHARP_F;
  SCHEME_OBJECT * inner_arg;
  SCHEME_OBJECT prim;
  /* fast long i; */
  /* Parallel processor test */
  Boolean I_Am_Master = (Start_Prim != BOOT_GET_WORK);
  OS_initialize ();
  if (I_Am_Master)
    {
      if (!option_batch_mode)
	{
	  outf_console ("MIT/GNU Scheme running under %s\n", OS_Variant);
	  OS_announcement ();
	  outf_console ("\n");
	  outf_flush_console ();
	}
      Current_State_Point = SHARP_F;
      Fluid_Bindings = EMPTY_LIST;
      INIT_FIXED_OBJECTS ();
    }

  /* The initial program to execute is one of
        (SCODE-EVAL (BINARY-FASLOAD <file-name>) SYSTEM-GLOBAL-ENVIRONMENT),
	(LOAD-BAND <file-name>), or
	((GET-WORK))
	(SCODE-EVAL (INITIALIZE-C-COMPILED-BLOCK <file>) GLOBAL-ENV)
     depending on the value of Start_Prim. */
  switch (Start_Prim)
  {
    case BOOT_FASLOAD:	/* (SCODE-EVAL (BINARY-FASLOAD <file>) GLOBAL-ENV) */
      FName = (char_pointer_to_string ((unsigned char *) File_Name));
      prim = (make_primitive ("BINARY-FASLOAD", 1));
      inner_arg = Free;
      *Free++ = prim;
      *Free++ = FName;
      prim = (make_primitive ("SCODE-EVAL", 2));
      expr = MAKE_POINTER_OBJECT (TC_PCOMB2, Free);
      *Free++ = prim;
      *Free++ = MAKE_POINTER_OBJECT (TC_PCOMB1, inner_arg);
      *Free++ = THE_GLOBAL_ENV;
      break;

    case BOOT_LOAD_BAND:	/* (LOAD-BAND <file>) */
      FName = (char_pointer_to_string ((unsigned char *) File_Name));
      prim = (make_primitive ("LOAD-BAND", 1));
      inner_arg = Free;
      *Free++ = prim;
      *Free++ = FName;
      expr = MAKE_POINTER_OBJECT (TC_PCOMB1, inner_arg);
      break;

    case BOOT_GET_WORK:		/* ((GET-WORK)) */
      prim = (make_primitive ("GET-WORK", 0));
      inner_arg = Free;
      *Free++ = prim;
      *Free++ = SHARP_F;
      expr = MAKE_POINTER_OBJECT (TC_COMBINATION, Free);
      *Free++ = MAKE_OBJECT (TC_MANIFEST_VECTOR, 1);
      *Free++ = MAKE_POINTER_OBJECT (TC_PCOMB1, inner_arg);
      break;

    case BOOT_EXECUTE:
      /* (SCODE-EVAL (INITIALIZE-C-COMPILED-BLOCK <file>) GLOBAL-ENV) */
      FName = (char_pointer_to_string ((unsigned char *) File_Name));
      prim = (make_primitive ("INITIALIZE-C-COMPILED-BLOCK", 1));
      inner_arg = Free;
      *Free++ = prim;
      *Free++ = FName;
      prim = (make_primitive ("SCODE-EVAL", 2));
      expr = (MAKE_POINTER_OBJECT (TC_PCOMB2, Free));
      *Free++ = prim;
      *Free++ = (MAKE_POINTER_OBJECT (TC_PCOMB1, inner_arg));
      *Free++ = THE_GLOBAL_ENV;
      break;
      

    default:
      outf_fatal ("Unknown boot time option: %d\n", Start_Prim);
      Microcode_Termination (TERM_BAD_PRIMITIVE);
      /*NOTREACHED*/
  }

  /* Setup registers */
  INITIALIZE_INTERRUPTS ();
  SET_INTERRUPT_MASK (0);
  env_register = THE_GLOBAL_ENV;
  Trapping = false;
  Return_Hook_Address = NULL;

  /* Give the interpreter something to chew on, and ... */
 Will_Push (CONTINUATION_SIZE);
  Store_Return (RC_END_OF_COMPUTATION);
  exp_register = SHARP_F;
  Save_Cont ();
 Pushed ();

  exp_register = expr;

  /* Go to it! */
  if ((sp_register <= Stack_Guard) || (Free > MemTop))
  {
    outf_fatal ("Configuration won't hold initial data.\n");
    termination_init_error ();
  }
  ENTRY_HOOK ();
  Enter_Interpreter ();
}

#ifdef __WIN32__
   extern void EXFUN (win32_enter_interpreter, (void (*) (void)));
#  define HOOK_ENTER_INTERPRETER win32_enter_interpreter
#else
#  ifdef __OS2__
     extern void EXFUN (OS2_enter_interpreter, (void (*) (void)));
#    define HOOK_ENTER_INTERPRETER OS2_enter_interpreter
#  else
#    define HOOK_ENTER_INTERPRETER(func) func ()
#  endif
#endif

static void
DEFUN_VOID (Do_Enter_Interpreter)
{
  Interpret (scheme_dumped_p);
  outf_fatal ("\nThe interpreter returned to top level!\n");
  Microcode_Termination (TERM_EXIT);
}

static void
DEFUN_VOID (Enter_Interpreter)
{
  HOOK_ENTER_INTERPRETER (Do_Enter_Interpreter);
}

/* This must be used with care, and only synchronously. */

extern SCHEME_OBJECT EXFUN (Re_Enter_Interpreter, (void));

SCHEME_OBJECT
DEFUN_VOID (Re_Enter_Interpreter)
{
  Interpret (1);
  return (val_register);
}

/* Garbage collection debugging utilities. */

extern SCHEME_OBJECT
  *deadly_free,
  *deadly_scan;

extern unsigned long
  gc_counter;

extern void EXFUN (gc_death,
		   (long code, char *, SCHEME_OBJECT *, SCHEME_OBJECT *));
extern void EXFUN (stack_death, (CONST char *));

extern char
  gc_death_message_buffer[];

SCHEME_OBJECT
  *deadly_free,
  *deadly_scan;

unsigned long
  gc_counter = 0;

char
  gc_death_message_buffer[100];

void
DEFUN (gc_death, (code, message, scan, free),
       long code AND char * message
       AND SCHEME_OBJECT * scan AND SCHEME_OBJECT * free)
{
  outf_fatal ("\n%s.\n", message);
  outf_fatal ("scan = 0x%lx; free = 0x%lx\n", scan, free);
  deadly_scan = scan;
  deadly_free = free;
  Microcode_Termination (code);
  /*NOTREACHED*/
}

void
DEFUN (stack_death, (name), CONST char * name)
{
  outf_fatal
    ("\n%s: The stack has overflowed and overwritten adjacent memory.\n",
     name);
  outf_fatal ("This was probably caused by a runaway recursion.\n");
  Microcode_Termination (TERM_STACK_OVERFLOW);
  /*NOTREACHED*/
}

/* Utility primitives. */

#define IDENTITY_LENGTH 	20	/* Plenty of room */
#define ID_RELEASE		0	/* System release (string) */
#define ID_MICRO_VERSION	1	/* Microcode version (fixnum) */
#define ID_MICRO_MOD		2	/* Microcode modification (fixnum) */
#define ID_PRINTER_WIDTH	3	/* TTY width (# chars) */
#define ID_PRINTER_LENGTH	4	/* TTY height (# chars) */
#define ID_NEW_LINE_CHARACTER	5	/* #\Newline */
#define ID_FLONUM_PRECISION	6	/* Flonum mantissa (# bits) */
#define ID_FLONUM_EPSILON	7	/* Flonum epsilon (flonum) */
#define ID_OS_NAME		8	/* OS name (string) */
#define ID_OS_VARIANT		9	/* OS variant (string) */
#define ID_STACK_TYPE		10	/* Scheme stack type (string) */

#ifdef USE_STACKLETS
#define STACK_TYPE_STRING "stacklets"
#else
#define STACK_TYPE_STRING "standard"
#endif

DEFINE_PRIMITIVE ("MICROCODE-IDENTIFY", Prim_microcode_identify, 0, 0, 0)
{
  SCHEME_OBJECT Result;
  PRIMITIVE_HEADER (0);
  Result = (make_vector (IDENTITY_LENGTH, SHARP_F, true));
  FAST_VECTOR_SET (Result, ID_RELEASE, SHARP_F);
  FAST_VECTOR_SET
    (Result, ID_MICRO_VERSION,
     (char_pointer_to_string ((unsigned char *) PACKAGE_VERSION)));
  FAST_VECTOR_SET (Result, ID_MICRO_MOD, SHARP_F);
  FAST_VECTOR_SET
    (Result, ID_PRINTER_WIDTH, (LONG_TO_UNSIGNED_FIXNUM (OS_tty_x_size ())));
  FAST_VECTOR_SET
    (Result, ID_PRINTER_LENGTH, (LONG_TO_UNSIGNED_FIXNUM (OS_tty_y_size ())));
  FAST_VECTOR_SET
    (Result, ID_NEW_LINE_CHARACTER, (ASCII_TO_CHAR ('\n')));
  FAST_VECTOR_SET
    (Result, ID_FLONUM_PRECISION, (LONG_TO_UNSIGNED_FIXNUM (DBL_MANT_DIG)));
  FAST_VECTOR_SET
    (Result, ID_FLONUM_EPSILON, (double_to_flonum ((double) DBL_EPSILON)));
  FAST_VECTOR_SET
    (Result, ID_OS_NAME, (char_pointer_to_string ((unsigned char *) OS_Name)));
  FAST_VECTOR_SET (Result, ID_OS_VARIANT,
		   (char_pointer_to_string ((unsigned char *) OS_Variant)));
  FAST_VECTOR_SET (Result, ID_STACK_TYPE,
		   (char_pointer_to_string
		    ((unsigned char *) STACK_TYPE_STRING)));
  PRIMITIVE_RETURN (Result);
}

DEFINE_PRIMITIVE ("MICROCODE-SYSTEM-CALL-NAMES", Prim_microcode_syscall_names, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (fixed_objects_syscall_names ());
}

DEFINE_PRIMITIVE ("MICROCODE-SYSTEM-ERROR-NAMES", Prim_microcode_syserr_names, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (fixed_objects_syserr_names ());
}

DEFINE_PRIMITIVE ("MICROCODE-TABLES-FILENAME", Prim_microcode_tables_filename, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    (char_pointer_to_string ((unsigned char *) option_utabmd_file));
}

DEFINE_PRIMITIVE ("MICROCODE-LIBRARY-PATH", Prim_microcode_library_path, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    CONST char ** scan = option_library_path;
    CONST char ** end = option_library_path;
    while (1)
      {
	if ((*end) == 0)
	  break;
	end += 1;
      }
    {
      SCHEME_OBJECT result =
	(allocate_marked_vector (TC_VECTOR, (end - scan), 1));
      SCHEME_OBJECT * scan_result = (VECTOR_LOC (result, 0));
      while (scan < end)
	(*scan_result++) =
	  (char_pointer_to_string ((unsigned char *) *scan++));
      PRIMITIVE_RETURN (result);
    }
  }
}

static SCHEME_OBJECT
DEFUN (argv_to_object, (argc, argv), int argc AND CONST char ** argv)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, argc, 1));
  CONST char ** scan = argv;
  CONST char ** end = (scan + argc);
  SCHEME_OBJECT * scan_result = (VECTOR_LOC (result, 0));
  while (scan < end)
    (*scan_result++) = (char_pointer_to_string ((unsigned char *) *scan++));
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
	      char * scan = ((char *) (STRING_LOC ((ARG_REF (1)), 0)));
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
