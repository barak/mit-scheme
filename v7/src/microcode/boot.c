/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/boot.c,v 9.64 1990/11/13 08:44:14 cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

/* This file contains `main' and associated startup code. */

#include "scheme.h"
#include "prims.h"
#include "version.h"
#include "option.h"
#ifndef islower
#include <ctype.h>
#endif
#include "ostop.h"

extern PTR EXFUN (malloc, (unsigned int size));
extern void EXFUN (free, (PTR ptr));
extern void EXFUN (init_exit_scheme, (void));
extern void Clear_Memory ();
extern void Setup_Memory ();
extern void compiler_initialize ();

forward void Start_Scheme ();
forward void Enter_Interpreter ();

CONST char * scheme_program_name;
CONST char * OS_Name;
CONST char * OS_Variant;
struct obstack scratch_obstack;
PTR initial_C_stack_pointer;

/* If true, this is an executable created by dump-world. */
Boolean scheme_dumped_p = false;

PTR
DEFUN (obstack_chunk_alloc, (size), unsigned int size)
{
  PTR result = (malloc (size));
  if (result == 0)
    {
      fprintf (stderr, "\n%s: unable to allocate obstack chunk of %d bytes\n",
	       scheme_program_name, size);
      fflush (stderr);
      Microcode_Termination (TERM_EXIT);
    }
  return (result);
}

#define obstack_chunk_free free

#ifndef INIT_FIXED_OBJECTS
#define INIT_FIXED_OBJECTS() Fixed_Objects = (make_fixed_objects_vector ())
#endif

/* Declare the outermost critical section. */
DECLARE_CRITICAL_SECTION ();

#define BLOCKS_TO_BYTES(n) ((n) * 1024)

static void
DEFUN (usage, (error_string), CONST char * error_string)
{
  fprintf (stderr, "%s: %s\n\n", scheme_program_name, error_string);
  fflush (stderr);
  termination_init_error ();
}

/* Exit is done in a different way on some operating systems (eg. VMS)  */

#ifndef main_type
#define main_type void
#endif

main_type
main (argc, argv)
     int argc;
     CONST char ** argv;
{
  init_exit_scheme ();
  scheme_program_name = (argv[0]);
  initial_C_stack_pointer = (&argc);
  obstack_init (&scratch_obstack);
  read_command_line_options (argc, argv);
  if (scheme_dumped_p)
    {
      if (! ((Heap_Size == option_heap_size)
	     && (Stack_Size == option_stack_size)
	     && (Constant_Size == option_constant_size)))
	{
	  fprintf (stderr, "%s: warning: ignoring allocation parameters.\n",
		   scheme_program_name);
	  fflush (stderr);
	}
      OS_reset ();
      if (!option_band_specified)
	{
	  printf ("Scheme Microcode Version %d.%d\n", VERSION, SUBVERSION);
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
      Heap_Size = option_heap_size;
      Stack_Size = option_stack_size;
      Constant_Size = option_constant_size;
      Setup_Memory ((BLOCKS_TO_BYTES (Heap_Size)),
		    (BLOCKS_TO_BYTES (Stack_Size)),
		    (BLOCKS_TO_BYTES (Constant_Size)));
      if (option_fasl_file)
	{
	  compiler_initialize (1);
	  Start_Scheme (BOOT_FASLOAD, option_fasl_file);
	}
      else
	{
	  compiler_initialize (0);
	  Start_Scheme (BOOT_LOAD_BAND, option_band_file);
	}
    }
  termination_init_error ();
}

SCHEME_OBJECT
make_fixed_objects_vector ()
{
  extern SCHEME_OBJECT initialize_history ();
  extern SCHEME_OBJECT make_primitive ();
  /* Create the fixed objects vector,
     with 4 extra slots for expansion and debugging. */
  fast SCHEME_OBJECT fixed_objects_vector =
    (make_vector ((NFixed_Objects + 4), SHARP_F, false));
  FAST_VECTOR_SET (fixed_objects_vector, Me_Myself, fixed_objects_vector);
  FAST_VECTOR_SET
    (fixed_objects_vector, Non_Object, (MAKE_OBJECT (TC_TRUE, 2)));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     System_Interrupt_Vector,
     (make_vector ((MAX_INTERRUPT_NUMBER + 2), SHARP_F, false)));
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
     (make_primitive ("INTEGER-ZERO?")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_POSITIVE_P,
     (make_primitive ("INTEGER-POSITIVE?")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_NEGATIVE_P,
     (make_primitive ("INTEGER-NEGATIVE?")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_SUCCESSOR,
     (make_primitive ("INTEGER-ADD-1")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_PREDECESSOR,
     (make_primitive ("INTEGER-SUBTRACT-1")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_EQUAL_P,
     (make_primitive ("INTEGER-EQUAL?")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_LESS_P,
     (make_primitive ("INTEGER-LESS?")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_GREATER_P,
     (make_primitive ("INTEGER-GREATER?")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_ADD,
     (make_primitive ("INTEGER-ADD")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_SUBTRACT,
     (make_primitive ("INTEGER-SUBTRACT")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_MULTIPLY,
     (make_primitive ("INTEGER-MULTIPLY")));
  FAST_VECTOR_SET
    (fixed_objects_vector,
     GENERIC_TRAMPOLINE_DIVIDE,
     SHARP_F);

  return (fixed_objects_vector);
}

/* Boot Scheme */

void
Start_Scheme (Start_Prim, File_Name)
     int Start_Prim;
     char * File_Name;
{
  extern SCHEME_OBJECT make_primitive ();
  SCHEME_OBJECT FName, Init_Prog, *Fasload_Call, prim;
  fast long i;
  /* Parallel processor test */
  Boolean I_Am_Master = (Start_Prim != BOOT_GET_WORK);
  if (I_Am_Master)
    {
      fprintf (stdout, "Scheme Microcode Version %d.%d\n",
	       VERSION, SUBVERSION);
      fflush (stdout);
    }
  OS_initialize ();
  if (I_Am_Master)
  {
    Current_State_Point = SHARP_F;
    Fluid_Bindings = EMPTY_LIST;
    INIT_FIXED_OBJECTS ();
  }

  /* The initial program to execute is one of
        (SCODE-EVAL (BINARY-FASLOAD <file-name>) SYSTEM-GLOBAL-ENVIRONMENT),
	(LOAD-BAND <file-name>), or
	((GET-WORK))
     depending on the value of Start_Prim. */
  switch (Start_Prim)
  {
    case BOOT_FASLOAD:	/* (SCODE-EVAL (BINARY-FASLOAD <file>) GLOBAL-ENV) */
      FName = char_pointer_to_string(File_Name);
      prim = make_primitive("BINARY-FASLOAD");
      Fasload_Call = Free;
      *Free++ = prim;
      *Free++ = FName;
      prim = make_primitive("SCODE-EVAL");
      Init_Prog = MAKE_POINTER_OBJECT (TC_PCOMB2, Free);
      *Free++ = prim;
      *Free++ = MAKE_POINTER_OBJECT (TC_PCOMB1, Fasload_Call);
      *Free++ = MAKE_OBJECT (GLOBAL_ENV, GO_TO_GLOBAL);
      break;

    case BOOT_LOAD_BAND:	/* (LOAD-BAND <file>) */
      FName = char_pointer_to_string(File_Name);
      prim = make_primitive("LOAD-BAND");
      Fasload_Call = Free;
      *Free++ = prim;
      *Free++ = FName;
      Init_Prog = MAKE_POINTER_OBJECT (TC_PCOMB1, Fasload_Call);
      break;

    case BOOT_GET_WORK:		/* ((GET-WORK)) */
      prim = make_primitive("GET-WORK");
      Fasload_Call = Free;
      *Free++ = prim;
      *Free++ = SHARP_F;
      Init_Prog = MAKE_POINTER_OBJECT (TC_COMBINATION, Free);
      *Free++ = MAKE_OBJECT (TC_MANIFEST_VECTOR, 1);
      *Free++ = MAKE_POINTER_OBJECT (TC_PCOMB1, Fasload_Call);
      break;

    default:
      fprintf (stderr, "Unknown boot time option: %d\n", Start_Prim);
      Microcode_Termination (TERM_BAD_PRIMITIVE);
      /*NOTREACHED*/
  }

  /* Setup registers */
  INITIALIZE_INTERRUPTS();
  Env = MAKE_OBJECT (GLOBAL_ENV, 0);
  Trapping = false;
  Return_Hook_Address = NULL;

  /* Give the interpreter something to chew on, and ... */
 Will_Push (CONTINUATION_SIZE);
  Store_Return (RC_END_OF_COMPUTATION);
  Store_Expression (SHARP_F);
  Save_Cont ();
 Pushed ();

  Store_Expression (Init_Prog);

  /* Go to it! */
  if ((Stack_Pointer <= Stack_Guard) || (Free > MemTop))
  {
    fprintf (stderr, "Configuration won't hold initial data.\n");
    termination_init_error ();
  }
#ifdef ENTRY_HOOK
  ENTRY_HOOK ();
#endif
  Enter_Interpreter ();
}

void
Enter_Interpreter ()
{
  Interpret (scheme_dumped_p);
  fprintf (stderr, "\nThe interpreter returned to top level!\n");
  Microcode_Termination (TERM_EXIT);
}

/* Garbage collection debugging utilities. */

extern SCHEME_OBJECT
  *deadly_free,
  *deadly_scan;

extern unsigned long
  gc_counter;

extern void
  gc_death();

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
gc_death (code, message, scan, free)
     long code;
     char *message;
     SCHEME_OBJECT *scan, *free;
{
  fprintf (stderr, "\n%s.\n", message);
  fprintf (stderr, "scan = 0x%lx; free = 0x%lx\n", scan, free);
  deadly_scan = scan;
  deadly_free = free;
  Microcode_Termination (code);
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
  fast SCHEME_OBJECT Result;
  PRIMITIVE_HEADER (0);
  Result = (make_vector (IDENTITY_LENGTH, SHARP_F, true));
  FAST_VECTOR_SET
    (Result, ID_RELEASE, (char_pointer_to_string (RELEASE)));
  FAST_VECTOR_SET
    (Result, ID_MICRO_VERSION, (LONG_TO_UNSIGNED_FIXNUM (VERSION)));
  FAST_VECTOR_SET
    (Result, ID_MICRO_MOD, (LONG_TO_UNSIGNED_FIXNUM (SUBVERSION)));
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
    (Result, ID_OS_NAME, (char_pointer_to_string (OS_Name)));
  FAST_VECTOR_SET
    (Result, ID_OS_VARIANT, (char_pointer_to_string (OS_Variant)));
  FAST_VECTOR_SET
    (Result, ID_STACK_TYPE, (char_pointer_to_string (STACK_TYPE_STRING)));
  PRIMITIVE_RETURN (Result);
}

DEFINE_PRIMITIVE ("MICROCODE-TABLES-FILENAME", Prim_microcode_tables_filename, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string (option_utabmd_file));
}

static SCHEME_OBJECT
DEFUN (argv_to_object, (argc, argv), int argc AND CONST char ** argv)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, argc, 1));
  CONST char ** scan = argv;
  CONST char ** end = (scan + argc);
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
  PRIMITIVE_RETURN (argv_to_object (option_unused_argc, option_unused_argv));
}
