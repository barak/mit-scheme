/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/option.c,v 1.3 1990/11/14 13:29:38 cph Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

/* Command-line option processing */

#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>
#include "ansidecl.h"
#include "obstack.h"

extern char * getenv ();
extern void free ();
#define xfree(p) ((PTR) (p))
extern int strlen ();
extern int atoi ();
extern int access ();
extern struct obstack scratch_obstack;
extern CONST char * scheme_program_name;
extern void EXFUN (termination_init_error, (void));

static int option_summary;
static int option_large_sizes;

#ifdef HAS_COMPILER_SUPPORT
static int option_compiler_defaults;
static int option_edwin_defaults;
#endif

static CONST char * option_raw_library = 0;
static CONST char * option_raw_utabmd = 0;
static CONST char * option_raw_utab = 0;
static CONST char * option_raw_band = 0;
static CONST char * option_raw_heap = 0;
static CONST char * option_raw_constant = 0;
static CONST char * option_raw_stack = 0;

/* Command-line arguments */
int option_saved_argc;
CONST char ** option_saved_argv;
int option_unused_argc;
CONST char ** option_unused_argv;

/* Boolean options */
int option_emacs_subprocess;
int option_force_interactive;
int option_disable_core_dump;

/* String options */
CONST char ** option_library_path = 0;
CONST char * option_band_file = 0;
CONST char * option_fasl_file = 0;
int option_band_specified;
CONST char * option_utabmd_file = 0;
CONST char * option_gc_file = 0;

/* Numeric options */
unsigned int option_heap_size;
unsigned int option_constant_size;
unsigned int option_stack_size;
/*
Scheme accepts the following command-line options.  The options may
appear in any order, but they must all appear before any other
arguments on the command line.

-library PATH
  Sets the library search path to PATH.  This is a colon-separated
  list of directories that is searched to find various library files,
  such as bands.  If this option is not given, the value of the
  environment variable MITSCHEME_LIBRARY_PATH is used; it that isn't
  defined, "/usr/local/lib/mit-scheme" is used.

-band FILENAME
  Specifies the initial band to be loaded.  Searches for FILENAME in
  the working directory and the library directories, returning the
  full pathname of the first readable file of that name.  If this
  option isn't given, the filename is the value of the environment
  variable MITSCHEME_BAND, or if that isn't defined, "runtime.com"; in
  these cases the library directories are searched, but not the
  working directory.

-fasl FILENAME
  Specifies that a cold load should be performed, using FILENAME as
  the initial file to be loaded.  If this option isn't given, a normal
  load is performed instead.  This option may not be used together
  with the "-band" option.

-utabmd FILENAME
  Specifies the name of the microcode tables file.  The file is
  searched for in the working directory and the library directories.
  If this option isn't given, the filename is the value of the
  environment variable MITSCHEME_UTABMD_FILE, or if that isn't
  defined, "utabmd.bin"; in these cases the library directories are
  searched, but not the working directory.

-utab FILENAME
  An alternate name for the "-utabmd" option.  At most one of these
  options may be given.

-large
  Specifies that large heap, constant, and stack default sizes should
  be used.  These are specified by the environment variables
  MITSCHEME_LARGE_HEAP, MITSCHEME_LARGE_CONSTANT, and
  MITSCHEME_LARGE_STACK.  If this option isn't given, the small sizes
  are used, specified by the environment variables
  MITSCHEME_SMALL_HEAP, MITSCHEME_SMALL_CONSTANT, and
  MITSCHEME_SMALL_STACK.  There are reasonable built-in defaults for
  these environment variables, should any of them be undefined.  [The
  Scheme procedure `(print-gc-statistics)' shows how much heap and
  constant space is available and in use.]

-heap BLOCKS
  Specifies the size of the heap in 1024-word blocks.  Overrides any
  default.  Normally two such heaps are allocated; `bchscheme'
  allocates only one.

-constant BLOCKS
  Specifies the size of constant space in 1024-word blocks.  Overrides
  any default.

-stack BLOCKS
  Specifies the size of the stack in 1024-word blocks.  Overrides any
  default.

-option-summary
  Causes Scheme to write option information to standard error.

-emacs
  Specifies that Scheme is running as a subprocess of GNU Emacs.
  This option is automatically supplied by GNU Emacs, and should not
  be given under other circumstances.

-interactive
  If this option isn't specified, and Scheme's standard I/O is not a
  terminal, Scheme will detach itself from its controlling terminal.
  This will prevent it from getting signals sent to the process group
  of that terminal.  If this option is specified, Scheme will not
  detach itself from the controlling terminal.

-nocore
  Specifies that Scheme should not generate a core dump under any
  circumstances.

-gcfile FILENAME
  Specifies that FILENAME should be used garbage collection.  Used
  only when garbage-collecting to disk.

The following options are available only on machines with
compiled-code support:

-compiler
  This option specifies defaults appropriate for loading the compiler.
  It changes the defaults for "-band": the environment variable
  MITSCHEME_COMPILER_BAND is used, otherwise "compiler.com" is used.
  It also specifies the use of large sizes, exactly like "-large".

-edwin
  This option specifies defaults appropriate for loading the editor.
  It changes the defaults for "-band": the environment variable
  MITSCHEME_EDWIN_BAND is used, otherwise "edwin.com" is used.  It
  also specifies the use of large sizes, exactly like "-large".

*/

#ifndef LIBRARY_PATH_VARIABLE
#define LIBRARY_PATH_VARIABLE "MITSCHEME_LIBRARY_PATH"
#endif

#ifndef DEFAULT_LIBRARY_PATH
#define DEFAULT_LIBRARY_PATH "/usr/local/lib/mit-scheme"
#endif

#ifndef BAND_VARIABLE
#define BAND_VARIABLE "MITSCHEME_BAND"
#endif

#ifndef DEFAULT_BAND
#define DEFAULT_BAND "runtime.com"
#endif

#ifndef COMPILER_BAND_VARIABLE
#define COMPILER_BAND_VARIABLE "MITSCHEME_COMPILER_BAND"
#endif

#ifndef COMPILER_DEFAULT_BAND
#define COMPILER_DEFAULT_BAND "compiler.com"
#endif

#ifndef EDWIN_BAND_VARIABLE
#define EDWIN_BAND_VARIABLE "MITSCHEME_EDWIN_BAND"
#endif

#ifndef EDWIN_DEFAULT_BAND
#define EDWIN_DEFAULT_BAND "edwin.com"
#endif

#ifndef UTABMD_FILE_VARIABLE
#define UTABMD_FILE_VARIABLE "MITSCHEME_UTABMD_FILE"
#endif

#ifndef DEFAULT_UTABMD_FILE
#define DEFAULT_UTABMD_FILE "utabmd.bin"
#endif

#ifdef HAS_COMPILER_SUPPORT

#ifdef hp9000s800
/* HPPA compiled binaries are large! */

#ifndef DEFAULT_SMALL_CONSTANT
#define DEFAULT_SMALL_CONSTANT 600
#endif

#ifndef DEFAULT_LARGE_CONSTANT
#define DEFAULT_LARGE_CONSTANT 1300
#endif

#endif /* hp9000s800 */

#ifdef mips
/* MIPS compiled binaries are large! */

#ifndef DEFAULT_SMALL_CONSTANT
#define DEFAULT_SMALL_CONSTANT 700
#endif

#ifndef DEFAULT_LARGE_CONSTANT
#define DEFAULT_LARGE_CONSTANT 1500
#endif

#endif /* mips */

#endif /* HAS_COMPILER_SUPPORT */

#ifndef DEFAULT_SMALL_HEAP
#define DEFAULT_SMALL_HEAP 250
#endif

#ifndef SMALL_HEAP_VARIABLE
#define SMALL_HEAP_VARIABLE "MITSCHEME_SMALL_HEAP"
#endif

#ifndef DEFAULT_SMALL_CONSTANT
#define DEFAULT_SMALL_CONSTANT 400
#endif

#ifndef SMALL_CONSTANT_VARIABLE
#define SMALL_CONSTANT_VARIABLE "MITSCHEME_SMALL_CONSTANT"
#endif

#ifndef DEFAULT_SMALL_STACK
#define	DEFAULT_SMALL_STACK 100
#endif

#ifndef SMALL_STACK_VARIABLE
#define SMALL_STACK_VARIABLE "MITSCHEME_SMALL_STACK"
#endif

#ifndef DEFAULT_LARGE_HEAP
#define DEFAULT_LARGE_HEAP 1000
#endif

#ifndef LARGE_HEAP_VARIABLE
#define LARGE_HEAP_VARIABLE "MITSCHEME_LARGE_HEAP"
#endif

#ifndef DEFAULT_LARGE_CONSTANT
#define DEFAULT_LARGE_CONSTANT 1000
#endif

#ifndef LARGE_CONSTANT_VARIABLE
#define LARGE_CONSTANT_VARIABLE "MITSCHEME_LARGE_CONSTANT"
#endif

#ifndef DEFAULT_LARGE_STACK
#define DEFAULT_LARGE_STACK DEFAULT_SMALL_STACK
#endif

#ifndef LARGE_STACK_VARIABLE
#define LARGE_STACK_VARIABLE "MITSCHEME_LARGE_STACK"
#endif

static int
DEFUN (string_compare_ci, (string1, string2),
       CONST char * string1 AND
       CONST char * string2)
{
  CONST char * scan1 = string1;
  unsigned int length1 = (strlen (string1));
  CONST char * scan2 = string2;
  unsigned int length2 = (strlen (string2));
  unsigned int length = ((length1 < length2) ? length1 : length2);
  CONST char * end1 = (scan1 + length);
  CONST char * end2 = (scan2 + length);
  while ((scan1 < end1) && (scan2 < end2))
    {
      int c1 = (*scan1++);
      int c2 = (*scan2++);
      if (islower (c1))
	{
	  if (! (islower (c2)))
	    c1 = (toupper (c1));
	}
      else
	{
	  if (islower (c2))
	    c2 = (toupper (c2));
	}
      if (c1 != c2)
	return ((c1 < c2) ? (-1) : 1);
    }
  return
    ((length1 == length2)
     ? 0
     : ((length1 < length2) ? (-1) : 1));
}

static char *
DEFUN (strchr, (s, c), CONST char * s AND int c)
{
  while (1)
    {
      int c1 = (*s++);
      if (c1 == c) return ((char *) (s - 1));
      if (c1 == '\0') return (0);
    }
}

static PTR
DEFUN (xmalloc, (n), unsigned long n)
{
  extern char * malloc ();
  PTR result = (malloc (n));
  if (result == 0)
    {
      fprintf (stderr, "%s: unable to allocate space while parsing options.\n",
	       scheme_program_name);
      termination_init_error ();
    }
  return (result);
}

static char *
DEFUN (string_copy, (s), CONST char * s)
{
  char * result = (xmalloc ((strlen (s)) + 1));
  {
    CONST char * s1 = s;
    char * s2 = result;
    while (((*s2++) = (*s1++)) != '\0') ;
  }
  return (result);
}

struct option_descriptor
{
  CONST char * option;
  int argument_p;
  PTR value_cell;
};

static void
DEFUN (option_argument, (option, argument_p, value_cell),
       CONST char * option AND
       int argument_p AND
       PTR value_cell)
{
  struct option_descriptor descriptor;
  (descriptor . option) = option;
  (descriptor . argument_p) = argument_p;
  (descriptor . value_cell) = value_cell;
  obstack_grow ((&scratch_obstack), (&descriptor), (sizeof (descriptor)));
}

static void
DEFUN (parse_options, (argc, argv), int argc AND CONST char ** argv)
{
  CONST char ** scan_argv = (argv + 1);
  CONST char ** end_argv = (scan_argv + (argc - 1));
  unsigned int n_descriptors =
    ((obstack_object_size (&scratch_obstack))
     / (sizeof (struct option_descriptor)));
  struct option_descriptor * descriptors = (obstack_finish (&scratch_obstack));
  struct option_descriptor * end_desc = (descriptors + n_descriptors);
  struct option_descriptor * scan_desc;
  for (scan_desc = descriptors; (scan_desc < end_desc); scan_desc += 1)
    if (scan_desc -> argument_p)
      {
	CONST char ** value_cell = (scan_desc -> value_cell);
	(*value_cell) = 0;
      }
    else
      {
	int * value_cell = (scan_desc -> value_cell);
	(*value_cell) = 0;
      }
  while (scan_argv < end_argv)
    {
      CONST char * option = (*scan_argv++);
      for (scan_desc = descriptors; (scan_desc < end_desc); scan_desc += 1)
	if ((string_compare_ci (option, (scan_desc -> option))) == 0)
	  {
	    if (scan_desc -> argument_p)
	      {
		CONST char ** value_cell = (scan_desc -> value_cell);
		if (scan_argv < end_argv)
		  (*value_cell) = (*scan_argv++);
		else
		  {
		    fprintf (stderr, "%s: %s option requires an argument\n",
			     scheme_program_name, option);
		    termination_init_error ();
		  }
	      }
	    else
	      {
		int * value_cell = (scan_desc -> value_cell);
		(*value_cell) = 1;
	      }
	    break;
	  }
      if (scan_desc == end_desc)
	{
	  scan_argv -= 1;
	  break;
	}
    }
  obstack_free ((&scratch_obstack), descriptors);
  option_saved_argc = argc;
  option_saved_argv = argv;
  option_unused_argc = (end_argv - scan_argv);
  option_unused_argv = scan_argv;
}

static void
DEFUN (parse_standard_options, (argc, argv), int argc AND CONST char ** argv)
{
  option_argument ("-band", 1, (&option_raw_band));
  option_argument ("-constant", 1, (&option_raw_constant));
  option_argument ("-emacs", 0, (&option_emacs_subprocess));
  option_argument ("-fasl", 1, (&option_fasl_file));
  option_argument ("-gcfile", 1, (&option_gc_file));
  option_argument ("-heap", 1, (&option_raw_heap));
  option_argument ("-interactive", 0, (&option_force_interactive));
  option_argument ("-large", 0, (&option_large_sizes));
  option_argument ("-library", 1, (&option_raw_library));
  option_argument ("-nocore", 0, (&option_disable_core_dump));
  option_argument ("-option-summary", 0, (&option_summary));
  option_argument ("-stack", 1, (&option_raw_stack));
  option_argument ("-utab", 1, (&option_raw_utab));
  option_argument ("-utabmd", 1, (&option_raw_utabmd));
#ifdef HAS_COMPILER_SUPPORT
  option_argument ("-compiler", 0, (&option_compiler_defaults));
  option_argument ("-edwin", 0, (&option_edwin_defaults));
#endif
  parse_options (argc, argv);
}

static CONST char *
DEFUN (standard_string_option, (option, variable, defval),
       CONST char * option AND
       CONST char * variable AND
       CONST char * defval)
{
  if (option != 0)
    return (option);
  {
    CONST char * t = (getenv (variable));
    return ((t != 0) ? t : defval);
  }
}

static unsigned int
DEFUN (standard_numeric_option, (option, optval, variable, defval),
       CONST char * option AND
       CONST char * optval AND
       CONST char * variable AND
       unsigned int defval)
{
  if (optval != 0)
    {
      int n = (atoi (optval));
      if (n <= 0)
	{
	  fprintf (stderr, "%s: illegal %s option: %s\n",
		   scheme_program_name, option, optval);
	  termination_init_error ();
	}
      return (n);
    }
  {
    CONST char * t = (getenv (variable));
    if (t != 0)
      {
	int n = (atoi (t));
	if (n <= 0)
	  {
	    fprintf (stderr, "%s: illegal %s variable: %s\n",
		     scheme_program_name, variable, t);
	    termination_init_error ();
	  }
	return (n);
      }
  }
  return (defval);
}

static CONST char **
DEFUN (parse_path_string, (path), CONST char * path)
{
  CONST char * start = path;
  while (1)
    {
      CONST char * scan = start;
      CONST char * end;
      while (1)
	{
	  int c = (*scan++);
	  if ((c == '\0') || (c == ':'))
	    {
	      end = (scan - 1);
	      break;
	    }
	}
      if ((start < end) && ((* (end - 1)) == '/'))
	end -= 1;
      if (end == start)
	obstack_ptr_grow ((&scratch_obstack), (string_copy (".")));
      else
	{
	  char * element = (xmalloc ((end - start) + 1));
	  {
	    CONST char * s1 = start;
	    char * s2 = element;
	    while (s1 < end)
	      (*s2++) = (*s1++);
	    (*s2) = '\0';
	  }
	  obstack_ptr_grow ((&scratch_obstack), element);
	}
      if ((* (scan - 1)) == '\0')
	break;
    }
  obstack_ptr_grow ((&scratch_obstack), 0);
  {
    unsigned int n_bytes = (obstack_object_size (&scratch_obstack));
    CONST char ** elements = (obstack_finish (&scratch_obstack));
    CONST char ** scan = elements;
    CONST char ** end = (scan + (n_bytes / (sizeof (char *))));
    CONST char ** result = (xmalloc (n_bytes));
    CONST char ** scan_result = result;
    while (scan < end)
      (*scan_result++) = (*scan++);
    obstack_free ((&scratch_obstack), elements);
    return (result);
  }
}

static void
DEFUN (free_parsed_path, (path), CONST char ** path)
{
  CONST char ** scan = path;
  while (1)
    {
      CONST char * element = (*scan++);
      if (element == 0)
	break;
      xfree (element);
    }
  xfree (path);
}

#define FILE_ABSOLUTE(filename) (((filename) [0]) == '/')
#define FILE_READABLE(filename) ((access ((filename), 4)) >= 0)

static CONST char *
DEFUN (search_path_for_file, (path, filename),
       CONST char * option AND
       CONST char * filename AND
       int default_p)
{
  unsigned int flen = (strlen (filename));
  CONST char ** scan_path = option_library_path;
  while (1)
    {
      CONST char * directory = (*scan_path++);
      unsigned int dlen;
      CONST char * fullname;
      if (directory == 0)
	{
	  fprintf (stderr, "%s: can't find readable %s for %s option.\n",
		   scheme_program_name,
		   (default_p ? "default" : "file"),
		   option);
	  fprintf (stderr, "    searched for file %s in these directories:\n",
		   filename);
	  if (!default_p)
	    fprintf (stderr, "    .\n");
	  scan_path = option_library_path;
	  while (1)
	    {
	      CONST char * element = (*scan_path++);
	      if (element == 0)
		break;
	      fprintf (stderr, "    %s\n", element);
	    }
	  termination_init_error ();
	}
      dlen = (strlen (directory));
      if (dlen > 0)
	{
	  obstack_grow ((&scratch_obstack), directory, dlen);
	  obstack_1grow ((&scratch_obstack), '/');
	}
      obstack_grow ((&scratch_obstack), filename, flen);
      obstack_1grow ((&scratch_obstack), '\0');
      fullname = (obstack_finish (&scratch_obstack));
      if (FILE_READABLE (fullname))
	{
	  CONST char * result = (string_copy (fullname));
	  obstack_free ((&scratch_obstack), ((char *) fullname));
	  return (result);
	}
      obstack_free ((&scratch_obstack), ((char *) fullname));
    }
}

static CONST char *
DEFUN (standard_filename_option, (option, optval, variable, defval),
       CONST char * option AND
       CONST char * optval AND
       CONST char * variable AND
       CONST char * defval)
{
  if (optval != 0)
    {
      if (FILE_READABLE (optval))
	return (string_copy (optval));
      if (FILE_ABSOLUTE (optval))
	{
	  fprintf (stderr, "%s: can't read option file: %s %s\n",
		   scheme_program_name, option, optval);
	  termination_init_error ();
	}
      return (search_path_for_file (option, optval, 0));
    }
  {
    CONST char * filename = (getenv (variable));
    if (filename == 0)
      filename = defval;
    if (FILE_ABSOLUTE (filename))
      {
	if (! (FILE_READABLE (filename)))
	  {
	    fprintf (stderr, "%s: can't read default file for %s option: %s\n",
		     scheme_program_name, option, filename);
	    termination_init_error ();
	  }
	return (string_copy (filename));
      }
    return (search_path_for_file (option, filename, 1));
  }
}

static void
DEFUN (conflicting_options, (option1, option2),
       CONST char * option1 AND
       CONST char * option2)
{
  fprintf (stderr, "%s: can't specify both %s and %s options.\n",
	   scheme_program_name, option1, option2);
  termination_init_error ();
}

static void
DEFUN (describe_boolean_option, (name, value),
       CONST char * name AND
       int value)
{
  fprintf (stderr, "  %s: %s\n", name, (value ? "yes" : "no"));
}

static void
DEFUN (describe_string_option, (name, value),
       CONST char * name AND
       CONST char * value)
{
  fprintf (stderr, "  %s: %s\n", name, value);
}

static void
DEFUN (describe_size_option, (name, value),
       CONST char * name AND
       unsigned int value)
{
  fprintf (stderr, "  %s size: %d\n", name, value);
}

static void
DEFUN (describe_path_option, (name, value),
       CONST char * name AND
       CONST char ** value)
{
  fprintf (stderr, "  %s: ", name);
  {
    CONST char ** scan = value;
    fprintf (stderr, "%s", (*scan++));
    while (1)
      {
	CONST char * element = (*scan++);
	if (element == 0) break;
	fprintf (stderr, ":%s", element);
      }
  }
  fprintf (stderr, "\n");
}

static void
DEFUN_VOID (describe_options)
{
  fprintf (stderr, "Summary of configuration options:\n");
  describe_size_option ("heap", option_heap_size);
  describe_size_option ("constant-space", option_constant_size);
  describe_size_option ("stack", option_stack_size);
  describe_path_option ("library path", option_library_path);
  if (option_fasl_file != 0)
    describe_string_option ("FASL file", option_fasl_file);
  else
    describe_string_option ("band", option_band_file);
  describe_string_option ("microcode tables", option_utabmd_file);
  if (option_gc_file != 0)
    describe_string_option ("GC file", option_gc_file);
  describe_boolean_option ("emacs subprocess", option_emacs_subprocess);
  describe_boolean_option ("force interactive", option_force_interactive);
  describe_boolean_option ("disable core dump", option_disable_core_dump);
  if (option_unused_argc == 0)
    fprintf (stderr, "  no unused arguments\n");
  else
    {
      CONST char ** scan = option_unused_argv;
      CONST char ** end = (scan + option_unused_argc);
      fprintf (stderr, "  unused arguments:");
      while (scan < end)
	fprintf (stderr, " %s", (*scan++));
      fprintf (stderr, "\n");
    }
  fflush (stderr);
}

void
DEFUN (read_command_line_options, (argc, argv),
       int argc AND
       CONST char ** argv)
{
  parse_standard_options (argc, argv);
  if (option_library_path != 0)
    free_parsed_path (option_library_path);
  option_library_path =
    (parse_path_string
     (standard_string_option (option_raw_library,
			      LIBRARY_PATH_VARIABLE,
			      DEFAULT_LIBRARY_PATH)));
  {
    CONST char * band_variable = BAND_VARIABLE;
    CONST char * default_band = DEFAULT_BAND;
    option_band_specified = 0;
    if (option_band_file != 0)
      xfree (option_band_file);
#ifdef HAS_COMPILER_SUPPORT
    if (option_compiler_defaults)
      {
	if (option_edwin_defaults)
	  conflicting_options ("-compiler", "-edwin");
	option_large_sizes = 1;
	option_band_specified = 1;
	band_variable = COMPILER_BAND_VARIABLE;
	default_band = COMPILER_DEFAULT_BAND;
      }
    else if (option_edwin_defaults)
      {
	option_large_sizes = 1;
	option_band_specified = 1;
	band_variable = EDWIN_BAND_VARIABLE;
	default_band = EDWIN_DEFAULT_BAND;
      }
#endif
    if (option_fasl_file != 0)
      {
	if (option_raw_band != 0)
	  conflicting_options ("-fasl", "-band");
	if (! (FILE_READABLE (option_fasl_file)))
	  {
	    fprintf (stderr, "%s: can't read option file: -fasl %s\n",
		     scheme_program_name, option_fasl_file);
	    termination_init_error ();
	  }
	option_band_specified = 1;
	option_band_file = 0;
      }
    else
      {
	if (option_raw_band != 0)
	  option_band_specified = 1;
	option_band_file =
	  (standard_filename_option ("-band",
				     option_raw_band,
				     band_variable,
				     default_band));
      }
  }
  if (option_large_sizes)
    {
      option_heap_size =
	(standard_numeric_option ("-heap",
				  option_raw_heap,
				  LARGE_HEAP_VARIABLE,
				  DEFAULT_LARGE_HEAP));
      option_constant_size =
	(standard_numeric_option ("-constant",
				  option_raw_constant,
				  LARGE_CONSTANT_VARIABLE,
				  DEFAULT_LARGE_CONSTANT));
      option_stack_size =
	(standard_numeric_option ("-stack",
				  option_raw_stack,
				  LARGE_STACK_VARIABLE,
				  DEFAULT_LARGE_STACK));
    }
  else
    {
      option_heap_size =
	(standard_numeric_option ("-heap",
				  option_raw_heap,
				  SMALL_HEAP_VARIABLE,
				  DEFAULT_SMALL_HEAP));
      option_constant_size =
	(standard_numeric_option ("-constant",
				  option_raw_constant,
				  SMALL_CONSTANT_VARIABLE,
				  DEFAULT_SMALL_CONSTANT));
      option_stack_size =
	(standard_numeric_option ("-stack",
				  option_raw_stack,
				  SMALL_STACK_VARIABLE,
				  DEFAULT_SMALL_STACK));
    }
  if (option_utabmd_file != 0)
    xfree (option_utabmd_file);
  if (option_raw_utabmd != 0)
    {
      if (option_raw_utab != 0)
	conflicting_options ("-utabmd", "-utab");
      option_utabmd_file =
	(standard_filename_option ("-utabmd",
				   option_raw_utabmd,
				   UTABMD_FILE_VARIABLE,
				   DEFAULT_UTABMD_FILE));
    }
  else
    option_utabmd_file =
      (standard_filename_option ("-utab",
				 option_raw_utab,
				 UTABMD_FILE_VARIABLE,
				 DEFAULT_UTABMD_FILE));
  if (option_summary)
    describe_options ();
}
