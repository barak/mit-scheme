/* -*-C-*-

$Id: option.c,v 1.54 2000/10/16 17:22:12 cph Exp $

Copyright (c) 1990-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Command-line option processing */

#include <ctype.h>
#include "scheme.h"
#include "fasl.h"
#include "osenv.h"
#include "osfs.h"
#include <sys/stat.h>

extern char * getenv ();
extern void free ();
#define xfree(p) free ((PTR) (p))
extern int atoi ();

#ifdef WINNT

#include <io.h>
#include <string.h>
#include <stdlib.h>
#include "nt.h"
#include "ntio.h"

#else /* not WINNT */

#ifdef _POSIX
#include <unistd.h>
#else
extern int strlen ();
#endif

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#else
extern char * EXFUN (malloc, (int));
#endif

#endif /* not WINNT */

#ifndef NULL
# define NULL 0
#endif

#if defined(DOS386) || defined(WINNT) || defined(_OS2)
#define DOS_LIKE_FILENAMES
#endif

extern struct obstack scratch_obstack;
extern CONST char * scheme_program_name;
extern void EXFUN (termination_init_error, (void));

#ifndef SUB_DIRECTORY_DELIMITER
#  ifdef DOS_LIKE_FILENAMES
#    define SUB_DIRECTORY_DELIMITER '\\'
#  else
#    define SUB_DIRECTORY_DELIMITER '/'
#  endif
#endif

#ifndef PATH_DELIMITER
#  ifdef DOS_LIKE_FILENAMES
#    define PATH_DELIMITER ';'
#  else
#    define PATH_DELIMITER ':'
#  endif
#endif

#ifdef DOS_LIKE_FILENAMES
#  define FILE_ABSOLUTE(filename)			\
     ((((filename) [0]) == SUB_DIRECTORY_DELIMITER)	\
      || (((filename) [1]) == ':'))
#else
#  define FILE_ABSOLUTE(filename) (((filename) [0]) == SUB_DIRECTORY_DELIMITER)
#endif

#define FILE_READABLE(filename) (OS_file_access ((filename), 4))

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
int option_band_specified;
int option_empty_list_eq_false;

/* String options */
CONST char ** option_library_path = 0;
CONST char * option_band_file = 0;
CONST char * option_fasl_file = 0;
CONST char * option_utabmd_file = 0;

/* Numeric options */
unsigned int option_heap_size;
unsigned int option_constant_size;
unsigned int option_stack_size;

/* These only matter for bchscheme */
static CONST char * option_raw_gc_end_position = 0;
static CONST char * option_raw_gc_file = 0;
static CONST char * option_raw_gc_read_overlap = 0;
static CONST char * option_raw_gc_start_position = 0;
static CONST char * option_raw_gc_window_size = 0;
static CONST char * option_raw_gc_write_overlap = 0;
CONST char * option_gc_directory = 0;
CONST char * option_gc_drone = 0;
CONST char * option_gc_file = 0;
int option_gc_keep;
int option_gc_read_overlap;
int option_gc_window_size;
int option_gc_write_overlap;
long option_gc_start_position;
long option_gc_end_position;
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

The following options are only meaningful to bchscheme:

-gc-directory DIRECTORY
  Specifies what directory to use to allocate the garbage collection file.

-gc-drone FILENAME
  Specifies the program to use as the gc drones for overlapped I/O.

-gc-end-position N
  Specifies a position into the gc file past which bchscheme should not use.

-gc-file FILENAME
  Specifies that FILENAME should be used garbage collection.  Overrides
  -gc-directory if it is an absolute pathname.  -gcfile means the same thing,
  but is deprecated.

-gc-keep
  Specifles that newly allocated gc files should be kept rather than deleted.

-gc-read-overlap N
  Specifies the number of additional GC windows to use when reading
  for overlapped I/O.  Each implies a drone process to manage it,
  if supported.

-gc-start-position N
  Specifies a position into the gc file before which bchscheme should not use.

-gc-window-size BLOCKS
  Specifies the size in 1024-word blocks of each GC window.

-gc-write-overlap N
  Specifies the number of additional GC windows to use when writing for
  overlapped I/O.  Each implies a drone process to manage it, if supported.
*/

#ifndef LIBRARY_PATH_VARIABLE
#define LIBRARY_PATH_VARIABLE "MITSCHEME_LIBRARY_PATH"
#endif

#ifndef DEFAULT_LIBRARY_PATH
#ifdef DOS_LIKE_FILENAMES
#define DEFAULT_LIBRARY_PATH "\\scheme\\lib"
#else
#define DEFAULT_LIBRARY_PATH "/usr/local/lib/mit-scheme"
#endif
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

#ifndef ALL_BAND_VARIABLE
#define ALL_BAND_VARIABLE "MITSCHEME_ALL_BAND"
#endif

#ifndef ALL_DEFAULT_BAND
#define ALL_DEFAULT_BAND "all.com"
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
#define DEFAULT_LARGE_CONSTANT 1400
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

#ifdef i386
/* 386 code is large too! */

#ifndef DEFAULT_SMALL_CONSTANT
#define DEFAULT_SMALL_CONSTANT 600
#endif

#ifndef DEFAULT_LARGE_CONSTANT
#define DEFAULT_LARGE_CONSTANT 1200
#endif

#endif /* i386 */

#endif /* HAS_COMPILER_SUPPORT */

#ifndef DEFAULT_SMALL_HEAP
#define DEFAULT_SMALL_HEAP 250
#endif

#ifndef SMALL_HEAP_VARIABLE
#define SMALL_HEAP_VARIABLE "MITSCHEME_SMALL_HEAP"
#endif

#ifndef DEFAULT_SMALL_CONSTANT
#define DEFAULT_SMALL_CONSTANT 450
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

/* These are only meaningful for bchscheme */

#ifndef DEFAULT_GC_DIRECTORY
#ifdef DOS_LIKE_FILENAMES
#define DEFAULT_GC_DIRECTORY		"\\tmp"
#else
#define DEFAULT_GC_DIRECTORY		"/tmp"
#endif
#endif

#ifndef GC_DIRECTORY_VARIABLE
#define GC_DIRECTORY_VARIABLE		"MITSCHEME_GC_DIRECTORY"
#endif

#ifndef DEFAULT_GC_DRONE
#define DEFAULT_GC_DRONE		"gcdrone"
#endif

#ifndef GC_DRONE_VARIABLE
#define GC_DRONE_VARIABLE		"MITSCHEME_GC_DRONE"
#endif

#ifndef DEFAULT_GC_END_POSITION
#define DEFAULT_GC_END_POSITION		-1
#endif

#ifndef GC_END_POSITION_VARIABLE
#define GC_END_POSITION_VARIABLE	"MITSCHEME_GC_END_POSITION"
#endif

#ifndef DEFAULT_GC_FILE
#define DEFAULT_GC_FILE			"GCXXXXXX"
#endif

#ifndef GC_FILE_VARIABLE
#define GC_FILE_VARIABLE		"MITSCHEME_GC_FILE"
#endif

#ifndef DEFAULT_GC_READ_OVERLAP
#define DEFAULT_GC_READ_OVERLAP		0
#endif

#ifndef GC_READ_OVERLAP_VARIABLE
#define GC_READ_OVERLAP_VARIABLE	"MITSCHEME_GC_READ_OVERLAP"
#endif

#ifndef DEFAULT_GC_START_POSITION
#define DEFAULT_GC_START_POSITION	0
#endif

#ifndef GC_START_POSITION_VARIABLE
#define GC_START_POSITION_VARIABLE	"MITSCHEME_GC_START_POSITION"
#endif

#ifndef DEFAULT_GC_WINDOW_SIZE
#define DEFAULT_GC_WINDOW_SIZE		16
#endif

#ifndef GC_WINDOW_SIZE_VARIABLE
#define GC_WINDOW_SIZE_VARIABLE		"MITSCHEME_GC_WINDOW_SIZE"
#endif

#ifndef DEFAULT_GC_WRITE_OVERLAP
#define DEFAULT_GC_WRITE_OVERLAP	0
#endif

#ifndef GC_WRITE_OVERLAP_VARIABLE
#define GC_WRITE_OVERLAP_VARIABLE	"MITSCHEME_GC_WRITE_OVERLAP"
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

#if 0
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
#endif

static PTR
DEFUN (xmalloc, (n), unsigned long n)
{
  PTR result = (malloc (n));
  if (result == 0)
    {
      outf_fatal ("%s: unable to allocate space while parsing options.\n",
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
       CONST PTR value_cell)
{
  struct option_descriptor descriptor;
  (descriptor . option) = option;
  (descriptor . argument_p) = argument_p;
  (descriptor . value_cell) = ((PTR) value_cell);
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
		    outf_fatal ("%s: option %s requires an argument.\n",
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
  option_argument ("-heap", 1, (&option_raw_heap));
  option_argument ("-interactive", 0, (&option_force_interactive));
  option_argument ("-large", 0, (&option_large_sizes));
  option_argument ("-library", 1, (&option_raw_library));
  option_argument ("-nocore", 0, (&option_disable_core_dump));
  option_argument ("-option-summary", 0, (&option_summary));
  option_argument ("-stack", 1, (&option_raw_stack));
  option_argument ("-utab", 1, (&option_raw_utab));
  option_argument ("-utabmd", 1, (&option_raw_utabmd));
  option_argument ("-empty-list-eq-false", 0, (&option_empty_list_eq_false));
#ifdef HAS_COMPILER_SUPPORT
  option_argument ("-compiler", 0, (&option_compiler_defaults));
  option_argument ("-edwin", 0, (&option_edwin_defaults));
#endif
  /* The following options are only meaningful to bchscheme. */
  option_argument ("-gc-directory", 1, (&option_gc_directory));
  option_argument ("-gc-drone", 1, (&option_gc_drone));
  option_argument ("-gc-end-position", 1, (&option_raw_gc_end_position));
  option_argument ("-gc-file", 1, (&option_gc_file));
  option_argument ("-gc-keep", 0, (&option_gc_keep));
  option_argument ("-gc-start-position", 1, (&option_raw_gc_start_position));
  option_argument ("-gc-read-overlap", 1, (&option_raw_gc_read_overlap));
  option_argument ("-gc-window-size", 1, (&option_raw_gc_window_size));
  option_argument ("-gc-write-overlap", 1, (&option_raw_gc_write_overlap));
  option_argument ("-gcfile", 1, (&option_raw_gc_file)); /* Obsolete */
  parse_options (argc, argv);
}

static CONST char *
DEFUN (string_option, (option, defval),
       CONST char * option AND CONST char * defval)
{
  return ((option == ((char *) NULL)) ? defval : option);
}

static CONST char *
DEFUN (environment_default, (variable, defval),
       CONST char * variable AND CONST char * defval)
{
  CONST char * temp = (getenv (variable));
  return ((temp == ((char *) NULL)) ? defval : temp);
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

static long
DEFUN (non_negative_numeric_option, (option, optval, variable, defval),
       CONST char * option AND
       CONST char * optval AND
       CONST char * variable AND
       long defval)
{
  if (optval != 0)
    {
      long n = (strtol (optval, ((char **) NULL), 0));
      if (n < 0)
	{
	  outf_fatal ("%s: illegal argument %s for option %s.\n",
		   scheme_program_name, optval, option);
	  termination_init_error ();
	}
      return (n);
    }
  {
    CONST char * t = (getenv (variable));
    if (t != 0)
      {
	long n = (strtol (t, ((char **) NULL), 0));
	if (n < 0)
	  {
	    outf_fatal ("%s: illegal value %s for variable %s.\n",
		     scheme_program_name, t, variable);
	    termination_init_error ();
	  }
	return (n);
      }
  }
  return (defval);
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
	  outf_fatal ("%s: illegal argument %s for option %s.\n",
		   scheme_program_name, optval, option);
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
	    outf_fatal ("%s: illegal value %s for variable %s.\n",
		     scheme_program_name, t, variable);
	    termination_init_error ();
	  }
	return (n);
      }
  }
  return (defval);
}

static CONST char *
DEFUN_VOID (get_wd)
{
  CONST char * wd = (OS_working_dir_pathname ());
  unsigned int len = (strlen (wd));
  if ((wd [len - 1]) == SUB_DIRECTORY_DELIMITER)
    len -= 1;
  {
    char * result = (xmalloc (len + 1));
    char * scan_result = result;
    CONST char * scan_wd = wd;
    CONST char * end_wd = (scan_wd + len);
    while (scan_wd < end_wd)
      (*scan_result++) = (*scan_wd++);
    (*scan_result) = '\0';
    return (result);
  }
}

static CONST char **
DEFUN (parse_path_string, (path), CONST char * path)
{
  CONST char * start = path;
  /* It is important that this get_wd be called here to make sure that
     the the unix getcwd is called now, before it allocates heap space
     This is because getcwd forks off a new process and we want to do
     that before the scheme process gets too big
  */
  CONST char * wd = (get_wd ());
  unsigned int lwd = (strlen (wd));
  while (1)
    {
      CONST char * scan = start;
      CONST char * end;
      while (1)
	{
	  int c = (*scan++);
	  if ((c == '\0') || (c == PATH_DELIMITER))
	    {
	      end = (scan - 1);
	      break;
	    }
	}
      if ((start < end) && ((* (end - 1)) == SUB_DIRECTORY_DELIMITER))
	end -= 1;
      if (end == start)
	obstack_ptr_grow ((&scratch_obstack), (string_copy (wd)));
      else
	{
	  int absolute = (FILE_ABSOLUTE (start));
	  {
	    char * element =
	      (xmalloc ((absolute ? 0 : (lwd + 1)) + (end - start) + 1));
	    char * scan_element = element;
	    if (!absolute)
	      {
		CONST char * s = wd;
		CONST char * e = (wd + lwd);
		while (s < e)
		  (*scan_element++) = (*s++);
		(*scan_element++) = SUB_DIRECTORY_DELIMITER;
	      }
	    {
	      CONST char * s = start;
	      while (s < end)
		(*scan_element++) = (*s++);
	    }
	    (*scan_element) = '\0';
	    obstack_ptr_grow ((&scratch_obstack), element);
	  }
	}
      if ((* (scan - 1)) == '\0')
	break;
      start = scan;
    }
  obstack_ptr_grow ((&scratch_obstack), 0);
  if (wd != 0)
    xfree (wd);
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

CONST char *
DEFUN (search_for_library_file, (filename), CONST char * filename)
{
  unsigned int flen = (strlen (filename));
  CONST char ** scan_path = option_library_path;
  while (1)
    {
      CONST char * directory = (*scan_path++);
      unsigned int dlen;
      CONST char * fullname;
      if (directory == 0)
	return ((char *) NULL);
      dlen = (strlen (directory));
      if (dlen > 0)
	{
	  obstack_grow ((&scratch_obstack), directory, dlen);
	  obstack_1grow ((&scratch_obstack), SUB_DIRECTORY_DELIMITER);
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

CONST char *
DEFUN (search_path_for_file, (option, filename, default_p, fail_p),
       CONST char * option AND
       CONST char * filename AND
       int default_p AND
       int fail_p)
{
  CONST char * result;

  if ((result = (search_for_library_file (filename))) != ((char *) NULL))
    return (result);
  if (!fail_p)
    return (filename);
  else
  {
    CONST char ** scan_path = option_library_path;

    outf_fatal ("%s: can't find a readable %s",
	     scheme_program_name, (default_p ? "default" : "file"));
    if (option != 0)
      outf_fatal (" for option %s", option);
    outf_fatal (".\n");
    outf_fatal ("\tsearched for file %s in these directories:\n",
	     filename);
    if (!default_p)
      outf_fatal ("\t.\n");
    while (1)
    {
      CONST char * element = (*scan_path++);
      if (element == 0)
	break;
      outf_fatal ("\t%s\n", element);
    }
    termination_init_error ();
    /*NOTREACHED*/
    return (0);
  }
}

static CONST char *
DEFUN (standard_filename_option, (option, optval, variable, defval, fail_p),
       CONST char * option AND
       CONST char * optval AND
       CONST char * variable AND
       CONST char * defval AND
       int fail_p)
{
  if (optval != 0)
    {
      if (FILE_READABLE (optval))
	return (string_copy (optval));
      if (FILE_ABSOLUTE (optval))
	{
	  if (fail_p)
	    {
	      outf_fatal ("%s: can't read file %s for option %s.\n",
		       scheme_program_name, optval, option);
	      termination_init_error ();
	    }
	  return (string_copy (optval));
	}
      return (search_path_for_file (option, optval, 0, fail_p));
    }
  {
    CONST char * filename = (getenv (variable));
    if (filename == 0)
      filename = defval;
    if (FILE_ABSOLUTE (filename))
      {
	if ((! (FILE_READABLE (filename))) && fail_p)
	  {
	    outf_fatal ("%s: can't read default file %s for option %s.\n",
		     scheme_program_name, filename, option);
	    termination_init_error ();
	  }
	return (string_copy (filename));
      }
    else
      return (search_path_for_file (option, filename, 1, fail_p));
  }
}

static void
DEFUN (conflicting_options, (option1, option2),
       CONST char * option1 AND
       CONST char * option2)
{
  outf_fatal ("%s: can't specify both options %s and %s.\n",
	   scheme_program_name, option1, option2);
  termination_init_error ();
}

#define SCHEME_WORDS_TO_BLOCKS(n) (((n) + 1023) / 1024)

static int
DEFUN (read_band_header, (filename, header),
       CONST char * filename AND
       SCHEME_OBJECT * header)
{
#ifdef WINNT

  HANDLE handle
    = (CreateFile (filename,
		   GENERIC_READ,
		   (FILE_SHARE_READ | FILE_SHARE_WRITE),
		   0,
		   OPEN_EXISTING,
		   (FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN),
		   0));
  DWORD bytes_to_read = ((sizeof (SCHEME_OBJECT)) * FASL_HEADER_LENGTH);
  DWORD bytes_read;
  if (handle == INVALID_HANDLE_VALUE)
    return (0);
  if (! ((ReadFile (handle, header, bytes_to_read, (&bytes_read), 0))
	 && (bytes_read == bytes_to_read)))
    {
      CloseHandle (handle);
      return (0);
    }
  CloseHandle (handle);
  return (1);

#else /* not WINNT */

  FILE * stream = (fopen (filename, "r"));
  if (stream == 0)
    return (0);
  if ((fread (header, (sizeof (SCHEME_OBJECT)), FASL_HEADER_LENGTH, stream))
      != FASL_HEADER_LENGTH)
    {
      fclose (stream);
      return (0);
    }
  fclose (stream);
  return (1);

#endif /* not WINNT */
}

static int
DEFUN (read_band_sizes, (filename, constant_size, heap_size),
       CONST char * filename AND
       unsigned long * constant_size AND
       unsigned long * heap_size)
{
  SCHEME_OBJECT header [FASL_HEADER_LENGTH];
  if (!read_band_header (filename, header))
    return (0);
  (*constant_size)
    = (SCHEME_WORDS_TO_BLOCKS
       (OBJECT_DATUM (header [FASL_Offset_Const_Count])));
  (*heap_size)
    = (SCHEME_WORDS_TO_BLOCKS
       (OBJECT_DATUM (header [FASL_Offset_Heap_Count])));
  return (1);
}

static void
DEFUN (describe_boolean_option, (name, value),
       CONST char * name AND
       int value)
{
  outf_fatal ("  %s: %s\n", name, (value ? "yes" : "no"));
}

static void
DEFUN (describe_string_option, (name, value),
       CONST char * name AND
       CONST char * value)
{
  outf_fatal ("  %s: %s\n", name, value);
}

static void
DEFUN (describe_numeric_option, (name, value),
       CONST char * name AND
       int value)
{
  outf_fatal ("  %s: %d\n", name, value);
}

static void
DEFUN (describe_size_option, (name, value),
       CONST char * name AND
       unsigned int value)
{
  outf_fatal ("  %s size: %d\n", name, value);
}

static void
DEFUN (describe_path_option, (name, value),
       CONST char * name AND
       CONST char ** value)
{
  outf_fatal ("  %s: ", name);
  {
    CONST char ** scan = value;
    outf_fatal ("%s", (*scan++));
    while (1)
      {
	CONST char * element = (*scan++);
	if (element == 0) break;
	outf_fatal (":%s", element);
      }
  }
  outf_fatal ("\n");
}

static void
DEFUN_VOID (describe_options)
{
  outf_fatal ("Summary of configuration options:\n");
  describe_size_option ("heap", option_heap_size);
  describe_size_option ("constant-space", option_constant_size);
  describe_size_option ("stack", option_stack_size);
  describe_path_option ("library path", option_library_path);
  if (option_fasl_file != 0)
    describe_string_option ("FASL file", option_fasl_file);
  else
    describe_string_option ("band", option_band_file);
  describe_string_option ("microcode tables", option_utabmd_file);
  {
    /* These are only relevant to bchscheme. */
    if (option_gc_directory != DEFAULT_GC_DIRECTORY)
      describe_string_option ("GC directory", option_gc_directory);
    if (option_gc_drone != DEFAULT_GC_DRONE)
      describe_string_option ("GC drone program", option_gc_drone);
    if (option_raw_gc_end_position)
      describe_numeric_option ("GC end position", option_gc_end_position);
    if (option_gc_file != DEFAULT_GC_FILE)
      describe_string_option ("GC file", option_gc_file);
    if (option_raw_gc_read_overlap)
      describe_numeric_option ("GC read overlap", option_gc_read_overlap);
    if (option_raw_gc_start_position)
      describe_numeric_option ("GC start position", option_gc_start_position);
    if (option_raw_gc_window_size)
      describe_size_option ("GC window size", option_gc_window_size);
    if (option_raw_gc_write_overlap)
      describe_numeric_option ("GC write overlap", option_gc_write_overlap);
    if (option_gc_keep)
      describe_boolean_option ("keep GC file", option_gc_keep);
  }
  describe_boolean_option ("emacs subprocess", option_emacs_subprocess);
  describe_boolean_option ("force interactive", option_force_interactive);
  describe_boolean_option ("disable core dump", option_disable_core_dump);
  if (option_unused_argc == 0)
    outf_fatal ("  no unused arguments\n");
  else
    {
      CONST char ** scan = option_unused_argv;
      CONST char ** end = (scan + option_unused_argc);
      outf_fatal ("  unused arguments:");
      while (scan < end)
	outf_fatal (" %s", (*scan++));
      outf_fatal ("\n");
    }
}

void
DEFUN (read_command_line_options, (argc, argv),
       int argc AND
       CONST char ** argv)
{
  int band_sizes_valid = 0;
  unsigned long band_constant_size;
  unsigned long band_heap_size;

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

    /* If the default band doesn't exist, look for alternates.  */
    if (!search_for_library_file (DEFAULT_BAND))
      {
	CONST char * alternate_bands [] =
	  {
	    ALL_DEFAULT_BAND,
	    COMPILER_DEFAULT_BAND,
	    EDWIN_DEFAULT_BAND,
	    "6001.com",
	    "mechanics.com",
	    0
	  };
	unsigned int i = 0;
	while (1)
	  {
	    CONST char * band = (alternate_bands[i]);
	    if (band == 0)
	      break;
	    if (search_for_library_file (band))
	      {
		default_band = band;
		option_large_sizes = 1;
		break;
	      }
	    i += 1;
	  }
      }

    option_band_specified = 0;
    if (option_band_file != 0)
      xfree (option_band_file);
#ifdef HAS_COMPILER_SUPPORT
    if (option_compiler_defaults)
      if (option_edwin_defaults)
	{
	  option_large_sizes = 1;
	  option_band_specified = 1;
	  band_variable = ALL_BAND_VARIABLE;
	  default_band = ALL_DEFAULT_BAND;
	}
      else
	{
	  option_large_sizes = 1;
	  option_band_specified = 1;
	  band_variable = COMPILER_BAND_VARIABLE;
	  default_band = COMPILER_DEFAULT_BAND;
	}
    else
      if (option_edwin_defaults)
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
#ifndef NATIVE_CODE_IS_C
	if (! (FILE_READABLE (option_fasl_file)))
	  {
	    outf_fatal ("%s: can't read option file: -fasl %s\n",
		     scheme_program_name, option_fasl_file);
	    termination_init_error ();
	  }
#endif /* NATIVE_CODE_IS_C */
	option_large_sizes = 1;
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
				     default_band,
				     1));
      }
  }
  if (option_band_file != 0)
    band_sizes_valid
      = (read_band_sizes (option_band_file,
			  (&band_constant_size),
			  (&band_heap_size)));
  option_heap_size
    = ((standard_numeric_option ("-heap",
				 option_raw_heap,
				 (option_large_sizes
				  ? LARGE_HEAP_VARIABLE
				  : SMALL_HEAP_VARIABLE),
				 (option_large_sizes
				  ? DEFAULT_LARGE_HEAP
				  : DEFAULT_SMALL_HEAP)))
       + (band_sizes_valid ? band_heap_size : 0));
  option_constant_size
    = (standard_numeric_option ("-constant",
				option_raw_constant,
				(option_large_sizes
				 ? LARGE_CONSTANT_VARIABLE
				 : SMALL_CONSTANT_VARIABLE),
				(band_sizes_valid
				 ? band_constant_size
				 : option_large_sizes
				 ? DEFAULT_LARGE_CONSTANT
				 : DEFAULT_SMALL_CONSTANT)));
  option_stack_size
    = (standard_numeric_option ("-stack",
				option_raw_stack,
				(option_large_sizes
				 ? LARGE_STACK_VARIABLE
				 : SMALL_STACK_VARIABLE),
				(option_large_sizes
				 ? DEFAULT_LARGE_STACK
				 : DEFAULT_SMALL_STACK)));
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
				   DEFAULT_UTABMD_FILE,
				   (option_fasl_file != 0)));
    }
  else
    option_utabmd_file =
      (standard_filename_option ("-utab",
				 option_raw_utab,
				 UTABMD_FILE_VARIABLE,
				 DEFAULT_UTABMD_FILE,
				 (option_fasl_file != 0)));

  /* These are only meaningful for bchscheme. */

  if (option_raw_gc_file != ((char *) 0))
  {
    if (option_gc_file != ((char *) 0))
      conflicting_options ("-gcfile", "-gc-file");
    else
      option_gc_file = option_raw_gc_file;
  }

  {
    CONST char * dir = (environment_default (GC_DIRECTORY_VARIABLE, 0));
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = (environment_default ("TMPDIR", 0));
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = (environment_default ("TEMP", 0));
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = (environment_default ("TMP", 0));
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = (environment_default ("TMP", 0));
#ifdef _UNIX
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      {
	if (OS_file_directory_p ("/var/tmp"))
	  dir = "/var/tmp";
	if (OS_file_directory_p ("/usr/tmp"))
	  dir = "/usr/tmp";
	if (OS_file_directory_p ("/tmp"))
	  dir = "/tmp";
      }
#endif /* _UNIX */
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = DEFAULT_GC_DIRECTORY;
    option_gc_directory = (string_option (option_gc_directory, dir));
  }
  option_gc_drone =
    (standard_filename_option ("-gc-drone",
			       option_gc_drone,
			       GC_DRONE_VARIABLE,
			       DEFAULT_GC_DRONE,
			       0));

  option_gc_end_position =
    (non_negative_numeric_option ("-gc-end-position",
				  option_raw_gc_end_position,
				  GC_END_POSITION_VARIABLE,
				  DEFAULT_GC_END_POSITION));

  option_gc_file =
    (standard_string_option (option_gc_file,
			     GC_FILE_VARIABLE,
			     DEFAULT_GC_FILE));

  option_gc_read_overlap =
    ((int)
     (non_negative_numeric_option ("-gc-read-overlap",
				   option_raw_gc_read_overlap,
				   GC_READ_OVERLAP_VARIABLE,
				   DEFAULT_GC_READ_OVERLAP)));

  option_gc_start_position =
    (non_negative_numeric_option ("-gc-start-position",
				  option_raw_gc_start_position,
				  GC_START_POSITION_VARIABLE,
				  DEFAULT_GC_START_POSITION));

  option_gc_window_size =
    (standard_numeric_option ("-gc-window-size",
			      option_raw_gc_window_size,
			      GC_WINDOW_SIZE_VARIABLE,
			      DEFAULT_GC_WINDOW_SIZE));

  option_gc_write_overlap =
    ((int)
     (non_negative_numeric_option ("-gc-write-overlap",
				   option_raw_gc_write_overlap,
				   GC_WRITE_OVERLAP_VARIABLE,
				   DEFAULT_GC_WRITE_OVERLAP)));

  if (option_summary)
    describe_options ();

}
