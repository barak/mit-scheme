/* -*-C-*-

$Id: option.c,v 1.68 2009/04/25 03:35:45 mhb Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* Command-line option processing */

#include <ctype.h>
#include "scheme.h"
#include "fasl.h"
#include "osenv.h"
#include "osfs.h"
#include <sys/stat.h>

#define xfree(p) OS_free ((void *) (p))

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#ifdef __WIN32__
#  include <io.h>
#  include "nt.h"
#  include "ntio.h"
#endif

#if defined(__WIN32__) || defined(__OS2__)
#  define DOS_LIKE_FILENAMES
#endif

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

static bool option_summary;

static const char * option_raw_library;
static const char * option_raw_utabmd;
static const char * option_raw_band;
static const char * option_raw_heap;
static const char * option_raw_constant;
static const char * option_raw_stack;

/* Command-line arguments */
int option_saved_argc;
const char ** option_saved_argv;
int option_unused_argc;
const char ** option_unused_argv;

/* Boolean options */
bool option_emacs_subprocess;
bool option_force_interactive;
bool option_disable_core_dump;
bool option_batch_mode;
bool option_show_version;
bool option_show_help;

/* String options */
const char ** option_library_path = 0;
const char * option_band_file = 0;
const char * option_fasl_file = 0;
const char * option_utabmd_file = 0;

/* Numeric options */
unsigned long option_heap_size;
unsigned long option_constant_size;
unsigned long option_stack_size;

void
print_help (void)
{
  outf_fatal ("Usage: mit-scheme --OPTION ARG ... --OPTION ARG ...\n\
\n\
This machine accepts the following command-line options.  The options\n\
may appear in any order, but they must all appear before any options\n\
for the band.\n\
\n\
--library PATH\n\
  Sets the library search path to PATH.  This is a colon-separated\n\
  list of directories that is searched to find various library files,\n\
  such as bands.  If this option is not given, the value of the\n\
  environment variable MITSCHEME_LIBRARY_PATH is used; it that isn't\n\
  defined, \"/usr/local/lib/mit-scheme\" is used.\n\
\n\
--band FILENAME\n\
  Specifies the initial band to be loaded.  Searches for FILENAME in\n\
  the working directory and the library directories, returning the\n\
  full pathname of the first readable file of that name.  If this\n\
  option isn't given, the filename is the value of the environment\n\
  variable MITSCHEME_BAND, or if that isn't defined, \"runtime.com\"; in\n\
  these cases the library directories are searched, but not the\n\
  working directory.\n\
\n\
--fasl FILENAME\n\
  Specifies that a cold load should be performed, using FILENAME as\n\
  the initial file to be loaded.  If this option isn't given, a normal\n\
  load is performed instead.  This option may not be used together\n\
  with the \"--band\" option.\n\
\n\
--utabmd FILENAME\n\
  Specifies the name of the microcode tables file.  The file is\n\
  searched for in the working directory and the library directories.\n\
  If this option isn't given, the filename is the value of the\n\
  environment variable MITSCHEME_UTABMD_FILE, or if that isn't\n\
  defined, \"utabmd.bin\"; in these cases the library directories are\n\
  searched, but not the working directory.\n\
\n\
--heap BLOCKS\n\
  Specifies the size of the heap in 1024-word blocks.  Overrides any\n\
  default.  Normally two such heaps are allocated; `bchscheme'\n\
  allocates only one.\n\
\n\
--constant BLOCKS\n\
  Specifies the size of constant space in 1024-word blocks.  Overrides\n\
  any default.\n\
\n\
--stack BLOCKS\n\
  Specifies the size of the stack in 1024-word blocks.  Overrides any\n\
  default.\n\
\n\
--option-summary\n\
  Causes Scheme to write option values to standard error.\n\
\n\
--help\n\
  Causes Scheme to report the available command line options.\n\
\n\
--version\n\
  Causes Scheme to report versions and copyrights, then exit.\n\
\n\
--batch-mode, --quiet, --silent\n\
  Suppresses the startup report of versions and copyrights, and the\n\
  valediction.\n\
\n\
--emacs\n\
  Specifies that Scheme is running as a subprocess of GNU Emacs.\n\
  This option is automatically supplied by GNU Emacs, and should not\n\
  be given under other circumstances.\n\
\n\
--interactive\n\
  If this option isn't specified, and Scheme's standard I/O is not a\n\
  terminal, Scheme will detach itself from its controlling terminal.\n\
  This will prevent it from getting signals sent to the process group\n\
  of that terminal.  If this option is specified, Scheme will not\n\
  detach itself from the controlling terminal.\n\
\n\
--nocore\n\
  Specifies that Scheme should not generate a core dump under any\n\
  circumstances.\n\
\n\
Please report bugs to %s.\n\
\n\
Additional options may be supported by the band (and described below).\n\
\n", PACKAGE_BUGREPORT);
}

#ifndef LIBRARY_PATH_VARIABLE
#  define LIBRARY_PATH_VARIABLE "MITSCHEME_LIBRARY_PATH"
#endif

#ifndef DEFAULT_LIBRARY_PATH
#  ifdef DOS_LIKE_FILENAMES
#    define DEFAULT_LIBRARY_PATH "\\scheme\\lib"
#  else
#    define DEFAULT_LIBRARY_PATH "/usr/local/lib/mit-scheme"
#  endif
#endif

#ifndef BAND_VARIABLE
#  define BAND_VARIABLE "MITSCHEME_BAND"
#endif

#ifndef DEFAULT_STD_BAND
#  define DEFAULT_STD_BAND "all.com"
#endif

#ifndef UTABMD_FILE_VARIABLE
#  define UTABMD_FILE_VARIABLE "MITSCHEME_UTABMD_FILE"
#endif

#ifndef DEFAULT_UTABMD_FILE
#  define DEFAULT_UTABMD_FILE "utabmd.bin"
#endif

#ifndef DEFAULT_HEAP_SIZE
#  define DEFAULT_HEAP_SIZE 4096
#endif

#ifndef HEAP_SIZE_VARIABLE
#  define HEAP_SIZE_VARIABLE "MITSCHEME_HEAP_SIZE"
#endif

#ifndef DEFAULT_CONSTANT_SIZE
#  define DEFAULT_CONSTANT_SIZE 1024
#endif

#ifndef MINIMUM_FASL_CONSTANT
#  define MINIMUM_FASL_CONSTANT (DEFAULT_CONSTANT_SIZE * 2)
#endif

#ifndef DEFAULT_STACK_SIZE
#  define DEFAULT_STACK_SIZE 128
#endif

#ifndef STACK_SIZE_VARIABLE
#  define STACK_SIZE_VARIABLE "MITSCHEME_STACK_SIZE"
#endif

static int
string_compare_ci (const char * string1, const char * string2)
{
  const char * scan1 = string1;
  unsigned int length1 = (strlen (string1));
  const char * scan2 = string2;
  unsigned int length2 = (strlen (string2));
  unsigned int length = ((length1 < length2) ? length1 : length2);
  const char * end1 = (scan1 + length);
  const char * end2 = (scan2 + length);
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
string_copy (const char * s)
{
  char * result = (OS_malloc ((strlen (s)) + 1));
  {
    const char * s1 = s;
    char * s2 = result;
    while (((*s2++) = (*s1++)) != '\0') ;
  }
  return (result);
}

struct option_descriptor
{
  const char * option;
  bool argument_p;
  void * value_cell;
};

static void
option_argument (const char * option, bool argument_p, void * value_cell)
{
  struct option_descriptor descriptor;
  (descriptor . option) = option;
  (descriptor . argument_p) = argument_p;
  (descriptor . value_cell) = value_cell;
  obstack_grow ((&scratch_obstack), (&descriptor), (sizeof (descriptor)));
}

static void
parse_options (int argc, const char ** argv)
{
  const char ** scan_argv = (argv + 1);
  const char ** end_argv = (scan_argv + (argc - 1));
  unsigned int n_descriptors
    = ((obstack_object_size (&scratch_obstack))
       / (sizeof (struct option_descriptor)));
  struct option_descriptor * descriptors = (obstack_finish (&scratch_obstack));
  struct option_descriptor * end_desc = (descriptors + n_descriptors);
  struct option_descriptor * scan_desc;
  for (scan_desc = descriptors; (scan_desc < end_desc); scan_desc += 1)
    if (scan_desc->argument_p)
      {
	const char ** value_cell = (scan_desc->value_cell);
	if (value_cell != 0)
	  (*value_cell) = 0;
      }
    else
      {
	bool * value_cell = (scan_desc->value_cell);
	if (value_cell != 0)
	  (*value_cell) = false;
      }
  while (scan_argv < end_argv)
    {
      const char * option = (*scan_argv++);
      if ((strncmp ("--", option, 2)) == 0)
	option += 2;
      else if ((strncmp ("-", option, 1)) == 0)
	option += 1;
      else
	{
	  scan_argv -= 1;
	  break;
	}
      for (scan_desc = descriptors; (scan_desc < end_desc); scan_desc += 1)
	if ((string_compare_ci (option, (scan_desc->option))) == 0)
	  {
	    if (scan_desc->argument_p)
	      {
		const char ** value_cell = (scan_desc->value_cell);
		if (scan_argv < end_argv)
		  {
		    if (value_cell != 0)
		      (*value_cell) = (*scan_argv++);
		  }
		else
		  {
		    outf_fatal ("%s: option --%s requires an argument.\n",
				scheme_program_name, option);
		    termination_init_error ();
		  }
	      }
	    else
	      {
		bool * value_cell = (scan_desc->value_cell);
		if (value_cell != 0)
		  (*value_cell) = true;
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
  /* Pass --version and --help through to the band, sort of. */
  if (strncmp ("--version", scan_argv[-1], 9) == 0)
    scan_argv--;
  if (strncmp ("--help", scan_argv[-1], 6) == 0)
    scan_argv--;

  option_saved_argc = argc;
  option_saved_argv = argv;
  option_unused_argc = (end_argv - scan_argv);
  option_unused_argv = scan_argv;
}

static void
parse_standard_options (int argc, const char ** argv)
{
  option_argument ("band", true, (&option_raw_band));
  option_argument ("batch-mode", false, (&option_batch_mode));
  option_argument ("constant", true, (&option_raw_constant));
  option_argument ("emacs", false, (&option_emacs_subprocess));
  option_argument ("fasl", true, (&option_fasl_file));
  option_argument ("heap", true, (&option_raw_heap));
  option_argument ("help", false, (&option_show_help));
  option_argument ("interactive", false, (&option_force_interactive));
  option_argument ("library", true, (&option_raw_library));
  option_argument ("nocore", false, (&option_disable_core_dump));
  option_argument ("option-summary", false, (&option_summary));
  option_argument ("quiet", false, (&option_batch_mode));
  option_argument ("silent", false, (&option_batch_mode));
  option_argument ("stack", true, (&option_raw_stack));
  option_argument ("utabmd", true, (&option_raw_utabmd));
  option_argument ("version", false, (&option_show_version));

  /* These are deprecated: */
  option_argument ("compiler", false, 0);
  option_argument ("edwin", false, 0);
  option_argument ("large", false, 0);
  option_argument ("utab", true, (&option_raw_utabmd));

  parse_options (argc, argv);
}

static const char *
standard_string_option (const char * option,
			const char * variable,
			const char * defval)
{
  if (option != 0)
    return (option);
  {
    const char * t = (getenv (variable));
    return ((t != 0) ? t : defval);
  }
}

static unsigned long
standard_numeric_option (const char * option,
			 const char * optval,
			 const char * variable,
			 unsigned long defval)
{
  if (optval != 0)
    {
      char * end;
      unsigned long n = (strtoul (optval, (&end), 0));;
      if ((end == optval) || ((*end) != '\0'))
	{
	  outf_fatal ("%s: illegal argument for option --%s: %s\n",
		      scheme_program_name, option, optval);
	  termination_init_error ();
	}
      return (n);
    }
  if (variable != 0)
    {
      const char * t = (getenv (variable));
      if (t != 0)
	{
	  char * end;
	  unsigned long n = (strtoul (t, (&end), 0));;
	  if ((end == t) || ((*end) != '\0'))
	    {
	      outf_fatal
		("%s: illegal value for environment variable %s: %s\n",
		 scheme_program_name, variable, t);
	      termination_init_error ();
	    }
	  return (n);
	}
    }
  return (defval);
}

static const char *
get_wd (void)
{
  const char * wd = (OS_working_dir_pathname ());
  unsigned int len = (strlen (wd));
  if ((wd [len - 1]) == SUB_DIRECTORY_DELIMITER)
    len -= 1;
  {
    char * result = (OS_malloc (len + 1));
    char * scan_result = result;
    const char * scan_wd = wd;
    const char * end_wd = (scan_wd + len);
    while (scan_wd < end_wd)
      (*scan_result++) = (*scan_wd++);
    (*scan_result) = '\0';
    return (result);
  }
}

static const char **
parse_path_string (const char * path)
{
  const char * start = path;
  /* It is important that this get_wd be called here to make sure that
     the the unix getcwd is called now, before it allocates heap space
     This is because getcwd forks off a new process and we want to do
     that before the scheme process gets too big
  */
  const char * wd = (get_wd ());
  unsigned int lwd = (strlen (wd));
  while (1)
    {
      const char * scan = start;
      const char * end;
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
	    char * element
	      = (OS_malloc ((absolute ? 0 : (lwd + 1)) + (end - start) + 1));
	    char * scan_element = element;
	    if (!absolute)
	      {
		const char * s = wd;
		const char * e = (wd + lwd);
		while (s < e)
		  (*scan_element++) = (*s++);
		(*scan_element++) = SUB_DIRECTORY_DELIMITER;
	      }
	    {
	      const char * s = start;
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
    const char ** elements = (obstack_finish (&scratch_obstack));
    const char ** scan = elements;
    const char ** end = (scan + (n_bytes / (sizeof (char *))));
    const char ** result = (OS_malloc (n_bytes));
    const char ** scan_result = result;
    while (scan < end)
      (*scan_result++) = (*scan++);
    obstack_free ((&scratch_obstack), elements);
    return (result);
  }
}

static void
free_parsed_path (const char ** path)
{
  const char ** scan = path;
  while (1)
    {
      const char * element = (*scan++);
      if (element == 0)
	break;
      xfree (element);
    }
  xfree (path);
}

const char *
search_for_library_file (const char * filename)
{
  unsigned int flen = (strlen (filename));
  const char ** scan_path = option_library_path;
  while (1)
    {
      const char * directory = (*scan_path++);
      unsigned int dlen;
      const char * fullname;
      if (directory == 0)
	return (0);
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
	  const char * result = (string_copy (fullname));
	  obstack_free ((&scratch_obstack), ((char *) fullname));
	  return (result);
	}
      obstack_free ((&scratch_obstack), ((char *) fullname));
    }
}

const char *
search_path_for_file (const char * option,
		      const char * filename,
		      bool default_p,
		      bool fail_p)
{
  const char * result = (search_for_library_file (filename));
  if (result != 0)
    return (result);
  if (!fail_p)
    return (filename);
  else
    {
      const char ** scan_path = option_library_path;
      outf_fatal ("%s: can't find a readable %s",
		  scheme_program_name,
		  (default_p ? "default" : "file"));
      if (option != 0)
	outf_fatal (" for option --%s", option);
      outf_fatal (".\n");
      outf_fatal ("\tsearched for file %s in these directories:\n", filename);
      if (!default_p)
	outf_fatal ("\t.\n");
      while (1)
	{
	  const char * element = (*scan_path++);
	  if (element == 0)
	    break;
	  outf_fatal ("\t%s\n", element);
	}
      termination_init_error ();
      /*NOTREACHED*/
      return (0);
    }
}

static const char *
standard_filename_option (const char * option,
			  const char * optval,
			  const char * variable,
			  const char * defval,
			  bool fail_p)
{
  if (optval != 0)
    {
      if (FILE_READABLE (optval))
	return (string_copy (optval));
      if (FILE_ABSOLUTE (optval))
	{
	  if (fail_p)
	    {
	      outf_fatal ("%s: can't read file %s for option --%s.\n",
			  scheme_program_name, optval, option);
	      termination_init_error ();
	    }
	  return (string_copy (optval));
	}
      return (search_path_for_file (option, optval, false, fail_p));
    }
  {
    const char * filename = (getenv (variable));
    if (filename == 0)
      filename = defval;
    if (FILE_ABSOLUTE (filename))
      {
	if ((! (FILE_READABLE (filename))) && fail_p)
	  {
	    outf_fatal ("%s: can't read default file %s for option --%s.\n",
			scheme_program_name, filename, option);
	    termination_init_error ();
	  }
	return (string_copy (filename));
      }
    else
      return (search_path_for_file (option, filename, true, fail_p));
  }
}

static void
conflicting_options (const char * option1, const char * option2)
{
  outf_fatal ("%s: can't specify both options --%s and --%s.\n",
	      scheme_program_name, option1, option2);
  termination_init_error ();
}

#define SCHEME_WORDS_TO_BLOCKS(n) (((n) + 1023) / 1024)

static int
read_band_sizes (const char * filename,
		 unsigned long * constant_size,
		 unsigned long * heap_size)
{
  fasl_file_handle_t handle;
  fasl_header_t h;
  bool ok;

  if (!open_fasl_input_file (filename, (&handle)))
    return (0);
  ok = (read_fasl_header ((&h), handle));
  if (! ((close_fasl_input_file (handle)) && ok))
    return (0);
  if ((check_fasl_version (&h)) != FASL_FILE_FINE)
    return (0);
  if ((check_fasl_cc_version ((&h),
			      COMPILER_INTERFACE_VERSION,
			      COMPILER_PROCESSOR_TYPE))
      != FASL_FILE_FINE)
    return (0);
  (*constant_size) = (SCHEME_WORDS_TO_BLOCKS (FASLHDR_CONSTANT_SIZE (&h)));
  (*heap_size) = (SCHEME_WORDS_TO_BLOCKS (FASLHDR_HEAP_SIZE (&h)));
  return (1);
}

static void
describe_boolean_option (const char * name, int value)
{
  outf_fatal ("  %s: %s\n", name, (value ? "yes" : "no"));
}

static void
describe_string_option (const char * name, const char * value)
{
  outf_fatal ("  %s: %s\n", name, value);
}

static void
describe_size_option (const char * name, unsigned int value)
{
  outf_fatal ("  %s size: %d\n", name, value);
}

static void
describe_path_option (const char * name, const char ** value)
{
  outf_fatal ("  %s: ", name);
  {
    const char ** scan = value;
    outf_fatal ("%s", (*scan++));
    while (1)
      {
	const char * element = (*scan++);
	if (element == 0) break;
	outf_fatal (":%s", element);
      }
  }
  outf_fatal ("\n");
}

static void
describe_options (void)
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
  describe_boolean_option ("emacs subprocess", option_emacs_subprocess);
  describe_boolean_option ("force interactive", option_force_interactive);
  describe_boolean_option ("disable core dump", option_disable_core_dump);
  describe_boolean_option ("suppress noise", option_batch_mode);
  if (option_unused_argc == 0)
    outf_fatal ("  no unused arguments\n");
  else
    {
      const char ** scan = option_unused_argv;
      const char ** end = (scan + option_unused_argc);
      outf_fatal ("  unused arguments:");
      while (scan < end)
	outf_fatal (" %s", (*scan++));
      outf_fatal ("\n");
    }
}

void
read_command_line_options (int argc, const char ** argv)
{
  bool band_sizes_valid = false;
  unsigned long band_constant_size = 0;
  unsigned long band_heap_size = 0;

  parse_standard_options (argc, argv);
  if (option_library_path != 0)
    free_parsed_path (option_library_path);
  option_library_path
    = (parse_path_string
       (standard_string_option (option_raw_library,
				LIBRARY_PATH_VARIABLE,
				DEFAULT_LIBRARY_PATH)));

  if (option_band_file != 0)
    {
      xfree (option_band_file);
      option_band_file = 0;
    }
  if (option_fasl_file != 0)
    {
      if (option_raw_band != 0)
	conflicting_options ("fasl", "band");
#ifndef CC_IS_C
      if (!FILE_READABLE (option_fasl_file))
	{
	  /* Kludge; FILE_READABLE doesn't work right for this case.  */
	  outf_fatal ("%s: can't read option file: --fasl %s\n",
		   scheme_program_name, option_fasl_file);
	  termination_init_error ();
	}
#endif
    }
  else
    {
      const char * default_band = DEFAULT_STD_BAND;
      const char * bands [] =
	{
	  DEFAULT_STD_BAND,
	  "runtime.com",
	  "mechanics.com",
	  "edwin-mechanics.com",
	  0
	};
      unsigned int i = 0;
      for ( ; ((bands[i]) != 0); i += 1)
	if (search_for_library_file (bands[i]))
	  {
	    default_band = (bands[i]);
	    break;
	  }
      option_band_file
	= (standard_filename_option ("band",
				     option_raw_band,
				     BAND_VARIABLE,
				     default_band,
				     true));
    }
  if (option_band_file != 0)
    band_sizes_valid
      = (read_band_sizes (option_band_file,
			  (&band_constant_size),
			  (&band_heap_size)));

  option_heap_size
    = (standard_numeric_option ("heap",
				option_raw_heap,
				HEAP_SIZE_VARIABLE,
				DEFAULT_HEAP_SIZE));
  if (band_sizes_valid)
    option_heap_size += band_heap_size;
  else if ((option_fasl_file != 0)
	   && (option_heap_size < MINIMUM_FASL_CONSTANT))
    option_heap_size = MINIMUM_FASL_CONSTANT;
  option_constant_size
    = (standard_numeric_option ("constant",
				option_raw_constant,
				0,
				(band_sizes_valid
				 ? band_constant_size
				 : DEFAULT_CONSTANT_SIZE)));
  option_stack_size
    = (standard_numeric_option ("stack",
				option_raw_stack,
				STACK_SIZE_VARIABLE,
				DEFAULT_STACK_SIZE));
  if (option_utabmd_file != 0)
    {
      xfree (option_utabmd_file);
      option_utabmd_file = 0;
    }
  option_utabmd_file
    = (standard_filename_option ("utabmd",
				 option_raw_utabmd,
				 UTABMD_FILE_VARIABLE,
				 DEFAULT_UTABMD_FILE,
#ifdef CC_IS_C
				 /* FIXME: This should check if we
				    have "microcode_utabmd"
				    compiled */
				 false
#else
				 (option_fasl_file != 0)
#endif
				 ));

  if (option_show_version)
    outf_console ("MIT/GNU Scheme %s\n", PACKAGE_VERSION);
  if (option_show_help)
    print_help ();
  if (option_summary)
    describe_options ();
}
