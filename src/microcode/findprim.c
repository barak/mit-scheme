/* -*-C-*-

$Id: findprim.c,v 9.62 2008/01/30 20:02:12 cph Exp $

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

/* Preprocessor to find and declare defined primitives.  */

/*
 * This program searches for a particular token which tags primitive
 * definitions.  This token is also a macro defined in primitive.h.
 * For each macro invocation it creates an entry in the primitives
 * descriptor vector used by Scheme.  The entry consists of the C
 * routine implementing the primitive, the (fixed) number of arguments
 * it requires, and the name Scheme uses to refer to it.
 *
 * The output is a C source file to be compiled and linked with the
 * Scheme microcode.
 *
 * This program understands the following options (must be given in
 * this order):
 *
 * -o fname
 *    Put the output file in fname.  The default is to put it on the
 *    standard output.
 *
 * -e or -b n (exclusive)
 *    -e: produce the old external primitive table instead of the
 *    complete primitive table.
 *    -b: Produce the old built-in primitive table instead of the
 *    complete primitive table.  The table should have size n (in hex).
 *
 * -l fname
 *    The list of files to examine is contained in fname, one file
 *    per line.  Semicolons (';') introduce comment lines.
 *
 * Note that some output lines are done in a strange fashion because
 * some C compilers (the vms C compiler, for example) remove comments
 * even from within string quotes!!
 *
 */

/* Some utility imports and definitions. */

#include "config.h"
#include <stdio.h>

#ifdef vms
/* VMS version 3 has no void. */
/* #define void */
#  define NORMAL_EXIT() return
#else
#  define NORMAL_EXIT() exit(0)
#endif

/* The 4.2 bsd vax compiler has a bug which forces the following. */

#define pseudo_void int
#define pseudo_return return (0)

void *
xmalloc (unsigned long length)
{
  void * result = (malloc (length));
  if (result == 0)
    {
      fprintf (stderr, "malloc: unable to allocate %ld bytes\n", length);
      exit (1);
    }
  return (result);
}

void *
xrealloc (void * ptr, unsigned long length)
{
  void * result = (realloc (ptr, length));
  if (result == 0)
    {
      fprintf (stderr, "realloc: unable to allocate %ld bytes\n", length);
      exit (1);
    }
  return (result);
}

#define FIND_INDEX_LENGTH(index, size)					\
{									\
  char index_buffer [64];						\
									\
  sprintf (index_buffer, "%x", (index));				\
  (size) = (strlen (index_buffer));					\
}

#ifdef DEBUGGING
#  define dprintf(one, two) fprintf(stderr, one, two)
#else
#  define dprintf(one, two)
#endif

/* Maximum number of primitives that can be handled. */

bool built_in_p;

char * token_array [4];
char default_token [] = "Define_Primitive";
char default_token_alternate [] = "DEFINE_PRIMITIVE";
char built_in_token [] = "Built_In_Primitive";
char external_token [] = "Define_Primitive";

typedef pseudo_void (* TOKEN_PROCESSOR) (void);
TOKEN_PROCESSOR token_processors [4];

char * the_kind;
char default_kind [] = "Static_Primitive";
char built_in_kind [] = "Primitive";
char external_kind [] = "External";

char * the_variable;
char default_variable [] = "MAX_STATIC_PRIMITIVE";
char built_in_variable [] = "MAX_PRIMITIVE";
char external_variable [] = "MAX_EXTERNAL_PRIMITIVE";

#define LEXPR_ARITY_STRING	"-1"

FILE * input;
FILE * output;
char * name;
char * file_name;

struct descriptor
  {
    char * c_name;		/* The C name of the function */
    char * arity;		/* Number of arguments */
    char * scheme_name;		/* Scheme name of the primitive */
    char * documentation;	/* Documentation string */
    char * file_name;		/* File where found. */
  };

int buffer_index;
int buffer_length;
struct descriptor (* data_buffer) [];
struct descriptor ** result_buffer;

int max_scheme_name_length;
int max_c_name_length;
int max_arity_length;
int max_documentation_length;
int max_file_name_length;
int max_index_length;

struct descriptor dummy_entry =
  {"Dummy_Primitive", "0", "DUMMY-PRIMITIVE", "", "Findprim.c"};

char dummy_error_string [] =
  "Microcode_Termination (TERM_BAD_PRIMITIVE)";

struct descriptor inexistent_entry =
  {"Prim_inexistent", LEXPR_ARITY_STRING, "INEXISTENT-PRIMITIVE", "", "Findprim.c"};

char inexistent_error_string [] =
  "signal_error_from_primitive (ERR_UNIMPLEMENTED_PRIMITIVE)";

/* forward references */

TOKEN_PROCESSOR scan (void);
bool whitespace (int c);
int compare_descriptors (struct descriptor * d1, struct descriptor * d2);
int read_index (char * arg, char * identification);
int strcmp_ci (char * s1, char * s2);
pseudo_void create_alternate_entry (void);
pseudo_void create_builtin_entry (void);
pseudo_void create_normal_entry (void);
void dump (bool check);
void grow_data_buffer (void);
void grow_token_buffer (void);
void initialize_builtin (char * arg);
void initialize_data_buffer (void);
void initialize_default (void);
void initialize_external (void);
void initialize_token_buffer (void);
static void fp_mergesort
  (int, int, struct descriptor **, struct descriptor **);
void print_procedure (FILE * output,
			      struct descriptor * primitive_descriptor,
			      char * error_string);
void print_primitives (FILE * output, int limit);
void print_spaces (FILE * output, int how_many);
void print_entry (FILE * output, int index,
			  struct descriptor * primitive_descriptor);
void process (void);
void process_argument (char * fn);
void scan_to_token_start (void);
void skip_token (void);
void sort (void);
void update_from_entry (struct descriptor * primitive_descriptor);

int
main (int argc, char ** argv)
{
  name = argv[0];

  /* Check for specified output file */

  if ((argc >= 2) && ((strcmp ("-o", argv[1])) == 0))
    {
      output = (fopen (argv[2], "w"));
      if (output == NULL)
	{
	  fprintf(stderr, "Error: %s can't open %s\n", name, argv[2]);
	  exit (1);
	}
      argv += 2;
      argc -= 2;
    }
  else
    output = stdout;

  initialize_data_buffer ();
  initialize_token_buffer ();

  /* Check whether to produce the built-in table instead.
     The argument after the option letter is the size of the
     table to build.  */

  if ((argc >= 2) && ((strcmp ("-b", argv[1])) == 0))
    {
      initialize_builtin (argv[2]);
      argv += 2;
      argc -= 2;
    }
  else if ((argc >= 1) && ((strcmp ("-e", argv[1])) == 0))
    {
      initialize_external ();
      argv += 1;
      argc -= 1;
    }
  else
    initialize_default ();

  /* Check whether there are any files left. */
  if (argc == 1)
    {
      dump (0);
      goto done;
    }

  if ((argc >= 2) && ((strcmp ("-l", argv[1])) == 0))
    {
      /* The list of files is stored in another file. */

      char fn [1024];
      FILE * file_list_file;

      file_list_file = (fopen (argv[2], "r"));
      if (file_list_file == NULL)
	{
	  fprintf (stderr, "Error: %s can't open %s\n", name, argv[2]);
	  dump (1);
	  exit (1);
	}
      while ((fgets (fn, 1024, file_list_file)) != NULL)
	{
	  int i;

	  i = (strlen (fn)) - 1;
	  if ((i >= 0) && (fn[i] == '\n'))
	    {
	      fn[i] = '\0';
	      i -= 1;
	    }
	  if ((i > 0) && (fn[0] != ';'))
	    {
	      char * arg;

	      arg = (xmalloc ((strlen (fn)) + 1));
	      strcpy (arg, fn);
	      process_argument (arg);
	    }
	}
      fclose (file_list_file);
    }
  else
    /* The list of files is in the argument list. */
    while ((--argc) > 0)
      process_argument (*++argv);

  if (! built_in_p)
    {
      dprintf ("About to sort %s\n", "");
      sort ();
    }
  dprintf ("About to dump %s\n", "");
  dump (1);

 done:
  if (output != stdout)
    fclose (output);
  NORMAL_EXIT ();
  return (0);
}

void
process_argument (char * fn)
{
  file_name = fn;
  if ((strcmp ("-", file_name)) == 0)
    {
      input = stdin;
      file_name = "stdin";
      dprintf ("About to process %s\n", "STDIN");
      process ();
    }
  else if ((input = (fopen (file_name, "r"))) == NULL)
    {
      fprintf (stderr, "Error: %s can't open %s\n", name, file_name);
      dump (1);
      exit (1);
    }
  else
    {
      dprintf ("About to process %s\n", file_name);
      process ();
      fclose (input);
    }
  return;
}

/* Search for tokens and when found, create primitive entries. */

void
process (void)
{
  TOKEN_PROCESSOR processor;

  while (1)
    {
      processor = (scan ());
      if (processor == NULL) break;
      dprintf ("Process: place found.%s\n", "");
      (* processor) ();
    }
  return;
}

/* Search for token and stop when found.  If you hit open comment
 * character, read until you hit close comment character.
 * *** FIX *** : It is not a complete C parser, thus it may be fooled,
 *      currently the token must always begin a line.
 */

TOKEN_PROCESSOR
scan (void)
{
  int c;
  char compare_buffer [1024];

  c = '\n';
  while (c != EOF)
    {
      switch (c)
	{
	case '/':
	  if ((c = (getc (input)))  == '*')
	    {
	      c = (getc (input));
	      while (1)
		{
		  while (c != '*')
		    {
		      if (c == EOF)
			{
			  fprintf (stderr,
				   "Error: EOF in comment in file %s, or %s confused\n",
				   file_name, name);
			  dump (1);
			  exit (1);
			}
		      c = (getc (input));
		    }
		  c = (getc (input));
		  if (c == '/') break;
		}
	    }
	  else if (c != '\n') break;

	case '\n':
	  {
	    {
	      char * scan_buffer;

	      scan_buffer = (& (compare_buffer [0]));
	      while (1)
		{
		  c = (getc (input));
		  if (c == EOF)
		    return (NULL);
		  else if ((isalnum (c)) || (c == '_'))
		    (*scan_buffer++) = c;
		  else
		    {
		      ungetc (c, input);
		      (*scan_buffer++) = '\0';
		      break;
		    }
		}
	    }
	    {
	      char **scan_tokens;

	      for (scan_tokens = (& (token_array [0]));
		   ((* scan_tokens) != NULL);
		   scan_tokens += 1)
		if ((strcmp ((& (compare_buffer [0])), (* scan_tokens))) == 0)
		  return (token_processors [scan_tokens - token_array]);
	    }
	    break;
	  }

	default: {}
	}
      c = (getc (input));
    }
  return (NULL);
}

/* Output Routines */

void
dump (bool check)
{
  int max_index;
  int count;

  FIND_INDEX_LENGTH (buffer_index, max_index_length);
  max_index = (buffer_index - 1);

  /* Print header. */
  fprintf (output, "/%c Emacs: This is -*- C -*- code. %c/\n\n", '*', '*');
  fprintf (output, "/%c %s primitive declarations. %c/\n\n",
	   '*', ((built_in_p) ? "Built in" : "User defined" ), '*');
  fprintf (output, "#include \"usrdef.h\"\n\n");
  fprintf (output,
	   "long %s = %d; /%c = 0x%x %c/\n\n",
	   the_variable, max_index, '*', max_index, '*');

  if (built_in_p)
    fprintf (output,
	     "/%c The number of implemented primitives is %d. %c/\n\n",
	     '*', buffer_index, '*');

  if (buffer_index == 0)
    {
      if (check)
	fprintf (stderr, "No primitives found!\n");

      /* C does not understand empty arrays, thus it must be faked. */
      fprintf (output, "/%c C does not understand empty arrays, ", '*');
      fprintf (output, "thus it must be faked. %c/\n\n", '*');
    }
  else
    {
      /* Print declarations. */
      fprintf (output, "extern SCHEME_OBJECT\n");
      for (count = 0; (count <= max_index); count += 1)
      {
	fprintf (output, "  %s (void)",
		 (((* data_buffer) [count]) . c_name));
	if (count == max_index)
	  fprintf (output, ";\n\n");
	else
	  fprintf (output, ",\n");
      }
    }

  print_procedure
    (output, (& inexistent_entry), (& (inexistent_error_string [0])));
  print_primitives (output, buffer_index);
  return;
}

void
print_procedure (FILE * output,
		 struct descriptor * primitive_descriptor,
		 char * error_string)
{
  fprintf (output, "SCHEME_OBJECT\n");
  fprintf (output, "%s (void)\n",
	   (primitive_descriptor -> c_name));
  fprintf (output, "{\n");
  fprintf (output, "  PRIMITIVE_HEADER (%s);\n",
	   (primitive_descriptor -> arity));
  fprintf (output, "\n");
  fprintf (output, "  %s;\n", error_string);
  fprintf (output, "  /%cNOTREACHED%c/\n", '*', '*');
  fprintf (output, "  PRIMITIVE_RETURN (UNSPECIFIC);\n");
  fprintf (output, "}\n");

  return;
}

void
print_primitives (FILE * output, int limit)
{
  int last;
  int count;
  char * table_entry;

  last = (limit - 1);

  /* Print the procedure table. */
  fprintf
    (output,
     "\f\nSCHEME_OBJECT (* (%s_Procedure_Table [])) (void) = {\n",
     the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      print_entry (output, count, (result_buffer [count]));
      fprintf (output, ",\n");
    }
  print_entry (output, (-1), (& inexistent_entry));
  fprintf (output, "\n};\n");

  /* Print the names table. */
  fprintf (output, "\f\nconst char * %s_Name_Table [] = {\n", the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      fprintf (output, "  \"%s\",\n", ((result_buffer [count]) -> scheme_name));
    }
  fprintf (output, "  \"%s\"\n};\n", inexistent_entry.scheme_name);

  /* Print the documentation table. */
  fprintf (output, "\f\nconst char * %s_Documentation_Table [] = {\n", the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      fprintf (output, "  ");
      table_entry = ((result_buffer [count]) -> documentation);
      if ((table_entry [0]) == '\0')
	fprintf (output, "0,\n");
      else
	fprintf (output, "\"%s\",\n", table_entry);
    }
  fprintf (output, "  ((char *) 0)\n};\n");

  /* Print the arity table. */
  fprintf (output, "\f\nint %s_Arity_Table [] = {\n", the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      fprintf (output, "  %s,\n", ((result_buffer [count]) -> arity));
    }
  fprintf (output, "  %s\n};\n", inexistent_entry.arity);

  /* Print the counts table. */
  fprintf (output, "\f\nint %s_Count_Table [] = {\n", the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      fprintf (output,
	       "  (%s * ((int) (sizeof (SCHEME_OBJECT)))),\n",
	       ((result_buffer [count]) -> arity));
    }
  fprintf (output, "  (%s * ((int) (sizeof (SCHEME_OBJECT))))\n};\n",
	   inexistent_entry.arity);

  return;
}

void
print_entry (FILE * output,
	     int index,
	     struct descriptor * primitive_descriptor)
{
  int index_length;

  fprintf (output, "  %-*s ",
	   max_c_name_length, (primitive_descriptor -> c_name));
  fprintf (output, "/%c ", '*');
  fprintf (output, "%*s %-*s",
	   max_arity_length, (primitive_descriptor -> arity),
	   max_scheme_name_length, (primitive_descriptor -> scheme_name));
  fprintf (output, " %s ", the_kind);
  if (index >= 0)
    {
      FIND_INDEX_LENGTH (index, index_length);
      print_spaces (output, (max_index_length - index_length));
      fprintf (output, "0x%x", index);
    }
  else
    {
      print_spaces (output, (max_index_length - 1));
      fprintf (output, "???");
    }
  fprintf (output, " in %s %c/", (primitive_descriptor -> file_name), '*');
  return;
}

void
print_spaces (FILE * output, int how_many)
{
  while ((--how_many) >= 0)
    putc (' ', output);
  return;
}

/* Input Parsing */

char * token_buffer;
int token_buffer_length;

void
initialize_token_buffer (void)
{
  token_buffer_length = 80;
  token_buffer = (xmalloc (token_buffer_length));
  return;
}

void
grow_token_buffer (void)
{
  token_buffer_length *= 2;
  token_buffer = (xrealloc (token_buffer, token_buffer_length));
  return;
}

#define TOKEN_BUFFER_DECLS()						\
  char * TOKEN_BUFFER_scan;					\
  char * TOKEN_BUFFER_end

#define TOKEN_BUFFER_START()						\
{									\
  TOKEN_BUFFER_scan = token_buffer;					\
  TOKEN_BUFFER_end = (token_buffer + token_buffer_length);		\
}

#define TOKEN_BUFFER_WRITE(c)						\
{									\
  if (TOKEN_BUFFER_scan == TOKEN_BUFFER_end)				\
    {									\
      int n;								\
									\
      n = (TOKEN_BUFFER_scan - token_buffer);				\
      grow_token_buffer ();						\
      TOKEN_BUFFER_scan = (token_buffer + n);				\
      TOKEN_BUFFER_end = (token_buffer + token_buffer_length);		\
    }									\
  (*TOKEN_BUFFER_scan++) = (c);						\
}

#define TOKEN_BUFFER_OVERWRITE(s)					\
{									\
  int TOKEN_BUFFER_n;							\
									\
  TOKEN_BUFFER_n = ((strlen (s)) + 1);					\
  while (TOKEN_BUFFER_n > token_buffer_length)				\
    {									\
      grow_token_buffer ();						\
      TOKEN_BUFFER_end = (token_buffer + token_buffer_length);		\
    }									\
  strcpy (token_buffer, s);						\
  TOKEN_BUFFER_scan = (token_buffer + TOKEN_BUFFER_n);			\
}

#define TOKEN_BUFFER_FINISH(target, size)				\
{									\
  int TOKEN_BUFFER_n;							\
  char * TOKEN_BUFFER_result;						\
									\
  TOKEN_BUFFER_n = (TOKEN_BUFFER_scan - token_buffer);			\
  TOKEN_BUFFER_result = (xmalloc (TOKEN_BUFFER_n));			\
  strcpy (TOKEN_BUFFER_result, token_buffer);				\
  (target) = TOKEN_BUFFER_result;					\
  TOKEN_BUFFER_n -= 1;							\
  if ((size) < TOKEN_BUFFER_n)						\
    (size) = TOKEN_BUFFER_n;						\
}

enum tokentype
  {
    tokentype_integer,
    tokentype_identifier,
    tokentype_string,
    tokentype_string_upcase
  };

void
copy_token (char ** target, int * size, enum tokentype token_type)
{
  int c;
  TOKEN_BUFFER_DECLS ();

  TOKEN_BUFFER_START ();
  c = (getc (input));
  if (c == '\"')
    {
      while (1)
	{
	  c = (getc (input));
	  if (c == '\"') break;
	  if (c == '\\')
	    {
	      TOKEN_BUFFER_WRITE (c);
	      c = (getc (input));
	      TOKEN_BUFFER_WRITE (c);
	    }
	  else
	    TOKEN_BUFFER_WRITE
	      (((token_type == tokentype_string_upcase) &&
		(isalpha (c)) &&
		(islower (c)))
	       ? (toupper (c))
	       : c);
	}
      TOKEN_BUFFER_WRITE ('\0');
    }
  else
    {
      TOKEN_BUFFER_WRITE (c);
      while (1)
	{
	  c = (getc (input));
	  if (whitespace (c)) break;
	  TOKEN_BUFFER_WRITE (c);
	}
      TOKEN_BUFFER_WRITE ('\0');
      if ((strcmp (token_buffer, "LEXPR")) == 0)
	{
	  TOKEN_BUFFER_OVERWRITE (LEXPR_ARITY_STRING);
	}
      else if ((token_type == tokentype_string) &&
	       ((strcmp (token_buffer, "0")) == 0))
	TOKEN_BUFFER_OVERWRITE ("");
    }
  TOKEN_BUFFER_FINISH ((* target), (* size));
  return;
}

bool
whitespace (int c)
{
  switch (c)
    {
    case ' ':
    case '\t':
    case '\n':
    case '\r':
    case '(':
    case ')':
    case ',': return 1;
    default: return 0;
    }
}

void
scan_to_token_start (void)
{
  int c;

  while (whitespace (c = (getc (input)))) ;
  ungetc (c, input);
  return;
}

void
skip_token (void)
{
  int c;

  while (! (whitespace (c = (getc (input))))) ;
  ungetc (c, input);
  return;
}

void
initialize_data_buffer (void)
{
  buffer_length = 0x200;
  buffer_index = 0;
  data_buffer =
    ((struct descriptor (*) [])
     (xmalloc (buffer_length * (sizeof (struct descriptor)))));
  result_buffer =
    ((struct descriptor **)
     (xmalloc (buffer_length * (sizeof (struct descriptor *)))));

  max_c_name_length = 0;
  max_arity_length = 0;
  max_scheme_name_length = 0;
  max_documentation_length = 0;
  max_file_name_length = 0;
  update_from_entry (& inexistent_entry);

  return;
}

void
grow_data_buffer (void)
{
  char * old_data_buffer = ((char *) data_buffer);
  buffer_length *= 2;
  data_buffer =
    ((struct descriptor (*) [])
     (xrealloc (((char *) data_buffer),
		(buffer_length * (sizeof (struct descriptor))))));
  {
    struct descriptor ** scan = result_buffer;
    struct descriptor ** end = (result_buffer + buffer_index);
    long offset = (((char *) data_buffer) - old_data_buffer);
    while (scan < end)
      {
	(*scan) = ((struct descriptor *) (((char*) (*scan)) + offset));
	scan += 1;
      }
  }
  result_buffer =
    ((struct descriptor **)
     (xrealloc (((char *) result_buffer),
		(buffer_length * (sizeof (struct descriptor *))))));
  return;
}

#define MAYBE_GROW_BUFFER()						\
{									\
  if (buffer_index == buffer_length)					\
    grow_data_buffer ();						\
}

#define COPY_SCHEME_NAME(desc)						\
{									\
  scan_to_token_start ();						\
  copy_token ((& ((desc) . scheme_name)),				\
	      (& max_scheme_name_length),				\
	      tokentype_string_upcase);					\
}

#define COPY_C_NAME(desc)						\
{									\
  scan_to_token_start ();						\
  copy_token ((& ((desc) . c_name)),					\
	      (& max_c_name_length),					\
	      tokentype_identifier);					\
}

#define COPY_ARITY(desc)						\
{									\
  scan_to_token_start ();						\
  copy_token ((& ((desc) . arity)),					\
	      (& max_arity_length),					\
	      tokentype_integer);					\
}

#define COPY_DOCUMENTATION(desc)					\
{									\
  scan_to_token_start ();						\
  copy_token ((& ((desc) . documentation)),				\
	      (& max_documentation_length),				\
	      tokentype_string);					\
}

#define DEFAULT_DOCUMENTATION(desc)					\
{									\
  ((desc) . documentation) = "";					\
}

#define COPY_FILE_NAME(desc)						\
{									\
  int length;								\
									\
  ((desc) . file_name) = file_name;					\
  length = (strlen (file_name));					\
  if (max_file_name_length < length)					\
    max_file_name_length = length;					\
}

void
initialize_default (void)
{
  built_in_p = 0;
  (token_array [0]) = (& (default_token [0]));
  (token_array [1]) = (& (default_token_alternate [0]));
  (token_array [2]) = NULL;
  (token_processors [0]) = create_normal_entry;
  (token_processors [1]) = create_alternate_entry;
  (token_processors [2]) = NULL;
  the_kind = (& (default_kind [0]));
  the_variable = (& (default_variable [0]));
  return;
}

void
initialize_external (void)
{
  built_in_p = 0;
  (token_array [0]) = (& (external_token [0]));
  (token_array [1]) = NULL;
  (token_processors [0]) = create_normal_entry;
  (token_processors [1]) = NULL;
  the_kind = (& (external_kind [0]));
  the_variable = (& (external_variable [0]));
  return;
}

void
initialize_builtin (char * arg)
{
  int length;
  int index;

  built_in_p = 1;
  length = (read_index (arg, "built_in_table_size"));
  while (buffer_length < length)
    grow_data_buffer ();
  for (index = 0; (index < buffer_length); index += 1)
    (result_buffer [index]) = NULL;
  buffer_index = length;
  (token_array [0]) = (& (built_in_token [0]));
  (token_array [1]) = NULL;
  (token_processors [0]) = create_builtin_entry;
  (token_processors [1]) = NULL;
  the_kind = (& (built_in_kind [0]));
  the_variable = (& (built_in_variable [0]));
  return;
}

void
update_from_entry (struct descriptor * primitive_descriptor)
{
  int temp;

  temp = (strlen (primitive_descriptor -> scheme_name));
  if (max_scheme_name_length < temp)
    max_scheme_name_length = temp;

  temp = (strlen (primitive_descriptor -> c_name));
  if (max_c_name_length < temp)
    max_c_name_length = temp;

  temp = (strlen (primitive_descriptor -> arity));
  if (max_arity_length < temp)
    max_arity_length = temp;

  temp = (strlen (primitive_descriptor -> documentation));
  if (max_documentation_length < temp)
    max_documentation_length = temp;

  temp = (strlen (primitive_descriptor -> file_name));
  if (max_file_name_length < temp)
    max_file_name_length = temp;

  return;
}

pseudo_void
create_normal_entry (void)
{
  MAYBE_GROW_BUFFER ();
  COPY_C_NAME ((* data_buffer) [buffer_index]);
  COPY_ARITY ((* data_buffer) [buffer_index]);
  COPY_SCHEME_NAME ((* data_buffer) [buffer_index]);
  DEFAULT_DOCUMENTATION ((* data_buffer) [buffer_index]);
  COPY_FILE_NAME ((* data_buffer) [buffer_index]);
  (result_buffer [buffer_index]) = (& ((* data_buffer) [buffer_index]));
  buffer_index += 1;
  pseudo_return;
}

pseudo_void
create_alternate_entry (void)
{
  MAYBE_GROW_BUFFER ();
  COPY_SCHEME_NAME ((* data_buffer) [buffer_index]);
  COPY_C_NAME ((* data_buffer) [buffer_index]);
  scan_to_token_start ();
  skip_token ();		/* min_args */
  COPY_ARITY ((* data_buffer) [buffer_index]);
  COPY_DOCUMENTATION ((* data_buffer) [buffer_index]);
  COPY_FILE_NAME ((* data_buffer) [buffer_index]);
  (result_buffer [buffer_index]) = (& ((* data_buffer) [buffer_index]));
  buffer_index += 1;
  pseudo_return;
}

pseudo_void
create_builtin_entry (void)
{
  struct descriptor desc;
  int length;
  int index;
  char * index_buffer;

  COPY_C_NAME (desc);
  COPY_ARITY (desc);
  COPY_SCHEME_NAME (desc);
  DEFAULT_DOCUMENTATION (desc);
  COPY_FILE_NAME (desc);
  index = 0;
  scan_to_token_start();
  copy_token ((& index_buffer), (& index), tokentype_integer);
  index = (read_index (index_buffer, "index"));
  length = (index + 1);
  if (buffer_length < length)
    {
      int i;

      while (buffer_length < length)
	grow_data_buffer ();
      for (i = buffer_index; (i < buffer_length); i += 1)
	(result_buffer [i]) = NULL;
    }
  if (buffer_index < length)
    buffer_index = length;
  if ((result_buffer [index]) != NULL)
    {
      fprintf (stderr, "%s: redefinition of primitive %d.\n", name, index);
      fprintf (stderr, "previous definition:\n");
      FIND_INDEX_LENGTH (buffer_index, max_index_length);
      print_entry (stderr, index, (result_buffer [index]));
      fprintf (stderr, "\n");
      fprintf (stderr, "new definition:\n");
      print_entry (stderr, index, (& ((* data_buffer) [index])));
      fprintf (stderr, "\n");
      exit (1);
    }
  ((* data_buffer) [index]) = desc;
  (result_buffer [index]) = (& ((* data_buffer) [index]));
  pseudo_return;
}

int
read_index (char * arg, char * identification)
{
  int result = 0;
  if (((arg [0]) == '0') && ((arg [1]) == 'x'))
    sscanf ((& (arg [2])), "%x", (& result));
  else
    sscanf ((& (arg [0])), "%d", (& result));
  if (result < 0)
    {
      fprintf (stderr, "%s == %d\n", identification, result);
      exit (1);
    }
  return (result);
}

/* Sorting */

void
sort (void)
{
  struct descriptor ** temp_buffer;
  int count;

  if (buffer_index <= 0)
    return;
  temp_buffer =
    ((struct descriptor **)
     (xmalloc (buffer_index * (sizeof (struct descriptor *)))));
  for (count = 0; (count < buffer_index); count += 1)
    (temp_buffer [count]) = (result_buffer [count]);
  fp_mergesort (0, (buffer_index - 1), result_buffer, temp_buffer);
  free (temp_buffer);
}

static void
fp_mergesort (int low,
	      int high,
	      struct descriptor ** array,
	      struct descriptor ** temp_array)
{
  int index;
  int low1;
  int low2;
  int high1;
  int high2;

  dprintf ("fp_mergesort: low = %d", low);
  dprintf ("; high = %d", high);

  if (high <= low)
    {
      dprintf ("; done.%s\n", "");
      return;
    }

  low1 = low;
  high1 = ((low + high) / 2);
  low2 = (high1 + 1);
  high2 = high;

  dprintf ("; high1 = %d\n", high1);

  fp_mergesort (low, high1, temp_array, array);
  fp_mergesort (low2, high, temp_array, array);

  dprintf ("fp_mergesort: low1 = %d", low1);
  dprintf ("; high1 = %d", high1);
  dprintf ("; low2 = %d", low2);
  dprintf ("; high2 = %d\n", high2);

  for (index = low; (index <= high); index += 1)
    {
      dprintf ("index = %d", index);
      dprintf ("; low1 = %d", low1);
      dprintf ("; low2 = %d\n", low2);

      if (low1 > high1)
	{
	  (array [index]) = (temp_array [low2]);
	  low2 += 1;
	}
      else if (low2 > high2)
	{
	  (array [index]) = (temp_array [low1]);
	  low1 += 1;
	}
      else
	{
	  switch (compare_descriptors ((temp_array [low1]),
				       (temp_array [low2])))
	    {
	    case (-1):
	      (array [index]) = (temp_array [low1]);
	      low1 += 1;
	      break;

	    case 1:
	      (array [index]) = (temp_array [low2]);
	      low2 += 1;
	      break;

	    default:
	      fprintf (stderr, "Error: bad comparison.\n");
	      goto comparison_abort;

	    case 0:
	      {
		fprintf (stderr, "Error: repeated primitive.\n");
	      comparison_abort:
		FIND_INDEX_LENGTH (buffer_index, max_index_length);
		output = stderr;
		fprintf (stderr, "definition 1:\n");
		print_entry (output, low1, (temp_array [low1]));
		fprintf (stderr, "\ndefinition 2:\n");
		print_entry (output, low2, (temp_array [low2]));
		fprintf (stderr, "\n");
		exit (1);
		break;
	      }
	    }
	}
    }
}

int
compare_descriptors (struct descriptor * d1, struct descriptor * d2)
{
  int value;

  dprintf ("comparing \"%s\"", (d1 -> scheme_name));
  dprintf(" and \"%s\".\n", (d2 -> scheme_name));
  value = (strcmp_ci ((d1 -> scheme_name), (d2 -> scheme_name)));
  if (value > 0)
    return (1);
  else if (value < 0)
    return (-1);
  else
    return (0);
}

int
strcmp_ci (char * s1, char * s2)
{
  int length1 = (strlen (s1));
  int length2 = (strlen (s2));
  int length = ((length1 < length2) ? length1 : length2);

  while ((length--) > 0)
    {
      int c1 = (*s1++);
      int c2 = (*s2++);
      if (islower (c1)) c1 = (toupper (c1));
      if (islower (c2)) c2 = (toupper (c2));
      if (c1 < c2) return (-1);
      if (c1 > c2) return (1);
    }
  return (length1 - length2);
}
