/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/findprim.c,v 9.34 1988/08/15 20:31:50 cph Exp $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

#include <stdio.h>

/* For macros toupper, isalpha, etc,
   supposedly on the standard library.  */

#include <ctype.h>

extern int strcmp ();
extern int strlen ();

typedef int boolean;
#define TRUE 1
#define FALSE 0

#ifdef vms
/* VMS version 3 has no void. */
/* #define void */
#define NORMAL_EXIT() return
#else
#define NORMAL_EXIT() exit(0)
#endif

/* The 4.2 bsd vax compiler has a bug which forces the following. */

#define pseudo_void int

char *
xmalloc (length)
     int length;
{
  char * result;
  extern char * malloc ();

  result = (malloc (length));
  if (result == NULL)
    {
      fprintf (stderr, "malloc: unable to allocate %d bytes\n", length);
      exit (1);
    }
  return (result);
}

char *
xrealloc (ptr, length)
     char * ptr;
     int length;
{
  char * result;
  extern char * realloc ();

  result = (realloc (ptr, length));
  if (result == NULL)
    {
      fprintf (stderr, "realloc: unable to allocate %d bytes\n", length);
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
#define dprintf(one, two) fprintf(stderr, one, two)
#else
#define dprintf(one, two)
#endif

/* Maximum number of primitives that can be handled. */

boolean built_in_p;

char * token_array [4];
char default_token [] = "Define_Primitive";
char default_token_alternate [] = "DEFINE_PRIMITIVE";
char built_in_token [] = "Built_In_Primitive";
char external_token [] = "Define_Primitive";

typedef pseudo_void (* TOKEN_PROCESSOR) ();
TOKEN_PROCESSOR token_processors [4];

char * the_kind;
char default_kind [] = "Primitive";
char built_in_kind [] = "Primitive";
char external_kind [] = "External";

char * the_variable;
char default_variable [] = "MAX_PRIMITIVE";
char built_in_variable [] = "MAX_PRIMITIVE";
char external_variable [] = "MAX_EXTERNAL_PRIMITIVE";

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
  {"Prim_inexistent", "0", "INEXISTENT-PRIMITIVE", "", "Findprim.c"};

char inexistent_error_string [] =
  "signal_error_from_primitive (ERR_UNIMPLEMENTED_PRIMITIVE)";

/* forward references */

TOKEN_PROCESSOR scan ();
boolean whitespace ();
int compare_descriptors ();
int read_index ();
pseudo_void create_alternate_entry ();
pseudo_void create_builtin_entry ();
pseudo_void create_normal_entry ();
void dump ();
void grow_data_buffer ();
void grow_token_buffer ();
void initialize_builtin ();
void initialize_data_buffer ();
void initialize_default ();
void initialize_external ();
void initialize_token_buffer ();
void mergesort ();
void print_procedure ();
void print_primitives ();
void print_spaces ();
void print_entry ();
void process ();
void process_argument ();
void scan_to_token_start ();
void skip_token ();
void sort ();
void update_from_entry ();

void
main (argc, argv)
     int argc;
     char * argv [];
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
      dump (FALSE);
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
	  dump (TRUE);
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
  dump (TRUE);

 done:
  if (output != stdout)
    fclose (output);
  NORMAL_EXIT ();
}

void
process_argument (fn)
    char * fn;
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
      dump (TRUE);
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
process ()
{
  TOKEN_PROCESSOR processor;

  while (TRUE)
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
scan ()
{
  register int c;
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
	      while (TRUE)
		{
		  while (c != '*')
		    {
		      if (c == EOF)
			{
			  fprintf (stderr,
				   "Error: EOF in comment in file %s, or %s confused\n",
				   file_name, name);
			  dump (TRUE);
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
	      register char * scan_buffer;

	      scan_buffer = (& (compare_buffer [0]));
	      while (TRUE)
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
	      register char **scan_tokens;

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
dump (check)
     boolean check;
{
  register int max_index;
  register int count;

  FIND_INDEX_LENGTH (buffer_index, max_index_length);
  max_index = (buffer_index - 1);

  /* Print header. */
  fprintf (output, "/%c Emacs: This is -*- C -*- code. %c/\n\n", '*', '*');
  fprintf (output, "/%c %s primitive declarations %c/\n\n",
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

      /* C does not understand the empty array, thus it must be faked. */
      fprintf (output, "/%c C does not understand the empty array, ", '*');
      fprintf (output, "thus it must be faked. %c/\n\n", '*');

      /* Dummy entry */
      (result_buffer [0]) = (& dummy_entry);
      update_from_entry (& dummy_entry);
      print_procedure (output, (& dummy_entry), (& (dummy_error_string [0])));
      fprintf (output, "\n");
    }
  else
    {
      /* Print declarations. */
      fprintf (output, "extern Pointer\n");
      for (count = 0; (count < max_index); count += 1)
	fprintf (output, "       %s (),\n",
		 (((* data_buffer) [count]) . c_name));
      fprintf (output, "       %s ();\n\n",
	       (((* data_buffer) [max_index]) . c_name));
    }

  print_procedure
    (output, (& inexistent_entry), (& (inexistent_error_string [0])));
  print_primitives (output, buffer_index);
  return;
}

void
print_procedure (output, primitive_descriptor, error_string)
     FILE * output;
     struct descriptor * primitive_descriptor;
     char * error_string;
{
  fprintf (output, "Pointer\n");
  fprintf (output, "%s ()\n", (primitive_descriptor -> c_name));
  fprintf (output, "{\n");
  fprintf (output, "  PRIMITIVE_HEADER (%s);\n",
	   (primitive_descriptor -> arity));
  fprintf (output, "\n");
  fprintf (output, "  %s;\n", error_string);
  fprintf (output, "  /%cNOTREACHED%c/\n", '*', '*');
  fprintf (output, "}\n");
  return;
}

#define TABLE_NEWLINE()							\
{									\
  if (count != last)							\
    fprintf (output, ",\n");						\
  else									\
    fprintf (output, "\n};\n");						\
}

void
print_primitives (output, limit)
     FILE * output;
     register int limit;
{
  register int last;
  register int count;
  register char * table_entry;

  last = (limit - 1);

  /* Print the procedure table. */
  fprintf (output, "\f\nPointer (* (%s_Procedure_Table [])) () = {\n",
	   the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      print_entry (output, count, (result_buffer [count]));
      fprintf (output, ",\n");
    }
  print_entry (output, (-1), (& inexistent_entry));
  fprintf (output, "\n};\n");

  /* Print the names table. */
  fprintf (output, "\f\nchar * %s_Name_Table [] = {\n", the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      fprintf (output, "  \"%s\"", ((result_buffer [count]) -> scheme_name));
      TABLE_NEWLINE ();
    }

  /* Print the documentation table. */
  fprintf (output, "\f\nchar * %s_Documentation_Table [] = {\n", the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      fprintf (output, "  ");
      table_entry = ((result_buffer [count]) -> documentation);
      if ((table_entry [0]) == '\0')
	fprintf (output, "((char *) 0)");
      else
	fprintf (output, "\"%s\"", table_entry);
      TABLE_NEWLINE ();
    }

  /* Print the arity table. */
  fprintf (output, "\f\nint %s_Arity_Table [] = {\n", the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      fprintf (output, "  %s", ((result_buffer [count]) -> arity));
      TABLE_NEWLINE ();
    }

  /* Print the counts table. */
  fprintf (output, "\f\nint %s_Count_Table [] = {\n", the_kind);
  for (count = 0; (count < limit); count += 1)
    {
      fprintf (output,
	       "  (%s * sizeof(Pointer))",
	       ((result_buffer [count]) -> arity));
      TABLE_NEWLINE ();
    }

  return;
}

void
print_entry (output, index, primitive_descriptor)
     FILE * output;
     int index;
     struct descriptor * primitive_descriptor;
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
print_spaces (output, how_many)
     FILE * output;
     register int how_many;
{
  while ((--how_many) >= 0)
    putc (' ', output);
  return;
}

/* Input Parsing */

char * token_buffer;
int token_buffer_length;

void
initialize_token_buffer ()
{
  token_buffer_length = 80;
  token_buffer = (xmalloc (token_buffer_length));
  return;
}

void
grow_token_buffer ()
{
  token_buffer_length *= 2;
  token_buffer = (xrealloc (token_buffer, token_buffer_length));
  return;
}

#define TOKEN_BUFFER_DECLS()						\
  register char * TOKEN_BUFFER_scan;					\
  register char * TOKEN_BUFFER_end

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
copy_token (target, size, token_type)
     char ** target;
     int * size;
     register enum tokentype token_type;
{
  register int c;
  TOKEN_BUFFER_DECLS ();

  TOKEN_BUFFER_START ();
  c = (getc (input));
  if (c == '\"')
    {
      while (1)
	{
	  c = (getc (input));
	  if (c == '\"') break;
	  TOKEN_BUFFER_WRITE
	    ((c == '\\')
	     ? (getc (input))
	     : (((token_type == tokentype_string_upcase) &&
		 (isalpha (c)) &&
		 (islower (c)))
		? (toupper (c))
		: c));
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
	  TOKEN_BUFFER_OVERWRITE ("-1");
	}
      else if ((token_type == tokentype_string) &&
	       ((strcmp (token_buffer, "0")) == 0))
	TOKEN_BUFFER_OVERWRITE ("");
    }
  TOKEN_BUFFER_FINISH ((* target), (* size));
  return;
}

boolean
whitespace (c)
     register int c;
{
  switch (c)
    {
    case ' ':
    case '\t':
    case '\n':  
    case '(':
    case ')':
    case ',': return TRUE;
    default: return FALSE;
    }
}

void
scan_to_token_start ()
{
  register int c;

  while (whitespace (c = (getc (input)))) ;
  ungetc (c, input);
  return;
}

void
skip_token ()
{
  register int c;

  while (! (whitespace (c = (getc (input))))) ;
  ungetc (c, input);
  return;
}

void
initialize_data_buffer ()
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
grow_data_buffer ()
{
  buffer_length *= 2;
  data_buffer =
    ((struct descriptor (*) [])
     (xrealloc (data_buffer, (buffer_length * (sizeof (struct descriptor))))));
  result_buffer =
    ((struct descriptor **)
     (xrealloc (result_buffer,
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
initialize_default ()
{
  built_in_p = FALSE;
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
initialize_external ()
{
  built_in_p = FALSE;
  (token_array [0]) = (& (external_token [0]));
  (token_array [1]) = NULL;
  (token_processors [0]) = create_normal_entry;
  (token_processors [1]) = NULL;
  the_kind = (& (external_kind [0]));
  the_variable = (& (external_variable [0]));
  return;
}

void
initialize_builtin (arg)
     char * arg;
{
  register int length;
  register int index;

  built_in_p = TRUE;
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
update_from_entry (primitive_descriptor)
     register struct descriptor * primitive_descriptor;
{
  register int temp;

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
create_normal_entry ()
{
  MAYBE_GROW_BUFFER ();
  COPY_C_NAME ((* data_buffer) [buffer_index]);
  COPY_ARITY ((* data_buffer) [buffer_index]);
  COPY_SCHEME_NAME ((* data_buffer) [buffer_index]);
  DEFAULT_DOCUMENTATION ((* data_buffer) [buffer_index]);
  COPY_FILE_NAME ((* data_buffer) [buffer_index]);
  (result_buffer [buffer_index]) = (& ((* data_buffer) [buffer_index]));
  buffer_index += 1;
  return;
}

pseudo_void
create_alternate_entry ()
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
  return;
}

pseudo_void
create_builtin_entry ()
{
  struct descriptor desc;
  register int length;
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
      register int i;

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
  return;
}

int
read_index (arg, identification)
     char * arg;
     char * identification;
{
  int result;

  result = 0;
  if (((arg [0]) == '0') && ((arg [1]) == 'x'))
    sscanf ((& (arg [2])), "%x", (& result));
  else
    sscanf ((& (arg [0])), "%d", (& result));
  if (result < 0)
    {
      fprintf (stderr, "%s: %s == %d\n", identification, result);
      exit (1);
    }
  return (result);
}

/* Sorting */

void
sort ()
{
  register struct descriptor ** temp_buffer;
  register int count;

  if (buffer_index <= 0)
    return;
  temp_buffer =
    ((struct descriptor **)
     (xmalloc (buffer_index * (sizeof (struct descriptor *)))));
  for (count = 0; (count < buffer_index); count += 1)
    (temp_buffer [count]) = (result_buffer [count]);
  mergesort (0, (buffer_index - 1), result_buffer, temp_buffer);
  free (temp_buffer);
  return;
}

void
mergesort (low, high, array, temp_array)
     int low;
     register int high;
     register struct descriptor ** array;
     register struct descriptor ** temp_array;
{
  register int index;
  register int low1;
  register int low2;
  int high1;
  int high2;

  dprintf ("mergesort: low = %d", low);
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

  mergesort (low, high1, temp_array, array);
  mergesort (low2, high, temp_array, array);

  dprintf ("mergesort: low1 = %d", low1);
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
  return;
}

int
compare_descriptors (d1, d2)
     struct descriptor * d1;
     struct descriptor * d2;
{
  int value;

  dprintf ("comparing \"%s\"", (d1 -> scheme_name));
  dprintf(" and \"%s\".\n", (d2 -> scheme_name));
  value = (strcmp ((d1 -> scheme_name), (d2 -> scheme_name)));
  if (value > 0)
    return (1);
  else if (value < 0)
    return (-1);
  else
    return (0);
}
