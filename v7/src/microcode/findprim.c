/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/findprim.c,v 9.33 1987/12/23 04:48:05 cph Rel $
 *
 * Preprocessor to find and declare defined primitives.
 *
 */

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
   supposedly on the standard library.
*/

#include <ctype.h>

extern int strcmp(), strlen();

typedef int boolean;
#define TRUE 1
#define FALSE 0

#ifdef vms
/* VMS version 3 has no void. */
/* #define void */
#define normal_exit() return
#else
#define normal_exit() exit(0)
#endif

/* The 4.2 bsd vax compiler has a bug which forces the following. */

#define pseudo_void	int

#define error_exit(do_it)						\
{									\
  if (do_it)								\
    dump(TRUE);								\
  exit(1);								\
}

void dump();

#ifdef DEBUGGING
#define dprintf(one, two) fprintf(stderr, one, two)
#else
#define dprintf(one, two)
#endif

/* Maximum number of primitives that can be handled. */

#ifndef BUFFER_SIZE
#define BUFFER_SIZE	0x400
#endif

static boolean Built_in_p;
static long Built_in_table_size;

static char *token_array[4];
static char Default_Token[] = "Define_Primitive";
static char default_token_alternate[] = "DEFINE_PRIMITIVE";
static char Built_in_Token[] = "Built_In_Primitive";
static char External_Token[] = "Define_Primitive";

typedef pseudo_void (*TOKEN_PROCESSOR) ();
static TOKEN_PROCESSOR token_processors[4];

static char *The_Kind;
static char Default_Kind[] = "Primitive";
static char Built_in_Kind[] = "Primitive";
static char External_Kind[] = "External";

static char *The_Variable;
static char Default_Variable[] = "MAX_PRIMITIVE";
static char Built_in_Variable[] = "MAX_PRIMITIVE";
static char External_Variable[] = "MAX_EXTERNAL_PRIMITIVE";

static FILE *input, *output;
static char *name;
static char *file_name;

main(argc, argv)
     int argc;
     char *argv[];
{
  void process_argument(), sort();
  FILE *fopen();

  name = argv[0];

  /* Check for specified output file */

  if ((argc >= 2) && (strcmp("-o", argv[1]) == 0))
  {
    if ((output = fopen(argv[2], "w")) == NULL)
    {
      fprintf(stderr, "Error: %s can't open %s\n", name, argv[2]);
      error_exit(FALSE);
    }
    argv += 2;
    argc -= 2;
  }
  else
    output = stdout;

  /* Check whether to produce the built-in table instead.
     The argument after the option letter is the size of the
     table to build.
   */

  if ((argc >= 2) && (strcmp("-b", argv[1]) == 0))
  {
    void initialize_builtin();

    initialize_builtin(argv[2]);
    argv += 2;
    argc -= 2;
  }
  else if ((argc >= 2) && (strcmp("-e", argv[1]) == 0))
  {
    void initialize_external();

    initialize_external();
  }
  else
  {
    void initialize_default();

    initialize_default();
  }

  /* Check whether there are any files left. */

  if (argc == 1)
  {
    dump(FALSE);
    normal_exit();
  }

  if ((argc >= 2) && (strcmp("-l", argv[1]) == 0))
  {
    /* The list of files is stored in another file. */

    char fn[100];
    FILE *file_list_file;

    if ((file_list_file = fopen(argv[2], "r")) == NULL)
    {
      fprintf(stderr, "Error: %s can't open %s\n", name, argv[2]);
      error_exit(TRUE);
    }
    else
    {
      while (fgets(fn, 100, file_list_file) != NULL)
      {
	int i;

	i = strlen(fn) - 1;
	if (i >=0 && fn[i] == '\n')
	{
	  fn[i] = '\0';
	  i--;
	}
	if (i > 0 && fn[0] != ';')
	  process_argument(fn);
      }
      fclose(file_list_file);
    }
  }
  else
  {
    /* The list of files is in the argument list. */

    while (--argc > 0)
    {
      process_argument(*++argv);
    }
  }
  if (!Built_in_p)
  {
    dprintf("About to sort %s\n", "");
    sort();
  }
  dprintf("About to dump %s\n", "");
  dump(TRUE);
  if (output != stdout)
  {
    fclose(output);
  }
  normal_exit();
}

void process_argument(fn)
    char *fn;
{
  void process();
  
  file_name = fn;
  if (strcmp("-", file_name)==0)
  {
    input = stdin;
    file_name = "stdin";
    dprintf("About to process %s\n", "STDIN");
    process();
  }
  else if ((input = fopen(file_name, "r")) == NULL)
  {
    fprintf(stderr, "Error: %s can't open %s\n", name, file_name);
    error_exit(TRUE);
  }
  else 
  {
    dprintf("About to process %s\n", file_name);
    process();
    fclose(input);
  }
}

/* Search for tokens and when found, create primitive entries. */

void
process()
{
  TOKEN_PROCESSOR scan();
  TOKEN_PROCESSOR processor;

  while (TRUE)
    {
      processor = (scan ());
      if (processor == NULL)
	break;
      dprintf("Process: place found.%s\n", "");
      (*processor)();
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
  char compare_buffer[1024];

  c = '\n';
  while(c != EOF)
  {
    switch(c)
    { case '/':
	if ((c = getc(input))  == '*')
	{
	  c = getc(input);
	  while (TRUE)
	  { while (c != '*')
	    { if (c == EOF)
	      { fprintf(stderr,
			"Error: EOF in comment in file %s, or %s confused\n",
			file_name, name);
		error_exit(TRUE);
	      }
	      c = getc(input);
	    }
	    if ((c = getc(input)) == '/') break;
	  }
	}
	else if (c != '\n') break;

      case '\n':
	{
	  {
	    register char *scan_buffer;

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
		 ((*scan_tokens) != NULL);
		 scan_tokens += 1)
	      if ((strcmp ((& (compare_buffer [0])), (*scan_tokens))) == 0)
		return (token_processors [(scan_tokens - token_array)]);
	  }
	  break;
	}

      default: {}
    }
    c = getc(input);
  }
  return (NULL);
}

boolean
whitespace(c)
     int c;
{
  switch(c)
  { case ' ':
    case '\t':
    case '\n':  
    case '(':
    case ')':
    case ',': return TRUE;
    default: return FALSE;
  }
}

void
scan_to_token_start()
{
  int c;

  while (whitespace(c = getc(input))) {};
  ungetc(c, input);
  return;
}

/* *** FIX *** This should check for field overflow (n too small) */

void
copy_token(s, size)
     char s[];
     int *size;
{
  register int c, n;

  n = 0;
  while (!(whitespace(c = getc(input))))
  {
    s[n++] = c;
  }
  s[n] = '\0';
  if (n > *size)
  {
    *size = n;
  }
  return;
}

void
copy_symbol(s, size)
     char s[];
     int *size;
{
  register int c, n;

  n = 0;
  c = getc(input);
  if (c != '\"')
  {
  }
  while ((!(whitespace(c = getc(input)))) && (c != '\"'))
  {
    s[n++] = ((isalpha(c) && islower(c)) ? toupper(c) : c);
  }
  s[n] = '\0';
  if (n > *size)
  {
    *size = n;
  }
  return;
}

void
copy_string(is, s, size)
     register char *is;
     char s[];
     int *size;
{
  register int c, n;

  n = 0;
  while ((c = *is++) != '\0')
  {
    s[n++] = c;
  }
  s[n] = '\0';
  if (n > *size)
  {
    *size = n;
  }
  return;
}

#define STRING_SIZE  80
#define ARITY_SIZE    6

typedef struct dsc
{
  char C_Name[STRING_SIZE];		/* The C name of the function */
  char Arity[ARITY_SIZE];         	/* Number of arguments */
  char Scheme_Name[STRING_SIZE];	/* Scheme name of the primitive */
  char File_Name[STRING_SIZE];		/* File where found. */
} descriptor;

/*
 * *** FIX ***
 * This should really be malloced incrementally, but for the time being ... 
 *
 */

static int buffer_index = 0;
descriptor Data_Buffer[BUFFER_SIZE];
descriptor *Result_Buffer[BUFFER_SIZE];
descriptor *Temp_Buffer[BUFFER_SIZE];

static descriptor Dummy_Entry =
{
  "Dummy_Primitive",
  "0",
  "DUMMY-PRIMITIVE",
  "Findprim.c"
};

static char Dummy_Error_String[] =
  "Microcode_Termination(TERM_BAD_PRIMITIVE)";

static descriptor Inexistent_Entry =
{
  "Prim_Inexistent",
  "0",
  "INEXISTENT-PRIMITIVE",
  "Findprim.c"
};

static char Inexistent_Error_String[] =
  "Primitive_Error(ERR_UNIMPLEMENTED_PRIMITIVE)";

static int C_Size = 0;
static int A_Size = 0;
static int S_Size = 0;
static int F_Size = 0;

void
update_from_entry(primitive_descriptor)
     descriptor *primitive_descriptor;
{
  int temp;
  temp = strlen(primitive_descriptor->C_Name);
  if (temp > C_Size)
  {
    C_Size = temp;
  }
  temp = strlen(primitive_descriptor->Arity);
  if (temp > A_Size)
  {
    A_Size = temp;
  }
  temp = strlen(primitive_descriptor->Scheme_Name);
  if (temp > S_Size)
  {
    S_Size = temp;
  }
  temp = strlen(primitive_descriptor->File_Name);
  if (temp > F_Size)
  {
    F_Size = temp;
  }
  return;
}

void
copy_arity_token (s, size)
     char s[];
     int *size;
{
  char buffer [ARITY_SIZE];
  int buffer_size;

  buffer_size = (*size);
  copy_token (buffer, (& buffer_size));
  if ((strcmp (buffer, "LEXPR")) == 0)
    {
      strcpy (buffer, "-1");
      buffer_size = 2;
    }
  strcpy (s, buffer);
  if ((*size) < buffer_size)
    (*size) = buffer_size;
  return;
}

pseudo_void
create_normal_entry()
{
  if (buffer_index >= BUFFER_SIZE)
  {
    fprintf(stderr, "Error: %s cannot handle so many primitives.\n", name);
    fprintf(stderr, "Recompile %s with BUFFER_SIZE larger than %d.\n",
	    name, BUFFER_SIZE);
    error_exit(FALSE);
  }
  scan_to_token_start();
  copy_token((Data_Buffer[buffer_index]).C_Name, &C_Size);
  scan_to_token_start();
  copy_arity_token((Data_Buffer[buffer_index]).Arity, &A_Size);
  scan_to_token_start();
  copy_symbol((Data_Buffer[buffer_index]).Scheme_Name, &S_Size);
  copy_string(file_name, (Data_Buffer[buffer_index]).File_Name, &F_Size);
  Result_Buffer[buffer_index] = &Data_Buffer[buffer_index];
  buffer_index++;
  return;
}

pseudo_void
create_alternate_entry()
{
  if (buffer_index >= BUFFER_SIZE)
  {
    fprintf(stderr, "Error: %s cannot handle so many primitives.\n", name);
    fprintf(stderr, "Recompile %s with BUFFER_SIZE larger than %d.\n",
	    name, BUFFER_SIZE);
    error_exit(FALSE);
  }
  scan_to_token_start();
  copy_symbol((Data_Buffer[buffer_index]).Scheme_Name, &S_Size);
  scan_to_token_start();
  copy_token((Data_Buffer[buffer_index]).C_Name, &C_Size);
  scan_to_token_start();
  copy_arity_token((Data_Buffer[buffer_index]).Arity, &A_Size);
  copy_string(file_name, (Data_Buffer[buffer_index]).File_Name, &F_Size);
  Result_Buffer[buffer_index] = &Data_Buffer[buffer_index];
  buffer_index++;
  return;
}

void
initialize_external()
{
  Built_in_p = FALSE;
  (token_array [0]) = &External_Token[0];
  (token_array [1]) = NULL;
  (token_processors [0]) = create_normal_entry;
  (token_processors [1]) = NULL;
  The_Kind = &External_Kind[0];
  The_Variable = &External_Variable[0];
  update_from_entry(&Inexistent_Entry);
  return;
}

void
initialize_default()
{
  Built_in_p = FALSE;
  (token_array [0]) = &Default_Token[0];
  (token_array [1]) = (& (default_token_alternate [0]));
  (token_array [2]) = NULL;
  (token_processors [0]) = create_normal_entry;
  (token_processors [1]) = create_alternate_entry;
  (token_processors [2]) = NULL;
  The_Kind = &Default_Kind[0];
  The_Variable = &Default_Variable[0];
  update_from_entry(&Inexistent_Entry);
  return;
}

int
read_index(arg)
     char *arg;
{
  int result = 0;

  if ((arg[0] == '0') && (arg[1] == 'x'))
    sscanf(&arg[2], "%x", &result);
  else
    sscanf(&arg[0], "%d", &result);
  return result;
}

pseudo_void
create_builtin_entry()
{
  static char index_buffer[STRING_SIZE];
  int index = 0;

  scan_to_token_start();
  copy_token((Data_Buffer[buffer_index]).C_Name, &C_Size);
  scan_to_token_start();
  copy_arity_token((Data_Buffer[buffer_index]).Arity, &A_Size);
  scan_to_token_start();
  copy_token((Data_Buffer[buffer_index]).Scheme_Name, &S_Size);
  copy_string(file_name, (Data_Buffer[buffer_index]).File_Name, &F_Size);
  scan_to_token_start();
  copy_token(index_buffer, &index);
  index = read_index(index_buffer);
  if (index >= Built_in_table_size)
  {
    fprintf(stderr, "%s: Table size = %d; Found Primitive %d.\n",
	    name, Built_in_table_size, index);
    error_exit(FALSE);
  }
  if (Result_Buffer[index] != &Inexistent_Entry)
  {
    void print_entry(), initialize_index_size();

    fprintf(stderr, "%s: redefinition of primitive %d.\n", name, index);
    fprintf(stderr, "previous definition:\n");
    initialize_index_size();
    output = stderr,
    print_entry(index, Result_Buffer[index]);
    fprintf(stderr, "\n");
    fprintf(stderr, "new definition:\n");
    print_entry(index, &Data_Buffer[buffer_index]);
    fprintf(stderr, "\n");
    error_exit(FALSE);
  }
  Result_Buffer[index] = &Data_Buffer[buffer_index];
  buffer_index++;
  return;
}

void
initialize_builtin(arg)
     char *arg;
{
  register int index;

  Built_in_p = TRUE;
  Built_in_table_size = read_index(arg);
  if (Built_in_table_size > BUFFER_SIZE)
  {
    fprintf(stderr, "%s: built_in_table_size > BUFFER_SIZE.\n", name);
    fprintf(stderr, "Recompile with a larger value of BUFFER_SIZE.\n");
    error_exit(FALSE);
  }
  (token_array [0]) = &Built_in_Token[0];
  (token_array [1]) = NULL;
  (token_processors [0]) = create_builtin_entry;
  (token_processors [1]) = NULL;
  The_Kind = &Built_in_Kind[0];
  The_Variable = &Built_in_Variable[0];
  for (index = Built_in_table_size; --index >= 0; )
  {
    Result_Buffer[index] = &Inexistent_Entry;
  }
  update_from_entry(&Inexistent_Entry);
  return;
}

int
compare_descriptors(d1, d2)
     descriptor *d1, *d2;
{
  int value;

  dprintf("comparing \"%s\"", d1->Scheme_Name);
  dprintf(" and \"%s\".\n", d2->Scheme_Name);
  value = strcmp(d1->Scheme_Name, d2->Scheme_Name);
  if (value > 0)
  {
    return 1;
  }
  else if (value < 0)
  {
    return -1;
  }
  else
  {
    return 0;
  }
}

void
mergesort(low, high, array, temp_array)
     int low;
     register int high;
     register descriptor **array, **temp_array;
{
  void print_entry(), initialize_index_size();
  register int index, low1, low2;
  int high1, high2;

  dprintf("mergesort: low = %d", low);
  dprintf("; high = %d", high);

  if (high <= low)
  {
    dprintf("; done.%s\n", "");
    return;
  }

  low1 = low;
  high1 = ((low + high) / 2);
  low2 = (high1 + 1);
  high2 = high;

  dprintf("; high1 = %d\n", high1);

  mergesort(low, high1, temp_array, array);
  mergesort(low2, high, temp_array, array);

  dprintf("mergesort: low1 = %d", low1);
  dprintf("; high1 = %d", high1);
  dprintf("; low2 = %d", low2);
  dprintf("; high2 = %d\n", high2);

  for (index = low; index <= high; index += 1)
  {
    dprintf("index = %d", index);
    dprintf("; low1 = %d", low1);
    dprintf("; low2 = %d\n", low2);

    if (low1 > high1)
    {
      array[index] = temp_array[low2];
      low2 += 1;
    }
    else if (low2 > high2)
    {
      array[index] = temp_array[low1];
      low1 += 1;
    }
    else
    {
      switch(compare_descriptors(temp_array[low1], temp_array[low2]))
      {
	case -1:
	  array[index] = temp_array[low1];
	  low1 += 1;
	  break;

	case 1:
	  array[index] = temp_array[low2];
	  low2 += 1;
	  break;

	default:
	  fprintf(stderr, "Error: bad comparison.\n");
	  goto comparison_abort;

	case 0:
	{
	  fprintf(stderr, "Error: repeated primitive.\n");
comparison_abort:
	  initialize_index_size();
	  output = stderr;
	  fprintf(stderr, "definition 1:\n");
	  print_entry(low1, temp_array[low1]);
	  fprintf(stderr, "\ndefinition 2:\n");
	  print_entry(low2, temp_array[low2]);
	  fprintf(stderr, "\n");
	  error_exit(FALSE);
	  break;
	}
      }
    }
  }
  return;
}

void
sort()
{
  register int count;
  if (buffer_index <= 0)
    return;
  
  for (count = (buffer_index - 1); count >= 0; count -= 1)
  {
    Temp_Buffer[count] = Result_Buffer[count];
  }
  mergesort(0, (buffer_index - 1), Result_Buffer, Temp_Buffer);
  return;
}

static int max, max_index_size;
static char index_buffer[STRING_SIZE];

#define find_index_size(index, size)					\
{									\
  sprintf(index_buffer, "%x", (index));					\
  size = strlen(index_buffer);						\
}

void
initialize_index_size()
{
  if (Built_in_p)
  {
    max = Built_in_table_size;
  }
  else
  {
    max = buffer_index;
  }
  find_index_size(max, max_index_size);
  max -= 1;
  return;
}

void
print_spaces(how_many)
     register int how_many;
{
  for(; --how_many >= 0;)
  {
    putc(' ', output);
  }
  return;
}

void
print_entry(index, primitive_descriptor)
     int index;
     descriptor *primitive_descriptor;
{
  int index_size;

  fprintf(output, "  %s ", (primitive_descriptor->C_Name));
  print_spaces(C_Size - (strlen(primitive_descriptor->C_Name)));
  fprintf(output, "/%c ", '*');
  print_spaces(A_Size - (strlen(primitive_descriptor->Arity)));
  fprintf(output,
	  "%s \"%s\"",
	  (primitive_descriptor->Arity),
	  (primitive_descriptor->Scheme_Name));
  print_spaces(S_Size-(strlen(primitive_descriptor->Scheme_Name)));
  fprintf(output, " %s ", The_Kind);
  if (index >= 0)
  {
    find_index_size(index, index_size);
    print_spaces(max_index_size - index_size);
    fprintf(output, "0x%x", index);
  }
  else
  {
    print_spaces(max_index_size - 1);
    fprintf(output, "???");
  }
  fprintf(output, " in %s %c/", (primitive_descriptor->File_Name), '*');
  return;
}

void
print_procedure(primitive_descriptor, error_string)
     descriptor *primitive_descriptor;
     char *error_string;
{
  fprintf(output, "Pointer\n");
  fprintf(output, "%s()\n", (primitive_descriptor->C_Name));
  fprintf(output, "{\n");
  fprintf(output, "  Primitive_%s_Args();\n", (primitive_descriptor->Arity));
  fprintf(output, "\n");
  fprintf(output, "  %s;\n", error_string);
  fprintf(output, "  /%cNOTREACHED%c/\n", '*', '*');
  fprintf(output, "}\n");
  return;
}

void
print_primitives(last)
     register int last;
{

  register int count;

  /* Print the procedure table. */

  fprintf(output, "Pointer (*(%s_Procedure_Table[]))() = {\n", The_Kind);

  for (count = 0; count <= last; count++)
  {
    print_entry(count, Result_Buffer[count]);
    fprintf(output, ",\n");
  }
  print_entry(-1, &Inexistent_Entry);
  fprintf(output, "\n};\n\f\n");

  /* Print the names table. */
  
  fprintf(output, "char *%s_Name_Table[] = {\n", The_Kind);

  for (count = 0; count < last; count++)
  {
    fprintf(output, "  \"%s\",\n", ((Result_Buffer[count])->Scheme_Name));
  }
  fprintf(output, "  \"%s\"\n", ((Result_Buffer[last])->Scheme_Name));
  fprintf(output, "};\n\f\n");

  /* Print the arity table. */
  
  fprintf(output, "int %s_Arity_Table[] = {\n", The_Kind);

  for (count = 0; count < last; count++)
  {
    fprintf(output, "  %s,\n", ((Result_Buffer[count])->Arity));
  }
  fprintf(output, "  %s\n", ((Result_Buffer[last])->Arity));
  fprintf(output, "};\n\f\n");

  /* Print the counts table. */
  
  fprintf(output, "int %s_Count_Table[] = {\n", The_Kind);

  for (count = 0; count < last; count++)
  {
    fprintf(output,
	    "  (%s * sizeof(Pointer)),\n",
	    ((Result_Buffer[count])->Arity));
  }
  fprintf(output,
	  "  (%s * sizeof(Pointer))\n",
	  ((Result_Buffer[last])->Arity));
  fprintf(output, "};\n\n");

  return;
}

/* Produce C source. */

void
dump(check)
     boolean check;
{
  register int count, end;

  initialize_index_size();

  /* Print header. */

  fprintf(output, "/%c Emacs: This is -*- C -*- code. %c/\n\n", '*', '*');

  fprintf(output, "/%c %s primitive declarations %c/\n\n",
	  '*', ((Built_in_p) ? "Built in" : "User defined" ), '*');

  fprintf(output, "#include \"usrdef.h\"\n\n");

  fprintf(output,
	  "long %s = %d; /%c = 0x%x %c/\n\n",
	  The_Variable, max, '*', max, '*');

  if (Built_in_p)
  {
    fprintf(output,
	    "/%c The number of implemented primitives is %d. %c/\n\n",
	    '*', buffer_index, '*');
  }

  if (max < 0)
  {
    if (check)
    {
      fprintf(stderr, "No primitives found!\n");
    }

    /* C does not understand the empty array, thus it must be faked. */

    fprintf(output, "/%c C does not understand the empty array, ", '*');
    fprintf(output, "thus it must be faked. %c/\n\n", '*');

    /* Dummy entry */

    Result_Buffer[0] = &Dummy_Entry;
    update_from_entry(&Dummy_Entry);
    print_procedure(&Dummy_Entry, &Dummy_Error_String[0]);
    fprintf(output, "\n");
  }

  else
  {
    /* Print declarations. */

    fprintf(output, "extern Pointer\n");

    end = (Built_in_p ? buffer_index : max);
    for (count = 0; count < end; count++)
    {
      fprintf(output, "       %s(),\n", &(Data_Buffer[count].C_Name)[0]);
    }

    fprintf(output, "       %s();\n\n", &(Data_Buffer[end].C_Name)[0]);
  }

  print_procedure(&Inexistent_Entry, &Inexistent_Error_String[0]);
  fprintf(output, "\f\n");
  print_primitives((max < 0) ? 0 : max);
  return;
}
