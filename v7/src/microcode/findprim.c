/* Emacs, please use -*-C-*- mode */

/****************************************************************
*                                                               *
*                         Copyright (c) 1985                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: Findprim.c
 *
 * Preprocessor to find and declare user defined primitives.
 *
 * Searches for a token which is a macro defined in primitive.h.
 * For each macro invocation it creates an entry in the External
 * Primitives descriptor used by Scheme.  The entry consists of
 * the C routine implementing the primitive, the (fixed) number of
 * arguments it requires, and the name Scheme uses to refer to it.
 *
 * The output is a C source file (on stdout, must be redirected)
 * to be compiled and linked with the Scheme microcode.
*/

/* In the following some output lines are done in a strange fashion
 * because some C compilers (the vms C compiler, for example) remove
 * comments even from within string quotes!!
 */

static char The_Token[] = "Define_Primitive";

/* Maximum number of primitives that can be handled. */

#ifndef BUFFER_SIZE
#define BUFFER_SIZE 200
#endif

#include <stdio.h>

/* For macros toupper, isalpha, etc, supposedly on the standard library */
#include <ctype.h>

#ifdef vax
#ifdef vms
#define normal_exit() return
#else	/* Vax, but not a VMS */
#define normal_exit() exit(0)
#include <strings.h>
#endif
#else	/* Not a Vax */
#define normal_exit() exit(0)
#endif

#define TRUE 1
#define FALSE 0

typedef int boolean;

#ifdef DEBUGGING
#define dprintf(one, two) fprintf(stderr, one, two)
#else
#define dprintf(one, two)
#endif

static FILE *input, *output;
static char *name;
static char *file_name;

#define error_exit(do_it) { if (do_it) dump(TRUE); exit(1); }

main(argc, argv)
int argc;
char *argv[];
{  FILE *fopen();

   name = argv[0];

   /* Check for specified output file */

   if ((argc >= 2) && (strcmp("-o", argv[1])==0))
   { if ((output = fopen(argv[2], "w")) == NULL)
     { fprintf(stderr, "Error: %s can't open %s\n", name, argv[2]);
       error_exit(FALSE);
     }
     argv += 2;
     argc -= 2;
   }
   else output = stdout;
     
   if (argc == 1)
     { dump(FALSE);
       normal_exit();
     }

   while (--argc > 0)
   { file_name = *++argv;
     if (strcmp("-", file_name)==0)
     { input = stdin;
       file_name = "stdin";
       dprintf("About to process %s\n", "STDIN");
       process();
     }
     else if ((input = fopen(file_name, "r")) == NULL)
       { fprintf(stderr, "Error: %s can't open %s\n", name, file_name);
	 error_exit(TRUE);
       }
     else 
       { dprintf("About to process %s\n", file_name);
         process();
	 fclose(input);
       }
   }
   dprintf("About to sort %s\n", "");
   sort();
   dprintf("About to dump %s\n", "");
   dump(TRUE);
   if (output != stdout) fclose(output);
   normal_exit();
 }

#define DONE 0
#define FOUND 1

/* Search for tokens and when found, create primitive entries. */

process()
{ while ((scan() != DONE))
  { dprintf("Process: place found.%s\n", "");
    create_entry();
  }
}

/* Search for token and stop when found.  If you hit open comment
 * character, read until you hit close comment character.
 * FIX: It is not a complete C parser, thus it may be fooled,
 *      currently the token must always begin a line.
*/

scan()
{ register char c, *temp;

  c = '\n';
  while(c != EOF)
  {
    switch(c)
    { case '/':
	if ((c = getc(input))  == '*')
	{ c = getc(input);
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
	temp = &The_Token[0];
	while ((c = getc(input)) == *temp++) {}
	if (temp[-1] == '\0') return FOUND;
	ungetc(c, input);
	break;

      default: {}
    }
    c = getc(input);
  }
  return DONE;
}

#define STRING_SIZE  80
#define ARITY_SIZE    6

typedef struct dsc
{ char C_Name[STRING_SIZE];		/* The C name of the function */
  char Arity[ARITY_SIZE];         	/* Number of arguments */
  char Scheme_Name[STRING_SIZE];	/* Scheme name of the primitive */
} descriptor;

/* FIX: This should really be malloced incrementally,
 * but for the time being ... */

descriptor Data_Buffer[BUFFER_SIZE]; /* New Primitives Allowed */
static int buffer_index = 0;

static int C_Size = 0;
static int A_Size = 0;
static int S_Size = 0;

#define DONT_CAP FALSE
#define DO_CAP TRUE

create_entry()
{ if (buffer_index >= BUFFER_SIZE)
  { fprintf(stderr, "Error: %s cannot handle so many primitives.\n", name);
    fprintf(stderr, "Recompile %s with BUFFER_SIZE larger than %d.\n",
	    name, BUFFER_SIZE);
    error_exit(FALSE);
  }
  scan_to_token_start();
  copy_token((Data_Buffer[buffer_index]).C_Name, DONT_CAP, &C_Size);
  scan_to_token_start();
  copy_token((Data_Buffer[buffer_index]).Arity, DONT_CAP, &A_Size);
  scan_to_token_start();
  copy_token((Data_Buffer[buffer_index]).Scheme_Name, DO_CAP, &S_Size);
  buffer_index++;
}

scan_to_token_start()
{ char c;
  while (whitespace(c = getc(input))) {};
  ungetc(c, input);
}

/* FIX: This should check for field overflow (n too small) */

copy_token(s, cap, Size)
char s[];
boolean cap;
int *Size;
{ register char c;
  register int n = 0;
  while (!(whitespace(c = getc(input))))
    s[n++] = ((cap && isalpha(c) && islower(c))? toupper(c) : c);
  s[n] = '\0';
  if (n > *Size) *Size = n;
}

whitespace(c)
char c;
{ switch(c)
  { case ' ':
    case '(':
    case ')':
    case ',': return TRUE;
    default: return FALSE;
  }
}

/* FIX: No-op for now */

sort()
{ return FALSE;
}

print_spaces(how_many)
register int how_many;
{ for(; --how_many >= 0;) putc(' ', output);
}

#define print_entry(index)					\
fprintf(output, "  %s,", (Data_Buffer[index].C_Name));		\
print_spaces(1+							\
	     (C_Size-(strlen(Data_Buffer[index].C_Name)))+	\
	     (A_Size-(strlen(Data_Buffer[index].Arity))));	\
fprintf(output, "%s", (Data_Buffer[index]).Arity);		\
fprintf(output, ", %s", (Data_Buffer[index]).Scheme_Name);	\
print_spaces(S_Size-(strlen(Data_Buffer[index].Scheme_Name)));	\
fprintf(output, "  /%c External %d %c/", '*', index, '*')

/* Produce C source. */

dump(check)
boolean check;
{ register int count;
  int max = buffer_index-1;

  /* Print header. */

  fprintf(output, "/%c User defined primitive declarations %c/\n\n",
	  '*', '*');
  fprintf(output, "#include \"scheme.h\"\n\n");

  if (max < 0)
  {
    if (check) fprintf(stderr, "No User primitives found!\n");

    /* C does not understand the empty array, thus it must be faked. */

    fprintf(output, "/%c C does not understand the empty array, ", '*');
    fprintf(output, "thus it must be faked. %c/\n\n", '*');

    /* Dummy entry */

    fprintf(output, "Pointer Dummy_Primitive()\n");
    fprintf(output, "{ /%c This should NEVER be called. %c/\n", '*', '*');
    fprintf(output, "  Microcode_Termination(TERM_BAD_PRIMITIVE);\n");
    fprintf(output, "}\n\n");

    /* Array with Dummy entry */

    fprintf(output, "External_Descriptor Ext_Prim_Desc[] = {\n");
    fprintf(output, "  Dummy_Primitive, 0, \"DUMMY-PRIMITIVE\"\n");
    fprintf(output, "};\n\n");
  }
  else
  {
  /* Print extern declarations. */

    fprintf(output, "extern Pointer\n");
    for (count = 0; count < max; count++)
      fprintf(output, "       %s(),\n", Data_Buffer[count].C_Name);
    fprintf(output, "       %s();\n\n", Data_Buffer[max].C_Name);

  /* Print structure. */

    fprintf(output, "External_Descriptor Ext_Prim_Desc[] = {\n");

    for (count = 0; count < max; count++)
    { print_entry(count);
      fprintf(output, ",\n");
    }
    print_entry(max);
  
    fprintf(output, "\n};\n\n");
  }

  fprintf(output, "long MAX_EXTERNAL_PRIMITIVE = %d;\n\n", max);
  return;
}
