/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/debug.c,v 9.43 1992/02/04 04:14:43 jinx Exp $

Copyright (c) 1987-1992 Massachusetts Institute of Technology

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

/* Utilities to help with debugging */

#include "scheme.h"
#include "prims.h"
#include "trap.h"
#include "lookup.h"

static void EXFUN (do_printing, (FILE *, SCHEME_OBJECT, Boolean));
static Boolean EXFUN (print_primitive_name, (FILE *, SCHEME_OBJECT));
static void EXFUN (print_expression, (FILE *, SCHEME_OBJECT, char *));

/* Compiled Code Debugging */

static SCHEME_OBJECT
DEFUN (compiled_block_debug_filename, (block), SCHEME_OBJECT block)
{
  extern SCHEME_OBJECT EXFUN (compiled_block_debugging_info, (SCHEME_OBJECT));
  SCHEME_OBJECT info;

  info = (compiled_block_debugging_info (block));
  return
    (((STRING_P (info)) ||
      ((PAIR_P (info)) &&
       (STRING_P (PAIR_CAR (info))) &&
       (FIXNUM_P (PAIR_CDR (info)))))
     ? info
     : SHARP_F);
}

extern void
  EXFUN (compiled_entry_type, (SCHEME_OBJECT, long *));

extern long
  EXFUN (compiled_entry_closure_p, (SCHEME_OBJECT)),
  EXFUN (compiled_entry_to_block_offset, (SCHEME_OBJECT));

extern SCHEME_OBJECT
  * EXFUN (compiled_entry_to_block_address, (SCHEME_OBJECT)),
  EXFUN (compiled_closure_to_entry, (SCHEME_OBJECT));

#define COMPILED_ENTRY_TO_BLOCK(entry)					\
(MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK,				\
		      (compiled_entry_to_block_address (entry))))

static SCHEME_OBJECT
DEFUN (compiled_entry_debug_filename, (entry), SCHEME_OBJECT entry)
{
  long results [3];

  compiled_entry_type (entry, (& (results [0])));
  if (((results [0]) == 0) && (compiled_entry_closure_p (entry)))
    entry = (compiled_closure_to_entry (entry));
  return (compiled_block_debug_filename (COMPILED_ENTRY_TO_BLOCK (entry)));
}

char *
DEFUN (compiled_entry_filename, (entry), SCHEME_OBJECT entry)
{
  SCHEME_OBJECT result;

  result = (compiled_entry_debug_filename (entry));
  if (STRING_P (result))
    return ((char *) (STRING_LOC ((result), 0)));
  else if (PAIR_P (result))
    return ((char *) (STRING_LOC ((PAIR_CAR (result)), 0)));
  else
    return ("**** filename not known ****");
}

void
DEFUN_VOID (Show_Pure)
{
  SCHEME_OBJECT *Obj_Address;
  long Pure_Size, Total_Size;

  Obj_Address = Constant_Space;
  while (true)
  {
    if (Obj_Address > Free_Constant)
    {
      printf ("Past end of area.\n");
      return;
    }
    if (Obj_Address == Free_Constant)
    {
      printf ("Done.\n");
      return;
    }
    Pure_Size = OBJECT_DATUM (*Obj_Address);
    Total_Size = OBJECT_DATUM (Obj_Address[1]);
    printf ("0x%lx: pure=0x%lx, total=0x%lx\n",
	    ((long) Obj_Address), ((long) Pure_Size), ((long) Total_Size));
    if (OBJECT_TYPE (*Obj_Address) != TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      printf ("Missing initial SNMV.\n");
      return;
    }
    if (OBJECT_TYPE (Obj_Address[1]) != PURE_PART)
    {
      printf ("Missing subsequent pure header.\n");
    }
    if (OBJECT_TYPE (Obj_Address[Pure_Size-1]) !=
        TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      printf ("Missing internal SNMV.\n");
      return;
    }
    if (OBJECT_TYPE (Obj_Address[Pure_Size]) != CONSTANT_PART)
    {
      printf ("Missing constant header.\n");
      return;
    }
    if (OBJECT_DATUM (Obj_Address[Pure_Size]) != Pure_Size)
    {
      printf ("Pure size mismatch 0x%lx.\n",
	      ((long) (OBJECT_DATUM (Obj_Address[Pure_Size]))));
    }
    if (OBJECT_TYPE (Obj_Address[Total_Size-1]) !=
        TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      printf ("Missing ending SNMV.\n");
      return;
    }
    if (OBJECT_TYPE (Obj_Address[Total_Size]) != END_OF_BLOCK)
    {
      printf ("Missing ending header.\n");
      return;
    }
    if (OBJECT_DATUM (Obj_Address[Total_Size]) != Total_Size)
    {
      printf ("Total size mismatch 0x%lx.\n",
	      ((long) (OBJECT_DATUM (Obj_Address[Total_Size]))));
    }
    Obj_Address += Total_Size+1;
#ifdef FLOATING_ALIGNMENT
    while (*Obj_Address == MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0))
    {
      Obj_Address += 1;
    }
#endif
  }
}

void
DEFUN (Show_Env, (The_Env), SCHEME_OBJECT The_Env)
{
  SCHEME_OBJECT *name_ptr, procedure, *value_ptr, extension;
  long count, i;

  procedure = MEMORY_REF (The_Env, ENVIRONMENT_FUNCTION);
  value_ptr = MEMORY_LOC (The_Env, ENVIRONMENT_FIRST_ARG);

  if (OBJECT_TYPE (procedure) == AUX_LIST_TYPE)
  {
    extension = procedure;
    procedure = FAST_MEMORY_REF (extension, ENV_EXTENSION_PROCEDURE);
  }
  else
    extension = SHARP_F;

  if ((OBJECT_TYPE (procedure) != TC_PROCEDURE) &&
      (OBJECT_TYPE (procedure) != TC_EXTENDED_PROCEDURE))
  {
    printf ("Not created by a procedure");
    return;
  }
  name_ptr = MEMORY_LOC (procedure, PROCEDURE_LAMBDA_EXPR);
  name_ptr = MEMORY_LOC (*name_ptr, LAMBDA_FORMALS);
  count = VECTOR_LENGTH (*name_ptr) - 1;

  name_ptr = MEMORY_LOC (*name_ptr, 2);
  for (i = 0; i < count; i++)
  {
    Print_Expression (*name_ptr++, "Name ");
    Print_Expression (*value_ptr++, " Value ");
    printf ("\n");
  }
  if (extension != SHARP_F)
  {
    printf ("Auxilliary Variables\n");
    count = OBJECT_DATUM (MEMORY_REF (extension, AUX_LIST_COUNT));
    for (i = 0, name_ptr = MEMORY_LOC (extension, AUX_LIST_FIRST);
	 i < count;
	 i++, name_ptr++)
    {
      Print_Expression ((PAIR_CAR (*name_ptr)), "Name ");
      Print_Expression ((PAIR_CDR (*name_ptr)), " Value ");
      printf ("\n");
    }
  }
}

static void
DEFUN (print_list, (stream, pair), FILE * stream AND SCHEME_OBJECT pair)
{
  int count;

  fprintf (stream, "(");
  count = 0;
  while (((PAIR_P (pair)) || (WEAK_PAIR_P (pair))) && (count < MAX_LIST_PRINT))
    {
      if (count > 0)
	fprintf (stream, " ");
      print_expression (stream,
			(PAIR_CAR (pair)),
			((WEAK_PAIR_P (pair)) ? "{weak}" : ""));
      pair = (PAIR_CDR (pair));
      count += 1;
    }
  if (pair != EMPTY_LIST)
    {
      if (count == MAX_LIST_PRINT)
	fprintf (stream, " ...");
      else
	{
	  fprintf (stream, " . ");
	  print_expression (stream, pair, "");
	}
    }
  fprintf (stream, ")");
  return;
}

static void
DEFUN (print_return_name, (stream, Ptr), FILE * stream AND SCHEME_OBJECT Ptr)
{
  long index;
  char * name;

  index = (OBJECT_DATUM (Ptr));
  if (index <= MAX_RETURN)
    {
      name = (Return_Names [index]);
      if ((name != ((char *) 0)) &&
	  ((name [0]) != '\0'))
	{
	  fprintf (stream, "%s", name);
	  return;
	}
    }
  fprintf (stream, "[0x%lx]", index);
  return;
}

void
DEFUN (Print_Return, (String), char * String)
{
  printf ("%s: ", String);
  print_return_name (stdout, Fetch_Return ());
  printf ("\n");
}

static void
DEFUN (print_string, (stream, string), FILE * stream AND SCHEME_OBJECT string)
{
  long length;
  long i;
  char * next;
  char this;

  fprintf (stream, "\"");
  length = (STRING_LENGTH (string));
  next = ((char *) (STRING_LOC (string, 0)));
  for (i = 0; (i < length); i += 1)
    {
      this = (*next++);
      switch (this)
	{
	case '\\':
	  fprintf (stream, "\\\\");
	  break;
	case '"':
	  fprintf (stream, "\\\"");
	  break;
	case '\t':
	  fprintf (stream, "\\t");
	  break;
	case '\n':
	  fprintf (stream, "\\n");
	  break;
	case '\f':
	  fprintf (stream, "\\f");
	  break;
	default:
	  if ((this >= ' ') && (this <= '~'))
	    putc (this, stream);
	  else
	    fprintf (stream, "\\%03o", this);
	  break;
	}
    }
  fprintf (stream, "\"");
  return;
}

static void
DEFUN (print_symbol, (stream, symbol), FILE * stream AND SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT string;
  long length;
  long i;
  char * next;

  string = (MEMORY_REF (symbol, SYMBOL_NAME));
  length = (STRING_LENGTH (string));
  next = ((char *) (STRING_LOC (string, 0)));
  for (i = 0; (i < length); i += 1)
    putc (*next++, stream);
  return;
}

static void
DEFUN (print_filename, (stream, filename),
       FILE * stream AND SCHEME_OBJECT filename)
{
  long length;
  char * scan;
  char * end;
  char * slash;

  length = (STRING_LENGTH (filename));
  scan = ((char *) (STRING_LOC (filename, 0)));
  end = (scan + length);
  slash = scan;
  while (scan < end)
    if ((*scan++) == '/')
      slash = scan;
  fprintf (stream, "\"%s\"", slash);
  return;
}

static void
DEFUN (print_object, (object), SCHEME_OBJECT object)
{
  do_printing (stdout, object, true);
  printf ("\n");
  fflush (stdout);
  return;
}

DEFINE_PRIMITIVE ("DEBUGGING-PRINTER", Prim_debugging_printer, 1, 1,
  "A cheap, built-in printer intended for debugging the interpreter.")
{
  PRIMITIVE_HEADER (1);

  print_object (ARG_REF (1));
  return (SHARP_F);
}

static void
DEFUN (print_objects, (objects, n),
       SCHEME_OBJECT * objects AND int n)
{
  SCHEME_OBJECT * scan;
  SCHEME_OBJECT * end;

  scan = objects;
  end = (objects + n);
  while (scan < end)
    {
      printf ("%4x: ", (((char *) scan) - ((char *) objects)));
      do_printing (stdout, (*scan++), true);
      printf ("\n");
    }
  fflush (stdout);
  return;
}

/* This is useful because `do_printing' doesn't print the contents of
   vectors.  The reason that it doesn't is because vectors are used to
   represent named structures, and most named structures don't want to
   be printed out explicitly.  */

static void
DEFUN (print_vector, (vector), SCHEME_OBJECT vector)
{
  print_objects
    ((MEMORY_LOC (vector, 1)), (OBJECT_DATUM (VECTOR_LENGTH (vector))));
  return;
}

static void
DEFUN (print_expression, (stream, expression, string),
       FILE * stream AND SCHEME_OBJECT expression AND char * string)
{
  if ((string [0]) != 0)
    fprintf (stream, "%s: ", string);
  do_printing (stream, expression, true);
  return;
}

void
DEFUN (Print_Expression, (expression, string),
       SCHEME_OBJECT expression AND char * string)
{
  print_expression (stdout, expression, string);
  return;
}

extern char * Type_Names [];

static void
DEFUN (do_printing, (stream, Expr, Detailed),
       FILE * stream AND SCHEME_OBJECT Expr AND Boolean Detailed)
{
  long Temp_Address;
  Boolean handled_p;

  Temp_Address = (OBJECT_DATUM (Expr));
  handled_p = false;

  switch (OBJECT_TYPE (Expr))
    {
    case TC_ACCESS:
      {
	fprintf (stream, "[ACCESS (");
	Expr = (MEMORY_REF (Expr, ACCESS_NAME));
      SPrint:
	print_symbol (stream, Expr);
	handled_p = true;
	fprintf (stream, ")");
	break;
      }

    case TC_ASSIGNMENT:
      fprintf (stream, "[SET! (");
      Expr = (MEMORY_REF ((MEMORY_REF (Expr, ASSIGN_NAME)), VARIABLE_SYMBOL));
      goto SPrint;

    case TC_CHARACTER_STRING:
      print_string (stream, Expr);
      return;

    case TC_DEFINITION:
      fprintf (stream, "[DEFINE (");
      Expr = (MEMORY_REF (Expr, DEFINE_NAME));
      goto SPrint;

    case TC_FIXNUM:
      fprintf (stream, "%ld", ((long) (FIXNUM_TO_LONG (Expr))));
      return;

    case TC_BIG_FLONUM:
      fprintf (stream, "%lf", (FLONUM_TO_DOUBLE (Expr)));
      return;

    case TC_WEAK_CONS:
    case TC_LIST:
      print_list (stream, Expr);
      return;

    case TC_NULL:
      if (Temp_Address == 0)
	{
	  fprintf (stream, "()");
	  return;
	}
      break;

    case TC_UNINTERNED_SYMBOL:
      fprintf (stream, "[UNINTERNED_SYMBOL (");
      goto SPrint;

    case TC_INTERNED_SYMBOL:
      print_symbol (stream, Expr);
      return;

    case TC_VARIABLE:
      Expr = (MEMORY_REF (Expr, VARIABLE_SYMBOL));
      if (Detailed)
	{
	  fprintf (stream, "[VARIABLE (");
	  goto SPrint;
	}
      print_symbol (stream, Expr);
      return;

    case TC_COMBINATION:
      fprintf (stream, "[COMBINATION (%ld args) 0x%lx]",
	      ((long) ((VECTOR_LENGTH (Expr)) - 1)),
	      ((long) Temp_Address));
      if (Detailed)
	{
	  fprintf (stream, " (");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_FN_SLOT)), false);
	  fprintf (stream, " ...)");
	}
      return;

    case TC_COMBINATION_1:
      fprintf (stream, "[COMBINATION_1 0x%lx]", ((long) Temp_Address));
      if (Detailed)
	{
	  fprintf (stream, " (");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_1_FN)), false);
	  fprintf (stream, ", ");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_1_ARG_1)), false);
	  fprintf (stream, ")");
	}
      return;

    case TC_COMBINATION_2:
      fprintf (stream, "[COMBINATION_2 0x%lx]", ((long) Temp_Address));
      if (Detailed)
	{
	  fprintf (stream, " (");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_2_FN)), false);
	  fprintf (stream, ", ");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_2_ARG_1)), false);
	  fprintf (stream, ", ");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_2_ARG_2)), false);
	  fprintf (stream, ")");
	}
      return;

    case TC_ENVIRONMENT:
      {
	SCHEME_OBJECT procedure;

	fprintf (stream, "[ENVIRONMENT 0x%lx]", ((long) Temp_Address));
	fprintf (stream, " (from ");
	procedure = (MEMORY_REF (Expr, ENVIRONMENT_FUNCTION));
	if ((OBJECT_TYPE (procedure)) == TC_QUAD)
	  procedure = (MEMORY_REF (procedure, ENV_EXTENSION_PROCEDURE));
	do_printing (stream, procedure, false);
	fprintf (stream, ")");
	return;
      }

    case TC_EXTENDED_LAMBDA:
      if (Detailed)
	fprintf (stream, "[EXTENDED_LAMBDA (");
      do_printing (stream,
		   (MEMORY_REF ((MEMORY_REF (Expr, ELAMBDA_NAMES)), 1)),
		   false);
      if (Detailed)
	fprintf (stream, ") 0x%lx", ((long) Temp_Address));
      return;

    case TC_EXTENDED_PROCEDURE:
      if (Detailed)
	fprintf (stream, "[EXTENDED_PROCEDURE (");
      do_printing (stream, (MEMORY_REF (Expr, PROCEDURE_LAMBDA_EXPR)), false);
      if (Detailed)
	fprintf (stream, ") 0x%lx]", ((long) Temp_Address));
      break;

    case TC_LAMBDA:
      if (Detailed)
	fprintf (stream, "[LAMBDA (");
      do_printing (stream,
		   (MEMORY_REF ((MEMORY_REF (Expr, LAMBDA_FORMALS)), 1)),
		  false);
      if (Detailed)
	fprintf (stream, ") 0x%lx]", ((long) Temp_Address));
      return;

    case TC_PRIMITIVE:
      fprintf (stream, "[PRIMITIVE ");
      print_primitive_name (stream, Expr);
      fprintf (stream, "]");
      return;

    case TC_PROCEDURE:
      if (Detailed)
	fprintf (stream, "[PROCEDURE (");
      do_printing (stream, (MEMORY_REF (Expr, PROCEDURE_LAMBDA_EXPR)), false);
      if (Detailed)
	fprintf (stream, ") 0x%lx]", ((long) Temp_Address));
      return;

    case TC_REFERENCE_TRAP:
      {
	if ((OBJECT_DATUM (Expr)) <= TRAP_MAX_IMMEDIATE)
	  break;
	fprintf (stream, "[REFERENCE-TRAP");
	print_expression (stream, (MEMORY_REF (Expr, TRAP_TAG)), " tag");
	print_expression (stream, (MEMORY_REF (Expr, TRAP_EXTRA)), " extra");
	fprintf (stream, "]");
	return;
      }

    case TC_RETURN_CODE:
      fprintf (stream, "[RETURN_CODE ");
      print_return_name (stream, Expr);
      fprintf (stream, "]");
      return;

    case TC_TRUE:
      if (Temp_Address == 0)
	{
	  fprintf (stream, "#T");
	  return;
	}
      break;

    case TC_COMPILED_ENTRY:
      {
	long results [3];
	char * type_string;
	SCHEME_OBJECT filename;
	SCHEME_OBJECT entry;
	Boolean closure_p;

	entry = Expr;
	closure_p = false;
	compiled_entry_type (entry, (& (results [0])));
	switch (results [0])
	  {
	  case 0:
	    if (compiled_entry_closure_p (entry))
	      {
		type_string = "COMPILED_CLOSURE";
		entry = (compiled_closure_to_entry (entry));
		closure_p = true;
	      }
	    else
	      type_string = "COMPILED_PROCEDURE";
	    break;
	  case 1:
	    type_string = "COMPILED_RETURN_ADDRESS";
	    break;
	  case 2:
	    type_string = "COMPILED_EXPRESSION";
	    break;
	  default:
	    type_string = "COMPILED_ENTRY";
	    break;
	  }

	fprintf (stream, "[%s offset: 0x%lx entry: 0x%lx",
		 type_string,
		 ((long) (compiled_entry_to_block_offset (entry))),
		 ((long) (OBJECT_DATUM (entry))));
	if (closure_p)
	  fprintf (stream, " address: 0x%lx", ((long) Temp_Address));

	filename = (compiled_entry_debug_filename (entry));
	if (STRING_P (filename))
	  {
	    fprintf (stream, " file: ");
	    print_filename (stream, filename);
	  }
	else if (PAIR_P (filename))
	  {
	    fprintf (stream, " file: ");
	    print_filename (stream, (PAIR_CAR (filename)));
	    fprintf (stream, " block: %ld",
		    ((long) (FIXNUM_TO_LONG (PAIR_CDR (filename)))));
	  }
	fprintf (stream, "]");
	return;
      }

    default:
      break;
    }
  if (! handled_p)
    {
      if ((OBJECT_TYPE (Expr)) <= LAST_TYPE_CODE)
	fprintf (stream, "[%s", (Type_Names [OBJECT_TYPE (Expr)]));
      else
	fprintf (stream, "[0x%02x", (OBJECT_TYPE (Expr)));
    }
  fprintf (stream, " 0x%lx]", ((long) Temp_Address));
  return;
}

static Boolean
DEFUN (print_one_continuation_frame, (stream, Temp),
       FILE * stream AND SCHEME_OBJECT Temp)
{
  SCHEME_OBJECT Expr;

  print_expression (stream, Temp, "Return code");
  fprintf (stream, "\n");
  Expr = (STACK_POP ());
  print_expression (stream, Expr, "Expression");
  fprintf (stream, "\n");
  if (((OBJECT_DATUM (Temp)) == RC_END_OF_COMPUTATION) ||
      ((OBJECT_DATUM (Temp)) == RC_HALT))
    return (true);
  if ((OBJECT_DATUM (Temp)) == RC_JOIN_STACKLETS)
    Stack_Pointer = (Previous_Stack_Pointer (Expr));
  return (false);
}

extern Boolean EXFUN (Print_One_Continuation_Frame, (SCHEME_OBJECT));

Boolean
DEFUN (Print_One_Continuation_Frame, (Temp), SCHEME_OBJECT Temp)
{
  return (print_one_continuation_frame (stdout, Temp));
}

/* Back_Trace relies on (a) only a call to Save_Cont puts a return code on the
   stack; (b) Save_Cont pushes the expression first.
 */

void
DEFUN (Back_Trace, (stream), FILE * stream)
{
  SCHEME_OBJECT Temp, * Old_Stack;

  Back_Trace_Entry_Hook();
  Old_Stack = Stack_Pointer;
  while (true)
  {
    if ((STACK_LOCATIVE_DIFFERENCE (Stack_Top, (STACK_LOC (0)))) <= 0)
    {
      if ((STACK_LOC (0)) == Old_Stack)
	fprintf (stream, "\n[Invalid stack pointer.]\n");
      else
	fprintf (stream, "\n[Stack ends abruptly.]\n");
      break;
    }
    if (Return_Hook_Address == (STACK_LOC (0)))
    {
      Temp = (STACK_POP ());
      if (Temp != (MAKE_OBJECT (TC_RETURN_CODE, RC_RETURN_TRAP_POINT)))
      {
        fprintf (stream, "\n--> Return trap is missing here <--\n");
      }
      else
      {
	fprintf (stream, "\n[Return trap found here as expected]\n");
        Temp = Old_Return_Code;
      }
    }
    else
    {
      Temp = (STACK_POP ());
    }
    if ((OBJECT_TYPE (Temp)) == TC_RETURN_CODE)
    {
      if (print_one_continuation_frame (stream, Temp))
	break;
    }
    else
    {
      print_expression (stream, Temp, "  ...");
      if ((OBJECT_TYPE (Temp)) == TC_MANIFEST_NM_VECTOR)
      {
	Stack_Pointer = (STACK_LOC (- (OBJECT_DATUM (Temp))));
        fprintf (stream, " (skipping)");
      }
      fprintf (stream, "\n");
    }
  }
  Stack_Pointer = Old_Stack;
  Back_Trace_Exit_Hook();
  fflush (stream);
  return;
}

static void
DEFUN (print_stack, (sp), SCHEME_OBJECT * sp)
{
  SCHEME_OBJECT * saved_sp;

  saved_sp = Stack_Pointer;
  Stack_Pointer = sp;
  Back_Trace (stdout);
  Stack_Pointer = saved_sp;
  return;
}

static Boolean
DEFUN (print_primitive_name, (stream, primitive),
       FILE * stream AND SCHEME_OBJECT primitive)
{
  extern char * EXFUN (primitive_to_name, (SCHEME_OBJECT));
  char *name;

  name = primitive_to_name(primitive);
  if (name == ((char *) NULL))
  {
    fprintf (stream, "Unknown primitive 0x%08x", PRIMITIVE_NUMBER(primitive));
    return false;
  }
  else
  {
    fprintf (stream, "%s", name);
    return true;
  }
}

void
DEFUN (Print_Primitive, (primitive), SCHEME_OBJECT primitive)
{
  extern long EXFUN (primitive_to_arity, (SCHEME_OBJECT));
  char buffer[40];
  int NArgs, i;

  printf ("Primitive: ");
  if (print_primitive_name (stdout, primitive))
  {
    NArgs = primitive_to_arity(primitive);
  }
  else
  {
    NArgs = 3;	        /* Unknown primitive */
  }
  printf ("\n");

  for (i = 0; i < NArgs; i++)
  {
    sprintf (buffer, "...Arg %ld", ((long) (i + 1)));
    print_expression (stdout, (STACK_REF (i)), buffer);
    printf ("\n");
  }
  return;
}

/* Code for interactively setting and clearing the interpreter
   debugging flags.  Invoked via the "D" command to the ^C
   handler or during each FASLOAD. */

#ifdef ENABLE_DEBUGGING_TOOLS

#ifndef MORE_DEBUG_FLAG_CASES
#define MORE_DEBUG_FLAG_CASES()
#endif

#ifndef MORE_DEBUG_FLAG_NAMES
#define MORE_DEBUG_FLAG_NAMES()
#endif

#ifndef SET_FLAG_HOOK
#define SET_FLAG_HOOK()
#endif

#ifndef DEBUG_GETDEC
#define DEBUG_GETDEC debug_getdec
#endif

#define D_EVAL			0
#define D_HEX_INPUT		1
#define D_FILE_LOAD		2
#define D_RELOC			3
#define D_INTERN		4
#define D_CONT			5
#define D_PRIMITIVE		6
#define D_LOOKUP		7
#define D_DEFINE		8
#define D_GC			9
#define D_UPGRADE		10
#define D_DUMP			11
#define D_TRACE_ON_ERROR	12
#define D_PER_FILE		13
#define D_BIGNUM		14
#define D_FLUIDS		15

#ifndef LAST_SWITCH
#define LAST_SWITCH D_FLUIDS
#endif

static Boolean *
DEFUN (find_flag, (flag_number), int flag_number)
{
  switch (flag_number)
    {
    case D_EVAL:		return (&Eval_Debug);
    case D_HEX_INPUT:		return (&Hex_Input_Debug);
    case D_FILE_LOAD:		return (&File_Load_Debug);
    case D_RELOC:		return (&Reloc_Debug);
    case D_INTERN:		return (&Intern_Debug);
    case D_CONT:		return (&Cont_Debug);
    case D_PRIMITIVE:		return (&Primitive_Debug);
    case D_LOOKUP:		return (&Lookup_Debug) ;
    case D_DEFINE:		return (&Define_Debug);
    case D_GC:			return (&GC_Debug);
    case D_UPGRADE:		return (&Upgrade_Debug);
    case D_DUMP:		return (&Dump_Debug);
    case D_TRACE_ON_ERROR:	return (&Trace_On_Error);
    case D_PER_FILE:		return (&Per_File);
    case D_BIGNUM:		return (&Bignum_Debug);
    case D_FLUIDS:		return (&Fluids_Debug);
    MORE_DEBUG_FLAG_CASES ();
    default:			return (0);
    }
}

static char *
DEFUN (flag_name, (flag_number), int flag_number)
{
  switch (flag_number)
    {
    case D_EVAL:		return ("Eval_Debug");
    case D_HEX_INPUT:		return ("Hex_Input_Debug");
    case D_FILE_LOAD:		return ("File_Load_Debug");
    case D_RELOC:		return ("Reloc_Debug");
    case D_INTERN:		return ("Intern_Debug");
    case D_CONT:		return ("Cont_Debug");
    case D_PRIMITIVE:		return ("Primitive_Debug");
    case D_LOOKUP:		return ("Lookup_Debug");
    case D_DEFINE:		return ("Define_Debug");
    case D_GC:			return ("GC_Debug");
    case D_UPGRADE:		return ("Upgrade_Debug");
    case D_DUMP:		return ("Dump_Debug");
    case D_TRACE_ON_ERROR:	return ("Trace_On_Error");
    case D_PER_FILE:		return ("Per_File");
    case D_BIGNUM:		return ("Bignum_Debug");
    case D_FLUIDS:		return ("Fluids_Debug");
    MORE_DEBUG_FLAG_NAMES ();
    default:			return ("Unknown Debug Flag");
    }
}

static void
DEFUN (show_flags, (all), int all)
{
  int i;
  for (i = 0; (i <= LAST_SWITCH); i += 1)
    {
      int value = (* (find_flag (i)));
      if (all || value)
	fprintf (stdout, "Flag %ld (%s) is %s.\n",
		 ((long) i), (flag_name (i)), (value ? "set" : "clear"));
    }
  fflush (stdout);
  return;
}

static int
DEFUN (set_flag, (flag_number, value), int flag_number AND int value)
{
  Boolean * flag = (find_flag (flag_number));
  if (flag == 0)
    show_flags (1);
  else
    {
      (*flag) = value;
      SET_FLAG_HOOK (flag);
    }
  return (0);
}

static int
DEFUN (debug_getdec, (string), CONST char * string)
{
  int result;

  sscanf (string, "%ld", (&result));
  return (result);
}

void
DEFUN_VOID (debug_edit_flags)
{
  char input_line [256];
  show_flags (0);
  while (1)
    {
      fputs ("Clear<number>, Set<number>, Done, ?, or Halt: ", stdout);
      fflush (stdout);
      {
	fgets (input_line, (sizeof (input_line)), stdin);
	switch (input_line[0])
	  {
	   case 'c':
	   case 'C':
	     set_flag ((DEBUG_GETDEC (input_line)), 0);
	     break;
	   case 's':
	   case 'S':
	     set_flag ((DEBUG_GETDEC (input_line)), 1);
	     break;
	   case 'd':
	   case 'D':
	     return;
	   case 'h':
	   case 'H':
	     termination_normal (0);
	   case '?':
	   default:
	     show_flags (1);
	     break;
	   }
      }
    }
}

#else /* not ENABLE_DEBUGGING_TOOLS */

void
DEFUN_VOID (debug_edit_flags)
{
  fprintf (stderr, "Not a debugging version.  No flags to handle.\n");
  fflush (stderr);
  return;
}

#endif /* not ENABLE_DEBUGGING_TOOLS */
