/* -*-C-*-

$Id: debug.c,v 9.56 2002/11/20 19:46:08 cph Exp $

Copyright (c) 1987-2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* Utilities to help with debugging */

#include "scheme.h"
#include "prims.h"
#include "trap.h"
#include "lookup.h"

static void EXFUN (do_printing, (outf_channel, SCHEME_OBJECT, Boolean));
static Boolean EXFUN (print_primitive_name, (outf_channel, SCHEME_OBJECT));
static void EXFUN (print_expression, (outf_channel, SCHEME_OBJECT, char *));

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
      outf_console ("Past end of area.\n");
      return;
    }
    if (Obj_Address == Free_Constant)
    {
      outf_console ("Done.\n");
      return;
    }
    Pure_Size = OBJECT_DATUM (*Obj_Address);
    Total_Size = OBJECT_DATUM (Obj_Address[1]);
    outf_console ("0x%lx: pure=0x%lx, total=0x%lx\n",
	    ((long) Obj_Address), ((long) Pure_Size), ((long) Total_Size));
    if (OBJECT_TYPE (*Obj_Address) != TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      outf_console ("Missing initial SNMV.\n");
      return;
    }
    if (OBJECT_TYPE (Obj_Address[1]) != PURE_PART)
    {
      outf_console ("Missing subsequent pure header.\n");
    }
    if (OBJECT_TYPE (Obj_Address[Pure_Size-1]) !=
        TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      outf_console ("Missing internal SNMV.\n");
      return;
    }
    if (OBJECT_TYPE (Obj_Address[Pure_Size]) != CONSTANT_PART)
    {
      outf_console ("Missing constant header.\n");
      return;
    }
    if (((long) (OBJECT_DATUM (Obj_Address[Pure_Size]))) != Pure_Size)
    {
      outf_console ("Pure size mismatch 0x%lx.\n",
	      ((long) (OBJECT_DATUM (Obj_Address[Pure_Size]))));
    }
    if (OBJECT_TYPE (Obj_Address[Total_Size-1]) !=
        TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      outf_console ("Missing ending SNMV.\n");
      return;
    }
    if (OBJECT_TYPE (Obj_Address[Total_Size]) != END_OF_BLOCK)
    {
      outf_console ("Missing ending header.\n");
      return;
    }
    if (((long) (OBJECT_DATUM (Obj_Address[Total_Size]))) != Total_Size)
    {
      outf_console ("Total size mismatch 0x%lx.\n",
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

  if (FRAME_EXTENSION_P (procedure))
  {
    extension = procedure;
    procedure = FAST_MEMORY_REF (extension, ENV_EXTENSION_PROCEDURE);
  }
  else
    extension = SHARP_F;

  if ((OBJECT_TYPE (procedure) != TC_PROCEDURE) &&
      (OBJECT_TYPE (procedure) != TC_EXTENDED_PROCEDURE))
  {
    outf_console ("Not created by a procedure");
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
    outf_console ("\n");
  }
  if (extension != SHARP_F)
  {
    outf_console ("Auxiliary Variables\n");
    count = (GET_FRAME_EXTENSION_LENGTH (extension));
    for (i = 0, name_ptr = (GET_FRAME_EXTENSION_BINDINGS (extension));
	 i < count;
	 i++, name_ptr++)
    {
      Print_Expression ((PAIR_CAR (*name_ptr)), "Name ");
      Print_Expression ((PAIR_CDR (*name_ptr)), " Value ");
      outf_console ("\n");
    }
  }
}

static void
DEFUN (print_list, (stream, pair), outf_channel stream AND SCHEME_OBJECT pair)
{
  int count;

  outf (stream, "(");
  count = 0;
  while (((PAIR_P (pair)) || (WEAK_PAIR_P (pair))) && (count < MAX_LIST_PRINT))
    {
      if (count > 0)
	outf (stream, " ");
      print_expression (stream,
			(PAIR_CAR (pair)),
			((WEAK_PAIR_P (pair)) ? "{weak}" : ""));
      pair = (PAIR_CDR (pair));
      count += 1;
    }
  if (pair != EMPTY_LIST)
    {
      if (count == MAX_LIST_PRINT)
	outf (stream, " ...");
      else
	{
	  outf (stream, " . ");
	  print_expression (stream, pair, "");
	}
    }
  outf (stream, ")");
  return;
}

static void
DEFUN (print_return_name, (stream, Ptr), outf_channel stream AND SCHEME_OBJECT Ptr)
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
	  outf (stream, "%s", name);
	  return;
	}
    }
  outf (stream, "[0x%lx]", index);
  return;
}

void
DEFUN (Print_Return, (String), char * String)
{
  outf_console ("%s: ", String);
  print_return_name (console_output, ret_register);
  outf_console ("\n");
}

static void
DEFUN (print_string, (stream, string), outf_channel stream AND SCHEME_OBJECT string)
{
  long length;
  long i;
  char * next;
  char this;

  outf (stream, "\"");
  length = (STRING_LENGTH (string));
  next = ((char *) (STRING_LOC (string, 0)));
  for (i = 0; (i < length); i += 1)
    {
      this = (*next++);
      switch (this)
	{
	case '\\':
	  outf (stream, "\\\\");
	  break;
	case '"':
	  outf (stream, "\\\"");
	  break;
	case '\t':
	  outf (stream, "\\t");
	  break;
	case '\n':
	  outf (stream, "\\n");
	  break;
	case '\f':
	  outf (stream, "\\f");
	  break;
	default:
	  if ((this >= ' ') && (this <= '~'))
	    outf (stream, "%c", this);
	  else
	    outf (stream, "\\%03o", this);
	  break;
	}
    }
  outf (stream, "\"");
  return;
}

static void
DEFUN (print_symbol, (stream, symbol), outf_channel stream AND SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT string;
  long length;
  long i;
  char * next;

  string = (MEMORY_REF (symbol, SYMBOL_NAME));
  length = (STRING_LENGTH (string));
  next = ((char *) (STRING_LOC (string, 0)));
  for (i = 0; (i < length); i += 1)
    outf(stream, "%c", *next++);  /*should use %s? */
  return;
}

static void
DEFUN (print_filename, (stream, filename),
       outf_channel stream AND SCHEME_OBJECT filename)
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
  outf (stream, "\"%s\"", slash);
  return;
}

static void
DEFUN (print_object, (object), SCHEME_OBJECT object)
{
  do_printing (console_output, object, true);
  outf_console ("\n");
  outf_flush_console();
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
      outf_console ("%4x: ", (((char *) scan) - ((char *) objects)));
      do_printing (console_output, (*scan++), true);
      outf_console ("\n");
    }
  outf_flush_console();
  return;
}

/* This is useful because `do_printing' doesn't print the contents of
   vectors.  The reason that it doesn't is because vectors are used to
   represent named structures, and most named structures don't want to
   be printed out explicitly.  */

void
DEFUN (Print_Vector, (vector), SCHEME_OBJECT vector)
{
  print_objects
    ((MEMORY_LOC (vector, 1)), (OBJECT_DATUM (VECTOR_LENGTH (vector))));
}

static void
DEFUN (print_expression, (stream, expression, string),
       outf_channel stream AND SCHEME_OBJECT expression AND char * string)
{
  if ((string [0]) != 0)
    outf (stream, "%s: ", string);
  do_printing (stream, expression, true);
  return;
}

void
DEFUN (Print_Expression, (expression, string),
       SCHEME_OBJECT expression AND char * string)
{
  print_expression (console_output, expression, string);
  return;
}

extern char * Type_Names [];

static void
DEFUN (do_printing, (stream, Expr, Detailed),
       outf_channel stream AND SCHEME_OBJECT Expr AND Boolean Detailed)
{
  long Temp_Address;
  Boolean handled_p;

  Temp_Address = (OBJECT_DATUM (Expr));
  handled_p = false;

  if (Expr == EMPTY_LIST)	{ outf (stream, "()");	return; }
  else if (Expr == SHARP_F)	{ outf (stream, "#F");	return; }
  else if (Expr == SHARP_T)	{ outf (stream, "#T");	return; }
  else if (Expr == UNSPECIFIC)	{ outf (stream, "[UNSPECIFIC]"); return; }

  switch (OBJECT_TYPE (Expr))
    {
    case TC_ACCESS:
      {
	outf (stream, "[ACCESS (");
	Expr = (MEMORY_REF (Expr, ACCESS_NAME));
      SPrint:
	print_symbol (stream, Expr);
	handled_p = true;
	outf (stream, ")");
	break;
      }

    case TC_ASSIGNMENT:
      outf (stream, "[SET! (");
      Expr = (MEMORY_REF ((MEMORY_REF (Expr, ASSIGN_NAME)), VARIABLE_SYMBOL));
      goto SPrint;

    case TC_CHARACTER_STRING:
      print_string (stream, Expr);
      return;

    case TC_DEFINITION:
      outf (stream, "[DEFINE (");
      Expr = (MEMORY_REF (Expr, DEFINE_NAME));
      goto SPrint;

    case_TC_FIXNUMs:
      outf (stream, "%ld", ((long) (FIXNUM_TO_LONG (Expr))));
      return;

    case TC_BIG_FLONUM:
      outf (stream, "%lf", (FLONUM_TO_DOUBLE (Expr)));
      return;

    case TC_WEAK_CONS:
    case TC_LIST:
      print_list (stream, Expr);
      return;

    case TC_NULL:
      break;

    case TC_UNINTERNED_SYMBOL:
      outf (stream, "[UNINTERNED_SYMBOL (");
      goto SPrint;

    case TC_INTERNED_SYMBOL:
      print_symbol (stream, Expr);
      return;

    case TC_VARIABLE:
      Expr = (MEMORY_REF (Expr, VARIABLE_SYMBOL));
      if (Detailed)
	{
	  outf (stream, "[VARIABLE (");
	  goto SPrint;
	}
      print_symbol (stream, Expr);
      return;

    case TC_COMBINATION:
      outf (stream, "[COMBINATION (%ld args) 0x%lx]",
	      ((long) ((VECTOR_LENGTH (Expr)) - 1)),
	      ((long) Temp_Address));
      if (Detailed)
	{
	  outf (stream, " (");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_FN_SLOT)), false);
	  outf (stream, " ...)");
	}
      return;

    case TC_COMBINATION_1:
      outf (stream, "[COMBINATION_1 0x%lx]", ((long) Temp_Address));
      if (Detailed)
	{
	  outf (stream, " (");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_1_FN)), false);
	  outf (stream, ", ");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_1_ARG_1)), false);
	  outf (stream, ")");
	}
      return;

    case TC_COMBINATION_2:
      outf (stream, "[COMBINATION_2 0x%lx]", ((long) Temp_Address));
      if (Detailed)
	{
	  outf (stream, " (");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_2_FN)), false);
	  outf (stream, ", ");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_2_ARG_1)), false);
	  outf (stream, ", ");
	  do_printing (stream, (MEMORY_REF (Expr, COMB_2_ARG_2)), false);
	  outf (stream, ")");
	}
      return;

    case TC_ENVIRONMENT:
      {
	SCHEME_OBJECT procedure;

	outf (stream, "[ENVIRONMENT 0x%lx]", ((long) Temp_Address));
	outf (stream, " (from ");
	procedure = (MEMORY_REF (Expr, ENVIRONMENT_FUNCTION));
	if ((OBJECT_TYPE (procedure)) == TC_QUAD)
	  procedure = (MEMORY_REF (procedure, ENV_EXTENSION_PROCEDURE));
	do_printing (stream, procedure, false);
	outf (stream, ")");
	return;
      }

    case TC_EXTENDED_LAMBDA:
      if (Detailed)
	outf (stream, "[EXTENDED_LAMBDA (");
      do_printing (stream,
		   (MEMORY_REF ((MEMORY_REF (Expr, ELAMBDA_NAMES)), 1)),
		   false);
      if (Detailed)
	outf (stream, ") 0x%lx", ((long) Temp_Address));
      return;

    case TC_EXTENDED_PROCEDURE:
      if (Detailed)
	outf (stream, "[EXTENDED_PROCEDURE (");
      do_printing (stream, (MEMORY_REF (Expr, PROCEDURE_LAMBDA_EXPR)), false);
      if (Detailed)
	outf (stream, ") 0x%lx]", ((long) Temp_Address));
      break;

    case TC_LAMBDA:
      if (Detailed)
	outf (stream, "[LAMBDA (");
      do_printing (stream,
		   (MEMORY_REF ((MEMORY_REF (Expr, LAMBDA_FORMALS)), 1)),
		  false);
      if (Detailed)
	outf (stream, ") 0x%lx]", ((long) Temp_Address));
      return;

    case TC_PRIMITIVE:
      outf (stream, "[PRIMITIVE ");
      print_primitive_name (stream, Expr);
      outf (stream, "]");
      return;

    case TC_PROCEDURE:
      if (Detailed)
	outf (stream, "[PROCEDURE (");
      do_printing (stream, (MEMORY_REF (Expr, PROCEDURE_LAMBDA_EXPR)), false);
      if (Detailed)
	outf (stream, ") 0x%lx]", ((long) Temp_Address));
      return;

    case TC_REFERENCE_TRAP:
      {
	if ((OBJECT_DATUM (Expr)) <= TRAP_MAX_IMMEDIATE)
	  break;
	outf (stream, "[REFERENCE-TRAP");
	print_expression (stream, (MEMORY_REF (Expr, TRAP_TAG)), " tag");
	print_expression (stream, (MEMORY_REF (Expr, TRAP_EXTRA)), " extra");
	outf (stream, "]");
	return;
      }

    case TC_RETURN_CODE:
      outf (stream, "[RETURN_CODE ");
      print_return_name (stream, Expr);
      outf (stream, "]");
      return;

    case TC_CONSTANT:
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

	outf (stream, "[%s offset: 0x%lx entry: 0x%lx",
		 type_string,
		 ((long) (compiled_entry_to_block_offset (entry))),
		 ((long) (OBJECT_DATUM (entry))));
	if (closure_p)
	  outf (stream, " address: 0x%lx", ((long) Temp_Address));

	filename = (compiled_entry_debug_filename (entry));
	if (STRING_P (filename))
	  {
	    outf (stream, " file: ");
	    print_filename (stream, filename);
	  }
	else if (PAIR_P (filename))
	  {
	    outf (stream, " file: ");
	    print_filename (stream, (PAIR_CAR (filename)));
	    outf (stream, " block: %ld",
		    ((long) (FIXNUM_TO_LONG (PAIR_CDR (filename)))));
	  }
	outf (stream, "]");
	return;
      }

    default:
      break;
    }
  if (! handled_p)
    {
      if ((OBJECT_TYPE (Expr)) <= LAST_TYPE_CODE)
	outf (stream, "[%s", (Type_Names [OBJECT_TYPE (Expr)]));
      else
	outf (stream, "[0x%02x", (OBJECT_TYPE (Expr)));
    }
  outf (stream, " 0x%lx]", ((long) Temp_Address));
  return;
}

extern void
DEFUN (Debug_Print, (Expr, Detailed),
       SCHEME_OBJECT Expr AND Boolean Detailed)
{
  do_printing(console_output, Expr, Detailed);
  outf_flush_console ();
  return;
}

static Boolean
DEFUN (print_one_continuation_frame, (stream, Temp),
       outf_channel stream AND SCHEME_OBJECT Temp)
{
  SCHEME_OBJECT Expr;

  print_expression (stream, Temp, "Return code");
  outf (stream, "\n");
  Expr = (STACK_POP ());
  print_expression (stream, Expr, "Expression");
  outf (stream, "\n");
  if (((OBJECT_DATUM (Temp)) == RC_END_OF_COMPUTATION) ||
      ((OBJECT_DATUM (Temp)) == RC_HALT))
    return (true);
  if ((OBJECT_DATUM (Temp)) == RC_JOIN_STACKLETS)
    sp_register = (Previous_Stack_Pointer (Expr));
  return (false);
}

extern Boolean EXFUN (Print_One_Continuation_Frame, (SCHEME_OBJECT));

Boolean
DEFUN (Print_One_Continuation_Frame, (Temp), SCHEME_OBJECT Temp)
{
  return (print_one_continuation_frame (console_output, Temp));
}

/* Back_Trace relies on (a) only a call to Save_Cont puts a return code on the
   stack; (b) Save_Cont pushes the expression first.
 */

void
DEFUN (Back_Trace, (stream), outf_channel stream)
{
  SCHEME_OBJECT Temp, * Old_Stack;

  Back_Trace_Entry_Hook();
  Old_Stack = sp_register;
  while (true)
  {
    if ((STACK_LOCATIVE_DIFFERENCE (Stack_Top, (STACK_LOC (0)))) <= 0)
    {
      if ((STACK_LOC (0)) == Old_Stack)
	outf (stream, "\n[Invalid stack pointer.]\n");
      else
	outf (stream, "\n[Stack ends abruptly.]\n");
      break;
    }
    if (Return_Hook_Address == (STACK_LOC (0)))
    {
      Temp = (STACK_POP ());
      if (Temp != (MAKE_OBJECT (TC_RETURN_CODE, RC_RETURN_TRAP_POINT)))
      {
        outf (stream, "\n--> Return trap is missing here <--\n");
      }
      else
      {
	outf (stream, "\n[Return trap found here as expected]\n");
        Temp = Old_Return_Code;
      }
    }
    else
    {
      Temp = (STACK_POP ());
    }
    if ((OBJECT_TYPE (Temp)) == TC_RETURN_CODE)
    {
      outf (stream, "{0x%x}", STACK_LOC(0));
      if (print_one_continuation_frame (stream, Temp))
	break;
    }
    else
    {
      outf (stream, "{0x%x}", STACK_LOC(0));
      print_expression (stream, Temp, "  ...");
      if ((OBJECT_TYPE (Temp)) == TC_MANIFEST_NM_VECTOR)
      {
	sp_register = (STACK_LOC (- ((long) (OBJECT_DATUM (Temp)))));
        outf (stream, " (skipping)");
      }
      outf (stream, "\n");
    }
  }
  sp_register = Old_Stack;
  Back_Trace_Exit_Hook();
  outf_flush (stream);
}

void
DEFUN (print_stack, (sp), SCHEME_OBJECT * sp)
{
  SCHEME_OBJECT * saved_sp = sp_register;
  sp_register = sp;
  Back_Trace (console_output);
  sp_register = saved_sp;
}

extern void
DEFUN_VOID(Debug_Stack_Trace)
{
  print_stack(STACK_LOC(0));
}

static Boolean
DEFUN (print_primitive_name, (stream, primitive),
       outf_channel stream AND SCHEME_OBJECT primitive)
{
  CONST char * name = (PRIMITIVE_NAME (primitive));
  if (name == 0)
  {
    outf (stream, "Unknown primitive 0x%08x", PRIMITIVE_NUMBER(primitive));
    return false;
  }
  else
  {
    outf (stream, "%s", name);
    return true;
  }
}

void
DEFUN (Print_Primitive, (primitive), SCHEME_OBJECT primitive)
{
  char buffer[40];
  int NArgs, i;

  outf_console ("Primitive: ");
  if (print_primitive_name (console_output, primitive))
    NArgs = (PRIMITIVE_ARITY (primitive));
  else
    NArgs = 3;	        /* Unknown primitive */

  outf_console ("\n");

  for (i = 0; i < NArgs; i++)
  {
    sprintf (buffer, "...Arg %ld", ((long) (i + 1)));
    print_expression (console_output, (STACK_REF (i)), buffer);
    outf_console ("\n");
  }
  return;
}

/* Code for interactively setting and clearing the interpreter
   debugging flags.  Invoked via the "D" command to the ^C
   handler or during each FASLOAD. */

#ifdef ENABLE_DEBUGGING_FLAGS

#ifndef MORE_DEBUG_FLAG_CASES
#define MORE_DEBUG_FLAG_CASES()
#endif

#ifndef MORE_DEBUG_FLAG_NAMES
#define MORE_DEBUG_FLAG_NAMES()
#endif

#ifndef SET_FLAG_HOOK
#define SET_FLAG_HOOK(hook)
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
	outf (console_output, "Flag %ld (%s) is %s.\n",
		 ((long) i), (flag_name (i)), (value ? "set" : "clear"));
    }
  outf_flush_console();
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
      outf_console("Clear<number>, Set<number>, Done, ?, or Halt: ");
      outf_flush_console();
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

#else /* not ENABLE_DEBUGGING_FLAGS */

void
DEFUN_VOID (debug_edit_flags)
{
  outf_error ("Not a debugging version.  No flags to handle.\n");
  outf_flush_error();
  return;
}

static int
DEFUN (set_flag, (flag_number, value), int flag_number AND int value)
{
  signal_error_from_primitive (ERR_UNIMPLEMENTED_PRIMITIVE);
  /*NOTREACHED*/
  return (0);
}

#endif /* not ENABLE_DEBUGGING_FLAGS */

DEFINE_PRIMITIVE("SET-DEBUG-FLAGS!", Prim_set_debug_flags, 2, 2,
  "(SET-DEBUG-FLAGS! flag_number boolean)")
{
  PRIMITIVE_HEADER (2);
  set_flag ((arg_integer (1)), (BOOLEAN_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
