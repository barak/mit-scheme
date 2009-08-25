/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/* Utilities to help with debugging */

#include "scheme.h"
#include "prims.h"
#include "trap.h"
#include "lookup.h"

#ifdef CC_SUPPORT_P
   static SCHEME_OBJECT compiled_entry_debug_filename (SCHEME_OBJECT);
   static SCHEME_OBJECT compiled_block_debug_filename (SCHEME_OBJECT);
#endif

static void do_printing (outf_channel, SCHEME_OBJECT, bool);
static bool print_primitive_name (outf_channel, SCHEME_OBJECT);
static void print_expression (outf_channel, SCHEME_OBJECT, char *);

/* Compiled Code Debugging */

#ifdef CC_SUPPORT_P

char *
compiled_entry_filename (SCHEME_OBJECT entry)
{
  SCHEME_OBJECT result = (compiled_entry_debug_filename (entry));
  return
    ((STRING_P (result))
     ? (STRING_POINTER (result))
     : (PAIR_P (result))
     ? (STRING_POINTER (PAIR_CAR (result)))
     : "**** filename not known ****");
}

static SCHEME_OBJECT
compiled_entry_debug_filename (SCHEME_OBJECT entry)
{
  return
    (compiled_block_debug_filename
     (cc_entry_to_block ((cc_entry_closure_p (entry))
			 ? (cc_closure_to_entry (entry))
			 : entry)));
}

static SCHEME_OBJECT
compiled_block_debug_filename (SCHEME_OBJECT block)
{
  SCHEME_OBJECT info;

  info = (cc_block_debugging_info (block));
  return
    (((STRING_P (info)) ||
      ((PAIR_P (info)) &&
       (STRING_P (PAIR_CAR (info))) &&
       (FIXNUM_P (PAIR_CDR (info)))))
     ? info
     : SHARP_F);
}

#endif /* CC_SUPPORT_P */

void
Show_Env (SCHEME_OBJECT The_Env)
{
  SCHEME_OBJECT *name_ptr, procedure, *value_ptr, extension;
  long count, i;

  procedure = MEMORY_REF (The_Env, ENVIRONMENT_FUNCTION);
  value_ptr = MEMORY_LOC (The_Env, ENVIRONMENT_FIRST_ARG);

  if (FRAME_EXTENSION_P (procedure))
  {
    extension = procedure;
    procedure = MEMORY_REF (extension, ENV_EXTENSION_PROCEDURE);
  }
  else
    extension = SHARP_F;

  if ((OBJECT_TYPE (procedure) != TC_PROCEDURE) &&
      (OBJECT_TYPE (procedure) != TC_EXTENDED_PROCEDURE))
  {
    outf_error ("Not created by a procedure");
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
    outf_error ("\n");
  }
  if (extension != SHARP_F)
  {
    outf_error ("Auxiliary Variables\n");
    count = (GET_FRAME_EXTENSION_LENGTH (extension));
    for (i = 0, name_ptr = (GET_FRAME_EXTENSION_BINDINGS (extension));
	 i < count;
	 i++, name_ptr++)
    {
      Print_Expression ((PAIR_CAR (*name_ptr)), "Name ");
      Print_Expression ((PAIR_CDR (*name_ptr)), " Value ");
      outf_error ("\n");
    }
  }
}

static void
print_list (outf_channel stream, SCHEME_OBJECT pair)
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
  if (!EMPTY_LIST_P (pair))
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
}

static void
print_return_name (outf_channel stream, SCHEME_OBJECT Ptr)
{
  unsigned long index = (OBJECT_DATUM (Ptr));
  if (index <= MAX_RETURN)
    {
      const char * name = (Return_Names[index]);
      if ((name != 0) && ((name[0]) != '\0'))
	{
	  outf (stream, "%s", name);
	  return;
	}
    }
  outf (stream, "[0x%lx]", index);
}

void
Print_Return (char * String)
{
  outf_error ("%s: ", String);
  print_return_name (ERROR_OUTPUT, GET_RET);
  outf_error ("\n");
}

static void
print_string (outf_channel stream, SCHEME_OBJECT string)
{
  long length;
  long i;
  char * next;
  char this;

  outf (stream, "\"");
  length = (STRING_LENGTH (string));
  next = (STRING_POINTER (string));
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
}

static void
print_symbol (outf_channel stream, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT string;
  unsigned long length;
  unsigned long limit;
  unsigned long i;
  char * next;

  string = (MEMORY_REF (symbol, SYMBOL_NAME));
  length = (STRING_LENGTH (string));
  limit = ((length > 64) ? 64 : length);
  next = (STRING_POINTER (string));
  for (i = 0; (i < limit); i += 1)
    {
      int c = (*next++);
      if (c < 0x80)
	outf (stream, "%c", c);
      else
	outf (stream, "\\x%02x", c);
    }
  if (limit < length)
    outf (stream, "...");
}

#ifdef CC_SUPPORT_P
static void
print_filename (outf_channel stream, SCHEME_OBJECT filename)
{
  long length;
  char * scan;
  char * end;
  char * slash;

  length = (STRING_LENGTH (filename));
  scan = (STRING_POINTER (filename));
  end = (scan + length);
  slash = scan;
  while (scan < end)
    if ((*scan++) == '/')
      slash = scan;
  outf (stream, "\"%s\"", slash);
}
#endif

static void
print_object (SCHEME_OBJECT object)
{
  do_printing (ERROR_OUTPUT, object, true);
  outf_error ("\n");
  outf_flush_error();
}

DEFINE_PRIMITIVE ("DEBUGGING-PRINTER", Prim_debugging_printer, 1, 1,
  "A cheap, built-in printer intended for debugging the interpreter.")
{
  PRIMITIVE_HEADER (1);

  print_object (ARG_REF (1));
  return (SHARP_F);
}

static void
print_objects (SCHEME_OBJECT * objects, int n)
{
  SCHEME_OBJECT * scan;
  SCHEME_OBJECT * end;

  scan = objects;
  end = (objects + n);
  while (scan < end)
    {
      outf_error
	("%4lx: ", ((unsigned long) (((char *) scan) - ((char *) objects))));
      do_printing (ERROR_OUTPUT, (*scan++), true);
      outf_error ("\n");
    }
  outf_flush_error();
}

/* This is useful because `do_printing' doesn't print the contents of
   vectors.  The reason that it doesn't is because vectors are used to
   represent named structures, and most named structures don't want to
   be printed out explicitly.  */

void
Print_Vector (SCHEME_OBJECT vector)
{
  print_objects
    ((MEMORY_LOC (vector, 1)), (OBJECT_DATUM (VECTOR_LENGTH (vector))));
}

static void
print_expression (outf_channel stream, SCHEME_OBJECT expression, char * string)
{
  if ((string [0]) != 0)
    outf (stream, "%s: ", string);
  do_printing (stream, expression, true);
}

void
Print_Expression (SCHEME_OBJECT expression, char * string)
{
  print_expression (ERROR_OUTPUT, expression, string);
}

static void
do_printing (outf_channel stream, SCHEME_OBJECT Expr, bool Detailed)
{
  long Temp_Address = (OBJECT_DATUM (Expr));
  bool handled_p = false;

  if (EMPTY_LIST_P (Expr))	{ outf (stream, "()");	return; }
  else if (Expr == SHARP_F)	{ outf (stream, "#F");	return; }
  else if (Expr == SHARP_T)	{ outf (stream, "#T");	return; }
  else if (Expr == UNSPECIFIC)	{ outf (stream, "[UNSPECIFIC]"); return; }

  else if (Expr == return_to_interpreter)
    {
      outf (stream, "[RETURN_TO_INTERPRETER]");
      return;
    }

  else if (Expr == reflect_to_interface)
    {
      outf (stream, "[REFLECT_TO_INTERFACE]");
      return;
    }


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

    case TC_FIXNUM:
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

#ifdef CC_SUPPORT_P
    case TC_COMPILED_ENTRY:
      {
	SCHEME_OBJECT entry = Expr;
	bool closure_p = false;
	cc_entry_type_t cet;
	const char * type_string;
	SCHEME_OBJECT filename;

	if (read_cc_entry_type ((&cet), (CC_ENTRY_ADDRESS (entry))))
	  type_string = "UNKNOWN";
	else
	  switch (cet.marker)
	    {
	    case CET_PROCEDURE:
	    case CET_CLOSURE:
	      if (cc_entry_closure_p (entry))
		{
		  type_string = "COMPILED_CLOSURE";
		  entry = (cc_closure_to_entry (entry));
		  closure_p = true;
		}
	      else
		type_string = "COMPILED_PROCEDURE";
	      break;

	    case CET_CONTINUATION:
	      type_string = "COMPILED_RETURN_ADDRESS";
	      break;

	    case CET_EXPRESSION:
	      type_string = "COMPILED_EXPRESSION";
	      break;

	    case CET_INTERNAL_CONTINUATION:
	      type_string = "COMPILED_RETURN_ADDRESS";
	      break;

	    case CET_INTERNAL_PROCEDURE:
	    case CET_TRAMPOLINE:
	      type_string = "COMPILED_ENTRY";
	      break;

	    case CET_RETURN_TO_INTERPRETER:
	      type_string = "COMPILED_RETURN_ADDRESS";
	      break;

	    default:
	      type_string = "COMPILED_ENTRY";
	      break;
	    }

	outf (stream, "[%s offset: %#lx entry: %#lx",
	      type_string,
	      (cc_entry_to_block_offset (entry)),
	      (OBJECT_DATUM (entry)));
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
#endif

    default:
      break;
    }
  if (!handled_p)
    {
      unsigned int type = (OBJECT_TYPE (Expr));
      const char * name = 0;
      if ((OBJECT_TYPE (Expr)) < TYPE_CODE_LIMIT)
	name = (type_names[type]);
      if (name != 0)
	outf (stream, "[%s", name);
      else
	outf (stream, "[%#02x", type);
    }
  outf (stream, " %#lx]", ((unsigned long) Temp_Address));
}

extern void
Debug_Print (SCHEME_OBJECT Expr, bool Detailed)
{
  do_printing (ERROR_OUTPUT, Expr, Detailed);
  outf_error ("\n");
  outf_flush_error ();
}

static bool
print_one_continuation_frame (outf_channel stream, SCHEME_OBJECT Temp)
{
  SCHEME_OBJECT Expr;

  outf (stream, "\n    ");
  print_expression (stream, Temp, "Return code");
  outf (stream, "\n    ");
  Expr = (STACK_POP ());
  print_expression (stream, Expr, "Expression");
  outf (stream, "\n");
  if (((OBJECT_DATUM (Temp)) == RC_END_OF_COMPUTATION)
      || ((OBJECT_DATUM (Temp)) == RC_HALT))
    return (true);
  if ((OBJECT_DATUM (Temp)) == RC_JOIN_STACKLETS)
    stack_pointer = (control_point_start (Expr));
  return (false);
}

extern bool Print_One_Continuation_Frame (SCHEME_OBJECT);

bool
Print_One_Continuation_Frame (SCHEME_OBJECT Temp)
{
  return (print_one_continuation_frame (ERROR_OUTPUT, Temp));
}

/* Back_Trace relies on (a) only a call to SAVE_CONT puts a return code on the
   stack; (b) SAVE_CONT pushes the expression first.  */

void
Back_Trace (outf_channel stream)
{
  SCHEME_OBJECT Temp, * Old_Stack;

  Old_Stack = stack_pointer;
  while (true)
    {
#if 0
      /* Not useful since this code prints the contents of control
	 points as well.  */
      if (!ADDRESS_IN_STACK_P (stack_pointer))
	{
	  if (stack_pointer == Old_Stack)
	    outf (stream, "\n[Invalid stack pointer.]\n");
	  else
	    outf (stream, "\n[Stack ends abruptly.]\n");
	  break;
	}
#endif
      Temp = (STACK_POP ());
      outf (stream, "{%#lx}", ((unsigned long) stack_pointer));
      if (RETURN_CODE_P (Temp))
	{
	  if (print_one_continuation_frame (stream, Temp))
	    break;
	}
      else
	{
	  print_expression (stream, Temp, "  ...");
	  if ((OBJECT_TYPE (Temp)) == TC_MANIFEST_NM_VECTOR)
	    {
	      outf (stream, " (skipping)");
	      stack_pointer = (STACK_LOC (OBJECT_DATUM (Temp)));
	    }
	  outf (stream, "\n");
	}
    }
  stack_pointer = Old_Stack;
  outf_flush (stream);
}

void
print_stack (SCHEME_OBJECT * sp)
{
  SCHEME_OBJECT * saved_sp = stack_pointer;
  stack_pointer = sp;
  Back_Trace (ERROR_OUTPUT);
  stack_pointer = saved_sp;
}

extern void
Debug_Stack_Trace(void)
{
  print_stack(STACK_LOC(0));
}

static bool
print_primitive_name (outf_channel stream, SCHEME_OBJECT primitive)
{
  const char * name = (PRIMITIVE_NAME (primitive));
  if (name == 0)
  {
    outf (stream, "Unknown primitive %#08lx", (PRIMITIVE_NUMBER (primitive)));
    return false;
  }
  else
  {
    outf (stream, "%s", name);
    return true;
  }
}

void
Print_Primitive (SCHEME_OBJECT primitive)
{
  char buffer[40];
  int NArgs, i;

  outf_error ("Primitive: ");
  if (print_primitive_name (ERROR_OUTPUT, primitive))
    NArgs = (PRIMITIVE_ARITY (primitive));
  else
    NArgs = 3;	        /* Unknown primitive */

  outf_error ("\n");

  for (i = 0; i < NArgs; i++)
  {
    sprintf (buffer, "...Arg %ld", ((long) (i + 1)));
    print_expression (ERROR_OUTPUT, (STACK_REF (i)), buffer);
    outf_error ("\n");
  }
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

#ifndef LAST_SWITCH
#define LAST_SWITCH D_BIGNUM
#endif

static bool *
find_flag (int flag_number)
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
    MORE_DEBUG_FLAG_CASES ();
    default:			return (0);
    }
}

static char *
flag_name (int flag_number)
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
    MORE_DEBUG_FLAG_NAMES ();
    default:			return ("Unknown Debug Flag");
    }
}

static void
show_flags (int all)
{
  unsigned int i;
  for (i = 0; (i <= LAST_SWITCH); i += 1)
    {
      int value = (* (find_flag (i)));
      if (all || value)
	outf_error ("Flag %u (%s) is %s.\n",
		      i, (flag_name (i)), (value ? "set" : "clear"));
    }
  outf_flush_error();
}

static int
set_flag (int flag_number, int value)
{
  bool * flag = (find_flag (flag_number));
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
debug_getdec (const char * string)
{
  int result;

  sscanf (string, "%d", (&result));
  return (result);
}

void
debug_edit_flags (void)
{
  char input_line [256];
  show_flags (0);
  while (1)
    {
      outf_error("Clear<number>, Set<number>, Done, ?, or Halt: ");
      outf_flush_error();
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
debug_edit_flags (void)
{
  outf_error ("Not a debugging version.  No flags to handle.\n");
  outf_flush_error();
}

static int
set_flag (int flag_number, int value)
{
  signal_error_from_primitive (ERR_UNIMPLEMENTED_PRIMITIVE);
  /*NOTREACHED*/
  return (0);
}

#endif /* not ENABLE_DEBUGGING_TOOLS */

DEFINE_PRIMITIVE("SET-DEBUG-FLAGS!", Prim_set_debug_flags, 2, 2,
  "(FLAG_NUMBER BOOLEAN)")
{
  PRIMITIVE_HEADER (2);
  set_flag ((arg_integer (1)), (BOOLEAN_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
