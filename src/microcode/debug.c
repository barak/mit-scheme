/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

static void print_object (outf_channel, SCHEME_OBJECT);
static bool print_primitive_name (outf_channel, SCHEME_OBJECT);
static void print_expression (outf_channel, SCHEME_OBJECT, const char *);

/* Compiled Code Debugging */

#ifdef CC_SUPPORT_P

const char *
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
Print_Return (const char * String)
{
  outf_error ("%s: ", String);
  print_return_name (ERROR_OUTPUT, GET_RET);
  outf_error ("\n");
}

static void
print_simple (outf_channel stream, SCHEME_OBJECT object)
{
  unsigned int type = (OBJECT_TYPE (object));
  const char * name = 0;
  if (type < TYPE_CODE_LIMIT)
    name = (type_names[type]);
  if (name != 0)
    outf (stream, "[%s", name);
  else
    outf (stream, "[%#02x", type);
  outf (stream, " %#lx]", OBJECT_DATUM (object));
}

static void
print_char (outf_channel stream, unsigned int cp)
{
  switch (cp)
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
      if ((cp >= ' ') && (cp <= '~'))
	outf (stream, "%c", cp);
      else
	outf (stream, "\\%03o", cp);
      break;
    }
}

static void
print_string (outf_channel stream, SCHEME_OBJECT string)
{
  long length, long_enough;
  long i;
  char * next;

  outf (stream, "\"");
  length = (STRING_LENGTH (string));
  long_enough = (length < 100 ? length : 90);
  next = (STRING_POINTER (string));
  for (i = 0; (i < long_enough); i += 1)
    print_char (stream, *next++);
  if (length != long_enough)
    outf (stream, "...[%ld total chars]", length);
  outf (stream, "\"");
}

static void
print_ustring (outf_channel stream, SCHEME_OBJECT string)
{
  long length, long_enough;
  long i;
  unsigned char * next;
  unsigned int cp;
  unsigned char cp_size;

  length = (STRING_LENGTH (string));
  long_enough = (length < 100 ? length : 90);
  next = (STRING_LOC (string, 0));

  cp_size
    = ((OBJECT_TYPE (MEMORY_REF (string, BYTEVECTOR_LENGTH_INDEX))) & 0x03);
  if (cp_size == 0)
    {
      print_simple (stream, string);
      return;
    }

  outf (stream, "\"");
  for (i = 0; (i < long_enough); i += 1)
    {
      switch (cp_size) {
      case 1:
	cp = *next++;
	break;
      case 2:
	cp = *next++;
	cp |= (*next++ << 8);
	break;
      case 3:
	cp = *next++;
	cp |= (*next++ << 8);
	cp |= (*next++ << 16);
	break;
      }
      print_char (stream, cp);
    }
  if (length != long_enough)
    outf (stream, "...[%ld total chars]", length);
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

DEFINE_PRIMITIVE ("DEBUGGING-PRINTER", Prim_debugging_printer, 1, 1,
  "A cheap, built-in printer intended for debugging the interpreter.")
{
  PRIMITIVE_HEADER (1);

  print_object (ERROR_OUTPUT, ARG_REF (1));
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
      outf_error ("%#lx: ", ((unsigned long) scan));
      print_object (ERROR_OUTPUT, (*scan++));
      outf_error ("\n");
    }
  outf_flush_error();
}

/* This is useful because `print_object' doesn't print the contents of
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
print_expression (outf_channel stream,
		  SCHEME_OBJECT expression, const char * string)
{
  if ((string [0]) != 0)
    outf (stream, "%s: ", string);
  print_object (stream, expression);
}

void
Print_Expression (SCHEME_OBJECT expression, const char * string)
{
  print_expression (ERROR_OUTPUT, expression, string);
}

static void
print_compiled_entry (outf_channel stream, SCHEME_OBJECT entry)
{
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
	    type_string = "compiled-closure";
	    entry = (cc_closure_to_entry (entry));
	    closure_p = true;
	  }
	else
	  type_string = "compiled-procedure";
	break;

      case CET_CONTINUATION:
	type_string = "compiled-return-address";
	break;

      case CET_EXPRESSION:
	type_string = "compiled-expression";
	break;

      case CET_INTERNAL_CONTINUATION:
	type_string = "compiled-return-address";
	break;

      case CET_INTERNAL_PROCEDURE:
      case CET_TRAMPOLINE:
	type_string = "compiled-entry";
	break;

      case CET_RETURN_TO_INTERPRETER:
	type_string = "compiled-return-address";
	break;

      default:
	type_string = "compiled-entry";
	break;
      }

  outf (stream, "[%s offset: %#lx entry: %#lx",
	type_string,
	(cc_entry_to_block_offset (entry)),
	(OBJECT_DATUM (entry)));
  if (closure_p)
    outf (stream, " address: %#lx", (OBJECT_DATUM (entry)));

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
}

static void
print_object (outf_channel stream, SCHEME_OBJECT obj)
{
  if (EMPTY_LIST_P (obj))	{ outf (stream, "()");	return; }
  else if (obj == SHARP_F)	{ outf (stream, "#F");	return; }
  else if (obj == SHARP_T)	{ outf (stream, "#T");	return; }
  else if (obj == UNSPECIFIC){ outf (stream, "[unspecific]"); return; }

  else if (obj == return_to_interpreter)
    {
      outf (stream, "[return-to-interpreter]");
      return;
    }

  else if (obj == reflect_to_interface)
    {
      outf (stream, "[reflect-to-interface]");
      return;
    }

  switch (OBJECT_TYPE (obj))
    {
    case TC_ACCESS:
      outf (stream, "[access ");
      print_symbol (stream, (MEMORY_REF (obj, ACCESS_NAME)));
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_ASSIGNMENT:
      outf (stream, "[set! ");
      print_symbol (stream, (MEMORY_REF ((MEMORY_REF (obj, ASSIGN_NAME)),
					 VARIABLE_SYMBOL)));
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_DEFINITION:
      outf (stream, "[define ");
      print_symbol (stream, (MEMORY_REF (obj, DEFINE_NAME)));
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_CHARACTER_STRING:
    case TC_BYTEVECTOR:
      print_string (stream, obj);
      return;

    case TC_UNICODE_STRING:
      print_ustring (stream, obj);
      return;

    case TC_FIXNUM:
      outf (stream, "%ld", ((long) (FIXNUM_TO_LONG (obj))));
      return;

    case TC_BIG_FLONUM:
      outf (stream, "%lf", (FLONUM_TO_DOUBLE (obj)));
      return;

    case TC_WEAK_CONS:
    case TC_LIST:
      print_list (stream, obj);
      return;

    case TC_FALSE:
      print_simple (stream, obj);
      return;

    case TC_UNINTERNED_SYMBOL:
      outf (stream, "[uninterned ");
      print_symbol (stream, obj);
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_INTERNED_SYMBOL:
      print_symbol (stream, obj);
      return;

    case TC_VARIABLE:
      outf (stream, "[variable ");
      print_symbol (stream, (MEMORY_REF (obj, VARIABLE_SYMBOL)));
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_COMBINATION:
      outf (stream, "[combination ");
      print_object (stream, (MEMORY_REF (obj, COMB_FN_SLOT)));
      outf (stream, " ... (%ld args)", (VECTOR_LENGTH (obj)) - 1);
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_ENVIRONMENT:
      outf (stream, "[environment from ");
      {
	SCHEME_OBJECT procedure;
	procedure = (MEMORY_REF (obj, ENVIRONMENT_FUNCTION));
	if ((OBJECT_TYPE (procedure)) == TC_QUAD)
	  procedure = (MEMORY_REF (procedure, ENV_EXTENSION_PROCEDURE));
	print_object (stream, procedure);
      }
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_EXTENDED_LAMBDA:
      outf (stream, "[extended-lambda ");
      print_object (stream, (MEMORY_REF ((MEMORY_REF (obj, ELAMBDA_NAMES)),
					 1)));
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_EXTENDED_PROCEDURE:
      outf (stream, "[extended-procedure ");
      print_object (stream, (MEMORY_REF (obj, PROCEDURE_LAMBDA_EXPR)));
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_LAMBDA:
      outf (stream, "[lambda ");
      print_object (stream, (MEMORY_REF ((MEMORY_REF (obj, LAMBDA_FORMALS)),
					 1)));
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_PRIMITIVE:
      outf (stream, "[primitive ");
      print_primitive_name (stream, obj);
      outf (stream, "]");
      return;

    case TC_PROCEDURE:
      outf (stream, "[procedure ");
      print_object (stream, (MEMORY_REF (obj, PROCEDURE_LAMBDA_EXPR)));
      outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
      return;

    case TC_REFERENCE_TRAP:
      if ((OBJECT_DATUM (obj)) <= TRAP_MAX_IMMEDIATE)
	print_simple (stream, obj);
      else
	{
	  outf (stream, "[reference-trap");
	  print_expression (stream, (MEMORY_REF (obj, TRAP_TAG)), " tag");
	  print_expression (stream, (MEMORY_REF (obj, TRAP_EXTRA)), " extra");
	  outf (stream, " %#lx]", (OBJECT_DATUM (obj)));
	}
      return;

    case TC_RETURN_CODE:
      outf (stream, "[return-code ");
      print_return_name (stream, obj);
      outf (stream, "]");
      return;

    case TC_CONSTANT:
      print_simple (stream, obj);
      return;

#ifdef CC_SUPPORT_P
    case TC_COMPILED_ENTRY:
      print_compiled_entry (stream, obj);
      return;
#endif

    default:
      print_simple (stream, obj);
    }
}

void
Print (SCHEME_OBJECT Expr)
{
  print_object (ERROR_OUTPUT, Expr);
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

/* Code to dump the Scheme stack. */

static void
dump_stack (outf_channel stream,
	    SCHEME_OBJECT *sp,
	    SCHEME_OBJECT *limit,
	    int count)
{
  int done = 0;
  while (((count == 0) || (done < count))
	 && (STACK_LOCATIVE_ABOVE_P (sp, limit)))
    {
      SCHEME_OBJECT obj;
      outf (stream, "%#lx: ", ((unsigned long) sp));
      obj = (STACK_LOCATIVE_POP (sp));
      print_object (stream, obj);
      outf (stream, "\n");
      done += 1;
      if ((RETURN_CODE_P (obj))
	  && ((OBJECT_DATUM (obj)) == RC_JOIN_STACKLETS))
	{
	  SCHEME_OBJECT cp = (STACK_LOCATIVE_POP (sp));
	  sp = (control_point_start (cp));
	  limit = (control_point_end (cp));
	}
    }
}

void
Stack (int count)
{
  dump_stack (ERROR_OUTPUT, stack_pointer, stack_end, count);
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
      outf (stream, "{%#lx}", ((unsigned long) stack_pointer));
      Temp = (STACK_POP ());
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

/* Code for scanning the heap for obviously broken or invalid objects. */

#ifdef ENABLE_DEBUGGING_TOOLS

static void
dump_word (SCHEME_OBJECT *addr)
{
  int i = 0;
  unsigned char * bytes = (unsigned char *)addr;
  outf_error ("%#lx: ", (unsigned long)addr);
  while (i < sizeof(SCHEME_OBJECT))
    {
      if (isgraph (bytes[i]))
	outf_error (" %2c", bytes[i]);
      else
	outf_error (" %02x", bytes[i]);
      i += 1;
    }
  outf_error ("\n");
}

static SCHEME_OBJECT *
next_addr (SCHEME_OBJECT * addr)
{
  SCHEME_OBJECT object = *addr;
  unsigned int type = OBJECT_TYPE (object);
  switch (type)
   {
#ifdef CC_SUPPORT_P
    case TC_LINKAGE_SECTION:
      {
	linkage_section_type_t section_type
	  = ((linkage_section_type_t)((OBJECT_DATUM (object)) >> 16));
	switch (section_type)
	  {
	  case LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR:
	  case LINKAGE_SECTION_TYPE_OPERATOR:
	    {
	      unsigned long n_words = ((OBJECT_DATUM (object)) & 0xFFFFUL);
	      return (addr + (1 + n_words));
	    }
	  case LINKAGE_SECTION_TYPE_REFERENCE:
	  case LINKAGE_SECTION_TYPE_ASSIGNMENT:
	    return (addr + 1);
	  default:
	    outf_error ("Invalid linkage section type: %d\n", section_type);
	    return (addr + 1);
	  }
      }
    case TC_MANIFEST_CLOSURE:
      return (compiled_closure_objects (addr + 1));
#endif /* CC_SUPPORT_P */
    case TC_MANIFEST_NM_VECTOR:
      {
	unsigned long n_words = (OBJECT_DATUM (object));
	return (addr + (1 + n_words));
      }
    default:
      return (addr + 1);
    }
}

static void
dump_object (SCHEME_OBJECT * addr)
{
  SCHEME_OBJECT object = *addr;
  outf_error ("%#lx: ", (unsigned long)addr);
  print_object (ERROR_OUTPUT, object);
  outf_error ("\n");
  {
    SCHEME_OBJECT * end = next_addr (addr);
    if (end > Free)
      end = Free;
    while (++addr < end)
      dump_word (addr);
  }
  if (addr == Free)
    outf_error ("%#lx: Free\n", (unsigned long)addr);
}

#define SAVE_COUNT 16
static SCHEME_OBJECT *saved_addrs[SAVE_COUNT];
static int saved_index;

static void
dump_heap_area_at (SCHEME_OBJECT * addr,
		   SCHEME_OBJECT * area, SCHEME_OBJECT * end)
{
  SCHEME_OBJECT * scan = area;
  assert (area <= addr && addr < end);
  saved_index = 0;
  while (scan < addr)
    {
      saved_addrs[saved_index++] = scan;
      if (saved_index == SAVE_COUNT) saved_index = 0;
      scan = next_addr (scan);
    }
  {
    int i = saved_index;
    do
      {
	SCHEME_OBJECT * saved = saved_addrs[i++];
	if (i == SAVE_COUNT) i = 0;
	dump_object (saved);
      }
    while (i != saved_index);
  }
  outf_error ("=> ");
  {
    int i = 0;
    while (i < SAVE_COUNT && scan < end)
      {
	dump_object (scan);
	i += 1;
	scan = next_addr (scan);
      }
  }
}

void
dump_heap_at (SCHEME_OBJECT *addr)
{
  if (constant_start <= addr && addr < constant_alloc_next)
    {
      outf_error ("Scanning constant area (%#lx - %#lx):\n",
		  (unsigned long)constant_start,
		  (unsigned long)constant_alloc_next);
      dump_heap_area_at (addr, constant_start, constant_alloc_next);
    }
  else if (heap_start <= addr && addr < Free)
    {
      outf_error ("Scanning heap area (%#lx - %#lx):\n",
		  (unsigned long)heap_start,
		  (unsigned long)Free);
      dump_heap_area_at (addr, heap_start, Free);
    }
  else
    {
      outf_error ("%#lx: not a heap address\n", (unsigned long)addr);
    }
  outf_flush_error ();
}

static bool
verify_object (SCHEME_OBJECT object)
{
  return (gc_type_code (OBJECT_TYPE (object)) != GC_UNDEFINED);    
}

#define VALID_ADDRESS_P(address)					\
  ((ADDRESS_IN_CONSTANT_P (address))					\
   || ((heap_start <= (address)) && ((address) < Free)))

static bool
verify_tuple (SCHEME_OBJECT object, int size, const char * name,
	      unsigned long address)
{
  SCHEME_OBJECT * location;
  int i;

  location = OBJECT_ADDRESS (object);
  if (! (VALID_ADDRESS_P (location)))
    {
      outf_error ("%#lx: Invalid %s\n", address, name);
      return (false);
    }
  i = 0;
  while (i < size)
    {
      SCHEME_OBJECT * slot = MEMORY_LOC (object, i);
      if (! ((VALID_ADDRESS_P (slot)) && verify_object (*slot)))
	{
	  outf_error ("%#lx: Invalid %s (word %d)\n", address, name, i);
	  return (false);
	}
      i += 1;
    }
  return (true);
}

static bool
verify_vector (SCHEME_OBJECT object, unsigned long address)
{
  unsigned long header, length;
  unsigned int header_type;
  SCHEME_OBJECT * location;

  location = OBJECT_ADDRESS (object);
  if (! (VALID_ADDRESS_P (location)))
    {
      outf_error ("%#lx: Invalid vector\n", address);
      return (false);
    }
  header = MEMORY_REF (object, 0);
  length = OBJECT_DATUM (header);
  header_type = OBJECT_TYPE (header);
  if (header_type != TC_MANIFEST_VECTOR
      && header_type != TC_MANIFEST_NM_VECTOR)
    {
      outf_error ("%#lx: Invalid vector header\n", address);
      return (false);
    }
  if (! (VALID_ADDRESS_P (location + length)))
    {
      outf_error ("%#lx: Invalid vector length\n", address);
      return (false);
    }
  /* Double-check each element? */
  if (length > 1000000)
    {
      outf_error ("%#lx: Extraordinary vector size: %ld\n", address, length);
    }
  return (true);
}

#ifdef CC_SUPPORT_P
static bool
verify_compiled (SCHEME_OBJECT object, unsigned long address)
{
  insn_t * block;

  if (! (VALID_ADDRESS_P (OBJECT_ADDRESS (object))))
    {
      outf_error ("%#lx: Invalid entry\n", address);
      return (false);
    }
  /* block = cc_entry_to_block_address (object);  too many SIGSEGVs! */
  block = CC_ENTRY_ADDRESS (object);
  while (1)
    {
      cc_entry_offset_t ceo;
      if (read_cc_entry_offset ((&ceo), block))
	{
	  outf_error ("%#lx: Invalid entry format\n", address);
	  return (false);
	}
      assert (ceo.offset > 0);
      block -= (ceo.offset);
      if (! (VALID_ADDRESS_P ((SCHEME_OBJECT *)block)))
	{
	  outf_error ("%#lx: Invalid entry offset\n", address);
	  return (false);
	}
      if (! (ceo.continued_p))
	{
	  unsigned int header_type;

	  if ((unsigned long)block % sizeof (SCHEME_OBJECT) != 0)
	    {
	      outf_error ("%#lx: Invalid block alignment\n", address);
	      return (false);
	    }
	  if ((CC_BLOCK_ADDR_END ((SCHEME_OBJECT *) block))
	      < ((SCHEME_OBJECT *) (CC_ENTRY_ADDRESS (object))))
	    {
	      outf_error ("%#lx: Invalid block size\n", address);
	      return (false);
	    }
	  header_type = OBJECT_TYPE(*((SCHEME_OBJECT *) block));
	  if (! (header_type == TC_MANIFEST_VECTOR
		 || header_type == TC_MANIFEST_CLOSURE))
	    {
	      outf_error ("%#lx: Invalid block header\n", address);
	      return (false);
	    }
	  break;
	}
    }
  return (true);
}
#endif /* CC_SUPPORT_P */

static bool
verify_heap_area (const char * name, SCHEME_OBJECT * area, SCHEME_OBJECT * end)
{
  int complaints = 0;

  while (area < end)
    {
      SCHEME_OBJECT object = *area;
      unsigned int type = OBJECT_TYPE (object);
      unsigned int code = gc_type_code (type);
      switch (code)
	{
	case GC_UNDEFINED:
	  outf_error ("%#lx: Invalid object type: %#x\n",
		      (unsigned long)area, type);
	  complaints += 1;
	  area += 1;
	  return (false);

	case GC_NON_POINTER:
	  area += 1;
	  break;

	case GC_CELL:
	  if (! verify_tuple (object, 1, "cell", (unsigned long)area))
	    complaints += 1;
	  area += 1;
	  break;

	case GC_PAIR:
	  if (! verify_tuple (object, 2, "pair", (unsigned long)area))
	    complaints += 1;
	  area += 1;
	  break;

	case GC_TRIPLE:
	  if (! verify_tuple (object, 3, "triple", (unsigned long)area))
	    complaints += 1;
	  area += 1;
	  break;

	case GC_QUADRUPLE:
	  if (! verify_tuple (object, 4, "quadruple", (unsigned long)area))
	    complaints += 1;
	  area += 1;
	  break;

	case GC_VECTOR:
	  if (! verify_vector (object, (unsigned long)area))
	    complaints += 1;
	  area += 1;
	  break;

	case GC_SPECIAL:
	  switch (type)
	    {
	    case TC_BROKEN_HEART:
	      /* These are not a problem??? */
#if 0
	      outf_error ("%#lx: Invalid broken-heart\n", (unsigned long)area);
	      complaints += 1;
#endif
	      area += 1;
	      break;

	    case TC_REFERENCE_TRAP:
	      if ((OBJECT_DATUM (object)) > TRAP_MAX_IMMEDIATE)
		{
		  if (! (verify_object (MEMORY_REF (object, 0))
			 && verify_object (MEMORY_REF (object, 1))))
		    {
		      outf_error ("%#lx: Invalid reference trap\n",
				  (unsigned long)area);
		      complaints += 1;
		    }
		}
	      area += 1;
	      break;

	    case TC_LINKAGE_SECTION:
	      {
		linkage_section_type_t section_type
		  = ((linkage_section_type_t)((OBJECT_DATUM (object)) >> 16));
		switch (section_type)
		  {
		  case LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR:
		  case LINKAGE_SECTION_TYPE_OPERATOR:
		    {
		      unsigned long n_words
			= ((OBJECT_DATUM (object)) & 0xFFFFUL);
		      SCHEME_OBJECT * next = area + (1 + n_words);
		      if (next > end)
			{
			  outf_error ("%#lx: Invalid linkage section size: %ld\n",
				      (unsigned long)area, n_words);
			  return (false);
			}
		      else if (n_words > 1000)
			{
			  outf_error
			    ("%#lx: Extraordinary linkage section size: %ld\n",
			     (unsigned long)area, n_words);
			}
		      area = next;
		    }
		    break;
		  case LINKAGE_SECTION_TYPE_REFERENCE:
		  case LINKAGE_SECTION_TYPE_ASSIGNMENT:
		    area += 1;
		    break;
		  default:
		    outf_error ("%#lx: Invalid linkage section type: %#x\n",
				(unsigned long)area, section_type);
		    complaints += 1;
		    area += 1;
		    break;
		  }
	      }
	      break;

	    case TC_MANIFEST_CLOSURE:
	      area = compiled_closure_objects (area + 1);
	      break;

	    case TC_MANIFEST_NM_VECTOR:
	      {
		unsigned long n_words = (OBJECT_DATUM (object));
		SCHEME_OBJECT * next = area + (1 + n_words);
		if (next > end)
		  {
		    outf_error ("%#lx: Invalid nm-vector size: %ld\n",
				(unsigned long)area, n_words);
		    return (false);
		  }
		else if (n_words > 1000000)
		  {
		    outf_error ("%#lx: Extraordinary nm-vector size: %ld\n",
				(unsigned long)area, n_words);
		  }
		area = next;
	      }
	      break;
	      
	    default:
	      outf_error ("%#lx: Invalid special\n", (unsigned long)area);
	      complaints += 1;
	      area += 1;
	      break;
	    }
	  break;

#ifdef CC_SUPPORT_P
	case GC_COMPILED:
	  if (! verify_compiled (object, (unsigned long)area))
	    complaints += 1;
	  area += 1;
	  break;
#endif

	default:
	  outf_error ("%#lx: unknown gc type code: %#x\n",
		      (unsigned long)area, gc_type_code (type));
	  complaints += 1;
	}
    }

  if (area != end)
    {
      outf_error ("%#lx: Invalid end address\n", (unsigned long)area);
      return (false);
    }
  return (complaints == 0);
}

bool
verify_stack (SCHEME_OBJECT * sp, SCHEME_OBJECT * bottom)
{
  int complaints = 0;
  while (STACK_LOCATIVE_ABOVE_P (sp, bottom))
    {
      SCHEME_OBJECT object = STACK_LOCATIVE_POP (sp);
      unsigned int type = OBJECT_TYPE (object);
      if (type == TC_MANIFEST_NM_VECTOR)
	{
	  unsigned long n_words = (OBJECT_DATUM (object));
	  if (n_words > 1000)
	    outf_error ("%#lx: Extraordinary finger size: %ld\n",
			((unsigned long)sp), n_words);
	  sp = STACK_LOCATIVE_OFFSET (sp, n_words);
	}
      else if (type == TC_MANIFEST_CLOSURE
	       || type == TC_BROKEN_HEART
	       || gc_type_code (type) == GC_UNDEFINED)
	{
	  outf_error ("%#lx: Invalid stack slot: ", ((unsigned long)sp));
	  print_object (ERROR_OUTPUT, object);
	  outf_error ("\n");
	  complaints += 1;
	}
    }
  return (complaints == 0);
}

bool
verify_heap (void)
{
  bool c = verify_heap_area ("constants", constant_start, constant_alloc_next);
  bool h = verify_heap_area ("heap", heap_start, Free);
  bool s = verify_stack (stack_pointer, STACK_BOTTOM);
  outf_flush_error ();
  return (c && h && s);
}

#else  /* !ENABLE_DEBUGGING_TOOLS */

bool
verify_heap (void)
{
  return true;
}

#endif

DEFINE_PRIMITIVE ("VERIFY-HEAP", Prim_verify_heap, 0, 0,
		  "Validate the heap.\n\
Complains if a scan of the heap encounters anything unexpected.\n\
Returns #T if the scan was successful and #F if there were any complaints.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (verify_heap () ? SHARP_T : SHARP_F);
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
#define D_PRINT_ERRORS		15
#ifndef LAST_SWITCH
#define LAST_SWITCH D_PRINT_ERRORS
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
    case D_PRINT_ERRORS:	return (&Print_Errors);
    MORE_DEBUG_FLAG_CASES ();
    default:			return (0);
    }
}

static const char *
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
    case D_PRINT_ERRORS:	return ("Print_Errors");
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

static void
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

static void
set_flag (int flag_number, int value)
{
  outf_error ("Not a debugging version.  No flags to set.\n");
  outf_flush_error();
}

#endif /* not ENABLE_DEBUGGING_TOOLS */

DEFINE_PRIMITIVE("SET-DEBUG-FLAGS!", Prim_set_debug_flags, 2, 2,
  "(FLAG_NUMBER BOOLEAN)")
{
  PRIMITIVE_HEADER (2);
  set_flag ((arg_integer (1)), (BOOLEAN_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
