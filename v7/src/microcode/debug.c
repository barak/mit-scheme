/* -*-C-*-

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/debug.c,v 9.30 1988/10/26 21:08:26 cph Exp $
 *
 * Utilities to help with debugging
 */

#include "scheme.h"
#include "prims.h"
#include "trap.h"
#include "lookup.h"

/* Compiled Code Debugging */

static Pointer
compiled_block_debug_filename (block)
     Pointer block;
{
  extern Pointer compiled_block_debugging_info ();
  Pointer info;

  info = (compiled_block_debugging_info (block));
  return
    (((STRING_P (info)) ||
      ((PAIR_P (info)) &&
       (STRING_P (Vector_Ref (info, CONS_CAR))) &&
       (FIXNUM_P (Vector_Ref (info, CONS_CDR)))))
     ? info
     : SHARP_F);
}

#define COMPILED_ENTRY_TO_BLOCK(entry)					\
(Make_Pointer (TC_COMPILED_CODE_BLOCK,					\
	       (compiled_entry_to_block_address (entry))))

static Pointer
compiled_entry_debug_filename (entry)
     Pointer entry;
{
  Pointer results [3];
  extern void compiled_entry_type ();
  extern long compiled_entry_manifest_closure_p ();
  extern long compiled_entry_to_block_offset ();
  extern Pointer compiled_closure_to_entry ();

  compiled_entry_type (entry, (& results));
  if (((results [0]) == 0) && (compiled_entry_manifest_closure_p (entry)))
    entry = (compiled_closure_to_entry (entry));
  return (compiled_block_debug_filename (COMPILED_ENTRY_TO_BLOCK (entry)));
}

char *
compiled_entry_filename (entry)
     Pointer entry;
{
  Pointer result;

  result = (compiled_entry_debug_filename (entry));
  if (STRING_P (result))
    return (Scheme_String_To_C_String (result));
  else if (PAIR_P (result))
    return (Scheme_String_To_C_String (Vector_Ref (result, CONS_CAR)));
  else
    return ("**** filename not known ****");
}

void
Show_Pure ()
{
  Pointer *Obj_Address;
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
    Pure_Size = Get_Integer(*Obj_Address);
    Total_Size = Get_Integer(Obj_Address[1]);
    printf ("0x%x: pure=0x%x, total=0x%x\n",
           Obj_Address, Pure_Size, Total_Size);
    if (Type_Code(*Obj_Address) != TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      printf ("Missing initial SNMV.\n");
      return;
    }
    if (Type_Code(Obj_Address[1]) != PURE_PART)
    {
      printf ("Missing subsequent pure header.\n");
    }
    if (Type_Code(Obj_Address[Pure_Size-1]) !=
        TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      printf ("Missing internal SNMV.\n");
      return;
    }
    if (Type_Code(Obj_Address[Pure_Size]) != CONSTANT_PART)
    {
      printf ("Missing constant header.\n");
      return;
    }
    if (Get_Integer(Obj_Address[Pure_Size]) != Pure_Size)
    {
      printf ("Pure size mismatch 0x%x.\n",
	     Get_Integer(Obj_Address[Pure_Size]));
    }
    if (Type_Code(Obj_Address[Total_Size-1]) !=
        TC_MANIFEST_SPECIAL_NM_VECTOR)
    {
      printf ("Missing ending SNMV.\n");
      return;
    }
    if (Type_Code(Obj_Address[Total_Size]) != END_OF_BLOCK)
    {
      printf ("Missing ending header.\n");
      return;
    }
    if (Get_Integer(Obj_Address[Total_Size]) != Total_Size)
    {
      printf ("Total size mismatch 0x%x.\n",
             Get_Integer(Obj_Address[Total_Size]));
    }
    Obj_Address += Total_Size+1;
#ifdef FLOATING_ALIGNMENT
    while (*Obj_Address == Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, 0))
    {
      Obj_Address += 1;
    }
#endif
  }
}

void
Show_Env (The_Env)
     Pointer The_Env;
{
  Pointer *name_ptr, procedure, *value_ptr, extension;
  long count, i;

  procedure = Vector_Ref (The_Env, ENVIRONMENT_FUNCTION);
  value_ptr = Nth_Vector_Loc(The_Env, ENVIRONMENT_FIRST_ARG);

  if (Type_Code(procedure) == AUX_LIST_TYPE)
  {
    extension = procedure;
    procedure = Fast_Vector_Ref (extension, ENV_EXTENSION_PROCEDURE);
  }
  else
    extension = NIL;

  if ((Type_Code(procedure) != TC_PROCEDURE) &&
      (Type_Code(procedure) != TC_EXTENDED_PROCEDURE))
  {
    printf ("Not created by a procedure");
    return;
  }
  name_ptr = Nth_Vector_Loc(procedure, PROCEDURE_LAMBDA_EXPR);
  name_ptr = Nth_Vector_Loc(*name_ptr, LAMBDA_FORMALS);
  count = Vector_Length(*name_ptr) - 1;

  name_ptr = Nth_Vector_Loc(*name_ptr, 2);
  for (i = 0; i < count; i++)
  {
    Print_Expression(*name_ptr++, "Name ");
    Print_Expression(*value_ptr++, " Value ");
    printf ("\n");
  }
  if (extension != NIL)
  {
    printf ("Auxilliary Variables\n");
    count = Get_Integer(Vector_Ref (extension, AUX_LIST_COUNT));
    for (i = 0, name_ptr = Nth_Vector_Loc(extension, AUX_LIST_FIRST);
	 i < count;
	 i++, name_ptr++)
    {
      Print_Expression(Vector_Ref (*name_ptr, CONS_CAR),
		       "Name ");
      Print_Expression(Vector_Ref (*name_ptr, CONS_CAR),
		       " Value ");
      printf ("\n");
    }
  }
}

int
List_Print (Expr)
     Pointer Expr;
{ int Count;
  Count = 0;
  printf ("(");
  while (((Type_Code(Expr) == TC_LIST) ||
	  (Type_Code(Expr) == TC_WEAK_CONS))
	  && Count < MAX_LIST_PRINT)
  { Print_Expression(Vector_Ref (Expr, CONS_CAR),
		     (Type_Code(Expr)==TC_LIST) ? "" : "{weak}");
    Expr = Vector_Ref (Expr, CONS_CDR);
    if (Type_Code(Expr) != TC_NULL) printf (" ");
    Count += 1;
  }
  if (Type_Code(Expr) != TC_NULL)
  { if (Count==MAX_LIST_PRINT) printf ("...");
    else
    { printf (". ");
      Print_Expression(Expr, "");
    }
  }
  printf (")");
}

static void
print_vector (vector)
     Pointer vector;
{
  long length;
  long limit;
  long index;

  length = (UNSIGNED_FIXNUM_VALUE (Fast_Vector_Ref ((vector), 0)));
  limit = ((length > MAX_LIST_PRINT) ? MAX_LIST_PRINT : length);
  index = 0;
  printf ("#(");
  while (1)
    {
      if (index >= limit)
	break;
      Print_Expression ((User_Vector_Ref (vector, index)), "");
      index += 1;
      if (index < limit)
	printf (" ");
    }
  if (limit < length)
    printf (" ...");
  printf (")");
  return;
}

void
Print_Return_Name (Ptr)
     Pointer Ptr;
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
	  printf ("%s", name);
	  return;
	}
    }
  printf ("[0x%x]", index);
  return;
}

void
Print_Return (String)
     char * String;
{
  printf ("%s: ", String);
  Print_Return_Name (Fetch_Return ());
  CRLF ();
}

static void
print_string (string)
     Pointer string;
{
  long length;
  long i;
  char * next;
  char this;

  printf ("\"");
  length = ((long) (Vector_Ref (string, STRING_LENGTH)));
  next = ((char *) (Nth_Vector_Loc (string, STRING_CHARS)));
  for (i = 0; (i < length); i += 1)
    {
      this = (*next++);
      switch (this)
	{
	case '\\':
	  printf ("\\\\");
	  break;
	case '"':
	  printf ("\\\"");
	  break;
	case '\t':
	  printf ("\\t");
	  break;
	case '\n':
	  printf ("\\n");
	  break;
	case '\f':
	  printf ("\\f");
	  break;
	default:
	  if ((this >= ' ') && (this <= '~'))
	    putchar (this);
	  else
	    printf ("\\%03o", this);
	  break;
	}
    }
  printf ("\"");
  return;
}

static void
print_symbol (symbol)
     Pointer symbol;
{
  Pointer string;
  long length;
  long i;
  char * next;

  string = (Vector_Ref (symbol, SYMBOL_NAME));
  length = ((long) (Vector_Ref (string, STRING_LENGTH)));
  next = ((char *) (Nth_Vector_Loc (string, STRING_CHARS)));
  for (i = 0; (i < length); i += 1)
    putchar (*next++);
  return;
}

static void
print_filename (filename)
     Pointer filename;
{
  long length;
  char * scan;
  char * end;
  char * slash;

  length = ((long) (Vector_Ref (filename, STRING_LENGTH)));
  scan = ((char *) (Nth_Vector_Loc (filename, STRING_CHARS)));
  end = (scan + length);
  slash = scan;
  while (scan < end)
    if ((*scan++) == '/')
      slash = scan;
  printf ("\"%s\"", slash);
  return;
}

void
print_object (object)
     Pointer object;
{
  fflush (stdout);
  Do_Printing (object, true);
  printf ("\n");
  fflush (stdout);
  return;
}

extern Boolean Prt_PName ();

void
Print_Expression (Expr, String)
     char *String;
     Pointer Expr;
{
  if (String[0] != 0)
  {
    printf ("%s: ", String);
  }
  Do_Printing (Expr, true);
}

extern char * Type_Names [];

int
Do_Printing (Expr, Detailed)
     Pointer Expr;
     Boolean Detailed;
{
  long Temp_Address;
  Boolean handled_p;

  Temp_Address = (OBJECT_DATUM (Expr));
  handled_p = false;

  switch (OBJECT_TYPE (Expr))
    {
    case TC_ACCESS:
      {
	printf ("[ACCESS (");
	Expr = (Vector_Ref (Expr, ACCESS_NAME));
      SPrint:
	print_symbol (Expr);
	handled_p = true;
	printf (")");
	break;
      }

    case TC_ASSIGNMENT:
      printf ("[SET! (");
      Expr = (Vector_Ref ((Vector_Ref (Expr, ASSIGN_NAME)), VARIABLE_SYMBOL));
      goto SPrint;

    case TC_CHARACTER_STRING:
      print_string (Expr);
      return;

    case TC_DEFINITION:
      printf ("[DEFINE (");
      Expr = (Vector_Ref (Expr, DEFINE_NAME));
      goto SPrint;

    case TC_FIXNUM:
      {
	long a;

	Sign_Extend (Expr, a);
	printf ("%d", a);
	return;
      }

    case TC_BIG_FLONUM:
      printf ("%f", (Get_Float (Expr)));
      return;

    case TC_WEAK_CONS:
    case TC_LIST:
      List_Print (Expr);
      return;

#if 0
      /* This would be nice except that named structures are
	 represented by vectors, so they print out their contents
	 instead of their usual printed representation.  */
    case TC_VECTOR:
      print_vector (Expr);
      return;
#endif

    case TC_NULL:
      if (Temp_Address == 0)
	{
	  printf ("()");
	  return;
	}
      break;

    case TC_UNINTERNED_SYMBOL:
      printf ("[UNINTERNED_SYMBOL (");
      goto SPrint;

    case TC_INTERNED_SYMBOL:
      print_symbol (Expr);
      return;

    case TC_VARIABLE:
      Expr = (Vector_Ref (Expr, VARIABLE_SYMBOL));
      if (Detailed)
	{
	  printf ("[VARIABLE (");
	  goto SPrint;
	}
      print_symbol (Expr);
      return;

    case TC_COMBINATION:
      printf ("[COMBINATION (%d args) 0x%x]",
	      ((Vector_Length (Expr)) - 1),
	      Temp_Address);
      if (Detailed)
	{
	  printf (" (");
	  Do_Printing ((Vector_Ref (Expr, COMB_FN_SLOT)), false);
	  printf (" ...)");
	}
      return;

    case TC_COMBINATION_1:
      printf ("[COMBINATION_1 0x%x]", Temp_Address);
      if (Detailed)
	{
	  printf (" (");
	  Do_Printing ((Vector_Ref (Expr, COMB_1_FN)), false);
	  printf (", ");
	  Do_Printing ((Vector_Ref (Expr, COMB_1_ARG_1)), false);
	  printf (")");
	}
      return;

    case TC_COMBINATION_2:
      printf ("[COMBINATION_2 0x%x]", Temp_Address);
      if (Detailed)
	{
	  printf (" (");
	  Do_Printing ((Vector_Ref (Expr, COMB_2_FN)), false);
	  printf (", ");
	  Do_Printing ((Vector_Ref (Expr, COMB_2_ARG_1)), false);
	  printf (", ");
	  Do_Printing ((Vector_Ref (Expr, COMB_2_ARG_2)), false);
	  printf (")");
	}
      return;

    case TC_ENVIRONMENT:
      {
	Pointer procedure;

	printf ("[ENVIRONMENT 0x%x]", Temp_Address);
	printf (" (from ");
	procedure = (Vector_Ref (Expr, ENVIRONMENT_FUNCTION));
	if ((Type_Code (procedure)) == TC_QUAD)
	  procedure = (Vector_Ref (procedure, ENV_EXTENSION_PROCEDURE));
	Do_Printing (procedure, false);
	printf (")");
	return;
      }

    case TC_EXTENDED_LAMBDA:
      if (Detailed)
	printf ("[EXTENDED_LAMBDA (");
      Do_Printing ((Vector_Ref ((Vector_Ref (Expr, ELAMBDA_NAMES)), 1)),
		   false);
      if (Detailed)
	printf (") 0x%x", Temp_Address);
      return;

    case TC_EXTENDED_PROCEDURE:
      if (Detailed)
	printf ("[EXTENDED_PROCEDURE (");
      Do_Printing ((Vector_Ref (Expr, PROCEDURE_LAMBDA_EXPR)), false);
      if (Detailed)
	printf (") 0x%x]", Temp_Address);
      break;

    case TC_LAMBDA:
      if (Detailed)
	printf ("[LAMBDA (");
      Do_Printing ((Vector_Ref ((Vector_Ref (Expr, LAMBDA_FORMALS)), 1)),
		  false);
      if (Detailed)
	printf (") 0x%x]", Temp_Address);
      return;

    case TC_PRIMITIVE:
      printf ("[PRIMITIVE ");
      Prt_PName (Expr);
      printf ("]");
      return;

    case TC_PROCEDURE:
      if (Detailed)
	printf ("[PROCEDURE (");
      Do_Printing ((Vector_Ref (Expr, PROCEDURE_LAMBDA_EXPR)), false);
      if (Detailed)
	printf (") 0x%x]", Temp_Address);
      return;

    case TC_REFERENCE_TRAP:
      {
	if ((Datum (Expr)) <= TRAP_MAX_IMMEDIATE)
	  break;
	printf ("[REFERENCE-TRAP");
	Print_Expression ((Vector_Ref (Expr, TRAP_TAG)), " tag");
	Print_Expression ((Vector_Ref (Expr, TRAP_EXTRA)), " extra");
	printf ("]");
	return;
      }

    case TC_RETURN_CODE:
      printf ("[RETURN_CODE ");
      Print_Return_Name (Expr);
      printf ("]");
      return;

    case TC_TRUE:
      if (Temp_Address == 0)
	{
	  printf ("#T");
	  return;
	}
      break;

    case TC_COMPILED_ENTRY:
      {
	extern void compiled_entry_type ();
	extern long compiled_entry_manifest_closure_p ();
	extern long compiled_entry_to_block_offset ();
	extern Pointer compiled_closure_to_entry ();

	Pointer results [3];
	char * type_string;
	Pointer filename;
	Pointer entry;
	Boolean closure_p;

	entry = Expr;
	closure_p = false;
	compiled_entry_type (entry, (& results));
	switch (results [0])
	  {
	  case 0:
	    if (compiled_entry_manifest_closure_p (entry))
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

	printf ("[%s offset: 0x%x entry: 0x%x",
		type_string,
		(compiled_entry_to_block_offset (entry)),
		(OBJECT_DATUM (entry)));
	if (closure_p)
	  printf (" address: 0x%x", Temp_Address);

	filename = (compiled_entry_debug_filename (entry));
	if (STRING_P (filename))
	  {
	    printf (" file: ");
	    print_filename (filename);
	  }
	else if (PAIR_P (filename))
	  {
	    int block_number;

	    printf (" file: ");
	    print_filename (Vector_Ref (filename, CONS_CAR));
	    FIXNUM_VALUE ((Vector_Ref (filename, CONS_CDR)), block_number);
	    printf (" block: %d", block_number);
	  }
	printf ("]");
	return;
      }

    default:
      break;
    }
  if (! handled_p)
    {
      if ((OBJECT_TYPE (Expr)) <= LAST_TYPE_CODE)
	printf ("[%s", (Type_Names [OBJECT_TYPE (Expr)]));
      else
	printf ("[0x%02x", (OBJECT_TYPE (Expr)));
    }
  printf (" 0x%x]", Temp_Address);
  return;
}

Boolean
Print_One_Continuation_Frame (Temp)
     Pointer Temp;
{
  Pointer Expr;

  Print_Expression (Temp, "Return code");
  CRLF ();
  Expr = (Pop ());
  Print_Expression (Expr, "Expression");
  printf ("\n");
  if (((Datum (Temp)) == RC_END_OF_COMPUTATION) ||
      ((Datum (Temp)) == RC_HALT))
    return (true);
  if ((Datum (Temp)) == RC_JOIN_STACKLETS)
    Stack_Pointer = (Previous_Stack_Pointer (Expr));
  return (false);
}

/* Back_Trace relies on (a) only a call to Save_Cont puts a return code on the
   stack; (b) Save_Cont pushes the expression first.

   NOTE: currently Back_Trace ignores where and always
   prints on stdout.  This should eventually be fixed.
 */

void
Back_Trace (where)
     FILE *where;
{
  Pointer Temp, *Old_Stack;

  fflush (stdout);
  Back_Trace_Entry_Hook();
  Old_Stack = Stack_Pointer;
  while (true)
  {
    if (Return_Hook_Address == &Top_Of_Stack())
    {
      Temp = Pop();
      if (Temp != Make_Non_Pointer(TC_RETURN_CODE, RC_RETURN_TRAP_POINT))
      {
        printf ("\n--> Return trap is missing here <--\n");
      }
      else
      {
	printf ("\n[Return trap found here as expected]\n");
        Temp = Old_Return_Code;
      }
    }
    else
    {
      Temp = Pop();
    }
    if (Type_Code(Temp) == TC_RETURN_CODE)
    {
      if (Print_One_Continuation_Frame(Temp))
      {
	break;
      }
    }
    else
    {
      Print_Expression(Temp, "  ...");
      if (Type_Code(Temp) == TC_MANIFEST_NM_VECTOR)
      {
	Stack_Pointer = Simulate_Popping(Get_Integer(Temp));
        printf (" (skipping)");
      }
      printf ("\n");
    }
  }
  Stack_Pointer = Old_Stack;
  Back_Trace_Exit_Hook();
  fflush (stdout);
  return;
}

void
Print_Stack (SP)
     Pointer *SP;
{
  Pointer *Saved_SP;

  Saved_SP = Stack_Pointer;
  Stack_Pointer = SP;
  Back_Trace(stdout);
  Stack_Pointer = Saved_SP;
  return;
}

Boolean
Prt_PName (primitive)
     Pointer primitive;
{
  extern char *primitive_to_name();
  char *name;

  name = primitive_to_name(primitive);
  if (name == ((char *) NULL))
  {
    printf ("Unknown primitive 0x%08x", PRIMITIVE_NUMBER(primitive));
    return false;
  }
  else
  {
    printf ("%s", name);
    return true;
  }
}

void
Print_Primitive (primitive)
     Pointer primitive;
{
  extern long primitive_to_arity();
  char buffer1[40], buffer2[40];
  int NArgs, i;

  printf ("Primitive: ");
  if (Prt_PName(primitive))
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
    sprintf (buffer1, "Stack_Ref(%d)", i);
    sprintf (buffer2, "...Arg %d", (i + 1));
    Print_Expression(buffer1, buffer2);
    printf ("\n");
  }
}

int
Debug_Printer (Expr)
     Pointer Expr;
{
  Print_Expression(Expr, "");
  putchar('\n');
}

DEFINE_PRIMITIVE ("DEBUGGING-PRINTER", Prim_debugging_printer, 1, 1,
  "A cheap, built-in printer intended for debugging the interpreter.")
{
  PRIMITIVE_HEADER (1);

  Debug_Printer (ARG_REF (1));
  return (SHARP_T);
}

/* Code for interactively setting and clearing the interpreter
   debugging flags.  Invoked via the "D" command to the ^B
   handler or during each FASLOAD.
*/

#ifdef ENABLE_DEBUGGING_TOOLS
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
#define LAST_NORMAL_SWITCH	15

Boolean *
Find_Flag (Num)
     int Num;
{ switch (Num)
  { case D_EVAL:	return &Eval_Debug;
    case D_HEX_INPUT:	return &Hex_Input_Debug;
    case D_FILE_LOAD:	return &File_Load_Debug;
    case D_RELOC:	return &Reloc_Debug;
    case D_INTERN: 	return &Intern_Debug;
    case D_CONT:	return &Cont_Debug;
    case D_PRIMITIVE:	return &Primitive_Debug;
    case D_LOOKUP:	return &Lookup_Debug ;
    case D_DEFINE:	return &Define_Debug;
    case D_GC:		return &GC_Debug;
    case D_UPGRADE:	return &Upgrade_Debug;
    case D_DUMP:	return &Dump_Debug;
    case D_TRACE_ON_ERROR: return &Trace_On_Error;
    case D_PER_FILE:	return &Per_File;
    case D_BIGNUM:      return &Bignum_Debug;
    case D_FLUIDS:      return &Fluids_Debug;
    More_Debug_Flag_Cases();
    default:		show_flags(true); return NULL;
  }
}

int
set_flag (Num, Value)
     int Num;
     Boolean Value;
{ Boolean *Flag = Find_Flag(Num);
  if (Flag != NULL) *Flag = Value;
  Set_Flag_Hook();
}

char *
Flag_Name (Num)
     int Num;
{ switch(Num)
  { case D_EVAL:            return "Eval_Debug";
    case D_HEX_INPUT:	    return "Hex_Input_Debug";
    case D_FILE_LOAD:	    return "File_Load_Debug";
    case D_RELOC:	    return "Reloc_Debug";
    case D_INTERN:	    return "Intern_Debug";
    case D_CONT:	    return "Cont_Debug";
    case D_PRIMITIVE:	    return "Primitive_Debug";
    case D_LOOKUP:	    return "Lookup_Debug";
    case D_DEFINE:	    return "Define_Debug";
    case D_GC:		    return "GC_Debug";
    case D_UPGRADE:	    return "Upgrade_Debug";
    case D_DUMP:	    return "Dump_Debug";
    case D_TRACE_ON_ERROR:  return "Trace_On_Error";
    case D_PER_FILE:	    return "Per_File";
    case D_BIGNUM:          return "Bignum_Debug";
    case D_FLUIDS:	    return "Fluids_Debug";
    More_Debug_Flag_Names();
    default:		    return "Unknown Debug Flag";
  }
}

int
show_flags (All)
     Boolean All;
{ int i;
  for (i=0; i <= LAST_SWITCH; i++)
  { Boolean Value = *Find_Flag(i);
    if (All || Value)
    { printf ("Flag %d (%s) is %s.\n",
             i, Flag_Name(i), Value? "set" : "clear");
    }
  }
}

extern int OS_tty_tyi();

#define C_STRING_LENGTH 256

void
Handle_Debug_Flags ()
{ char c, input_string[C_STRING_LENGTH];
  int Which, free;
  Boolean interrupted;
  show_flags(false);
  while (true)
  { interrupted = false;
    printf ("Clear<number>, Set<number>, Done, ?, or Halt: ");
    OS_Flush_Output_Buffer();

    /* Considerably haired up to go through standard (safe) interface */

    c = (char) OS_tty_tyi(false, &interrupted);
    if (interrupted) return;
    for (free = 0; free < C_STRING_LENGTH; free++)
    { input_string[free] = OS_tty_tyi(false, &interrupted);
      if (interrupted) return;
      if (input_string[free] == '\n')
      { input_string[free] = '\0';
        break;
      }
    }
    switch (c)
    { case 'c':
      case 'C': Which=debug_getdec(input_string);
                set_flag(Which, false);
                break;
      case 's':
      case 'S': Which=debug_getdec(input_string);
                set_flag(Which, true);
                break;
      case 'd':
      case 'D': return;
      case 'h':
      case 'H': Microcode_Termination(TERM_HALT);

      case '?':
      default :	show_flags(true);
                break;
    }
  }
}

int
normal_debug_getdec (str)
     int str;
{ int Result;
  sscanf(str, "%d", &Result);
  return Result;
}

#else /* ENABLE_DEBUGGING_TOOLS */
void
Handle_Debug_Flags ()
{ fprintf (stderr, "Not a debugging version.  No flags to handle.\n");
  return;
}
#endif /* not ENABLE_DEBUGGING_TOOLS */
