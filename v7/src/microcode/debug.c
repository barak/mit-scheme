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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/debug.c,v 9.21 1987/01/22 14:23:06 jinx Exp $
 *
 * Utilities to help with debugging
 */

#include "scheme.h"
#include "primitive.h"

void Show_Pure()
{ Pointer *Obj_Address;
  long Pure_Size, Total_Size;

  Obj_Address = Constant_Space;
  while (true)
  { if (Obj_Address > Free_Constant)
    { printf("Past end of area.\n");
      return;
    }
    if (Obj_Address == Free_Constant)
    { printf("Done.\n");
      return;
    }
    Pure_Size = Get_Integer(*Obj_Address);
    Total_Size = Get_Integer(Obj_Address[1]);
    printf("0x%x: pure=0x%x, total=0x%x\n",
           Obj_Address, Pure_Size, Total_Size);
    if (Type_Code(*Obj_Address) != TC_MANIFEST_SPECIAL_NM_VECTOR)
    { printf("Missing initial SNMV.\n");
      return;
    }
    if (Type_Code(Obj_Address[1]) != PURE_PART)
      printf("Missing subsequent pure header.\n");
    if (Type_Code(Obj_Address[Pure_Size-1]) !=
        TC_MANIFEST_SPECIAL_NM_VECTOR)
    { printf("Missing internal SNMV.\n");
      return;
    }
    if (Type_Code(Obj_Address[Pure_Size]) != CONSTANT_PART)
    { printf("Missing constant header.\n");
      return;
    }
    if (Get_Integer(Obj_Address[Pure_Size]) != Pure_Size)
      printf("Pure size mismatch 0x%x.\n",
	     Get_Integer(Obj_Address[Pure_Size]));
    if (Type_Code(Obj_Address[Total_Size-1]) != 
        TC_MANIFEST_SPECIAL_NM_VECTOR)
    { printf("Missing ending SNMV.\n");
      return;
    }
    if (Type_Code(Obj_Address[Total_Size]) != END_OF_BLOCK)
    { printf("Missing ending header.\n");
      return;
    }
    if (Get_Integer(Obj_Address[Total_Size]) != Total_Size)
      printf("Total size mismatch 0x%x.\n",
             Get_Integer(Obj_Address[Total_Size]));
    Obj_Address += Total_Size+1;
#ifdef FLOATING_ALIGNMENT
    while (*Obj_Address == Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, 0))
      Obj_Address += 1;
#endif
  }
}

void Show_Env(The_Env)
Pointer The_Env;
{ Pointer *Name_Ptr, *Value_Ptr, Aux_Ptr, Aux_Slot_Ptr;
  long Count, i;
  Value_Ptr = Nth_Vector_Loc(The_Env, HEAP_ENV_FUNCTION);
  if ((Type_Code(*Value_Ptr) == TC_PROCEDURE) || 
      (Type_Code(*Value_Ptr) == TC_EXTENDED_PROCEDURE))
  { Name_Ptr = Nth_Vector_Loc(*Value_Ptr, PROCEDURE_LAMBDA_EXPR);
    Name_Ptr = Nth_Vector_Loc(*Name_Ptr, LAMBDA_FORMALS);
    Count = Vector_Length(*Name_Ptr);
    Name_Ptr = Nth_Vector_Loc(*Name_Ptr, 1);
    for (i=0; i < Count; i++)
    { Print_Expression(*Name_Ptr++, "Name ");
      Print_Expression(*Value_Ptr++, " Value ");
      printf("\n");
    }
    Aux_Ptr = Vector_Ref(The_Env, HEAP_ENV_AUX_SLOT);
    if (Aux_Ptr != NIL)
    { printf("Auxilliary Variables\n");
      while (Aux_Ptr != NIL)
      { Aux_Slot_Ptr = Vector_Ref(Aux_Ptr, CONS_CAR);
        Print_Expression(Vector_Ref(Aux_Slot_Ptr, CONS_CAR),
                         "Name ");
        Print_Expression(Vector_Ref(Aux_Slot_Ptr, CONS_CAR),
                         " Value ");
        Aux_Ptr = Vector_Ref(Aux_Ptr, CONS_CDR);
	printf("\n");
      }
    }
  }
  else printf("Not created by a procedure");
}

/* For debugging, given a String, return either a "not interned"
 * message or the address of the symbol and its global value.
 */

void Find_Symbol(Scheme_String)
Pointer Scheme_String;
{ Pointer Ob_Array, The_Symbol, *Bucket;
  char *String, *Temp_String;
  long i, Hashed_Value;
  String = Scheme_String_To_C_String(Scheme_String);
  for (Temp_String=String, i=0; *Temp_String == '\0'; i++) Temp_String++;
  Hashed_Value = Do_Hash(String, i);
  Ob_Array = Get_Fixed_Obj_Slot(OBArray);
  Hashed_Value %= Vector_Length(Ob_Array);
  Bucket = Nth_Vector_Loc(Ob_Array, Hashed_Value);
  while (*Bucket != NIL)
  { if (String_Equal(Scheme_String,
                     Vector_Ref(Vector_Ref(*Bucket, CONS_CAR),
                                SYMBOL_NAME)))
    { The_Symbol = Vector_Ref(*Bucket, CONS_CAR);
      printf("\nInterned Symbol: 0x%x", The_Symbol);
      Print_Expression(Vector_Ref(The_Symbol, SYMBOL_GLOBAL_VALUE),
                       "Value");
      printf("\n");
      return;
    }
    Bucket = Nth_Vector_Loc(*Bucket, CONS_CDR);
  }
  printf("\nNot interned.\n");
}

List_Print(Expr)
Pointer Expr;
{ int Count;
  Count = 0;
  printf("(");
  while (((Type_Code(Expr) == TC_LIST) ||
	  (Type_Code(Expr) == TC_WEAK_CONS))
	  && Count < MAX_LIST_PRINT)
  { Print_Expression(Vector_Ref(Expr, CONS_CAR),
		     (Type_Code(Expr)==TC_LIST) ? "" : "{weak}");
    Expr = Vector_Ref(Expr, CONS_CDR);
    if (Type_Code(Expr) != TC_NULL) printf(" ");
    Count += 1;
  }
  if (Type_Code(Expr) != TC_NULL)
  { if (Count==MAX_LIST_PRINT) printf("...");
    else
    { printf(". ");
      Print_Expression(Expr, "");
    }
  }
  printf(")");
}

long Print_Return_Name(Ptr)
Pointer Ptr;
{ long index = Get_Integer(Ptr);
  char *name;
  if ((index <= MAX_RETURN) &&
      ((name = Return_Names[index]) != ((char *) NULL)))
    printf("%s", name);
  else
    printf("[0x%x]", index);
}

void Print_Return(String)
char *String;
{ printf("%s: ", String);
  Print_Return_Name(Fetch_Return());
  CRLF();
}

extern Boolean Prt_PName();

void Print_Expression(Expr, String)
char *String;
Pointer Expr;
{ if (String[0] != 0) printf("%s: ", String);
  Do_Printing(Expr, true);
}

Do_Printing(Expr, Detailed)
Pointer Expr;
Boolean Detailed;
{ long Temp_Address;
  Boolean Return_After_Print;
  Temp_Address = Get_Integer(Expr);
  Return_After_Print = false;
  if (Type_Code(Expr) > MAX_SAFE_TYPE) printf("{Dangerous}");
  switch(Safe_Type_Code(Expr))
  { case TC_ACCESS:
      printf("[ACCESS (");
      Expr = Vector_Ref(Expr, ACCESS_NAME);
      goto SPrint;

    case TC_ASSIGNMENT:
      printf("[SET! (");
      Expr = Vector_Ref(Vector_Ref(Expr, ASSIGN_NAME),
                        VARIABLE_SYMBOL);
      goto SPrint;

    case TC_CHARACTER_STRING:
    { long Length, i;
      char *Next, This;
      printf("\"");
      Length = Get_Integer(Vector_Ref(Expr, STRING_LENGTH));
      Next = (char *) Nth_Vector_Loc(Expr, STRING_CHARS);
      for (i=0; i < Length; i++)
      { This = *Next++;
        printf((This < ' ') || (This > '|') ? "\\%03o" : "%c",
                This);
      }
      printf("\"");
      return;
    }

/* Do_Printing continues on the next page */

/* Do_Printing, continued */

    case TC_DEFINITION:
      printf("[DEFINE (");
      Expr = Vector_Ref(Expr, DEFINE_NAME);
      goto SPrint;

    case TC_FIXNUM:
    { long A;
      Sign_Extend(Expr, A);
      printf("%d", A);
      return;
    }

    case TC_BIG_FLONUM: printf("%f", Get_Float(Expr)); return;

    case TC_WEAK_CONS:
    case TC_LIST: List_Print(Expr); return;

    case TC_NULL:
      if (Temp_Address==0)
      { printf("()");
        return;
      }
      printf("[NULL");
      break;

/* Do_Printing continues on the next page */

/* Do_Printing, continued */

    case TC_UNINTERNED_SYMBOL:
      printf("[UNINTERNED_SYMBOL ("); goto SPrint;

    case TC_INTERNED_SYMBOL:
    { Pointer Name;
      char   *Next_Char;
      long    Length, i;
      Return_After_Print = true;
SPrint:
      Name = Vector_Ref(Expr, SYMBOL_NAME);
      Length = Get_Integer(Vector_Ref(Name, STRING_LENGTH));
      Next_Char = (char *) Nth_Vector_Loc(Name, STRING_CHARS);
      for (i=0; i < Length; i++)
        printf("%c", *Next_Char++);
      if (Return_After_Print) return;
      printf(")");
      break;
    }

/* Do_Printing continues on the next page */

/* Do_Printing, continued */

  case TC_VARIABLE:
      if (Detailed) printf("[VARIABLE (");
      Expr = Vector_Ref(Expr, VARIABLE_SYMBOL);
      if (!Detailed) Return_After_Print = true;
      goto SPrint;

    case TC_BIG_FIXNUM: printf("[BIG_FIXNUM"); break;
    case TC_BROKEN_HEART: printf("[BROKEN_HEART"); break;
    case TC_CHARACTER: printf("[CHARACTER"); break;
    case TC_COMBINATION:
      printf("[COMBINATION (%d args) 0x%x]",
	     Vector_Length(Expr)-1, Temp_Address);
      if (Detailed)
      { printf(" (");
	Do_Printing(Vector_Ref(Expr, COMB_FN_SLOT), false);
        printf(" ...)");
      }
      return;
    case TC_COMBINATION_1:
      printf("[COMBINATION_1 0x%x]", Temp_Address);
      if (Detailed)
      { printf(" (");
	Do_Printing(Vector_Ref(Expr, COMB_1_FN), false);
	printf(", ");
	Do_Printing(Vector_Ref(Expr, COMB_1_ARG_1), false);
	printf(")");
      }
      return;

/* Do_Printing continues on the next page */

/* Do_Printing, continued */

    case TC_COMBINATION_2:
      printf("[COMBINATION_2 0x%x]", Temp_Address);
      if (Detailed)
      { printf(" (");
	Do_Printing(Vector_Ref(Expr, COMB_2_FN), false);
	printf(", ");
	Do_Printing(Vector_Ref(Expr, COMB_2_ARG_1), false);
	printf(", ");
	Do_Printing(Vector_Ref(Expr, COMB_2_ARG_2), false);
	printf(")");
      }
      return;
    case TC_CELL: printf("[CELL"); break;
    case TC_COMMENT: printf("[COMMENT"); break;
    case TC_COMPILED_EXPRESSION: printf("[COMPILED_EXPRESSION"); break;
    case TC_COMPILED_PROCEDURE:
     printf("[COMPILED_PROCEDURE"); break;
    case TC_CONDITIONAL: printf("[CONDITIONAL"); break;
    case TC_CONTROL_POINT: printf("[CONTROL_POINT"); break;
    case TC_DELAY: printf("[DELAY"); break;
    case TC_DELAYED: printf("[DELAYED"); break;
    case TC_DISJUNCTION: printf("[DISJUNCTION"); break;
    case TC_ENVIRONMENT:
      printf("[ENVIRONMENT 0x%x]", Temp_Address);
      printf(" (from ");
      Do_Printing(Vector_Ref(Expr, HEAP_ENV_FUNCTION), false);
      printf(")");
      return;
    case TC_EXTENDED_FIXNUM: printf("[EXTENDED_FIXNUM"); break;
    case TC_EXTENDED_LAMBDA:
      if (Detailed) printf("[EXTENDED_LAMBDA (");
      Do_Printing(
        Vector_Ref(
          Vector_Ref(Expr, ELAMBDA_NAMES),
	  1), false);
      if (Detailed) printf(") 0x%x", Temp_Address);
      return;
    case TC_EXTENDED_PROCEDURE:
      if (Detailed) printf("[EXTENDED_PROCEDURE (");
      Do_Printing(Vector_Ref(Expr, PROCEDURE_LAMBDA_EXPR), false);
      if (Detailed) printf(") 0x%x]", Temp_Address);
      break;

/* Do_Printing continues on the next page */

/* Do_Printing, continued */

    case TC_FUTURE: printf("[FUTURE"); break;
    case TC_HUNK3: printf("[HUNK3"); break;
    case TC_IN_PACKAGE: printf("[IN_PACKAGE"); break;
    case TC_LAMBDA:
      if (Detailed) printf("[LAMBDA (");
      Do_Printing(
        Vector_Ref(
          Vector_Ref(Expr, LAMBDA_FORMALS),
	  1), false);
      if (Detailed) printf(") 0x%x]", Temp_Address);
      return;
    case TC_LEXPR: printf("[LEXPR"); break;
    case TC_MANIFEST_NM_VECTOR: printf("[MANIFEST_NM_VECTOR"); break;
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
      printf("[MANIFEST_SPECIAL_NM_VECTOR"); break;
    case TC_NON_MARKED_VECTOR: printf("[NON_MARKED_VECTOR"); break;
    case TC_PCOMB0: printf("[PCOMB0"); break;
    case TC_PCOMB1: printf("[PCOMB1"); break;
    case TC_PCOMB2: printf("[PCOMB2"); break;
    case TC_PCOMB3: printf("[PCOMB3"); break;
    case TC_PRIMITIVE:
      printf("[PRIMITIVE "); Prt_PName(Temp_Address);
      printf("]"); return;
    case TC_PRIMITIVE_EXTERNAL: printf("[PRIMITIVE_EXTERNAL"); break;
    case TC_PROCEDURE:
      if (Detailed) printf("[PROCEDURE (");
      Do_Printing(Vector_Ref(Expr, PROCEDURE_LAMBDA_EXPR), false);
      if (Detailed) printf(") 0x%x]", Temp_Address);
      return;
  
/* Do_Printing continues on the next page */

/* Do_Printing, continued */

    case TC_RETURN_CODE:
      printf("[RETURN_CODE ");
      Print_Return_Name(Expr);
      printf("]");
      return;
    case TC_SCODE_QUOTE: printf("[SCODE_QUOTE"); break;
    case TC_SEQUENCE_2: printf("[SEQUENCE_2"); break;
    case TC_SEQUENCE_3: printf("[SEQUENCE_3"); break;
    case TC_THE_ENVIRONMENT: printf("[THE_ENVIRONMENT"); break;

    case TC_TRAP:
      printf("[TRAP ");
      Print_Expression(Vector_Ref(Expr, TRAP_TAG), " tag");
      Print_Expression(Vector_Ref(Expr, TRAP_DEFAULT), " default");
      Print_Expression(Vector_Ref(Expr, TRAP_FROB), " frob");
      printf("]");
      return;

    case TC_TRUE:
      if (Temp_Address == 0)
      { printf("#!true");
        return;
      }
      printf("[TRUE");
      break;
    case TC_UNASSIGNED:
      if (Temp_Address == UNBOUND)
      { printf("#!UNBOUND");
        return;
      }
      else if (Temp_Address == UNASSIGNED)
      { printf("#!UNASSIGNED");
        return;
      }
      else printf("[UNASSIGNED"); break;
    case TC_VECTOR: printf("[VECTOR"); break;
    case TC_VECTOR_16B: printf("[VECTOR_16B"); break;
    case TC_VECTOR_1B: printf("[VECTOR_1B"); break;
    default: printf("[0x%x", Type_Code(Expr));
  }
  printf(" 0x%x]", Temp_Address);
}

Boolean Print_One_Continuation_Frame(Temp)
Pointer Temp;
{ Pointer Expr;
  Print_Expression(Temp, "Return code");
  CRLF();
  Expr = Pop();
  Print_Expression(Expr, "Expression");
  printf("\n");
  if ((Datum(Temp) == RC_END_OF_COMPUTATION) ||
      (Datum(Temp) == RC_HALT)) return true;
  if (Datum(Temp) == RC_JOIN_STACKLETS)
    Stack_Pointer = Previous_Stack_Pointer(Expr);
  return false;
}

/* Back_Trace relies on (a) only a call to Save_Cont puts a return code on the
   stack; (b) Save_Cont pushes the expression first. */

void Back_Trace()
{ Pointer Temp, *Old_Stack;
  Back_Trace_Entry_Hook();
  Old_Stack = Stack_Pointer;
  while (true)
  { if (Return_Hook_Address == &Top_Of_Stack())
    { Temp = Pop();
      if (Temp != Make_Non_Pointer(TC_RETURN_CODE, RC_RETURN_TRAP_POINT))
        printf("\n--> Return trap is missing here <--\n");
      else
      { printf("\n[Return trap found here as expected]\n");
        Temp = Old_Return_Code;
      }
    }
    else Temp = Pop();
    if (Type_Code(Temp) == TC_RETURN_CODE)
    { if (Print_One_Continuation_Frame(Temp))
	break;
    }
    else
    { Print_Expression(Temp, "  ...");
      if (Safe_Type_Code(Temp) == TC_MANIFEST_NM_VECTOR)
      { Stack_Pointer = Simulate_Popping(Get_Integer(Temp));
        printf(" (skipping)");
      }
      printf("\n");
    }
  }
  Stack_Pointer = Old_Stack;
  Back_Trace_Exit_Hook();
}

void Print_Stack(SP)
Pointer *SP;
{ Pointer *Saved_SP;
  Saved_SP = Stack_Pointer;
  Stack_Pointer = SP;
  Back_Trace();
  Stack_Pointer = Saved_SP;
  return;
}

Boolean Prt_PName(Number)
long Number;
{ if ((Number < 0) ||
      (Number > MAX_PRIMITIVE) ||
      (Primitive_Names[Number] == NULL))
  { printf("Unknown primitive 0x%08x", Number);
    return false;
  }
  else
  { printf("%s", Primitive_Names[Number]);
    return true;
  }
}

void Print_Primitive(Number)
long Number;
{ short NArgs;

  printf("Primitive: ");
  if (Prt_PName(Number)) NArgs = (int) Arg_Count_Table[Number];
  else NArgs = 3;	        /* Unknown primitive */
  printf("\n");
  if (NArgs > 0)
  { Print_Expression(Stack_Ref(0), "...Arg 1");
    printf("\n");
  }
  if (NArgs > 1)
  { Print_Expression(Stack_Ref(1), "...Arg 2");
    printf("\n");
  }
  if (NArgs > 2)
  { Print_Expression(Stack_Ref(2), "...Arg 3");
    printf("\n");
  }
}

Debug_Printer(Expr)
Pointer Expr;
{ Print_Expression(Expr, "");
  putchar('\n');
}

/* (TEMP_PRINTER OBJECT)
      [Primitive number 0xB2]
      A cheap, built-in printer intended for debugging the
      interpreter.
*/
Built_In_Primitive(Prim_Temp_Printer, 1, "TEMP-PRINTER")
{ Primitive_1_Arg();
  Debug_Printer(Arg1);
  return TRUTH;
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

Boolean *Find_Flag(Num)
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

set_flag(Num, Value)
int Num;
Boolean Value;
{ Boolean *Flag = Find_Flag(Num);
  if (Flag != NULL) *Flag = Value;
  Set_Flag_Hook();
}

char *Flag_Name(Num)
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

show_flags(All)
Boolean All;
{ int i;
  for (i=0; i <= LAST_SWITCH; i++)
  { Boolean Value = *Find_Flag(i);
    if (All || Value)
    { printf("Flag %d (%s) is %s.\n",
             i, Flag_Name(i), Value? "set" : "clear");
    }
  }
}

extern char OS_tty_tyi();

#define C_STRING_LENGTH 256

void Handle_Debug_Flags()
{ char c, input_string[C_STRING_LENGTH];
  int Which, free;
  Boolean interrupted;
  show_flags(false);
  while (true)
  { interrupted = false;
    printf("Clear<number>, Set<number>, Done, ?, or Halt: ");
    OS_Flush_Output_Buffer();

    /* Considerably haired up to go through standard (safe) interface */

    c = OS_tty_tyi(false, &interrupted);
    if (interrupted) return;
    for (free = 0; free < C_STRING_LENGTH; free++)
    { input_string[free] = OS_tty_tyi(false, &interrupted);
      if (interrupted) return;
      if (input_string[free] == '\n')
      { input_string[free] = '\0';
        break;
      }
    }

/* Handle_Debug_Flags continues on the next page */

/* Handle_Debug_Flags, continued */

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

int normal_debug_getdec(str)
{ int Result;
  sscanf(str, "%d", &Result);
  return Result;
}

#else /* ENABLE_DEBUGGING_TOOLS */
void Handle_Debug_Flags()
{ fprintf(stderr, "Not a debugging version.  No flags to handle.\n");
  return;
}
#endif /* not ENABLE_DEBUGGING_TOOLS */
