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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fasload.c,v 9.21 1987/01/22 14:24:16 jinx Exp $

   The "fast loader" which reads in and relocates binary files and then
   interns symbols.  It is called with one argument: the (character
   string) name of a file to load.  It is called as a primitive, and
   returns a single object read in.
 */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"

#define CCheck_or_Reloc_Debug Or2(Consistency_Check, Reloc_Debug)
#define Reloc_or_Load_Debug   Or2(Reloc_Debug, File_Load_Debug)

#define print_char(C) printf(((C < ' ') || (C > '|')) ?	\
			     "\\%03o" : "%c", (C && MAX_CHAR));

Pointer String_To_Symbol();

#include "load.c"

/* Here is a totally randomly constructed string hashing function */
   
long Do_Hash(String_Ptr, String_Length)
char *String_Ptr;
long String_Length;
{ long i, Value, End_Count;

  Value = LENGTH_MULTIPLIER*String_Length;
  End_Count = (String_Length > MAX_HASH_CHARS) ?
              MAX_HASH_CHARS : String_Length;
  for (i=0; i < End_Count; i++)
    Value = (Value << SHIFT_AMOUNT) + (MAX_CHAR & String_Ptr[i]);
  if (Intern_Debug)
  { char *C;
    printf("  Hashing: %d: ", String_Length);
    C = String_Ptr;
    for (i=0; i < String_Length; i++, C++)
      print_char(*C);
    printf(" => 0x%x\n", Value);
  }
  return Value;
}

Pointer Hash(Ptr)
Pointer Ptr;
{ long String_Length;

  String_Length = Get_Integer(Fast_Vector_Ref(Ptr, STRING_LENGTH));
  return Make_Non_Pointer(TC_FIXNUM,
			  Do_Hash(Scheme_String_To_C_String(Ptr),
				  String_Length));
}

Pointer Hash_Chars(Ptr)
Pointer Ptr;
{ long Length;
  Pointer This_Char;
  char String[MAX_HASH_CHARS];

  Touch_In_Primitive(Ptr, Ptr);
  for (Length=0; Type_Code(Ptr)==TC_LIST; Length++)
  { if (Length < MAX_HASH_CHARS)
    { Touch_In_Primitive(Vector_Ref(Ptr, CONS_CAR), This_Char);
      if (Type_Code(This_Char) != TC_CHARACTER) 
        Primitive_Error(ERR_ARG_1_WRONG_TYPE);
      Range_Check(String[Length], This_Char,
                  (char) 0, (char) MAX_CHAR, ERR_ARG_1_WRONG_TYPE);
      Touch_In_Primitive(Vector_Ref(Ptr, CONS_CDR), Ptr);
    }
  }
  if (Ptr != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  return Make_Non_Pointer(TC_FIXNUM, Do_Hash(String, Length));
}

Boolean String_Equal(String1, String2)
Pointer String1, String2;
{ char *S1, *S2;
  long Length1, Length2, i;

  if (Address(String1)==Address(String2)) return true;
  Length1 = Get_Integer(Fast_Vector_Ref(String1, STRING_LENGTH));
  Length2 = Get_Integer(Fast_Vector_Ref(String2, STRING_LENGTH));
  if (Length1 != Length2) return false;
  S1 = (char *) Nth_Vector_Loc(String1, STRING_CHARS);
  S2 = (char *) Nth_Vector_Loc(String2, STRING_CHARS);
  for (i=0; i < Length1; i++) if (*S1++ != *S2++) return false;
  return true;
}

Pointer Make_String(Orig_List)
Pointer Orig_List;
{ char *Next;
  long Length;
  Pointer Result;

  Result = Make_Pointer(TC_CHARACTER_STRING, Free);
  Next = (char *) Nth_Vector_Loc(Result, STRING_CHARS);
  Length = 0;
  Touch_In_Primitive(Orig_List, Orig_List);
  while (Type_Code(Orig_List) == TC_LIST)
  { Pointer This_Char;
    long The_Character;

    Primitive_GC_If_Needed(Free - ((Pointer *) Next));
    Touch_In_Primitive(Vector_Ref(Orig_List, CONS_CAR), This_Char);
    if (Type_Code(This_Char) != TC_CHARACTER)
      Primitive_Error(ERR_ARG_1_WRONG_TYPE);
    Range_Check(The_Character, This_Char,
		0, MAX_CHAR, ERR_ARG_1_BAD_RANGE);
    *Next++ = (char) The_Character;
    Touch_In_Primitive(Vector_Ref(Orig_List, CONS_CDR), Orig_List);
    Length += 1;
  }
  if (Orig_List != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  *Next++ = '\0';		  /* Add the null */
  Free += 2 + (Length+sizeof(Pointer))/sizeof(Pointer);
  Vector_Set(Result, STRING_LENGTH, FIXNUM_0+Length);
  Vector_Set(Result, STRING_HEADER,
    Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Free-Get_Pointer(Result))-1));
  return Result;
}

/* Interning involves hashing the input string and either returning
   an existing symbol with that name from the ObArray or creating a
   new symbol and installing it in the ObArray. The resulting interned
   symbol is stored in *Un_Interned.
*/

long Intern(Un_Interned)
Pointer *Un_Interned;
{ long Hashed_Value;
  Pointer Ob_Array, *Bucket, String, Temp;

  String = Fast_Vector_Ref(*Un_Interned, SYMBOL_NAME);
  Temp = Hash(String);
  Hashed_Value = Get_Integer(Temp);
  Ob_Array = Get_Fixed_Obj_Slot(OBArray);
  Hashed_Value %= Vector_Length(Ob_Array);
  Bucket = Nth_Vector_Loc(Ob_Array, Hashed_Value + 1);

  if (Intern_Debug)
  { char *C;
    int i, String_Length;
    String_Length = Get_Integer(Fast_Vector_Ref(String, STRING_LENGTH));
    C = (char *) Nth_Vector_Loc(String, STRING_CHARS);  
    printf("\nInterning ");
    for (i=0; i < String_Length; i++, C++) print_char(*C);
  }

/* Intern continues on the next page */

/* Intern, continued */

  while (*Bucket != NIL)
  { if (Intern_Debug)
      printf("  Bucket #%o (0x%x) ...\n",
             Address(*Bucket), Address(*Bucket));
    if (String_Equal(String,
                     Fast_Vector_Ref(
                       Vector_Ref(*Bucket, CONS_CAR),
	               SYMBOL_NAME)))
    { if (Intern_Debug) printf("  found\n");
      *Un_Interned = Vector_Ref(*Bucket, CONS_CAR);
      return;
    }
    Bucket = Nth_Vector_Loc(*Bucket, CONS_CDR);
  }

/* Symbol does not exist yet in ObArray.  Bucket points to the
   cell containing the final #!NULL in the list.  Replace this
   with the CONS of the new symbol and #!NULL (i.e. extend the
   list in the bucket by 1 new element).
*/

  Store_Type_Code(*Un_Interned, TC_INTERNED_SYMBOL);
  if (Intern_Debug) printf("  adding at #%o (0x%x)\n",
                           (long) Free, (long) Free);
  *Bucket = Make_Pointer(TC_LIST, Free);
  Free[CONS_CAR] = *Un_Interned;
  Free[CONS_CDR] = NIL;
  Free += 2;
}

Load_File(Name)
Pointer Name;
{ char *Char;
  long N, i;
  Boolean File_Opened;
  File_Opened = Open_Dump_File(Name, OPEN_FLAG);
  if (Per_File) Handle_Debug_Flags();
  if (!File_Opened) Primitive_Error(ERR_ARG_1_BAD_RANGE);

/* Load_File continues on next page */

/* Load_File, continued */

  if (!Read_Header())
  { printf("\nThis file does not appear to be in FASL format.\n");
    goto CANNOT_LOAD;
  }
  if (File_Load_Debug)
    printf("\nMachine type %d, Version %d, Subversion %d\n",
           Machine_Type, Version, Sub_Version);
#ifdef BYTE_INVERSION
  if ((Sub_Version > FASL_SUBVERSION))
#else
  if ((Sub_Version > FASL_SUBVERSION) ||
      (Machine_Type != FASL_INTERNAL_FORMAT))
#endif
  { printf("\nFASL File Version %4d Subversion %4d Machine Type %4d\n",
	   Version, Sub_Version , Machine_Type);
    printf("Expected: Version %4d Subversion %4d Machine Type %4d\n",
	   FASL_FORMAT_VERSION, FASL_SUBVERSION, FASL_INTERNAL_FORMAT);
    printf("You may need to use the `Bintopsb' and `Psbtobin' programs.\n");
CANNOT_LOAD:
    fclose(File_Handle);
    Primitive_Error(ERR_FASL_FILE_BAD_DATA);
  }
  if (!Test_Pure_Space_Top(Free_Constant+Const_Count))
  { fclose(File_Handle);
    Primitive_Error(ERR_FASL_FILE_TOO_BIG);
  }
  if (GC_Check(Heap_Count))
  { fclose(File_Handle);
    Request_GC(Heap_Count);
    Primitive_Interrupt();
  }
  /* Aligning Free here confuses the counters
     Align_Float(Free);
   */
  Load_Data(Heap_Count, (char *) Free);
#ifdef BYTE_INVERSION
  Byte_Invert_Region((char *) Free, Heap_Count);
#endif
  Free += Heap_Count;
  Load_Data(Const_Count, (char *) Free_Constant);
#ifdef BYTE_INVERSION
  Byte_Invert_Region((char *) Free_Constant, Const_Count);
#endif
  Free_Constant += Const_Count;
  /* Same 
     Align_Float(Free);
   */
  fclose(File_Handle);
}

/* Statics used by Relocate, below */

relocation_type Heap_Relocation, Const_Reloc, Stack_Relocation;

/* Relocate a pointer as read in from the file.  If the pointer used
   to point into the heap, relocate it into the heap.  If it used to
   be constant area, relocate it to constant area.  Otherwise give an
   error.
*/

#ifdef ENABLE_DEBUGGING_TOOLS
static Boolean Warned = false;
Pointer *Relocate(P)
long P;
{ Pointer *Result;
  if ((P >= Heap_Base) && (P < Dumped_Heap_Top))
    Result = (Pointer *) (P + Heap_Relocation);
  else if ((P >= Const_Base) && (P < Dumped_Constant_Top))
    Result = (Pointer *) (P + Const_Reloc);
  else if (P < Dumped_Stack_Top)
    Result = (Pointer *) (P + Stack_Relocation);
  else
  { printf("Pointer out of range: 0x%x\n", P, P);
    if (!Warned)
    { printf("Heap: %x-%x, Constant: %x-%x, Stack: ?-0x%x\n",
             Heap_Base, Dumped_Heap_Top,
             Const_Base, Dumped_Constant_Top, Dumped_Stack_Top);
      Warned = true;
    }
    Result = (Pointer *) 0;
  }
  if (Reloc_Debug) printf("0x%06x => 0x%06x\n", P, Result);
  return Result;
}

#define Relocate_Into(Loc, P) (Loc) = Relocate(P)

#else

#define Relocate_Into(Loc, P)				\
if ((P) < Const_Base)					\
  (Loc) = ((Pointer *) ((P) + Heap_Relocation));	\
else if ((P) < Dumped_Constant_Top)			\
  (Loc) = ((Pointer *) ((P) + Const_Reloc));		\
else							\
  (Loc) = ((Pointer *) ((P) + Stack_Relocation))

#ifndef Conditional_Bug
#define Relocate(P)					\
	((P < Const_Base) ?				\
         ((Pointer *) (P + Heap_Relocation)) :		\
         ((P < Dumped_Constant_Top) ?			\
           ((Pointer *) (P + Const_Reloc)) :		\
           ((Pointer *) (P + Stack_Relocation))))
#else
static Pointer *Relocate_Temp;
#define Relocate(P)					\
  (Relocate_Into(Relocate_Temp, P), Relocate_Temp)
#endif
#endif

/* Next_Pointer starts by pointing to the beginning of the block of
   memory to be handled.  This loop relocates all pointers in the
   block of memory.
*/

long Relocate_Block(Next_Pointer, Stop_At)
fast Pointer *Next_Pointer, *Stop_At;
{ if (Reloc_Debug)
    fprintf(stderr,
	    "Relocation beginning, block=0x%x, length=0x%x, end=0x%x.\n",
	    Next_Pointer, (Stop_At-Next_Pointer)-1, Stop_At);
  while (Next_Pointer < Stop_At)
  { fast Pointer Temp = *Next_Pointer;

    Switch_by_GC_Type(Temp)
    { case TC_BROKEN_HEART:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
      case_Fasdump_Non_Pointer:
        Next_Pointer += 1;
	break;
	
      case TC_PRIMITIVE_EXTERNAL:
        Found_Ext_Prims = true;
        Next_Pointer += 1;
        break;

      case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += Get_Integer(Temp)+1;
        break;

#ifdef BYTE_INVERSION
      case TC_CHARACTER_STRING:
	String_Inversion(Relocate(Datum(Temp)));
			 /* THEN FALL THROUGH */
#endif

      	/* These work automagically */
      case_compiled_entry_point:
      default:
      { fast long Next = Datum(Temp);
	*Next_Pointer++ = Make_Pointer(Type_Code(Temp), Relocate(Next));
      }
    }
  }
}

Intern_Block(Next_Pointer, Stop_At)
Pointer *Next_Pointer, *Stop_At;
{ if (Reloc_Debug) printf("Interning a block.\n");
  while (Next_Pointer <= Stop_At)	/* BBN has < for <= */
  { if (Reloc_Debug && Dangerous(*Next_Pointer))
      printf("\nDangerous object at 0x%x: 0x%x",
             Next_Pointer, *Next_Pointer);
    switch (Safe_Type_Code(*Next_Pointer))
    { case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += Get_Integer(*Next_Pointer)+1;
        break;

      case TC_INTERNED_SYMBOL:
      if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_GLOBAL_VALUE)) ==
          TC_BROKEN_HEART)
      { Pointer Old_Symbol = *Next_Pointer;
        Vector_Set(*Next_Pointer, SYMBOL_GLOBAL_VALUE, UNBOUND_OBJECT);
        Intern(Next_Pointer);
        Primitive_GC_If_Needed(0);
        if (*Next_Pointer != Old_Symbol)
        { Vector_Set(Old_Symbol, SYMBOL_NAME,
		     Make_New_Pointer(TC_BROKEN_HEART, *Next_Pointer));
        }
      }
      else if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_NAME)) ==
              TC_BROKEN_HEART)
      { *Next_Pointer =
          Make_New_Pointer(Type_Code(*Next_Pointer),
                           Fast_Vector_Ref(*Next_Pointer,
					   SYMBOL_NAME));
      }
      Next_Pointer += 1;
      break;
      
      default: Next_Pointer += 1;
    }
  }
  if (Reloc_Debug) printf("Done interning block.\n");
  return;
}

/* Install the external primitives vector.  This requires changing
   the Ext_Prim_Vector from a vector of symbols (which is what is
   in the FASL file) into a vector of (C format) numbers representing
   the corresponding external primitives numbers for this interpreter.
   If an external primitive is known, then the existing assigned number
   is used.  If not, the symbol is added to the list of assigned
   numbers.  In the case of a band load (as opposed to a fasload),
   the existing vector of known but unimplemented external primitives
   is ignored and a completely new one will be built.
*/

Install_Ext_Prims(Normal_FASLoad)
Boolean Normal_FASLoad;
{ long i;
  Pointer *Next;

  Vector_Set(Ext_Prim_Vector, 0, 
	     Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, Ext_Prim_Count));
  Next = Nth_Vector_Loc(Ext_Prim_Vector, 1);
  if (Normal_FASLoad)
    for (i=0; i < Ext_Prim_Count; i++) Intern(Next++);
  else Undefined_Externals = NIL;
}

Update_Ext_Prims(Next_Pointer, Stop_At)
fast Pointer *Next_Pointer, *Stop_At;
{ for (;Next_Pointer < Stop_At; Next_Pointer++)
  { switch (Safe_Type_Code(*Next_Pointer))
    { case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += Get_Integer(*Next_Pointer);
        break;

      case TC_PRIMITIVE_EXTERNAL:
      {	long Which = Address(*Next_Pointer);
	if (Which > Ext_Prim_Count)
	  printf("External Primitive 0x%x out of range.\n", Which);
	else
	{ Pointer New_Value = User_Vector_Ref(Ext_Prim_Vector, Which);
	  if (Type_Code(New_Value) == TC_INTERNED_SYMBOL)
	  { New_Value = (Pointer) Get_Ext_Number(New_Value, TRUTH);
	    User_Vector_Set(Ext_Prim_Vector, Which, New_Value);
	  }
	  Store_Address(*Next_Pointer, New_Value);
	}
      }		 

      default: break;
    }
  }
}

Pointer Fasload(FileName, Not_From_Band_Load)
Pointer FileName;
Boolean Not_From_Band_Load;
{ Pointer *Heap_End, *Constant_End, *Orig_Heap, *Orig_Constant, *Xtemp;

#ifdef ENABLE_DEBUGGING_TOOLS
  Warned = false;
#endif

  if (Type_Code(FileName) != TC_CHARACTER_STRING)
    Primitive_Error(ERR_ARG_1_WRONG_TYPE);

	/* Read File */

  Orig_Heap = Free;
  Orig_Constant = Free_Constant;
  Load_File(FileName);
  Heap_End = Free;
  Constant_End = Free_Constant;
  Heap_Relocation = ((relocation_type) Orig_Heap) - Heap_Base;
  Const_Reloc = ((relocation_type) Orig_Constant) - Const_Base;
  Stack_Relocation = ((relocation_type) Stack_Top) - Dumped_Stack_Top;

  if (Reloc_Debug)
    printf("Heap_relocation = %d = %x; Const_Reloc = %d = %x\n",
	   Heap_Relocation, Heap_Relocation, 
           Const_Reloc,  Const_Reloc);

	/* Relocate the new Data */

#ifdef BYTE_INVERSION
  Setup_For_String_Inversion();
#endif

  Found_Ext_Prims = false;
  Relocate_Block(Orig_Heap, Free);
  Relocate_Block(Orig_Constant, Free_Constant);

#ifdef BYTE_INVERSION
  Finish_String_Inversion();
#endif

/* Fasload continues on the next page */

/* Fasload, continued */

	/* Intern */

  if (Not_From_Band_Load)
  { Intern_Block(Orig_Constant, Constant_End);
    Intern_Block(Orig_Heap, Heap_End);
  }

	/* Update External Primitives */

  if ((Ext_Prim_Vector != NIL) && Found_Ext_Prims)
  { Relocate_Into(Xtemp, Address(Ext_Prim_Vector));
    Ext_Prim_Vector = *Xtemp;
    Ext_Prim_Count = Vector_Length(Ext_Prim_Vector);
    Install_Ext_Prims(Not_From_Band_Load);
    Update_Ext_Prims(Orig_Heap, Free);
    Update_Ext_Prims(Orig_Constant, Free_Constant);
  }

  Set_Pure_Top();
  Relocate_Into(Xtemp, Dumped_Object);
  return *Xtemp;
}

/* (BINARY-FASLOAD FILE-NAME)
      [Primitive number 0x57]
      Load the contents of FILE-NAME into memory.  The file was
      presumably made by a call to PRIMITIVE_FASDUMP, and may contain
      data for the heap and/or the pure area.  The value returned is
      the object which was dumped.  Typically (but not always) this
      will be a piece of SCode which is then evaluated to perform
      definitions in some environment.
*/
Built_In_Primitive(Prim_Binary_Fasload, 1, "BINARY-FASLOAD")
{ /* The code for Fasload, which does all the work, is found in the
     file FASLOAD.C
  */
  Primitive_1_Arg();
  return Fasload(Arg1, true);
}

/* (LOAD-BAND FILE-NAME)
      [Primitive number 0xB9]
      Restores the heap and pure space from the contents of FILE-NAME,
      which is typically a file created by BAND_DUMP.  The file can,
      however, be any file which can be loaded with BINARY_FASLOAD.
*/
Built_In_Primitive(Prim_Band_Load, 1, "LOAD-BAND")
{ Pointer Save_FO, *Save_Free, *Save_Free_Constant, Save_Undefined,
          *Save_Stack_Pointer, *Save_Stack_Guard, Result;
  long Jump_Value;
  jmp_buf  Swapped_Buf, *Saved_Buf;
  Primitive_1_Arg();

  Save_Fixed_Obj(Save_FO);
  Save_Undefined = Undefined_Externals;
  Undefined_Externals = NIL;
  Save_Free = Free;
  Free = Heap_Bottom;
  Save_Free_Constant = Free_Constant;
  Free_Constant = Constant_Space;
  Save_Stack_Pointer = Stack_Pointer;
  Save_Stack_Guard = Stack_Guard;

/* Prim_Band_Load continues on next page */

/* Prim_Band_Load, continued */

  /* There is some jiggery-pokery going on here to make sure
     that all returns from Fasload (including error exits) return to
     the clean-up code before returning on up the C call stack.
  */
  Saved_Buf = Back_To_Eval;
  Jump_Value = setjmp(Swapped_Buf);
  if (Jump_Value == 0)
  { Back_To_Eval = (jmp_buf *) Swapped_Buf;
    Result = Fasload(Arg1, false);
    Back_To_Eval = Saved_Buf;
    History = Make_Dummy_History();
    Initialize_Stack();
    Store_Return(RC_END_OF_COMPUTATION);
    Store_Expression(NIL);
    Save_Cont();
    Store_Expression(Vector_Ref(Result,0));
    /* Primitive externals handled by Fasload */
    return_to_interpreter = Vector_Ref(Result, 1);
    Store_Env(Make_Non_Pointer(GLOBAL_ENV, GO_TO_GLOBAL));
    Set_Pure_Top();
    Band_Load_Hook();
    longjmp(*Back_To_Eval, PRIM_DO_EXPRESSION);
  }
  else
  { Back_To_Eval = Saved_Buf;
    Free = Save_Free;
    Free_Constant = Save_Free_Constant;
    Stack_Pointer = Save_Stack_Pointer;
    Set_Stack_Guard(Save_Stack_Guard);
    Undefined_Externals = Save_Undefined;
    Restore_Fixed_Obj(Save_FO);
    if (Jump_Value == PRIM_INTERRUPT)
    { printf("\nFile too large for memory.\n");
      Jump_Value = ERR_FASL_FILE_BAD_DATA;
    }
    Primitive_Error(Jump_Value);
  }
}

/* (CHARACTER-LIST-HASH LIST)
      [Primitive number 0x65]
      Takes a list of ASCII codes for characters and returns a hash
      code for them.  This uses the hashing function used to intern
      symbols in Fasload, and is really intended only for that
      purpose.
*/
Built_In_Primitive(Prim_Character_List_Hash, 1, "CHARACTER-LIST-HASH")
{ /* The work is done in Hash_Chars.
     A gross breach of modularity allows Hash_Chars to do the argument
     type checking.
  */
  Primitive_1_Arg();
  return Hash_Chars(Arg1);
}

/* (INTERN-CHARACTER-LIST LIST)
      [Primitive number 0xAB]
      LIST should consist of the ASCII codes for characters.  Returns
      a new (interned) symbol made out of these characters.  Notice
      that this is a fairly low-level primitive, and no checking is
      done on the characters except that they are in the range 0 to
      255.  Thus non-printing, lower-case, and special characters can
      be put into symbols this way.
*/
Built_In_Primitive(Prim_Intern_Character_List, 1, "INTERN-CHARACTER-LIST")
{ Primitive_1_Arg();
  return String_To_Symbol(Make_String(Arg1));
}

/* (SYMBOL->STRING STRING)
      [Primitive number 0x07]
      Similar to INTERN-CHARACTER-LIST, except this one takes a string
      instead of a list of ascii values as argument.
 */
Built_In_Primitive(Prim_String_To_Symbol, 1, "STRING->SYMBOL")
{ Primitive_1_Arg();
  Arg_1_Type(TC_CHARACTER_STRING);
  return String_To_Symbol(Arg1);
}

Pointer String_To_Symbol(String)
Pointer String;
{ Pointer New_Symbol, Interned_Symbol, *Orig_Free;
  Orig_Free = Free;
  New_Symbol = Make_Pointer(TC_UNINTERNED_SYMBOL, Free);
  Free[SYMBOL_NAME] = String;
  Free[SYMBOL_GLOBAL_VALUE] = UNBOUND_OBJECT;
  Free += 2;
  Interned_Symbol = New_Symbol;
  /* The work is done by Intern which returns in Interned_Symbol
     either the same symbol we gave it (in which case we need to check
     for GC) or an existing symbol (in which case we have to release
     the heap space acquired to hold New_Symbol).
  */
  Intern(&Interned_Symbol);
  if (Address(Interned_Symbol) == Address(New_Symbol))
  { Primitive_GC_If_Needed(0);	
  }
  else Free = Orig_Free;
  return Interned_Symbol;
}

#ifdef BYTE_INVERSION

#define MAGIC_OFFSET TC_FIXNUM+1

Pointer String_Chain, Last_String;
extern Boolean Byte_Invert_Fasl_Files;

Setup_For_String_Inversion()
{ if (!Byte_Invert_Fasl_Files) return;
  String_Chain = NIL;
  Last_String = NIL;
}

Finish_String_Inversion()
{ while (String_Chain != NIL)
  { long Count;
    Pointer Next;

    if (!Byte_Invert_Fasl_Files) return;

    Count = Get_Integer(Fast_Vector_Ref(String_Chain, STRING_HEADER));
    Count = 4*(Count-2)+Type_Code(String_Chain)-MAGIC_OFFSET;
    if (Reloc_Debug)
      printf("String at 0x%x: restoring length of %d.\n",
             Address(String_Chain), Count);
    Next = Fast_Vector_Ref(String_Chain, STRING_LENGTH);
    Fast_Vector_Set(String_Chain, STRING_LENGTH, FIXNUM_0+Count);
    String_Chain = Next;
  }
}

String_Inversion(Orig_Pointer)
Pointer *Orig_Pointer;
{ Pointer *Pointer_Address;
  char *To_Char;
  long Code;

  if (!Byte_Invert_Fasl_Files) return;

  Code = Type_Code(Orig_Pointer[STRING_LENGTH]);
  if (Code == TC_FIXNUM || Code == 0)	/* Already reversed? */
  { long Count, old_size, new_size, i;

    old_size = Get_Integer(Orig_Pointer[STRING_HEADER]);
    new_size = 
      2+(Get_Integer(Orig_Pointer[STRING_LENGTH]))/4;

    if (Reloc_Debug)
      printf("\nString at 0x%x with %d characters",
             Orig_Pointer,
             Get_Integer(Orig_Pointer[STRING_LENGTH]));

    if (old_size != new_size)
    { printf("\nWord count changed from %d to %d: ",
             old_size , new_size);
      printf("\nWhich, of course, is impossible!!\n");
      Microcode_Termination(TERM_EXIT);
    }

    Count = Get_Integer(Orig_Pointer[STRING_LENGTH])%4;
    if (Count==0) Count = 4;
    if (Last_String == NIL)
      String_Chain = Make_Pointer(Count+MAGIC_OFFSET, Orig_Pointer);
    else Fast_Vector_Set(Last_String, STRING_LENGTH,
			 Make_Pointer(Count+MAGIC_OFFSET, Orig_Pointer));
    Last_String = Make_Pointer(TC_NULL, Orig_Pointer);
    Orig_Pointer[STRING_LENGTH] = NIL;
    Count = Get_Integer(Orig_Pointer[STRING_HEADER])-1;
    if (Reloc_Debug) 
       printf("\nCell count=%d\n", Count);
    Pointer_Address = &(Orig_Pointer[STRING_CHARS]);
    To_Char = (char *) Pointer_Address;
    for (i=0; i < Count; i++, Pointer_Address++)
    { int C1, C2, C3, C4;
      C4 = Type_Code(*Pointer_Address) & 0xFF;
      C3 = (((long) *Pointer_Address)>>16) & 0xFF;
      C2 = (((long) *Pointer_Address)>>8) & 0xFF;
      C1 = ((long) *Pointer_Address) & 0xFF;
      if (Reloc_Debug || (old_size != new_size))
      { print_char(C1);
        print_char(C2);
        print_char(C3);
        print_char(C4);
      }
      *To_Char++ = C1;
      *To_Char++ = C2;
      *To_Char++ = C3;
      *To_Char++ = C4;
    }
  }
  if (Reloc_Debug) printf("\n");
}
#endif

