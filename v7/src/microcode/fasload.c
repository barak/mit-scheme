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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fasload.c,v 9.27 1987/06/05 04:14:38 jinx Exp $

   The "fast loader" which reads in and relocates binary files and then
   interns symbols.  It is called with one argument: the (character
   string) name of a file to load.  It is called as a primitive, and
   returns a single object read in.
 */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"
#include "trap.h"

#define CCheck_or_Reloc_Debug Or2(Consistency_Check, Reloc_Debug)
#define Reloc_or_Load_Debug   Or2(Reloc_Debug, File_Load_Debug)

#include "load.c"

long
read_file_start(name)
     Pointer name;
{
  Boolean file_opened;

  if (Type_Code(name) != TC_CHARACTER_STRING)
    return ERR_ARG_1_WRONG_TYPE;

  file_opened = Open_Dump_File(name, OPEN_FLAG);

  if (Per_File)
    Handle_Debug_Flags();

  if (!file_opened)
    return ERR_ARG_1_BAD_RANGE;

  if (!Read_Header())
    goto cannot_load;
  
  if (File_Load_Debug)
    printf("\nMachine type %d, Version %d, Subversion %d\n",
           Machine_Type, Version, Sub_Version);

#ifdef BYTE_INVERSION
  if ((Sub_Version != FASL_SUBVERSION))
#else
  if ((Sub_Version != FASL_SUBVERSION) ||
      (Machine_Type != FASL_INTERNAL_FORMAT))
#endif

  {
    fprintf(stderr,
	    "\nread_file: FASL File Version %4d Subversion %4d Machine Type %4d.\n",
	    Version, Sub_Version , Machine_Type);
    fprintf(stderr,
	    "           Expected: Version %4d Subversion %4d Machine Type %4d.\n",
	   FASL_FORMAT_VERSION, FASL_SUBVERSION, FASL_INTERNAL_FORMAT);

cannot_load:

    Close_Dump_File();
    return ERR_FASL_FILE_BAD_DATA;
  }

  if (!Test_Pure_Space_Top(Free_Constant + Const_Count))
  {
    Close_Dump_File();
    return ERR_FASL_FILE_TOO_BIG;
  }

  if (GC_Check(Heap_Count))
  {
    Close_Dump_File();
    Request_GC(Heap_Count);
    return PRIM_INTERRUPT;
  }
  return PRIM_DONE;
}

void
read_file_end()
{
  /* Aligning Free here confuses the counters
     Align_Float(Free);
   */
  if (Load_Data(Heap_Count, ((char *) Free)) != Heap_Count)
  {
    Close_Dump_File();
    Primitive_Error(ERR_EXTERNAL_RETURN);
  }

#ifdef BYTE_INVERSION
  Byte_Invert_Region((char *) Free, Heap_Count);
#endif

  Free += Heap_Count;
  if (Load_Data(Const_Count, ((char *) Free_Constant)) != Const_Count)
  {
    Close_Dump_File();
    Primitive_Error(ERR_EXTERNAL_RETURN);
  }

#ifdef BYTE_INVERSION
  Byte_Invert_Region((char *) Free_Constant, Const_Count);
#endif

  Free_Constant += Const_Count;

  /* Same 
     Align_Float(Free);
   */

  if (Close_Dump_File())
    return;
  else
    Primitive_Error(ERR_EXTERNAL_RETURN);
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

Pointer *
Relocate(P)
     long P;
{
  Pointer *Result;

  if ((P >= Heap_Base) && (P < Dumped_Heap_Top))
    Result = (Pointer *) (P + Heap_Relocation);
  else if ((P >= Const_Base) && (P < Dumped_Constant_Top))
    Result = (Pointer *) (P + Const_Reloc);
  else if (P < Dumped_Stack_Top)
    Result = (Pointer *) (P + Stack_Relocation);
  else
  {
    printf("Pointer out of range: 0x%x\n", P, P);
    if (!Warned)
    {
      printf("Heap: %x-%x, Constant: %x-%x, Stack: ?-0x%x\n",
             Heap_Base, Dumped_Heap_Top,
             Const_Base, Dumped_Constant_Top, Dumped_Stack_Top);
      Warned = true;
    }
    Result = (Pointer *) 0;
  }
  if (Reloc_Debug)
    printf("0x%06x -> 0x%06x\n", P, Result);
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

long
Relocate_Block(Next_Pointer, Stop_At)
     fast Pointer *Next_Pointer, *Stop_At;
{
  if (Reloc_Debug)
    fprintf(stderr,
	    "Relocation beginning, block=0x%x, length=0x%x, end=0x%x.\n",
	    Next_Pointer, (Stop_At-Next_Pointer)-1, Stop_At);
  while (Next_Pointer < Stop_At)
  {
    fast Pointer Temp;

    Temp = *Next_Pointer;
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

      case TC_REFERENCE_TRAP:
	if (Datum(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  Next_Pointer += 1;
	  break;
	}
	/* It is a pointer, fall through. */
      case_compiled_entry_point:
      	/* Compiled entry points work automagically. */
      default:
      {
	fast long Next;

	Next = Datum(Temp);
	*Next_Pointer++ = Make_Pointer(Type_Code(Temp), Relocate(Next));
      }
    }
  }
}

extern void Intern();

void
Intern_Block(Next_Pointer, Stop_At)
     Pointer *Next_Pointer, *Stop_At;
{
  if (Reloc_Debug)
    printf("Interning a block.\n");

  while (Next_Pointer <= Stop_At)	/* BBN has < for <= */
  {
    switch (Type_Code(*Next_Pointer))
    { case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += Get_Integer(*Next_Pointer)+1;
        break;

      case TC_INTERNED_SYMBOL:
      if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_GLOBAL_VALUE)) ==
          TC_BROKEN_HEART)
      {
	Pointer Old_Symbol;

	Old_Symbol = *Next_Pointer;
        Vector_Set(*Next_Pointer, SYMBOL_GLOBAL_VALUE, UNBOUND_OBJECT);
        Intern(Next_Pointer);
        Primitive_GC_If_Needed(0);
        if (*Next_Pointer != Old_Symbol)
        {
	  Vector_Set(Old_Symbol, SYMBOL_NAME,
		     Make_New_Pointer(TC_BROKEN_HEART, *Next_Pointer));
        }
      }
      else if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_NAME)) ==
              TC_BROKEN_HEART)
      {
	*Next_Pointer =
          Make_New_Pointer(Type_Code(*Next_Pointer),
                           Fast_Vector_Ref(*Next_Pointer,
					   SYMBOL_NAME));
      }
      Next_Pointer += 1;
      break;
      
      default: Next_Pointer += 1;
    }
  }
  if (Reloc_Debug)
    printf("Done interning block.\n");
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

void
Install_Ext_Prims(normal_fasload)
     Boolean normal_fasload;
{
  long i;
  Pointer *Next;

  Vector_Set(Ext_Prim_Vector, 0, 
	     Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, Ext_Prim_Count));
  Next = Nth_Vector_Loc(Ext_Prim_Vector, 1);
  if (normal_fasload)
  {
    for (i = 0; i < Ext_Prim_Count; i++)
      Intern(Next++);
  }
  else
    Undefined_Externals = NIL;
  return;
}

void
Update_Ext_Prims(Next_Pointer, Stop_At)
     fast Pointer *Next_Pointer, *Stop_At;
{
  extern long make_external_primitive();

  for ( ; Next_Pointer < Stop_At; Next_Pointer++)
  { switch (Type_Code(*Next_Pointer))
    { case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += Get_Integer(*Next_Pointer);
        break;

      case TC_PRIMITIVE_EXTERNAL:
      {
	long Which;

	Which = Address(*Next_Pointer);

	if (Which > Ext_Prim_Count)
	  fprintf(stderr, "\nExternal Primitive 0x%x out of range.\n", Which);
	else
	{
	  Pointer New_Value;

	  New_Value = User_Vector_Ref(Ext_Prim_Vector, Which);
	  if (Type_Code(New_Value) == TC_INTERNED_SYMBOL)
	  {
	    New_Value = ((Pointer) make_external_primitive(New_Value, TRUTH));
	    User_Vector_Set(Ext_Prim_Vector, Which, New_Value);
	  }
	  Store_Address(*Next_Pointer, New_Value);
	}
      }		 

      default: break;
    }
  }
  return;
}

Pointer
load_file(from_band_load)
     Boolean from_band_load;
{
  Pointer *Heap_End, *Constant_End, *Orig_Heap, *Orig_Constant, *Xtemp;

  /* Read File */

#ifdef ENABLE_DEBUGGING_TOOLS
  Warned = false;
#endif

  Orig_Heap = Free;
  Orig_Constant = Free_Constant;
  read_file_end();
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

  if (!from_band_load)
  {
    Intern_Block(Orig_Constant, Constant_End);
    Intern_Block(Orig_Heap, Heap_End);
  }

  /* Update External Primitives */

  if ((Ext_Prim_Vector != NIL) && Found_Ext_Prims)
  {
    Relocate_Into(Xtemp, Address(Ext_Prim_Vector));
    Ext_Prim_Vector = *Xtemp;
    Ext_Prim_Count = Vector_Length(Ext_Prim_Vector);
    Install_Ext_Prims(!from_band_load);
    Update_Ext_Prims(Orig_Heap, Free);
    Update_Ext_Prims(Orig_Constant, Free_Constant);
  }

  Set_Pure_Top();
  Relocate_Into(Xtemp, Dumped_Object);
  return *Xtemp;
}

/* (BINARY-FASLOAD FILE-NAME)
   Load the contents of FILE-NAME into memory.  The file was
   presumably made by a call to PRIMITIVE-FASDUMP, and may contain
   data for the heap and/or the pure area.  The value returned is
   the object which was dumped.  Typically (but not always) this
   will be a piece of SCode which is then evaluated to perform
   definitions in some environment.
*/
Built_In_Primitive(Prim_Binary_Fasload, 1, "BINARY-FASLOAD", 0x57)
{
  long result;
  Primitive_1_Arg();

  result = read_file_start(Arg1);
  if (result != PRIM_DONE)
  {
    if (result == PRIM_INTERRUPT)
    {
      Primitive_Interrupt();
    }
    else
    {
      Primitive_Error(result);
    }
  }
  PRIMITIVE_RETURN(load_file(false));
}

/* Band loading. */

static char *reload_band_name = ((char *) NULL);


/* (RELOAD-BAND-NAME)
   Returns the filename (as a Scheme string) from which the runtime system
   was band loaded (load-band'ed ?), or NIL if the system was fasl'ed.
*/
Built_In_Primitive(Prim_reload_band_name, 0, "RELOAD-BAND-NAME", 0x1A3)
{
  Primitive_0_Args();

  if (reload_band_name == NULL)
    return NIL;

  return C_String_To_Scheme_String(reload_band_name);
}

/* Utility for load band below. */

extern void compiler_reset_error();

void 
compiler_reset_error()
{
  fprintf(stderr,
	  "\ncompiler_restart_error: The band being restored and\n");
  fprintf(stderr,
	  "the compiled code interface in this microcode are inconsistent.\n");
  Microcode_Termination(TERM_COMPILER_DEATH);
}

/* (LOAD-BAND FILE-NAME)
   Restores the heap and pure space from the contents of FILE-NAME,
   which is typically a file created by DUMP-BAND.  The file can,
   however, be any file which can be loaded with BINARY-FASLOAD.
*/
Built_In_Primitive(Prim_Band_Load, 1, "LOAD-BAND", 0xB9)
{
  extern char *malloc();
  extern strcpy(), free();
  extern void compiler_reset();
  extern Pointer compiler_utilities;

  jmp_buf swapped_buf, *saved_buf;
  Pointer *saved_free, *saved_free_constant, *saved_stack_pointer;
  long temp, length;
  Pointer result;
  char *band_name;
  Primitive_1_Arg();

  saved_free = Free;
  Free = Heap_Bottom;
  saved_free_constant = Free_Constant;
  Free_Constant = Constant_Space;
  saved_stack_pointer = Stack_Pointer;
  Stack_Pointer = Highest_Allocated_Address;

  result = read_file_start(Arg1);
  if (result != PRIM_DONE)
  {
    Free = saved_free;
    Free_Constant = saved_free_constant;
    Stack_Pointer = saved_stack_pointer;

    if (result == PRIM_INTERRUPT)
    {
      Primitive_Interrupt();
    }
    else
    {
      Primitive_Error(result);
    }
  }

  /* Point of no return. */

  length = Get_Integer(Fast_Vector_Ref(Arg1, STRING_LENGTH));
  band_name = malloc(length);
  if (band_name != ((char *) NULL))
    strcpy(band_name, Scheme_String_To_C_String(Arg1));

  /* There is some jiggery-pokery going on here to make sure
     that all returns from Fasload (including error exits) return to
     the clean-up code before returning on up the C call stack.
  */

  saved_buf = Back_To_Eval;
  temp = setjmp(swapped_buf);
  if (temp != 0)
  {
    fprintf(stderr,
	    "\nload-band: Error %d past the point of no return.\n",
	    temp);
    if (band_name != ((char *) NULL))
    {
      fprintf(stderr, "band-name = \"%s\".\n", band_name);
      free(band_name);
    }
    Microcode_Termination(TERM_DISK_RESTORE);
    /*NOTREACHED*/
  }

  Back_To_Eval = ((jmp_buf *) swapped_buf);
  result = load_file(true);
  Back_To_Eval = saved_buf;

  if (reload_band_name != ((char *) NULL))
    free(reload_band_name);
  reload_band_name = band_name;

  History = Make_Dummy_History();
  Initialize_Stack();
  Store_Return(RC_END_OF_COMPUTATION);
  Store_Expression(NIL);
  Save_Cont();
  Store_Expression(Vector_Ref(result, 0));

  /* Primitive externals handled by load_file */

  compiler_utilities = Vector_Ref(result, 1);
  compiler_reset(compiler_utilities);
  Store_Env(Make_Non_Pointer(GLOBAL_ENV, GO_TO_GLOBAL));
  Set_Pure_Top();
  Band_Load_Hook();
  PRIMITIVE_ABORT(PRIM_DO_EXPRESSION);
  /*NOTREACHED*/
}

#ifdef BYTE_INVERSION

#define MAGIC_OFFSET (TC_FIXNUM + 1)

Pointer String_Chain, Last_String;
extern Boolean Byte_Invert_Fasl_Files;

Setup_For_String_Inversion()
{
  if (!Byte_Invert_Fasl_Files)
    return;
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
    Fast_Vector_Set(String_Chain, STRING_LENGTH, Make_Unsigned_Fixnum(Count));
    String_Chain = Next;
  }
}

#define print_char(C) printf(((C < ' ') || (C > '|')) ?	\
			     "\\%03o" : "%c", (C && MAX_CHAR));

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
#endif /* BYTE_INVERSION */
