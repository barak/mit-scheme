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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fasload.c,v 9.34 1988/02/10 15:43:35 jinx Exp $

   The "fast loader" which reads in and relocates binary files and then
   interns symbols.  It is called with one argument: the (character
   string) name of a file to load.  It is called as a primitive, and
   returns a single object read in.
 */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"
#include "trap.h"
#include "load.c"

long
read_file_start(name)
     Pointer name;
{
  long value, heap_length;
  Boolean file_opened;

  if (OBJECT_TYPE(name) != TC_CHARACTER_STRING)
  {
    return (ERR_ARG_1_WRONG_TYPE);
  }

  file_opened = Open_Dump_File(name, OPEN_FLAG);

  if (Per_File)
  {
    Handle_Debug_Flags();
  }

  if (!file_opened)
  {
    return (ERR_ARG_1_BAD_RANGE);
  }

  value = Read_Header();
  if (value != FASL_FILE_FINE)
  {
    Close_Dump_File();
    switch (value)
    {
      /* These may want to be separated further. */
      case FASL_FILE_TOO_SHORT:
      case FASL_FILE_NOT_FASL:
      case FASL_FILE_BAD_MACHINE:
      case FASL_FILE_BAD_VERSION:
      case FASL_FILE_BAD_SUBVERSION:
        return (ERR_FASL_FILE_BAD_DATA);

      case FASL_FILE_BAD_PROCESSOR:
      case FASL_FILE_BAD_INTERFACE:
	return (ERR_FASLOAD_COMPILED_MISMATCH);
    }
  }  

  if (Or2(Reloc_Debug, File_Load_Debug))
  {
    print_fasl_information();
  }

  if (!Test_Pure_Space_Top(Free_Constant + Const_Count))
  {
    Close_Dump_File();
    return (ERR_FASL_FILE_TOO_BIG);
  }

  heap_length = Heap_Count + Primitive_Table_Size + Primitive_Table_Length;

  if (GC_Check(heap_length))
  {
    Close_Dump_File();
    Request_GC(heap_length);
    return (PRIM_INTERRUPT);
  }
  return (PRIM_DONE);
}

Pointer *
read_file_end()
{
  Pointer *table;

#if false
  /* Aligning Free here confuses the counters. */

  Align_Float(Free);
#endif

  if ((Load_Data(Heap_Count, ((char *) Free))) != Heap_Count)
  {
    Close_Dump_File();
    Primitive_Error(ERR_IO_ERROR);
  }
  NORMALIZE_REGION(((char *) Free), Heap_Count);
  Free += Heap_Count;

  if ((Load_Data(Const_Count, ((char *) Free_Constant))) != Const_Count)
  {
    Close_Dump_File();
    Primitive_Error(ERR_IO_ERROR);
  }
  NORMALIZE_REGION(((char *) Free_Constant), Const_Count);
  Free_Constant += Const_Count;

  table = Free;
  if ((Load_Data(Primitive_Table_Size, ((char *) Free))) !=
      Primitive_Table_Size)
  {
    Close_Dump_File();
    Primitive_Error(ERR_IO_ERROR);
  }
  NORMALIZE_REGION(((char *) table), Primitive_Table_Size);
  Free += Primitive_Table_Size;

#if false
  /* Same */
  
  Align_Float(Free);
#endif

  if (Close_Dump_File())
  {
    return (table);
  }
  else
  {
    Primitive_Error(ERR_IO_ERROR);
  }
}

/* Statics used by Relocate, below */

relocation_type
  heap_relocation,
  const_relocation,
  stack_relocation;

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
  {
    Result = ((Pointer *) (P + heap_relocation));
  }
  else if ((P >= Const_Base) && (P < Dumped_Constant_Top))
  {
    Result = ((Pointer *) (P + const_relocation));
  }
  else if ((P >= Dumped_Constant_Top) && (P < Dumped_Stack_Top))
  {
    Result = ((Pointer *) (P + stack_relocation));
  }
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
    Result = ((Pointer *) 0);
  }
  if (Reloc_Debug)
  {
    printf("0x%06x -> 0x%06x\n", P, Result);
  }
  return (Result);
}

#define Relocate_Into(Loc, P) (Loc) = Relocate(P)

#else /* not ENABLE_DEBUGGING_TOOLS */

#define Relocate_Into(Loc, P)						\
{									\
  if ((P) < Dumped_Heap_Top)						\
  {									\
    (Loc) = ((Pointer *) ((P) + heap_relocation));			\
  }									\
  else if ((P) < Dumped_Constant_Top)					\
  {									\
    (Loc) = ((Pointer *) ((P) + const_relocation));			\
  }									\
  else									\
  {									\
    (Loc) = ((Pointer *) ((P) + stack_relocation));			\
  }									\
}

#ifndef Conditional_Bug

#define Relocate(P)							\
((P < Const_Base) ?							\
 ((Pointer *) (P + heap_relocation)) :					\
 ((P < Dumped_Constant_Top) ?						\
  ((Pointer *) (P + const_relocation)) :				\
  ((Pointer *) (P + stack_relocation))))

#else /* Conditional_Bug */

static Pointer *Relocate_Temp;

#define Relocate(P)							\
  (Relocate_Into(Relocate_Temp, P), Relocate_Temp)

#endif /* Conditional_Bug */
#endif /* ENABLE_DEBUGGING_TOOLS */

/* Next_Pointer starts by pointing to the beginning of the block of
   memory to be handled.  This loop relocates all pointers in the
   block of memory.
*/

void
Relocate_Block(Next_Pointer, Stop_At)
     fast Pointer *Next_Pointer, *Stop_At;
{
  extern Pointer *load_renumber_table;

  if (Reloc_Debug)
  {
    fprintf(stderr,
	    "Relocation beginning, block = 0x%x, length = 0x%x, end = 0x%x.\n",
	    Next_Pointer, (Stop_At - Next_Pointer) - 1, Stop_At);
  }
  while (Next_Pointer < Stop_At)
  {
    fast Pointer Temp;

    Temp = *Next_Pointer;
    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
      case_Fasload_Non_Pointer:
        Next_Pointer += 1;
	break;
	
      case TC_PRIMITIVE:
	*Next_Pointer++ = load_renumber_table[PRIMITIVE_NUMBER(Temp)];
	break;
	
      case TC_PCOMB0:
	*Next_Pointer++ =
	  Make_Non_Pointer(TC_PCOMB0,
			   load_renumber_table[PRIMITIVE_NUMBER(Temp)]);
        break;

      case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += Get_Integer(Temp)+1;
        break;

#ifdef BYTE_INVERSION
      case TC_CHARACTER_STRING:
	String_Inversion(Relocate(Datum(Temp)));
	goto normal_pointer;
#endif

      case TC_REFERENCE_TRAP:
	if (Datum(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  Next_Pointer += 1;
	  break;
	}
	/* It is a pointer, fall through. */

      	/* Compiled entry points and stack environments work automagically. */
	/* This should be more strict. */
      default:
      {
normal_pointer:
	{
	  fast long Next;

	  Next = Datum(Temp);
	  *Next_Pointer++ = Make_Pointer(Type_Code(Temp), Relocate(Next));
	}
      }
    }
  }
  return;
}

extern void Intern();

void
Intern_Block(Next_Pointer, Stop_At)
     fast Pointer *Next_Pointer, *Stop_At;
{
  if (Reloc_Debug)
  {
    printf("Interning a block.\n");
  }

  while (Next_Pointer < Stop_At)
  {
    switch (Type_Code(*Next_Pointer))
    {
      case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += (1 + Get_Integer(*Next_Pointer));
        break;

      case TC_INTERNED_SYMBOL:
	if (Type_Code(Vector_Ref(*Next_Pointer, SYMBOL_GLOBAL_VALUE)) ==
	    TC_BROKEN_HEART)
	{
	  Pointer Old_Symbol;

	  Old_Symbol = *Next_Pointer;
	  Vector_Set(*Next_Pointer, SYMBOL_GLOBAL_VALUE, UNBOUND_OBJECT);

	  /* This is weird.  How come Intern is not checking? */
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

      default:
	Next_Pointer += 1;
	break;
    }
  }
  if (Reloc_Debug)
  {
    printf("Done interning block.\n");
  }
  return;
}

Pointer
load_file(from_band_load)
     Boolean from_band_load;
{
  Pointer
    *Heap_End, *Orig_Heap,
    *Constant_End, *Orig_Constant,
    *temp, *primitive_table;

  extern void install_primitive_table();
  extern Pointer *load_renumber_table;

  /* Read File */

#ifdef ENABLE_DEBUGGING_TOOLS
  Warned = false;
#endif

  load_renumber_table = Free;
  Free += Primitive_Table_Length;
  Orig_Heap = Free;
  Orig_Constant = Free_Constant;
  primitive_table = read_file_end();
  Constant_End = Free_Constant;
  heap_relocation = ((relocation_type) Orig_Heap) - Heap_Base;

  /*
    Magic!
    The relocation of compiled code entry points depends on the fact
    that fasdump never dumps a constant section.

    If the file is not a band, any pointers into constant space are
    pointers into the compiler utilities vector.  const_relocation is
    computed appropriately.

    Otherwise (the file is a band, and only bands can contain constant
    space segments) the utilities vector stuff is relocated
    automagically: the utilities vector is part of the band.
   */

  if ((!band_p) && (dumped_utilities != NIL))
  {
    extern Pointer compiler_utilities;

    if (compiler_utilities == NIL)
    {
      Primitive_Error(ERR_FASLOAD_COMPILED_MISMATCH);
    }

    const_relocation = (((relocation_type) Get_Pointer(compiler_utilities)) -
			Datum(dumped_utilities));
    Dumped_Constant_Top =
      C_To_Scheme(Nth_Vector_Loc(dumped_utilities,
				 (1 + Vector_Length(compiler_utilities))));
  }
  else
  {
    const_relocation = (((relocation_type) Orig_Constant) - Const_Base);
  }
  stack_relocation = ((relocation_type) Stack_Top) - Dumped_Stack_Top;

#ifdef BYTE_INVERSION
  Setup_For_String_Inversion();
#endif

  /* Setup the primitive table */
  
  install_primitive_table(primitive_table,
			  Primitive_Table_Length,
			  from_band_load);

  if (Reloc_Debug)
  {
    printf("heap_relocation = %d = %x; const_relocation = %d = %x\n",
	   heap_relocation, heap_relocation, 
           const_relocation,  const_relocation);
  }

  /*
    Relocate the new data.

    There are no pointers in the primitive table, thus
    there is no need to relocate it.
    */

  Relocate_Block(Orig_Heap, primitive_table);
  Relocate_Block(Orig_Constant, Free_Constant);

#ifdef BYTE_INVERSION
  Finish_String_Inversion();
#endif

  if (!from_band_load)
  {
    /* Again, there are no symbols in the primitive table. */

    Intern_Block(Orig_Heap, primitive_table);
    Intern_Block(Orig_Constant, Constant_End);
  }

  Set_Pure_Top();
  FASLOAD_RELOCATE_HOOK (Orig_Heap, primitive_table, Orig_Constant, Free_Constant);
  Relocate_Into(temp, Dumped_Object);
  return (*temp);
}

/* (BINARY-FASLOAD FILE-NAME)
   Load the contents of FILE-NAME into memory.  The file was
   presumably made by a call to PRIMITIVE-FASDUMP, and may contain
   data for the heap and/or the pure area.  The value returned is
   the object which was dumped.  Typically (but not always) this
   will be a piece of SCode which is then evaluated to perform
   definitions in some environment.
*/

DEFINE_PRIMITIVE("BINARY-FASLOAD", Prim_Binary_Fasload, 1)
{
  long result;
  Primitive_1_Arg();

  result = read_file_start(Arg1);
  if (band_p)
  {
    Primitive_Error(ERR_FASLOAD_BAND);
  }
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

DEFINE_PRIMITIVE("RELOAD-BAND-NAME", Prim_reload_band_name, 0)
{
  Primitive_0_Args();

  if (reload_band_name == NULL)
  {
    PRIMITIVE_RETURN(NIL);
  }

  PRIMITIVE_RETURN(C_String_To_Scheme_String(reload_band_name));
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

DEFINE_PRIMITIVE("LOAD-BAND", Prim_Band_Load, 1)
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

  length = ((long) (Fast_Vector_Ref(Arg1, STRING_LENGTH)));
  band_name = malloc(length);
  if (band_name != ((char *) NULL))
  {
    strcpy(band_name, Scheme_String_To_C_String(Arg1));
  }

  /* There is some jiggery-pokery going on here to make sure
     that all returns from Fasload (including error exits) return to
     the clean-up code before returning on up the C call stack.
  */

  saved_buf = Back_To_Eval;
  temp = setjmp(swapped_buf);
  if (temp != 0)
  {
    extern char *Error_Names[], *Abort_Names[];

    if (temp > 0)
    {
      fprintf(stderr,
	      "\nload-band: Error %d (%s) past the point of no return.\n",
	      temp, Error_Names[temp]);
    }
    else
    {
      fprintf(stderr,
	      "\nload-band: Abort %d (%s) past the point of no return.\n",
	      temp, Abort_Names[(-temp)-1]);
    }

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
  {
    free(reload_band_name);
  }
  reload_band_name = band_name;

  History = Make_Dummy_History();
  Initialize_Stack();
  Store_Return(RC_END_OF_COMPUTATION);
  Store_Expression(NIL);
  Save_Cont();
  Store_Expression(Vector_Ref(result, 0));
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

Setup_For_String_Inversion()
{
  String_Chain = NIL;
  Last_String = NIL;
  return;
}

Finish_String_Inversion()
{
  if (Byte_Invert_Fasl_Files)
  {
    while (String_Chain != NIL)
    {
      long Count;
      Pointer Next;

      Count = Get_Integer(Fast_Vector_Ref(String_Chain, STRING_HEADER));
      Count = 4*(Count-2)+Type_Code(String_Chain)-MAGIC_OFFSET;
      if (Reloc_Debug)
      {
	printf("String at 0x%x: restoring length of %d.\n",
	       Address(String_Chain), Count);
      }
      Next = Fast_Vector_Ref(String_Chain, STRING_LENGTH);
      Fast_Vector_Set(String_Chain, STRING_LENGTH, ((Pointer) (Count)));
      String_Chain = Next;
    }
  }
  return;
}

#define print_char(C) printf(((C < ' ') || (C > '|')) ?	\
			     "\\%03o" : "%c", (C && MAX_CHAR));

String_Inversion(Orig_Pointer)
     Pointer *Orig_Pointer;
{
  Pointer *Pointer_Address;
  char *To_Char;
  long Code;

  if (!Byte_Invert_Fasl_Files)
  {
    return;
  }

  Code = Type_Code(Orig_Pointer[STRING_LENGTH]);
  if (Code == 0)	/* Already reversed? */
  {
    long Count, old_size, new_size, i;

    old_size = Get_Integer(Orig_Pointer[STRING_HEADER]);
    new_size = 
      2 + (((long) (Orig_Pointer[STRING_LENGTH]))) / 4;

    if (Reloc_Debug)
    {
      printf("\nString at 0x%x with %d characters",
             Orig_Pointer,
             ((long) (Orig_Pointer[STRING_LENGTH])));
    }

    if (old_size != new_size)
    {
      printf("\nWord count changed from %d to %d: ",
             old_size , new_size);
      printf("\nWhich, of course, is impossible!!\n");
      Microcode_Termination(TERM_EXIT);
    }

    Count = ((long) (Orig_Pointer[STRING_LENGTH])) % 4;
    if (Count == 0)
    {
      Count = 4;
    }
    if (Last_String == NIL)
    {
      String_Chain = Make_Pointer(Count + MAGIC_OFFSET, Orig_Pointer);
    }
    else
    {
      Fast_Vector_Set(Last_String, STRING_LENGTH,
		      Make_Pointer(Count + MAGIC_OFFSET, Orig_Pointer));
    }

    Last_String = Make_Pointer(TC_NULL, Orig_Pointer);
    Orig_Pointer[STRING_LENGTH] = NIL;
    Count = Get_Integer(Orig_Pointer[STRING_HEADER]) - 1;
    if (Reloc_Debug) 
    {
       printf("\nCell count=%d\n", Count);
     }
    Pointer_Address = &(Orig_Pointer[STRING_CHARS]);
    To_Char = (char *) Pointer_Address;
    for (i = 0; i < Count; i++, Pointer_Address++)
    {
      int C1, C2, C3, C4;

      C4 = Type_Code(*Pointer_Address) & 0xFF;
      C3 = (((long) *Pointer_Address)>>16) & 0xFF;
      C2 = (((long) *Pointer_Address)>>8) & 0xFF;
      C1 = ((long) *Pointer_Address) & 0xFF;
      if (Reloc_Debug || (old_size != new_size))
      {
	print_char(C1);
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
  if (Reloc_Debug)
  {
    printf("\n");
  }
  return;
}
#endif /* BYTE_INVERSION */
