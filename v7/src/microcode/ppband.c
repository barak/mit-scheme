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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/ppband.c,v 9.26 1987/07/01 17:55:37 jinx Rel $
 *
 * Dumps Scheme FASL in user-readable form .
 */

#include "scheme.h"

/* These are needed by load.c */

static Pointer *Memory_Base;

long
Load_Data(Count, To_Where)
     long Count;
     FILE *To_Where;
{
  extern int fread();

  return (fread(To_Where, sizeof(Pointer), Count, stdin));
}

long
Write_Data()
{
  fprintf(stderr, "Write_Data called\n");
  exit(1);
}

Boolean
Open_Dump_File()
{
  fprintf(stderr, "Open_Dump_File called\n");
  exit(1);
}

Boolean
Close_Dump_File()
{
  fprintf(stderr, "Close_Dump_File called\n");
  exit(1);
}

#define Reloc_or_Load_Debug true

#include "load.c"
#include "gctype.c"

#ifdef Heap_In_Low_Memory
#ifdef spectrum
#define File_To_Pointer(P)	((((long) (P))&ADDRESS_MASK) / sizeof(Pointer))
#else
#define File_To_Pointer(P)	((P) / sizeof(Pointer))
#endif /* spectrum */
#else
#define File_To_Pointer(P)	(P)
#endif

#ifndef Conditional_Bug
#define Relocate(P)						\
	(((long) (P) < Const_Base) ?				\
	 File_To_Pointer(((long) (P)) - Heap_Base) :		\
	 (Heap_Count+File_To_Pointer(((long) (P)) - Const_Base)))
#else
#define Relocate_Into(What, P)
if (((long) (P)) < Const_Base)
  (What) = File_To_Pointer(((long) (P)) - Heap_Base);
else
  (What) = Heap_Count + File_To_Pointer(((long) P) - Const_Base);

static long Relocate_Temp;
#define Relocate(P)	(Relocate_Into(Relocate_Temp, P), Relocate_Temp)
#endif

static Pointer *Data, *end_of_memory;

Boolean
scheme_string(From, Quoted)
long From;
Boolean Quoted;
{
  fast long i, Count;
  fast char *Chars;

  Chars = (char *) &Data[From+STRING_CHARS];
  if (Chars < ((char *) end_of_memory))
  { Count = Get_Integer(Data[From+STRING_LENGTH]);
    if (&Chars[Count] < ((char *) end_of_memory))
    { putchar(Quoted ? '\"' : '\'');
      for (i=0; i < Count; i++) printf("%c", *Chars++);
      if (Quoted) putchar('\"');
      putchar('\n');
      return true;
    }
  }
  if (Quoted)
    printf("String not in memory; datum = %x\n", From);
  return false;
}

#define via(File_Address)	Relocate(Address(Data[File_Address]))

void
scheme_symbol(From)
long From;
{
  Pointer *symbol;

  symbol = &Data[From+SYMBOL_NAME];
  if ((symbol >= end_of_memory) ||
      !scheme_string(via(From+SYMBOL_NAME), false))
    printf("symbol not in memory; datum = %x\n", From);
  return;
}

void
Display(Location, Type, The_Datum)
     long Location, Type, The_Datum;
{
  long Points_To;

  printf("%5x: %2x|%6x     ", Location, Type, The_Datum);
  if (GC_Type_Map[Type & MAX_SAFE_TYPE] != GC_Non_Pointer)
    Points_To = Relocate((Pointer *) The_Datum);
  else
    Points_To = The_Datum;
  if (Type > MAX_SAFE_TYPE)
    printf("*");
  switch (Type & SAFE_TYPE_MASK)
  { /* "Strange" cases */
    case TC_NULL: if (The_Datum == 0)
                  { printf("NIL\n");
		    return;
		  }
                  else printf("[NULL ");
                  break;
    case TC_TRUE: if (The_Datum == 0)
                  { printf("TRUE\n");
		    return;
		  }
		  else printf("[TRUE ");
                  break;
    case TC_BROKEN_HEART: printf("[BROKEN-HEART ");
                          if (The_Datum == 0)
			    Points_To = 0;
                          break;
    case TC_MANIFEST_SPECIAL_NM_VECTOR: printf("[MANIFEST-SPECIAL-NM ");
                                        Points_To = The_Datum;
                                        break;
    case TC_MANIFEST_NM_VECTOR: printf("[MANIFEST-NM-VECTOR ");
                                Points_To = The_Datum;
                                break;
    case TC_INTERNED_SYMBOL: scheme_symbol(Points_To);
                             return;
    case TC_UNINTERNED_SYMBOL: 
      printf("uninterned ");
      scheme_symbol(Points_To);
      return;
    case TC_CHARACTER_STRING: scheme_string(Points_To, true);
                              return;
    case TC_FIXNUM: printf("%d\n", Points_To);
                    return;

    /* Default cases */
    case TC_LIST: printf("[LIST "); break;
    case TC_CHARACTER: printf("[CHARACTER "); break;
    case TC_SCODE_QUOTE: printf("[SCODE-QUOTE "); break;
    case TC_PCOMB2: printf("[PCOMB2 "); break;
    case TC_BIG_FLONUM: printf("[BIG-FLONUM "); break;
    case TC_COMBINATION_1: printf("[COMBINATION-1 "); break;
    case TC_EXTENDED_PROCEDURE: printf("[EXTENDED-PROCEDURE "); break;
    case TC_VECTOR: printf("[VECTOR "); break;
    case TC_RETURN_CODE: printf("[RETURN-CODE "); break;
    case TC_COMBINATION_2: printf("[COMBINATION-2 "); break;
    case TC_COMPILED_PROCEDURE: printf("[COMPILED-PROCEDURE "); break;
    case TC_BIG_FIXNUM: printf("[BIG-FIXNUM "); break;
    case TC_PROCEDURE: printf("[PROCEDURE "); break;
    case TC_PRIMITIVE_EXTERNAL: printf("[PRIMITIVE-EXTERNAL "); break;
    case TC_DELAY: printf("[DELAY "); break;
    case TC_ENVIRONMENT: printf("[ENVIRONMENT "); break;
    case TC_DELAYED: printf("[DELAYED "); break;
    case TC_EXTENDED_LAMBDA: printf("[EXTENDED-LAMBDA "); break;
    case TC_COMMENT: printf("[COMMENT "); break;
    case TC_NON_MARKED_VECTOR: printf("[NON-MARKED-VECTOR "); break;
    case TC_LAMBDA: printf("[LAMBDA "); break;
    case TC_PRIMITIVE: printf("[PRIMITIVE "); break;
    case TC_SEQUENCE_2: printf("[SEQUENCE-2 "); break;
    case TC_PCOMB1: printf("[PCOMB1 "); break;
    case TC_CONTROL_POINT: printf("[CONTROL-POINT "); break;
    case TC_ACCESS: printf("[ACCESS "); break;
    case TC_DEFINITION: printf("[DEFINITION "); break;
    case TC_ASSIGNMENT: printf("[ASSIGNMENT "); break;
    case TC_HUNK3: printf("[HUNK3 "); break;
    case TC_IN_PACKAGE: printf("[IN-PACKAGE "); break;
    case TC_COMBINATION: printf("[COMBINATION "); break;
    case TC_COMPILED_EXPRESSION: printf("[COMPILED-EXPRESSION "); break;
    case TC_LEXPR: printf("[LEXPR "); break;
    case TC_PCOMB3: printf("[PCOMB3 "); break;

    case TC_VARIABLE: printf("[VARIABLE "); break;
    case TC_THE_ENVIRONMENT: printf("[THE-ENVIRONMENT "); break;
    case TC_FUTURE: printf("[FUTURE "); break;
    case TC_VECTOR_1B: printf("[VECTOR-1B "); break;
    case TC_PCOMB0: printf("[PCOMB0 "); break;
    case TC_VECTOR_16B: printf("[VECTOR-16B "); break;
    case TC_SEQUENCE_3: printf("[SEQUENCE-3 "); break;
    case TC_CONDITIONAL: printf("[CONDITIONAL "); break;
    case TC_DISJUNCTION: printf("[DISJUNCTION "); break;
    case TC_CELL: printf("[CELL "); break;
    case TC_WEAK_CONS: printf("[WEAK-CONS "); break;
    case TC_REFERENCE_TRAP: printf("[REFERENCE-TRAP "); break;
    case TC_RETURN_ADDRESS: printf("[RETURN-ADDRESS "); break;
    case TC_COMPILER_LINK: printf("[COMPILER-LINK "); break;
    case TC_STACK_ENVIRONMENT: printf("[STACK-ENVIRONMENT "); break;
    case TC_COMPLEX: printf("[COMPLEX "); break;
    case TC_QUAD: printf("[QUAD "); break;

    default: printf("[0x%02x ", Type); break;
  }
  printf("%x]\n", Points_To);
}

main(argc, argv)
int argc;
char **argv;
{
  Pointer *Next;
  long i, total_length;
  if (argc == 1)
  {
    if (!Read_Header())
    { fprintf(stderr, "Input does not appear to be in FASL format.\n");
      exit(1);
    }
    printf("Dumped object at 0x%x\n", Relocate(Dumped_Object));
    if (Sub_Version >= FASL_LONG_HEADER)
      printf("External primitives at 0x%x\n\n", Relocate(Ext_Prim_Vector));
  }
  else
  {
    Const_Count = 0;
    sscanf(argv[1], "%x", &Heap_Base);
    sscanf(argv[2], "%x", &Const_Base);
    sscanf(argv[3], "%d", &Heap_Count);
    printf("Heap Base = 0x%08x; Constant Base = 0x%08x; Heap Count = %d\n",
	   Heap_Base, Const_Base, Heap_Count);
  }    
  Data = ((Pointer *) malloc(sizeof(Pointer) * (Heap_Count + Const_Count)));
  if (Data == NULL)
  {
    fprintf(stderr, "Allocation of %d words failed.\n", (Heap_Count + Const_Count));
    exit(1);
  }
  end_of_memory = &Data[Heap_Count + Const_Count];
  total_length = Load_Data(Heap_Count + Const_Count, Data);
  if (total_length != (Heap_Count + Const_Count))
  {
    printf("The FASL file does not have the right length.\n");
    printf("Expected %d objects.  Obtained %d objects.\n\n",
	   (Heap_Count + Const_Count), total_length);
    if (total_length < Heap_Count)
    {
      Heap_Count = total_length;
    }
    total_length -= Heap_Count;
    if (total_length < Const_Count)
      Const_Count = total_length;
  }
  printf("Heap contents:\n\n");
  for (Next = Data, i = 0; i < Heap_Count;  Next++, i++)
  {
    if (Safe_Type_Code(*Next) == TC_MANIFEST_NM_VECTOR)
    {
      long j, count;

      count = Get_Integer(*Next);
      Display(i, Type_Code(*Next), Address(*Next));
      Next += 1;
      for (j = 0; j < count ; j++, Next++)
        printf("          %02x%06x\n",
               Type_Code(*Next), Address(*Next));
      i += count;
      Next -= 1;
    }
    else
      Display(i, Type_Code(*Next),  Address(*Next));
  }
  printf("\n\nConstant space:\n\n");
  for (; i < Heap_Count + Const_Count;  Next++, i++)
  {
    if (Safe_Type_Code(*Next) == TC_MANIFEST_NM_VECTOR)
    {
      long j, count;

      count = Get_Integer(*Next);
      Display(i, Type_Code(*Next), Address(*Next));
      Next += 1;
      for (j = 0; j < count ; j++, Next++)
        printf("          %02x%06x\n",
               Type_Code(*Next), Address(*Next));
      i += count;
      Next -= 1;
    }
    else
      Display(i, Type_Code(*Next),  Address(*Next));
  }
}
