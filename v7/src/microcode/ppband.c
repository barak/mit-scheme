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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/ppband.c,v 9.31 1988/02/10 15:42:58 jinx Exp $
 *
 * Dumps Scheme FASL in user-readable form .
 */

#include <stdio.h>
#include "config.h"
#include "types.h"
#include "const.h"
#include "object.h"
#include "sdata.h"

#define fast register

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

#define INHIBIT_COMPILED_VERSION_CHECK
#include "load.c"

#ifdef Heap_In_Low_Memory
#ifdef spectrum
#define File_To_Pointer(P)	((((long) (P)) & ADDRESS_MASK) / sizeof(Pointer))
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
	 (Heap_Count + File_To_Pointer(((long) (P)) - Const_Base)))
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

  Chars = ((char *) &Data[From +  STRING_CHARS]);
  if (Chars < ((char *) end_of_memory))
  {
    Count = ((long) (Data[From + STRING_LENGTH]));
    if (&Chars[Count] < ((char *) end_of_memory))
    {
      if (Quoted)
      {
	putchar('\"');
      }
      for (i = 0; i < Count; i++)
      {
	printf("%c", *Chars++);
      }
      if (Quoted)
      {
	putchar('\"');
      }
      putchar('\n');
      return (true);
    }
  }
  if (Quoted)
  {
    printf("String not in memory; datum = %lx\n", From);
  }
  return (false);
}

#define via(File_Address)	Relocate(OBJECT_DATUM(Data[File_Address]))

void
scheme_symbol(From)
     long From;
{
  Pointer *symbol;

  symbol = &Data[From+SYMBOL_NAME];
  if ((symbol >= end_of_memory) ||
      (!(scheme_string(via(From + SYMBOL_NAME), false))))
  {
    printf("symbol not in memory; datum = %lx\n", From);
  }
  return;
}

static char string_buffer[10];

#define PRINT_OBJECT(type, datum)					\
{									\
  printf("[%s %lx]", type, datum);					\
}

#define NON_POINTER(string)						\
{									\
  the_string = string;							\
  Points_To = The_Datum;						\
  break;								\
}

#define POINTER(string)							\
{									\
  the_string = string;							\
  break;								\
}

void
Display(Location, Type, The_Datum)
     long Location, Type, The_Datum;
{
  char *the_string;
  long Points_To;

  printf("%5lx: %2lx|%6lx     ", Location, Type, The_Datum);
  Points_To = Relocate((Pointer *) The_Datum);

  switch (Type)
  { /* "Strange" cases */
    case TC_NULL:
      if (The_Datum == 0)
      {
	printf("NIL\n");
	return;
      }
      NON_POINTER("NULL");

    case TC_TRUE:
      if (The_Datum == 0)
      {
	printf("TRUE\n");
	return;
      }
      NON_POINTER("TRUE");

    case TC_MANIFEST_SPECIAL_NM_VECTOR:
      NON_POINTER("MANIFEST-SPECIAL-NM");

    case TC_MANIFEST_NM_VECTOR:
      NON_POINTER("MANIFEST-NM-VECTOR");

    case TC_BROKEN_HEART:
      if (The_Datum == 0)
      {
	Points_To = 0;
      }
      POINTER("BROKEN-HEART");

    case TC_INTERNED_SYMBOL:
      PRINT_OBJECT("INTERNED-SYMBOL", Points_To);
      printf(" = ");
      scheme_symbol(Points_To);
      return;

    case TC_UNINTERNED_SYMBOL: 
      PRINT_OBJECT("UNINTERNED-SYMBOL", Points_To);
      printf(" = ");
      scheme_symbol(Points_To);
      return;

    case TC_CHARACTER_STRING:
      PRINT_OBJECT("CHARACTER-STRING", Points_To);
      printf(" = ");
      scheme_string(Points_To, true);
      return;

    case TC_FIXNUM:
      PRINT_OBJECT("FIXNUM", The_Datum);
      Sign_Extend(The_Datum, Points_To);
      printf(" = %ld\n", Points_To);
      return;

    case TC_REFERENCE_TRAP:
      if (The_Datum <= TRAP_MAX_IMMEDIATE)
      {
	NON_POINTER("REFERENCE-TRAP");
      }
      else
      {
	POINTER("REFERENCE-TRAP");
      }

    case TC_CHARACTER:			NON_POINTER("CHARACTER");
    case TC_RETURN_CODE:		NON_POINTER("RETURN-CODE");
    case TC_PRIMITIVE:			NON_POINTER("PRIMITIVE");
    case TC_THE_ENVIRONMENT:		NON_POINTER("THE-ENVIRONMENT");
    case TC_PCOMB0:			NON_POINTER("PCOMB0");
    case TC_LIST:			POINTER("LIST");
    case TC_SCODE_QUOTE:		POINTER("SCODE-QUOTE");
    case TC_PCOMB2:			POINTER("PCOMB2");
    case TC_BIG_FLONUM:			POINTER("FLONUM");

    case TC_COMBINATION_1:		POINTER("COMBINATION-1");
    case TC_EXTENDED_PROCEDURE:		POINTER("EXTENDED-PROCEDURE");
    case TC_VECTOR:			POINTER("VECTOR");
    case TC_COMBINATION_2:		POINTER("COMBINATION-2");
    case TC_COMPILED_PROCEDURE:		POINTER("COMPILED-PROCEDURE");
    case TC_BIG_FIXNUM:			POINTER("BIG-FIXNUM");
    case TC_PROCEDURE:			POINTER("PROCEDURE");
    case TC_DELAY:			POINTER("DELAY");
    case TC_ENVIRONMENT:		POINTER("ENVIRONMENT");
    case TC_DELAYED:			POINTER("DELAYED");
    case TC_EXTENDED_LAMBDA:		POINTER("EXTENDED-LAMBDA");
    case TC_COMMENT:			POINTER("COMMENT");
    case TC_NON_MARKED_VECTOR:		POINTER("NON-MARKED-VECTOR");
    case TC_LAMBDA:			POINTER("LAMBDA");
    case TC_SEQUENCE_2:			POINTER("SEQUENCE-2");
    case TC_PCOMB1:			POINTER("PCOMB1");
    case TC_CONTROL_POINT:		POINTER("CONTROL-POINT");
    case TC_ACCESS:			POINTER("ACCESS");
    case TC_DEFINITION:			POINTER("DEFINITION");
    case TC_ASSIGNMENT:			POINTER("ASSIGNMENT");
    case TC_HUNK3_A:			POINTER("HUNK3_A");
    case TC_HUNK3_B:			POINTER("HUNK3-B");
    case TC_IN_PACKAGE:			POINTER("IN-PACKAGE");
    case TC_COMBINATION:		POINTER("COMBINATION");
    case TC_COMPILED_EXPRESSION:	POINTER("COMPILED-EXPRESSION");
    case TC_LEXPR:			POINTER("LEXPR");
    case TC_PCOMB3:			POINTER("PCOMB3");
    case TC_VARIABLE:			POINTER("VARIABLE");
    case TC_FUTURE:			POINTER("FUTURE");
    case TC_VECTOR_1B:			POINTER("VECTOR-1B");
    case TC_VECTOR_16B:			POINTER("VECTOR-16B");
    case TC_SEQUENCE_3:			POINTER("SEQUENCE-3");
    case TC_CONDITIONAL:		POINTER("CONDITIONAL");
    case TC_DISJUNCTION:		POINTER("DISJUNCTION");
    case TC_CELL:			POINTER("CELL");
    case TC_WEAK_CONS:			POINTER("WEAK-CONS");
    case TC_RETURN_ADDRESS:		POINTER("RETURN-ADDRESS");
    case TC_COMPILER_LINK:		POINTER("COMPILER_LINK");
    case TC_STACK_ENVIRONMENT:		POINTER("STACK-ENVIRONMENT");
    case TC_COMPLEX:			POINTER("COMPLEX");
    case TC_QUAD:			POINTER("QUAD");
    case TC_COMPILED_CODE_BLOCK:	POINTER("COMPILED-CODE-BLOCK");

    default:
      sprintf(&the_string[0], "0x%02lx ", Type);
      POINTER(&the_string[0]);
  }
  PRINT_OBJECT(the_string, Points_To);
  putchar('\n');
  return;
}

Pointer *
show_area(area, size, name)
     fast Pointer *area;
     fast long size;
     char *name;
{
  fast long i;

  printf("\n%s contents:\n\n", name);
  for (i = 0; i < size;  area++, i++)
  {
    if (OBJECT_TYPE(*area) == TC_MANIFEST_NM_VECTOR)
    {
      fast long j, count;

      count = Get_Integer(*area);
      Display(i, OBJECT_TYPE(*area), OBJECT_DATUM(*area));
      area += 1;
      for (j = 0; j < count ; j++, area++)
      {
        printf("          %02lx%06lx\n",
               OBJECT_TYPE(*area), OBJECT_DATUM(*area));
      }
      i += count;
      area -= 1;
    }
    else
    {
      Display(i, OBJECT_TYPE(*area),  OBJECT_DATUM(*area));
    }
  }
  return (area);
}

main(argc, argv)
     int argc;
     char **argv;
{
  fast Pointer *Next;
  long total_length, load_length;

  if (argc == 1)
  {
    if (Read_Header() != FASL_FILE_FINE)
    {
      fprintf(stderr,
	      "%s: Input does not appear to be in correct FASL format.\n",
	      argv[0]);
      exit(1);
    }
    print_fasl_information();
    printf("Dumped object (relocated) at 0x%lx\n", Relocate(Dumped_Object));
  }
  else
  {
    Const_Count = 0;
    Primitive_Table_Size = 0;
    sscanf(argv[1], "%x", &Heap_Base);
    sscanf(argv[2], "%x", &Const_Base);
    sscanf(argv[3], "%d", &Heap_Count);
    printf("Heap Base = 0x%lx; Constant Base = 0x%lx; Heap Count = %ld\n",
	   Heap_Base, Const_Base, Heap_Count);
  }    

  load_length = (Heap_Count + Const_Count + Primitive_Table_Size);
  Data = ((Pointer *) malloc(sizeof(Pointer) * (load_length + 4)));
  if (Data == NULL)
  {
    fprintf(stderr, "Allocation of %ld words failed.\n", (load_length + 4));
    exit(1);
  }
  total_length = Load_Data(load_length, Data);
  end_of_memory = &Data[total_length];
  if (total_length != load_length)
  {
    printf("The FASL file does not have the right length.\n");
    printf("Expected %d objects.  Obtained %ld objects.\n\n",
	   load_length, total_length);
    if (total_length < Heap_Count)
    {
      Heap_Count = total_length;
    }
    total_length -= Heap_Count;
    if (total_length < Const_Count)
    {
      Const_Count = total_length;
    }
    total_length -= Const_Count;
    if (total_length < Primitive_Table_Size)
    {
      Primitive_Table_Size = total_length;
    }
  }

  if (Heap_Count > 0)
  {
    Next = show_area(Data, Heap_Count, "Heap");
  }
  if (Const_Count > 0)
  {
    Next = show_area(Next, Const_Count, "Constant Space");
  }
  if ((Primitive_Table_Size > 0) && (Next < end_of_memory))
  {
    long arity, size;
    fast long entries, count;

    /* This is done in case the file is short. */
    end_of_memory[0] = ((Pointer) 0);
    end_of_memory[1] = ((Pointer) 0);
    end_of_memory[2] = ((Pointer) 0);
    end_of_memory[3] = ((Pointer) 0);

    entries = Primitive_Table_Length;
    printf("\nPrimitive table: number of entries = %ld\n\n", entries);

    for (count = 0;
	 ((count < entries) && (Next < end_of_memory));
	 count += 1)
    {
      Sign_Extend(*Next++, arity);
      size = Get_Integer(*Next);
      printf("Number = %3lx; Arity = %2ld; Name = ", count, arity);
      scheme_string((Next - Data), true);
      Next += (1 + size);
    }
    printf("\n");
  }
  exit(0);
}
