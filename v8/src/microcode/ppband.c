/* -*-C-*-

$Id: 763ecef1d222c40e354c289dc989455c3ef3066c $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Dumps Scheme FASL in user-readable form. */

#include <stdio.h>
#include <ctype.h>
#include "ansidecl.h"
#include "config.h"
#include "errors.h"
#include "types.h"
#include "const.h"
#include "object.h"
#include "gccode.h"
#include "sdata.h"

#define fast register

#undef HEAP_MALLOC
#define HEAP_MALLOC malloc

/* These are needed when there is no compiler support. */

extern void EXFUN (gc_death,
		   (long code, char *, SCHEME_OBJECT *, SCHEME_OBJECT *));

extern char
  gc_death_message_buffer[];

void
DEFUN (gc_death, (code, message, scan, free),
       long code AND char * message
       AND SCHEME_OBJECT * scan AND SCHEME_OBJECT * free)
{
  fprintf (stderr, "gc_death: %s.\n", message);
  exit (1);
}

/* These are needed by load.c */

static SCHEME_OBJECT * memory_base;

#ifdef OS2

#include <fcntl.h>
#include <io.h>
#include <sys\types.h>

#define fread OS2_fread
extern off_t EXFUN (OS2_fread, (char *, unsigned int, off_t, FILE *));

#define fwrite OS2_fwrite
extern off_t EXFUN (OS2_fwrite, (char *, unsigned int, off_t, FILE *));

#endif /* OS2 */

long
DEFUN (Load_Data, (Count, To_Where), long Count AND SCHEME_OBJECT *To_Where)
{
#ifdef OS2
  setmode ((fileno (stdin)), O_BINARY);
#endif /* OS2 */

  return (fread (((char *) To_Where),
		 (sizeof (SCHEME_OBJECT)),
		 Count,
		 stdin));
}

#define INHIBIT_COMPILED_VERSION_CHECK
#define INHIBIT_CHECKSUMS
#define outf_error printf
#include "load.c"

#ifdef HEAP_IN_LOW_MEMORY
#ifdef hp9000s800
#  define File_To_Pointer(P)						\
    ((((long) (P)) & DATUM_MASK) / (sizeof (SCHEME_OBJECT)))
#else
#  define File_To_Pointer(P) ((P) / (sizeof (SCHEME_OBJECT)))
#endif /* hp9000s800 */
#else
#  define File_To_Pointer(P) (P)
#endif

#ifndef Conditional_Bug
#  define Relocate(P)							\
	(((long) (P) < Const_Base) ?					\
	 (File_To_Pointer (((long) (P)) - Heap_Base)) :			\
	 (Heap_Count + (File_To_Pointer (((long) (P)) - Const_Base))))
#else
#  define Relocate_Into(What, P)					\
if (((long) (P)) < Const_Base)						\
  (What) = (File_To_Pointer (((long) (P)) - Heap_Base));		\
else									\
  (What) = Heap_Count + (File_To_Pointer (((long) P) - Const_Base));

static long Relocate_Temp;
#  define Relocate(P)	(Relocate_Into (Relocate_Temp, P), Relocate_Temp)
#endif

static SCHEME_OBJECT *Data, *end_of_memory;

void
DEFUN (print_long_as_string, (string), char *string)
{
  int i;
  char *temp;
  unsigned char c;

  temp = string;
  putchar ('"');
  for (i = 0; i < (sizeof (long)); i++)
  {
    c = *temp++;
    if (isgraph ((int) c))
      putchar (c);
    else
      putchar (' ');
  }
  printf ("\" = ");

  temp = string;
  for (i = 0; i < (sizeof (long)); i++)
  {
    c = *temp++;
    if (isgraph ((int) c))
    {
      printf ("    ");
      putchar (c);
    }
    else
    {
      switch (c)
      {
	case '\0':
	  printf ("   \\0");
	  break;

	case ' ':
	  printf ("     ");
	  break;

#ifdef __STDC__
	case '\a':
#else
	case '\007':
#endif
	  printf ("   \\a");
	  break;

	case '\b':
	  printf ("   \\b");
	  break;

	case '\f':
	  printf ("   \\f");
	  break;

	case '\n':
	  printf ("   \\n");
	  break;

	case '\r':
	  printf ("   \\r");
	  break;

	case '\t':
	  printf ("   \\t");
	  break;

	case '\v':
	  printf ("   \\v");
	  break;

	default:
	  printf (" \\%03o", c);
	  break;
      }
    }
  }
  return;
}

Boolean
DEFUN (scheme_string, (From, Quoted), long From AND Boolean Quoted)
{
  fast long i, Count;
  fast char *Chars;

  Chars = ((char *) &Data[From +  STRING_CHARS]);
  if ((Chars < ((char *) end_of_memory))
      && (Chars >= ((char *) Data)))
  {
    Count = ((long) (Data[From + STRING_LENGTH_INDEX]));
    if (&Chars[Count] < ((char *) end_of_memory))
    {
      if (Quoted)
	putchar ('\"');
      for (i = 0; i < Count; i++)
	printf ("%c", *Chars++);
      if (Quoted)
	putchar ('\"');
      putchar ('\n');
      return (true);
    }
  }
  if (Quoted)
    printf ("String not in memory; datum = %lx\n", From);
  return (false);
}

#define via(File_Address) Relocate (OBJECT_DATUM (Data[File_Address]))

void
DEFUN (scheme_symbol, (From), long From)
{
  SCHEME_OBJECT *symbol;

  symbol = &Data[From+SYMBOL_NAME];
  if ((symbol >= end_of_memory) ||
      (!(scheme_string (via (From + SYMBOL_NAME), false))))
    printf ("symbol not in memory; datum = %lx\n", From);
  return;
}

static char string_buffer[10];

#define PRINT_OBJECT(type, datum) do					\
{									\
  printf ("[%s %lx]", type, datum);					\
} while (0)

#define NON_POINTER(string) do						\
{									\
  the_string = string;							\
  Points_To = The_Datum;						\
  break;								\
} while (0)

#define POINTER(string) do						\
{									\
  the_string = string;							\
  break;								\
} while (0)

char *Type_Names[] = TYPE_NAME_TABLE;

void
DEFUN (Display, (Location, Type, The_Datum),
                 long Location AND
                 long Type AND
                 long The_Datum)
{
  char string_buf[100];
  char *the_string;
  long Points_To;

  printf ("%5lx: %2lx|%6lx     ", Location, Type, The_Datum);
  Points_To = Relocate ((SCHEME_OBJECT *) The_Datum);

  switch (Type)
  { /* "Strange" cases */
    case TC_NULL:
      if (The_Datum == 0)
      {
	printf ("#F\n");
	return;
      }
      NON_POINTER ("NULL");

    case TC_CONSTANT:
      if (The_Datum == 0)
      {
	printf ("#T\n");
	return;
      }
      /* fall through */


    case TC_CHARACTER:
    case TC_RETURN_CODE:
    case TC_PRIMITIVE:
    case TC_THE_ENVIRONMENT:
    case TC_PCOMB0:
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
    case TC_MANIFEST_NM_VECTOR:
      NON_POINTER (Type_Names[Type]);

    case TC_INTERNED_SYMBOL:
      PRINT_OBJECT ("INTERNED-SYMBOL", Points_To);
      printf (" = ");
      scheme_symbol (Points_To);
      return;

    case TC_UNINTERNED_SYMBOL:
      PRINT_OBJECT ("UNINTERNED-SYMBOL", Points_To);
      printf (" = ");
      scheme_symbol (Points_To);
      return;

    case TC_CHARACTER_STRING:
      PRINT_OBJECT ("CHARACTER-STRING", Points_To);
      printf (" = ");
      scheme_string (Points_To, true);
      return;

    case TC_POSITIVE_FIXNUM:
    case TC_NEGATIVE_FIXNUM:
      PRINT_OBJECT ("FIXNUM", The_Datum);
      Points_To = (FIXNUM_TO_LONG ((MAKE_OBJECT (Type, The_Datum))));
      printf (" = %ld\n", Points_To);
      return;

    case TC_REFERENCE_TRAP:
      if (The_Datum <= TRAP_MAX_IMMEDIATE)
	NON_POINTER ("REFERENCE-TRAP");
      else
	POINTER ("REFERENCE-TRAP");

    case TC_BROKEN_HEART:
      if (The_Datum == 0)
	Points_To = 0;
    default:
      if (Type <= LAST_TYPE_CODE)
	POINTER (Type_Names[Type]);
      else
      {
	sprintf (&string_buf[0], "0x%02lx ", Type);
	POINTER (&string_buf[0]);
      }
  }
  PRINT_OBJECT (the_string, Points_To);
  putchar ('\n');
  return;
}

SCHEME_OBJECT *
DEFUN (show_area, (area, start, end, name),
       fast SCHEME_OBJECT *area AND
       long start AND
       fast long end AND
       char *name)
{
  fast long i;

  printf ("\n%s contents:\n\n", name);
  for (i = start; i < end;  area++, i++)
  {
    if (((OBJECT_TYPE (*area)) == TC_MANIFEST_NM_VECTOR) ||
	((OBJECT_TYPE (*area)) == TC_MANIFEST_CLOSURE) ||
	((OBJECT_TYPE (*area)) == TC_LINKAGE_SECTION))
    {
      fast long j, count;

      count =
	((OBJECT_TYPE (*area) == TC_LINKAGE_SECTION)
	 ? (READ_CACHE_LINKAGE_COUNT (*area))
	 : (OBJECT_DATUM (*area)));
      Display (i, (OBJECT_TYPE (*area)), (OBJECT_DATUM (*area)));
      area += 1;
      for (j = 0; j < count ; j++, area++)
      {
        printf ("          %08lx    = ", ((unsigned long) (*area)));
	print_long_as_string ((char *) area);
	putchar ('\n');
      }
      i += count;
      area -= 1;
    }
    else
      Display (i, (OBJECT_TYPE (*area)), (OBJECT_DATUM (*area)));
  }
  return (area);
}

void
DEFUN (main, (argc, argv),
       int argc AND
       char **argv)
{
  int counter = 0;

  while (1)
  {
    fast SCHEME_OBJECT *Next;
    long total_length, load_length;

    if (argc == 1)
    {
      switch (Read_Header ())
      {
	case FASL_FILE_FINE :
	  if (counter != 0)
	    printf ("\f\n\t*** New object ***\n\n");
          break;

	  /* There should really be a difference between no header
	     and a short header.
	   */

	case FASL_FILE_TOO_SHORT:
	  exit (0);

	default:
	{
	  fprintf (stderr,
		   "%s: Input does not appear to be in correct FASL format.\n",
		   argv[0]);
	  exit (1);
	  /* NOTREACHED */
	}
      }
      print_fasl_information ();
      printf ("Dumped object (relocated) at 0x%lx\n",
	      (Relocate (Dumped_Object)));
    }
    else
    {
      Const_Count = 0;
      Primitive_Table_Size = 0;
      sscanf (argv[1], "%lx", ((long) &Heap_Base));
      sscanf (argv[2], "%lx", ((long) &Const_Base));
      sscanf (argv[3], "%ld", ((long) &Heap_Count));
      printf ("Heap Base = 0x%lx; Constant Base = 0x%lx; Heap Count = %ld\n",
	      Heap_Base, Const_Base, Heap_Count);
    }

    load_length = (Heap_Count + Const_Count + Primitive_Table_Size);
    Data = ((SCHEME_OBJECT *)
	    (malloc (sizeof (SCHEME_OBJECT) * (load_length + 4))));
    if (Data == NULL)
    {
      fprintf (stderr, "Allocation of %ld words failed.\n", (load_length + 4));
      exit (1);
    }
    total_length = (Load_Data (load_length, Data));
    end_of_memory = &Data[total_length];
    if (total_length != load_length)
    {
      printf ("The FASL file does not have the right length.\n");
      printf ("Expected %ld objects.  Obtained %ld objects.\n\n",
	      ((long) load_length), ((long) total_length));
      if (total_length < Heap_Count)
	Heap_Count = total_length;
      total_length -= Heap_Count;
      if (total_length < Const_Count)
	Const_Count = total_length;
      total_length -= Const_Count;
      if (total_length < Primitive_Table_Size)
	Primitive_Table_Size = total_length;
    }

    if (Heap_Count > 0)
      Next = show_area (Data, 0, Heap_Count, "Heap");
    if (Const_Count > 0)
      Next = show_area (Next, Heap_Count, Const_Count, "Constant Space");
    if ((Primitive_Table_Size > 0) && (Next < end_of_memory))
    {
      long arity, size;
      fast long entries, count;

      /* This is done in case the file is short. */
      end_of_memory[0] = ((SCHEME_OBJECT) 0);
      end_of_memory[1] = ((SCHEME_OBJECT) 0);
      end_of_memory[2] = ((SCHEME_OBJECT) 0);
      end_of_memory[3] = ((SCHEME_OBJECT) 0);

      entries = Primitive_Table_Length;
      printf ("\nPrimitive table: number of entries = %ld\n\n", entries);

      for (count = 0;
	   ((count < entries) && (Next < end_of_memory));
	   count += 1)
      {
	arity = (FIXNUM_TO_LONG (*Next));
	Next += 1;
	size = (OBJECT_DATUM (*Next));
	printf ("Number = %3lx; Arity = %2ld; Name = ", count, arity);
	scheme_string ((Next - Data), true);
	Next += (1 + size);
      }
      printf ("\n");
    }
    if (argc != 1)
      exit (0);
    free ((char *) Data);
    counter = 1;
  }
}
