/* -*-C-*-

$Id: ppband.c,v 9.67 2007/01/05 15:33:07 cph Exp $

Copyright 1986,1987,1988,1989,1990,1992 Massachusetts Institute of Technology
Copyright 1993,1999,2000,2006 Massachusetts Institute of Technology

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

/* Dumps Scheme FASL in user-readable form. */

#include <stdio.h>
#include <ctype.h>
#include "config.h"
#include "errors.h"
#include "types.h"
#include "const.h"
#include "object.h"
#include "gccode.h"
#include "sdata.h"
#include "scheme.h"		/* For `fast' and several other niceties */

#ifdef STDC_HEADERS
#  include <stdlib.h>		/* For `malloc', `free' and `exit' (Linux) */
#  include <string.h>		/* For `strlen' and `strcpy' */
#else
   extern PTR EXFUN (malloc, (int));
   extern void EXFUN (free, (PTR));

   extern void EXFUN (exit, (int));

   extern int EXFUN (strcmp, (CONST char *, CONST char *));
   extern int EXFUN (strlen, (CONST char *));
#endif


#include "storage.c"		/* For `Type_Names' and "gctype.c" goodies */


#ifdef      ENABLE_DEBUGGING_TOOLS
#  ifndef   ENABLE_PPBAND_DEBUGGING_TOOLS
#    define ENABLE_PPBAND_DEBUGGING_TOOLS
#  endif
#endif
#if         ENABLE_PPBAND_DEBUGGING_TOOLS
#  ifndef   ENABLE_PPBAND_DEBUGGING_TOOLS_STORAGE_LAYOUT_DISPLAY
#    define ENABLE_PPBAND_DEBUGGING_TOOLS_STORAGE_LAYOUT_DISPLAY
#  endif
#endif
#if 0				/* Maybe make this a switch arg some day */
#define ENABLE_PPBAND_DEBUGGING_TOOLS_IMPLIES_EARLY_EXIT /* Header checks */
#endif
#    define ENABLE_PPBAND_DEBUGGING_TOOLS_STORAGE_LAYOUT_DISPLAY // MRB likes


#if (CHAR_BIT == 8)
#  if (SIZEOF_UNSIGNED_LONG == 4)	/* 32-bit word versions */
#    define UNSIGNED_LONG_HIGH_HALF(unsigned_long) ((unsigned_long) >> 16)
#    define UNSIGNED_LONG_LOW_HALF(unsigned_long)  ((unsigned_long) & 0xFFFF)
#  elif (SIZEOF_UNSIGNED_LONG == 8)	/* 32-bit word versions */
#    define UNSIGNED_LONG_HIGH_HALF(unsigned_long) ((unsigned_long) >> 32)
#    define UNSIGNED_LONG_LOW_HALF(unsigned_long)  ((unsigned_long) & 0xFFFFFFFF)
#  else
#    error "Unexpected SIZEOF_UNSIGNED_LONG for ppband."
#  endif
#else
#  error "`ppband' assumes that (CHAR_BIT == 8) is true."
#endif


#undef HEAP_MALLOC
#define HEAP_MALLOC malloc

/* This is needed when there is no compiler support.  Cf. "boot.c". */

void
DEFUN (gc_death, (code, message, scan, free),
       signed long code		/* So says "gccode.h", anyway. */
       AND char * message
       AND SCHEME_OBJECT * scan
       AND SCHEME_OBJECT * free)
{
  outf_fatal ("\n");
  outf_fatal ("gc_death [%s = 0x%lx]: %s.\n", Term_Names[code], code, message);
  outf_fatal ("scan = 0x%lx; free = 0x%lx\n", scan, free);

  exit (1);
}


/* These are needed by load.c */

#ifdef OS2

#include <fcntl.h>
#include <io.h>
#include <sys\types.h>

#define fread OS2_fread
extern off_t EXFUN (OS2_fread, (char *, unsigned int, off_t, FILE *));

#define fwrite OS2_fwrite
extern off_t EXFUN (OS2_fwrite, (char *, unsigned int, off_t, FILE *));

#endif /* OS2 */

unsigned long
DEFUN (Load_Data, (Count, To_Where), unsigned long Count AND SCHEME_OBJECT *To_Where)
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
#include "load.c"

#ifdef HEAP_IN_LOW_MEMORY
#  if defined(hp9000s800) || defined(__hp9000s800)
#    define File_To_Pointer(P)						\
        ((((unsigned long) (P)) & DATUM_MASK) / (sizeof (SCHEME_OBJECT)))
#  else
#    define File_To_Pointer(P)						\
         (((unsigned long) (P))               / (sizeof (SCHEME_OBJECT)))
#  endif /* [__]hp9000s800 */
#else
#  define File_To_Pointer(P)						\
        ((unsigned long) (P))
#endif /* HEAP_IN_LOW_MEMORY */


#ifndef Conditional_Bug

#  define Relocate(P)							\
(((unsigned long) (P) < Const_Base) ?					\
 (File_To_Pointer (((unsigned long) (P)) - Heap_Base)) :		\
 (Heap_Count + (File_To_Pointer (((unsigned long) (P)) - Const_Base))))

#else

   static unsigned long  Relocate_Temp;

#  define Relocate(P)							\
         (Relocate_Into (Relocate_Temp, P), Relocate_Temp)

#  define Relocate_Into(What, P)					\
	if (((unsigned long) (P)) < Const_Base)				\
	  (What) = (File_To_Pointer (((unsigned long) (P)) - Heap_Base))\
	else								\
	  (What) = (Heap_Count +					\
		    (File_To_Pointer (((unsigned long) P) - Const_Base)))

#endif /* Conditional_Bug */


static SCHEME_OBJECT *Data, *end_of_memory;

void
DEFUN (print_scheme_object_as_string, (string), char *string)
{
  int i;
  char *temp;
  unsigned char c;

  temp = string;
  putchar ('"');
  for (i = 0; i < (sizeof (SCHEME_OBJECT)); i++)
  {
    c = *temp++;
    if (isgraph ((int) c))
      putchar (c);
    else
      putchar (' ');
  }
  printf ("\" = ");

  temp = string;
  for (i = 0; i < (sizeof (SCHEME_OBJECT)); i++)
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
DEFUN (scheme_string, (From, Quoted), unsigned long From AND Boolean Quoted)
{
  fast unsigned long i, Count;
  fast char *Chars;

  Chars = ((char *) &Data[From +  STRING_CHARS]);
  if ((Chars < ((char *) end_of_memory))
      && (Chars >= ((char *) Data)))
  {
    Count = ((unsigned long) (Data[From + STRING_LENGTH_INDEX]));
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
    printf ("String not in memory; datum = 0x%lx\n", From);
  return (false);		/*        <0x><> */
}

#define via(File_Address) Relocate (OBJECT_DATUM (Data[File_Address]))

void
DEFUN (scheme_symbol, (From), unsigned long From)
{
  SCHEME_OBJECT *symbol;

  symbol = &Data[From+SYMBOL_NAME];
  if ((symbol >= end_of_memory) ||
      (!(scheme_string (via (From + SYMBOL_NAME), false))))
    printf ("Symbol not in memory; datum = 0x%lx\n", From);
  return; /*<S><>*/                    /* <0x><> */
}

#if (CHAR_BIT == 8)
#  if (SIZEOF_UNSIGNED_LONG == 4)	/* 32-bit word versions */
#    if (TYPE_CODE_LENGTH == 8) /* So DATUM_LENGTH == 24, so ----v */
#      define Display_LOC_TYPE_DAT_FORMAT_STRING "%6lx:    %2lx|%6lx   "
#      define Display_LOC_HILO_RAW_FORMAT_STRING "%6lx:                "\
                                                "[%04lx|%04lx]  =  "
#    endif
#    if (TYPE_CODE_LENGTH == 6)	/* So DATUM_LENGTH == 26, so ---v */
#      define Display_LOC_TYPE_DAT_FORMAT_STRING "%7lx:   %2lx|%7lx  "
#      define Display_LOC_HILO_RAW_FORMAT_STRING "%7lx:               "\
                                                "[%04lx|%04lx]  =  "
#    endif
#  elif (SIZEOF_UNSIGNED_LONG == 8)
#      define Display_LOC_TYPE_DAT_FORMAT_STRING "%7lx:   %2lx|%15lx  "
#      define Display_LOC_HILO_RAW_FORMAT_STRING "%7lx:               "\
                                                "[%08lx|%08lx]  =  "
#  else
#    error "Unexpected SIZEOF_UNSIGNED_LONG for ppband."
#  endif
#else
#  error "`ppband' assumes that (CHAR_BIT == 8) is true."
#endif

forward void EXFUN (Display, (unsigned long Location,
			      unsigned long Type,
			      unsigned long The_Datum));
void
DEFUN (Display_raw_type_dat_Scheme_object, (Location, Count, Area),
       fast unsigned long Location AND
       fast unsigned long Count AND
       fast SCHEME_OBJECT *Area)
{
  fast unsigned long i;

  for (i = 0; ((i < Count) && (Area+i < end_of_memory)); i += 1)
  {
    /* Show as deconstructed raw Scheme datum. */
    Display (Location+i, (OBJECT_TYPE ((* (Area+i)))),
	     /**/       (OBJECT_DATUM ((* (Area+i)))));
  }
}

void
DEFUN (Display_raw_hilo_hex_Scheme_object, (Location, Count, Area),
       fast unsigned long Location AND /* unused - For tracing */
       fast unsigned long Count AND
       fast SCHEME_OBJECT *Area)
{
  fast unsigned long i;

  for (i = 0; ((i < Count) && (Area+i < end_of_memory)); i += 1)
  {
    /* Show as raw hex anything that cannot be scanned as Scheme data. */
    printf (Display_LOC_HILO_RAW_FORMAT_STRING,
	    Location+i,
	    UNSIGNED_LONG_HIGH_HALF((unsigned long) (* (Area+i))),
	    UNSIGNED_LONG_LOW_HALF( (unsigned long) (* (Area+i))));
    print_scheme_object_as_string ((char *) (Area+i));
    putchar ('\n');
  }
}

#define PRINT_OBJECT(type, datum) do					\
{									\
  printf ("[%s 0x%lx]", type, datum);					\
} while (0) /*<0x><>*/

#define NON_POINTER(string) do						\
{									\
  the_string = string;							\
  Points_To = The_Datum;						\
} while (0)

#define POINTER(string) do						\
{									\
  the_string = string;							\
} while (0)

// char *Type_Names[] = TYPE_NAME_TABLE; /* We get this now from "storage.c" */

forward Boolean EXFUN (Display_constant, (unsigned long Location,
					  unsigned long Type,
					  unsigned long The_Datum));
void
DEFUN (Display, (Location, Type, The_Datum),
                 unsigned long Location AND
                 unsigned long Type AND
                 unsigned long The_Datum)
{
  char string_buf[100];
  char *the_string;
  unsigned long Points_To;

  printf (Display_LOC_TYPE_DAT_FORMAT_STRING, Location, Type, The_Datum);
  Points_To = Relocate ((SCHEME_OBJECT *) The_Datum);

  switch (Type)			/* I.e., `Switch_by_GC_Type(object)' */
  {
    /* "Strange" cases */

    case TC_NULL:
      if (The_Datum == 0)
      {
	printf ("#F\n");
	return;
      }
      NON_POINTER ("MANIFEST-VECTOR");	/* "types.h" defines this alias. */
      break;

    case TC_CONSTANT:
      {
	Boolean recognized_scheme_constant_p =
	  (Display_constant (Location, Type, The_Datum));

	if (recognized_scheme_constant_p)
	  return; /* Deed done. */
	else
	  NON_POINTER ("CONSTANT"); /* "const.h" implies this alias. */
      }
      break;

    /* Non-"Strange" Non_Pointer cases */

    case_TC_FIXNUMs:		/* Courtesy of "types.h" (q.v.) */
      PRINT_OBJECT ("FIXNUM", The_Datum);
      Points_To = (FIXNUM_TO_LONG ((MAKE_OBJECT (Type, The_Datum))));
      printf (" = %ld\n", ((signed long) Points_To));
      return;

      /* ------
       * Caveat:  The above special cases _must_ precede `case_Non_Pointer'.
       * ------
       *
       * As much as we'd dearly like to use `case_Non_Pointer' here
       * (defined from "gccode.h", q.v.), that results in duplicate
       * case values.  Instead, we enumerate the remaining Non_Pointer
       * cases in the order in which they would have expanded by using
       * `case_Non_Pointer' (with duplicates commented out).
       */
//  ----
//  From `case_simple_Non_Pointer' of "gccode.h":
//  ----
//  case TC_NULL:
//  case TC_CONSTANT:
    case TC_RETURN_CODE:
    case TC_THE_ENVIRONMENT:
//  ----
//  From `case_Fasload_Non_Pointer' of "gccode.h" (also includes preceding):
//  ----
//  case_TC_FIXNUMs:
    case TC_CHARACTER:
//  ----
//  From `case_Non_Pointer' of "gccode.h" (which alss includes the preceding):
//  ----
    case TC_PRIMITIVE:
    case TC_PCOMB0:
    case TC_STACK_ENVIRONMENT:
//  ----
//  From ``Missing Non Pointer types'' of "gccode.h".
//  ----
//  case TC_BROKEN_HEART:		Treated specially below...
    case TC_MANIFEST_NM_VECTOR:
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
//  case TC_RETURN_CODE:		Treated specially below...
    case TC_MANIFEST_CLOSURE:
    case TC_LINKAGE_SECTION:

      NON_POINTER (Type_Names[Type]);
      break;

    /* Special GC Fasdump_Pair of "gccode.h" */

    case TC_LIST:
      POINTER ("PAIR");		/* See comment for LIST in "sdata.h". */
      break;

    /* Non-Fasdump GC Pairs of "gccode.h" */

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

    /* Special case:  CHARACTER-STRING */

    case TC_CHARACTER_STRING:
      PRINT_OBJECT ("CHARACTER-STRING", Points_To);
      printf (" = ");
      scheme_string (Points_To, true);
      return;

    /* "Special" non-Non Pointer types of "gccode.h" */

    case TC_REFERENCE_TRAP:
      if (The_Datum <= TRAP_MAX_IMMEDIATE)
	NON_POINTER ("REFERENCE-TRAP");
      else
	POINTER ("REFERENCE-TRAP");
      break;

    case TC_BROKEN_HEART:
      if (The_Datum == 0)
	Points_To = 0;
      /* Fall through... */

    /* The rest are non-special Pointer types.  See "gccode.h" for details. */

    default:
      if (Type <= LAST_TYPE_CODE)
	POINTER (Type_Names[Type]);
      else
      {
	sprintf (&string_buf[0], "0x%02lx ", Type);
	POINTER (&string_buf[0]);
      }
      break;
  }

  /* The preceding will have established `the_string' and `Points_To'. */

  PRINT_OBJECT (the_string, Points_To);
  printf ("\tDatum = %ld (%lu)\n", ((signed long) Points_To), The_Datum);
  return;
}

#define RECOGNIZED_SCHEME_CONSTANT	TRUE

Boolean
DEFUN (Display_constant, (Location, Type, The_Datum),
			  unsigned long Location AND /* unused - For tracing */
			  unsigned long Type     AND /* unused - For tracing */
			  unsigned long The_Datum)
{
  switch (The_Datum)		/* See "const.h". */
    {
    case 0:			/* #t		*//* a.k.a. SHARP_T        */
      printf ("#T");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 1:			/* unspecific	*//* a.k.a. UNSPECIFIC     */
      printf ("UNSPECIFIC");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 2:			/* [non-object] */
      printf ("[Non-Object]");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 3:			/* #!optional	*/
      printf ("#!OPTIONAL");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 4:			/* #!rest	*/
      printf ("#!REST");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 5:			/* #!key	*/
      printf ("#!KEY");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 6:			/* #!eof	*/
      printf ("#!EOF");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 7:			/* #!default	*//* a.k.a. DEFAULT_OBJECT */
      printf ("DEFAULT_OBJECT");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 8:			/* #!aux	*/
      printf ("#!AUX");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    case 9:			/* '()		*//* a.k.a. EMPTY_LIST     */
      printf ("EMPTY_LIST");
      printf ("\n");
      return (RECOGNIZED_SCHEME_CONSTANT);

    default:
      return (! RECOGNIZED_SCHEME_CONSTANT);
    }
}

forward \
unsigned long EXFUN (show_area_raw_hex_count_for_special_non_pointer_types,
		     (fast SCHEME_OBJECT *));

SCHEME_OBJECT *
DEFUN (show_area, (area, start, end, name),
       fast SCHEME_OBJECT *area AND
       unsigned long start AND
       fast unsigned long end AND
       char *name)
{
  /*
   * Begin update of old ver.9.50 of 2000/12/05 (this file) to match the more
   *  current "uxtrap.c" ver.1.31 of 2001/12/16.  This file had bit rotted.
   *
   * Old code was botching counts so could get out of step w/ data in memory:
   *
   *   count =
   *	((OBJECT_TYPE (*area) == TC_LINKAGE_SECTION)
   *	 ? (READ_CACHE_LINKAGE_COUNT (*area))
   *	 : (OBJECT_DATUM (*area)));
   *
   * New code avoids direct counts in favor of direct `area' computations since
   *   the "cmpgc.h" COUNT/END macros automagically scale by entry sizes while
   *   also skipping intervening headers & format words and accommodating both
   *   short and long object formats for TC_MANIFEST_CLOSUREs (all of which the
   *   old bit-rotted code was now botching here in the future).
   *
   */

  fast unsigned long i;

  printf ("\n");
  printf ("\n===========================================================");
  printf ("\n%s contents:\n\n", name);

  for (i = start; i < end;  area++, i++)
  {
    /* Show object header */
    Display (i, (OBJECT_TYPE (*area)), (OBJECT_DATUM (*area)));

    /* Show object contents as raw hex bits if it's a non-scannable region */
    if (GC_Type_Special(*area))	/* Courtesy "gc.h"<-"gctype.c"<-"storage.c" */
    {
      fast unsigned long count;

      count = show_area_raw_hex_count_for_special_non_pointer_types(area);

      Display_raw_hilo_hex_Scheme_object ((i + 1), count, (area + 1));

      i    += count; /* Loopy `i++' will count the header we also Display'd. */
      area += count; /* Ditto `area++'. */
    }
  }
  return (area);
}

unsigned long
DEFUN (show_area_raw_hex_count_for_special_non_pointer_types, (area),
       fast SCHEME_OBJECT *area)
{
  fast unsigned long raw_hex_count; /* computed indirectly via `area_end' */

  /*
   * Begin update of old ver.9.50 of 2000/12/05 (this file) to match the more
   *  current "uxtrap.c" ver.1.31 of 2001/12/16.  This file had bit rotted.
   *
   * Old code was botching counts so could get out of step w/ data in memory.
   *
   * New code avoids direct counts in favor of direct `area' computations since
   *   the "cmpgc.h" COUNT/END macros automagically scale by entry sizes while
   *   also skipping intervening headers & format words and accommodating both
   *   short and long object formats for TC_MANIFEST_CLOSUREs (all of which the
   *   old bit-rotted code was now botching here in the future).
   *
   * For details, see fasdump.c, fasload.c, gcloop.c, purify.c, bintopsb.c, &c.
   *
   * For comparison, see skippy `uxtrap.c:find_block_address_in_area()' code.
   *
   * Note that we compute END-style ``one-object-shy-of-the-first-byte-after''
   * area pointers (called `area_end') since:  a) the END-macro based cases
   * already compute this directly, while:  b) the COUNT-macro based cases can
   * ``cheat'' by just adding the computed count to AREA, ignoring the header,
   * to (in effect) also compute an END-like area pointer one shy of the first
   * byte after the end of the object data.  This allows all cases to share
   * a common ``show-the-raw-hex'' code block at the end of the loop.
   *
   */

  fast SCHEME_OBJECT * area_end; /* value for AREA when pointing at last obj */
  {
      fast SCHEME_OBJECT object = (*area); /* current candidate */

      Switch_by_GC_Type(object)	/* Courtesy of "gccode.h" (q.v.) */
	{
	case TC_LINKAGE_SECTION:
	  {
	    switch (READ_LINKAGE_KIND (object))
	      {
	      case GLOBAL_OPERATOR_LINKAGE_KIND:
	      case        OPERATOR_LINKAGE_KIND:
		{
		  unsigned long \
		    count  = (READ_OPERATOR_LINKAGE_COUNT (object));
		  area_end =  (END_OPERATOR_LINKAGE_AREA (area, count));
		}
		break;

	      default:	/* This should never arise in true linkage sections. */
#if BAD_TYPES_LETHAL	/* This is handy for gdbugging:  please don't delete.*/
		{
		  char gc_death_message_buffer[100];

		  sprintf(gc_death_message_buffer,
			  "show_area:  Unknown compiler linkage kind (0x%lx).",
			  ((unsigned long) (OBJECT_TYPE (object))));

		  gc_death (TERM_EXIT, gc_death_message_buffer, area, NULL);
		  /*NOTREACHED*/
		}
#else
		/* Fall through, no reason to crash here. */
#endif
	      case       REFERENCE_LINKAGE_KIND:
	      case      ASSIGNMENT_LINKAGE_KIND:
	      case CLOSURE_PATTERN_LINKAGE_KIND:
		{
		  unsigned long \
		    count  = (READ_CACHE_LINKAGE_COUNT (object));
		  area_end = (area + count); /* Cheat:  ignores header */
		}
		break;
	      }	/* End `switch' on READ_LINKAGE_KIND */
	  }
	  break;

	case TC_MANIFEST_CLOSURE:
	  {
	    SCHEME_OBJECT * word_after_header = (area + 1); /* Cf. "cmpgc.h" */
	    {
	      unsigned long \
	        count  =  (MANIFEST_CLOSURE_COUNT (word_after_header));
	      area_end =  (MANIFEST_CLOSURE_END   (area, count)); /* Cheat!! */
	    }
	  }
	  break;

	case TC_MANIFEST_NM_VECTOR:
	  {
	    {
	      unsigned long \
	        count  = (OBJECT_DATUM (object));
	      area_end = (area + count); /* Cheat:  ignores header */
	    }
	  }
	  break;

	default:
	  /* Missing Non Pointer types (must always be treated specially):

	     TC_BROKEN_HEART
	     TC_MANIFEST_SPECIAL_NM_VECTOR
	     TC_REFERENCE_TRAP

	     ...are handled by the `Display' procedure w/o resorting to a block
	     of raw hex spewage, thank you very much.  MANIFEST_SPECIAL_NM_VECT
	     is just a noise header, for example, with no non-scannable data
	     following it.  The other two cases are handled similarly.
	  */
	  {
	    area_end = area;	/* i.e., nothing to show as raw bits, thanks */
	  }
	  break;

	} /* End `switch' GC_Type() case analysis */

    } /* End of `area_end' replacement for rotted old `count' computation */


  /* Compulsively name return values to make the code more self-documenting. */

  raw_hex_count = (area_end - area);

  return(raw_hex_count);
}

int
DEFUN (main, (argc, argv),
       int argc AND
       char **argv)
{
  int counter = 0;

  while (1)
  {
    fast SCHEME_OBJECT *Next = ((SCHEME_OBJECT *) NULL);
    unsigned long total_length, load_length;

#ifdef ENABLE_PPBAND_DEBUGGING_TOOLS_STORAGE_LAYOUT_DISPLAY
    /* debug hooks */
    unsigned long  Heap_first,  Heap_last,  Heap_size,  Heap_length,  Heap_top;
    unsigned long Const_first, Const_last, Const_size, Const_length, Const_top;
    unsigned long Prims_first, Prims_last, Prims_size, Prims_length;
    unsigned long CCode_first, CCode_last, CCode_size, CCode_length;

    /* debug hooks for symmetry w.r.t. Heap and Constant spaces */
    unsigned long Prims_count;
    unsigned long CCode_count;
#endif

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
    else if (argc == 4)	 /* Forge FASL header bases for RELOCATE/Data_Load() */
    {
      const char * mbase_format_string = "%lx";	/* sscanf warns if literals */
      const char * count_format_string = "%lu";

      /* Show only heap for FASL headerless data files */
      Const_Count = 0;
      Primitive_Table_Size = 0;
      C_Code_Table_Size = 0;

      /* Fake minimal bases to keep RELOCATE/Data_Load() happy */
      sscanf (argv[1], mbase_format_string, ((unsigned long) &Heap_Base));
      sscanf (argv[2], mbase_format_string, ((unsigned long) &Const_Base));
      sscanf (argv[3], count_format_string, ((unsigned long) &Heap_Count));
      printf ("Heap Base = 0x%lx; Constant Base = 0x%lx; Heap Count = %lu\n",
	      Heap_Base, Const_Base, Heap_Count);
    }

    else
    {
      printf("\nUsage: %s < FILE"
	     "\n       %s Heap_Base Const_Base Heap_Count < FILE"
	     "\n"
	     "\n where  FILE  is a fasdumped MIT Scheme file to inspect."
	     "\n",
	     argv[0], argv[0]);
      fprintf (stderr, "\nerror: %s: 0 or 3 arguments required (saw %u).\n",
	       argv[0], argc);
      exit (1);
      /* NOTREACHED */
    }

#ifdef ENABLE_PPBAND_DEBUGGING_TOOLS_STORAGE_LAYOUT_DISPLAY
    /*
    ** Fill in some handy debug hooks
    */

    /* Given:  tops */

    Heap_top  = Dumped_Heap_Top;
    Const_top = Dumped_Constant_Top;

    /* Given:  sizes and lengths */

    Prims_size   = Primitive_Table_Size;
    Prims_length = Primitive_Table_Length;

    CCode_size   =    C_Code_Table_Size;
    CCode_length =    C_Code_Table_Length;

    /* Derived:  sizes and lengths */

    Heap_size   = ( Heap_top -  Heap_Base);
    Heap_length = ( Heap_size >> 2);

    if (Const_Count == 0)
    {
     Const_size   = 0;
     Const_length = 0;
    }
    else
    {
     Const_size   = (Const_top - Const_Base);
     Const_length = (Const_size >> 2);
    }

    /* Derived:  firsts and lasts */

    Heap_first  = 0;
    Heap_last   = ( Heap_first +  Heap_length - 1);

    Const_first = ( Heap_last  + 1);
    Const_last  = (Const_first + Const_length - 1);

    Prims_first = (Const_last  + 1);
    Prims_last  = (Prims_first + Prims_length - 1);

    CCode_first = (Prims_last  + 1);
    CCode_last  = (CCode_first + CCode_length - 1);

    /* Derived:  counts */

    Prims_count = (Prims_last - Prims_first + 1);
    CCode_count = (CCode_last - CCode_first + 1);

    /* Show and tell */

    printf ("\n");
    printf ("\n-----------------------");
    printf ("\nPartition Configuration:");
    printf ("\n");
    printf ("\nHeap_first   = 0x%08lx (%10lu)",	Heap_first,	Heap_first);
    printf ("\nHeap_top     = 0x%08lx (%10lu)",	Heap_top,	Heap_top);
    printf ("\nHeap_Base    = 0x%08lx (%10lu)",	Heap_Base,	Heap_Base);
    printf ("\nHeap_size    = 0x%08lx (%10lu)",	Heap_size,	Heap_size);
    printf ("\nHeap_length  = 0x%08lx (%10lu)",	Heap_length,	Heap_length);
    printf ("\nHeap_Count   = 0x%08lx (%10lu)",	Heap_Count,	Heap_Count);
    printf ("\nHeap_last    = 0x%08lx (%10lu)",	Heap_last,	Heap_last);
    printf ("\n");
    printf ("\nConst_first  = 0x%08lx (%10lu)", Const_first,	Const_first);
    printf ("\nConst_top    = 0x%08lx (%10lu)",	Const_top,	Const_top);
    printf ("\nConst_Base   = 0x%08lx (%10lu)",	Const_Base,	Const_Base);
    printf ("\nConst_size   = 0x%08lx (%10lu)",	Const_size,	Const_size);
    printf ("\nConst_length = 0x%08lx (%10lu)",	Const_length,	Const_length);
    printf ("\nConst_Count  = 0x%08lx (%10lu)",	Const_Count,	Const_Count);
    printf ("\nConst_last   = 0x%08lx (%10lu)",	Const_last,	Const_last);
    printf ("\n");
    printf ("\nPrims_first  = 0x%08lx (%10lu)", Prims_first,	Prims_first);
    printf ("\nPrims_size   = 0x%08lx (%10lu)",	Prims_size,	Prims_size);
    printf ("\nPrims_length = 0x%08lx (%10lu)",	Prims_length,	Prims_length);
    printf ("\nPrims_count  = 0x%08lx (%10lu)",	Prims_count,	Prims_count);
    printf ("\nPrims_last   = 0x%08lx (%10lu)",	Prims_last,	Prims_last);
    printf ("\n");
    printf ("\nCCode_first  = 0x%08lx (%10lu)", CCode_first,	CCode_first);
    printf ("\nCCode_size   = 0x%08lx (%10lu)",	CCode_size,	CCode_size);
    printf ("\nCCode_length = 0x%08lx (%10lu)",	CCode_length,	CCode_length);
    printf ("\nCCode_count  = 0x%08lx (%10lu)",	CCode_count,	CCode_count);
    printf ("\nCCode_last   = 0x%08lx (%10lu)",	CCode_last,	CCode_last);
    printf ("\n");
    printf ("\n");

#ifdef ENABLE_PPBAND_DEBUGGING_TOOLS_IMPLIES_EARLY_EXIT
    exit(0);  /* Just wanted quick check of dump file header/partition info. */
#endif

#endif /* ENABLE_PPBAND_DEBUGGING_TOOLS_STORAGE_LAYOUT_DISPLAY */

    /*
    ** We allocate one Scheme object to serve as an end-of-memory sentinel, so
    ** the total allocation in units of Scheme objects is `load_length' plus 1.
    */
#define PPBAND_NUM_SCHEME_OBJECTS_TO_ALLOCATE	(load_length + 1) /* <EOM> */
#define PPBAND_NUM_DATA_WORDS_TO_ALLOCATE \
       (PPBAND_NUM_SCHEME_OBJECTS_TO_ALLOCATE * (sizeof (SCHEME_OBJECT)))
    /*
    ** Caveat:  The Heap_Count and Const_Count measure how many Scheme objs are
    **          in each area whereas Primitive_Table_Size and C_Code_Table_Size
    **          measure how many Scheme object sized parcels were dumped there.
    **          By contrast, the xx_Table_Length's measure how many table items
    **          were witnessed by the dump but _multiple_bytes_of_data_were_
    **          _dumped_for_each_item_witnessed_.  Don't get confused by this.
    */
    load_length = (Heap_Count + Const_Count + Primitive_Table_Size
		   /**/                     +    C_Code_Table_Size);
    Data = ((SCHEME_OBJECT *)
	    (malloc (PPBAND_NUM_DATA_WORDS_TO_ALLOCATE)));
    if (Data == NULL)
    {
      fprintf (stderr,
	       "Allocation of %lu words failed.\n",
	       PPBAND_NUM_DATA_WORDS_TO_ALLOCATE);
      exit (1);
    }
#ifdef ENABLE_PPBAND_DEBUGGING_TOOLS
    bzero(Data, PPBAND_DATA_WORDS_TO_ALLOCATE);
#endif
    total_length = (Load_Data (load_length, Data));
    end_of_memory = &Data[total_length];
    if (total_length != load_length)
    {
      printf ("The FASL file does not have the right length.\n");
      printf ("Expected %lu objects.  Obtained %lu objects.\n\n",
	      ((unsigned long) load_length), ((unsigned long) total_length));
      /*
       * The following truncates area counts/sizes upon running out of Data
       * space.  The first area that is too big to fit and all those checked
       * afterward will be ignored (dropped on the floor) as if not present.
       *
       * I'm not taking credit for this cleverness, just documenting the non-
       * obvious.  The code is straightforward once you know the intent.  -mrb
       */
      if (total_length < Heap_Count)
	Heap_Count = total_length;
      total_length -= Heap_Count;
      if (total_length < Const_Count)
	Const_Count = total_length;
      total_length -= Const_Count;
      if (total_length < Primitive_Table_Size)
	Primitive_Table_Size = total_length;
      total_length -= Primitive_Table_Size;
      if (total_length < C_Code_Table_Size)
	C_Code_Table_Size = total_length;
    }

    if (Heap_Count > 0)
      Next = show_area (Data, 0, Heap_Count, "Heap");
    if (Const_Count > 0)
      Next = show_area (Next, Heap_Count, (Heap_Count + Const_Count), "Constant Space");
    if ((Primitive_Table_Size > 0) && (Next < end_of_memory))
    {
        signed long arity;	/* Note:  LEXPR and UNKNOWN prim arity < 0. */
      unsigned long size;
      fast unsigned long entries, count;

      /* This is done in case the file is short.  See `<EOM>' marker above. */
      end_of_memory[0] = ((SCHEME_OBJECT) 0);
      end_of_memory[1] = ((SCHEME_OBJECT) 0);
      end_of_memory[2] = ((SCHEME_OBJECT) 0);
      end_of_memory[3] = ((SCHEME_OBJECT) 0);

      entries = Primitive_Table_Length;

      printf ("\n");

#ifdef ENABLE_PPBAND_DEBUGGING_TOOLS
      /*
       * For each primitive existent in the world, fasdump dumps its arity
       * and name string at the end of the fasdump file.  Spew them now.
       *
       * See <microcode/primutl.c>:copy_primitive_information() for details.
       *
       * For comparison, see <microcode/primutl.c>:install_primitive_table().
       *
       */
      printf ("\n===========================================================");
      printf ("\nRaw Scheme format of Primitive Table contents:\n");

      printf ("\n---------------");
      printf ("\nPrimitive table:  Number of entries = %lu (0x%03lx)\n\n",
	      entries, entries);

      Display_raw_type_dat_Scheme_object (0, entries, Next);


      printf ("\n===========================================================");
      printf ("\nRaw hex format of Primitive Table contents:\n");

      printf ("\n---------------");
      printf ("\nPrimitive table:  Number of entries = %lu (0x%03lx)\n\n",
	      entries, entries);

      Display_raw_hilo_hex_Scheme_object (0, entries, Next);

#endif /* ENABLE_PPBAND_DEBUGGING_TOOLS */

      /*
       * For each primitive existent in the world, fasdump dumps its arity
       * and name string at the end of the fasdump file.  Show them now.
       *
       * See <microcode/primutl.c>:copy_primitive_information() for details.
       *
       * For comparison, see <microcode/primutl.c>:install_primitive_table().
       *
       */
      printf ("\n===========================================================");
      printf ("\nPrimitive Table contents:\n");

      printf ("\n---------------");
      printf ("\nPrimitive table:  Number of entries = %lu (0x%03lx)\n\n",
	      entries, entries);

      for (count = 0;
	   ((count < entries) && (Next < end_of_memory));
	   count += 1)
      {
	arity = (FIXNUM_TO_LONG (*Next));
	Next += 1;
	size = (OBJECT_DATUM (*Next)); /* word count of Scheme char string */
#ifdef ENABLE_PPBAND_DEBUGGING_TOOLS
	printf ("size = %2lu; ", size);
#endif /**/           /* <0x><><%0> */
	printf ("Number = 0x%03lx; Arity = %2ld; Name = ", count, arity);
	scheme_string ((Next - Data), true);
	Next += (1 + size);
      }
      printf ("\n");
    }

    if ((C_Code_Table_Size > 0) && (Next < end_of_memory))
    {
      unsigned long dumped_initial_entry_number, nentries;
      fast unsigned long entries, count;

      /* This is done in case the file is short.  See `<EOM>' marker above. */
      end_of_memory[0] = ((SCHEME_OBJECT) 0);
      end_of_memory[1] = ((SCHEME_OBJECT) 0);
      end_of_memory[2] = ((SCHEME_OBJECT) 0);
      end_of_memory[3] = ((SCHEME_OBJECT) 0);

      entries = C_Code_Table_Length;

      printf ("\n");

#ifdef ENABLE_PPBAND_DEBUGGING_TOOLS
      /*
       * For each C code block existent in the world, fasdump dumps its entry
       * count and name string at the end of the fasdump file.  Spew them now.
       *
       * Details: see <microcode/cmpauxmd/c.c>:copy_c_code_block_information().
       *
       * For comparison, see <microcode/cmpauxmd/c.c>:install_c_code_table().
       *
       */
      printf ("\n===========================================================");
      printf ("\nRaw Scheme format of C Code Table contents:\n");

      printf ("\n------------");
      printf ("\nC Code table:  Number of entries = %lu\n\n", entries);

      /* See: <microcode/cmpauxmd/c.c>:cons_c_code_table(). */
      dumped_initial_entry_number = (FIXNUM_TO_ULONG (* Next));
      printf ("Initial Entry Number = %lu (0x%02lx)\n\n",
	      dumped_initial_entry_number,
	      dumped_initial_entry_number);

      Display_raw_type_dat_Scheme_object (0, entries, Next);


      printf ("\n===========================================================");
      printf ("\nRaw hex format of C Code Table contents:\n");

      printf ("\n------------");
      printf ("\nC Code table:  Number of entries = %lu\n\n", entries);

      /* See: <microcode/cmpauxmd/c.c>:cons_c_code_table(). */
      dumped_initial_entry_number = (FIXNUM_TO_ULONG (* Next));
      printf ("Initial Entry Number = %lu (0x%02lx)\n\n",
	      dumped_initial_entry_number,
	      dumped_initial_entry_number);

      Display_raw_hilo_hex_Scheme_object (0, entries, Next);

#endif /* ENABLE_PPBAND_DEBUGGING_TOOLS */

      /*
       * For each C code block existent in the world, fasdump dumps its entry
       * count and name string at the end of the fasdump file.  Show them now.
       *
       * Details: see <microcode/cmpauxmd/c.c>:copy_c_code_block_information().
       *
       * For comparison, see <microcode/cmpauxmd/c.c>:install_c_code_table().
       *
       */
      printf ("\n===========================================================");
      printf ("\nC Code Table contents:\n");

      printf ("\n------------");
      printf ("\nC Code table:  Number of entries = %lu (0x%03lx)\n\n",
	      entries, entries);

      /* See: <microcode/cmpauxmd/c.c>:cons_c_code_table(). */
      dumped_initial_entry_number = (FIXNUM_TO_ULONG (* Next));
      Next += 1;
      printf ("Initial Entry Number = %lu (0x%02lx)\n\n",
	      dumped_initial_entry_number,
	      dumped_initial_entry_number);

      for (count = 0;
	   ((count < entries) && (Next < end_of_memory));
	   count += 1)
      {
	int nlen, size;
	char * ncopy;

	nentries = (FIXNUM_TO_ULONG (*Next));
	Next += 1;
	nlen = (strlen ((char *) Next)); /* `fasdump'd a native C string */
	size = (nlen + 1);

	ncopy = ((char *) (malloc (size)));
	if (ncopy == ((char *) NULL))
	{
	  fprintf (stderr,
		   "Allocation of C code block no.%lu name string failed.\n",
		   count);
	  exit (1);
	  /*NOTREACHED*/
	}
	(void) strcpy (ncopy, ((char *) Next));

	printf ("Index = 0x%02lx; NEntries = %2lu; Name = \"%s\"\n",
		count, nentries, ncopy);
	printf ("size = %u\n", size);
	Next += (1 + nlen);
      }
      printf ("\n");
    }

    if (argc != 1)
      exit (0);
    free ((char *) Data);
    counter = 1;
  }
  return (0);
}
