/* -*-C-*-

$Id: psbmap.h,v 9.44 2000/12/05 21:23:48 cph Exp $

Copyright (c) 1987-2000 Massachusetts Institute of Technology

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

/* This file contains macros and declarations for "bintopsb.c"
   and "psbtobin.c". 
 */

#ifndef PSBMAP_H_INCLUDED
#define PSBMAP_H_INCLUDED

/* These definitions insure that the appropriate code is extracted
   from the included files.
*/

#define fast register

#include "config.h"
#include <stdio.h>
#ifdef STDC_HEADERS
#  include <stdlib.h>
#endif
#include "types.h"
#include "object.h"
#include "bignum.h"
#include "bignmint.h"
#include "bitstr.h"
#include "sdata.h"
#include "const.h"
#include "gccode.h"
#include "cmptype.h"
#define boolean Boolean
#include "comlin.h"

#ifndef COMPILER_PROCESSOR_TYPE
#define COMPILER_PROCESSOR_TYPE COMPILER_NONE_TYPE
#endif

extern double
  EXFUN (frexp, (double, int *)),
  EXFUN (ldexp, (double, int));

#define PORTABLE_VERSION	6

/* Number of objects which, when traced recursively, point at all other
   objects dumped.
   Currently the dumped object, and the compiler utilities.
 */

#define NROOTS			2

/* Types to recognize external object references.  Any occurrence of these
   (which are external types and thus handled separately) means a reference
   to an external object.
 */

#define CONSTANT_CODE			TC_FIXNUM
#define HEAP_CODE			TC_CHARACTER
#define PURE_CODE			TC_BIG_FIXNUM

#define fixnum_to_bits			FIXNUM_LENGTH
#define hex_digits(nbits)		(((nbits) + 3) / 4)

#define to_pointer BYTES_TO_WORDS

#define float_to_pointer						\
  BYTES_TO_WORDS(sizeof(double))

#ifndef FLOATING_ALIGNMENT

#define flonum_to_pointer(nfloats)					\
  ((nfloats) * (1 + float_to_pointer))

#else /* FLOATING_ALIGNMENT */

/* When computing the space needed for flonums, the worst case is that
   every flonum needs alignment.  To estimate the space needed, add
   padding to each flonum to round it up to an alignment boundary.  */

#define flonum_to_pointer(nfloats)					\
  ((nfloats)								\
   * (((((1 + float_to_pointer) * (sizeof (char)))			\
	& FLOATING_ALIGNMENT)						\
       == 0)								\
      ? (1 + float_to_pointer)						\
      : ((((1 + float_to_pointer) * (sizeof (char)))			\
	  + ((FLOATING_ALIGNMENT + 1)					\
	     - (((1 + float_to_pointer) * (sizeof (char)))		\
		& FLOATING_ALIGNMENT)))					\
	 / (sizeof (char)))))

#endif /* FLOATING_ALIGNMENT */

#define char_to_pointer(nchars)						\
  BYTES_TO_WORDS(nchars)

#define pointer_to_char(npoints)					\
  ((npoints) * sizeof(SCHEME_OBJECT))

/* Status flags */

#define COMPACT_P	(1 << 0)
#define NULL_NMV_P	(1 << 1)
#define COMPILED_P	(1 << 2)
#define NMV_P		(1 << 3)
#define BAND_P		(1 << 4)
#define C_CODE_P	(1 << 5)

#define MAKE_FLAGS()							\
(  (compact_p ? COMPACT_P : 0)						\
 | (null_nmv_p ? NULL_NMV_P : 0)					\
 | (compiled_p ? COMPILED_P : 0)					\
 | (nmv_p ? NMV_P : 0)							\
 | (band_p ? BAND_P : 0)						\
 | (c_compiled_p ? C_CODE_P : 0))

#define READ_FLAGS(f) do						\
{									\
  compact_p = ((f) & COMPACT_P);					\
  null_nmv_p  = ((f) & NULL_NMV_P);					\
  compiled_p = ((f) & COMPILED_P);					\
  nmv_p = ((f) & NMV_P);						\
  band_p = ((f) & BAND_P);						\
  c_compiled_p = ((f) & C_CODE_P);					\
} while (0)

/*
  If true, make all integers fixnums if possible, and all strings as
  short as possible (trim extra stuff).
 */

static Boolean compact_p = true;

/* If true, null out all elements of random non-marked vectors. */

static Boolean null_nmv_p = false;

/* If true, the portable file contains compiled code. */

static Boolean compiled_p = false;

/* If true, the portable file contains "random" non-marked vectors. */

static Boolean nmv_p = false;

#define TC_C_COMPILED_TAG			TC_MANIFEST_CLOSURE
#define C_COMPILED_FAKE_NMV			0
#define C_COMPILED_ENTRY_FORMAT			1
#define C_COMPILED_ENTRY_CODE			2
#define C_COMPILED_CLOSURE_HEADER		3
#define C_COMPILED_MULTI_CLOSURE_HEADER		4
#define C_COMPILED_LINKAGE_HEADER		5
#define C_COMPILED_RAW_QUAD			6
#define C_COMPILED_EXECUTE_ENTRY		7
#define C_COMPILED_EXECUTE_ARITY		8

/* Global data */

#ifndef HEAP_IN_LOW_MEMORY
SCHEME_OBJECT * memory_base;
#endif

static long
  compiler_processor_type = COMPILER_PROCESSOR_TYPE,
  compiler_interface_version = 0;

static SCHEME_OBJECT
  compiler_utilities = SHARP_F;

/* Utilities */

static char
  *input_file_name = "-",
  *output_file_name = "-";

FILE *input_file, *output_file;

static Boolean
DEFUN (strequal, (s1, s2), register char * s1 AND register char * s2)
{
  for ( ; *s1 != '\0'; s1++, s2++)
    if (*s1 != *s2)
      return (false);
  return (*s2 == '\0');
}

static void
DEFUN (setup_io, (input_mode, output_mode),
       CONST char * input_mode AND CONST char * output_mode)
{
  if (strequal (input_file_name, "-"))
    input_file = stdin;
  else
  {
    input_file = (fopen (input_file_name, input_mode));
    if (input_file == ((FILE *) NULL))
    {
      fprintf (stderr, "%s: failed to open %s for input.\n",
	       program_name, input_file_name);
      exit (1);
    }
  }

  if (strequal (output_file_name, "-"))
    output_file = stdout;
  else
  {
    output_file = (fopen (output_file_name, output_mode));
    if (output_file == ((FILE *) NULL))
    {
      fprintf (stderr, "%s: failed to open %s for output.\n",
	       program_name, output_file_name);
      fclose (input_file);
      exit (1);
    }
  }
  return;
}

static void
DEFUN (quit, (code), int code)
{
  fclose(input_file);
  fclose(output_file);
#ifdef vms
  /* This assumes that it is only invoked with 0 in tail recursive psn. */
  if (code != 0)
    exit(code);
  else
    return;
#else /* not vms */
  exit(code);
#endif /*vms */
}

#ifndef TERM_COMPILER_DEATH
#define TERM_COMPILER_DEATH 0
#endif

void
DEFUN (gc_death, (code, message, scan, free),
       long code
       AND char * message
       AND SCHEME_OBJECT * scan
       AND SCHEME_OBJECT * free)
{
  fprintf (stderr, "%s: %s\n", program_name, message);
  quit (1);
}

/* Include the command line parser */

#include "comlin.c"

#define INPUT_KEYWORD()						\
KEYWORD("input", &input_file_name, STRING_KYWRD, SFRMT, NULL)

#define OUTPUT_KEYWORD()					\
KEYWORD("output", &output_file_name, STRING_KYWRD, SFRMT, NULL)

#endif /* PSBMAP_H_INCLUDED */
