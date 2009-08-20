/* -*-C-*-

$Id$

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

/* This file contains macros, declarations and some sahred code
   for "bintopsb.c" and "psbtobin.c". 
 */

#ifndef PSBMAP_H_INCLUDED
#define PSBMAP_H_INCLUDED

/* Objects in the portable file are tagged with a values from this set.
   There is no direct correspondence with the TC_ typecodes because we
   wish PSB files to be portable across many representation choices. Unless
   the TC_ code can be infered (as in +0ve and -ve fixnums), there is at least
   one TA_ code for every TC_ code that might appear in a PSB file
*/

/* interesting constants whose representation varies: */
#define  TA_FALSE                         0  /*  #F     */         
#define  TA_TRUE                          1  /*  #T     */         
#define  TA_NIL                           2  /*  '()    */         
#define  TA_UNSPECIFIC                    3

#define  TA_CONSTANT                      4  /*  other  TC_CONSTANT  */
#define  TA_CHARACTER                     5  /*  #\x    etc          */
#define  TA_TC_NULL                       6

#define  TA_FIXNUM                       10

#define  TA_BIGNUM                       11
#define  TA_FLONUM                       12
#define  TA_RATNUM                       13
#define  TA_RECNUM                       14

#define  TA_MANIFEST_NM_VECTOR           20
#define  TA_MANIFEST_SPECIAL_NM_VECTOR   21
#define  TA_PRIMITIVE                    22

#define  TA_COMPILED_ENTRY               30
#define  TA_MANIFEST_CLOSURE             31
#define  TA_REFERENCE_TRAP               32
#define  TA_COMPILED_CODE_BLOCK          33
#define  TA_LINKAGE_SECTION              34
#define  TA_CONTROL_POINT                35
#define  TA_STACK_ENVIRONMENT            36

#define  TA_CELL                         40
#define  TA_BROKEN_HEART                 41
#define  TA_PAIR                         42
#define  TA_WEAK_CONS                    43
#define  TA_UNINTERNED_SYMBOL            44
#define  TA_INTERNED_SYMBOL              45
#define  TA_HUNK3_A                      46
#define  TA_HUNK3_B                      47
#define  TA_QUAD                         48

#define  TA_NON_MARKED_VECTOR            70
#define  TA_VECTOR                       71
#define  TA_RECORD                       72
#define  TA_VECTOR_1B                    73
#define  TA_CHARACTER_STRING             74
#define  TA_VECTOR_16B                   75

#define  TA_CONSTANT_CODE                80
#define  TA_HEAP_CODE                    81
#define  TA_PURE_CODE                    82

#define  TA_PROCEDURE                   100
#define  TA_EXTENDED_PROCEDURE          101
#define  TA_LEXPR                       102
#define  TA_ENTITY                      103
#define  TA_ENVIRONMENT                 104
#define  TA_PROMISE                     105
#define  TA_FUTURE                      106
#define  TA_IN_PACKAGE                  107
#define  TA_COMMENT                     108
#define  TA_SCODE_QUOTE                 109
#define  TA_VARIABLE                    110
#define  TA_ACCESS                      111
#define  TA_LAMBDA                      112
#define  TA_EXTENDED_LAMBDA             113
#define  TA_SEQUENCE_2                  114
#define  TA_SEQUENCE_3                  115
#define  TA_CONDITIONAL                 116
#define  TA_DISJUNCTION                 117
#define  TA_COMBINATION                 118
#define  TA_COMBINATION_1               119
#define  TA_COMBINATION_2               120
#define  TA_PCOMB0                      121
#define  TA_PCOMB1                      122
#define  TA_PCOMB2                      123
#define  TA_PCOMB3                      124
#define  TA_DEFINITION                  125
#define  TA_DELAY                       126
#define  TA_ASSIGNMENT                  127
#define  TA_THE_ENVIRONMENT             128
#define  TA_RETURN_CODE                 129

#define  TA_C_COMPILED_TAG              200

/* These definitions insure that the appropriate code is extracted
   from the included files.
*/

#define WINNT_RAW_ADDRESSES
#define fast register

#include <stdio.h>
#ifndef _NEXTOS
#include <stdlib.h>
#endif
#include "ansidecl.h"
#include "config.h"
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

/* compatibilty with previous version of microcode */
#ifndef TC_CONSTANT
#define TC_CONSTANT TC_TRUE
#endif

#ifndef EMPTY_LIST_VALUE
#define EMPTY_LIST_VALUE EMPTY_LIST
#endif

extern double
  EXFUN (frexp, (double, int *)),
  EXFUN (ldexp, (double, int));

#define PORTABLE_VERSION	7

/* Number of objects which, when traced recursively, point at all other
   objects dumped.
   Currently the dumped object, and the compiler utilities.
 */

#define NROOTS			2

/* Types to recognize external object references.  Any occurrence of these
   (which are external types and thus handled separately) means a
   reference to an external object.  These values are required to be
   TC_xxx values so that they can fit in a normal object typecode in
   bintopsb until they are translated to TA_xxx values on output.
 */

#define CONSTANT_CODE			TC_POSITIVE_FIXNUM
#define HEAP_CODE			TC_CHARACTER
#define PURE_CODE			TC_BIG_FIXNUM

/* 
   The special constants #F () #T and UNSPECIFIC might appear in the
   vector length position of a vector or record.  If this happens we
   want to translate the value for its datum field rather than
   maintain that it represents #T or #F etc.  In the original (7.3)
   tagging scheme #F was the value 0x0, and so was the the vector
   length of #().

   We detect these unusual vector lengths and translate them to
   ALIASED_LENGTH_xxx values when the vector/record is copied.  We
   choose MANIFEST_NM_VECTOR with very high datum fields as these can
   never appear in a fasdump file if the datum field indicates a
   length greater than the total heap size.
*/

#define ALIASED_LENGTH_SHARP_F \
          (MAKE_OBJECT(TC_MANIFEST_NM_VECTOR, (DATUM_MASK & (-1))))

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
