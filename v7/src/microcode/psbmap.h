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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/psbmap.h,v 9.24 1987/11/20 08:13:32 jinx Exp $
 *
 * This file contains macros and declarations for Bintopsb.c
 * and Psbtobin.c
 *
 */

/* These definitions insure that the appropriate code is extracted
   from the included files.
*/

#include <stdio.h>
#define fast register

#include "config.h"
#include "object.h"
#include "bignum.h"
#include "bitstr.h"
#include "types.h"
#include "sdata.h"
#include "const.h"
#include "gccode.h"
#include "character.h"

#ifdef HAS_FREXP
extern double frexp(), ldexp();
#else
#include "missing.c"
#endif

#define PORTABLE_VERSION	4

/* Number of objects which, when traced recursively, point at all other
   objects dumped.  Currently only the dumped object.
 */

#define NROOTS			1

/* Types to recognize external object references.  Any occurrence of these 
   (which are external types and thus handled separately) means a reference
   to an external object.
 */

#define CONSTANT_CODE			TC_FIXNUM
#define HEAP_CODE			TC_CHARACTER

#define fixnum_to_bits			FIXNUM_LENGTH
#define bignum_to_bits(len)		((len) * SHIFT)
#define bits_to_bigdigit(nbits)		(((nbits) + (SHIFT-1)) / SHIFT)

#define hex_digits(nbits)		(((nbits) + 3) / 4)

/*
  This assumes that a bignum header is 2 Pointers.
  The bignum code is not very portable, unfortunately
 */

#define bignum_header_to_pointer	Align(0)

#define to_pointer(size)						\
  (((size) + (sizeof(Pointer) - 1)) / sizeof(Pointer))

#define bigdigit_to_pointer(ndig)					\
  to_pointer((ndig) * sizeof(bigdigit))

#define float_to_pointer						\
  to_pointer(sizeof(double))

#define flonum_to_pointer(nchars)					\
  ((nchars) * (1 + float_to_pointer))

#define char_to_pointer(nchars)						\
  to_pointer(nchars)

#define pointer_to_char(npoints)					\
  ((npoints) * sizeof(Pointer))

/* Status flags */

#define COMPACT_P	(1 << 0)
#define NULL_NMV_P	(1 << 1)
#define COMPILED_P	(1 << 2)
#define NMV_P		(1 << 3)

#define MAKE_FLAGS()							\
((compact_p ? COMPACT_P : 0)	|					\
 (null_nmv_p ? NULL_NMV_P : 0)	|					\
 (compiled_p ? COMPILED_P : 0)	|					\
 (nmv_p ? NMV_P : 0))

#define READ_FLAGS(f)							\
{									\
  compact_p = ((f) & COMPACT_P);					\
  null_nmv_p  = ((f) & NULL_NMV_P);					\
  compiled_p = ((f) & COMPILED_P);					\
  nmv_p = ((f) & NMV_P);						\
}

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

/* Global data */

#ifndef Heap_In_Low_Memory
static Pointer *Memory_Base;
#endif

static FILE *Input_File, *Output_File;

static char *Program_Name;

/* Argument List Parsing */

struct Option_Struct
{
  char *name;
  Boolean value;
  Boolean *ptr;
};

Boolean
strequal(s1, s2)
     fast char *s1, *s2;
{
  while (*s1 != '\0')
  {
    if (*s1++ != *s2++)
    {
      return false;
    }
  }
  return (*s2 == '\0');
}

char *
Find_Options(argc, argv, Noptions, Options)
     int argc;
     char **argv;
     int Noptions;
     struct Option_Struct Options[];
{
  for ( ; --argc >= 0; argv++)
  {
    char *this;
    int n;

    this = *argv;
    for (n = 0;
	 ((n < Noptions) && (!strequal(this, Options[n].name)));
	 n++)
    {};
    if (n >= Noptions)
    {
      return (this);
    }
    *(Options[n].ptr) = Options[n].value;
  }
  return (NULL);
}

/* Usage information */

void
Print_Options(n, options, where)
     int n;
     struct Option_Struct *options;
     FILE *where;
{
  if (--n < 0)
  {
    return;
  }
  fprintf(where, "[%s]", options->name);
  options += 1;
  for (; --n >= 0; options += 1)
  {
    fprintf(where, " [%s]", options->name);
  }
  return;
}

void
Print_Usage_and_Exit(noptions, options, io_options)
     int noptions;
     struct Option_Struct *options;
     char *io_options;
{
  fprintf(stderr, "usage: %s%s%s",
	  Program_Name,
	  (((io_options == NULL) ||
	    (io_options[0] == '\0')) ? "" : " "),
	  io_options);
  if (noptions != 0)
  {
    putc(' ', stderr);
    Print_Options(noptions, options, stderr);
  }
  putc('\n', stderr);
  exit(1);
}

/* Top level of program */

/* When debugging force arguments on command line */

#ifdef DEBUG
#undef unix
#endif

#ifdef unix

/* On unix use io redirection */

void
Setup_Program(argc, argv, Noptions, Options)
     int argc;
     char *argv[];
     int Noptions;
     struct Option_Struct *Options;
{
  Program_Name = argv[0];
  Input_File = stdin;
  Output_File = stdout;
  if (((argc - 1) > Noptions) ||
      (Find_Options((argc - 1), &argv[1], Noptions, Options) != NULL))
  {
    Print_Usage_and_Exit(Noptions, Options, "");
  }
  return;
}

#define quit exit

#else /* not unix */

/* Otherwise use command line arguments */

void
Setup_Program(argc, argv, Noptions, Options)
     int argc;
     char *argv[];
     int Noptions;
     struct Option_Struct *Options;
{
  Program_Name = argv[0];
  if ((argc < 3) ||
      ((argc - 3) > Noptions) ||
      (Find_Options((argc - 3), &argv[3], Noptions, Options) != NULL))
  {
    Print_Usage_and_Exit(Noptions, Options, "input_file output_file");
  }
  Input_File = ((strequal(argv[1], "-")) ?
		stdin :
		fopen(argv[1], "r"));
  if (Input_File == NULL)
  {
    perror("Open failed.");
    exit(1);
  }
  Output_File = ((strequal(argv[2], "-")) ?
		 stdout :
		 fopen(argv[2], "w"));
  if (Output_File == NULL)
  {
    perror("Open failed.");
    fclose(Input_File);
    exit(1);
  }
  fprintf(stderr, "%s: Reading from %s, writing to %s.\n",
          Program_Name, argv[1], argv[2]);
  return;
}

void
quit(code)
     int code;
{
  fclose(Input_File);
  fclose(Output_File);
  /* VMS brain dammage */
  if (code != 0)
  {
    exit(code);
  }
  return;
}

#endif /* unix */

