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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dmpwrld.c,v 9.23 1987/04/11 16:05:19 jinx Exp $
 *
 * This file contains a primitive to dump an executable version of Scheme.
 * It uses unexec.c from GNU Emacs.
 * Look at unexec.c for more information.
 */

#include "scheme.h"
#include "primitive.h"

#ifndef unix
#include "Error: dumpworld.c does not work on non-unix machines."
#endif

/* Compatibility definitions for GNU Emacs's unexec.c.
   Taken from the various m-*.h and s-*.h files for GNU Emacs.
*/

#ifdef vax
#define UNEXEC_AVAILABLE
#endif

#ifdef hp9000s200
#define UNEXEC_AVAILABLE
#define ADJUST_EXEC_HEADER   						\
  hdr.a_magic = ((ohdr.a_magic.file_type == OLDMAGIC.file_type) ?	\
		 NEWMAGIC : ohdr.a_magic);

#endif

#ifdef sun3
#define UNEXEC_AVAILABLE
#define SEGMENT_MASK		(SEGSIZ - 1)
#define A_TEXT_OFFSET(HDR)	sizeof (HDR)
#define TEXT_START		(PAGSIZ + (sizeof(struct exec)))
#endif

/* I don't know whether the following two are right or not. */

#ifdef sun2
#define UNEXEC_AVAILABLE
#define SEGMENT_MASK		(SEGSIZ - 1)
#endif

#ifdef celerity
#define UNEXEC_AVAILABLE
#endif

#ifndef UNEXEC_AVAILABLE
#include "Error: dumpworld.c only works on a few machines."
#endif

#ifndef TEXT_START
#define TEXT_START	0
#endif

#ifndef SEGMENT_MASK
#define DATA_START	(&etext)
#else
#define DATA_START	\
(((((unsigned) &etext) - 1) & ~SEGMENT_MASK) + (SEGMENT_MASK + 1))
#endif

#ifdef hpux
#define USG
#define HPUX
#endif

/* More compatibility definitions for unexec. */

extern int end, etext, edata;
char *start_of_text(), *start_of_data();
void bzero();

#include "unexec.c"

char 
*start_of_text()
{ 
  return ((char *) TEXT_START);
}

char 
*start_of_data()
{ 
  return ((char *) DATA_START);
}

void
bzero (b, length)
     register char *b;
     register int length;
{
  while (length-- > 0)
    *b++ = 0;
}

/* Making sure that IO will be alright when restored. */

Boolean
there_are_open_files()
{
  register int i;

  i = FILE_CHANNELS;
  while (i > 0)
    if (Channels[--i] != NULL) return true;
  return false;
}

/* These two procedures depend on the internal structure of a 
   FILE object.  See /usr/include/stdio.h for details. */

long 
Save_Input_Buffer()
{ 
  long result;

  result = (stdin)->_cnt;
  (stdin)->_cnt = 0;
  return result;
}

void 
Restore_Input_Buffer(Buflen)
     fast long Buflen;
{
  (stdin)->_cnt = Buflen;
  return;
}

/* The primitive visible from Scheme. */

extern Boolean Was_Scheme_Dumped;
extern unix_find_pathname();

Define_Primitive(Prim_Dump_World, 1, "DUMP-WORLD")
{
  char *fname, path_buffer[FILE_NAME_LENGTH];
  Boolean Saved_Dumped_Value, Saved_Photo_Open;
  int Result;
  long Buflen;
  Primitive_1_Arg();

  Arg_1_Type(TC_CHARACTER_STRING);

  if (there_are_open_files())
     Primitive_Error(ERR_OUT_OF_FILE_HANDLES);

  fname = Scheme_String_To_C_String(Arg1);

  /* Set up for restore */

  Saved_Dumped_Value = Was_Scheme_Dumped;
  Saved_Photo_Open = Photo_Open;

  /* IO: flushing pending output, and flushing cached input. */

  fflush(stdout);
  fflush(stderr);

  if (Photo_Open)
  {
    fflush(Photo_File_Handle);
    Photo_Open = false;
  }

  Buflen = Save_Input_Buffer();

  Was_Scheme_Dumped = true;
  Val = TRUTH;
  OS_Quit();
  Pop_Primitive_Frame(1);

  /* Dump! */
  
  unix_find_pathname(Saved_argv[0], path_buffer);
  Result = unexec(fname,
		  path_buffer,
		  ((unsigned) 0),			/* default */
		  ((unsigned) 0),			/* default */
		  ((unsigned) start_of_text())
		  );

  /* Restore State */

  OS_Re_Init();
  Val = NIL;
  Was_Scheme_Dumped = Saved_Dumped_Value;

  /* IO: Restoring cached input for this job. */

  Restore_Input_Buffer(Buflen);
  Photo_Open = Saved_Photo_Open;

  if (Result != 0)
  {
    Push(Arg1);		/* Since popped above */
    Primitive_Error(ERR_EXTERNAL_RETURN);
  }

  longjmp(*Back_To_Eval, PRIM_POP_RETURN);
  /*NOTREACHED*/
}

