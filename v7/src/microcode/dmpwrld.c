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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dmpwrld.c,v 9.21 1987/01/22 14:23:30 jinx Exp $
 *
 * This file contains a primitive to dump an executable version of Scheme.
 */

#include "scheme.h"
#include "primitive.h"

#ifndef unix
#include "Error: dumpworld.c does not work on non-unix machines."
#endif

/* Suns and others probably work also, but we have no machines
   where to try them out.
*/

#if (!defined(vax) && !defined(hp9000s200) && !defined(celerity))
#include "Error: dumpworld.c only supported for vax and hp9000s200."
#endif

/* Making sure that IO will be alright when restored. */

Boolean there_are_open_files()
{ register int i = FILE_CHANNELS;
  while (i > 0)
    if (Channels[--i] != NULL) return true;
  return false;
}

/* These two procedures depend on the internal structure of a 
   FILE object.  See /usr/include/stdio.h for details. */

long Save_Input_Buffer()
{ long result = (stdin)->_cnt;
  (stdin)->_cnt = 0;
  return result;
}

void Restore_Input_Buffer(Buflen)
fast long Buflen;
{ (stdin)->_cnt = Buflen;
  return;
}

extern int end, etext, edata;
extern int unexec();
static jmp_buf for_error;

/* The primitive itself.  Uses unexec from GNU-EMACS */

Define_Primitive(Prim_Dump_World, 1, "DUMP-WORLD")
{ char *fname;
  extern Boolean Was_Scheme_Dumped;
  Boolean Saved_Dumped_Value = Was_Scheme_Dumped;
  Boolean Saved_Photo_Open = Photo_Open;
  int Result;
  long Buflen;

  Primitive_1_Arg();
  Arg_1_Type(TC_CHARACTER_STRING);

  if (there_are_open_files())
     Primitive_Error(ERR_OUT_OF_FILE_HANDLES);

  fname = Scheme_String_To_C_String(Arg1);

  /* Set up for restore */

  /* IO: flushing pending output, and flushing cached input. */
  fflush(stdout);
  fflush(stderr);
  if (Photo_Open)
  { fflush(Photo_File_Handle);
    Photo_Open = false;
  }
  Buflen = Save_Input_Buffer();

  Was_Scheme_Dumped = true;
  Val = TRUTH;
  OS_Quit();
  Pop_Primitive_Frame(1);

  /* Dump! */
  
  Result = setjmp(for_error);
  if (Result == 0)
    Result = unexec(fname,
		    Saved_argv[0],
		    ((unsigned) (&etext)),
		    ((unsigned) 0),
		    ((unsigned) 0)
		    );

  /* Restore State */

  OS_Re_Init();
  Val = NIL;
  Was_Scheme_Dumped = Saved_Dumped_Value;
  /* IO: Restoring cached input for this job. */
  Restore_Input_Buffer(Buflen);
  Photo_Open = Saved_Photo_Open;

  if (Result != 0)
  { Push(Arg1);		/* Since popped above */
    Primitive_Error(ERR_FASL_FILE_TOO_BIG);
  }
  longjmp(*Back_To_Eval, PRIM_POP_RETURN);
}

/* These things are needed by unexec */

#ifdef hpux
#define USG
#define HPUX
#endif

char *start_of_text()
{ 
#if false
  return ((char *) _start);
#else
  return ((char *) 0);
#endif
}

char *start_of_data()
{ return ((char *) (&etext));
}

#define has_error

void error(msg, a1, a2)
char *msg;
int a1, a2;
{ putc('\n', stderr);
  fprintf(stderr, msg, a1, a2);
  putc('\n', stderr);
  longjmp(for_error, -1);
}

#include "unexec.c"
