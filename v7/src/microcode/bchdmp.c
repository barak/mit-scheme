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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchdmp.c,v 9.27 1987/04/16 02:06:33 jinx Exp $ */

/* bchgcl, bchmmg, bchpur, and bchdmp can replace gcloop, memmag,
   purify, and fasdump, respectively, to provide garbage collection
   and related utilities to disk.
*/

#include "scheme.h"
#include "primitive.h"
#define In_Fasdump
#include "bchgcc.h"
#include "dump.c"

/* (PRIMITIVE-FASDUMP object-to-dump file-name flag)
   Not implemented yet.
*/

Built_In_Primitive(Prim_Prim_Fasdump, 3, "PRIMITIVE-FASDUMP", 0x56)
{
  Primitive_3_Args();

  Primitive_Error(ERR_UNIMPLEMENTED_PRIMITIVE);
  /*NOTREACHED*/
}

/* (DUMP-BAND PROCEDURE FILE-NAME)
      Saves all of the heap and pure space on FILE-NAME.  When the
      file is loaded back using BAND_LOAD, PROCEDURE is called with an
      argument of NIL.
*/
Built_In_Primitive(Prim_Band_Dump, 2, "DUMP-BAND", 0xB7)
{
  Pointer Combination, Ext_Prims;
  long Arg1Type;
  Primitive_2_Args();

  Band_Dump_Permitted();
  Arg1Type = Type_Code(Arg1);
  if ((Arg1Type != TC_CONTROL_POINT) &&
      (Arg1Type != TC_PRIMITIVE) &&
      (Arg1Type != TC_PRIMITIVE_EXTERNAL) &&
      (Arg1Type != TC_EXTENDED_PROCEDURE)) Arg_1_Type(TC_PROCEDURE);
  Arg_2_Type(TC_CHARACTER_STRING);
  if (!Open_Dump_File(Arg2, WRITE_FLAG))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  /* Free cannot be saved around this code since Make_Prim_Exts will
     intern the undefined externals and potentially allocate space.
   */
  Ext_Prims = Make_Prim_Exts();
  Combination = Make_Pointer(TC_COMBINATION_1, Free);
  Free[COMB_1_FN] = Arg1;
  Free[COMB_1_ARG_1] = NIL;
  Free += 2;
  *Free++ = Combination;
  *Free++ = return_to_interpreter;
  *Free = Make_Pointer(TC_LIST, Free-2);
  Free++;  /* Some compilers are TOO clever about this and increment Free
	      before calculating Free-2! */
  *Free++ = Ext_Prims;
  /* Aligning here confuses some of the counts computed.
     Align_Float(Free);
   */
  Write_File(((long) (Free-Heap_Bottom)), Heap_Bottom, Free-2,
             ((long) (Free_Constant-Constant_Space)),
	     Constant_Space, Free-1);
  fclose(File_Handle);
  return TRUTH;
}
