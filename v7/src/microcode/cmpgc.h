/* -*-C-*-

Copyright (c) 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpgc.h,v 1.2 1989/10/23 21:36:48 jinx Exp $
   $MC68020-Header: cmp68kgc.h,v 9.30 89/03/27 23:14:31 GMT jinx Exp $

Utilities to relocate compiled code in garbage collection-like processes. 

This file is conditionally included by gccode.h.

See cmpint.txt, cmpint.h, cmpint.c, and cmpaux.m4 for more details.
*/

#include "cmpint.h"

/* The following is a kludge which is used to get
   return_to_interpreter to work.  The return to interpreter block is
   never dumped on normal bin files, but is dumped in complete bands.
   As long as it does not change in position with respect to the
   beginning of constant space, it will be relocated correctly on
   reload.  */

#ifndef In_Fasdump

#define Compiled_Code_Pre_Test(then_what)

#else

extern SCHEME_OBJECT compiler_utilities;

#define Compiled_Code_Pre_Test(then_what)				\
if (Old == (OBJECT_ADDRESS(compiler_utilities)))			\
  then_what;								\
else

#endif

/*
   The following code handles compiled entry points, where the
   addresses point to the "middle" of the code vector.  From the entry
   address, the offset word can be extracted, and this offset allows
   us to find the beginning of the block, so it can be copied as a
   whole.  The broken heart for the whole block lives in its usual
   place (first word in the vector).

   The offset word contains an encoding of the offset and an encoding
   of whether the resulting pointer points to the beginning of the
   block or is another entry, so the process may have to be repeated.

   Pointers to char are used here because compiled entry points do not
   in general point to Pointer boundaries.
 */

#define Get_Compiled_Block(var, address)				\
{									\
  machine_word offset_word;						\
									\
  var = (address);							\
									\
  do									\
  {									\
    offset_word = (COMPILED_ENTRY_OFFSET_WORD(var));			\
    var = ((SCHEME_OBJECT *)						\
	   (((char *) (var)) -						\
	    (OFFSET_WORD_TO_BYTE_OFFSET(offset_word))));		\
  } while (OFFSET_WORD_CONTINUATION_P(offset_word));			\
}

#define RELOCATE_COMPILED_ADDRESS(object, new_block, old_block)		\
((SCHEME_OBJECT *) (((char *) new_block) +				\
		    (((char *) (OBJECT_ADDRESS(object))) -		\
		     ((char *) old_block))))

#define Relocate_Compiled(object, new_block, old_block)			\
MAKE_POINTER_OBJECT((OBJECT_TYPE(object)),				\
		    (RELOCATE_COMPILED_ADDRESS(object, new_block, old_block)))

#define Compiled_BH(In_GC, then_what)					\
{									\
  /* Has it already been relocated? */					\
									\
  Get_Compiled_Block(Old, Old);						\
  Compiled_Code_Pre_Test(then_what)					\
  if (OBJECT_TYPE(*Old) == TC_BROKEN_HEART)				\
  {									\
    *Scan = Relocate_Compiled(Temp, (OBJECT_ADDRESS(*Old)), Old);	\
    then_what;								\
  }									\
}

#define Transport_Compiled()						\
{									\
  SCHEME_OBJECT *Saved_Old = Old;					\
									\
  Real_Transport_Vector();						\
  *Saved_Old = New_Address;						\
  *Scan = Relocate_Compiled(Temp,					\
			    (OBJECT_ADDRESS (New_Address)),		\
			    Saved_Old);					\
}

/* Manifest and implied types */

/* Manifest closures */

/* Bump back to header. */

#define FIRST_MANIFEST_CLOSURE_ENTRY(scan)				\
  ((machine_word *) (((SCHEME_OBJECT *) scan) - 1))

#define VALID_MANIFEST_CLOSURE_ENTRY(word_ptr)				\
  ((OBJECT_TYPE(*((SCHEME_OBJECT *) word_ptr))) == TC_MANIFEST_CLOSURE)

/* *** THIS DOES NOT WORK *** */

/* The macro is more general and needs a destination.
   The garbage collector must be changed for that.
 */

#define MANIFEST_CLOSURE_ENTRY_ADDRESS(word_ptr)		\
  (COMPILED_CLOSURE_ENTRY_ADDRESS(word_ptr))

#define NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr)			\
  ((machine_word *)						\
   (((SCHEME_OBJECT *) word_ptr) +				\
    (COMPILED_CLOSURE_ENTRY_SIZE + 1)))

/* This takes into account the fact that the relocation loop increments
   by 1 on each major iteration.
 */

#define MANIFEST_CLOSURE_END(end_ptr, start_ptr)		\
  (((SCHEME_OBJECT *) end_ptr) - 1)

#define MANIFEST_CLOSURE_VALID_FITS_P(word_ptr, top)		\
  ((NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr)) <=			\
   ((machine_word *) top))

/* Linkage sections */

#define OPERATOR_LINKAGE_KIND			0x000000
#define REFERENCE_LINKAGE_KIND			0x010000
#define ASSIGNMENT_LINKAGE_KIND			0x020000

#define READ_LINKAGE_KIND(header)				\
  ((header) & 0xff0000)

#define READ_CACHE_LINKAGE_COUNT(header)			\
  ((header) & 0xffff)

#define READ_OPERATOR_LINKAGE_COUNT(header)			\
  (OPERATOR_LINK_COUNT_TO_ENTRIES((header) & 0xffff))
  
/* This takes into account the 1 added by the main loop of the
   relocators.
 */

#define END_OPERATOR_LINKAGE_AREA(scan, count)			\
  (((SCHEME_OBJECT *) (scan)) + ((count) * OPERATOR_LINK_ENTRY_SIZE))

#define FIRST_OPERATOR_LINKAGE_ENTRY(scan)			\
  ((machine_word *) (((SCHEME_OBJECT *) (scan)) + 1))

/* *** THIS DOES NOT WORK *** */

/* The macro is more general and needs a destination.
   The garbage collector must be changed for that.
 */

#define OPERATOR_LINKAGE_ENTRY_ADDRESS(word_ptr)		\
  (OPERATOR_LINK_ADDRESS(word_ptr))

#define NEXT_LINKAGE_OPERATOR_ENTRY(word_ptr)			\
  ((machine_word *) (((SCHEME_OBJECT *) (word_ptr)) +		\
		     OPERATOR_LINK_ENTRY_SIZE))

/* Heuristic recovery aid.  See unix.c for details. */

#define CC_BLOCK_FIRST_GC_OFFSET				\
  (CC_BLOCK_FIRST_ENTRY_OFFSET - (sizeof(machine_word)))

#define PLAUSIBLE_CC_BLOCK_P(block)					\
((*((machine_word *) (((char *) block) + CC_BLOCK_FIRST_GC_OFFSET))) ==	\
   ((BYTE_OFFSET_TO_OFFSET_WORD(CC_BLOCK_FIRST_ENTRY_OFFSET))))
