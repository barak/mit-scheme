/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpgc.h,v 1.16 1991/05/05 00:40:42 jinx Exp $

Copyright (c) 1989-1991 Massachusetts Institute of Technology

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

/*

Utilities to relocate compiled code in garbage collection-like processes. 

This file is included by gccode.h.

See cmpint.txt, cmpint.c, cmpint-md.h, and cmpaux-md.m4 for more details.
*/

#ifndef CMPGC_H_INCLUDED
#define CMPGC_H_INCLUDED

#define NOP() do {} while (0) /* A useful macro */

#ifdef HAS_COMPILER_SUPPORT

#include "cmpint2.h"

/*
  The following is a kludge which is used to get return_to_interpreter
  to work.  The return to interpreter block is never dumped on normal
  bin files, but is dumped in complete bands.  As long as it does not
  change in position with respect to the beginning of constant space,
  it will be relocated correctly on reload.
*/

#ifndef In_Fasdump

#define COMPILED_CODE_PRE_TEST(then_what)

#else

extern SCHEME_OBJECT compiler_utilities;

#define COMPILED_CODE_PRE_TEST(then_what)				\
if (Old == (OBJECT_ADDRESS (compiler_utilities)))			\
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
  long offset_word;							\
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

#define RELOCATE_COMPILED(object, new_block, old_block)			\
MAKE_POINTER_OBJECT((OBJECT_TYPE(object)),				\
		    (RELOCATE_COMPILED_ADDRESS(object, new_block, old_block)))

#define Compiled_BH(In_GC, then_what)					\
{									\
  /* Has it already been relocated? */					\
									\
  Get_Compiled_Block (Old, Old);					\
  COMPILED_CODE_PRE_TEST (then_what)					\
  if (BROKEN_HEART_P (*Old))						\
  {									\
    Temp = (RELOCATE_COMPILED (Temp, (OBJECT_ADDRESS (*Old)), Old));	\
    then_what;								\
  }									\
}

#define Transport_Compiled()						\
{									\
  SCHEME_OBJECT *Saved_Old = Old;					\
									\
  Real_Transport_Vector();						\
  *Saved_Old = New_Address;						\
  Temp = (RELOCATE_COMPILED (Temp,					\
			     (OBJECT_ADDRESS (New_Address)),		\
			     Saved_Old));				\
}

/* Manifest and implied types */

/* Manifest closures */

/* A manifest closure header is followed by one or more closure entry
   points.  Each entry point consist of a pair of machine words (16
   bits each) that contain a format word and a GC offset followed by
   the machine code for the closure (typically a jsr-type
   instruction).  If there is only one entry point to a closure, the
   GC offset will be 8 bytes, pointing back to the manifest closure
   header itself.  Otherwise what would have been the first GC offset
   is 0, and what would have been the first format word is the count
   in entry points.  The format word and GC offset for the first entry
   follow this additional word.  After the entry points there are the
   values of the variables closed over:

          >=1 Entry Point		  =1 Entry Point
   (offset in bytes from 1st instruction of 1st (only) entry)

     -12: Manifest Closure | tot. length
     - 8: Count format word (with 0 GC)  Manifest Closure | tot. length
     - 4: Format word, 1st entry         Format word, only entry
     - 2: GC offset to -12               GC offset to -8
       0: jsr instr., 1st entry		 jsr instr.
      xx: more instructions if needed    same
        : Format word, 2nd entry         closed over variable values
        : GC offset to -16
     ...: etc.
     ...: closed over variable values

   FIRST_MANIFEST_CLOSURE_ENTRY receives the address of the word past
   the manifest closure header (-4 for single entry point closures in
   the above picture).  It bumps it to the first entry point (i.e. to
   0 above), past the format word and the gc offset and the count
   formart word if present.

   MANIFEST_CLOSURE_COUNT receives the address of the word past the
   manifest closure header and extracts the count of entry points
   in the closure block.

   CLOSURE_HEADER_TO_ENTRY is the distance (in bytes) from the
   manifest closure header to the 1st instruction of the (1st) entry.
 */

#define CLOSURE_HEADER_TO_ENTRY						\
((sizeof (SCHEME_OBJECT)) + (2 * (sizeof (format_word))))

#define CLOSURE_HEADER_TO_ENTRY_WORD					\
((format_word) (BYTE_OFFSET_TO_OFFSET_WORD (CLOSURE_HEADER_TO_ENTRY)))

#define MANIFEST_CLOSURE_COUNT(scan)					\
(((((format_word *) (scan))[1]) ==					\
  CLOSURE_HEADER_TO_ENTRY_WORD) ?					\
 1 :									\
 ((long) (((format_word *) (scan))[0])))

#define FIRST_MANIFEST_CLOSURE_ENTRY(scan)				\
(((((format_word *) (scan))[1]) ==   CLOSURE_HEADER_TO_ENTRY_WORD) ?	\
 (((char *) (scan)) + (2 * (sizeof (format_word)))) :			\
 (((char *) (scan)) + (4 * (sizeof (format_word)))))

#define NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr)				\
  (((char *) (word_ptr)) + (COMPILED_CLOSURE_ENTRY_SIZE))

/* Where this closure entry ends with respect to the entry point.
   Since an entry point is preceded by a format word and a gc offset,
   it is the address of the next entry minus these two words.
 */

#define CLOSURE_ENTRY_END(word_ptr)					\
  (((char *) (word_ptr)) +						\
   ((COMPILED_CLOSURE_ENTRY_SIZE) - (2 * (sizeof (format_word)))))

#define CHAR_TO_SCHEME_OBJECT(chars)					\
(((chars) + ((sizeof (SCHEME_OBJECT)) - 1)) / (sizeof (SCHEME_OBJECT)))

/* This takes into account the fact that the relocation loop increments
   by 1 on each major iteration.
   Note: It also assumes that closures with exactly one entry point
   are always represented in short format.
 */

#define MANIFEST_CLOSURE_END(start, count)				\
(((SCHEME_OBJECT *) (start)) +						\
 ((CHAR_TO_SCHEME_OBJECT (((count) * COMPILED_CLOSURE_ENTRY_SIZE) +	\
			  (((count) == 1) ?				\
			   0 :						\
			   (2 * sizeof(format_word)))))			\
  - 1))

/* Linkage sections */

#define OPERATOR_LINKAGE_KIND			0x000000
#define REFERENCE_LINKAGE_KIND			0x010000
#define ASSIGNMENT_LINKAGE_KIND			0x020000
#define GLOBAL_OPERATOR_LINKAGE_KIND		0x030000

#define READ_LINKAGE_KIND(header)					\
  ((header) & 0xff0000)

#define READ_CACHE_LINKAGE_COUNT(header)				\
  ((header) & 0xffff)

#define READ_OPERATOR_LINKAGE_COUNT(header)				\
  (EXECUTE_CACHE_COUNT_TO_ENTRIES((header) & 0xffff))
  
#define MAKE_LINKAGE_SECTION_HEADER(kind, count)			\
  (MAKE_OBJECT(TC_LINKAGE_SECTION,					\
	       ((kind) |						\
		((((kind) == OPERATOR_LINKAGE_KIND)			\
		  || ((kind) == GLOBAL_OPERATOR_LINKAGE_KIND)) ?	\
		 (EXECUTE_CACHE_ENTRIES_TO_COUNT (count)) :		\
		 (count)))))

/* This takes into account the 1 added by the main loop of the
   relocators.
 */

#define END_OPERATOR_LINKAGE_AREA(scan, count)				\
  (((SCHEME_OBJECT *) (scan)) + ((count) * EXECUTE_CACHE_ENTRY_SIZE))

#define FIRST_OPERATOR_LINKAGE_ENTRY(scan)				\
  ((char *) (((SCHEME_OBJECT *) (scan)) + 1))

#define NEXT_LINKAGE_OPERATOR_ENTRY(word_ptr)				\
  ((char *) (((SCHEME_OBJECT *) (word_ptr)) +				\
	     EXECUTE_CACHE_ENTRY_SIZE))

#define EXTRACT_OPERATOR_LINKAGE_ADDRESS(target, source)		\
{									\
  EXTRACT_EXECUTE_CACHE_ADDRESS (target, source);			\
}

#define STORE_OPERATOR_LINKAGE_ADDRESS(source, target)			\
{									\
  STORE_EXECUTE_CACHE_ADDRESS (target, source);				\
}

/* Heuristic recovery aid. See uxtrap.c for its use.
   block is the address of a vector header followed by a non-marked
   header (the way that compiled blocks start).
   PLAUSIBLE_CC_BLOCK_P returns true if it is likely that compiled
   code is contained in the header.  This is done by checking whether
   an entry is the first thing in the compiled code section.
   There are two kinds of "possible entries": expressions (first thing
   in the block) and procedures/continuations, which follow an interrupt
   check.
 */

#define PLAUSIBLE_CC_BLOCK_P(block)					\
((PLAUSIBLE_BLOCK_START_P((block), CC_BLOCK_FIRST_ENTRY_OFFSET)) ||	\
 (PLAUSIBLE_BLOCK_START_P((block),					\
			  (CC_BLOCK_FIRST_ENTRY_OFFSET +		\
			   ENTRY_PREFIX_LENGTH))))

#define PLAUSIBLE_BLOCK_START_P(addr, offset)				\
((*((format_word *)							\
    (((char *) (addr)) + ((offset) - (sizeof (format_word)))))) ==	\
   ((BYTE_OFFSET_TO_OFFSET_WORD(offset))))

#else /* not HAS_COMPILER_SUPPORT */

/* This can be anything. */

typedef unsigned short format_word;

/* Is there anything else that can be done here? */

#define GC_NO_COMPILER_STMT()						\
  gc_death								\
    (TERM_COMPILER_DEATH,						\
     "relocate_compiled: No compiler support!",				\
     0, 0)

#define GC_NO_COMPILER_EXPR(value_type)					\
  ((GC_NO_COMPILER_STMT ()), (value_type 0))

#define RELOCATE_COMPILED(obj, nb, ob) (GC_NO_COMPILER_EXPR ((SCHEME_OBJECT)))

#define Transport_Compiled() (GC_NO_COMPILER_STMT ())
#define Compiled_BH(flag, then_what) (GC_NO_COMPILER_STMT ())
#define Get_Compiled_Block(var, address) (GC_NO_COMPILER_STMT ())

#define FIRST_MANIFEST_CLOSURE_ENTRY(scan)				\
  (GC_NO_COMPILER_EXPR ((char *)))

#define MANIFEST_CLOSURE_COUNT(scan)					\
  (GC_NO_COMPILER_EXPR ((long)))

#define NEXT_MANIFEST_CLOSURE_ENTRY(word_ptr)				\
  (GC_NO_COMPILER_EXPR ((char *)))

#define CLOSURE_ENTRY_END(word_ptr)					\
  (GC_NO_COMPILER_EXPR ((char *)))

#define MANIFEST_CLOSURE_END(end, start)				\
  (GC_NO_COMPILER_EXPR ((SCHEME_OBJECT *)))

#define EXTRACT_CLOSURE_ENTRY_ADDRESS(target, source)			\
  (GC_NO_COMPILER_STMT ())

#define STORE_CLOSURE_ENTRY_ADDRESS(source, target)			\
  (GC_NO_COMPILER_STMT ())

#define READ_LINKAGE_KIND(header)					\
  (GC_NO_COMPILER_EXPR ((int)))

#define OPERATOR_LINKAGE_KIND 0

#define READ_CACHE_LINKAGE_COUNT(header)				\
  (GC_NO_COMPILER_EXPR ((int)))

#define READ_OPERATOR_LINKAGE_COUNT(header)				\
  (GC_NO_COMPILER_EXPR ((int)))

#define END_OPERATOR_LINKAGE_AREA(scan, count)				\
  (GC_NO_COMPILER_EXPR ((SCHEME_OBJECT *)))

#define FIRST_OPERATOR_LINKAGE_ENTRY(scan)				\
  (GC_NO_COMPILER_EXPR ((char *)))

#define NEXT_LINKAGE_OPERATOR_ENTRY(ptr)				\
  (GC_NO_COMPILER_EXPR ((char *)))

#define EXTRACT_OPERATOR_LINKAGE_ADDRESS(target, source)		\
  (GC_NO_COMPILER_STMT ())

#define STORE_OPERATOR_LINKAGE_ADDRESS(source, target)			\
  (GC_NO_COMPILER_STMT ())

#endif /* HAS_COMPILER_SUPPORT */

#ifndef FLUSH_I_CACHE
#define FLUSH_I_CACHE() do {} while (0)
#endif /* FLUSH_I_CACHE */

#ifndef COMPILER_TRANSPORT_END
#define COMPILER_TRANSPORT_END() do					\
{									\
  Registers[REGBLOCK_CLOSURE_SPACE] = ((SCHEME_OBJECT) 0);		\
  Registers[REGBLOCK_CLOSURE_FREE] = ((SCHEME_OBJECT) NULL);		\
  FLUSH_I_CACHE ();							\
} while (0)
#endif /* COMPILER_TRANSPORT_END */

#endif /* CMPGC_H_INCLUDED */
