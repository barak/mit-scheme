/* -*-C-*-

$Id: liarc.h,v 1.1 1993/06/08 06:13:32 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

#ifndef LIARC_INCLUDED
#define LIARC_INCLUDED

#include <stdio.h>
#include "ansidecl.h"
#include "config.h"
#include "default.h"
#include "object.h"
#include "sdata.h"
#include "types.h"
#include "errors.h"
#include "const.h"
#include "interp.h"
#include "prim.h"
#include "cmpgc.h"
#include "cmpint2.h"

#ifdef __STDC__
#  define USE_STDARG
#  include <stdarg.h>
#else
#  include <varargs.h>
#endif /* __STDC__ */

/* #define USE_GLOBAL_VARIABLES */
#define USE_SHORTCKT_JUMP

typedef unsigned long ulong;

extern PTR dstack_position;
extern SCHEME_OBJECT * Free;
extern SCHEME_OBJECT * Ext_Stack_Pointer;
extern SCHEME_OBJECT Registers[];

extern void EXFUN (lose_big, (char *));
extern int EXFUN (multiply_with_overflow, (long, long, long *));
extern SCHEME_OBJECT * EXFUN (invoke_utility, (int, long, long, long, long));
extern void EXFUN (error_band_already_built, (void));

#define ERROR_UNKNOWN_DISPATCH( pc ) lose_big ("Unknown tag.")

#define ADDRESS_UNITS_PER_OBJECT	(sizeof (SCHEME_OBJECT))

#undef FIXNUM_TO_LONG
#define FIXNUM_TO_LONG(source)						\
  ((((long) (source)) << TYPE_CODE_LENGTH) >> TYPE_CODE_LENGTH)

#define ADDRESS_TO_LONG(source) ((long) (source))

#define LONG_TO_ADDRESS(source) (DATUM_TO_ADDRESS (source))

#define C_STRING_TO_SCHEME_STRING(len,str)				\
  (MEMORY_TO_STRING ((len), (unsigned char *) str))

#define C_SYM_INTERN(len,str)						\
  (MEMORY_TO_SYMBOL ((len), ((unsigned char *) str)))

#define MAKE_PRIMITIVE_PROCEDURE(name,arity)				\
  (SEARCH_FOR_PRIMITIVE (SHARP_F, name, true, true, arity))

#define MAKE_LINKER_HEADER(kind,count)					\
  (OBJECT_NEW_TYPE (TC_FIXNUM,						\
		    (MAKE_LINKAGE_SECTION_HEADER ((kind), (count)))))

#define ALLOCATE_VECTOR(len) (MAKE_VECTOR ((len), SHARP_F, true))

#define ALLOCATE_RECORD(len)						\
  (OBJECT_NEW_TYPE (TC_RECORD, (ALLOCATE_VECTOR (len))))

#define RECORD_SET(rec,off,val)	VECTOR_SET(rec,off,val)

#define INLINE_DOUBLE_TO_FLONUM(src,tgt) do				\
{									\
  double num = (src);							\
  SCHEME_OBJECT * val;							\
									\
  ALIGN_FLOAT (free_pointer);						\
  val = free_pointer;							\
  free_pointer += (1 + (BYTES_TO_WORDS (sizeof (double))));		\
  * val = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,				\
			(BYTES_TO_WORDS (sizeof (double)))));		\
  (* ((double *) (val + 1))) = num;					\
  (tgt) = (MAKE_POINTER_OBJECT (TC_BIG_FLONUM, (val)));			\
} while (0)

#define MAKE_RATIO(num,den)						\
  (OBJECT_NEW_TYPE (TC_RATNUM, (CONS (num, den))))

#define MAKE_COMPLEX(real,imag)						\
  (OBJECT_NEW_TYPE (TC_COMPLEX, (CONS (real, imag))))

#define CC_BLOCK_TO_ENTRY(block,offset)					\
  (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY,				\
			((OBJECT_ADDRESS (block)) + (offset))))

#ifdef USE_GLOBAL_VARIABLES

#define value_reg Val
#define free_pointer Free
#define register_block Regs
#define stack_pointer Stack_Pointer

#define DECLARE_VARIABLES() int unsed_variable_to_keep_C_happy
#define UNCACHE_VARIABLES() do {} while (0)
#define CACHE_VARIABLES() do {} while (0)

#else /* not USE_GLOBAL_VARIABLES */

#define REGISTER register

#define register_block Regs

#define DECLARE_VARIABLES()						\
REGISTER SCHEME_OBJECT value_reg = Val;					\
REGISTER SCHEME_OBJECT * free_pointer = Free;				\
REGISTER SCHEME_OBJECT * stack_pointer = Stack_Pointer

#define UNCACHE_VARIABLES() do						\
{									\
  Stack_Pointer = stack_pointer;					\
  Free = free_pointer;							\
  Val = value_reg;							\
} while (0)

#define CACHE_VARIABLES() do						\
{									\
  value_reg = Val;							\
  free_pointer = Free;							\
  stack_pointer = Stack_Pointer;					\
} while (0)

#endif /* USE_GLOBAL_VARIABLES */

#define REPEAT_DISPATCH() do						\
{									\
  if ((LABEL_PROCEDURE (my_pc)) != current_C_proc)			\
  {									\
    UNCACHE_VARIABLES ();						\
    return (my_pc);							\
  }									\
  /* fall through. */							\
} while (0)

#ifdef USE_SHORTCKT_JUMP

#define JUMP(destination) do						\
{									\
  my_pc = (destination);						\
  goto repeat_dispatch;							\
} while(0)

#define JUMP_EXTERNAL(destination) do					\
{									\
  my_pc = (destination);						\
  if ((LABEL_PROCEDURE (my_pc)) == current_C_proc)			\
  {									\
    CACHE_VARIABLES ();							\
    goto perform_dispatch;						\
  }									\
  return (my_pc);							\
} while (0)

#define JUMP_EXECUTE_CHACHE(entry) do					\
{									\
  my_pc = ((SCHEME_OBJECT *) current_block[entry]);			\
  goto repeat_dispatch;							\
} while (0)

#define POP_RETURN() goto pop_return_repeat_dispatch

#define POP_RETURN_REPEAT_DISPATCH() do					\
{									\
  my_pc = (OBJECT_ADDRESS (*stack_pointer++));				\
  /* fall through to repeat_dispatch */					\
} while (0)

#else /* not USE_SHORTCKT_JUMP */

#define JUMP(destination) do						\
{									\
  UNCACHE_VARIABLES ();							\
  return (destination);							\
} while (0)

#define JUMP_EXTERNAL(destination) return (destination)

#define JUMP_EXECUTE_CHACHE(entry) do					\
{									\
  SCHEME_OBJECT* destination						\
    = ((SCHEME_OBJECT *) current_block[entry]);				\
									\
  JUMP (destination);							\
} while (0)

#define POP_RETURN() do							\
{									\
    SCHEME_OBJECT target = *stack_pointer++;				\
    SCHEME_OBJECT destination = (OBJECT_ADDRESS (target));		\
    JUMP (destination);							\
} while (0)

#define POP_RETURN_REPEAT_DISPATCH() do					\
{									\
} while (0)

#endif /* USE_SHORTCKT_JUMP */

#define INVOKE_PRIMITIVE(prim, nargs) do				\
{									\
  primitive = (prim);							\
  primitive_nargs = (nargs);						\
  goto invoke_primitive;						\
} while (0)

#define INVOKE_PRIMITIVE_CODE() do					\
{									\
  SCHEME_OBJECT * destination;						\
									\
  UNCACHE_VARIABLES ();							\
  PRIMITIVE_APPLY (Val, primitive);					\
  POP_PRIMITIVE_FRAME (primitive_nargs);				\
  destination = (OBJECT_ADDRESS (STACK_POP ()));			\
  JUMP_EXTERNAL (destination);						\
} while(0)

#define INVOKE_INTERFACE_CODE() do					\
{									\
  SCHEME_OBJECT * destination;						\
									\
  UNCACHE_VARIABLES ();							\
  destination = (invoke_utility (subtmp_code, subtmp_1, subtmp_2,	\
				 subtmp_3, subtmp_4));			\
  JUMP_EXTERNAL (destination);						\
} while (0)

#define INVOKE_INTERFACE_4(code, one, two, three, four) do		\
{									\
  subtmp_4 = ((long) (four));						\
  subtmp_3 = ((long) (three));						\
  subtmp_2 = ((long) (two));						\
  subtmp_1 = ((long) (one));						\
  subtmp_code = (code);							\
  goto invoke_interface_4;						\
} while (0)

#define INVOKE_INTERFACE_3(code, one, two, three) do			\
{									\
  subtmp_3 = ((long) (three));						\
  subtmp_2 = ((long) (two));						\
  subtmp_1 = ((long) (one));						\
  subtmp_code = (code);							\
  goto invoke_interface_3;						\
} while (0)

#define INVOKE_INTERFACE_2(code, one, two) do				\
{									\
  subtmp_2 = ((long) (two));						\
  subtmp_1 = ((long) (one));						\
  subtmp_code = (code);							\
  goto invoke_interface_2;						\
} while (0)

#define INVOKE_INTERFACE_1(code, one) do				\
{									\
  subtmp_1 = ((long) (one));						\
  subtmp_code = (code);							\
  goto invoke_interface_1;						\
} while (0)

#define INVOKE_INTERFACE_0(code) do					\
{									\
  subtmp_code = (code);							\
  goto invoke_interface_0;						\
} while (0)

#define MAX_BIT_SHIFT DATUM_LENGTH

#define RIGHT_SHIFT_UNSIGNED(source, number)				\
(((number) > MAX_BIT_SHIFT)						\
 ? 0									\
 : ((((unsigned long) (source)) & DATUM_MASK)				\
    >> (number)))

#define RIGHT_SHIFT(source, number)					\
(((number) > MAX_BIT_SHIFT)						\
 ? 0									\
 : ((source) >> (number)))

#define LEFT_SHIFT(source, number)					\
(((number) > MAX_BIT_SHIFT)						\
 ? 0									\
 : ((source) << (number)))

#define FIXNUM_LSH(source, number)					\
(((number) >= 0)							\
 ? (LEFT_SHIFT (source, number))					\
 : (RIGHT_SHIFT_UNSIGNED (source, (- (number)))))

#define FIXNUM_REMAINDER(source1, source2)				\
(((source2) > 0)							\
 ? (((source1) >= 0)							\
    ? ((source1) % (source2))						\
    : (- ((- (source1)) % (source2))))					\
 : (((source1) >= 0)							\
    ? ((source1) % (- (source2)))					\
    : (- ((- (source1)) % (- (source2))))))

#define FIXNUM_QUOTIENT(source1, source2)				\
(((source2) > 0)							\
 ? (((source1) >= 0)							\
    ? ((source1) / (source2))						\
    : (- ((- (source1)) / (source2))))					\
 : (((source1) >= 0)							\
    ? (- ((source1) / (- (source2))))					\
    : ((- (source1)) / (- (source2)))))

#define CLOSURE_HEADER(offset) do					\
{									\
  SCHEME_OBJECT * entry = ((SCHEME_OBJECT *) my_pc[1]);			\
  current_block = (entry - offset);					\
  *--stack_pointer = (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY, my_pc));	\
} while (0)

#define CLOSURE_INTERRUPT_CHECK(code) do				\
{									\
  if (((long) free_pointer)						\
      >= ((long) (register_block[REGBLOCK_MEMTOP])))			\
    INVOKE_INTERFACE_0 (code);						\
} while (0)

#define INTERRUPT_CHECK(code, entry_point) do				\
{									\
  if (((long) free_pointer)						\
      >= ((long) (register_block[REGBLOCK_MEMTOP])))			\
    INVOKE_INTERFACE_1 (code, &current_block[entry_point]);		\
} while (0)

#define DLINK_INTERRUPT_CHECK(code, entry_point) do			\
{									\
  if (((long) free_pointer)						\
      >= ((long) (register_block[REGBLOCK_MEMTOP])))			\
    INVOKE_INTERFACE_2 (code, &current_block[entry_point],		\
			dynamic_link);					\
} while (0)

/* This does nothing in the sources. */

#define DECLARE_COMPILED_CODE(string, decl, code)			\
extern void EXFUN (decl, (void));					\
extern SCHEME_OBJECT * EXFUN (code, (SCHEME_OBJECT *));

#ifdef USE_STDARG
# define RCONSM_TYPE(frob) SCHEME_OBJECT EXFUN (frob, (int, SCHEME_OBJECT DOTS))
#else /* not USE_STDARG */
# define RCONSM_TYPE(frob) SCHEME_OBJECT frob ()
#endif /* USE_STDARG */

extern RCONSM_TYPE(rconsm);

struct compiled_file
{
  int number_of_procedures;
  char ** names;
  void * EXFUN ((**procs), (void));
};

extern int EXFUN (declare_compiled_code,
		  (char *,
		   void EXFUN ((*), (void)),
		   SCHEME_OBJECT * EXFUN ((*), (SCHEME_OBJECT *))));
extern SCHEME_OBJECT EXFUN (initialize_subblock, (char *));
extern void EXFUN (NO_SUBBLOCKS, (void));

#ifdef __GNUC__
# ifdef hp9000s800
#  define BUG_GCC_LONG_CALLS
# endif
#endif

#ifndef BUG_GCC_LONG_CALLS

extern SCHEME_OBJECT EXFUN (memory_to_string, (long, unsigned char *));
extern SCHEME_OBJECT EXFUN (memory_to_symbol, (long, unsigned char *));
extern SCHEME_OBJECT EXFUN (make_vector, (long, SCHEME_OBJECT, Boolean));
extern SCHEME_OBJECT EXFUN (cons, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (double_to_flonum, (double));
extern SCHEME_OBJECT EXFUN (long_to_integer, (long));
extern SCHEME_OBJECT EXFUN (digit_string_to_integer, (Boolean, long, char *));
extern SCHEME_OBJECT EXFUN (digit_string_to_bit_string, (long, long, char *));
extern SCHEME_OBJECT EXFUN (search_for_primitive,
			    (SCHEME_OBJECT, char *, Boolean, Boolean, int));

#define MEMORY_TO_STRING memory_to_string
#define MEMORY_TO_SYMBOL memory_to_symbol
#define MAKE_VECTOR make_vector
#define CONS cons
#define RCONSM rconsm
#define DOUBLE_TO_FLONUM double_to_flonum
#define LONG_TO_INTEGER long_to_integer
#define DIGIT_STRING_TO_INTEGER digit_string_to_integer
#define DIGIT_STRING_TO_BIT_STRING digit_string_to_bit_string
#define SEARCH_FOR_PRIMITIVE search_for_primitive

#else /* GCC on Specturm has a strange bug so do thing differently .... */

extern SCHEME_OBJECT EXFUN ((* (constructor_kludge [10])), ());

#define MEMORY_TO_STRING						\
     ((SCHEME_OBJECT EXFUN ((*), (long, unsigned char *))) (constructor_kludge[0]))

#define MEMORY_TO_SYMBOL						\
     ((SCHEME_OBJECT EXFUN ((*), (long, unsigned char *))) (constructor_kludge[1]))

#define MAKE_VECTOR							\
     ((SCHEME_OBJECT EXFUN ((*), (long, SCHEME_OBJECT, Boolean))) (constructor_kludge[2]))

#define CONS								\
     ((SCHEME_OBJECT EXFUN ((*), (SCHEME_OBJECT, SCHEME_OBJECT))) (constructor_kludge[3]))

#define RCONSM								\
     ((RCONSM_TYPE ((*))) (constructor_kludge[4]))

#define DOUBLE_TO_FLONUM						\
     ((SCHEME_OBJECT EXFUN ((*), (double))) (constructor_kludge[5]))

#define LONG_TO_INTEGER							\
     ((SCHEME_OBJECT EXFUN ((*), (long))) (constructor_kludge[6]))

#define DIGIT_STRING_TO_INTEGER						\
     ((SCHEME_OBJECT EXFUN ((*), (Boolean, long, char *))) (constructor_kludge[7]))

#define DIGIT_STRING_TO_BIT_STRING					\
     ((SCHEME_OBJECT EXFUN ((*), (long, long, char *))) (constructor_kludge[8]))

#define SEARCH_FOR_PRIMITIVE						\
     ((SCHEME_OBJECT EXFUN ((*), (SCHEME_OBJECT, char *,		\
				  Boolean, Boolean, int)))		\
      (constructor_kludge[9]))

#endif /* BUG_GCC_LONG_CALLS */

#endif /* LIARC_INCLUDED */
