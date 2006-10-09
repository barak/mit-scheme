/* -*-C-*-

$Id: liarc.h,v 1.23 2006/10/09 06:50:58 cph Exp $

Copyright (c) 1992-2002, 2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#ifndef LIARC_INCLUDED
#define LIARC_INCLUDED

#ifndef COMPILE_FOR_STATIC_LINKING
#ifndef COMPILE_FOR_DYNAMIC_LOADING
#define COMPILE_FOR_DYNAMIC_LOADING
#endif
#endif

#ifndef MIT_SCHEME
#define MIT_SCHEME
#endif

#include <stdio.h>
#include "config.h"
#include "dstack.h"
#include "default.h"
#include "object.h"
#include "sdata.h"
#include "types.h"
#include "errors.h"
#include "const.h"
#include "interp.h"
#include "prim.h"
#include "cmpgc.h"
#include "cmpintmd.h"
#include "trap.h"
#include "outf.h"
#include "extern.h"

#ifdef __STDC__
#  define USE_STDARG
#  include <stdarg.h>
#else
#  include <varargs.h>
#endif /* __STDC__ */

#ifdef __GNUC__
/* Add attributes to avoid warnings from -Wall for unreferenced labels */
#  define DEFLABEL(name) name : __attribute__((unused))
#else /* not __GNUC__ */
#  define DEFLABEL(name) name :
#endif /* __GNUC__ */

/* #define USE_GLOBAL_VARIABLES */

#ifdef LIARC_IN_MICROCODE
#define USE_GLOBAL_VARIABLES
#endif

#define USE_SHORTCKT_JUMP

extern PTR dstack_position;
extern SCHEME_OBJECT * Free;
extern SCHEME_OBJECT * sp_register;
extern SCHEME_OBJECT Registers [];

union machine_word_u
{
  SCHEME_OBJECT Obj;
  SCHEME_OBJECT * pObj;
  long Lng;
  char * pChr;
  unsigned long uLng;
  double * pDbl;
};

typedef union machine_word_u machine_word;

typedef unsigned long entry_count_t;

#define ADDRESS_UNITS_PER_OBJECT	(sizeof (SCHEME_OBJECT))
#define ADDRESS_UNITS_PER_FLOAT		(sizeof (double))

#ifdef HEAP_IN_LOW_MEMORY
#define CLOSURE_ENTRY_DELTA	ADDRESS_UNITS_PER_OBJECT
#else /* not HEAP_IN_LOW_MEMORY */
#define CLOSURE_ENTRY_DELTA	1
#endif /* HEAP_IN_LOW_MEMORY */

#undef FIXNUM_TO_LONG
#define FIXNUM_TO_LONG(source)						\
  ((((long) (source)) << TYPE_CODE_LENGTH) >> TYPE_CODE_LENGTH)

#define ADDRESS_TO_LONG(source) ((long) (source))

#define LONG_TO_ADDRESS(source) (DATUM_TO_ADDRESS (source))

#define C_STRING_TO_SCHEME_STRING(len,str)				\
  (MEMORY_TO_STRING ((len), (unsigned char *) str))

#define C_SYM_INTERN(len,str)						\
  (MEMORY_TO_SYMBOL ((len), ((CONST char *) str)))

#define MAKE_PRIMITIVE_PROCEDURE(name,arity) (MAKE_PRIMITIVE (name, arity))

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
  ALIGN_FLOAT (Rhp);							\
  val = Rhp;								\
  Rhp += (1 + (BYTES_TO_WORDS (sizeof (double))));			\
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

#define INDEX_FIXNUM_P(arg) ((FIXNUM_P(arg)) && (FIXNUM_TO_LONG(arg)>=0))

#ifdef USE_GLOBAL_VARIABLES

#define Rvl val_register
#define Rhp Free
#define Rrb Registers
#define Rsp sp_register

#define DECLARE_VARIABLES() int unused_variable_to_keep_C_happy
#define UNCACHE_VARIABLES() do {} while (0)
#define CACHE_VARIABLES() do {} while (0)

#else /* not USE_GLOBAL_VARIABLES */

#define REGISTER register

#define Rrb Registers

#ifdef HEAP_IN_LOW_MEMORY

#define DECLARE_VARIABLES()						\
REGISTER SCHEME_OBJECT Rvl = val_register;				\
REGISTER SCHEME_OBJECT * Rhp = Free;					\
REGISTER SCHEME_OBJECT * Rsp = sp_register

#define DECLARE_VARIABLES_FOR_DATA()

#else

#undef MEMBASE
#define MEMBASE lcl_membase

#define DECLARE_VARIABLES()						\
REGISTER SCHEME_OBJECT Rvl = val_register;				\
REGISTER SCHEME_OBJECT * Rhp = Free;					\
REGISTER SCHEME_OBJECT * Rsp = sp_register;				\
REGISTER SCHEME_OBJECT * lcl_membase = memory_base

#define DECLARE_VARIABLES_FOR_DATA()					\
REGISTER SCHEME_OBJECT * lcl_membase = memory_base

#endif

#define DECLARE_VARIABLES_FOR_OBJECT()

/* lcl_membase is not cached/uncached because it is a constant */

#define UNCACHE_VARIABLES() do						\
{									\
  sp_register = Rsp;							\
  Free = Rhp;								\
  val_register = Rvl;							\
} while (0)

#define CACHE_VARIABLES() do						\
{									\
  Rvl = val_register;							\
  Rhp = Free;								\
  Rsp = sp_register;							\
} while (0)

#endif /* USE_GLOBAL_VARIABLES */

#define JUMP(destination) do						\
{									\
  Rpc = (destination);							\
  goto perform_dispatch;						\
} while(0)

#define JUMP_EXECUTE_CHACHE(label)					\
  JUMP ((SCHEME_OBJECT *) (current_block[label]))

#define POP_RETURN() goto pop_return

#define INVOKE_PRIMITIVE_DECLS						\
  SCHEME_OBJECT primitive;						\
  long primitive_nargs;

#define INVOKE_PRIMITIVE(prim, nargs) do				\
{									\
  primitive = (prim);							\
  primitive_nargs = (nargs);						\
  goto invoke_primitive;						\
} while (0)

#define INVOKE_PRIMITIVE_TARGET						\
DEFLABEL (invoke_primitive)						\
{									\
  SCHEME_OBJECT * destination;						\
									\
  UNCACHE_VARIABLES ();							\
  PRIMITIVE_APPLY (val_register, primitive);				\
  POP_PRIMITIVE_FRAME (primitive_nargs);				\
  destination = (OBJECT_ADDRESS (STACK_POP ()));			\
  CACHE_VARIABLES ();							\
  JUMP (destination);							\
}

#define INVOKE_INTERFACE_DECLS						\
  int utlarg_code;							\
  long utlarg_1;							\
  long utlarg_2;							\
  long utlarg_3;							\
  long utlarg_4;

#define INVOKE_INTERFACE_0(code) do					\
{									\
  utlarg_code = (code);							\
  goto invoke_interface_0;						\
} while (0)

#define INVOKE_INTERFACE_1(code, one) do				\
{									\
  utlarg_code = (code);							\
  utlarg_1 = ((long) (one));						\
  goto invoke_interface_1;						\
} while (0)

#define INVOKE_INTERFACE_2(code, one, two) do				\
{									\
  utlarg_code = (code);							\
  utlarg_1 = ((long) (one));						\
  utlarg_2 = ((long) (two));						\
  goto invoke_interface_2;						\
} while (0)

#define INVOKE_INTERFACE_3(code, one, two, three) do			\
{									\
  utlarg_code = (code);							\
  utlarg_1 = ((long) (one));						\
  utlarg_2 = ((long) (two));						\
  utlarg_3 = ((long) (three));						\
  goto invoke_interface_3;						\
} while (0)

#define INVOKE_INTERFACE_4(code, one, two, three, four) do		\
{									\
  utlarg_code = (code);							\
  utlarg_1 = ((long) (one));						\
  utlarg_2 = ((long) (two));						\
  utlarg_3 = ((long) (three));						\
  utlarg_4 = ((long) (four));						\
  goto invoke_interface_4;						\
} while (0)

#define INVOKE_INTERFACE_TARGET_0					\
DEFLABEL (invoke_interface_0)						\
  utlarg_1 = 0;								\
  INVOKE_INTERFACE_TARGET_1

#define INVOKE_INTERFACE_TARGET_1					\
DEFLABEL (invoke_interface_1)						\
  utlarg_2 = 0;								\
  INVOKE_INTERFACE_TARGET_2

#define INVOKE_INTERFACE_TARGET_2					\
DEFLABEL (invoke_interface_2)						\
  utlarg_3 = 0;								\
  INVOKE_INTERFACE_TARGET_3

#define INVOKE_INTERFACE_TARGET_3					\
DEFLABEL (invoke_interface_3)						\
  utlarg_4 = 0;								\
  INVOKE_INTERFACE_TARGET_4

#define INVOKE_INTERFACE_TARGET_4					\
DEFLABEL (invoke_interface_4)						\
{									\
  SCHEME_OBJECT * destination;						\
									\
  UNCACHE_VARIABLES ();							\
  destination = (invoke_utility (utlarg_code, utlarg_1, utlarg_2,	\
				 utlarg_3, utlarg_4));			\
  CACHE_VARIABLES ();							\
  JUMP (destination);							\
}

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

#define INTERRUPT_CHECK(code, entry_point) do				\
{									\
  if (((long) Rhp) >= ((long) (Rrb[REGBLOCK_MEMTOP])))			\
    INVOKE_INTERFACE_1 (code, &current_block[entry_point]);		\
} while (0)

#define DLINK_INTERRUPT_CHECK(code, entry_point) do			\
{									\
  if (((long) Rhp) >= ((long) (Rrb[REGBLOCK_MEMTOP])))			\
    INVOKE_INTERFACE_2 (code, &current_block[entry_point], Rdl);	\
} while (0)

#define CLOSURE_HEADER(offset) do					\
{									\
  SCHEME_OBJECT * entry = ((SCHEME_OBJECT *) Rpc[1]);			\
  current_block = (entry - offset);					\
  *--Rsp = (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY, Rpc));		\
} while (0)

#define CLOSURE_INTERRUPT_CHECK(code) do				\
{									\
  if (((long) Rhp) >= ((long) (Rrb[REGBLOCK_MEMTOP])))			\
    INVOKE_INTERFACE_0 (code);						\
} while (0)

/* Linking and initialization */

struct liarc_code_S
{
  const char * name;
  entry_count_t nentries;
  SCHEME_OBJECT * EXFUN ((* code), (SCHEME_OBJECT *, entry_count_t));
};

struct liarc_data_S
{
  const char * name;
  SCHEME_OBJECT * EXFUN ((* data), (entry_count_t));
};

#define DECLARE_SUBCODE(name, nentries, code) do			\
{									\
  int result								\
    = (declare_compiled_code (name, nentries, NO_SUBBLOCKS, code));	\
  if (result != 0)							\
    return (result);							\
} while (0)

#define DECLARE_SUBDATA(name, data) do					\
{									\
  int result = (declare_compiled_data (name, NO_SUBBLOCKS, data));	\
  if (result != 0)							\
    return (result);							\
} while (0)

#define DECLARE_SUBCODE_MULTIPLE(code_array) do				\
{									\
  int result  =								\
    declare_compiled_code_mult (((sizeof (code_array))			\
				 / (sizeof (struct liarc_code_S))),	\
				(& code_array[0]));			\
  if (result != 0)							\
    return (result);							\
} while (0)

#define DECLARE_SUBDATA_MULTIPLE(data_array) do				\
{									\
  int result =								\
    declare_compiled_data_mult (((sizeof (data_array))			\
				 / (sizeof (struct liarc_data_S))),	\
				(& data_array[0]));			\
  if (result != 0)							\
    return (result);							\
} while (0)

#ifndef COMPILE_FOR_DYNAMIC_LOADING

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)
#define DECLARE_COMPILED_DATA(name, decl_data, data)
#define DECLARE_COMPILED_DATA_NS(name, data)
#define DECLARE_DATA_OBJECT(name, data)
#define DECLARE_DYNAMIC_INITIALIZATION(name)
#define DECLARE_DYNAMIC_OBJECT_INITIALIZATION(name)

#else /* COMPILE_FOR_DYNAMIC_LOADING */

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)		\
int EXFUN (decl_code, (void));						\
SCHEME_OBJECT * EXFUN (code, (SCHEME_OBJECT *, entry_count_t));		\
static int								\
DEFUN_VOID (dload_initialize_code)					\
{									\
  return (declare_compiled_code (name, nentries, decl_code, code));	\
}

#define DECLARE_COMPILED_DATA(name, decl_data, data)			\
int EXFUN (decl_data, (void));						\
SCHEME_OBJECT * EXFUN (data, (entry_count_t));				\
static int								\
DEFUN_VOID (dload_initialize_data)					\
{									\
  return (declare_compiled_data (name, decl_data, data));		\
}

#define DECLARE_COMPILED_DATA_NS(name, data)				\
SCHEME_OBJECT * EXFUN (data, (entry_count_t));				\
static int								\
DEFUN_VOID (dload_initialize_data)					\
{									\
  return (declare_compiled_data_ns (name, data));			\
}

#define DECLARE_DATA_OBJECT(name, data)					\
SCHEME_OBJECT EXFUN (data, (void));					\
static int								\
DEFUN_VOID (dload_initialize_data)					\
{									\
  return (declare_data_object (name, data));				\
}

#define DECLARE_DYNAMIC_INITIALIZATION(name)				\
char *									\
DEFUN_VOID (dload_initialize_file)					\
{									\
  int result = (dload_initialize_code ());				\
  if (result != 0)							\
    return ((char *) NULL);						\
  result = (dload_initialize_data ());					\
  if (result != 0)							\
    return ((char *) NULL);						\
  else									\
    return (name);							\
}

#define DECLARE_DYNAMIC_OBJECT_INITIALIZATION(name)			\
char *									\
DEFUN_VOID (dload_initialize_file)					\
{									\
  int result = (dload_initialize_data ());				\
  if (result != 0)							\
    return ((char *) NULL);						\
  else									\
    return (name);							\
}

#endif /* COMPILE_FOR_DYNAMIC_LOADING */

#ifdef USE_STDARG
# define RCONSM_TYPE(frob) SCHEME_OBJECT EXFUN (frob, (int, SCHEME_OBJECT DOTS))
#else /* not USE_STDARG */
# define RCONSM_TYPE(frob) SCHEME_OBJECT frob ()
#endif /* USE_STDARG */

extern RCONSM_TYPE(rconsm);

extern int
  EXFUN (multiply_with_overflow, (long, long, long *)),
  EXFUN (declare_compiled_code,
	 (char *,
	  entry_count_t,
	  int EXFUN ((*), (void)),
	  SCHEME_OBJECT * EXFUN ((*), (SCHEME_OBJECT *, entry_count_t)))),
  EXFUN (declare_compiled_data,
	 (char *,
	  int EXFUN ((*), (void)),
	  SCHEME_OBJECT * EXFUN ((*), (entry_count_t)))),
  EXFUN (declare_compiled_data_ns,
	 (char *,
	  SCHEME_OBJECT * EXFUN ((*), (entry_count_t)))),
  EXFUN (declare_data_object,
	 (char *,
	  SCHEME_OBJECT EXFUN ((*), (void)))),
  EXFUN (declare_compiled_code_mult, (unsigned, CONST struct liarc_code_S *)),
  EXFUN (declare_compiled_data_mult, (unsigned, CONST struct liarc_data_S *)),
  EXFUN (NO_SUBBLOCKS, (void));

extern SCHEME_OBJECT
  EXFUN (initialize_subblock, (char *)),
  * EXFUN (invoke_utility, (int, long, long, long, long)),
  EXFUN (unstackify, (unsigned char * prog, entry_count_t dispatch_base));

extern double
  EXFUN (acos, (double)),
  EXFUN (asin, (double)),
  EXFUN (atan, (double)),
  EXFUN (ceil, (double)),
  EXFUN (cos, (double)),
  EXFUN (exp, (double)),
  EXFUN (floor, (double)),
  EXFUN (log, (double)),
  EXFUN (sin, (double)),
  EXFUN (sqrt, (double)),
  EXFUN (tan, (double)),
  EXFUN (double_truncate, (double)),
  EXFUN (atan2, (double, double));

#define DOUBLE_ACOS acos
#define DOUBLE_ASIN asin
#define DOUBLE_ATAN atan
#define DOUBLE_CEILING ceil
#define DOUBLE_COS cos
#define DOUBLE_EXP exp
#define DOUBLE_FLOOR floor
#define DOUBLE_LOG log
#define DOUBLE_ROUND(dx) (double_truncate ((dx < 0) ? (dx - 0.5) : (dx + 0.5)))
#define DOUBLE_SIN sin
#define DOUBLE_SQRT sqrt
#define DOUBLE_TAN tan
#define DOUBLE_TRUNCATE double_truncate
#define DOUBLE_ATAN2 atan2

#ifdef __GNUC__
# if defined(hp9000s800) || defined(__hp9000s800)
#  define BUG_GCC_LONG_CALLS
# endif
#endif

#ifndef BUG_GCC_LONG_CALLS

extern SCHEME_OBJECT EXFUN (memory_to_string,
			    (unsigned long, CONST unsigned char *));
extern SCHEME_OBJECT EXFUN (memory_to_symbol, (unsigned long, CONST char *));
extern SCHEME_OBJECT EXFUN (make_vector, (long, SCHEME_OBJECT, Boolean));
extern SCHEME_OBJECT EXFUN (cons, (SCHEME_OBJECT, SCHEME_OBJECT));
extern SCHEME_OBJECT EXFUN (double_to_flonum, (double));
extern SCHEME_OBJECT EXFUN (long_to_integer, (long));
extern SCHEME_OBJECT EXFUN (digit_string_to_integer,
			    (Boolean, unsigned long, unsigned char *));
extern SCHEME_OBJECT EXFUN (digit_string_to_bit_string,
			    (unsigned long, unsigned long, unsigned char *));
extern SCHEME_OBJECT EXFUN (make_primitive, (char *, int));
extern SCHEME_OBJECT EXFUN (memory_to_uninterned_symbol,
			    (unsigned long, unsigned char *));

#define MEMORY_TO_STRING memory_to_string
#define MEMORY_TO_SYMBOL(len,str) memory_to_symbol (len, str)
#define MAKE_VECTOR(len,init,flag) make_vector (((long) len), init, flag)
#define CONS cons
#define RCONSM rconsm
#define DOUBLE_TO_FLONUM double_to_flonum
#define LONG_TO_INTEGER long_to_integer
#define DIGIT_STRING_TO_INTEGER(sgn,len,str) \
   digit_string_to_integer(sgn, ((unsigned long) len), ((unsigned char *) str))
#define DIGIT_STRING_TO_BIT_STRING(blen,len,str)			\
   digit_string_to_bit_string(((unsigned long) blen),			\
			      ((unsigned long) len),			\
			      ((unsigned char *) str))
#define MAKE_PRIMITIVE(str,arity)					\
  make_primitive (((char *) str), ((int) arity))
#define C_TO_UNINTERNED_SYMBOL memory_to_uninterned_symbol

#else /* GCC on Spectrum has a strange bug so do thing differently .... */

extern SCHEME_OBJECT EXFUN ((* (constructor_kludge [11])), ());

#define MEMORY_TO_STRING						\
     ((SCHEME_OBJECT EXFUN ((*), (unsigned long, unsigned char *)))	\
      (constructor_kludge[0]))

#define MEMORY_TO_SYMBOL						\
     ((SCHEME_OBJECT EXFUN ((*), (unsigned long, unsigned char *)))	\
      (constructor_kludge[1]))

#define MAKE_VECTOR							\
     ((SCHEME_OBJECT EXFUN ((*), (unsigned long, SCHEME_OBJECT, Boolean))) \
      (constructor_kludge[2]))

#define CONS								\
     ((SCHEME_OBJECT EXFUN ((*), (SCHEME_OBJECT, SCHEME_OBJECT)))	\
      (constructor_kludge[3]))

#define RCONSM								\
     ((RCONSM_TYPE ((*))) (constructor_kludge[4]))

#define DOUBLE_TO_FLONUM						\
     ((SCHEME_OBJECT EXFUN ((*), (double))) (constructor_kludge[5]))

#define LONG_TO_INTEGER							\
     ((SCHEME_OBJECT EXFUN ((*), (long))) (constructor_kludge[6]))

#define DIGIT_STRING_TO_INTEGER						\
     ((SCHEME_OBJECT EXFUN ((*), (Boolean, unsigned long, char *)))	\
      (constructor_kludge[7]))

#define DIGIT_STRING_TO_BIT_STRING					\
     ((SCHEME_OBJECT EXFUN ((*), (unsigned long, unsigned long, char *))) \
      (constructor_kludge[8]))

#define MAKE_PRIMITIVE							\
     ((SCHEME_OBJECT EXFUN ((*), (char *, int))) (constructor_kludge[9]))

#define C_TO_UNINTERNED_SYMBOL						\
     ((SCHEME_OBJECT EXFUN ((*), (unsigned long, char *)))		\
      (constructor_kludge[10]))

#endif /* BUG_GCC_LONG_CALLS */

#endif /* LIARC_INCLUDED */
