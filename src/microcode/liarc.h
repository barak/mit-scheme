/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

#ifndef SCM_LIARC_H_INCLUDED
#define SCM_LIARC_H_INCLUDED 1

#ifndef MIT_SCHEME
#  define MIT_SCHEME
#endif

#include "config.h"
#include "dstack.h"
#include "types.h"
#include "const.h"
#include "object.h"
#include "sdata.h"
#include "fixnum.h"
#include "errors.h"
#include "stack.h"
#include "interp.h"
#include "outf.h"
#include "extern.h"
#include "prim.h"
#include "cmpint.h"
#include "trap.h"

extern SCHEME_OBJECT * sp_register;

#ifdef __GNUC__
/* Add attributes to avoid warnings from -Wall for unreferenced labels */
#  define DEFLABEL(name) name : __attribute__((unused))
#else
#  define DEFLABEL(name) name :
#endif

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

#define ADDRESS_UNITS_PER_OBJECT SIZEOF_SCHEME_OBJECT
#define ADDRESS_UNITS_PER_FLOAT (sizeof (double))

#define CLOSURE_ENTRY_DELTA 1

#undef FIXNUM_TO_LONG
#define FIXNUM_TO_LONG(source)						\
  ((((long) (source)) << TYPE_CODE_LENGTH) >> TYPE_CODE_LENGTH)

#define ADDRESS_TO_LONG(source) ((long) (source))

#define LONG_TO_ADDRESS(source) (DATUM_TO_ADDRESS (source))

#define C_STRING_TO_SCHEME_STRING(len, str)				\
  (MEMORY_TO_STRING ((len), ((const byte_t *) (str))))

#define C_SYM_INTERN(len, str)						\
  (MEMORY_TO_SYMBOL ((len), ((const byte_t *) (str))))

#define MAKE_PRIMITIVE_PROCEDURE(name, arity) (MAKE_PRIMITIVE (name, arity))

#define WRITE_LABEL_DESCRIPTOR(entry, code_word, offset)		\
  ((entry[-1]) = (MAKE_LABEL_DESCRIPTOR ((code_word), (offset))))

#define MAKE_LABEL_DESCRIPTOR(code_word, offset)			\
  ((insn_t) (((offset) << 17) | (code_word)))

#define MAKE_LINKER_HEADER(kind, count)					\
  (OBJECT_NEW_TYPE (TC_FIXNUM,						\
		    (make_linkage_section_marker ((kind), (count)))))

#define ALLOCATE_VECTOR(len) (MAKE_VECTOR ((len), SHARP_F, true))

#define ALLOCATE_RECORD(len)						\
  (OBJECT_NEW_TYPE (TC_RECORD, (ALLOCATE_VECTOR (len))))

#define RECORD_SET(rec, off, val) VECTOR_SET ((rec), (off), (val))

#define INLINE_DOUBLE_TO_FLONUM(src, tgt) do				\
{									\
  double num = (src);							\
  SCHEME_OBJECT * val;							\
									\
  ALIGN_FLOAT (Rhp);							\
  val = Rhp;								\
  Rhp += (1 + (BYTES_TO_WORDS (sizeof (double))));			\
  (*val) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,				\
			 (BYTES_TO_WORDS (sizeof (double)))));		\
  (* ((double *) (val + 1))) = num;					\
  (tgt) = (MAKE_POINTER_OBJECT (TC_BIG_FLONUM, (val)));			\
} while (false)

#define MAKE_RATIO(num, den)						\
  (OBJECT_NEW_TYPE (TC_RATNUM, (CONS ((num), (den)))))

#define MAKE_COMPLEX(real, imag)					\
  (OBJECT_NEW_TYPE (TC_COMPLEX, (CONS ((real), (imag)))))

#define CC_BLOCK_TO_ENTRY(block, offset)				\
  (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY,				\
			((OBJECT_ADDRESS (block)) + (offset))))

#define INDEX_FIXNUM_P(arg) ((FIXNUM_P(arg)) && (FIXNUM_TO_ULONG_P (arg)))

#ifdef LIARC_IN_MICROCODE

#define Rvl (Registers[REGBLOCK_VAL])
#define Rhp Free
#define Rrb Registers
#define Rsp stack_pointer

#define DECLARE_VARIABLES() int unused_variable_to_keep_C_happy
#define UNCACHE_VARIABLES() do {} while (false)
#define CACHE_VARIABLES() do {} while (false)

#else /* !LIARC_IN_MICROCODE */

#define Rrb Registers

#undef MEMBASE
#define MEMBASE lcl_membase

#define DECLARE_VARIABLES()						\
  SCHEME_OBJECT Rvl = GET_VAL;						\
  SCHEME_OBJECT * Rhp = Free;						\
  SCHEME_OBJECT * Rsp = stack_pointer;					\
  SCHEME_OBJECT * lcl_membase = memory_base

#define DECLARE_VARIABLES_FOR_DATA()					\
  SCHEME_OBJECT * lcl_membase = memory_base

#define DECLARE_VARIABLES_FOR_OBJECT()

/* lcl_membase is not cached/uncached because it is a constant */

#define UNCACHE_VARIABLES() do						\
{									\
  stack_pointer = Rsp;							\
  Free = Rhp;								\
  SET_VAL (Rvl);							\
} while (false)

#define CACHE_VARIABLES() do						\
{									\
  Rvl = GET_VAL;							\
  Rhp = Free;								\
  Rsp = stack_pointer;							\
} while (false)

#endif /* !LIARC_IN_MICROCODE */

#ifdef ENABLE_DEBUGGING_TOOLS

#define JUMP(destination) do						\
{									\
  SCHEME_OBJECT * JUMP_new_pc = (destination);				\
  assert (JUMP_new_pc != 0);						\
  Rpc = JUMP_new_pc;							\
  goto perform_dispatch;						\
} while (false)

#else

#define JUMP(destination) do						\
{									\
  Rpc = (destination);							\
  goto perform_dispatch;						\
} while (false)

#endif

#define POP_RETURN() goto pop_return

#define INVOKE_PRIMITIVE_DECLS
#define INVOKE_PRIMITIVE_TARGET

#define INVOKE_PRIMITIVE(prim, nargs) do				\
{									\
  SCHEME_OBJECT * IPdest;						\
									\
  UNCACHE_VARIABLES ();							\
  PRIMITIVE_APPLY (prim);						\
  POP_PRIMITIVE_FRAME (nargs);						\
  IPdest = (OBJECT_ADDRESS (STACK_POP ()));				\
  CACHE_VARIABLES ();							\
  JUMP (IPdest);							\
} while (false)

#define INVOKE_INTERFACE_DECLS
#define INVOKE_INTERFACE_TARGET_0
#define INVOKE_INTERFACE_TARGET_1
#define INVOKE_INTERFACE_TARGET_2
#define INVOKE_INTERFACE_TARGET_3
#define INVOKE_INTERFACE_TARGET_4

#define INVOKE_INTERFACE_0(code)					\
  INVOKE_INTERFACE_4 (code, 0, 0, 0, 0)

#define INVOKE_INTERFACE_1(code, one)					\
  INVOKE_INTERFACE_4 (code, one, 0, 0, 0)

#define INVOKE_INTERFACE_2(code, one, two)				\
  INVOKE_INTERFACE_4 (code, one, two, 0, 0)

#define INVOKE_INTERFACE_3(code, one, two, three)			\
  INVOKE_INTERFACE_4 (code, one, two, three, 0)

#define INVOKE_INTERFACE_4(code, one, two, three, four) do		\
{									\
  SCHEME_OBJECT * IICdest;						\
									\
  UNCACHE_VARIABLES ();							\
  IICdest								\
    = (invoke_utility ((code),						\
		       ((unsigned long) (one)),				\
		       ((unsigned long) (two)),				\
		       ((unsigned long) (three)),			\
		       ((unsigned long) (four))));			\
  CACHE_VARIABLES ();							\
  JUMP (IICdest);							\
} while (false)

#define INTERRUPT_CHECK(code, entry_point) do				\
{									\
  if ((((long) Rhp) >= ((long) GET_MEMTOP))				\
      || (((long) Rsp) < ((long) GET_STACK_GUARD)))			\
    INVOKE_INTERFACE_1 (code, (&current_block[entry_point]));		\
} while (false)

#define DLINK_INTERRUPT_CHECK(code, entry_point) do			\
{									\
  if ((((long) Rhp) >= ((long) GET_MEMTOP))				\
      || (((long) Rsp) < ((long) GET_STACK_GUARD)))			\
    INVOKE_INTERFACE_2 (code, (&current_block[entry_point]), Rdl);	\
} while (false)

#define CLOSURE_INTERRUPT_CHECK(code) do				\
{									\
  if ((((long) Rhp) >= ((long) GET_MEMTOP))				\
      || (((long) Rsp) < ((long) GET_STACK_GUARD)))			\
    INVOKE_INTERFACE_0 (code);						\
} while (false)

#define CLOSURE_HEADER(offset) do					\
{									\
  SCHEME_OBJECT * entry = ((SCHEME_OBJECT *) (Rpc[1]));			\
  current_block = (entry - offset);					\
  (*--Rsp) = (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY, Rpc));		\
} while (false)

/* Linking and initialization */

typedef int liarc_decl_code_t (void);
typedef int liarc_decl_data_t (void);
typedef SCHEME_OBJECT * liarc_code_proc_t (SCHEME_OBJECT *, entry_count_t);
typedef SCHEME_OBJECT * liarc_data_proc_t (entry_count_t);
typedef SCHEME_OBJECT liarc_object_proc_t (void);

struct liarc_code_S
{
  const char * name;
  entry_count_t nentries;
  liarc_code_proc_t * code;
};

struct liarc_data_S
{
  const char * name;
  liarc_data_proc_t * data;
};

#define DECLARE_SUBCODE(name, nentries, code) do			\
{									\
  int result = (declare_compiled_code_ns (name, nentries, code));	\
  if (result != 0)							\
    return (result);							\
} while (false)

#define DECLARE_SUBDATA(name, data) do					\
{									\
  int result = (declare_compiled_data_ns (name, data));			\
  if (result != 0)							\
    return (result);							\
} while (false)

#define DECLARE_SUBCODE_MULTIPLE(code_array) do				\
{									\
  int result								\
    = (declare_compiled_code_mult					\
       (((sizeof (code_array)) / (sizeof (struct liarc_code_S))),	\
	code_array));							\
  if (result != 0)							\
    return (result);							\
} while (false)

#define DECLARE_SUBDATA_MULTIPLE(data_array) do				\
{									\
  int result								\
    = (declare_compiled_data_mult					\
       (((sizeof (data_array)) / (sizeof (struct liarc_data_S))),	\
	data_array));							\
  if (result != 0)							\
    return (result);							\
} while (false)

#ifdef ENABLE_LIARC_FILE_INIT

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)		\
static int								\
dload_initialize_code (void)						\
{									\
  return (declare_compiled_code (name, nentries, decl_code, code));	\
}

#define DECLARE_COMPILED_DATA(name, decl_data, data)			\
static int								\
dload_initialize_data (void)						\
{									\
  return (declare_compiled_data (name, decl_data, data));		\
}

#define DECLARE_COMPILED_DATA_NS(name, data)				\
static int								\
dload_initialize_data (void)						\
{									\
  return (declare_compiled_data_ns (name, data));			\
}

#define DECLARE_DATA_OBJECT(name, data)					\
static int								\
dload_initialize_data (void)						\
{									\
  return (declare_data_object (name, data));				\
}

#define DECLARE_DYNAMIC_INITIALIZATION(name, nonce)			\
const char dload_nonce [] = nonce;					\
									\
const char *								\
dload_initialize_file (void)						\
{									\
  return								\
    ((((dload_initialize_code ()) == 0)					\
      && ((dload_initialize_data ()) == 0))				\
     ? (liarc_object_file_name (name))					\
     : 0);								\
}

#define DECLARE_DYNAMIC_OBJECT_INITIALIZATION(name, nonce)		\
const char dload_nonce [] = nonce;					\
									\
const char *								\
dload_initialize_file (void)						\
{									\
  return                                                                \
    (((dload_initialize_data ()) == 0)                                  \
     ? (liarc_object_file_name (name))					\
     : 0);                                                              \
}

#else /* !ENABLE_LIARC_FILE_INIT */

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code)
#define DECLARE_COMPILED_DATA(name, decl_data, data)
#define DECLARE_COMPILED_DATA_NS(name, data)
#define DECLARE_DATA_OBJECT(name, data)
#define DECLARE_DYNAMIC_INITIALIZATION(name, nonce)
#define DECLARE_DYNAMIC_OBJECT_INITIALIZATION(name, nonce)

#endif /* !ENABLE_LIARC_FILE_INIT */

extern SCHEME_OBJECT initialize_subblock (const char *);

extern SCHEME_OBJECT * invoke_utility
  (unsigned int, unsigned long, unsigned long, unsigned long, unsigned long);

extern int declare_compiled_code
  (const char *, entry_count_t, liarc_decl_code_t *, liarc_code_proc_t *);

extern int declare_compiled_code_ns
  (const char *, entry_count_t, liarc_code_proc_t *);

extern int declare_compiled_data
  (const char *, liarc_decl_data_t *, liarc_data_proc_t *);

extern int declare_compiled_data_ns (const char *, liarc_data_proc_t *);
extern int declare_data_object (const char *, liarc_object_proc_t *);
extern int declare_compiled_code_mult (unsigned, const struct liarc_code_S *);
extern int declare_compiled_data_mult (unsigned, const struct liarc_data_S *);

extern const char * liarc_object_file_name (const char *);

extern SCHEME_OBJECT unstackify (unsigned char *, size_t, entry_count_t);

extern int multiply_with_overflow (long, long, long *);

#define DOUBLE_ACOS acos
#define DOUBLE_ASIN asin
#define DOUBLE_ATAN atan
#define DOUBLE_CEILING ceil
#define DOUBLE_COS cos
#define DOUBLE_EXP exp
#define DOUBLE_EXPM1 expm1
#define DOUBLE_FLOOR floor
#define DOUBLE_LOG log
#define DOUBLE_LOG1P log1p
#define DOUBLE_SIN sin
#define DOUBLE_SQRT sqrt
#define DOUBLE_TAN tan
#define DOUBLE_TRUNCATE double_truncate
#define DOUBLE_ROUND double_round
#define DOUBLE_ATAN2 atan2

#define MAKE_PRIMITIVE(str, arity)					\
  (make_primitive (((const char *) (str)), ((int) (arity))))

#define MEMORY_TO_STRING memory_to_string
#define MEMORY_TO_SYMBOL memory_to_symbol
#define MAKE_VECTOR make_vector
#define CONS cons
#define RCONSM rconsm
#define DOUBLE_TO_FLONUM double_to_flonum
#define LONG_TO_INTEGER long_to_integer
#define C_TO_UNINTERNED_SYMBOL memory_to_uninterned_symbol
#define DIGIT_STRING_TO_INTEGER digit_string_to_integer
#define DIGIT_STRING_TO_BIT_STRING digit_string_to_bit_string

extern SCHEME_OBJECT rconsm (unsigned int, SCHEME_OBJECT, ...);
extern SCHEME_OBJECT memory_to_uninterned_symbol (unsigned long, const void *);

extern SCHEME_OBJECT digit_string_to_integer
  (bool, unsigned long, const char *);

extern SCHEME_OBJECT digit_string_to_bit_string
  (unsigned long, unsigned long, const char *);

#endif /* !SCM_LIARC_H_INCLUDED */
