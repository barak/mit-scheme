/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

/* Scheme Virtual Machine version 1 */

#include "scheme.h"
#include "fixnum.h"
#include "svm1-defns.h"
#include "cmpintmd/svm1.h"

#ifndef __GNUC__
#  define GNUC_PREREQ(x, y) 0
#else
#  define GNUC_PREREQ(x, y)						\
  ((__GNUC__ > (x)) || ((__GNUC__ == (x)) && (__GNUC_MINOR__ >= (y))))
#endif

/* Don't trust pre-C99 inline.  */
#if __STDC_VERSION__ < 199901L
#  define inline
#endif

#if GNUC_PREREQ(2,96)
#  define predict_true(condition)	__builtin_expect(((condition) != 0), 1)
#  define predict_false(condition)	__builtin_expect(((condition) != 0), 0)
#else
#endif

typedef SCHEME_OBJECT word_t;	/* convenience abbreviation */

#define N_WORD_REGISTERS 0x100
#define N_FLOAT_REGISTERS 0x100

#if (N_WORD_REGISTERS < (UCHAR_MAX + 1))
#  define WORD_REGISTER_P(b) ((b) < N_WORD_REGISTERS)
#else
#  define WORD_REGISTER_P(b) true
#endif
#if (N_FLOAT_REGISTERS < (UCHAR_MAX + 1))
#  define FLOAT_REGISTER_P(b) ((b) < N_FLOAT_REGISTERS)
#else
#  define FLOAT_REGISTER_P(b) true
#endif

static word_t word_registers [N_WORD_REGISTERS];
static double float_registers [N_FLOAT_REGISTERS];

#define SBYTE (sizeof (byte_t))
#define SWORD (sizeof (word_t))
#define SFLOAT (sizeof (double))

#define PC program_counter
#define NEXT_BYTE (*program_counter++)

typedef byte_t wreg_t;
typedef byte_t freg_t;
typedef byte_t tc_t;

#define WREG_REF(wr) (word_registers [(wr)])
#define FREG_REF(fr) (float_registers [(fr)])

#define WREG_SET(wr, w) ((word_registers [(wr)]) = (w))
#define FREG_SET(fr, f) ((float_registers [(fr)]) = (f))

#define BYTE_ADDR(a) ((byte_t *) a)
#define WORD_ADDR(a) ((word_t *) a)
#define FLOAT_ADDR(a) ((double *) a)

#define BYTE_REF(a) (* (BYTE_ADDR (a)))
#define WORD_REF(a) (* (WORD_ADDR (a)))
#define FLOAT_REF(a) (* (FLOAT_ADDR (a)))


typedef byte_t * inst_defn_t (byte_t *);
static inst_defn_t * inst_defns [256];

#define DEFINE_INST(name) static byte_t * insn_##name (byte_t * PC)
#define NEXT_PC return (PC)
#define OFFSET_PC(o) do							\
{									\
  PC = PC + (o);							\
  return (PC);								\
} while (0)

#define COND_OFFSET_PC(p, o) do						\
{									\
  if (predict_false (p)) { PC = PC + (o); }				\
  return (PC);								\
} while (0)

#define NEW_PC(addr) do							\
{									\
  return (addr);							\
} while (0)

static long svm1_result;

#define EXIT_VM(code) do						\
{									\
  svm1_result = (code);							\
  return (0);								\
} while (0)

#define	ILL EXIT_VM (ERR_COMPILED_CODE_ERROR)

typedef byte_t * address_decoder_t (byte_t *, word_t *);
static address_decoder_t * address_decoders [256];

#define ADDRESS_VALUE(name) name

#define DEFINE_ADDRESS_DECODER(name)					\
  static byte_t * decode_addr_##name (byte_t * PC,			\
				      word_t * address)
#define ADDRESS_DECODED(addr) (*address) = (addr); return (PC)
#define DECODE_ADDRESS(name)						\
  word_t name;								\
  {									\
    byte_t * new_pc = decode_address (PC, &name);			\
    if (predict_false (! new_pc)) ILL;					\
    PC = new_pc;							\
  }
static byte_t * decode_address (byte_t *, word_t *);

typedef byte_t * trap_0_t (byte_t *);
typedef byte_t * trap_1_t (byte_t *, wreg_t);
typedef byte_t * trap_2_t (byte_t *, wreg_t, wreg_t);
typedef byte_t * trap_3_t (byte_t *, wreg_t, wreg_t, wreg_t);

static trap_0_t * traps_0 [256];
static trap_1_t * traps_1 [256];
static trap_2_t * traps_2 [256];
static trap_3_t * traps_3 [256];

#define DECODE_TRAP_0(name) byte_t name = NEXT_BYTE
#define DECODE_TRAP_1(name) byte_t name = NEXT_BYTE
#define DECODE_TRAP_2(name) byte_t name = NEXT_BYTE
#define DECODE_TRAP_3(name) byte_t name = NEXT_BYTE

static void initialize_decoder_tables (void);

static int initialized_p = 0;
static int little_endian_p;

static void
compute_little_endian_p (void)
{
  union
    {
      unsigned long n;
      char b [(sizeof (unsigned long))];
    } ue;
  (ue.n) = 1;
  little_endian_p = (((ue.b) [0]) == 1);
}

void
initialize_svm1 (void)
{
  unsigned int i;

  if (predict_false (!initialized_p))
    {
      compute_little_endian_p ();
      initialize_decoder_tables ();
      initialized_p = 1;
    }
  for (i = 0; (i < N_WORD_REGISTERS); i += 1)
    WREG_SET (i, 0);
  for (i = 0; (i < N_FLOAT_REGISTERS); i += 1)
    FREG_SET (i, 0.0);
  WREG_SET (SVM1_REG_INTERPRETER_REGISTER_BLOCK, ((word_t)Registers));

  assert (((sizeof (double)) / (sizeof (SCHEME_OBJECT))) <= COMPILER_TEMP_SIZE);
}

#define IMPORT_REGS() do						\
{									\
  WREG_SET (SVM1_REG_STACK_POINTER, ((word_t)stack_pointer));		\
  WREG_SET (SVM1_REG_FREE_POINTER, ((word_t)Free));			\
  WREG_SET (SVM1_REG_VALUE, GET_VAL);					\
  WREG_SET (SVM1_REG_DYNAMIC_LINK, ((word_t)(OBJECT_ADDRESS(GET_VAL)))); \
} while (0)

#define EXPORT_REGS() do						\
{									\
  stack_pointer								\
    = ((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_STACK_POINTER)));		\
  Free = ((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_FREE_POINTER)));	\
  SET_VAL ((SCHEME_OBJECT) (WREG_REF (SVM1_REG_VALUE)));		\
} while (0)

long
C_to_interface (void * address)
{
  insn_t * PC = (insn_t *)address;
  IMPORT_REGS ();
  while (predict_true (PC))
    {
      byte_t opcode = *PC++;
      PC = (* (inst_defns[opcode])) (PC);
    }
  EXPORT_REGS ();
  return (svm1_result);
}

static byte_t *
illegal_instruction (byte_t * PC)
{
  ILL;
}

#define TO_SIGNED(n) ((long) (n))
#define FROM_SIGNED(n) ((word_t) (n))

#define SIGNED_UNARY(op, a1)						\
  (FROM_SIGNED (op (TO_SIGNED (a1))))

#define SIGNED_BINARY(op, a1, a2)					\
  (FROM_SIGNED ((TO_SIGNED (a1)) op (TO_SIGNED (a2))))

#define SIGNED_BINFUNC(func, a1, a2)					\
  (FROM_SIGNED (func (TO_SIGNED (a1), TO_SIGNED (a2))))

#if 0
/* The above definition isn't guaranteed to work in ANSI C, but in
   practice it usually does.  Here's an alternative that should always
   work (in machines that use 2's complement).  */
#define TO_SIGNED(n) (to_signed (n))

static inline long
to_signed (word_t n)
{
  union { unsigned long n1; long n2; } us;
  (us.n1) = n;
  return (us.n2);
}
#endif

/* Primitive decoders */

#define DECODE_ARG(name,func) do					\
  {									\
    byte_t * new_pc = func (PC, &name);			\
    if (predict_false (! new_pc)) ILL;					\
    PC = new_pc;						\
  } while (0);

#define DECODE_WORD_REGISTER(name)  wreg_t name; DECODE_ARG(name,decode_wreg)
#define DECODE_FLOAT_REGISTER(name) freg_t name; DECODE_ARG(name,decode_freg)
#define DECODE_TYPE_WORD(name)        tc_t name; DECODE_ARG(name,decode_type_word)
#define DECODE_UNSIGNED_8(name)     word_t name; DECODE_ARG(name,decode_unsigned_8)
#define DECODE_UNSIGNED_16(name)    word_t name; DECODE_ARG(name,decode_unsigned_16)
#define DECODE_UNSIGNED_32(name)    word_t name; DECODE_ARG(name,decode_unsigned_32)
#define DECODE_SIGNED_8(name)         long name; DECODE_ARG(name,decode_signed_8)
#define DECODE_SIGNED_16(name)        long name; DECODE_ARG(name,decode_signed_16)
#define DECODE_SIGNED_32(name)        long name; DECODE_ARG(name,decode_signed_32)
#define DECODE_FLOAT(name)          double name; DECODE_ARG(name,decode_float)

static inline byte_t *
decode_wreg (byte_t * PC, wreg_t *wreg)
{
  byte_t b = NEXT_BYTE;
  if (predict_false (! (WORD_REGISTER_P (b))))
    return (NULL);
  *wreg = b;
  return (PC);
}

static inline byte_t *
decode_freg (byte_t * PC, freg_t *freg)
{
  byte_t b = NEXT_BYTE;
  if (predict_false (! (FLOAT_REGISTER_P (b))))
    return (NULL);
  *freg = b;
  return (PC);
}

static inline byte_t *
decode_type_word (byte_t * PC, tc_t *tc)
{
  byte_t b = NEXT_BYTE;
  if (predict_false (! (b <= N_TYPE_CODES)))
    return (NULL);
  *tc = b;
  return (PC);
}

static inline byte_t *
decode_unsigned_8 (byte_t * PC, word_t *word)
{
  *word = (NEXT_BYTE);
  return (PC);
}

static inline byte_t *
decode_unsigned_16 (byte_t * PC, word_t *word)
{
  word_t b0 = NEXT_BYTE;
  word_t b1 = NEXT_BYTE;
  *word = ((b1 << 8) | b0);
  return (PC);
}

static inline byte_t *
decode_unsigned_32 (byte_t * PC, word_t *word)
{
  word_t b0 = NEXT_BYTE;
  word_t b1 = NEXT_BYTE;
  word_t b2 = NEXT_BYTE;
  word_t b3 = NEXT_BYTE;
  *word = ((b3 << 24) | (b2 << 16) | (b1 << 8) | b0);
  return (PC);
}

static inline byte_t *
decode_unsigned_64 (byte_t * PC, uint64_t *big)
{
  uint64_t b0, b1, b2, b3, b4, b5, b6, b7;
  b0 = NEXT_BYTE; b1 = NEXT_BYTE; b2 = NEXT_BYTE; b3 = NEXT_BYTE;
  b4 = NEXT_BYTE; b5 = NEXT_BYTE; b6 = NEXT_BYTE; b7 = NEXT_BYTE;
  *big = ((b7 << 56) | (b6 << 48) | (b5 << 40) | (b4 << 32)
	  | (b3 << 24) | (b2 << 16) | (b1 << 8) | b0);
  return (PC);
}

static inline byte_t *
decode_signed_8 (byte_t * PC, long *lng)
{
  long b = NEXT_BYTE;
  *lng = ((b < 0x80) ? b : (b - 0x100));
  return (PC);
}

static inline byte_t *
decode_signed_16 (byte_t * PC, long *lng)
{
  unsigned long n;
  PC = decode_unsigned_16 (PC, &n);
  if (predict_true (PC))
    *lng = ((n < 0x8000) ? n : (n - 0x10000));
  return (PC);
}

static inline byte_t *
decode_signed_32 (byte_t * PC, long *lng)
{
  word_t n;
  PC = (decode_unsigned_32 (PC, &n));
  if (predict_false(! PC)) return (PC);
  if (n < 0x80000000UL)
    {
      *lng = (long) n;
      return (PC);
    }
#if (LONG_MAX > 0x7FFFFFFFUL)
  *lng = (((long) n) - 0x100000000L);
#else
  n -= 0x80000000UL;
  {
    long r = ((long) n);
    r -= 0x40000000L;
    r -= 0x40000000L;
    *lng = r;
  }
#endif
  return (PC);
}

static inline byte_t *
decode_signed_64 (byte_t * PC, int64_t * big)
{
  uint64_t n;
  PC = (decode_unsigned_64 (PC, &n));
  if (predict_false (! PC)) return (PC);
  if (n < ((uint64_t) 0x8000000000000000))
    {
      *big = ((int64_t) n);
      return (PC);
    }
  n -= ((uint64_t) 0x8000000000000000);
  {
    int64_t r = ((int64_t) n);
    r -= ((int64_t) 0x4000000000000000);
    r -= ((int64_t) 0x4000000000000000);
    *big = r;
    return (PC);
  }
}

static inline byte_t *
decode_float (byte_t * PC, double * dbl)
{
  int64_t significand;
  long exponent;
  PC = decode_signed_64 (PC, &significand);
  if (predict_false (! PC)) return (PC);
  PC = decode_signed_16 (PC, &exponent);
  if (predict_false (! PC)) return (PC);
  *dbl = (ldexp (((double) significand), exponent));
  return (PC);
}

/* Instruction definitions */

DEFINE_INST (store_b_wr_addr)
{
  DECODE_SVM1_INST_STORE_B_WR_ADDR (source, address);
  (BYTE_REF (ADDRESS_VALUE (address))) = ((WREG_REF (source)) & 0xFF);
  NEXT_PC;
}

DEFINE_INST (store_w_wr_addr)
{
  DECODE_SVM1_INST_STORE_W_WR_ADDR (source, address);
  (WORD_REF (ADDRESS_VALUE (address))) = (WREG_REF (source));
  NEXT_PC;
}

DEFINE_INST (store_f_fr_addr)
{
  DECODE_SVM1_INST_STORE_F_FR_ADDR (source, address);
  (FLOAT_REF (ADDRESS_VALUE (address))) = (FREG_REF (source));
  NEXT_PC;
}

DEFINE_INST (load_b_wr_addr)
{
  DECODE_SVM1_INST_LOAD_B_WR_ADDR (target, address);
  WREG_SET (target, (BYTE_REF (ADDRESS_VALUE (address))));
  NEXT_PC;
}

DEFINE_INST (load_w_wr_addr)
{
  DECODE_SVM1_INST_LOAD_W_WR_ADDR (target, address);
  WREG_SET (target, (WORD_REF (ADDRESS_VALUE (address))));
  NEXT_PC;
}

DEFINE_INST (load_f_fr_addr)
{
  DECODE_SVM1_INST_LOAD_F_FR_ADDR (target, address);
  FREG_SET (target, (FLOAT_REF (ADDRESS_VALUE (address))));
  NEXT_PC;
}

DEFINE_INST (load_address_addr)
{
  DECODE_SVM1_INST_LOAD_ADDRESS_ADDR (target, address);
  WREG_SET (target, (ADDRESS_VALUE (address)));
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_s8)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_S8 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_s16)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_S16 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_s32)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_S32 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_fr_flt)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_FR_FLT (target, value);
  FREG_SET (target, value);
  NEXT_PC;
}

#define TYPE_CODE_MASK_LOW (N_TYPE_CODES - 1U)

#define X_MAKE_OBJECT(t, d)						\
  (MAKE_OBJECT (((t) & TYPE_CODE_MASK_LOW), ((d) & DATUM_MASK)))

#define X_MAKE_PTR(t, a) (X_MAKE_OBJECT (t, (ADDRESS_TO_DATUM (a))))

#define X_OBJECT_ADDRESS(o) ((word_t) (OBJECT_ADDRESS (o)))

DEFINE_INST (load_non_pointer_tc_s8)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_TC_S8 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT (type, datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_tc_s16)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_TC_S16 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT (type, datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_tc_s32)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_TC_S32 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT (type, datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_wr_s8)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_WR_S8 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT ((WREG_REF (type)), datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_wr_s16)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_WR_S16 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT ((WREG_REF (type)), datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_wr_s32)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_WR_S32 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT ((WREG_REF (type)), datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_tc_wr)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_TC_WR (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT (type, (WREG_REF (datum)))));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT ((WREG_REF (type)), (WREG_REF (datum)))));
  NEXT_PC;
}

DEFINE_INST (load_pointer_tc_wr)
{
  DECODE_SVM1_INST_LOAD_POINTER_TC_WR (target, type, address);
  WREG_SET (target, (X_MAKE_PTR (type, (WREG_REF (address)))));
  NEXT_PC;
}

DEFINE_INST (load_pointer)
{
  DECODE_SVM1_INST_LOAD_POINTER (target, type, address);
  WREG_SET (target, (X_MAKE_PTR ((WREG_REF (type)), (WREG_REF (address)))));
  NEXT_PC;
}

static void
copy_block (word_t * to, word_t * from, word_t n_words)
{
  if (to > from)
    {
      word_t * p1 = (to + n_words);
      word_t * p2 = (from + n_words);
      while (p2 > from)
	(*--p1) = (*--p2);
    }
  else if (to < from)
    {
      word_t * p2 = (from + n_words);
      while (from < p2)
	(*to++) = (*from++);
    }
}

DEFINE_INST (copy_block_u8_w)
{
  DECODE_SVM1_INST_COPY_BLOCK_U8_W (r_to, r_from, n_words);
  copy_block ((WORD_ADDR (WREG_REF (r_to))),
	      (WORD_ADDR (WREG_REF (r_from))),
	      n_words);
  NEXT_PC;
}

DEFINE_INST (copy_block_wr_w)
{
  DECODE_SVM1_INST_COPY_BLOCK_WR_W (r_to, r_from, r_n_words);
  copy_block ((WORD_ADDR (WREG_REF (r_to))),
	      (WORD_ADDR (WREG_REF (r_from))),
	      (WREG_REF (r_n_words)));
  NEXT_PC;
}

DEFINE_INST (jump_pcr_s8)
{
  DECODE_SVM1_INST_JUMP_PCR_S8 (offset);
  OFFSET_PC (offset);
}

DEFINE_INST (jump_pcr_s16)
{
  DECODE_SVM1_INST_JUMP_PCR_S16 (offset);
  OFFSET_PC (offset);
}

DEFINE_INST (jump_pcr_s32)
{
  DECODE_SVM1_INST_JUMP_PCR_S32 (offset);
  OFFSET_PC (offset);
}

DEFINE_INST (jump_indir_wr)
{
  DECODE_SVM1_INST_JUMP_INDIR_WR (address);
  NEW_PC (BYTE_ADDR (WREG_REF (address)));
}

#define IJUMP(offset)							\
  NEW_PC (BYTE_ADDR (OBJECT_ADDRESS (* ((SCHEME_OBJECT *) (PC + (offset))))))

DEFINE_INST (ijump_u8)
{
  DECODE_SVM1_INST_IJUMP_U8 (offset);
  IJUMP (offset);
}

DEFINE_INST (ijump_u16)
{
  DECODE_SVM1_INST_IJUMP_U16 (offset);
  IJUMP (offset);
}

DEFINE_INST (ijump_u32)
{
  DECODE_SVM1_INST_IJUMP_U32 (offset);
  IJUMP (offset);
}

static inline void
push_object (SCHEME_OBJECT object)
{
  stack_pointer = ((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_STACK_POINTER)));
  STACK_PUSH (object);
  WREG_SET (SVM1_REG_STACK_POINTER, ((SCHEME_OBJECT) stack_pointer));
}

static inline void
push_entry (byte_t * PC)
{
  push_object (MAKE_CC_ENTRY (PC + CC_ENTRY_HEADER_SIZE));
}

DEFINE_INST (icall_u8)
{
  DECODE_SVM1_INST_ICALL_U8 (offset);
  push_entry (PC);
  IJUMP (offset);
}

DEFINE_INST (icall_u16)
{
  DECODE_SVM1_INST_ICALL_U16 (offset);
  push_entry (PC);
  IJUMP (offset);
}

DEFINE_INST (icall_u32)
{
  DECODE_SVM1_INST_ICALL_U32 (offset);
  push_entry (PC);
  IJUMP (offset);
}

/* Conditional jumps */

#define DEFINE_CJ_1(pl, pu, rl, ru, sl, su)				\
DEFINE_INST (cjump_##pl##_##rl##_##rl##_pcr_##sl)			\
{									\
  DECODE_SVM1_INST_CJUMP_##pu##_##ru##_##ru##_PCR_##su			\
    (source1, source2, offset);						\
  CJ_PCR (CMP_##pu ((ru##EG_REF (source1)), (ru##EG_REF (source2))));	\
}

#define DEFINE_CJ_2(pl, pu, rl, ru, z, sl, su)				\
DEFINE_INST (cjump_##pl##_##rl##_pcr_##sl)				\
{									\
  DECODE_SVM1_INST_CJUMP_##pu##_##ru##_PCR_##su (source, offset);	\
  CJ_PCR (CMP_##pu ((ru##EG_REF (source)), z));				\
}

#define CJ_PCR(p) COND_OFFSET_PC (p, offset)

#define DEFINE_CJ_3(pl, pu, rl, ru, z, sl, su)				\
DEFINE_CJ_1 (pl, pu, rl, ru, sl, su)					\
DEFINE_CJ_2 (pl, pu, rl, ru, z, sl, su)

#define DEFINE_CJ_WR(pl, pu)						\
DEFINE_CJ_3 (pl, pu, wr, WR, 0, s8, S8)					\
DEFINE_CJ_3 (pl, pu, wr, WR, 0, s16, S16)				\
DEFINE_CJ_3 (pl, pu, wr, WR, 0, s32, S32)

#define DEFINE_CJ_WR_NZ(pl, pu)						\
DEFINE_CJ_1 (pl, pu, wr, WR, s8, S8)					\
DEFINE_CJ_1 (pl, pu, wr, WR, s16, S16)					\
DEFINE_CJ_1 (pl, pu, wr, WR, s32, S32)

#define DEFINE_CJ_FR(pl, pu)						\
DEFINE_CJ_3 (pl, pu, fr, FR, 0.0, s8, S8)				\
DEFINE_CJ_3 (pl, pu, fr, FR, 0.0, s16, S16)				\
DEFINE_CJ_3 (pl, pu, fr, FR, 0.0, s32, S32)

#define CMP_EQ(a, b) ((a) == (b))
#define CMP_NEQ(a, b) ((a) != (b))
#define CMP_LT(a, b) ((a) < (b))
#define CMP_LE(a, b) ((a) <= (b))
#define CMP_GT(a, b) ((a) > (b))
#define CMP_GE(a, b) ((a) >= (b))
#define CMP_SLT(a, b) ((TO_SIGNED (a)) < (TO_SIGNED (b)))
#define CMP_SLE(a, b) ((TO_SIGNED (a)) <= (TO_SIGNED (b)))
#define CMP_SGT(a, b) ((TO_SIGNED (a)) > (TO_SIGNED (b)))
#define CMP_SGE(a, b) ((TO_SIGNED (a)) >= (TO_SIGNED (b)))
#define CMP_CMP(a, b) (cmp_cmp ((a), (b)))
#define CMP_NCMP(a, b) (!cmp_cmp ((a), (b)))

static inline int
cmp_cmp (double a, double b)
{
  return ((a < b) || (a > b) || (a == b));
}

DEFINE_CJ_WR (eq, EQ)
DEFINE_CJ_WR (neq, NEQ)
DEFINE_CJ_WR (slt, SLT)
DEFINE_CJ_WR (sle, SLE)
DEFINE_CJ_WR (sgt, SGT)
DEFINE_CJ_WR (sge, SGE)

DEFINE_CJ_WR_NZ (lt, LT)
DEFINE_CJ_WR_NZ (le, LE)
DEFINE_CJ_WR_NZ (gt, GT)
DEFINE_CJ_WR_NZ (ge, GE)

DEFINE_CJ_FR (eq, EQ)
DEFINE_CJ_FR (neq, NEQ)
DEFINE_CJ_FR (lt, LT)
DEFINE_CJ_FR (le, LE)
DEFINE_CJ_FR (gt, GT)
DEFINE_CJ_FR (ge, GE)
DEFINE_CJ_FR (cmp, CMP)
DEFINE_CJ_FR (ncmp, NCMP)

#define DEFINE_CJF_1(pl, pu, sl, su)					\
DEFINE_INST (cjump_##pl##_wr_pcr_##sl)					\
{									\
  DECODE_SVM1_INST_CJUMP_##pu##_WR_PCR_##su (source, offset);		\
  CJ_PCR (CMP_##pu (WREG_REF (source)));				\
}

#define DEFINE_CJF(pl, pu)						\
DEFINE_CJF_1 (pl, pu, s8, S8)						\
DEFINE_CJF_1 (pl, pu, s16, S16)						\
DEFINE_CJF_1 (pl, pu, s32, S32)

#define CMP_FIX(a) (LONG_TO_FIXNUM_P (a))
#define CMP_NFIX(a) (!CMP_FIX (a))
#define CMP_IFIX(a) (((a) & SIGN_MASK) == (MAKE_OBJECT (TC_FIXNUM, 0)))
#define CMP_NIFIX(a) (!CMP_IFIX (a))

DEFINE_CJF (fix, FIX)
DEFINE_CJF (nfix, NFIX)
DEFINE_CJF (ifix, IFIX)
DEFINE_CJF (nifix, NIFIX)

DEFINE_INST (trap_trap_0)
{
  DECODE_SVM1_INST_TRAP_TRAP_0 (code);
  return ((* (traps_0[code])) (PC));
}

static byte_t *
illegal_trap_0 (byte_t * PC)
{
  ILL;
}

DEFINE_INST (trap_trap_1_wr)
{
  DECODE_SVM1_INST_TRAP_TRAP_1_WR (code, r1);
  return ((* (traps_1[code])) (PC, r1));
}

static byte_t *
illegal_trap_1 (byte_t * PC, wreg_t r1)
{
  ILL;
}

DEFINE_INST (trap_trap_2_wr)
{
  DECODE_SVM1_INST_TRAP_TRAP_2_WR (code, r1, r2);
  return ((* (traps_2[code])) (PC, r1, r2));
}

static byte_t *
illegal_trap_2 (byte_t * PC, wreg_t r1, wreg_t r2)
{
  ILL;
}

DEFINE_INST (trap_trap_3_wr)
{
  DECODE_SVM1_INST_TRAP_TRAP_3_WR (code, r1, r2, r3);
  return ((* (traps_3[code])) (PC, r1, r2, r3));
}

static byte_t *
illegal_trap_3 (byte_t * PC, wreg_t r1, wreg_t r2, wreg_t r3)
{
  ILL;
}

#define TRAP_PREFIX(result)						\
  utility_result_t result;						\
  EXPORT_REGS ()

#define TRAP_SUFFIX(result)						\
  IMPORT_REGS ();							\
  if ((result).scheme_p)						\
    {									\
      NEW_PC ((result).arg.new_pc);					\
    }									\
  else									\
    EXIT_VM ((result).arg.interpreter_code)

#define DEFINE_TRAP_0(nl, util_name)					\
byte_t *								\
trap_##nl (byte_t * PC)							\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       0,						\
		       0,						\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_1(nl, util_name)					\
byte_t *								\
trap_##nl (byte_t * PC, wreg_t source1)					\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (WREG_REF (source1))),			\
		       0,						\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_2(nl, util_name)					\
byte_t *								\
trap_##nl (byte_t * PC, wreg_t source1, wreg_t source2)			\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (WREG_REF (source1))),			\
		       ((long) (WREG_REF (source2))),			\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_3(nl, util_name)					\
byte_t *								\
trap_##nl (byte_t * PC, wreg_t source1, wreg_t source2, wreg_t source3)	\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (WREG_REF (source1))),			\
		       ((long) (WREG_REF (source2))),			\
		       ((long) (WREG_REF (source3))),			\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_R0(nl, util_name)					\
byte_t *								\
trap_##nl (byte_t * PC)							\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (PC + CC_ENTRY_HEADER_SIZE)),		\
		       0,						\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_R1(nl, util_name)					\
byte_t *								\
trap_##nl (byte_t * PC, wreg_t source1)					\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (PC + CC_ENTRY_HEADER_SIZE)),		\
		       ((long) (WREG_REF (source1))),			\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_R2(nl, util_name)					\
byte_t *								\
trap_##nl (byte_t * PC, wreg_t source1, wreg_t source2)			\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (PC + CC_ENTRY_HEADER_SIZE)),		\
		       ((long) (WREG_REF (source1))),			\
		       ((long) (WREG_REF (source2))),			\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_R3(nl, util_name)					\
byte_t *								\
trap_##nl (byte_t * PC, wreg_t source1, wreg_t source2, wreg_t source3)	\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (PC + CC_ENTRY_HEADER_SIZE)),		\
		       ((long) (WREG_REF (source1))),			\
		       ((long) (WREG_REF (source2))),			\
		       ((long) (WREG_REF (source3))));			\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAMPOLINE(nl, util_name)				\
byte_t *								\
trap_##nl (byte_t * PC)							\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name							\
    ((&result),								\
     ((long)								\
      (trampoline_storage						\
       (cc_entry_address_to_block_address (PC - 2)))),			\
     0,									\
     0,									\
     0);								\
  TRAP_SUFFIX (result);							\
}

DEFINE_TRAP_0 (add, plus)
DEFINE_TRAP_0 (decrement, decrement)
DEFINE_TRAP_0 (divide, divide)
DEFINE_TRAP_0 (equal_p, equal)
DEFINE_TRAP_0 (greater_p, greater)
DEFINE_TRAP_0 (increment, increment)
DEFINE_TRAP_0 (less_p, less)
DEFINE_TRAP_0 (modulo, modulo)
DEFINE_TRAP_0 (multiply, multiply)
DEFINE_TRAP_0 (negative_p, negative)
DEFINE_TRAP_0 (positive_p, positive)
DEFINE_TRAP_0 (quotient, quotient)
DEFINE_TRAP_0 (remainder, remainder)
DEFINE_TRAP_0 (subtract, minus)
DEFINE_TRAP_0 (zero_p, zero)

DEFINE_TRAP_1 (error, error)
DEFINE_TRAP_1 (primitive_apply, primitive_apply)
DEFINE_TRAP_1 (primitive_lexpr_apply, primitive_lexpr_apply)

DEFINE_TRAP_R1 (lookup, lookup_trap)
DEFINE_TRAP_R1 (safe_lookup, safe_lookup_trap)
DEFINE_TRAP_R1 (unassigned_p, unassigned_p_trap)

DEFINE_TRAP_2 (apply, apply)
DEFINE_TRAP_2 (lexpr_apply, lexpr_apply)

DEFINE_TRAP_R2 (assignment, assignment_trap)
DEFINE_TRAP_R2 (primitive_error, primitive_error)

DEFINE_TRAP_3 (cache_reference_apply, cache_lookup_apply)

DEFINE_TRAP_R3 (link, link)

DEFINE_TRAMPOLINE (operator_1_0, operator_1_0_trap)
DEFINE_TRAMPOLINE (operator_2_0, operator_2_0_trap)
DEFINE_TRAMPOLINE (operator_2_1, operator_2_1_trap)
DEFINE_TRAMPOLINE (operator_3_0, operator_3_0_trap)
DEFINE_TRAMPOLINE (operator_3_1, operator_3_1_trap)
DEFINE_TRAMPOLINE (operator_3_2, operator_3_2_trap)
DEFINE_TRAMPOLINE (operator_4_0, operator_4_0_trap)
DEFINE_TRAMPOLINE (operator_4_1, operator_4_1_trap)
DEFINE_TRAMPOLINE (operator_4_2, operator_4_2_trap)
DEFINE_TRAMPOLINE (operator_4_3, operator_4_3_trap)
DEFINE_TRAMPOLINE (operator_apply, operator_apply_trap)
DEFINE_TRAMPOLINE (operator_lexpr, operator_lexpr_trap)
DEFINE_TRAMPOLINE (operator_lookup, operator_lookup_trap)
DEFINE_TRAMPOLINE (operator_primitive, operator_primitive_trap)
DEFINE_TRAMPOLINE (reflect_to_interface, reflect_to_interface)
DEFINE_TRAMPOLINE (return_to_interpreter, return_to_interpreter)

#define INTERRUPT_TEST							\
    (((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_FREE_POINTER)))		\
       >= GET_MEMTOP)							\
      || (((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_STACK_POINTER)))	\
	  < GET_STACK_GUARD)

#define DEFINE_INTERRUPT_TEST(name, a1, a2)				\
DEFINE_INST (interrupt_test_##name)					\
{									\
  if (predict_false (INTERRUPT_TEST))					\
    {									\
      TRAP_PREFIX(result);						\
      compiler_interrupt_common ((&result), (a1), (a2));		\
      TRAP_SUFFIX (result);						\
    }									\
  NEXT_PC;								\
}

DEFINE_INTERRUPT_TEST (procedure, (PC - 1), SHARP_F)
DEFINE_INTERRUPT_TEST (ic_procedure, (PC - 1), GET_ENV)
DEFINE_INTERRUPT_TEST (continuation, 0, GET_VAL)

DEFINE_INTERRUPT_TEST (dynamic_link,
		       (PC - 1),
		       (MAKE_CC_STACK_ENV (WREG_REF (SVM1_REG_DYNAMIC_LINK))))

DEFINE_INST (enter_closure)
{
  DECODE_SVM1_INST_ENTER_CLOSURE (index);

  if (predict_false (INTERRUPT_TEST))
    {
      TRAP_PREFIX(result);
      compiler_interrupt_common ((&result), PC - 3, SHARP_F);
      TRAP_SUFFIX (result);
    }

  {
    byte_t * block = (PC - (CLOSURE_ENTRY_START
			    + ((index + 1) * CLOSURE_ENTRY_SIZE)));
    unsigned int count = (read_u16 (block));
    SCHEME_OBJECT * targets
      = (skip_compiled_closure_padding
	 (block + (CLOSURE_ENTRY_START + (count * CLOSURE_ENTRY_SIZE))));
    push_object (MAKE_CC_ENTRY (((SCHEME_OBJECT *)
				 (block + CLOSURE_ENTRY_OFFSET))));
    NEW_PC (BYTE_ADDR (OBJECT_ADDRESS (targets[index])));
  }
}

DEFINE_INST (flonum_header_u8)
{
  DECODE_SVM1_INST_FLONUM_HEADER_U8 (target, value);
  WREG_SET (target,
	    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (FLONUM_SIZE * value))));
  NEXT_PC;
}

DEFINE_INST (flonum_header_u16)
{
  DECODE_SVM1_INST_FLONUM_HEADER_U16 (target, value);
  WREG_SET (target,
	    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (FLONUM_SIZE * value))));
  NEXT_PC;
}

DEFINE_INST (flonum_header_u32)
{
  DECODE_SVM1_INST_FLONUM_HEADER_U32 (target, value);
  WREG_SET (target,
	    (X_MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (FLONUM_SIZE * value))));
  NEXT_PC;
}

DEFINE_INST (flonum_header)
{
  DECODE_SVM1_INST_FLONUM_HEADER (target, source);
  WREG_SET (target,
	    (X_MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,
			    (FLONUM_SIZE * (WREG_REF (source))))));
  NEXT_PC;
}

DEFINE_INST (flonum_align)
{
  DECODE_SVM1_INST_FLONUM_ALIGN (target, source);
  SCHEME_OBJECT * p = ((SCHEME_OBJECT *) (WREG_REF (source)));
  ALIGN_FLOAT (p);
  WREG_SET (target, ((word_t) p));
  NEXT_PC;
}

#define UNARY_NOP(x) x
#define SIGNED_NEGATE(x) (SIGNED_UNARY (-, (x)))
#define WINCR(x) ((x) + 1)
#define FINCR(x) ((x) + 1.0)
#define WDECR(x) ((x) - 1)
#define FDECR(x) ((x) - 1.0)
#define WABS(x) (SIGNED_UNARY (labs, (x)))

#define FOP_ADD(x, y) ((x) + (y))
#define FOP_SUBTRACT(x, y) ((x) - (y))
#define FOP_MULTIPLY(x, y) ((x) * (y))
#define FOP_DIVIDE(x, y) ((x) / (y))

#define OP_ADD(x, y) (SIGNED_BINARY (+, (x), (y)))
#define OP_SUBTRACT(x, y) (SIGNED_BINARY (-, (x), (y)))
#define OP_MULTIPLY(x, y) (SIGNED_BINARY (*, (x), (y)))
#define OP_DIVIDE(x, y) (SIGNED_BINFUNC (FIXNUM_QUOTIENT, (x), (y)))
#define OP_REMAINDER(x, y) (SIGNED_BINFUNC (FIXNUM_REMAINDER, (x), (y)))
#define OP_AND(x, y) ((x) & (y))
#define OP_ANDC(x, y) ((x) &~ (y))
#define OP_OR(x, y) ((x) | (y))
#define OP_XOR(x, y) ((x) ^ (y))

#define DEFINE_UNARY_WR(nl, nu, op)					\
DEFINE_INST (nl)							\
{									\
  DECODE_SVM1_INST_##nu (target, source);				\
  WREG_SET (target, (op (WREG_REF (source))));				\
  NEXT_PC;								\
}

DEFINE_UNARY_WR (copy_wr, COPY_WR, UNARY_NOP)
DEFINE_UNARY_WR (negate_wr, NEGATE_WR, SIGNED_NEGATE)
DEFINE_UNARY_WR (increment_wr, INCREMENT_WR, WINCR)
DEFINE_UNARY_WR (decrement_wr, DECREMENT_WR, WDECR)
DEFINE_UNARY_WR (abs_wr, ABS_WR, WABS)
DEFINE_UNARY_WR (not, NOT, ~)

DEFINE_UNARY_WR (object_type, OBJECT_TYPE, OBJECT_TYPE)
DEFINE_UNARY_WR (object_datum, OBJECT_DATUM, OBJECT_DATUM)
DEFINE_UNARY_WR (object_address, OBJECT_ADDRESS, X_OBJECT_ADDRESS)
DEFINE_UNARY_WR (fixnum_to_integer, FIXNUM_TO_INTEGER, FIXNUM_TO_LONG)
DEFINE_UNARY_WR (integer_to_fixnum, INTEGER_TO_FIXNUM, LONG_TO_FIXNUM)
DEFINE_UNARY_WR (flonum_length, FLONUM_LENGTH, FLOATING_VECTOR_LENGTH)

#define DEFINE_UNARY_FR(nl, nu, op)					\
DEFINE_INST (nl)							\
{									\
  DECODE_SVM1_INST_##nu (target, source);				\
  FREG_SET (target, (op (FREG_REF (source))));				\
  NEXT_PC;								\
}

DEFINE_UNARY_FR (copy_fr, COPY_FR, UNARY_NOP)
DEFINE_UNARY_FR (negate_fr, NEGATE_FR, -)
DEFINE_UNARY_FR (increment_fr, INCREMENT_FR, FINCR)
DEFINE_UNARY_FR (decrement_fr, DECREMENT_FR, FDECR)
DEFINE_UNARY_FR (abs_fr, ABS_FR, fabs)
DEFINE_UNARY_FR (sqrt, SQRT, sqrt)
DEFINE_UNARY_FR (round, ROUND, double_round)
DEFINE_UNARY_FR (ceiling, CEILING, ceil)
DEFINE_UNARY_FR (floor, FLOOR, floor)
DEFINE_UNARY_FR (truncate, TRUNCATE, double_truncate)
DEFINE_UNARY_FR (log, LOG, log)
DEFINE_UNARY_FR (exp, EXP, exp)
DEFINE_UNARY_FR (cos, COS, cos)
DEFINE_UNARY_FR (sin, SIN, sin)
DEFINE_UNARY_FR (tan, TAN, tan)
DEFINE_UNARY_FR (acos, ACOS, acos)
DEFINE_UNARY_FR (asin, ASIN, asin)
DEFINE_UNARY_FR (atan, ATAN, atan)

#define DEFINE_BINARY_WR(nl, nu, op)					\
DEFINE_INST (nl)							\
{									\
  DECODE_SVM1_INST_##nu (target, source1, source2);			\
  WREG_SET (target, (op ((WREG_REF (source1)), (WREG_REF (source2)))));	\
  NEXT_PC;								\
}

static word_t
multiply_with_overflow (long x, long y)
{
  word_t ans = (Mul ((LONG_TO_FIXNUM (x)), (LONG_TO_FIXNUM (y))));
  return (ans == SHARP_F
	  ? SHARP_T   /* This need only be !NFIX for overflow-test. */
	  : FIXNUM_TO_LONG (ans));
}

DEFINE_BINARY_WR (add_wr, ADD_WR, OP_ADD)
DEFINE_BINARY_WR (subtract_wr, SUBTRACT_WR, OP_SUBTRACT)
DEFINE_BINARY_WR (multiply_wr, MULTIPLY_WR, OP_MULTIPLY)
DEFINE_BINARY_WR (product, PRODUCT, multiply_with_overflow)
DEFINE_BINARY_WR (quotient, QUOTIENT, OP_DIVIDE)
DEFINE_BINARY_WR (remainder, REMAINDER, OP_REMAINDER)
DEFINE_BINARY_WR (and, AND, OP_AND)
DEFINE_BINARY_WR (andc, ANDC, OP_ANDC)
DEFINE_BINARY_WR (or, OR, OP_OR)
DEFINE_BINARY_WR (xor, XOR, OP_XOR)

DEFINE_INST (lsh)
{
  DECODE_SVM1_INST_LSH (target, source1, source2);
  long n = (TO_SIGNED (WREG_REF (source2)));
  WREG_SET (target, FIXNUM_LSH((WREG_REF (source1)), n));
  NEXT_PC;
}

DEFINE_INST (max_unsigned)
{
  DECODE_SVM1_INST_MAX_UNSIGNED (target, source1, source2);
  word_t n1 = (WREG_REF (source1));
  word_t n2 = (WREG_REF (source2));
  WREG_SET (target, ((n1 > n2) ? n1 : n2));
  NEXT_PC;
}

DEFINE_INST (min_unsigned)
{
  DECODE_SVM1_INST_MIN_UNSIGNED (target, source1, source2);
  word_t n1 = (WREG_REF (source1));
  word_t n2 = (WREG_REF (source2));
  WREG_SET (target, ((n1 < n2) ? n1 : n2));
  NEXT_PC;
}

#define DEFINE_BINARY_FR(nl, nu, op)					\
DEFINE_INST (nl)							\
{									\
  DECODE_SVM1_INST_##nu (target, source1, source2);			\
  FREG_SET (target, (op ((FREG_REF (source1)), (FREG_REF (source2)))));	\
  NEXT_PC;								\
}

DEFINE_BINARY_FR (add_fr, ADD_FR, FOP_ADD)
DEFINE_BINARY_FR (subtract_fr, SUBTRACT_FR, FOP_SUBTRACT)
DEFINE_BINARY_FR (multiply_fr, MULTIPLY_FR, FOP_MULTIPLY)
DEFINE_BINARY_FR (divide, DIVIDE, FOP_DIVIDE)
DEFINE_BINARY_FR (atan2, ATAN2, atan2)

/* Address decoders */

static inline byte_t *
decode_address (byte_t * PC, word_t * address)
{
  address_decoder_t * decoder = address_decoders[NEXT_BYTE];
  return ((*decoder) (PC, address));
}

static byte_t *
illegal_address (byte_t * PC, word_t * address)
{
  (void) PC;			/* ignore */
  (void) address;		/* ignore */
  return (0);
}

static word_t
offset_address_value (wreg_t base, word_t offset, unsigned int scale)
{
  return ((WREG_REF (base)) + (offset * scale));
}

#define MAKE_OFFSET_ADDRESS(base, offset, scale)			\
  ADDRESS_DECODED (offset_address_value ((base), (offset), (scale)))

DEFINE_ADDRESS_DECODER (indir)
{
  DECODE_SVM1_ADDR_INDIR (base);
  MAKE_OFFSET_ADDRESS (base, 0, 0);
}

DEFINE_ADDRESS_DECODER (offset_s8_b)
{
  DECODE_SVM1_ADDR_OFFSET_S8_B (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SBYTE);
}

DEFINE_ADDRESS_DECODER (offset_s8_w)
{
  DECODE_SVM1_ADDR_OFFSET_S8_W (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SWORD);
}

DEFINE_ADDRESS_DECODER (offset_s8_f)
{
  DECODE_SVM1_ADDR_OFFSET_S8_F (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SFLOAT);
}

DEFINE_ADDRESS_DECODER (offset_s16_b)
{
  DECODE_SVM1_ADDR_OFFSET_S16_B (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SBYTE);
}

DEFINE_ADDRESS_DECODER (offset_s16_w)
{
  DECODE_SVM1_ADDR_OFFSET_S16_W (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SWORD);
}

DEFINE_ADDRESS_DECODER (offset_s16_f)
{
  DECODE_SVM1_ADDR_OFFSET_S16_F (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SFLOAT);
}

DEFINE_ADDRESS_DECODER (offset_s32_b)
{
  DECODE_SVM1_ADDR_OFFSET_S32_B (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SBYTE);
}

DEFINE_ADDRESS_DECODER (offset_s32_w)
{
  DECODE_SVM1_ADDR_OFFSET_S32_W (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SWORD);
}

DEFINE_ADDRESS_DECODER (offset_s32_f)
{
  DECODE_SVM1_ADDR_OFFSET_S32_F (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SFLOAT);
}

static inline word_t
indexed_address_value (wreg_t base, word_t offset, unsigned int oscale,
		       wreg_t index, unsigned int iscale)
{
  return
    ((WREG_REF (base))
     + (offset * oscale)
     + ((WREG_REF (index)) * iscale));
}

#define MAKE_INDEXED_ADDRESS(base, offset, oscale, index, iscale)	\
  ADDRESS_DECODED							\
    (indexed_address_value ((base), (offset), (oscale), (index), (iscale)))

DEFINE_ADDRESS_DECODER (index_b_b)
{
  DECODE_SVM1_ADDR_INDEX_B_B (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SBYTE, index, SBYTE);
}

DEFINE_ADDRESS_DECODER (index_b_w)
{
  DECODE_SVM1_ADDR_INDEX_B_W (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SBYTE, index, SWORD);
}

DEFINE_ADDRESS_DECODER (index_b_f)
{
  DECODE_SVM1_ADDR_INDEX_B_F (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SBYTE, index, SFLOAT);
}

DEFINE_ADDRESS_DECODER (index_w_b)
{
  DECODE_SVM1_ADDR_INDEX_W_B (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SWORD, index, SBYTE);
}

DEFINE_ADDRESS_DECODER (index_w_w)
{
  DECODE_SVM1_ADDR_INDEX_W_W (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SWORD, index, SWORD);
}

DEFINE_ADDRESS_DECODER (index_w_f)
{
  DECODE_SVM1_ADDR_INDEX_W_F (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SWORD, index, SFLOAT);
}

DEFINE_ADDRESS_DECODER (index_f_b)
{
  DECODE_SVM1_ADDR_INDEX_F_B (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SFLOAT, index, SBYTE);
}

DEFINE_ADDRESS_DECODER (index_f_w)
{
  DECODE_SVM1_ADDR_INDEX_F_W (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SFLOAT, index, SWORD);
}

DEFINE_ADDRESS_DECODER (index_f_f)
{
  DECODE_SVM1_ADDR_INDEX_F_F (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SFLOAT, index, SFLOAT);
}

static inline word_t
preinc_address_value (wreg_t reg, signed int incr)
{
  WREG_SET (reg, ((WREG_REF (reg)) + incr));
  return (WREG_REF (reg));
}

#define MAKE_PREINC_ADDRESS(base, scale)				\
  ADDRESS_DECODED (preinc_address_value ((base), (scale)))

DEFINE_ADDRESS_DECODER (predec_b)
{
  DECODE_SVM1_ADDR_PREDEC_B (base);
  MAKE_PREINC_ADDRESS (base, (- ((signed) SBYTE)));
}

DEFINE_ADDRESS_DECODER (predec_w)
{
  DECODE_SVM1_ADDR_PREDEC_W (base);
  MAKE_PREINC_ADDRESS (base, (- ((signed) SWORD)));
}

DEFINE_ADDRESS_DECODER (predec_f)
{
  DECODE_SVM1_ADDR_PREDEC_F (base);
  MAKE_PREINC_ADDRESS (base, (- ((signed) SFLOAT)));
}

DEFINE_ADDRESS_DECODER (preinc_b)
{
  DECODE_SVM1_ADDR_PREINC_B (base);
  MAKE_PREINC_ADDRESS (base, SBYTE);
}

DEFINE_ADDRESS_DECODER (preinc_w)
{
  DECODE_SVM1_ADDR_PREINC_W (base);
  MAKE_PREINC_ADDRESS (base, SWORD);
}

DEFINE_ADDRESS_DECODER (preinc_f)
{
  DECODE_SVM1_ADDR_PREINC_F (base);
  MAKE_PREINC_ADDRESS (base, SFLOAT);
}

static inline word_t
postinc_address_value (wreg_t reg, signed int incr)
{
  word_t value = (WREG_REF (reg));
  WREG_SET (reg, ((WREG_REF (reg)) + incr));
  return (value);
}

#define MAKE_POSTINC_ADDRESS(base, scale)				\
  ADDRESS_DECODED (postinc_address_value ((base), (scale)))

DEFINE_ADDRESS_DECODER (postdec_b)
{
  DECODE_SVM1_ADDR_POSTDEC_B (base);
  MAKE_POSTINC_ADDRESS (base, (- ((signed) SBYTE)));
}

DEFINE_ADDRESS_DECODER (postdec_w)
{
  DECODE_SVM1_ADDR_POSTDEC_W (base);
  MAKE_POSTINC_ADDRESS (base, (- ((signed) SWORD)));
}

DEFINE_ADDRESS_DECODER (postdec_f)
{
  DECODE_SVM1_ADDR_POSTDEC_F (base);
  MAKE_POSTINC_ADDRESS (base, (- ((signed) SFLOAT)));
}

DEFINE_ADDRESS_DECODER (postinc_b)
{
  DECODE_SVM1_ADDR_POSTINC_B (base);
  MAKE_POSTINC_ADDRESS (base, SBYTE);
}

DEFINE_ADDRESS_DECODER (postinc_w)
{
  DECODE_SVM1_ADDR_POSTINC_W (base);
  MAKE_POSTINC_ADDRESS (base, SWORD);
}

DEFINE_ADDRESS_DECODER (postinc_f)
{
  DECODE_SVM1_ADDR_POSTINC_F (base);
  MAKE_POSTINC_ADDRESS (base, SFLOAT);
}

static inline word_t
pcr_value (byte_t * PC, word_t offset)
{
  return (((word_t) PC) + offset);
}

#define MAKE_PCR_ADDRESS(offset)					\
  ADDRESS_DECODED (pcr_value (PC, offset))

DEFINE_ADDRESS_DECODER (pcr_s8)
{
  DECODE_SVM1_ADDR_PCR_S8 (offset);
  MAKE_PCR_ADDRESS (offset);
}

DEFINE_ADDRESS_DECODER (pcr_s16)
{
  DECODE_SVM1_ADDR_PCR_S16 (offset);
  MAKE_PCR_ADDRESS (offset);
}

DEFINE_ADDRESS_DECODER (pcr_s32)
{
  DECODE_SVM1_ADDR_PCR_S32 (offset);
  MAKE_PCR_ADDRESS (offset);
}

#define INITIALIZE_DECODER_TABLE(table, initial_value) do		\
{									\
  unsigned int i;							\
  for (i = 0; (i < 256); i += 1)					\
    (table[i]) = initial_value;						\
} while (false)

static void
initialize_decoder_tables (void)
{
  INITIALIZE_DECODER_TABLE (address_decoders, illegal_address);
#define BIND_ADDR(code, name) (address_decoders[code]) = decode_addr_##name
  SVM1_ADDR_BINDINGS (BIND_ADDR);

  INITIALIZE_DECODER_TABLE (inst_defns, illegal_instruction);
#define BIND_INST(code, name) (inst_defns[code]) = insn_##name
  SVM1_INST_BINDINGS (BIND_INST);

  INITIALIZE_DECODER_TABLE (traps_0, illegal_trap_0);
#define BIND_TRAP_0(code, name) (traps_0[code]) = trap_##name
  SVM1_TRAP_0_BINDINGS (BIND_TRAP_0);

  INITIALIZE_DECODER_TABLE (traps_1, illegal_trap_1);
#define BIND_TRAP_1(code, name) (traps_1[code]) = trap_##name
  SVM1_TRAP_1_BINDINGS (BIND_TRAP_1);

  INITIALIZE_DECODER_TABLE (traps_2, illegal_trap_2);
#define BIND_TRAP_2(code, name) (traps_2[code]) = trap_##name
  SVM1_TRAP_2_BINDINGS (BIND_TRAP_2);

  INITIALIZE_DECODER_TABLE (traps_3, illegal_trap_3);
#define BIND_TRAP_3(code, name) (traps_3[code]) = trap_##name
  SVM1_TRAP_3_BINDINGS (BIND_TRAP_3);
}
