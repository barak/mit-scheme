/* -*-C-*-

$Id: 2fa0609ea3d778689afc7903f19208b20185b210 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

#include "config.h"

#include <stdlib.h>
#include <string.h>

#define LIARC_IN_MICROCODE
#include "liarc.h"
#include "stackops.h"

typedef struct
{
  unsigned char * strptr;
  entry_count_t dispatch_base;
  SCHEME_OBJECT * sp;
  SCHEME_OBJECT * regmap;
} stackify_context_s, * stackify_context_t;

static entry_count_t dispatch_base;

static unsigned char * prog_start;
static unsigned char * prog_end;
static unsigned char * pc;

static unsigned char * string_start;
static unsigned char * string_end;
static unsigned char * strptr;

static SCHEME_OBJECT * sp_lower;
static SCHEME_OBJECT * sp_upper;
static SCHEME_OBJECT * sp;

static SCHEME_OBJECT * regmap;
static SCHEME_OBJECT * regmap_end;

#ifdef ENABLE_DEBUGGING_TOOLS

#undef NDEBUG
#include <assert.h>

static const char * opcode_names [] =
{
  "stackify-opcode/illegal",
  "stackify-opcode/escape",
  "stackify-opcode/push-+fixnum",
  "stackify-opcode/push--fixnum",
  "stackify-opcode/push-+integer",
  "stackify-opcode/push--integer",
  "stackify-opcode/push-false",
  "stackify-opcode/push-true",
  "stackify-opcode/push-nil",
  "stackify-opcode/push-flonum",
  "stackify-opcode/push-cons-ratnum",
  "stackify-opcode/push-cons-recnum",
  "stackify-opcode/push-string",
  "stackify-opcode/push-symbol",
  "stackify-opcode/push-uninterned-symbol",
  "stackify-opcode/push-char",
  "stackify-opcode/push-bit-string",
  "stackify-opcode/push-empty-cons",
  "stackify-opcode/pop-and-set-car",
  "stackify-opcode/pop-and-set-cdr",
  "stackify-opcode/push-cons*",
  "stackify-opcode/push-empty-vector",
  "stackify-opcode/pop-and-vector-set",
  "stackify-opcode/push-vector",
  "stackify-opcode/push-empty-record",
  "stackify-opcode/pop-and-record-set",
  "stackify-opcode/push-record",
  "stackify-opcode/push-lookup",
  "stackify-opcode/store",
  "stackify-opcode/push-constant",
  "stackify-opcode/push-unassigned",
  "stackify-opcode/push-primitive",
  "stackify-opcode/push-primitive-lexpr",
  "stackify-opcode/push-nm-header",
  "stackify-opcode/push-label-entry",
  "stackify-opcode/push-linkage-header-operator",
  "stackify-opcode/push-linkage-header-reference",
  "stackify-opcode/push-linkage-header-assignment",
  "stackify-opcode/push-linkage-header-global",
  "stackify-opcode/push-linkage-header-closure",
  "stackify-opcode/push-ulong",
  "stackify-opcode/push-label-descriptor",
  "stackify-opcode/cc-block-to-entry",
  "stackify-opcode/retag-cc-block",
  "stackify-opcode/push-return-code",
  "unknown-055",
  "unknown-056",
  "unknown-057",
  "unknown-060",
  "unknown-061",
  "unknown-062",
  "unknown-063",
  "unknown-064",
  "unknown-065",
  "unknown-066",
  "unknown-067",
  "unknown-070",
  "unknown-071",
  "unknown-072",
  "unknown-073",
  "unknown-074",
  "unknown-075",
  "unknown-076",
  "unknown-077",
  "unknown-0100",
  "unknown-0101",
  "unknown-0102",
  "unknown-0103",
  "unknown-0104",
  "unknown-0105",
  "unknown-0106",
  "unknown-0107",
  "unknown-0110",
  "unknown-0111",
  "unknown-0112",
  "unknown-0113",
  "unknown-0114",
  "unknown-0115",
  "unknown-0116",
  "unknown-0117",
  "unknown-0120",
  "unknown-0121",
  "unknown-0122",
  "unknown-0123",
  "unknown-0124",
  "unknown-0125",
  "unknown-0126",
  "unknown-0127",
  "unknown-0130",
  "unknown-0131",
  "unknown-0132",
  "unknown-0133",
  "unknown-0134",
  "unknown-0135",
  "unknown-0136",
  "unknown-0137",
  "unknown-0140",
  "unknown-0141",
  "unknown-0142",
  "unknown-0143",
  "unknown-0144",
  "unknown-0145",
  "unknown-0146",
  "unknown-0147",
  "unknown-0150",
  "unknown-0151",
  "unknown-0152",
  "unknown-0153",
  "unknown-0154",
  "unknown-0155",
  "unknown-0156",
  "unknown-0157",
  "unknown-0160",
  "unknown-0161",
  "unknown-0162",
  "unknown-0163",
  "unknown-0164",
  "unknown-0165",
  "unknown-0166",
  "unknown-0167",
  "unknown-0170",
  "unknown-0171",
  "unknown-0172",
  "unknown-0173",
  "unknown-0174",
  "unknown-0175",
  "unknown-0176",
  "unknown-0177",
  "stackify-opcode/push-0",
  "stackify-opcode/push-1",
  "stackify-opcode/push-2",
  "stackify-opcode/push-3",
  "stackify-opcode/push-4",
  "stackify-opcode/push-5",
  "stackify-opcode/push-6",
  "stackify-opcode/push--1",
  "stackify-opcode/push-cons*-0",
  "stackify-opcode/push-cons*-1",
  "stackify-opcode/push-cons*-2",
  "stackify-opcode/push-cons*-3",
  "stackify-opcode/push-cons*-4",
  "stackify-opcode/push-cons*-5",
  "stackify-opcode/push-cons*-6",
  "stackify-opcode/push-cons*-7",
  "stackify-opcode/pop-and-vector-set-0",
  "stackify-opcode/pop-and-vector-set-1",
  "stackify-opcode/pop-and-vector-set-2",
  "stackify-opcode/pop-and-vector-set-3",
  "stackify-opcode/pop-and-vector-set-4",
  "stackify-opcode/pop-and-vector-set-5",
  "stackify-opcode/pop-and-vector-set-6",
  "stackify-opcode/pop-and-vector-set-7",
  "stackify-opcode/push-vector-1",
  "stackify-opcode/push-vector-2",
  "stackify-opcode/push-vector-3",
  "stackify-opcode/push-vector-4",
  "stackify-opcode/push-vector-5",
  "stackify-opcode/push-vector-6",
  "stackify-opcode/push-vector-7",
  "stackify-opcode/push-vector-8",
  "stackify-opcode/pop-and-record-set-0",
  "stackify-opcode/pop-and-record-set-1",
  "stackify-opcode/pop-and-record-set-2",
  "stackify-opcode/pop-and-record-set-3",
  "stackify-opcode/pop-and-record-set-4",
  "stackify-opcode/pop-and-record-set-5",
  "stackify-opcode/pop-and-record-set-6",
  "stackify-opcode/pop-and-record-set-7",
  "stackify-opcode/push-record-1",
  "stackify-opcode/push-record-2",
  "stackify-opcode/push-record-3",
  "stackify-opcode/push-record-4",
  "stackify-opcode/push-record-5",
  "stackify-opcode/push-record-6",
  "stackify-opcode/push-record-7",
  "stackify-opcode/push-record-8",
  "stackify-opcode/push-lookup-0",
  "stackify-opcode/push-lookup-1",
  "stackify-opcode/push-lookup-2",
  "stackify-opcode/push-lookup-3",
  "stackify-opcode/push-lookup-4",
  "stackify-opcode/push-lookup-5",
  "stackify-opcode/push-lookup-6",
  "stackify-opcode/push-lookup-7",
  "stackify-opcode/store-0",
  "stackify-opcode/store-1",
  "stackify-opcode/store-2",
  "stackify-opcode/store-3",
  "stackify-opcode/store-4",
  "stackify-opcode/store-5",
  "stackify-opcode/store-6",
  "stackify-opcode/store-7",
  "stackify-opcode/push-primitive-0",
  "stackify-opcode/push-primitive-1",
  "stackify-opcode/push-primitive-2",
  "stackify-opcode/push-primitive-3",
  "stackify-opcode/push-primitive-4",
  "stackify-opcode/push-primitive-5",
  "stackify-opcode/push-primitive-6",
  "stackify-opcode/push-primitive-7",
  "unknown-0310",
  "unknown-0311",
  "unknown-0312",
  "unknown-0313",
  "unknown-0314",
  "unknown-0315",
  "unknown-0316",
  "unknown-0317",
  "unknown-0320",
  "unknown-0321",
  "unknown-0322",
  "unknown-0323",
  "unknown-0324",
  "unknown-0325",
  "unknown-0326",
  "unknown-0327",
  "unknown-0330",
  "unknown-0331",
  "unknown-0332",
  "unknown-0333",
  "unknown-0334",
  "unknown-0335",
  "unknown-0336",
  "unknown-0337",
  "unknown-0340",
  "unknown-0341",
  "unknown-0342",
  "unknown-0343",
  "unknown-0344",
  "unknown-0345",
  "unknown-0346",
  "unknown-0347",
  "unknown-0350",
  "unknown-0351",
  "unknown-0352",
  "unknown-0353",
  "unknown-0354",
  "unknown-0355",
  "unknown-0356",
  "unknown-0357",
  "unknown-0360",
  "unknown-0361",
  "unknown-0362",
  "unknown-0363",
  "unknown-0364",
  "unknown-0365",
  "unknown-0366",
  "unknown-0367",
  "unknown-0370",
  "unknown-0371",
  "unknown-0372",
  "unknown-0373",
  "unknown-0374",
  "unknown-0375",
  "unknown-0376",
  "unknown-0377",
};

static bool debug_trace_p = 0;

static void
debug_trace (void)
{
  if (debug_trace_p)
    {
      if (pc == prog_start)
	fprintf (stderr,
		 "sp_lower = 0x%08x;"
		 " sp_upper = 0x%08x;"
		 " string_end = 0x%08x\n",
		 ((unsigned int) sp_lower),
		 ((unsigned int) sp_upper),
		 ((unsigned int) string_end));
      fprintf (stderr,
	       "(opcode %s stack-depth %d pc %d strtab-ptr %d)\n",
	       (opcode_names[*pc]),
	       (sp_upper - sp),
	       (pc - prog_start),
	       (strptr - string_start));
    }
}

#endif /* ENABLE_DEBUGGING_TOOLS */

static inline SCHEME_OBJECT
unstackify_pop (void)
{
  assert (sp < sp_upper);
  return (*sp++);
}

static inline SCHEME_OBJECT
unstackify_tos (void)
{
  assert (sp < sp_upper);
  return (*sp);
}

static inline void
unstackify_push (SCHEME_OBJECT object)
{
  assert (sp > sp_lower);
  (*--sp) = object;
}

/* Note: The encoded value is one greater than the actual value,
   so that the encoding of a ulong never uses a null character.
   Thus we subtract one after decoding.  */

static unsigned long
unstackify_read_ulong (void)
{
  unsigned int shift = 0;
  unsigned long value = 0;
#ifdef ENABLE_DEBUGGING_TOOLS
  unsigned long sentinel = 1;
#endif
  while (true)
    {
      unsigned char byte = (*strptr++);
      assert (strptr <= string_end);
      value |= ((byte & 0x7f) << shift);
      if ((byte & 0x80) == 0)
	break;
      assert ((sentinel <<= 7) != 0);
      shift += 7;
    }
  return (value - 1);
}

static char *
unstackify_read_string (unsigned long * plen)
{
  unsigned long len = (unstackify_read_ulong ());
  char * res = ((char *) strptr);
  assert ((strptr + len) <= string_end);
  strptr += len;
  (*plen) = len;
  return (res);
}

/* This returns a newly allocated string */

static char *
unstackify_read_C_string (void)
{
  unsigned long len;
  char * temp = (unstackify_read_string (&len));
  char * str = (malloc (len + 1));
  memcpy (str, temp, len);
  (str[len]) = '\0';
  return (str);
}

static void
unstackify_push_consS (unsigned long N)
{
  unsigned long i;
  SCHEME_OBJECT kar;
  SCHEME_OBJECT kdr = (unstackify_pop ());

  for (i = 0; (i <= N); i += 1)
    {
      kar = (unstackify_pop ());
      kdr = (CONS (kar, kdr));
    }

  unstackify_push (kdr);
}

static void
unstackify_pop_and_set_cXr (unsigned long N)
{
  SCHEME_OBJECT cXr = (unstackify_pop ());
  SCHEME_OBJECT pair = (unstackify_tos ());
  MEMORY_SET (pair, N, cXr);
}

static void
unstackify_push_empty_vector (unsigned long N)
{
  SCHEME_OBJECT res = (ALLOCATE_VECTOR (N));
  unstackify_push (res);
}

static void
unstackify_pop_and_vector_set (unsigned long N)
{
  SCHEME_OBJECT el = (unstackify_pop ());
  SCHEME_OBJECT vec = (unstackify_tos ());
  VECTOR_SET (vec, N, el);
}

static void
unstackify_push_vector (unsigned long N)
{
  SCHEME_OBJECT vec = (ALLOCATE_VECTOR (N));
  unsigned long i;

  for (i = 0; (i < N); i += 1)
    VECTOR_SET (vec, i, (unstackify_pop ()));

  unstackify_push (vec);
}

static void
unstackify_push_empty_record (unsigned long N)
{
  unstackify_push (ALLOCATE_RECORD (N));
}

static void
unstackify_pop_and_record_set (unsigned long N)
{
  SCHEME_OBJECT el = (unstackify_pop ());
  SCHEME_OBJECT rec = (unstackify_tos ());
  RECORD_SET (rec, N, el);
}

static void
unstackify_push_record (unsigned long N)
{
  SCHEME_OBJECT rec = (ALLOCATE_RECORD (N));
  unsigned long i;

  for (i = 0; (i < N); i++)
    RECORD_SET (rec, i, (unstackify_pop ()));

  unstackify_push (rec);
}

static inline void
unstackify_push_lookup (unsigned long N)
{
  assert ((regmap + N) < regmap_end);
  unstackify_push (regmap[N]);
}

static inline void
unstackify_store (unsigned long N)
{
  assert ((regmap + N) < regmap_end);
  (regmap[N]) = (unstackify_tos ());
}

static void
unstackify_push_primitive (long N)
{
  const char * prim_name = (unstackify_read_C_string ());
  SCHEME_OBJECT res = (MAKE_PRIMITIVE_PROCEDURE (prim_name, N));
  free ((void *) prim_name);
  unstackify_push (res);
}

static inline void
unstackify_undefined_opcode (stackify_opcode_t op)
{
  outf_fatal ("unstackify/undefined_opcode invoked.\n");
}

static void
stackify_push_ulong (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push ((SCHEME_OBJECT) N);
}

static void
stackify_push_Pfixnum (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push (ULONG_TO_FIXNUM (N));
}

static void
stackify_push__fixnum (stackify_opcode_t op)
{
  long val = (0 - ((long) (unstackify_read_ulong ())));
  unstackify_push (LONG_TO_FIXNUM (val));
}

static void
stackify_push_Pinteger (stackify_opcode_t op)
{
  unsigned long len;
  char * digits = (unstackify_read_string (&len));
  unstackify_push (DIGIT_STRING_TO_INTEGER (false, len, digits));
}

static void
stackify_push__integer (stackify_opcode_t op)
{
  unsigned long len;
  char * digits = (unstackify_read_string (&len));
  unstackify_push (DIGIT_STRING_TO_INTEGER (true, len, digits));
}

static inline void
stackify_push_false (stackify_opcode_t op)
{
  unstackify_push (SHARP_F);
}

static inline void
stackify_push_true (stackify_opcode_t op)
{
  unstackify_push (SHARP_T);
}

static inline void
stackify_push_nil (stackify_opcode_t op)
{
  unstackify_push (EMPTY_LIST);
}

static void
stackify_push_flonum (stackify_opcode_t op)
{
  char * str = (unstackify_read_C_string ());
  double val = (strtod (str, 0));
  free (str);
  unstackify_push (DOUBLE_TO_FLONUM (val));
}

static void
stackify_push_cons_ratnum (stackify_opcode_t op)
{
  SCHEME_OBJECT den = (unstackify_pop ());
  SCHEME_OBJECT num = (unstackify_pop ());
  unstackify_push (MAKE_RATIO (num, den));
}

static void
stackify_push_cons_recnum (stackify_opcode_t op)
{
  SCHEME_OBJECT imag = (unstackify_pop ());
  SCHEME_OBJECT real = (unstackify_pop ());
  unstackify_push (MAKE_COMPLEX (real, imag));
}

static void
stackify_push_string (stackify_opcode_t op)
{
  unsigned long len;
  char * str = (unstackify_read_string (&len));
  unstackify_push (C_STRING_TO_SCHEME_STRING (len, str));
}

static void
stackify_push_symbol (stackify_opcode_t op)
{
  unsigned long len;
  char * str = (unstackify_read_string (&len));
  unstackify_push (C_SYM_INTERN (len, str));
}

static void
stackify_push_uninterned_symbol (stackify_opcode_t op)
{
  unsigned long len;
  char * str = (unstackify_read_string (&len));
  unstackify_push (C_TO_UNINTERNED_SYMBOL (len, str));
}

static void
stackify_push_char (stackify_opcode_t op)
{
  unsigned long bits = (unstackify_read_ulong ());
  unsigned long code = (unstackify_read_ulong ());
  unstackify_push (MAKE_CHAR (bits, code));
}

static void
stackify_push_bit_string (stackify_opcode_t op)
{
  unsigned long n_bits = (unstackify_read_ulong ());
  unsigned long len;
  char * digits = (unstackify_read_string (&len));
  unstackify_push (DIGIT_STRING_TO_BIT_STRING (n_bits, len, digits));
}

static void
stackify_push_empty_cons (stackify_opcode_t op)
{
  unstackify_push (CONS (SHARP_F, SHARP_F));
}

static inline void
stackify_pop_and_set_car (stackify_opcode_t op)
{
  unstackify_pop_and_set_cXr (CONS_CAR);
}

static inline void
stackify_pop_and_set_cdr (stackify_opcode_t op)
{
  unstackify_pop_and_set_cXr (CONS_CDR);
}

static void
stackify_push_consS (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push_consS (N);
}

static void
stackify_push_empty_vector (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push_empty_vector (N);
}

static void
stackify_pop_and_vector_set (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_pop_and_vector_set (N);    
}

static void
stackify_push_vector (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push_vector (N);
}

static void
stackify_push_empty_record (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push_empty_record (N);
}

static void
stackify_pop_and_record_set (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_pop_and_record_set (N);
}

static void
stackify_push_record (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push_record (N);
}

static void
stackify_push_lookup (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push_lookup (N);
}

static void
stackify_store (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_store (N);
}

static void
stackify_push_constant (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push (MAKE_OBJECT (TC_CONSTANT, N));
}

static inline void
stackify_push_unassigned (stackify_opcode_t op)
{
  unstackify_push (UNASSIGNED_OBJECT);
}

static void
stackify_push_primitive (stackify_opcode_t op)
{
  unsigned long N = (unstackify_read_ulong ());
  unstackify_push_primitive ((long) N);
}

static inline void
stackify_push_primitive_lexpr (stackify_opcode_t op)
{
  unstackify_push_primitive (-1);
}

static void
stackify_push_N (stackify_opcode_t op)
{
  unstackify_push (ULONG_TO_FIXNUM (op - stackify_opcode_push_0));
}

static void
stackify_push__1 (stackify_opcode_t op)
{
  unstackify_push (LONG_TO_FIXNUM (-1));
}

static inline void
stackify_push_consS_N (stackify_opcode_t op)
{
  unstackify_push_consS (op - stackify_opcode_push_consS_0);
}

static inline void
stackify_pop_and_vector_set_N (stackify_opcode_t op)
{
  unstackify_pop_and_vector_set (op - stackify_opcode_pop_and_vector_set_0);
}

static inline void
stackify_push_vector_N (stackify_opcode_t op)
{
  unstackify_push_vector (1 + (op - stackify_opcode_push_vector_1));
}

static inline void
stackify_pop_and_record_set_N (stackify_opcode_t op)
{
  unstackify_pop_and_record_set (op - stackify_opcode_pop_and_record_set_0);
}

static inline void
stackify_push_record_N (stackify_opcode_t op)
{
  unstackify_push_record (1 + (op - stackify_opcode_push_record_1));
}

static inline void
stackify_push_lookup_N (stackify_opcode_t op)
{
  unstackify_push_lookup (op - stackify_opcode_push_lookup_0);
}

static inline void
stackify_store_N (stackify_opcode_t op)
{
  unstackify_store (op - stackify_opcode_store_0);
}

static inline void
stackify_push_primitive_N (stackify_opcode_t op)
{
  unstackify_push_primitive (op - stackify_opcode_push_primitive_0);
}

static void
stackify_push_nm_header (stackify_opcode_t op)
{
  unstackify_push
    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (unstackify_read_ulong ())));
}

static void
stackify_push_label_entry (stackify_opcode_t op)
{
  unstackify_push
    ((SCHEME_OBJECT)
     (((unsigned long) dispatch_base)
      + (unstackify_read_ulong ())));
}

static void
stackify_push_label_descriptor (stackify_opcode_t op)
{
  unsigned long offset = (unstackify_read_ulong ());
  unsigned long code_word = (unstackify_read_ulong ());
  unstackify_push (MAKE_LABEL_DESCRIPTOR (code_word, offset));
}

static void
stackify_retag_cc_block (stackify_opcode_t op)
{
  unstackify_push
    (OBJECT_NEW_TYPE (TC_COMPILED_CODE_BLOCK, (unstackify_pop ())));
}

static void
stackify_cc_block_to_entry (stackify_opcode_t op)
{
  unsigned long offset = (unstackify_read_ulong ());
  SCHEME_OBJECT block = (unstackify_pop ());
  unstackify_push (CC_BLOCK_TO_ENTRY (block, offset));
}

static void
stackify_push_return_code (stackify_opcode_t op)
{
  unstackify_push (MAKE_OBJECT (TC_RETURN_CODE, (unstackify_read_ulong ())));
}

static void
unstackify_push_linkage_header (linkage_section_type_t type)
{
  unstackify_push (MAKE_LINKER_HEADER (type, (unstackify_read_ulong ())));
}

static void
stackify_push_linkage_header_operator (stackify_opcode_t op)
{
  unstackify_push_linkage_header (LINKAGE_SECTION_TYPE_OPERATOR);
}

static void
stackify_push_linkage_header_reference (stackify_opcode_t op)
{
  unstackify_push_linkage_header (LINKAGE_SECTION_TYPE_REFERENCE);
}

static void
stackify_push_linkage_header_assignment (stackify_opcode_t op)
{
  unstackify_push_linkage_header (LINKAGE_SECTION_TYPE_ASSIGNMENT);
}

static void
stackify_push_linkage_header_global (stackify_opcode_t op)
{
  unstackify_push_linkage_header (LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR);
}

static void
stackify_push_linkage_header_closure (stackify_opcode_t op)
{
  outf_fatal ("stackify_push_linkage_header_closure.\n");
}

static void
unstackify_save_context (stackify_context_t context)
{
  (context->strptr) = strptr;
  (context->dispatch_base) = dispatch_base;
  (context->sp) = sp;
  (context->regmap) = regmap;
}

static void
unstackify_restore_context (stackify_context_t context)
{
  strptr = (context->strptr);
  dispatch_base = (context->dispatch_base);
  sp = (context->sp);
  regmap = (context->regmap);
}

SCHEME_OBJECT
unstackify (unsigned char * bytes, size_t n_bytes, entry_count_t db)
{
  unsigned char op;
  SCHEME_OBJECT result;
  SCHEME_OBJECT * scratch;
  unsigned long stack_length;
  unsigned long regmap_length;
  unsigned long prog_length;
  stackify_context_s context;
	
  unstackify_save_context (& context);

  /* Read the header */

  string_start = bytes;
  string_end = (bytes + n_bytes);
  strptr = string_start;

  stack_length = (unstackify_read_ulong ());
  regmap_length = (unstackify_read_ulong ());
  prog_length = (unstackify_read_ulong ());

  /* Set up for execution */

  prog_start = strptr;
  prog_end = (prog_start + prog_length);
  pc = prog_start;

  string_start = prog_end;
  strptr = string_start;
    
  scratch = (malloc ((stack_length + regmap_length) * SIZEOF_SCHEME_OBJECT));
  if (scratch == 0)
    return (SHARP_F);

  sp_lower = scratch;
  sp_upper = (sp_lower + stack_length);
  sp = sp_upper;

  regmap = sp_upper;
  regmap_end = (regmap + regmap_length);

  dispatch_base = db;

  /* Now, execute the program */

  while (pc < prog_end)
    {
#ifdef ENABLE_DEBUGGING_TOOLS
      debug_trace ();
#endif
      op = ((stackify_opcode_t) (*pc++));
      switch (op)
	{
	default:
	case stackify_opcode_illegal:
	case stackify_opcode_escape:
	  unstackify_undefined_opcode (op);
	  break;

	case stackify_opcode_push_Pfixnum:
	  stackify_push_Pfixnum (op);
	  break;

	case stackify_opcode_push__fixnum:
	  stackify_push__fixnum (op);
	  break;

	case stackify_opcode_push_Pinteger:
	  stackify_push_Pinteger (op);
	  break;

	case stackify_opcode_push__integer:
	  stackify_push__integer (op);
	  break;

	case stackify_opcode_push_false:
	  stackify_push_false (op);
	  break;

	case stackify_opcode_push_true:
	  stackify_push_true (op);
	  break;

	case stackify_opcode_push_nil:
	  stackify_push_nil (op);
	  break;

	case stackify_opcode_push_flonum:
	  stackify_push_flonum (op);
	  break;

	case stackify_opcode_push_cons_ratnum:
	  stackify_push_cons_ratnum (op);
	  break;

	case stackify_opcode_push_cons_recnum:
	  stackify_push_cons_recnum (op);
	  break;

	case stackify_opcode_push_string:
	  stackify_push_string (op);
	  break;

	case stackify_opcode_push_symbol:
	  stackify_push_symbol (op);
	  break;

	case stackify_opcode_push_uninterned_symbol:
	  stackify_push_uninterned_symbol (op);
	  break;

	case stackify_opcode_push_char:
	  stackify_push_char (op);
	  break;

	case stackify_opcode_push_bit_string:
	  stackify_push_bit_string (op);
	  break;

	case stackify_opcode_push_empty_cons:
	  stackify_push_empty_cons (op);
	  break;

	case stackify_opcode_pop_and_set_car:
	  stackify_pop_and_set_car (op);
	  break;

	case stackify_opcode_pop_and_set_cdr:
	  stackify_pop_and_set_cdr (op);
	  break;

	case stackify_opcode_push_consS:
	  stackify_push_consS (op);
	  break;

	case stackify_opcode_push_empty_vector:
	  stackify_push_empty_vector (op);
	  break;

	case stackify_opcode_pop_and_vector_set:
	  stackify_pop_and_vector_set (op);
	  break;

	case stackify_opcode_push_vector:
	  stackify_push_vector (op);
	  break;

	case stackify_opcode_push_empty_record:
	  stackify_push_empty_record (op);
	  break;

	case stackify_opcode_pop_and_record_set:
	  stackify_pop_and_record_set (op);
	  break;

	case stackify_opcode_push_record:
	  stackify_push_record (op);
	  break;

	case stackify_opcode_push_lookup:
	  stackify_push_lookup (op);
	  break;

	case stackify_opcode_store:
	  stackify_store (op);
	  break;

	case stackify_opcode_push_constant:
	  stackify_push_constant (op);
	  break;

	case stackify_opcode_push_unassigned:
	  stackify_push_unassigned (op);
	  break;

	case stackify_opcode_push_primitive:
	  stackify_push_primitive (op);
	  break;

	case stackify_opcode_push_primitive_lexpr:
	  stackify_push_primitive_lexpr (op);
	  break;

	case stackify_opcode_push_0:
	case stackify_opcode_push_1:
	case stackify_opcode_push_2:
	case stackify_opcode_push_3:
	case stackify_opcode_push_4:
	case stackify_opcode_push_5:
	case stackify_opcode_push_6:
	  stackify_push_N (op);
	  break;

	case stackify_opcode_push__1:
	  stackify_push__1 (op);
	  break;

	case stackify_opcode_push_consS_0:
	case stackify_opcode_push_consS_1:
	case stackify_opcode_push_consS_2:
	case stackify_opcode_push_consS_3:
	case stackify_opcode_push_consS_4:
	case stackify_opcode_push_consS_5:
	case stackify_opcode_push_consS_6:
	case stackify_opcode_push_consS_7:
	  stackify_push_consS_N (op);
	  break;

	case stackify_opcode_pop_and_vector_set_0:
	case stackify_opcode_pop_and_vector_set_1:
	case stackify_opcode_pop_and_vector_set_2:
	case stackify_opcode_pop_and_vector_set_3:
	case stackify_opcode_pop_and_vector_set_4:
	case stackify_opcode_pop_and_vector_set_5:
	case stackify_opcode_pop_and_vector_set_6:
	case stackify_opcode_pop_and_vector_set_7:
	  stackify_pop_and_vector_set_N (op);
	  break;

	case stackify_opcode_push_vector_1:
	case stackify_opcode_push_vector_2:
	case stackify_opcode_push_vector_3:
	case stackify_opcode_push_vector_4:
	case stackify_opcode_push_vector_5:
	case stackify_opcode_push_vector_6:
	case stackify_opcode_push_vector_7:
	case stackify_opcode_push_vector_8:
	  stackify_push_vector_N (op);
	  break;

	case stackify_opcode_pop_and_record_set_0:
	case stackify_opcode_pop_and_record_set_1:
	case stackify_opcode_pop_and_record_set_2:
	case stackify_opcode_pop_and_record_set_3:
	case stackify_opcode_pop_and_record_set_4:
	case stackify_opcode_pop_and_record_set_5:
	case stackify_opcode_pop_and_record_set_6:
	case stackify_opcode_pop_and_record_set_7:
	  stackify_pop_and_record_set_N (op);
	  break;

	case stackify_opcode_push_record_1:
	case stackify_opcode_push_record_2:
	case stackify_opcode_push_record_3:
	case stackify_opcode_push_record_4:
	case stackify_opcode_push_record_5:
	case stackify_opcode_push_record_6:
	case stackify_opcode_push_record_7:
	case stackify_opcode_push_record_8:
	  stackify_push_record_N (op);
	  break;

	case stackify_opcode_push_lookup_0:
	case stackify_opcode_push_lookup_1:
	case stackify_opcode_push_lookup_2:
	case stackify_opcode_push_lookup_3:
	case stackify_opcode_push_lookup_4:
	case stackify_opcode_push_lookup_5:
	case stackify_opcode_push_lookup_6:
	case stackify_opcode_push_lookup_7:
	  stackify_push_lookup_N (op);
	  break;

	case stackify_opcode_store_0:
	case stackify_opcode_store_1:
	case stackify_opcode_store_2:
	case stackify_opcode_store_3:
	case stackify_opcode_store_4:
	case stackify_opcode_store_5:
	case stackify_opcode_store_6:
	case stackify_opcode_store_7:
	  stackify_store_N (op);
	  break;

	case stackify_opcode_push_primitive_0:
	case stackify_opcode_push_primitive_1:
	case stackify_opcode_push_primitive_2:
	case stackify_opcode_push_primitive_3:
	case stackify_opcode_push_primitive_4:
	case stackify_opcode_push_primitive_5:
	case stackify_opcode_push_primitive_6:
	case stackify_opcode_push_primitive_7:
	  stackify_push_primitive_N (op);
	  break;

	  /* Compiler support */
	  /* Ordinary objects don't need the following */

	case stackify_opcode_push_nm_header:
	  stackify_push_nm_header (op);
	  break;

	case stackify_opcode_push_linkage_header_operator:
	  stackify_push_linkage_header_operator (op);
	  break;

	case stackify_opcode_push_linkage_header_reference:
	  stackify_push_linkage_header_reference (op);
	  break;

	case stackify_opcode_push_linkage_header_assignment:
	  stackify_push_linkage_header_assignment (op);
	  break;

	case stackify_opcode_push_linkage_header_global:
	  stackify_push_linkage_header_global (op);
	  break;

	case stackify_opcode_push_linkage_header_closure:
	  stackify_push_linkage_header_closure (op);
	  break;

	case stackify_opcode_push_ulong:
	  stackify_push_ulong (op);
	  break;

	case stackify_opcode_push_label_entry:
	  stackify_push_label_entry (op);
	  break;

	case stackify_opcode_push_label_descriptor:
	  stackify_push_label_descriptor (op);
	  break;

	case stackify_opcode_retag_cc_block:
	  stackify_retag_cc_block (op);
	  break;

	case stackify_opcode_cc_block_to_entry:
	  stackify_cc_block_to_entry (op);
	  break;

	case stackify_opcode_push_return_code:
	  stackify_push_return_code (op);
	  break;
	}
    }

  /* Grab the result and return it */

  result = (unstackify_pop ());
  free (scratch);
  unstackify_restore_context (&context);
  return (result);
}
