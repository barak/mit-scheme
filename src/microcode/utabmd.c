/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

/* The "microcode tables" that provide information to the runtime
   system about the microcode.  */

#include "scheme.h"
#include "ostop.h"
#include "ostty.h"

#ifdef __WIN32__
   extern void NT_initialize_fov (SCHEME_OBJECT);
#endif

extern void OS_syscall_names (unsigned long *, const char ***);
extern void OS_syserr_names (unsigned long *, const char ***);
extern SCHEME_OBJECT initialize_history (void);
extern SCHEME_OBJECT initialize_interrupt_handler_vector (void);
extern SCHEME_OBJECT initialize_interrupt_mask_vector (void);

static const char * cc_arch_name (void);
static SCHEME_OBJECT fixed_objects_syscall_names (void);
static SCHEME_OBJECT fixed_objects_syserr_names (void);
static SCHEME_OBJECT names_to_vector (unsigned long, const char **);

#define IDENTITY_LENGTH		20	/* Plenty of room */
#define ID_RELEASE		0	/* System release (string) */
#define ID_MICRO_VERSION	1	/* Microcode version (fixnum) */
/* 2 unused */
#define ID_PRINTER_WIDTH	3	/* TTY width (# chars) */
#define ID_PRINTER_LENGTH	4	/* TTY height (# chars) */
#define ID_NEW_LINE_CHARACTER	5	/* #\Newline */
#define ID_FLONUM_PRECISION	6	/* Flonum mantissa (# bits) */
#define ID_FLONUM_EPSILON	7	/* Flonum epsilon (flonum) */
#define ID_OS_NAME		8	/* OS name (string) */
#define ID_OS_VARIANT		9	/* OS variant (string) */
#define ID_STACK_TYPE		10	/* Scheme stack type (string) */
#define ID_MACHINE_TYPE		11	/* Machine type (string) */
#define ID_CC_ARCH		12	/* Compiled-code support (string) */
#define ID_FLONUM_EXP_MIN	13	/* Minimum finite (normal) exponent */
#define ID_FLONUM_EXP_MAX	14	/* Maximum finite exponent */
#define ID_NONNEG_FIXNUM_LENGTH	15	/* Number of bits in nonneg. fixnum */
#define ID_NONNEG_FIXNUM_MASK	16	/* Mask for nonneg. fixnum */

#define N_IDENTITY_NAMES 0x11
static const char * identity_names [] =
{
  /* 0x00 */	"system-release-string",
  /* 0x01 */	"microcode-version",
  /* 0x02 */	0,
  /* 0x03 */	"console-width",
  /* 0x04 */	"console-height",
  /* 0x05 */	"newline-char",
  /* 0x06 */	"flonum-mantissa-length",
  /* 0x07 */	"flonum-epsilon",
  /* 0x08 */	"os-name-string",
  /* 0x09 */	"os-variant-string",
  /* 0x0A */	"stack-type-string",
  /* 0x0B */	"machine-type-string",
  /* 0x0C */	"cc-arch-string",
  /* 0x0D */	"flonum-exponent-min",
  /* 0x0E */	"flonum-exponent-max",
  /* 0x0F */	"nonnegative-fixnum-length",
  /* 0x10 */	"nonnegative-fixnum-mask",
};

SCHEME_OBJECT
make_microcode_identification_vector (void)
{
  SCHEME_OBJECT v = (make_vector (IDENTITY_LENGTH, SHARP_F, true));
  vector_set (v, ID_RELEASE, SHARP_F);
  vector_set (v, ID_MICRO_VERSION, (char_pointer_to_string (PACKAGE_VERSION)));
  vector_set (v, ID_PRINTER_WIDTH, (ULONG_TO_FIXNUM (OS_tty_x_size ())));
  vector_set (v, ID_PRINTER_LENGTH, (ULONG_TO_FIXNUM (OS_tty_y_size ())));
  vector_set (v, ID_NEW_LINE_CHARACTER, (ASCII_TO_CHAR ('\n')));
  vector_set (v, ID_FLONUM_PRECISION, (ULONG_TO_FIXNUM (DBL_MANT_DIG)));
  vector_set (v, ID_FLONUM_EPSILON, (double_to_flonum ((double) DBL_EPSILON)));
  vector_set (v, ID_OS_NAME, (char_pointer_to_string (OS_Name)));
  vector_set (v, ID_OS_VARIANT, (char_pointer_to_string (OS_Variant)));
  vector_set (v, ID_STACK_TYPE, (char_pointer_to_string ("standard")));
  vector_set (v, ID_MACHINE_TYPE, (char_pointer_to_string (MACHINE_TYPE)));
  vector_set (v, ID_FLONUM_EXP_MIN, (LONG_TO_FIXNUM (DBL_MIN_EXP - 1)));
  vector_set (v, ID_FLONUM_EXP_MAX, (LONG_TO_FIXNUM (DBL_MAX_EXP - 1)));
  vector_set (v, ID_NONNEG_FIXNUM_LENGTH, (ULONG_TO_FIXNUM (FIXNUM_LENGTH)));
  vector_set (v, ID_NONNEG_FIXNUM_MASK, (ULONG_TO_FIXNUM (FIXNUM_MASK)));
  vector_set (v, ID_CC_ARCH, (char_pointer_to_string (cc_arch_name ())));
  return (v);
}

static const char *
cc_arch_name (void)
{
  switch (compiler_processor_type)
    {
    case COMPILER_NONE_TYPE: return ("none");
    case COMPILER_IA32_TYPE: return ("i386");
    case COMPILER_C_TYPE: return ("c");
    case COMPILER_SVM_TYPE: return ("svm1");
    case COMPILER_X86_64_TYPE: return ("x86-64");
    case COMPILER_AARCH64_TYPE: return ("aarch64");
    default: return ("unknown");
    }
}

static inline void
fixobj_set (unsigned int slot, SCHEME_OBJECT object)
{
  vector_set (fixed_objects, slot, object);
}

static inline void
fixobj_set_names (unsigned int slot, const char** names, size_t length)
{
  fixobj_set (slot, names_to_vector (length, names));
}

static inline void
fixobj_set_prim (unsigned int slot, const char* name, int arity)
{
  fixobj_set (slot, make_primitive (name, arity));
}

void
initialize_fixed_objects_vector (void)
{
  fixed_objects = make_vector (N_FIXED_OBJECTS, SHARP_F, false);
  fixobj_set (NON_OBJECT, make_object (TC_CONSTANT, 2));
  fixobj_set (SYSTEM_INTERRUPT_VECTOR,
	      initialize_interrupt_handler_vector ());
  fixobj_set (FIXOBJ_INTERRUPT_MASK_VECTOR,
	      initialize_interrupt_mask_vector ());
  /* Error vector is not needed at boot time */
  fixobj_set (SYSTEM_ERROR_VECTOR, SHARP_F);

  /* This must happen before we initialize name vectors.  */
  fixobj_set (OBARRAY, make_vector (OBARRAY_SIZE, EMPTY_LIST, false));

  fixobj_set_names (TYPES_VECTOR, type_names, TYPE_CODE_LIMIT);
  fixobj_set_names (ERRORS_VECTOR, Error_Names, (MAX_ERROR + 1));
  fixobj_set_names (TERMINATION_VECTOR, Term_Names, (MAX_TERMINATION + 1));
  fixobj_set_names
    (FIXED_OBJECTS_SLOTS, fixed_objects_names, N_FIXED_OBJECTS + 1);
  fixobj_set_names (IDENTIFICATION_VECTOR, identity_names, N_IDENTITY_NAMES);

  fixobj_set (RETURNS_VECTOR, make_return_code_names_table());
  fixobj_set (FIXOBJ_RETURN_FRAMES_VECTOR, make_frame_type_info_table ());

  fixobj_set (DUMMY_HISTORY, initialize_history ());
  fixobj_set (FIXOBJ_BIGNUM_ONE, long_to_bignum (1));
  fixobj_set (FIXOBJ_EDWIN_AUTO_SAVE, EMPTY_LIST);
  fixobj_set (FIXOBJ_FILES_TO_DELETE, EMPTY_LIST);
  fixobj_set (FIXOBJ_SYSTEM_CALL_NAMES, fixed_objects_syscall_names ());
  fixobj_set (FIXOBJ_SYSTEM_CALL_ERRORS, fixed_objects_syserr_names ());
  fixobj_set (FIXOBJ_GC_RECLAIMED, GC_RECLAIMED);

  fixobj_set_prim (GENERIC_TRAMPOLINE_ZERO_P, "integer-zero?", 1);
  fixobj_set_prim (GENERIC_TRAMPOLINE_POSITIVE_P, "integer-positive?", 1);
  fixobj_set_prim (GENERIC_TRAMPOLINE_NEGATIVE_P, "integer-negative?", 1);
  fixobj_set_prim (GENERIC_TRAMPOLINE_SUCCESSOR, "integer-add-1", 1);
  fixobj_set_prim (GENERIC_TRAMPOLINE_PREDECESSOR, "integer-subtract-1", 1);
  fixobj_set_prim (GENERIC_TRAMPOLINE_EQUAL_P, "integer-equal?", 2);
  fixobj_set_prim (GENERIC_TRAMPOLINE_LESS_P, "integer-less?", 2);
  fixobj_set_prim (GENERIC_TRAMPOLINE_GREATER_P, "integer-greater?", 2);
  fixobj_set_prim (GENERIC_TRAMPOLINE_ADD, "integer-add", 2);
  fixobj_set_prim (GENERIC_TRAMPOLINE_SUBTRACT, "integer-subtract", 2);
  fixobj_set_prim (GENERIC_TRAMPOLINE_MULTIPLY, "integer-multiply", 2);

  fixobj_set (GENERIC_TRAMPOLINE_DIVIDE, SHARP_F);
  fixobj_set (GENERIC_TRAMPOLINE_QUOTIENT, SHARP_F);
  fixobj_set (GENERIC_TRAMPOLINE_REMAINDER, SHARP_F);
  fixobj_set (GENERIC_TRAMPOLINE_MODULO, SHARP_F);

  fixobj_set (FIXOBJ_PROXIED_RECORD_TYPES,
	      (make_vector (FASDUMP_RECORD_MARKER_END
                            - FASDUMP_RECORD_MARKER_START,
			    SHARP_F,
			    false)));
#ifdef CC_SUPPORT_P
  fixobj_set (FIXOBJ_REFLECT_CODE_NAMES, make_reflect_code_table ());
#endif

#ifdef __WIN32__
  NT_initialize_fov (fixed_objects);
#endif
}

static SCHEME_OBJECT
fixed_objects_syscall_names (void)
{
  unsigned long length;
  const char ** names;
  OS_syscall_names ((&length), (&names));
  return (names_to_vector (length, names));
}

static SCHEME_OBJECT
fixed_objects_syserr_names (void)
{
  unsigned long length;
  const char ** names;
  OS_syserr_names ((&length), (&names));
  return (names_to_vector (length, names));
}

static SCHEME_OBJECT
names_to_vector (unsigned long length, const char ** names)
{
  SCHEME_OBJECT v = (allocate_vector (length, true));
  unsigned long i;
  for (i = 0; (i < length); i += 1)
    vector_set (v, i,
		(((names[i]) == 0)
		 ? SHARP_F
		 : (char_pointer_to_symbol (names[i]))));
  return (v);
}
