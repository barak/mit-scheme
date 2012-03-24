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

#define IDENTITY_LENGTH 	20	/* Plenty of room */
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

#define N_IDENTITY_NAMES 0x0D
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
  /* 0x0C */	"cc-arch-string"
};

SCHEME_OBJECT
make_microcode_identification_vector (void)
{
  SCHEME_OBJECT v = (make_vector (IDENTITY_LENGTH, SHARP_F, true));
  VECTOR_SET (v, ID_RELEASE, SHARP_F);
  VECTOR_SET (v, ID_MICRO_VERSION, (char_pointer_to_string (PACKAGE_VERSION)));
  VECTOR_SET (v, ID_PRINTER_WIDTH, (ULONG_TO_FIXNUM (OS_tty_x_size ())));
  VECTOR_SET (v, ID_PRINTER_LENGTH, (ULONG_TO_FIXNUM (OS_tty_y_size ())));
  VECTOR_SET (v, ID_NEW_LINE_CHARACTER, (ASCII_TO_CHAR ('\n')));
  VECTOR_SET (v, ID_FLONUM_PRECISION, (ULONG_TO_FIXNUM (DBL_MANT_DIG)));
  VECTOR_SET (v, ID_FLONUM_EPSILON, (double_to_flonum ((double) DBL_EPSILON)));
  VECTOR_SET (v, ID_OS_NAME, (char_pointer_to_string (OS_Name)));
  VECTOR_SET (v, ID_OS_VARIANT, (char_pointer_to_string (OS_Variant)));
  VECTOR_SET (v, ID_STACK_TYPE, (char_pointer_to_string ("standard")));
  VECTOR_SET (v, ID_MACHINE_TYPE, (char_pointer_to_string (MACHINE_TYPE)));
  {
    const char * name = (cc_arch_name ());
    if (name != 0)
      VECTOR_SET (v, ID_CC_ARCH, (char_pointer_to_string (name)));
  }
  return (v);
}

static const char *
cc_arch_name (void)
{
  switch (compiler_processor_type)
    {
    case COMPILER_NONE_TYPE: return ("none");
    case COMPILER_MC68020_TYPE: return ("mc68k");
    case COMPILER_VAX_TYPE: return ("vax");
    case COMPILER_SPECTRUM_TYPE: return ("hppa");
    case COMPILER_MC68040_TYPE: return ("mc68k");
    case COMPILER_SPARC_TYPE: return ("sparc");
    case COMPILER_IA32_TYPE: return ("i386");
    case COMPILER_ALPHA_TYPE: return ("alpha");
    case COMPILER_MIPS_TYPE: return ("mips");
    case COMPILER_C_TYPE: return ("c");
    case COMPILER_SVM_TYPE: return ("svm1");
    case COMPILER_X86_64_TYPE: return ("x86-64");
    default: return (0);
    }
}

#define STORE_FIXOBJ(slot, object)					\
  VECTOR_SET (fixed_objects, slot, object)

#define STORE_NAME_VECTOR(slot, names, length)				\
  STORE_FIXOBJ (slot, (names_to_vector (length, names)))

#define STORE_GENERIC(slot, name, arity)				\
  STORE_FIXOBJ (slot, (make_primitive (name, arity)))

void
initialize_fixed_objects_vector (void)
{
  fixed_objects = (make_vector (N_FIXED_OBJECTS, SHARP_F, false));
  STORE_FIXOBJ (NON_OBJECT, (MAKE_OBJECT (TC_CONSTANT, 2)));
  STORE_FIXOBJ (SYSTEM_INTERRUPT_VECTOR,
		(initialize_interrupt_handler_vector ()));
  STORE_FIXOBJ (FIXOBJ_INTERRUPT_MASK_VECTOR,
		(initialize_interrupt_mask_vector ()));
  /* Error vector is not needed at boot time */
  STORE_FIXOBJ (SYSTEM_ERROR_VECTOR, SHARP_F);

  /* This must happen before we initialize name vectors.  */
  STORE_FIXOBJ (OBARRAY, (make_vector (OBARRAY_SIZE, EMPTY_LIST, false)));

  STORE_NAME_VECTOR (TYPES_VECTOR, type_names, TYPE_CODE_LIMIT);
  STORE_NAME_VECTOR (RETURNS_VECTOR, Return_Names, (MAX_RETURN_CODE + 1));
  STORE_NAME_VECTOR (ERRORS_VECTOR, Error_Names, (MAX_ERROR + 1));
  STORE_NAME_VECTOR (Termination_Vector, Term_Names, (MAX_TERMINATION + 1));
  STORE_NAME_VECTOR (FIXED_OBJECTS_SLOTS,
		     fixed_objects_names, (N_FIXED_OBJECTS + 1));
  STORE_NAME_VECTOR (IDENTIFICATION_VECTOR, identity_names, N_IDENTITY_NAMES);

  STORE_FIXOBJ (DUMMY_HISTORY, (initialize_history ()));
  STORE_FIXOBJ (Bignum_One, (long_to_bignum (1)));
  STORE_FIXOBJ (FIXOBJ_EDWIN_AUTO_SAVE, EMPTY_LIST);
  STORE_FIXOBJ (FIXOBJ_FILES_TO_DELETE, EMPTY_LIST);
  STORE_FIXOBJ (FIXOBJ_SYSTEM_CALL_NAMES, (fixed_objects_syscall_names ()));
  STORE_FIXOBJ (FIXOBJ_SYSTEM_CALL_ERRORS, (fixed_objects_syserr_names ()));

  STORE_GENERIC (GENERIC_TRAMPOLINE_ZERO_P, "INTEGER-ZERO?", 1);
  STORE_GENERIC (GENERIC_TRAMPOLINE_POSITIVE_P, "INTEGER-POSITIVE?", 1);
  STORE_GENERIC (GENERIC_TRAMPOLINE_NEGATIVE_P, "INTEGER-NEGATIVE?", 1);
  STORE_GENERIC (GENERIC_TRAMPOLINE_SUCCESSOR, "INTEGER-ADD-1", 1);
  STORE_GENERIC (GENERIC_TRAMPOLINE_PREDECESSOR, "INTEGER-SUBTRACT-1", 1);
  STORE_GENERIC (GENERIC_TRAMPOLINE_EQUAL_P, "INTEGER-EQUAL?", 2);
  STORE_GENERIC (GENERIC_TRAMPOLINE_LESS_P, "INTEGER-LESS?", 2);
  STORE_GENERIC (GENERIC_TRAMPOLINE_GREATER_P, "INTEGER-GREATER?", 2);
  STORE_GENERIC (GENERIC_TRAMPOLINE_ADD, "INTEGER-ADD", 2);
  STORE_GENERIC (GENERIC_TRAMPOLINE_SUBTRACT, "INTEGER-SUBTRACT", 2);
  STORE_GENERIC (GENERIC_TRAMPOLINE_MULTIPLY, "INTEGER-MULTIPLY", 2);

  STORE_FIXOBJ (GENERIC_TRAMPOLINE_DIVIDE, SHARP_F);
  STORE_FIXOBJ (GENERIC_TRAMPOLINE_QUOTIENT, SHARP_F);
  STORE_FIXOBJ (GENERIC_TRAMPOLINE_REMAINDER, SHARP_F);
  STORE_FIXOBJ (GENERIC_TRAMPOLINE_MODULO, SHARP_F);

  STORE_FIXOBJ (ARITY_DISPATCHER_TAG,
		(char_pointer_to_symbol
		 ("#[(microcode)arity-dispatcher-tag]")));

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
  SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, length, true));
  unsigned long i;
  for (i = 0; (i < length); i += 1)
    VECTOR_SET (v, i,
		(((names[i]) == 0)
		 ? SHARP_F
		 : (char_pointer_to_symbol (names[i]))));
  return (v);
}
