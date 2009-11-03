/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/* Shared part of "config.h".  */

#ifndef SCM_CONFSHARED_H
#define SCM_CONFSHARED_H

#ifndef __STDC__
#  include "error: compiler must support ANSI C"
#endif

/* Enable the stepper.  */
#define COMPILE_STEPPER 1

#ifdef ENABLE_DEBUGGING_TOOLS
#  undef NDEBUG			/* for assert() */
#  define WHEN_DEBUGGING(code) do { code } while (0)
#  define ENABLE_PRIMITIVE_PROFILING
#else
#  define NDEBUG 1		/* for assert() */
#  define WHEN_DEBUGGING(code) do {} while (0)
#  undef ENABLE_PRIMITIVE_PROFILING
#endif

/* For use in the C pre-processor, not in code!  */
#define FALSE 0
#define TRUE 1

#include <stdio.h>
#include <math.h>

#if STDC_HEADERS
#  include <stdlib.h>
#  include <stdarg.h>
#  include <stddef.h>
#  include <string.h>
#  include <ctype.h>
#  include <limits.h>
#  include <float.h>
#  include <assert.h>
#else
#  ifdef HAVE_LIMITS_H
#    include <limits.h>
#  endif
#  ifdef HAVE_FLOAT_H
#    include <float.h>
#  else
#    include "float.h"
#  endif
#  ifdef HAVE_ASSERT_H
#    include <assert.h>
#  endif
#  ifdef HAVE_MALLOC_H
#    include <malloc.h>
#  endif
#  if !HAVE_STRCHR
#    define strchr index
#    define strrchr rindex
#  endif
   extern char * strchr ();
   extern char * strrchr ();
#  if !HAVE_MEMCPY
#    define memcpy(d, s, n) bcopy ((s), (d), (n))
#    define memmove(d, s, n) bcopy ((s), (d), (n))
#  endif
#endif

#ifdef HAVE_STDBOOL_H
#  include <stdbool.h>
#else
#  ifndef HAVE__BOOL
#    ifdef __cplusplus
       typedef bool _Bool;
#    else
       typedef unsigned char _Bool;
#    endif
#  endif
#  define bool _Bool
#  define false 0
#  define true 1
#  define __bool_true_false_are_defined 1
#  if ((defined (__GNUC__)) && (__GNUC__ < 3))
     /* Old versions of GCC have an incompatible <stdbool.h>.
	This declaration should prevent them from overriding our defs.  */
#    define __STDBOOL_H__ 1
#  endif
#endif

#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif

#if (CHAR_BIT != 8)
#  include "error: characters must be 8 bits wide"
#endif

#if (FLT_RADIX != 2)
#  include "error: floating-point radix must be 2"
#endif

#if (SIZEOF_UINTPTR_T > SIZEOF_UNSIGNED_LONG)
#  include "error: pointers must fit in 'unsigned long'"
#endif

#if ((defined (__GNUC__)) && (__GNUC__ >= 3))
#  define ATTRIBUTE(x) __attribute__ (x)
#  define NORETURN __attribute__ ((__noreturn__))
#else
#  define ATTRIBUTE(x)
#  define NORETURN
#endif

/* Operating System / Machine dependencies:

   For each implementation, be sure to specify CURRENT_FASL_ARCH.
   Make sure that there is an appropriate FASL_<machine name>.
   If there isn't, add one to the list below.

   If you do not know the values of the parameters specified below,
   try compiling and running the Wsize program ("make Wsize" if on a
   unix variant).  It may not run, but if it does, it will probably
   compute the correct information.

   Note that the C type void is used in the sources.  If your version
   of C does not have this type, you should bypass it.  This can be
   done by inserting the preprocessor command '#define void' in this
   file, under the heading for your kind of machine.

   These parameters MUST be specified (and are computed by Wsize):

   CHAR_BIT is the size of a character in bits.

   FLOATING_ALIGNMENT should be defined ONLY if the system requires
   floating point numbers (double) to be aligned more strictly than
   SCHEME_OBJECTs (unsigned long).  The value must be a mask of the
   low order bits which are required to be zero for the storage
   address.  For example, a value of 0x7 requires octabyte alignment
   on a machine where addresses are specified in bytes.  The alignment
   must be an integral multiple of the length of a long.

   Other flags (the safe option is NOT to define them, which will
   sacrifice speed for safety):

   HEAP_IN_LOW_MEMORY should be defined if malloc returns the lowest
   available memory and thus all addresses will fit in the datum portion
   of a Scheme object.  The datum portion of a Scheme object is 8 bits
   less than the length of a C long.  */

/* Possible values for CURRENT_FASL_ARCH.  For the most part this
   means the processor type, so for example there are several aliases
   for 68000 family processors.  This scheme allows sharing of
   compiled code on machines with the same processor type.  Probably
   we will have to create a more powerful method of identifying FASL
   files when we introduce new differences, such as whether or not a
   68881 coprocessor is installed. */

typedef enum
{
  FASL_UNKNOWN,
  FASL_PDP10,
  FASL_VAX,
  FASL_68020,
  FASL_68000,
  FASL_HP_9000_500,
  FASL_IA32,
  FASL_BFLY,
  FASL_CYBER,
  FASL_CELERITY,
  FASL_HP_SPECTRUM,
  FASL_UMAX,
  FASL_PYR,
  FASL_ALLIANT,
  FASL_SPARC,
  FASL_MIPS,
  FASL_APOLLO_68K,
  FASL_APOLLO_PRISM,
  FASL_ALPHA,
  FASL_RS6000,
  FASL_PPC32,
  FASL_X86_64,
  FASL_PPC64,
  FASL_IA64
} fasl_arch_t;

/* Possible values for COMPILER_PROCESSOR_TYPE.  This identifies the
   processor for which native-code support is provided.  This is
   related to the fasl_arch_t types above, but can also take on values
   that are independent of the host architecture.  */

typedef enum
{
  COMPILER_NONE_TYPE,
  COMPILER_MC68020_TYPE,
  COMPILER_VAX_TYPE,
  COMPILER_SPECTRUM_TYPE,
  COMPILER_OLD_MIPS_TYPE,
  COMPILER_MC68040_TYPE,
  COMPILER_SPARC_TYPE,
  COMPILER_RS6000_TYPE,
  COMPILER_MC88K_TYPE,
  COMPILER_IA32_TYPE,
  COMPILER_ALPHA_TYPE,
  COMPILER_MIPS_TYPE,
  COMPILER_C_TYPE,
  COMPILER_SVM_TYPE,
  COMPILER_X86_64_TYPE,
} cc_arch_t;

#include "cmpintmd-config.h"

#ifdef vax

/* Amazingly unix and vms agree on all these */

#define MACHINE_TYPE		"vax"
#define CURRENT_FASL_ARCH	FASL_VAX
#define PC_ZERO_BITS		0
#define HEAP_IN_LOW_MEMORY	1

/* Not on these, however */

#ifdef vms

#define VMS_VERSION		4
#define VMS_SUBVERSION		5

/* If your C runtime library already defines the `tbuffer' datatype,
   then define this symbol. */
/* #define HAVE_TBUFFER */

/* Name conflict in VMS with system variable */
#define Free			Free_Register

#if (VMS_VERSION < 4)
   /* Pre version 4 VMS has no void type. */
#  define void
#endif

/* This eliminates a spurious warning from the C compiler. */
#define main_type

/* exit(0) produces horrible message on VMS */
#define NORMAL_EXIT 1

#define EXIT_SCHEME_DECLARATIONS static jmp_buf exit_scheme_jmp_buf

#define INIT_EXIT_SCHEME()						\
{									\
  int which_way = (setjmp (exit_scheme_jmp_buf));			\
  if (which_way == NORMAL_EXIT)						\
    return;								\
}

#define EXIT_SCHEME(value)						\
{									\
  if (value != 0)							\
    exit (value);							\
  longjmp (exit_scheme_jmp_buf, NORMAL_EXIT);				\
}

#else /* not vms */

/* Vax Unix C compiler bug */
#define HAVE_DOUBLE_TO_LONG_BUG

#endif /* not vms */
#endif /* vax */

#if defined(hp9000s800) || defined(__hp9000s800)
#if defined(hp9000s700) || defined(__hp9000s700)
#define MACHINE_TYPE		"hp9000s700"
#else
#define MACHINE_TYPE		"hp9000s800"
#endif
#define CURRENT_FASL_ARCH	FASL_HP_SPECTRUM
#define PC_ZERO_BITS		2
#define FLOATING_ALIGNMENT	0x7

/* Heap resides in data space, pointed at by space register 5.
   Short pointers must have their high two bits set to 01 so that
   it is interpreted as space register 5, 2nd quadrant.

   This is kludged by the definitions below, and is still considered
   HEAP_IN_LOW_MEMORY.  */

#define HEAP_IN_LOW_MEMORY

/* data segment bits and mask for all bits */

#define HPPA_QUAD_BIT	0x40000000
#define HPPA_QUAD_MASK	0xC0000000

#define DATUM_TO_ADDRESS(datum)						\
  ((SCHEME_OBJECT *) (((unsigned long) (datum)) | HPPA_QUAD_BIT))

#define ADDRESS_TO_DATUM(address)					\
  ((SCHEME_OBJECT) (((unsigned long) (address)) & (~(HPPA_QUAD_MASK))))

/* SHARP_F is a magic value:
   Typecode TC_CONSTANT, high datum bits #b100, low datum bits are the top
   TYPE_CODE_LENGTH bits of HPPA_QUAD_BIT

   SHARP_F is stored in gr5 for access by compiled code.  This allows
   us to generate #F and test against #F quickly, and also to use gr5
   for compiled OBJECT->ADDRESS operations.  If we ever go to 5bit
   typecodes we will be able to dispense with this overloading.

   See also cmpauxmd/hppa.m4.  */

#define SHARP_F         0x22000010

#endif /* hp9000s800 */

#if defined(hp9000s300) || defined(__hp9000s300)

#if defined(hp9000s400) || defined(__hp9000s400)
#  define MACHINE_TYPE		"hp9000s400"
#else
#  define MACHINE_TYPE		"hp9000s300"
#endif

#ifdef MC68010
#  define CURRENT_FASL_ARCH	FASL_68000
#else
#  define CURRENT_FASL_ARCH	FASL_68020
#endif

#define PC_ZERO_BITS		1
#define HEAP_IN_LOW_MEMORY	1

#endif /* hp9000s300 */

#ifdef hp9000s500
#define MACHINE_TYPE		"hp9000s500"
#define CURRENT_FASL_ARCH 	FASL_HP_9000_500

/* An unfortunate fact of life on this machine:
   the C heap is in high memory thus HEAP_IN_LOW_MEMORY is not
   defined and the whole thing runs slowly.  */

#endif /* hp9000s500 */

#ifdef __sparc
#  define MACHINE_TYPE		"sun4"
#  define CURRENT_FASL_ARCH	FASL_SPARC
#  define FLOATING_ALIGNMENT	0x7
#  define HEAP_IN_LOW_MEMORY
#  define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef sun3
#  define MACHINE_TYPE		"sun3"
#  define CURRENT_FASL_ARCH	FASL_68020
#  define HEAP_IN_LOW_MEMORY
#  define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef sun2
#  define MACHINE_TYPE		"sun2"
#  define CURRENT_FASL_ARCH	FASL_68000
#  define HEAP_IN_LOW_MEMORY
#  define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef NeXT
#  define MACHINE_TYPE		"next"
#  define CURRENT_FASL_ARCH	FASL_68020
#  define HEAP_IN_LOW_MEMORY
#endif

#if defined(_M_IX86) || defined(__i386__) || defined(__i386) || defined(i386)
#  define __IA32__
#endif

#ifdef __IA32__

#define CURRENT_FASL_ARCH	FASL_IA32
#define PC_ZERO_BITS		0
#define HEAP_IN_LOW_MEMORY	1

#ifdef sequent
#  define MACHINE_TYPE		"sequent386"
#endif

#ifdef sun
#  define MACHINE_TYPE		"sun386i"
#endif

#ifndef MACHINE_TYPE
#  define MACHINE_TYPE		"IA-32"
#endif

#endif /* __IA32__ */

#ifdef mips

#define MACHINE_TYPE		"mips"
#define CURRENT_FASL_ARCH	FASL_MIPS
#define PC_ZERO_BITS		2
#define FLOATING_ALIGNMENT   	0x7

#ifdef _IRIX6
   extern void * irix_heap_malloc (unsigned long);
#  define HEAP_MALLOC irix_heap_malloc
#endif

/* Heap resides in data space which begins at 0x10000000. This is
   kludged by the definitions below, and is still considered
   HEAP_IN_LOW_MEMORY.  */

#define HEAP_IN_LOW_MEMORY
#define MIPS_DATA_BIT	0x10000000

#define DATUM_TO_ADDRESS(datum)						\
  ((SCHEME_OBJECT *) (((unsigned long) (datum)) | MIPS_DATA_BIT))

#define ADDRESS_TO_DATUM(address)					\
  ((SCHEME_OBJECT) (((unsigned long) (address)) & (~(MIPS_DATA_BIT))))

/* MIPS compiled binaries are large! */
#ifndef DEFAULT_SMALL_CONSTANT
#  define DEFAULT_SMALL_CONSTANT 700
#endif

#ifndef DEFAULT_LARGE_CONSTANT
#  define DEFAULT_LARGE_CONSTANT 1500
#endif

#endif /* mips */

#ifdef __alpha
#define MACHINE_TYPE		"Alpha"
#define CURRENT_FASL_ARCH	FASL_ALPHA
#define PC_ZERO_BITS		2
#define HEAP_IN_LOW_MEMORY	1

/* Flonums have no special alignment constraints. */
#define FLONUM_MANTISSA_BITS	53
#define FLONUM_EXPT_SIZE	10
#define MAX_FLONUM_EXPONENT	1023
/* Floating point representation uses hidden bit. */

extern void * alpha_heap_malloc (unsigned long);
#define HEAP_MALLOC alpha_heap_malloc

#endif /* __alpha */

#ifdef __OS2__

#define PREALLOCATE_HEAP_MEMORY()					\
{									\
  extern void OS2_alloc_heap (void);					\
  OS2_alloc_heap ();							\
}

extern void * OS2_commit_heap (unsigned long);
#define HEAP_MALLOC OS2_commit_heap
#define HEAP_FREE(address)

#define EXIT_SCHEME_DECLARATIONS extern void OS2_exit_scheme (int)
#define EXIT_SCHEME OS2_exit_scheme

extern void OS2_stack_reset (void);
#define STACK_RESET OS2_stack_reset

extern int OS2_stack_overflowed_p (void);
#define STACK_OVERFLOWED_P OS2_stack_overflowed_p

#define CC_ARCH_INITIALIZE i386_interface_initialize

#endif /* __OS2__ */

#ifdef __WIN32__

extern void win32_stack_reset (void);
#define STACK_RESET win32_stack_reset

#define HEAP_MALLOC(size) (WIN32_ALLOCATE_HEAP ((size), (&scheme_heap_handle)))
#define HEAP_FREE(base)							\
  WIN32_RELEASE_HEAP (((char *) (base)), scheme_heap_handle)

/* We must not define `main' as that causes conflicts when compiling
   this code with the Watcom C compiler.  */
#define main_name scheme_main

#endif /* __WIN32__ */

/* These (pdp10, nu) haven't worked in a while.
   Should be upgraded or flushed some day.  */

#ifdef pdp10
#define MACHINE_TYPE		"pdp10"
#define CURRENT_FASL_ARCH       FASL_PDP10
#define HEAP_IN_LOW_MEMORY
#define CHAR_BIT 36		/ * Ugh! Supposedly fixed in newer Cs * /
#define UNSIGNED_SHIFT_BUG
#endif

#ifdef nu
#define MACHINE_TYPE		"nu"
#define CURRENT_FASL_ARCH	FASL_68000
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT_BUG
#endif

/* These are pretty old too, but more recent versions have run. */

#ifdef butterfly
#define MACHINE_TYPE		"butterfly"
#define CURRENT_FASL_ARCH	FASL_BFLY
#define HEAP_IN_LOW_MEMORY
#include <public.h>
#endif

#ifdef cyber180
#define MACHINE_TYPE		"cyber180"
#define CURRENT_FASL_ARCH	FASL_CYBER
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT_BUG
/* The Cyber180 C compiler manifests a bug in hairy conditional expressions */
#define Conditional_Bug
#endif

#ifdef celerity
#define MACHINE_TYPE		"celerity"
#define CURRENT_FASL_ARCH	FASL_CELERITY
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef umax
#define MACHINE_TYPE		"umax"
#define CURRENT_FASL_ARCH	FASL_UMAX
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef pyr
#define MACHINE_TYPE		"pyramid"
#define CURRENT_FASL_ARCH	FASL_PYR
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef alliant
#define MACHINE_TYPE		"alliant"
#define CURRENT_FASL_ARCH	FASL_ALLIANT
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef apollo
#if _ISP__M68K
#define MACHINE_TYPE          "Apollo 68k"
#define CURRENT_FASL_ARCH     FASL_APOLLO_68K
#else
#define MACHINE_TYPE          "Apollo Prism"
#define CURRENT_FASL_ARCH     FASL_APOLLO_PRISM
#endif
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef _IBMR2
#define MACHINE_TYPE          "IBM RS6000"
#define CURRENT_FASL_ARCH     FASL_RS6000
/* Heap is not in Low Memory. */
#define FLONUM_MANTISSA_BITS   53
#define FLONUM_EXPT_SIZE       10
#define MAX_FLONUM_EXPONENT    1023
#endif

#ifdef __powerpc__
#  define __ppc__ 1
#endif

#ifdef __powerpc64__
#  define __ppc64__ 1
#endif

#ifdef __ppc__
#  define MACHINE_TYPE		"PowerPC-32"
#  define CURRENT_FASL_ARCH	FASL_PPC32
#  define FLOATING_ALIGNMENT	0x7
#endif

#ifdef __ppc64__
#  define MACHINE_TYPE		"PowerPC-64"
#  define CURRENT_FASL_ARCH	FASL_PPC64
#endif

#ifdef __x86_64__
#  define MACHINE_TYPE		"x86-64"
#  define CURRENT_FASL_ARCH	FASL_X86_64
#  define PC_ZERO_BITS		0
#  define HEAP_IN_LOW_MEMORY	1
#endif

#ifdef __ia64__
#  define MACHINE_TYPE		"ia64"
#  define CURRENT_FASL_ARCH	FASL_IA64
#endif

#ifdef sonyrisc
      /* On the Sony NEWS 3250, this procedure initializes the
	 floating-point CPU control register to enable the IEEE traps.
	 This is normally executed by 'compiler_reset' from LOAD-BAND,
	 but the Sony operating system saves the control register in
	 'setjmp' and restores it on 'longjmp', so we must initialize
	 the register before 'setjmp' is called.  */
#define CC_ARCH_INITIALIZE interface_initialize
#endif

/* Make sure that some definition applies.  If this error occurs, and
   the parameters of the configuration are unknown, try the Wsize
   program.  */
#ifndef MACHINE_TYPE
#  include "Error: confshared.h: Unknown configuration."
#endif

#ifndef PC_ZERO_BITS
#  ifdef CC_IS_NATIVE
#    include "Error: confshared.h: Unknown PC alignment."
#  else
#    define PC_ZERO_BITS 0
#  endif
#endif

#define PC_ALIGNED_P(pc)						\
  ((((unsigned long) (pc)) & ((1 << PC_ZERO_BITS) - 1)) == 0)

/* Virtually all machines have 8-bit characters these days, so don't
   explicitly specify this value unless it is different.  */
#ifndef CHAR_BIT
#  define CHAR_BIT 8
#endif

/* The GNU C compiler does not have any of these bugs. */
#ifdef __GNUC__
#  undef HAVE_DOUBLE_TO_LONG_BUG
#  undef UNSIGNED_SHIFT_BUG
#  undef Conditional_Bug
#endif

#ifdef NO_HEAP_IN_LOW_MEMORY
#  undef HEAP_IN_LOW_MEMORY
#endif

#if defined(USE_MMAP_HEAP_MALLOC) && defined(HEAP_IN_LOW_MEMORY)
   extern void * mmap_heap_malloc (unsigned long);
#  define HEAP_MALLOC mmap_heap_malloc
#  define HEAP_FREE(address)
#endif

#endif /* SCM_CONFSHARED_H */
