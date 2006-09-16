/* -*-C-*-

$Id: confshared.h,v 11.9 2006/09/16 11:19:09 gjr Exp $

Copyright 2000,2002,2003,2006 Massachusetts Institute of Technology

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

/* Shared part of "config.h".  */

#ifndef SCM_CONFSHARED_H
#define SCM_CONFSHARED_H

#include "ansidecl.h"

/* To enable the STEPPER.  Incompatible with futures. */
#define COMPILE_STEPPER 

/* Some configuration consistency testing */

#ifdef COMPILE_STEPPER
#  ifdef COMPILE_FUTURES
#    include "Error: The stepper doesn't work with futures."
#  endif
#  ifdef USE_STACKLETS
#    include "Error: The stepper doesn't work with stacklets."
#  endif
#endif

/* For use in the C pre-processor, not in code! */
#define FALSE		0
#define TRUE		1

/* These C type definitions are needed by everybody.
   They should not be here, but it is unavoidable. */
typedef char Boolean;
#define true		((Boolean) TRUE)
#define false		((Boolean) FALSE)

/* This is the Scheme object type.
   The various fields are defined in "object.h". */
typedef unsigned long SCHEME_OBJECT;
#define OBJECT_LENGTH (CHAR_BIT * SIZEOF_UNSIGNED_LONG)

/* Operating System / Machine dependencies:

   For each implementation, be sure to specify FASL_INTERNAL_FORMAT.
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

/* Possible values for FASL_INTERNAL_FORMAT.  For the most part this
   means the processor type, so for example there are several aliases
   for 68000 family processors.  This scheme allows sharing of
   compiled code on machines with the same processor type.  Probably
   we will have to create a more powerful method of identifying FASL
   files when we introduce new differences, such as whether or not a
   68881 coprocessor is installed. */

#define FASL_UNKNOWN		0
#define FASL_PDP10		1
#define FASL_VAX		2
#define FASL_68020		3
#define FASL_68000  		4
#define FASL_HP_9000_500	5
#define FASL_IA32		6
#define FASL_BFLY		7
#define FASL_CYBER		8
#define FASL_CELERITY		9
#define FASL_HP_SPECTRUM	10
#define FASL_UMAX		11
#define FASL_PYR		12
#define FASL_ALLIANT		13
#define FASL_SPARC		14
#define FASL_MIPS		15
#define FASL_APOLLO_68K		16
#define FASL_APOLLO_PRISM	17
#define FASL_ALPHA		18
#define FASL_RS6000		19
#define FASL_PPC32		20
#define FASL_X86_64		21
#define FASL_PPC64		22
#define FASL_IA64		23

#ifdef vax

/* Amazingly unix and vms agree on all these */

#define MACHINE_TYPE		"vax"
#define FASL_INTERNAL_FORMAT	FASL_VAX
#define HEAP_IN_LOW_MEMORY

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
#define FASL_INTERNAL_FORMAT	FASL_HP_SPECTRUM
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
#define MACHINE_TYPE		"hp9000s400"
#else
#define MACHINE_TYPE		"hp9000s300"
#endif
#ifdef MC68010
#define FASL_INTERNAL_FORMAT	FASL_68000
#else
#define FASL_INTERNAL_FORMAT	FASL_68020
#endif
#define HEAP_IN_LOW_MEMORY

#endif /* hp9000s300 */

#ifdef hp9000s500
#define MACHINE_TYPE		"hp9000s500"
#define FASL_INTERNAL_FORMAT 	FASL_HP_9000_500

/* An unfortunate fact of life on this machine:
   the C heap is in high memory thus HEAP_IN_LOW_MEMORY is not
   defined and the whole thing runs slowly.  */

/* C Compiler bug when constant folding and anchor pointing */
#define And2(x, y)	((x) ? (y) : false)
#define And3(x, y, z)	((x) ? ((y) ? (z) : false) : false)
#define Or2(x, y)	((x) ? true : (y))
#define Or3(x, y, z)	((x) ? true : ((y) ? true : (z)))

#endif /* hp9000s500 */

#ifdef sparc
#  define MACHINE_TYPE		"sun4"
#  define FASL_INTERNAL_FORMAT	FASL_SPARC
#  define FLOATING_ALIGNMENT	0x7
#  define HEAP_IN_LOW_MEMORY
#  define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef sun3
#  define MACHINE_TYPE		"sun3"
#  define FASL_INTERNAL_FORMAT	FASL_68020
#  define HEAP_IN_LOW_MEMORY
#  define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef sun2
#  define MACHINE_TYPE		"sun2"
#  define FASL_INTERNAL_FORMAT	FASL_68000
#  define HEAP_IN_LOW_MEMORY
#  define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef NeXT
#  define MACHINE_TYPE		"next"
#  define FASL_INTERNAL_FORMAT	FASL_68020
#  define HEAP_IN_LOW_MEMORY
#endif

#if defined(_M_IX86) || defined(__i386__) || defined(__i386) || defined(i386)
#  define __IA32__
#endif

#ifdef __IA32__

#define FASL_INTERNAL_FORMAT	FASL_IA32
#define HEAP_IN_LOW_MEMORY

#ifdef sequent
#  define MACHINE_TYPE		"sequent386"
#endif

#ifdef sun
#  define MACHINE_TYPE		"sun386i"
#endif

#ifndef MACHINE_TYPE
#  define MACHINE_TYPE		"IA-32"
#endif

#ifdef NATIVE_CODE_IS_C
#undef HEAP_IN_LOW_MEMORY
#endif

#endif /* __IA32__ */

#ifdef mips

#define MACHINE_TYPE		"mips"
#define FASL_INTERNAL_FORMAT	FASL_MIPS
#define FLOATING_ALIGNMENT   	0x7

#if defined(_IRIX6) && defined(HAS_COMPILER_SUPPORT) && !defined(NATIVE_CODE_IS_C)
   extern void * irix_heap_malloc (long);
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
#ifdef HAS_COMPILER_SUPPORT

#ifndef DEFAULT_SMALL_CONSTANT
#define DEFAULT_SMALL_CONSTANT 700
#endif

#ifndef DEFAULT_LARGE_CONSTANT
#define DEFAULT_LARGE_CONSTANT 1500
#endif

#endif /* HAS_COMPILER_SUPPORT */

#endif /* mips */

#ifdef __alpha
#define MACHINE_TYPE           "Alpha"
#define FASL_INTERNAL_FORMAT   FASL_ALPHA
#define TYPE_CODE_LENGTH       8

/* The ASCII character set is used. */
#define HEAP_IN_LOW_MEMORY     1

/* Flonums have no special alignment constraints. */
#define FLONUM_MANTISSA_BITS   53
#define FLONUM_EXPT_SIZE       10
#define MAX_FLONUM_EXPONENT    1023
/* Floating point representation uses hidden bit. */

#if defined(HAS_COMPILER_SUPPORT) && !defined(NATIVE_CODE_IS_C)
   extern void * alpha_heap_malloc (long);
#  define HEAP_MALLOC		alpha_heap_malloc
#endif

#endif /* __alpha */

#if defined(USE_MMAP_HEAP_MALLOC) && defined(HEAP_IN_LOW_MEMORY)
   extern void * mmap_heap_malloc (unsigned long);
#  define HEAP_MALLOC mmap_heap_malloc
#  define HEAP_FREE(address)
#endif

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

#endif /* __OS2__ */

#ifdef __WIN32__

extern void EXFUN (win32_stack_reset, (void));
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
#define FASL_INTERNAL_FORMAT    FASL_PDP10
#define HEAP_IN_LOW_MEMORY
#define CHAR_BIT 36		/ * Ugh! Supposedly fixed in newer Cs * /
#define UNSIGNED_SHIFT_BUG
#endif

#ifdef nu
#define MACHINE_TYPE		"nu"
#define FASL_INTERNAL_FORMAT	FASL_68000
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT_BUG
#endif

/* These are pretty old too, but more recent versions have run. */

#ifdef butterfly
#define MACHINE_TYPE		"butterfly"
#define FASL_INTERNAL_FORMAT	FASL_BFLY
#define HEAP_IN_LOW_MEMORY
#include <public.h>
#endif

#ifdef cyber180
#define MACHINE_TYPE		"cyber180"
#define FASL_INTERNAL_FORMAT	FASL_CYBER
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT_BUG
/* The Cyber180 C compiler manifests a bug in hairy conditional expressions */
#define Conditional_Bug
#endif

#ifdef celerity
#define MACHINE_TYPE		"celerity"
#define FASL_INTERNAL_FORMAT	FASL_CELERITY
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef umax
#define MACHINE_TYPE		"umax"
#define FASL_INTERNAL_FORMAT	FASL_UMAX
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef pyr
#define MACHINE_TYPE		"pyramid"
#define FASL_INTERNAL_FORMAT	FASL_PYR
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef alliant
#define MACHINE_TYPE		"alliant"
#define FASL_INTERNAL_FORMAT	FASL_ALLIANT
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef apollo
#if _ISP__M68K
#define MACHINE_TYPE          "Apollo 68k"
#define FASL_INTERNAL_FORMAT  FASL_APOLLO_68K
#else
#define MACHINE_TYPE          "Apollo Prism"
#define FASL_INTERNAL_FORMAT  FASL_APOLLO_PRISM
#endif
#define HEAP_IN_LOW_MEMORY
#endif

#ifdef _IBMR2
#define MACHINE_TYPE          "IBM RS6000"
#define FASL_INTERNAL_FORMAT   FASL_RS6000
/* Heap is not in Low Memory. */
#define FLONUM_MANTISSA_BITS   53
#define FLONUM_EXPT_SIZE       10
#define MAX_FLONUM_EXPONENT    1023
#endif

#ifdef __ppc__
#define MACHINE_TYPE		"PowerPC-32"
#define FASL_INTERNAL_FORMAT	FASL_PPC32
#define FLOATING_ALIGNMENT	0x7
#endif

#ifdef __ppc64__
#define MACHINE_TYPE		"PowerPC-64"
#define FASL_INTERNAL_FORMAT	FASL_PPC64
#endif

#ifdef __x86_64__
#define MACHINE_TYPE		"x86-64"
#define FASL_INTERNAL_FORMAT	FASL_X86_64
#endif

#ifdef __ia64__
#define MACHINE_TYPE		"ia64"
#define FASL_INTERNAL_FORMAT	FASL_IA64
#endif

#ifdef NATIVE_CODE_IS_C
#  ifndef HAS_COMPILER_SUPPORT
#    define HAS_COMPILER_SUPPORT
#  endif
#endif

/* Make sure that some definition applies.  If this error occurs, and
   the parameters of the configuration are unknown, try the Wsize
   program.  */
#ifndef MACHINE_TYPE
#  include "Error: confshared.h: Unknown configuration."
#endif

/* Virtually all machines have 8-bit characters these days, so don't
   explicitly specify this value unless it is different.  */
#ifndef CHAR_BIT
#  define CHAR_BIT 8
#endif

#ifndef TYPE_CODE_LENGTH
#  define TYPE_CODE_LENGTH 6
#endif

/* The GNU C compiler does not have any of these bugs. */
#ifdef __GNUC__
#  undef HAVE_DOUBLE_TO_LONG_BUG
#  undef UNSIGNED_SHIFT_BUG
#  undef Conditional_Bug
#endif

#endif /* SCM_CONFSHARED_H */
