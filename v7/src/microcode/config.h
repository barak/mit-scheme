/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/config.h,v 9.68 1992/02/20 16:30:07 jinx Exp $

Copyright (c) 1987-1992 Massachusetts Institute of Technology

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

/* This file contains the configuration information and the information
   given on the command line on Unix. */

/* Default pathnames. */

#ifndef butterfly
#ifndef unix
/* On unix, these are part of the make file. */

/* Runtime debugging flags, with appropriate defaults: */

/* To debug the interpreter code itself, define ENABLE_DEBUGGING_TOOLS */
/* #define ENABLE_DEBUGGING_TOOLS */

/* If runtime HISTORY recording (a Scheme code debugging tool) is not
   desired. */
/* #define DISABLE_HISTORY */

/* To enable the STEPPER.  Incompatible with futures. */
#define COMPILE_STEPPER 

/* To enable FUTURES (a multiprocessor / multiprocessing extension).
   This option is incompatible with the stepper.
   Future.c must also be compiled. */
/* #define COMPILE_FUTURES */

/* To enable stacklets (mostly useful with FUTURES).  These allow the
   stack to be allocated in small chunks from the heap, rather than
   in a single contiguous area at start up time. The use of the this
   option is incompatible with the stepper and compiler. */

/* #define USE_STACKLETS */
#endif
#endif

/* Some configuration consistency testing */

#ifdef COMPILE_STEPPER
#ifdef COMPILE_FUTURES
#include "Error: Futures and stepping are not currently compatible."
#endif
#endif

#ifdef USE_STACKLETS
#ifdef COMPILE_STEPPER
#include "Error: The stepper doesn't work with stacklets."
#endif
#endif

/* For use in the C pre-processor, not in code! */
#define FALSE			0
#define TRUE			1

/* These C type definitions are needed by everybody.
   They should not be here, but it is unavoidable. */
typedef char Boolean;
#define true			((Boolean) TRUE)
#define false			((Boolean) FALSE)

/* This is the Scheme object type.
   The various fields are defined in "object.h". */
typedef unsigned long SCHEME_OBJECT;
#define OBJECT_LENGTH (CHAR_BIT * (sizeof (unsigned long)))

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

   VAX_BYTE_ORDER should be defined ONLY if the least significant byte
   of a longword in memory lies at the lowest address, not defined
   otherwise (i.e. Motorola MC68020, with opposite convention, or
   PDP-10 with word addressing).

   Other flags (the safe option is NOT to define them, which will
   sacrifice speed for safety):

   HEAP_IN_LOW_MEMORY should be defined if malloc returns the lowest
   available memory and thus all addresses will fit in the datum portion
   of a Scheme object.  The datum portion of a Scheme object is 8 bits
   less than the length of a C long.

   b32 says that objects are 32 bits long.  The information is
   redundant, but some C compilers do not do constant folding when
   shifts are involved, so it sometimes makes a big difference to
   define the constants directly rather than in terms of other
   constants.  Similar things can be done for other word sizes.

   The following switches say whether to use the system-provided
   library routines rather than the emulated versions in the Scheme
   sources.  The library routines should be more accurate and probably
   more efficient.

   HAS_FLOOR should be defined if the system has the double-precision
   procedures floor and ceil.  On Unix, look for floor(3M).

   HAS_FREXP should be defined if the system has the double-precision
   procedures ldexp and frexp.  On Unix, look for frexp(3C).

   HAS_MODF should be defined if the system has the double-precision
   procedure modf.  On Unix, look for frexp(3C).  **** This flag is
   new as of 22-SEP-89; please comment out any incorrect #define's as
   we haven't been able to test this on all machines.

*/

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
#define FASL_I386		6
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

#ifdef vax

/* Amazingly unix and vms agree on all these */

#define MACHINE_TYPE		"vax"
#define FASL_INTERNAL_FORMAT	FASL_VAX
#define VAX_BYTE_ORDER
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF

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
#define void
#endif /* VMS_VERSION */

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

#ifdef hpux

#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF

#ifdef hp9000s300
#define MACHINE_TYPE		"hp9000s300"
#ifdef MC68010
#define FASL_INTERNAL_FORMAT	FASL_68000
#else
#define FASL_INTERNAL_FORMAT	FASL_68020
#endif
#define b32
#define HEAP_IN_LOW_MEMORY
#endif /* hp9000s300 */

#ifdef hp9000s800
#define MACHINE_TYPE		"hp9000s800"
#define FASL_INTERNAL_FORMAT	FASL_HP_SPECTRUM
#define FLOATING_ALIGNMENT	0x7
#define b32

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

#endif /* hp9000s800 */

#ifdef hp9000s500
#define MACHINE_TYPE		"hp9000s500"
#define FASL_INTERNAL_FORMAT 	FASL_HP_9000_500
#define b32

/* An unfortunate fact of life on this machine:
   the C heap is in high memory thus HEAP_IN_LOW_MEMORY is not
   defined and the whole thing runs slowly.  */

/* C Compiler bug when constant folding and anchor pointing */
#define And2(x, y)	((x) ? (y) : false)
#define And3(x, y, z)	((x) ? ((y) ? (z) : false) : false)
#define Or2(x, y)	((x) ? true : (y))
#define Or3(x, y, z)	((x) ? true : ((y) ? true : (z)))

#endif /* hp9000s500 */

#endif /* hpux */

#ifdef sparc
#define MACHINE_TYPE		"sun4"
#define FASL_INTERNAL_FORMAT	FASL_SPARC
#define FLOATING_ALIGNMENT	0x7
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef sun3
#define MACHINE_TYPE		"sun3"
#define FASL_INTERNAL_FORMAT	FASL_68020
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef sun2
#define MACHINE_TYPE		"sun2"
#define FASL_INTERNAL_FORMAT	FASL_68000
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef NeXT
#define MACHINE_TYPE		"next"
#define FASL_INTERNAL_FORMAT	FASL_68020
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#endif

#ifdef i386

#define FASL_INTERNAL_FORMAT	FASL_I386
#define VAX_BYTE_ORDER
#define b32

#ifdef sequent
#define MACHINE_TYPE		"sequent386"
#else /* not sequent */
#ifdef sun
#define MACHINE_TYPE		"sun386i"
#else /* not sun */
#define MACHINE_TYPE		"i386"
#endif /* not sun */
#endif /* not sequent */

/* These are really OS-dependent.  They are correct for the sequent
   and for SYSV3, but we don't know about other 386 systems. */
#define HEAP_IN_LOW_MEMORY
#ifndef _MACH_UNIX
/* Bug in Mach 3.0 for 386s floating point library. */
#  define HAS_FLOOR
#  define HAS_FREXP
#endif

#endif /* i386 */

#ifdef mips

#define MACHINE_TYPE		"mips"
#define FASL_INTERNAL_FORMAT	FASL_MIPS
#define FLOATING_ALIGNMENT   	0x7
#define b32

#ifdef ultrix
#define VAX_BYTE_ORDER
#else
#ifdef MIPSEL
#define VAX_BYTE_ORDER
#endif
#endif

#ifdef sony
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
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

#ifdef alpha
#define MACHINE_TYPE           "Alpha"
#define FASL_INTERNAL_FORMAT   FASL_ALPHA

/* The ASCII character set is used. */
#define HEAP_IN_LOW_MEMORY     1
/* unsigned longs use logical shifting. */
#define VAX_BYTE_ORDER         1

/* Flonums have no special alignment constraints. */
#define FLONUM_MANTISSA_BITS   53
#define FLONUM_EXPT_SIZE       10
#define MAX_FLONUM_EXPONENT    1023
/* Floating point representation uses hidden bit. */
#define HAS_FLOOR
/* #define HAS_FREXP */
#define HAS_MODF
#endif /* alpha */

/* These (pdp10, nu) haven't worked in a while.
   Should be upgraded or flushed some day.  */

#ifdef pdp10
#define MACHINE_TYPE		"pdp10"
#define FASL_INTERNAL_FORMAT    FASL_PDP10
#define HEAP_IN_LOW_MEMORY
#define CHAR_BIT 36		/ * Ugh! Supposedly fixed in newer Cs * /
#define UNSIGNED_SHIFT_BUG
#endif /* pdp10 */

#ifdef nu
#define MACHINE_TYPE		"nu"
#define FASL_INTERNAL_FORMAT	FASL_68000
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FREXP
#define UNSIGNED_SHIFT_BUG
#endif /* nu */

/* These are pretty old too, but more recent versions have run. */

#ifdef butterfly
#define MACHINE_TYPE		"butterfly"
#define FASL_INTERNAL_FORMAT	FASL_BFLY
#define b32
#define HEAP_IN_LOW_MEMORY
#include <public.h>
#define HAS_FREXP
#define HAS_MODF
#endif

#ifdef cyber180
#define MACHINE_TYPE		"cyber180"
#define FASL_INTERNAL_FORMAT	FASL_CYBER
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT_BUG
/* The Cyber180 C compiler manifests a bug in hairy conditional expressions */
#define Conditional_Bug
#endif /* cyber180 */

#ifdef celerity
#define MACHINE_TYPE		"celerity"
#define FASL_INTERNAL_FORMAT	FASL_CELERITY
#define b32
#define HEAP_IN_LOW_MEMORY
#endif /* celerity */

#ifdef umax
#define MACHINE_TYPE		"umax"
#define FASL_INTERNAL_FORMAT	FASL_UMAX
#define VAX_BYTE_ORDER
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#endif /* umax */

#ifdef pyr
#define MACHINE_TYPE		"pyramid"
#define FASL_INTERNAL_FORMAT	FASL_PYR
#define b32
#define HEAP_IN_LOW_MEMORY
#endif /* pyr */

#ifdef alliant
#define MACHINE_TYPE		"alliant"
#define FASL_INTERNAL_FORMAT	FASL_ALLIANT
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#endif /* alliant */

#ifdef apollo
#if _ISP__M68K
#define MACHINE_TYPE          "Apollo 68k"
#define FASL_INTERNAL_FORMAT  FASL_APOLLO_68K
#else
#define MACHINE_TYPE          "Apollo Prism"
#define FASL_INTERNAL_FORMAT  FASL_APOLLO_PRISM
#endif
#define b32
#define HEAP_IN_LOW_MEMORY
#define HAS_FLOOR
#define HAS_FREXP
#endif /* apollo */

/* Make sure that some definition applies.  If this error occurs, and
   the parameters of the configuration are unknown, try the Wsize
   program.  */
#ifndef MACHINE_TYPE
#include "Error: config.h: Unknown configuration."
#endif

/* Virtually all machines have 8-bit characters these days, so don't
   explicitly specify this value unless it is different.  */
#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif

/* The GNU C compiler does not have any of these bugs. */
#ifdef __GNUC__
#undef HAVE_DOUBLE_TO_LONG_BUG
#undef UNSIGNED_SHIFT_BUG
#undef Conditional_Bug
#endif
