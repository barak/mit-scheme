/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/config.h,v 9.55 1990/06/20 17:39:15 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

/* If runtime HISTORY recording (a Scheme code debugging tool) is desired. */
#define COMPILE_HISTORY

/* To enable the STEPPER.  Incompatible with futures. */
/* #define COMPILE_STEPPER */

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

/* These C type definitions are needed by everybody.
   They should not be here, but it is unavoidable. */
typedef char Boolean;
#define true			1
#define false			0

/* This is the Scheme object type; it should be called `SCHEME_OBJECT'.
   The various fields are defined in "object.h". */
typedef unsigned long SCHEME_OBJECT;

/* This definition makes the value of `OBJECT_LENGTH' available to
   the preprocessor. */
#define OBJECT_LENGTH ULONG_SIZE

/* Operating System / Machine dependencies:

   For each implementation, be sure to specify FASL_INTERNAL_FORMAT,
   the various sizes, and the floating point information.
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

   USHORT_SIZE is the size of an unsigned short in bits.  It should
   be equivalent to (sizeof(unsigned short) * CHAR_BIT), but is
   available to the preprocessor.

   ULONG_SIZE is the size of an unsigned long in bits.

   ******** The following flonum constants have been superseded by the
   use of "float.h". ********

   FLONUM_EXPT_SIZE is the number of bits in the largest positive
   exponent of a (double) floating point number.
   Note that if excess exponents are used in the representation,
   this number is one less than the size in bits of the exponent field.

   FLONUM_MANTISSA_BITS is the number of bits in the (positive) mantissa
   of a (double) floating point number.  It includes the hidden bit if
   the representation uses them.

   Thus 2+FLONUM_EXPT_SIZE+FLONUM_MANTISSA_BITS(-1 if hidden bit is
   used) should be no greater than the size in bits of a (double)
   floating point number.  Note that
   MAX_FLONUM_EXPONENT = (2^FLONUM_EXPONENT_SIZE) - 1

   FLOATING_ALIGNMENT should be defined ONLY if the system requires
   floating point numbers (double) to be aligned more strictly than
   SCHEME_OBJECTs (long).  The value must be a mask of the low order
   bits which are required to be zero for the storage address.  For
   example, a value of 0x7 requires octabyte alignment on a machine
   where addresses are specified in bytes.  The alignment must be an
   integral multiple of the length of a long.

   VAX_BYTE_ORDER should be defined ONLY if the least significant byte
   of a longword in memory lies at the lowest address, not defined
   otherwise (ie. Motorola MC68020, with opposite convention, or
   PDP-10 with word addressing). */

/*
   Other flags (the safe option is NOT to define them, which will
   sacrifice speed for safety):

   b32 should be defined for machines whose word size
   (CHAR_BIT*sizeof(long)) is 32 bits.  The information is redundant,
   but some C compilers do not do constant folding when shifts are
   involved, so it sometimes makes a big difference to define the
   constants directly rather than in terms of other constants.
   Similar things can be done for other word sizes.

   HEAP_IN_LOW_MEMORY should be defined if malloc returns the lowest
   available memory and thus all addresses will fit in the datum portion
   of a Scheme object.  The datum portion of a Scheme object is 8 bits
   less than the length of a C long.

   UNSIGNED_SHIFT is defined if right shifting an unsigned long
   (i.e. SCHEME_OBJECT) results in a logical (vs. arithmetic) shift.
   Setting the flag allows faster type code extraction.

   BELL is the character which rings the terminal bell.

   The following switches are used to use the system provided library
   routines rather than the emulated versions in the Scheme sources.
   The system provided ones should be more accurate and probably more
   efficient.

   HAS_FLOOR should be defined if the system has the double precision
   procedures floor and ceil.  On Unix, look for floor(3M).

   HAS_FREXP should be defined if the system has the double precision
   procedures ldexp and frexp.  On Unix, look for frexp(3C).

   HAS_MODF should be defined if the system has the double precision
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
#define FASL_HP_9000_300	FASL_68020
#define FASL_SUN_3		FASL_68020
#define FASL_68000  		4
#define FASL_HP_9000_200	FASL_68000
#define FASL_SUN_2		FASL_68000
#define FASL_HP_9000_500	5
/* #define FASL_SUN		6 */
#define FASL_BFLY		7
#define FASL_CYBER		8
#define FASL_CELERITY		9
#define FASL_HP_SPECTRUM	10
#define FASL_UMAX		11
#define FASL_PYR		12
#define FASL_ALLIANT		13
#define FASL_SUN4		14
#define FASL_MIPS		15

/* These (pdp10 and nu) haven't worked in a while.
 * Should be upgraded or flushed some day.
 */

#ifdef pdp10
#define MACHINE_TYPE		"pdp10"
#define HEAP_IN_LOW_MEMORY
#define CHAR_BIT 36		/ * Ugh! Supposedly fixed in newer Cs * /
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT    FASL_PDP10
#endif

#ifdef nu
#define MACHINE_TYPE		"nu"
#define HEAP_IN_LOW_MEMORY
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT	FASL_68000
/* #define FLONUM_EXPT_SIZE	7 */
/* #define FLONUM_MANTISSA_BITS	56 */
/* #define MAX_FLONUM_EXPONENT	127 */
#define HAS_FREXP
#ifdef quick
/* Bignum code fails for certain variables in registers because of a
   compiler bug!
*/
#undef quick
#define quick
#endif
#endif

#ifdef vax

/* Amazingly unix and vms agree on all these */

#define MACHINE_TYPE		"vax"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define VAX_BYTE_ORDER
#define CHAR_BIT 		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL			'\007'
#define FASL_INTERNAL_FORMAT	FASL_VAX
/* #define FLONUM_EXPT_SIZE	7 */
/* #define FLONUM_MANTISSA_BITS	56    D format */
/* #define MAX_FLONUM_EXPONENT	127 */
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

#define Exit_Scheme_Declarations static jmp_buf Exit_Point

#define Init_Exit_Scheme()						\
{									\
  int Which_Way = setjmp(Exit_Point);					\
  if (Which_Way == NORMAL_EXIT)						\
    return;								\
}

#define Exit_Scheme(value)						\
if (value != 0)								\
  exit(value);								\
longjmp(Exit_Point, NORMAL_EXIT)

#else /* not VMS ie. unix */

/* Vax Unix C compiler bug */
#define HAVE_DOUBLE_TO_LONG_BUG

#endif /* VMS */
#endif /* vax */

#ifdef hp9000s300
#define MACHINE_TYPE		"hp9000s300"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#ifdef MC68020
#define FASL_INTERNAL_FORMAT	FASL_68020
#else /* not MC68020 */
#define FASL_INTERNAL_FORMAT	FASL_68000
#endif /* MC68020 */
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
/* C compiler bug in GC_Type */
#define term_type		int
#endif

#ifdef hp9000s500
#define MACHINE_TYPE		"hp9000s500"
/* An unfortunate fact of life on this machine:
   the C heap is in high memory thus HEAP_IN_LOW_MEMORY is not
   defined and the whole thing runs slowly.  *Sigh*
*/
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT 	FASL_HP_9000_500
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF

/* C Compiler bug when constant folding and anchor pointing */
#define And2(x, y)	((x) ? (y) : false)
#define And3(x, y, z)	((x) ? ((y) ? (z) : false) : false)
#define Or2(x, y)	((x) ? true : (y))
#define Or3(x, y, z)	((x) ? true : ((y) ? true : (z)))
#endif

#ifdef sun4
#define MACHINE_TYPE		"sun4"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT	FASL_SUN4
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#define FLOATING_ALIGNMENT	0x7	/* Low 3 MBZ for float storage */
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef sun3
#define MACHINE_TYPE		"sun3"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT	FASL_68020
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef sun2
#define MACHINE_TYPE		"sun2"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT	FASL_68000
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#define HAVE_DOUBLE_TO_LONG_BUG
#endif

#ifdef butterfly
#define MACHINE_TYPE		"butterfly"
#define HEAP_IN_LOW_MEMORY
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT	FASL_BFLY
/* #define FLONUM_EXPT_SIZE	7 */
/* #define FLONUM_MANTISSA_BITS	56 */
/* #define MAX_FLONUM_EXPONENT	127 */
#include <public.h>
#define HAS_FREXP
#define HAS_MODF
#define STACK_SIZE		4	/* 4K objects */
#endif

#ifdef cyber180
#define MACHINE_TYPE		"cyber180"
/* Word size is 64 bits. */
#define HEAP_IN_LOW_MEMORY
#define CHAR_BIT		8
#define USHORT_SIZE		???
#define ULONG_SIZE		???
#define BELL			'\007'
#define FASL_INTERNAL_FORMAT	FASL_CYBER
/* #define FLONUM_EXPT_SIZE	14 */
/* #define FLONUM_MANTISSA_BITS	48 */
/* Not the full range, or so the manual says. */
/* #define MAX_FLONUM_EXPONENT	4095 */
/* The Cyber180 C compiler manifests a bug in hairy conditional
   expressions */
#define Conditional_Bug
#endif

#ifdef celerity
#define MACHINE_TYPE		"celerity"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT	FASL_CELERITY
/* #define FLONUM_EXPT_SIZE	11 */
/* #define FLONUM_MANTISSA_BITS 	53 */
/* #define MAX_FLONUM_EXPONENT	2047 */
#endif

#ifdef hp9000s800
#define MACHINE_TYPE		"hp9000s800"
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT	FASL_HP_SPECTRUM
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#define FLOATING_ALIGNMENT	0x7	/* Low 3 MBZ for float storage */
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF

/* Heap resides in data space, pointed at by space register 5.
   Short pointers must have their high two bits set to 01 so that
   it is interpreted as space register 5, 2nd quadrant.

   This is kludged by the definitions below, and is still considered
   HEAP_IN_LOW_MEMORY.
  */

#define HEAP_IN_LOW_MEMORY

#define HPPA_QUAD_BIT	0x40000000

#define DATUM_TO_ADDRESS(datum)						\
((SCHEME_OBJECT *) (((unsigned long) (datum)) | HPPA_QUAD_BIT))

#define ADDRESS_TO_DATUM(address)					\
((SCHEME_OBJECT) (((unsigned long) (address)) & (~(HPPA_QUAD_BIT))))

/* HPPA compiled binaries are large! */

#ifdef HAS_COMPILER_SUPPORT
#ifndef CONSTANT_SIZE
#define CONSTANT_SIZE		600	/* Default Kcells for constant */
#endif
#endif

#ifndef COMPILER_CONSTANT_SIZE
#define COMPILER_CONSTANT_SIZE	1300
#endif

#endif /* hp9000s800 */

#ifdef umax
#define MACHINE_TYPE		"umax"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define VAX_BYTE_ORDER
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define DBFLT_SIZE		64
#define BELL			'\007'
#define FASL_INTERNAL_FORMAT	FASL_UMAX
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#endif

#ifdef pyr
#define MACHINE_TYPE		"pyramid"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL			'\007'
#define FASL_INTERNAL_FORMAT	FASL_PYR
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#endif

#ifdef alliant
#define MACHINE_TYPE		"alliant"
#define HEAP_IN_LOW_MEMORY
#define UNSIGNED_SHIFT
#define CHAR_BIT		8
#define USHORT_SIZE		16
#define ULONG_SIZE		32
#define BELL 			'\007'
#define FASL_INTERNAL_FORMAT	FASL_ALLIANT
/* #define FLONUM_EXPT_SIZE	10 */
/* #define FLONUM_MANTISSA_BITS 	53 */
/* #define MAX_FLONUM_EXPONENT	1023 */
#define HAS_FLOOR
#define HAS_FREXP
#define HAS_MODF
#endif

#ifdef mips
#define MACHINE_TYPE		"MIPS (DECStation 3100)"
#define UNSIGNED_SHIFT
#define VAX_BYTE_ORDER
#define CHAR_BIT            	8
#define USHORT_SIZE          	16
#define ULONG_SIZE           	32
/* Flonum (double) size is 64 bits. */
#define FLOATING_ALIGNMENT   	0x7
/* #define FLONUM_MANTISSA_BITS 	53 */
/* #define FLONUM_EXPT_SIZE     	10 */
/* #define MAX_FLONUM_EXPONENT  	1023 */
/* Floating point representation uses hidden bit. */
#define FASL_INTERNAL_FORMAT	FASL_MIPS
#define BELL 			'\007'

/* Heap resides in data space which begins at 0x10000000. This is
   kludged by the definitions below, and is still considered
   HEAP_IN_LOW_MEMORY.
*/

#define HEAP_IN_LOW_MEMORY
#define MIPS_DATA_BIT	0x10000000

#define DATUM_TO_ADDRESS(datum)						\
((SCHEME_OBJECT *) (((unsigned long) (datum)) | MIPS_DATA_BIT))

#define ADDRESS_TO_DATUM(address)					\
((SCHEME_OBJECT) (((unsigned long) (address)) & (~(MIPS_DATA_BIT))))

/* MIPS compiled binaries are large! */

#ifdef HAS_COMPILER_SUPPORT
#ifndef CONSTANT_SIZE
#define CONSTANT_SIZE		700	/* Default Kcells for constant */
#endif
#endif

#ifndef COMPILER_CONSTANT_SIZE
#define COMPILER_CONSTANT_SIZE	1500
#endif

#endif /* mips */

/* Make sure that some definition applies.
   If this error occurs, and the parameters of the
   configuration are unknown, try the Wsize program.
*/

#ifndef CHAR_BIT
#include "Error: config.h: Unknown configuration."
#endif

#if (ULONG_SIZE == 32)
#define b32
#endif

#ifndef MACHINE_TYPE
#define MACHINE_TYPE		"unknown"
#endif

/* Default "segment" sizes */

#ifndef STACK_SIZE
#ifndef USE_STACKLETS
#define	STACK_SIZE		100	/* Default Kcells for stack */
#else
#define STACK_SIZE		256	/* Default stacklet size */
#endif
#endif

#ifndef CONSTANT_SIZE
#define CONSTANT_SIZE		400	/* Default Kcells for constant */
#endif

#ifndef HEAP_SIZE
#define HEAP_SIZE		250	/* Default Kcells for each heap */
#endif

#ifndef COMPILER_STACK_SIZE
#define COMPILER_STACK_SIZE STACK_SIZE
#endif

#ifndef COMPILER_HEAP_SIZE
#define COMPILER_HEAP_SIZE	1000
#endif

#ifndef COMPILER_CONSTANT_SIZE
#define COMPILER_CONSTANT_SIZE	1000
#endif
