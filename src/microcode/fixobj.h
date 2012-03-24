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

/* Declarations of user offsets into the Fixed Objects Vector.  */

#define NON_OBJECT		0x00	/* Used for unassigned variables. */
#define SYSTEM_INTERRUPT_VECTOR	0x01	/* Handlers for interrupts. */
#define SYSTEM_ERROR_VECTOR	0x02	/* Handlers for errors. */
#define OBARRAY			0x03	/* Array for interning symbols. */
#define TYPES_VECTOR		0x04	/* Type number -> Name map. */
#define RETURNS_VECTOR		0x05	/* Return code -> Name map. */

/* For each interrupt, an interrupt mask to be set when invoking the
   handler for that interrupt.  */
#define FIXOBJ_INTERRUPT_MASK_VECTOR	0x06

#define ERRORS_VECTOR		0x07	/* Error code -> Name map. */
#define IDENTIFICATION_VECTOR	0x08	/* ID Vector index -> name map. */
#define FIXOBJ_SYSTEM_CALL_NAMES	0x09	/* System call index -> name */
#define FIXOBJ_SYSTEM_CALL_ERRORS	0x0A	/* System call error -> name */
#define GC_DAEMON		0x0B	/* Procedure to run after GC. */
#define TRAP_HANDLER		0x0C	/* Abort after disaster. */
#define FIXOBJ_EDWIN_AUTO_SAVE	0x0D	/* Files to save if fatal error. */
#define STEPPER_STATE		0x0E
#define FIXED_OBJECTS_SLOTS	0x0F	/* Names of these slots. */
#define FIXOBJ_FILES_TO_DELETE	0x10	/* Temporary files to delete. */
/* #define UNUSED		0x11 */
/* #define UNUSED		0x12 */
#define DUMMY_HISTORY		0x13	/* Empty history structure. */
#define Bignum_One              0x14    /* Cache for bignum one. */
/* #define UNUSED		0x15 */
#define Termination_Vector	0x16    /* Names for terminations. */
#define Termination_Proc_Vector	0x17	/* Handlers for terminations. */
/* #define UNUSED		0x18 */
/* #define UNUSED		0x19 */
/* #define UNUSED		0x1A */
/* #define UNUSED		0x1B */
/* #define UNUSED		0x1C */
#define Error_Procedure		0x1D	/* User invoked error handler. */
/* #define UNUSED		0x1E */
/* #define UNUSED		0x1F */
#define CC_ERROR_PROCEDURE	0x20	/* Error handler for compiled code. */
/* #define UNUSED	 	0x21 */
/* #define UNUSED		0x22 */
#define Primitive_Profiling_Table 0x23	/* Table of profile counts for
					   primitives. */

/* Trampolines for various generic arithmetic operations.
   These facilitate upwards compatibility and simplify compilation.  */

#define GENERIC_TRAMPOLINE_ZERO_P	0x24
#define GENERIC_TRAMPOLINE_POSITIVE_P	0x25
#define GENERIC_TRAMPOLINE_NEGATIVE_P	0x26
#define GENERIC_TRAMPOLINE_SUCCESSOR	0x27
#define GENERIC_TRAMPOLINE_PREDECESSOR	0x28
#define GENERIC_TRAMPOLINE_EQUAL_P	0x29
#define GENERIC_TRAMPOLINE_LESS_P	0x2A
#define GENERIC_TRAMPOLINE_GREATER_P	0x2B
#define GENERIC_TRAMPOLINE_ADD		0x2C
#define GENERIC_TRAMPOLINE_SUBTRACT	0x2D
#define GENERIC_TRAMPOLINE_MULTIPLY	0x2E
#define GENERIC_TRAMPOLINE_DIVIDE	0x2F
#define GENERIC_TRAMPOLINE_QUOTIENT	0x30
#define GENERIC_TRAMPOLINE_REMAINDER	0x31
#define GENERIC_TRAMPOLINE_MODULO	0x32

#define ARITY_DISPATCHER_TAG		0x33

/* Descartes profiling tables */

#define PC_Sample_Builtin_Table		0x34 /* ``built in'' assembly code */
#define PC_Sample_Utility_Table		0x35 /* Foreign func'n utilities */
#define PC_Sample_Primitive_Table	0x36 /* Primitive proc samples */

#define PC_Sample_Code_Block_Table	0x37 /* Compiled  proc samples */

#define PC_Sample_PCB_Block_Buffer	0x38 /* Double buffer pure compiled */
#define PC_Sample_PCB_Offset_Buffer	0x39 /* Double buffer pure comp offs */
#define PC_Sample_HCB_Block_Buffer	0x3A /* Double buffer heathen comps */
#define PC_Sample_HCB_Offset_Buffer	0x3B /* Double buffer heathen comps */

#define PC_Sample_Interp_Proc_Buffer	0x3C /* Double buffer interp procs */

#define PC_Sample_Prob_Comp_Table	0x3D /* Sure looked compiled ?! */
#define PC_Sample_UFO_Table		0x3E /* Invalid ENV at sample time  */

#define CC_BKPT_PROCEDURE		0x3F /* Procedure to invoke when
						compiled code hits a
						breakpoint.  */
/* #F or a vector of 4 elements:
   - A boolean flag
   - A vector of objects to find
   - A vector to fill with references
   - A boolean flag = do you want a vector of all obj heads returned
     in this slot. If so, slot 0 will be a boolean flag indicating if
     there may be more.  */

#define GC_WABBIT_DESCRIPTOR		0x40

#define CALLBACK_HANDLER		0x41

/* 3 extra slots for expansion and debugging.  */
#define N_FIXED_OBJECTS			0x45

#define FIXED_OBJECTS_NAMES						\
{									\
  /* 0x00 */	"non-object",						\
  /* 0x01 */	"system-interrupt-vector",				\
  /* 0x02 */	"system-error-vector",					\
  /* 0x03 */	"obarray",						\
  /* 0x04 */	"microcode-types-vector",				\
  /* 0x05 */	"microcode-returns-vector",				\
  /* 0x06 */	"interrupt-mask-vector",				\
  /* 0x07 */	"microcode-errors-vector",				\
  /* 0x08 */	"microcode-identification-vector",			\
  /* 0x09 */	"system-call-names",					\
  /* 0x0A */	"system-call-errors",					\
  /* 0x0B */	"gc-daemon",						\
  /* 0x0C */	"trap-handler",						\
  /* 0x0D */	"edwin-auto-save",					\
  /* 0x0E */	"stepper-state",					\
  /* 0x0F */	"microcode-fixed-objects-slots",			\
  /* 0x10 */	"files-to-delete",					\
  /* 0x11 */	0,							\
  /* 0x12 */	0,							\
  /* 0x13 */	"dummy-history",					\
  /* 0x14 */	"bignum-one",						\
  /* 0x15 */	0,							\
  /* 0x16 */	"microcode-terminations-vector",			\
  /* 0x17 */	"microcode-terminations-procedures",			\
  /* 0x18 */	0,							\
  /* 0x19 */	0,							\
  /* 0x1A */	0,							\
  /* 0x1B */	0,							\
  /* 0x1C */	0,							\
  /* 0x1D */	"error-procedure",					\
  /* 0x1E */	0,							\
  /* 0x1F */	0,							\
  /* 0x20 */	"compiler-error-procedure",				\
  /* 0x21 */	0,							\
  /* 0x22 */	0,							\
  /* 0x23 */	"primitive-profiling-table",				\
  /* 0x24 */	"generic-trampoline-zero?",				\
  /* 0x25 */	"generic-trampoline-positive?",				\
  /* 0x26 */	"generic-trampoline-negative?",				\
  /* 0x27 */	"generic-trampoline-add-1",				\
  /* 0x28 */	"generic-trampoline-subtract-1",			\
  /* 0x29 */	"generic-trampoline-equal?",				\
  /* 0x2A */	"generic-trampoline-less?",				\
  /* 0x2B */	"generic-trampoline-greater?",				\
  /* 0x2C */	"generic-trampoline-add",				\
  /* 0x2D */	"generic-trampoline-subtract",				\
  /* 0x2E */	"generic-trampoline-multiply",				\
  /* 0x2F */	"generic-trampoline-divide",				\
  /* 0x30 */	"generic-trampoline-quotient",				\
  /* 0x31 */	"generic-trampoline-remainder",				\
  /* 0x32 */	"generic-trampoline-modulo",				\
  /* 0x33 */	"arity-dispatcher-tag",					\
  /* 0x34 */	"pc-sample/builtin-table"				\
  /* 0x35 */	"pc-sample/utility-table",				\
  /* 0x36 */	"pc-sample/primitive-table",				\
  /* 0x37 */	"pc-sample/code-block-table",				\
  /* 0x38 */	"pc-sample/purified-code-block-block-buffer",		\
  /* 0x39 */	"pc-sample/purified-code-block-offset-buffer",		\
  /* 0x3A */	"pc-sample/heathen-code-block-block-buffer",		\
  /* 0x3B */	"pc-sample/heathen-code-block-offset-buffer",		\
  /* 0x3C */	"pc-sample/interp-proc-buffer",				\
  /* 0x3D */	"pc-sample/prob-comp-table",				\
  /* 0x3E */	"pc-sample/ufo-table",					\
  /* 0x3F */	"compiled-code-bkpt-handler",				\
  /* 0x40 */	"gc-wabbit-descwiptor",					\
  /* 0x41 */	"callback-handler",					\
  /* 0x42 */	0,							\
  /* 0x43 */	0,							\
  /* 0x44 */	0,							\
  /* 0x45 */	0							\
}
