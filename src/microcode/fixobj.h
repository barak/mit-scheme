/* -*-C-*-

$Id: fixobj.h,v 9.44 2008/01/30 20:02:12 cph Exp $

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

/* Declarations of user offsets into the Fixed Objects Vector.
   This should correspond to the file "utabmd.scm".  */

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
#define State_Space_Tag		0x11	/* Tag for state spaces. */
#define State_Point_Tag		0x12	/* Tag for state points. */
#define DUMMY_HISTORY		0x13	/* Empty history structure. */
#define Bignum_One              0x14    /* Cache for bignum one. */
/* #define UNUSED		0x15 */
#define Termination_Vector	0x16    /* Names for terminations. */
#define Termination_Proc_Vector	0x17	/* Handlers for terminations. */
/* #define UNUSED		0x18 */
/* #define UNUSED		0x19 */
/* #define UNUSED		0x1A */
/* #define UNUSED		0x1B */
#define Precious_Objects	0x1C	/* Objects that should not be lost! */
#define Error_Procedure		0x1D	/* User invoked error handler. */
/* #define UNUSED		0x1E */
/* #define UNUSED		0x1F */
#define CC_ERROR_PROCEDURE	0x20	/* Error handler for compiled code. */
#define Lost_Objects_Base 	0x21	/* Free at the end of the "real" gc. */
#define State_Space_Root	0x22 	/* Root of state space. */
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

/* 4 extra slots for expansion and debugging.  */
#define N_FIXED_OBJECTS			0x45
