/* -*-C-*-

$Id: 8f086e50390cc2a9b1261ec0713086a88eba9560 $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Declarations of user offsets into the Fixed Objects Vector.
   This should correspond to the file "utabmd.scm". */

#define Non_Object		0x00	/* Used for unassigned variables. */
#define System_Interrupt_Vector	0x01	/* Handlers for interrupts. */
#define System_Error_Vector	0x02	/* Handlers for errors. */
#define OBArray			0x03	/* Array for interning symbols. */
#define Types_Vector		0x04	/* Type number -> Name map. */
#define Returns_Vector		0x05	/* Return code -> Name map. */

/* For each interrupt, an interrupt mask to be set when invoking the
   handler for that interrupt.  */
#define FIXOBJ_INTERRUPT_MASK_VECTOR	0x06

#define Errors_Vector		0x07	/* Error code -> Name map. */
#define Identification_Vector	0x08	/* ID Vector index -> name map. */
#define FIXOBJ_SYSTEM_CALL_NAMES	0x09	/* System call index -> name */
#define FIXOBJ_SYSTEM_CALL_ERRORS	0x0A	/* System call error -> name */
#define GC_Daemon		0x0B	/* Procedure to run after GC. */
#define Trap_Handler		0x0C	/* Abort after disaster. */
#define FIXOBJ_EDWIN_AUTO_SAVE	0x0D	/* Files to save if fatal error. */
#define Stepper_State		0x0E	/* UNUSED in CScheme. */
#define Fixed_Objects_Slots	0x0F	/* Names of these slots. */
#define FIXOBJ_FILES_TO_DELETE	0x10	/* Temporary files to delete. */
#define State_Space_Tag		0x11	/* Tag for state spaces. */
#define State_Point_Tag		0x12	/* Tag for state points. */
#define Dummy_History		0x13	/* Empty history structure. */
#define Bignum_One              0x14    /* Cache for bignum one. */
#define System_Scheduler	0x15	/* MultiScheme:
					   Scheduler for touched futures. */
#define Termination_Vector	0x16    /* Names for terminations. */
#define Termination_Proc_Vector	0x17	/* Handlers for terminations. */
#define Me_Myself		0x18	/* MultiScheme:
					   The shared fixed objects vector. */
#define The_Work_Queue		0x19	/* MultiScheme:
					   Where work is stored. */
#define Future_Logger           0x1A    /* MultiScheme: When logging futures,
					   routine to log touched futures. */
#define Touched_Futures         0x1B    /* MultiScheme: When logging futures,
					   vector of touched futures. */
#define Precious_Objects	0x1C	/* Objects that should not be lost! */
#define Error_Procedure		0x1D	/* User invoked error handler. */
#define Unsnapped_Link		0x1E    /* UNUSED in CScheme. */
#define Utilities_Vector	0x1F	/* UNUSED in CScheme. */
#define Compiler_Err_Procedure  0x20	/* User invoked error handler
					   from compiled code. */
#define Lost_Objects_Base 	0x21	/* Free at the end of the "real" gc. */
#define State_Space_Root	0x22 	/* Root of state space. */
#define Primitive_Profiling_Table 0x23	/* Table of profile counts for
					   primitives. */

/* Trampolines for various generic arithmetic operations.
   These facilitate upwards compatibility and simplify compilation. 
 */

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

#define COMPILED_CODE_BKPT_HANDLER	0x3F /* Procedure to invoke when
						compiled code hits a
						breakpoint.
					      */

#define GC_WABBIT_DESCRIPTOR		0x40 /* #F or a vector of 4 elements:
						- A boolean flag
						- A vector of objects to find
						- A vector to fill with
						  references.
						- A boolean flag = do you want
						  a vector of all obj heads
						  returned in this slot. If so,
						  slot 0 will be a boolean flag
						  indicating if there may be more.
					      */

#define Linker_Cache_Generator		0x41 /* Procedure for generating
						compiled code substitutes
						for tramplines.
					     */

#define NFixed_Objects			0x42
