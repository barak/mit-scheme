/* -*-C-*-

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fixobj.h,v 9.27 1989/03/27 23:15:06 jinx Rel $
 *
 * Declarations of user offsets into the Fixed Objects Vector.
 * This should correspond to the file UTABMD.SCM
 */

#define Non_Object		0x00	/* Used for unassigned variables. */
#define System_Interrupt_Vector	0x01	/* Handlers for interrups. */
#define System_Error_Vector	0x02	/* Handlers for errors. */
#define OBArray			0x03	/* Array for interning symbols. */
#define Types_Vector		0x04	/* Type number -> Name map. */
#define Returns_Vector		0x05	/* Return code -> Name map. */
#define Primitives_Vector	0x06	/* Primitive code -> Name map. */
#define Errors_Vector		0x07	/* Error code -> Name map. */
#define Identification_Vector	0x08	/* ID Vector index -> name map. */
/* UNUSED slot			0x09 */
/* UNUSED slot			0x0A */
#define GC_Daemon		0x0B	/* Procedure to run after GC. */
#define Trap_Handler		0x0C	/* Abort after disaster. */
/* UNUSED slot			0x0D */
#define Stepper_State		0x0E	/* UNUSED in CScheme. */
#define Fixed_Objects_Slots	0x0F	/* Names of these slots. */
/* UNUSED slot			0x10 used to be
   External_Primitives			   Names of external prims. */
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

#define NFixed_Objects		0x24

