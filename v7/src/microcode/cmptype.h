/* -*-C-*-

$Id: cmptype.h,v 1.1 1993/06/24 06:52:52 gjr Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

/*
 *
 * Processor type definitions
 *
 */

/*
This file contains the associations between processor numbers and
their descriptions.  This file should only be modified by the Scheme
project at MIT, to avoid conflicts.

These numbers are the numbers stored in Scheme images (bands) so that
the microcode can detect whether the compiled code in the band is
compatible with it.

These _are not_ the same numbers as the PROC_TYPE_XXX used to 
configure the microcode under Unix (cf.h)---they probably should be.

Number	Description
______	___________

0	No compiled code support

1	Motorola MC68020 with MC68881 floating point coprocessor,
	or MC68030 (not MC68040).
	Examples: HP series 9000 models 320, 350, 370, 375
		  Sun models 340, 360

2	DEC Vax
	Examples: Vax-11 750
		  MicroVax II
		  VaxStation 3100

3	HP Precision architecture (version 1.0 and later).
	Examples: HP series 9000 models 850, 835, 720, 750, 710, 877.

4	MIPS R2000/R3000 with cache line <= 16 bytes.
	Examples: DecStation 3100, 2100, 5000/200.
		  Sony News 3250

5	Motorola MC68020-MC68040
	Examples: HP series 9000 models 320-380, models 400+
		  All Next computers (up to Aug. 1992).

6	Sun Sparc
	Examples: Sun 4, SparcStation 2, IPC, ELC.
		  Solbourne ?

7	IBM POWER and POWER/PC architecture.
	Examples: IBM RS6000 model 560.

8	Motorola 88000 architecture (88100 and 88110).
	Examples: ?

9	Intel i386/i486/Pentium architecture.
	Examples: IBM PC AT clones with 386+ processors.

10	DEC Alpha architecture
	Examples: DEC AXP 500

11	MIPS R200-R4000 (32 bit) with arbitrary cache line size.
	Examples: DecStation 2100, 3100, 5000/200, 5000/240
		  Sony News 3250
		  Silicon Graphics Predator, Indigo, and Crimson

12	Virtual C processor.
	The Scheme compiler produces C to be compiled by the
	same C compiler as the microcode.

*/

#ifndef CMPTYPE_H_INCLUDED
#define CMPTYPE_H_INCLUDED

#define COMPILER_NONE_TYPE			0
#define COMPILER_MC68020_TYPE			1
#define COMPILER_VAX_TYPE			2
#define COMPILER_SPECTRUM_TYPE			3
#define COMPILER_OLD_MIPS_TYPE			4
#define COMPILER_MC68040_TYPE			5
#define COMPILER_SPARC_TYPE			6
#define COMPILER_RS6000_TYPE			7
#define COMPILER_MC88K_TYPE			8
#define COMPILER_I386_TYPE			9
#define COMPILER_ALPHA_TYPE			10
#define COMPILER_MIPS_TYPE			11
#define COMPILER_LOSING_C_TYPE			12

#endif /* CMPTYPE_H_INCLUDED */
