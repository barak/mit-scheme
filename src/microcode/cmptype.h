/* -*-C-*-

$Id: cmptype.h,v 1.3 2000/12/05 21:23:43 cph Exp $

Copyright (c) 1993, 1999, 2000 Massachusetts Institute of Technology

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

9	Intel IA-32 architecture.
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
#define COMPILER_IA32_TYPE			9
#define COMPILER_ALPHA_TYPE			10
#define COMPILER_MIPS_TYPE			11
#define COMPILER_LOSING_C_TYPE			12

#endif /* CMPTYPE_H_INCLUDED */
