/* -*-C-*-

$Id: dosexcp.h,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

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

#ifndef _DOSEXCP_H_
#  define _DOSEXCP_H_

#define NUM_DOS_EXCP					18

#define DOS_INVALID_TRAP				-1

#define DOS_EXCP_Integer_divide_by_zero			0
#define DOS_EXCP_Debug_exception			1
#define DOS_EXCP_Non_maskable_interrupt			2
#define DOS_EXCP_Breakpoint				3
#define DOS_EXCP_Integer_overflow			4
#define DOS_EXCP_Bounds_check				5
#define DOS_EXCP_Invalid_opcode				6
#define DOS_EXCP_Numeric_co_processor_not_available	7
#define DOS_EXCP_Double_fault				8
#define DOS_EXCP_Numeric_co_processor_segment_overrun	9
  /* ^ can only occur on an FP-less chip (386 or 486SX). */
#define DOS_EXCP_Invalid_TSS				10
#define DOS_EXCP_Segment_not_present			11
#define DOS_EXCP_Stack_exception			12
#define DOS_EXCP_General_protection			13
#define DOS_EXCP_Page_Fault				14
  /* 15 is reserved by Intel. */
#define DOS_EXCP_Floating_point_exception		16
#define DOS_EXCP_Alignment_check			17
  /* 18-31 are reserved by Intel. */

struct sigcontext
{
  unsigned sc_eax;
  unsigned sc_ecx;
  unsigned sc_edx;
  unsigned sc_ebx;
  unsigned sc_esp;
  unsigned sc_ebp;
  unsigned sc_esi;
  unsigned sc_edi;
  unsigned sc_eip;
  unsigned sc_eflags;
  unsigned sc_cs;
  unsigned sc_ss;
  unsigned sc_ds;
  unsigned sc_es;
  unsigned sc_fs;
  unsigned sc_gs;
};

extern int
  DPMI_get_exception_vector (unsigned exception,
			     unsigned short * cs_selector,
			     unsigned * code_offset);

extern int
  DPMI_set_exception_vector (unsigned exception,
			     unsigned short cs_selector,
			     unsigned code_offset);

extern int
  DPMI_set_exception_handler (unsigned exception,
			      void (*funcptr) (unsigned,
					       unsigned,
					       struct sigcontext *),
			      void * stack);

extern int
  DPMI_restore_exception_handler (unsigned exception,
				  unsigned short cs_selector,
				  unsigned code_offset);


extern int
  X32_get_exception_vector (unsigned exception,
			    unsigned short * cs_selector,
			    unsigned * code_offset);

extern int
  X32_set_exception_handler (unsigned exception,
			     void (*funcptr) (unsigned,
					      unsigned,
					      struct sigcontext *),
			     void * stack);

extern int
  X32_restore_exception_handler (unsigned exception,
				 unsigned short cs_selector,
				 unsigned code_offset);

#endif /* _DOSEXCP_H_ */
