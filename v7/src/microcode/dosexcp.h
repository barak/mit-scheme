/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosexcp.h,v 1.1 1992/05/05 06:55:13 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
				  

#endif /* _DOSEXCP_H_ */
