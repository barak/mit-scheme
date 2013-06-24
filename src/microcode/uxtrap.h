/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

#ifndef SCM_UXTRAP_H
#define SCM_UXTRAP_H

#include "os.h"

/* Machine/OS-dependent section (long) */

#if defined(hp9000s300) || defined(__hp9000s300)

#include <sys/sysmacros.h>
#include <machine/sendsig.h>
#include <machine/reg.h>

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T		struct full_sigcontext
#define SIGCONTEXT_NREGS		GPR_REGS /* Missing sp */
#define SIGCONTEXT_FIRST_REG(scp)	(&((scp)->fs_regs[GPR_START]))
#define SIGCONTEXT_SP(scp)		((scp)->fs_context.sc_sp)
#define SIGCONTEXT_PC(scp)		((scp)->fs_context.sc_pc)
#define SIGCONTEXT_RFREE(scp)		((scp)->fs_regs[AR5])

#define INITIALIZE_UX_SIGNAL_CODES()					\
{									\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), 0, "software floating point exception");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), 5, "integer divide by zero");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (1L << 15), (1L << 15), "branch/set on unordered");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (1L << 14), (1L << 14), "signalling NAN");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (1L << 13), (1L << 13), "operand error");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (1L << 12), (1L << 12), "overflow");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (1L << 11), (1L << 11), "underflow");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (1L << 10), (1L << 10), "divide by zero");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (1L << 9), (1L << 9), "inexact operation");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (1L << 8), (1L << 8), "inexact decimal input");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), 0, "illegal instruction");				\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), 6, "check instruction");				\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), 7, "TRAPV instruction");				\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), 8, "privileged instruction");			\
}

#endif /* hp9000s300 */

#if defined(hp9000s800) || defined(__hp9000s800)

/* The bottom 2 bits of the PC are protection bits.
   They should be masked away before looking at the PC.
 */

#define PC_VALUE_MASK ((~0) << 2)

/* pcoq is the offset (32 bit in 64 bit virtual address space)
   in the space included in the corresponding sc_pcsq.
   head is the current instruction, tail is the next instruction
   which is not necessarily the following instruction because
   of delayed branching, etc.
   Both queues need to be collected for some screw cases of
   debugging and if there is ever a hope to restart the code.
 */

#ifdef __HPUX__

/* HPUX 9.x does not have siginfo, but HPUX 10.x does.  This can be
   tested by the definition of SA_SIGINFO.  Since we want to support
   both, we use the no-siginfo way.  */

#ifdef SA_SIGINFO
#  undef SA_SIGINFO
#endif

#include <sys/sysmacros.h>

/* See <machine/save_state.h> included by <signal.h> */
#ifndef sc_pc
#  define sc_pc sc_pcoq_head
#endif

#define ss_gr0 ss_flags		/* not really true */

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T		struct sigcontext
#define SIGCONTEXT_NREGS		32
#define SIGCONTEXT_FIRST_REG(scp)	(&((scp)->sc_sl.sl_ss.ss_gr0))
#define SIGCONTEXT_SP(scp)		((scp) -> sc_sp)
#define SIGCONTEXT_PC(scp)		((scp) -> sc_pc)
#define SIGCONTEXT_RFREE(scp)		((scp)->sc_sl.sl_ss.ss_gr21)
#define SIGCONTEXT_SCHSP(scp)		((scp)->sc_sl.sl_ss.ss_gr22)

# define INITIALIZE_UX_SIGNAL_CODES()					\
{									\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), 8, "illegal instruction trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), 9, "break instruction trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), 10, "privileged operation trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), 11, "privileged register trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), 12, "overflow trap");				\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), 13, "conditional trap");				\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), 14, "assist exception trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), 22, "assist emulation trap");			\
}

# define SPECIAL_SIGNAL_CODE_NAMES()					\
{									\
  if ((signo == SIGFPE) && (code == 14))				\
    switch ((((*scp) . sc_sl . sl_ss . ss_frexcp1) >> 26) & 0x3f)	\
      {									\
      case 0x20:							\
	name = "invalid operation";					\
	break;								\
      case 0x10:							\
	name = "divide by zero";					\
	break;								\
      case 0x08:							\
	name = "overflow";						\
	break;								\
      case 0x04:							\
      case 0x14:							\
      case 0x24:							\
      case 0x34:							\
	name = "underflow";						\
	break;								\
      case 0x02:							\
	name = "inexact";						\
	break;								\
      case 0x0a:							\
	name = "inexact and overflow";					\
	break;								\
      case 0x06:							\
      case 0x16:							\
      case 0x26:							\
      case 0x36:							\
	name = "inexact and underflow";					\
	break;								\
      }									\
}

#else /* not __HPUX__, BSD ? */

# ifndef sc_pc
#  define sc_pc				sc_pcoqh
# endif /* sc_pc */

#endif /* __HPUX__ */

#endif /* hp9000s800 */

#ifdef sun3

struct full_sigcontext
{
  struct sigcontext * fs_original;
  unsigned long fs_regs [SIGCONTEXT_NREGS];
};

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T		struct sigcontext
#define SIGCONTEXT_T			struct full_sigcontext
#define SIGCONTEXT_NREGS		15 /* missing sp */
#define SIGCONTEXT_FIRST_REG(scp)	(& (((scp) -> fs_regs) [0]))
#define SIGCONTEXT_SP(scp)		((scp)->fs_original->sc_sp)
#define SIGCONTEXT_PC(scp)		((scp)->fs_original->sc_pc)
#define SIGCONTEXT_RFREE(scp)		(((scp) -> fs_regs) [8 + 5]) /* A5 */

#define DECLARE_SIGCONTEXT(scp, arg)					\
  SIGCONTEXT_T scp [1];							\
  static void sun3_save_regs, (int *);					\
  sun3_save_regs (& ((((scp) [0]) . fs_regs) [0]));			\
  (((scp) [0]) . fs_original) = (arg)

#endif /* sun3 */

#ifdef vax

struct full_sigcontext
{
  struct sigcontext * fs_original;
  int fs_regs [SIGCONTEXT_NREGS];
};

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T		struct sigcontext
#define SIGCONTEXT_T			struct full_sigcontext
#define SIGCONTEXT_NREGS		16
#define SIGCONTEXT_FIRST_REG(scp)	(&((scp)->fs_regs[0]))
#define SIGCONTEXT_SP(scp)		((scp)->fs_original->sc_sp)
#define SIGCONTEXT_PC(scp)		((scp)->fs_original->sc_pc)
#define SIGCONTEXT_RFREE(scp)		((scp)->fs_regs[12]) /* fp */

/* r0 has to be kludged. */
#define DECLARE_SIGCONTEXT(partial, full)				\
  SIGCONTEXT_T scp [1];							\
  static int vax_get_r0 (void);						\
  static int * vax_save_start (int *, int);				\
  static void vax_save_finish						\
    (int *,								\
		       struct sigcontext *,				\
		       struct full_sigcontext *);			\
  vax_save_finish ((vax_save_start ((& ((((full) [0]) . fs_regs) [0])),	\
				    (vax_get_r0 ()))),			\
		   (partial),						\
		   (&(full)[0]))

#endif /* vax */

#ifdef mips
#ifdef __IRIX__

/* Information on sigcontext structure in signal.h */

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T		struct sigcontext
#define SIGCONTEXT_NREGS		32
#define SIGCONTEXT_FIRST_REG(scp)	(& (((scp) -> sc_regs) [0]))
#define SIGCONTEXT_SP(scp)		(((scp) -> sc_regs) [29])
#define SIGCONTEXT_PC(scp)		((scp) -> sc_pc)
#define SIGCONTEXT_RFREE(scp)		(((scp) -> sc_regs) [9])
#define SIGCONTEXT_SCHSP(scp)		(((scp) -> sc_regs) [3])

#define INITIALIZE_UX_SIGNAL_CODES()					\
{									\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGTRAP, (~ 0L), BRK_OVERFLOW, "integer overflow trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGTRAP, (~ 0L), BRK_DIVZERO, "integer divide by 0 trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGTRAP, (~ 0L), BRK_MULOVF, "integer multiply overflow");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE,  (  0L),       0,      "floating-point exception");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGSEGV, (~ 0L),       EFAULT,   "Invalid virtual address");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGSEGV, (~ 0L),       EACCES,   "Read-only address");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGSEGV, (~ 0L),       ENXIO,   "Read beyond mapped object");	\
}

#else /* not __IRIX__ */
#ifndef _SYSV4

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T		struct sigcontext
#define SIGCONTEXT_NREGS		32
#define SIGCONTEXT_FIRST_REG(scp)	(& (((scp) -> sc_regs) [0]))
#define SIGCONTEXT_SP(scp)		(((scp) -> sc_regs) [29])
#define SIGCONTEXT_PC(scp)		((scp) -> sc_pc)
#define SIGCONTEXT_RFREE(scp)		(((scp) -> sc_regs) [9])
#define SIGCONTEXT_SCHSP(scp)		(((scp) -> sc_regs) [3])

#define INITIALIZE_UX_SIGNAL_CODES()					\
{									\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTOVF_TRAP, "integer overflow trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTDIV_TRAP, "integer divide by 0 trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTOVF_TRAP, "floating-point overflow trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTDIV_TRAP, "floating-point divide by 0 trap"); \
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTUND_TRAP, "floating-point underflow trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_DECOVF_TRAP, "decimal overflow trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_SUBRNG_TRAP, "subscript-range trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTOVF_FAULT, "floating-point overflow fault"); \
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTDIV_FAULT, "floating-point divide by 0 fault"); \
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTUND_FAULT, "floating-point underflow fault"); \
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_PRIVIN_FAULT, "reserved instruction trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_RESOP_FAULT, "reserved operand trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_RESAD_FAULT, "reserved addressing trap");	\
}

#else /* _SYSV4 */

/* Many of these definitions are not specific to the MIPS processor. */

#include <sys/siginfo.h>
#include <sys/ucontext.h>

/* For Sony NEWS-OS 5.0.1 and earlier: */
#if defined(sonyrisc) && !defined(_CFE)
#  define gregs gpregs
#endif

#define SIGINFO_T siginfo_t *
#define SIGINFO_VALID_P(info) ((info) != 0)
#define SIGINFO_CODE(info) ((info) -> si_code)

#define __SIGCONTEXT_REG(scp, ir) ((((scp) -> uc_mcontext) . gregs) [(ir)])

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T void
#define SIGCONTEXT_T ucontext_t
#define SIGCONTEXT_NREGS NGREG
#define SIGCONTEXT_FIRST_REG(scp) (& (__SIGCONTEXT_REG (scp, 0)))
#define SIGCONTEXT_SP(scp) (__SIGCONTEXT_REG (scp, CXT_SP))
#define SIGCONTEXT_PC(scp) (__SIGCONTEXT_REG (scp, CXT_EPC))
#define SIGCONTEXT_RFREE(scp) (__SIGCONTEXT_REG (scp, CXT_T1))
#define SIGCONTEXT_SCHSP(scp) (__SIGCONTEXT_REG (scp, CXT_V1))

#define INITIALIZE_UX_SIGNAL_CODES()					\
{									\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTDIV, "integer divide by 0 trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTOVF, "integer overflow trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTDIV, "floating-point divide by 0 trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTOVF, "floating-point overflow trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTUND, "floating-point underflow trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTRES, "floating-point inexact result");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTSUB, "subscript-range trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTINV, "invalid floating-point operation");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLOPC, "illegal opcode trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLOPN, "illegal operand trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLADR, "illegal addressing mode trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLTRP, "illegal trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_PRVOPC, "privileged opcode trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_PRVREG, "privileged register trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_COPROC, "co-processor trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_BADSTK, "bad stack trap");			\
}

#endif /* _SYSV4 */
#endif /* __IRIX__ */
#endif /* mips */

#ifdef __IA32__
 
#if defined(__FreeBSD__) || defined(__DragonFly__)
#  include <ucontext.h>
#endif

#ifdef _MACH_UNIX
/* The following are true for Mach (BSD 4.3 compatible).
   I don't know about SCO or other versions.  */

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T		struct sigcontext
#define SIGCONTEXT_NREGS		8
#define SIGCONTEXT_FIRST_REG(scp)	(& ((scp) -> sc_edi))
#define SIGCONTEXT_SP(scp)		((scp) -> sc_esp)
#define SIGCONTEXT_PC(scp)		((scp) -> sc_eip)
#define SIGCONTEXT_RFREE(scp)		((scp) -> sc_edi)

/* INITIALIZE_UX_SIGNAL_CODES should be defined. */

#endif /* _MACH_UNIX */

#endif /* __IA32__ */

#ifdef __alpha

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_ARG_T		struct sigcontext
#define SIGCONTEXT_NREGS		32
#define SIGCONTEXT_FIRST_REG(scp)	(& (((scp) -> sc_regs) [0]))
#define SIGCONTEXT_SP(scp)		(((scp) -> sc_regs) [30])
#define SIGCONTEXT_PC(scp)		((scp) -> sc_pc)
#define SIGCONTEXT_RFREE(scp)		(((scp) -> sc_regs) [4])
#define SIGCONTEXT_SCHSP(scp)		(((scp) -> sc_regs) [2])

#ifdef FPE_COMPLETE_FAULT
#define STUPID_FIRST_SIGNAL()						\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_COMPLETE_FAULT, "software completion fault")
#endif

#ifdef FPE_UNIMP_FAULT
#define STUPID_FIRST_SIGNAL()						\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_UNIMP_FAULT, "unimplemented fp instruction fault")
#endif

#ifndef STUPID_FIRST_SIGNAL
#define STUPID_FIRST_SIGNAL()	{ }
#endif

#define INITIALIZE_UX_SIGNAL_CODES()					\
{                                                                       \
  STUPID_FIRST_SIGNAL();						\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INVALID_FAULT, "invalid operation fault");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INEXACT_FAULT, "floating-point inexact result");\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTOVF_FAULT, "integer overflow fault");	\
}

#endif /* __alpha */

#ifdef __sparc

#ifdef __svr4__
/* SPARC Solaris */

/* This is adapted from the MIPS SysV4 code above,
   adjusted for the SPARC. */

#include <siginfo.h>
#include <ucontext.h>

#define __SIGCONTEXT_REG(scp, ir) ((((scp) -> uc_mcontext) . gregs) [(ir)])

/* SIGINFO_T, SIGINFO_VALID_P, SIGINFO_CODE, SIGINFO_ARG_T, and SIGCONTEXT_T
   are all defined by the conditional for _POSIX_REALTIME_SIGNALS below. */

#define HAVE_SIGCONTEXT
#define SIGCONTEXT_NREGS NGREG
#define SIGCONTEXT_FIRST_REG(scp) (& (__SIGCONTEXT_REG (scp, 0)))

#define SIGCONTEXT_SP(scp) (__SIGCONTEXT_REG (scp, REG_SP))
#define SIGCONTEXT_PC(scp) (__SIGCONTEXT_REG (scp, REG_PC))

/* I don't think that either of these actually matters for liarc, which
   is all that we support on the SPARC at the moment. */

#define SIGCONTEXT_RFREE(scp) 0
#define SIGCONTEXT_SCHSP(scp) 0

#define INITIALIZE_UX_SIGNAL_CODES()					\
{									\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTDIV, "integer divide by 0 trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTOVF, "integer overflow trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTDIV, "floating-point divide by 0 trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTOVF, "floating-point overflow trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTUND, "floating-point underflow trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTRES, "floating-point inexact result");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTSUB, "subscript-range trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTINV, "invalid floating-point operation");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLOPC, "illegal opcode trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLOPN, "illegal operand trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLADR, "illegal addressing mode trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLTRP, "illegal trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_PRVOPC, "privileged opcode trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_PRVREG, "privileged register trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_COPROC, "co-processor trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_BADSTK, "bad stack trap");			\
}

/* Solaris's siginfo(3HEAD) man page lists many more signal codes, but
   so does Linux's sigaction(2) man page, and this is the same list as
   in the Linux section.  Unless this is used for anything other than
   documentative purposes during trap recovery, then I can't imagine
   why these lists aren't populated more completely. */

#endif /* __svr4__ */

#endif /* __sparc */

#ifdef __linux__

/* This isn't really the right test: what we really want to know is if
   the kernel supports the newer signal-delivery mechanism.  */

#  ifdef _POSIX_REALTIME_SIGNALS

#  ifdef __IA32__
#    define __SIGCONTEXT_REG(scp, ir) ((((scp) -> uc_mcontext) . gregs) [(ir)])
#    define HAVE_SIGCONTEXT
#    define SIGCONTEXT_NREGS NGREG
#    define SIGCONTEXT_FIRST_REG(scp) (& (__SIGCONTEXT_REG (scp, REG_GS)))
#    define SIGCONTEXT_SP(scp) (__SIGCONTEXT_REG (scp, REG_ESP))
#    define SIGCONTEXT_PC(scp) (__SIGCONTEXT_REG (scp, REG_EIP))
#    define SIGCONTEXT_RFREE(scp) (__SIGCONTEXT_REG (scp, REG_EDI))
#  endif /* __IA32__ */

#  ifdef __x86_64__
#    define __SIGCONTEXT_REG(scp, ir) ((((scp) -> uc_mcontext) . gregs) [(ir)])
#    define HAVE_SIGCONTEXT
#    define SIGCONTEXT_NREGS NGREG
#    define SIGCONTEXT_FIRST_REG(scp) (& (__SIGCONTEXT_REG (scp, REG_R8)))
#    define SIGCONTEXT_SP(scp) (__SIGCONTEXT_REG (scp, REG_RSP))
#    define SIGCONTEXT_PC(scp) (__SIGCONTEXT_REG (scp, REG_RIP))
#    define SIGCONTEXT_RFREE(scp) (__SIGCONTEXT_REG (scp, REG_RDI))
#  endif /* __x86_64__ */

#  else /* not _POSIX_REALTIME_SIGNALS */

#    ifdef __IA32__

/* In Linux 2.0 and earlier, signal handlers are called with one
   argument.  There's an "iBCS2 signal stack" register dump just above
   it.  Thus, the fictitious `info' argument to the handler is
   actually the first member of this register dump (described by
   linux_sigcontext_t, below).  Unfortunately, kludging SIGINFO_CODE
   to access the sc_trapno will fail later on when looking at the
   saved_info.  */

typedef struct
{
  unsigned short gs, __gsh;
  unsigned short fs, __fsh;
  unsigned short es, __esh;
  unsigned short ds, __dsh;
  unsigned long edi;
  unsigned long esi;
  unsigned long ebp;
  unsigned long esp;
  unsigned long ebx;
  unsigned long edx;
  unsigned long ecx;
  unsigned long eax;
  unsigned long trapno;
  unsigned long err;
  unsigned long eip;
  unsigned short cs, __csh;
  unsigned long eflags;
  unsigned long esp_at_signal;
  unsigned short ss, __ssh;
  void * fpstate;
} linux_sigcontext_t;

#      define HAVE_SIGCONTEXT
#      define SIGCONTEXT_T linux_sigcontext_t
#      define SIGCONTEXT_NREGS 19
#      define SIGCONTEXT_FIRST_REG(scp) (scp)
#      define SIGCONTEXT_SP(scp) ((scp) -> esp)
#      define SIGCONTEXT_PC(scp) ((scp) -> eip)
#      define SIGCONTEXT_RFREE(scp) ((scp) -> edi)

/* DECLARE_SIGCONTEXT gives us a chance to generate a pointer to the
   register dump, since it is used at the beginning of STD_HANDLER's.
   In terms of the expected arguments to the STD_ signal HANDLER's,
   the register dump is right above `signo', at `info', one long below
   `pscp', which is what DECLARE_SIGCONTEXT is getting for `arg'.
   Thus, our pointer to a scp is initialized to the address of `arg'
   minus 1 long.  */

#      define DECLARE_SIGCONTEXT(scp, arg)				\
  SIGCONTEXT_T * scp;							\
  scp = ((SIGCONTEXT_T *) (((unsigned long *) (& (arg))) - 1))

#    endif /* __IA32__ */

#  endif /* _POSIX_REALTIME_SIGNALS */

#  define INITIALIZE_UX_SIGNAL_CODES()					\
{									\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTDIV, "integer divide by 0 trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTOVF, "integer overflow trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTDIV, "floating-point divide by 0 trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTOVF, "floating-point overflow trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTUND, "floating-point underflow trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTRES, "floating-point inexact result");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTSUB, "subscript-range trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_FLTINV, "invalid floating-point operation");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLOPC, "illegal opcode trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLOPN, "illegal operand trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLADR, "illegal addressing mode trap");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_ILLTRP, "illegal trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_PRVOPC, "privileged opcode trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_PRVREG, "privileged register trap");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_COPROC, "co-processor trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGILL, (~ 0L), ILL_BADSTK, "bad stack trap");			\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGSEGV, (~ 0L), SEGV_MAPERR, "address not mapped to object");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGSEGV, (~ 0L), SEGV_ACCERR, "invalid permissions for mapped object"); \
  DECLARE_UX_SIGNAL_CODE						\
    (SIGBUS, (~ 0L), BUS_ADRALN, "invalid address alignment");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGBUS, (~ 0L), BUS_ADRERR, "nonexistent physical address");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGBUS, (~ 0L), BUS_OBJERR, "object-specific hardware error");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGTRAP, (~ 0L), TRAP_BRKPT, "process breakpoint");		\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGTRAP, (~ 0L), TRAP_TRACE, "process trace trap");		\
}

#endif /* __linux__ */

/* Judging by the CVS logs, NetBSD seems to have begun supporting
   siginfo cruft somewhere around 1.5ish or 1.6ish, so 2.0 is a
   reasonable guess.  */

#if defined(__NetBSD__)
#include <sys/param.h>
#if defined(__NetBSD_Version__) && __NetBSD_Version__ >= 200000000

#  define HAVE_SIGACTION_SIGINFO_SIGNALS

#  include <sys/siginfo.h>
#  include <sys/ucontext.h>
#  include <machine/mcontext.h>

#  ifdef __IA32__
#    define SIGCONTEXT_RFREE(scp) ((SIGCONTEXT_FIRST_REG (scp)) [_REG_EDI])
#    define HAVE_NETBSD_GENERIC_MCONTEXT
#  endif

#  ifdef __x86_64__
#    define SIGCONTEXT_RFREE(scp) ((SIGCONTEXT_FIRST_REG (scp)) [_REG_RDI])
#    define HAVE_NETBSD_GENERIC_MCONTEXT
#  endif

/* When I checked, on 2010-07-22, the only NetBSD ports without the
   generic mcontext interface were IA-64 and usermode.  */

#  ifdef HAVE_NETBSD_GENERIC_MCONTEXT
#    define HAVE_SIGCONTEXT
#    define SIGCONTEXT_NREGS _NGREG
#    define SIGCONTEXT_FIRST_REG(scp) (((scp) -> uc_mcontext) . __gregs)
#    define SIGCONTEXT_SP(scp) (_UC_MACHINE_SP (scp))
#    define SIGCONTEXT_PC(scp) (_UC_MACHINE_PC (scp))
#  endif

#  define INITIALIZE_UX_SIGNAL_CODES()                                  \
  do {                                                                  \
    DECLARE_UX_SIGNAL_CODE (SIGILL, (~0L), ILL_ILLOPC, "Illegal opcode"); \
    DECLARE_UX_SIGNAL_CODE (SIGILL, (~0L), ILL_ILLOPN, "Illegal operand"); \
    DECLARE_UX_SIGNAL_CODE (SIGILL, (~0L), ILL_ILLADR, "Illegal addressing mode"); \
    DECLARE_UX_SIGNAL_CODE (SIGILL, (~0L), ILL_ILLTRP, "Illegal trap"); \
    DECLARE_UX_SIGNAL_CODE (SIGILL, (~0L), ILL_PRVOPC, "Privileged opcode"); \
    DECLARE_UX_SIGNAL_CODE (SIGILL, (~0L), ILL_PRVREG, "Privileged register"); \
    DECLARE_UX_SIGNAL_CODE (SIGILL, (~0L), ILL_COPROC, "Coprocessor error"); \
    DECLARE_UX_SIGNAL_CODE (SIGILL, (~0L), ILL_BADSTK, "Internal stack error"); \
    DECLARE_UX_SIGNAL_CODE (SIGFPE, (~0L), FPE_INTDIV, "Integer divide by zero"); \
    DECLARE_UX_SIGNAL_CODE (SIGFPE, (~0L), FPE_INTOVF, "Integer overflow"); \
    DECLARE_UX_SIGNAL_CODE (SIGFPE, (~0L), FPE_FLTDIV, "Floating-point divide by zero"); \
    DECLARE_UX_SIGNAL_CODE (SIGFPE, (~0L), FPE_FLTOVF, "Floating-point overflow"); \
    DECLARE_UX_SIGNAL_CODE (SIGFPE, (~0L), FPE_FLTUND, "Floating-point underflow"); \
    DECLARE_UX_SIGNAL_CODE (SIGFPE, (~0L), FPE_FLTRES, "Floating-point inexact result"); \
    DECLARE_UX_SIGNAL_CODE (SIGFPE, (~0L), FPE_FLTINV, "Invalid floating-point operation"); \
    DECLARE_UX_SIGNAL_CODE (SIGFPE, (~0L), FPE_FLTSUB, "Subscript out of range"); \
    DECLARE_UX_SIGNAL_CODE (SIGSEGV, (~0L), SEGV_MAPERR, "Address not mapped to object"); \
    DECLARE_UX_SIGNAL_CODE (SIGSEGV, (~0L), SEGV_ACCERR, "Invalid permissions for mapped object"); \
    DECLARE_UX_SIGNAL_CODE (SIGBUS, (~0L), BUS_ADRALN, "Invalid address alignment"); \
    DECLARE_UX_SIGNAL_CODE (SIGBUS, (~0L), BUS_ADRERR, "Nonexistent physical address"); \
    DECLARE_UX_SIGNAL_CODE (SIGBUS, (~0L), BUS_OBJERR, "Object-specific hardware error"); \
    DECLARE_UX_SIGNAL_CODE (SIGTRAP, (~0L), TRAP_BRKPT, "Process breakpoint"); \
    DECLARE_UX_SIGNAL_CODE (SIGTRAP, (~0L), TRAP_TRACE, "Process trace trap"); \
  } while (0)

#endif /* __NetBSD_Version__ >= 200000000 */
#endif /* __NetBSD__ */

#ifdef _POSIX_REALTIME_SIGNALS
#  define HAVE_SIGACTION_SIGINFO_SIGNALS
#endif

#ifdef HAVE_SIGACTION_SIGINFO_SIGNALS
#  define SIGINFO_T siginfo_t *
#  define SIGINFO_VALID_P(info) (1)
#  define SIGINFO_CODE(info) ((info) -> si_code)
#  define SIGCONTEXT_ARG_T void
#  define SIGCONTEXT_T ucontext_t
#endif

#ifndef SIGINFO_T
#  define SIGINFO_T int
#  define SIGINFO_VALID_P(info) (0)
#  define SIGINFO_CODE(info) (0)
#endif

#ifndef SIGCONTEXT_ARG_T
#  define SIGCONTEXT_ARG_T int
#endif

#ifndef SIGCONTEXT_T
#  define SIGCONTEXT_T SIGCONTEXT_ARG_T
#endif

#ifndef DECLARE_SIGCONTEXT
#  define DECLARE_SIGCONTEXT(scp, arg)					\
     SIGCONTEXT_T * scp;						\
     scp = ((SIGCONTEXT_T *) (arg))
#endif

#ifndef SIGCONTEXT_NREGS
#  define SIGCONTEXT_NREGS (0)
#endif

#ifndef SIGCONTEXT_FIRST_REG
#  define SIGCONTEXT_FIRST_REG(scp) (0)
#endif

#ifndef SIGCONTEXT_SP
#  define SIGCONTEXT_SP(scp) (0)
#endif

#ifndef SIGCONTEXT_PC
#  define SIGCONTEXT_PC(scp) (0)
#endif

#ifndef SIGCONTEXT_RFREE
#  define SIGCONTEXT_RFREE(scp) ((unsigned long) heap_alloc_limit)
#endif

#ifndef SIGCONTEXT_SCHSP
#  define SIGCONTEXT_SCHSP SIGCONTEXT_SP
#endif

#ifndef INITIALIZE_UX_SIGNAL_CODES
#  define INITIALIZE_UX_SIGNAL_CODES()
#endif

#ifdef _AIX
   extern int _etext;
#endif

#if defined(__linux__) || defined(__NetBSD__)
   extern unsigned int _init;
   extern unsigned int etext;
#  define ADDRESS_UCODE_P(addr)						\
     ((((unsigned int *) (addr)) >= (&_init))				\
      && (((unsigned int *) (addr)) <= (&etext)))
#endif

#ifdef __CYGWIN__
   extern unsigned int end;
#endif

#ifndef ADDRESS_UCODE_P
#  define ADDRESS_UCODE_P(addr) (0)
#endif

/* Machine/OS-independent section */

enum trap_state
{
  trap_state_trapped,
  trap_state_exit,
  trap_state_suspend,
  trap_state_query,
  trap_state_recover,
  trap_state_exitting_soft,
  trap_state_exitting_hard
};

extern void UX_initialize_trap_recovery (void);
extern enum trap_state OS_set_trap_state (enum trap_state state);
extern void hard_reset (SIGCONTEXT_T * scp);
extern void soft_reset (void);
extern void trap_handler
  (const char *, int, SIGINFO_T, SIGCONTEXT_T *);
#ifdef CC_SUPPORT_P
   extern SCHEME_OBJECT find_ccblock (unsigned long);
#endif

#define STATE_UNKNOWN		(LONG_TO_UNSIGNED_FIXNUM (0))
#define STATE_PRIMITIVE		(LONG_TO_UNSIGNED_FIXNUM (1))
#define STATE_COMPILED_CODE	(LONG_TO_UNSIGNED_FIXNUM (2))
#define STATE_PROBABLY_COMPILED	(LONG_TO_UNSIGNED_FIXNUM (3))
#define STATE_BUILTIN		(LONG_TO_UNSIGNED_FIXNUM (4))
#define STATE_UTILITY		(LONG_TO_UNSIGNED_FIXNUM (5))  /* CommGas? */

struct trap_recovery_info
{
  SCHEME_OBJECT state;
  SCHEME_OBJECT pc_info_1;
  SCHEME_OBJECT pc_info_2;
  SCHEME_OBJECT extra_trap_info;
};

#endif /* SCM_UXTRAP_H */
