/* -*-C-*-

$Id: uxtrap.h,v 1.16 1992/08/29 13:10:09 jinx Exp $

Copyright (c) 1990-1992 Massachusetts Institute of Technology

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

#ifndef SCM_UXTRAP_H
#define SCM_UXTRAP_H

#include "os.h"

#ifdef hp9000s300

#include <sys/sysmacros.h>
#include <machine/sendsig.h>
#include <machine/reg.h>

#define HAVE_FULL_SIGCONTEXT
#define PROCESSOR_NREGS			16
#define FULL_SIGCONTEXT_NREGS		GPR_REGS /* Missing sp */

#define RFREE				AR5
#define SIGCONTEXT			full_sigcontext
#define SIGCONTEXT_SP(scp)		((scp)->fs_context.sc_sp)
#define SIGCONTEXT_PC(scp)		((scp)->fs_context.sc_pc)
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->fs_regs[RFREE])
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->fs_regs[GPR_START]))

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

#ifdef hp9000s800

#include <sys/sysmacros.h>

/* See <machine/save_state.h> included by <signal.h> */

#ifndef sc_pc
/* pcoq is the offset (32 bit in 64 bit virtual address space)
   in the space included in the corresponding sc_pcsq.
   head is the current instruction, tail is the next instruction
   which is not necessarily the following instruction because
   of delayed branching, etc.
   Both queues need to be collected for some screw cases of
   debugging and if there is ever a hope to restart the code.
 */
#define sc_pc				sc_pcoq_head
#endif

#define ss_gr0				ss_flags	/* not really true */
#define ss_rfree			ss_gr21		/* or some such */
#define ss_schsp			ss_gr22

#define HAVE_FULL_SIGCONTEXT
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->sc_sl.sl_ss.ss_rfree)
#define FULL_SIGCONTEXT_SCHSP(scp)	((scp)->sc_sl.sl_ss.ss_schsp)
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->sc_sl.sl_ss.ss_gr0))
#define FULL_SIGCONTEXT_NREGS		32
#define PROCESSOR_NREGS			32

/* The bottom 2 bits of the PC are protection bits.
   They should be masked away before looking at the PC.
 */

#define PC_VALUE_MASK			((~0) << 2)

#define INITIALIZE_UX_SIGNAL_CODES()					\
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

#define SPECIAL_SIGNAL_CODE_NAMES()					\
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

#endif /* hp9000s800 */

#ifdef sun3

#define HAVE_FULL_SIGCONTEXT
#define PROCESSOR_NREGS			16
#define FULL_SIGCONTEXT_NREGS		15		/* missing sp */

struct full_sigcontext
{
  struct sigcontext * fs_original;
  int fs_regs[FULL_SIGCONTEXT_NREGS];
};

#define RFREE				(8 + 5)		/* A5 */
#define FULL_SIGCONTEXT			full_sigcontext
#define FULL_SIGCONTEXT_SP(scp)		(scp->fs_original->sc_sp)
#define FULL_SIGCONTEXT_PC(scp)		(scp->fs_original->sc_pc)
#define FULL_SIGCONTEXT_RFREE(scp)	(scp->fs_regs[RFREE])
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->fs_regs[0]))

#define DECLARE_FULL_SIGCONTEXT(name)					\
  struct FULL_SIGCONTEXT name [1]

#define INITIALIZE_FULL_SIGCONTEXT(partial, full)			\
{									\
  static void EXFUN (sun3_save_regs, (int * regs));			\
  sun3_save_regs (& ((((full) [0]) . fs_regs) [0]));			\
  (((full) [0]) . fs_original) = (partial);				\
}

#endif /* sun3 */

#ifdef vax

#define HAVE_FULL_SIGCONTEXT
#define PROCESSOR_NREGS			16
#define FULL_SIGCONTEXT_NREGS		16

struct full_sigcontext
{
  struct sigcontext * fs_original;
  int fs_regs [FULL_SIGCONTEXT_NREGS];
};

#define RFREE				12		/* fp */
#define FULL_SIGCONTEXT			full_sigcontext
#define FULL_SIGCONTEXT_SP(scp)		((scp)->fs_original->sc_sp)
#define FULL_SIGCONTEXT_PC(scp)		((scp)->fs_original->sc_pc)
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->fs_regs[RFREE])
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->fs_regs[0]))

#define DECLARE_FULL_SIGCONTEXT(name)					\
  struct FULL_SIGCONTEXT name [1]

/* r0 has to be kludged. */

#define INITIALIZE_FULL_SIGCONTEXT(partial, full)			\
{									\
  static int EXFUN (vax_get_r0, (void));				\
  static int * EXFUN (vax_save_start, (int * regs, int r0));		\
  static void EXFUN							\
    (vax_save_finish, (int * fp,					\
		       struct sigcontext * pscp,			\
		       struct full_sigcontext * scp));			\
  vax_save_finish ((vax_save_start ((& ((((full) [0]) . fs_regs) [0])),	\
				    (vax_get_r0 ()))),			\
		   (partial),						\
		   (&(full)[0]));					\
}

#endif /* vax */

#ifdef mips
#ifdef _IRIX4

/* Information on sigcontext structure in signal.h */

#ifndef sc_sp
#define sc_sp				sc_regs[29]
#endif

#define sc_rfree			sc_regs[9]
#define sc_schsp			sc_regs[3]

#define HAVE_FULL_SIGCONTEXT
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->sc_rfree)
#define FULL_SIGCONTEXT_SCHSP(scp)	((scp)->sc_schsp)
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->sc_regs[0]))
#define FULL_SIGCONTEXT_NREGS		32
#define PROCESSOR_NREGS			32

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

#else /* not _IRIX4 */
#ifndef _SYSV4

/* Information on sigcontext structure in signal.h */

#ifndef sc_sp
#define sc_sp				sc_regs[29]
#endif

#define sc_rfree			sc_regs[9]
#define sc_schsp			sc_regs[3]

#define HAVE_FULL_SIGCONTEXT
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->sc_rfree)
#define FULL_SIGCONTEXT_SCHSP(scp)	((scp)->sc_schsp)
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->sc_regs[0]))
#define FULL_SIGCONTEXT_NREGS		32
#define PROCESSOR_NREGS			32

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
#define gregs gpregs
#endif

#define SIGINFO_T siginfo_t *
#define SIGINFO_VALID_P(info) ((info) != 0)
#define SIGINFO_CODE(info) ((info) -> si_code)

#define SIGCONTEXT ucontext
#define SIGCONTEXT_SP(scp) ((((scp) -> uc_mcontext) . gregs) [CXT_SP])
#define SIGCONTEXT_PC(scp) ((((scp) -> uc_mcontext) . gregs) [CXT_EPC])

#define HAVE_FULL_SIGCONTEXT
#define FULL_SIGCONTEXT_RFREE(scp) ((((scp) -> uc_mcontext) . gregs) [CXT_T1])
#define FULL_SIGCONTEXT_SCHSP(scp) ((((scp) -> uc_mcontext) . gregs) [CXT_V1])
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(((scp) -> uc_mcontext) . gregs)
#define FULL_SIGCONTEXT_NREGS		NGREG
#define PROCESSOR_NREGS			NGREG

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
#endif /* _IRIX4 */
#endif /* mips */

#if defined(i386) && defined(_MACH_UNIX)
/* The following are true for Mach (BSD 4.3 compatible).
   I don't know about SCO or other versions.
 */

#define HAVE_FULL_SIGCONTEXT
#define PROCESSOR_NREGS			8
#define FULL_SIGCONTEXT_NREGS		8

#define SIGCONTEXT			sigcontext
#define SIGCONTEXT_SP(scp)		((scp)->sc_esp)
#define SIGCONTEXT_PC(scp)		((scp)->sc_eip)
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->sc_edi)
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->sc_edi))

/* INITIALIZE_UX_SIGNAL_CODES should be defined. */

#endif /* i386 */

#ifdef __alpha

#define sc_sp				sc_regs[30]
#define sc_rfree			sc_regs[4]
#define sc_schsp			sc_regs[2]

#define HAVE_FULL_SIGCONTEXT
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->sc_rfree)
#define FULL_SIGCONTEXT_SCHSP(scp)	((scp)->sc_schsp)
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->sc_regs[0]))
#define FULL_SIGCONTEXT_NREGS		32
#define PROCESSOR_NREGS			32

#define INITIALIZE_UX_SIGNAL_CODES()					\
{									\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_COMPLETE_FAULT, "software completion fault");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INVALID_FAULT, "invalid operation fault");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INEXACT_FAULT, "floating-point inexact result");	\
  DECLARE_UX_SIGNAL_CODE						\
    (SIGFPE, (~ 0L), FPE_INTOVF_FAULT, "integer overflow fault");	\
}

#endif /* __alpha */

#ifndef SIGINFO_T
#define SIGINFO_T int
#define SIGINFO_VALID_P(info) (1)
#define SIGINFO_CODE(info) (info)
#endif

#ifndef SIGCONTEXT
#define SIGCONTEXT		sigcontext
#define SIGCONTEXT_SP(scp)	((scp)->sc_sp)
#define SIGCONTEXT_PC(scp)	((scp)->sc_pc)
#endif /* SIGCONTEXT */

#ifndef FULL_SIGCONTEXT

#define FULL_SIGCONTEXT SIGCONTEXT
#define FULL_SIGCONTEXT_SP SIGCONTEXT_SP
#define FULL_SIGCONTEXT_PC SIGCONTEXT_PC

#define DECLARE_FULL_SIGCONTEXT(name)					\
  struct FULL_SIGCONTEXT * name

#define INITIALIZE_FULL_SIGCONTEXT(partial, full)			\
  ((full) = ((struct FULL_SIGCONTEXT *) (partial)))

#endif /* not FULL_SIGCONTEXT */

#ifndef FULL_SIGCONTEXT_NREGS
#define FULL_SIGCONTEXT_NREGS 0
#define FULL_SIGCONTEXT_FIRST_REG(scp) ((int *) 0)
#endif

#ifndef PROCESSOR_NREGS
#define PROCESSOR_NREGS 0
#endif

#ifndef FULL_SIGCONTEXT_SCHSP
#define FULL_SIGCONTEXT_SCHSP FULL_SIGCONTEXT_SP
#endif

#ifndef INITIALIZE_UX_SIGNAL_CODES
#define INITIALIZE_UX_SIGNAL_CODES()
#endif

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

extern void EXFUN (initialize_trap_recovery, (char * C_sp));
extern enum trap_state EXFUN (OS_set_trap_state, (enum trap_state state));
extern void EXFUN
  (trap_handler,
   (CONST char * message,
    int signo,
    SIGINFO_T info,
    struct FULL_SIGCONTEXT * scp));
extern void EXFUN (hard_reset, (struct FULL_SIGCONTEXT * scp));
extern void EXFUN (soft_reset, (void));

#endif /* SCM_UXTRAP_H */
