/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxtrap.h,v 1.3 1990/07/30 16:54:26 jinx Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

#define DECLARE_UX_SIGNAL_CODES						\
static struct ux_sig_code_desc ux_signal_codes[] =			\
{									\
  { SIGFPE, (~ 0L), 0, "software floating point exception" },		\
  { SIGFPE, (~ 0L), 5, "integer divide by zero" },			\
  { SIGFPE, (1L << 15), (1L << 15), "branch/set on unordered" },	\
  { SIGFPE, (1L << 14), (1L << 14), "signalling NAN" },			\
  { SIGFPE, (1L << 13), (1L << 13), "operand error" },			\
  { SIGFPE, (1L << 12), (1L << 12), "overflow" },			\
  { SIGFPE, (1L << 11), (1L << 11), "underflow" },			\
  { SIGFPE, (1L << 10), (1L << 10), "divide by zero" },			\
  { SIGFPE, (1L << 9), (1L << 9), "inexact operation" },		\
  { SIGFPE, (1L << 8), (1L << 8), "inexact decimal input" },		\
  { SIGILL, (~ 0L), 0, "illegal instruction" },				\
  { SIGILL, (~ 0L), 6, "check instruction" },				\
  { SIGILL, (~ 0L), 7, "TRAPV instruction" },				\
  { SIGILL, (~ 0L), 8, "priviledged instruction" },			\
  { 0, 0, 0, ((char *) NULL) }						\
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
#define ss_rfree			ss_gr25		/* or some such */
#define HAVE_FULL_SIGCONTEXT
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->sc_sl.sl_ss.ss_rfree)
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(&((scp)->sc_sl.sl_ss.ss_gr0))
#define FULL_SIGCONTEXT_NREGS		32
#define PROCESSOR_NREGS			32

#define DECLARE_UX_SIGNAL_CODES						\
static struct ux_sig_code_desc ux_signal_codes[] =			\
{									\
  { SIGFPE, (~ 0L), 12, "overflow trap" },				\
  { SIGFPE, (~ 0L), 13, "conditional trap" },				\
  { SIGFPE, (~ 0L), 22, "floating-point assist exception trap" },	\
  { SIGFPE, (~ 0L), 22, "floating-point assist emulation trap" },	\
  { SIGILL, (~ 0L), 8, "illegal instruction trap" },			\
  { SIGILL, (~ 0L), 9, "break instruction trap" },			\
  { SIGILL, (~ 0L), 10, "priviledged operation trap" },			\
  { SIGILL, (~ 0L), 11, "priviledged register trap" },			\
  { 0, 0, 0, ((char *) NULL) }						\
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

/* For now, no trap handling support in compiled code.
   This should be fixed.
 */

#define sc_sp sc_regs[29]

#endif /* mips */

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

enum trap_state
{
  trap_state_trapped,
  trap_state_exit,
  trap_state_suspend,
  trap_state_query,
  trap_state_recover
};

extern void EXFUN (initialize_trap_recovery, (char * C_sp));
extern enum trap_state EXFUN (OS_set_trap_state, (enum trap_state state));
extern void EXFUN
  (trap_handler,
   (CONST char * message, int signo, int code, struct FULL_SIGCONTEXT * scp));
extern void EXFUN (hard_reset, (struct FULL_SIGCONTEXT * scp));
extern void EXFUN (soft_reset, (void));

#ifndef DECLARE_UX_SIGNAL_CODES

#define DECLARE_UX_SIGNAL_CODES						\
static struct ux_sig_code_desc ux_signal_codes[] =			\
{									\
  { 0, 0, 0, ((char *) NULL) }						\
}

#endif

#endif /* SCM_UXTRAP_H */
