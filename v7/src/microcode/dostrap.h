/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dostrap.h,v 1.1 1992/05/05 06:55:13 jinx Exp $

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

#ifndef SCM_DOSTRAP_H
#define SCM_DOSTRAP_H

#ifndef SIGINFO_T
#define SIGINFO_T unsigned
#define SIGINFO_VALID_P(info) (1)
#define SIGINFO_CODE(info) (info)
#endif

/* EIP not included here, not a "register", except on the Vax.
   8 General registers.
   6 Segment registers.
   1 Flags   register.
 */

#define HAVE_SIGCONTEXT
#define HAVE_FULL_SIGCONTEXT
#define PROCESSOR_NREGS			(8 + 6 + 1)
#define FULL_SIGCONTEXT_NREGS		PROCESSOR_NREGS

#define SIGCONTEXT			sigcontext
#define SIGCONTEXT_SP(scp)		((scp)->sc_esp)
#define SIGCONTEXT_PC(scp)		((scp)->sc_eip)

#define FULL_SIGCONTEXT			SIGCONTEXT
#define FULL_SIGCONTEXT_SP		SIGCONTEXT_SP
#define FULL_SIGCONTEXT_PC		SIGCONTEXT_PC
#define FULL_SIGCONTEXT_RFREE(scp)	((scp)->sc_edi)
#define FULL_SIGCONTEXT_FIRST_REG(scp)	(& (scp->sc_eax))
#define FULL_SIGCONTEXT_SCHSP		FULL_SIGCONTEXT_SP

#define DECLARE_FULL_SIGCONTEXT(name)					\
  struct FULL_SIGCONTEXT * name

#define INITIALIZE_FULL_SIGCONTEXT(partial, full)			\
  ((full) = ((struct FULL_SIGCONTEXT *) (partial)))

#define INVALID_TRAP			-1

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

extern enum trap_state EXFUN (OS_set_trap_state, (enum trap_state state));
extern void EXFUN
  (trap_handler,
   (CONST char * message,
    int signo,
    SIGINFO_T info,
    struct FULL_SIGCONTEXT * scp));
extern void EXFUN (hard_reset, (struct FULL_SIGCONTEXT * scp));
extern void EXFUN (soft_reset, (void));

#endif /* SCM_DOSTRAP_H */
