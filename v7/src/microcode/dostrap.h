/* -*-C-*-

$Id: dostrap.h,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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
