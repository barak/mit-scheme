/* -*-C-*-

$Id: uxsig.h,v 1.1 1993/08/28 23:08:30 gjr Exp $

Copyright (c) 1990-1993 Massachusetts Institute of Technology

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

/* Signal Handlers */

#ifndef SCM_UXSIG_H
#define SCM_UXSIG_H

#ifdef HAVE_POSIX_SIGNALS
 extern void EXFUN (INSTALL_HANDLER, (int, Tsignal_handler));

#else /* not HAVE_POSIX_SIGNALS */
#ifdef HAVE_SYSV3_SIGNALS
# define INSTALL_HANDLER UX_sigset
# define NEED_HANDLER_TRANSACTION
# define ENTER_HANDLER(signo)
# define ABORT_HANDLER(signo, handler) UX_sigrelse (signo)
# define EXIT_HANDLER(signo, handler)

#else /* not HAVE_SYSV3_SIGNALS */
# define INSTALL_HANDLER UX_signal
# define NEED_HANDLER_TRANSACTION
# define ENTER_HANDLER(signo) UX_signal ((signo), SIG_IGN)
# define ABORT_HANDLER UX_signal
# define EXIT_HANDLER UX_signal

#endif /* HAVE_SYSV3_SIGNALS */
#endif /* HAVE_POSIX_SIGNALS */

#ifndef NEED_HANDLER_TRANSACTION

#define DEFUN_STD_HANDLER(name, statement)				\
Tsignal_handler_result							\
DEFUN (name, (signo, info, pscp),					\
       int signo AND							\
       SIGINFO_T info AND						\
       struct SIGCONTEXT * pscp)					\
{									\
  int STD_HANDLER_abortp;						\
  DECLARE_FULL_SIGCONTEXT (scp);					\
  INITIALIZE_FULL_SIGCONTEXT (pscp, scp);				\
  STD_HANDLER_abortp = (enter_interruption_extent ());			\
  statement;								\
  if (STD_HANDLER_abortp)						\
    exit_interruption_extent ();					\
  SIGNAL_HANDLER_RETURN ();						\
}

#else /* NEED_HANDLER_TRANSACTION */

struct handler_record
{
  int signo;
  Tsignal_handler handler;
};

#define DEFUN_STD_HANDLER(name, statement)				\
Tsignal_handler_result							\
DEFUN (name, (signo, info, pscp),					\
       int signo AND							\
       SIGINFO_T info AND						\
       struct SIGCONTEXT * pscp)					\
{									\
  int STD_HANDLER_abortp;						\
  DECLARE_FULL_SIGCONTEXT (scp);					\
  INITIALIZE_FULL_SIGCONTEXT (pscp, scp);				\
  ENTER_HANDLER (signo);						\
  STD_HANDLER_abortp = (enter_interruption_extent ());			\
  transaction_begin ();							\
  {									\
    struct handler_record * record =					\
      (dstack_alloc (sizeof (struct handler_record)));			\
    (record -> signo) = signo;						\
    (record -> handler) = name;						\
    transaction_record_action (tat_abort, ta_abort_handler, record);	\
  }									\
  statement;								\
  if (STD_HANDLER_abortp)						\
    {									\
      transaction_abort ();						\
      exit_interruption_extent ();					\
    }									\
  transaction_commit ();						\
  EXIT_HANDLER (signo, name);						\
  SIGNAL_HANDLER_RETURN ();						\
}

extern void EXFUN (ta_abort_handler, (PTR));

#endif /* NEED_HANDLER_TRANSACTION */
#endif /* SCM_UXSIG_H */
