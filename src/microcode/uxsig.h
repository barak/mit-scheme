/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* Signal Handlers */

#ifndef SCM_UXSIG_H
#define SCM_UXSIG_H

#ifndef HAVE_POSIX_SIGNALS
#  ifdef HAVE_SIGHOLD
#    define INSTALL_HANDLER UX_sigset
#    define NEED_HANDLER_TRANSACTION
#    define ENTER_HANDLER(signo)
#    define ABORT_HANDLER(signo, handler) UX_sigrelse (signo)
#    define EXIT_HANDLER(signo, handler)
#  else
#    define INSTALL_HANDLER UX_signal
#    define NEED_HANDLER_TRANSACTION
#    define ENTER_HANDLER(signo) UX_signal ((signo), SIG_IGN)
#    define ABORT_HANDLER UX_signal
#    define EXIT_HANDLER UX_signal
#  endif
#endif

#ifndef NEED_HANDLER_TRANSACTION

#define DEFUN_STD_HANDLER(name, statement)				\
Tsignal_handler_result							\
name (int signo,							\
      SIGINFO_T info,							\
      SIGCONTEXT_ARG_T * pscp)						\
{									\
  int STD_HANDLER_abortp;						\
  DECLARE_SIGCONTEXT (scp, pscp);					\
  record_signal_delivery (signo);					\
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
name (int signo,							\
      SIGINFO_T info,							\
      SIGCONTEXT_ARG_T * pscp)						\
{									\
  int STD_HANDLER_abortp;						\
  DECLARE_SIGCONTEXT (scp, pscp);					\
  ENTER_HANDLER (signo);						\
  record_signal_delivery (signo);					\
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

extern void ta_abort_handler (void *);

#endif /* NEED_HANDLER_TRANSACTION */

#ifndef DEBUG_SIGNAL_DELIVERY
#define record_signal_delivery(signo)
#endif

#endif /* SCM_UXSIG_H */
