#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

|#

;;;; Tests of low-level subprocess support

(declare (usual-integrations))

(define (shell-subprocess command)
  (start-pipe-subprocess "/bin/sh" `#("/bin/sh" "-c" ,command) '#()))

(define-test 'SIMPLE-SHELL-SUBPROCESS
  (lambda ()
    (assert-true (subprocess? (shell-subprocess ":")))))

(define-test 'SUBPROCESS-WAIT:EXIT
  (lambda ()
    (let ((subprocess (shell-subprocess ":")))
      (assert-eqv (subprocess-wait subprocess) 'EXITED))))

(define-test 'SUBPROCESS-WAIT:KILL
  (lambda ()
    (let ((subprocess
           ;; `read x' is a cheesy way to keep it from exiting on its
           ;; own, without busy-waiting or relying on external
           ;; executables.
           (shell-subprocess "read x")))
      (subprocess-kill subprocess)
      (assert-eqv (subprocess-wait subprocess) 'SIGNALLED))))

(define-test 'REGRESSION:SUBPROCESS-KILL-ERROR-AFTER-TERMINATION
  ;; This is a slightly dangerous test: if we regress, then this might
  ;; send SIGKILL a random process on your system!  Maybe this will be
  ;; an incentive to you to avoid regressing.
  (lambda ()
    (let ((subprocess (shell-subprocess ":")))
      (assert-eqv (subprocess-wait subprocess) 'EXITED)
      (assert-error (lambda () (subprocess-kill subprocess))
                    (list condition-type:process-terminated-error)))))

(define-test 'GRACEFUL-SETPGID-RACE
  ;; This is a slightly bogus test.  We actually do want to report an
  ;; error in this situation, but it's not clear that there's a
  ;; straightforward way to do that nicely.  What this test is actually
  ;; checking is whether Scheme handles the setpgid race condition
  ;; without signalling an error when setpgid fails for the loser of
  ;; the race.
  (lambda ()
    (let ((subprocess                   ;An apostrophe!  Yikes!  Run!
           (run-subprocess-in-foreground "/this/program/doesn't/exist"
                                         '#("fnord")
                                         '#())))
      (assert-eqv (subprocess-wait subprocess) 'EXITED)
      (assert-eqv (subprocess-exit-reason subprocess) 1))))
