#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Tests of synchronous subprocess support

(declare (usual-integrations))

(define (shell command)
  (run-shell-command command 'INPUT #f 'OUTPUT #f))

(define-test 'SUBPROCESS-EXITED-ZERO
  (lambda ()
    (assert-eqv (shell ":") 0)))

(define-test 'SUBPROCESS-EXITED-NONZERO
  (lambda ()
    (assert-eqv (shell "exit 1") 1)))

(define-test 'SUBPROCESS-SIGNALLED
  (lambda ()
    (assert-error (lambda () (shell "kill $$"))
                  (list condition-type:subprocess-signalled))))

(define-test 'SUBPROCESS-STOPPED
  (lambda ()
    (assert-error
     (lambda ()
       ;; Run a subshell that will continue the parent shell just in
       ;; case Scheme fails the test.
       (shell "pid=$$; (sleep 2; kill -CONT $pid) & sleep 1; kill -STOP $pid"))
     (list condition-type:subprocess-stopped))))
