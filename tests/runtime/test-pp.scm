#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; Tests of pretty-printer

(declare (usual-integrations))

(define (carefully procedure if-overflow if-timeout)
  (let ((thread #f)
        (mutex (make-thread-mutex))
        (condvar (make-condition-variable))
        (gc-env (->environment '(runtime garbage-collector))))
    (define (start-it)
      (with-thread-mutex-lock mutex
        (lambda ()
          (do () (thread)
            (condition-variable-wait! condvar mutex))))
      (let ((default/stack-overflow (access default/stack-overflow gc-env)))
        (define (give-up)
          (if (eq? thread (current-thread))
              (exit-current-thread (if-overflow))
              (default/stack-overflow)))
        (call-with-current-continuation
          (lambda (abort)
            (fluid-let (((access hook/stack-overflow gc-env)
                         (lambda () (within-continuation abort give-up))))
              (exit-current-thread (procedure)))))))
    (define (stop-it)
      (assert thread)
      (signal-thread-event thread
                           (lambda ()
                             (exit-current-thread (if-timeout)))))
    (let ((t (create-thread #f start-it)))
      (with-thread-mutex-lock mutex
        (lambda ()
          (set! thread t)
          (condition-variable-broadcast! condvar))))
    (let ((result #f))
      (define (done-it thread* value)
        (assert (eq? thread* thread))
        (set! result value))
      (join-thread thread done-it)
      (let ((timer))
        (dynamic-wind
          (lambda () (set! timer (register-timer-event 1000 stop-it)))
          (lambda () (do () (result) (suspend-current-thread)))
          (lambda () (deregister-timer-event (set! timer))))))))

(define-test 'circular
  (lambda ()
    (define (doit)
      (let ((c (cons 0 0)))
        (set-car! c c)
        (set-cdr! c c)
        (call-with-output-string (lambda (p) (pp c p)))))
    (expect-failure
     (lambda ()
       (assert-eqv
        (carefully doit (lambda () 'stack-overflow) (lambda () 'timeout))
        "#0=(#0# . #0#)")))))
