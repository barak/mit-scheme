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

;;;; Tests of cache traps

(declare (usual-integrations))

(define (with-expected-failure xfail body)
  (if (default-object? xfail)
      (body)
      (xfail body)))

(define-test 'restart-unassigned
  (lambda ()
    (define program
      '(begin
         (declare (usual-integrations))
         (define null)                  ;unassigned
         (define (map1 f l)
           (let loop ((l l))
             (if (pair? l)
                 (cons (f (car l)) (loop (cdr l)))
                 null)))
         map1))
    (let* ((env (make-top-level-environment))
           (scode (syntax&integrate program '() env))
           (expr (compile-scode scode))
           (map1 (eval expr env)))
      (with-expected-failure
          (if (memq microcode-id/compiled-code-type '(i386 x86-64))
              expect-failure
              #!default)
        (lambda ()
          (assert-equal
           (bind-condition-handler (list condition-type:unassigned-variable)
               (lambda (condition)
                 condition
                 (use-value '()))
             (lambda ()
               (map1 - '(1 2 3))))
           '(-1 -2 -3)))))))