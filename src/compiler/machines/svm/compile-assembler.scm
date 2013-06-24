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

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (let ((environment (make-top-level-environment)))
      #;
      (environment-define-macro environment 'LAP
	(rsc-macro-transformer
	 (lambda (form environment)
	   (if (syntax-match? '(* DATUM) (cdr form))
	       `(,(close-syntax 'QUASIQUOTE environment) ,(cdr form))
	       (ill-formed-syntax form)))))

      ;; The 20090107 snapshot does not have write-mit-scheme-copyright.
      (if (not (environment-bound? environment 'WRITE-MIT-SCHEME-COPYRIGHT))
	  (begin
	    (eval '(define inits '()) environment)
	    (eval '(define (add-boot-init! thunk)
		     (set! inits (cons thunk inits))) environment)
	    (load "../../../runtime/version" environment)
	    (eval '(for-each (lambda (thunk) (thunk)) inits) environment)))

      (load "machine" environment)
      (load "assembler-runtime" environment)
      (load "assembler-compiler" environment)
      ((access compile-assembler-rules environment) "assembler-rules.scm"))))