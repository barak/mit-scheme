#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Script to run the tests

;++ This whole script is a horrible kludge.  Please rewrite it!

(declare (usual-integrations))

;;; Suppress useless (expected) error reports from a ucode that has
;;; --enabled-debugging.
(if ((make-primitive-procedure 'get-primitive-address) 'set-debug-flags! #f)
    ((make-primitive-procedure 'set-debug-flags!) 15 #f)) ;D_PRINT_ERRORS

;;; Can't just look at */test-*.scm because not everything has been
;;; converted to use the automatic framework.

(define known-tests
  '(
    "microcode/test-chacha"
    ;++ Kludge to run the flonum cast tests interpreted and compiled --
    ;++ the compiler has a bug with negative zero.
    "microcode/test-flonum-casts"
    "microcode/test-flonum-casts.scm"
    "microcode/test-flonum-casts.com"
    "microcode/test-keccak"
    "microcode/test-lookup"
    "runtime/test-arith"
    "runtime/test-binary-port"
    "runtime/test-bundle"
    "runtime/test-bytevector"
    ("runtime/test-char" (runtime))
    ("runtime/test-char-set" (runtime character-set))
    ("runtime/test-compound-predicate" (runtime compound-predicate))
    "runtime/test-division"
    "runtime/test-dragon4"
    "runtime/test-dynamic-env"
    "runtime/test-entity"
    "runtime/test-ephemeron"
    ("runtime/test-file-attributes" (runtime))
    "runtime/test-floenv"
    "runtime/test-hash-table"
    "runtime/test-integer-bits"
    ("runtime/test-library-parser" (runtime library))
    ("runtime/test-library-standard" (runtime library))
    ("runtime/test-library-imports" (runtime library))
    ("runtime/test-library-loader" (runtime library))
    "runtime/test-md5"
    "runtime/test-mime-codec"
    ("runtime/test-parametric-predicate" (runtime parametric-predicate))
    "runtime/test-predicate"
    ("runtime/test-predicate-dispatch" (runtime predicate-dispatch))
    "runtime/test-process"
    "runtime/test-readwrite"
    "runtime/test-regsexp"
    "runtime/test-rgxcmp"
    "runtime/test-sha3"
    "runtime/test-string"
    "runtime/test-string-normalization"
    "runtime/test-string-search"
    "runtime/test-syncproc"
    "runtime/test-syntax-rename"
    "runtime/test-thread-queue"
    "runtime/test-url"
    ("runtime/test-wttree" (runtime wt-tree))
    "ffi/test-ffi"
    "sos/test-genmult"
    ))

(with-working-directory-pathname
    (directory-pathname (current-load-pathname))
  (lambda ()
    (load "load")
    (let ((results
	   (map (lambda (entry)
		  (receive (pathname environment)
		      (if (pair? entry)
			  (values (car entry) (->environment (cadr entry)))
			  (values entry #!default))
		    (with-notification
		     (lambda (output-port)
		       (write-string "Running tests in " output-port)
		       (write pathname output-port)
		       (if (not (default-object? environment))
			   (begin
			     (write-string " in environment " output-port)
			     (write (cond ((environment->package environment)
					   => package/name)
					  (else environment))
				    output-port))))
		     (lambda ()
		       (if (not (pathname-type pathname))
			   (with-working-directory-pathname
			    (directory-pathname pathname)
			    (lambda ()
			      ;;++ Kludge around a bug in SF...
			      (compile-file (file-pathname pathname)
					    '()
					    environment))))
		       (let* ((t (pathname-type pathname))
			      (p (if (and t (string=? "com" t)
					  (eq? 'C
					       microcode-id/compiled-code-type))
				     (pathname-new-type pathname "so")
				     pathname)))
			 (cons pathname
			       (run-unit-tests p environment)))))))
		(let ((test-name (get-environment-variable "TEST")))
		  (if test-name
		      (let ((e
			     (find (lambda (e)
				     (string=? test-name
					       (if (pair? e) (car e) e)))
				   known-tests)))
			(if e
			    (list e)
			    (begin
			      (warn "Unknown test name:" test-name)
			      '())))
		      known-tests)))))

      (define (show-results results tag)
	(for-each (lambda (p)
		    (write-string tag)
		    (write-string ": ")
		    (write (car p))
		    (newline))
		  results))

      (fresh-line)
      (newline)
      (let ((passed (filter cdr results))
	    (failed (remove cdr results)))
	(if (or (pair? passed)
		(pair? failed))
	    (begin
	      (write-string "Test results:")
	      (newline)
	      (show-results passed "PASSED")
	      (if (and (pair? passed)
		       (pair? failed))
		  (newline))
	      (show-results failed "FAILED"))
	    (begin
	      (write-string "No tests run")
	      (newline)))))))