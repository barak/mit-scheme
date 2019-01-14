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

;;;; Program to compile MIT/GNU Scheme.

;;; This compiles the part of the system written in Scheme.
;;; The part written in C is compiled using "make".

(define (compile-everything)
  (compile-all-dirs compile-dir))

(define (compile-all-dirs compile-dir)
  (compile-boot-dirs compile-dir)
  (compile-remaining-dirs compile-dir))

(define (compile-remaining-dirs compile-dir)
  (compile-dir "sos")
  (with-working-directory-pathname "sos"
    (lambda ()
      (load "load")))
  (for-each compile-dir '("xml" "win32" "ssp" "ffi")))

(define (compile-boot-dirs compile-dir)
  (compile-cref compile-dir)
  (for-each compile-dir '("runtime" "cref" "sf" "compiler" "star-parser")))

(define (compile-cref compile-dir)
  (compile-dir "cref")
  (if (not (name->package '(cross-reference)))
      (with-working-directory-pathname "cref"
	(lambda ()
	  (load "make")))))

(define (compile-dir name)
  (with-working-directory-pathname name
    (lambda ()
      (if (file-exists? (pathname-new-type name "sf"))
	  (begin
	    (load (pathname-new-type name "sf"))
	    (echo-cref-output name)
	    (load (pathname-new-type name "cbf")))
	  (load "compile"))))
  ;; Run secondary GC daemons and whatnot.
  (gc-clean))

(define (echo-cref-output name)
  (let ((cref-output-file (pathname-new-type (package-set-pathname name) "crf")))
    (if (file-exists? cref-output-file)
	(call-with-input-file cref-output-file
	  (lambda (inport)
	    (do ((line (read-line inport) (read-line inport)))
		((eof-object? line))
	      (write-string line)
	      (newline)))))))

(define (compile-bootstrap-1)
  (load-option 'SF)
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "compiler.sf"))))

(define (compile-bootstrap-2)
  (let ((action
	 (lambda ()
	   (with-working-directory-pathname "compiler"
	     (lambda ()
	       (load "compiler.cbf")))
	   (c-compile-pkgs "compiler"))))
    (if (eq? microcode-id/compiled-code-type 'C)
	(in-liarc action)
	(action))))

(define (c-prepare)
  (load-option 'SF)
  (with-working-directory-pathname "compiler"
    (lambda ()
      (load "make")))
  (in-liarc
   (lambda ()
     (compile-boot-dirs c-compile-dir))))

(define (native-prepare)
  (load-option 'SF)
  (load-option 'CREF)
  (let ((make-file (compiler-make-file)))
    (with-working-directory-pathname "compiler"
      (lambda ()
	(load "compiler.so")
	(load make-file))))
  (fluid-let ((compiler:cross-compiling? #t))
    (compile-boot-dirs compile-dir)))

(define (compiler-make-file)
  (string-append
   (or (file-symbolic-link? "compiler/machine")
       (error "Missing compiler/machine link."))
   "/make"))

(define (c-compile)
  (in-liarc
   (lambda ()
     (compile-all-dirs c-compile-dir))))

(define (in-liarc thunk)
  (fluid-let ((compiler:invoke-c-compiler? #f)
	      (in-liarc? #t))
    (thunk)))

(define in-liarc? #f)

(define (c-compile-dir name)
  (compile-dir name)
  (c-compile-pkgs name))

(define (c-compile-pkgs name)
  (let ((root
	 (if (string=? name "star-parser")
	     "parser"
	     name)))
    (with-working-directory-pathname name
      (lambda ()
	(let ((compile-pkg
	       (lambda (os)
		 (cbf-conditionally (string-append root "-" os ".pkd")))))
	  (compile-pkg "unx")
	  (compile-pkg "w32"))))))

(define (cbf-conditionally pathname)
  (let ((input (pathname-default-type pathname "bin")))
    (if (file-modification-time<? (compiler-output-pathname pathname)
				  input)
	(cbf input))))

(define (cf-conditionally pathname)
  (let ((input (pathname-default-type pathname "scm")))
    (if (file-modification-time<? (compiler-output-pathname pathname)
				  input)
	(cf input))))

(define (compiler-output-pathname pathname)
  (pathname-new-type pathname (if in-liarc? "c" "com")))