#| -*-Scheme-*-

$Id: crstop.scm,v 1.17 2007/01/05 21:19:20 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Cross Compiler Top Level.
;;; This code shares and should be merged with "toplev.scm".
;;; Many of the procedures only differ in the default extensions.

(declare (usual-integrations))

(define (cross-compile-bin-file input-string #!optional output-string)
  (let ((input-default
	 (make-pathname false false false false "bin" 'NEWEST))
	(output-default
	 (make-pathname false false false false "moc" false)))
    (compiler-pathnames
     input-string
     (if (not (default-object? output-string))
	 output-string
	 (merge-pathnames output-default
			  (merge-pathnames input-string input-default)))
     input-default
     (lambda (input-pathname output-pathname)
       (maybe-open-file compiler:generate-rtl-files?
			(pathname-new-type output-pathname "rtl")
	 (lambda (rtl-output-port)
	   (maybe-open-file compiler:generate-lap-files?
			    (pathname-new-type output-pathname "lap")
	     (lambda (lap-output-port)
	       (cross-compile-scode (compiler-fasload input-pathname)
				    (pathname-new-type output-pathname
						       "fni")
				    rtl-output-port
				    lap-output-port)))))))))

(define (cross-compile-bin-file-end input-string #!optional output-string)
  (compiler-pathnames
   input-string
   (and (not (default-object? output-string)) output-string)
   (make-pathname false false false false "moc" 'NEWEST)
   (lambda (input-pathname output-pathname)
     output-pathname			; ignored
     (cross-compile-scode-end (compiler-fasload input-pathname)))))

(define (cross-compile-scode-end cross-compilation)
  (in-compiler
   (lambda ()
     (cross-link-end cross-compilation)
     *result*)))

;;; This should be merged with compile-scode

(define (cross-compile-scode scode
			     #!optional
			     info-output-pathname
			     rtl-output-port
			     lap-output-port
			     wrapper)
  (let ((info-output-pathname
	 (if (default-object? info-output-pathname)
	     false
	     info-output-pathname))
	(rtl-output-port
	 (if (default-object? rtl-output-port) false rtl-output-port))
	(lap-output-port
	 (if (default-object? lap-output-port) false lap-output-port))
	(wrapper
	 (if (default-object? wrapper) in-compiler wrapper)))
    (fluid-let ((compiler:compile-by-procedures? false)
		(compiler:cross-compiling? true)
		(compiler:dump-info-file compiler:dump-inf-file)
		(*info-output-filename*
		 (if (pathname? info-output-pathname)
		     (->namestring info-output-pathname)
		     *info-output-filename*))
		(*rtl-output-port* rtl-output-port)
		(*lap-output-port* lap-output-port))
      ((if (default-object? wrapper)
	   in-compiler
	   wrapper)
       (lambda ()
	 (set! *input-scode* scode)
	 (phase/fg-generation)
	 (phase/fg-optimization)
	 (phase/rtl-generation)
	 (phase/rtl-optimization)
	 (if rtl-output-port
	     (phase/rtl-file-output rtl-output-port))
	 (phase/lap-generation)
	 (phase/lap-linearization)
	 (if lap-output-port
	     (phase/lap-file-output lap-output-port))
	 (phase/assemble)
	 ;; Here is were this procedure differs
	 ;; from compile-scode
	 (if info-output-pathname
	     (cross-compiler-phase/info-generation-2 info-output-pathname))
	 (cross-compiler-phase/link)
	 *result*)))))

(define-structure (cc-code-block (type vector)
				 (conc-name cc-code-block/))
  (debugging-info false read-only false)
  (bit-string false read-only true)
  (objects false read-only true)
  (object-width false read-only true))

(define-structure (cc-vector (type vector)
			     (constructor cc-vector/make)
			     (conc-name cc-vector/))
  (code-vector false read-only true)
  (entry-label false read-only true)
  (entry-points false read-only true)
  (label-bindings false read-only true)
  (ic-procedure-headers false read-only true))

(define (cross-compiler-phase/info-generation-2 pathname)
  (info-generation-2 pathname set-cc-code-block/debugging-info!))

(define (cross-compiler-phase/link)
  (compiler-phase
   "Cross Linkification"
   (lambda ()
     (set! *result*
	   (cc-vector/make *code-vector*
			   (last-reference *entry-label*)
			   (last-reference *entry-points*)
			   (last-reference *label-bindings*)
			   (last-reference *ic-procedure-headers*)))
     unspecific)))

(define (cross-link-end cc-vector)
  (set! *code-vector* (cc-vector/code-vector cc-vector))
  (set! *entry-label* (cc-vector/entry-label cc-vector))
  (set! *entry-points* (cc-vector/entry-points cc-vector))
  (set! *label-bindings* (cc-vector/label-bindings cc-vector))
  (set! *ic-procedure-headers* (cc-vector/ic-procedure-headers cc-vector))
  (phase/link))