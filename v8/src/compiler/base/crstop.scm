#| -*-Scheme-*-

$Id: crstop.scm,v 1.2 1995/07/13 20:23:21 adams Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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
MIT in each case. |#

;;;; Cross Compiler Top Level.
;;; This code shares and should be merged with "toplev.scm".
;;; Many of the procedures only differ in the default extensions.

(declare (usual-integrations))

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
  (info-generation-2 pathname
		     set-cc-code-block/debugging-info!
		     (lambda (constant) #f)))

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