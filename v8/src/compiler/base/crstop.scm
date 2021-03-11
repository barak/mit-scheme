#| -*-Scheme-*-

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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