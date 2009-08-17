#| -*-Scheme-*-

$Id: ca4ad2594b82cd73147b9714a79f73c9a42f0427 $

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

;;;; Compiled Code Information: Structures
;;; package: (runtime compiler-info)

(declare (usual-integrations))

;;;; Compiled files
;;
;; A COMPILED-MODULE structure is the thing that lives in a .com file.
;; It contains everything that the system needs to know to load and
;; execute the file.  Note that having a data structure rather than an
;; scode expression complicates the boot process as make.scm must be
;; an scode (or compiled) expression.  This can be fixed by editing
;; the make.com file or by -fasl-ing a .bin file that evals the
;; module's expression.

(define-structure
    (compiled-module
     (type vector)
     (named
      ((ucode-primitive string->symbol)
       "#[(runtime compiler-info)compiled-module]"))
     (conc-name compiled-module/)
     (constructor make-compiled-module
		  (expression all-compiled-code-blocks
		   dbg-locator purification-root)))
  (version compiled-module-format:current-version read-only true)
  (expression false read-only true)	;top level expression of file
  (all-compiled-code-blocks false)	;in a vector
  (dbg-locator false)			;how to find debugging info
  (purification-root false)		;what should be purified?
  (linkage 'EXECUTE)			;How to link it? (not used yet)
  (extra false))

(define compiled-module-format:current-version 0)
(define compiled-module-format:oldest-acceptable-version 0)

;; A compiled code block's debugging-info slot contains one of
;;  (1) A DBG-INFO object.
;;  (1) A pair (dbg-locator . recursive-compilation-number-or-0).  This pair
;;      is called a `descriptor' in infutl.scm.
;;  (2) A pair of a (dbg-info . `(2)'), while the dbg info is in core.
;;  (3) something else => no info
;; All of the compiled code blocks in a compiled file structurally share
;; the same DBG-LOCATOR which is also accessible from the COMPILED-MODULE.

(define-structure
    (dbg-locator
     (type vector)
     (named
      ((ucode-primitive string->symbol)
       "#[(runtime compiler-info)dbg-locator]"))
     (constructor make-dbg-locator (file timestamp))
     (conc-name dbg-locator/)
     (print-procedure
      (standard-unparser-method 'DBG-LOCATOR
	(lambda (locator port)
	  (write-char #\space port)
	  (write (->namestring (dbg-locator/file locator)) port)))))

  (file false)				;pathname or canonicalized string
  (timestamp false read-only true)
  (status false))			;for system bookkeeping


;; Any debugging information that is fasdumped to a file has a
;; DBG-WRAPPER around it.  The purpose of this is to ensure that
;; debugging information comes from the same compilation as the
;; dbg-locator (EQUAL? timestamps), and is in an acceptable format.

(define-structure (dbg-wrapper
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-wrapper]"))
		   (constructor make-dbg-wrapper (objects timestamp))
		   (conc-name dbg-wrapper/))
  (objects false read-only true) ;a vector indexed by
  (timestamp false read-only true)
  (format-version dbg-format:current-version read-only true))


;; Change these when the format of any DBG-* object changes, or the path
;; language is extended.

(define dbg-format:current-version 0)
(define dbg-format:oldest-acceptable-version 0)

;; A DBG-INFO holds the information pertaining to a single compiled code
;; block.

(define-structure (dbg-info
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-info]"))
		   (conc-name dbg-info/))
  (expression false read-only true)	;dbg-expression
  (procedures false read-only true)	;vector of dbg-procedure
  (continuations false read-only true)	;vector of dbg-continuation
  ;; vector of dbg-label, sorted by offset, or 'DUMPED-SEPARATELY, or #F if
  ;; not dumped at all.
  (labels/desc false read-only false))

(define-structure (dbg-expression
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)new-dbg-expression]"))
		   (conc-name dbg-expression/))
  (block false)				;dbg-block
  (label false)				;dbg-label
  (source-code false))

(define-integrable (dbg-expression/label-offset expression)
  (dbg-label/offset (dbg-expression/label expression)))


(define-structure (dbg-procedure
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)new-dbg-procedure]"))
		   (conc-name dbg-procedure/)
		   (constructor make-dbg-procedure (source-code))
		   (constructor %make-dbg-procedure))
  (block false read-only false)
  (label false read-only false)
  (source-code false read-only true))

(define (dbg-procedure/name dbg-procedure)
  (let ((scode  (dbg-procedure/source-code dbg-procedure)))
    (lambda-name scode)))

(define (dbg-procedure/label-offset procedure)
  (dbg-label/offset
   (or ;;(dbg-procedure/external-label procedure)
       (dbg-procedure/label procedure))))

(define-integrable (dbg-procedure<? x y)
  (< (dbg-procedure/label-offset x) (dbg-procedure/label-offset y)))

(define-structure (dbg-continuation
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)new-dbg-continuation]"))
		   (conc-name dbg-continuation/))
  (block false)				;dbg-block
  (label false)				;dbg-label
  (type false read-only true)
  (outer false)				; source code
  (inner false)				; source code
  )

(define-integrable (dbg-continuation/label-offset continuation)
  (dbg-label/offset (dbg-continuation/label continuation)))

(define-integrable (dbg-continuation<? x y)
  (< (dbg-continuation/label-offset x) (dbg-continuation/label-offset y)))

(define-structure (dbg-block
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)new-dbg-block]"))
		   (constructor make-dbg-block (type parent variables))
		   (conc-name dbg-block/))
  (type false read-only true)		;continuation, stack, closure, ic
  (parent false read-only true)		;parent block, or #F
  (parent-path-prefix false)		;
  (variables false read-only true)	;vector of variables, except #F for ic
  (procedure false)			;procedure/entry which this is block of
  )

;;(define-structure (dbg-variable
;;		   (type vector)
;;		   (named
;;		    ((ucode-primitive string->symbol)
;;		     "#[(runtime compiler-info)new-dbg-variable]"))
;;		   (conc-name dbg-variable/))
;;  (name false read-only true)		;symbol
;;  (path false read-only true))

;; Pairs are more compact 
(define (dbg-variable? object)
  (and (pair? object) (symbol? (car object))))

(define-integrable (dbg-variable/make name) (cons name #F))
(define-integrable (dbg-variable/name var) (car var))
(define-integrable (dbg-variable/path var) (cdr var))

(define-integrable (guarantee-dbg-label object procedure)
  (if (not (pair? object))
      (error:wrong-type-argument object "debugging label" procedure)))

(define (make-dbg-label name offset)
  (cons name offset))

(define (dbg-label/name label)
  (guarantee-dbg-label label 'DBG-LABEL/NAME)
  (car label))

(define (dbg-label/offset label)
  (guarantee-dbg-label label 'DBG-LABEL/OFFSET)
  (abs (cdr label)))

(define (dbg-label/external? label)
  (guarantee-dbg-label label DBG-LABEL/EXTERNAL?)
  (negative? (cdr label)))

(define (set-dbg-label/external?! label external?)
  (guarantee-dbg-label label 'SET-DBG-LABEL/EXTERNAL?!)
  (let ((offset (abs (cdr label))))
    (if external?
	(set-cdr! label (- offset))
	(set-cdr! label offset)))
  unspecific)