#| -*-Scheme-*-

$Id: infstr.scm,v 1.10 2001/03/21 19:15:10 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Compiled Code Information: Structures
;;; package: (runtime compiler-info)

(declare (usual-integrations))

(define-integrable (make-dbg-info-vector info-vector)
  (cons dbg-info-vector-tag info-vector))

(define (dbg-info-vector? object)
  (and (pair? object) (eq? (car object) dbg-info-vector-tag)))

(define-integrable (dbg-info-vector/items info-vector)
  (cdr info-vector))

(define-integrable dbg-info-vector-tag
  ((ucode-primitive string->symbol)
   "#[(runtime compiler-info)dbg-info-vector-tag]"))

(define-structure (dbg-info
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-info]"))
		   (conc-name dbg-info/))
  (expression false read-only true)	;dbg-expression
  (procedures false read-only true)	;vector of dbg-procedure
  (continuations false read-only true)	;vector of dbg-continuation
  (labels/desc false read-only false)	;vector of dbg-label, sorted by offset
  )

(define (dbg-info/labels dbg-info)
  (let ((labels/desc (dbg-info/labels/desc dbg-info)))
    (if (vector? labels/desc)
	labels/desc
	(let ((labels (read-labels labels/desc)))
	  (and labels
	       (begin
		 (set-dbg-info/labels/desc! dbg-info labels)
		 labels))))))

(define-structure (dbg-expression
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-expression]"))
		   (conc-name dbg-expression/))
  (block false read-only true)		;dbg-block
  (label false)				;dbg-label
  )

(define-integrable (dbg-expression/label-offset expression)
  (dbg-label/offset (dbg-expression/label expression)))

(define-structure (dbg-procedure
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-procedure]"))
		   (constructor
		    make-dbg-procedure
		    (block label type name required optional rest auxiliary
			   source-code))
		   (conc-name dbg-procedure/))
  (block false read-only true)		;dbg-block
  (label false)				;dbg-label
  (type false read-only true)
  (name false read-only true)		;procedure's name
  (required false read-only true)	;names of required arguments
  (optional false read-only true)	;names of optional arguments
  (rest false read-only true)		;name of rest argument, or #F
  (auxiliary false read-only true)	;names of internal definitions
  (external-label false)		;for closure, external entry
  (source-code false read-only true)	;SCode
  )

(define (dbg-procedure/label-offset procedure)
  (dbg-label/offset
   (or (dbg-procedure/external-label procedure)
       (dbg-procedure/label procedure))))

(define-integrable (dbg-procedure<? x y)
  (< (dbg-procedure/label-offset x) (dbg-procedure/label-offset y)))

(define-structure (dbg-continuation
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-continuation]"))
		   (conc-name dbg-continuation/))
  (block false read-only true)		;dbg-block
  (label false)				;dbg-label
  (type false read-only true)
  (offset false read-only true)		;difference between sp and block
  (source-code false read-only true)
  )

(define-integrable (dbg-continuation/label-offset continuation)
  (dbg-label/offset (dbg-continuation/label continuation)))

(define-integrable (dbg-continuation<? x y)
  (< (dbg-continuation/label-offset x) (dbg-continuation/label-offset y)))

(define-structure (dbg-block
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-block]"))
		   (constructor
		    make-dbg-block
		    (type parent original-parent layout stack-link))
		   (conc-name dbg-block/))
  (type false read-only true)		;continuation, stack, closure, ic
  (parent false read-only true)		;parent block, or #F
  (original-parent false read-only true) ;for closures, closing block
  (layout false read-only true)		;vector of names, except #F for ic
  (stack-link false read-only true)	;next block on stack, or #F
  (procedure false)			;procedure which this is block of
  )

(define-structure (dbg-variable
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-variable]"))
		   (conc-name dbg-variable/))
  (name false read-only true)		;symbol
  (type false read-only true)		;normal, cell, integrated
  value					;for integrated, the value
  )

(let-syntax
    ((dbg-block-name
      (macro (name)
	(let ((symbol (symbol-append 'DBG-BLOCK-NAME/ name)))
	  `(DEFINE-INTEGRABLE ,symbol
	     ',((ucode-primitive string->symbol)
		(string-append "#[(runtime compiler-info)"
			       (string-downcase (symbol-name symbol))
			       "]")))))))
  ;; Various names used in `layout' to identify things that wouldn't
  ;; otherwise have names.
  (dbg-block-name dynamic-link)
  (dbg-block-name ic-parent)
  (dbg-block-name normal-closure)
  (dbg-block-name return-address)
  (dbg-block-name static-link))

(define (dbg-label/name label)
  (cond ((dbg-label-2? label) (dbg-label-2/name label))
	((dbg-label-1? label) (dbg-label-1/name label))
	(else
	 (error:wrong-type-argument label "debugging label" 'DBG-LABEL/NAME))))

(define (set-dbg-label/name! label name)
  (cond ((dbg-label-1? label) (set-dbg-label-1/name! label name))
	(else
	 (error:wrong-type-argument label "debugging label"
				    'SET-DBG-LABEL/NAME!))))

(define (dbg-label/offset label)
  (cond ((dbg-label-2? label) (dbg-label-2/offset label))
	((dbg-label-1? label) (dbg-label-1/offset label))
	(else
	 (error:wrong-type-argument label "debugging label"
				    'DBG-LABEL/OFFSET))))

(define (dbg-label/external? label)
  (cond ((dbg-label-2? label) (dbg-label-2/external? label))
	((dbg-label-1? label) (dbg-label-1/external? label))
	(else
	 (error:wrong-type-argument label "debugging label"
				    'DBG-LABEL/EXTERNAL?))))

(define (set-dbg-label/external?! label external?)
  (cond ((dbg-label-2? label) (set-dbg-label-2/external?! label external?))
	((dbg-label-1? label) (set-dbg-label-1/external?! label external?))
	(else
	 (error:wrong-type-argument label "debugging label"
				    'SET-DBG-LABEL/EXTERNAL?!))))

(define (dbg-label/names label)
  (cond ((dbg-label-2? label) (dbg-label-2/names label))
	((dbg-label-1? label) (dbg-label-1/names label))
	(else
	 (error:wrong-type-argument label "debugging label"
				    'DBG-LABEL/NAMES))))

(define (set-dbg-label/names! label names)
  (cond ((dbg-label-1? label) (set-dbg-label-1/names! label names))
	(else
	 (error:wrong-type-argument label "debugging label"
				    'SET-DBG-LABEL/NAMES!))))

(define-structure (dbg-label-1
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-label]"))
		   (constructor make-dbg-label (name offset))
		   (conc-name dbg-label-1/))
  (name false)				;a string, primary name
  (offset false read-only true)		;mach. dependent offset into code block
  (external? false)			;if true, can have pointer to this
  (names (list name))			;names of all labels at this offset
  )

(define-integrable make-dbg-label-2 cons)
(define-integrable dbg-label-2? pair?)
(define-integrable dbg-label-2/name car)
(define-integrable (dbg-label-2/offset label) (abs (cdr label)))
(define-integrable (dbg-label-2/external? label) (negative? (cdr label)))
(define-integrable (dbg-label-2/names label) (list (car label)))

(define (set-dbg-label-2/external?! label external?)
  (let ((offset (cdr label)))
    (if (if external?
	    (not (negative? offset))
	    (negative? offset))
	(set-cdr! label (- offset))))
  unspecific)