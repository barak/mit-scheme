#| -*-Scheme-*-

$Id: infstr.scm,v 1.11 2001/08/10 17:09:18 cph Exp $

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
  (expression #f read-only #t)		;dbg-expression
  (procedures #f read-only #t)		;vector of dbg-procedure
  (continuations #f read-only #t)	;vector of dbg-continuation
  (labels/desc #f read-only #f)		;vector of dbg-label, sorted by offset
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
  (block #f read-only #t)		;dbg-block
  (label #f)				;dbg-label
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
  (block #f read-only #t)		;dbg-block
  (label #f)				;dbg-label
  (type #f read-only #t)
  (name #f read-only #t)		;procedure's name
  (required #f read-only #t)		;names of required arguments
  (optional #f read-only #t)		;names of optional arguments
  (rest #f read-only #t)		;name of rest argument, or #F
  (auxiliary #f read-only #t)		;names of internal definitions
  (external-label #f)			;for closure, external entry
  (source-code #f read-only #t)		;SCode
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
  (block #f read-only #t)		;dbg-block
  (label #f)				;dbg-label
  (type #f read-only #t)
  (offset #f read-only #t)		;difference between sp and block
  (source-code #f read-only #t)
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
  (type #f read-only #t)		;continuation, stack, closure, ic
  (parent #f read-only #t)		;parent block, or #F
  (original-parent #f read-only #t)	;for closures, closing block
  (layout #f read-only #t)		;vector of names, except #F for ic
  (stack-link #f read-only #t)		;next block on stack, or #F
  (procedure #f)			;procedure which this is block of
  )

(define-structure (dbg-variable
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime compiler-info)dbg-variable]"))
		   (conc-name dbg-variable/))
  (name #f read-only #t)		;symbol
  (type #f read-only #t)		;normal, cell, integrated
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
  (name #f)				;a string, primary name
  (offset #f read-only #t)		;mach. dependent offset into code block
  (external? #f)			;if true, can have pointer to this
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

;;;; Debugging-info wrappers

(define (compiled-code-block/debugging-wrapper block)
  (let ((wrapper (compiled-code-block/debugging-info block)))
    (if (debugging-wrapper? wrapper)
	wrapper
	(let ((wrapper (convert-old-debugging-wrapper wrapper)))
	  (if wrapper
	      (set-compiled-code-block/debugging-info! block wrapper))
	  wrapper))))

(define (debugging-wrapper? wrapper)
  (and (vector? wrapper)
       (fix:= (vector-length wrapper) 6)
       (eq? (vector-ref wrapper 0) 'DEBUGGING-INFO-WRAPPER)
       (or (fix:= (vector-ref wrapper 1) 1)
	   (fix:= (vector-ref wrapper 1) 2))
       (or (and (not (vector-ref wrapper 2))
		(not (vector-ref wrapper 3))
		(not (vector-ref wrapper 4))
		(dbg-info? (vector-ref wrapper 5)))
	   (and (if (fix:= (vector-ref wrapper 1) 1)
		    (not (vector-ref wrapper 2))
		    (dbg-info-key? (vector-ref wrapper 2)))
		(debug-info-pathname? (vector-ref wrapper 3))
		(index-fixnum? (vector-ref wrapper 4))
		(or (not (vector-ref wrapper 5))
		    (dbg-info? (vector-ref wrapper 5)))))))

(define (debugging-wrapper/version wrapper)
  (vector-ref wrapper 1))

(define (debugging-wrapper/key wrapper)
  (vector-ref wrapper 2))

(define (debugging-wrapper/pathname wrapper)
  (vector-ref wrapper 3))

(define (set-debugging-wrapper/pathname! wrapper pathname)
  (vector-set! wrapper 3 pathname))

(define (debugging-wrapper/index wrapper)
  (vector-ref wrapper 4))

(define (debugging-wrapper/info wrapper)
  (vector-ref wrapper 5))

(define (set-debugging-wrapper/info! wrapper info)
  (vector-set! wrapper 5 info))

(define (convert-old-debugging-wrapper wrapper)
  (let ((make-wrapper
	 (lambda (pathname index info)
	   (vector 'DEBUGGING-INFO-WRAPPER 1 #f pathname index info))))
    (cond ((dbg-info? wrapper)
	   (make-wrapper #f #f wrapper))
	  ((debug-info-pathname? wrapper)
	   (make-wrapper wrapper 0 #f))
	  ((and (pair? wrapper)
		(debug-info-pathname? (car wrapper))
		(dbg-info? (cdr wrapper)))
	   (make-wrapper (car wrapper) 0 (cdr wrapper)))
	  ((and (pair? wrapper)
		(debug-info-pathname? (car wrapper))
		(index-fixnum? (cdr wrapper))
		(fix:> (cdr wrapper) 0))
	   (make-wrapper (car wrapper) (cdr wrapper) #f))
	  ((and (pair? wrapper)
		(pair? (car wrapper))
		(debug-info-pathname? (caar wrapper))
		(index-fixnum? (cdar wrapper))
		(fix:> (cdar wrapper) 0)
		(dbg-info? (cdr wrapper)))
	   (make-wrapper (caar wrapper) (cdar wrapper) (cdr wrapper)))
	  (else #f))))

(define (debugging-file-wrapper? wrapper)
  (and (vector? wrapper)
       (fix:= (vector-length wrapper) 4)
       (eq? (vector-ref wrapper 0) 'DEBUGGING-FILE-WRAPPER)
       (or (and (fix:= (vector-ref wrapper 1) 1)
		(not (vector-ref wrapper 2)))
	   (and (fix:= (vector-ref wrapper 1) 2)
		(dbg-info-key? (vector-ref wrapper 2))))
       (let ((info (vector-ref wrapper 3)))
	 (let ((n (vector-length info)))
	   (and (fix:>= n 1)
		(let loop ((i 0))
		  (or (fix:= i n)
		      (and (dbg-info? (vector-ref info i))
			   (loop (fix:+ i 1))))))))))

(define (debugging-file-wrapper/version wrapper)
  (vector-ref wrapper 1))

(define (debugging-file-wrapper/key wrapper)
  (vector-ref wrapper 2))

(define (debugging-file-wrapper/info wrapper)
  (vector-ref wrapper 3))

(define (canonicalize-file-wrapper wrapper)
  (cond ((debugging-file-wrapper? wrapper)
	 wrapper)
	((dbg-info? wrapper)
	 (vector 'DEBUGGING-FILE-WRAPPER 1 #f (vector wrapper)))
	((and (vector? wrapper)
	      (let ((n (vector-length wrapper)))
		(and (fix:>= n 1)
		     (let loop ((i 0))
		       (or (fix:= i n)
			   (and (dbg-info? (vector-ref wrapper i))
				(loop (fix:+ i 1))))))))
	 (vector 'DEBUGGING-FILE-WRAPPER 1 #f wrapper))
	(else #f)))

(define (get-wrapped-dbg-info file-wrapper wrapper)
  (and (let ((k1 (debugging-wrapper/key wrapper))
	     (k2 (debugging-file-wrapper/key file-wrapper)))
	 (or (and k1 k2 (dbg-info-key=? k1 k2))
	     (and (not k1) (not k2))))
       (let ((v (debugging-file-wrapper/info file-wrapper))
	     (index (debugging-wrapper/index wrapper)))
	 (and (fix:< index (vector-length v))
	      (vector-ref v index)))))

(define (dbg-info-key? object)
  (and (string? object)
       (fix:= (string-length object) 32)))

(define (dbg-info-key=? a b)
  (string=? a b))

(define (debug-info-pathname? object)
  (or (pathname? object)
      (string? object)))