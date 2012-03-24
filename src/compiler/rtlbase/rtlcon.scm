#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Register Transfer Language: Complex Constructors
;;; package: (compiler)

(declare (usual-integrations))

;;;; Statements

(define (rtl:make-assignment locative expression)
  (locative-dereference-for-statement locative
    (lambda (locative)
      (let ((receiver
	     (lambda (expression)
	       (rtl:make-assignment-internal locative expression))))
	(if (rtl:pseudo-register-expression? locative)
	    (expression-simplify-for-pseudo-assignment expression receiver)
	    (expression-simplify-for-statement expression receiver))))))

(define (rtl:make-assignment-internal locative expression)
  (cond ((and (or (rtl:register? locative) (rtl:offset? locative))
	      (equal? locative expression))
	 (make-null-cfg))
	((or (rtl:register? locative) (rtl:register? expression))
	 (%make-assign locative expression))
	(else
	 (let ((register (rtl:make-pseudo-register)))
	   (scfg*scfg->scfg! (%make-assign register expression)
			     (%make-assign locative register))))))

(define (rtl:make-pop locative)
  (locative-dereference-for-statement locative
    (lambda (locative)
      (rtl:make-assignment-internal locative (stack-pop-address)))))

(define (rtl:make-push expression)
  (expression-simplify-for-statement expression
    (lambda (expression)
      (rtl:make-assignment-internal (stack-push-address) expression))))

(define (rtl:make-eq-test expression-1 expression-2)
  (expression-simplify-for-predicate expression-1
    (lambda (expression-1)
      (expression-simplify-for-predicate expression-2
	(lambda (expression-2)
	  (%make-eq-test expression-1 expression-2))))))

(define (rtl:make-false-test expression)
  (rtl:make-eq-test expression (rtl:make-constant false)))

(define (rtl:make-true-test expression)
  (pcfg-invert (rtl:make-false-test expression)))

(define (rtl:make-type-test expression type)
  (expression-simplify-for-predicate expression
    (lambda (expression)
      (%make-type-test expression type))))

(define (rtl:make-pred-1-arg predicate operand)
  (expression-simplify-for-predicate operand
    (lambda (operand)
      (%make-pred-1-arg predicate operand))))

(define (rtl:make-pred-2-args predicate operand1 operand2)
  (expression-simplify-for-predicate operand1
    (lambda (operand1)
      (expression-simplify-for-predicate operand2
	(lambda (operand2)
	  (%make-pred-2-args predicate operand1 operand2))))))

(define (rtl:make-unassigned-test expression)
  (rtl:make-eq-test
   expression
   (rtl:make-cons-non-pointer
    (rtl:make-machine-constant (ucode-type unassigned))
    (rtl:make-machine-constant 0))))

(define (rtl:make-fixnum-pred-1-arg predicate operand)
  (expression-simplify-for-predicate operand
    (lambda (operand)
      (%make-fixnum-pred-1-arg predicate operand))))

(define (rtl:make-fixnum-pred-2-args predicate operand1 operand2)
  (expression-simplify-for-predicate operand1
    (lambda (operand1)
      (expression-simplify-for-predicate operand2
	(lambda (operand2)
	  (%make-fixnum-pred-2-args predicate operand1 operand2))))))

(define (rtl:make-flonum-pred-1-arg predicate operand)
  (expression-simplify-for-predicate operand
    (lambda (operand)
      (%make-flonum-pred-1-arg predicate operand))))

(define (rtl:make-flonum-pred-2-args predicate operand1 operand2)
  (expression-simplify-for-predicate operand1
    (lambda (operand1)
      (expression-simplify-for-predicate operand2
	(lambda (operand2)
	  (%make-flonum-pred-2-args predicate operand1 operand2))))))

(define (rtl:make-push-return continuation)
  (rtl:make-push
   (rtl:make-cons-pointer (rtl:make-machine-constant type-code:compiled-entry)
			  (rtl:make-entry:continuation continuation))))

(define (rtl:make-push-link)
  (rtl:make-push
   (rtl:make-environment (rtl:make-fetch register:dynamic-link))))

(define (rtl:make-pop-link)
  (rtl:make-assignment register:dynamic-link
		       (rtl:make-object->address (stack-pop-address))))

(define (rtl:make-stack-pointer->link)
  (rtl:make-assignment register:dynamic-link
		       (rtl:make-fetch register:stack-pointer)))

(define (rtl:make-link->stack-pointer)
  (rtl:make-assignment register:stack-pointer
		       (rtl:make-fetch register:dynamic-link)))

(define (rtl:make-constant value)
  (if (unassigned-reference-trap? value)
      (rtl:make-cons-non-pointer
       (rtl:make-machine-constant type-code:unassigned)
       (rtl:make-machine-constant 0))
      (%make-constant value)))

;;; Interpreter Calls

(define rtl:make-interpreter-call:access)
(define rtl:make-interpreter-call:unassigned?)
(define rtl:make-interpreter-call:unbound?)
(let ((interpreter-lookup-maker
       (lambda (%make)
	 (lambda (cont environment name)
	   (expression-simplify-for-statement environment
	     (lambda (environment)
	       (%make cont environment name)))))))
  (set! rtl:make-interpreter-call:access
	(interpreter-lookup-maker %make-interpreter-call:access))
  (set! rtl:make-interpreter-call:unassigned?
	(interpreter-lookup-maker %make-interpreter-call:unassigned?))
  (set! rtl:make-interpreter-call:unbound?
	(interpreter-lookup-maker %make-interpreter-call:unbound?)))

(define rtl:make-interpreter-call:define)
(define rtl:make-interpreter-call:set!)
(let ((interpreter-assignment-maker
       (lambda (%make)
	 (lambda (cont environment name value)
	   (expression-simplify-for-statement value
	     (lambda (value)
	       (expression-simplify-for-statement environment
		 (lambda (environment)
		   (%make cont environment name value)))))))))
  (set! rtl:make-interpreter-call:define
	(interpreter-assignment-maker %make-interpreter-call:define))
  (set! rtl:make-interpreter-call:set!
	(interpreter-assignment-maker %make-interpreter-call:set!)))

(define (rtl:make-interpreter-call:lookup cont environment name safe?)
  (expression-simplify-for-statement environment
    (lambda (environment)
      (%make-interpreter-call:lookup cont environment name safe?))))

(define (rtl:make-interpreter-call:cache-assignment cont name value)
  (expression-simplify-for-statement name
    (lambda (name)
      (expression-simplify-for-statement value
	(lambda (value)
	  (%make-interpreter-call:cache-assignment cont name value))))))

(define (rtl:make-interpreter-call:cache-reference cont name safe?)
  (expression-simplify-for-statement name
    (lambda (name)
      (%make-interpreter-call:cache-reference cont name safe?))))

(define (rtl:make-interpreter-call:cache-unassigned? cont name)
  (expression-simplify-for-statement name
    (lambda (name)
      (%make-interpreter-call:cache-unassigned? cont name))))

;;;; Expression Simplification

(package (locative-dereference-for-statement
	  expression-simplify-for-statement
	  expression-simplify-for-predicate
	  expression-simplify-for-pseudo-assignment)

(define-export (locative-dereference-for-statement locative receiver)
  (locative-dereference locative scfg*scfg->scfg!
    receiver
    (lambda (register offset granularity)
      (receiver (make-offset register offset granularity)))))

(define-export (expression-simplify-for-statement expression receiver)
  (expression-simplify expression scfg*scfg->scfg! receiver))

(define-export (expression-simplify-for-predicate expression receiver)
  (expression-simplify expression scfg*pcfg->pcfg! receiver))

(define-export (expression-simplify-for-pseudo-assignment expression receiver)
  (let ((entry (hash-table/get expression-methods (car expression) #f)))
    (if entry
	(apply entry receiver scfg*scfg->scfg! (cdr expression))
	(receiver expression))))

(define (expression-simplify expression scfg-append! receiver)
  (if (rtl:register? expression)
      (receiver expression)
      (let ((entry (hash-table/get expression-methods (car expression) #f)))
	(if entry
	    (apply entry
		   (lambda (expression)
		     (if (rtl:register? expression)
			 (receiver expression)
			 (assign-to-temporary expression
					      scfg-append!
					      receiver)))
		   scfg-append!
		   (cdr expression))
	    (assign-to-temporary expression scfg-append! receiver)))))

(define (simplify-expressions expressions scfg-append! generator)
  (let loop ((expressions* expressions) (simplified-expressions '()))
    (if (null? expressions*)
	(generator (reverse! simplified-expressions))
	(expression-simplify (car expressions*) scfg-append!
	  (lambda (expression)
	    (loop (cdr expressions*)
		  (cons expression simplified-expressions)))))))

(define (assign-to-temporary expression scfg-append! receiver)
  (let ((pseudo (rtl:make-pseudo-register)))
    (scfg-append! (rtl:make-assignment-internal pseudo expression)
		  (receiver pseudo))))

(define (make-offset register offset granularity)
  (case granularity
    ((OBJECT)
     (rtl:make-offset register (rtl:make-machine-constant offset)))
    ((BYTE)
     (rtl:make-byte-offset register (rtl:make-machine-constant offset)))
    ((FLOAT)
     (rtl:make-float-offset register (rtl:make-machine-constant offset)))
    (else
     (error "unknown offset granularity" granularity))))

(define (make-offset-address register offset granularity)
  (case granularity
    ((OBJECT)
     (rtl:make-offset-address register offset))
    ((BYTE)
     (rtl:make-byte-offset-address register offset))
    ((FLOAT)
     (rtl:make-float-offset-address register offset))
    (else
     (error "unknown offset granularity" granularity))))

(define (locative-dereference locative scfg-append! if-register if-memory)
  (let ((dereference-fetch
	 (lambda (locative offset granularity)
	   (let ((if-address
		  (lambda (address)
		    (if-memory address offset granularity))))
	     (let ((if-not-address
		    (lambda (register)
		      (assign-to-address-temporary register
						   scfg-append!
						   if-address))))
	       (locative-dereference (cadr locative) scfg-append!
		 (lambda (expression)
		   (let ((register (rtl:register-number expression)))
		     (if (and (machine-register? register)
			      (register-value-class=address? register))
			 (if-address expression)
			 (if-not-address expression))))
		 (lambda (register offset granularity)
		   (assign-to-temporary
		    (make-offset register offset granularity)
		    scfg-append!
		    if-not-address)))))))
	(dereference-constant
	 (lambda (locative offset granularity)
	   (assign-to-temporary locative scfg-append!
	     (lambda (register)
	       (assign-to-address-temporary register scfg-append!
		 (lambda (register)
		   (if-memory register offset granularity))))))))
    (cond ((symbol? locative)
	   (let ((register (rtl:machine-register? locative)))
	     (if register
		 (if-register register)
		 (if-memory (interpreter-regs-pointer)
			    (rtl:interpreter-register->offset locative)
			    'OBJECT))))
	  ((pair? locative)
	   (case (car locative)
	     ((REGISTER)
	      (if-register locative))
	     ((FETCH)
	      (dereference-fetch locative 0 'OBJECT))
	     ((OFFSET)
	      (let ((base (rtl:locative-offset-base locative))
		    (offset (rtl:locative-offset-offset locative))
		    (granularity (rtl:locative-offset-granularity locative)))
		(if (not (pair? base))
		    (error "offset base not pair" locative))
		(case (car base)
		  ((FETCH)
		   (dereference-fetch base offset granularity))
		  ((CONSTANT)
		   (dereference-constant base offset granularity))
		  ((INDEX)
		   (locative-dereference
		    base
		    scfg-append!
		    (lambda (reg)
		      (error "Can't be a reg" locative reg))
		    (lambda (base* zero granularity*)
		      zero granularity*	; ignored
		      (if-memory base* offset granularity))))
		  ((OFFSET)
		   (locative-dereference
		    base
		    scfg-append!
		    (lambda (reg)
		      (error "Can't be a reg" locative reg))
		    (lambda (base* offset* granularity*)
		      (assign-to-temporary
		       (make-offset-address
			base*
			(rtl:make-machine-constant offset*)
			granularity*)
		       scfg-append!
		       (lambda (base-reg)
			(if-memory base-reg offset granularity))))))
		  (else
		   (error "illegal offset base" locative)))))
	     ((INDEX)
	      (let ((base (rtl:locative-index-base locative))
		    (offset (rtl:locative-index-offset locative))
		    (granularity (rtl:locative-index-granularity locative)))
		(define (finish base-reg-expr offset-expr)
		  (assign-to-temporary
		   (make-offset-address base-reg-expr offset-expr granularity)
		   scfg-append!
		   (lambda (loc-reg-expr)
		     ;; granularity ok?
		     (if-memory loc-reg-expr 0 granularity))))
		(expression-simplify
		 offset
		 scfg-append!
		 (lambda (offset-expr)
		   (locative-dereference
		    base
		    scfg-append!
		    (lambda (base-reg-expr)
		      (finish base-reg-expr offset-expr))
		    (lambda (base*-reg-expr offset* granularity*)
		      (if (zero? offset*)
			  (finish base*-reg-expr offset-expr)
			  (assign-to-temporary
			   (make-offset-address
			    base*-reg-expr
			    (rtl:make-machine-constant offset*)
			    granularity*)
			   scfg-append!
			   (lambda (loc-reg-expr)
			     (finish loc-reg-expr offset-expr))))))))))
	     ((CONSTANT)
	      (dereference-constant locative 0 'OBJECT))
	     (else
	      (error "unknown keyword" locative))))
	  (else
	   (error "illegal locative" locative)))))

(define (assign-to-address-temporary expression scfg-append! receiver)
  (let ((pseudo (rtl:make-pseudo-register)))
    (scfg-append!
     (rtl:make-assignment-internal pseudo
				   (rtl:make-object->address expression))
     (receiver pseudo))))

(define (define-expression-method name method)
  (hash-table/put! expression-methods name method)
  name)

(define expression-methods
  (make-strong-eq-hash-table))

(define-expression-method 'FETCH
  (lambda (receiver scfg-append! locative)
    (locative-dereference locative scfg-append!
      receiver
      (lambda (register offset granularity)
	(receiver (make-offset register offset granularity))))))

(define (address-method generator)
  (lambda (receiver scfg-append! locative)
    (locative-dereference locative scfg-append!
      (lambda (register)
	register
	(error "Can't take ADDRESS of a register" locative))
      (generator receiver scfg-append!))))

(define-expression-method 'ADDRESS
  (address-method
   (lambda (receiver scfg-append!)
     scfg-append!			;ignore
     (lambda (address offset granularity)
       (receiver
	(case granularity
	  ((OBJECT)
	   (if (zero? offset)
	       address
	       (rtl:make-offset-address address
					(rtl:make-machine-constant offset))))
	  ((BYTE)
	   (rtl:make-byte-offset-address address
					 (rtl:make-machine-constant offset)))
	  ((FLOAT)
	   (rtl:make-float-offset-address address
					  (rtl:make-machine-constant offset)))
	  (else
	   (error "ADDRESS: Unknown granularity" granularity))))))))

(define-expression-method 'ENVIRONMENT
  (address-method
   (lambda (receiver scfg-append!)
     (lambda (address offset granularity)
       (if (not (eq? granularity 'OBJECT))
	   (error "can't take address of non-object offset" granularity))
       (let ((receiver
	      (lambda (address)
		(expression-simplify
		 (rtl:make-cons-pointer
		  (rtl:make-machine-constant (ucode-type stack-environment))
		  address)
		 scfg-append!
		 receiver))))
	 (if (zero? offset)
	     (receiver address)
	     (assign-to-temporary
	      (rtl:make-offset-address address
				       (rtl:make-machine-constant offset))
	      scfg-append!
	      receiver)))))))

(define-expression-method 'CONS-POINTER
  (lambda (receiver scfg-append! type datum)
    (expression-simplify type scfg-append!
      (lambda (type)
	(expression-simplify datum scfg-append!
	  (lambda (datum)
	    (receiver (rtl:make-cons-pointer type datum))))))))

(define-expression-method 'CONS-NON-POINTER
  (lambda (receiver scfg-append! type datum)
    (expression-simplify type scfg-append!
      (lambda (type)
	(expression-simplify datum scfg-append!
	  (lambda (datum)
	    (receiver (rtl:make-cons-non-pointer type datum))))))))

(define-expression-method 'CELL-CONS
  (lambda (receiver scfg-append! expression)
    (typed-cons receiver scfg-append!
		(rtl:make-machine-constant type-code:cell)
		(list expression))))

(define-expression-method 'TYPED-CONS:PAIR
  (lambda (receiver scfg-append! type car cdr)
    (typed-cons receiver scfg-append! type (list car cdr))))

(define-expression-method 'TYPED-CONS:VECTOR
  (lambda (receiver scfg-append! type . elements)
    (let ((header
	   (rtl:make-cons-non-pointer
	    (rtl:make-machine-constant
	     (ucode-type manifest-vector))
	    (rtl:make-machine-constant (length elements)))))
      (typed-cons receiver scfg-append!
		  type
		  (cons header elements)))))


;; TYPED-CONS stores ELEMENTS in sequence into newly allocated memory
;; and `returns' a tagged pointer to the beginning of the allocated
;; memory.
;;
;; The old code used to make the tagged pointer and then do the
;; storing, which left the tagged pointer live throughout the
;; allocation.  This version does the storing first.  If the
;; allocation is done by `pushing', the tagging operation now includes
;; a negative offset.  All the machines that I can think of can fold
;; the offset and tag (e.g. MC68000, i386), either together, or into
;; other operations (e.g. HPPA must copy the free pointer, so the
;; offset be put in a move (i.e. ldo) instead.)


(define (typed-cons receiver scfg-append! type elements)

  (let ((nelements (length elements)))
    (let ((chunk-size (max 1 (-1+ (number-of-available-word-registers))))
	  (free (interpreter-free-pointer)))
      (let ((nchunks (quotient (+ nelements (-1+ chunk-size)) chunk-size))
	    (store-element!
	     (if use-pre/post-increment?
		 (lambda (element offset)
		   offset		; ignored
		   (rtl:make-assignment-internal
		    (rtl:make-post-increment free 1)
		    element))
		 (lambda (element offset)
		   (rtl:make-assignment-internal
		    (rtl:make-offset free (rtl:make-machine-constant offset))
		    element)))))

	(define (finish)
	  (expression-simplify type scfg-append!
	    (lambda (type)
	      (if use-pre/post-increment?
		  (assign-to-temporary
		   (rtl:make-offset-address free
					    (rtl:make-machine-constant (- nelements)))
		   scfg-append!
		   (lambda (temporary)
		     (receiver (rtl:make-cons-pointer type temporary))))
		  (assign-to-temporary
		   (rtl:make-cons-pointer type free)
		   scfg-append!
		   (lambda (temporary)
		     (scfg-append!
		      (rtl:make-assignment-internal
		       free
		       (rtl:make-offset-address
			free
			(rtl:make-machine-constant nelements)))
		      (receiver temporary))))))))

	(define (do-chunk elements offset tail)
	  (simplify-expressions elements scfg-append!
	    (lambda (elements)
	      (let loop ((elements elements) (offset offset))
		(if (null? elements)
		    tail
		    (scfg-append! (store-element! (car elements) offset)
				  (loop (cdr elements)
					(1+ offset))))))))

	(let process ((elements elements)
		      (offset 0)
		      (chunk 1))
	  (if (= chunk nchunks)
	      (do-chunk elements
			offset
			(finish))
	      (do-chunk (list-head elements chunk-size)
			offset
			(process (list-tail elements chunk-size)
				 (+ offset chunk-size)
				 (1+ chunk)))))))))

;; This re-caches and re-computes if we change the number of registers

(define number-of-available-word-registers
  (let ((reg-list false)
	(value false))
  (lambda ()
    (if (and value
	     (eq? reg-list available-machine-registers))
	value
	(begin
	  (set! reg-list available-machine-registers)
	  (set! value
		(length (list-transform-positive reg-list
			  (lambda (reg)
			    (value-class/ancestor-or-self?
			     (machine-register-value-class reg)
			     value-class=word)))))
	  value)))))

(define-expression-method 'TYPED-CONS:PROCEDURE
  (lambda (receiver scfg-append! entry)
    (expression-simplify
     entry scfg-append!
     (lambda (entry)
       (receiver (rtl:make-cons-pointer
		  (rtl:make-machine-constant type-code:compiled-entry)
		  entry))))))

(define-expression-method 'BYTE-OFFSET-ADDRESS
  (lambda (receiver scfg-append! base offset)
    (expression-simplify
     base scfg-append!
     (lambda (base)
       (expression-simplify
	offset scfg-append!
	(lambda (offset)
	  (receiver (rtl:make-byte-offset-address base offset))))))))

(define-expression-method 'FLOAT-OFFSET-ADDRESS
  (lambda (receiver scfg-append! base offset)
    (expression-simplify
     base scfg-append!
     (lambda (base)
       (expression-simplify
	offset scfg-append!
	(lambda (offset)
	  (receiver (rtl:make-float-offset-address base offset))))))))

;; NOPs for simplification

(define-expression-method 'ENTRY:CONTINUATION
  (lambda (receiver scfg-append! label)
    scfg-append!			; unused
    (receiver (rtl:make-entry:continuation label))))

(define-expression-method 'ENTRY:PROCEDURE
  (lambda (receiver scfg-append! label)
    scfg-append!			; unused
    (receiver (rtl:make-entry:procedure label))))

(define-expression-method 'CONS-CLOSURE
  (lambda (receiver scfg-append! entry min max size)
    scfg-append!			; unused
    (receiver (rtl:make-cons-closure entry min max size))))

(define-expression-method 'CONS-MULTICLOSURE
  (lambda (receiver scfg-append! nentries size entries)
    scfg-append!			; unused
    (receiver (rtl:make-cons-multiclosure nentries size entries))))

(define (object-selector make-object-selector)
  (lambda (receiver scfg-append! expression)
    (expression-simplify expression scfg-append!
      (lambda (expression)
	(receiver (make-object-selector expression))))))

(define-expression-method 'OBJECT->TYPE
  (object-selector rtl:make-object->type))

(define-expression-method 'CHAR->ASCII
  (object-selector rtl:make-char->ascii))

(define-expression-method 'OBJECT->DATUM
  (object-selector rtl:make-object->datum))

(define-expression-method 'OBJECT->ADDRESS
  (object-selector rtl:make-object->address))

(define-expression-method 'FIXNUM->OBJECT
  (object-selector rtl:make-fixnum->object))

(define-expression-method 'FIXNUM->ADDRESS
  (object-selector rtl:make-fixnum->address))

(define-expression-method 'ADDRESS->FIXNUM
  (object-selector rtl:make-address->fixnum))

(define-expression-method 'OBJECT->FIXNUM
  (object-selector rtl:make-object->fixnum))

(define-expression-method 'FLOAT->OBJECT
  (object-selector rtl:make-float->object))

(define-expression-method 'OBJECT->FLOAT
  (object-selector rtl:make-object->float))

(define-expression-method 'FIXNUM-2-ARGS
  (lambda (receiver scfg-append! operator operand1 operand2 overflow?)
    (expression-simplify operand1 scfg-append!
      (lambda (operand1)
	(expression-simplify operand2 scfg-append!
	  (lambda (operand2)
	    (receiver
	     (rtl:make-fixnum-2-args operator
				     operand1
				     operand2
				     overflow?))))))))

(define-expression-method 'FIXNUM-1-ARG
  (lambda (receiver scfg-append! operator operand overflow?)
    (expression-simplify operand scfg-append!
      (lambda (operand)
	(receiver (rtl:make-fixnum-1-arg operator operand overflow?))))))

(define-expression-method 'FLONUM-1-ARG
  (lambda (receiver scfg-append! operator operand overflow?)
    (expression-simplify operand scfg-append!
      (lambda (s-operand)
	(receiver (rtl:make-flonum-1-arg
		   operator
		   s-operand
		   overflow?))))))

(define-expression-method 'FLONUM-2-ARGS
  (lambda (receiver scfg-append! operator operand1 operand2 overflow?)
    (expression-simplify operand1 scfg-append!
      (lambda (s-operand1)
	(expression-simplify operand2 scfg-append!
	  (lambda (s-operand2)
	    (receiver (rtl:make-flonum-2-args
		       operator
		       s-operand1
		       s-operand2
		       overflow?))))))))

;;; end EXPRESSION-SIMPLIFY package
)