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

;;;; Boot-time definitions
;;; package: (runtime boot-definitions)

(declare (usual-integrations))

;;;; Low-level structure primitives.

(define-primitives
  (%make-tagged-object 2)
  (%record -1)
  (%record-length 1)
  (%record-ref 2)
  (%record-set! 3)
  (%record? 1)
  (%tagged-object-datum 1)
  (%tagged-object-tag 1)
  (%tagged-object? 1)
  (%weak-cons weak-cons 2)
  (%weak-car weak-car 1)
  (%weak-set-car! weak-set-car! 2)
  (weak-cdr 1)
  (weak-pair? 1)
  (weak-pair/car? weak-car 1)
  (weak-set-cdr! 2))

(define (%make-record tag length #!optional fill)
  (let ((fill (if (default-object? fill) #f fill)))
    (let-syntax
	((expand-cases
	  (sc-macro-transformer
	   (lambda (form use-env)
	     (declare (ignore use-env))
	     (let ((limit (cadr form))	;must be a power of 2
		   (gen-accessor
		    (lambda (i)
		      `(%record tag ,@(make-list (- i 1) 'fill)))))
	       `(if (and (fix:fixnum? length)
			 (fix:> length 0)
			 (fix:<= length ,limit))
		    ,(let loop ((low 1) (high limit))
		       (if (< low high)
			   (let ((mid (quotient (- (+ high low) 1) 2)))
			     `(if (fix:<= length ,mid)
				  ,(loop low mid)
				  ,(loop (+ mid 1) high)))
			   (gen-accessor low)))
		    (let ((record
			   ((ucode-primitive %make-record 2) length fill)))
		      (%record-set! record 0 tag)
		      record)))))))
      (expand-cases 16))))

(define (weak-cons car cdr)
  (%weak-cons (%false->weak-false car) cdr))

(define (weak-car weak-pair)
  (%weak-false->false (%weak-car weak-pair)))

(define (weak-set-car! weak-pair object)
  (%weak-set-car! weak-pair (%false->weak-false object)))

(define-integrable (%false->weak-false object)
  (if object object %weak-false))

(declare (integrate-operator %weak-false->false))
(define (%weak-false->false object)
  (if (%weak-false? object) #f object))

(define-integrable (%weak-false? object)
  (eq? object %weak-false))

(define-integrable %weak-false
  (let-syntax
      ((ugh
	(sc-macro-transformer
	 (lambda (form use-env)
	   (declare (ignore form use-env))
	   (object-new-type (ucode-type constant) 10)))))
    (ugh)))

;;;; Simple weak-set implementation

;;; Does not support #f as an item of the set.

(define (%make-weak-set)
  (%weak-cons 'weak-set '()))

(define (%weak-set->list weak-set)
  (weak-list->list (weak-cdr weak-set)))

(define (%add-to-weak-set item weak-set)
  (let loop
      ((this (weak-cdr weak-set))
       (prev weak-set))
    (if (weak-pair? this)
	(let ((item* (%weak-car this))
	      (next (weak-cdr this)))
	  (cond ((not item*)
		 (weak-set-cdr! prev next)
		 (loop next prev))
		((eq? item item*)
		 #f)
		(else
		 (loop next this))))
	(begin
	  (weak-set-cdr! prev (%weak-cons item '()))
	  #t))))

(define (%remove-from-weak-set item weak-set)
  (let loop
      ((this (weak-cdr weak-set))
       (prev weak-set))
    (if (weak-pair? this)
	(let ((item* (%weak-car this))
	      (next (weak-cdr this)))
	  (cond ((not item*)
		 (weak-set-cdr! prev next)
		 (loop next prev))
		((eq? item item*)
		 (weak-set-cdr! prev next)
		 #t)
		(else
		 (loop next this))))
	#f)))

(define (%weak-set-any predicate weak-set)
  (let loop
      ((this (weak-cdr weak-set))
       (prev weak-set))
    (if (weak-pair? this)
	(let ((item (%weak-car this))
	      (next (weak-cdr this)))
	  (cond ((not item)
		 (weak-set-cdr! prev next)
		 (loop next prev))
		((predicate item)
		 #t)
		(else
		 (loop next this))))
	#f)))

(define (%weak-set-for-each procedure weak-set)
  (let loop
      ((this (weak-cdr weak-set))
       (prev weak-set))
    (if (weak-pair? this)
	(let ((item (%weak-car this))
	      (next (weak-cdr this)))
	  (if item
	      (begin
		(procedure item)
		(loop next this))
	      (begin
		(weak-set-cdr! prev next)
		(loop next prev)))))))

;;;; Interrupt control

(define interrupt-bit/stack     #x0001)
(define interrupt-bit/global-gc #x0002)
(define interrupt-bit/gc        #x0004)
(define interrupt-bit/global-1  #x0008)
(define interrupt-bit/kbd       #x0010)
(define interrupt-bit/after-gc  #x0020)
(define interrupt-bit/timer     #x0040)
(define interrupt-bit/global-3  #x0080)
(define interrupt-bit/suspend   #x0100)
;; Interrupt bits #x0200 through #x4000 inclusive are reserved
;; for the Descartes PC sampler.

;; GC & stack overflow only
(define interrupt-mask/gc-ok    #x0007)

;; GC, stack overflow, and timer only
(define interrupt-mask/timer-ok #x0047)

;; Absolutely everything off
(define interrupt-mask/none     #x0000)

;; Normal: all enabled
(define interrupt-mask/all      #xFFFF)

(define (with-absolutely-no-interrupts thunk)
  ((ucode-primitive with-interrupt-mask)
   interrupt-mask/none
   (lambda (interrupt-mask)
     interrupt-mask
     (thunk))))

(define (without-interrupts thunk)
  (with-limited-interrupts interrupt-mask/gc-ok
    (lambda (interrupt-mask)
      interrupt-mask
      (thunk))))

(define (with-limited-interrupts limit-mask procedure)
  ((ucode-primitive with-interrupts-reduced) limit-mask procedure))

;;;; Boot-time initializers

(define (init-boot-inits!)
  (set! boot-inits '())
  unspecific)

(define (add-boot-init! thunk)
  (if (and booting? boot-inits)
      (set! boot-inits (cons thunk boot-inits))
      (thunk))
  unspecific)

(define (save-boot-inits! environment)
  (if (pair? boot-inits)
      (let ((inits (reverse! boot-inits)))
	(set! boot-inits #f)
	(let ((p (assq environment saved-boot-inits)))
	  (if p
	      (set-cdr! p (append! (cdr p) inits))
	      (begin
		(set! saved-boot-inits
		      (cons (cons environment inits)
			    saved-boot-inits))
		unspecific))))))

(define (get-boot-init-runner environment)
  (let ((p (assq environment saved-boot-inits)))
    (and p
	 (let ((inits (cdr p)))
	   (set! saved-boot-inits (delq! p saved-boot-inits))
	   (lambda ()
	     (for-each (lambda (init) (init))
		       inits))))))

(define (defer-boot-action group-name thunk)
  (if booting?
      (let ((group (%get-boot-action-group group-name)))
	(set-cdr! group
		  (cons thunk
			(cdr group))))
      (thunk)))

(define (run-deferred-boot-actions group-name)
  (let ((group (%find-boot-action-group group-name)))
    (if group
	(begin
	  (set! boot-action-groups (delq! group boot-action-groups))
	  (for-each (lambda (thunk) (thunk))
		    (reverse! (cdr group)))))))

(define (%get-boot-action-group group-name)
  (or (%find-boot-action-group group-name)
      (let ((group (cons group-name '())))
	(set! boot-action-groups (cons group boot-action-groups))
	group)))

(define (%find-boot-action-group group-name)
  (let loop ((groups boot-action-groups))
    (and (pair? groups)
	 (if (eq? group-name (caar groups))
	     (car groups)
	     (loop (cdr groups))))))

(define (finished-booting!)
  (set! booting? #f)
  (if (pair? boot-inits)
      (warn "boot-inits not saved:" boot-inits))
  (if (pair? saved-boot-inits)
      (warn "saved-boot-inits not run:" saved-boot-inits))
  (if (pair? boot-action-groups)
      (warn "boot-action-groups not run:" boot-action-groups)))

(define booting? #t)
(define boot-inits #f)
(define saved-boot-inits '())
(define boot-action-groups '())

;;;; Printing

(define (define-print-method predicate print-method)
  (defer-boot-action 'print-methods
    (lambda ()
      (define-print-method predicate print-method))))

(define (standard-print-method name #!optional get-parts)
  (%record standard-print-method-tag
	   name
	   (if (and get-parts (not (default-object? get-parts)))
	       get-parts
	       (lambda (object)
		 (declare (ignore object))
		 '()))))

;;; Would have used normal records here but the record abstraction is defined
;;; after this is needed.

(define (standard-print-method? object)
  (and (%record? object)
       (fix:= 3 (%record-length object))
       (eq? standard-print-method-tag (%record-ref object 0))))

(define (standard-print-method-name spm object)
  (let ((name (%record-ref spm 1)))
    (if (procedure? name)
	(name object)
	name)))

(define (standard-print-method-parts spm object)
  ((%record-ref spm 2) object))

(define-integrable standard-print-method-tag
  '|#[standard-print-method-tag]|)

(define (bracketed-print-method name printer)
  (lambda (object port)
    (if (get-param:print-with-maximum-readability?)
	(begin
	  (write-string "#@" port)
	  (write (hash-object object) port))
	(begin
	  (write-string "#[" port)
	  (display (if (procedure? name) (name object) name) port)
	  (if (or (param:print-hash-number-in-objects?)
		  (not printer))
	      (begin
		(write-char #\space port)
		(write (hash-object object) port)))
	  (if printer (printer object port))
	  (write-char #\] port)))))

(define (define-pp-describer predicate describer)
  (defer-boot-action 'pp-describers
    (lambda ()
      (define-pp-describer predicate describer))))

(define (simple-parser-method procedure)
  (lambda (objects lose)
    (or (and (pair? (cdr objects))
	     (procedure (cddr objects)))
	(lose))))

;;;; Predicate registrations

(define predicate?)
(define register-predicate!)
(define predicate->dispatch-tag)
(define set-predicate-tag!)
(let ((predicates '())
      (associations '()))
  (set! predicate?
	(lambda (object)
	  (if (or (memq object predicates)
		  (assq object associations))
	      #t
	      #f)))
  (set! register-predicate!
	(lambda (predicate name . keylist)
	  (defer-boot-action 'predicate-registrations
	    (lambda ()
	      (apply register-predicate! predicate name keylist)))
	  (set! predicates (cons predicate predicates))
	  unspecific))
  (set! predicate->dispatch-tag
	(lambda (predicate)
	  (cdr (assq predicate associations))))
  (set! set-predicate-tag!
	(lambda (predicate tag)
	  (set! associations (cons (cons predicate tag) associations))
	  (defer-boot-action 'set-predicate-tag!
	    (lambda ()
	      (set-predicate-tag! predicate tag)))))
  unspecific)

(define (set-dispatch-tag<=! t1 t2)
  (defer-boot-action 'predicate-relations
    (lambda ()
      (set-dispatch-tag<=! t1 t2))))

(define (set-predicate<=! p1 p2)
  (defer-boot-action 'predicate-relations
    (lambda ()
      (set-predicate<=! p1 p2))))

(define (guarantee predicate object #!optional caller)
  (if (not (predicate object))
      (error:not-a predicate object caller))
  object)

(define (error:not-a predicate object #!optional caller)
  (error:wrong-type-argument object (predicate-description predicate) caller))

(define (guarantee-list-of predicate object #!optional caller)
  (if (not (list-of-type? object predicate))
      (error:not-a-list-of predicate object caller))
  object)

(define (error:not-a-list-of predicate object #!optional caller)
  (error:wrong-type-argument object
                             (string-append "list of "
                                            (predicate-description predicate))
                             caller))

(define (predicate-description predicate)
  (if (predicate? predicate)
      (predicate-name predicate)
      (call-with-output-string
	(lambda (port)
	  (write-string "object satisfying " port)
	  (write predicate port)))))

;;;; Promises

(define (promise? object)
  (and (cell? object)
       (object-type? (ucode-type delayed) (cell-contents object))))

(define (make-promise object)
  (if (promise? object)
      object
      (make-cell (system-pair-cons (ucode-type delayed) #t object))))

(define (make-unforced-promise thunk)
  ;(guarantee thunk? thunk 'make-unforced-promise)
  (make-cell (system-pair-cons (ucode-type delayed) #f thunk)))

;;; Don't use multiple-values here because this gets called before they are
;;; defined.
(define-integrable (%promise-parts promise k)
  (let ((p (cell-contents promise)))
    (k (system-pair-car p)
       (system-pair-cdr p))))

(define (promise-forced? promise)
  (guarantee promise? promise 'promise-forced?)
  (system-pair-car (cell-contents promise)))

(define (promise-value promise)
  (guarantee promise? promise 'promise-value)
  (%promise-parts promise
    (lambda (forced? value)
      (if (not forced?)
          (error "Promise not yet forced:" promise))
      value)))

(define (force promise)
  (guarantee promise? promise 'force)
  (%force promise))

(define (%force promise)
  (%promise-parts promise
    (lambda (forced? value)
      (if forced?
          value
          (let ((promise* (value)))
            (guarantee promise? promise* 'force)
            (without-interrupts
             (lambda ()
               (let ((p (cell-contents promise)))
                 (if (not (system-pair-car p))
                     (let ((p* (cell-contents promise*)))
                       (system-pair-set-car! p (system-pair-car p*))
                       (system-pair-set-cdr! p (system-pair-cdr p*))
                       (set-cell-contents! promise* p))))))
            (%force promise))))))

;;;; Miscellany

(define (object-constant? object)
  ((ucode-primitive constant?) object))

(define (object-pure? object)
  object
  #f)

(define (default-object? object)
  (eq? object #!default))

(define (default-object)
  #!default)

(define (gc-space-status)
  ((ucode-primitive gc-space-status)))

(define (bytes-per-object)
  (vector-ref (gc-space-status) 0))