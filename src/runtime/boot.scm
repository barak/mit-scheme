#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Printing

(define (define-print-method type print-method)
  (seq:after-printer 'add-action!
    (lambda ()
      (define-print-method type print-method))))

(define (standard-print-method name #!optional get-parts hash?)
  (%record standard-print-method-tag
	   name
	   hash?
	   (if (and get-parts (not (default-object? get-parts)))
	       get-parts
	       (lambda (object)
		 (declare (ignore object))
		 '()))))

;;; Would have used normal records here but the record abstraction is defined
;;; after this is needed.

(define standard-print-method?)
(define (initialize-package!)
  (set! standard-print-method?
	(refine-type %record?
	  (lambda (r)
	    (and (fx=? 4 (%record-length r))
		 (eq? standard-print-method-tag (%record-ref r 0))))))
  unspecific)

(define (standard-print-method-name spm object)
  (let ((name (%record-ref spm 1)))
    (if (procedure? name)
	(name object)
	name)))

(define (standard-print-method-hash? spm)
  (%record-ref spm 2))

(define (standard-print-method-parts spm object)
  ((%record-ref spm 3) object))

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

(define (define-pp-describer type describer)
  (seq:after-pretty-printer 'add-action!
    (lambda ()
      (define-pp-describer type describer))))

(define (simple-parser-method procedure)
  (lambda (objects lose)
    (or (and (pair? objects)
	     (procedure (cdr objects)))
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
	  (seq:after-predicate 'add-action!
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
	  (seq:set-predicate-tag! 'add-action!
	    (lambda ()
	      (set-predicate-tag! predicate tag)))))
  unspecific)

(define (set-dispatch-tag<=! t1 t2)
  (seq:after-predicate 'add-action!
    (lambda ()
      (set-dispatch-tag<=! t1 t2))))

(define (set-predicate<=! p1 p2)
  (seq:after-predicate 'add-action!
    (lambda ()
      (set-predicate<=! p1 p2))))

(declare (integrate-operator guarantee))
(define (guarantee predicate object #!optional caller)
  (declare (integrate-operator predicate))
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

(define (make-promise object)
  (make-cell (make-cell (%make-delayed #t object))))

(define (make-unforced-promise thunk)
  ;(guarantee thunk? thunk 'make-unforced-promise)
  (make-cell (make-cell (%make-delayed #f thunk))))

;;; Don't use multiple-values here because this gets called before they are
;;; defined.
(define-integrable (%promise-parts promise k)
  (let ((p (cell-contents (cell-contents promise))))
    (k (%delayed-forced? p)
       (%delayed-value p))))

(define (promise-forced? promise)
  (guarantee promise? promise 'promise-forced?)
  (%delayed-forced? (cell-contents (cell-contents promise))))

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
	    (if (eq? promise* promise)
		(error "Infinite recursion in promise:" promise))
	    (without-interrupts
	     (lambda ()
	       (let ((q (cell-contents promise)))
		 (if (not (%delayed-forced? (cell-contents q)))
		     (let ((q* (cell-contents promise*)))
		       ;; Reduce the chain of indirections by one link so
		       ;; that we don't accumulate space.
		       (set-cell-contents! q (cell-contents q*))
		       ;; Point promise* at the same chain of
		       ;; indirections as promise so that forcing
		       ;; promise* will yield the same result.
		       (set-cell-contents! promise* q))))))
	    (%force promise))))))

(define promise?)
(seq:after-files-loaded 'add-action!
  (lambda ()
    (set! promise?
	  (refine-type cell?
	    (lambda (cell)
	      (and (cell? (cell-contents cell))
		   (delayed? (cell-contents (cell-contents cell)))))))
    (define-print-method promise?
      (standard-print-method 'promise
	(lambda (promise)
	  (if (promise-forced? promise)
	      (list '(evaluated) (promise-value promise))
	      (list '(unevaluated))))))))

;;;; Multiple values

(define <multi-values>
  (list '<multi-values>))

(define (make-multi-values objects)
  (cons <multi-values> objects))

(define (multi-values? object)
  (and (pair? object)
       (eq? <multi-values> (car object))))

(define (multi-values-list mv)
  (cdr mv))

(seq:after-record 'add-action!
  (lambda ()
    (set! <multi-values> (make-record-type '<multi-values> '(list)))
    (set! make-multi-values (record-constructor <multi-values>))
    (set! multi-values? (record-predicate <multi-values>))
    (set! multi-values-list (record-accessor <multi-values> 'list))
    unspecific))

(define (values . objects)
  (if (and (pair? objects)
	   (null? (cdr objects)))
      (car objects)
      (make-multi-values objects)))

(define (call-with-values thunk receiver)
  (let ((v (thunk)))
    (if (multi-values? v)
	(apply receiver (multi-values-list v))
	(receiver v))))

;;;; Miscellany

(define (object-constant? object)
  ((ucode-primitive constant?) object))

(define (object-pure? object)
  (declare (ignore object))
  #f)

(define (gc-space-status)
  ((ucode-primitive gc-space-status)))

(define (bytes-per-object)
  (vector-ref (gc-space-status) 0))

;;; XXX Cross-compilation kludge: We redefine this to return the
;;; characteristic of the target system.  In the future, macro
;;; expanders should just see a binding of bytes-per-object that
;;; reflects the target system.

(define (target-bytes-per-object)
  (bytes-per-object))