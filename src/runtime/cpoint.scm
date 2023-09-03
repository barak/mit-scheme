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

;;;; Control Points
;;; package: (runtime control-point)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables))

(define-integrable (control-point? object)
  (object-type? (ucode-type control-point) object))

(define-integrable (control-point/interrupt-mask control-point)
  (control-point-ref control-point 1))

(define-integrable (control-point/history control-point)
  (control-point-ref control-point 3))

(define-integrable (control-point/previous-history-offset control-point)
  (control-point-ref control-point 4))

(define-integrable (control-point/previous-history-control-point control-point)
  (control-point-ref control-point 5))

(define-integrable (control-point-ref control-point index)
  (system-vector-ref control-point (control-point-index index)))

(define-integrable (control-point-index index)
  (fix:+ 2 index))

(define-integrable first-element-index
  (control-point-index 6))

(define-integrable control-point-next-frame
  (ucode-primitive control-point-next-frame 2))

(define-integrable return-frame-type
  (ucode-primitive return-frame-type 2))

(define-integrable primitive-datum-ref
  (ucode-primitive primitive-datum-ref 2))

(define-deferred return-frame-types
  (microcode-return-frame-types))

(define (control-point->frame-generator control-point)
  (gmap decode-raw-control-point-frame
	(control-point->raw-frame-generator control-point)))

(define (control-point->raw-frame-generator control-point)
  (let ((index)
	(end))

    (define (new-cp! cp)
      (set! control-point cp)
      (set! index 2)
      (set! end (system-vector-length cp)))

    (define (generator)
      (if (fix:< index end)
	  (let ((index* (control-point-next-frame control-point index)))
	    (let ((frame (make-vector (fix:- index* index))))
	      (do ((i index (fix:+ i 1))
		   (j 0 (fix:+ j 1)))
		  ((not (fix:< i index*))
		   (set! index index*))
		(vector-set! frame j (system-vector-ref control-point i)))
	      (if (eq? (ucode-return-address join-stacklets)
		       (vector-ref frame 0))
		  (new-cp! (vector-ref frame 1)))
	      frame))
	  (eof-object)))

    (new-cp! control-point)
    generator))

(define-record-type <cpoint-frame>
    make-cpoint-frame
    cpoint-frame?
  (type cpoint-frame-type)
  (raw cpoint-frame-raw)
  (fields cpoint-frame-fields))

(define (cpoint-frame-ref frame keyword)
  (let ((p (assq keyword (cpoint-frame-fields frame))))
    (if (not p)
	(error "Unknown frame keyword:" keyword))
    (cdr p)))

(define (cpoint-frame-keywords frame)
  (map car (cpoint-frame-fields frame)))

(define (cpoint-frame->alist frame)
  (cons* (list 'frame-type (cpoint-frame-type frame))
	 (list 'raw-frame (cpoint-frame-raw frame))
	 (map (lambda (p) (list (car p) (cdr p)))
	      (cpoint-frame-fields frame))))

(define (decode-raw-control-point-frame frame)
  (let ((info (vector-ref return-frame-types (return-frame-type frame 0)))
	(return-code-name
	 (let ((address (vector-ref frame 0)))
	   (and (interpreter-return-address? address)
		(return-address/name address)))))

    (define (make ftype . alist)
      (make-cpoint-frame ftype frame alist))

    (define-integrable (name index)
      (vector-ref info (fix:+ 1 (fix:* 2 index))))

    (define-integrable (val-loc index)
      (vector-ref info (fix:+ 2 (fix:* 2 index))))

    (define-integrable (elt index)
      (cons (name index) (vector-ref frame (val-loc index))))

    (define-integrable (rest-elts index)
      (cons (name index) (vector-copy frame (val-loc index))))

    (let (
	  (frame-type-name (vector-ref info 0)))
      (case frame-type-name
	((with-arg)
	 (let ((name
		(case return-code-name
		  ((join-stacklets) 'control-point)
		  ((access-continue) 'expression)
		  ((force-snap-thunk) 'delayed)
		  ((normal-garbage-collect-done) 'gc-result)
		  ((restore-value pop-return-error) 'value)
		  ((restore-interrupt-mask) 'interrupt-mask)
		  ((halt) 'termination-code)
		  (else #f))))
	   (if name
	       (make-cpoint-frame return-code-name frame
				  (list (cons name
					      (vector-ref frame (val-loc 0)))))
	       (make return-code-name))))
	((exp+env history stack-marker)
	 (make return-code-name (elt 0) (elt 1)))
	((apply)
	 (make return-code-name (elt 0) (rest-elts 1)))
	((return-to-compiled-code)
	 (make return-code-name (elt 0)))
	((compiled-address)
	 (make frame-type-name))
	((combination-save)
	 ;; The index to primitive-datum-ref is relative to the address of
	 ;; the object.  For a vector, that's one greater than the vector
	 ;; index.
	 (let ((n-blanks
		(primitive-datum-ref frame (fix:+ 1 (vector-ref info 6)))))
	   (make return-code-name
		 (elt 0)
		 (elt 1)
		 n-blanks
		 (rest-elts (fix:+ (vector-ref info 8) n-blanks)))))
	((hardware-trap)
	 (make return-code-name (elt 0) (elt 1) (elt 2) (elt 3)
	       (elt 4) (elt 5) (elt 6) (elt 7)))
	((return-to-interpreter)
	 (make frame-type-name))
	((cc-internal-apply cc-bkpt cc-invocation)
	 (make frame-type-name (elt 0) (rest-elts 1)))
	((cc-restore-interrupt-mask)
	 (make frame-type-name (elt 0)))
	((cc-stack-marker)
	 (make frame-type-name (elt 0) (elt 1)))
	(else
	 (error "Unknown return-frame-type code:" frame-type-name))))))


(define (cpoint-frame-subproblem? frame)
  (let ((p (assq (cpoint-frame-type frame) subproblem-frame-type-map)))
    (if (not p)
	(error "Unknown frame type:" frame))
    (cadr p)))

(define (cpoint-frame-history-subproblem? frame)
  (let ((p (assq (cpoint-frame-type frame) subproblem-frame-type-map)))
    (if (not p)
	(error "Unknown frame type:" frame))
    (caddr p)))

(define subproblem-frame-type-map
  '((access-continue #t #t)
    (assignment-continue #t #t)
    (cc-bkpt #t #f)
    (cc-internal-apply #t #f)
    (cc-invocation #t #f)
    (cc-restore-interrupt-mask #t #f)
    (cc-stack-marker #t #f)
    (combination-apply #t #t)
    (combination-save-value #t #t)
    (compiled-address #t #f)
    (compiler-assignment-trap-restart #t #t)
    (compiler-error-restart #t #t)
    (compiler-interrupt-restart #f #t)
    (compiler-link-caches-restart #f #t)
    (compiler-lookup-apply-trap-restart #t #t)
    (compiler-operator-lookup-trap-restart #t #t)
    (compiler-reference-trap-restart #t #t)
    (compiler-safe-reference-trap-restart #t #t)
    (compiler-unassigned?-trap-restart #t #t)
    (conditional-decide #t #t)
    (definition-continue #t #t)
    (disjunction-decide #t #t)
    (end-of-computation #f #f)
    (eval-error #t #t)
    (force-snap-thunk #t #t)
    (halt #f #f)
    (hardware-trap #t #f)
    (internal-apply #t #f)
    (internal-apply-val #t #f)
    (join-stacklets #f #f)
    (pop-return-error #f #f)
    (reenter-compiled-code #f #t)
    (restore-dont-copy-history #f #f)
    (restore-history #f #f)
    (restore-interrupt-mask #f #f)
    (restore-value #f #f)
    (return-to-interpreter #f #t)
    (sequence-continue #t #t)
    (stack-marker #f #f)))

(define (cpoint-frames->cpoint frames)
  (object-new-type (ucode-type control-point)
		   (vector-concatenate
		    (cons '#(#f 0)
			  (cpoint-frames-raw-prefix frames)))))

(define (cpoint-frames-raw-prefix frames)
  (let ((join
	 (find (lambda (frame)
		 (eq? 'join-stacklets (cpoint-frame-type frame)))
	       frames)))
    (if join
	(let loop ((frames frames) (raw '()))
	  (let ((raw (cons (cpoint-frame-raw (car frames)) raw)))
	    (if (eq? (car frames) join)
		(reverse raw)
		(loop (cdr frames) raw))))
	(map cpoint-frame-raw frames))))

#|

;;; Disabled because some procedures in conpar.scm and environment.scm
;;; depend on the actual length for finding compiled code variables,
;;; etc.

(define (control-point/n-elements control-point)
  (let ((real-length
	 (fix:- (system-vector-length control-point) first-element-index)))
    (if (control-point/next-control-point? control-point)
	(fix:- real-length 2)
	real-length)))
|#

(define (control-point/n-elements control-point)
  (fix:- (system-vector-length control-point) first-element-index))

(define (control-point/element-stream control-point)
  (let ((end
	 (let ((end (system-vector-length control-point)))
	   (if (control-point/next-control-point? control-point)
	       (fix:- end 2)
	       end))))
    (let loop ((index first-element-index))
      (if (fix:< index end)
	  (if ((ucode-primitive primitive-object-type? 2)
	       (ucode-type manifest-nm-vector)
	       (system-vector-ref control-point index))
	      (let ((n-skips
		     (object-datum (system-vector-ref control-point index))))
		(cons-stream
		 (make-non-pointer-object n-skips)
		 (let skip-loop ((n n-skips) (index (fix:+ index 1)))
		   (if (fix:> n 0)
		       (cons-stream #f (skip-loop (fix:- n 1) (fix:+ index 1)))
		       (loop index)))))
	      (cons-stream (map-reference-trap
			    (lambda ()
			      (system-vector-ref control-point index)))
			   (loop (fix:+ index 1))))
	  '()))))

(define (control-point/next-control-point control-point)
  (and (control-point/next-control-point? control-point)
       (system-vector-ref control-point
			  (fix:- (system-vector-length control-point) 1))))

(define (make-control-point interrupt-mask
			    history
			    previous-history-offset
			    previous-history-control-point
			    element-stream
			    next-control-point)
  (let ((result
	 (make-vector (+ first-element-index
			 (stream-length element-stream)
			 (if next-control-point 2 0))))
	(index 0))
    (let ((assign
	   (lambda (value)
	     (vector-set! result index value)
	     (set! index (fix:+ index 1))
	     unspecific)))
      ;; The first two elements are unused artifacts from the old days
      ;; when "stacklets" were used.
      (assign #f)
      (assign (make-non-pointer-object 0))
      (assign (ucode-return-address restore-interrupt-mask))
      (assign interrupt-mask)
      (assign (ucode-return-address restore-history))
      (assign history)
      (assign previous-history-offset)
      (assign previous-history-control-point)
      (stream-for-each (lambda (element)
			 (assign (unmap-reference-trap element)))
		       element-stream)
      (if next-control-point
	  (begin
	    (assign (ucode-return-address join-stacklets))
	    (assign next-control-point))))
    (object-new-type (ucode-type control-point) result)))

(define (control-point/next-control-point? control-point)
  ((ucode-primitive primitive-object-eq? 2)
   (system-vector-ref control-point
		      (fix:- (system-vector-length control-point) 2))
   (ucode-return-address join-stacklets)))