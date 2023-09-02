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
  (ucode-primitive return-frame-type 1))

(define-integrable primitive-datum-ref
  (ucode-primitive primitive-datum-ref 2))

(define-deferred return-frame-types
  (microcode-return-frame-types))

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

(define (decode-raw-control-point-frame frame)
  (let* ((return-address (vector-ref frame 0))
	 (return-code-name
	  (and (interpreter-return-address? return-address)
	       (return-address/name return-address)))
	 (frame-type (return-frame-type return-address))
	 (info (vector-ref return-frame-types frame-type)))

    (define (return-with-arg-name)
      (case return-code-name
	((join-stacklets) 'control-point)
	((access-continue) 'expression)
	((force-snap-thunk) 'delayed)
	((normal-garbage-collect-done) 'gc-result)
	((restore-value pop-return-error) 'value)
	((restore-interrupt-mask) 'interrupt-mask)
	((halt) 'termination-code)
	(else #f)))

    (case (vector-ref info 0)
      ((return-with-arg)
       (let ((name (return-with-arg-name)))
	 (if name
	     (vector return-code-name
		     name
		     (vector-ref frame (vector-ref info 2)))
	     (vector return-code-name))))
      ((return-exp-env return-history return-stack-marker)
       (vector return-code-name
	       (vector-ref info 1)
	       (vector-ref frame (vector-ref info 2))
	       (vector-ref info 3)
	       (vector-ref frame (vector-ref info 4))))
      ((return-apply)
       (vector return-code-name
	       (vector-ref info 1)
	       (vector-ref frame (vector-ref info 2))
	       (vector-ref info 3)
	       (vector-copy frame (vector-ref info 4))))
      ((return-compiled-code)
       (vector return-code-name
	       (vector-ref info 1)
	       (vector-ref frame (vector-ref info 2))))
      ((return-compiled-address)
       (vector return-address
	       (vector-ref info 1)
	       (vector-copy frame (vector-ref info 2))))
      ((return-combination-save)
       ;; The index to primitive-datum-ref is relative to the address of
       ;; the object.  For a vector, that's one greater than the vector
       ;; index.
       (let ((n-blanks
	      (primitive-datum-ref frame (fix:+ 1 (vector-ref info 6)))))
	 (vector return-code-name
		 (vector-ref info 1)
		 (vector-ref frame (vector-ref info 2))
		 (vector-ref info 3)
		 (vector-ref frame (vector-ref info 4))
		 (vector-ref info 5)
		 n-blanks
		 (vector-ref info 7)
		 (vector-copy frame (vector-ref info 8)))))
      ((return-hardware_trap)
       (vector return-code-name
	       (vector-ref info 1)
	       (vector-ref frame (vector-ref info 2))
	       (vector-ref info 3)
	       (vector-ref frame (vector-ref info 4))
	       (vector-ref info 5)
	       (vector-ref frame (vector-ref info 6))
	       (vector-ref info 7)
	       (vector-ref frame (vector-ref info 8))
	       (vector-ref info 9)
	       (vector-ref frame (vector-ref info 10))
	       (vector-ref info 11)
	       (vector-ref frame (vector-ref info 12))
	       (vector-ref info 13)
	       (vector-ref frame (vector-ref info 14))
	       (vector-ref info 15)
	       (vector-ref frame (vector-ref info 16))))
      (else
       (error "Unknown return-frame-type code:" (vector-ref info 0))))))

(define (decoded-control-point-frame->alist frame)
  (cons (cons 'return-code (vector-ref frame 0))
	(let ((n (vector-length frame)))
	  (do ((i n (fix:- i 2))
	       (alist '()
		      (cons (cons (vector-ref frame (fix:- i 2))
				  (vector-ref frame (fix:- i 1)))
                            alist)))
	      ((not (fix:> i 1)) alist)))))

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