#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn3.scm,v 1.5 1987/11/21 18:45:34 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; LAP Generator

(declare (usual-integrations))

;;;; Constants

(define *next-constant*)
(define *interned-constants*)
(define *interned-variables*)
(define *interned-assignments*)
(define *interned-uuo-links*)

(define (allocate-constant-label)
  (let ((label
	 (string->symbol
	  (string-append "CONSTANT-" (number->string *next-constant*)))))
    (set! *next-constant* (1+ *next-constant*))
    label))

(define-integrable (object->label find read write)
  (lambda (object)
    (let ((entry (find object (read))))
      (if entry
	  (cdr entry)
	  (let ((label (allocate-constant-label)))
	    (write (cons (cons object label)
			 (read)))
	    label)))))

(let-syntax ((->label
	      (macro (find var)
		`(object->label ,find
				(lambda () ,var)
				(lambda (new)
				  (declare (integrate new))
				  (set! ,var new))))))

  (define constant->label (->label assv *interned-constants*))

  (define free-reference-label (->label assq *interned-variables*))

  (define free-assignment-label (->label assq *interned-assignments*))

  (define free-uuo-link-label (->label assq *interned-uuo-links*))
  ;; End of let-syntax
  )

(define-integrable (set-current-branches! consequent alternative)
  (set-pblock-consequent-lap-generator! *current-bblock* consequent)
  (set-pblock-alternative-lap-generator! *current-bblock* alternative))

;;;; Frame Pointer

(define *frame-pointer-offset*)

(define (disable-frame-pointer-offset! instructions)
  (set! *frame-pointer-offset* false)
  instructions)

(define (enable-frame-pointer-offset! offset)
  (if (not offset) (error "Null frame-pointer offset"))
  (set! *frame-pointer-offset* offset))

(define (record-push! instructions)
  (if *frame-pointer-offset*
      (set! *frame-pointer-offset* (1+ *frame-pointer-offset*)))
  instructions)

(define (record-pop!)
  (if *frame-pointer-offset*
      (set! *frame-pointer-offset* (-1+ *frame-pointer-offset*))))

(define (decrement-frame-pointer-offset! n instructions)
  (if *frame-pointer-offset*
      (set! *frame-pointer-offset*
	    (and (<= n *frame-pointer-offset*) (- *frame-pointer-offset* n))))
  instructions)

(define (guarantee-frame-pointer-offset!)
  (if (not *frame-pointer-offset*) (error "Frame pointer not initialized")))

(define (increment-frame-pointer-offset! n instructions)
  (guarantee-frame-pointer-offset!)
  (set! *frame-pointer-offset* (+ *frame-pointer-offset* n))
  instructions)

(define (frame-pointer-offset)
  (guarantee-frame-pointer-offset!)
  *frame-pointer-offset*)

(define (record-continuation-frame-pointer-offset! label)
  (guarantee-frame-pointer-offset!)
  (let ((continuation (label->continuation label))
	(offset *frame-pointer-offset*))
    (cond ((not (continuation-frame-pointer-offset continuation))
	   (set-continuation-frame-pointer-offset! continuation offset))
	  ((not (= (continuation-frame-pointer-offset continuation) offset))
	   (error "Continuation frame-pointer offset mismatch" continuation)))
    (enqueue! *continuation-queue* continuation)))

(define (record-bblock-frame-pointer-offset! bblock offset)
  (cond ((not (bblock-frame-pointer-offset bblock))
	 (set-bblock-frame-pointer-offset! bblock offset))
	((not (and offset (= (bblock-frame-pointer-offset bblock) offset)))
	 (error "Basic block frame-pointer offset mismatch" bblock offset))))