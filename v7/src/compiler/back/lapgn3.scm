#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn3.scm,v 4.2 1988/03/14 20:45:17 jinx Exp $

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

  ;; End of let-syntax
  )

;; This one is different because a different uuo-link is used for different
;; numbers of arguments.

(define (free-uuo-link-label name frame-size)
  (let ((entry (assq name *interned-uuo-links*)))
    (if entry
	 (let ((place (assv frame-size (cdr entry))))
	   (if place
	       (cdr place)
	       (let ((label (allocate-constant-label)))
		 (set-cdr! entry
			   (cons (cons frame-size label)
				 (cdr entry)))
		 label)))
	 (let ((label (allocate-constant-label)))
	   (set! *interned-uuo-links*
		 (cons (list name (cons frame-size label))
		       *interned-uuo-links*))
	   label))))

(define-integrable (set-current-branches! consequent alternative)
  (set-pblock-consequent-lap-generator! *current-bblock* consequent)
  (set-pblock-alternative-lap-generator! *current-bblock* alternative))