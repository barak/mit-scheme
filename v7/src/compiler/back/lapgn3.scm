#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/lapgn3.scm,v 4.6 1991/10/22 09:02:43 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Constants

(define *next-constant*)
(define *interned-constants*)
(define *interned-variables*)
(define *interned-assignments*)
(define *interned-uuo-links*)
(define *interned-global-links*)
(define *interned-static-variables*)

(define (allocate-named-label prefix)
  (let ((label
	 (string->uninterned-symbol
	  (string-append prefix (number->string *next-constant*)))))
    (set! *next-constant* (1+ *next-constant*))
    label))

(define (allocate-constant-label)
  (allocate-named-label "CONSTANT-"))

(define-integrable (object->label find read write allocate-label)
  (lambda (object)
    (let ((entry (find object (read))))
      (if entry
	  (cdr entry)
	  (let ((label (allocate-label object)))
	    (write (cons (cons object label)
			 (read)))
	    label)))))

(let-syntax ((->label
	      (macro (find var #!optional suffix)
		`(object->label ,find
				(lambda () ,var)
				(lambda (new)
				  (declare (integrate new))
				  (set! ,var new))
				,(if (default-object? suffix)
				     `(lambda (object)
					object ; ignore
					(allocate-named-label "OBJECT-"))
				     `(lambda (object)
					(allocate-named-label
					 (string-append (symbol->string object)
							,suffix))))))))
(define constant->label
  (->label assoc *interned-constants*))

(define free-reference-label
  (->label assq *interned-variables* "-READ-CELL-"))

(define free-assignment-label
  (->label assq *interned-assignments* "-WRITE-CELL-"))

(define free-static-label
  (->label assq *interned-static-variables* "-HOME-"))

;; End of let-syntax
)

;; These are different because different uuo-links are used for different
;; numbers of arguments.

(define (allocate-uuo-link-label prefix name frame-size)
  (allocate-named-label
   (string-append prefix
		  (symbol->string name)
		  "-"
		  (number->string (-1+ frame-size))
		  "-ARGS-")))

(define-integrable (uuo-link-label read write! prefix)
  (lambda (name frame-size)
    (let* ((all (read))
	   (entry (assq name all)))
      (if entry
	  (let ((place (assv frame-size (cdr entry))))
	    (if place
		(cdr place)
		(let ((label (allocate-uuo-link-label prefix name frame-size)))
		  (set-cdr! entry
			    (cons (cons frame-size label)
				  (cdr entry)))
		  label)))
	  (let ((label (allocate-uuo-link-label prefix name frame-size)))
	    (write! (cons (list name (cons frame-size label))
			  all))
	    label)))))

(define free-uuo-link-label
  (uuo-link-label (lambda () *interned-uuo-links*)
		  (lambda (new)
		    (set! *interned-uuo-links* new))
		  ""))

(define global-uuo-link-label
  (uuo-link-label (lambda () *interned-global-links*)
		  (lambda (new)
		    (set! *interned-global-links* new))
		  "GLOBAL-"))

(define-integrable (set-current-branches! consequent alternative)
  (set-pblock-consequent-lap-generator! *current-bblock* consequent)
  (set-pblock-alternative-lap-generator! *current-bblock* alternative))