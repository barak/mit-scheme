#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/object.scm,v 1.1 1987/03/19 00:44:29 cph Exp $

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

;;;; Support for tagged objects

(declare (usual-integrations))

(define (make-vector-tag parent name)
  (let ((tag (cons '() (or parent vector-tag:object))))
    (vector-tag-put! tag ':TYPE-NAME name)
    ((access add-unparser-special-object! unparser-package)
     tag tagged-vector-unparser)
    tag))

(define *tagged-vector-unparser-show-hash*
  true)

(define (tagged-vector-unparser object)
  (unparse-with-brackets
   (lambda ()
     (write-string "LIAR ")
     (if *tagged-vector-unparser-show-hash*
	 (begin (fluid-let ((*unparser-radix* 10))
		  (write (hash object)))
		(write-string " ")))
     (fluid-let ((*unparser-radix* 16))
       ((vector-method object ':UNPARSE) object)))))

(define (vector-tag-put! tag key value)
  (let ((entry (assq key (car tag))))
    (if entry
	(set-cdr! entry value)
	(set-car! tag (cons (cons key value) (car tag))))))

(define (vector-tag-get tag key)
  (define (loop tag)
    (and (pair? tag)
	 (or (assq key (car tag))
	     (loop (cdr tag)))))
  (let ((value
	 (or (assq key (car tag))
	     (loop (cdr tag)))))
    (and value (cdr value))))

(define vector-tag:object
  (list '()))

(vector-tag-put! vector-tag:object ':TYPE-NAME 'OBJECT)

(define-integrable (vector-tag vector)
  (vector-ref vector 0))

(define (define-vector-method tag name method)
  (vector-tag-put! tag name method)
  name)

(define (vector-tag-method tag name)
  (or (vector-tag-get tag name)
      (error "Unbound method" tag name)))

(define-integrable (vector-tag-parent-method tag name)
  (vector-tag-method (cdr tag) name))

(define-integrable (vector-method vector name)
  (vector-tag-method (vector-tag vector) name))

(define (define-unparser tag unparser)
  (define-vector-method tag ':UNPARSE unparser))

(define-integrable make-tagged-vector
  vector)

(define ((tagged-vector-predicate tag) object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? tag (vector-tag object))))

(define (tagged-vector-subclass-predicate tag)
  (define (loop tag*)
    (or (eq? tag tag*)
	(and (pair? tag*)
	     (loop (cdr tag*)))))
  (lambda (object)
    (and (vector? object)
	 (not (zero? (vector-length object)))
	 (loop (vector-tag object)))))

(define tagged-vector?
  (tagged-vector-subclass-predicate vector-tag:object))

(define-unparser vector-tag:object
  (lambda (object)
    (write (vector-method object ':TYPE-NAME))))

(define (->tagged-vector object)
  (or (and (tagged-vector? object) object)
      (and (integer? object)
	   (let ((object (unhash object)))
	     (and (tagged-vector? object) object)))))