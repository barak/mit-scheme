;;; -*-Scheme-*-
;;;
;;; $Id: genmult.scm,v 1.1 1996/04/23 20:37:42 cph Exp $
;;;
;;; Copyright (c) 1995-96 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; Multiplexed Generic Procedures

;;; This code assumes that a non-multiplexed generic procedure
;;; generator is equivalent to the same generator stored in a
;;; multiplexer.  Multiplexers assume that each of their generators is
;;; applicable to a particular set of objects, and that the set does
;;; not intersect any of the sets handled by other generators stored
;;; in the same multiplexer.  Combining these two assumptions means
;;; that a non-multiplexed generator must follow the convention for
;;; multiplexed generators, even though there is no reason to do so in
;;; the absence of multiplexers.

;;; This convention is encouraged by hiding the low-level procedures
;;; that allow direct access to a generic procedure's generator, and
;;; forcing the programmer to go through the multiplexing interface.
;;; That way, multiplexing appears to be an integral part of the
;;; generic-procedure interface.

(declare (usual-integrations))

(define (generic-procedure-generator-list generic)
  (let ((m (generic-procedure-generator generic)))
    (if m
	(if (multiplexer? m)
	    (list-copy (multiplexer-list m))
	    (list m))
	'())))

(define (add-generic-procedure-generator generic generator)
  (let ((m (generic-procedure-generator generic)))
    (if (multiplexer? m)
	(begin
	  (purge-generic-procedure-cache generic)
	  (add-generator m generator))
	(add-generator (install-multiplexer generic) generator))))

(define (remove-generic-procedure-generator generic generator)
  (let ((m (generic-procedure-generator generic)))
    (if (multiplexer? m)
	(begin
	  (purge-generic-procedure-cache generic)
	  (set-multiplexer-list! m (delq! generator (multiplexer-list m)))
	  (maybe-deinstall-multiplexer generic))
	(if (eq? generator m)
	    (set-generic-procedure-generator! generic #f)))))

(define (remove-generic-procedure-generators generic tags)
  (for-each (lambda (generator)
	      (if (generator generic tags)
		  (remove-generic-procedure-generator generic generator)))
	    (generic-procedure-generator-list generic)))

(define (generic-procedure-default-generator generic)
  (let ((m (generic-procedure-generator generic)))
    (and (multiplexer? m)
	 (multiplexer-default m))))

(define (set-generic-procedure-default-generator! generic generator)
  (let ((m (generic-procedure-generator generic)))
    (cond ((multiplexer? m)
	   (purge-generic-procedure-cache generic)
	   (set-multiplexer-default! m generator)
	   (maybe-deinstall-multiplexer generic))
	  (generator
	   (set-multiplexer-default! (install-multiplexer generic)
				     generator)))))

(define (install-multiplexer generic)
  (let ((m (make-multiplexer)))
    (let ((g (generic-procedure-generator generic)))
      (if g
	  (add-generator m g)))
    (set-generic-procedure-generator! generic m)
    m))

(define (add-generator m generator)
  (set-multiplexer-list! m (cons generator (multiplexer-list m))))

(define (maybe-deinstall-multiplexer generic)
  (let* ((m (generic-procedure-generator generic))
	 (generators (multiplexer-list m)))
    (cond ((and (null? generators)
		(not (multiplexer-default m)))
	   (set-generic-procedure-generator! generic #f))
	  ((and (null? (cdr generators))
		(not (multiplexer-default m)))
	   (set-generic-procedure-generator! generic (car generators))))))

(define (make-multiplexer)
  (make-entity (lambda (multiplexer generic tags)
		 (multiplexer-dispatch multiplexer generic tags))
	       (make-multiplexer-record '() #f)))

(define (multiplexer? object)
  (and (entity? object)
       (multiplexer-record? (entity-extra object))))

(define (multiplexer-list multiplexer)
  (multiplexer-record/list (entity-extra multiplexer)))

(define (set-multiplexer-list! multiplexer list)
  (set-multiplexer-record/list! (entity-extra multiplexer) list))

(define (multiplexer-default multiplexer)
  (multiplexer-record/default (entity-extra multiplexer)))

(define (set-multiplexer-default! multiplexer default)
  (set-multiplexer-record/default! (entity-extra multiplexer) default))

(define-structure (multiplexer-record (conc-name multiplexer-record/))
  list
  default)

(define (multiplexer-dispatch multiplexer generic tags)
  (let loop ((generators (multiplexer-list multiplexer)))
    (if (null? generators)
	(let ((default (multiplexer-default multiplexer)))
	  (and default
	       (default generic tags)))
	(let ((procedure ((car generators) generic tags)))
	  (cond ((not procedure)
		 (loop (cdr generators)))
		((there-exists? (cdr generators)
		   (lambda (generator)
		     (generator generic tags)))
		 (lambda args
		   (error:extra-applicable-methods generic args)))
		(else procedure))))))

(define multiplexer-tag)
(define del-rassq)
(define condition-type:extra-applicable-methods)
(define error:extra-applicable-methods)

(define (initialize-multiplexer!)
  (set! multiplexer-tag (list 'GENERIC-PROCEDURE-MULTIPLEXER))
  (set! del-rassq (delete-association-procedure list-deletor eq? cdr))
  unspecific)

(define (initialize-conditions!)
  (set! condition-type:extra-applicable-methods
	(make-condition-type 'EXTRA-APPLICABLE-METHODS condition-type:error
	    '(OPERATOR OPERANDS)
	  (lambda (condition port)
	    (write-string "Too many applicable methods for " port)
	    (write (access-condition condition 'OPERATOR) port)
	    (write-string " with these arguments: " port)
	    (write (access-condition condition 'OPERANDS) port)
	    (write-string "." port))))
  (set! error:extra-applicable-methods
	(condition-signaller condition-type:extra-applicable-methods
			     '(OPERATOR OPERANDS)
			     standard-error-handler))
  unspecific)