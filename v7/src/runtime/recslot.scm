;;; -*-Scheme-*-
;;;
;;; $Id: recslot.scm,v 1.2 1997/06/24 05:33:57 cph Exp $
;;;
;;; Copyright (c) 1995-97 Massachusetts Institute of Technology
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

;;;; Record Slot Access

(declare (usual-integrations))

(define (%record-accessor-generator name)
  (lambda (generic tags)
    generic
    (let ((index (%record-slot-index (%record (car tags)) name)))
      (and index
	   (%record-accessor index)))))

(define (%record-modifier-generator name)
  (lambda (generic tags)
    generic
    (let ((index (%record-slot-index (%record (car tags)) name)))
      (and index
	   (%record-modifier index)))))

(define (%record-initpred-generator name)
  (lambda (generic tags)
    generic
    (let ((index (%record-slot-index (%record (car tags)) name)))
      (and index
	   (%record-initpred index)))))

(define-macro (generate-index-cases index limit expand-case)
  `(CASE ,index
     ,@(let loop ((i 1))
	 (if (= i limit)
	     `((ELSE (,expand-case ,index)))
	     `(((,i) (,expand-case ,i)) ,@(loop (+ i 1)))))))

(define (%record-accessor index)
  (generate-index-cases index 16
    (lambda (index)
      (declare (integrate index)
	       (ignore-reference-traps (set record-slot-uninitialized)))
      (lambda (record)
	(if (eq? record-slot-uninitialized (%record-ref record index))
	    (error:uninitialized-slot record index)
	    (%record-ref record index))))))

(define (%record-modifier index)
  (generate-index-cases index 16
    (lambda (index)
      (declare (integrate index))
      (lambda (record value) (%record-set! record index value)))))

(define (%record-initpred index)
  (generate-index-cases index 16
    (lambda (index)
      (declare (integrate index)
	       (ignore-reference-traps (set record-slot-uninitialized)))
      (lambda (record)
	(not (eq? record-slot-uninitialized (%record-ref record index)))))))

(define (%record-slot-name record index)
  (if (not (and (exact-integer? index) (positive? index)))
      (error:wrong-type-argument index "record index" '%RECORD-SLOT-NAME))
  (let ((names
	 (call-with-current-continuation
	  (lambda (k)
	    (bind-condition-handler (list condition-type:no-applicable-methods)
		(lambda (condition) condition (k 'UNKNOWN))
	      (lambda ()
		(%record-slot-names record))))))
	(index (- index 1)))
    (and (list? names)
	 (< index (length names))
	 (list-ref names index))))

(define %record-slot-index)
(define %record-slot-names)

(define (initialize-record-slot-access!)
  (set! %record-slot-index (make-generic-procedure 2 '%RECORD-SLOT-INDEX))
  (add-generic-procedure-generator %record-slot-index
    (lambda (generic tags)
      generic
      (and (record-type? (dispatch-tag-contents (car tags)))
	   (lambda (record name)
	     (record-type-field-index (record-type-descriptor record)
				      name
				      #f)))))
  (set! %record-slot-names (make-generic-procedure 1 '%RECORD-SLOT-NAMES))
  (add-generic-procedure-generator %record-slot-names
    (lambda (generic tags)
      generic
      (and (record-type? (dispatch-tag-contents (car tags)))
	   (lambda (record)
	     (record-type-field-names (record-type-descriptor record)))))))

(define condition-type:uninitialized-slot)
(define error:uninitialized-slot)

(define (initialize-conditions!)
  (set! condition-type:uninitialized-slot
	(make-condition-type 'UNINITIALIZED-SLOT condition-type:cell-error
	    '(RECORD)
	  (lambda (condition port)
	    (write-string "Attempt to reference slot " port)
	    (write (access-condition condition 'LOCATION) port)
	    (write-string " in record " port)
	    (write (access-condition condition 'RECORD) port)
	    (write-string " failed because the slot is not initialized."
			  port))))
  (set! error:uninitialized-slot
	(let ((signal
	       (condition-signaller condition-type:uninitialized-slot
				    '(RECORD LOCATION)
				    standard-error-handler)))
	  (lambda (record index)
	    (let ((location
		   (or (%record-slot-name record index)
		       index)))
	      (call-with-current-continuation
	       (lambda (k)
		 (with-restart 'STORE-VALUE
		     (lambda (port)
		       (write-string "Initialize slot " port)
		       (write location port)
		       (write-string " to a given value." port))
		     (lambda (value)
		       (%record-set! record index value)
		       (k value))
		     (let ((prompt
			    (string-append "Set "
					   (write-to-string location)
					   " to")))
		       (lambda ()
			 (values (prompt-for-evaluated-expression prompt))))
		   (lambda ()
		     (with-restart 'USE-VALUE
			 (lambda (port)
			   (write-string
			    "Specify a value to use instead of the contents of slot "
			    port)
			   (write location port)
			   (write-string "." port))
			 k
			 (let ((prompt
				(string-append "Value to use instead of "
					       (write-to-string location))))
			   (lambda ()
			     (values
			      (prompt-for-evaluated-expression prompt))))
		       (lambda ()
			 (signal record location)))))))))))
  unspecific)