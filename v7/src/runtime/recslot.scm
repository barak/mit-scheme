;;; -*-Scheme-*-
;;;
;;; $Id: recslot.scm,v 1.5 2001/12/20 20:51:16 cph Exp $
;;;
;;; Copyright (c) 1995-1999, 2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

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

(define-syntax generate-index-cases
  (lambda (index limit expand-case)
    `(CASE ,index
       ,@(let loop ((i 1))
	   (if (= i limit)
	       `((ELSE (,expand-case ,index)))
	       `(((,i) (,expand-case ,i)) ,@(loop (+ i 1))))))))

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

(define (store-value-restart location k thunk)
  (let ((location (write-to-string location)))
    (with-restart 'STORE-VALUE
	(string-append "Initialize slot " location " to a given value.")
	k
	(string->interactor (string-append "Set " location " to"))
      thunk)))

(define (use-value-restart noun-phrase k thunk)
  (with-restart 'USE-VALUE
      (string-append "Specify a " noun-phrase ".")
      k
      (string->interactor (string-capitalize noun-phrase))
    thunk))

(define ((string->interactor string))
  (values (prompt-for-evaluated-expression string)))

(define condition-type:slot-error)
(define condition-type:uninitialized-slot)
(define condition-type:no-such-slot)
(define error:uninitialized-slot)
(define error:no-such-slot)

(define (initialize-conditions!)
  (set! condition-type:slot-error
	(make-condition-type 'SLOT-ERROR condition-type:cell-error
	    '()
	  (lambda (condition port)
	    (write-string "Anonymous error for slot " port)
	    (write (access-condition condition 'LOCATION) port)
	    (write-string "." port))))
  (set! condition-type:uninitialized-slot
	(make-condition-type 'UNINITIALIZED-SLOT condition-type:slot-error
	    '(RECORD)
	  (lambda (condition port)
	    (write-string "Attempt to reference slot " port)
	    (write (access-condition condition 'LOCATION) port)
	    (write-string " in record " port)
	    (write (access-condition condition 'RECORD) port)
	    (write-string " failed because the slot is not initialized."
			  port))))
  (set! condition-type:no-such-slot
	(make-condition-type 'NO-SUCH-SLOT condition-type:slot-error
	    '(RECORD-TYPE)
	  (lambda (condition port)
	    (write-string "No slot named " port)
	    (write (access-condition condition 'LOCATION) port)
	    (write-string " in records of type " port)
	    (write (access-condition condition 'RECORD-TYPE) port)
	    (write-string "." port))))
  (set! error:uninitialized-slot
	(let ((signal
	       (condition-signaller condition-type:uninitialized-slot
				    '(RECORD LOCATION)
				    standard-error-handler)))
	  (lambda (record index)
	    (let* ((location (or (%record-slot-name record index) index))
		   (ls (write-to-string location)))
	      (call-with-current-continuation
	       (lambda (k)
		 (store-value-restart ls
				      (lambda (value)
					(%record-set! record index value)
					(k value))
		   (lambda ()
		     (use-value-restart
		      (string-append
		       "value to use instead of the contents of slot "
		       ls)
		      k
		      (lambda () (signal record location)))))))))))
  (set! error:no-such-slot
	(let ((signal
	       (condition-signaller condition-type:no-such-slot
				    '(RECORD-TYPE LOCATION)
				    standard-error-handler)))
	  (lambda (record-type name)
	    (call-with-current-continuation
	     (lambda (k)
	       (use-value-restart
		(string-append "slot name to use instead of "
			       (write-to-string name))
		k
		(lambda () (signal record-type name))))))))
  unspecific)