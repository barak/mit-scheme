;;; -*-Scheme-*-
;;;
;;; $Id: printer.scm,v 1.1 1997/06/04 06:09:14 cph Exp $
;;;
;;; Copyright (c) 1996 Massachusetts Institute of Technology
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

;;;; Printer Support

(declare (usual-integrations))

(define write-instance
  (make-generic-procedure 2 'WRITE-INSTANCE))

(add-method write-instance
  (make-method (list <instance>)
    (lambda (instance port)
      (write-instance-helper 'INSTANCE instance port
	(lambda ()
	  (let ((name (class-name (instance-class instance))))
	    (if name
		(begin
		  (write-string " of " port)
		  (write name port)))))))))
#|
(add-method write-instance
  (make-method (list <class>)
    (lambda (class port)
      (write-instance-helper 'CLASS class port
	(lambda ()
	  (let ((name (class-name class)))
	    (if name
		(begin
		  (write-char #\space port)
		  (write name port)))))))))
|#
(add-method write-instance
  (make-method (list <generic-procedure>)
    (lambda (procedure port)
      (write-instance-helper 'GENERIC-PROCEDURE procedure port
	(lambda ()
	  (let ((name (generic-procedure-name procedure)))
	    (if name
		(begin
		  (write-char #\space port)
		  (write name port)))))))))

(let ((install
       (lambda (class name)
	 (add-method write-instance
	   (make-method (list class)
	     (lambda (object port)
	       (write-instance-helper name object port #f)))))))
  (install <method> 'METHOD)
  (install <chained-method> 'CHAINED-METHOD)
  (install <computed-method> 'COMPUTED-METHOD)
  (install <computed-emp> 'COMPUTED-EMP)
  (install <%record> '%RECORD))

(add-method write-instance
  (make-method (list <record>)
    (lambda (record port)
      (write-instance-helper (record-type-name (record-type-descriptor record))
			     record port #f))))

(add-method write-instance
  (make-method (list <dispatch-tag>)
    (lambda (tag port)
      (write-instance-helper 'DISPATCH-TAG tag port
	(lambda ()
	  (write-char #\space port)
	  (write (dispatch-tag-contents tag) port))))))

(define (write-instance-helper name object port thunk)
  (write-string "#[" port)
  (display name port)
  (if object
      (begin
	(write-char #\space port)
	(write (hash object) port)))
  (if thunk
      (thunk))
  (write-char #\] port))

(add-generic-procedure-generator unparse-record
  (lambda (generic tags)
    generic
    (and (let ((class (dispatch-tag-contents (cadr tags))))
	   (and (class? class)
		(subclass? class <instance>)))
	 (lambda (state instance)
	   (with-current-unparser-state state
	     (lambda (port)
	       (write-instance instance port)))))))

(add-generic-procedure-generator pp-description
  (lambda (generic tags)
    generic
    (and (let ((class (dispatch-tag-contents (car tags))))
	   (and (class? class)
		(subclass? class <instance>)))
	 instance-description)))

(define (instance-description instance)
  (map (lambda (slot)
	 (let ((name (slot-name slot)))
	   (cons name
		 (if (slot-initialized? instance name)
		     (list (slot-value instance name))
		     '()))))
       (class-slots (instance-class instance))))