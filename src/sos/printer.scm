;;; -*-Scheme-*-
;;;
;;; $Id: printer.scm,v 1.2 1999/01/02 06:19:10 cph Exp $
;;;
;;; Copyright (c) 1996, 1999 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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