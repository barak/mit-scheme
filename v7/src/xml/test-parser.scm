;;; -*-Scheme-*-
;;;
;;; $Id: test-parser.scm,v 1.5 2001/07/16 20:40:25 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
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

(define (test-parser pathname)
  (call-with-input-file pathname
    (lambda (port)
      (parse-xml-document (input-port->parser-buffer port)))))

(define (test-directory directory)
  (map (lambda (pathname)
	 (write-string ";")
	 (write-string (file-namestring pathname))
	 (write-string ":\t")
	 (let ((v (ignore-errors (lambda () (test-parser pathname)))))
	   (cond ((not v)
		  (write-string "No match."))
		 ((condition? v)
		  (write-condition-report v (current-output-port)))
		 (else
		  (write-string "Parsed: ")
		  (write v)))
	   (newline)
	   v))
       (directory-read
	(merge-pathnames "*.xml" (pathname-as-directory directory)))))

(define (run-xml-tests root)
  (let ((root
	 (merge-pathnames "xmlconf/xmltest/"
			  (pathname-as-directory root))))
    (for-each (lambda (dir)
		(newline)
		(write-string ";")
		(write-string dir)
		(newline)
		(test-directory (merge-pathnames dir root)))
	      '("valid/sa" "valid/ext-sa" "valid/not-sa"
			   "invalid"
			   "not-wf/sa" "not-wf/ext-sa" "not-wf/not-sa"))))

(define (run-output-tests root output)
  (let ((root
	 (merge-pathnames "xmlconf/xmltest/"
			  (pathname-as-directory root)))
	(output (pathname-as-directory output)))
    (for-each (lambda (pathname)
		(write-string ";")
		(write-string (file-namestring pathname))
		(write-string ":\t")
		(let ((v (ignore-errors (lambda () (test-parser pathname)))))
		  (cond ((not v)
			 (write-string "No match.")
			 (newline))
			((condition? v)
			 (write-condition-report v (current-output-port))
			 (newline))
			(else
			 (write-string "Parsed: ")
			 (write v)
			 (newline)
			 (call-with-output-file
			     (merge-pathnames (file-pathname pathname) output)
			   (lambda (port)
			     (write-xml v port)))))
		  v))
	      (directory-read (merge-pathnames "valid/sa/*.xml" root)))))