;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufout.scm,v 1.4 1991/05/15 21:19:11 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Buffer Output Ports
;;; Package: (edwin buffer-output-port)

(declare (usual-integrations))

(define (with-output-to-mark mark thunk)
  (with-output-to-port (mark->output-port mark)
    thunk))

(define (mark->output-port mark #!optional buffer)
  (output-port/copy mark-output-port-template
		    (cons (mark-left-inserting-copy mark)
			  (if (default-object? buffer)
			      false
			      buffer))))

(define (fresh-line #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (let ((operation (output-port/custom-operation port 'fresh-line)))
      (if operation
	  (operation port)
	  (output-port/write-char port #\newline))
      (output-port/flush-output port))))

(define (fresh-lines n #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (let ((operation (output-port/custom-operation port 'fresh-lines)))
      (if operation
	  (operation port n)
	  (let loop ((n n))
	    (if (positive? n)
		(begin
		  (output-port/write-char port #\newline)
		  (loop (-1+ n))))))
      (output-port/flush-output port))))

(define-integrable (output-port/mark port)
  (car (output-port/state port)))

(define-integrable (output-port/buffer port)
  (cdr (output-port/state port)))

(define (operation/flush-output port)
  (let ((mark (output-port/mark port))
	(buffer (output-port/buffer port)))
    (if buffer
	(for-each (if (mark= mark (buffer-point buffer))
		      (lambda (window)
			(set-window-point! window mark)
			(window-direct-update! window false))
		      (lambda (window)
			(window-direct-update! window false)))
		  (buffer-windows buffer)))))

(define (operation/fresh-line port)
  (guarantee-newline (output-port/mark port)))

(define (operation/fresh-lines port n)
  (guarantee-newlines n (output-port/mark port)))

(define (operation/print-self state port)
  (unparse-string state "to buffer at ")
  (unparse-object state (output-port/mark port)))

(define (operation/write-char port char)
  (region-insert-char! (output-port/mark port) char))

(define (operation/write-string port string)
  (region-insert-string! (output-port/mark port) string))

(define (operation/close port)
  (mark-temporary! (output-port/mark port)))

(define mark-output-port-template
  (make-output-port `((CLOSE ,operation/close)
		      (FLUSH-OUTPUT ,operation/flush-output)
		      (FRESH-LINE ,operation/fresh-line)
		      (FRESH-LINES ,operation/fresh-lines)
		      (PRINT-SELF ,operation/print-self)
		      (WRITE-CHAR ,operation/write-char)
		      (WRITE-STRING ,operation/write-string))
		    false))