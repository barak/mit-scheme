;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/keymap.scm,v 1.5 1989/03/14 08:01:09 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Command Summary

(declare (usual-integrations))

(define-command ("Make Command Summary")
  "Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first."
  (let ((buffer (temporary-buffer "*Summary*")))
    (with-output-to-mark (buffer-point buffer)
      (lambda ()
	(write-keymap
	 ""
	 (comtab-dispatch-alists (car (mode-comtabs fundamental-mode))))))
    (select-buffer buffer)
    (set-current-point! (buffer-start buffer))))

(define (write-keymap prefix da)
  (for-each (lambda (element)
	      (write-string prefix)
	      (write-string (pad-on-right-to (char-name (car element)) 9))
	      (write-string " ")
	      (write-string (command-name (cdr element)))
	      (newline))
	    (sort-by-char (filter-uninteresting (cdr da))))
  (for-each (lambda (element)
	      (write-keymap (string-append prefix
					   (char-name (car element))
					   " ")
			    (cdr element)))
	    (sort-by-char (car da))))

(define (uninteresting-element? element)
  (or (char-lower-case? (char-base (car element)))
      (let ((name (command-name (cdr element))))
	(or (string=? name "^R Insert Self")
	    (string=? name "^R Negative Argument")
	    (string=? name "^R Argument Digit")
	    (string=? name "^R Auto Negative Argument")
	    (string=? name "^R Autoargument Digit")
	    (string=? name "^R Autoargument")))))

(define (filter-uninteresting items)
  (list-transform-negative items uninteresting-element?))

(define (sort-by-char elements)
  (sort elements
	(lambda (a b)
	  (char<? (car a) (car b)))))