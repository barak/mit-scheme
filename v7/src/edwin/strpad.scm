;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/strpad.scm,v 1.5 1991/05/17 23:42:30 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989-91 Massachusetts Institute of Technology
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

;;;; String Padding Stuff

(declare (usual-integrations))

(define (pad-on-right-to string n)
  (let ((l (string-length string)))
    (if (> n l)
	(let ((result (string-allocate n)))
	  (substring-move-right! string 0 l result 0)
	  (substring-fill! result l n #\space)
	  result)
	string)))

(define (pad-on-left-to string n)
  (let ((l (string-length string)))
    (let ((delta (- n l)))
      (if (positive? delta)
	  (let ((result (string-allocate n)))
	    (substring-fill! result 0 delta #\space)
	    (substring-move-right! string 0 l result delta)
	    result)
	  string))))

(define (write-strings-densely strings #!optional port x-size)
  (let ((port (if (default-object? port) (current-output-port) port))
	(n (reduce max 0 (map string-length strings))))
    (let ((x-size
	   (if (default-object? x-size) (output-port/x-size port) x-size)))
      (let ((n-per-line (max 1 (quotient (+ x-size 1) (+ 2 n)))))
	(do ((strings strings (cdr strings))
	     (i 1 (if (< i n-per-line) (+ i 1) (begin (newline) 1))))
	    ((null? strings) unspecific)
	  (if (> i 1) (write-string "  " port))
	  (write-string (pad-on-right-to (car strings) n) port))))))