;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; String Padding Stuff

(declare (usual-integrations))

(define (add-padding-on-right string n)
  (if (zero? n)
      string
      (let ((l (string-length string)))
	(let ((result (make-string (+ l n) #\Space)))
	  (substring-move-right! string 0 l result 0)
	  result))))

(define (add-padding-on-left string n)
  (if (zero? n)
      string
      (let ((l (string-length string)))
	(let ((result (make-string (+ l n) #\Space)))
	  (substring-move-right! string 0 l result n)
	  result))))

(define (pad-on-right-to string n)
  (let ((l (string-length string)))
    (if (> n l)
	(let ((result (make-string n #\Space)))
	  (substring-move-right! string 0 l result 0)
	  result)
	string)))

(define (pad-on-left-to string n)
  (let ((l (string-length string)))
    (let ((delta (- n l)))
      (if (positive? delta)
	  (let ((result (make-string n #\Space)))
	    (substring-move-right! string 0 l result delta)
	    result)
	  string))))

(define (write-strings-densely strings)
  (pad-strings-on-right strings
    (lambda (n strings)
      (let ((n-per-line (max 1 (quotient 79 (+ 2 n)))))
	(define (loop strings i)
	  (if (not (null? strings))
	      (begin (write-string "  ")
		     (write-string (car strings))
		     (if (= i n-per-line)
			 (begin (newline)
				(loop (cdr strings) 1))
			 (loop (cdr strings) (1+ i))))))
	(loop strings 1)))))

(define ((pad-strings-to-max-column pad) strings receiver)
  (define (max-loop strings n acc)
    (if (null? strings)
	(adjust-loop acc n '())
	(let ((c (string-length (car strings))))
	  (max-loop (cdr strings)
		    (if (> c n) c n)
		    (cons (cons (car strings) c) acc)))))
  (define (adjust-loop strings n acc)
    (if (null? strings)
	(receiver n acc)
	(adjust-loop (cdr strings)
		     n
		     (cons (pad (caar strings) (- n (cdar strings)))
			   acc))))
  (max-loop strings 0 '()))

(define pad-strings-on-right
  (pad-strings-to-max-column add-padding-on-right))

(define pad-strings-on-left
  (pad-strings-to-max-column add-padding-on-left))