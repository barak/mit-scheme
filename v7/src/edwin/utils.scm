;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; Editor Utilities

(declare (usual-integrations))

(define (string-append-char string char)
  (let ((size (string-length string)))
    (let ((result (string-allocate (1+ size))))
      (substring-move-right! string 0 size result 0)
      (string-set! result size char)
      result)))

(define (string-append-substring string1 string2 start2 end2)
  (let ((length1 (string-length string1)))
    (let ((result (string-allocate (+ length1 (- end2 start2)))))
      (substring-move-right! string1 0 length1 result 0)
      (substring-move-right! string2 start2 end2 result length1)
      result)))

(define (dotimes n procedure)
  (define (loop i)
    (if (< i n)
	(begin (procedure i)
	       (loop (1+ i)))))
  (loop 0))

(define char-set:null
  (char-set))

(define char-set:return
  (char-set #\Return))

(define char-set:not-space
  (char-set-invert (char-set #\Space)))

(define char-set:not-graphic
  (char-set-invert char-set:graphic))

(define (read-line)
  (let ((port (current-input-port)))
    (let ((string ((access :read-string port) char-set:return)))
      ((access :discard-char port))
      string)))

(define (y-or-n? . strings)
  (define (loop)
    (let ((char (char-upcase (read-char))))
      (cond ((or (char=? char #\Y)
		 (char=? char #\Space))
	     (write-string "Yes")
	     #!TRUE)
	    ((or (char=? char #\N)
		 (char=? char #\Rubout))
	     (write-string "No")
	     #!FALSE)
	    (else
	     (beep)
	     (loop)))))
  (newline)
  (write-string (apply string-append strings))
  (loop))

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; End:
