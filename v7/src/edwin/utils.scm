;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/utils.scm,v 1.13 1989/04/23 23:28:48 cph Exp $
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

(define (string-greatest-common-prefix strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index*
		 (string-match-forward string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (xchar->name char)
  (if (pair? char)
      (chars->name char)
      (char-name char)))

(define (chars->name chars)
  (if (null? chars)
      ""
      (string-append-separated (char-name (car chars))
			       (chars->name (cdr chars)))))

(define (string-append-separated x y)
  (cond ((string-null? x) y)
	((string-null? y) x)
	(else (string-append x " " y))))
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

(define (read-line #!optional port)
  (read-string char-set:return
	       (if (default-object? port)
		   (current-input-port)
		   (guarantee-input-port port))))

(define (read-from-string string)
  (with-input-from-string string read))
(define (y-or-n? . strings)
  (define (loop)
    (let ((char (char-upcase (read-char))))
      (cond ((or (char=? char #\Y)
		 (char=? char #\Space))
	     (write-string "Yes")
	     true)
	    ((or (char=? char #\N)
		 (char=? char #\Rubout))
	     (write-string "No")
	     false)
	    (else
	     (if (not (char=? char #\newline))
		 (beep))
	     (loop)))))
  (newline)
  (for-each write-string strings)
  (loop))

(define (char-controlify char)
  (if (ascii-controlified? char)
      char
      (make-char (char-code char)
		 (let ((bits (char-bits char)))
		   (if (odd? (quotient bits 2)) bits (+ bits 2))))))

(define (char-controlified? char)
  (or (ascii-controlified? char)
      (odd? (quotient (char-bits char) 2))))

(define (char-metafy char)
  (make-char (char-code char)
	     (let ((bits (char-bits char)))
	       (if (odd? bits) bits (1+ bits)))))

(define-integrable (char-metafied? char)
  (odd? (char-bits char)))

(define (char-control-metafy char)
  (char-controlify (char-metafy char)))

(define (char-base char)
  (make-char (char-code char) 0))

(define (catch-file-errors if-error thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler
	 (list error-type:file)
	 (lambda (condition)
	   condition
	   (continuation (if-error)))
       thunk))))