#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sicp/compat.scm,v 1.2 1990/11/14 14:57:44 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; 6.001 Compatibility Definitions

(declare (usual-integrations))

;;; Make rationals print as flonums to create the illusion of not having
;;; rationals at all, since the Chipmunks don't.

(in-package (->environment '(runtime number))
  (define (rat:->string q radix)
    (if (ratnum? q)
	(let ((divided (flo:/ (int:->flonum (ratnum-numerator q))
			      (int:->flonum (ratnum-denominator q)))))
	  (if (integer? divided)
	      (int:->string divided radix)
	      (flo:->string divided radix)))
	(int:->string q radix))))

(define (alphaless? symbol1 symbol2)
  (string<? (symbol->string symbol1) (symbol->string symbol2)))

(define (and* . args)
  (let and-loop ((args args))
    (or (null? args)
	(and (car args)
	     (and-loop (cdr args))))))

(define (digit? object)
  (and (exact-nonnegative-integer? object) (<= object 9)))

(define (singleton-symbol? object)
  (and (symbol? object)
       (= (string-length (symbol->string object)) 1)))

(define (ascii object)
  (cond ((singleton-symbol? object)
	 (char->ascii (char-upcase (string-ref (symbol->string object) 0))))
	((digit? object)
	 (char->ascii (string-ref (number->string object) 0)))
	(else
	 (error:illegal-datum object 'ASCII))))

(define (atom? object)
  (not (pair? object)))

(define (or* . args)
  (let or-loop ((args args))
    (and (not (null? args))
	 (or (car args)
	     (or-loop (cdr args))))))

(define char ascii->char)

(define nil false)
(define t true)

(define (nth n l)
  (list-ref l n))

(define (nthcdr n l)
  (list-tail l n))

(define (explode string)
  (map (lambda (character)
	 (let ((string (char->string character)))
	   (or (string->number string)
	       (string->symbol string))))
       (string->list string)))

(define (implode list)
  (list->string
   (map (lambda (element)
	  (cond ((digit? element)
		 (string-ref (number->string element) 0))
		((singleton-symbol? element)
		 (string-ref (symbol->string element) 0))
		(else
		 (error "Element neither digit nor singleton symbol"
			element))))
	list)))

(define (close-channel port)
  (cond ((input-port? port) (close-input-port port))
	((output-port? port) (close-output-port port))
	(else (error "CLOSE-CHANNEL: Wrong type argument" port))))

(define (tyi #!optional port)
  (let ((char
	 (read-char
	  (if (default-object? port)
	      (current-output-port)
	      (guarantee-output-port port)))))
    (if (char? char)
	(char->ascii char)
	char)))

(define (tyipeek #!optional port)
  (let ((char
	 (peek-char
	  (if (default-object? port)
	      (current-output-port)
	      (guarantee-output-port port)))))
    (if (char? char)
	(char->ascii char)
	char)))

(define (tyo ascii #!optional port)
  (write-char (ascii->char ascii)
	      (if (default-object? port)
		  (current-output-port)
		  (guarantee-output-port port))))

(define (print-depth #!optional newval)
  (let ((newval (if (default-object? newval) false newval)))
    (if (not (or (not newval) (and (exact-integer? newval) (> newval 0))))
	(error:illegal-datum newval 'PRINT-DEPTH))
    (set! *unparser-list-depth-limit* newval)
    unspecific))

(define (print-breadth #!optional newval)
  (let ((newval (if (default-object? newval) false newval)))
    (if (not (or (not newval) (and (exact-integer? newval) (> newval 0))))
	(error:illegal-datum newval 'PRINT-BREADTH))
    (set! *unparser-list-breadth-limit* newval)
    unspecific))

(define (vector-cons size fill)
  (make-vector size fill))

(define (read-from-keyboard)
  (let ((input (read)))
    (if (eq? input 'abort)
	(cmdl-interrupt/abort-nearest)
	input)))