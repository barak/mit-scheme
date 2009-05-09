#| -*-Scheme-*-

$Id: compat.scm,v 1.14 2008/01/30 20:02:39 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

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

(define (object->string object)
  (cond ((symbol? object) (symbol->string object))
	((number? object) (number->string object))
	((string? object) (string-append "\"" object "\""))
	(else
	 (with-output-to-string
	   (lambda ()
	     (write object))))))

(define (string->object object)
  (with-input-from-string object
    read))

(define (explode object)
  (map (lambda (character)
	 (let ((string (char->string character)))
	   (or (string->number string)
	       (string->symbol string))))
       (string->list
	(object->string object))))

(define (implode list)
  (string->object
   (list->string
    (map (lambda (element)
	   (cond ((digit? element)
		  (string-ref (number->string element) 0))
		 ((singleton-symbol? element)
		  (string-ref (symbol->string element) 0))
		 (else
		  (error "Element neither digit nor singleton symbol"
			 element))))
	 list))))

(define (close-channel port)
  (cond ((input-port? port) (close-input-port port))
	((output-port? port) (close-output-port port))
	(else (error "CLOSE-CHANNEL: Wrong type argument" port))))

(define (tyi #!optional port)
  (let ((char
	 (read-char
	  (if (default-object? port)
	      (current-input-port)
	      (guarantee-input-port port 'TYI)))))
    (if (char? char)
	(char->ascii char)
	char)))

(define (tyipeek #!optional port)
  (let ((char
	 (peek-char
	  (if (default-object? port)
	      (current-input-port)
	      (guarantee-input-port port 'TYIPEEK)))))
    (if (char? char)
	(char->ascii char)
	char)))

(define (tyo ascii #!optional port)
  (write-char (ascii->char ascii)
	      (if (default-object? port)
		  (current-output-port)
		  (guarantee-output-port port 'TYO))))

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

(define (ceiling->exact number)
  (inexact->exact (ceiling number)))

(define (floor->exact number)
  (inexact->exact (floor number)))

(define (round->exact number)
  (inexact->exact (round number)))

(define (truncate->exact number)
  (inexact->exact (truncate number)))

(define (vector-cons size fill)
  (make-vector size fill))

(define (read-from-keyboard)
  (let ((input (read)))
    (if (eq? input 'abort)
	(cmdl-interrupt/abort-nearest)
	input)))

(define (student-pp object . args)
  (define (supply what old new)
    (if (eq? old 'NOT-SUPPLIED)
	new
	(error "pp: Overspecified option"
	       (list what old new))))

  (define (parse-args args port as-code?)
    (cond ((null? args)
	   (let ((port 
		  (if (eq? port 'NOT-SUPPLIED)
		      (current-output-port)
		      port)))
	     (if (eq? as-code? 'NOT-SUPPLIED)
		 (pp object port)
		 (pp object port as-code?))))
	  ((eq? (car args) 'AS-CODE)
	   (parse-args (cdr args)
		       port
		       (supply 'AS-CODE as-code? true)))
	  ((output-port? (car args))
	   (parse-args (cdr args)
		       (supply 'PORT port (car args))
		       as-code?))
	  (else
	   (error "pp: Unknown option" (car args)))))		       

  (if (null? args)
      (pp object)
      (parse-args args 'NOT-SUPPLIED 'NOT-SUPPLIED)))