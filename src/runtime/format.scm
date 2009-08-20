#| -*-Scheme-*-

$Id$

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

;;;; Output Formatter
;;; package: (runtime format)

(declare (usual-integrations))

;;; Please don't believe this implementation!  I don't like either the
;;; calling interface or the control string syntax, but I need the
;;; functionality pretty badly and I don't have the time to think
;;; about all of that right now -- CPH.

;;; (format port format-string argument ...)
;;;
;;; Format strings are normally interpreted literally, except that
;;; certain escape sequences allow insertion of computed values.  The
;;; following escape sequences are recognized:
;;;
;;; ~n% inserts n newlines
;;; ~n~ inserts n tildes
;;;
;;; ~<c> inserts the next argument.
;;; ~n<c> pads the argument on the left to size n.
;;; ~n@<c> pads the argument on the right to size n.
;;;
;;; where <c> may be:
;;; A meaning the argument is printed using `display'.
;;; S meaning the argument is printed using `write'.

;;;; Top Level

(define (format destination format-string . arguments)
  (if (not (string? format-string))
      (error "FORMAT: illegal format string" format-string))
  (let ((start
	 (lambda (port)
	   (format-loop port format-string arguments)
	   (output-port/discretionary-flush port))))
    (cond ((not destination)
	   (with-output-to-string (lambda () (start (current-output-port)))))
	  ((eq? destination true)
	   (start (current-output-port)))
	  ((output-port? destination)
	   (start destination))
	  (else
	   (error "FORMAT: illegal destination" destination)))))

(define (format-loop port string arguments)
  (let ((index (string-find-next-char string #\~)))
    (cond (index
	   (if (not (zero? index))
	       (output-port/write-string port (substring string 0 index)))
	   (parse-dispatch port
			   (string-tail string (1+ index))
			   arguments
			   '()
			   '()))
	  ((null? arguments)
	   (output-port/write-string port string))
	  (else
	   (error "FORMAT: Too many arguments" arguments)))))

(define (parse-dispatch port string supplied-arguments parsed-arguments
			modifiers)
  ((vector-ref format-dispatch-table (vector-8b-ref string 0))
   port
   string
   supplied-arguments
   parsed-arguments
   modifiers))

(define format-dispatch-table)

(define (parse-default port string supplied-arguments parsed-arguments
		       modifiers)
  port supplied-arguments parsed-arguments modifiers ;ignore
  (error "FORMAT: Unknown formatting character" (string-ref string 0)))

;;;; Argument Parsing

(define ((format-wrapper operator)
	 port string supplied-arguments parsed-arguments modifiers)
  ((apply operator modifiers (reverse! parsed-arguments))
   port
   (string-tail string 1)
   supplied-arguments))

(define ((parse-modifier keyword)
	 port string supplied-arguments parsed-arguments modifiers)
  (parse-dispatch port
		  (string-tail string 1)
		  supplied-arguments
		  parsed-arguments
		  (cons keyword modifiers)))

(define (parse-digit port string supplied-arguments parsed-arguments modifiers)
  (let accumulate ((acc (char->digit (string-ref string 0) 10)) (i 1))
    (if (char-numeric? (string-ref string i))
	(accumulate (+ (* acc 10) (char->digit (string-ref string i) 10))
		    (1+ i))
	(parse-dispatch port
			(string-tail string i)
			supplied-arguments
			(cons acc parsed-arguments)
			modifiers))))

(define (parse-ignore port string supplied-arguments parsed-arguments
		      modifiers)
  (parse-dispatch port (string-tail string 1) supplied-arguments
		  parsed-arguments modifiers))

(define (parse-arity port string supplied-arguments parsed-arguments modifiers)
  (parse-dispatch port
		  (string-tail string 1)
		  supplied-arguments
		  (cons (length supplied-arguments) parsed-arguments)
		  modifiers))

(define (parse-argument port string supplied-arguments parsed-arguments
			modifiers)
  (parse-dispatch port
		  (string-tail string 1)
		  (cdr supplied-arguments)
		  (cons (car supplied-arguments) parsed-arguments)
		  modifiers))

;;;; Formatters

(define (format-insert-character character)
  (lambda (modifiers #!optional n)
    modifiers
    (lambda (port string arguments)
      (if (default-object? n)
	  (output-port/write-char port character)
	  (let loop ((i 0))
	    (if (not (= i n))
		(begin (output-port/write-char port character)
		       (loop (1+ i))))))
      (format-loop port string arguments))))

(define ((format-ignore-comment modifiers) port string arguments)
  modifiers				;ignore
  (format-loop port
	       (substring string
			  (1+ (string-find-next-char string #\Newline))
			  (string-length string))
	       arguments))

(define ((format-ignore-whitespace modifiers) port string arguments)
  (format-loop port
	       (cond ((null? modifiers) (eliminate-whitespace string))
		     ((memq 'AT modifiers)
		      (string-append "\n" (eliminate-whitespace string)))
		     (else string))
	       arguments))

(define (eliminate-whitespace string)
  (let ((limit (string-length string)))
    (let loop ((n 0))
      (cond ((= n limit) "")
	    ((let ((char (string-ref string n)))
	       (and (char-whitespace? char)
		    (not (char=? char #\Newline))))
	     (loop (1+ n)))
	    (else
	     (substring string n limit))))))

(define (((format-object write) modifiers #!optional n-columns)
	 port string arguments)
  (if (null? arguments)
      (error "FORMAT: too few arguments" string))
  (if (default-object? n-columns)
      (write (car arguments) port)
      (output-port/write-string port
				((if (memq 'AT modifiers)
				     string-pad-left
				     string-pad-right)
				 (with-output-to-string
				   (lambda ()
				     (write (car arguments))))
				 n-columns)))
  (format-loop port string (cdr arguments)))

;;;; Dispatcher Setup

(define (initialize-package!)
  (set! format-dispatch-table
	(let ((table (make-vector 256 parse-default)))
	  (for-each (lambda (entry)
		      (vector-set! table
				   (char->ascii (car entry))
				   (cadr entry)))
		    (let ((format-string
			   (format-wrapper (format-object display)))
			  (format-object
			   (format-wrapper (format-object write))))
		      `((#\0 ,parse-digit)
			(#\1 ,parse-digit)
			(#\2 ,parse-digit)
			(#\3 ,parse-digit)
			(#\4 ,parse-digit)
			(#\5 ,parse-digit)
			(#\6 ,parse-digit)
			(#\7 ,parse-digit)
			(#\8 ,parse-digit)
			(#\9 ,parse-digit)
			(#\, ,parse-ignore)
			(#\# ,parse-arity)
			(#\V ,parse-argument)
			(#\v ,parse-argument)
			(#\@ ,(parse-modifier 'AT))
			(#\: ,(parse-modifier 'COLON))
			(#\%
			 ,(format-wrapper (format-insert-character #\Newline)))
			(#\~ ,(format-wrapper (format-insert-character #\~)))
			(#\; ,(format-wrapper format-ignore-comment))
			(#\Newline ,(format-wrapper format-ignore-whitespace))
			(#\A ,format-string)
			(#\a ,format-string)
			(#\S ,format-object)
			(#\s ,format-object))))
	  table)))