#| -*-Scheme-*-

$Id: output.scm,v 14.44 2008/07/26 07:01:34 cph Exp $

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

;;;; Output
;;; package: (runtime output-port)

(declare (usual-integrations))

;;;; Low level

(define (output-port/write-char port char)
  ((port/operation/write-char port) port char))

(define (output-port/write-string port string)
  (output-port/write-substring port string 0 (xstring-length string)))

(define (output-port/write-substring port string start end)
  ((port/operation/write-substring port) port string start end))

(define (output-port/fresh-line port)
  ((port/operation/fresh-line port) port))

(define (output-port/line-start? port)
  ((port/operation/line-start? port) port))

(define (output-port/flush-output port)
  ((port/operation/flush-output port) port))

(define (output-port/discretionary-flush port)
  ((port/operation/discretionary-flush-output port) port))

(define (output-port/write-object port object environment)
  (unparse-object/top-level object port #t environment))

(define (output-port/x-size port)
  (or (let ((operation (port/operation port 'X-SIZE)))
	(and operation
	     (operation port)))
      80))

(define (output-port/y-size port)
  (let ((operation (port/operation port 'Y-SIZE)))
    (and operation
	 (operation port))))

(define (output-port/column port)
  (let ((operation (port/operation port 'OUTPUT-COLUMN)))
    (and operation
	 (operation port))))

(define (output-port/bytes-written port)
  (let ((operation (port/operation port 'BYTES-WRITTEN)))
    (and operation
	 (operation port))))

;;;; High level

(define (write-char char #!optional port)
  (let ((port (optional-output-port port 'WRITE-CHAR)))
    (if (let ((n (output-port/write-char port char)))
	  (and n
	       (fix:> n 0)))
	(output-port/discretionary-flush port))))

(define (write-string string #!optional port)
  (let ((port (optional-output-port port 'WRITE-STRING)))
    (if (let ((n (output-port/write-string port string)))
	  (and n
	       (> n 0)))
	(output-port/discretionary-flush port))))

(define (write-substring string start end #!optional port)
  (let ((port (optional-output-port port 'WRITE-SUBSTRING)))
    (if (let ((n (output-port/write-substring port string start end)))
	  (and n
	       (> n 0)))
	(output-port/discretionary-flush port))))

(define (newline #!optional port)
  (let ((port (optional-output-port port 'NEWLINE)))
    (if (let ((n (output-port/write-char port #\newline)))
	  (and n
	       (fix:> n 0)))
	(output-port/discretionary-flush port))))

(define (fresh-line #!optional port)
  (let ((port (optional-output-port port 'FRESH-LINE)))
    (if (let ((n (output-port/fresh-line port)))
	  (and n
	       (fix:> n 0)))
	(output-port/discretionary-flush port))))

(define (display object #!optional port environment)
  (let ((port (optional-output-port port 'DISPLAY)))
    (unparse-object/top-level object port #f environment)
    (output-port/discretionary-flush port)))

(define (write object #!optional port environment)
  (let ((port (optional-output-port port 'WRITE)))
    (output-port/write-object port object environment)
    (output-port/discretionary-flush port)))

(define (write-line object #!optional port environment)
  (let ((port (optional-output-port port 'WRITE-LINE)))
    (output-port/write-object port object environment)
    (output-port/write-char port #\newline)
    (output-port/discretionary-flush port)))

(define (flush-output #!optional port)
  (output-port/flush-output (optional-output-port port 'FLUSH-OUTPUT)))

(define (wrap-custom-operation-0 operation-name)
  (lambda (#!optional port)
    (let ((port (optional-output-port port operation-name)))
      (let ((operation (port/operation port operation-name)))
	(if operation
	    (begin
	      (operation port)
	      (output-port/discretionary-flush port)))))))

(define beep (wrap-custom-operation-0 'BEEP))
(define clear (wrap-custom-operation-0 'CLEAR))

(define (optional-output-port port caller)
  (if (default-object? port)
      (current-output-port)
      (guarantee-output-port port caller)))

;;;; Tabular output

(define (write-strings-in-columns strings port row-major? min-minor
				  left-margin col-sep right-margin)
  (if (not (list-of-type? strings string?))
      (error:wrong-type-argument strings "list of strings"
				 'WRITE-STRINGS-IN-COLUMNS))
  (guarantee-output-port port 'WRITE-STRINGS-IN-COLUMNS)
  (guarantee-exact-positive-integer min-minor 'WRITE-STRINGS-IN-COLUMNS)
  (guarantee-string left-margin 'WRITE-STRINGS-IN-COLUMNS)
  (guarantee-string col-sep 'WRITE-STRINGS-IN-COLUMNS)
  (guarantee-string right-margin 'WRITE-STRINGS-IN-COLUMNS)
  (let ((n-strings (length strings))
	(max-width (output-port/x-size port))
	(lm-width (string-length left-margin))
	(cs-width (string-length col-sep))
	(rm-width (string-length right-margin)))

    (define (do-row-major)
      (do-it (let loop
		 ((strings (cdr strings))
		  (width (+ lm-width rm-width (string-length (car strings))))
		  (n-cols 1))
	       (if (pair? strings)
		   (let ((width*
			  (+ width cs-width (string-length (car strings)))))
		     (if (< width* max-width)
			 (loop (cdr strings) width* (+ n-cols 1))
			 (max n-cols min-minor)))
		   (max n-cols min-minor)))
	     (lambda (n-cols) (- n-cols 1))
	     (lambda (n-cols) (= n-cols min-minor))
	     (lambda (n-cols)
	       (let ((cols (make-list n-cols '())))
		 (do ((strings strings (cdr strings))
		      (p cols (if (pair? (cdr p)) (cdr p) cols)))
		     ((not (pair? strings)))
		   (set-car! p (cons (car strings) (car p))))
		 (do ((p cols (cdr p)))
		     ((not (pair? p)))
		   (set-car! p (reverse! (car p))))
		 cols))
	     (lambda ()
	       (write-string left-margin port)
	       (for-each (lambda (string)
			   (write-string col-sep port)
			   (write-string string port))
			 strings)
	       (write-string right-margin port)
	       (newline port))))

    (define (do-col-major)
      (do-it min-minor
	     (lambda (n-rows) (+ n-rows 1))
	     (lambda (n-rows) n-rows #f)
	     (lambda (n-rows)
	       (let loop
		   ((strings strings)
		    (n-strings (length strings))
		    (cols '()))
		 (if (> n-strings n-rows)
		     (loop (list-tail strings n-rows)
			   (- n-strings n-rows)
			   (cons (list-head strings n-rows) cols))
		     (reverse!
		      (if (> n-strings 0) (cons strings cols) cols)))))
	     (lambda ()
	       (for-each (lambda (string)
			   (write-string left-margin port)
			   (write-string string port)
			   (write-string right-margin port)
			   (newline port))
			 strings))))

    (define (do-it start-n-minor step-minor limit-n-minor? ->cols single-major)
      (let loop ((n-minor start-n-minor))
	(if (<= n-minor n-strings)
	    (let* ((cols (->cols n-minor))
		   (col-widths
		    (map (lambda (col)
			   (apply max (map string-length col)))
			 cols)))
	      (if (or (limit-n-minor? n-minor)
		      (< (apply +
				lm-width
				(* cs-width (- (length cols) 1))
				rm-width
				col-widths)
			 max-width))
		  (write-cols cols col-widths)
		  (loop (step-minor n-minor))))
	    (single-major))))

    (define (write-cols cols col-widths)
      (let per-row ()
	(if (pair? (car cols))
	    (let per-col
		((cols cols)
		 (col-widths col-widths)
		 (prefix left-margin)
		 (pending-spaces 0))
	      (if (pair? cols)
		  (let ((strings (car cols)))
		    (if (pair? strings)
			(begin
			  (write-spaces pending-spaces)
			  (write-string prefix port)
			  (write-string (car strings) port)
			  (set-car! cols (cdr strings))
			  (per-col (cdr cols)
				   (cdr col-widths)
				   col-sep
				   (- (car col-widths)
				      (string-length (car strings)))))))
		  (begin
		    (write-string right-margin port)
		    (newline port)
		    (per-row)))))))

    (define (write-spaces n)
      (if (> n 0)
	  (begin
	    (write-char #\space port)
	    (write-spaces (- n 1)))))

    (if row-major? (do-row-major) (do-col-major))))

;;;; Output truncation

(define (call-with-truncated-output-port limit port generator)
  (call-with-current-continuation
   (lambda (k)
     (let ((port (make-port truncated-output-type
			    (make-tstate port limit k 0))))
       (generator port)
       #f))))

(define-structure tstate
  (port #f read-only #t)
  (limit #f read-only #t)
  (continuation #f read-only #t)
  count)

(define (trunc-out/write-char port char)
  (let ((ts (port/state port)))
    (if (< (tstate-count ts) (tstate-limit ts))
	(begin
	  (set-tstate-count! ts (+ (tstate-count ts) 1))
	  (output-port/write-char (tstate-port ts) char))
	((tstate-continuation ts) #t))))

(define (trunc-out/flush-output port)
  (output-port/flush-output (tstate-port (port/state port))))

(define (trunc-out/discretionary-flush-output port)
  (output-port/discretionary-flush (tstate-port (port/state port))))

(define truncated-output-type)
(define (initialize-package!)
  (set! truncated-output-type
	(make-port-type `((WRITE-CHAR ,trunc-out/write-char)
			  (FLUSH-OUTPUT ,trunc-out/flush-output)
			  (DISCRETIONARY-FLUSH-OUTPUT
			   ,trunc-out/discretionary-flush-output))
			#f))
  unspecific)