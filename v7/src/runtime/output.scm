#| -*-Scheme-*-

$Id: output.scm,v 14.31 2003/01/03 21:56:01 cph Exp $

Copyright (c) 1986,1987,1988,1989,1990 Massachusetts Institute of Technology
Copyright (c) 1991,1992,1993,1999,2001 Massachusetts Institute of Technology
Copyright (c) 2002,2003 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Output
;;; package: (runtime output-port)

(declare (usual-integrations))

;;;; Output Ports

(define (output-port/write-char port char)
  ((output-port/operation/write-char port) port char))

(define (output-port/write-string port string)
  (output-port/write-substring port string 0 (xstring-length string)))

(define (output-port/write-substring port string start end)
  ((output-port/operation/write-substring port) port string start end))

(define (output-port/write-object port object)
  (unparse-object/top-level object port #t (current-unparser-table)))

(define (output-port/fresh-line port)
  ((output-port/operation/fresh-line port) port))

(define (output-port/flush-output port)
  ((output-port/operation/flush-output port) port))

(define (output-port/discretionary-flush port)
  ((output-port/operation/discretionary-flush port) port))

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

;;;; Output Procedures

(define (newline #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'NEWLINE))))
    (output-port/write-char port #\newline)
    (output-port/discretionary-flush port)))

(define (fresh-line #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'FRESH-LINE))))
    (output-port/fresh-line port)
    (output-port/discretionary-flush port)))

(define (write-char char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'WRITE-CHAR))))
    (output-port/write-char port char)
    (output-port/discretionary-flush port)))

(define (write-string string #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'WRITE-STRING))))
    (output-port/write-string port string)
    (output-port/discretionary-flush port)))

(define (write-substring string start end #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'WRITE-SUBSTRING))))
    (output-port/write-substring port string start end)
    (output-port/discretionary-flush port)))

(define (wrap-custom-operation-0 operation-name)
  (lambda (#!optional port)
    (let ((port
	   (if (default-object? port)
	       (current-output-port)
	       (guarantee-output-port port operation-name))))
      (let ((operation (port/operation port operation-name)))
	(if operation
	    (begin
	      (operation port)
	      (output-port/discretionary-flush port)))))))

(define beep (wrap-custom-operation-0 'BEEP))
(define clear (wrap-custom-operation-0 'CLEAR))

(define (display object #!optional port unparser-table)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'DISPLAY)))
	(unparser-table
	 (if (default-object? unparser-table)
	     (current-unparser-table)
	     (guarantee-unparser-table unparser-table 'DISPLAY))))
    (if (string? object)
	(output-port/write-string port object)
	(unparse-object/top-level object port #f unparser-table))
    (output-port/discretionary-flush port)))

(define (write object #!optional port unparser-table)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'WRITE)))
	(unparser-table
	 (if (default-object? unparser-table)
	     (current-unparser-table)
	     (guarantee-unparser-table unparser-table 'WRITE))))
    (unparse-object/top-level object port #t unparser-table)
    (output-port/discretionary-flush port)))

(define (write-line object #!optional port unparser-table)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port 'WRITE-LINE)))
	(unparser-table
	 (if (default-object? unparser-table)
	     (current-unparser-table)
	     (guarantee-unparser-table unparser-table 'WRITE-LINE))))
    (unparse-object/top-level object port #t unparser-table)
    (output-port/write-char port #\newline)
    (output-port/discretionary-flush port)))

(define (flush-output #!optional port)
  (output-port/flush-output
   (if (default-object? port)
       (current-output-port)
       (guarantee-output-port port 'FLUSH-OUTPUT))))

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