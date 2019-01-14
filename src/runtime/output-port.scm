#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
  ((textual-port-operation/write-char port) port char))

(define (output-port/write-string port string)
  (output-port/write-substring port string 0 (string-length string)))

(define (output-port/write-substring port string start end)
  ((textual-port-operation/write-substring port) port string start end))

(define (output-port/fresh-line port)
  ((textual-port-operation/fresh-line port) port))

(define (output-port/line-start? port)
  ((textual-port-operation/line-start? port) port))

(define (output-port/flush-output port)
  ((textual-port-operation/flush-output port) port))

(define (output-port/discretionary-flush port)
  ((textual-port-operation/discretionary-flush-output port) port))

(define (output-port/x-size port)
  (or (let ((operation (textual-port-operation port 'x-size)))
	(and operation
	     (operation port)))
      80))

(define (output-port/y-size port)
  (let ((operation (textual-port-operation port 'y-size)))
    (and operation
	 (operation port))))

(define (output-port/column port)
  (let ((operation (textual-port-operation port 'output-column)))
    (and operation
	 (operation port))))

(define (output-port/bytes-written port)
  (let ((operation (textual-port-operation port 'bytes-written)))
    (and operation
	 (operation port))))

(define (output-port/synchronize-output port)
  (let ((operation (textual-port-operation port 'synchronize-output)))
    (if operation
	(operation port))))

;;;; High level

(define (write-char char #!optional port)
  (let ((port (optional-output-port port 'write-char)))
    (if (let ((n (output-port/write-char port char)))
	  (and n
	       (fix:> n 0)))
	(output-port/discretionary-flush port))))

(define (write-string string #!optional port start end)
  (let ((port (optional-output-port port 'write-string))
	(end (fix:end-index end (string-length string) 'write-string)))
    (let ((start (fix:start-index start end 'write-string)))
      (if (let ((n (output-port/write-substring port string start end)))
	    (and n
		 (fix:> n 0)))
	  (output-port/discretionary-flush port)))))

(define (write-substring string start end #!optional port)
  (write-string string port start end))

(define (newline #!optional port)
  (write-char #\newline port))

(define (flush-output-port #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (if (binary-output-port? port)
	(flush-binary-output-port port)
	(flush-textual-output-port port))))

(define (flush-textual-output-port port)
  (output-port/flush-output (optional-output-port port 'flush-output-port)))

(define (synchronize-output-port #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (if (binary-output-port? port)
	(synchronize-binary-output-port port)
	(synchronize-textual-output-port port))))

(define (synchronize-textual-output-port port)
  (output-port/synchronize-output
   (optional-output-port port 'synchronize-output-port)))

(define (fresh-line #!optional port)
  (let ((port (optional-output-port port 'fresh-line)))
    (if (let ((n (output-port/fresh-line port)))
	  (and n
	       (fix:> n 0)))
	(output-port/discretionary-flush port))))

(define (display object #!optional port)
  (%write object port #f #f 'display))

(define (write object #!optional port)
  (%write object port #t 'circularity 'write))

(define (write-shared object #!optional port)
  (%write object port #t 'sharing 'write-shared))

(define (write-simple object #!optional port)
  (%write object port #t #f 'write-simple))

(define (%write object port slashify? label-mode caller)
  (let ((port (optional-output-port port caller)))
    (print-top-level object port slashify? label-mode)
    (output-port/discretionary-flush port)))

(define (write-line object #!optional port)
  (let ((port (optional-output-port port 'write-line)))
    (print-top-level object port #t 'circularity)
    (output-port/write-char port #\newline)
    (output-port/discretionary-flush port)))

(define (wrap-custom-operation-0 operation-name)
  (lambda (#!optional port)
    (let ((port (optional-output-port port operation-name)))
      (let ((operation (textual-port-operation port operation-name)))
	(if operation
	    (begin
	      (operation port)
	      (output-port/discretionary-flush port)))))))

(define beep (wrap-custom-operation-0 'beep))
(define clear (wrap-custom-operation-0 'clear))

(define (optional-output-port port caller)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (guarantee textual-output-port? port caller)
    (if (not (textual-output-port-open? port))
	(error:bad-range-argument port caller))
    port))

;;;; Tabular output

(define (write-strings-in-columns strings port row-major? min-minor
				  left-margin col-sep right-margin)
  (if (not (list-of-type? strings string?))
      (error:wrong-type-argument strings "list of strings"
				 'write-strings-in-columns))
  (guarantee textual-output-port? port 'write-strings-in-columns)
  (guarantee exact-positive-integer? min-minor 'write-strings-in-columns)
  (guarantee string? left-margin 'write-strings-in-columns)
  (guarantee string? col-sep 'write-strings-in-columns)
  (guarantee string? right-margin 'write-strings-in-columns)
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
			  (write-spaces pending-spaces port)
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

    (if row-major? (do-row-major) (do-col-major))))

(define (write-spaces n port)
  (if (> n 0)
      (begin
	(write-char #\space port)
	(write-spaces (- n 1) port))))

(define (write-strings-in-paragraph strings port width indent first)
  (if (and (not (list-of-type? strings string?))
	   (pair? strings))
      (error:wrong-type-argument strings "non-empty list of strings"
				 'write-strings-in-paragraph))
  (guarantee textual-output-port? port 'write-strings-in-paragraph)
  (guarantee exact-positive-integer? width 'write-strings-in-paragraph)
  (guarantee exact-nonnegative-integer? indent 'write-strings-in-paragraph)
  (guarantee exact-nonnegative-integer? first 'write-strings-in-paragraph)
  (if (< width (+ indent first (string-length (car strings))))
      (error:bad-range-argument width 'write-strings-in-paragraph))

  (fresh-line port)
  (write-spaces indent port)
  (write-spaces first port)
  (write-string (car strings) port)
  (let loop ((column (+ indent first (string-length (car strings))))
	     (strings (cdr strings)))
    (if (pair? strings)
	(let* ((string (car strings))
	       (length (string-length string))
	       (new (+ column 1 length)))
	  (if (<= new width)
	      (begin
		(write-char #\space port)
		(write-string string port)
		(loop new (cdr strings)))
	      (begin
		(newline port)
		(write-spaces indent port)
		(write-string string port)
		(loop (+ indent (string-length string)) (cdr strings)))))
	(newline port))))

;;;; Output truncation

(define (call-with-truncated-output-port limit port generator)
  (call-with-current-continuation
   (lambda (k)
     (let ((port
	    (make-textual-port truncated-output-type
			       (make-tstate port limit k 0))))
       (generator port)
       #f))))

(define-structure tstate
  (port #f read-only #t)
  (limit #f read-only #t)
  (continuation #f read-only #t)
  count)

(define (trunc-out/write-char port char)
  (let ((ts (textual-port-state port)))
    (if (< (tstate-count ts) (tstate-limit ts))
	(begin
	  (set-tstate-count! ts (+ (tstate-count ts) 1))
	  (output-port/write-char (tstate-port ts) char))
	((tstate-continuation ts) #t))))

(define (trunc-out/flush-output port)
  (output-port/flush-output (tstate-port (textual-port-state port))))

(define (trunc-out/discretionary-flush-output port)
  (output-port/discretionary-flush (tstate-port (textual-port-state port))))

(define-deferred truncated-output-type
  (make-textual-port-type `((write-char ,trunc-out/write-char)
			    (flush-output ,trunc-out/flush-output)
			    (discretionary-flush-output
			     ,trunc-out/discretionary-flush-output))
			  #f))