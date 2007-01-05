#| -*-Scheme-*-

$Id: dbgutl.scm,v 14.26 2007/01/05 15:33:09 cph Exp $

Copyright 1988,1989,1990,1991,1992,2001 Massachusetts Institute of Technology
Copyright 2002,2003,2005 Massachusetts Institute of Technology

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

;;;; Debugger Utilities
;;; package: (runtime debugger-utilities)

(declare (usual-integrations))

(define (print-user-friendly-name environment port)
  (let ((name (environment-procedure-name environment)))
    (if name
	(let ((rename (special-form-procedure-name? name)))
	  (if rename
	      (begin
		(write-string "a " port)
		(write-string (string-upcase rename) port)
		(write-string " special form" port))
	      (begin
		(write-string "the procedure: " port)
		(write-dbg-upcase-name name port))))
	(write-string "an unknown procedure" port))))

(define (show-environment-procedure environment port)
  (let ((scode-lambda (environment-lambda environment)))
    (if scode-lambda
	(debugger-presentation port
	  (lambda ()
	    (pretty-print scode-lambda port)))
	(debugger-failure port "No procedure for this environment."))))

(define (write-dbg-name name port)
  (cond ((string? name)
	 (write-string name port))
	((interned-symbol? name)
	 (write-string (symbol-name name) port))
	(else
	 (write name port))))

(define (write-dbg-upcase-name name port)
  (cond ((string? name)
	 (write-string (string-upcase name)))
	((interned-symbol? name)
	 (write-string (string-upcase (symbol-name name)) port))
	(else
	 (write name port))))

(define (debug/read-eval-print-1 environment port)
  (let ((value
	 (debug/eval (prompt-for-expression "Evaluate expression"
					    port environment)
		     environment)))
    (if (undefined-value? value)
	(debugger-message port "No value")
	(debugger-message port "Value: " value))))

(define (output-to-string length thunk)
  (let ((x (with-output-to-truncated-string length thunk)))
    (if (and (car x) (> length 4))
	(substring-move! " ..." 0 4 (cdr x) (- length 4)))
    (cdr x)))

(define (show-frames environment depth port)
  (debugger-presentation port
    (lambda ()
      (let loop ((environment environment) (depth depth))
	(write-string "----------------------------------------" port)
	(newline port)
	(show-frame environment depth true port)
	(if (environment-has-parent? environment)
	    (begin
	      (newline port)
	      (newline port)
	      (loop (environment-parent environment) (+ depth 1))))))))

(define (show-frame environment depth brief? port)
  (show-environment-name environment port)
  (if (not (negative? depth))
      (begin
	(write-string "Depth (relative to initial environment): " port)
	(write depth port)
	(newline port)))
  (if (not (and (environment->package environment) brief?))
      (show-environment-bindings environment brief? port)))

(define (show-environment-name environment port)
  (write-string "Environment " port)
  (let ((package (environment->package environment)))
    (if package
	(begin
	  (write-string "named: " port)
	  (write (package/name package) port))
	(begin
	  (write-string "created by " port)
	  (print-user-friendly-name environment port))))
  (newline port))

(define (show-environment-bindings environment brief? port)
  (let ((bindings
	 (sort (environment-bindings environment)
	   (lambda (a b) (symbol<? (car a) (car b))))))
    (let ((n-bindings (length bindings))
	  (finish
	   (lambda (bindings)
	     (newline port)
	     (for-each (lambda (binding)
			 (print-binding binding port))
		       bindings))))
      (cond ((= n-bindings 0)
	     (write-string " has no bindings" port)
	     (newline port))
	    ((and brief? (> n-bindings brief-bindings-limit))
	     (write-string " has " port)
	     (write n-bindings port)
	     (write-string " bindings (first " port)
	     (write brief-bindings-limit port)
	     (write-string " shown):" port)
	     (newline port)
	     (finish (list-head bindings brief-bindings-limit)))
	    (else
	     (write-string " has bindings:" port)
	     (newline port)
	     (finish bindings))))))

(define brief-bindings-limit
  16)

(define (print-binding binding port)
  (write-string
   (let ((x-size (- (output-port/x-size port) 1)))
     (let ((name
	    (output-to-string (quotient x-size 2)
	      (lambda ()
		(write-dbg-name (car binding) (current-output-port))))))
       (cond ((not (pair? (cdr binding)))
	      (string-append name " is unassigned"))
	     ((macro-reference-trap? (cadr binding))
	      (string-append name " is a syntactic keyword"))
	     (else
	      (let ((s (string-append name " = ")))
		(string-append
		 s
		 (output-to-string (max (- x-size (string-length s)) 0)
				   (lambda ()
				     (write (cadr binding))))))))))
   port)
  (newline port))

(define (debugger-failure port . objects)
  (port/debugger-failure port (message-arguments->string objects)))

(define (debugger-message port . objects)
  (port/debugger-message port (message-arguments->string objects)))

(define (message-arguments->string objects)
  (apply string-append
	 (map (lambda (x) (if (string? x) x (write-to-string x)))
	      objects)))

(define (debugger-presentation port thunk)
  (port/debugger-presentation port thunk))