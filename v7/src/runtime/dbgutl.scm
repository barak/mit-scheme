#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/dbgutl.scm,v 14.15 1992/02/08 15:03:05 cph Exp $

Copyright (c) 1988-92 Massachusetts Institute of Technology

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
		(write-string " special form") port)
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
  (if (string? name) (write-string name port) (write name port)))

(define (write-dbg-upcase-name name port)
  (cond ((string? name)
	 (write-string (string-upcase name)))
	((interned-symbol? name)
	 (write-string (string-upcase (symbol->string name)) port))
	(else
	 (write name port))))

(define (debug/read-eval-print-1 environment port)
  (let ((value
	 (debug/eval (prompt-for-expression "Evaluate expression" port)
		     environment)))
    (if (undefined-value? value)
	(debugger-message port "No value")
	(debugger-message port "Value: " value))))

(define (output-to-string length thunk)
  (let ((x (with-output-to-truncated-string length thunk)))
    (if (and (car x) (> length 4))
	(substring-move-right! " ..." 0 4 (cdr x) (- length 4)))
    (cdr x)))

(define (show-frames environment depth port)
  (debugger-presentation port
    (lambda ()
      (let loop ((environment environment) (depth depth))
	(write-string "----------------------------------------" port)
	(newline port)
	(show-frame environment depth true port)
	(if (eq? true (environment-has-parent? environment))
	    (begin
	      (newline port)
	      (newline port)
	      (loop (environment-parent environment) (1+ depth))))))))

(define (show-frame environment depth brief? port)
  (show-environment-name environment port)
  (if (not (negative? depth))
      (begin
	(newline port)
	(write-string "Depth (relative to initial environment): " port)
	(write depth port)))
  (if (not (and (environment->package environment) brief?))
      (begin
	(newline port)
	(show-environment-bindings environment brief? port))))

(define (show-environment-name environment port)
  (write-string "Environment " port)
  (let ((package (environment->package environment)))
    (if package
	(begin
	  (write-string "named: " port)
	  (write (package/name package) port))
	(begin
	  (write-string "created by " port)
	  (print-user-friendly-name environment port)))))

(define (show-environment-bindings environment brief? port)
  (let ((names (environment-bound-names environment)))
    (let ((n-bindings (length names))
	  (finish
	   (lambda (names)
	     (newline port)
	     (for-each (lambda (name)
			 (print-binding name
					(environment-lookup environment name)
					port))
		       names))))
      (cond ((zero? n-bindings)
	     (write-string " has no bindings" port))
	    ((and brief? (> n-bindings brief-bindings-limit))
	     (write-string " has " port)
	     (write n-bindings port)
	     (write-string " bindings (first " port)
	     (write brief-bindings-limit port)
	     (write-string " shown):" port)
	     (finish (list-head names brief-bindings-limit)))
	    (else
	     (write-string " has bindings:" port)
	     (finish names))))))

(define brief-bindings-limit
  16)

(define (print-binding name value port)
  (let ((x-size (output-port/x-size port)))
    (newline port)
    (write-string
     (let ((name
	    (output-to-string (quotient x-size 2)
	      (lambda ()
		(write-dbg-name name (current-output-port))))))
       (if (unassigned-reference-trap? value)
	   (string-append name " is unassigned")
	   (let ((s (string-append name " = ")))
	     (string-append
	      s
	      (output-to-string (max (- x-size (string-length s)) 0)
		(lambda ()
		  (write value)))))))
     port)))

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