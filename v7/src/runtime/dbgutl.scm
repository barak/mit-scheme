#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/dbgutl.scm,v 14.13 1991/07/15 23:40:42 arthur Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

(define (print-user-friendly-name environment)
  (let ((name (environment-procedure-name environment)))
    (if name
	(let ((rename (special-form-procedure-name? name)))
	  (if rename
	      (begin (write-string "a ")
		     (write-string (string-upcase rename))
		     (write-string " special form"))
	      (begin (write-string "the procedure: ")
		     (write-dbg-upcase-name name))))
	(write-string "an unknown procedure"))))

(define (show-environment-procedure environment)
  (let ((scode-lambda (environment-lambda environment)))
    (if scode-lambda
	(presentation (lambda () (pretty-print scode-lambda)))
	(debugger-failure "No procedure for this environment."))))

(define (write-dbg-name name)
  (if (string? name) (write-string name) (write name)))

(define (write-dbg-upcase-name name)
  (let ((string
	 (if (string? name)
	     name
	     (with-output-to-string (lambda () (write name))))))
    (write-string (string-upcase string))))

(define (debug/read-eval-print-1 environment)
  (let ((value
	 (debug/eval (prompt-for-expression "Evaluate expression")
		     environment)))
    (if (undefined-value? value)
	(debugger-message "No value")
	(debugger-message "Value: " value))))

(define (output-to-string length thunk)
  (let ((x (with-output-to-truncated-string length thunk)))
    (if (and (car x) (> length 4))
	(substring-move-right! " ..." 0 4 (cdr x) (- length 4)))
    (cdr x)))

(define (show-frames environment depth)
  (presentation
   (lambda ()
     (let loop ((environment environment) (depth depth))
       (write-string "----------------------------------------")
       (newline)
       (show-frame environment depth true)
       (if (eq? true (environment-has-parent? environment))
	   (begin
	     (newline)
	     (newline)
	     (loop (environment-parent environment) (1+ depth))))))))

(define (show-frame environment depth brief?)
  (show-environment-name environment)
  (if (not (negative? depth))
      (begin (newline)
	     (write-string "Depth (relative to initial environment): ")
	     (write depth)))
  (if (not (and (environment->package environment) brief?))
      (begin
	(newline)
	(show-environment-bindings environment brief?))))

(define (show-environment-name environment)
  (write-string "Environment ")
  (let ((package (environment->package environment)))
    (if package
	(begin
	  (write-string "named: ")
	  (write (package/name package)))
	(begin
	  (write-string "created by ")
	  (print-user-friendly-name environment)))))

(define (show-environment-bindings environment brief?)
  (let ((names (environment-bound-names environment)))
    (let ((n-bindings (length names))
	  (finish
	   (lambda (names)
	     (newline)
	     (for-each (lambda (name)
			 (print-binding name
					(environment-lookup environment name)))
		       names))))
      (cond ((zero? n-bindings)
	     (write-string " has no bindings"))
	    ((and brief? (> n-bindings brief-bindings-limit))
	     (write-string " has ")
	     (write n-bindings)
	     (write-string " bindings (first ")
	     (write brief-bindings-limit)
	     (write-string " shown):")
	     (finish (list-head names brief-bindings-limit)))
	    (else
	     (write-string " has bindings:")
	     (finish names))))))

(define brief-bindings-limit
  16)

(define (print-binding name value)
  (let ((x-size (output-port/x-size (current-output-port))))
    (newline)
    (write-string
     (let ((name
	    (output-to-string (quotient x-size 2)
	      (lambda ()
		(write-dbg-name name)))))
       (if (unassigned-reference-trap? value)
	   (string-append name " is unassigned")
	   (let ((s (string-append name " = ")))
	     (string-append
	      s
	      (output-to-string (max (- x-size (string-length s)) 0)
		(lambda ()
		  (write value))))))))))

(define hook/debugger-failure)
(define hook/debugger-message)
(define hook/presentation)

(define (initialize-package!)
  (set! hook/debugger-failure default/debugger-failure)
  (set! hook/debugger-message default/debugger-message)
  (set! hook/presentation default/presentation)
  unspecific)

(define (debugger-failure . objects)
  (hook/debugger-failure (message-arguments->string objects)))

(define (default/debugger-failure message)
  (beep)
  (default/debugger-message message))

(define (debugger-message . objects)
  (hook/debugger-message (message-arguments->string objects)))

(define (default/debugger-message message)
  (newline)
  (write-string message))

(define (message-arguments->string objects)
  (apply string-append
	 (map (lambda (x) (if (string? x) x (write-to-string x)))
	      objects)))

(define (presentation thunk)
  (hook/presentation thunk))

(define (default/presentation thunk)
  (newline)
  (thunk))