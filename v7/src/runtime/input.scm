#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/input.scm,v 14.10 1990/11/09 08:43:53 cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Input
;;; package: (runtime input-port)

(declare (usual-integrations))

;;;; Input Ports

(define (initialize-package!)
  (set! *current-input-port* console-input-port))

(define (input-port/unparse state port)
  ((unparser/standard-method 'INPUT-PORT
			     (input-port/custom-operation port 'PRINT-SELF))
   state
   port))

(define-structure (input-port (conc-name input-port/)
			      (constructor %make-input-port)
			      (copier %input-port/copy)
			      (print-procedure input-port/unparse))
  state
  (operation/char-ready? false read-only true)
  (operation/peek-char false read-only true)
  (operation/read-char false read-only true)
  (operation/discard-char false read-only true)
  (operation/read-string false read-only true)
  (operation/discard-chars false read-only true)
  (custom-operations false read-only true)
  (operation-names false read-only true))

(define (guarantee-input-port port)
  (if (not (input-port? port)) (error "Bad input port" port))
  port)

(define (input-port/copy port state)
  (guarantee-input-port port)
  (let ((result (%input-port/copy port)))
    (set-input-port/state! result state)
    result))

(define (input-port/custom-operation port name)
  (guarantee-input-port port)
  (let ((entry (assq name (input-port/custom-operations port))))
    (and entry
	 (cdr entry))))

(define (input-port/operation port name)
  ;; Try the custom operations first since the user is less likely to
  ;; use this procedure to access the standard operations.
  (or (input-port/custom-operation port name)
      (case name
	((CHAR-READY?) (input-port/operation/char-ready? port))
	((PEEK-CHAR) (input-port/operation/peek-char port))
	((READ-CHAR) (input-port/operation/read-char port))
	((DISCARD-CHAR) (input-port/operation/discard-char port))
	((READ-STRING) (input-port/operation/read-string port))
	((DISCARD-CHARS) (input-port/operation/discard-chars port))
	(else false))))

(define (make-input-port operations state)
  (let ((operations
	 (map (lambda (entry)
		(cons (car entry) (cadr entry)))
	      operations)))
    (let ((operation
	   (lambda (name default)
	     (let ((entry (assq name operations)))
	       (if entry
		   (begin
		     (set! operations (delq! entry operations))
		     (cdr entry))
		   (or default
		       (error "MAKE-INPUT-PORT: missing operation" name)))))))
      (let ((char-ready? (operation 'CHAR-READY? false))
	    (peek-char (operation 'PEEK-CHAR false))
	    (read-char (operation 'READ-CHAR false)))
	(let ((discard-char (operation 'DISCARD-CHAR read-char))
	      (read-string
	       (operation 'READ-STRING default-operation/read-string))
	      (discard-chars
	       (operation 'DISCARD-CHARS default-operation/discard-chars)))
	  (%make-input-port state
			    char-ready?
			    peek-char
			    read-char
			    discard-char
			    read-string
			    discard-chars
			    operations
			    (append '(CHAR-READY?
				      PEEK-CHAR
				      READ-CHAR
				      DISCARD-CHAR
				      READ-STRING
				      DISCARD-CHARS)
				    (map car operations))))))))

(define (default-operation/read-string port delimiters)
  (let ((peek-char (input-port/operation/peek-char port))
	(discard-char (input-port/operation/discard-char port)))
    (let ((peek-char (let loop () (or (peek-char port) (loop)))))
      (let ((char (peek-char)))
	(if (eof-object? char)
	    char
	    (list->string
	     (let loop ((char char))
	       (if (or (eof-object? char)
		       (char-set-member? delimiters char))
		   '()
		   (begin
		     (discard-char port)
		     (cons char (loop (peek-char))))))))))))

(define (default-operation/discard-chars port delimiters)
  (let ((peek-char (input-port/operation/peek-char port))
	(discard-char (input-port/operation/discard-char port)))
    (let loop ()
      (let ((char
	     (let loop ()
	       (or (peek-char port)
		   (loop)))))
	(if (not (or (eof-object? char)
		     (char-set-member? delimiters char)))
	    (begin
	      (discard-char port)
	      (loop)))))))

(define (input-port/char-ready? port interval)
  ((input-port/operation/char-ready? port) port interval))

(define (input-port/peek-char port)
  ((input-port/operation/peek-char port) port))

(define (input-port/read-char port)
  ((input-port/operation/read-char port) port))

(define (input-port/discard-char port)
  ((input-port/operation/discard-char port) port))

(define (input-port/read-string port delimiters)
  ((input-port/operation/read-string port) port delimiters))

(define (input-port/discard-chars port delimiters)
  ((input-port/operation/discard-chars port) port delimiters))

(define (input-port/channel port)
  (let ((operation (input-port/custom-operation port 'CHANNEL)))
    (and operation
	 (operation port))))

(define eof-object
  "EOF Object")

(define (eof-object? object)
  (eq? object eof-object))

(define (make-eof-object port)
  port
  eof-object)

(define *current-input-port*)

(define-integrable (current-input-port)
  *current-input-port*)

(define (set-current-input-port! port)
  (guarantee-input-port port)
  (set! *current-input-port* port)
  unspecific)

(define (with-input-from-port port thunk)
  (guarantee-input-port port)
  (fluid-let ((*current-input-port* port)) (thunk)))

(define (with-input-from-file input-specifier thunk)
  (let ((new-port (open-input-file input-specifier))
	(old-port false))
    (dynamic-wind (lambda ()
		    (set! old-port *current-input-port*)
		    (set! *current-input-port* new-port)
		    (set! new-port false))
		  thunk
		  (lambda ()
		    (if *current-input-port*
			(close-input-port *current-input-port*))
		    (set! *current-input-port* old-port)
		    (set! old-port false)))))

(define (call-with-input-file input-specifier receiver)
  (let ((port (open-input-file input-specifier)))
    (let ((value (receiver port)))
      (close-input-port port)
      value)))

;;;; Input Procedures

(define (char-ready? #!optional port interval)
  (input-port/char-ready? (if (default-object? port)
			      (current-input-port)
			      (guarantee-input-port port))
			  (if (default-object? interval)
			      0
			      (begin
				(if (not (exact-nonnegative-integer? interval))
				    (error:illegal-datum interval
							 'CHAR-READY?))
				interval))))

(define (peek-char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (let loop ()
      (or (input-port/peek-char port)
	  (loop)))))

(define (read-char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (let loop ()
      (or (input-port/read-char port)
	  (loop)))))

(define (read-char-no-hang #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (if (input-port/char-ready? port 0)
	(input-port/read-char port)
	(let ((eof? (input-port/custom-operation port 'EOF?)))
	  (and eof?
	       (eof? port)
	       eof-object)))))

(define (read-string delimiters #!optional port)
  (input-port/read-string (if (default-object? port)
			      (current-input-port)
			      (guarantee-input-port port))
			  delimiters))

(define (read #!optional port parser-table)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port)))
	(parser-table
	 (if (default-object? parser-table)
	     (current-parser-table)
	     (guarantee-parser-table parser-table))))
    (let ((read-start! (input-port/custom-operation port 'READ-START!)))
      (if read-start!
	  (read-start! port)))
    (let ((object (parse-object/internal port parser-table)))
      (let ((read-finish! (input-port/custom-operation port 'READ-FINISH!)))
	(if read-finish!
	    (read-finish! port)))
      object)))

(define (close-input-port port)
  (let ((operation (input-port/custom-operation port 'CLOSE)))
    (if operation
	(operation port))))