#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/input.scm,v 14.5 1989/10/26 06:46:27 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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
  (operation/peek-char-immediate false read-only true)
  (operation/read-char-immediate false read-only true)
  (operation/discard-char false read-only true)
  (operation/read-string false read-only true)
  (operation/discard-chars false read-only true)
  (operation/read-start! false read-only true)
  (operation/read-finish! false read-only true)
  (custom-operations false read-only true))

(define (guarantee-input-port port)
  (if (not (input-port? port)) (error "Bad input port" port))
  port)

(define (input-port/copy port state)
  (let ((result (%input-port/copy port)))
    (set-input-port/state! result state)
    result))

(define (input-port/custom-operation port name)
  (let ((entry (assq name (input-port/custom-operations port))))
    (and entry
	 (cdr entry))))

(define (input-port/operation port name)
  (or (input-port/custom-operation port name)
      (case name
	((OPERATION/CHAR-READY?) (input-port/operation/char-ready? port))
	((OPERATION/PEEK-CHAR) (input-port/operation/peek-char port))
	((OPERATION/READ-CHAR) (input-port/operation/read-char port))
	((OPERATION/PEEK-CHAR-IMMEDIATE)
	 (input-port/operation/peek-char-immediate port))
	((OPERATION/READ-CHAR-IMMEDIATE)
	 (input-port/operation/read-char-immediate port))
	((OPERATION/DISCARD-CHAR) (input-port/operation/discard-char port))
	((OPERATION/READ-STRING) (input-port/operation/read-string port))
	((OPERATION/DISCARD-CHARS) (input-port/operation/discard-chars port))
	((OPERATION/READ-START!) (input-port/operation/read-start! port))
	((OPERATION/READ-FINISH!) (input-port/operation/read-finish! port))
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
		   (begin (set! operations (delq! entry operations))
			  (cdr entry))
		   (or default
		       (error "MAKE-INPUT-PORT: missing operation" name)))))))
      (let ((char-ready? (operation 'CHAR-READY? false))
	    (peek-char (operation 'PEEK-CHAR false))
	    (read-char (operation 'READ-CHAR false))
	    (read-string
	     (operation 'READ-STRING default-operation/read-string))
	    (discard-chars
	     (operation 'DISCARD-CHARS default-operation/discard-chars))
	    (read-start!
	     (operation 'READ-START! default-operation/read-start!))
	    (read-finish!
	     (operation 'READ-FINISH! default-operation/read-finish!)))
	(let ((peek-char-immediate (operation 'PEEK-CHAR-IMMEDIATE peek-char))
	      (read-char-immediate (operation 'READ-CHAR-IMMEDIATE read-char))
	      (discard-char (operation 'DISCARD-CHAR read-char)))
	  (%make-input-port state
			    char-ready?
			    peek-char
			    read-char
			    peek-char-immediate
			    read-char-immediate
			    discard-char
			    read-string
			    discard-chars
			    read-start!
			    read-finish!
			    operations))))))

(define (default-operation/read-string port delimiters)
  (list->string
   (let ((peek-char (input-port/operation/peek-char port))
	 (read-char (input-port/operation/read-char port)))
     (let loop ()
       (if (char-set-member? delimiters (peek-char port))
	   '()
	   (let ((char (read-char port)))
	     (cons char (loop))))))))

(define (default-operation/discard-chars port delimiters)
  (let ((peek-char (input-port/operation/peek-char port))
	(discard-char (input-port/operation/discard-char port)))
    (let loop ()
      (if (not (char-set-member? delimiters (peek-char port)))
	  (begin (discard-char port)
		 (loop))))))

(define (default-operation/read-start! port)
  port
  false)

(define (default-operation/read-finish! port)
  port
  false)

(define (input-port/char-ready? port interval)
  ((input-port/operation/char-ready? port) port interval))

(define (input-port/peek-char port)
  ((input-port/operation/peek-char port) port))

(define (input-port/read-char port)
  ((input-port/operation/read-char port) port))

(define (input-port/peek-char-immediate port)
  ((input-port/operation/peek-char-immediate port) port))

(define (input-port/read-char-immediate port)
  ((input-port/operation/read-char-immediate port) port))

(define (input-port/discard-char port)
  ((input-port/operation/discard-char port) port))

(define (input-port/read-string port delimiters)
  ((input-port/operation/read-string port) port delimiters))

(define (input-port/discard-chars port delimiters)
  ((input-port/operation/discard-chars port) port delimiters))

(define (input-port/read-start! port)
  ((input-port/operation/read-start! port) port))

(define (input-port/read-finish! port)
  ((input-port/operation/read-finish! port) port))

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

;;; **** The INTERVAL option for this operation works only for the
;;; console port.  Only Edwin uses this option.

(define (char-ready? #!optional port interval)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port)))
	(interval
	 (if (default-object? interval)
	     0
	     (begin
	       (if (not (exact-nonnegative-integer? interval))
		   (error "interval must be exact nonnegative integer"
			  interval))
	       interval))))
    (input-port/char-ready? port interval)))

(define (peek-char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (or (input-port/peek-char-immediate port)
	eof-object)))

(define (read-char #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (or (input-port/read-char-immediate port)
	eof-object)))

(define (read-char-no-hang #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (and (input-port/char-ready? port 0)
	 (or (input-port/read-char-immediate port)
	     eof-object))))

(define (read-string delimiters #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port))))
    (or (input-port/read-string port delimiters)
	eof-object)))

(define (read #!optional port parser-table)
  (let ((port
	 (if (default-object? port)
	     (current-input-port)
	     (guarantee-input-port port)))
	(parser-table
	 (if (default-object? parser-table)
	     (current-parser-table)
	     (guarantee-parser-table parser-table))))
    (input-port/read-start! port)
    (let ((object (parse-object/internal port parser-table)))
      (input-port/read-finish! port)
      object)))

(define (close-input-port port)
  (let ((operation (input-port/custom-operation port 'CLOSE)))
    (if operation
	(operation port))))