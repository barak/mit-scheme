#| -*-Scheme-*-

$Id: port.scm,v 1.12 1999/02/16 00:49:21 cph Exp $

Copyright (c) 1991-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; I/O Ports
;;; package: (runtime port)

(declare (usual-integrations))

(define port-rtd
  (make-record-type "port"
    '(STATE
      THREAD-MUTEX
      OPERATION-NAMES
      CUSTOM-OPERATIONS
      ;; input operations:
      CHAR-READY?
      PEEK-CHAR
      READ-CHAR
      DISCARD-CHAR
      READ-STRING
      DISCARD-CHARS
      READ-SUBSTRING
      ;; output operations:
      WRITE-CHAR
      WRITE-SUBSTRING
      FLUSH-OUTPUT
      DISCRETIONARY-FLUSH-OUTPUT)))

(define port? (record-predicate port-rtd))
(define port/state (record-accessor port-rtd 'STATE))
(define set-port/state! (record-updater port-rtd 'STATE))
(define port/thread-mutex (record-accessor port-rtd 'THREAD-MUTEX))
(define set-port/thread-mutex! (record-updater port-rtd 'THREAD-MUTEX))
(define port/operation-names (record-accessor port-rtd 'OPERATION-NAMES))
(define set-port/operation-names! (record-updater port-rtd 'OPERATION-NAMES))
(define port/custom-operations (record-accessor port-rtd 'CUSTOM-OPERATIONS))

(define input-port/operation/char-ready?
  (record-accessor port-rtd 'CHAR-READY?))

(define input-port/operation/peek-char
  (record-accessor port-rtd 'PEEK-CHAR))

(define input-port/operation/read-char
  (record-accessor port-rtd 'READ-CHAR))

(define input-port/operation/discard-char
  (record-accessor port-rtd 'DISCARD-CHAR))

(define input-port/operation/read-string
  (record-accessor port-rtd 'READ-STRING))

(define input-port/operation/discard-chars
  (record-accessor port-rtd 'DISCARD-CHARS))

(define input-port/operation/read-substring
  (record-accessor port-rtd 'READ-SUBSTRING))

(define output-port/operation/write-char
  (record-accessor port-rtd 'WRITE-CHAR))

(define output-port/operation/write-substring
  (record-accessor port-rtd 'WRITE-SUBSTRING))

(define output-port/operation/flush-output
  (record-accessor port-rtd 'FLUSH-OUTPUT))

(define output-port/operation/discretionary-flush
  (record-accessor port-rtd 'DISCRETIONARY-FLUSH-OUTPUT))

(set-record-type-unparser-method! port-rtd
  (lambda (state port)
    ((let ((name
	    (cond ((i/o-port? port) 'I/O-PORT)
		  ((input-port? port) 'INPUT-PORT)
		  ((output-port? port) 'OUTPUT-PORT)
		  (else 'PORT))))
       (cond ((port/operation port 'WRITE-SELF)
	      => (lambda (operation)
		   (standard-unparser-method name operation)))
	     ((port/operation port 'PRINT-SELF)
	      => (lambda (operation)
		   (unparser/standard-method name operation)))
	     (else
	      (standard-unparser-method name #f))))
     state
     port)))

(define (port/copy port state)
  (let ((port (record-copy port)))
    (set-port/state! port state)
    (set-port/thread-mutex! port (make-thread-mutex))
    port))

(define (port/operation port name)
  ;; Optimized for custom operations, since standard operations will
  ;; usually be accessed directly.
  (let ((entry (assq name (port/custom-operations port))))
    (if entry
	(cdr entry)
	(case name
	  ((CHAR-READY?) (input-port/operation/char-ready? port))
	  ((PEEK-CHAR) (input-port/operation/peek-char port))
	  ((READ-CHAR) (input-port/operation/read-char port))
	  ((DISCARD-CHAR) (input-port/operation/discard-char port))
	  ((READ-STRING) (input-port/operation/read-string port))
	  ((DISCARD-CHARS) (input-port/operation/discard-chars port))
	  ((READ-SUBSTRING) (input-port/operation/read-substring port))
	  ((WRITE-CHAR) (output-port/operation/write-char port))
	  ((WRITE-SUBSTRING) (output-port/operation/write-substring port))
	  ((FLUSH-OUTPUT) (output-port/operation/flush-output port))
	  ((DISCRETIONARY-FLUSH-OUTPUT)
	   (output-port/operation/discretionary-flush port))
	  (else false)))))

(define (close-port port)
  (let ((operation (port/operation port 'CLOSE)))
    (if operation
	(operation port))))

(define (port/input-channel port)
  (let ((operation (port/operation port 'INPUT-CHANNEL)))
    (and operation
	 (operation port))))

(define (port/output-channel port)
  (let ((operation (port/operation port 'OUTPUT-CHANNEL)))
    (and operation
	 (operation port))))

;; These names required by Scheme standard:
(define close-input-port close-port)
(define close-output-port close-port)

;; These names for upwards compatibility:
(define input-port/channel port/input-channel)
(define input-port/copy port/copy)
(define input-port/operation-names port/operation-names)
(define input-port/state port/state)
(define set-input-port/state! set-port/state!)
(define output-port/channel port/output-channel)
(define output-port/copy port/copy)
(define output-port/operation-names port/operation-names)
(define output-port/state port/state)
(define set-output-port/state! set-port/state!)

(define (input-port/operation port name)
  (port/operation port
		  (case name
		    ((BUFFER-SIZE) 'INPUT-BUFFER-SIZE)
		    ((SET-BUFFER-SIZE) 'SET-INPUT-BUFFER-SIZE)
		    ((BUFFERED-CHARS) 'BUFFERED-INPUT-CHARS)
		    ((CHANNEL) 'INPUT-CHANNEL)
		    (else name))))

(define (output-port/operation port name)
  (port/operation port
		  (case name
		    ((BUFFER-SIZE) 'OUTPUT-BUFFER-SIZE)
		    ((SET-BUFFER-SIZE) 'SET-OUTPUT-BUFFER-SIZE)
		    ((BUFFERED-CHARS) 'BUFFERED-OUTPUT-CHARS)
		    ((CHANNEL) 'OUTPUT-CHANNEL)
		    (else name))))

(define input-port/custom-operation input-port/operation)
(define output-port/custom-operation output-port/operation)

(define (input-port? object)
  (and (port? object)
       (input-port/operation/read-char object)
       #t))

(define (output-port? object)
  (and (port? object)
       (output-port/operation/write-char object)
       #t))

(define (i/o-port? object)
  (and (port? object)
       (input-port/operation/read-char object)
       (output-port/operation/write-char object)
       #t))

(define (guarantee-input-port port)
  (if (not (input-port? port))
      (error:wrong-type-argument port "input port" #f))
  port)

(define (guarantee-output-port port)
  (if (not (output-port? port))
      (error:wrong-type-argument port "output port" #f))
  port)

(define (guarantee-i/o-port port)
  (if (not (i/o-port? port))
      (error:wrong-type-argument port "I/O port" #f))
  port)

;;;; Constructors

(define (make-input-port operations state)
  (make-port operations state 'MAKE-INPUT-PORT true false))

(define (make-output-port operations state)
  (make-port operations state 'MAKE-OUTPUT-PORT false true))

(define (make-i/o-port operations state)
  (make-port operations state 'MAKE-I/O-PORT true true))

(define make-port
  (let ((constructor
	 (record-constructor
	  port-rtd
	  '(STATE THREAD-MUTEX OPERATION-NAMES CUSTOM-OPERATIONS))))
    (lambda (operations state procedure-name input? output?)
      (let ((port
	     (constructor state
			  (make-thread-mutex)
			  '()
			  (parse-operations-list operations procedure-name))))
	(install-input-operations! port input?)
	(install-output-operations! port output?)
	(set-port/operation-names! port
				   (map* (port/operation-names port)
					 car
					 (port/custom-operations port)))
	port))))

(define (parse-operations-list operations procedure)
  (if (not (list? operations))
      (error:wrong-type-argument operations "list" procedure))
  (map (lambda (operation)
	 (if (not (and (pair? operation)
		       (symbol? (car operation))
		       (pair? (cdr operation))
		       (procedure? (cadr operation))
		       (null? (cddr operation))))
	     (error:wrong-type-argument operation "port operation" procedure))
	 (cons (car operation) (cadr operation)))
       operations))

(define extract-operation!
  (let ((updater (record-updater port-rtd 'CUSTOM-OPERATIONS)))
    (lambda (port name)
      (let ((operations (port/custom-operations port)))
	(let ((operation (assq name operations)))
	  (and operation
	       (begin
		 (updater port (delq! operation operations))
		 (cdr operation))))))))

;;;; Input Operations

(define install-input-operations!
  (let ((operation-names
	 '(CHAR-READY? PEEK-CHAR READ-CHAR
		       DISCARD-CHAR READ-STRING DISCARD-CHARS READ-SUBSTRING)))
    (let ((updaters
	   (map (lambda (name)
		  (record-updater port-rtd name))
		operation-names)))
      (lambda (port install?)
	(if install?
	    (let ((operations
		   (map (lambda (name)
			  (extract-operation! port name))
			operation-names)))
	      (for-each (lambda (updater operation default name)
			  (updater
			   port
			   (or operation
			       default
			       (error "Must specify operation:" name))))
			updaters
			operations
			(list default-operation/char-ready?
			      false
			      false
			      (caddr operations)
			      default-operation/read-string
			      default-operation/discard-chars
			      default-operation/read-substring)
			operation-names)
	      (set-port/operation-names!
	       port
	       (append operation-names (port/operation-names port))))
	    (begin
	      (for-each (lambda (name)
			  (if (extract-operation! port name)
			      (error "Illegal operation name:" name)))
			operation-names)
	      (for-each (lambda (updater)
			  (updater port false))
			updaters)))))))

(define (default-operation/char-ready? port interval)
  port interval
  true)

(define (default-operation/read-string port delimiters)
  (let ((peek-char (input-port/operation/peek-char port))
	(discard-char (input-port/operation/discard-char port)))
    (let ((peek-char (lambda () (let loop () (or (peek-char port) (loop))))))
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

(define (default-operation/read-substring port string start end)
  (let ((read-char (input-port/operation/read-char port)))
    (let loop ((index start))
      (if (fix:< index end)
	  (let ((char (read-char port)))
	    (cond ((not char)
		   (if (fix:= index start)
		       #f
		       (fix:- index start)))
		  ((eof-object? char)
		   (fix:- index start))
		  (else
		   (string-set! string index char)
		   (loop (fix:+ index 1)))))
	  (fix:- index start)))))

;;;; Output Operations

(define (default-operation/write-char port char)
  ((output-port/operation/write-substring port) port (string char) 0 1))

(define (default-operation/write-substring port string start end)
  (let ((write-char (output-port/operation/write-char port)))
    (let loop ((index start))
      (if (< index end)
	  (begin
	    (write-char port (string-ref string index))
	    (loop (+ index 1)))))))

(define (default-operation/flush-output port)
  port
  unspecific)

(define install-output-operations!
  (let ((operation-names
	 '(WRITE-CHAR WRITE-SUBSTRING FLUSH-OUTPUT DISCRETIONARY-FLUSH-OUTPUT))
	(operation-defaults
	 (list default-operation/write-char
	       default-operation/write-substring
	       default-operation/flush-output
	       default-operation/flush-output)))
    (let ((updaters
	   (map (lambda (name)
		  (record-updater port-rtd name))
		operation-names)))
      (lambda (port install?)
	(if install?
	    (let ((operations
		   (map (lambda (name)
			  (extract-operation! port name))
			operation-names)))
	      (if (not (or (car operations) (cadr operations)))
		  (error "Must specify at least one of the following:"
			 '(WRITE-CHAR WRITE-SUBSTRING)))
	      (for-each (lambda (updater operation default)
			  (updater port (or operation default)))
			updaters
			operations
			operation-defaults)
	      (set-port/operation-names! port
					 (append operation-names
						 (port/operation-names port))))
	    (begin
	      (for-each (lambda (name)
			  (if (extract-operation! port name)
			      (error "Illegal operation name:" name)))
			operation-names)
	      (for-each (lambda (updater)
			  (updater port false))
			updaters)))))))

;;;; Special Operations

(define (port/input-blocking-mode port)
  (let ((operation (port/operation port 'INPUT-BLOCKING-MODE)))
    (if operation
	(operation port)
	false)))

(define (port/set-input-blocking-mode port mode)
  (let ((operation (port/operation port 'SET-INPUT-BLOCKING-MODE)))
    (if operation
	(operation port mode))))

(define (port/with-input-blocking-mode port mode thunk)
  (bind-mode port 'INPUT-BLOCKING-MODE 'SET-INPUT-BLOCKING-MODE mode thunk))

(define (port/output-blocking-mode port)
  (let ((operation (port/operation port 'OUTPUT-BLOCKING-MODE)))
    (if operation
	(operation port)
	false)))

(define (port/set-output-blocking-mode port mode)
  (let ((operation (port/operation port 'SET-OUTPUT-BLOCKING-MODE)))
    (if operation
	(operation port mode))))

(define (port/with-output-blocking-mode port mode thunk)
  (bind-mode port 'OUTPUT-BLOCKING-MODE 'SET-OUTPUT-BLOCKING-MODE mode thunk))

(define (port/input-terminal-mode port)
  (let ((operation (port/operation port 'INPUT-TERMINAL-MODE)))
    (if operation
	(operation port)
	false)))

(define (port/set-input-terminal-mode port mode)
  (let ((operation (port/operation port 'SET-INPUT-TERMINAL-MODE)))
    (if operation
	(operation port mode))))

(define (port/with-input-terminal-mode port mode thunk)
  (bind-mode port 'INPUT-TERMINAL-MODE 'SET-INPUT-TERMINAL-MODE mode thunk))

(define (port/output-terminal-mode port)
  (let ((operation (port/operation port 'OUTPUT-TERMINAL-MODE)))
    (if operation
	(operation port)
	false)))

(define (port/set-output-terminal-mode port mode)
  (let ((operation (port/operation port 'SET-OUTPUT-TERMINAL-MODE)))
    (if operation
	(operation port mode))))

(define (port/with-output-terminal-mode port mode thunk)
  (bind-mode port 'OUTPUT-TERMINAL-MODE 'SET-OUTPUT-TERMINAL-MODE mode thunk))

(define (bind-mode port read-mode write-mode mode thunk)
  (let ((read-mode (port/operation port read-mode))
	(write-mode (port/operation port write-mode)))
    (if (and read-mode write-mode (read-mode port))
	(let ((outside-mode))
	  (dynamic-wind (lambda ()
			  (set! outside-mode (read-mode port))
			  (write-mode port mode))
			thunk
			(lambda ()
			  (set! mode (read-mode port))
			  (write-mode port outside-mode))))
	(thunk))))

;;;; Standard Ports

(define *current-input-port*)
(define *current-output-port*)
(define *notification-output-port* #f)
(define *trace-output-port* #f)
(define *interaction-i/o-port* #f)

(define (current-input-port)
  *current-input-port*)

(define (set-current-input-port! port)
  (set! *current-input-port* (guarantee-input-port port))
  unspecific)

(define (with-input-from-port port thunk)
  (fluid-let ((*current-input-port* (guarantee-input-port port)))
    (thunk)))

(define (current-output-port)
  *current-output-port*)

(define (set-current-output-port! port)
  (set! *current-output-port* (guarantee-output-port port))
  unspecific)

(define (with-output-to-port port thunk)
  (fluid-let ((*current-output-port* (guarantee-output-port port)))
    (thunk)))

(define (notification-output-port)
  (or *notification-output-port* (nearest-cmdl/port)))

(define (set-notification-output-port! port)
  (set! *notification-output-port* (guarantee-output-port port))
  unspecific)

(define (with-notification-output-port port thunk)
  (fluid-let ((*notification-output-port* (guarantee-output-port port)))
    (thunk)))

(define (trace-output-port)
  (or *trace-output-port* (nearest-cmdl/port)))

(define (set-trace-output-port! port)
  (set! *trace-output-port* (guarantee-output-port port))
  unspecific)

(define (with-trace-output-port port thunk)
  (fluid-let ((*trace-output-port* (guarantee-output-port port)))
    (thunk)))

(define (interaction-i/o-port)
  (or *interaction-i/o-port* (nearest-cmdl/port)))

(define (set-interaction-i/o-port! port)
  (set! *interaction-i/o-port* (guarantee-i/o-port port))
  unspecific)

(define (with-interaction-i/o-port port thunk)
  (fluid-let ((*interaction-i/o-port* (guarantee-i/o-port port)))
    (thunk)))

(define standard-port-accessors
  (list (cons current-input-port set-current-input-port!)
	(cons current-output-port set-current-output-port!)
	(cons notification-output-port set-notification-output-port!)
	(cons trace-output-port set-trace-output-port!)
	(cons interaction-i/o-port set-interaction-i/o-port!)))