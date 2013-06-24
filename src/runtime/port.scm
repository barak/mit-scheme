#| -*-Scheme-*-

$Id: port.scm,v 1.23 2002/02/09 06:09:55 cph Exp $

Copyright (c) 1991-2002 Massachusetts Institute of Technology

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

(define-structure (port-type (type-descriptor port-type-rtd)
			     (conc-name port-type/)
			     (constructor %make-port-type (custom-operations)))
  custom-operations
  ;; input operations:
  (char-ready? #f read-only #t)
  (peek-char #f read-only #t)
  (read-char #f read-only #t)
  (discard-char #f read-only #t)
  (read-string #f read-only #t)
  (discard-chars #f read-only #t)
  (read-substring #f read-only #t)
  ;; output operations:
  (write-char #f read-only #t)
  (write-substring #f read-only #t)
  (fresh-line #f read-only #t)
  (flush-output #f read-only #t)
  (discretionary-flush-output #f read-only #t))

(set-record-type-unparser-method! port-type-rtd
  (lambda (state type)
    ((standard-unparser-method
      (if (port-type/supports-input? type)
	  (if (port-type/supports-output? type)
	      'I/O-PORT-TYPE
	      'INPUT-PORT-TYPE)
	  (if (port-type/supports-output? type)
	      'OUTPUT-PORT-TYPE
	      'PORT-TYPE))
      #f)
     state
     type)))

(define (guarantee-port-type object procedure)
  (if (not (port-type? object))
      (error:wrong-type-argument object "port type" procedure))
  object)

(define-integrable (port-type/supports-input? type)
  (port-type/read-char type))

(define-integrable (port-type/supports-output? type)
  (port-type/write-char type))

(define (input-port-type? object)
  (and (port-type? object)
       (port-type/supports-input? object)
       #t))

(define (output-port-type? object)
  (and (port-type? object)
       (port-type/supports-output? object)
       #t))

(define (i/o-port-type? object)
  (and (port-type? object)
       (port-type/supports-input? object)
       (port-type/supports-output? object)
       #t))

(define input-operation-names
  '(CHAR-READY?
    DISCARD-CHAR
    DISCARD-CHARS
    PEEK-CHAR
    READ-CHAR
    READ-STRING
    READ-SUBSTRING))

(define input-operation-accessors
  (map (lambda (name) (record-accessor port-type-rtd name))
       input-operation-names))

(define input-operation-modifiers
  (map (lambda (name) (record-modifier port-type-rtd name))
       input-operation-names))

(define output-operation-names
  '(DISCRETIONARY-FLUSH-OUTPUT
    FLUSH-OUTPUT
    FRESH-LINE
    WRITE-CHAR
    WRITE-SUBSTRING))

(define output-operation-accessors
  (map (lambda (name) (record-accessor port-type-rtd name))
       output-operation-names))

(define output-operation-modifiers
  (map (lambda (name) (record-modifier port-type-rtd name))
       output-operation-names))

(define (port-type/operation-names type)
  (guarantee-port-type type 'PORT-TYPE/OPERATION-NAMES)
  (append (if (port-type/supports-input? type) input-operation-names '())
	  (if (port-type/supports-output? type) output-operation-names '())
	  (map car (port-type/custom-operations type))))

(define (port-type/operations type)
  (guarantee-port-type type 'PORT-TYPE/OPERATIONS)
  (append (if (port-type/supports-input? type)
	      (map (lambda (name accessor)
		     (list name (accessor type)))
		   input-operation-names
		   input-operation-accessors)
	      '())
	  (if (port-type/supports-output? type)
	      (map (lambda (name accessor)
		     (list name (accessor type)))
		   output-operation-names
		   output-operation-accessors)
	      '())
	  (map (lambda (entry)
		 (list (car entry) (cdr entry)))
	       (port-type/custom-operations type))))

(define (port-type/operation type name)
  (guarantee-port-type type 'PORT-TYPE/OPERATION)
  ;; Optimized for custom operations, since standard operations will
  ;; usually be accessed directly.
  (let ((entry (assq name (port-type/custom-operations type))))
    (if entry
	(cdr entry)
	(let ((accessor
	       (letrec ((loop
			 (lambda (names accessors)
			   (and (pair? names)
				(if (eq? name (car names))
				    (car accessors)
				    (loop (cdr names) (cdr accessors)))))))
		 (or (and (port-type/supports-input? type)
			  (loop input-operation-names
				input-operation-accessors))
		     (and (port-type/supports-output? type)
			  (loop output-operation-names
				output-operation-accessors))))))
	  (and accessor
	       (accessor type))))))

(define port-rtd (make-record-type "port" '(TYPE STATE THREAD-MUTEX)))
(define %make-port (record-constructor port-rtd '(TYPE STATE THREAD-MUTEX)))
(define port? (record-predicate port-rtd))
(define port/type (record-accessor port-rtd 'TYPE))
(define %port/state (record-accessor port-rtd 'STATE))
(define port/thread-mutex (record-accessor port-rtd 'THREAD-MUTEX))
(define set-port/thread-mutex! (record-modifier port-rtd 'THREAD-MUTEX))

(define (port/state port)
  (%port/state (base-port port)))

(define set-port/state!
  (let ((modifier (record-modifier port-rtd 'STATE)))
    (lambda (port state)
      (modifier (base-port port) state))))

(define (base-port port)
  (let ((state (%port/state port)))
    (if (encapsulated-port-state? state)
	(base-port (encapsulated-port-state/port state))
	port)))

(define (port/operation-names port)
  (port-type/operation-names (port/type port)))

(let-syntax
    ((define-port-operation
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((dir (cadr form))
		(name (caddr form)))
	    `(DEFINE (,(symbol-append dir '-PORT/OPERATION/ name) PORT)
	       (,(close-syntax (symbol-append 'PORT-TYPE/ name) environment)
		(PORT/TYPE PORT))))))))
  (define-port-operation input char-ready?)
  (define-port-operation input peek-char)
  (define-port-operation input read-char)
  (define-port-operation input discard-char)
  (define-port-operation input read-string)
  (define-port-operation input discard-chars)
  (define-port-operation input read-substring)
  (define-port-operation output write-char)
  (define-port-operation output write-substring)
  (define-port-operation output fresh-line)
  (define-port-operation output flush-output))

(define (output-port/operation/discretionary-flush port)
  (port-type/discretionary-flush-output (port/type port)))

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

(define (close-port port)
  (let ((close (port/operation port 'CLOSE)))
    (if close
	(close port)
	(begin
	  (close-output-port port)
	  (close-input-port port)))))

(define (close-input-port port)
  (let ((close-input (port/operation port 'CLOSE-INPUT)))
    (if close-input
	(close-input port))))

(define (close-output-port port)
  (let ((close-output (port/operation port 'CLOSE-OUTPUT)))
    (if close-output
	(close-output port))))

(define (port/input-channel port)
  (let ((operation (port/operation port 'INPUT-CHANNEL)))
    (and operation
	 (operation port))))

(define (port/output-channel port)
  (let ((operation (port/operation port 'OUTPUT-CHANNEL)))
    (and operation
	 (operation port))))

(define (port/operation port name)
  (port-type/operation (port/type port) name))

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

(define (input-port? object)
  (and (port? object)
       (port-type/supports-input? (port/type object))))

(define (output-port? object)
  (and (port? object)
       (port-type/supports-output? (port/type object))))

(define (i/o-port? object)
  (and (port? object)
       (let ((type (port/type object)))
	 (and (port-type/supports-input? type)
	      (port-type/supports-output? type)))))

(define (guarantee-port port)
  (if (not (port? port))
      (error:wrong-type-argument port "port" #f))
  port)

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

;;;; Encapsulation

(define-structure (encapsulated-port-state
		   (conc-name encapsulated-port-state/))
  (port #f read-only #t)
  state)

(define (encapsulated-port? object)
  (and (port? object)
       (encapsulated-port-state? (%port/state object))))

(define (guarantee-encapsulated-port object procedure)
  (guarantee-port object)
  (if (not (encapsulated-port-state? (%port/state object)))
      (error:wrong-type-argument object "encapsulated port" procedure)))

(define (encapsulated-port/port port)
  (guarantee-encapsulated-port port 'ENCAPSULATED-PORT/PORT)
  (encapsulated-port-state/port (%port/state port)))

(define (encapsulated-port/state port)
  (guarantee-encapsulated-port port 'ENCAPSULATED-PORT/STATE)
  (encapsulated-port-state/state (%port/state port)))

(define (set-encapsulated-port/state! port state)
  (guarantee-encapsulated-port port 'SET-ENCAPSULATED-PORT/STATE!)
  (set-encapsulated-port-state/state! (%port/state port) state))

(define (make-encapsulated-port port state rewrite-operation)
  (guarantee-port port)
  (%make-port (let ((type (port/type port)))
		(make-port-type
		 (append-map
		  (lambda (entry)
		    (let ((operation
			   (rewrite-operation (car entry) (cadr entry))))
		      (if operation
			  (list (list (car entry) operation))
			  '())))
		  (port-type/operations type))
		 #f))
	      (make-encapsulated-port-state port state)
	      (port/thread-mutex port)))

;;;; Constructors

(define (make-port type state)
  (guarantee-port-type type 'MAKE-PORT)
  (%make-port type state (make-thread-mutex)))

(define (make-port-type operations type)
  (let ((type
	 (parse-operations-list
	  (append operations
		  (if type
		      (list-transform-negative (port-type/operations type)
			(let ((ignored
			       (append
				(if (assq 'READ-CHAR operations)
				    '(DISCARD-CHAR
				      DISCARD-CHARS
				      PEEK-CHAR
				      READ-CHAR
				      READ-STRING
				      READ-SUBSTRING)
				    '())
				(if (or (assq 'WRITE-CHAR operations)
					(assq 'WRITE-SUBSTRING operations))
				    '(WRITE-CHAR
				      WRITE-SUBSTRING)
				    '()))))
			  (lambda (entry)
			    (or (assq (car entry) operations)
				(memq (car entry) ignored)))))
		      '()))
	  'MAKE-PORT-TYPE)))
    (let ((operations (port-type/operations type)))
      (let ((input? (assq 'READ-CHAR operations))
	    (output?
	     (or (assq 'WRITE-CHAR operations)
		 (assq 'WRITE-SUBSTRING operations))))
	(if (not (or input? output?))
	    (error "Port type must implement one of the following operations:"
		   '(READ-CHAR WRITE-CHAR WRITE-SUBSTRING)))
	(install-operations! type input?
			     input-operation-names
			     input-operation-modifiers
			     input-operation-defaults)
	(install-operations! type output?
			     output-operation-names
			     output-operation-modifiers
			     output-operation-defaults)))
    type))

(define (parse-operations-list operations procedure)
  (if (not (list? operations))
      (error:wrong-type-argument operations "list" procedure))
  (%make-port-type
   (map (lambda (operation)
	  (if (not (and (pair? operation)
			(symbol? (car operation))
			(pair? (cdr operation))
			(procedure? (cadr operation))
			(null? (cddr operation))))
	      (error:wrong-type-argument operation "port operation" procedure))
	  (cons (car operation) (cadr operation)))
	operations)))

(define (install-operations! type install? names modifiers defaults)
  (if install?
      (let* ((operations
	      (map (lambda (name)
		     (extract-operation! type name))
		   names))
	     (defaults (defaults names operations)))
	(for-each (lambda (modifier operation name)
		    (modifier
		     type
		     (or operation
			 (let ((entry (assq name defaults)))
			   (if (not entry)
			       (error "Must specify operation:" name))
			   (cadr entry)))))
		  modifiers
		  operations
		  names))
      (begin
	(for-each (lambda (name)
		    (if (extract-operation! type name)
			(error "Illegal operation name:" name)))
		  names)
	(for-each (lambda (modifier)
		    (modifier type #f))
		  modifiers))))

(define extract-operation!
  (let ((set-port-type/custom-operations!
	 (record-modifier port-type-rtd 'CUSTOM-OPERATIONS)))
    (lambda (type name)
      (let ((operation (assq name (port-type/custom-operations type))))
	(and operation
	     (begin
	       (set-port-type/custom-operations!
		type
		(delq! operation (port-type/custom-operations type)))
	       (cdr operation)))))))

(define (search-paired-lists key keys datums error?)
  (if (pair? keys)
      (if (eq? key (car keys))
	  (car datums)
	  (search-paired-lists key (cdr keys) (cdr datums) error?))
      (and error?
	   (error "Unable to find key:" key))))

;;;; Default Operations

(define (input-operation-defaults names operations)
  `((CHAR-READY? ,default-operation/char-ready?)
    (DISCARD-CHAR ,(search-paired-lists 'READ-CHAR names operations #t))
    (DISCARD-CHARS ,default-operation/discard-chars)
    (READ-STRING ,default-operation/read-string)
    (READ-SUBSTRING ,default-operation/read-substring)))

(define (default-operation/char-ready? port interval)
  port interval
  #t)

(define (default-operation/read-string port delimiters)
  (let ((peek-char
	 (lambda () (let loop () (or (input-port/peek-char port) (loop))))))
    (let ((char (peek-char)))
      (if (eof-object? char)
	  char
	  (list->string
	   (let loop ((char char))
	     (if (or (eof-object? char)
		     (char-set-member? delimiters char))
		 '()
		 (begin
		   (input-port/discard-char port)
		   (cons char (loop (peek-char)))))))))))

(define (default-operation/discard-chars port delimiters)
  (let loop ()
    (let ((char (let loop () (or (input-port/peek-char port) (loop)))))
      (if (not (or (eof-object? char)
		   (char-set-member? delimiters char)))
	  (begin
	    (input-port/discard-char port)
	    (loop))))))

(define (default-operation/read-substring port string start end)
  (let loop ((index start))
    (if (fix:< index end)
	(let ((char (input-port/read-char port)))
	  (cond ((not char)
		 (if (fix:= index start)
		     #f
		     (fix:- index start)))
		((eof-object? char)
		 (fix:- index start))
		(else
		 (string-set! string index char)
		 (loop (fix:+ index 1)))))
	(fix:- index start))))

(define (output-operation-defaults names operations)
  (if (not (or (search-paired-lists 'WRITE-CHAR names operations #f)
	       (search-paired-lists 'WRITE-SUBSTRING names operations #f)))
      (error "Must specify at least one of the following:"
	     '(WRITE-CHAR WRITE-SUBSTRING)))
  `((DISCRETIONARY-FLUSH-OUTPUT ,default-operation/flush-output)
    (FLUSH-OUTPUT ,default-operation/flush-output)
    (FRESH-LINE ,default-operation/fresh-line)
    (WRITE-CHAR ,default-operation/write-char)
    (WRITE-SUBSTRING ,default-operation/write-substring)))

(define (default-operation/write-char port char)
  (output-port/write-substring port (string char) 0 1))

(define (default-operation/write-substring port string start end)
  (let loop ((index start))
    (if (< index end)
	(begin
	  (output-port/write-char port (string-ref string index))
	  (loop (+ index 1))))))

(define (default-operation/fresh-line port)
  (output-port/write-char port #\newline))

(define (default-operation/flush-output port)
  port
  unspecific)

;;;; Special Operations

(define (port/input-blocking-mode port)
  (let ((operation (port/operation port 'INPUT-BLOCKING-MODE)))
    (if operation
	(operation port)
	#f)))

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
	#f)))

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
	#f)))

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
	#f)))

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
  (or *current-input-port* (nearest-cmdl/port)))

(define (set-current-input-port! port)
  (set! *current-input-port* (guarantee-input-port port))
  unspecific)

(define (with-input-from-port port thunk)
  (fluid-let ((*current-input-port* (guarantee-input-port port)))
    (thunk)))

(define (current-output-port)
  (or *current-output-port* (nearest-cmdl/port)))

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

;;;; Upwards Compatibility

(define input-port/channel port/input-channel)
(define input-port/copy port/copy)
(define input-port/custom-operation input-port/operation)
(define input-port/operation-names port/operation-names)
(define input-port/state port/state)
(define output-port/channel port/output-channel)
(define output-port/copy port/copy)
(define output-port/custom-operation output-port/operation)
(define output-port/operation-names port/operation-names)
(define output-port/state port/state)
(define set-input-port/state! set-port/state!)
(define set-output-port/state! set-port/state!)

(define (make-input-port type state)
  (make-port (if (port-type? type) type (make-port-type type #f)) state))

(define make-output-port make-input-port)
(define make-i/o-port make-input-port)