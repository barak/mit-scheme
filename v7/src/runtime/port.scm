#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/port.scm,v 1.4 1992/02/27 01:11:19 cph Exp $

Copyright (c) 1991-92 Massachusetts Institute of Technology

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
      ;; output operations:
      WRITE-CHAR
      WRITE-STRING
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

(define output-port/operation/write-char
  (record-accessor port-rtd 'WRITE-CHAR))

(define output-port/operation/write-string
  (record-accessor port-rtd 'WRITE-STRING))

(define output-port/operation/write-substring
  (record-accessor port-rtd 'WRITE-SUBSTRING))

(define output-port/operation/flush-output
  (record-accessor port-rtd 'FLUSH-OUTPUT))

(define output-port/operation/discretionary-flush
  (record-accessor port-rtd 'DISCRETIONARY-FLUSH-OUTPUT))

(set-record-type-unparser-method! port-rtd
  (lambda (state port)
    ((unparser/standard-method
      (cond ((i/o-port? port) 'I/O-PORT)
	    ((input-port? port) 'INPUT-PORT)
	    ((output-port? port) 'OUTPUT-PORT)
	    (else 'PORT))
      (port/operation port 'PRINT-SELF))
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
	  ((WRITE-CHAR) (output-port/operation/write-char port))
	  ((WRITE-STRING) (output-port/operation/write-string port))
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

;;;; Constructors

(define (input-port? object)
  (and (port? object)
       (input-port/operation/read-char object)
       true))

(define (output-port? object)
  (and (port? object)
       (output-port/operation/write-char object)
       true))

(define (i/o-port? object)
  (and (port? object)
       (input-port/operation/read-char object)
       (output-port/operation/write-char object)
       true))

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
		       DISCARD-CHAR READ-STRING DISCARD-CHARS)))
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
			      default-operation/discard-chars)
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

;;;; Output Operations

(define (default-operation/write-char port char)
  ((output-port/operation/write-substring port) port (string char) 0 1))

(define (default-operation/write-string port string)
  ((output-port/operation/write-substring port)
   port
   string 0 (string-length string)))

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
	 '(WRITE-CHAR WRITE-SUBSTRING WRITE-STRING
		      FLUSH-OUTPUT DISCRETIONARY-FLUSH-OUTPUT))
	(operation-defaults
	 (list default-operation/write-char
	       default-operation/write-substring
	       default-operation/write-string
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