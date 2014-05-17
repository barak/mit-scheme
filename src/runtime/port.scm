#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; I/O Ports
;;; package: (runtime port)

(declare (usual-integrations))

;;;; Port type

(define-structure (port-type (type-descriptor <port-type>)
			     (conc-name port-type/)
			     (constructor %make-port-type))
  (parent #f read-only #t)
  standard-operations
  custom-operations
  ;; input operations:
  (char-ready? #f read-only #t)
  (read-char #f read-only #t)
  (unread-char #f read-only #t)
  (peek-char #f read-only #t)
  (read-substring #f read-only #t)
  ;; output operations:
  (write-char #f read-only #t)
  (write-substring #f read-only #t)
  (fresh-line #f read-only #t)
  (line-start? #f read-only #t)
  (flush-output #f read-only #t)
  (discretionary-flush-output #f read-only #t))

(set-record-type-unparser-method! <port-type>
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

(define (port-type/operation-names type)
  (guarantee-port-type type 'PORT-TYPE/OPERATION-NAMES)
  (append (map car (port-type/standard-operations type))
	  (map car (port-type/custom-operations type))))

(define (port-type/operations type)
  (guarantee-port-type type 'PORT-TYPE/OPERATIONS)
  (append! (map (lambda (entry)
		  (list (car entry) (cdr entry)))
		(port-type/standard-operations type))
	   (map (lambda (entry)
		  (list (car entry) (cdr entry)))
		(port-type/custom-operations type))))

;; Assumes type is a PORT-TYPE
(define (port-type/%operation type name)
  (let ((entry
	 (or (assq name (port-type/custom-operations type))
	     (assq name (port-type/standard-operations type)))))
    (and entry
	 (cdr entry))))

(define (port-type/operation type name)
  (guarantee-port-type type 'PORT-TYPE/OPERATION)
  (port-type/%operation type name))

;;;; Constructors

(define (make-port-type operations parent-type)
  (if (not (list-of-type? operations
	     (lambda (elt)
	       (and (pair? elt)
		    (symbol? (car elt))
		    (pair? (cdr elt))
		    (procedure? (cadr elt))
		    (null? (cddr elt))))))
      (error:wrong-type-argument operations "operations list" 'MAKE-PORT-TYPE))
  (if parent-type
      (guarantee-port-type parent-type 'MAKE-PORT-TYPE))
  (receive (standard-operations custom-operations)
      (parse-operations-list operations parent-type)
    (let ((op
	   (let ((input? (assq 'READ-CHAR standard-operations))
		 (output? (assq 'WRITE-CHAR standard-operations))
		 (cond-op
		  (lambda (flag mapper)
		    (if flag
			mapper
			(lambda (op) op)))))
	     ((cond-op output? provide-output-features)
	      ((cond-op input? provide-input-features)
	       ((cond-op output? provide-default-output-operations)
		((cond-op input? provide-default-input-operations)
		 (lambda (name)
		   (let ((p (assq name standard-operations)))
		     (and p
			  (cdr p)))))))))))
      (%make-port-type parent-type
		       standard-operations
		       custom-operations
		       (op 'CHAR-READY?)
		       (op 'READ-CHAR)
		       (op 'UNREAD-CHAR)
		       (op 'PEEK-CHAR)
		       (op 'READ-SUBSTRING)
		       (op 'WRITE-CHAR)
		       (op 'WRITE-SUBSTRING)
		       (op 'FRESH-LINE)
		       (op 'LINE-START?)
		       (op 'FLUSH-OUTPUT)
		       (op 'DISCRETIONARY-FLUSH-OUTPUT)))))

(define (parse-operations-list operations parent-type)
  (parse-operations-list-1
   (if parent-type
       (append operations
	       (delete-matching-items (port-type/operations parent-type)
		 (let ((excluded
			(append
			 (if (assq 'READ-CHAR operations)
			     standard-input-operation-names
			     '())
			 (if (assq 'WRITE-CHAR operations)
			     standard-output-operation-names
			     '()))))
		   (lambda (p)
		     (or (assq (car p) operations)
			 (memq (car p) excluded))))))
       operations)))

(define (parse-operations-list-1 operations)
  (let loop ((operations operations) (standard '()) (custom '()))
    (if (pair? operations)
	(let ((p (cons (caar operations) (cadar operations))))
	  (if (or (memq (caar operations) standard-input-operation-names)
		  (memq (caar operations) standard-output-operation-names))
	      (loop (cdr operations) (cons p standard) custom)
	      (loop (cdr operations) standard (cons p custom))))
	(values (reverse! standard) (reverse! custom)))))

(define standard-input-operation-names
  '(CHAR-READY?
    PEEK-CHAR
    READ-CHAR
    READ-SUBSTRING
    UNREAD-CHAR))

(define standard-output-operation-names
  '(WRITE-CHAR
    WRITE-SUBSTRING
    FLUSH-OUTPUT
    DISCRETIONARY-FLUSH-OUTPUT))

;;;; Default I/O operations

(define (required-operation op name)
  (if (not (op name))
      (error "Missing required operation:" name)))

(define (provide-default-input-operations op)
  (required-operation op 'READ-CHAR)
  (if (and (or (op 'UNREAD-CHAR)
	       (op 'PEEK-CHAR))
	   (not (and (op 'UNREAD-CHAR)
		     (op 'PEEK-CHAR))))
      (error "Must provide both UNREAD-CHAR and PEEK-CHAR operations."))
  (let ((char-ready?
	 (or (op 'CHAR-READY?)
	     (lambda (port) port #t)))
	(read-substring
	 (or (op 'READ-SUBSTRING)
	     generic-port-operation:read-substring)))
    (lambda (name)
      (case name
	((CHAR-READY?) char-ready?)
	((READ-SUBSTRING) read-substring)
	(else (op name))))))

(define (generic-port-operation:read-substring port string start end)
  (let ((char-ready? (port/operation/char-ready? port))
	(read-char (port/operation/read-char port)))
    (let ((char (read-char port)))
      (cond ((not char) #f)
	    ((eof-object? char) 0)
	    (else
	     (xstring-set! string start char)
	     (let loop ((index (+ start 1)))
	       (if (and (< index end)
			(char-ready? port))
		   (let ((char (read-char port)))
		     (if (or (not char) (eof-object? char))
			 (- index start)
			 (begin
			   (xstring-set! string index char)
			   (loop (+ index 1)))))
		   (- index start))))))))

(define (provide-default-output-operations op)
  (required-operation op 'WRITE-CHAR)
  (let ((write-substring
	 (or (op 'WRITE-SUBSTRING)
	     generic-port-operation:write-substring))
	(flush-output
	 (or (op 'FLUSH-OUTPUT)
	     no-flush))
	(discretionary-flush-output
	 (or (op 'DISCRETIONARY-FLUSH-OUTPUT)
	     no-flush)))
    (lambda (name)
      (case name
	((WRITE-SUBSTRING) write-substring)
	((FLUSH-OUTPUT) flush-output)
	((DISCRETIONARY-FLUSH-OUTPUT) discretionary-flush-output)
	(else (op name))))))

(define (no-flush port)
  port
  unspecific)

(define (generic-port-operation:write-substring port string start end)
  (let ((write-char (port/operation/write-char port)))
    (let loop ((i start))
      (if (< i end)
	  (let ((n (write-char port (xstring-ref string i))))
	    (cond ((not n) (and (> i start) (- i start)))
		  ((> n 0) (loop (+ i 1)))
		  (else (- i start))))
	  (- i start)))))

;;;; Input features

(define (provide-input-features op)
  (let ((read-char
	 (let ((defer (op 'READ-CHAR)))
	   (lambda (port)
	     (let ((char (defer port)))
	       (transcribe-input-char char port)
	       (set-port/unread?! port #f)
	       char))))
	(unread-char
	 (let ((defer (op 'UNREAD-CHAR)))
	   (and defer
		(lambda (port char)
		  (defer port char)
		  (set-port/unread?! port #t)))))
	(peek-char
	 (let ((defer (op 'PEEK-CHAR)))
	   (and defer
		(lambda (port)
		  (let ((char (defer port)))
		    (transcribe-input-char char port)
		    (set-port/unread?! port #t)
		    char)))))
	(read-substring
	 (let ((defer (op 'READ-SUBSTRING)))
	   (lambda (port string start end)
	     (let ((n (defer port string start end)))
	       (transcribe-input-substring string start n port)
	       (set-port/unread?! port #f)
	       n)))))
    (lambda (name)
      (case name
	((READ-CHAR) read-char)
	((UNREAD-CHAR) unread-char)
	((PEEK-CHAR) peek-char)
	((READ-SUBSTRING) read-substring)
	(else (op name))))))

(define (transcribe-input-char char port)
  (if (and (char? char)
	   (not (port/unread? port)))
      (transcribe-char char port)))

(define (transcribe-input-substring string start n port)
  (if (and n (> n 0))
      (transcribe-substring string
			    (if (port/unread? port) (+ start 1) start)
			    (+ start n)
			    port)))

;;;; Output features

(define (provide-output-features op)
  (let ((write-char
	 (let ((defer (op 'WRITE-CHAR)))
	   (lambda (port char)
	     (let ((n (defer port char)))
	       (if (and n (fix:> n 0))
		   (begin
		     (set-port/previous! port char)
		     (transcribe-char char port)))
	       n))))
	(write-substring
	 (let ((defer (op 'WRITE-SUBSTRING)))
	   (lambda (port string start end)
	     (let ((n (defer port string start end)))
	       (if (and n (> n 0))
		   (let ((end (+ start n)))
		     (set-port/previous! port (xstring-ref string (- end 1)))
		     (transcribe-substring string start end port)))
	       n))))
	(flush-output
	 (let ((defer (op 'FLUSH-OUTPUT)))
	   (lambda (port)
	     (defer port)
	     (flush-transcript port))))
	(discretionary-flush-output
	 (let ((defer (op 'DISCRETIONARY-FLUSH-OUTPUT)))
	   (lambda (port)
	     (defer port)
	     (discretionary-flush-transcript port))))
	(line-start?
	 (lambda (port)
	   (if (port/previous port)
	       (char=? (port/previous port) #\newline)
	       'UNKNOWN))))
    (let ((fresh-line
	   (lambda (port)
	     (if (and (port/previous port)
		      (not (char=? (port/previous port) #\newline)))
		 (write-char port #\newline)
		 0))))
      (lambda (name)
	(case name
	  ((WRITE-CHAR) write-char)
	  ((WRITE-SUBSTRING) write-substring)
	  ((FRESH-LINE) fresh-line)
	  ((LINE-START?) line-start?)
	  ((FLUSH-OUTPUT) flush-output)
	  ((DISCRETIONARY-FLUSH-OUTPUT) discretionary-flush-output)
	  (else (op name)))))))

;;;; Port object

(define-structure (port (type-descriptor <port>)
			(conc-name port/)
			(constructor %make-port (%type %state)))
  %type
  %state
  (%thread-mutex (make-thread-mutex))
  (unread? #f)
  (previous #f)
  (properties '())
  (transcript #f))

(define (make-port type state)
  (guarantee-port-type type 'MAKE-PORT)
  (%make-port type state))

(define (port/type port)
  (guarantee-port port 'PORT/TYPE)
  (port/%type port))

(define (set-port/type! port type)
  (guarantee-port port 'SET-PORT/TYPE!)
  (guarantee-port-type type 'SET-PORT/TYPE!)
  (set-port/%type! port type))

(define (port/state port)
  (guarantee-port port 'PORT/STATE)
  (port/%state port))

(define (set-port/state! port state)
  (guarantee-port port 'SET-PORT/STATE!)
  (set-port/%state! port state))

(define (port/thread-mutex port)
  (guarantee-port port 'PORT/THREAD-MUTEX)
  (port/%thread-mutex port))

(define (set-port/thread-mutex! port mutex)
  (set-port/%thread-mutex! port mutex))

(define (port=? p1 p2)
  (guarantee-port p1 'PORT=?)
  (guarantee-port p2 'PORT=?)
  (eq? p1 p2))

(define (port/operation-names port)
  (port-type/operation-names (port/type port)))

(define-integrable (port/%operation port name)
  (port-type/%operation (port/%type port) name))

(define (port/operation port name)
  (guarantee-port port 'port/operation)
  (port/%operation port name))

(define-syntax define-port-operation
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form)))
       `(DEFINE (,(symbol-append 'PORT/OPERATION/ name) PORT)
	  (,(close-syntax (symbol-append 'PORT-TYPE/ name) environment)
	   (PORT/TYPE PORT)))))))

(define-port-operation char-ready?)
(define-port-operation read-char)
(define-port-operation unread-char)
(define-port-operation peek-char)
(define-port-operation read-substring)
(define-port-operation write-char)
(define-port-operation write-substring)
(define-port-operation fresh-line)
(define-port-operation line-start?)
(define-port-operation flush-output)
(define-port-operation discretionary-flush-output)

;;; These operations assume that the port is in fact a port.
(define-syntax define-unsafe-port-operation
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form)))
       `(DEFINE-INTEGRABLE (,(symbol-append 'PORT/%OPERATION/ name) PORT)
	  (,(close-syntax (symbol-append 'PORT-TYPE/ name) environment)
	   (PORT/%TYPE PORT)))))))

(define-unsafe-port-operation discretionary-flush-output)
(define-unsafe-port-operation read-char)
(define-unsafe-port-operation peek-char)
(define-unsafe-port-operation write-char)

(define (port-position port)
  ((or (port/operation port 'POSITION)
       (error:bad-range-argument port 'PORT-POSITION))
   port))

(define (set-port-position! port position)
  ((or (port/operation port 'SET-POSITION!)
       (error:bad-range-argument port 'SET-PORT-POSITION!))
   port position))

(set-record-type-unparser-method! <port>
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
  (let ((port (copy-record port)))
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

(define (port/open? port)
  (let ((open? (port/operation port 'OPEN?)))
    (if open?
	(open? port)
	(and (if (input-port? port) (%input-open? port) #t)
	     (if (output-port? port) (%output-open? port) #t)))))

(define (port/input-open? port)
  (and (input-port? port)
       (%input-open? port)))

(define (%input-open? port)
  (let ((open? (port/%operation port 'INPUT-OPEN?)))
    (if open?
	(open? port)
	#t)))

(define (port/output-open? port)
  (and (output-port? port)
       (%output-open? port)))

(define (%output-open? port)
  (let ((open? (port/%operation port 'OUTPUT-OPEN?)))
    (if open?
	(open? port)
	#t)))

(define (port/input-channel port)
  (let ((operation (port/operation port 'INPUT-CHANNEL)))
    (and operation
	 (operation port))))

(define (port/output-channel port)
  (let ((operation (port/operation port 'OUTPUT-CHANNEL)))
    (and operation
	 (operation port))))

(define (port/get-property port name default)
  (guarantee-symbol name 'PORT/GET-PROPERTY)
  (let ((p (assq name (port/properties port))))
    (if p
	(cdr p)
	default)))

(define (port/set-property! port name value)
  (guarantee-symbol name 'PORT/SET-PROPERTY!)
  (let ((alist (port/properties port)))
    (let ((p (assq name alist)))
      (if p
	  (set-cdr! p value)
	  (set-port/properties! port (cons (cons name value) alist))))))

(define (port/intern-property! port name get-value)
  (guarantee-symbol name 'PORT/INTERN-PROPERTY!)
  (let ((alist (port/properties port)))
    (let ((p (assq name alist)))
      (if p
	  (cdr p)
	  (let ((value (get-value)))
	    (set-port/properties! port (cons (cons name value) alist))
	    value)))))

(define (port/remove-property! port name)
  (guarantee-symbol name 'PORT/REMOVE-PROPERTY!)
  (set-port/properties! port (del-assq! name (port/properties port))))

(define (transcribe-char char port)
  (let ((tport (port/transcript port)))
    (if tport
	(%write-char char tport))))

(define (transcribe-substring string start end port)
  (let ((tport (port/transcript port)))
    (if tport
	(write-substring string start end tport))))

(define (flush-transcript port)
  (let ((tport (port/transcript port)))
    (if tport
	(flush-output tport))))

(define (discretionary-flush-transcript port)
  (let ((tport (port/transcript port)))
    (if tport
	(output-port/discretionary-flush tport))))

(define (input-port? object)
  (and (port? object)
       (port-type/supports-input? (port/%type object))
       #t))

(define (output-port? object)
  (and (port? object)
       (port-type/supports-output? (port/%type object))
       #t))

(define (i/o-port? object)
  (and (port? object)
       (let ((type (port/%type object)))
	 (and (port-type/supports-input? type)
	      (port-type/supports-output? type)
	      #t))))

(define-integrable (guarantee-port port caller)
  (if (not (port? port))
      (error:not-port port caller))
  port)

(define (error:not-port port caller)
  (error:wrong-type-argument port "port" caller))

(define-integrable (guarantee-input-port port caller)
  (if (not (input-port? port))
      (error:not-input-port port caller))
  port)

(define (error:not-input-port port caller)
  (error:wrong-type-argument port "input port" caller))

(define-integrable (guarantee-output-port port caller)
  (if (not (output-port? port))
      (error:not-output-port port caller))
  port)

(define (error:not-output-port port caller)
  (error:wrong-type-argument port "output port" caller))

(define-integrable (guarantee-i/o-port port caller)
  (if (not (i/o-port? port))
      (error:not-i/o-port port caller))
  port)

(define (error:not-i/o-port port caller)
  (error:wrong-type-argument port "I/O port" caller))

(define (port/supports-coding? port)
  (let ((operation (port/operation port 'SUPPORTS-CODING?)))
    (if operation
	(operation port)
	#f)))

(define (port/coding port)
  ((or (port/operation port 'CODING)
       (error:bad-range-argument port 'PORT/CODING))
   port))

(define (port/set-coding port name)
  ((or (port/operation port 'SET-CODING)
       (error:bad-range-argument port 'PORT/SET-CODING))
   port name))

(define (port/known-coding? port name)
  ((or (port/operation port 'KNOWN-CODING?)
       (error:bad-range-argument port 'PORT/KNOWN-CODING?))
   port name))

(define (port/known-codings port)
  ((or (port/operation port 'KNOWN-CODINGS)
       (error:bad-range-argument port 'PORT/KNOWN-CODINGS))
   port))

(define (port/line-ending port)
  ((or (port/operation port 'LINE-ENDING)
       (error:bad-range-argument port 'PORT/LINE-ENDING))
   port))

(define (port/set-line-ending port name)
  ((or (port/operation port 'SET-LINE-ENDING)
       (error:bad-range-argument port 'PORT/SET-LINE-ENDING))
   port name))

(define (port/known-line-ending? port name)
  ((or (port/operation port 'KNOWN-LINE-ENDING?)
       (error:bad-range-argument port 'PORT/KNOWN-LINE-ENDING?))
   port name))

(define (port/known-line-endings port)
  ((or (port/operation port 'KNOWN-LINE-ENDINGS)
       (error:bad-range-argument port 'PORT/KNOWN-LINE-ENDINGS))
   port))

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
			  (if (port/open? port)
			      (begin
				(set! outside-mode (read-mode port))
				(write-mode port mode))))
			thunk
			(lambda ()
			  (if (port/open? port)
			      (begin
				(set! mode (read-mode port))
				(write-mode port outside-mode))))))
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
  (set! *current-input-port*
	(guarantee-input-port port 'SET-CURRENT-INPUT-PORT!))
  unspecific)

(define (with-input-from-port port thunk)
  (fluid-let ((*current-input-port*
	       (guarantee-input-port port 'WITH-INPUT-FROM-PORT)))
    (thunk)))

(define (current-output-port)
  (or *current-output-port* (nearest-cmdl/port)))

(define (set-current-output-port! port)
  (set! *current-output-port*
	(guarantee-output-port port 'SET-CURRENT-OUTPUT-PORT!))
  unspecific)

(define (with-output-to-port port thunk)
  (fluid-let ((*current-output-port*
	       (guarantee-output-port port 'WITH-OUTPUT-TO-PORT)))
    (thunk)))

(define (notification-output-port)
  (or *notification-output-port* (nearest-cmdl/port)))

(define (set-notification-output-port! port)
  (set! *notification-output-port*
	(guarantee-output-port port 'SET-NOTIFICATION-OUTPUT-PORT!))
  unspecific)

(define (with-notification-output-port port thunk)
  (fluid-let ((*notification-output-port*
	       (guarantee-output-port port 'WITH-NOTIFICATION-OUTPUT-PORT)))
    (thunk)))

(define (trace-output-port)
  (or *trace-output-port* (nearest-cmdl/port)))

(define (set-trace-output-port! port)
  (set! *trace-output-port*
	(guarantee-output-port port 'SET-TRACE-OUTPUT-PORT!))
  unspecific)

(define (with-trace-output-port port thunk)
  (fluid-let ((*trace-output-port*
	       (guarantee-output-port port 'WITH-TRACE-OUTPUT-PORT)))
    (thunk)))

(define (interaction-i/o-port)
  (or *interaction-i/o-port* (nearest-cmdl/port)))

(define (set-interaction-i/o-port! port)
  (set! *interaction-i/o-port*
	(guarantee-i/o-port port 'SET-INTERACTION-I/O-PORT!))
  unspecific)

(define (with-interaction-i/o-port port thunk)
  (fluid-let ((*interaction-i/o-port*
	       (guarantee-i/o-port port 'WITH-INTERACTION-I/O-PORT)))
    (thunk)))

(define standard-port-accessors
  (list (cons current-input-port set-current-input-port!)
	(cons current-output-port set-current-output-port!)
	(cons notification-output-port set-notification-output-port!)
	(cons trace-output-port set-trace-output-port!)
	(cons interaction-i/o-port set-interaction-i/o-port!)))