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

;;;; I/O Ports
;;; package: (runtime port)

(declare (usual-integrations))

;;;; Textual port types

(define-record-type <textual-port-type>
    (%make-textual-port-type operations
			     char-ready?
			     read-char
			     unread-char
			     peek-char
			     read-substring
			     write-char
			     write-substring
			     fresh-line
			     line-start?
			     flush-output
			     discretionary-flush-output)
    textual-port-type?
  (operations port-type-operations)
  ;; input operations:
  (char-ready? port-type-operation:char-ready?)
  (read-char port-type-operation:read-char)
  (unread-char port-type-operation:unread-char)
  (peek-char port-type-operation:peek-char)
  (read-substring port-type-operation:read-substring)
  ;; output operations:
  (write-char port-type-operation:write-char)
  (write-substring port-type-operation:write-substring)
  (fresh-line port-type-operation:fresh-line)
  (line-start? port-type-operation:line-start?)
  (flush-output port-type-operation:flush-output)
  (discretionary-flush-output port-type-operation:discretionary-flush-output))

(define-print-method textual-port-type?
  (standard-print-method
   (lambda (type)
     (if (port-type-supports-input? type)
	(if (port-type-supports-output? type)
	    'textual-i/o-port-type
	    'textual-input-port-type)
	(if (port-type-supports-output? type)
	    'textual-output-port-type
	    'textual-port-type)))))

(define (port-type-supports-input? type)
  (port-type-operation:read-char type))

(define (port-type-supports-output? type)
  (port-type-operation:write-char type))

(define (textual-input-port-type? object)
  (and (textual-port-type? object)
       (port-type-supports-input? object)
       #t))

(define (textual-output-port-type? object)
  (and (textual-port-type? object)
       (port-type-supports-output? object)
       #t))

(define (textual-i/o-port-type? object)
  (and (textual-port-type? object)
       (port-type-supports-input? object)
       (port-type-supports-output? object)
       #t))

(define (textual-port-type-operation-names type)
  (map car (port-type-operations type)))

(define (textual-port-type-operations type)
  (map (lambda (entry)
	 (list (car entry) (cdr entry)))
       (port-type-operations type)))

(define (textual-port-type-operation type name)
  (let ((entry (assq name (port-type-operations type))))
    (and entry
	 (cdr entry))))

;;;; Constructors

(define (make-textual-port-type operations parent-type)
  (guarantee-list-of textual-port-type-operation? operations
		     'make-textual-port-type)
  (if parent-type
      (guarantee textual-port-type? parent-type 'make-textual-port-type))
  (receive (standard-operations custom-operations)
      (parse-operations-list operations parent-type)
    (let ((op
	   (let ((input? (assq 'read-char standard-operations))
		 (output? (assq 'write-char standard-operations))
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
      (%make-textual-port-type (append custom-operations standard-operations)
			       (op 'char-ready?)
			       (op 'read-char)
			       (op 'unread-char)
			       (op 'peek-char)
			       (op 'read-substring)
			       (op 'write-char)
			       (op 'write-substring)
			       (op 'fresh-line)
			       (op 'line-start?)
			       (op 'flush-output)
			       (op 'discretionary-flush-output)))))

(define (textual-port-type-operation? object)
  (and (pair? object)
       (symbol? (car object))
       (pair? (cdr object))
       (procedure? (cadr object))
       (null? (cddr object))))
(register-predicate! textual-port-type-operation? 'port-type-operation)

(define (parse-operations-list operations parent-type)
  (parse-operations-list-1
   (if parent-type
       (append operations
	       (remove (let ((excluded
			      (append
			       (if (assq 'read-char operations)
				   standard-input-operation-names
				   '())
			       (if (assq 'write-char operations)
				   standard-output-operation-names
				   '()))))
			 (lambda (p)
			   (or (assq (car p) operations)
			       (memq (car p) excluded))))
		       (textual-port-type-operations parent-type)))
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
  '(char-ready?
    peek-char
    read-char
    read-substring
    unread-char))

(define standard-output-operation-names
  '(write-char
    write-substring
    flush-output
    discretionary-flush-output))

;;;; Default I/O operations

(define (required-operation op name)
  (if (not (op name))
      (error "Missing required operation:" name)))

(define (provide-default-input-operations op)
  (required-operation op 'read-char)
  (if (and (or (op 'unread-char)
	       (op 'peek-char))
	   (not (and (op 'unread-char)
		     (op 'peek-char))))
      (error "Must provide both UNREAD-CHAR and PEEK-CHAR operations."))
  (let ((char-ready?
	 (or (op 'char-ready?)
	     (lambda (port) port #t)))
	(read-substring
	 (or (op 'read-substring)
	     generic-port-operation:read-substring)))
    (lambda (name)
      (case name
	((char-ready?) char-ready?)
	((read-substring) read-substring)
	(else (op name))))))

(define (generic-port-operation:read-substring port string start end)
  (let ((char-ready? (textual-port-operation/char-ready? port))
	(read-char (textual-port-operation/read-char port)))
    (let ((char (read-char port)))
      (cond ((not char) #f)
	    ((eof-object? char) 0)
	    (else
	     (string-set! string start char)
	     (let loop ((index (+ start 1)))
	       (if (and (< index end)
			(char-ready? port))
		   (let ((char (read-char port)))
		     (if (or (not char) (eof-object? char))
			 (- index start)
			 (begin
			   (string-set! string index char)
			   (loop (+ index 1)))))
		   (- index start))))))))

(define (provide-default-output-operations op)
  (required-operation op 'write-char)
  (let ((write-substring
	 (or (op 'write-substring)
	     generic-port-operation:write-substring))
	(flush-output
	 (or (op 'flush-output)
	     no-flush))
	(discretionary-flush-output
	 (or (op 'discretionary-flush-output)
	     no-flush)))
    (lambda (name)
      (case name
	((write-substring) write-substring)
	((flush-output) flush-output)
	((discretionary-flush-output) discretionary-flush-output)
	(else (op name))))))

(define (no-flush port)
  port
  unspecific)

(define (generic-port-operation:write-substring port string start end)
  (let ((write-char (textual-port-operation/write-char port)))
    (let loop ((i start))
      (if (< i end)
	  (let ((n (write-char port (string-ref string i))))
	    (cond ((not n) (and (> i start) (- i start)))
		  ((> n 0) (loop (+ i 1)))
		  (else (- i start))))
	  (- i start)))))

;;;; Input features

(define (provide-input-features op)
  (let ((read-char
	 (let ((defer (op 'read-char)))
	   (lambda (port)
	     (let ((char (defer port)))
	       (transcribe-input-char char port)
	       (set-textual-port-unread?! port #f)
	       char))))
	(unread-char
	 (let ((defer (op 'unread-char)))
	   (and defer
		(lambda (port char)
		  (defer port char)
		  (set-textual-port-unread?! port #t)))))
	(peek-char
	 (let ((defer (op 'peek-char)))
	   (and defer
		(lambda (port)
		  (let ((char (defer port)))
		    (transcribe-input-char char port)
		    (set-textual-port-unread?! port #t)
		    char)))))
	(read-substring
	 (let ((defer (op 'read-substring)))
	   (lambda (port string start end)
	     (let ((n (defer port string start end)))
	       (transcribe-input-substring string start n port)
	       (set-textual-port-unread?! port #f)
	       n)))))
    (lambda (name)
      (case name
	((read-char) read-char)
	((unread-char) unread-char)
	((peek-char) peek-char)
	((read-substring) read-substring)
	(else (op name))))))

(define (transcribe-input-char char port)
  (if (and (char? char)
	   (not (textual-port-unread? port)))
      (transcribe-char char port)))

(define (transcribe-input-substring string start n port)
  (if (and n (> n 0))
      (transcribe-substring string
			    (if (textual-port-unread? port) (+ start 1) start)
			    (+ start n)
			    port)))

;;;; Output features

(define (provide-output-features op)
  (let ((write-char
	 (let ((defer (op 'write-char)))
	   (lambda (port char)
	     (let ((n (defer port char)))
	       (if (and n (fix:> n 0))
		   (begin
		     (set-textual-port-previous! port char)
		     (transcribe-char char port)))
	       n))))
	(write-substring
	 (let ((defer (op 'write-substring)))
	   (lambda (port string start end)
	     (let ((n (defer port string start end)))
	       (if (and n (> n 0))
		   (let ((end (+ start n)))
		     (set-textual-port-previous! port
						 (string-ref string (- end 1)))
		     (transcribe-substring string start end port)))
	       n))))
	(flush-output
	 (let ((defer (op 'flush-output)))
	   (lambda (port)
	     (defer port)
	     (flush-transcript port))))
	(discretionary-flush-output
	 (let ((defer (op 'discretionary-flush-output)))
	   (lambda (port)
	     (defer port)
	     (discretionary-flush-transcript port))))
	(line-start?
	 (lambda (port)
	   (if (textual-port-previous port)
	       (char=? (textual-port-previous port) #\newline)
	       'unknown))))
    (let ((fresh-line
	   (lambda (port)
	     (if (and (textual-port-previous port)
		      (not (char=? (textual-port-previous port) #\newline)))
		 (write-char port #\newline)
		 0))))
      (lambda (name)
	(case name
	  ((write-char) write-char)
	  ((write-substring) write-substring)
	  ((fresh-line) fresh-line)
	  ((line-start?) line-start?)
	  ((flush-output) flush-output)
	  ((discretionary-flush-output) discretionary-flush-output)
	  (else (op name)))))))

;;;; Textual ports

(define-record-type <textual-port>
    (%make-textual-port thread-mutex type state unread? previous transcript
			metadata)
    textual-port?
  (thread-mutex textual-port-thread-mutex)
  (type textual-port-type set-textual-port-type!)
  (state textual-port-state set-textual-port-state!)
  (unread? textual-port-unread? set-textual-port-unread?!)
  (previous textual-port-previous set-textual-port-previous!)
  (transcript textual-port-transcript set-textual-port-transcript!)
  (metadata textual-port-metadata))

(define (make-textual-port type state #!optional caller)
  (guarantee textual-port-type? type caller)
  (%make-textual-port (make-thread-mutex) type state #f #f #f
		      (make-alist-metadata-table)))

(define (textual-input-port? object)
  (and (textual-port? object)
       (port-type-supports-input? (textual-port-type object))
       #t))
(register-predicate! textual-input-port? 'textual-input-port
		     '<= textual-port?)

(define (textual-output-port? object)
  (and (textual-port? object)
       (port-type-supports-output? (textual-port-type object))
       #t))
(register-predicate! textual-output-port? 'textual-output-port
		     '<= textual-port?)

(define (textual-i/o-port? object)
  (and (textual-port? object)
       (let ((type (textual-port-type object)))
	 (and (port-type-supports-input? type)
	      (port-type-supports-output? type)
	      #t))))
(register-predicate! textual-i/o-port? 'textual-i/o-port
		     '<= textual-port?)

(define-print-method textual-port?
  (bracketed-print-method
   (lambda (port)
     (cond ((textual-i/o-port? port) 'textual-i/o-port)
	   ((textual-input-port? port) 'textual-input-port)
	   ((textual-output-port? port) 'textual-output-port)
	   (else 'textual-port)))
   (lambda (port output-port)
     (cond ((textual-port-operation port 'write-self)
	    => (lambda (operation)
		 (operation port output-port)))))))

(define (close-textual-port port)
  (let ((close (textual-port-operation port 'close)))
    (if close
	(close port)
	(begin
	  (close-textual-output-port port)
	  (close-textual-input-port port)))))

(define (close-textual-input-port port)
  (let ((close-input (textual-port-operation port 'close-input)))
    (if close-input
	(close-input port))))

(define (close-textual-output-port port)
  (let ((close-output (textual-port-operation port 'close-output)))
    (if close-output
	(close-output port))))

(define (textual-port-open? port)
  (let ((open? (textual-port-operation port 'open?)))
    (if open?
	(open? port)
	(and (if (textual-input-port? port)
		 (textual-input-port-open? port)
		 #t)
	     (if (textual-output-port? port)
		 (textual-output-port-open? port)
		 #t)))))

(define (textual-input-port-open? port)
  (let ((open? (textual-port-operation port 'input-open?)))
    (if open?
	(open? port)
	#t)))

(define (textual-output-port-open? port)
  (let ((open? (textual-port-operation port 'output-open?)))
    (if open?
	(open? port)
	#t)))

(define (textual-input-port-channel port)
  (let ((operation (textual-port-operation port 'input-channel)))
    (and operation
	 (operation port))))

(define (textual-output-port-channel port)
  (let ((operation (textual-port-operation port 'output-channel)))
    (and operation
	 (operation port))))

(define (textual-port-operation-names port)
  (textual-port-type-operation-names (textual-port-type port)))

(define (textual-port-operation port name)
  (textual-port-type-operation (textual-port-type port) name))

(define-syntax define-port-operation
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form)))
       `(define (,(symbol 'textual-port-operation/ name) port)
	  (,(close-syntax (symbol 'port-type-operation: name) environment)
	   (textual-port-type port)))))))

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

(define (port-property port name #!optional default-value)
  (guarantee symbol? name 'port-property)
  ((port-metadata port) 'get name default-value))

(define (set-port-property! port name value)
  (guarantee symbol? name 'set-port-property!)
  ((port-metadata port) 'put! name value))

(define (intern-port-property! port name get-value)
  (guarantee symbol? name 'intern-port-property!)
  ((port-metadata port) 'intern! name get-value))

(define (remove-port-property! port name)
  (guarantee symbol? name 'remove-port-property!)
  ((port-metadata port) 'delete! name))

(define (port-properties port)
  (alist-copy ((port-metadata port) 'get-alist)))

(define (transcribe-char char port)
  (let ((tport (textual-port-transcript port)))
    (if tport
	(write-char char tport))))

(define (transcribe-substring string start end port)
  (let ((tport (textual-port-transcript port)))
    (if tport
	(write-string string tport start end))))

(define (flush-transcript port)
  (let ((tport (textual-port-transcript port)))
    (if tport
	(flush-output-port tport))))

(define (discretionary-flush-transcript port)
  (let ((tport (textual-port-transcript port)))
    (if tport
	(output-port/discretionary-flush tport))))

(define (textual-port-char-set port)
  (let ((operation (textual-port-operation port 'char-set)))
    (if operation
	(operation port)
	char-set:iso-8859-1)))

(define (port/supports-coding? port)
  (let ((operation (textual-port-operation port 'supports-coding?)))
    (if operation
	(operation port)
	#f)))

(define (port/coding port)
  ((or (textual-port-operation port 'coding)
       (error:bad-range-argument port 'port/coding))
   port))

(define (port/set-coding port name)
  ((or (textual-port-operation port 'set-coding)
       (error:bad-range-argument port 'port/set-coding))
   port name))

(define (port/known-coding? port name)
  ((or (textual-port-operation port 'known-coding?)
       (error:bad-range-argument port 'port/known-coding?))
   port name))

(define (port/known-codings port)
  ((or (textual-port-operation port 'known-codings)
       (error:bad-range-argument port 'port/known-codings))
   port))

(define (port/line-ending port)
  ((or (textual-port-operation port 'line-ending)
       (error:bad-range-argument port 'port/line-ending))
   port))

(define (port/set-line-ending port name)
  ((or (textual-port-operation port 'set-line-ending)
       (error:bad-range-argument port 'port/set-line-ending))
   port name))

(define (port/known-line-ending? port name)
  ((or (textual-port-operation port 'known-line-ending?)
       (error:bad-range-argument port 'port/known-line-ending?))
   port name))

(define (port/known-line-endings port)
  ((or (textual-port-operation port 'known-line-endings)
       (error:bad-range-argument port 'port/known-line-endings))
   port))

;;;; Generic ports

(define port?)
(define input-port?)
(define output-port?)
(define i/o-port?)
(add-boot-init!
 (lambda ()
   (set! port? (disjoin textual-port? binary-port?))
   (set! input-port? (disjoin textual-input-port? binary-input-port?))
   (set! output-port? (disjoin textual-output-port? binary-output-port?))
   (set! i/o-port? (disjoin textual-i/o-port? binary-i/o-port?))
   unspecific))

(define-guarantee port "port")
(define-guarantee input-port "input port")
(define-guarantee output-port "output port")
(define-guarantee i/o-port "I/O port")

(define (input-port-open? port)
  (cond ((binary-input-port? port) (binary-input-port-open? port))
	((textual-input-port? port) (textual-input-port-open? port))
	(else (error:not-a input-port? port 'input-port-open?))))

(define (output-port-open? port)
  (cond ((binary-output-port? port) (binary-output-port-open? port))
	((textual-output-port? port) (textual-output-port-open? port))
	(else (error:not-a output-port? port 'output-port-open?))))

(define (close-port port)
  (cond ((binary-port? port) (close-binary-port port))
	((textual-port? port) (close-textual-port port))
	(else (error:not-a port? port 'close-port))))

(define (close-input-port port)
  (cond ((binary-input-port? port) (close-binary-input-port port))
	((textual-input-port? port) (close-textual-input-port port))
	(else (error:not-a input-port? port 'close-input-port))))

(define (close-output-port port)
  (cond ((binary-output-port? port) (close-binary-output-port port))
	((textual-output-port? port) (close-textual-output-port port))
	(else (error:not-a output-port? port 'close-output-port))))

(define (input-port-channel port)
  (cond ((binary-input-port? port) (binary-input-port-channel port))
	((textual-input-port? port) (textual-input-port-channel port))
	(else (error:not-a input-port? port 'input-port-channel))))

(define (output-port-channel port)
  (cond ((binary-output-port? port) (binary-output-port-channel port))
	((textual-output-port? port) (textual-output-port-channel port))
	(else (error:not-a output-port? port 'output-port-channel))))

(define (port-metadata port)
  (cond ((binary-port? port) (binary-port-metadata port))
	((textual-port? port) (textual-port-metadata port))
	(else (error:not-a port? port 'port-metadata))))

(define (call-with-port port procedure)
  (let ((value (procedure port)))
    (close-port port)
    value))

;;;; Port modes

(define (input-port-blocking-mode port)
  (channel-blocking-mode (input-port-channel port)))

(define (set-input-port-blocking-mode! port mode)
  (guarantee blocking-mode? mode 'set-input-port-blocking-mode!)
  (set-channel-blocking-mode! (input-port-channel port) mode))

(define (with-input-port-blocking-mode port mode thunk)
  (guarantee blocking-mode? mode 'with-input-port-blocking-mode)
  (with-channel-blocking-mode (input-port-channel port) mode thunk))

(define (output-port-blocking-mode port)
  (channel-blocking-mode (output-port-channel port)))

(define (set-output-port-blocking-mode! port mode)
  (guarantee blocking-mode? mode 'set-output-port-blocking-mode!)
  (set-channel-blocking-mode! (output-port-channel port) mode))

(define (with-output-port-blocking-mode port mode thunk)
  (guarantee blocking-mode? mode 'with-output-port-blocking-mode)
  (with-channel-blocking-mode (output-port-channel port) mode thunk))

(define (input-port-terminal-mode port)
  (channel-terminal-mode (input-port-channel port)))

(define (set-input-port-terminal-mode! port mode)
  (guarantee terminal-mode? mode 'set-input-port-terminal-mode!)
  (set-channel-terminal-mode! (input-port-channel port) mode))

(define (with-input-port-terminal-mode port mode thunk)
  (guarantee terminal-mode? mode 'with-input-port-terminal-mode)
  (with-channel-terminal-mode (input-port-channel port) mode thunk))

(define (output-port-terminal-mode port)
  (channel-terminal-mode (output-port-channel port)))

(define (set-output-port-terminal-mode! port mode)
  (guarantee terminal-mode? mode 'set-output-port-terminal-mode!)
  (set-channel-terminal-mode! (output-port-channel port) mode))

(define (with-output-port-terminal-mode port mode thunk)
  (guarantee terminal-mode? mode 'with-output-port-terminal-mode)
  (with-channel-terminal-mode (output-port-channel port) mode thunk))

(define (blocking-mode? object)
  (or (eq? 'blocking object)
      (eq? 'nonblocking object)))
(register-predicate! blocking-mode? 'blocking-mode)

(define (channel-blocking-mode channel)
  (if channel
      (if (channel-blocking? channel) 'blocking 'nonblocking)
      #f))

(define (set-channel-blocking-mode! channel mode)
  (if channel
      (if (eq? 'blocking mode)
	  (channel-blocking channel)
	  (channel-nonblocking channel))))

(define (channel-mode-binder bind? get-mode set-mode!)
  (lambda (channel mode thunk)
    (if (bind? channel)
	(let ((outside-mode))
	  (dynamic-wind (lambda ()
			  (if (channel-open? channel)
			      (begin
				(set! outside-mode (get-mode channel))
				(set-mode! channel mode))))
			thunk
			(lambda ()
			  (if (channel-open? channel)
			      (begin
				(set! mode (get-mode channel))
				(set-mode! channel outside-mode))))))
	(thunk))))

(define with-channel-blocking-mode
  (channel-mode-binder (lambda (channel) channel)
		       channel-blocking-mode
		       set-channel-blocking-mode!))

(define (terminal-mode? object)
  (or (eq? 'cooked object)
      (eq? 'raw object)))
(register-predicate! terminal-mode? 'terminal-mode)

(define (channel-terminal-mode channel)
  (if (and channel (channel-type=terminal? channel))
      (if (terminal-cooked-input? channel) 'cooked 'raw)
      #f))

(define (set-channel-terminal-mode! channel mode)
  (if (and channel (channel-type=terminal? channel))
      (if (eq? 'cooked mode)
	  (terminal-cooked-input channel)
	  (terminal-raw-input channel))))

(define with-channel-terminal-mode
  (channel-mode-binder (lambda (channel)
			 (and channel (channel-type=terminal? channel)))
		       channel-terminal-mode
		       set-channel-terminal-mode!))

;;;; Standard Ports

(define-deferred current-input-port (make-port-parameter input-port?))
(define-deferred current-output-port (make-port-parameter output-port?))
(define-deferred current-error-port (make-port-parameter output-port?))
(define-deferred notification-output-port (make-port-parameter output-port?))
(define-deferred trace-output-port (make-port-parameter output-port?))
(define-deferred interaction-i/o-port (make-port-parameter i/o-port?))

(define (make-port-parameter predicate)
  (make-general-parameter #f
			  (lambda (port)
			    (if port (guarantee predicate port))
			    port)
			  default-parameter-merger
			  (lambda (port)
			    (or port (nearest-cmdl/port)))
			  default-parameter-setter))

(define (set-current-input-port! port)
  (current-input-port port))

(define (with-input-from-port port thunk)
  (parameterize ((current-input-port port))
    (thunk)))

(define (set-current-output-port! port)
  (current-output-port port))

(define (with-output-to-port port thunk)
  (parameterize ((current-output-port port))
    (thunk)))

(define (set-notification-output-port! port)
  (notification-output-port port))

(define (with-notification-output-port port thunk)
  (parameterize ((notification-output-port port))
    (thunk)))

(define (set-trace-output-port! port)
  (trace-output-port port))

(define (with-trace-output-port port thunk)
  (parameterize ((trace-output-port port))
    (thunk)))

(define (set-interaction-i/o-port! port)
  (interaction-i/o-port port))

(define (with-interaction-i/o-port port thunk)
  (parameterize ((interaction-i/o-port port))
    (thunk)))