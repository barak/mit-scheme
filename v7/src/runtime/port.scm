#| -*-Scheme-*-

$Id: port.scm,v 1.33 2004/05/26 15:20:09 cph Exp $

Copyright 1991,1992,1993,1994,1997,1999 Massachusetts Institute of Technology
Copyright 2001,2002,2003,2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; I/O Ports
;;; package: (runtime port)

(declare (usual-integrations))

;;;; Port type

(define-structure (port-type (type-descriptor <port-type>)
			     (conc-name port-type/)
			     (constructor %make-port-type))
  standard-operations
  custom-operations
  ;; input operations:
  (char-ready? #f read-only #t)
  (read-char #f read-only #t)
  (unread-char #f read-only #t)
  (peek-char #f read-only #t)
  (discard-char #f read-only #t)
  (read-substring #f read-only #t)
  (read-wide-substring #f read-only #t)
  (read-external-substring #f read-only #t)
  ;; output operations:
  (write-char #f read-only #t)
  (write-substring #f read-only #t)
  (write-wide-substring #f read-only #t)
  (write-external-substring #f read-only #t)
  (fresh-line #f read-only #t)
  (flush-output #f read-only #t)
  (discretionary-flush-output #f read-only #t)
  ;; transcript operations:
  (get-transcript-port #f read-only #t)
  (set-transcript-port #f read-only #t))

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

(define (port-type/operation type name)
  (guarantee-port-type type 'PORT-TYPE/OPERATION)
  (let ((entry
	 (or (assq name (port-type/custom-operations type))
	     (assq name (port-type/standard-operations type)))))
    (and entry
	 (cdr entry))))

;;;; Constructors

(define (make-port-type operations type)
  (if (not (list-of-type? operations
	     (lambda (elt)
	       (and (pair? elt)
		    (symbol? (car elt))
		    (pair? (cdr elt))
		    (procedure? (cadr elt))
		    (null? (cddr elt))))))
      (error:wrong-type-argument operations "operations list" 'MAKE-PORT-TYPE))
  (receive (standard-operations custom-operations)
      (parse-operations-list operations type)
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
      (%make-port-type standard-operations
		       custom-operations
		       (op 'CHAR-READY?)
		       (op 'READ-CHAR)
		       (op 'UNREAD-CHAR)
		       (op 'PEEK-CHAR)
		       (op 'DISCARD-CHAR)
		       (op 'READ-SUBSTRING)
		       (op 'READ-WIDE-SUBSTRING)
		       (op 'READ-EXTERNAL-SUBSTRING)
		       (op 'WRITE-CHAR)
		       (op 'WRITE-SUBSTRING)
		       (op 'WRITE-WIDE-SUBSTRING)
		       (op 'WRITE-EXTERNAL-SUBSTRING)
		       (op 'FRESH-LINE)
		       (op 'FLUSH-OUTPUT)
		       (op 'DISCRETIONARY-FLUSH-OUTPUT)
		       port/transcript
		       set-port/transcript!))))

(define (parse-operations-list operations type)
  (parse-operations-list-1
   (if type
       (append operations
	       (delete-matching-items (port-type/operations type)
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
    READ-CHAR
    READ-SUBSTRING
    READ-WIDE-SUBSTRING
    READ-EXTERNAL-SUBSTRING))

(define standard-output-operation-names
  '(WRITE-CHAR
    WRITE-SUBSTRING
    WRITE-WIDE-SUBSTRING
    WRITE-EXTERNAL-SUBSTRING
    FLUSH-OUTPUT
    DISCRETIONARY-FLUSH-OUTPUT))

;;;; Default input operations

(define (provide-default-input-operations op)
  (let ((char-ready? (or (op 'CHAR-READY?) (lambda (port) port #t)))
	(read-char (op 'READ-CHAR)))
    (let ((read-substring
	   (or (op 'READ-SUBSTRING)
	       (lambda (port string start end)
		 (let ((char (read-char port)))
		   (cond ((not char) #f)
			 ((eof-object? char) 0)
			 (else
			  (guarantee-8-bit-char char)
			  (string-set! string start char)
			  (let loop ((index (fix:+ start 1)))
			    (if (and (fix:< index end)
				     (char-ready? port))
				(let ((char (read-char port)))
				  (cond ((or (not char)
					     (eof-object? char))
					 (fix:- index start))
					(else
					 (guarantee-8-bit-char char)
					 (string-set! string index char)
					 (loop (fix:+ index 1)))))
				(fix:- index start)))))))))
	  (read-wide-substring
	   (or (op 'READ-WIDE-SUBSTRING)
	       (lambda (port string start end)
		 (let ((char (read-char port)))
		   (cond ((not char) #f)
			 ((eof-object? char) 0)
			 (else
			  (wide-string-set! string start char)
			  (let loop ((index (fix:+ start 1)))
			    (if (and (fix:< index end)
				     (char-ready? port))
				(let ((char (read-char port)))
				  (if (or (not char) (eof-object? char))
				      (fix:- index start)
				      (begin
					(wide-string-set! string
							  index
							  char)
					(loop (fix:+ index 1)))))
				(fix:- index start))))))))))
      (let ((read-external-substring
	     (or (op 'READ-EXTERNAL-SUBSTRING)
		 (lambda (port string start end)
		   (let ((l (min (- end start) #x1000)))
		     (let ((bounce (make-string l)))
		       (let ((n (read-substring port bounce 0 l)))
			 (if (and n (fix:> n 0))
			     (xsubstring-move! bounce 0 n string start))
			 n)))))))
	(lambda (name)
	  (case name
	    ((CHAR-READY?) char-ready?)
	    ((READ-CHAR) read-char)
	    ((READ-SUBSTRING) read-substring)
	    ((READ-WIDE-SUBSTRING) read-wide-substring)
	    ((READ-EXTERNAL-SUBSTRING) read-external-substring)
	    (else (op name))))))))

;;;; Default output operations

(define (provide-default-output-operations op)
  (let ((write-char (op 'WRITE-CHAR))
	(no-flush (lambda (port) port unspecific)))
    (let ((write-substring
	   (or (op 'WRITE-SUBSTRING)
	       (lambda (port string start end)
		 (let loop ((i start))
		   (if (fix:< i end)
		       (let ((n (write-char port (string-ref string i))))
			 (cond ((not n)
				(and (fix:> i start)
				     (fix:- i start)))
			       ((fix:> n 0) (loop (fix:+ i 1)))
			       (else (fix:- i start))))
		       (fix:- i start))))))
	  (write-wide-substring
	   (or (op 'WRITE-WIDE-SUBSTRING)
	       (lambda (port string start end)
		 (let loop ((i start))
		   (if (fix:< i end)
		       (let ((n
			      (write-char port
					  (wide-string-ref string i))))
			 (cond ((not n)
				(and (fix:> i start)
				     (fix:- i start)))
			       ((fix:> n 0) (loop (fix:+ i 1)))
			       (else (fix:- i start))))
		       (fix:- i start))))))
	  (flush-output (or (op 'FLUSH-OUTPUT) no-flush))
	  (discretionary-flush-output
	   (or (op 'DISCRETIONARY-FLUSH-OUTPUT) no-flush)))
      (let ((write-external-substring
	     (or (op 'WRITE-EXTERNAL-SUBSTRING)
		 (lambda (port string start end)
		   (let ((bounce (make-string #x1000)))
		     (let loop ((i start))
		       (if (< i end)
			   (let ((m (min (- end i) #x1000)))
			     (xsubstring-move! string i (+ i m) bounce 0)
			     (let ((n (write-substring port bounce 0 m)))
			       (cond ((not n) (and (> i start) (- i start)))
				     ((fix:> n 0) (loop (+ i n)))
				     (else (- i start)))))
			   (- end start))))))))
	(lambda (name)
	  (case name
	    ((WRITE-CHAR) write-char)
	    ((WRITE-SUBSTRING) write-substring)
	    ((WRITE-WIDE-SUBSTRING) write-wide-substring)
	    ((WRITE-EXTERNAL-SUBSTRING) write-external-substring)
	    ((FLUSH-OUTPUT) flush-output)
	    ((DISCRETIONARY-FLUSH-OUTPUT) discretionary-flush-output)
	    (else (op name))))))))

;;;; Input features

(define (provide-input-features op)
  (let ((char-ready?
	 (let ((defer (op 'CHAR-READY?)))
	   (lambda (port)
	     (if (port/unread port)
		 #t
		 (defer port)))))
	(read-char
	 (let ((defer (op 'READ-CHAR)))
	   (lambda (port)
	     (let ((char (port/unread port)))
	       (if char
		   (begin
		     (set-port/unread! port #f)
		     char)
		   (let ((char (defer port)))
		     (if (and (port/transcript port) (char? char))
			 (write-char char (port/transcript port)))
		     char))))))
	(unread-char
	 (lambda (port char)
	   (if (port/unread port)
	       (error "Can't unread second character:" char port))
	   (set-port/unread! port char)
	   unspecific))
	(peek-char
	 (let ((defer (op 'READ-CHAR)))
	   (lambda (port)
	     (or (port/unread port)
		 (let ((char (defer port)))
		   (if (char? char)
		       (set-port/unread! port char))
		   char)))))
	(discard-char
	 (lambda (port)
	   (if (not (port/unread port))
	       (error "No character to discard:" port))
	   (set-port/unread! port #f)
	   unspecific))
	(read-substring
	 (let ((defer (op 'READ-SUBSTRING)))
	   (lambda (port string start end)
	     (if (port/unread port)
		 (begin
		   (guarantee-8-bit-char (port/unread port))
		   (string-set! string start (port/unread port))
		   (set-port/unread! port #f)
		   1)
		 (let ((n (defer port string start end)))
		   (if (and n (fix:> n 0) (port/transcript port))
		       (write-substring string start (fix:+ start n)
					(port/transcript port)))
		   n)))))
	(read-wide-substring
	 (let ((defer (op 'READ-WIDE-SUBSTRING)))
	   (lambda (port string start end)
	     (if (port/unread port)
		 (begin
		   (wide-string-set! string start (port/unread port))
		   (set-port/unread! port #f)
		   1)
		 (let ((n (defer port string start end)))
		   (if (and n (fix:> n 0) (port/transcript port))
		       (write-substring string start (fix:+ start n)
					(port/transcript port)))
		   n)))))
	(read-external-substring
	 (let ((defer (op 'READ-EXTERNAL-SUBSTRING)))
	   (lambda (port string start end)
	     (if (port/unread port)
		 (begin
		   (guarantee-8-bit-char (port/unread port))
		   (xsubstring-move! (make-string 1 (port/unread port)) 0 1
				     string start)
		   (set-port/unread! port #f)
		   1)
		 (let ((n (defer port string start end)))
		   (if (and n (> n 0) (port/transcript port))
		       (write-substring string start (+ start n)
					(port/transcript port)))
		   n))))))
    (lambda (name)
      (case name
	((CHAR-READY?) char-ready?)
	((READ-CHAR) read-char)
	((UNREAD-CHAR) unread-char)
	((PEEK-CHAR) peek-char)
	((DISCARD-CHAR) discard-char)
	((READ-SUBSTRING) read-substring)
	((READ-WIDE-SUBSTRING) read-wide-substring)
	((READ-EXTERNAL-SUBSTRING) read-external-substring)
	(else (op name))))))

;;;; Output features

(define (provide-output-features op)
  (let ((write-char
	 (let ((defer (op 'WRITE-CHAR)))
	   (lambda (port char)
	     (let ((n (defer port char)))
	       (if (and n (fix:> n 0))
		   (begin
		     (set-port/previous! port char)
		     (if (port/transcript port)
			 (write-char char (port/transcript port)))))
	       n))))
	(write-substring
	 (let ((defer (op 'WRITE-SUBSTRING)))
	   (lambda (port string start end)
	     (let ((n (defer port string start end)))
	       (if (and n (fix:> n 0))
		   (begin
		     (set-port/previous!
		      port
		      (string-ref string (fix:+ start (fix:- n 1))))
		     (if (and (port/transcript port))
			 (write-substring string start (fix:+ start n)
					  (port/transcript port)))))
	       n))))
	(write-wide-substring
	 (let ((defer (op 'WRITE-WIDE-SUBSTRING)))
	   (lambda (port string start end)
	     (let ((n (defer port string start end)))
	       (if (and n (fix:> n 0))
		   (begin
		     (set-port/previous!
		      port
		      (string-ref string (fix:+ start (fix:- n 1))))
		     (if (and (port/transcript port))
			 (write-substring string start (fix:+ start n)
					  (port/transcript port)))))
	       n))))
	(write-external-substring
	 (let ((defer (op 'WRITE-EXTERNAL-SUBSTRING)))
	   (lambda (port string start end)
	     (let ((n (defer port string start end)))
	       (if (and n (> n 0))
		   (let ((i (+ start n))
			 (bounce (make-string 1)))
		     (xsubstring-move! string (- i 1) i bounce 0)
		     (set-port/previous! port (string-ref bounce 0))
		     (if (port/transcript port)
			 (write-substring string start i
					  (port/transcript port)))))
	       n))))
	(flush-output
	 (let ((defer (op 'FLUSH-OUTPUT)))
	   (lambda (port)
	     (defer port)
	     (if (port/transcript port)
		 (flush-output (port/transcript port))))))
	(discretionary-flush-output
	 (let ((defer (op 'DISCRETIONARY-FLUSH-OUTPUT)))
	   (lambda (port)
	     (defer port)
	     (if (port/transcript port)
		 (output-port/discretionary-flush (port/transcript port)))))))
    (lambda (name)
      (case name
	((WRITE-CHAR) write-char)
	((WRITE-SUBSTRING) write-substring)
	((WRITE-WIDE-SUBSTRING) write-wide-substring)
	((WRITE-EXTERNAL-SUBSTRING) write-external-substring)
	((FRESH-LINE)
	 (lambda (port)
	   (if (and (port/previous port)
		    (not (char=? (port/previous port) #\newline)))
	       (write-char port #\newline)
	       0)))
	((FLUSH-OUTPUT) flush-output)
	((DISCRETIONARY-FLUSH-OUTPUT) discretionary-flush-output)
	(else (op name))))))

;;;; Port object

(define-structure (port (type-descriptor <port>)
			(conc-name port/)
			(constructor %make-port (%type %state)))
  (%type #f read-only #t)
  %state
  (%thread-mutex (make-thread-mutex))
  (unread #f)
  (previous #f)
  (transcript #f))

(define (make-port type state)
  (guarantee-port-type type 'MAKE-PORT)
  (%make-port type state))

(define (port/type port)
  (guarantee-port port 'PORT/TYPE)
  (port/%type port))

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

(define (port/operation port name)
  (port-type/operation (port/type port) name))

(let-syntax
    ((define-port-operation
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((name (cadr form)))
	    `(DEFINE (,(symbol-append 'PORT/OPERATION/ name) PORT)
	       (,(close-syntax (symbol-append 'PORT-TYPE/ name) environment)
		(PORT/TYPE PORT))))))))
  (define-port-operation char-ready?)
  (define-port-operation read-char)
  (define-port-operation unread-char)
  (define-port-operation peek-char)
  (define-port-operation discard-char)
  (define-port-operation read-substring)
  (define-port-operation read-wide-substring)
  (define-port-operation read-external-substring)
  (define-port-operation write-char)
  (define-port-operation write-substring)
  (define-port-operation write-wide-substring)
  (define-port-operation write-external-substring)
  (define-port-operation fresh-line)
  (define-port-operation flush-output)
  (define-port-operation discretionary-flush-output)
  (define-port-operation get-transcript-port)
  (define-port-operation set-transcript-port))

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

(define (port/input-channel port)
  (let ((operation (port/operation port 'INPUT-CHANNEL)))
    (and operation
	 (operation port))))

(define (port/output-channel port)
  (let ((operation (port/operation port 'OUTPUT-CHANNEL)))
    (and operation
	 (operation port))))

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

(define-integrable (guarantee-8-bit-char char)
  (if (fix:>= (char->integer char) #x100)
      (error:not-8-bit-char char)))

(define (port/supports-coding? port)
  (let ((operation (port/operation port 'SUPPORTS-CODING?)))
    (if operation
	(operation port)
	#f)))

(define (port/coding port)
  (let ((operation (port/operation port 'CODING)))
    (if operation
	(operation port)
	'TEXT)))

(define (port/set-coding port name)
  (let ((operation (port/operation port 'SET-CODING)))
    (if operation
	(operation port name))))

(define (port/known-coding? port name)
  (let ((operation (port/operation port 'KNOWN-CODING?)))
    (if operation
	(operation port name)
	(memq name default-codings))))

(define (port/known-codings port)
  (let ((operation (port/operation port 'KNOWN-CODINGS)))
    (if operation
	(operation port)
	(list-copy default-codings))))

(define default-codings
  '(TEXT BINARY))

(define (port/line-ending port)
  (let ((operation (port/operation port 'LINE-ENDING)))
    (if operation
	(operation port)
	'TEXT)))

(define (port/set-line-ending port name)
  (let ((operation (port/operation port 'SET-LINE-ENDING)))
    (if operation
	(operation port name))))

(define (port/known-line-ending? port name)
  (let ((operation (port/operation port 'KNOWN-LINE-ENDING?)))
    (if operation
	(operation port name)
	(memq name default-line-endings))))

(define (port/known-line-endings port)
  (let ((operation (port/operation port 'KNOWN-LINE-ENDINGS)))
    (if operation
	(operation port)
	(list-copy default-line-endings))))

(define default-line-endings
  '(TEXT BINARY NEWLINE))

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