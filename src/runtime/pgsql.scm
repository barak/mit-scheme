#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; PostgreSQL Interface
;;; package: (runtime postgresql)

(declare (usual-integrations))

(define-primitives
  (pq-clear 1)
  (pq-cmd-status 1)
  (pq-cmd-tuples 1)
  (pq-connect-db 2)
  (pq-connect-poll 1)
  (pq-connect-start 2)
  (pq-db 1)
  (pq-end-copy 1)
  (pq-error-message 1)
  (pq-escape-bytea 1)
  (pq-escape-string 2)
  (pq-exec 3)
  (pq-field-name 2)
  (pq-finish 1)
  (pq-get-is-null? 3)
  (pq-get-line 2)
  (pq-get-value 3)
  (pq-host 1)
  (pq-make-empty-pg-result 3)
  (pq-n-fields 1)
  (pq-n-tuples 1)
  (pq-options 1)
  (pq-pass 1)
  (pq-port 1)
  (pq-put-line 2)
  (pq-res-status 1)
  (pq-reset 1)
  (pq-reset-poll 1)
  (pq-reset-start 1)
  (pq-result-error-message 1)
  (pq-result-status 1)
  (pq-status 1)
  (pq-tty 1)
  (pq-unescape-bytea 1)
  (pq-user 1))

(define-syntax define-enum
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER * IDENTIFIER) (cdr form))
	 `(BEGIN
	    ,@(let loop ((names (cddr form)) (index 0))
		(if (pair? names)
		     `((DEFINE ,(car names) ,index)
		       ,@(loop (cdr names) (+ index 1)))
		     '()))
	    (DEFINE ,(cadr form) '#(,@(cddr form))))
	 (ill-formed-syntax form)))))

(define (index->name index enum)
  (guarantee-index-fixnum index 'INDEX->NAME)
  (if (not (fix:< index (vector-length enum)))
      (error:bad-range-argument index 'INDEX->NAME))
  (vector-ref enum index))

(define-enum connection-status
  PGSQL-CONNECTION-OK
  PGSQL-CONNECTION-BAD
  PGSQL-CONNECTION-STARTED
  PGSQL-CONNECTION-MADE
  PGSQL-CONNECTION-AWAITING-RESPONSE
  PGSQL-CONNECTION-AUTH-OK
  PGSQL-CONNECTION-SETENV)

(define-enum postgres-polling-status
  PGSQL-POLLING-FAILED
  PGSQL-POLLING-READING
  PGSQL-POLLING-WRITING
  PGSQL-POLLING-OK
  PGSQL-POLLING-ACTIVE)

(define-enum exec-status
  PGSQL-EMPTY-QUERY
  PGSQL-COMMAND-OK
  PGSQL-TUPLES-OK
  PGSQL-COPY-OUT
  PGSQL-COPY-IN
  PGSQL-BAD-RESPONSE
  PGSQL-NONFATAL-ERROR
  PGSQL-FATAL-ERROR)

(define pgsql-initialized? #f)
(define connections)
(define results)

(define-structure connection handle)
(define-structure result handle)

(define-syntax define-guarantee
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL EXPRESSION) (cdr form))
	 (let ((type (cadr form)))
	   (let ((type? (symbol-append type '?))
		 (guarantee-type (symbol-append 'GUARANTEE- type))
		 (error:not-type (symbol-append 'ERROR:NOT- type))
		 (guarantee-valid-type (symbol-append 'GUARANTEE-VALID- type))
		 (type-handle (symbol-append type '-HANDLE)))
	     `(BEGIN
		(DEFINE-INTEGRABLE (,guarantee-type OBJECT CALLER)
		  (IF (NOT (,type? OBJECT))
		      (,error:not-type OBJECT CALLER)))
		(DEFINE (,error:not-type OBJECT CALLER)
		  (ERROR:WRONG-TYPE-ARGUMENT OBJECT ,(caddr form) CALLER))
		(DEFINE-INTEGRABLE (,guarantee-valid-type OBJECT CALLER)
		  (IF (AND (,type? OBJECT) (,type-handle OBJECT))
		      (,type-handle OBJECT)
		      (,error:not-type OBJECT CALLER))))))
	 (ill-formed-syntax form)))))

(define-guarantee connection "PostgreSQL connection")
(define-guarantee result "PostgreSQL query result")

(define (pgsql-available?)
  (load-library-object-file "prpgsql" #f)
  (and (implemented-primitive-procedure? (ucode-primitive pq-connect-db 2))
       (begin
	 (if (not pgsql-initialized?)
	     (begin
	       (set! connections
		     (make-gc-finalizer pq-finish
					connection?
					connection-handle
					set-connection-handle!))
	       (set! results
		     (make-gc-finalizer pq-clear
					result?
					result-handle
					set-result-handle!))
	       (set! pgsql-initialized? #t)))
	 #t)))

(define (guarantee-pgsql-available)
  (if (not (pgsql-available?))
      (error "This Scheme system was built without PostgreSQL support.")))

(define condition-type:pgsql-error
  (make-condition-type 'PGSQL-ERROR condition-type:error '()
    (lambda (condition port)
      condition
      (write-string "Unknown PostgreSQL error." port))))

(define condition-type:pgsql-connection-error
  (make-condition-type 'PGSQL-CONNECTION-ERROR condition-type:pgsql-error
      '(MESSAGE)
    (lambda (condition port)
      (write-string "Unable to connect to PostgreSQL server" port)
      (write-message (access-condition condition 'MESSAGE) port))))

(define error:pgsql-connection
  (condition-signaller condition-type:pgsql-connection-error
		       '(MESSAGE)
		       standard-error-handler))

(define condition-type:pgsql-query-error
  (make-condition-type 'PGSQL-QUERY-ERROR condition-type:pgsql-error
      '(QUERY RESULT)
    (lambda (condition port)
      (write-string "PostgreSQL query error" port)
      (write-message
       (pgsql-result-error-message (access-condition condition 'RESULT))
       port))))

(define error:pgsql-query
  (condition-signaller condition-type:pgsql-query-error
		       '(QUERY RESULT)
		       standard-error-handler))

(define (write-message string port)
  (if string
      (begin
	(write-string ": " port)
	(let ((regs
	       (re-string-match "\\`\\s *\\(error:\\)?\\s *\\(.*\\)\\s *\\'"
				string
				#t)))
	  (if regs
	      (write-substring string
			       (re-match-start-index 2 regs)
			       (re-match-end-index 2 regs)
			       port)
	      (write-string string port))))
      (write-string "." port)))

(define (open-pgsql-conn parameters #!optional wait?)
  (guarantee-pgsql-available)
  (let ((wait? (if (default-object? wait?) #t wait?)))
    (make-gc-finalized-object
     connections
     (lambda (p)
       (if wait?
	   (pq-connect-db parameters p)
	   (pq-connect-start parameters p)))
     (lambda (handle)
       (cond ((= 0 handle)
	      (error:pgsql-connection #f))
	     ((= PGSQL-CONNECTION-BAD (pq-status handle))
	      (let ((msg (pq-error-message handle)))
		(pq-finish handle)
		(error:pgsql-connection msg))))
       (make-connection handle)))))

(define (close-pgsql-conn connection)
  (remove-from-gc-finalizer! connections connection))

(define (call-with-pgsql-conn parameters procedure)
  (let ((conn))
    (dynamic-wind (lambda ()
		    (set! conn (open-pgsql-conn parameters))
		    unspecific)
		  (lambda ()
		    (procedure conn))
		  (lambda ()
		    (close-pgsql-conn conn)
		    (set! conn)
		    unspecific))))

(define (pgsql-conn-open? connection)
  (guarantee-connection connection 'PGSQL-CONN-OPEN?)
  (if (connection-handle connection) #t #f))

(define-integrable (connection->handle connection)
  (guarantee-valid-connection connection 'CONNECTION->HANDLE))

(define (poll-pgsql-conn connection)
  (index->name (pq-connect-poll (connection->handle connection))
	       postgres-polling-status))

(define (poll-pgsql-reset connection)
  (index->name (pq-reset-poll (connection->handle connection))
	       postgres-polling-status))

(define-syntax define-connection-accessor
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL) (cdr form))
	 (let ((field (cadr form)))
	   `(DEFINE (,(symbol-append 'PGSQL-CONN- field) OBJECT)
	      (,(symbol-append 'PQ- field) (CONNECTION->HANDLE OBJECT))))
	 (ill-formed-syntax form)))))

(define-connection-accessor db)
(define-connection-accessor user)
(define-connection-accessor pass)
(define-connection-accessor host)
(define-connection-accessor port)
(define-connection-accessor tty)
(define-connection-accessor options)
(define-connection-accessor reset)
(define-connection-accessor reset-start)
(define-connection-accessor error-message)

(define (pgsql-conn-status connection)
  (index->name (pq-status (connection->handle connection)) connection-status))

(define (pgsql-get-line connection buffer)
  (pq-get-line (connection->handle connection) buffer))

(define (pgsql-put-line connection buffer)
  (pq-put-line (connection->handle connection) buffer))

(define (pgsql-end-copy connection)
  (pq-end-copy (connection->handle connection)))

(define (escape-pgsql-string string)
  (guarantee-pgsql-available)
  (let ((escaped (make-string (fix:* 2 (string-length string)))))
    (string-head! escaped (pq-escape-string string escaped))))

(define (encode-pgsql-bytea bytes)
  (guarantee-pgsql-available)
  (pq-escape-bytea bytes))

(define (decode-pgsql-bytea string)
  (guarantee-pgsql-available)
  (pq-unescape-bytea string))

(define (exec-pgsql-query connection query)
  (guarantee-string query 'EXEC-PGSQL-QUERY)
  (let ((result
	 (let ((handle (connection->handle connection)))
	   (make-gc-finalized-object
	    results
	    (lambda (p)
	      (pq-exec handle query p))
	    (lambda (result-handle)
	      (if (= 0 result-handle)
		  (error "Unable to execute PostgreSQL query:" query))
	      (make-result result-handle))))))
    (if (not (memq (pgsql-result-status result)
		   '(PGSQL-COMMAND-OK
		     PGSQL-TUPLES-OK
		     PGSQL-COPY-OUT
		     PGSQL-COPY-IN)))
	(error:pgsql-query query result))
    result))

(define (make-empty-pgsql-result connection status)
  (let ((handle (connection->handle connection)))
    (make-gc-finalized-object
     results
     (lambda (p)
       (pq-make-empty-pg-result handle status p))
     (lambda (result-handle)
       (if (= 0 result-handle)
	   (error "Unable to create PostgreSQL result:" status))
       (make-result result-handle)))))

(define-integrable (result->handle result)
  (guarantee-valid-result result 'RESULT->HANDLE))

(define-syntax define-result-accessor
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL) (cdr form))
	 (let ((field (cadr form)))
	   `(DEFINE (,(symbol-append 'PGSQL- field) OBJECT)
	      (,(symbol-append 'PQ- field) (RESULT->HANDLE OBJECT))))
	 (ill-formed-syntax form)))))

(define-result-accessor result-error-message)
(define-result-accessor n-tuples)
(define-result-accessor n-fields)
(define-result-accessor cmd-status)

(define (pgsql-result-status result)
  (index->name (pq-result-status (result->handle result)) exec-status))

(define (pgsql-clear result)
  (remove-from-gc-finalizer! results result))

(define (pgsql-field-name result index)
  (pq-field-name (result->handle result) index))

(define (pgsql-get-value result row column)
  (let ((handle (result->handle result)))
    (if (pq-get-is-null? handle row column)
	#f
	(pq-get-value handle row column))))

(define (pgsql-get-is-null? result row column)
  (pq-get-is-null? (result->handle result) row column))

(define (pgsql-cmd-tuples result)
  (string->number (pq-cmd-tuples (result->handle result))))
