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

;;;; PostgreSQL Interface
;;; package: (postgresql)

(declare (usual-integrations))

(define (import-postgresql)
  (let ((target-environment (nearest-repl/environment))
	(source-environment (->environment '(postgresql))))
    (for-each (lambda (name)
		(link-variables target-environment name
				source-environment name))
	      '(
		call-with-pgsql-conn
		close-pgsql-conn
		condition-type:pgsql-connection-error
		condition-type:pgsql-error
		condition-type:pgsql-query-error
		decode-pgsql-bytea
		encode-pgsql-bytea
		escape-pgsql-string
		exec-pgsql-query
		;; guarantee-pgsql-available
		make-empty-pgsql-result
		open-pgsql-conn
		;; pgsql-available?
		pgsql-bad-response
		pgsql-clear
		pgsql-cmd-status
		pgsql-cmd-tuples
		pgsql-command-ok
		pgsql-conn-db
		pgsql-conn-error-message
		pgsql-conn-host
		pgsql-conn-open?
		pgsql-conn-options
		pgsql-conn-pass
		pgsql-conn-port
		pgsql-conn-reset
		pgsql-conn-reset-start
		pgsql-conn-status
		pgsql-conn-tty
		pgsql-conn-user
		pgsql-connection-auth-ok
		pgsql-connection-awaiting-response
		pgsql-connection-bad
		pgsql-connection-made
		pgsql-connection-ok
		pgsql-connection-setenv
		pgsql-connection-started
		pgsql-copy-in
		pgsql-copy-out
		pgsql-empty-query
		pgsql-fatal-error
		pgsql-field-name
		pgsql-get-is-null?
		pgsql-get-line
		pgsql-get-value
		pgsql-n-fields
		pgsql-n-tuples
		pgsql-nonfatal-error
		pgsql-polling-active
		pgsql-polling-failed
		pgsql-polling-ok
		pgsql-polling-reading
		pgsql-polling-writing
		pgsql-put-line
		pgsql-result-error-message
		pgsql-result-status
		pgsql-tuples-ok
		poll-pgsql-conn
		poll-pgsql-reset
		))))

(C-include "pgsql")

(define-integrable (every-loop proc ref string start end)
  (let loop ((i start))
    (if (fix:< i end)
	(and (proc (ref string i))
	     (loop (fix:+ i 1)))
	#t)))

(define (->bytes string)
  (if (and (or (bytevector? string)
	       (and (ustring? string)
		    (fix:= 1 (ustring-cp-size string))))
	   (let ((end (string-length string)))
	     (every-loop (lambda (cp) (fix:< cp #x80))
			 cp1-ref string 0 end)))
      string
      (string->utf8 string)))

(declare (integrate-operator bytes-length))
(define (bytes-length bytes)
  (if (bytevector? bytes)
      (bytevector-length bytes)
      (string-length bytes)))

(define-integrable (pq-clear handle)
  (C-call "PQclear" handle))

(define-integrable (peek-cstring alien)
  (let ((bv (c-peek-cstring alien)))
    (if (bytevector? bv)
	(utf8->string bv)
	bv)))

(define-integrable (pq-cmd-status handle)
  (peek-cstring (C-call "PQcmdStatus" (make-alien 'char) handle)))

(define-integrable (pq-cmd-tuples handle)
  (peek-cstring (C-call "PQcmdTuples" (make-alien 'char) handle)))

(define-integrable (pq-connect-db conninfo weak-pair)
  (weak-set-cdr! weak-pair
		 (C-call "PQconnectdb" (make-alien '|PGconn|) conninfo)))

(define-integrable (pq-connect-poll handle)
  (C-call "PQconnectPoll" handle))

(define-integrable (pq-connect-start conninfo weak-pair)
  (weak-set-cdr! weak-pair
		 (C-call "PQconnectStart" (make-alien '|PGconn|) conninfo)))

(define-integrable (pq-db handle)
  (peek-cstring (C-call "PQdb" handle)))

(define-integrable (pq-end-copy handle)
  (C-call "PQendcopy" handle))

(define-integrable (pq-error-message conn)
  (peek-cstring (C-call "PQerrorMessage" (make-alien 'char) conn)))

(define (pq-escape-bytea string)
  (peek-memory string
	       (lambda (memory bytes length)
		 (C-call "PQescapeBytea" (memory-alien memory)
			 bytes (bytes-length bytes) length))))

(define (peek-memory string callout)
  (let ((bytes (->bytes string))
	(memory (create-memory))
	(length (malloc (c-sizeof "size_t") '|size_t|)))
    (callout memory bytes length)
    (if (alien-null? (memory-alien memory))
	(error "insufficient memory")
	(let* ((nbytes (C-> length "size_t")) ;includes terminating #\null
	       (bv (make-bytevector nbytes)))
	  (c-peek-bytes (memory-alien memory) 0 nbytes bv 0)
	  (free length)
	  (free-memory memory)
	  bv))))

(define (free-memory memory)
  (remove-from-gc-finalizer! memories memory))

(define (create-memory)
  (make-gc-finalized-object
   memories
   (lambda (p)
     (weak-set-cdr! p (make-alien 'uchar)))
   (lambda (alien)
     (make-memory alien))))

(declare (integrate-operator pq-escape-string))
(define (pq-escape-string bytes escaped)
  (C-call "PQescapeString" escaped bytes (bytes-length bytes)))

(define-integrable (pq-exec handle query weak-pair)
  (weak-set-cdr! weak-pair
		 (C-call "PQexec" (make-alien '|PQresult|) handle query)))

(define-integrable (pq-field-name handle index)
  (peek-cstring (C-call "PQfname" (make-alien 'char) handle index)))

(define-integrable (pq-finish handle)
  (C-call "PQfinish" handle))

(define-integrable (pq-freemem handle)
  (if (not (alien-null? handle))
      (without-interruption
       (lambda ()
	 (C-call "PQfreemem" handle)
	 (alien-null! handle)))))

(define-integrable (pq-get-is-null? result tup-num field-num)
  (= 1 (C-call "PQgetisnull" result tup-num field-num)))

(define-integrable (pq-get-line conn buffer length)
  (C-call "PQgetline" conn buffer length))

(define-integrable (pq-get-value handle tup-num field-num)
  (peek-cstring (C-call "PQgetvalue" (make-alien 'char)
			handle tup-num field-num)))

(define-integrable (pq-host handle)
  (peek-cstring (C-call "PQhost" handle)))

(define-integrable (pq-make-empty-pg-result handle status weak-pair)
  (weak-set-cdr! weak-pair
		 (C-call "PQmakeEmptyPGresult" (make-alien '|PQresult|)
			 handle status)))

(define-integrable (pq-n-fields handle)
  (C-call "PQnfields" handle))

(define-integrable (pq-n-tuples handle)
  (C-call "PQntuples" handle))

(define-integrable (pq-options handle)
  (peek-cstring (C-call "PQoptions" (make-alien 'char) handle)))

(define-integrable (pq-pass handle)
  (peek-cstring (C-call "PQpass" (make-alien 'char) handle)))

(define-integrable (pq-port handle)
  (peek-cstring (C-call "PQport" (make-alien 'char) handle)))

(define-integrable (pq-put-line handle buffer)
  (C-call "PQputline" handle buffer))

(define-integrable (pq-res-status status)
  (peek-cstring (C-call "PQresStatus" (make-alien 'char) status)))

(define-integrable (pq-reset handle)
  (C-call "PQreset" handle))

(define-integrable (pq-reset-poll handle)
  (C-call "PQresetPoll" handle))

(define-integrable (pq-reset-start handle)
  (C-call "PQresetStart" handle))

(define-integrable (pq-result-error-message handle)
  (peek-cstring (C-call "PQresultErrorMessage" (make-alien 'char) handle)))

(define-integrable (pq-result-status handle)
  (C-call "PQresultStatus" handle))

(define-integrable (pq-status handle)
  (C-call "PQstatus" handle))

(define-integrable (pq-tty handle)
  (peek-cstring (C-call "PQtty" (make-alien 'char) handle)))

(define (pq-unescape-bytea string)
  (peek-memory string
	       (lambda (memory bytes length)
		 (C-call "PQunescapeBytea" (memory-alien memory)
			 bytes length))))

(define-integrable (pq-user handle)
  (peek-cstring (C-call "PQuser" (make-alien 'char) handle)))

(define-syntax define-enum
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(identifier * identifier) (cdr form))
	 `(BEGIN
	    ,@(let loop ((names (cddr form)) (index 0))
		(if (pair? names)
		     `((DEFINE ,(car names) ,index)
		       ,@(loop (cdr names) (+ index 1)))
		     '()))
	    (DEFINE ,(cadr form) '#(,@(cddr form))))
	 (ill-formed-syntax form)))))

(define (index->name index enum)
  (guarantee index-fixnum? index 'INDEX->NAME)
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
(define memories)

(define-structure connection handle)
(define-structure result handle)
(define-structure memory alien)

(define-syntax define-guarantee
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(symbol expression) (cdr form))
	 (let ((type (cadr form)))
	   (let ((type? (symbol type '?))
		 (guarantee-type (symbol 'GUARANTEE- type))
		 (error:not-type (symbol 'ERROR:NOT- type))
		 (guarantee-valid-type (symbol 'GUARANTEE-VALID- type))
		 (type-handle (symbol type '-HANDLE)))
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

(define (initialize-package!)
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
	(set! memories
	      (make-gc-finalizer pq-freemem
				 memory?
				 memory-alien
				 set-memory-alien!))
	(set! pgsql-initialized? #t))))

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
	(write-string
	 (let ((result (regsexp-match-string error-regsexp string)))
	   (if result
	       (cdr (assv 'message (cddr result)))
	       string))
	 port))
      (write-string "." port)))

(define error-regsexp
  (compile-regsexp
   '(seq (string-start)
	 (* (char-in whitespace))
	 (? (string-ci "error:"))
	 (* (char-in whitespace))
	 (group message (* (any-char)))
	 (* (char-in whitespace))
	 (string-end))))

(define (open-pgsql-conn parameters #!optional wait?)
  (let ((wait? (if (default-object? wait?) #t wait?)))
    (make-gc-finalized-object
     connections
     (lambda (p)
       (if wait?
	   (pq-connect-db parameters p)
	   (pq-connect-start parameters p)))
     (lambda (handle)
       (cond ((alien-null? handle)
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
     (if (syntax-match? '(symbol) (cdr form))
	 (let ((field (cadr form)))
	   `(DEFINE (,(symbol 'PGSQL-CONN- field) OBJECT)
	      (,(symbol 'PQ- field) (CONNECTION->HANDLE OBJECT))))
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
  (pq-get-line (connection->handle connection)
	       buffer (bytevector-length buffer)))

(define (pgsql-put-line connection buffer)
  (pq-put-line (connection->handle connection) buffer))

(define (pgsql-end-copy connection)
  (pq-end-copy (connection->handle connection)))

(define (escape-pgsql-string string)
  (let* ((bytes (->bytes string))
	 (length (bytes-length bytes))
	 (escaped-bytes (malloc (fix:1+ (fix:* 2 length)) 'char))
	 (escaped-string (begin
			   (pq-escape-string bytes escaped-bytes)
			   (peek-cstring escaped-bytes))))
    (free escaped-bytes)
    escaped-string))

(define (encode-pgsql-bytea bytes)
  (pq-escape-bytea bytes))

(define (decode-pgsql-bytea string)
  (pq-unescape-bytea string))

(define (exec-pgsql-query connection query)
  (guarantee string? query 'EXEC-PGSQL-QUERY)
  (let ((result
	 (let ((handle (connection->handle connection)))
	   (make-gc-finalized-object
	    results
	    (lambda (p)
	      (pq-exec handle query p))
	    (lambda (result-handle)
	      (if (alien-null? result-handle)
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

(define-integrable (result->handle result operator)
  (guarantee-valid-result result operator))

(define-syntax define-result-accessor
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(symbol) (cdr form))
	 (let* ((field (cadr form))
		(operator (symbol 'PGSQL- field)))
	   `(DEFINE (,operator OBJECT)
	      (,(symbol 'PQ- field) (RESULT->HANDLE OBJECT ',operator))))
	 (ill-formed-syntax form)))))

(define-result-accessor result-error-message)
(define-result-accessor n-tuples)
(define-result-accessor n-fields)
(define-result-accessor cmd-status)

(define (pgsql-result-status result)
  (index->name (pq-result-status (result->handle result 'pgsql-result-status))
	       exec-status))

(define (pgsql-clear result)
  (remove-from-gc-finalizer! results result))

(define (pgsql-field-name result index)
  (pq-field-name (result->handle result 'pgsql-field-name) index))

(define (pgsql-get-value result row column)
  (let ((handle (result->handle result 'pgsql-get-value)))
    (if (pq-get-is-null? handle row column)
	#f
	(pq-get-value handle row column))))

(define (pgsql-get-is-null? result row column)
  (pq-get-is-null? (result->handle result 'pgsql-get-is-null?) row column))

(define (pgsql-cmd-tuples result)
  (string->number (pq-cmd-tuples (result->handle result 'pgsql-cmd-tuples))))