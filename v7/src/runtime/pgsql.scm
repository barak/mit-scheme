#| -*-Scheme-*-

$Id: pgsql.scm,v 1.3 2003/11/06 00:16:21 cph Exp $

Copyright 2003 Massachusetts Institute of Technology

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
  (pq-error-message 1)
  (pq-escape-string 2)
  (pq-exec 3)
  (pq-field-name 2)
  (pq-finish 1)
  (pq-get-is-null? 3)
  (pq-get-value 3)
  (pq-host 1)
  (pq-make-empty-pg-result 3)
  (pq-n-fields 1)
  (pq-n-tuples 1)
  (pq-options 1)
  (pq-pass 1)
  (pq-port 1)
  (pq-res-status 1)
  (pq-reset 1)
  (pq-reset-poll 1)
  (pq-reset-start 1)
  (pq-result-error-message 1)
  (pq-result-status 1)
  (pq-status 1)
  (pq-tty 1)
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
  (and (implemented-primitive-procedure? pq-connect-db)
       (begin
	 (if (not pgsql-initialized?)
	     (begin
	       (set! connections (make-gc-finalizer pq-finish))
	       (set! results (make-gc-finalizer pq-clear))
	       (set! pgsql-initialized? #t)))
	 #t)))

(define (open-pgsql-conn parameters #!optional wait?)
  (if (not (pgsql-available?))
      (error "No PostgreSQL support in this sytem."))
  (let ((wait? (if (default-object? wait?) #t wait?)))
    (make-gc-finalized-object
     connections
     (lambda (p)
       (if wait?
	   (pq-connect-db parameters p)
	   (pq-connect-start parameters p)))
     (lambda (handle)
       (cond ((= 0 handle)
	      (error "Unable to connect to PostgreSQL server."))
	     ((= PGSQL-CONNECTION-BAD (pq-status handle))
	      (let ((msg (pq-error-message handle)))
		(pq-finish handle)
		(error "Unable to connect to PostgreSQL server:" msg))))
       (make-connection handle)))))

(define (close-pgsql-conn connection)
  (guarantee-connection connection 'CLOSE-PGSQL-CONN)
  (without-interrupts
   (lambda ()
     (if (connection-handle connection)
	 (begin
	   (remove-from-gc-finalizer! connections connection)
	   (set-connection-handle! connection #f))))))

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

(define (escape-pgsql-string string)
  (let ((escaped (make-string (fix:* 2 (string-length string)))))
    (set-string-maximum-length! escaped (pq-escape-string string escaped))
    escaped))

(define (exec-pgsql-query connection query)
  (guarantee-string query 'EXEC-PGSQL-QUERY)
  (let ((handle (connection->handle connection)))
    (make-gc-finalized-object
     results
     (lambda (p)
       (pq-exec handle query p))
     (lambda (result-handle)
       (if (= 0 result-handle)
	   (error "Unable to execute PostgreSQL query:" query))
       (make-result result-handle)))))

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
(define-result-accessor clear)
(define-result-accessor n-tuples)
(define-result-accessor n-fields)
(define-result-accessor cmd-status)

(define (pgsql-result-status result)
  (index->name (pq-result-status (result->handle result)) exec-status))

(define (pgsql-field-name result index)
  (pq-field-name (result->handle result) index))

(define (pgsql-get-value result row column)
  (pq-get-value (result->handle result) row column))

(define (pgsql-get-is-null? result row column)
  (pq-get-is-null? (result->handle result) row column))

(define (pgsql-cmd-tuples result)
  (string->number (pq-cmd-tuples (result->handle result))))