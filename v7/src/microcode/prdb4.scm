#| -*-Scheme-*-

$Id: prdb4.scm,v 1.1 2008/01/29 06:09:55 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Program to generate db4 code tables

(declare (usual-integrations))

(define (write-forward-map entries port)
  (write-map entries port
	     (lambda (name port)
	       (write-string "	RC_TO_NAME_CASE (" port)
	       (write-string name port)
	       (write-string ", \"" port)
	       (write-string (string-downcase name) port)
	       (write-string "\");" port)
	       (newline port))))

(define (write-backward-map entries port)
  (write-map entries port
	     (lambda (name port)
	       (write-string "    NAME_TO_RC_CASE (\"" port)
	       (write-string (string-downcase name) port)
	       (write-string "\", " port)
	       (write-string name port)
	       (write-string ");" port)
	       (newline port))))

(define (write-map entries port write-entry)
  (for-each (lambda (entry)
	      (let ((name (car entry))
		    (changes (cdr entry)))
		(if (pair? changes)
		    (begin
		      (write-conditional-start changes port)
		      (write-entry name port)
		      (write-conditional-end port))
		    (write-entry name port))))
	    entries))

(define (write-conditional-start changes port)
  (write-string "#if " port)
  (write-predicate changes port)
  (newline port))

(define (write-conditional-end port)
  (write-string "#endif" port)
  (newline port))

(define (write-predicate changes port)
  (write-comparison (caar changes) (cadar changes) port)
  (for-each (lambda (change)
	      (write-string " && " port)
	      (write-comparison (car change) (cadr change) port))
	    (cdr changes)))

(define (write-comparison action version port)
  (write-string "(UNIFIED_VERSION " port)
  (write-string (case action
		  ((added) ">=")
		  ((removed) "<")
		  (else (error "Unknown action:" action)))
		port)
  (write-string " " port)
  (write-string version port)
  (write-string ")" port))

(define (generate-changes)
  (let ((table (make-string-hash-table))
	(groups
	 (sort (map (lambda (group)
		      (cons (car group)
			    (sort (cdr group) string<?)))
		    rc-names)
	   (lambda (g1 g2)
	     (string<? (car g1) (car g2))))))
    (for-each (lambda (name)
		(hash-table-set! table name '()))
	      (cdar groups))
    (let ((make-notes
	   (lambda (names action version)
	     (for-each
	      (lambda (name)
		(hash-table-update!/default table
					    name
					    (lambda (tail)
					      (cons (list action version)
						    tail))
					    '()))
	      names))))
      (let loop ((groups rc-names))
	(if (pair? (cdr groups))
	    (let ((g1 (car groups))
		  (g2 (cadr groups)))
	      (receive (g1-only g2-only)
		  (generate-one-diff (cdr g1) (cdr g2))
		(make-notes g1-only 'removed (car g2))
		(make-notes g2-only 'added (car g2))
		(loop (cdr groups)))))))
    (sort (map (lambda (entry)
		 (set-cdr! entry (reverse! (cdr entry)))
		 entry)
	       (hash-table->alist table))
      (lambda (e1 e2)
	(string<? (car e1) (car e2))))))

(define (generate-one-diff s1 s2)
  (let loop
      ((s1 (sort s1 string<?))
       (s2 (sort s2 string<?))
       (s1-only '())
       (s2-only '()))
    (cond ((not (pair? s1))
	   (values (reverse! s1-only)
		   (append! (reverse! s2-only) s2)))
	  ((not (pair? s2))
	   (values (append! (reverse! s1-only) s1)
		   (reverse! s2-only)))
	  ((string<? (car s1) (car s2))
	   (loop (cdr s1)
		 s2
		 (cons (car s1) s1-only)
		 s2-only))
	  ((string<? (car s2) (car s1))
	   (loop s1
		 (cdr s2)
		 s1-only
		 (cons (car s2) s2-only)))
	  (else
	   (loop (cdr s1)
		 (cdr s2)
		 s1-only
		 s2-only)))))

(define rc-names
  '(("040200"
     "DB_DONOTINDEX"
     "DB_FILEOPEN"
     "DB_KEYEMPTY"
     "DB_KEYEXIST"
     "DB_LOCK_DEADLOCK"
     "DB_LOCK_NOTGRANTED"
     "DB_NOSERVER"
     "DB_NOSERVER_HOME"
     "DB_NOSERVER_ID"
     "DB_NOTFOUND"
     "DB_OLD_VERSION"
     "DB_PAGE_NOTFOUND"
     "DB_REP_DUPMASTER"
     "DB_REP_HANDLE_DEAD"
     "DB_REP_HOLDELECTION"
     "DB_REP_ISPERM"
     "DB_REP_NEWMASTER"
     "DB_REP_NEWSITE"
     "DB_REP_NOTPERM"
     "DB_REP_OUTDATED"
     "DB_REP_UNAVAIL"
     "DB_RUNRECOVERY"
     "DB_SECONDARY_BAD"
     "DB_VERIFY_BAD"
     "DB_ALREADY_ABORTED"
     "DB_DELETED"
     "DB_LOCK_NOTEXIST"
     "DB_NEEDSPLIT"
     "DB_SURPRISE_KID"
     "DB_SWAPBYTES"
     "DB_TIMEOUT"
     "DB_TXN_CKP"
     "DB_VERIFY_FATAL")

    ("040300"
     "DB_BUFFER_SMALL"
     "DB_DONOTINDEX"
     "DB_KEYEMPTY"
     "DB_KEYEXIST"
     "DB_LOCK_DEADLOCK"
     "DB_LOCK_NOTGRANTED"
     "DB_LOG_BUFFER_FULL"
     "DB_NOSERVER"
     "DB_NOSERVER_HOME"
     "DB_NOSERVER_ID"
     "DB_NOTFOUND"
     "DB_OLD_VERSION"
     "DB_PAGE_NOTFOUND"
     "DB_REP_DUPMASTER"
     "DB_REP_HANDLE_DEAD"
     "DB_REP_HOLDELECTION"
     "DB_REP_ISPERM"
     "DB_REP_NEWMASTER"
     "DB_REP_NEWSITE"
     "DB_REP_NOTPERM"
     "DB_REP_STARTUPDONE"
     "DB_REP_UNAVAIL"
     "DB_RUNRECOVERY"
     "DB_SECONDARY_BAD"
     "DB_VERIFY_BAD"
     "DB_VERSION_MISMATCH"
     "DB_ALREADY_ABORTED"
     "DB_DELETED"
     "DB_LOCK_NOTEXIST"
     "DB_NEEDSPLIT"
     "DB_REP_EGENCHG"
     "DB_REP_LOGREADY"
     "DB_REP_PAGEDONE"
     "DB_SURPRISE_KID"
     "DB_SWAPBYTES"
     "DB_TIMEOUT"
     "DB_TXN_CKP"
     "DB_VERIFY_FATAL")

    ("040400"
     "DB_BUFFER_SMALL"
     "DB_DONOTINDEX"
     "DB_KEYEMPTY"
     "DB_KEYEXIST"
     "DB_LOCK_DEADLOCK"
     "DB_LOCK_NOTGRANTED"
     "DB_LOG_BUFFER_FULL"
     "DB_NOSERVER"
     "DB_NOSERVER_HOME"
     "DB_NOSERVER_ID"
     "DB_NOTFOUND"
     "DB_OLD_VERSION"
     "DB_PAGE_NOTFOUND"
     "DB_REP_DUPMASTER"
     "DB_REP_HANDLE_DEAD"
     "DB_REP_HOLDELECTION"
     "DB_REP_IGNORE"
     "DB_REP_ISPERM"
     "DB_REP_JOIN_FAILURE"
     "DB_REP_LOCKOUT"
     "DB_REP_NEWMASTER"
     "DB_REP_NEWSITE"
     "DB_REP_NOTPERM"
     "DB_REP_STARTUPDONE"
     "DB_REP_UNAVAIL"
     "DB_RUNRECOVERY"
     "DB_SECONDARY_BAD"
     "DB_VERIFY_BAD"
     "DB_VERSION_MISMATCH"
     "DB_ALREADY_ABORTED"
     "DB_DELETED"
     "DB_NEEDSPLIT"
     "DB_REP_BULKOVF"
     "DB_REP_EGENCHG"
     "DB_REP_LOGREADY"
     "DB_REP_PAGEDONE"
     "DB_SURPRISE_KID"
     "DB_SWAPBYTES"
     "DB_TIMEOUT"
     "DB_TXN_CKP"
     "DB_VERIFY_FATAL")

    ("040500"
     "DB_BUFFER_SMALL"
     "DB_DONOTINDEX"
     "DB_KEYEMPTY"
     "DB_KEYEXIST"
     "DB_LOCK_DEADLOCK"
     "DB_LOCK_NOTGRANTED"
     "DB_LOG_BUFFER_FULL"
     "DB_NOSERVER"
     "DB_NOSERVER_HOME"
     "DB_NOSERVER_ID"
     "DB_NOTFOUND"
     "DB_OLD_VERSION"
     "DB_PAGE_NOTFOUND"
     "DB_REP_DUPMASTER"
     "DB_REP_HANDLE_DEAD"
     "DB_REP_HOLDELECTION"
     "DB_REP_IGNORE"
     "DB_REP_ISPERM"
     "DB_REP_JOIN_FAILURE"
     "DB_REP_LOCKOUT"
     "DB_REP_NEWMASTER"
     "DB_REP_NEWSITE"
     "DB_REP_NOTPERM"
     "DB_REP_UNAVAIL"
     "DB_RUNRECOVERY"
     "DB_SECONDARY_BAD"
     "DB_VERIFY_BAD"
     "DB_VERSION_MISMATCH"
     "DB_ALREADY_ABORTED"
     "DB_DELETED"
     "DB_NEEDSPLIT"
     "DB_REP_BULKOVF"
     "DB_REP_EGENCHG"
     "DB_REP_LOGREADY"
     "DB_REP_PAGEDONE"
     "DB_SURPRISE_KID"
     "DB_SWAPBYTES"
     "DB_TIMEOUT"
     "DB_TXN_CKP"
     "DB_VERIFY_FATAL")

    ("040600"
     "DB_BUFFER_SMALL"
     "DB_DONOTINDEX"
     "DB_KEYEMPTY"
     "DB_KEYEXIST"
     "DB_LOCK_DEADLOCK"
     "DB_LOCK_NOTGRANTED"
     "DB_LOG_BUFFER_FULL"
     "DB_NOSERVER"
     "DB_NOSERVER_HOME"
     "DB_NOSERVER_ID"
     "DB_NOTFOUND"
     "DB_OLD_VERSION"
     "DB_PAGE_NOTFOUND"
     "DB_REP_DUPMASTER"
     "DB_REP_HANDLE_DEAD"
     "DB_REP_HOLDELECTION"
     "DB_REP_IGNORE"
     "DB_REP_ISPERM"
     "DB_REP_JOIN_FAILURE"
     "DB_REP_LEASE_EXPIRED"
     "DB_REP_LOCKOUT"
     "DB_REP_NEWSITE"
     "DB_REP_NOTPERM"
     "DB_REP_UNAVAIL"
     "DB_RUNRECOVERY"
     "DB_SECONDARY_BAD"
     "DB_VERIFY_BAD"
     "DB_VERSION_MISMATCH"
     "DB_ALREADY_ABORTED"
     "DB_DELETED"
     "DB_EVENT_NOT_HANDLED"
     "DB_NEEDSPLIT"
     "DB_REP_BULKOVF"
     "DB_REP_EGENCHG"
     "DB_REP_LOGREADY"
     "DB_REP_NEWMASTER"
     "DB_REP_PAGEDONE"
     "DB_SURPRISE_KID"
     "DB_SWAPBYTES"
     "DB_TIMEOUT"
     "DB_TXN_CKP"
     "DB_VERIFY_FATAL")))