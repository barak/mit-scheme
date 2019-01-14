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
;;; package: (runtime postgresql)

(declare (usual-integrations))

;;; Access to the PostgreSQL library is now accomplished with the FFI
;;; rather than a microcode module.  The bindings in this package are
;;; linked to those in the (pgsql) package after the plugin is loaded.

(define linked? #f)

(define (pgsql-available?)
  (and (plugin-available? "pgsql")
       (or linked?
	   (begin
	     (load-option 'pgsql)
	     (link!)
	     #t))))

(define (link!)
  (for-each
    (let ((runtime (->environment '(runtime postgresql)))
	  (pgsql (->environment '(pgsql))))
      (lambda (name)
	(environment-link-name runtime pgsql name)))
    names)
  (set! linked? #t))

(define names
  '(call-with-pgsql-conn
    close-pgsql-conn
    condition-type:pgsql-connection-error
    condition-type:pgsql-error
    condition-type:pgsql-query-error
    decode-pgsql-bytea
    encode-pgsql-bytea
    escape-pgsql-string
    exec-pgsql-query
    guarantee-pgsql-available
    make-empty-pgsql-result
    open-pgsql-conn
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
    poll-pgsql-reset))

(define call-with-pgsql-conn)
(define close-pgsql-conn)
(define condition-type:pgsql-connection-error)
(define condition-type:pgsql-error)
(define condition-type:pgsql-query-error)
(define decode-pgsql-bytea)
(define encode-pgsql-bytea)
(define escape-pgsql-string)
(define exec-pgsql-query)
(define guarantee-pgsql-available)
(define make-empty-pgsql-result)
(define open-pgsql-conn)
(define pgsql-bad-response)
(define pgsql-clear)
(define pgsql-cmd-status)
(define pgsql-cmd-tuples)
(define pgsql-command-ok)
(define pgsql-conn-db)
(define pgsql-conn-error-message)
(define pgsql-conn-host)
(define pgsql-conn-open?)
(define pgsql-conn-options)
(define pgsql-conn-pass)
(define pgsql-conn-port)
(define pgsql-conn-reset)
(define pgsql-conn-reset-start)
(define pgsql-conn-status)
(define pgsql-conn-tty)
(define pgsql-conn-user)
(define pgsql-connection-auth-ok)
(define pgsql-connection-awaiting-response)
(define pgsql-connection-bad)
(define pgsql-connection-made)
(define pgsql-connection-ok)
(define pgsql-connection-setenv)
(define pgsql-connection-started)
(define pgsql-copy-in)
(define pgsql-copy-out)
(define pgsql-empty-query)
(define pgsql-fatal-error)
(define pgsql-field-name)
(define pgsql-get-is-null?)
(define pgsql-get-line)
(define pgsql-get-value)
(define pgsql-n-fields)
(define pgsql-n-tuples)
(define pgsql-nonfatal-error)
(define pgsql-polling-active)
(define pgsql-polling-failed)
(define pgsql-polling-ok)
(define pgsql-polling-reading)
(define pgsql-polling-writing)
(define pgsql-put-line)
(define pgsql-result-error-message)
(define pgsql-result-status)
(define pgsql-tuples-ok)
(define poll-pgsql-conn)
(define poll-pgsql-reset)