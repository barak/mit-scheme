#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Operating-system primitives
;;; package: (runtime os-primitives)

(declare (usual-integrations))

(add-boot-deps! '(runtime hash-table))

(define (get-environment-variable name)
  (guarantee string? name 'get-environment-variable)
  (with-thread-mutex-lock %env-lock
    (lambda ()
      (hash-table-ref %local-env name
	(lambda ()
	  (hash-table-ref/default (env-cache) name #f))))))

(define (get-environment-variables)
  (with-thread-mutex-lock %env-lock
    (lambda ()
      (let ((local-copy (hash-table-copy %local-env)))
	(hash-table-for-each (lambda (name value)
			       (if (not (hash-table-exists? local-copy name))
				   (hash-table-set! local-copy name value)))
			     (env-cache))
	(let ((entries
	       (hash-table-fold local-copy
				(lambda (name value result)
				  (if value
				      (cons (cons name value) result)
				      result))
				'())))
	  (sort entries string<? car))))))

(define (set-environment-variable! name value)
  (guarantee string? name 'set-environment-variable!)
  (if value
      (guarantee string? value 'set-environment-variable!))
  (with-thread-mutex-lock %env-lock
    (lambda ()
      (hash-table-set! %local-env name value))))

(define (delete-environment-variable! name)
  (guarantee string? name 'delete-environment-variable!)
  (with-thread-mutex-lock %env-lock
    (lambda ()
      ;; XXX Kinda does the wrong thing on save/restore -- restoring a
      ;; band in a new environment might cause a variable that you had
      ;; deleted to become available.  But the alternative is for
      ;; delete-environment-variable! to leak memory indefinitely.
      (if (hash-table-ref/default (env-cache) name #f)
	  (hash-table-set! %local-env name #f)
	  (hash-table-delete! %local-env name)))))

(define (env-cache)
  (assert-thread-mutex-owned %env-lock)
  (if (not %env-cache)
      (begin
	(set! %env-cache (os/make-env-cache))
	(vector-for-each (lambda (s)
			   (let ((i (string-find-next-char s #\=)))
			     (if i
				 (let ((var (string-head s i))
				       (val (string-tail s (fix:+ i 1))))
				   (hash-table-set! %env-cache var val)))))
			 ((ucode-primitive get-environment 0)))))
  %env-cache)

(define (reset-environment-variables!)
  (with-thread-mutex-lock %env-lock
    (lambda ()
      (set! %env-cache #f))))

(define-deferred %env-lock
  (make-thread-mutex))

(define %env-cache)

(define-deferred %local-env
  (os/make-env-cache))

(add-boot-init!
 (lambda ()
   (reset-environment-variables!)
   (add-event-receiver! event:after-restart reset-environment-variables!)
   (add-secondary-gc-daemon! reset-environment-variables!)))