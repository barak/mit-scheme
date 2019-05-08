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

;;;; Operating-system primitives
;;; package: (runtime os-primitives)

(declare (usual-integrations))

(define (get-environment-variable name)
  (guarantee string? name 'get-environment-variable)
  (hash-table-ref/default %env-cache name #f))

(define (get-environment-variables)
  (hash-table-fold %env-cache
		   (lambda (name value result)
		     (if value
			 (cons (cons name value) result)
			 result))
		   '()))

(define (set-environment-variable! name value)
  (guarantee string? name 'set-environment-variable!)
  (if value
      (guarantee string? value 'set-environment-variable!))
  (hash-table-set! %env-cache name value))

(define (delete-environment-variable! name)
  (guarantee string? name 'delete-environment-variable!)
  (hash-table-delete! %env-cache name))

(define (reset-environment-variables!)
  (hash-table-clear! %env-cache)
  (vector-for-each (lambda (s)
		     (let ((s (string-from-primitive s))
			   (i (string-find-next-char s #\=)))
		       (if i
			   (hash-table-set! %env-cache
					    (string-head s i)
					    (string-tail s (fix:+ i 1))))))
		   ((ucode-primitive get-environment 0))))

(define-deferred %env-cache
  (os/make-env-cache))

(add-boot-init!
 (lambda ()
   (reset-environment-variables!)
   (add-event-receiver! event:after-restart reset-environment-variables!)))