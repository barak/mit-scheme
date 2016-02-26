#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;;; Parameters
;;; package: (runtime dynamic)

(declare (usual-integrations))

(define bindings '())	    ; The current thread's parameter bindings.
(define parameter?)
(define parameter-metadata)
(define set-parameter-metadata!)
(define get-metadata-alist)

(define (use-metadata-implementation! implementation)
  (set! parameter? (implementation 'has?))
  (set! parameter-metadata (implementation 'get))
  (set! set-parameter-metadata! (implementation 'put!))
  (set! get-metadata-alist (implementation 'get-alist))
  unspecific)

;; Use alist for cold-load.
(use-metadata-implementation! (make-alist-metadata-table))

;; Later move metadata to hash table.
(define (initialize-package!)
  (let ((implementation (make-hashed-metadata-table)))
    ((implementation 'put-alist!) (get-metadata-alist))
    (use-metadata-implementation! implementation)))

(define-guarantee parameter "parameter")

(define (make-parameter init #!optional converter)
  (let* ((converter
	  (if (default-object? converter)
	      (lambda (x) x)
	      converter))
	 (metadata (cons converter (converter init)))
	 (parameter
	  (lambda (#!optional new-value)
	    (let ((p (or (assq metadata bindings) metadata)))
	      (if (default-object? new-value)
		  (cdr p)
		  (set-cdr! p (converter new-value)))))))
    (set-parameter-metadata! parameter metadata)
    parameter))

(define (parameterize* new-bindings thunk)
  (guarantee-alist new-bindings 'parameterize*)
  (let ((temp
	 (map* bindings
	       (lambda (p)
		 (let ((metadata (parameter-metadata (car p))))
		   (cons metadata
			 ((car metadata) (cdr p)))))
	       new-bindings)))
    (let ((swap!
	   (lambda ()
	     (set! bindings (set! temp (set! bindings)))
	     unspecific)))
      (shallow-fluid-bind swap! thunk swap!))))