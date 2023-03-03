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

;;;; Generate runtime standard libraries

(declare (usual-integrations))

(define (generate-standard-libs file)
  (call-with-output-file file
    (lambda (port)
      (let ((stds (lib-standards)))
	(generate-standard-lib (car stds) port)
	(for-each (lambda (std)
		    (newline port)
		    (newline port)
		    (generate-standard-lib std port))
		  (cdr stds))
	(newline port)
	(newline port)
	(write `(define srfi-features ',(supported-srfi-list)) port)))))

(define (generate-standard-lib std port)
  (write-string "(define-standard-library '" port)
  (write (std-library std) port)
  (write-string "\n  '(" port)
  (let ((bound (std-bound+extra std)))
    (write (car bound) port)
    (for-each (lambda (name)
		(write-string "\n    " port)
		(write name port))
	      (cdr bound)))
  (write-string "))" port))

(define (lib-standards)
  (filter (lambda (std)
	    (and (std-library std)
		 (eq? 'all (std-prop std 'global cadr #f))))
	  standards))

(define (supported-srfi-list)
  (append-map srfi-features
	      (filter (lambda (std)
			(and (srfi? std)
			     (eq? 'full (std-prop std 'support cadr #f))))
		      standards)))

(define (srfi-features std)
  (cons (string->symbol (string-append "srfi-" (srfi-number-string std)))
	(std-prop std 'features cdr '())))