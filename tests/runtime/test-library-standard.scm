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

;;;; Tests for standard libraries

(declare (usual-integrations))

(include "test-library-data/support-code.scm")

(define-test 'check-standard-libraries!
  (lambda ()
    (check-standard-libraries!)))

(define-test 'make-standard-libraries
  (map (lambda (library)
	 (lambda ()
	   (check-standard-library library)))
       (make-standard-libraries)))

(define-test 'add-standard-libraries!
  (let ((db (make-library-db 'test)))
    (add-standard-libraries! db)
    (map (lambda (name)
	   (let ((library (registered-library name db)))
	     (lambda ()
	       (check-standard-library library))))
	 (standard-library-names))))

(define (check-standard-library library)
  (let ((exports (standard-library-exports (library-name library))))
    (assert-null (library-parsed-imports library))
    (assert-lset= library-export=?
		  (library-exports library)
		  (map make-library-export exports))
    (assert-null (library-parsed-contents library))
    (assert-false (library-filename library))
    (assert-eqv (library-environment library)
		system-global-environment)))