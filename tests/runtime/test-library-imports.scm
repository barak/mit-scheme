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

;;;; Tests for library import management

(declare (usual-integrations))

(include "test-library-data/support-code.scm")

(define-test 'expand-parsed-imports:ex1
  (lambda ()
    (let ((library (parse-define-library-form ex1 test-pathname))
	  (db (make-library-db 'test)))
      (register-library! library db)
      (register-libraries! (read-dependencies) db)
      (assert-lset= library-import=?
		    (library-imports library)
		    (list (make-library-import '(foo mumble) 'foo-mumble?)
			  (make-library-import '(foo mumble) 'make-foo-mumble)
			  (make-library-import '(foo mumble) 'foo-mumble-a)
			  (make-library-import '(foo mumble) 'foo-mumble-b)
			  (make-library-import '(foo grumble)
					       'foo-grumble?
					       'grumble-foo-grumble?)
			  (make-library-import '(foo grumble)
					       'make-foo-grumble
					       'grumble-make-foo-grumble)
			  (make-library-import '(foo grumble)
					       'foo-grumble-a
					       'grumble-foo-grumble-a)
			  (make-library-import '(foo grumble)
					       'foo-grumble-b
					       'grumble-foo-grumble-b)
			  (make-library-import '(foo quux) 'foo-quux?)
			  (make-library-import '(foo quux) 'foo-quux-a)
			  (make-library-import '(foo quux) 'foo-quux-b)
			  (make-library-import '(foo quux)
					       'make-foo-quux
					       'create-foo-quux))))))