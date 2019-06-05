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

;;;; Working Directory
;;; package: (runtime working-directory)

(declare (usual-integrations))

(define (initialize-package!)
  (set! working-directory-pathname
	(make-general-parameter #f
				default-parameter-converter
				default-parameter-merger
				default-parameter-getter
				wd-setter))
  (reset!)
  (add-event-receiver! event:after-restore reset!))

(define working-directory-pathname)

(define (wd-setter set-param pathname)
  (set-param pathname)
  (param:default-pathname-defaults pathname)
  pathname)

(define (reset!)
  (working-directory-pathname
   (pathname-simplify
    (pathname-as-directory
     (string-from-primitive
      ((ucode-primitive working-directory-pathname)))))))

(define (set-working-directory-pathname! name)
  (let ((pathname (new-pathname name)))
    ;; XXX Checking FILE-DIRECTORY? is a stop-gap kludge until we fix
    ;; the concept of `working directory' by (a) making it be the
    ;; directory, not the pathname; (b) making it thread-local, and (c)
    ;; opening it in SET-WORKING-DIRECTORY! and hanging onto it for
    ;; relative lookups, either by changing directory just before each
    ;; lookup or using relative lookup system calls.
    (if (not (file-directory? pathname))
	(error:file-operation 0 "enter" "directory"
			      (if (file-exists?
				   (directory-pathname-as-file pathname))
				  "not a directory"
				  "no such directory")
			      'set-working-directory-pathname!
			      (list name)))
    (working-directory-pathname pathname)
    (cmdl/set-default-directory (nearest-cmdl) pathname)
    pathname))

(define (with-working-directory-pathname name thunk)
  (let ((pathname (new-pathname name)))
    (fluid-let ((*default-pathname-defaults* pathname))
      (parameterize ((param:default-pathname-defaults pathname)
		     (working-directory-pathname pathname))
	(thunk)))))

(define (new-pathname name)
  (pathname-simplify
   (pathname-as-directory
    (merge-pathnames name (working-directory-pathname)))))