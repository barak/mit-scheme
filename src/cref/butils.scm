#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Build utilities
;;; package: (cross-reference build-utilities)

(declare (usual-integrations))

(define (compile-system name directory . options)
  ;; Gets a list of file.package from DIRECTORY/NAME.pkg, creates the
  ;; packages described therein, then (re)compiles (as necessary) and
  ;; loads each file in order.
  ;;
  ;; If OPTIONS includes 'dependencies, its value should be an alist
  ;; of filenames, as they appear in the NAME.pkg file, each
  ;; associated with a list of pathnames (relative to DIRECTORY).

  (define (find-option name options default)
    (let loop ((opts options))
      (if (pair? opts)
	  (if (eq? (car opts) name)
	      (cadr opts)
	      (loop (cddr opts)))
	  default)))

  (with-working-directory-pathname directory
    (lambda ()
      (let* ((os-type microcode-id/operating-system)
	     (pmodel (read-package-model name os-type))
	     (pathname (pmodel/pathname pmodel))
	     (dependencies (find-option 'dependencies options '()))
	     (syntax-only? (find-option 'syntax-only? options #f)))

	(define (env file)
	  (->environment
	   (let loop ((cps (pmodel/packages pmodel)))
	     (if (pair? cps)
		 (if (find (lambda (f) (pathname=? f file))
			   (package/files (car cps)))
		     (package/name (car cps))
		     (loop (cdr cps)))
		 (error "No cref-package for file:" file)))))

	(define (deps file)
	  (let ((entry (assoc file dependencies)))
	    (if entry (cdr entry) '())))

	(for-each (lambda (file.deps)
		    (if (not (for-all? string? file.deps))
			(error "Bogus dependency:" file.deps)))
		  dependencies)

	(let ((existing
	       (let loop ((packages (pmodel/packages pmodel)))
		 (if (pair? packages)
		     (or (name->package (package/name (car packages)))
			 (loop (cdr packages)))
		     #f))))
	  (if existing
	      (warn "Package already exists:" (package/name (car packages)))
	      ;; Build package(s) for use at syntax-time.
	      (construct-packages-from-file
	       (construct-external-descriptions pmodel))))

	(for-each
	  (lambda (file)
	    (compile-file file (deps file) (env file))
	    (load file))
	  (append-map package/files (pmodel/packages pmodel)))

	(cref/generate-constructors name 'ALL)))))