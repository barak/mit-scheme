#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; Build utilities
;;; package: (cross-reference build-utilities)

(declare (usual-integrations))

(define (compile-system name directory . options)
  ;; Gets a list of files from DIRECTORY/NAME.pkg, creates the
  ;; packages described therein, and loads each file, in order,
  ;; re-compiling it first when necessary.
  ;;
  ;; If OPTIONS includes 'dependencies, its value should be an alist
  ;; of filenames, as they appear in the NAME.pkg file, each
  ;; associated with a list of pathnames (relative to DIRECTORY).
  ;;
  ;; If OPTIONS includes 'imports, its value should be a list,
  ;;
  ;;     ( (<target-package> <source-package> . <bound-names>)... )
  ;;
  ;; e.g. (((gtk keys) (ffi) find-c-includes c-enum-constant-values)).
  ;; Each symbol in <bound-names> is bound in the target package and
  ;; linked to the same name in the source package.  <target-package>
  ;; and <source-package> should be package names -- lists of symbols.

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
	     (dependencies (find-option 'dependencies options '()))
	     (imports (find-option 'imports options '())))

	(declare (integrate-operator file-package))
	(define (file-package file)
	  (let loop ((packages (pmodel/packages pmodel)))
	    (if (pair? packages)
		(if (find (lambda (f) (pathname=? f file))
			  (package/files (car packages)))
		    (car packages)
		    (loop (cdr packages)))
		(error "No cref package for file:" file pmodel))))

	(define-integrable (file-environment file)
	  (->environment (package/name (file-package file))))

	(define-integrable (file-dependencies file)
	  (let ((entry (assoc file dependencies)))
	    (if entry (cdr entry) '())))

	(for-each (lambda (file.deps)
		    (if (not (for-all? file.deps string?))
			(error "Bogus dependency:" file.deps)))
		  dependencies)

	(for-each (lambda (import)
		    (let ((target (car import))
			  (source (cadr import))
			  (names (cddr import))
			  (glt guarantee-list-of-type))
		      (declare (integrate glt))
		      (glt target symbol? "package name" 'compile-system)
		      (glt source symbol? "package name" 'compile-system)
		      (glt names symbol? "imported names" 'compile-system)))
		  imports)
    
	(let ((existing
	       (let loop ((packages (pmodel/packages pmodel)))
		 (if (pair? packages)
		     (or (name->package (package/name (car packages)))
			 (loop (cdr packages)))
		     #f))))
	  (if existing
	      (error "Package already exists:" existing)
	      (construct-packages-from-file
	       (construct-external-descriptions pmodel))))

	(for-each
	  (lambda (import)
	    (let ((target (->environment (car import)))
		  (source (->environment (cadr import)))
		  (names (cddr import)))
	      (for-each (lambda (name)
			 (environment-link-name target source name))
		       names)))
	  imports)

	(for-each
	  (lambda (file)
	    (let ((env (file-environment file))
		  (deps (file-dependencies file))
		  (type (if compile-file:sf-only? "bin" #f)))
	      (compile-file file deps env)
	      (load (pathname-new-type file type) env)))
	  (append-map package/files (pmodel/packages pmodel)))

	(cref/generate-constructors name 'ALL)))))