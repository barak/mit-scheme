#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; R7RS libraries: define-library parser
;;; package: (runtime library parser)

(declare (usual-integrations))

(define (parse-define-library-form form #!optional pathname)
  (let ((directory
	 (if (default-object? pathname)
	     (working-directory-pathname)
	     (directory-pathname pathname))))
    (let ((result (%parse-define-library form)))
      (and result
	   (let loop
	       ((decls (expand-parsed-decls (cdr result) pathname))
		(exports '())
		(imports '())
		(contents '()))
	     (if (pair? decls)
		 (let ((decl (car decls))
		       (decls (cdr decls)))
		   (case (car decl)
		     ((export)
		      (loop decls
			    (append (reverse (cdr decl)) exports)
			    imports
			    contents))
		     ((import)
		      (loop decls
			    exports
			    (append (reverse (cdr decl)) imports)
			    contents))
		     (else
		      (loop decls
			    exports
			    imports
			    (append (reverse (cdr decl)) contents)))))
		 (make-parsed-library (car result)
				      (reverse exports)
				      (reverse imports)
				      (reverse contents))))))))

(define-record-type <parsed-library>
    (make-parsed-library name exports imports contents)
    parsed-library?
  (name parsed-library-name)
  (exports parsed-library-exports)
  (imports parsed-library-imports)
  (contents parsed-library-contents))

(define (expand-parsed-decls parsed-decls directory)
  (append-map (lambda (parsed-decl)
		(case (car parsed-decl)
		  ((include-library-declarations)
		   (append-map (lambda (pathname)
				 (let ((pathname*
					(merge-pathnames pathname directory)))
				   (expand-parsed-decls
				    (get-library-declarations pathname*)
				    (directory-pathname pathname*))))
			       (cdr parsed-decl)))
		  ((cond-expand)
		   (expand-parsed-decls
		    (evaluate-cond-expand eq? parsed-decl)))
		  ((include include-ci)
		   (list
		    (cons (car parsed-decl)
			  (map (lambda (pathname)
				 (merge-pathnames pathname directory))
			       (cdr parsed-decl)))))
		  (else
		   (list parsed-decl))))
	      parsed-decls))

(define (get-library-declarations pathname)
  (cdr
   (%parse-define-library
    (call-with-input-file (pathname-default-type pathname "scm") read))))

(define define-library-parser
  (object-parser
    (encapsulate list
      (list 'define-library
	    (object (alt (match library-name?)
			 (sexp (parsing-error "library name"))))
	    library-declarations-parser))))

(define library-declarations-parser
  (list-parser
    (* (object (alt library-declaration-parser
		    (sexp (parsing-error "library declaration")))))))

(define library-declaration-parser
  (object-parser
    (alt export-parser
	 import-parser
	 include-parser
	 begin-parser
	 cond-expand-parser)))

(define export-parser
  (object-parser
   (encapsulate list
     (list (match export)
           (* (object export-spec-parser))))))

(define export-spec-parser
  (object-parser
   (alt (encapsulate (lambda (name)
                       (cons name name))
                     (match-if symbol?))
        (encapsulate cons
	  (list 'rename
		(match-if symbol?)
		(match-if symbol?)))
        (sexp (parsing-error "export spec")))))

(define import-parser
  (object-parser
   (encapsulate list
     (list (match import)
           (* (object import-set-parser))))))

(define import-set-parser
  (object-parser
   (alt (encapsulate (lambda (library-name) (list 'library library-name))
          (match-if library-name?))
        (encapsulate list
          (alt (list (alt (match only) (match except))
                     (object import-set-parser)
                     (* (match-if symbol?)))
               (list (match prefix)
                     (object import-set-parser)
                     (match-if symbol?))
               (list (match rename)
                     (object import-set-parser)
                     (* (encapsulate cons
                          (list (match-if symbol?)
                                (match-if symbol?)))))))
        (sexp (parsing-error "import set")))))

(define include-parser
  (object-parser
   (encapsulate (lambda (keyword . pathnames)
                  (cons 'include
                        (map (lambda (pathname)
			       (list pathname keyword))
                             pathnames)))
     (list (alt (match include) (match include-ci))
           (* (object pathname-parser))))))

(define include-library-declarations-parser
  (object-parser
   (encapsulate list
     (list (match include-library-declarations)
           (* (object pathname-parser))))))

(define (pathname-parser object win lose)
  (let ((pathname
         (ignore-errors
          (lambda ()
            (merge-pathnames object)))))
    (if (not (pathname? pathname))
        (error "Unrecognized pathname:" object))
    (win (structure-parser-values pathname)
         lose)))

(define begin-parser
  (object-parser
    (encapsulate list
      (list (match begin)
	    (* (match-any))))))

(define cond-expand-parser
  (object-parser
    (encapsulate list
      (list (match cond-expand)
	    (* (object cond-expand-clause-parser))))))

(define cond-expand-clause-parser
  (object-parser
    (encapsulate list
      (list (object feature-requirement-parser)
	    library-declarations-parser))))

(define feature-requirement-parser
  (object-parser
    (alt (match-if symbol?)
	 (encapsulate list
	   (list (alt (match or) (match and))
		 (* (object feature-requirement-parser))))
	 (encapsulate list
	   (list (match not)
		 (object feature-requirement-parser)))
	 (encapsulate list
	   (list (match library)
		 (match-if library-name?))))))

(define (wrap-parser parser description)
  (let ((message (string-append "Unable to parse " description ":")))
    (lambda (object)
      (let ((result (apply-object-parser parser object)))
	(if (not result)
	    (error message object))
	(car result)))))

(define %parse-define-library
  (wrap-parser define-library-parser "define-library form"))

(define parse-import-form
  (wrap-parser import-parser "import form"))

(define parse-import-set
  (wrap-parser import-set-parser "import set"))

(define (parsing-error description)
  (lambda (object win lose)
    (win (error (string-append "Unrecognized " description ":") object)
         lose)))

(define (library-name? object)
  (and (list? object)
       (every (lambda (elt)
		(or (interned-symbol? elt)
		    (exact-nonnegative-integer? elt)))
	      object)))