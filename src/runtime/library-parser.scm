#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

(add-boot-deps! '(runtime comparator))

;; Returns one of the following:
;; * Zero or more libraries, one or more imports, and a body.
;; * Zero or more libraries, no imports, and no body.
;; * #F, meaning this isn't R7RS source.
(define (read-r7rs-source pathname)
  (parameterize ((param:reader-fold-case? #f))
    (call-with-input-file (or (loadable-source-pathname pathname) pathname)
      (lambda (port)
	(%parse-r7rs-source (lambda () (read port)) pathname)))))

(define (parse-r7rs-source forms #!optional pathname)
  (%parse-r7rs-source (lambda ()
			(if (null-list? forms)
			    (eof-object)
			    (let ((next (car forms)))
			      (set! forms (cdr forms))
			      next)))
		       pathname))

(define (%parse-r7rs-source generator pathname)

  (define (read-libs libs)
    (let ((form (generator)))
      (cond ((eof-object? form)
	     (make-r7rs-source (reverse libs) #f))
	    ((r7rs-library? form)
	     (read-libs
	      (cons (parse-define-library-form form pathname)
		    libs)))
	    ((mit-library? form)
	     (read-libs
	      (cons (parse-define-library*-form form pathname)
		    libs)))
	    ((r7rs-import? form)
	     (read-imports (list (parse-import-form form))
			   (reverse libs)))
	    ((mit-import? form)
	     (read-imports (list (parse-import*-form form))
			   (reverse libs)))
	    ;; Not a valid R7RS file.
	    (else #f))))

  (define (read-imports imports libs)
    (let ((form (generator)))
      (if (eof-object? form)
	  (error "EOF while reading imports"))
      (if (or (r7rs-library? form)
	      (mit-library? form))
	  (error "Can't mix libraries and imports:" form))
      (cond ((r7rs-import? form)
	     (read-imports (cons (parse-import-form form) imports) libs))
	    ((mit-import? form)
	     (read-imports (cons (parse-import*-form form) imports) libs))
	    (else
	     (make-r7rs-source
	      libs
	      ;; A program is simply an anonymous library.
	      (make-library #f
			    'parsed-imports (reverse imports)
			    'parsed-exports '()
			    'parsed-contents (read-body (list form))
			    'filename (if (default-object? pathname)
					  #f
					  (->namestring pathname))))))))

  (define (read-body forms)
    (let ((form (generator)))
      (if (eof-object? form)
	  `((begin ,@(reverse forms)))
	  (read-body (cons form forms)))))

  (read-libs '()))

(define (r7rs-library? object)
  (and (pair? object)
       (eq? 'define-library (car object))))

(define (r7rs-import? object)
  (and (pair? object)
       (eq? 'import (car object))))

(define (mit-library? object)
  (and (pair? object)
       (eq? 'define-library* (car object))))

(define (mit-import? object)
  (and (pair? object)
       (eq? 'import* (car object))))

(define-record-type <r7rs-source>
    (make-r7rs-source libraries program)
    r7rs-source?
  (libraries r7rs-source-libraries)
  (program r7rs-source-program))

(define (r7rs-source-elements source)
  (let ((libraries (r7rs-source-libraries source))
	(program (r7rs-source-program source)))
    (if program
	(append libraries (list program))
	libraries)))

(define (register-r7rs-source! source db)
  (register-libraries! (r7rs-source-libraries source) db)
  (let ((program (r7rs-source-program source)))
    (and program
	 (register-library! program db))))

(define (top-level-define-library-parser parser)
  (lambda (form #!optional pathname)
    (let ((directory
	   (if (default-object? pathname)
	       (working-directory-pathname)
	       (directory-pathname pathname))))
      (let ((result (parser form)))
	(and result
	     (let ((part
		    (partition-decls
		     (expand-parsed-decls (cdr result) directory))))
	       (make-library (car result)
			     'parsed-imports (part 'import)
			     'parsed-exports (part 'export)
			     'parsed-defines (part 'define)
			     'parsed-contents (part 'content)
			     'filename (if (default-object? pathname)
					   #f
					   (->namestring pathname)))))))))

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
		    (evaluate-cond-expand
		     eq?
		     (filter (lambda (clause) (not (eq? 'else (car clause))))
			     (cdr parsed-decl))
		     (cond ((find (lambda (clause) (eq? 'else (car clause)))
				  (cdr parsed-decl))
			    => cdr)
			   (else '())))
		    directory))
		  ((include include-ci)
		   (list
		    (cons (car parsed-decl)
			  (map (lambda (p)
				 (merge-pathnames p directory))
			       (cdr parsed-decl)))))
		  (else
		   (list parsed-decl))))
	      parsed-decls))

(define (get-library-declarations pathname)
  (cdr (%parse-define-library (call-with-input-file pathname read))))

(define-deferred partition-decls
  (partition-generator (lambda (decl)
			 (case (car decl)
			   ((r7rs-import mit-import) 'import)
			   ((r7rs-export mit-export) 'export)
			   ((mit-define) 'define)
			   (else 'content)))
		       (make-eq-comparator)
		       cons-last!
		       '()))

(define (pathname-parser object win lose)
  (let ((pathname
         (ignore-errors
          (lambda ()
            (parse-namestring object)))))
    (if (not (pathname? pathname))
        (error "Unrecognized pathname:" object))
    (win (structure-parser-values pathname)
         lose)))

(define r7rs-include-parser
  (object-parser
   (encapsulate list
     (list (alt (match include) (match include-ci))
	   (* (object pathname-parser))))))

(define r7rs-include-decls-parser
  (object-parser
   (encapsulate list
     (list (match include-library-declarations)
           (* (object pathname-parser))))))

(define r7rs-begin-parser
  (object-parser
    (encapsulate list
      (list (match begin)
	    (* (match-any))))))

(define (cond-expand-parser decl-parser)
  (define clause-parser
    (object-parser
      (encapsulate list
	(list (object feature-requirement-parser)
	      (* (object decl-parser))))))
  (object-parser
    (encapsulate list
      (list (match cond-expand)
	    (* (object clause-parser))))))

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

(define define-library-parser
  (object-parser
    (encapsulate list
      (list 'define-library
	    (match-if library-name?)
	    (* (object r7rs-declaration-parser))))))

(define r7rs-declaration-parser
  (object-parser
    (alt r7rs-export-parser
	 r7rs-import-parser
	 r7rs-include-parser
	 r7rs-include-decls-parser
	 r7rs-begin-parser
	 r7rs-cond-expand-parser)))

(define r7rs-export-parser
  (object-parser
   (encapsulate list
     (list 'export
	   (values 'r7rs-export)
           (* (object r7rs-export-spec-parser))))))

(define r7rs-export-spec-parser
  (object-parser
    (alt (match-if symbol?)
	 (encapsulate list
	   (list (match rename)
		 (match-if symbol?)
		 (match-if symbol?))))))

(define r7rs-import-parser
  (object-parser
   (encapsulate list
     (list 'import
	   (values 'r7rs-import)
           (* (object r7rs-import-set-parser))))))

(define r7rs-import-set-parser
  (object-parser
   (alt (match-if library-name?)
	(encapsulate list
	  (alt (list (alt (match only) (match except))
		     (object r7rs-import-set-parser)
		     (* (match-if symbol?)))
	       (list (match prefix)
		     (object r7rs-import-set-parser)
		     (match-if symbol?))
	       (list (match rename)
		     (object r7rs-import-set-parser)
		     (* (encapsulate list
			  (list (match-if symbol?)
				(match-if symbol?))))))))))

(define r7rs-cond-expand-parser
  (cond-expand-parser r7rs-declaration-parser))

(define define-library*-parser
  (object-parser
    (encapsulate list
      (list 'define-library*
	    (match-if library-name?)
	    (* (object mit-declaration-parser))))))

(define mit-declaration-parser
  (object-parser
    (alt mit-define-parser
	 mit-export-parser
	 mit-export-to-parser
	 mit-import-parser
	 mit-cond-expand-parser
	 r7rs-include-parser
	 r7rs-begin-parser)))

(define mit-define-parser
  (object-parser
   (encapsulate list
     (list 'define
	   (values 'mit-define)
	   (match-if symbol?)
	   (object mit-nameset-parser)))))

(define mit-export-parser
  (object-parser
    (encapsulate list
      (list 'export
	    (values 'mit-export #f)
	    (* (object mit-inclusion-parser))))))

(define mit-export-to-parser
  (object-parser
    (encapsulate list
      (list 'export-to
	    (values 'mit-export-to)
	    (match-if library-name?)
	    (* (object mit-inclusion-parser))))))

(define mit-import-parser
  (object-parser
    (encapsulate list
      (list 'import
	    (values 'mit-import)
	    (* (object mit-import-set-parser))))))

(define import*-parser
  (object-parser
   (encapsulate list
     (list 'import*
	   (values 'mit-import)
	   (* (object mit-import-set-parser))))))

(define mit-cond-expand-parser
  (cond-expand-parser mit-declaration-parser))

(define mit-import-set-parser
  (object-parser
    (alt (match-if library-name?)
	 (encapsulate list
	   (alt (list (match take)
		      (match-if library-name?)
		      (+ (object mit-inclusion-parser)))
		(list (match drop)
		      (match-if library-name?)
		      (+ (object mit-exclusion-parser))))))))

(define mit-inclusion-parser
  (object-parser
    (alt (encapsulate (lambda (nameset) (cons 'nameset nameset))
	   mit-nameset-parser)
	 mit-match-parser
	 mit-rename-parser)))

(define mit-exclusion-parser
  (object-parser
    (alt (encapsulate (lambda (nameset) (cons 'nameset nameset))
	   mit-nameset-parser)
	 mit-match-parser)))

(define mit-match-parser
  (object-parser
    (encapsulate (lambda (sre) (cons 're-match sre))
      (match-if valid-sre?))))

(define mit-rename-parser
  (object-parser
    (encapsulate list
      (alt (list (values 'rename)
		 (match-if symbol?)
		 (match-if symbol?))
	   (list (values 're-rename)
		 (match-if valid-sre?)
		 (match-if regexp-match-replacement?))))))

(define mit-nameset-parser
  (object-parser
    (alt (match-if symbol?)
	 (encapsulate list
	   (alt (list (match exports)
		      (match-if library-name?))
		(list (alt (match intersection) (match union))
		      (* (object mit-nameset-parser)))
		(list (match difference)
		      (object mit-nameset-parser)
		      (object mit-nameset-parser))
		(list (alt (match re-filter) (match re-remove))
		      (match-if valid-sre?)
		      (object mit-nameset-parser))
		(list (match re-rename)
		      (match-if valid-sre?)
		      (match-if regexp-match-replacement?)
		      (object mit-nameset-parser)))))))

(define (wrap-parser parser description)
  (let ((message (string-append "Unable to parse " description ":")))
    (lambda (object)
      (let ((result (apply-object-parser parser object)))
	(if (not result)
	    (error message object))
	(car result)))))

(define %parse-define-library
  (wrap-parser define-library-parser "define-library form"))

(define parse-define-library-form
  (top-level-define-library-parser %parse-define-library))

(define parse-define-library*-form
  (top-level-define-library-parser
   (wrap-parser define-library*-parser "define-library* form")))

(define parse-import-form
  (wrap-parser r7rs-import-parser "import form"))

(define parse-import*-form
  (wrap-parser mit-import-parser "import* form"))

(define general-import-set-parser
  (object-parser
    (alt (encapsulate (lambda (set) `(r7rs-import ,set))
	   r7rs-import-set-parser)
	 (encapsulate (lambda (set) `(mit-import ,set))
	   mit-import-set-parser))))

(define parse-import-set
  (wrap-parser general-import-set-parser "import set"))

(define (library-name? object)
  (and (pair? object)
       (list? (cdr object))
       (every library-name-elt? object)))
(register-predicate! library-name? 'library-name)

(define (library-name-elt? object)
  (or (interned-symbol? object)
      (exact-nonnegative-integer? object)))
(register-predicate! library-name-elt? 'library-name-element)

(define (library-name=? n1 n2)
  (guarantee library-name? n1 'library-name=?)
  (guarantee library-name? n2 'library-name=?)
  (and (= (length n1) (length n2))
       (every library-name-elt=? n1 n2)))

(define (library-name-elt=? e1 e2)
  (eqv? e1 e2))

(define (library-name<? n1 n2)
  (guarantee library-name? n1 'library-name<?)
  (guarantee library-name? n2 'library-name<?)
  (let loop ((n1 n1) (n2 n2))
    (and (pair? n1)
	 (pair? n2)
	 (if (eqv? (car n1) (car n2))
	     (loop (cdr n1) (cdr n2))
	     (library-name-elt<? (car n1) (car n2))))))

(define (library-name-elt<? e1 e2)
  (if (interned-symbol? e1)
      (if (interned-symbol? e2) (symbol<? e1 e2) #t)
      (if (interned-symbol? e2) #f (< e1 e2))))

(define (library-name-adjoin name names)
  (if (member name names library-name=?)
      names
      (cons name names)))