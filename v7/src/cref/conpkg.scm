#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/cref/conpkg.scm,v 1.1 1988/06/13 12:38:19 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Generate construction program from package model

(declare (usual-integrations)
	 (integrate-external "object"))

;;; Construct expressions to construct the package structure.

(define (construct-constructor pmodel)
  (let ((packages (pmodel/packages pmodel)))
    `((DECLARE (USUAL-INTEGRATIONS))
      ,@(mapcan*
	 `((LET ()
	     (DECLARE (INTEGRATE-PRIMITIVE-PROCEDURES ENVIRONMENT-LINK-NAME))
	     ,@(mapcan* (mapcan construct-links (pmodel/extra-packages pmodel))
			construct-links packages)))
	 construct-definitions
	 (sort packages package-structure<?)))))

(define (construct-definitions package)
  (cond ((package/root? package)
	 `((IN-PACKAGE #F
	     ,@(map (lambda (binding) `(DEFINE ,(binding/name binding)))
		    (package/source-bindings package)))))
	((equal? (package/name package) '(PACKAGE))
	 ;; This environment is hand built by the cold-load.
	 '())
	(else
	 (package-definition
	  (package/name package)
	  `(IN-PACKAGE ,(package-reference (package/parent package))
	     (LET (,@(map (lambda (binding) `(,(binding/name binding)))
			  (package/source-bindings package)))
	       (THE-ENVIRONMENT)))))))

(define (construct-links package)
  (if (equal? (package/name package) '(PACKAGE))
      '()
      (mapcan (lambda (binding)
		(map (lambda (link)
		       (let ((source (link/source link))
			     (destination (link/destination link)))
			 `(ENVIRONMENT-LINK-NAME
			   ,(package-reference (binding/package destination))
			   ,(package-reference (binding/package source))
			   ',(binding/name source))))
		     (binding/links binding)))
	      (btree-fringe (package/bindings package)))))

(define (package/source-bindings package)
  (list-transform-positive (btree-fringe (package/bindings package))
    (lambda (binding)
      (eq? (binding/source-binding binding) binding))))

(define (package-structure<? x y)
  (cond ((package/topological<? x y) true)
	((package/topological<? y x) false)
	(else (package<? x y))))

(define (package/topological<? x y)
  (and (not (eq? x y))
       (let loop ((y (package/parent y)))
	 (and y
	      (if (eq? x y)
		  true
		  (loop (package/parent y)))))))

;;; Construct a procedure which will load the files into the package
;;; structure.

(define (construct-loader pmodel)
  `((DECLARE (USUAL-INTEGRATIONS))
    (LAMBDA (LOAD KEY-ALIST)
      (LET ((LOOKUP-KEY
	     (LAMBDA (KEY)
	       (LET LOOP ((ALIST KEY-ALIST))
		 (IF (NULL? ALIST)
		     (ERROR "Missing key" KEY))
		 (IF (EQ? KEY (CAR (CAR ALIST)))
		     (CDR (CAR ALIST))
		     (LOOP (CDR ALIST)))))))
	LOOKUP-KEY			;ignore if not referenced
	,@(mapcan (lambda (package)
		    (let ((reference (package-reference package)))
		      (if (> (package/n-files package) 1)
			  `((LET ((ENVIRONMENT ,reference))
			      ,@(load-package package 'ENVIRONMENT)))
			  (load-package package reference))))
		  (pmodel/packages pmodel))))))

(define (load-package package environment)
  (mapcan (lambda (file-case)
	    (let ((type (file-case/type file-case)))
	      (if type
		  `((CASE (LOOKUP-KEY ',type)
		      ,@(map (lambda (clause)
			       `(,(file-case-clause/keys clause)
				 ,@(clause-loader clause environment)))
			     (file-case/clauses file-case))))
		  (clause-loader (car (file-case/clauses file-case))
				 environment))))
	  (package/file-cases package)))

(define (clause-loader clause environment)
  (let ((files (file-case-clause/files clause)))
    (if (null? files)
	`(FALSE)
	(map (lambda (file)
	       `(LOAD ,(pathname->string file) ,environment))
	     files))))

(define (package-definition name value)
  (let ((path (reverse name)))
    `((PACKAGE/ADD-CHILD! (FIND-PACKAGE ',(reverse (cdr path)))
			  ',(car path)
			  ,value))))

(define (package-reference package)
  `(PACKAGE/ENVIRONMENT (FIND-PACKAGE ',(package/name package))))