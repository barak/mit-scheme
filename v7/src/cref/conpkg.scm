#| -*-Scheme-*-

$Id: conpkg.scm,v 1.8 2001/08/09 03:06:12 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Generate construction program from package model

(declare (usual-integrations)
	 (integrate-external "object"))

;;; Construct expressions to construct the package structure.

(define (construct-constructor pmodel)
  (let ((packages (pmodel/packages pmodel)))
    ;; SYSTEM-GLOBAL-ENVIRONMENT is here so that it is not integrated.
    ;; This is necessary for cross-syntaxing when the representation of
    ;; #F, () or the system-global-environment changes.
    `((DECLARE (USUAL-INTEGRATIONS SYSTEM-GLOBAL-ENVIRONMENT))
      ,@(append-map*
	 (let ((links
		(append-map*
		 (append-map construct-links (pmodel/extra-packages pmodel))
		 construct-links packages)))
	   (if (pair? links)
	       `((LET ((LINK-VARIABLES
			(LET-SYNTAX
			    ((UCODE-PRIMITIVE
			      (MACRO (NAME ARITY)
				(MAKE-PRIMITIVE-PROCEDURE NAME ARITY))))
			  (UCODE-PRIMITIVE LINK-VARIABLES 4))))
		   ,@links))
	       '()))
	 construct-definitions
	 (sort packages package-structure<?)))))

(define (construct-definitions package)
  (cond ((package/root? package)
	 `((IN-PACKAGE SYSTEM-GLOBAL-ENVIRONMENT
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
      (append-map
       (lambda (binding)
	 (map (lambda (link)
		(let ((source (link/source link))
		      (destination (link/destination link)))
		  `(LINK-VARIABLES
		    ,(package-reference (binding/package destination))
		    ',(binding/name destination)
		    ,(package-reference (binding/package source))
		    ',(binding/name source))))
	      (binding/links binding)))
       (package/sorted-bindings package))))

(define (package/source-bindings package)
  (list-transform-positive (package/sorted-bindings package)
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
	,@(append-map (lambda (package)
			(let ((reference (package-reference package)))
			  (if (> (package/n-files package) 1)
			      `((LET ((ENVIRONMENT ,reference))
				  ,@(load-package package 'ENVIRONMENT)))
			      (load-package package reference))))
		      (pmodel/packages pmodel))))))

(define (load-package package environment)
  (append-map (lambda (file-case)
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
	       `(LOAD ,(->namestring file) ,environment))
	     files))))

(define (package-definition name value)
  (let ((path (reverse name)))
    `((PACKAGE/ADD-CHILD! (FIND-PACKAGE ',(reverse (cdr path)))
			  ',(car path)
			  ,value))))

(define (package-reference package)
  `(PACKAGE/ENVIRONMENT (FIND-PACKAGE ',(package/name package))))