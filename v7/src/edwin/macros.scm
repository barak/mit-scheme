;;; -*-Scheme-*-
;;;
;;; $Id: macros.scm,v 1.70 2001/12/23 17:20:58 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-1999, 2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Editor Macros

(declare (usual-integrations))

(define edwin-syntax-table (->environment '(EDWIN))) ;upwards compatibility

(define-syntax define-command
  (non-hygienic-macro-transformer
   (lambda (name description interactive procedure)
     (let ((name (canonicalize-name name)))
       (let ((scheme-name (command-name->scheme-name name)))
	 `(DEFINE ,scheme-name
	    (MAKE-COMMAND ',name
			  ,description
			  ,(if (null? interactive)
			       `'()
			       interactive)
			  ,(if (and (pair? procedure)
				    (eq? 'LAMBDA (car procedure))
				    (pair? (cdr procedure)))
			       `(NAMED-LAMBDA (,scheme-name
					       ,@(cadr procedure))
				  ,@(cddr procedure))
			       procedure))))))))

(define-syntax ref-command-object
  (non-hygienic-macro-transformer
   (lambda (name)
     (command-name->scheme-name (canonicalize-name name)))))

(define-syntax ref-command
  (non-hygienic-macro-transformer
   (lambda (name)
     `(COMMAND-PROCEDURE
       ,(command-name->scheme-name (canonicalize-name name))))))

(define-syntax command-defined?
  (non-hygienic-macro-transformer
   (lambda (name)
     (let ((variable-name
	    (command-name->scheme-name (canonicalize-name name))))
       `(LET ((_ENV (->ENVIRONMENT '(EDWIN))))
	  (AND (ENVIRONMENT-BOUND? _ENV ',variable-name)
	       (ENVIRONMENT-ASSIGNED? _ENV ',variable-name)))))))

(define (command-name->scheme-name name)
  (symbol-append 'EDWIN-COMMAND$ name))

(define-syntax define-variable
  (non-hygienic-macro-transformer
   (lambda args
     (apply (variable-definition #f) args))))

(define-syntax define-variable-per-buffer
  (non-hygienic-macro-transformer
   (lambda args
     (apply (variable-definition #t) args))))

(define (variable-definition buffer-local?)
  (lambda (name description #!optional value test normalization)
    (let ((name (canonicalize-name name)))
      (let ((scheme-name (variable-name->scheme-name name)))
	`(BEGIN
	   (DEFINE ,scheme-name
	     (MAKE-VARIABLE ',name
			    ,description
			    ,(if (default-object? value) '#F value)
			    ',buffer-local?))
	   ,@(if (default-object? test)
		 '()
		 `((SET-VARIABLE-VALUE-VALIDITY-TEST! ,scheme-name
						      ,test)))
	   ,@(if (default-object? normalization)
		 '()
		 `((SET-VARIABLE-VALUE-NORMALIZATION!
		    ,scheme-name
		    ,normalization))))))))

(define-syntax ref-variable-object
  (non-hygienic-macro-transformer
   (lambda (name)
     (variable-name->scheme-name (canonicalize-name name)))))

(define-syntax ref-variable
  (non-hygienic-macro-transformer
   (lambda (name #!optional buffer)
     (let ((name (variable-name->scheme-name (canonicalize-name name))))
       (if (default-object? buffer)
	   `(VARIABLE-VALUE ,name)
	   `(VARIABLE-LOCAL-VALUE ,buffer ,name))))))

(define-syntax set-variable!
  (non-hygienic-macro-transformer
   (lambda (name #!optional value buffer)
     (let ((name (variable-name->scheme-name (canonicalize-name name)))
	   (value (if (default-object? value) '#F value)))
       (if (default-object? buffer)
	   `(SET-VARIABLE-VALUE! ,name ,value)
	   `(SET-VARIABLE-LOCAL-VALUE! ,buffer ,name ,value))))))

(define-syntax local-set-variable!
  (non-hygienic-macro-transformer
   (lambda (name #!optional value buffer)
     `(DEFINE-VARIABLE-LOCAL-VALUE!
       ,(if (default-object? buffer) '(CURRENT-BUFFER) buffer)
       ,(variable-name->scheme-name (canonicalize-name name))
       ,(if (default-object? value) '#F value)))))

(define (variable-name->scheme-name name)
  (symbol-append 'EDWIN-VARIABLE$ name))

(define-syntax define-major-mode
  (non-hygienic-macro-transformer
   (lambda (name super-mode-name display-name description
		 #!optional initialization)
     (let ((name (canonicalize-name name))
	   (super-mode-name
	    (and super-mode-name (canonicalize-name super-mode-name))))
       `(DEFINE ,(mode-name->scheme-name name)
	  (MAKE-MODE ',name
		     #T
		     ',(or display-name (symbol->string name))
		     ,(if super-mode-name
			  `(->MODE ',super-mode-name)
			  `#F)
		     ,description
		     ,(let ((super-initialization
			     (and super-mode-name
				  `(MODE-INITIALIZATION
				    ,(mode-name->scheme-name
				      super-mode-name))))
			    (initialization
			     (and (not (default-object? initialization))
				  initialization)))
			(cond (super-initialization
			       `(LAMBDA (BUFFER)
				  (,super-initialization BUFFER)
				  ,@(if initialization
					`((,initialization BUFFER))
					`())))
			      (initialization)
			      (else
			       `(LAMBDA (BUFFER) BUFFER UNSPECIFIC))))))))))

(define-syntax define-minor-mode
  (non-hygienic-macro-transformer
   (lambda (name display-name description #!optional initialization)
     (let ((name (canonicalize-name name)))
       `(DEFINE ,(mode-name->scheme-name name)
	  (MAKE-MODE ',name
		     #F
		     ',(or display-name (symbol->string name))
		     #F
		     ,description
		     ,(if (and (not (default-object? initialization))
			       initialization)
			  initialization
			  `(LAMBDA (BUFFER) BUFFER UNSPECIFIC))))))))

(define-syntax ref-mode-object
  (non-hygienic-macro-transformer
   (lambda (name)
     (mode-name->scheme-name (canonicalize-name name)))))

(define (mode-name->scheme-name name)
  (symbol-append 'EDWIN-MODE$ name))

(define (canonicalize-name name)
  (cond ((symbol? name) name)
	((string? name) (intern (string-replace name #\Space #\-)))
	(else (error "illegal name" name))))