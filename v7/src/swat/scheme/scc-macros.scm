;;;; -*-Scheme-*-
;;; $Id: scc-macros.scm,v 1.3 2001/12/23 17:21:00 cph Exp $

(define-syntax define-constant
  (non-hygienic-macro-transformer
   (lambda (name value)
     `(DEFINE-INTEGRABLE ,name ,value))))

(define-syntax define-in-line
  (non-hygienic-macro-transformer
   (lambda (arg-list . body)
     `(DEFINE-INTEGRABLE ,arg-list . ,body))))

(define-syntax scc-define-syntax
  (non-hygienic-macro-transformer
   (lambda (name-and-arglist . body)
     (let ((name (car name-and-arglist))
	   (arglist (cdr name-and-arglist)))
       `(DEFINE-SYNTAX ,name
	  (NON-HYGIENIC-MACRO-TRANSFORMER
	   (LAMBDA ,arglist ,@body)))))))

(define-integrable *running-in-mit-scheme* #t)