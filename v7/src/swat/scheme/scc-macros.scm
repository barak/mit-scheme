;;;; -*-Scheme-*-
;;; $Id: scc-macros.scm,v 1.2 2001/12/20 06:43:25 cph Exp $

(syntax-table/define system-global-environment 'DEFINE-CONSTANT
  (lambda (name value)
    `(DEFINE-INTEGRABLE ,name ,value)))

(syntax-table/define system-global-environment 'DEFINE-IN-LINE
  (lambda (arg-list . body)
    `(DEFINE-INTEGRABLE ,arg-list . ,body)))

(syntax-table/define system-global-environment 'SCC-DEFINE-SYNTAX
  (lambda (name-and-arglist . body)
    (let ((name (car name-and-arglist))
	  (arglist (cdr name-and-arglist)))
      `(SYNTAX-TABLE/DEFINE SYSTEM-GLOBAL-ENVIRONMENT ',name
	 (LAMBDA ,arglist ,@body)))))

(define-integrable *running-in-mit-scheme* #t)