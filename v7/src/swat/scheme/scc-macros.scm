;;;; -*-Scheme-*-
;;; $Id: scc-macros.scm,v 1.1 1995/08/02 21:26:49 adams Exp $

(syntax-table-define system-global-syntax-table
    'DEFINE-CONSTANT
  (macro (name value) `(DEFINE-INTEGRABLE ,name ,value)))

(syntax-table-define system-global-syntax-table
    'DEFINE-IN-LINE
  (macro (arg-list . body)
    `(DEFINE-INTEGRABLE ,arg-list . ,body)))

(syntax-table-define system-global-syntax-table
    'SCC-DEFINE-SYNTAX
  (macro (name-and-arglist . body)
    (let ((name (car name-and-arglist))
	  (arglist (cdr name-and-arglist)))
      `(SYNTAX-TABLE-DEFINE SYSTEM-GLOBAL-SYNTAX-TABLE
	   ',name
	 (MACRO ,arglist ,@body)))))

(define-integrable *running-in-mit-scheme* #T)
