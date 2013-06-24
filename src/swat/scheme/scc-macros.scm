;;;; -*-Scheme-*-
;;; $Id: scc-macros.scm,v 1.4 2002/02/03 03:38:58 cph Exp $

(define-syntax define-constant
  define-integrable)

(define-syntax define-in-line
  define-integrable)

(define-integrable *running-in-mit-scheme* #t)