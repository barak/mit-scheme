#| -*-Scheme-*-

Copyright (c) 1995, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; KMP scheme syntax
;;; package: (compiler midend)

(declare (usual-integrations))

;;______________________________________________________________________
;;
;;  Syntax abstractions
;;______________________________________________________________________

(let-syntax
    ((kmp-form-accessors
      (macro (name . args)
	(define (->string x)  (if (symbol? x) (symbol-name x) x))
	(define (->sym . stuff)
	  (intern (apply string-append (map ->string stuff))))
	(define (loop  args path defs)
	  (define (add-def field path)
	    (let  ((base-name    (->sym name "/" field))
		   (safe-name    (->sym name "/" field "/safe"))
		   (unsafe-name  (->sym name "/" field "/unsafe")))
	      (cons* `(DEFINE-INTEGRABLE (,base-name FORM)
			(,safe-name FORM))
		     `(DEFINE-INTEGRABLE (,unsafe-name FORM)
			,path)
		     `(DEFINE            (,safe-name FORM)
			(IF (AND (PAIR? FORM)
				 (EQ? (CAR FORM) ',name))
			    ,path
			    (INTERNAL-ERROR "Illegal KMP syntax" ',name FORM)))
		     defs)))
	    (cond ((null? args)
		   defs)
		  ((eq? (car args) #!rest)
		   (add-def (cadr args) path))
		  ((eq? (car args) '#F)
		   (loop (cdr args) `(CDR ,path) defs))
		  (else
		   (loop (cdr args)
			 `(CDR ,path)
			 (add-def (car args) `(CAR ,path))))))
	  `(BEGIN 1			;bogon for 0 defs
		  ,@(reverse (loop args `(CDR FORM) '())))))

     (alternate-kmp-form
      (macro (name . args)
	`(kmp-form-accessors ,name . ,args)))
     (kmp-form
      (macro (name . args)
	`(BEGIN (DEFINE-INTEGRABLE (,(symbol-append name '/?) FORM)
		  (AND (PAIR? FORM)
		       (EQ? (CAR FORM) ',name)))
		(kmp-form-accessors ,name . ,args)))))  

  ;; Generate KMP accessors like QUOTE/TEXT (doesn't check head of
  ;; form) and QUOTE/TEXT/SAFE (requires head of form to be QUOTE)

  (kmp-form QUOTE   text)
  (kmp-form LOOKUP  name)
  (kmp-form LAMBDA  formals body)
  (kmp-form LET     bindings body)
  (kmp-form DECLARE #!rest declarations)
  (kmp-form CALL    operator continuation #!rest operands)
  (alternate-kmp-form
            CALL    #F #!rest cont-and-operands)
  (kmp-form BEGIN   #!rest exprs)	; really 1 or more
  (kmp-form IF      predicate consequent alternate)
  (kmp-form LETREC  bindings body)

  (kmp-form SET!    name expr)
  (kmp-form ACCESS  name env-expr)
  (kmp-form DEFINE  name expr)
  (kmp-form THE-ENVIRONMENT)
  (kmp-form IN-PACKAGE env-expr expr)
  )

(define-integrable if/alternative if/alternate)
(define-integrable (call/operand1 form)  (first  (call/operands form)))
(define-integrable (call/operand2 form)  (second (call/operands form)))
(define-integrable (call/operand3 form)  (third  (call/operands form)))
