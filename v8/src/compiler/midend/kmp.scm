#| -*-Scheme-*-

$Id: kmp.scm,v 1.1 1995/09/04 21:07:36 adams Exp $

Copyright (c) 1995 Massachusetts Institute of Technology

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
		  ((eq? (car args) '#!REST)
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
