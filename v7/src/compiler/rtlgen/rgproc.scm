#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgproc.scm,v 1.5 1987/06/23 03:31:43 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; RTL Generation: Procedure Headers

(declare (usual-integrations))

(package (generate/procedure-header)

(define-export (generate/procedure-header procedure body)
  (scfg*scfg->scfg!
   (if (procedure/ic? procedure)
       (setup-ic-frame procedure)
       (scfg*scfg->scfg!
	((if (or (procedure-rest procedure)
		 (and (procedure/closure? procedure)
		      (not (null? (procedure-optional procedure)))))
	     rtl:make-setup-lexpr
	     rtl:make-procedure-heap-check)
	 procedure)
	(setup-stack-frame procedure)))
   body))

(define (setup-ic-frame procedure)
  (scfg-append!
   (rtl:make-procedure-heap-check procedure)
   (rtl:make-assignment register:frame-pointer
			(rtl:make-fetch register:stack-pointer))
   (scfg*->scfg!
    (map (let ((block (procedure-block procedure)))
	   (lambda (name value)
	     (transmit-values (generate/rvalue value)
	       (lambda (prefix expression)
		 (scfg*scfg->scfg!
		  prefix
		  (rtl:make-interpreter-call:set!
		   (rtl:make-fetch register:environment)
		   (intern-scode-variable! block (variable-name name))
		   expression))))))
	 (procedure-names procedure)
	 (procedure-values procedure)))))

(define (setup-stack-frame procedure)
  (let ((block (procedure-block procedure)))
    (define (cellify-variables variables)
      (scfg*->scfg! (map cellify-variable variables)))

    (define (cellify-variable variable)
      (if (variable-in-cell? variable)
	  (let ((locative
		 (stack-locative-offset (rtl:make-fetch register:frame-pointer)
					(variable-offset block variable))))
	    (rtl:make-assignment
	     locative
	     (rtl:make-cell-cons (rtl:make-fetch locative))))
	  (make-null-cfg)))

    (let ((names (procedure-names procedure))
	  (values (procedure-values procedure)))
      (scfg-append! (setup-bindings names values '())
		    (setup-auxiliary (procedure-auxiliary procedure) '())
		    (rtl:make-assignment
		     register:frame-pointer
		     (rtl:make-fetch register:stack-pointer))
		    (cellify-variables (procedure-required procedure))
		    (cellify-variables (procedure-optional procedure))
		    (let ((rest (procedure-rest procedure)))
		      (if rest
			  (cellify-variable rest)
			  (make-null-cfg)))
		    (scfg*->scfg!
		     (map (lambda (name value)
			    (if (and (procedure? value)
				     (procedure/closure? value))
				(letrec-close block name value)
				(make-null-cfg)))
			  names values))))))

(define (setup-bindings names values pushes)
  (if (null? names)
      (scfg*->scfg! pushes)
      (setup-bindings (cdr names)
		      (cdr values)
		      (cons (make-auxiliary-push (car names)
						 (letrec-value (car values)))
			    pushes))))

(define (letrec-value value)
  (cond ((constant? value)
	 (rtl:make-constant (constant-value value)))
	((procedure? value)
	 (case (procedure/type value)
	   ((CLOSURE)
	    (make-closure-cons value (rtl:make-constant '())))
	   ((IC)
	    (make-ic-cons value))
	   ((OPEN-EXTERNAL OPEN-INTERNAL)
	    (error "Letrec value is open procedure" value))
	   (else
	    (error "Unknown procedure type" value))))
	(else
	 (error "Unknown letrec binding value" value))))

(define (letrec-close block variable value)
  (transmit-values (make-closure-environment value)
    (lambda (prefix environment)
      (scfg*scfg->scfg! prefix
			(rtl:make-assignment
			 (closure-procedure-environment-locative
			  (find-variable block variable
			    (lambda (locative) locative)
			    (lambda (nearest-ic-locative name)
			      (error "Missing closure variable" variable))
			    (lambda (name)
			      (error "Missing closure variable" variable))))
			 environment)))))

(define (setup-auxiliary variables pushes)
  (if (null? variables)
      (scfg*->scfg! pushes)
      (setup-auxiliary (cdr variables)
		       (cons (make-auxiliary-push (car variables)
						  (rtl:make-unassigned))
			     pushes))))

(define (make-auxiliary-push variable value)
  (rtl:make-push (if (variable-in-cell? variable)
		     (rtl:make-cell-cons value)
		     value)))

;;; end GENERATE/PROCEDURE-HEADER
)