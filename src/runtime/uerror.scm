#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Microcode Errors
;;; package: (runtime microcode-errors)

(declare (usual-integrations))

(define condition-type:anomalous-microcode-error)
(define condition-type:compiled-code-error)
(define condition-type:fasdump-environment)
(define condition-type:fasl-file-bad-data)
(define condition-type:fasl-file-compiled-mismatch)
(define condition-type:fasl-file-too-big)
(define condition-type:fasload-band)
(define condition-type:fasload-error)
(define condition-type:hardware-trap)
(define condition-type:inapplicable-object)
(define condition-type:out-of-file-handles)
(define condition-type:primitive-io-error)
(define condition-type:primitive-procedure-error)
(define condition-type:system-call-error)
(define condition-type:unimplemented-primitive)
(define condition-type:unimplemented-primitive-for-os)
(define condition-type:user-microcode-reset)
(define condition-type:wrong-arity-primitives)

(define error-handler-vector)
(define default-error-handler)
(define unknown-error-names)

(define (define-error-handler error-name handler)
  (let ((error-code (microcode-error/name->code error-name)))
    (if error-code
	(vector-set! error-handler-vector
		     error-code
		     (lambda (error-code interrupt-enables)
		       (set-interrupt-enables! interrupt-enables)
		       (call-with-current-continuation
			(lambda (continuation)
			  (handler continuation)
			  (default-error-handler continuation error-code)))))
	(begin
	  (set! unknown-error-names (cons error-name unknown-error-names))
	  unspecific))))

(define (define-low-level-handler error-name handler)
  (vector-set! error-handler-vector
	       (microcode-error/name->code error-name)
	       (lambda (error-code interrupt-enables)
		 (set-interrupt-enables! interrupt-enables)
		 (call-with-current-continuation
		  (lambda (continuation)
		    (handler continuation error-code)
		    (default-error-handler continuation error-code))))))

(define (condition-signaller type field-names)
  (let ((make-condition (condition-constructor type field-names)))
    (lambda (continuation . field-values)
      (error (apply make-condition
		    (cons* continuation
			   'BOUND-RESTARTS
			   field-values))))))

;;;; Restart Bindings

(define (unbound-variable/store-value continuation environment name thunk)
  (with-restart 'STORE-VALUE
      (lambda (port)
	(write-string "Define " port)
	(write name port)
	(write-string " to a given value." port))
      (lambda (value)
	(environment-define environment name value)
	(continuation unspecific))
      (let ((prompt (string-append "Define " (write-to-string name) " as")))
	(lambda ()
	  (values (prompt-for-evaluated-expression prompt environment))))
    thunk))

(define (unassigned-variable/store-value continuation environment name thunk)
  (with-restart 'STORE-VALUE
      (lambda (port)
	(write-string "Set " port)
	(write name port)
	(write-string " to a given value." port))
      (lambda (value)
	(environment-assign! environment name value)
	(continuation unspecific))
      (let ((prompt (string-append "Set " (write-to-string name) " to")))
	(lambda ()
	  (values (prompt-for-evaluated-expression prompt environment))))
    thunk))

(define (variable/use-value continuation environment name thunk)
  (let ((continuation (continuation/next-continuation continuation)))
    (if (continuation-restartable? continuation)
	(with-restart 'USE-VALUE
	    (lambda (port)
	      (write-string "Specify a value to use instead of " port)
	      (write name port)
	      (write-string "." port))
	    continuation
	    (let ((prompt
		   (string-append "Value to use instead of "
				  (write-to-string name))))
	      (lambda ()
		(values
		 (prompt-for-evaluated-expression prompt environment))))
	  thunk)
	(thunk))))

(define (inapplicable-object/use-value continuation operands thunk)
  (let ((continuation (continuation/next-continuation continuation)))
    (if (continuation-restartable? continuation)
	(with-restart 'USE-VALUE "Specify a procedure to use in its place."
	    (lambda (operator)
	      (within-continuation continuation
		(lambda ()
		  (apply operator operands))))
	    (lambda ()
	      (values (prompt-for-evaluated-expression "New procedure")))
	  thunk)
	(thunk))))

(define (illegal-arg-signaller type)
  (let ((signal (condition-signaller type '(DATUM OPERATOR OPERAND))))
    (lambda (continuation operator operands index)
      (illegal-argument/use-value continuation operator operands index
	(lambda ()
	  (signal continuation (list-ref operands index) operator index))))))

(define (illegal-argument/use-value continuation operator operands index thunk)
  (let ((continuation
	 (continuation/next-continuation/no-compiled-code continuation)))
    (if (continuation-restartable? continuation)
	(with-restart 'USE-VALUE "Specify an argument to use in its place."
	    (lambda (operand)
	      (within-continuation continuation
		(lambda ()
		  (apply operator
			 (substitute-element operands index operand)))))
	    (lambda ()
	      (values (prompt-for-evaluated-expression "New argument")))
	  thunk)
	(thunk))))

(define (file-operation-signaller)
  (let ((signal
	 (condition-signaller condition-type:file-operation-error
			      '(FILENAME VERB NOUN REASON OPERATOR OPERANDS))))
    (lambda (continuation operator operands index verb noun reason)
      (file-operation/use-value continuation operator operands index verb noun
	(lambda ()
	  (file-operation/retry continuation operator operands verb noun
	    (lambda ()
	      (signal continuation (list-ref operands index)
		      verb noun reason operator operands))))))))

(define (file-operation/use-value continuation operator operands index
				  verb noun thunk)
  (let ((continuation (continuation/next-continuation continuation)))
    (if (continuation-restartable? continuation)
	(with-restart 'USE-VALUE
	    (string-append "Try to " verb " a different " noun ".")
	    (lambda (operand)
	      (within-continuation continuation
		(lambda ()
		  (apply operator
			 (substitute-element operands index operand)))))
	    (let ((prompt
		   (string-append "New "
				  noun
				  " name (an expression to be evaluated)")))
	      (lambda ()
		(values (prompt-for-evaluated-expression prompt))))
	  thunk)
	(thunk))))

(define (file-operation/retry continuation operator operands verb noun thunk)
  (let ((continuation (continuation/next-continuation continuation)))
    (if (continuation-restartable? continuation)
	(with-restart 'RETRY
	    (string-append "Try to " verb " the same " noun " again.")
	    (lambda ()
	      (within-continuation continuation
		(lambda ()
		  (apply operator operands))))
	    values
	  thunk)
	(thunk))))

(define (substitute-element list index element)
  (let loop ((list list) (i 0))
    (if (= i index)
	(cons element (cdr list))
	(cons (car list) (loop (cdr list) (+ i 1))))))

;;;; Continuation Parsing

(define (continuation/next-continuation continuation)
  (let ((first-subproblem (continuation/first-subproblem continuation)))
    (and first-subproblem
	 (let ((next-subproblem (stack-frame/next first-subproblem)))
	   (and next-subproblem
		(stack-frame->continuation next-subproblem))))))

;; With the 8.0 compiler, we do not want to restart a primitive that
;; signalled a bad argument type or range.  This allows the compiler
;; to generate better code. We return #F if the continuation is an
;; apply frame of a primitive called from compiled code:

(define (continuation/next-continuation/no-compiled-code continuation)
  (let ((first-subproblem (continuation/first-subproblem continuation)))
    (and first-subproblem
	 (let ((next-subproblem (stack-frame/next first-subproblem)))
	   (and next-subproblem
		(if (and (apply-frame? first-subproblem)
			 (primitive-procedure?
			  (apply-frame/operator first-subproblem))
			 (let ((further-subproblem
				(stack-frame/next next-subproblem)))
			   (stack-frame/compiled-code? further-subproblem)))
		    #F
		    (stack-frame->continuation next-subproblem)))))))

(define (continuation-restartable? continuation)
  continuation)

(define-integrable (frame/type frame)
  (microcode-return/code->name (stack-frame/return-code frame)))

(define (apply-frame? frame)
  (let ((code (stack-frame/return-code frame)))
    (and code
	 (or (= return-code:internal-apply code)
	     (= return-code:internal-apply-val code)))))

(define-integrable (apply-frame/operator frame)
  (stack-frame/ref frame 3))

(define-integrable (apply-frame/operand frame index)
  (stack-frame/ref frame (+ 4 index)))

(define (apply-frame/operands frame)
  (let ((elements (stack-frame/elements frame)))
    (subvector->list elements 4 (vector-length elements))))

(define-integrable (eval-frame/expression frame)
  (stack-frame/ref frame 1))

(define-integrable (eval-frame/environment frame)
  (stack-frame/ref frame 2))

(define (pop-return-frame/value continuation)
  (let loop ((frame (continuation->stack-frame continuation)))
    (if (or (not frame) (stack-frame/subproblem? frame))
	(error "Can't find POP-RETURN-ERROR frame."))
    (if (let ((code (stack-frame/return-code frame)))
	  (and code
	       (= return-code:pop-return-error code)))
	(stack-frame/ref frame 1)
	(loop (stack-frame/next frame)))))

(define-integrable (reference-trap-frame/name frame)
  (stack-frame/ref frame 2))

(define-integrable (reference-trap-frame/environment frame)
  (stack-frame/ref frame 3))

(define-integrable (compiled-code-error-frame? frame)
  (let ((code (stack-frame/return-code frame)))
    (and code
	 (= return-code:compiler-error-restart code))))

(define-integrable (compiled-code-error-frame/irritant frame)
  (stack-frame/ref frame 2))

(define return-code:internal-apply)
(define return-code:internal-apply-val)
(define return-code:pop-return-error)
(define return-code:compiler-error-restart)

;;;; Utilities

(define (error-type->string error-type)
  (or (and (string? error-type)
	   error-type)
      (let ((code
	     (if (symbol? error-type)
		 (microcode-system-call-error/name->code error-type)
		 (and (exact-nonnegative-integer? error-type) error-type))))
	(and code
	     ((ucode-primitive system-call-error-message 1) code)))
      (if (symbol? error-type)
	  (string-replace (symbol-name error-type) #\- #\space)
	  (string-append "error " (write-to-string error-type)))))

(define (normalize-trap-code-name name)
  (let loop ((prefixes '("floating-point " "integer ")))
    (if (not (null? prefixes))
	(if (string-prefix-ci? (car prefixes) name)
	    (set! name (string-tail name (string-length (car prefixes))))
	    (loop (cdr prefixes)))))
  (let loop ((suffixes '(" trap" " fault")))
    (if (not (null? suffixes))
	(if (string-suffix-ci? (car suffixes) name)
	    (set! name
		  (string-head name
			       (- (string-length name)
				  (string-length (car suffixes)))))
	    (loop (cdr suffixes)))))
  (cond ((string-ci=? "underflow" name) 'UNDERFLOW)
	((string-ci=? "overflow" name) 'OVERFLOW)
	((or (string-ci=? "divide by 0" name)
	     (string-ci=? "divide by zero" name))
	 'DIVIDE-BY-ZERO)
	(else false)))

(define (file-primitive-description primitive)
  (cond ((or (eq? primitive (ucode-primitive file-exists? 1))
	     (eq? primitive (ucode-primitive file-exists-direct? 1)))
	 (values "determine existence of" "file"))
	((or (eq? primitive (ucode-primitive file-directory? 1))
	     (eq? primitive (ucode-primitive file-soft-link? 1))
	     (eq? primitive (ucode-primitive file-type-direct 1))
	     (eq? primitive (ucode-primitive file-type-indirect 1)))
	 (values "determine type of" "file"))
	((or (eq? primitive (ucode-primitive file-open-append-channel 1))
	     (eq? primitive (ucode-primitive file-open-input-channel 1))
	     (eq? primitive (ucode-primitive file-open-io-channel 1))
	     (eq? primitive (ucode-primitive file-open-output-channel 1))
	     (eq? primitive (ucode-primitive new-file-open-append-channel 2))
	     (eq? primitive (ucode-primitive new-file-open-input-channel 2))
	     (eq? primitive (ucode-primitive new-file-open-io-channel 2))
	     (eq? primitive (ucode-primitive new-file-open-output-channel 2)))
	 (values "open" "file"))
	((eq? primitive (ucode-primitive new-directory-open 1))
	 (values "open" "directory"))
	((or (eq? primitive (ucode-primitive file-modes 1))
	     (eq? primitive (ucode-primitive file-access 2)))
	 (values "read permissions of" "file"))
	((eq? primitive (ucode-primitive set-file-modes! 2))
	 (values "set permissions of" "file"))
	((or (eq? primitive (ucode-primitive file-mod-time 1))
	     (eq? primitive (ucode-primitive file-mod-time-indirect 1)))
	 (values "read modification time of" "file"))
	((or (eq? primitive (ucode-primitive file-attributes 1))
	     (eq? primitive (ucode-primitive file-attributes-indirect 1)))
	 (values "read attributes of" "file"))
	((eq? primitive (ucode-primitive directory-make 1))
	 (values "create" "directory"))
	((eq? primitive (ucode-primitive directory-delete 1))
	 (values "delete" "directory"))
	((eq? primitive (ucode-primitive file-copy 2))
	 (values "copy" "file"))
	((or (eq? primitive (ucode-primitive file-link-hard 2))
	     (eq? primitive (ucode-primitive file-link-soft 2))
	     (eq? primitive (ucode-primitive link-file 3)))
	 (values "link" "file"))
	((or (eq? primitive (ucode-primitive file-remove 1))
	     (eq? primitive (ucode-primitive file-remove-link 1)))
	 (values "delete" "file"))
	((eq? primitive (ucode-primitive file-rename 2))
	 (values "rename" "file"))
	((eq? primitive (ucode-primitive file-touch 1))
	 (values "touch" "file"))
	(else
	 (values false false))))

(define (initialize-package!)

(set! return-code:internal-apply
  (microcode-return/name->code 'INTERNAL-APPLY))

(set! return-code:internal-apply-val
  (microcode-return/name->code 'INTERNAL-APPLY-VAL))

(set! return-code:pop-return-error
  (microcode-return/name->code 'POP-RETURN-ERROR))

(set! return-code:compiler-error-restart
  (microcode-return/name->code 'COMPILER-ERROR-RESTART))

(set! error-handler-vector
  (make-vector (microcode-error/code-limit)
	       (lambda (error-code interrupt-enables)
		 (set-interrupt-enables! interrupt-enables)
		 (call-with-current-continuation
		  (lambda (continuation)
		    (default-error-handler continuation error-code))))))

(set! condition-type:anomalous-microcode-error
  (make-condition-type 'ANOMALOUS-MICROCODE-ERROR condition-type:error
      '(ERROR-CODE EXTRA)
    (lambda (condition port)
      (write-string "Anomalous microcode error " port)
      (write (access-condition condition 'ERROR-CODE) port)
      (write-string " -- get a wizard." port))))

(set! default-error-handler
  (let ((signal
	 (condition-signaller condition-type:anomalous-microcode-error
			      '(ERROR-CODE EXTRA))))
    (lambda (continuation error-code)
      (let ((doit
	     (lambda (error-code extra)
	       (signal continuation
		       (or (and (exact-nonnegative-integer? error-code)
				(microcode-error/code->name error-code))
			   error-code)
		       extra))))
	(if (vector? error-code)
	    (doit (vector-ref error-code 0)
		  (subvector->list error-code 1 (vector-length error-code)))
	    (doit error-code '()))))))

(set! unknown-error-names '())

(define-low-level-handler 'ERROR-WITH-ARGUMENT
  (lambda (continuation argument)
    ((if (and (vector? argument)
	      (fix:>= (vector-length argument) 1)
	      (eqv? (vector-ref argument 0)
		    (microcode-error/name->code 'SYSTEM-CALL)))
	 system-call-error-handler
	 default-error-handler)
     continuation
     argument)))

(let ((fixed-objects (get-fixed-objects-vector)))
  (vector-set! fixed-objects
	       (fixed-objects-vector-slot 'SYSTEM-ERROR-VECTOR)
	       error-handler-vector)
  (vector-set! fixed-objects
	       (fixed-objects-vector-slot 'ERROR-PROCEDURE)
	       (lambda (datum arguments environment)
		 environment
		 (apply error (cons* datum arguments))))
  (vector-set! fixed-objects
	       (fixed-objects-vector-slot 'COMPILER-ERROR-PROCEDURE)
	       error)
  ((ucode-primitive set-fixed-objects-vector!) fixed-objects))

;;;; Variable Errors

(define-error-handler 'UNBOUND-VARIABLE
  (let ((signal
	 (condition-signaller condition-type:unbound-variable
			      '(ENVIRONMENT LOCATION))))
    (lambda (continuation)
      (signal-variable-error
       continuation
       (lambda (environment name)
	 (unbound-variable/store-value continuation environment name
	   (lambda ()
	     (variable/use-value continuation environment name
	       (lambda ()
		 (signal continuation environment name))))))
       (lambda (environment name)
	 (unbound-variable/store-value continuation environment name
	   (lambda ()
	     (signal continuation environment name))))))))

(define-error-handler 'UNASSIGNED-VARIABLE
  (let ((signal
	 (condition-signaller condition-type:unassigned-variable
			      '(ENVIRONMENT LOCATION))))
    (lambda (continuation)
      (signal-variable-error
       continuation
       (lambda (environment name)
	 (unassigned-variable/store-value continuation environment name
	   (lambda ()
	     (variable/use-value continuation environment name
	       (lambda ()
		 (signal continuation environment name))))))
       (lambda (environment name)
	 environment name
	 unspecific)))))

(define-error-handler 'MACRO-BINDING
  (let ((signal
	 (condition-signaller condition-type:macro-binding
			      '(ENVIRONMENT LOCATION))))
    (lambda (continuation)
      (signal-variable-error
       continuation
       (lambda (environment name)
	 (unbound-variable/store-value continuation environment name
	   (lambda ()
	     (variable/use-value continuation environment name
	       (lambda ()
		 (signal continuation environment name))))))
       (lambda (environment name)
	 (unbound-variable/store-value continuation environment name
	   (lambda ()
	     (signal continuation environment name))))))))

(define (signal-variable-error continuation signal-reference signal-other)
  (let ((frame (continuation/first-subproblem continuation)))
    (case (frame/type frame)
      ((EVAL-ERROR)
       (let ((expression (eval-frame/expression frame)))
	 (if (variable? expression)
	     (signal-reference (eval-frame/environment frame)
			       (variable-name expression)))))
      ((ASSIGNMENT-CONTINUE)
       (signal-other (eval-frame/environment frame)
		     (assignment-name (eval-frame/expression frame))))
      ((ACCESS-CONTINUE)
       (signal-reference (pop-return-frame/value continuation)
			 (access-name (eval-frame/expression frame))))
      ((INTERNAL-APPLY INTERNAL-APPLY-VAL)
       (let ((operator (apply-frame/operator frame)))
	 (cond ((or (eq? (ucode-primitive lexical-reference) operator)
		    (eq? (ucode-primitive safe-lexical-reference 2)
			 operator))
		(signal-reference (apply-frame/operand frame 0)
				  (apply-frame/operand frame 1)))
	       ((eq? (ucode-primitive lexical-assignment) operator)
		(signal-other (apply-frame/operand frame 0)
			      (apply-frame/operand frame 1)))
	       ((eq? (ucode-primitive link-variables 4) operator)
		(signal-other (apply-frame/operand frame 0)
			      (apply-frame/operand frame 1)))
	       ((eq? (ucode-primitive lexical-unassigned?) operator)
		(signal-other (apply-frame/operand frame 0)
			      (apply-frame/operand frame 1))))))
      ((COMPILER-REFERENCE-TRAP-RESTART
	COMPILER-SAFE-REFERENCE-TRAP-RESTART)
       (signal-reference (reference-trap-frame/environment frame)
			 (reference-trap-frame/name frame)))
      ((COMPILER-ASSIGNMENT-TRAP-RESTART
	COMPILER-UNASSIGNED?-TRAP-RESTART
	COMPILER-OPERATOR-LOOKUP-TRAP-RESTART)
       (signal-other (reference-trap-frame/environment frame)
		     (reference-trap-frame/name frame))))))

;;;; Argument Errors

(define signal-bad-range-argument
  (illegal-arg-signaller condition-type:bad-range-argument))

(define signal-wrong-type-argument
  (illegal-arg-signaller condition-type:wrong-type-argument))

(define (define-arg-error error-code n signal)
  (define-error-handler error-code
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (apply-frame? frame)
	    (signal continuation
		    (apply-frame/operator frame)
		    (apply-frame/operands frame)
		    n))))))

(define-arg-error 'BAD-RANGE-ARGUMENT-0 0 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-1 1 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-2 2 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-3 3 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-4 4 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-5 5 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-6 6 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-7 7 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-8 8 signal-bad-range-argument)
(define-arg-error 'BAD-RANGE-ARGUMENT-9 9 signal-bad-range-argument)

(define-arg-error 'WRONG-TYPE-ARGUMENT-0 0 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-1 1 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-2 2 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-3 3 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-4 4 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-5 5 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-6 6 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-7 7 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-8 8 signal-wrong-type-argument)
(define-arg-error 'WRONG-TYPE-ARGUMENT-9 9 signal-wrong-type-argument)

;;;; Primitive Errors

(define (define-primitive-error error-name type)
  (define-error-handler error-name
    (let ((signal (condition-signaller type '(OPERATOR OPERANDS))))
      (lambda (continuation)
	(let ((frame (continuation/first-subproblem continuation)))
	  (if (apply-frame? frame)
	      (let ((operator (apply-frame/operator frame)))
		(if (primitive-procedure? operator)
		    (signal continuation
			    operator
			    (apply-frame/operands frame))))))))))

(set! condition-type:primitive-procedure-error
  (make-condition-type 'PRIMITIVE-PROCEDURE-ERROR condition-type:error
      '(OPERATOR OPERANDS)
    (lambda (condition port)
      (write-string "The primitive " port)
      (write-operator (access-condition condition 'OPERATOR) port)
      (write-string " signalled an anonymous error." port))))

(define-primitive-error 'EXTERNAL-RETURN
  condition-type:primitive-procedure-error)

(set! condition-type:unimplemented-primitive
  (make-condition-type 'UNIMPLEMENTED-PRIMITIVE
      condition-type:primitive-procedure-error
      '()
    (lambda (condition port)
      (write-string "The primitive " port)
      (write-operator (access-condition condition 'OPERATOR) port)
      (write-string " is not implemented in this version of Scheme." port))))

(define-primitive-error 'UNIMPLEMENTED-PRIMITIVE
  condition-type:unimplemented-primitive)

(set! condition-type:unimplemented-primitive-for-os
  (make-condition-type 'UNIMPLEMENTED-PRIMITIVE-FOR-OS
      condition-type:unimplemented-primitive
      '()
    (lambda (condition port)
      (write-string "The primitive " port)
      (write-operator (access-condition condition 'OPERATOR) port)
      (write-string " is not implemented for this operating system." port))))

(define-primitive-error 'UNDEFINED-PRIMITIVE-OPERATION
  condition-type:unimplemented-primitive-for-os)

(set! condition-type:compiled-code-error
  (make-condition-type 'COMPILED-CODE-ERROR
      condition-type:primitive-procedure-error
      '()
    (lambda (condition port)
      (write-string "The open-coded primitive " port)
      (write-operator (access-condition condition 'OPERATOR) port)
      (write-string " was called with an inappropriate argument." port))))

(define-error-handler 'COMPILED-CODE-ERROR
  (let ((signal
	 (condition-signaller condition-type:compiled-code-error
			      '(OPERATOR OPERANDS))))
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (compiled-code-error-frame? frame)
	    (let ((irritant (compiled-code-error-frame/irritant frame)))
	      (if (primitive-procedure? irritant)
		  (signal continuation irritant 'UNKNOWN))))))))

(set! condition-type:primitive-io-error
  ;; Primitives that signal this error should be changed to signal a
  ;; system-call error instead, since that is more descriptive.
  (make-condition-type 'PRIMITIVE-IO-ERROR
      condition-type:primitive-procedure-error
      '()
    (lambda (condition port)
      (write-string "The primitive " port)
      (write-operator (access-condition condition 'OPERATOR) port)
      (write-string " signalled an anonymous I/O error." port))))

(define-error-handler 'IO-ERROR
  (let ((signal
	 (condition-signaller condition-type:primitive-io-error
			      '(OPERATOR OPERANDS))))
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (apply-frame? frame)
	    (signal continuation
		    (apply-frame/operator frame)
		    (apply-frame/operands frame)))))))

(set! condition-type:out-of-file-handles
  (make-condition-type 'OUT-OF-FILE-HANDLES
      condition-type:primitive-procedure-error
      '()
    (lambda (condition port)
      (write-string "The primitive " port)
      (write-operator (access-condition condition 'OPERATOR) port)
      (write-string " could not allocate a channel or subprocess." port))))

(define-error-handler 'OUT-OF-FILE-HANDLES
  (let ((signal
	 (condition-signaller condition-type:out-of-file-handles
			      '(OPERATOR OPERANDS)))
	(signal-file-operation (file-operation-signaller)))
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (apply-frame? frame)
	    (let ((operator (apply-frame/operator frame))
		  (operands (apply-frame/operands frame)))
	      (if (or (eq? (ucode-primitive file-open-input-channel) operator)
		      (eq? (ucode-primitive file-open-output-channel) operator)
		      (eq? (ucode-primitive file-open-io-channel) operator)
		      (eq? (ucode-primitive file-open-append-channel) operator)
		      (eq? (ucode-primitive new-file-open-input-channel)
			   operator)
		      (eq? (ucode-primitive new-file-open-output-channel)
			   operator)
		      (eq? (ucode-primitive new-file-open-io-channel)
			   operator)
		      (eq? (ucode-primitive new-file-open-append-channel)
			   operator))
		  (signal-file-operation continuation operator operands 0
					 "open" "file" "channel table full")
		  (signal continuation operator operands))))))))

(set! condition-type:system-call-error
  (make-condition-type 'SYSTEM-CALL-ERROR
      condition-type:primitive-procedure-error
      '(SYSTEM-CALL ERROR-TYPE)
    (lambda (condition port)
      (write-string "The primitive " port)
      (write-operator (access-condition condition 'OPERATOR) port)
      (write-string ", while executing " port)
      (let ((system-call (access-condition condition 'SYSTEM-CALL)))
	(if (symbol? system-call)
	    (begin
	      (write-string "the " port)
	      (write system-call port)
	      (write-string " system call" port))
	    (begin
	      (write-string "system call " port)
	      (write system-call port))))
      (write-string ", received " port)
      (let ((error-type (access-condition condition 'ERROR-TYPE)))
	(if (or (symbol? error-type) (string? error-type))
	    (write-string "the error: " port))
	(write-string (error-type->string error-type) port))
      (write-string "." port))))

(define system-call-error-handler
  (let ((make-condition
	 (condition-constructor condition-type:system-call-error
				'(OPERATOR OPERANDS SYSTEM-CALL ERROR-TYPE)))
	(signal-file-operation (file-operation-signaller)))
    (lambda (continuation error-code)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (and (apply-frame? frame)
		 (vector? error-code)
		 (= 3 (vector-length error-code)))
	    (let ((operator (apply-frame/operator frame))
		  (operands (apply-frame/operands frame))
		  (system-call
		   (if (string? (vector-ref error-code 1))
		       (string->symbol (vector-ref error-code 1))
		       (let ((system-call (vector-ref error-code 2)))
			 (or (microcode-system-call/code->name system-call)
			     system-call))))
		  (error-type
		   (let ((error-type
			  (if (string? (vector-ref error-code 1))
			      (vector-ref error-code 2)
			      (vector-ref error-code 1))))
		     (if (string? error-type)
			 error-type
			 (or (microcode-system-call-error/code->name
			      error-type)
			     error-type)))))
	      (let ((make-condition
		     (lambda ()
		       (make-condition continuation 'BOUND-RESTARTS
				       operator operands
				       system-call error-type))))
		(cond ((port-error-test operator operands)
		       => (lambda (port)
			    (error:derived-port port (make-condition))))
		      ((and (primitive-procedure? operator)
			    (not (null? operands))
			    (string? (car operands)))
		       (receive (verb noun)
			   (file-primitive-description operator)
			 (if verb
			     (signal-file-operation
			      continuation operator operands 0 verb noun
			      (error-type->string error-type))
			     (error (make-condition)))))
		      (else
		       (error (make-condition)))))))))))

(define-low-level-handler 'SYSTEM-CALL system-call-error-handler)

;;;; FASLOAD Errors

(define (define-fasload-error error-code type)
  (define-error-handler error-code
    (let ((signal (condition-signaller type '(FILENAME OPERATOR OPERANDS))))
      (lambda (continuation)
	(let ((frame (continuation/first-subproblem continuation)))
	  (if (apply-frame? frame)
	      (let ((operator (apply-frame/operator frame)))
		(if (or (eq? (ucode-primitive load-band) operator)
			(eq? (ucode-primitive binary-fasload) operator))
		    (signal continuation
			    (apply-frame/operand frame 0)
			    operator
			    (apply-frame/operands frame))))))))))

(set! condition-type:fasload-error
  (make-condition-type 'FASLOAD-ERROR condition-type:file-error
      '(OPERATOR OPERANDS)
    false))

(set! condition-type:fasl-file-bad-data
  (make-condition-type 'FASL-FILE-BAD-DATA condition-type:fasload-error '()
    (lambda (condition port)
      (write-string "Attempt to read binary file " port)
      (write (access-condition condition 'FILENAME) port)
      (write-string " failed: either it's not binary or the wrong version."
		    port))))

(define-fasload-error 'FASL-FILE-BAD-DATA
  condition-type:fasl-file-bad-data)

(set! condition-type:fasl-file-compiled-mismatch
  (make-condition-type 'FASL-FILE-COMPILED-MISMATCH
      condition-type:fasl-file-bad-data
      '()
    false))

(define-fasload-error 'FASLOAD-COMPILED-MISMATCH
  condition-type:fasl-file-compiled-mismatch)

(set! condition-type:fasl-file-too-big
  (make-condition-type 'FASL-FILE-TOO-BIG condition-type:fasload-error '()
    (lambda (condition port)
      (write-string "Attempt to read binary file " port)
      (write (access-condition condition 'FILENAME) port)
      (write-string " failed: it's too large to fit in the heap." port))))

(define-fasload-error 'FASL-FILE-TOO-BIG
  condition-type:fasl-file-too-big)

(set! condition-type:wrong-arity-primitives
  (make-condition-type 'WRONG-ARITY-PRIMITIVES condition-type:fasload-error '()
    (lambda (condition port)
      (write-string "Attempt to read binary file " port)
      (write (access-condition condition 'FILENAME) port)
      (write-string " failed: it contains primitives with incorrect arity."
		    port))))

(define-fasload-error 'WRONG-ARITY-PRIMITIVES
  condition-type:wrong-arity-primitives)

(set! condition-type:fasload-band
  (make-condition-type 'FASLOAD-BAND condition-type:fasl-file-bad-data '()
    false))

(define-error-handler 'FASLOAD-BAND
  (let ((signal
	 (condition-signaller condition-type:fasload-band
			      '(FILENAME OPERATOR OPERANDS))))
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (apply-frame? frame)
	    (let ((operator (apply-frame/operator frame)))
	      (if (eq? (ucode-primitive binary-fasload) operator)
		  (signal continuation
			  (apply-frame/operand frame 0)
			  operator
			  (apply-frame/operands frame)))))))))

;;;; Miscellaneous Errors

(set! condition-type:inapplicable-object
  (make-condition-type 'INAPPLICABLE-OBJECT condition-type:illegal-datum
      '(OPERANDS)
    (lambda (condition port)
      (write-string "The object " port)
      (write (access-condition condition 'DATUM) port)
      (write-string " is not applicable." port))))

(define-error-handler 'UNDEFINED-PROCEDURE
  (let ((signal
	 (condition-signaller condition-type:inapplicable-object
			      '(DATUM OPERANDS))))
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (apply-frame? frame)
	    (let ((operator (apply-frame/operator frame))
		  (operands (apply-frame/operands frame)))
	      (inapplicable-object/use-value continuation operands
		(lambda ()
		  (signal continuation operator operands)))))))))

(define-error-handler 'WRONG-NUMBER-OF-ARGUMENTS
  (let ((signal
	 (condition-signaller condition-type:wrong-number-of-arguments
			      '(DATUM TYPE OPERANDS))))
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (apply-frame? frame)
	    (let ((operator (apply-frame/operator frame)))
	      (signal continuation
		      operator
		      (procedure-arity operator)
		      (apply-frame/operands frame))))))))

(define-error-handler 'FLOATING-OVERFLOW
  (let ((signal
	 (condition-signaller condition-type:floating-point-overflow
			      '(OPERATOR OPERANDS))))
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (apply-frame? frame)
	    (signal continuation
		    (apply-frame/operator frame)
		    (apply-frame/operands frame)))))))

(set! condition-type:fasdump-environment
  (make-condition-type 'FASDUMP-ENVIRONMENT condition-type:bad-range-argument
      '()
    (lambda (condition port)
      (write-string
       "Object cannot be dumped because it contains an environment: "
       port)
      (write (access-condition condition 'DATUM) port))))

(define-error-handler 'FASDUMP-ENVIRONMENT
  (let ((signal
	 (condition-signaller condition-type:fasdump-environment
			      '(DATUM OPERATOR OPERAND))))
    (lambda (continuation)
      (let ((frame (continuation/first-subproblem continuation)))
	(if (apply-frame? frame)
	    (let ((operator (apply-frame/operator frame)))
	      (if (eq? (ucode-primitive primitive-fasdump) operator)
		  (signal continuation
			  (apply-frame/operand frame 0)
			  operator
			  0))))))))

;;;; Asynchronous Microcode Errors

(set! condition-type:hardware-trap
  (make-condition-type 'HARDWARE-TRAP condition-type:error '(NAME CODE)
    (lambda (condition port)
      (write-string "Hardware trap " port)
      (display (access-condition condition 'NAME) port)
      (let ((code (access-condition condition 'CODE)))
	(if code
	    (begin
	      (write-string ": " port)
	      (write code port)))))))

(set! condition-type:user-microcode-reset
  (make-condition-type 'USER-MICROCODE-RESET condition-type:serious-condition
      '()
    "User microcode reset"))

(set! hook/hardware-trap
      (let ((signal-user-microcode-reset
	     (condition-signaller condition-type:user-microcode-reset '()))
	    (signal-divide-by-zero
	     (condition-signaller condition-type:divide-by-zero
				  '(OPERATOR OPERANDS)))
	    (signal-floating-point-overflow
	     (condition-signaller condition-type:floating-point-overflow
				  '(OPERATOR OPERANDS)))
	    (signal-floating-point-underflow
	     (condition-signaller condition-type:floating-point-underflow
				  '(OPERATOR OPERANDS)))
	    (signal-arithmetic-error
	     (condition-signaller condition-type:arithmetic-error
				  '(OPERATOR OPERANDS)))
	    (signal-hardware-trap
	     (condition-signaller condition-type:hardware-trap '(NAME CODE))))
	(lambda (name)
	  (call-with-current-continuation
	   (lambda (k)
	     (if (not name)
		 (signal-user-microcode-reset k)
		 (case microcode-id/operating-system
		   ((OS/2)
		    (cond ((string=? "XCPT_FLOAT_UNDERFLOW" name)
			   (signal-floating-point-underflow k #f '()))
			  ((or (string=? "XCPT_FLOAT_OVERFLOW" name)
			       (string=? "XCPT_INTEGER_OVERFLOW" name))
			   (signal-floating-point-overflow k #f '()))
			  ((or (string=? "XCPT_FLOAT_DIVIDE_BY_ZERO" name)
			       (string=? "XCPT_INTEGER_DIVIDE_BY_ZERO" name))
			   (signal-divide-by-zero k #f '()))
			  ((or (string=? "XCPT_FLOAT_DENORMAL_OPERAND" name)
			       (string=? "XCPT_FLOAT_INEXACT_RESULT" name)
			       (string=? "XCPT_FLOAT_INVALID_OPERATION" name)
			       (string=? "XCPT_FLOAT_STACK_CHECK" name)
			       (string=? "XCPT_B1NPX_ERRATA_02" name))
			   (signal-arithmetic-error k #f '()))
			  (else
			   (signal-hardware-trap k name #f))))
		   (else
		    (let ((code
			   (let ((frame (continuation/first-subproblem k)))
			     (and (hardware-trap-frame? frame)
				  (hardware-trap-frame/code frame)))))
		      (if (string=? "SIGFPE" name)
			  ((case (and (string? code)
				      (normalize-trap-code-name code))
			     ((UNDERFLOW) signal-floating-point-underflow)
			     ((OVERFLOW) signal-floating-point-overflow)
			     ((DIVIDE-BY-ZERO) signal-divide-by-zero)
			     (else signal-arithmetic-error))
			   k false '())
			  (signal-hardware-trap k
						name
						code)))))))))))

;;; end INITIALIZE-PACKAGE!.
)