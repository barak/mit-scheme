#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/uerror.scm,v 14.13 1990/02/21 23:24:25 jinx Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Microcode Errors
;;; package: (runtime microcode-errors)

(declare (usual-integrations))

(define (initialize-package!)
  (set! internal-apply-frame/fasload?
	(internal-apply-frame/operator-filter
	 (ucode-primitive binary-fasload)
	 (ucode-primitive load-band)))
  (set! internal-apply-frame/fasdump?
	(internal-apply-frame/operator-filter
	 (ucode-primitive primitive-fasdump)))
  (build-condition-types!)
  (set! microcode-error-types (make-error-types))
  (set! error-type:bad-error-code (microcode-error-type 'BAD-ERROR-CODE))
  (let ((fixed-objects (get-fixed-objects-vector)))
    (vector-set! fixed-objects
		 (fixed-objects-vector-slot 'SYSTEM-ERROR-VECTOR)
		 (make-error-handlers))
    ((ucode-primitive set-fixed-objects-vector!) fixed-objects))
  unspecific)

(define (make-error-handlers)
  (let ((error-code-limit (microcode-error/code-limit)))
    (let ((alists (make-error-alists error-code-limit)))
      (make-initialized-vector error-code-limit
	(lambda (index)
	  (let ((alist (vector-ref alists index)))
	    (if (procedure? alist)
		alist
		(let ((error-type (vector-ref microcode-error-types index)))
		  (if error-type
		      (make-error-translator alist error-type)
		      anomalous-microcode-error)))))))))

(define (make-error-translator alist error-type)
  (lambda (error-code interrupt-enables)
    error-code
    (set-interrupt-enables! interrupt-enables)
    (with-proceed-point proceed-value-filter
      (lambda ()
	(signal-error
	 (let ((frame
		(continuation/first-subproblem
		 (current-proceed-continuation))))
	   (let ((translator
		  (let ((return-code (stack-frame/return-code frame)))
		    (and return-code
			 (let ((entry (assv return-code alist)))
			   (and entry
				(let loop ((translators (cdr entry)))
				  (and (not (null? translators))
				       (if (or (eq? (caar translators) true)
					       ((caar translators) frame))
					   (cdar translators)
					   (loop (cdr translators)))))))))))
	     (if translator
		 (translator error-type frame)
		 (make-error-condition error-type
				       '()
				       repl-environment)))))))))

(define (anomalous-microcode-error error-code interrupt-enables)
  (set-interrupt-enables! interrupt-enables)
  (with-proceed-point proceed-value-filter
    (lambda ()
      (signal-error
       (make-error-condition
	error-type:anomalous
	(list (or (microcode-error/code->name error-code) error-code))
	repl-environment)))))

;;;; Frame Decomposition

(define-integrable (standard-frame/expression frame)
  (stack-frame/ref frame 1))

(define-integrable (standard-frame/environment frame)
  (stack-frame/ref frame 2))

(define (standard-frame/variable? frame)
  (variable? (standard-frame/expression frame)))

(define-integrable (expression-only-frame/expression frame)
  (stack-frame/ref frame 1))

(define-integrable (internal-apply-frame/operator frame)
  (stack-frame/ref frame 3))

(define-integrable (internal-apply-frame/operand frame index)
  (stack-frame/ref frame (+ 4 index)))

(define-integrable (internal-apply-frame/n-operands frame)
  (- (stack-frame/length frame) 4))

(define (internal-apply-frame/select frame selector)
  (if (exact-nonnegative-integer? selector)
      (internal-apply-frame/operand frame selector)
      (selector frame)))

(define ((internal-apply-frame/operator-filter . operators) frame)
  (memq (internal-apply-frame/operator frame) operators))

(define internal-apply-frame/fasload?)
(define internal-apply-frame/fasdump?)

(define (internal-apply-frame/add-fluid-binding-name frame)
  (let ((name (internal-apply-frame/operand frame 1)))
    (cond ((variable? name) (variable-name name))
	  ((symbol? name) name)
	  (else name))))

;;;; Special Handlers

(define (wrong-number-of-arguments-error condition-type frame)
  (make-error-condition
   condition-type
   (let ((operator (internal-apply-frame/operator frame)))
     (let ((arity (procedure-arity operator)))
       (list (internal-apply-frame/n-operands frame)
	     (error-irritant/noise char:newline)
	     (error-irritant/noise "within procedure")
	     operator
	     (error-irritant/noise char:newline)
	     (error-irritant/noise "minimum/maximum number of arguments:")
	     (car arity)
	     (cdr arity))))
   repl-environment))

(define (file-error condition-type frame)
  condition-type frame
  (make-error-condition error-type:file '() repl-environment))

(define (open-file-error condition-type frame)
  condition-type
  (make-error-condition error-type:open-file
			(list (internal-apply-frame/operand frame 0))
			repl-environment))

(define (out-of-file-handles-error condition-type frame)
  (make-error-condition condition-type
			(list (internal-apply-frame/operand frame 0))
			repl-environment))

(define (write-into-pure-space-error error-code interrupt-enables)
  error-code
  (set-interrupt-enables! interrupt-enables)
  (let ((port (cmdl/output-port (nearest-cmdl))))
    (newline port)
    (write-string "Automagically impurifying an object..." port))
  (call-with-current-continuation
   (lambda (continuation)
     (impurify
      (internal-apply-frame/operand
       (continuation/first-subproblem continuation)
       0)))))

(define (bad-error-code-handler error-code interrupt-enables)
  ;; This could be a "translator" except that it needs the error-code
  ;; and "translators" don't normally get it.
  (set-interrupt-enables! interrupt-enables)
  (with-proceed-point proceed-value-filter
    (lambda ()
      (signal-error
       (make-error-condition error-type:bad-error-code
			     (list error-code)
			     repl-environment)))))

(define error-type:bad-error-code)

(define error-type:anomalous)
(define error-type:bad-range-argument)
(define error-type:failed-argument-coercion)
(define error-type:fasdump)
(define error-type:fasload)
(define error-type:illegal-argument)
(define error-type:file)
(define error-type:open-file)
(define error-type:random-internal)
(define error-type:wrong-type-argument)

(define (build-condition-types!)
  (set! error-type:random-internal
	(make-base-type "Random internal error"))
  (set! error-type:illegal-argument
	(make-base-type "Illegal argument"))
  (set! error-type:wrong-type-argument
	(make-condition-type (list error-type:illegal-argument)
			     "Illegal datum"))
  (set! error-type:bad-range-argument
	(make-condition-type (list error-type:illegal-argument)
			     "Datum out of range"))
  (set! error-type:failed-argument-coercion
	(make-base-type "Argument cannot be coerced to floating point"))
  (set! error-type:file
	(make-base-type "File operation error"))
  (set! error-type:open-file
	(make-condition-type (list error-type:file) "Unable to open file"))
  (set! error-type:fasdump
	(make-condition-type (list error-type:file) "Fasdump error"))
  (set! error-type:fasload
	(make-condition-type (list error-type:file) "Fasload error"))
  (set! error-type:anomalous
	(make-internal-type "Anomalous microcode error")))

(define (make-base-type message)
  (make-condition-type (list condition-type:error) message))

(define (make-internal-type message)
  (make-condition-type (list error-type:random-internal)
		       (string-append message " -- get a wizard")))

(define (make-bad-range-type n)
  (make-condition-type (list error-type:bad-range-argument)
		       (string-append "Datum out of range in "
				      (vector-ref nth-string n)
				      " argument position")))

(define (make-wrong-type-type n)
  (make-condition-type (list error-type:wrong-type-argument)
		       (string-append "Illegal datum in "
				      (vector-ref nth-string n)
				      " argument position")))

(define (make-failed-arg-type n)
  (make-condition-type (list error-type:failed-argument-coercion)
		       (string-append
			(string-capitalize (vector-ref nth-string n))
			" argument cannot be coerced to floating point")))

(define nth-string
  '#("first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth"
	     "ninth" "tenth"))

(define (microcode-error-type name)
  (vector-ref microcode-error-types (microcode-error name)))

(define microcode-error-types)

(define (make-error-types)
  (let ((types (make-vector (microcode-error/code-limit) false)))
    (for-each
     (lambda (entry)
       (vector-set! types (microcode-error (car entry)) (cadr entry)))
     `(
       (BAD-ASSIGNMENT ,(make-internal-type "Illegal to rebind variable"))
       (BAD-ERROR-CODE ,(make-internal-type "Illegal error code"))
       (BAD-FRAME ,(make-internal-type "Illegal environment frame"))
       (BAD-INTERRUPT-CODE ,(make-internal-type "Illegal interrupt code"))
       (BAD-RANGE-ARGUMENT-0 ,(make-bad-range-type 0))
       (BAD-RANGE-ARGUMENT-1 ,(make-bad-range-type 1))
       (BAD-RANGE-ARGUMENT-2 ,(make-bad-range-type 2))
       (BAD-RANGE-ARGUMENT-3 ,(make-bad-range-type 3))
       (BAD-RANGE-ARGUMENT-4 ,(make-bad-range-type 4))
       (BAD-RANGE-ARGUMENT-5 ,(make-bad-range-type 5))
       (BAD-RANGE-ARGUMENT-6 ,(make-bad-range-type 6))
       (BAD-RANGE-ARGUMENT-7 ,(make-bad-range-type 7))
       (BAD-RANGE-ARGUMENT-8 ,(make-bad-range-type 8))
       (BAD-RANGE-ARGUMENT-9 ,(make-bad-range-type 9))
       (BROKEN-CVARIABLE ,(make-internal-type "Broken compiled variable"))
       (BROKEN-VARIABLE-CACHE
	,(make-internal-type "Broken variable value cell"))
       (COMPILED-CODE-ERROR ,(make-internal-type "Compiled code error"))
       (EXECUTE-MANIFEST-VECTOR
	,(make-internal-type "Attempt to execute manifest vector"))
       (EXTERNAL-RETURN
	,(make-internal-type "Error during external application"))
       (FAILED-ARG-1-COERCION ,(make-failed-arg-type 0))
       (FAILED-ARG-2-COERCION ,(make-failed-arg-type 1))
       (FASDUMP-ENVIRONMENT
	,(make-condition-type
	  (list error-type:fasdump)
	  "Object to dump is or points to environment objects"))
       (FASL-FILE-BAD-DATA
	,(make-condition-type (list error-type:fasload) "Bad binary file"))
       (FASL-FILE-TOO-BIG
	,(make-condition-type (list error-type:fasload) "Not enough room"))
       (FASLOAD-BAND
	,(make-condition-type
	  (list error-type:fasload)
	  "Binary file contains a scheme image (band), not an object"))
       (FASLOAD-COMPILED-MISMATCH
	,(make-condition-type
	  (list error-type:fasload)
	  "Binary file contains compiled code for a different microcode"))
       (FLOATING-OVERFLOW ,(make-base-type "Floating point overflow"))
       (ILLEGAL-REFERENCE-TRAP ,(make-internal-type "Illegal reference trap"))
       (INAPPLICABLE-CONTINUATION
	,(make-internal-type "Inapplicable continuation"))
       (IO-ERROR ,(make-condition-type (list error-type:file) "I/O error"))
       (OUT-OF-FILE-HANDLES
	,(make-condition-type (list error-type:open-file)
			      "Too many open files"))
       (UNASSIGNED-VARIABLE ,(make-base-type "Unassigned variable"))
       (UNBOUND-VARIABLE ,(make-base-type "Unbound variable"))
       (UNDEFINED-PRIMITIVE-OPERATION
	,(make-internal-type "Undefined primitive procedure"))
       (UNDEFINED-PROCEDURE
	,(make-base-type "Application of inapplicable object"))
       (UNDEFINED-USER-TYPE ,(make-internal-type "Undefined type code"))
       (UNIMPLEMENTED-PRIMITIVE
	,(make-internal-type "Unimplemented primitive procedure"))
       (WRONG-ARITY-PRIMITIVES
	,(make-condition-type
	  (list error-type:fasload)
	  "Primitives in binary file have the wrong arity"))
       (WRONG-NUMBER-OF-ARGUMENTS
	,(make-base-type "Wrong number of arguments"))
       (WRONG-TYPE-ARGUMENT-0 ,(make-wrong-type-type 0))
       (WRONG-TYPE-ARGUMENT-1 ,(make-wrong-type-type 1))
       (WRONG-TYPE-ARGUMENT-2 ,(make-wrong-type-type 2))
       (WRONG-TYPE-ARGUMENT-3 ,(make-wrong-type-type 3))
       (WRONG-TYPE-ARGUMENT-4 ,(make-wrong-type-type 4))
       (WRONG-TYPE-ARGUMENT-5 ,(make-wrong-type-type 5))
       (WRONG-TYPE-ARGUMENT-6 ,(make-wrong-type-type 6))
       (WRONG-TYPE-ARGUMENT-7 ,(make-wrong-type-type 7))
       (WRONG-TYPE-ARGUMENT-8 ,(make-wrong-type-type 8))
       (WRONG-TYPE-ARGUMENT-9 ,(make-wrong-type-type 9))
       ))
    types))

(define (make-error-alists error-code-limit)
  (let ((alists (make-vector error-code-limit '())))

    (define (define-total-error-handler error-type handler)
      (vector-set! alists
		   (microcode-error error-type)
		   handler))

    (define (define-error-handler error-type frame-type frame-filter handler)
      (let ((error-code (microcode-error error-type))
	    (return-code (microcode-return frame-type)))
	(let ((entry (vector-ref alists error-code)))
	  (cond ((pair? entry)
		 (let ((entry* (assv return-code entry)))
		   (if entry*
		       (let ((entry** (assq frame-filter (cdr entry*))))
			 (if entry**
			     (set-cdr! entry** handler)
			     (set-cdr! entry*
				       (let ((entry**
					      (cons frame-filter handler)))
					 (if (eq? frame-filter true)
					     (append! (cdr entry*)
						      (list entry**))
					     (cons entry** (cdr entry*)))))))
		       (vector-set! alists
				    error-code
				    (cons (list return-code
						(cons frame-filter handler))
					  entry)))))
		((null? entry)
		 (vector-set! alists
			      error-code
			      (list (list return-code
					  (cons frame-filter handler)))))
		(else
		 (error "Can't overwrite error handler" entry)))))
      unspecific)

    (define (define-standard-frame-handler error-type frame-type frame-filter
	      irritant)
      (define-error-handler error-type frame-type frame-filter
	(lambda (condition-type frame)
	  (make-error-condition
	   condition-type
	   (list (irritant (standard-frame/expression frame)))
	   (standard-frame/environment frame)))))

    (define (define-expression-frame-handler error-type frame-type frame-filter
	      irritant)
      (define-error-handler error-type frame-type frame-filter
	(lambda (condition-type frame)
	  (make-error-condition
	   condition-type
	   (list (irritant (expression-only-frame/expression frame)))
	   repl-environment))))

    (define (define-apply-handler definer)
      (for-each definer '(INTERNAL-APPLY INTERNAL-APPLY-VAL)))

    (define (define-internal-apply-handler error-type environment irritant
	      . operators)
      (define-apply-handler
       (lambda (return-address)
	 (define-error-handler error-type return-address
	   (apply internal-apply-frame/operator-filter operators)
	   (lambda (condition-type frame)
	     (make-error-condition
	      condition-type
	      (list (internal-apply-frame/select frame irritant))
	      (if environment
		  (internal-apply-frame/select frame environment)
		  repl-environment)))))))

    (define (define-operator-handler error-type)
      (define-apply-handler
	(lambda (return-address)
	  (define-error-handler error-type return-address true
	    (lambda (condition-type frame)
	      (make-error-condition condition-type
				    (list (internal-apply-frame/operator frame))
				    repl-environment))))))

    (define (define-operand-handler error-type irritant #!optional filter)
      (define-apply-handler
	(lambda (return-address)
	  (define-error-handler error-type return-address
	    (if (default-object? filter) true filter)
	    (lambda (condition-type frame)
	      (make-error-condition
	       condition-type
	       (list (internal-apply-frame/select frame irritant)
		     (error-irritant/noise char:newline)
		     (error-irritant/noise "within procedure")
		     (internal-apply-frame/operator frame))
	       repl-environment))))))

    (define (define-reference-trap-handler error-type frame-type)
      (define-error-handler error-type frame-type true
	(lambda (condition-type frame)
	  (make-error-condition
	   condition-type
	   (list (stack-frame/ref frame 2))
	   (stack-frame/ref frame 3)))))

    (define-standard-frame-handler 'UNBOUND-VARIABLE 'EVAL-ERROR
      standard-frame/variable? variable-name)

    (define-standard-frame-handler 'UNBOUND-VARIABLE 'ASSIGNMENT-CONTINUE true
      assignment-name)

    (define-expression-frame-handler 'UNBOUND-VARIABLE 'ACCESS-CONTINUE true
      access-name)

    (define-internal-apply-handler 'UNBOUND-VARIABLE 0 1
      (ucode-primitive lexical-reference)
      (ucode-primitive lexical-assignment))

    (define-internal-apply-handler 'UNBOUND-VARIABLE 0
      internal-apply-frame/add-fluid-binding-name
      (ucode-primitive add-fluid-binding! 3))

    (define-internal-apply-handler 'UNBOUND-VARIABLE 0 2
      (ucode-primitive environment-link-name))

    (define-reference-trap-handler 'UNBOUND-VARIABLE
      'COMPILER-REFERENCE-TRAP-RESTART)

    (define-reference-trap-handler 'UNBOUND-VARIABLE
      'COMPILER-SAFE-REFERENCE-TRAP-RESTART)

    (define-reference-trap-handler 'UNBOUND-VARIABLE
      'COMPILER-ASSIGNMENT-TRAP-RESTART)

    (define-reference-trap-handler 'UNBOUND-VARIABLE
      'COMPILER-UNASSIGNED?-TRAP-RESTART)

    (define-reference-trap-handler 'UNBOUND-VARIABLE
      'COMPILER-OPERATOR-LOOKUP-TRAP-RESTART)

    (define-internal-apply-handler 'BAD-ASSIGNMENT 1 2
      (ucode-primitive environment-link-name))

    (define-internal-apply-handler 'ILLEGAL-REFERENCE-TRAP 1 2
      (ucode-primitive environment-link-name))

    (define-standard-frame-handler 'UNASSIGNED-VARIABLE 'EVAL-ERROR
      standard-frame/variable? variable-name)

    (define-expression-frame-handler 'UNASSIGNED-VARIABLE 'ACCESS-CONTINUE true
      access-name)

    (define-internal-apply-handler 'UNASSIGNED-VARIABLE 0 1
      (ucode-primitive lexical-reference))

    (define-reference-trap-handler 'UNASSIGNED-VARIABLE
      'COMPILER-REFERENCE-TRAP-RESTART)

    (define-reference-trap-handler 'UNASSIGNED-VARIABLE
      'COMPILER-OPERATOR-LOOKUP-TRAP-RESTART)

    (define-expression-frame-handler 'BAD-FRAME 'ACCESS-CONTINUE true
      access-environment)

    (define-expression-frame-handler 'BAD-FRAME 'IN-PACKAGE-CONTINUE true
      in-package-environment)

    (define-internal-apply-handler 'BAD-FRAME 0 2
      (ucode-primitive environment-link-name))

    (define-standard-frame-handler 'BROKEN-CVARIABLE 'EVAL-ERROR
      standard-frame/variable? variable-name)

    (define-standard-frame-handler 'BROKEN-CVARIABLE 'ASSIGNMENT-CONTINUE true
      assignment-name)

    (define-apply-handler
      (lambda (return-address)
	(define-error-handler 'WRONG-NUMBER-OF-ARGUMENTS return-address true
	  wrong-number-of-arguments-error)))

    (define-operator-handler 'UNDEFINED-PROCEDURE)
    (define-operator-handler 'UNDEFINED-PRIMITIVE-OPERATION)
    (define-operator-handler 'UNIMPLEMENTED-PRIMITIVE)
    (define-operator-handler 'EXTERNAL-RETURN)

    (define-operand-handler 'FAILED-ARG-1-COERCION 0)
    (define-operand-handler 'FAILED-ARG-2-COERCION 1)

    (define-operand-handler 'WRONG-TYPE-ARGUMENT-0 0)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-1 1)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-2 2)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-3 3)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-4 4)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-5 5)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-6 6)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-7 7)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-8 8)
    (define-operand-handler 'WRONG-TYPE-ARGUMENT-9 9)

    (define-operand-handler 'BAD-RANGE-ARGUMENT-0 0)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-1 1)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-2 2)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-3 3)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-4 4)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-5 5)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-6 6)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-7 7)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-8 8)
    (define-operand-handler 'BAD-RANGE-ARGUMENT-9 9)

    (define-operand-handler 'FASL-FILE-TOO-BIG 0
      internal-apply-frame/fasload?)
    (define-operand-handler 'FASL-FILE-BAD-DATA 0
      internal-apply-frame/fasload?)
    (define-operand-handler 'WRONG-ARITY-PRIMITIVES 0
      internal-apply-frame/fasload?)
    (define-operand-handler 'IO-ERROR 0
      internal-apply-frame/fasload?)
    (define-operand-handler 'FASLOAD-COMPILED-MISMATCH 0
      internal-apply-frame/fasload?)
    (define-operand-handler 'FASLOAD-BAND 0
      internal-apply-frame/fasload?)

    (define-operand-handler 'IO-ERROR 1
      internal-apply-frame/fasdump?)
    (define-operand-handler 'FASDUMP-ENVIRONMENT 0
      internal-apply-frame/fasdump?)

    (define-apply-handler
      (lambda (return-address)
	(define-error-handler 'BAD-RANGE-ARGUMENT-0 return-address
	  (internal-apply-frame/operator-filter
	   (ucode-primitive file-open-channel)
	   (ucode-primitive make-directory))
	  open-file-error)))

    (define-apply-handler
      (lambda (return-address)
	(define-error-handler 'OUT-OF-FILE-HANDLES return-address
	  (internal-apply-frame/operator-filter
	   (ucode-primitive file-open-channel))
	  out-of-file-handles-error)))

    (define-apply-handler
      (lambda (return-address)
	(define-error-handler 'EXTERNAL-RETURN return-address
	  (internal-apply-frame/operator-filter
	   (ucode-primitive file-length)
	   (ucode-primitive file-read-char)
	   (ucode-primitive file-write-char)
	   (ucode-primitive file-write-string)
	   (ucode-primitive copy-file)
	   (ucode-primitive rename-file)
	   (ucode-primitive remove-file)
	   (ucode-primitive link-file)
	   (ucode-primitive set-file-modes! 2))
	  file-error)))

    (define-total-error-handler 'WRITE-INTO-PURE-SPACE
      write-into-pure-space-error)

    (define-total-error-handler 'BAD-ERROR-CODE
      bad-error-code-handler)

    alists))