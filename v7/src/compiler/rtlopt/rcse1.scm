#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcse1.scm,v 1.102 1987/04/24 14:13:51 cph Exp $

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

;;;; RTL Common Subexpression Elimination
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(define (common-subexpression-elimination blocks n-registers)
  (fluid-let ((*next-quantity-number* 0))
    (state:initialize n-registers
      (lambda ()
	(for-each walk-block blocks)))))

(define (walk-block block)
  (state:reset!)
  (walk-rnode block))

(define (walk-rnode rnode)
  (if (node-previous>1? rnode) (state:reset!)) ;Easy non-optimal solution.
  ((vector-method rnode walk-rnode) rnode))

(define-vector-method rtl-snode-tag walk-rnode
  (lambda (rnode)
    (cse-statement (rnode-rtl rnode))
    (let ((next (snode-next rnode)))
      (if next (walk-rnode next)))))

(define-vector-method rtl-pnode-tag walk-rnode
  (lambda (rnode)
    (cse-statement (rnode-rtl rnode))
    (let ((consequent (pnode-consequent rnode))
	  (alternative (pnode-alternative rnode)))
      (if consequent
	  (if alternative
	      ;; Copy the world's state.
	      (let ((state (state:get)))
		(walk-rnode consequent)
		(state:set! state)
		(walk-rnode alternative))
	      (walk-rnode consequent))
	  (if alternative
	      (walk-rnode alternative))))))

(define (cse-statement statement)
  ((cdr (or (assq (rtl:expression-type statement) cse-methods)
	    (error "Missing CSE method" (car statement))))
   statement))

(define cse-methods '())

(define (define-cse-method type method)
  (let ((entry (assq type cse-methods)))
    (if entry
	(set-cdr! entry method)
	(set! cse-methods (cons (cons type method) cse-methods))))
  type)

(define-cse-method 'ASSIGN
  (lambda (statement)
    (expression-replace! rtl:assign-expression rtl:set-assign-expression!
			 statement
      (let ((address (rtl:assign-address statement)))
	(cond ((rtl:register? address)
	       (lambda (volatile? insert-source!)
		 (register-expression-invalidate! address)
		 (if (not volatile?)
		     (insert-register-destination! address (insert-source!)))))
	      ((stack-reference? address)
	       (lambda (volatile? insert-source!)
		 (stack-reference-invalidate! address)
		 (if (not volatile?)
		     (insert-stack-destination! address (insert-source!)))))
	      (else
	       (lambda (volatile? insert-source!)
		 (let ((memory-invalidate!
			(cond ((stack-push/pop? address)
			       (lambda () 'DONE))
			      ((heap-allocate? address)
			       (lambda ()
				 (register-expression-invalidate!
				  (rtl:address-register address))))
			      (else
			       (memory-invalidator
				(expression-varies? address))))))
		   (full-expression-hash address
		     (lambda (hash volatile?* in-memory?*)
		       (cond (volatile?* (memory-invalidate!))
			     ((not volatile?)
			      (let ((address
				     (find-cheapest-expression address hash
							       false)))
				(let ((element (insert-source!)))
				  (memory-invalidate!)
				  (insert-memory-destination!
				   address
				   element
				   (modulo (+ (symbol-hash 'ASSIGN) hash)
					   n-buckets)))))))))
		 ;; **** Kludge.  Works only because stack-pointer
		 ;; gets used in very fixed way by code generator.
		 (if (stack-push/pop? address)
		     (stack-pointer-adjust!
		      (rtl:address-number address))))))))))

(define (noop statement) 'DONE)

(define (trivial-action volatile? insert-source!)
  (if (not volatile?) (insert-source!)))

(define ((normal-action thunk) volatile? insert-source!)
  (thunk)
  (if (not volatile?) (insert-source!)))

(define (define-trivial-one-arg-method type get set)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get set statement trivial-action))))

(define (define-trivial-two-arg-method type get-1 set-1 get-2 set-2)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get-1 set-1 statement trivial-action)
      (expression-replace! get-2 set-2 statement trivial-action))))

(define-trivial-two-arg-method 'EQ-TEST
  rtl:eq-test-expression-1 rtl:set-eq-test-expression-1!
  rtl:eq-test-expression-2 rtl:set-eq-test-expression-2!)

(define-trivial-one-arg-method 'TRUE-TEST
  rtl:true-test-expression rtl:set-true-test-expression!)

(define-trivial-one-arg-method 'TYPE-TEST
  rtl:type-test-expression rtl:set-type-test-expression!)

(define-trivial-one-arg-method 'UNASSIGNED-TEST
  rtl:type-test-expression rtl:set-unassigned-test-expression!)

(define-cse-method 'RETURN noop)
(define-cse-method 'PROCEDURE-HEAP-CHECK noop)
(define-cse-method 'CONTINUATION-HEAP-CHECK noop)

(define (define-stack-trasher type)
  (define-cse-method type trash-stack))

(define (trash-stack statement)
  (stack-invalidate!)
  (stack-pointer-invalidate!))

(define-stack-trasher 'SETUP-LEXPR)
(define-stack-trasher 'MESSAGE-SENDER:VALUE)

(define-cse-method 'INTERPRETER-CALL:ENCLOSE
  (lambda (statement)
    (let ((n (rtl:interpreter-call:enclose-size statement)))
      (stack-region-invalidate! 0 n)
      (stack-pointer-adjust! n))
    (expression-invalidate! (interpreter-register:enclose))))

(define (define-lookup-method type get-environment set-environment! register)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get-environment set-environment! statement
	(normal-action
	 (lambda ()
	   (expression-invalidate! (register))
	   (non-object-invalidate!)))))))

(define-lookup-method 'INTERPRETER-CALL:ACCESS
  rtl:interpreter-call:access-environment
  rtl:set-interpreter-call:access-environment!
  interpreter-register:access)

(define-lookup-method 'INTERPRETER-CALL:LOOKUP
  rtl:interpreter-call:lookup-environment
  rtl:set-interpreter-call:lookup-environment!
  interpreter-register:lookup)

(define-lookup-method 'INTERPRETER-CALL:UNASSIGNED?
  rtl:interpreter-call:unassigned?-environment
  rtl:set-interpreter-call:unassigned?-environment!
  interpreter-register:unassigned?)

(define-lookup-method 'INTERPRETER-CALL:UNBOUND?
  rtl:interpreter-call:unbound?-environment
  rtl:set-interpreter-call:unbound?-environment!
  interpreter-register:unbound?)

(define (define-assignment-method type
	  get-environment set-environment!
	  get-value set-value!)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get-value set-value! statement trivial-action)
      (expression-replace! get-environment set-environment! statement
	(normal-action
	 (lambda ()
	   (memory-invalidate! true)
	   (non-object-invalidate!)))))))

(define-assignment-method 'INTERPRETER-CALL:DEFINE
  rtl:interpreter-call:define-environment
  rtl:set-interpreter-call:define-environment!
  rtl:interpreter-call:define-value
  rtl:set-interpreter-call:define-value!)

(define-assignment-method 'INTERPRETER-CALL:SET!
  rtl:interpreter-call:set!-environment
  rtl:set-interpreter-call:set!-environment!
  rtl:interpreter-call:set!-value
  rtl:set-interpreter-call:set!-value!)

(define (define-invocation-method type)
  (define-cse-method type
    noop
#|  This will be needed when the snode-next of an invocation
    gets connected to the callee's entry node.
    (lambda (statement)
      (let ((prefix (rtl:invocation-prefix statement)))
	(case (car prefix)
	  ((NULL) (continuation-adjustment statement))
	  ((MOVE-FRAME-UP)
	   (let ((size (second prefix))
		 (distance (third prefix)))
	     (stack-region-invalidate! 0 (+ size distance)) ;laziness
	     (stack-pointer-adjust! distance)))
	  ((APPLY-STACK APPLY-CLOSURE) (trash-stack statement))
	  (else (error "Bad prefix type" prefix)))))
|#
    ))

(define (continuation-adjustment statement)
  (let ((continuation (rtl:invocation-continuation statement)))
    (if continuation
	(stack-pointer-adjust! (+ (rtl:invocation-pushed statement)
				  (continuation-delta continuation))))))

(define-invocation-method 'INVOCATION:APPLY)
(define-invocation-method 'INVOCATION:JUMP)
(define-invocation-method 'INVOCATION:LEXPR)
(define-invocation-method 'INVOCATION:PRIMITIVE)

(define-cse-method 'INVOCATION:LOOKUP
  (lambda (statement)
    (continuation-adjustment statement)
    (expression-replace! rtl:invocation:lookup-environment
			 rtl:set-invocation:lookup-environment!
			 statement
			 trivial-action)))

(define (define-message-receiver type size)
  (define-cse-method type
    (let ((size (delay (- (size)))))
      (lambda (statement)
	(stack-pointer-adjust! (force size))))))

(define-message-receiver 'MESSAGE-RECEIVER:CLOSURE
  rtl:message-receiver-size:closure)

(define-message-receiver 'MESSAGE-RECEIVER:STACK
  rtl:message-receiver-size:closure)

(define-message-receiver 'MESSAGE-RECEIVER:SUBPROBLEM
  rtl:message-receiver-size:subproblem)

;;;; Canonicalization

(define (expression-replace! statement-expression set-statement-expression!
			     statement receiver)
  ;; Replace the expression by its cheapest equivalent.  Returns two
  ;; values: (1) a flag which is true iff the expression is volatile;
  ;; and (2) a thunk which, when called, will insert the expression in
  ;; the hash table, returning the element.  Do not call the thunk if
  ;; the expression is volatile.
  (let ((expression
	 (expression-canonicalize (statement-expression statement))))
    (full-expression-hash expression
      (lambda (hash volatile? in-memory?)
	(let ((element
	       (find-cheapest-valid-element expression hash volatile?)))
	  (define (finish expression hash volatile? in-memory?)
	    (set-statement-expression! statement expression)
	    (receiver
	     volatile?
	     (expression-inserter expression element hash in-memory?)))
	  (if element
	      (let ((expression (element-expression element)))
		(full-expression-hash expression
		  (lambda (hash volatile? in-memory?)
		    (finish expression hash volatile? in-memory?))))
	      (finish expression hash volatile? in-memory?)))))))

(define ((expression-inserter expression element hash in-memory?))
  (or element
      (begin (if (rtl:register? expression)
		 (set-register-expression! (rtl:register-number expression)
					   expression)
		 (mention-registers! expression))
	     (let ((element* (hash-table-insert! hash expression false)))
	       (set-element-in-memory?! element* in-memory?)
	       (element-first-value element*)))))

(define (expression-canonicalize expression)
  (cond ((rtl:register? expression)
	 (or (register-expression
	      (quantity-first-register
	       (get-register-quantity (rtl:register-number expression))))
	     expression))
	((stack-reference? expression)
	 (let ((register
		(quantity-first-register
		 (stack-reference-quantity expression))))
	   (or (and register (register-expression register))
	       expression)))
	(else
	 (rtl:map-subexpressions expression expression-canonicalize))))

;;;; Invalidation

(define (memory-invalidator variable?)
  (let ((predicate (if variable? element-address-varies? element-in-memory?)))
    (lambda ()
      (hash-table-delete-class! predicate))))

(define (memory-invalidate! variable?)
  (hash-table-delete-class!
   (if variable? element-address-varies? element-in-memory?)))

(define (non-object-invalidate!)
  (hash-table-delete-class!
   (lambda (element)
     (expression-not-object? (element-expression element)))))

(define (element-address-varies? element)
  (expression-address-varies? (element-expression element)))

(define (expression-invalidate! expression)
  ;; Delete any expression which refers to this expression from the
  ;; table.
  (if (rtl:register? expression)
      (register-expression-invalidate! expression)
      (hash-table-delete-class!
       (lambda (element)
	 (expression-refers-to? (element-expression element) expression)))))

(define (register-expression-invalidate! expression)
  ;; Invalidate a register expression.  These expressions are handled
  ;; specially for efficiency -- the register is marked invalid but we
  ;; delay searching the hash table for relevant expressions.
  (register-invalidate! (rtl:register-number expression))
  (let ((hash (expression-hash expression)))
    (hash-table-delete! hash (hash-table-lookup hash expression))))

(define (register-invalidate! register)
  (let ((next (register-next-equivalent register))
	(previous (register-previous-equivalent register))
	(quantity (get-register-quantity register)))
    (set-register-tick! register (1+ (register-tick register)))
    (if next
	(set-register-previous-equivalent! next previous)
	(set-quantity-last-register! quantity previous))
    (if previous
	(set-register-next-equivalent! previous next)
	(set-quantity-first-register! quantity next))
    (set-register-quantity! register (new-quantity register))
    (set-register-next-equivalent! register false)
    (set-register-previous-equivalent! register false)))

;;;; Destination Insertion

(define (insert-register-destination! expression element)
  ;; Insert EXPRESSION, which should be a register expression, into
  ;; the hash table as the destination of an assignment.  ELEMENT is
  ;; the hash table element for the value being assigned to
  ;; EXPRESSION.
  (let ((class (element->class element))
	(register (rtl:register-number expression)))
    (define (register-equivalence! quantity)
      (set-register-quantity! register quantity)
      (let ((last (quantity-last-register quantity)))
	(if last
	    (begin (set-register-next-equivalent! last register)
		   (set-register-previous-equivalent! register last))
	    (begin (set-quantity-first-register! quantity register)
		   (set-quantity-last-register! quantity register))))
      (set-register-next-equivalent! register false)
      (set-quantity-last-register! quantity register))

    (set-register-expression! register expression)
    (if class
	(let ((expression (element-expression class)))
	  (cond ((rtl:register? expression)
		 (register-equivalence!
		  (get-register-quantity (rtl:register-number expression))))
		((stack-reference? expression)
		 (register-equivalence!
		  (stack-reference-quantity expression))))))
    (set-element-in-memory?!
     (hash-table-insert! (expression-hash expression) expression class)
     false)))

(define (insert-stack-destination! expression element)
  (set-element-in-memory?! (hash-table-insert! (expression-hash expression)
					       expression
					       (element->class element))
			   false))

(define (insert-memory-destination! expression element hash)
  (let ((class (element->class element)))
    (mention-registers! expression)
    (set-element-in-memory?! (hash-table-insert! hash expression class) true)))

(define (mention-registers! expression)
  (if (rtl:register? expression)
      (let ((register (rtl:register-number expression)))
	(remove-invalid-references! register)
	(set-register-in-table! register (register-tick register)))
      (rtl:for-each-subexpression expression mention-registers!)))

(define (remove-invalid-references! register)
  ;; If REGISTER is invalid, delete all expressions which refer to it
  ;; from the hash table.
  (if (let ((in-table (register-in-table register)))
	(and (not (negative? in-table))
	     (not (= in-table (register-tick register)))))
      (let ((expression (register-expression register)))
	(hash-table-delete-class!
	 (lambda (element)
	   (let ((expression* (element-expression element)))
	     (and (not (rtl:register? expression*))
		  (expression-refers-to? expression* expression))))))))

;;;; Table Search

(define (find-cheapest-expression expression hash volatile?)
  ;; Find the cheapest equivalent expression for EXPRESSION.
  (let ((element (find-cheapest-valid-element expression hash volatile?)))
    (if element
	(element-expression element)
	expression)))

(define (find-cheapest-valid-element expression hash volatile?)
  ;; Find the cheapest valid hash table element for EXPRESSION.
  ;; Returns false if no such element exists or if EXPRESSION is
  ;; VOLATILE?.
  (and (not volatile?)
       (let ((element (hash-table-lookup hash expression)))
	 (and element
	      (let ((element* (element-first-value element)))
		(if (eq? element element*)
		    element
		    (let loop ((element element*))
		      (and element
			   (let ((expression (element-expression element)))
			     (if (or (rtl:register? expression)
				     (expression-valid? expression))
				 element
				 (loop (element-next-value element))))))))))))

(define (expression-valid? expression)
  ;; True iff all registers mentioned in EXPRESSION have valid values
  ;; in the hash table.
  (if (rtl:register? expression)
      (let ((register (rtl:register-number expression)))
	(= (register-in-table register) (register-tick register)))
      (rtl:all-subexpressions? expression expression-valid?)))

(define (element->class element)
  ;; Return the cheapest element in the hash table which has the same
  ;; value as ELEMENT.  This is necessary because ELEMENT may have
  ;; been deleted due to register or memory invalidation.
  (and element
       ;; If ELEMENT has been deleted from the hash table,
       ;; CLASS will be false.  [ref crock-1]
       (let ((class (element-first-value element)))
	 (or class
	     (element->class (element-next-value element))))))

;;;; Expression Hash

(define (expression-hash expression)
  (full-expression-hash expression
    (lambda (hash do-not-record? hash-arg-in-memory?)
      hash)))

(define (full-expression-hash expression receiver)
  (let ((do-not-record? false)
	(hash-arg-in-memory? false))
    (define (loop expression)
      (let ((type (rtl:expression-type expression)))
	(+ (symbol-hash type)
	   (case type
	     ((REGISTER)
	      (quantity-number
	       (get-register-quantity (rtl:register-number expression))))
	     ((OFFSET)
	      ;; Note that stack-references do not get treated as
	      ;; memory for purposes of invalidation.  This is because
	      ;; (supposedly) no one ever accesses the stack directly
	      ;; except the compiler's output, which is explicit.
	      (let ((register (rtl:offset-register expression)))
		(if (interpreter-stack-pointer? register)
		    (quantity-number (stack-reference-quantity expression))
		    (begin (set! hash-arg-in-memory? true)
			   (continue expression)))))
	     ((PRE-INCREMENT POST-INCREMENT)
	      (set! hash-arg-in-memory? true)
	      (set! do-not-record? true)
	      0)
	     (else (continue expression))))))

    (define (continue expression)
      (rtl:reduce-subparts expression + 0 loop hash-object))

    (let ((hash (loop expression)))
      (receiver (modulo hash n-buckets) do-not-record? hash-arg-in-memory?))))

(define (hash-object object)
  (cond ((integer? object) object)
	((symbol? object) (symbol-hash object))
  rtl:set-interpreter-call:set!-value!)