;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; RTL Common Subexpression Elimination
;;;  Based on the GNU C Compiler

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

(define (common-subexpression-elimination blocks n-registers)
  (fluid-let ((*next-quantity-number* 0))
    (state:initialize n-registers
      (lambda ()
	(for-each walk-block blocks)))))

(define (walk-block block)
  (state:reset!)
  (walk-rnode block))

(define (walk-rnode rnode)
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
			(if (destination-safe? address)
			    (lambda () 'DONE)
			    (memory-invalidator
			     (expression-varies? address)))))
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

(define (noop statement)
  'DONE)

(define (trivial-action volatile? insert-source!)
  (if (not volatile?) (insert-source!)))

(define ((normal-action thunk) volatile? insert-source!)
  (thunk)
  (if (not volatile?) (insert-source!)))

(define-cse-method 'EQ-TEST
  (lambda (statement)
    (expression-replace! rtl:eq-test-expression-1
			 rtl:set-eq-test-expression-1!
			 statement
			 trivial-action)
    (expression-replace! rtl:eq-test-expression-2
			 rtl:set-eq-test-expression-2!
			 statement
			 trivial-action)))

(define (define-trivial-method type get-expression set-expression!)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get-expression set-expression! statement
			   trivial-action))))

(define-trivial-method 'TRUE-TEST
  rtl:true-test-expression
  rtl:set-true-test-expression!)

(define-trivial-method 'TYPE-TEST
  rtl:type-test-expression
  rtl:set-type-test-expression!)

(define-cse-method 'RETURN noop)
(define-cse-method 'PROCEDURE-HEAP-CHECK noop)
(define-cse-method 'CONTINUATION-HEAP-CHECK noop)

(define (define-lookup-method type get-environment set-environment! register)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get-environment set-environment! statement
	(normal-action (lambda () (expression-invalidate! (register))))))))

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

(define-cse-method 'INTERPRETER-CALL:ENCLOSE
  (lambda (statement)
    (let ((n (rtl:interpreter-call:enclose-size statement)))
      (stack-region-invalidate! 0 n)
      (stack-pointer-adjust! n))
    (expression-invalidate! (interpreter-stack-pointer))))

(define (define-assignment-method type
	  get-environment set-environment!
	  get-value set-value!)
  (lambda (statement)
    (expression-replace! get-value set-value! statement trivial-action)
    (expression-replace! get-environment set-environment! statement
      (normal-action (lambda ()	(memory-invalidate! true))))))

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
    (lambda (statement)
      (let ((prefix (rtl:invocation-prefix statement)))
	(case (car prefix)
	  ((NULL) (continuation-adjustment statement))
	  ((MOVE-FRAME-UP)
	   (let ((size (second prefix))
		 (distance (third prefix)))
	     (stack-region-invalidate! 0 (+ size distance)) ;laziness
	     (stack-pointer-adjust! distance))
	   (expression-invalidate! (interpreter-stack-pointer)))
	  ((APPLY-STACK APPLY-CLOSURE) (trash-stack statement))
	  (else (error "Bad prefix type" prefix)))))))

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

(define (define-message-receiver type)
  (define-cse-method type
    (lambda (statement)
      (stack-pointer-adjust! -2)
      (expression-invalidate! (interpreter-stack-pointer)))))

(define-message-receiver 'MESSAGE-RECEIVER:CLOSURE)
(define-message-receiver 'MESSAGE-RECEIVER:STACK)
(define-message-receiver 'MESSAGE-RECEIVER:SUBPROBLEM)

(define (define-stack-trasher type)
  (define-cse-method type trash-stack))

(define (trash-stack statement)
  (stack-invalidate!)
  (expression-invalidate! (interpreter-stack-pointer)))

(define-stack-trasher 'SETUP-CLOSURE-LEXPR)
(define-stack-trasher 'SETUP-STACK-LEXPR)
(define-stack-trasher 'MESSAGE-SENDER:VALUE)

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
	       (register-quantity (rtl:register-number expression))))
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
	(quantity (register-quantity register)))
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
		  (register-quantity (rtl:register-number expression))))
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
	       (register-quantity (rtl:register-number expression))))
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
	(else (hash object))))

;;;; Expression Predicates

(define (expression-equivalent? x y validate?)
  ;; If VALIDATE? is true, assume that Y comes from the hash table and
  ;; should have its register references validated.
  (define (loop x y)
    (let ((type (rtl:expression-type x)))
      (and (eq? type (rtl:expression-type y))
	   (case type
	     ((REGISTER)
	      (register-equivalent? x y))
	     ((OFFSET)
	      (let ((rx (rtl:offset-register x)))
		(and (register-equivalent? rx (rtl:offset-register y))
		     (if (interpreter-stack-pointer? rx)
			 (eq? (stack-reference-quantity x)
			      (stack-reference-quantity y))
			 (= (rtl:offset-number x)
			    (rtl:offset-number y))))))
	     (else
	      (rtl:match-subexpressions x y loop))))))

  (define (register-equivalent? x y)
    (let ((x (rtl:register-number x))
	  (y (rtl:register-number y)))
      (and (eq? (register-quantity x) (register-quantity y))
	   (or (not validate?)
	       (= (register-in-table y) (register-tick y))))))

  (loop x y))

(define (expression-refers-to? x y)
  ;; True iff any subexpression of X matches Y.
  (define (loop x)
    (or (eq? x y)
	(if (eq? (rtl:expression-type x) (rtl:expression-type y))
	    (expression-equivalent? x y false)
	    (rtl:any-subexpression? x loop))))
  (loop x))

(define (expression-address-varies? expression)
  (if (memq (rtl:expression-type expression)
	    '(OFFSET PRE-INCREMENT POST-INCREMENT))
      (register-expression-varies? (rtl:address-register expression))
      (rtl:any-subexpression? expression expression-address-varies?)))

(define (expression-varies? expression)
  ;; This procedure should not be called on a register expression.
  (let ((type (rtl:expression-type expression)))
    (or (memq type '(OFFSET PRE-INCREMENT POST-INCREMENT))
	(if (eq? type 'REGISTER)
	    (register-expression-varies? expression)
	    (rtl:any-subexpression? expression expression-varies?)))))

(define (register-expression-varies? expression)
  (not (= regnum:regs-pointer (rtl:register-number expression))))

(define (destination-safe? expression)
  ;; Pushing on the stack and consing can't invalidate anything.
  (and (memq (rtl:expression-type expression) '(PRE-INCREMENT POST-INCREMENT))
       (or (interpreter-stack-pointer? (rtl:address-register expression))
	   (interpreter-free-pointer? (rtl:address-register expression)))))

(define (stack-push/pop? expression)
  (and (memq (rtl:expression-type expression) '(PRE-INCREMENT POST-INCREMENT))
       (interpreter-stack-pointer? (rtl:address-register expression))))

;;;; Stack References

(define *stack-offset*)
(define *stack-reference-quantities*)

(define (stack-reference? expression)
  (and (eq? (rtl:expression-type expression) 'OFFSET)
       (interpreter-stack-pointer? (rtl:address-register expression))))

(define (stack-reference-quantity expression)
  (let ((n (+ *stack-offset* (rtl:offset-number expression))))
    (let ((entry (ass= n *stack-reference-quantities*)))
      (if entry
	  (cdr entry)
	  (let ((quantity (new-quantity false)))
	    (set! *stack-reference-quantities*
		  (cons (cons n quantity)
			*stack-reference-quantities*))
	    quantity)))))

(define (stack-pointer-adjust! offset)
  (set! *stack-offset* (+ (stack->memory-offset offset) *stack-offset*)))

(define (stack-invalidate!)
  (set! *stack-reference-quantities* '()))

(define (stack-region-invalidate! start end)
  (let ((end (+ *stack-offset* end)))
    (define (loop i quantities)
      (if (< i end)
	  (loop (1+ i)
		(del-ass=! i quantities))
	  (set! *stack-reference-quantities* quantities)))
    (loop (+ *stack-offset* start) *stack-reference-quantities*)))

(define (stack-reference-invalidate! expression)
  (expression-invalidate! expression)
  (set! *stack-reference-quantities*
	(del-ass=! (+ *stack-offset* (rtl:offset-number expression))
		   *stack-reference-quantities*)))

(define ass= (association-procedure = car))
(define del-ass=! (delete-association-procedure list-deletor! = car))

;;;; Hash Table Abstraction

(define n-buckets 31)

(define (make-hash-table)
  (make-vector n-buckets false))

(define *hash-table*)

(define-integrable (hash-table-ref hash)
  (vector-ref *hash-table* hash))

(define-integrable (hash-table-set! hash element)
  (vector-set! *hash-table* hash element))

(define element-tag (make-vector-tag false 'ELEMENT))
(define element? (tagged-vector-predicate element-tag))

(define-vector-slots element 1
  expression cost in-memory?
  next-hash previous-hash
  next-value previous-value first-value)

(define (make-element expression)
  (vector element-tag expression false false false false false false false))

(define (hash-table-lookup hash expression)
  (define (loop element)
    (and element
	 (if (let ((expression* (element-expression element)))
	       (or (eq? expression expression*)
		   (expression-equivalent? expression expression* true)))
	     element
	     (loop (element-next-hash element)))))
  (loop (hash-table-ref hash)))

(define (hash-table-insert! hash expression class)
  (let ((element (make-element expression))
	(cost (rtl:expression-cost expression)))
    (set-element-cost! element cost)
    (let ((next (hash-table-ref hash)))
      (set-element-next-hash! element next)
      (if next (set-element-previous-hash! next element)))
    (hash-table-set! hash element)
    (cond ((not class)
	   (set-element-first-value! element element))
	  ((< cost (element-cost class))
	   (set-element-next-value! element class)
	   (set-element-previous-value! class element)
	   (let loop ((x element))
	     (if x
		 (begin (set-element-first-value! x element)
			(loop (element-next-value x))))))
	  (else
	   (set-element-first-value! element class)
	   (let loop ((previous class)
		      (next (element-next-value class)))
	     (cond ((not next)
		    (set-element-next-value! element false)
		    (set-element-next-value! previous element)
		    (set-element-previous-value! element previous))
		   ((<= cost (element-cost next))
		    (set-element-next-value! element next)
		    (set-element-previous-value! next element)
		    (set-element-next-value! previous element)
		    (set-element-previous-value! element previous))
		   (else
		    (loop next (element-next-value next)))))))
    element))

(define (hash-table-delete! hash element)
  (if element
      (begin
       ;; **** Mark this element as removed.  [ref crock-1]
       (set-element-first-value! element false)
       (let ((next (element-next-value element))
	     (previous (element-previous-value element)))
	 (if next (set-element-previous-value! next previous))
	 (if previous
	     (set-element-next-value! previous next)
	     (let loop ((element next))
	       (if element
		   (begin (set-element-first-value! element next)
			  (loop (element-next-value element)))))))
       (let ((next (element-next-hash element))
	     (previous (element-previous-hash element)))
	 (if next (set-element-previous-hash! next previous))
	 (if previous
	     (set-element-next-hash! previous next)
	     (hash-table-set! hash next))))))

(define (hash-table-delete-class! predicate)
  (let table-loop ((i 0))
    (if (< i n-buckets)
	(let bucket-loop ((element (hash-table-ref i)))
	  (if element
	      (begin (if (predicate element)
			 (hash-table-delete! i element))
		     (bucket-loop (element-next-hash element)))
	      (table-loop (1+ i)))))))

(package (hash-table-copy)

(define *elements*)

(define-export (hash-table-copy table)
  (fluid-let ((*elements* '()))
    (vector-map table element-copy)))

(define (element-copy element)
  (and element
       (let ((entry (assq element *elements*)))
	 (if entry
	     (cdr entry)
	     (let ((new (make-element (element-expression element))))
	       (set! *elements* (cons (cons element new) *elements*))
	       (set-element-cost! new (element-cost element))
	       (set-element-in-memory?! new (element-in-memory? element))
	       (set-element-next-hash!
		new
		(element-copy (element-next-hash element)))
	       (set-element-previous-hash!
		new
		(element-copy (element-previous-hash element)))
	       (set-element-next-value!
		new
		(element-copy (element-next-value element)))
	       (set-element-previous-value!
		new
		(element-copy (element-previous-value element)))
	       (set-element-first-value!
		new
		(element-copy (element-first-value element)))
	       new)))))

)

;;;; State Abstraction

(define (state:initialize n-registers thunk)
  (fluid-let ((*register-quantity* (make-vector n-registers))
	      (*register-next-equivalent* (make-vector n-registers))
	      (*register-previous-equivalent* (make-vector n-registers))
	      (*register-expression* (make-vector n-registers))
	      (*register-tick* (make-vector n-registers))
	      (*register-in-table* (make-vector n-registers))
	      (*hash-table* (make-hash-table))
	      (*stack-offset*)
	      (*stack-reference-quantities*))
    (thunk)))

(define (state:reset!)
  (vector-fill-with-quantities! *register-quantity*)
  (vector-fill! *register-next-equivalent* false)
  (vector-fill! *register-previous-equivalent* false)
  (vector-fill! *register-expression* false)
  (for-each-machine-register
   (lambda (register)
     (set-register-expression! register (rtl:make-machine-register register))))
  (vector-fill! *register-tick* 0)
  (vector-fill! *register-in-table* -1)
  (set! *hash-table* (make-hash-table))
  (set! *stack-offset* 0)
  (set! *stack-reference-quantities* '()))

(define (vector-fill-with-quantities! vector)
  (define (loop i)
    (vector-set! vector i (new-quantity i))
    (if (not (zero? i))
	(loop (-1+ i))))
  (loop (-1+ (vector-length vector))))

(define (state:get)
  (vector (vector-map *register-quantity* quantity-copy)
	  (vector-copy *register-next-equivalent*)
	  (vector-copy *register-previous-equivalent*)
	  (vector-copy *register-expression*)
	  (vector-copy *register-tick*)
	  (vector-copy *register-in-table*)
	  (hash-table-copy *hash-table*)
	  *stack-offset*
	  (copy-alist *stack-reference-quantities*)))

(define (state:set! state)
  (set! *register-quantity* (vector-ref state 0))
  (set! *register-next-equivalent* (vector-ref state 1))
  (set! *register-previous-equivalent* (vector-ref state 2))
  (set! *register-expression* (vector-ref state 3))
  (set! *register-tick* (vector-ref state 4))
  (set! *register-in-table* (vector-ref state 5))
  (set! *hash-table* (vector-ref state 6))
  (set! *stack-offset* (vector-ref state 7))
  (set! *stack-reference-quantities* (vector-ref state 8)))

;;;; Register/Quantity Abstractions

(define quantity-tag (make-vector-tag false 'QUANTITY))
(define quantity? (tagged-vector-predicate quantity-tag))
(define-vector-slots quantity 1 number first-register last-register)

(define *next-quantity-number*)

(define (generate-quantity-number)
  (let ((n *next-quantity-number*))
    (set! *next-quantity-number* (1+ *next-quantity-number*))
    n))

(define (make-quantity number first-register last-register)
  (vector quantity-tag number first-register last-register))

(define (new-quantity register)
  (make-quantity (generate-quantity-number) register register))

(define (quantity-copy quantity)
  (make-quantity (quantity-number quantity)
		 (quantity-first-register quantity)
		 (quantity-last-register quantity)))

(define-register-references quantity)
(define-register-references next-equivalent)
(define-register-references previous-equivalent)
(define-register-references expression)
(define-register-references tick)
(define-register-references in-table)

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access rtl-cse-package compiler-package)
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
  rtl:set-interpreter-call:set!-value!)