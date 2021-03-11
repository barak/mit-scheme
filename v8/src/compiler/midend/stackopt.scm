#| -*-Scheme-*-

Copyright (c) 1994-1999 Massachusetts Institute of Technology

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

;;;; Stack optimization (reordering)
;;; package: (compiler midend)

(declare (usual-integrations))

#| Big Note A

This optimizer works by building a model of the current stack frame,
with parent and child links mapping from the state of the stack frame
at one point in time to the state earlier/later.  It then attempts to
make the frames similar by assigning the slots in the frame to contain
the same object where possible, thus reducing shuffling.  The bulk of
the reordering calculation is contained in the procedures
STACKOPT/REARRANGE! and STACKOPT/REARRANGE/PROCESS!.

The algorithm is complicated by two issues: some elements of a stack
frame have fixed locations that cannot be changed at a given point in
the computation: values pushed for calls to primitives, and values
pushed for passing the last arguments to unknown procedures with a
large number of arguments.  The former case is detectable because the
call to MAKE-STACK-CLOSURE (which announces the new format of the
stack frame) will not contain a LAMBDA expression in the
CALL/%MAKE-STACK-CLOSURE/LAMBDA-EXPRESSION slot.

The latter case is detected by looking at the vector of names
available to the continuation (from the
CALL/%FETCH-STACK-CLOSURE/VECTOR slot that must exist within the
lambda-expresion) and comparing it with the names
available at the call side in the CALL/%MAKE-STACK-CLOSURE/VECTOR
slot.  These will have a common prefix consisting of the values to be
saved, followed in one case by the parameters being passed on the
stack and in the other the values being passed to the continuation on
the stack.  Only the common prefix is subject to reordering, the other
parts being fixed by the parameter passing convention.

There is one unusual property of the stack model currently produced.
Consider the case of a many-argument call to a procedure where the
continuation receives many values.  We produce a separate model for
the stack frame on the call side (showing the values saved on the
stack for use in the continuation plus the values being passed as
parameters to the called procedure on the stack) and the stack frame
on the continuation side (showing the values saved on the stack plus
the values being supplied by the procedure to the continuation).  We
require the following property of any implementation of the reordering
algorithm: the stack slot assignments provided for the saved values in
these two frames must be identical -- the compiler is free to reorder
them in any way, but the reordering must be the same on both sides of
the call.  This is in addition to the requirement that the slot
assignments for the parameters and values are fixed by the calling
sequence.


		THEOREM AND PROOF

THEOREM: The stack slot assignments provided for the saved values in
these two frames will be identical.

We prove the following stronger property of the *CURRENT* algorithm,
from which the theorem follows directly.

THEOREM: For any frame with a single child in which the names of the
unwired variables and the numbers of the unwired slots are the same in
the parent and child, the slot assignments for these variables will be
the same in the parent and the child.

PROOF: Inductively on the number of unwired names/slots in the parent
frame.  If there are no unwired names/slots then the theorem follows
trivially.  We prove that wiring a name to a slot in either the parent
or child frame preserves the invariant.

Whenever an assignment transforms an unwired name to a wired
name, the assignment is propagated to the parent and all children of
the model in which the assignment occurs (see PROPAGATE in
STACKOPT/REARRANGE/PROCESS!).  For convenience, let us call the models
PARENT and CHILD.  We consider two cases:
  a: An assignment is generated in PARENT.  It will be propagated to
     CHILD.  By our induction hypothesis, the child will have both the
     name and the slot unwired, and will proceed to wire them
     together.
  b: An assignment is generated in CHILD.  Conversely, it will be
     propagated to PARENT, where the induction hypothesis also implies
     that the name and slot are free, hence will be wired.

End of Big Note A |#


(define (stackopt/top-level program)
  (fluid-let ((stackopt/dbg-refs (stackopt/get-dbg-refs)))
    (stackopt/expr false program)))

(define stackopt/dbg-refs) ; table from frame name to dbg info references

(define-macro (define-stack-optimizer keyword bindings . body)
  (let ((proc-name (symbol-append 'STACKOPT/ keyword)))
    (call-with-values
     (lambda () (%matchup (cdr bindings) '(handler state) '(cdr form)))
     (lambda (names code)
       `(define ,proc-name
	  (let ((handler (lambda ,(cons (car bindings) names) ,@body)))
	    (named-lambda (,proc-name state form)
	      (stackopt/remember ,code
			       form))))))))

(define-stack-optimizer LOOKUP (state name)
  state					; ignored
  `(LOOKUP ,name))

(define-stack-optimizer LAMBDA (state lambda-list body)
  (define (wrap body)
    `(LAMBDA ,lambda-list ,body))
  (cond ((form/match stackopt/fat-procedure-body-pattern body)
	 => (lambda (result)
	      (wrap (stackopt/fat-procedure state body result))))
	(else
	 (wrap (stackopt/expr false body)))))


(define-stack-optimizer LET (state bindings body)
  `(LET ,(map (lambda (binding)
		(list (car binding)
		      (stackopt/expr state (cadr binding))))
	      bindings)
     ,(stackopt/expr state body)))

(define-stack-optimizer LETREC (state bindings body)
  `(LETREC ,(map (lambda (binding)
		   (list (car binding)
			 (stackopt/expr false (cadr binding))))
		 bindings)
     ,(stackopt/expr state body)))

(define-stack-optimizer QUOTE (state object)
  state					; ignored
  `(QUOTE ,object))

(define-stack-optimizer DECLARE (state #!rest anything)
  state					; ignored
  `(DECLARE ,@anything))

(define-stack-optimizer IF (state pred conseq alt)
  `(IF ,(stackopt/expr state pred)
       ,(stackopt/expr state conseq)
       ,(stackopt/expr state alt)))

(define-stack-optimizer BEGIN (state #!rest actions)
  (if (null? actions)
      `(BEGIN)
      (let ((actions* (reverse actions)))
	`(BEGIN ,@(stackopt/expr* state (reverse (cdr actions*)))
		,(stackopt/expr state (car actions*))))))

;;  HAIR.
;;
;;  When copying the program we make a copy of the frame vector at its
;;  definition site and use that.  Then we make sure that all
;;  references to that frame variable have frame vector elements are
;;  then EQ? to the copy.  This ensures that if a big chunk of code
;;  has been replicated then the frame vectors and hence stack models
;;  for the two now separate chunks are distinct and do not interfere
;;  but we still have the nice EQ? property between all vectors for
;;  any particular frame.

(define *stackopt/lexical-stack-frame-name* #F)
(define *stackopt/lexical-stack-frame-vector* #F)

;; MORE HAIR
;;
;; These fluid bound variables are used solely for propogating the frame
;; information in <expr> below (this occurs when calling primitives):
;;
;;   (CALL (LAMBDA (cont) <expr>)
;;         (CALL %make-stack-closure ...)
;;
;; The problem is that <expr> may have free variables, including the
;; surrounding frame vector, but should have no model (because we do
;; not model what is on the other side of the continuation [usually we
;; dont know], Trying to model this would make things even trickier as
;; the machine continuation may be naturally kept in the stack (an
;; invisible offset) or in a register.

;; A better solution would be to separate the model tree from the
;; environment function of matching up the frame vectors for a
;; particular frame variable.

(define-stack-optimizer CALL (state rator cont #!rest rands)

  (define (default)
    (with-letfied-nested-stack-closures
     rator cont rands
     (lambda (rator cont rands)
       (define (wrap lambda-special? cont*)
	 `(CALL ,(if (and lambda-special?
			  (LAMBDA/? rator)
			  (null? rands)
			  state)
		     (fluid-let ((*stackopt/lexical-stack-frame-name*
				  (stackopt/model/name state))
				 (*stackopt/lexical-stack-frame-vector*
				  (stackopt/model/frame state)))
		       (stackopt/expr state rator))
		     (stackopt/expr state rator))
		,cont*
		,@(stackopt/expr* state rands)))
       (cond ((form/match stackopt/cont-pattern cont)
	      => (lambda (result)
		   (wrap #T
			 (stackopt/call/can-see-both-frames
			  state
			  (call/%make-stack-closure/lambda-expression cont)
			  result))))
	     ((call/%make-stack-closure? cont)
	      (wrap #T (stackopt/call/terminal state cont)))
	     (else
	      (wrap #F (stackopt/expr state cont)))))))

  (define (fixup-vector)
    (let ((var  (lookup/name (first rands))) ;rands = (closure offset 'name)
	  (name (quote/text (third rands))))
      (define (bad)
	(internal-error "Inconsistent %stack-closure-ref"
			(error-irritant/noise "\n; state: ") state
			(error-irritant/noise "\n; form:  ")
			`(CALL ,rator ,cont ,@rands)))
      (define (good frame-vector)
	`(CALL ',%stack-closure-ref
	       '#F
	       (LOOKUP ,var)
	       ',frame-vector
	       ',name))
      (cond ((and (not state) (eq? var *stackopt/lexical-stack-frame-name*))
	     (good *stackopt/lexical-stack-frame-vector*))
	    ((and state (eq? var (stackopt/model/name state)))
	     (good (stackopt/model/frame state)))
	    (else
	     (bad)))))

  (if (QUOTE/? rator)
      (cond ((eq? (quote/text rator) %stack-closure-ref)
	     (fixup-vector))
	    ((eq? (quote/text rator) %make-stack-closure)
	     (internal-error "Explicit make-stack-closure") #F)
	    (else (default)))
      (default)))

(define (with-letfied-nested-stack-closures rator cont rands
					    receiver-of-rator+cont+rands)
  ;; The loop does the `letifying' transformation until there are no
  ;; calls to %make-stack-closure in the top-level position.
  ;;    (CALL <procedure>
  ;;          (CALL 'make-stack-closure
  ;;                #F
  ;;                <lambda>
  ;;                #<frame>
  ;;                (CALL 'make-stack-closure #F ...)
  ;;                ...)
  ;;          ...)
  ;; Is transformed to
  ;;    (CALL (LAMBDA (cont)
  ;;            (CALL <procedure>
  ;;                  (CALL 'make-stack-closure
  ;;                        #F
  ;;                        <lambda>
  ;;                        #<frame>
  ;;                        (lookup cont)
  ;;                        ...)
  ;;                  ...))
  ;;          (CALL 'make-stack-closure #F ...))
  ;; This kind of code occurs because we want to generate a continutaion
  ;; and then push it on the stack when callingprimitives.
  (let loop ((rator rator) (cont cont) (rands rands))
    (if (and (call/%make-stack-closure? cont)
	     (pair? (call/%make-stack-closure/values cont))
	     (call/%make-stack-closure?
	      (first (call/%make-stack-closure/values cont))))
	(let ((cont-var (new-continuation-variable)))
	  (loop
	   `(LAMBDA (,cont-var)
	      (CALL ,rator
		    (CALL ',%make-stack-closure
			  '#F
			  ,(call/%make-stack-closure/lambda-expression cont)
			  ,(call/%make-stack-closure/vector cont)
			  (LOOKUP ,cont-var)
			  ,@(cdr (call/%make-stack-closure/values cont)))
		    ,@rands))
	   (first (call/%make-stack-closure/values cont))
	   '() ))
	(receiver-of-rator+cont+rands rator cont rands))))

(define (stackopt/expr state expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (stackopt/quote state expr))
    ((LOOKUP)   (stackopt/lookup state expr))
    ((LAMBDA)   (stackopt/lambda state expr))
    ((LET)      (stackopt/let state expr))
    ((DECLARE)  (stackopt/declare state expr))
    ((CALL)     (stackopt/call state expr))
    ((BEGIN)    (stackopt/begin state expr))
    ((IF)       (stackopt/if state expr))
    ((LETREC)   (stackopt/letrec state expr))
    (else
     (illegal expr))))

(define (stackopt/expr* state exprs)
  (map (lambda (expr)
	 (stackopt/expr state expr))
       exprs))

(define (stackopt/remember new old)
  (code-rewrite/remember new old))

(define stackopt/?lambda-list (->pattern-variable 'LAMBDA-LIST))
(define stackopt/?frame-name (->pattern-variable 'FRAME-VECTOR-NAME))
(define stackopt/?frame-vector (->pattern-variable 'FRAME-VECTOR))
(define stackopt/?call-side-frame-vector (->pattern-variable 'CALL-FRAME))
(define stackopt/?continuation-side-frame-vector (->pattern-variable 'CONT-FRAME))
(define stackopt/?body (->pattern-variable 'BODY))
(define stackopt/?closure-elts (->pattern-variable 'CLOSURE-ELTS))

(define stackopt/cont-pattern
  `(CALL (QUOTE ,%make-stack-closure)
	 (QUOTE #F)
	 (LAMBDA ,stackopt/?lambda-list
	   (LET ((,stackopt/?frame-name
		  (CALL (QUOTE ,%fetch-stack-closure)
			(QUOTE #F)
			(QUOTE ,stackopt/?continuation-side-frame-vector))))
	     ,stackopt/?body))
	 (QUOTE ,stackopt/?call-side-frame-vector)
	 ,@stackopt/?closure-elts))


(define stackopt/fat-procedure-body-pattern
  `(LET ((,stackopt/?frame-name
	  (CALL (QUOTE ,%fetch-stack-closure)
		(QUOTE #F)
		(QUOTE ,stackopt/?frame-vector))))
     ,stackopt/?body))


(define (stackopt/fat-procedure state lambda-body match-result)
  ;;Following test wrong: (lambda () (subproblem) (lambda (a1 ... a100) ...))
  ;;(if state
  ;;    (internal-error "Model exists at non-continuation lambda!" state))
  state
  (let* ((frame-vector  (cadr (assq stackopt/?frame-vector match-result)))
	 (frame-name    (cadr (assq stackopt/?frame-name match-result)))
	 (model  (stackopt/model/make #F (vector-copy frame-vector) frame-name
				      #T #T))
	 (form*  (stackopt/expr model lambda-body)))
    (set-stackopt/model/form! model #F)
    (stackopt/reorder! model)
    form*))

(define (stackopt/call/can-see-both-frames state handler match-result)

  (define (first-mismatch v1 v2)
    (let ((length (min (vector-length v1) (vector-length v2))))
      (let loop ((i 0))
	(cond ((= i length) length)
	      ((eq? (vector-ref v1 i) (vector-ref v2 i))
	       (loop (+ i 1)))
	      (else i)))))

  (define (wire-from! model frame from)
    (let ((end (vector-length frame)))
      (do ((i from (+ i 1)))
	  ((= i end) 'OK)
	(let ((var (vector-ref frame i)))
	  (if (not (continuation-variable? var))
	      (stackopt/wire! model `((,var . ,i))))))))
	
  ;; Handler for "standard" %make-stack-closure (those with a LAMBDA
  ;; expression)
  (let ((lambda-list (cadr (assq stackopt/?lambda-list match-result)))
	(frame-name (cadr (assq stackopt/?frame-name match-result)))
	(call-frame-vector
	 (vector-copy
	  (cadr (assq stackopt/?call-side-frame-vector match-result))))
	(cont-frame-vector
	 (vector-copy
	  (cadr (assq stackopt/?continuation-side-frame-vector
		      match-result))))
	(body (cadr (assq stackopt/?body match-result)))
	(real-rands (cadr (assq stackopt/?closure-elts match-result))))
    (let* ((call-model (stackopt/model/make state call-frame-vector 
					    frame-name #F #F))
	   (cont-model
	    (if (eq? call-frame-vector cont-frame-vector)
		call-model
		(stackopt/model/make call-model cont-frame-vector frame-name
				     #F #F)))
	   ;; See Big Note A at the top of this file.
	   (handler*
	    `(LAMBDA ,lambda-list
	       (LET ((,frame-name (CALL (QUOTE ,%fetch-stack-closure)
					(QUOTE #F)
					(QUOTE ,cont-frame-vector))))
		 ,(stackopt/expr cont-model body))))
	   (form*
	    `(CALL (QUOTE ,%make-stack-closure)
		   (QUOTE #F)
		   ,(stackopt/remember handler* handler)
		   (QUOTE ,call-frame-vector)
		   ,@(stackopt/expr* state real-rands))))
      (if (not (eq? call-model cont-model))
	  (let ((mismatch (first-mismatch call-frame-vector
					  cont-frame-vector)))
	    (wire-from! call-model call-frame-vector mismatch)
	    (wire-from! cont-model cont-frame-vector mismatch)
	    (set-stackopt/model/form! cont-model #F)))
      (stackopt/%call state call-model form*))))

(define (stackopt/call/terminal state cont)
  ;; Handler for CONT being the "push" %make-stack-closure (i.e. with
  ;; anything other than a LAMBDA expression)
  (let ((frame-vector (vector-copy
		       (quote/text (call/%make-stack-closure/vector cont))))
	(real-rands   (call/%make-stack-closure/values cont))
	(non-lambda   (call/%make-stack-closure/lambda-expression cont)))
    (let* ((model (stackopt/model/make state frame-vector #F #T #F))
	   (form* `(CALL (QUOTE ,%make-stack-closure)
			 (QUOTE #F)
			 ,(stackopt/expr state non-lambda)
			 (QUOTE ,frame-vector)
			 ,@(stackopt/expr* state real-rands))))
      (stackopt/%call state model form*))))

(define (stackopt/%call state model form*)
  (set-stackopt/model/form! model form*)
  (if (not state)
      (stackopt/reorder! model))
  form*)

;; For now, this is a very simple rearranger.
;; The problem is really complicated (probably NP-complete),
;; and it's not clear how to even do a good heuristic.
;; The problem is simplified if we allow stack frames to have holes,
;; as C compilers do, since then each preserved variable can have a
;; home in the stack.  The problem is garbage collection:
;; no-longer-used slots need to be cleared, and this is as costly as
;; reshuffling.

(define (stackopt/reorder! model)
  (define (stackopt/model-intersection model)
    ;; Find the set of variables present in the model and all of its children
    (define (walk set models)
      (cond ((null? models) set)
	    ((null? set) set)
	    (else (walk
		   (intersection set
				 (vector->list
				  (stackopt/model/frame
				   (car models))))
		   (append (stackopt/model/children (car models))
			   (cdr models))))))
    (walk (vector->list (stackopt/model/frame model))
	  (stackopt/model/children model)))
    
  (stackopt/rearrange! model
   (stackopt/constrain model
    (stackopt/model-intersection model)
    (let min-all ((model model))
      ;; Calculate the smallest frame size that appears anywhere in
      ;; the tree of frame extensions
      (fold-right (lambda (model current-min)
		    (min (min-all model) current-min))
		  (vector-length (stackopt/model/frame model))
		  (stackopt/model/children model)))))
  (stackopt/rewrite! model))

(define (stackopt/rewrite! model)
  ;; Rewrite the form for this model and those for all of its children
  ;; by calculating the new order of names in the frame and reordering
  ;; the value expressions to match the new order.
  (for-each stackopt/rewrite! (stackopt/model/children model))
  (let* ((frame* (stackopt/model/frame model))
	 (frame (vector-copy frame*))	; copy so we can see mutations
	 (form (stackopt/model/form model)))
    (stackopt/update-frame! model)
    (if (and form (not (equal? frame* frame)))
	(let* ((names&values
		(map cons
		     (vector->list frame)
		     (call/%make-stack-closure/values form)))
	       (values*
		(map (lambda (name*)
		       (let ((place (assq name* names&values)))
			 (if (not place)
			     (stackopt/inconsistency model))
			 (cdr place)))
		     (vector->list frame*))))
	  (stackopt/rewrite-dbg-frames! (stackopt/model/name model) frame*)
	  (form/rewrite! form
	    `(CALL ,(call/operator form)
		   ,(call/continuation form)
		   ,(call/%make-stack-closure/lambda-expression form)
		   ,(call/%make-stack-closure/vector form)
		   ,@values*))))))

(define (stackopt/get-dbg-refs)
  (let ((info (make-eq-hash-table)))
    (define (walk expr)
      (cond ((dbg/stack-closure-ref? expr)
	     (let ((frame-var (vector-ref expr 1)))
	       (hash-table/put!
		info
		frame-var
		(cons expr
		      (hash-table/get info frame-var '())))))
	    ((dbg/heap-closure-ref? expr)
	     (walk (vector-ref expr 1)))
	    ((CALL/? expr)
	     (for-each walk (call/operands expr)))
	    (else unspecific)))
    (dbg-info/for-all-dbg-expressions! walk)
    info))

(define (stackopt/rewrite-dbg-frames! frame-var new-vector)
  (for-each (lambda (ref)
	      (vector-set! ref 2 new-vector))
    (hash-table/get stackopt/dbg-refs frame-var '())))

(define (stackopt/rearrange! model wired)
  (define (arrange-locally! model)
    ;; Generate the wiring for a model by performing a union of WIRED
    ;; with the wired elements of the model's frame (WIRED wins if a
    ;; name is wired in two different places?!)
    (let* ((wired*
	    (let ((wired* (stackopt/model/wired model)))
	      (if (not wired*)
		  wired
		  (append wired
			  (list-transform-negative wired*
			    (lambda (wired-pair)
			      (assq (car wired-pair) wired)))))))
	   (unwired
	    (list-transform-negative
		(vector->list (stackopt/model/frame model))
	      (lambda (var)
		(assq var wired*)))))
      (set-stackopt/model/wired! model wired*)
      (set-stackopt/model/unwired! model unwired)
      (set-stackopt/model/n-unwired! model (length unwired))))

  (define (max-all model)
    ;; Maximum number of unwired slots in this frame or any
    ;; [grand*]child frame
    (fold-right (lambda (model current-max)
		  (max (max-all model) current-max))
		(stackopt/model/n-unwired model)
		(stackopt/model/children model)))

  ;; Walk the model's frame and all of its (recursive) children.  This
  ;; will add the WIRED set to all of the wired names of this frame
  ;; and its children.
  (let walk ((model model))
    (arrange-locally! model)
    (for-each walk (stackopt/model/children model)))

  ;; If this model has children and they aren't all wired down by this
  ;; time, gyrate around filling in the unfilled slots.
  (if (not (null? (stackopt/model/children model)))
      (let ((max-unwired (max-all model)))
	(if (not (zero? max-unwired))
	    (let ((buckets (make-vector max-unwired '())))
	      (let insert! ((model model))
		(for-each insert! (stackopt/model/children model))
		(let ((n-unwired (stackopt/model/n-unwired model)))
		  (if (not (zero? n-unwired))
		      (let ((index (- n-unwired 1)))
			(vector-set! buckets index
				     (cons model
					   (vector-ref buckets index)))))))
	      (stackopt/rearrange/process! buckets))))))

(define (stackopt/rearrange/process! buckets)
  ;; BUCKETS is a vector long enough to hold an entry for each unwired
  ;; slot in the largest frame here or in one of the children.  It
  ;; maps from number of open slots to models with that number of open
  ;; slots (off by one). That is, entry 0 has a list of all models
  ;; with one unwired slot,etc.
  (define (propagate model unwired index)
    ;; Do the assignment in the model itself, and then propagate it as
    ;; far up and down the tree as possible.
    (define (wire!? model unwired index)
      ;; Wire the name UNWIRED to offset INDEX in the MODEL if that slot
      ;; is available, and return a boolean indicating whether it was
      ;; done.
      (and (memq unwired (stackopt/model/unwired model))
	   (stackopt/free-index? model index)
	   (let ((bucket (- (stackopt/model/n-unwired model) 1)))
	     (stackopt/wire! model (list (cons unwired index)))
	     (vector-set! buckets bucket
			  (delq model (vector-ref buckets bucket)))
	     ;; Move this model to a bucket indicating the next
	     ;; available location to be filled.
	     (if (not (zero? bucket))
		 (let ((bucket* (- bucket 1)))
		   (vector-set! buckets bucket*
				(cons model (vector-ref buckets bucket*)))))
	     true)))

    (define (try-up model unwired index)
      ;; Try to wire UNWIRED to offset INDEX in this MODEL and all of
      ;; its parents.  Stops when it can't be wired or the top of the
      ;; frame tree is encountered.
      (let loop ((model model))
	(and model
	     (wire!? model unwired index)
	     (loop (stackopt/model/parent model)))))
	     
    (define (try-down model unwired index)
      ;; Try to wire UNWIRED to offset INDEX in this MODEL and all of
      ;; its descendents.  Stops when it can't be wired any lower in
      ;; this branch of the frame tree.
      (let walk ((model model))
	(and (wire!? model unwired index)
	     (for-each walk (stackopt/model/children model)))))

    (if (not (wire!? model unwired index))
	(internal-error "STACKOPT/REARRANGE/PROCESS!: Can't wire"
			model unwired index))
    (try-up (stackopt/model/parent model) unwired index)
    (for-each (lambda (model*)
		(try-down model* unwired index))
	      (stackopt/model/children model)))

  (define (find-wired model models*)
    ;; Return the first model in MODELS* which has already decided on
    ;; a binding for one of the unwired variables in MODEL and for
    ;; which that same binding slot is available in MODEL; otherwise #F.
    (and (not (null? models*))
	 (let ((model* (car models*)))
	   (or (list-search-positive (stackopt/model/wired model*)
		 (lambda (wired*)
		   (and (memq (car wired*) (stackopt/model/unwired model))
			(stackopt/free-index? model (cdr wired*)))))
	       (find-wired model (cdr models*))))))

  (define (pick-to-wire model)
    ;; Assigns an unwired variable to a free index at random.
    (cons (pick-random (stackopt/model/unwired model))
	  (pick-random (stackopt/free-indices model))))

  (define (phase-2)
    ;; For all of the frames that have more than one free slot, grab
    ;; the most highly constrained frame (fewest free slots), assign
    ;; an unwired variable, propagate, and repeat from phase-1 until
    ;; there are no models remaining.
    (let ((bucketlen (vector-length buckets)))
      (let loop ((i 1))
	(and (< i bucketlen)
	     (if (null? (vector-ref buckets i))
		 (loop (1+ i))
		 (let* ((model (car (vector-ref buckets i)))
			(children (stackopt/model/children model))
			(to-wire
			 (or (find-wired
			      model
			      (if (stackopt/model/parent model)
				  (cons (stackopt/model/parent model)
					children)
				  children))
			     (pick-to-wire model))))
		   (propagate model (car to-wire) (cdr to-wire))
		   (phase-1)))))))

  (define (phase-1)
    ;; For all of the models that have only one free slot available,
    ;; wire their first unwired variable to that slot and propagate
    ;; that choice up and down the tree.  This may promote other
    ;; models to having only one free slot, so the iteration doesn't
    ;; terminate in the obvious manner.  When all remaining models
    ;; have more than one free slot, go on to phase-2.
    (let ((bucket0 (vector-ref buckets 0)))
      (if (null? bucket0)
	  (phase-2)
	  (let* ((model (car bucket0))
		 (unwired (car (stackopt/model/unwired model)))
		 (index (car (stackopt/free-indices model))))
	    (vector-set! buckets 0 (delq model bucket0))
	    (propagate model unwired index)
	    (phase-1)))))

  (phase-1))

(define (stackopt/update-frame! model)
  ;; Calculate offsets for all elements in this model's frame by first
  ;; using the wired offsets and then filling in order from the
  ;; unwired list.
  (let* ((frame  (stackopt/model/frame model))
	 (len    (vector-length frame))
	 (frame* (make-vector len false)))
    (for-each (lambda (wired)
		(let ((name  (car wired))
		      (index (cdr wired)))
		  (if (vector-ref frame* index)
		      (stackopt/inconsistency model)
		      (vector-set! frame* index name))))
	      (stackopt/model/wired model))
    (let loop ((i (- len 1))
	       (unwired (stackopt/model/unwired model)))
      (cond ((negative? i)
	     (if (not (null? unwired))
		 (stackopt/inconsistency model)))
	    ((vector-ref frame* i)	; This slot wired
	     (loop (- i 1) unwired))
	    ((null? unwired)
	     (stackopt/inconsistency model))
	    (else
	     (vector-set! frame* i (car unwired))
	     (loop (- i 1) (cdr unwired)))))
    (stackopt/clobber! frame frame*)))

(define (stackopt/free-index? model index)
  ;; #T iff the index-th entry in the frame is not in use for a wired
  ;; value.
  (let ((len (vector-length (stackopt/model/frame model))))
    (and (< index len)
	 (not (rassq index (stackopt/model/wired model))))))

(define (stackopt/free-indices model)
  ;; Return a list of all offsets in the frame that aren't currently
  ;; in use for a wired value.
  (let* ((len (vector-length (stackopt/model/frame model)))
	 (frame* (make-vector len true)))
    (for-each (lambda (wired)
		(vector-set! frame* (cdr wired) false))
	      (stackopt/model/wired model))
    (let loop ((index 0)
	       (free '()))
      (cond ((= index len)
	     free)
	    ((vector-ref frame* index)
	     (loop (+ index 1)
		   (cons index free)))
	    (else
	     (loop (+ index 1) free))))))

(define (stackopt/wire! model pairs)
  ;; Each element of PAIRS is (<var> . <offset>)
  (let ((wired* (append pairs (stackopt/model/wired model)))
	(unwired* (delq* (map car pairs)
			 (stackopt/model/unwired model))))
    (set-stackopt/model/wired! model wired*)
    (set-stackopt/model/unwired! model unwired*)
    (set-stackopt/model/n-unwired! model (length unwired*))))

(define (stackopt/inconsistency model)
  (internal-error "Inconsistent wiring" model))

(define (stackopt/clobber! v1 v2)
  ;; Copy the values from v2 into v1 (sort of like "v1 := v2")
  (do ((i (- (vector-length v1) 1) (- i 1)))
      ((< i 0) 'done)
    (vector-set! v1 i (vector-ref v2 i))))

(define (stackopt/new-name prefix)
  (new-variable prefix))

(define-structure (stackopt/model
		   (conc-name stackopt/model/)
		   (constructor stackopt/model/%make (parent frame name)))
  (parent false read-only true)
  (frame false read-only true)		; Vector of variable names
  (name  false read-only true)		; Frame variable name
  (wired '() read-only false)		; List mapping names to offsets
  (unwired '() read-only false)		; List of names, currently
					; without offsets
  (form false read-only false)
  (children '() read-only false)
  (n-unwired false read-only false)
  (extended? false read-only false))

(define (stackopt/model/make parent frame name wire-all? dont-reorder?)
  ;; DONT-REORDER? is used to prevent moving continuations to the
  ;; front; it really implies more than just suppression of
  ;; reordering.  This is basically a patch to avoid fixing a harder
  ;; problem: ignored-continuations are being closed over and passed
  ;; as procedure arguments.
  (let ((new (stackopt/model/%make parent frame name)))
    (if parent
	(set-stackopt/model/children! parent
				      (cons new
					    (stackopt/model/children parent))))
    (call-with-values
     (lambda ()
       (if dont-reorder?
	   (values '() (vector->list frame))
	   (list-split (vector->list frame) continuation-variable?)))
     (lambda (cont-vars others)
       (cond ((null? cont-vars) 'OK)
	     ((null? (cdr cont-vars))
	      (set-stackopt/model/wired! new `((,(car cont-vars) . 0))))
	     (else (internal-error
		    "STACKOPT/MODEL/MAKE: multiple continuation variables"
		    frame)))
       (if wire-all?
	   (let* ((zero-counts (iota (length others)))
		  (counts (if (null? cont-vars)
			      zero-counts
			      (map 1+ zero-counts))))
	     (set-stackopt/model/wired! new
	      (append (stackopt/model/wired new)
		      (map cons others counts)))))))
    new))

;; This is more general than it needs to be, because it accomodates
;; partially wired frames.

(define (stackopt/constrain model common sup-index)
  ;; MODEL is a model to be processed
  ;; COMMON is the list of variables that appears in the model's frame
  ;;        and all of its descendent frames
  ;; SUP-INDEX is the size of the smallest frame appearing in
  ;;           the tree of frames rooted in this model's frame.
  ;;
  ;; Returns a mapping from names in the COMMON set to fixed stack
  ;; offsets.  This might not provide locations for all values,
  ;; because it treats a wiring in any frame as though it applied to
  ;; all frames. Later we will generate final assignments that allow
  ;; assignments in one frame configuration to be wired but will use
  ;; the same slot for another purpose in a different configuration.

  (define (walk model pairs)
    ;; Each element of PAIRS is (<name> <possible offsets for this name>)
    ;; Returns a similar list of pairs, but the possible offsets have
    ;; been corrected to account for wired down names.  The entry for
    ;; a name may become '() if there's no place to put it (i.e. you
    ;; lose track of the name if it can't go anywhere).
    (if (null? pairs)
	pairs
	(fold-right
	 walk
	 (let ((wired (stackopt/model/wired model)))
	   (if (not wired)
	       pairs
	       (let ((nogood (map cdr wired)))
		 (append-map
		  (lambda (pair)
		    (let* ((name (car pair))
			   (place (assq name wired)))
		      (cond ((not place)
			     (let ((possible (difference (cadr pair) nogood)))
			       (if (null? possible)
				   '()	; Nowhere to go
				   (list (list name possible)))))
					; Anywhere but the wired locations
			    ((memq (cdr place) (cadr pair))
			     (list (list name (list (cdr place)))))
					; Wired location is free, so that's it
			    (else '())))) ; Wired but slot's not free
		  pairs))))
	 (stackopt/model/children model))))

  (call-with-values
      (lambda ()
	(list-split (walk model
			  (map (lambda (common)
				 (list common (iota sup-index)))
			       common))
		    (lambda (pair)
		      (referenced-continuation-variable? (car pair)))))
    (lambda (cont-variables rest)
      ;; At least the continuation variable must be shared if there are
      ;; any children frames, and the continuation must be in slot 0.
      (cond ((null? cont-variables)
	     ;; This is no longer true.  A better test would be that the
	     ;; continuation variables must be shared across non-leaf models
	     ;; (if (not (null? (stackopt/model/children model)))
	     ;;     (internal-error "No continuation variables shared"
	     ;;                     model common))
	     (stackopt/constrain* rest))
	    ((not (null? (cdr cont-variables)))
	     (internal-error "Too many continuation variables"
			     model common))
	    ((not (memq 0 (cadr (car cont-variables))))
	     (internal-error "Unexpected offset for shared continuation"
			     model (car cont-variables)))
	    (else
	     (stackopt/constrain* (cons (list (car (car cont-variables)) '(0))
					rest)))))))

(define (stackopt/constrain* pairs)
  ;; PAIRS maps names to possible stack offset locations
  ;; Returns a mapping from names to fixed stack offsets.  This may
  ;; not provide locations for all values originally in PAIRS.
  (call-with-values
      (lambda ()
	(list-split pairs
		    (lambda (pair)
		      (null? (cdr (cadr pair))))))
    (lambda (wired free)
      ;; WIRED variables now have no other place they can go
      (let loop ((wired (map (lambda (pair)
			       (cons (car pair) (car (cadr pair))))
			     wired))
		 (free free))
	(if (null? free)
	    wired
	    ;; This is not necessarily a good choice
	    (let* ((next (car free))
		   (index (list-search-negative (cadr next)
			    (lambda (index)
			      (rassq index wired)))))
	      (loop (if (not index)
			wired
			(cons (cons (car next) index)
			      wired))
		    (cdr free))))))))
