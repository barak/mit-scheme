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

;;;; ??
;;; package: (compiler midend)

(declare (usual-integrations))

(define *rtlgen/procedures*)
(define *rtlgen/continuations*)
(define *rtlgen/object-queue*)
(define *rtlgen/delayed-objects*)
(define *rtlgen/fold-tag-predicates?* true)
(define *rtlgen/fold-simple-value-tests?* #T)

(define *rtlgen/quick&dirty-interrupt-check-map*)

;; Does not currently work if #F:
(define *rtlgen/pre-load-stack-frame?* #T)

(define (rtlgen/top-level program)
  (initialize-machine-register-map!)
  (fluid-let ((*rtlgen/object-queue* (queue/make))
	      (*rtlgen/delayed-objects* '())
	      (*rtlgen/procedures* '())
	      (*rtlgen/continuations* '())
	      (*rtlgen/quick&dirty-interrupt-check-map* (make-form-map)))
    (call-with-values
	(lambda ()
	  (if *procedure-result?*
	      (rtlgen/top-level-procedure program)
	      (rtlgen/expression program)))
      (lambda (root label)
	(queue/drain! *rtlgen/object-queue* rtlgen/dispatch)
	(set! *entry-label* label)
	(append! root
		 (fold-right append!
			     (fold-right append! '()
					 (reverse! *rtlgen/continuations*))
			     (reverse! *rtlgen/procedures*)))))))

(define (rtlgen/debugging-info form)
  (code-rewrite/original-form/previous form))

(define (rtlgen/expression form)
  (let ((label (rtlgen/new-name 'EXPRESSION)))
    (values (rtlgen/%%procedure label form form #F rtlgen/wrap-expression)
	    label)))

(define (rtlgen/top-level-procedure form)
  (define (fail)
    (internal-error
     "Improperly formatted top-level procedure expression"))
  (define result (form/match rtlgen/outer-expression-pattern form))
  (if (not result)
      (fail))
  (let ((continuation-name (cadr (assq rtlgen/?cont-name result)))
	(env-name          (cadr (assq rtlgen/?env-name result))))
    (let loop ((body  (third form)))
      (cond
       ((and (LET/? body)
	     (for-all? (let/bindings body)
	       (lambda (binding)
		 (form/static? (cadr binding)))))
	(loop (let/body body)))
       ((LETREC/? body)
	(rtlgen/letrec/bindings (letrec/bindings body))
	(loop (letrec/body body)))
       ((form/match rtlgen/top-level-trivial-closure-pattern body)
	=> (lambda (result)
	     (sample/1 '(rtlgen/procedures-by-kind histogram)
		       'Top-level-trivial-closure)
	     (let ((cont-name  (cadr (assq rtlgen/?cont-name result)))
		   (lam-expr   (cadr (assq rtlgen/?lambda-expression result))))
	       (if (not (eq? continuation-name cont-name))
		   (fail))
	       (let* ((label (rtlgen/new-name 'TOP-LEVEL))
		      (code (rtlgen/%%procedure
			     label
			     lam-expr	;dbg-form  form
			     lam-expr
			     #F
			     rtlgen/wrap-trivial-closure)))
		 (values code label)))))
       ((form/match rtlgen/top-level-heap-closure-pattern body)
	=> (lambda (result)
	     (sample/1 '(rtlgen/procedures-by-kind histogram)
		       'Top-level-heap-closure)
	     (let ((cont-name  (cadr (assq rtlgen/?cont-name result)))
		   (lam-expr   (cadr (assq rtlgen/?lambda-expression result))))
	       (if (not (eq? continuation-name cont-name))
		   (fail))
	       (let* ((label (rtlgen/new-name 'TOP-LEVEL-CLOSURE))
		      (code
		       (rtlgen/%%procedure
			label
			lam-expr ;dbg-form  form
			`(LAMBDA (,cont-name ,env-name)
			   ,body)
			'SELF-ARG
			rtlgen/wrap-trivial-closure)))
		 (set! *procedure-result?* 'CALL-ME)
		 (values code label)))))
       (else
	(sample/1 '(rtlgen/procedures-by-kind histogram)
		  'top-level-expression)
	(let* ((label (rtlgen/new-name 'EXPRESSION))
	       (code
		(rtlgen/%%procedure label
				    form 
				    `(LAMBDA (,continuation-name ,env-name)
				       ,body)
				    #F
				    rtlgen/wrap-trivial-closure)))
	  (set! *procedure-result?* 'CALL-ME)
	  (values code label)))
       ;;(else (fail))
       ))))

(define-structure
    (rtlgen/descriptor
     (conc-name rtlgen/descriptor/)
     (constructor rtlgen/descriptor/make))
  kind
  label
  object)

(define (rtlgen/dispatch desc)
  (let ((kind   (rtlgen/descriptor/kind  desc))
	(label  (rtlgen/descriptor/label desc))
	(object (rtlgen/descriptor/object desc)))
    (sample/1 '(rtlgen/procedures-by-kind histogram) kind)
    (case kind
      ((CONTINUATION)    (rtlgen/continuation label object))
      ((PROCEDURE)       (rtlgen/procedure label object))
      ((CLOSURE)         (rtlgen/closure label object))
      ((TRIVIAL-CLOSURE) (rtlgen/trivial-closure label object))
      (else              (internal-error "Unknown object kind" desc)))))

(define (rtlgen/enqueue! desc)
  (queue/enqueue! *rtlgen/object-queue* desc))

(define (rtlgen/trivial-closure label lam-expr)
  (rtlgen/%procedure label lam-expr #F rtlgen/wrap-trivial-closure))

(define (rtlgen/closure label lam-expr)
  (rtlgen/%procedure label lam-expr #T rtlgen/wrap-closure))

(define (rtlgen/procedure label lam-expr)
  (rtlgen/%procedure label lam-expr #F rtlgen/wrap-procedure))

(define (rtlgen/%procedure label lam-expr self-arg? wrap)
  (set! *rtlgen/procedures*
	(cons (rtlgen/%%procedure label lam-expr lam-expr self-arg? wrap)
	      *rtlgen/procedures*))
  unspecific)

(define (rtlgen/%%procedure label dbg-form lam-expr self-arg? wrap)
  ;; This is called directly for top-level expressions and procedures.
  ;; All other calls are from rtlgen/%procedure which adds the result
  ;; to the list of all procedures (*rtlgen/procedures*)
  (rtlgen/%body-with-stack-references label dbg-form lam-expr self-arg? wrap
   (lambda ()
     (let ((lambda-list (lambda/formals lam-expr))
	   (body        (lambda/body lam-expr)))
       (rtlgen/body
	body
	(lambda (body*) (wrap label dbg-form body* lambda-list 0))
	(lambda () (rtlgen/initial-state lambda-list self-arg? false body)))))))

(define (rtlgen/wrap-expression label dbg-form body lambda-list saved-size)
  lambda-list				; Not used
  saved-size				; only continuations
  (cons `(EXPRESSION ,label ,(new-dbg-expression->old-dbg-expression
			      label
			      (rtlgen/debugging-info dbg-form)))
	(rtlgen/wrap-with-interrupt-check/expression
	 body
	 `(INTERRUPT-CHECK:CONTINUATION ,label (MACHINE-CONSTANT 1)))))

(define (rtlgen/wrap-continuation label dbg-form body lambda-list saved-size)
  (let* ((arity (lambda-list/count-names lambda-list))
	 (frame-size
	  (+ (- saved-size 1)		; Don't count the return address
	     (- arity
		(min arity (rtlgen/number-of-argument-registers))))))
    (cons `(RETURN-ADDRESS ,label
			   ,(new-dbg-continuation->old-dbg-continuation
			     label
			     frame-size
			     (rtlgen/debugging-info dbg-form))
			   (MACHINE-CONSTANT ,frame-size)
			   (MACHINE-CONSTANT 1))
	  ;; Kludge
	  (if (rtlgen/quick&dirty/forbid-interrupt-check? dbg-form)
	      body
	      (rtlgen/wrap-with-interrupt-check/continuation
	       body
	       `(INTERRUPT-CHECK:CONTINUATION ,label (MACHINE-CONSTANT 2)))))))

(define (rtlgen/wrap-closure label dbg-form body lambda-list saved-size)
  saved-size				; only continuations have this
  (let ((frame-size (lambda-list/count-names lambda-list)))
    (cons `(CLOSURE ,label
		    ,(new-dbg-procedure->old-dbg-procedure
		      label
		      'CLOSURE
		      (rtlgen/debugging-info dbg-form))
		    (MACHINE-CONSTANT ,frame-size))
	  (rtlgen/wrap-with-interrupt-check/procedure
	   true
	   body
	   `(INTERRUPT-CHECK:CLOSURE (MACHINE-CONSTANT ,frame-size))))))

(define (rtlgen/wrap-trivial-closure label dbg-form body lambda-list saved-size)
  saved-size				; only continuations have this
  (let ((frame-size (lambda-list/count-names lambda-list))
	(procedure-header
	 `(TRIVIAL-CLOSURE ,label
			   ,(new-dbg-procedure->old-dbg-procedure
			     label
			     'TRIVIAL-CLOSURE
			     (rtlgen/debugging-info dbg-form))
			   ,@(map
			      (lambda (value)
				`(MACHINE-CONSTANT ,value))
			      (lambda-list/arity-info lambda-list)))))
    (if (rtlgen/omit-interrupt-check? label)
	(cons procedure-header
	      body)
	(cons procedure-header
	      (rtlgen/wrap-with-interrupt-check/procedure
	       true
	       body
	       `(INTERRUPT-CHECK:PROCEDURE
		 ,label
		 (MACHINE-CONSTANT ,frame-size)))))))

(define (rtlgen/wrap-procedure label dbg-form body lambda-list saved-size)
  saved-size				; only continuations have this
  (let* ((frame-size (lambda-list/count-names lambda-list))
	 (procedure-header
	  `(PROCEDURE ,label
		      ,(new-dbg-procedure->old-dbg-procedure
			label
			'PROCEDURE
			(rtlgen/debugging-info dbg-form))
		      (MACHINE-CONSTANT ,frame-size))))
    (if (rtlgen/omit-interrupt-check? label)
	(cons procedure-header
	      body)
	(cons procedure-header
	      (rtlgen/wrap-with-interrupt-check/procedure
	       false
	       body
	       `(INTERRUPT-CHECK:PROCEDURE ,label
					   (MACHINE-CONSTANT ,frame-size)))))))

(define (rtlgen/continuation label lam-expr)
  (set! *rtlgen/continuations*
	(cons (rtlgen/%%continuation
	       label lam-expr lam-expr rtlgen/wrap-continuation)
	      *rtlgen/continuations*))
  unspecific)

(define *rtlgen/frame-size* false)

(define (rtlgen/->number-of-args-on-stack lambda-list frame-vector)
  ;; The lambda list is like (cont arg1 ... argn) including #!optional, etc.
  ;; The frame-vector is #(saved1 ... savedm argk+1 ... argn)
  ;; Returns n-k
  ;; NOTE: Assumes that the arguments passed on the stack are taken
  ;; from the end of the formal parameter list.
  (let ((n (vector-length frame-vector)))
    (let loop ((lst  (reverse (lambda-list->names lambda-list)))
	       (i    (- n 1)))
      (if (or (null? lst)
	      (negative? i)
	      (not (eq? (vector-ref frame-vector i) (car lst))))
	  (- n i 1)
	  (loop (cdr lst) (- i 1))))))

(define (rtlgen/%%continuation label dbg-form lam-expr wrap)
  (rtlgen/%body-with-stack-references
   label dbg-form lam-expr #F wrap
   (lambda ()
     (internal-error "continuation without stack frame" lam-expr))))

#|
(define (rtlgen/%body-with-stack-references
	 label dbg-form lam-expr self-arg? wrap no-stack-refs)
  (sample/1 '(rtlgen/formals-per-lambda histogram vector)
	    (lambda-list/count-names (lambda/formals lam-expr)))
  (cond ((form/match rtlgen/continuation-pattern lam-expr)
	 => (lambda (result)
	      (let ((lambda-list  (cadr (assq rtlgen/?lambda-list result)))
		    (frame-vector (cadr (assq rtlgen/?frame-vector result)))
		    (body         (cadr (assq rtlgen/?continuation-body
					      result))))
		(let ((frame-size (vector-length frame-vector)))
		  (sample/1 '(rtlgen/frame-size histogram) frame-size)
		  (fluid-let ((*rtlgen/frame-size* frame-size))
		    (rtlgen/body
		     body
		     (lambda (body*)
		       (let ((saved-size
			      (- frame-size
				 (rtlgen/->number-of-args-on-stack
				  lambda-list frame-vector))))
			 (wrap label dbg-form body* lambda-list saved-size)))
		     (lambda ()
		       (rtlgen/initial-state lambda-list self-arg?
					     frame-vector body))))))))
	(else (no-stack-refs))))
|#

;; This version recognizes continuation bodies containing calls to %halt.
;; This means that the continuation should never be called.
;; Currently, these continuations are compiled to calls to the global
;; 1-argument procedure named in the %halt literal operand.  The
;; procedure must signal an error.  This device is a platform
;; independent way of issuing a `trap' instruction.  The procedure
;; receives the continuation's (first) argument.  The continuation for
;; the call is the current continuation (which has already been set
;; up), which is essentially free to compute and gives good debugging
;; information by, in effect, causing an infine trapping loop.

(define (rtlgen/%body-with-stack-references
	 label dbg-form lam-expr self-arg? wrap no-stack-refs)
  (sample/1 '(rtlgen/formals-per-lambda histogram vector)
	    (lambda-list/count-names (lambda/formals lam-expr)))

  (let ((result (form/match rtlgen/continuation-pattern lam-expr)))
    (if result
	(let ((lambda-list  (cadr (assq rtlgen/?lambda-list result)))
	      (frame-vector (cadr (assq rtlgen/?frame-vector result)))
	      (body         (cadr (assq rtlgen/?continuation-body result))))
	  (let* ((frame-size (vector-length frame-vector))
		 (saved-size (- frame-size
				(rtlgen/->number-of-args-on-stack
				 lambda-list frame-vector)))
		 (error-continuation?
		  (and (CALL/? body)
		       (QUOTE/? (call/operator body))
		       (eq? %halt (quote/text (call/operator body))))))
	    (sample/1 '(rtlgen/frame-size histogram) frame-size)
	    (fluid-let ((*rtlgen/frame-size* frame-size))

	      (if error-continuation?

		  (rtlgen/with-body-state
		   (lambda ()
		     (let ((trap-procedure (quote/text (call/operand1 body))))
		       (rtlgen/quick&dirty/forbid-interrupt-check! dbg-form)
		       (wrap label dbg-form
			     `((INVOCATION:GLOBAL-LINK 2 ,label
						       ,trap-procedure))
			     lambda-list saved-size))))

		  (rtlgen/body
		   body
		   (lambda (body*)
		     (wrap label dbg-form body* lambda-list saved-size))
		   (lambda ()
		     (rtlgen/initial-state lambda-list self-arg?
					   frame-vector body)))))))
	(no-stack-refs))))

(define (rtlgen/initial-state params self-arg? frame-vector body)
  ;; . PARAMS is a lambda list
  ;; . SELF-ARG? is true if the entry is a closure body (i.e. closure passed
  ;;   in standard unboxed place)
  ;; . FRAME-VECTOR is a description of parameters on the stack or #F
  ;; . BODY is the procedure/continuation/closure body  
  (define env '())
  (define (add-binding! name reg home)
    (let ((binding  (rtlgen/binding/make name reg home)))
      (set! env (cons binding env))
      binding))

  (define register-arg-positions-used '())
  (define (add-used! home i)
    (if (rtlgen/register? home)
	(set! register-arg-positions-used
	      (cons i register-arg-positions-used))))

  (define (do-register-params params)
    (let ((first-stack-param		; stop at first stack param
	   (if frame-vector
	       (let ((n-on-stack
		      (rtlgen/->number-of-args-on-stack params frame-vector)))
		 (if (zero? n-on-stack)
		     #F
		     (vector-ref frame-vector
				 (- (vector-length frame-vector)
				    n-on-stack))))
	       #F)))
      (let loop ((params params)
		 (i 0))
	(cond ((or (null? params) (eq? (car params) first-stack-param))
	       'done)
	      ((memq (car params) '(#!rest #!optional))
	       (loop (cdr params) i))
	      (else
	       (let* ((home  (rtlgen/argument-home i))
		      (reg   (rtlgen/new-reg))
		      (home-syllable (and (rtlgen/register? home) home)))
		 (rtlgen/emit!/1 `(ASSIGN ,reg ,home))
		 (add-binding! (car params) reg home-syllable)
		 (add-used! home i)
		 (loop (cdr params) (+ i 1))))))))

  (define (do-continuation name stack-offset)
    ;; We previously removed the assignment if NAME wasn't a
    ;; referenced-continuation-variable, but that caused problems
    ;; because "unreferenced" in this case actually means "never
    ;; invoked", not "never passed as an argument"!  However, we
    ;; must be careful to make sure we dont think that the
    ;; unreferenced continuation has a stack slot!
    (let* ((used?     (referenced-continuation-variable? name))
	   (source    (cond ((not used?)
			     `(CONSTANT unused-continuation-variable))
			    ((rtlgen/cont-in-stack?)
			     (rtlgen/stack-ref stack-offset))
			    (else
			     (rtlgen/reference-to-cont))))
	   (home      (if used? source #F))
	   (coerce?   (and used? (rtlgen/tagged-entry-points?)))
	   (raw-reg   (rtlgen/new-reg))
	   (cont-reg  (if coerce? (rtlgen/new-reg) raw-reg)))
      (rtlgen/emit!/1
       `(ASSIGN ,raw-reg ,source))
      (if coerce?
	  (rtlgen/emit!/1
	   `(ASSIGN ,cont-reg (COERCE-VALUE-CLASS ,raw-reg ADDRESS))))
      (add-binding! name cont-reg home)))

  (define (do-closure name stack-offset)
    (let* ((source   (if (rtlgen/closure-in-stack?)
			 (rtlgen/stack-ref stack-offset)
			 (rtlgen/reference-to-closure)))
	   (coerce?  (rtlgen/tagged-entry-points?))
	   (raw-reg  (rtlgen/new-reg))
	   (closure-reg (if coerce? (rtlgen/new-reg) raw-reg)))
      (rtlgen/emit!/1
       `(ASSIGN ,raw-reg ,source))
      (if coerce?
	  (rtlgen/emit!/1
	   `(ASSIGN ,closure-reg (COERCE-VALUE-CLASS ,raw-reg ADDRESS))))
      (add-binding! name closure-reg source)))

  (let* ((continuation-name  (if (and (pair? params)
				      (continuation-variable? (car params)))
				 (car params)
				 #F))
	 (sans-cont          (if continuation-name (cdr params) params))
	 (closure-name       (if (and self-arg?
				      (pair? sans-cont)
				      (closure-variable? (car sans-cont)))
				 (car sans-cont)
				 #F))
	 (sans-special       (if closure-name (cdr sans-cont) sans-cont))

	 (receives-continuation?
	  (and continuation-name
	       (referenced-continuation-variable? continuation-name)))
	 (closure-offset       (and (rtlgen/closure-in-stack?)
				    (if closure-name 0 #F)))
	 (continuation-offset  (and (rtlgen/cont-in-stack?)
				    receives-continuation?
				    (if closure-offset 1 0)))
	 (stack-offset-adjustment  (+ 1 (max (or closure-offset -1)
					     (or continuation-offset -1)))))

    (do-register-params sans-special)
    (let* ((closure-binding
	    (and closure-name (do-closure closure-name closure-offset)))
	   (continuation-binding
	    (and continuation-name
		 (do-continuation continuation-name continuation-offset))))

      (rtlgen/state/stmt/make
       (if frame-vector
	   (rtlgen/initial-stack-state
	    env register-arg-positions-used
	    stack-offset-adjustment
	    frame-vector body)
	   env)
       (and receives-continuation? continuation-binding)
       closure-binding
       (+ (if frame-vector
	      (vector-length frame-vector)
	      0)
	  stack-offset-adjustment)))))


(define (rtlgen/find-preferred-call stmt)
  ;; (values call operator unconditional?)
  (define (tail-call? form)
    (let ((cont (call/continuation form)))
      (or (LOOKUP/? cont)
	  (CALL/%stack-closure-ref? cont))))

  (let ((unconditional? true)
	(tail-call  false)
	(other-call false)
	(any-call   false))
    (let walk ((form stmt))
      (and (pair? form)
	   (case (car form)
	     ((CALL)
	      (if (LOOKUP/? (call/operator form))
		  (if (and (not tail-call) (tail-call? form))
		      (set! tail-call form)
		      (set! other-call form))
		  (set! any-call form))
	      unspecific)
	     ((LET)
	      (walk (let/body form)))
	     ((IF)
	      (set! unconditional? false)
	      (walk (if/consequent form))
	      (walk (if/alternate form)))
	     ((BEGIN)
	      (walk (car (last-pair (cdr form)))))
	     (else
	      false))))
    (let ((call (or tail-call other-call any-call)))
      (values call (and call (call/operator call)) unconditional?))))

(define (rtlgen/initial-stack-state
	 env register-arg-positions-used
	 stack-offset-adjustment
	 frame-vector body)

  (define (first-stack-offset)
    (+ (vector-length frame-vector)
       stack-offset-adjustment
       -1))

  (define (default env handled)
    ;; Continuation dealt with specially
    (let loop ((stack-offset (first-stack-offset))
	       (i   0)
	       (env env))
      (cond ((= i (vector-length frame-vector)) env)
	    ((continuation-variable? (vector-ref frame-vector i))
	     (loop (- stack-offset 1) (+ i 1) env))
	    (else
	     (loop (- stack-offset 1)
		   (+ i 1)
		   (let ((name (vector-ref frame-vector i)))
		     (if (memq name handled)
			 env
			 (cons (let ((home (rtlgen/stack-ref stack-offset)))
				 (rtlgen/binding/make
				  name
				  (if *rtlgen/pre-load-stack-frame?*
				      (rtlgen/->register home)
				      home)
				  home))
			       env))))))))

  ;; Try to target register assignments from stack locations
  (call-with-values
      (lambda () (rtlgen/find-preferred-call body))
    (lambda (call rator unconditional?)
      unconditional?			; ignored
      (if (or (not call) (QUOTE/? rator))
	  ;; THIS IS OVERKILL.  We need to analyze the "known operators" and do
	  ;; something to target well for things like %internal-apply.
	  ;; Or ditch this and have Daniel write a good register
	  ;; allocator.
	  (default env '())
	  (let ((max-index    (rtlgen/number-of-argument-registers))
		(first-offset (first-stack-offset)))
	    ;; Directly target the arguments registers for a likely
	    ;; call and move any stack references into the argument
	    ;; registers for that particular call.  All other stack
	    ;; references will be targeted to default locations.
	    (let target ((rands (call/operands call))
			 (env   env)
			 (names '())
			 (arg-position 0))
	      (cond ((or (null? rands) (>= arg-position max-index))
		     (default env names))
		    ((CALL/%stack-closure-ref? (car rands))
		     (let ((name (quote/text (CALL/%stack-closure-ref/name (car rands))))
			   (offset
			    (- first-offset
			       (CALL/%stack-closure-ref/index (car rands)))))
		       (if (or (memq name names)
			       (memq arg-position register-arg-positions-used))
			   (target (cdr rands) env names (+ arg-position 1))
			   (let* ((home (rtlgen/argument-home arg-position))
				  (reg (rtlgen/new-reg)))
			     (rtlgen/emit!
			      (list
			       (rtlgen/read-stack-loc home offset)
			       `(ASSIGN ,reg ,home)))
			     (target (cdr rands)
				     `(,(rtlgen/binding/make
					 name
					 reg
					 (rtlgen/stack-offset offset))
				       . ,env)
				     (cons name names)
				     (+ arg-position 1))))))
		    (else
		     (target (cdr rands) env names (+ arg-position 1))))))))))

(define *rtlgen/next-rtl-pseudo-register*)
(define *rtlgen/pseudo-register-values*)
(define *rtlgen/pseudo-registers*)
(define *rtlgen/statements*)
(define *rtlgen/words-allocated*)
(define *rtlgen/stack-depth*)
(define *rtlgen/max-stack-depth*)
(define *rtlgen/form-calls-external?*)
(define *rtlgen/form-calls-internal?*)
(define *rtlgen/form-returns?*)

(define (rtlgen/with-body-state thunk)
  (fluid-let ((*rtlgen/next-rtl-pseudo-register* 0)
	      (*rtlgen/pseudo-registers* '())
	      (*rtlgen/pseudo-register-values* '())
	      (*rtlgen/words-allocated* 0)
	      (*rtlgen/stack-depth* 0)
	      (*rtlgen/max-stack-depth* 0)
	      (*rtlgen/statements* (queue/make))
	      (*rtlgen/form-calls-internal?* false)
	      (*rtlgen/form-calls-external?* false)
	      (*rtlgen/form-returns?* false))
    (thunk)))


(define (rtlgen/body form wrap gen-state)
  (rtlgen/with-body-state
   (lambda ()
     (rtlgen/stmt (gen-state) form)
     (rtlgen/renumber-pseudo-registers!
      (rtlgen/first-pseudo-register-number))
     (wrap (queue/contents *rtlgen/statements*)))))


(define (rtlgen/wrap-with-interrupt-check/expression body desc)
  ;; *** For now, this does not check interrupts.
  ;; The environment must be handled specially ***
  desc					; ignored
  body)

(define (rtlgen/wrap-with-interrupt-check/procedure external? body desc)
  external?				;ignored

  #|
  (pp `((desc , desc)
	(external? , external?)
	(*rtlgen/form-calls-external?* , *rtlgen/form-calls-external?*)
	(*rtlgen/form-calls-internal?* , *rtlgen/form-calls-internal?*)
	(*rtlgen/words-allocated* , *rtlgen/words-allocated*)
	(*rtlgen/max-stack-depth* , *rtlgen/max-stack-depth*)))
  |#

  (rtlgen/wrap-with-intrpt-check
   ;;  This change is required since the internal procedures are being
   ;;  compiled as external procedures (trivial closures) at the
   ;;  moment (this so that they can share entry points).  Old code:
   ;;(and (rtlgen/generate-interrupt-checks?)
   ;;	(or *rtlgen/form-calls-external?*
   ;;	    (and (not external?)
   ;;		 *rtlgen/form-calls-internal?*)))
   (and (rtlgen/generate-interrupt-checks?)
	(or *rtlgen/form-calls-external?*
	    *rtlgen/form-calls-internal?*))
   (and (rtlgen/generate-heap-checks?)
	(not (= *rtlgen/words-allocated* 0))
	*rtlgen/words-allocated*)
   (and (rtlgen/generate-stack-checks?)
	(not (= *rtlgen/max-stack-depth* 0))
	*rtlgen/max-stack-depth*)
   body
   desc))

(define (rtlgen/wrap-with-interrupt-check/continuation body desc)
  ;; For now, this is dumb about interrupt checks.
  (rtlgen/wrap-with-intrpt-check (rtlgen/generate-interrupt-checks?)
				 (and (rtlgen/generate-heap-checks?)
				      (not (= *rtlgen/words-allocated* 0))
				      *rtlgen/words-allocated*)
				 (and (rtlgen/generate-stack-checks?)
				      (not (= *rtlgen/max-stack-depth* 0))
				      *rtlgen/max-stack-depth*)
				 body
				 desc))

(define (rtlgen/wrap-with-intrpt-check calls? heap-check? stack-check?
				       body desc)
  (if (not (or calls? heap-check? stack-check?))
      body
      (cons `(,(car desc) ,calls? ,heap-check? ,stack-check? ,@(cdr desc))
	    body)))

(define #|-integrable|# (rtlgen/emit! insts)
  ;;(pp `(emit ,@insts))
  (queue/enqueue!* *rtlgen/statements* insts))

(define #|-integrable|# (rtlgen/emit!/1 inst)
  ;;(pp `(emit ,inst))
  (queue/enqueue! *rtlgen/statements* inst))


(define (rtlgen/emit!/profile name count)
  (if (and name
	   compiler:generate-profiling-instructions?)
      (rtlgen/emit!/1
       `(PROFILE-DATA (CONSTANT (,name . ,count))))))


(define-integrable (rtlgen/declare-allocation! nwords)
  ;; *** NOTE: This does not currently include floats! ***
  (set! *rtlgen/words-allocated* (+ nwords *rtlgen/words-allocated*))
  unspecific)

(define (rtlgen/declare-stack-allocation! nwords)
  (let ((new (+ nwords *rtlgen/stack-depth*)))
    (set! *rtlgen/stack-depth* new)
    (if (> new *rtlgen/max-stack-depth*)
	(set! *rtlgen/max-stack-depth* new)))
  unspecific)

(define (rtlgen/stack-allocation/protect thunk)	; /compatible ?
  (let ((sd *rtlgen/stack-depth*)
	(msd *rtlgen/max-stack-depth*))
    (let ((result (thunk)))
      (set! *rtlgen/stack-depth* sd)
      (set! *rtlgen/max-stack-depth* msd)
      result)))

(define (rtlgen/emit-alternatives! gen1 gen2 need-merge?)
  ;; The resetting fof *rtlgen/pseudo-register-values* below has been
  ;; commented out because it does not quite do the right thing.  It
  ;; is possible for the generated RTL to have a CFG with some node
  ;; internal to the predicate which dominates the consequent or
  ;; alternate node.  CSE will find and use the value defined at that
  ;; dominator, so we have to keep all of the preservation information.
  ;;
  ;; Example: the node for pair? dominates the node for vector?
  ;;
  ;;    (define (foo x y)
  ;;      (if (and y
  ;;    	  (or (pair? (car x))
  ;;    	      (null? (car x))))
  ;;          (if (vector? (car x))
  ;;    	  (f global (car x)))))

  (let ((merge-label (and need-merge? (rtlgen/new-name 'MERGE))))
    (let ((orig-depth  *rtlgen/stack-depth*)
	  (orig-heap   *rtlgen/words-allocated*)
	  (orig-values *rtlgen/pseudo-register-values*))
      orig-values
      (gen1)
      (if merge-label
	  (rtlgen/emit!/1 `(JUMP ,merge-label)))
      (let ((heap-after-one *rtlgen/words-allocated*))
	(set! *rtlgen/stack-depth* orig-depth)
	(set! *rtlgen/words-allocated* orig-heap)
	;;(set! *rtlgen/pseudo-register-values* orig-values)
	(gen2)
	(if merge-label
	    (rtlgen/emit!/1 `(LABEL ,merge-label)))
	(let ((heap-after-two *rtlgen/words-allocated*))
	  (set! *rtlgen/stack-depth* orig-depth)
	  (if (> heap-after-one heap-after-two)
	      (set! *rtlgen/words-allocated* heap-after-one))
	  ;;(set! *rtlgen/pseudo-register-values* orig-values)
	  unspecific)))))

(define-integrable (rtlgen/register? frob)
  (and (pair? frob)
       (eq? (car frob) 'REGISTER)))

(define-integrable (rtlgen/%pseudo-register? frob)
  (not (null? (cddr frob))))

(define-integrable (rtlgen/%machine-register? frob)
  (null? (cddr frob)))

(define-integrable (rtlgen/machine-register? frob)
  (and (rtlgen/register? frob)
       (rtlgen/%machine-register? frob)))

(define (rtlgen/new-reg)
  (let ((next-reg *rtlgen/next-rtl-pseudo-register*))
    (set! *rtlgen/next-rtl-pseudo-register* (+ next-reg 1))
    (let ((result `(REGISTER ,next-reg PSEUDO)))
      (set! *rtlgen/pseudo-registers* (cons result *rtlgen/pseudo-registers*))
      result)))

(define (rtlgen/renumber-pseudo-registers! base)
  (for-each (lambda (reg)
	      (set-cdr! (cdr reg) '())
	      (set-car! (cdr reg) (+ (cadr reg) base)))
	    *rtlgen/pseudo-registers*))

(define (rtlgen/assign! rand* rand)
  (if (not (rtlgen/register? rand*))
      (internal-error "rtlgen/assign! invoked on non-register"))
  (if (rtlgen/%pseudo-register? rand*)
      ;; Pseudo register
      (set! *rtlgen/pseudo-register-values*
	    (cons (list rand* rand)
		  *rtlgen/pseudo-register-values*)))
  (rtlgen/emit!/1 `(ASSIGN ,rand* ,rand)))

(define (rtlgen/assign!* instructions)
  (for-each
   (lambda (instruction)
     (if (and (pair? instruction)
	      (eq? (first instruction) 'ASSIGN)
	      (rtlgen/register? (second instruction)))
	 (rtlgen/assign! (second instruction) (third instruction))
	 (rtlgen/emit!/1 instruction)))
   instructions))

(define (rtlgen/->register rand)
  (if (rtlgen/register? rand)
      rand
      (let ((rand* (rtlgen/new-reg)))
	(rtlgen/assign! rand* rand)
	rand*)))

(define (rtlgen/value-assignment state value)
  (let ((target (rtlgen/state/expr/target state)))
    (case (car target)
      ((ANY)
       (let ((target* (rtlgen/new-reg))) ; new register even if already in one
	 (rtlgen/assign! target* value)
	 target*))
      ((REGISTER)
       (rtlgen/assign! target value)
       target)
      ((PREDICATE)
       ;; This case is extremely rare - for example, the predicate is a
       ;; %make-heap-closure which does not have a predicate position
       ;; method. In this case we generate the value and test it (even
       ;; though we known a heap closure must be `true').
       (rtlgen/branch/false? state value))
      (else
       (internal-error "Unexpected target for value" target)))))

;;;; Stack and Heap allocation

(define (rtlgen/heap-push! elts)
  (rtlgen/declare-allocation! (length elts))
  (if (rtlgen/heap-post-increment?)
      (rtlgen/heap-push!/post-increment elts)
      (rtlgen/heap-push!/bump-once elts)))

(define (rtlgen/heap-push!/post-increment elts)
  (let ((free (rtlgen/reference-to-free)))
    (for-each
	(lambda (elt)
	  (rtlgen/emit!/1
	   `(ASSIGN (POST-INCREMENT ,free 1) ,(rtlgen/->register elt))))
      elts)))



(define (rtlgen/heap-push!/bump-once elts)
  (let ((free (rtlgen/reference-to-free)))
    (do ((i 0 (+ i 1))
	 (elts elts (cdr elts))
	 (acc '() (cons `(ASSIGN (OFFSET ,free (MACHINE-CONSTANT ,i))
				 ,(rtlgen/->register (car elts)))
			acc)))
	((null? elts)
	 (rtlgen/emit!
	  (reverse!
	   (cons `(ASSIGN ,free (OFFSET-ADDRESS ,free (MACHINE-CONSTANT ,i)))
		 acc)))))))

(define (rtlgen/stack-push! elts)
  (rtlgen/declare-stack-allocation! (length elts))
  (if (rtlgen/stack-pre-increment?)
      (rtlgen/stack-push!/pre-increment elts)
      (rtlgen/stack-push!/bump-once elts)))

(define-integrable (rtlgen/stack-push!/1 elt)
  (rtlgen/stack-push! (list elt)))

(define (rtlgen/stack-push!/pre-increment elts)
  (let ((sp (rtlgen/reference-to-sp)))
    (rtlgen/emit!
     (map (lambda (elt)
	    `(ASSIGN (PRE-INCREMENT ,sp -1) ,(rtlgen/->register elt)))
	  elts))))

(define (rtlgen/stack-push!/bump-once elts)
  (let ((nelts (length elts)))
    (do ((i (- nelts 1) (- i 1))
	 (elts elts (cdr elts))
	 (acc '() (cons (rtlgen/write-stack-loc
			 (rtlgen/->register (car elts))
			 i)
			acc)))
	((null? elts)
	 (rtlgen/emit!
	  (cons (rtlgen/bop-stack-pointer (- 0 nelts))
		(reverse! acc)))))))

(define (rtlgen/stack-pop!)
  (let ((target (rtlgen/new-reg)))
    (rtlgen/%stack-pop! target)
    target))

(define (rtlgen/%stack-pop! target)
  (let ((rsp (rtlgen/reference-to-sp)))
    (if (rtlgen/stack-post-increment?)
	(rtlgen/emit!/1
	 `(ASSIGN ,target (POST-INCREMENT ,rsp 1)))
	(rtlgen/emit!
	 (list (rtlgen/read-stack-loc target 0)
	       (rtlgen/bop-stack-pointer 1))))))

(define (rtlgen/bop-stack-pointer! n)
  (if (not (= n 0))
      (rtlgen/emit!/1 (rtlgen/bop-stack-pointer n))))

;;;; Machine-dependent parameters
;; *** Currently Spectrum-specific ***

;; The rtlgen/reference-* are expected to return an RTL register reference

(define (rtlgen/cont-in-stack?)
  continuation-in-stack?)

(define (rtlgen/closure-in-stack?)
  closure-in-stack?)

(define (rtlgen/reference-to-free)
  (interpreter-free-pointer))

(define-integrable (rtlgen/reference-to-sp)
  (interpreter-stack-pointer))

(define-integrable (rtlgen/stack-ref n)
  `(OFFSET ,(rtlgen/reference-to-sp) (MACHINE-CONSTANT ,n)))

(define-integrable (rtlgen/stack-offset n)
  `(OFFSET-ADDRESS ,(rtlgen/reference-to-sp) (MACHINE-CONSTANT ,n)))

(define #|-integrable|# (rtlgen/bop-stack-pointer n)
  `(ASSIGN ,(rtlgen/reference-to-sp) ,(rtlgen/stack-offset n)))

(define-integrable (rtlgen/read-stack-loc reg n)
  `(ASSIGN ,reg ,(rtlgen/stack-ref n)))

(define-integrable (rtlgen/write-stack-loc reg n)
  `(ASSIGN ,(rtlgen/stack-ref n) ,reg))

(define (rtlgen/stack-ref? syllable)
  (and (pair? syllable)
       (eq? (first syllable) 'OFFSET)
       (eq? (second syllable) (rtlgen/reference-to-sp))))

(define (rtlgen/reference-to-regs)
  (interpreter-regs-pointer))


(define (rtlgen/reference-to-cont)
  ;; defined only if not cont-in-stack?
  (interpreter-continuation-register))

(define (rtlgen/reference-to-closure)
  (interpreter-closure-register))

(define (rtlgen/fetch-memtop)
  (interpreter-memtop-register))

(define (rtlgen/fetch-int-mask)
  (interpreter-int-mask-register))

(define (rtlgen/fetch-environment)
  (interpreter-environment-register))

;; *rtlgen/argument-registers*
;; This is a parameter in machin.scm
;; for index = 0, it must be the same as reference-to-val
;; This should leave some temps (e.g. 1, 28, 29, 30)

(define rtlgen/reference-to-val
  (let ((reg (vector-ref *rtlgen/argument-registers* 0)))
    (lambda () `(REGISTER ,reg))))

(define (rtlgen/argument-registers)
  (if (rtlgen/cont-in-stack?)
      (vector->list *rtlgen/argument-registers*)
      (cons (rtl:register-number (rtlgen/reference-to-closure))
	    (vector->list *rtlgen/argument-registers*))))

#|
(define (rtlgen/available-registers available)
  (let ((arg-regs (rtlgen/argument-registers)))
    ;; Order is important!
    (append arg-regs
	    (eq-set-difference (delq rtlgen/cont-register available)
			       arg-regs))))
|#
#|
;; If the incoming raw-continuation register is made available,
;; other code below that implicitly assumes that it is preserved
;; must be changed.  See rtlgen.scm.contin (part of the way there).

(define (rtlgen/available-registers available)
  (let ((arg-regs (rtlgen/argument-registers)))
    ;; Order is important!
    (append arg-regs
	    (eq-set-difference available
			       arg-regs))))
|#
(define (rtlgen/available-registers available)
  (let ((arg-regs (rtlgen/argument-registers)))
    ;; Order is important!
    (append arg-regs
	    (eq-set-difference (if (rtlgen/cont-in-stack?)
				   available
				   (delq (rtl:register-number
					  (rtlgen/reference-to-cont))
					 available))
			       arg-regs))))

(define (rtlgen/number-of-argument-registers)
  (vector-length *rtlgen/argument-registers*))

(define (rtlgen/home-offset reg-index)
  (pseudo-register-offset reg-index))

(define (rtlgen/argument-home index)
  (let ((vlen (vector-length *rtlgen/argument-registers*)))
    (if (< index vlen)
	`(REGISTER ,(vector-ref *rtlgen/argument-registers* index))
	(internal-error "more arguments than registers" index))))

;; rtlgen/interpreter-call/argument-home moved to machin.sc,

(define (rtlgen/first-pseudo-register-number)
  number-of-machine-registers)

(define (rtlgen/number-of-pseudo-register-homes)
  number-of-temporary-registers)

;;;; Machine-dependent parameters (continued)

(define (rtlgen/stack-post-increment?)
  stack-use-pre/post-increment?)

(define (rtlgen/stack-pre-increment?)
  stack-use-pre/post-increment?)

(define (rtlgen/heap-post-increment?)
  heap-use-pre/post-increment?)


(define (rtlgen/indexed-loads? type)
  (machine/indexed-loads? type))

(define (rtlgen/indexed-stores? type)
  (machine/indexed-stores? type))

(define (rtlgen/tagged-entry-points?)
  (not untagged-entries?))

(define (rtlgen/tagged-closures?)
  ;; Closures are represented as entry points
  (rtlgen/tagged-entry-points?))

(define (rtlgen/cont-adjustment)
  ;; This needs to be a parameter in machin.scm
  ;; Distance in bytes between a raw continuation
  ;; (as left behind by JSR) and the real continuation
  ;; (after descriptor)
  (machine/cont-adjustment))

(define (rtlgen/closure-adjustment)
  (closure-environment-adjustment 1 0))

(define-integrable rtlgen/chars-per-object
  (quotient address-units-per-object address-units-per-packed-char))

(define (rtlgen/chars->words nchars)
  ;; Rounds up to word size and includes a zero byte.
  (quotient (+ nchars rtlgen/chars-per-object) rtlgen/chars-per-object))

(define (rtlgen/words->chars nwords)
  (* nwords rtlgen/chars-per-object))

(define rtlgen/fp->words
  (let ((objects-per-float
	 (quotient address-units-per-float address-units-per-object)))
    (lambda (nfp)
      (* objects-per-float nfp))))

(define (rtlgen/closure-first-offset)
  (closure-first-offset 1 0))

(define (rtlgen/closure-prefix-size)
  (closure-object-first-offset 1))

(define (rtlgen/floating-align-free)
  (let ((free (rtlgen/reference-to-free)))
    (rtlgen/emit!/1 `(ASSIGN ,free (ALIGN-FLOAT ,free)))))

(define (rtlgen/generate-interrupt-checks?)
  true)

(define (rtlgen/generate-heap-checks?)
  true)

(define (rtlgen/generate-stack-checks?)
  compiler:generate-stack-checks?)

(define rtlgen/unassigned-object
  (let ((tag (machine-tag 'REFERENCE-TRAP)))
    (lambda ()
      `(CONS-NON-POINTER (MACHINE-CONSTANT ,tag) (MACHINE-CONSTANT 0)))))

(define (rtlgen/preserve-state state)
  ;; (values gen-prefix gen-suffix)
  ;; IMPORTANT: this depends crucially on the fact that variables are
  ;; bound to objects.  The exceptions to this are the continuation
  ;; and variable caches that are treated specially. In the future,
  ;; when variables are bound to floats and other non-objects, they
  ;; will have to be tagged and handled appropriately.

  (define (invoke-thunk thunk)
    (thunk))

  (define (preserve infos)
    (let loop ((infos  infos)
	       (prefix '())
	       (suffix '()))

      (define (%preserve&restore preserve restore)
	(loop (cdr infos)
	      (cons preserve prefix)
	      (cons restore suffix)))

      (define (preserve&restore reg how value)
	(if (not (pair? value))
	    (internal-error "Bad preservation" reg how value))
	(%preserve&restore
	 (lambda () (rtlgen/emit!/1 `(PRESERVE ,reg ,how)))
	 (lambda () (rtlgen/emit!/1 `(RESTORE ,reg ,value)))))

      (define (box/unbox-preserve&restore reg value box-gen unbox-gen)
	(if (rtlgen/stack-ref? value)
	    (%preserve&restore
	     (lambda ()
	       (rtlgen/emit!/1
		`(ASSIGN ,value ,(rtlgen/->register (box-gen state)))))
	     (lambda ()
	       (rtlgen/emit!
		`((ASSIGN ,reg ,(unbox-gen value))
		  (ASSIGN ,value ,reg)))))
	    (%preserve&restore
	     (lambda ()
	       (rtlgen/stack-push!/1 (rtlgen/->register (box-gen state))))
	     (lambda ()
	       (rtlgen/emit!
	       `((ASSIGN ,reg ,(unbox-gen (rtlgen/stack-pop!)))
		 (ASSIGN ,value ,reg)))))))

      (if (null? infos)
	  (values (lambda ()
		    (for-each invoke-thunk (reverse prefix)))
		  (lambda ()
		    (for-each invoke-thunk suffix)))
	  (let* ((first (car infos))
		 (name  (vector-ref first 0))
		 (reg   (vector-ref first 1))
		 (value (vector-ref first 2))
		 (how   (vector-ref first 3)))
	    name			; unused
	    (case how
	      ((SAVE)
	       (preserve&restore reg 'SAVE reg))
	      ((IF-AVAILABLE RECOMPUTE)
	       (preserve&restore reg how value))
	      ((PUSH)
	       ;; These cases should really communicate with the LAP level
	       ;; rather than emitting voluminous code
	       (cond ((continuation-variable? name)
		      (box/unbox-preserve&restore reg value
						  rtlgen/boxed-continuation
						  rtlgen/unboxed-continuation))
		     ((closure-variable? name)
		      (box/unbox-preserve&restore reg value
						  rtlgen/boxed-closure
						  rtlgen/unboxed-closure))
		     (else
		      (internal-error "Cannot preserve by PUSHing"
				      (car infos)))))
	      (else
	       (internal-error "Unknown preservation kind" how)))))))

  (call-with-values
      (lambda ()
	(list-split (rtlgen/preservation-state state
					       *rtlgen/pseudo-register-values*)
		    (lambda (info)
		      (eq? (vector-ref info 3) 'PUSH))))
    (lambda (pushed-info other-info)
      (call-with-values
	  (lambda ()
	    (list-split other-info
			(lambda (info)
			  (eq? (vector-ref info 3) 'RECOMPUTE))))
	(lambda (recomputed maybe-preserved)
	  (preserve (append pushed-info
			    (reverse recomputed)
			    maybe-preserved)))))))

(define (rtlgen/preservation-state state orig-reg-defns)
  ;; Returns a list to 4-vectors:
  ;;  #(variable-name register home PUSH/SAVE/RECOMPUTE/IF-AVAILABLE)

  (define (check result)
    (if (not (= (length (remove-duplicates 
			 (map (lambda (4v) (second (vector-ref 4v 1))) result)))
		(length result)))
	(if compiler:guru?
	    (begin
	      (internal-warning "Duplicate preservation:")
	      (pp `((,(length (rtlgen/state/env state)) bindings)
		    (,(length orig-reg-defns) orig-reg-defns)
		    (,(length result) result))))))
    result)

  (define (preservations-from-state state)
    (let loop
	((bindings
	  (list-transform-positive (rtlgen/state/env state)
	    (lambda (binding)
	      (rtlgen/register? (rtlgen/binding/place binding)))))
	 (preservations '()))
      (if (null? bindings)
	  preservations
	  (let* ((binding  (car bindings))
		 (name     (rtlgen/binding/name binding))
		 (reg      (rtlgen/binding/place binding))
		 (regno    (second reg)))
	    (loop
	     (cdr bindings)
	     (if (assq regno preservations)
		 preservations
		 (cons
		  (cons regno
			(cond ((variable-cache-variable? name)
			       => (lambda (info)
				    (vector name reg (cadr info) 'RECOMPUTE)))
			      (else
			       (vector
				name
				reg
				(rtlgen/binding/home binding)
				(cond ((eq? binding
					    (rtlgen/state/continuation state))
				       'PUSH)
				      ((eq? binding
					    (rtlgen/state/closure state))
				       'PUSH)
				      (else 'SAVE))))))
		  preservations)))))))

  ;; The following loop is basically optional; it could be replaced by
  ;; (reverse (map cdr (preservations-from-state state)))
  ;;
  ;; You *MUST* generate PRESERVEs for all registers that are referenced in
  ;; the state, since they will be referenced by RTL code after the
  ;; return point.  All other registers are optionally saved: if they
  ;; can be saved safely (i.e. they are guaranteed to to be valid
  ;; Scheme objects), they are.  Later on, CSE will decide to reuse
  ;; some of these registers.  Thus, not saving a register inhibits
  ;; CSE but doesn't change the correctness of the algorithm.  Those
  ;; values which are unboxed must be preserved some other way, for
  ;; example by recomputing it from the objects from which it was
  ;; derived.


  (let loop
      ((reg-defns (reverse orig-reg-defns))
       (preservations (preservations-from-state state)))

    (if (null? reg-defns)
	(check (reverse! (map cdr preservations)))
	(let* ((defn  (car reg-defns))
	       (reg   (car defn))
	       (value (cadr defn))
	       (regno (cadr reg)))

	  (define (ignore)
	    (loop (cdr reg-defns) preservations))

	  (define (preserve)
	    (loop (cdr reg-defns)
		  (cons (cons regno (vector false reg false 'SAVE))
			preservations)))

	  (define (maybe-preserve)
	    (loop (cdr reg-defns)
		  (cons (cons regno (vector false reg value 'IF-AVAILABLE))
			preservations)))

	  (define (reg-preserved? reg)
	    (and (rtlgen/%pseudo-register? reg)
		 (assq (cadr reg) preservations)))

	  (define (compute)
	    (loop (cdr reg-defns)
		  (cons (cons (cadr reg)
			      (vector false reg value 'RECOMPUTE))
			preservations)))

	  (define (non-pointer-memory-operation)
	    (let ((index (caddr value)))
	      (cond ((not (reg-preserved? (cadr value)))
		     (ignore))
		    ((or (not (rtlgen/register? index))
			 (reg-preserved? index))
		     (compute))
		    (else
		     (ignore)))))

	  (if (assq regno preservations)
	      (ignore)
	      (case (car value)
		((REGISTER)		; Added by JSM
		 ;;(bkpt "; case = register")
		 (if compiler:guru?
		     (if (reg-preserved? value)
			 (internal-warning
			  "rtlgen/preservation-state register preserved"
			  reg value)
			 (internal-warning
			  "rtlgen/preservation-state register not preserved"
			  reg value)))
		 (ignore))
		((OFFSET)
		 ;; *** Kludge ***
		 (let ((old (reg-preserved? (cadr value))))
		   (if (or (not old)
			   (not (vector-ref (cdr old) 2))
			   (not (memq (car (vector-ref (cdr old) 2))
				      '(VARIABLE-CACHE ASSIGNMENT-CACHE))))
		       (preserve)
		       (compute))))
		((FLOAT->OBJECT CONS-POINTER)
		 ;; This assumes they are proper objects, and therefore
		 ;; can be preserved on their own
		 (preserve))
		((CONS-NON-POINTER)
		 ;; CONS-NON-POINTER is used to make non-objects like
		 ;; non-marked vector headers.
		 (compute))
		((CONS-CLOSURE)
		 (if (rtlgen/tagged-entry-points?)
		     (ignore)
		     (preserve)))
		((OFFSET-ADDRESS BYTE-OFFSET-ADDRESS FLOAT-OFFSET-ADDRESS)
		 (non-pointer-memory-operation))
		((OBJECT->TYPE)
		 ;; Assumption: the type looks like a fixnum
		 (preserve))
		((OBJECT->ADDRESS #|OBJECT->TYPE|# OBJECT->DATUM OBJECT->FLOAT)
		 (if (reg-preserved? (cadr value))
		     (compute)
		     (ignore)))
		((FLOAT-OFFSET)
		 ;; *** These should be preserved, since the preservation
		 ;; mechanism should handle floating objects.  For now... *** 
		 (non-pointer-memory-operation))
		((BYTE-OFFSET)
		 (non-pointer-memory-operation))
		((ENTRY:PROCEDURE ENTRY:CONTINUATION)
		 (compute))
		((VARIABLE-CACHE ASSIGNMENT-CACHE)
		 (compute))
		((CONSTANT)
		 (maybe-preserve))
		((FIXNUM-2-ARGS FIXNUM-1-ARG FLONUM-2-ARGS FLONUM-1-ARG)
		 ;;(internal-warning
		 ;; "rtlgen/preservation-state: arithmetic" value)
		 ;;Assumption: fixnum arithmetic is not used to generate
		 ;;illegal objects.
		 (preserve))
		((MACHINE-CONSTANT)
		 ;; In general, machine constants could look like objects that
		 ;; would crash the garbage collector.  We could change this to
                 ;; preserve constants which happen to be harmless when treated
		 ;; as objects (e.g. fixnums etc).
		 (compute))
		(else
		 (internal-warning
		  "rtlgen/preservation-state: unknown operation" value)
		 (ignore))))))))

;;;; RTL generation of statements

(define-macro (define-rtl-generator/stmt keyword bindings . body)
  (let ((proc-name (symbol-append 'RTLGEN/ keyword '/STMT)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler state) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name STATE FORM)
	     STATE FORM			; might be ignored
	     (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	       ,code)))))))

(define-rtl-generator/stmt LET (state bindings body)
  (define (default)
    (rtlgen/let* state bindings body rtlgen/stmt rtlgen/state/stmt/new-env))
  (cond ((or (not (eq? 'STATIC (binding-context-type 'LET 'STATIC bindings)))
	     (and (not (null? bindings))
		  (continuation-variable? (caar bindings))))
	 (default))
	((or (null? bindings)
	     (not (null? (cdr bindings)))
	     (not (form/match rtlgen/fetch-env-pattern
			      (cadr (car bindings)))))
	 (rtlgen/stmt state body))
	(else
	 (default))))

(define rtlgen/fetch-env-pattern
  `(CALL (QUOTE ,%fetch-environment) (QUOTE #F)))

(define (rtlgen/let* state bindings body rtlgen/body rtlgen/state/new-env)
  (let* ((env   (rtlgen/state/env state))
	 (rands (rtlgen/expr* state (map cadr bindings))))
    (rtlgen/body (rtlgen/state/new-env
		  state
		  (map* env
			(lambda (binding rand)
			  (rtlgen/binding/make (car binding) rand false))
			bindings
			rands))
		 body)))

(define-rtl-generator/stmt BEGIN (state #!rest actions)
  (if (null? actions)
      (internal-error "Empty BEGIN"))
  (let loop ((next (car actions))
	     (rest (cdr actions)))
    (if (null? rest)
	(rtlgen/stmt state next)
	(begin
	  (rtlgen/stmt/begin-action state next)
	  (loop (car rest) (cdr rest))))))

(define (rtlgen/stmt/begin-action state form)
  (define (illegal-action)
    (internal-error "Illegal BEGIN action" form))
  (cond ((not (pair? form))
	 (illegal-action))
	((DECLARE/? form)
	 (rtlgen/check-declarations (declare/declarations form))
	 false)
	(else
	 (rtlgen/expr (rtlgen/state/->expr state '(NONE)) form))))

(define-rtl-generator/stmt CALL (state rator cont #!rest rands)
  ;; This CALL must be in tail-recursive position of the combination
  (define (bad-rator)
    (internal-error "Illegal CALL statement operator" rator))
  (cond
   ((QUOTE/? rator)
    (rtlgen/call* state form (quote/text rator) cont rands))
   ((LOOKUP/? rator)
    (set! *rtlgen/form-calls-internal?* true)
    (rtlgen/jump state (lookup/name rator) cont rands))
   ((LAMBDA/? rator)
    (let ((call `(CALL ,rator ,cont ,@rands)))
      (cond ((not (null? rands)) (bad-rator))
	    ((form/match rtlgen/extended-call-pattern call)
	     ;; /compatible
	     ;; Compatibility only, extended stack frame
	     => (lambda (result)
		  (rtlgen/extended-call state form result call)))
	    ((form/match rtlgen/call-lambda-with-stack-closure-pattern call)
	     => (lambda (result)
		  (rtlgen/call-lambda-with-stack-closure
		   state result call rator cont rands)))
	    (else (bad-rator)))))
   (else (bad-rator))))

(define (rtlgen/extended-call state form match-result call)
  (let (#| (cont-name (cadr (assq rtlgen/?cont-name match-result))) |#
	(rator (cadr (assq rtlgen/?rator match-result)))
	(frame-vector* (cadr (assq rtlgen/?frame-vector* match-result)))
	(closure-elts* (cadr (assq rtlgen/?closure-elts* match-result)))
	(rands (cadr (assq rtlgen/?rands match-result)))
	(ret-add (cadr (assq rtlgen/?return-address match-result)))
	(frame-vector (cadr (assq rtlgen/?frame-vector match-result)))
	(closure-elts (cadr (assq rtlgen/?closure-elts match-result))))
    (if (not (LAMBDA/? ret-add))
	(internal-error "Bad extended call" call)
	(rtlgen/call* state
		      form
		      rator
		      `(CALL (QUOTE ,%make-stack-closure)
			     (QUOTE #F)
			     (QUOTE #F)
			     (QUOTE ,(list->vector
				      (append (vector->list frame-vector)
					      (vector->list frame-vector*))))
			     ,@closure-elts
			     (CALL (QUOTE ,%make-return-address)
				   (QUOTE #F)
				   ,ret-add)
			     ,@closure-elts*)
		      rands))))


(define (rtlgen/call-lambda-with-stack-closure state dict call rator cont rands)
  ;; This usually occurs when calling a primitive procedure as a
  ;; subproblem.
  ;;   (CALL (LAMBDA (CONT) ...)
  ;;         (call %make-stack-closure ...))
  ;; This is nasty because the LAMBDA has free variables which might be
  ;; stack references and the stack might contain a (raw) closure
  ;; pointer.
  ;;
  ;; We rely on the fact that the state bindings for stack resident names
  ;; are already loaded into pseudo-registers, as are the continuation
  ;; and closure pointers.  We also rely on the continuation CONT
  ;; being a make-stack-closure that saves the current valid
  ;; continuation.
  ;;
  ;; Most of the work is loading the continuation (register or stack
  ;; location) with the right value, and making a state for compiling
  ;; the body of the LAMBDA in-line.

  (define (bad-rator)
    (internal-error "Illegal CALL statement operator" rator))

  rands					; ignored

  (if compiler:guru?
      (internal-warning "call-lambda-with-stack-closure" call))

  ;; Sanity check: we can only rearrange the stack if all stack references
  ;; have already been loaded into pseudo-registers.  This may include
  ;; the continuation and closure pointer.
  (for-each
      (lambda (binding)
	(define (on-stack? syllable)
	  (form/match
	   `(OFFSET ,(rtlgen/reference-to-sp)
		    (MACHINE-CONSTANT ,(->pattern-variable 'offset)))
	   syllable))
	(if (and (on-stack? (rtlgen/binding/home binding))
		 (not (rtlgen/register?
		       (rtlgen/binding/place binding))))
	    (internal-error "Stack variable not in register" binding)))
    (rtlgen/state/stmt/env state))

  (let ((cont-var  (cadr (assq rtlgen/?cont-name dict)))
	(code-body (cadr (assq rtlgen/?body dict))))
    (let* ((old-closure-binding  (rtlgen/state/stmt/closure state))
	   (clos-reg             (and old-closure-binding (rtlgen/new-reg)))
	   (new-closure-binding
	    (and old-closure-binding
		 (rtlgen/binding/make
		  (rtlgen/binding/name old-closure-binding)
		  clos-reg
		  (rtlgen/binding/home old-closure-binding))))	   
	   ;;(old-continuation-binding (rtlgen/state/stmt/continuation state))
	   (cont-label
	    (rtlgen/continuation-is-stack-closure state cont bad-rator #F #T))
	   (cont-adj  (rtlgen/cont-adjustment))
	   (label-reg (rtlgen/new-reg))
	   (cont-reg  (if (zero? cont-adj) label-reg (rtlgen/new-reg)))
	   (new-continuation-home
	    (if (rtlgen/cont-in-stack?)
		(rtlgen/stack-ref
		 (if (and (rtlgen/closure-in-stack?) new-closure-binding) 1 0))
		(rtlgen/reference-to-cont)))
	   (new-continuation-binding
	    (rtlgen/binding/make  cont-var cont-reg new-continuation-home))
	   (new-size
	    (+ (if (and (rtlgen/cont-in-stack?) new-continuation-binding) 1 0)
	       (if (and (rtlgen/closure-in-stack?) new-closure-binding) 1 0))))

      (if (not cont-label)
	  (internal-error "call-lambda-with-stack-closure and no label" call))

      ;; JIM says "I don't see what guarantees
      ;; me that no one needs the current value
      ;; of the physical continuation register!"
      ;; SRA: It should be saved by the stack rewriting.

      ;; Allocate stack space for stack-based values:
      (rtlgen/bop-stack-pointer! (- new-size))

      (rtlgen/emit!/1
       `(ASSIGN ,label-reg (ENTRY:CONTINUATION ,cont-label)))
      (if (not (zero? cont-adj))
	  (rtlgen/emit!/1
	   `(ASSIGN ,cont-reg
		    (BYTE-OFFSET-ADDRESS ,label-reg
					 (MACHINE-CONSTANT ,(- 0 cont-adj))))))

      (if (rtlgen/cont-in-stack?)
	  (begin
	    ;; write the continuation into the stack
	    (rtlgen/emit!/1
	     `(ASSIGN ,(rtlgen/binding/home new-continuation-binding)
		      ,cont-reg)))
	  (begin
	    (rtlgen/emit!/1
	     `(ASSIGN ,(rtlgen/reference-to-cont) ,cont-reg))))

      (if old-closure-binding
	  (begin
	    (if (rtlgen/closure-in-stack?)
		(begin
		  ;; write closure pointer back into stack
		  (rtlgen/emit!/1
		   `(ASSIGN ,(rtlgen/binding/home old-closure-binding)
			    ,(rtlgen/binding/place old-closure-binding)))))
	    (rtlgen/emit!/1
	     `(ASSIGN ,clos-reg ,(rtlgen/binding/place old-closure-binding)))))

      ;;(bkpt "\n;;; rtlgen/call-lambda-with-stack-closure")

      (let ((new-state
	     (rtlgen/state/stmt/make
	      `(,new-continuation-binding
		,@(if new-closure-binding (list new-closure-binding) '())
		. ,(rtlgen/state/stmt/env state))
	      new-continuation-binding
	      new-closure-binding
	      new-size)))
	;;(bkpt 'hi)
	(rtlgen/stmt new-state code-body)))))


(define-rtl-generator/stmt LETREC (state bindings body)
  (rtlgen/letrec/bindings bindings)
  (rtlgen/stmt state body))

(define (rtlgen/letrec/bindings bindings)
  (sample/1 '(rtlgen/bindings-per-letrec histogram) (length bindings))
  (set! *rtlgen/delayed-objects*
	(map*
 	  *rtlgen/delayed-objects*
	  (lambda (binding)
	   (cons (car binding)
		 (rtlgen/descriptor/make 'TRIVIAL-CLOSURE #F (cadr binding))
		 ;;(rtlgen/descriptor/make 'PROCEDURE #F (cadr binding))
		 ))
	  bindings))
  unspecific)

(define-rtl-generator/stmt IF (state pred conseq alt)
  (rtlgen/if* state pred conseq alt rtlgen/stmt false))

(define (rtlgen/if* state pred conseq alt rtlgen/form need-merge?)
  (let ((true-label  (rtlgen/new-name 'TRUE))
	(false-label (rtlgen/new-name 'FALSE)))
    (call-with-values
	(lambda ()
	  (rtlgen/predicate state true-label false-label pred))
      (lambda (true-label-taken? false-label-taken?)
	(define (do-true)
	  (rtlgen/with-label true-label rtlgen/form state conseq))
	(define (do-false)
	  (rtlgen/with-label false-label rtlgen/form state alt))
	(cond ((not true-label-taken?)
	       (if (not false-label-taken?)
		   (internal-error "Predicate takes neither branch" pred))
	       (do-false))
	      ((not false-label-taken?)
	       (do-true))
	      (else
	       (rtlgen/emit-alternatives! do-true do-false need-merge?)))))))

(define (rtlgen/stmt state expr)
  ;; No meaningful value
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((LET)     (rtlgen/let/stmt state expr))
    ((CALL)    (rtlgen/call/stmt state expr))
    ((IF)      (rtlgen/if/stmt state expr))
    ((BEGIN)   (rtlgen/begin/stmt state expr))
    ((LETREC)  (rtlgen/letrec/stmt state expr))
    ((QUOTE LOOKUP LAMBDA DECLARE)
     (internal-error "Illegal statement" expr))
    (else
     (illegal expr))))

(define (rtlgen/with-label label generator state expr)
  (rtlgen/emit!/1 `(LABEL ,label))
  (generator state expr))

(define (rtlgen/predicate state true-label false-label pred)
  (let ((tl (list true-label 0))
	(fl (list false-label 0)))
    (let ((loc (rtlgen/expr (rtlgen/state/->expr state `(PREDICATE ,tl ,fl))
			    pred)))
      (if loc
	  (if compiler:guru?
	      (internal-warning "Predicate returned a value" pred loc)))
      (values (not (zero? (cadr tl)))
	      (not (zero? (cadr fl)))))))

(define (rtlgen/reference-true-label! target)
  (let ((true-label (cadr target)))
    (set-car! (cdr true-label) (+ (cadr true-label) 1))
    (car true-label)))

(define (rtlgen/reference-false-label! target)
  (let ((false-label (caddr target)))
    (set-car! (cdr false-label) (+ (cadr false-label) 1))
    (car false-label)))

(define (rtlgen/branch/true state)
  (let ((cont (rtlgen/state/expr/target state)))
    (rtlgen/emit!/1 `(JUMP ,(rtlgen/reference-true-label! cont))))
  false)

(define (rtlgen/branch/false state)
  (let ((cont (rtlgen/state/expr/target state)))
    (rtlgen/emit!/1 `(JUMP ,(rtlgen/reference-false-label! cont))))
  false)

(define (rtlgen/branch/likely state predicate)
  (let ((cont (rtlgen/state/expr/target state)))
    (rtlgen/emit!
     (list `(JUMPC ,predicate ,(rtlgen/reference-true-label! cont))
	   `(JUMP ,(rtlgen/reference-false-label! cont))))
    false))

(define (rtlgen/branch/unlikely state predicate)
  (let ((cont (rtlgen/state/expr/target state)))
    (rtlgen/emit!
     (list `(JUMPC (NOT ,predicate) ,(rtlgen/reference-false-label! cont))
	   `(JUMP  ,(rtlgen/reference-true-label! cont))))
    false))

(define (rtlgen/branch/unpredictable state predicate)
  (let ((cont (rtlgen/state/expr/target state)))
    (rtlgen/emit!
     (list `(JUMPC (UNPREDICTABLE ,predicate)
		   ,(rtlgen/reference-true-label! cont))
	   `(JUMP ,(rtlgen/reference-false-label! cont))))
    false))

(define (rtlgen/branch/false? state loc)
  (let* ((cont (rtlgen/state/expr/target state))
	 (default
	   (lambda ()
	     (let ((reg (rtlgen/->register loc)))
	       (rtlgen/emit!
		(list `(JUMPC (NOT (PRED-1-ARG FALSE? ,reg))
			      ,(rtlgen/reference-true-label! cont))
		      `(JUMP ,(rtlgen/reference-false-label! cont))))))))
    (if (not (rtlgen/constant? loc))
	(default)
	(case (boolean/discriminate (rtlgen/constant-value loc))
	  ((FALSE)
	   (rtlgen/emit!/1 `(JUMP ,(rtlgen/reference-false-label! cont))))
	  ((TRUE)
	   (rtlgen/emit!/1 `(JUMP ,(rtlgen/reference-true-label! cont))))
	  (else
	   (default)))))
  false)

(define (rtlgen/call* state form rator* cont rands)
  (define (bad-rator)
    (internal-error "Illegal CALL statement operator" rator*))

  (define (verify-rands len)
    (if (not (= len (length rands)))
	(internal-error "Wrong number of arguments" rator* rands)))

  (cond ((eq? rator* %invoke-continuation)
	 (set! *rtlgen/form-returns?* true)
	 (rtlgen/return state cont rands))
	((eq? rator* %internal-apply)
	 (set! *rtlgen/form-calls-external?* true)
	 (rtlgen/%apply state (second rands) cont
			(quote/text (first rands)) (cddr rands)))
	((eq? rator* %internal-apply-unchecked)
	 (set! *rtlgen/form-calls-external?* true)
	 (rtlgen/%apply-unchecked state (second rands) cont
				  (quote/text (first rands)) (cddr rands)))
	((eq? rator* %invoke-operator-cache)
	 (set! *rtlgen/form-calls-external?* true)
	 (rtlgen/invoke-operator-cache state
				       'INVOCATION:UUO-LINK
				       (first rands)    ; name+nargs
				       cont
				       (cddr rands))) ; exprs
	((eq? rator* %invoke-remote-cache)
	 (if (not (rtlgen/global-call-not-worth-interrupt-check? (first rands)))
	     (set! *rtlgen/form-calls-external?* true))
	 (rtlgen/invoke-operator-cache state
				       'INVOCATION:GLOBAL-LINK
				       (first rands)    ; name+nargs
				       cont
				       (cddr rands))) ; exprs
	((eq? rator* %primitive-apply/compatible)
	 (verify-rands 2)		; arity, primitive
	 (if (rtlgen/primitive-is-apply-like? (second rands))
	     (set! *rtlgen/form-calls-external?* true))
	 (rtlgen/invoke-primitive/compatible state
					     (first rands)  ; nargs
					     (second rands) ; prim
					     cont))
	((hash-table/get *open-coders* rator* false)
	 (set! *rtlgen/form-returns?* true)
	 (fluid-let ((*rtlgen/current-form* form))
	   (if (not (operator/satisfies? rator* '(SPECIAL-INTERFACE)))
	       (begin
		 (rtlgen/invoke-out-of-line state rator* cont rands))
	       (rtlgen/invoke-special state rator* cont rands))))
	(else
	 (bad-rator))))

(define (rtlgen/return state cont exprs)
  (define (illegal-continuation)
    (internal-error "Unexpected continuation for return" cont))
  (rtlgen/exprs->call-registers state #F exprs)
  (cond ((LOOKUP/? cont)
	 (let* ((adj    (rtlgen/cont-adjustment))
		(rcont  (rtlgen/state/reference-to-cont state))
		(result (if (zero? adj) rcont (rtlgen/new-reg))))
	   (rtlgen/bop-stack-pointer!  (rtlgen/state/stmt/size state))
	   (if (not (zero? adj))
	       (rtlgen/emit!/1
		`(ASSIGN ,result
			 (BYTE-OFFSET-ADDRESS ,rcont
					      (MACHINE-CONSTANT ,adj)))))
	   (rtlgen/emit!/1
	    `(INVOCATION:REGISTER 0
				  #F
				  ,result
				  #F
				  (MACHINE-CONSTANT 1)))))
	((CALL/%stack-closure-ref? cont)
	 (let ((size  (rtlgen/state/stmt/size state)))
	   (let* ((offset  (- size 1))
		  (obj     (rtlgen/new-reg))
		  (retad   (if (rtlgen/tagged-entry-points?)
			       (rtlgen/new-reg)
			       obj)))
	     (rtlgen/emit!
	      (list (rtlgen/read-stack-loc obj offset)
		    (rtlgen/bop-stack-pointer size)))
	     (if (rtlgen/tagged-entry-points?)
		 (rtlgen/emit!/1
		  `(ASSIGN ,retad (OBJECT->ADDRESS ,obj))))
	     (rtlgen/emit!/1
	      `(INVOCATION:REGISTER 0
				    #F
				    ,retad
				    #F
				    (MACHINE-CONSTANT 1))))))
	((CALL/%make-stack-closure? cont)
	 ;; This will not work for stack closures used just to push
	 ;; arguments, but it makes no sense to encounter that case
	 ;;(let ((handler  (rtlgen/continuation-is-stack-closure
	 ;;		    state cont illegal-continuation #F #F)))
	 ;;  (rtlgen/emit!
	 ;;   (rtlgen/%%continuation
	 ;;    'FAKE-LABEL handler
	 ;;    (lambda (label body saved-size arity)
	 ;;      saved-size arity		; Unused
	 ;;      (if (not (eq? label 'FAKE-LABEL))
	 ;;        (internal-error "New label generated for FAKE-LABEL"))
	 ;;      body))))
	 (let ((label	(rtlgen/continuation-is-stack-closure
	 		 state cont illegal-continuation #F #T)))
	   (if label
	       (begin
		 ;; Cant use jump because jump in an internal edge in rtl graph
		 ;;(rtlgen/emit!/1 `(JUMP ,label))

		 ;; The mention of the continuation is necessary otherwise the
		 ;; lap linearizer fails to see the continuation and discards
		 ;; it.
		 (rtlgen/emit!/1 `(ASSIGN ,(rtlgen/new-reg)
		 			  (ENTRY:CONTINUATION ,label)))
		 (rtlgen/emit!/1
		   `(INVOCATION:PROCEDURE 0 #F ,label (MACHINE-CONSTANT 1)))

		 ;; This also works but produces poor code:
		 ;;(let ((reg (rtlgen/new-reg)))
		 ;;  (rtlgen/emit!
		 ;;   `((ASSIGN ,reg (ENTRY:CONTINUATION ,label))
		 ;;     (INVOCATION:REGISTER 0 #F ,reg #F (MACHINE-CONSTANT 1)))))
		 )
	       ;; If it was not a label then ../continuation-is-stack-closure
	       ;; left the raw continuation in the standard place:
	       (let* ((adj    (rtlgen/cont-adjustment))
		      (rcont  (if (rtlgen/cont-in-stack?)
				  (rtlgen/new-reg)
				  (rtlgen/state/reference-to-cont state)))
		      (result (if (zero? adj) rcont (rtlgen/new-reg))))
		 (if (rtlgen/cont-in-stack?)
		     (rtlgen/stack-pop! rcont))
		 (if (not (zero? adj))
		     (rtlgen/emit!/1
		      `(ASSIGN ,result
			       (BYTE-OFFSET-ADDRESS ,rcont
						    (MACHINE-CONSTANT ,adj)))))
		 (rtlgen/emit!/1
		  `(INVOCATION:REGISTER 0
					#F
					,result
					#F
					(MACHINE-CONSTANT 1)))))))
	(else (illegal-continuation))))


(define (rtlgen/continuation-label->object label)
  (rtlgen/continuation->object `(ENTRY:CONTINUATION ,label)))

(define-integrable (rtlgen/continuation->object cont)
  (rtlgen/entry->object cont))

(define compiled-entry-tag
  (machine-tag 'COMPILED-ENTRY))

(define (rtlgen/entry->object cont)
  (if (not (rtlgen/tagged-entry-points?))
      cont
      (let ((rand (rtlgen/->register cont)))
	`(CONS-POINTER (MACHINE-CONSTANT ,compiled-entry-tag)
		       ,rand))))

(define (rtlgen/%apply state rator cont nargs rands)
  (let ((rator (rtlgen/->register
		(rtlgen/expr (rtlgen/state/->expr state '(ANY))
			     rator))))
    (rtlgen/invoke
     state cont rands
     (lambda (cont-label)
       (rtlgen/emit!/1
	`(INVOCATION:NEW-APPLY ,(+ nargs 1)
			       ,cont-label
			       ,rator
			       (MACHINE-CONSTANT 0)))))))

(define (rtlgen/%apply-unchecked state rator cont nargs rands)
  (let ((rator (rtlgen/->register
		(rtlgen/expr (rtlgen/state/->expr state '(ANY))
			     rator))))
    (rtlgen/invoke
     state cont rands
     (lambda (cont-label)
       (let ((dest-reg  (if (rtlgen/tagged-entry-points?)
			    (rtlgen/new-reg)
			    rator)))
	 (if (rtlgen/tagged-entry-points?)
	     (rtlgen/emit!/1
	      `(ASSIGN ,dest-reg (OBJECT->ADDRESS ,rator))))
	 (rtlgen/emit!/1
	  `(INVOCATION:REGISTER  ,(+ nargs 1)
				 ,cont-label
				 ,dest-reg
				 #F
				 (MACHINE-CONSTANT 0))))))))

(define (rtlgen/invoke-operator-cache state kind name+arity cont rands)
  (if (not (QUOTE/? name+arity))
      (internal-error "Unexpected execute cache descriptor" name+arity))
  (let ((name+arity* (cadr name+arity)))
    (let ((name   (car name+arity*))
	  (nargs (cadr name+arity*)))
      (if (not nargs)
	  (internal-error
	   "RTLGEN/INVOKE-OPERATOR-CACHE: Unknown number of arguments"
	   name+arity*))
      (rtlgen/invoke
       state cont rands
       (lambda (cont-label)
	 (rtlgen/emit!/1 `(,kind ,(+ nargs 1) ,cont-label ,name)))))))

(define (rtlgen/invoke-primitive/compatible state nargs prim cont)
  (rtlgen/invoke/compatible
   state cont
   (lambda (cont-label)
     (rtlgen/emit!/1
      `(INVOCATION:PRIMITIVE ,(+ (cadr nargs) 1) ,cont-label
			     ,(cadr prim))))))

(define (rtlgen/invoke-out-of-line state rator* cont rands)
  (rtlgen/exprs->call-registers state #F rands)
  (rtlgen/open-code/out-of-line
   (rtlgen/continuation-setup/jump! state cont)
   rator*))

(define (rtlgen/invoke-special state rator* cont rands)
  (let ((rands* (rtlgen/expr* state rands)))
    (rtlgen/with-local-continuation
     state cont
     (lambda (cont-label)
       (rtlgen/open-code/special cont-label rator* rands*)))))

(define (rtlgen/with-local-continuation state cont codegen)
  (rtlgen/stack-allocation/protect	; /compatible
   (lambda ()
     (let ((cont-label (rtlgen/continuation-setup/saved! state cont)))
       (if cont-label
	   (codegen cont-label)
	   (let ((label* (rtlgen/new-name 'AFTER-HOOK)))
	     (codegen label*)
	     (rtlgen/emit!
	      (list `(RETURN-ADDRESS
		      ,label*
		      #f
		      (MACHINE-CONSTANT ,(if (not *rtlgen/frame-size*)
					     0
					     (-1+ *rtlgen/frame-size*)))
		      (MACHINE-CONSTANT 1))
		    `(POP-RETURN)))))))))

(define (rtlgen/invoke/compatible state cont jump-gen)
  ;; rands will be on the stack by now
  (jump-gen (rtlgen/continuation-setup/compatible! state cont)))

(define (rtlgen/invoke state cont rands jump-gen)
  ;; SRA - should the continuation setup be done before call register setup
  ;; to reduce register pressure (as saved argument registers might
  ;; then be dead) ?? -- NO: the registers may be set up from the
  ;; stack-frame, so it must be setup after -- is this true??
  (rtlgen/exprs->call-registers state #F rands)
  ; JSM ... double check this
  (jump-gen (rtlgen/continuation-setup/jump! state cont)))

(define (rtlgen/continuation-setup/compatible! state cont)
  (define (bad-cont)
    (internal-error "Unexpected CALL continuation [compatible!]"
		    cont))
  (rtlgen/continuation-is-stack-closure state cont bad-cont #T #T))

(define (rtlgen/exprs->call-registers state *self* rands)
  ;; *self* is either #F or the expression which must be loaded into
  ;; the closure register before calling the destination procedure.
  (define (rtlgen/possibly-used-regs env form)
    (let loop ((vars (form/%free-vars form false))
	       (regs '()))
      (if (null? vars)
	  regs
	  (let* ((var   (car vars))
		 (place (rtlgen/binding/find var env)))
	    (cond ((not place)
		   (if (or (get-variable-property var 'VARIABLE-CELL)
			   (get-variable-property var 'FRAME-VARIABLE)
			   (assq var *rtlgen/delayed-objects*))
		       (loop (cdr vars) regs)
		       (free-var-error var)))
		  ((rtlgen/machine-register? (rtlgen/binding/home place))
		   (loop (cdr vars)
			 (eqv-set-adjoin (cadr (rtlgen/binding/home place))
					 regs)))
		  (else
		   (loop (cdr vars) regs)))))))	      
  (define (do-rand rand target)
    (let ((result (rtlgen/expr (rtlgen/state/->expr state target)
			       rand)))
      (if (not (equal? result target))
	  (internal-error "Argument value not in expected place"
			  result))))

  (let* ((env  (rtlgen/state/env state))
	 (arg-info
	  (do ((arg-number 0 (+ arg-number 1))
	       (rands rands (cdr rands))
	       (result
		(if *self*
		    `((,(rtlgen/reference-to-closure)
		       ,*self*
		       ,(rtlgen/possibly-used-regs env *self*)))
		    '())
		(let ((target (rtlgen/argument-home arg-number)))
		  (cons
		   (list target
			 (car rands)
			 (rtlgen/possibly-used-regs env (car rands)))
		   result))))
	      ((null? rands)
	       result))))

    (call-with-values
	(lambda ()
	  (list-split arg-info (lambda (arg) (rtlgen/register? (car arg)))))
      (lambda (->regs ->homes)
	(let ((->homes*
	       (map (lambda (arg)
		      (cons (rtlgen/new-reg) arg))
		    ->homes)))
	  (for-each (lambda (arg)
		      (do-rand (caddr arg) (car arg)))
		    ->homes*)
	  (let* ((pairs (map (lambda (info) (cons (cadr (car info)) info))
			     ->regs))
		 (sorted
		  (map (lambda (result)
			 (let ((pair (assv (car (vector-ref result 1))
					   pairs)))
			   (cond ((not pair)
				  (internal-error
				   "Parallel assignment found a register"
				   result))
				 ((vector-ref result 0) ; early?
				  (cons (rtlgen/new-reg) (cdr pair)))
				 (else
				  (cons (cadr pair) (cdr pair))))))
		       (parallel-assignment
			(map (lambda (arg)
			       (cons (cadr (car arg)) (caddr arg)))
			     ->regs)))))
	    (for-each (lambda (arg)
			(do-rand (caddr arg) (car arg)))
		      sorted)
	    (for-each (lambda (arg)
			(if (not (eq? (car arg) (cadr arg)))
			    (rtlgen/emit!/1 `(ASSIGN ,(cadr arg) ,(car arg)))))
		      sorted))
	  (for-each (lambda (arg)
		      (rtlgen/emit!/1 `(ASSIGN ,(cadr arg) ,(car arg))))
		    ->homes*))))))

(define (rtlgen/expr-results->call-registers state rands)
  state					; Not used
  (define (make-descr rand home) (cons rand home))
  ; (define (descr/rand descr) (car descr))
  (define (descr/home descr) (cdr descr))
    
  (let ((homes  (let process ((rands rands) (i 0))
		  (if (null? rands)
		      '()
		      (cons (make-descr (car rands) (rtlgen/argument-home i))
			    (process (cdr rands) (1+ i))))))
	(temps  (map (lambda (ignore) ignore (rtlgen/new-reg)) rands)))

    (for-each (lambda (rand temp)
		(rtlgen/emit!/1 `(ASSIGN ,temp ,rand)))
	      rands
	      temps)
    (for-each (lambda (temp descr)
		(rtlgen/emit!/1 `(ASSIGN ,(descr/home descr) ,temp)))
	      temps
	      homes)))

(define (rtlgen/jump state var-name cont rands)
  (let ((label      (rtlgen/enqueue-delayed-object! var-name 'TRIVIAL-CLOSURE))
	;;(label      (rtlgen/enqueue-delayed-object! var-name 'PROCEDURE))
	)
    (let* ((proc-info    (rtlgen/find-delayed-object var-name))
	   (lambda-expr  (rtlgen/descriptor/object proc-info))
	   (params       (and (LAMBDA/? lambda-expr)
			      (lambda/formals lambda-expr))))
      (if (not params)
	  (internal-error "rtlgen/jump: bad destination"
			  var-name lambda-expr))
      (let* ((needs-self? (and (pair? (cdr params))
			       (closure-variable? (cadr params))))
	     (true-rands (if needs-self? (cdr rands) rands)))
	;;(if needs-self?
	;;    (rtlgen/exprs->call-registers state (car rands) (cdr rands))
	;;    (rtlgen/exprs->call-registers state #F rands))
	(rtlgen/exprs->call-registers state #F rands)
	(let ((cont-label (rtlgen/continuation-setup/jump! state cont)))
	  (rtlgen/emit!/1
	   `(INVOCATION:PROCEDURE 0 ,cont-label
				  ,label
				  (MACHINE-CONSTANT ,(+ (length true-rands) 1)))))))))

(define (rtlgen/continuation-setup/jump! state cont)
  ;; returns continuation label or #F
  (define (bad-cont)
    (internal-error "Unexpected CALL continuation [jump!]" cont))
  (cond ((LOOKUP/? cont)
	 ;; Continuation already in the right place!
	 (rtlgen/pop state))
	((CALL/%stack-closure-ref? cont)
	 ;; This assumes it is the continuation variable!
	 (rtlgen/reload-continuation&pop state))
	((CALL/%make-stack-closure? cont)
	 (rtlgen/continuation-is-stack-closure
	  state cont bad-cont #F #T))
	(else
	 (bad-cont))))

(define (rtlgen/pop state)
  (if state
      (rtlgen/%pop state))
  false)

(define (rtlgen/%pop state)
  ;; Pop off the current stack frame, but be sure to leave the current
  ;; continuation (which may be near the top of the stack) in the
  ;; usual place.

  (let ((size (rtlgen/state/stmt/size state)))

    (cond ((not size)   false)

	  ((and (rtlgen/cont-in-stack?) (rtlgen/closure-in-stack?))
	   ;; ... xxx xxx cont closure -> ... cont
	   ;; size includes CONT and CLOSURE
	   (let ((cont    (rtlgen/state/continuation state))
		 (closure (rtlgen/state/closure state)))
	     (cond (;; ... cont closure -> ... cont
		    (and cont closure (= size 2)) 
		    (rtlgen/bop-stack-pointer! 1))
		   (;; ... xxx cont closure -> ... cont
		    (and cont closure)	
		    (let ((tempreg (rtlgen/new-reg)))
		      (rtlgen/emit!/1
		       `(ASSIGN ,tempreg
				,(rtlgen/state/reference-to-cont state)))
		      (rtlgen/bop-stack-pointer! (- size 1))
		      (rtlgen/emit!/1 (rtlgen/write-stack-loc tempreg 0))))
		   ((and cont (= size 1)) false) ; all fine
		   (;; ... xxx xxx cont -> cont
		    cont
		    (let ((tempreg (rtlgen/stack-pop!)))
		      (rtlgen/bop-stack-pointer! (- size 2))
		      (rtlgen/emit!/1 (rtlgen/write-stack-loc tempreg 0))))
		   (else
		    (rtlgen/bop-stack-pointer! size)))))
	  
	  ((or (rtlgen/cont-in-stack?) (rtlgen/closure-in-stack?))
	   (internal-error
	    "Not implemented for only one of CONT or CLOSURE in stack"))

	  (else
	   (rtlgen/bop-stack-pointer! size)))))


(define (rtlgen/reload-continuation&pop state)
  (rtlgen/%reload-continuation&pop (rtlgen/state/stmt/guaranteed-size state)))

(define (rtlgen/%reload-continuation&pop size)
  (let* ((adj       (rtlgen/cont-adjustment))
	 (in-stack? (rtlgen/cont-in-stack?))
	 (pop?      (and (= size 1)
			 (rtlgen/stack-post-increment?)
			 (not in-stack?)))
	 (offset    (cond (pop? 0)
			  (in-stack? (- size 1))
			  (else size)))
	 (contreg   (if in-stack?
			(rtlgen/new-reg)
			(rtlgen/reference-to-cont)))
	 (tempreg   (if (zero? adj)
			contreg
			(rtlgen/new-reg)))
	 (contobj   (if (rtlgen/tagged-entry-points?)
			(rtlgen/new-reg)
			tempreg)))
    (cond (pop?
	   (rtlgen/%stack-pop! contobj))
	  (else
	   (rtlgen/emit!/1 (rtlgen/read-stack-loc contobj (- size 1)))
	   (rtlgen/bop-stack-pointer! offset)))
    (if (rtlgen/tagged-entry-points?)
	(rtlgen/emit!/1 `(ASSIGN ,tempreg (OBJECT->ADDRESS ,contobj))))
    (if (not (zero? adj))
	(rtlgen/emit!/1
	 `(ASSIGN ,contreg
		  (BYTE-OFFSET-ADDRESS ,tempreg
				       (MACHINE-CONSTANT ,(- 0 adj))))))
    (if in-stack? (rtlgen/emit!/1 (rtlgen/write-stack-loc contreg 0))))
  false)

(define (rtlgen/boxed-continuation state)
  (let ((adj  (rtlgen/cont-adjustment))
	(raw  (rtlgen/->register (rtlgen/state/reference-to-cont state))))
    (rtlgen/continuation->object
     (if (zero? adj)
	 raw
	 (rtlgen/->register
	  `(BYTE-OFFSET-ADDRESS  ,raw  (MACHINE-CONSTANT ,adj)))))))

(define (rtlgen/unboxed-continuation reg)
  (let  ((adj      (rtlgen/cont-adjustment))
	 (untagged (if (rtlgen/tagged-entry-points?)
		       `(OBJECT->ADDRESS ,(rtlgen/->register reg))
		       reg)))
    (if (zero? adj)
	untagged
	`(BYTE-OFFSET-ADDRESS ,(rtlgen/->register untagged)
			      (MACHINE-CONSTANT ,(- adj))))))
    
(define (rtlgen/boxed-closure state)
  (let ((adj  (rtlgen/closure-adjustment))
	(raw  (rtlgen/->register (rtlgen/state/reference-to-closure state))))
    (rtlgen/entry->object
     (if (zero? adj)
	 raw
	 (rtlgen/->register
	  `(BYTE-OFFSET-ADDRESS  ,raw  (MACHINE-CONSTANT ,adj)))))))

(define (rtlgen/unboxed-closure reg)
  (let  ((adj      (rtlgen/closure-adjustment))
	 (untagged (if (rtlgen/tagged-entry-points?)
		       `(OBJECT->ADDRESS ,(rtlgen/->register reg))
		       reg)))
    (if (zero? adj)
	untagged
	`(BYTE-OFFSET-ADDRESS ,(rtlgen/->register untagged)
			      (MACHINE-CONSTANT ,(- adj))))))
   

(define (rtlgen/continuation-is-stack-closure
	 state cont bad-cont allow-sharp-f? enqueue?)
  ;; Returns the continuation's label or #F if not known, adjusts
  ;; the stack to match the model specified by the continuation, and
  ;; moves the continuation to the standard location (register or top
  ;; of stack)
  (define (core) (rtlgen/setup-stack-closure! state cont))
  (define (setup! label)
    (if (not label)
	;; Not a true subproblem, no need to stack
        ;; check if this is the only stuff on the stack.
	(rtlgen/stack-allocation/protect core)
	(core))
    label)
  (if (not (CALL/%make-stack-closure? cont))  (bad-cont))
  (let ((handler  (call/%make-stack-closure/lambda-expression cont)))
    (cond ((LAMBDA/? handler)
	   (setup!
	    (if enqueue?
		(rtlgen/enqueue-object! handler 'CONTINUATION)
		handler)))
	  ((LOOKUP/? handler)		;stack adjustment using unboxed cont.
	   (if (rtlgen/cont-in-stack?)
	       (let ((temp-reg  (rtlgen/state/reference-to-cont state)))
		 (setup! false)
		 (rtlgen/stack-push!/1 temp-reg)
		 false)
	       (setup! false)))
	  ((CALL/%stack-closure-ref? handler) ;
	   (if (rtlgen/state/continuation state)
	       (internal-error "Continuation has a raw continuation"
			       cont state))
	   (rtlgen/setup-stack-closure/saved-continuation
	    (rtlgen/state/stmt/guaranteed-size state)
	    handler
	    (lambda () (setup! false))))
	  ((and allow-sharp-f? (equal? ''#F handler))
	   (setup! false))
	  (else (bad-cont)))))


(define (rtlgen/setup-stack-closure/saved-continuation size ref rearrange!)
  ;; A continuation is returning/tailing using a saved & boxed continuation/
  ;; Assumption: the %stack-closure-ref REF is to the base of the stack frame.
  ;; This looks too much like RTLGEN/%RELOAD-CONTINUATION&POP for comfort
  ref					; Unused
  (let* ((adj       (rtlgen/cont-adjustment))
	 (in-stack? (rtlgen/cont-in-stack?))
	 (contreg   (if in-stack?
			(rtlgen/new-reg)
			(rtlgen/reference-to-cont)))
	 (tempreg   (if (zero? adj)
			contreg
			(rtlgen/new-reg)))
	 (contobj   (if (rtlgen/tagged-entry-points?)
			(rtlgen/new-reg)
			tempreg)))
    (rtlgen/emit!
     (list (rtlgen/read-stack-loc contobj (- size 1))))
    (if (rtlgen/tagged-entry-points?)
	(rtlgen/emit!/1 `(ASSIGN ,tempreg (OBJECT->ADDRESS ,contobj))))
    (if (not (zero? adj))
	(rtlgen/emit!/1
	 `(ASSIGN ,contreg
		  (BYTE-OFFSET-ADDRESS ,tempreg
				       (MACHINE-CONSTANT ,(- 0 adj))))))
    (rearrange!)
    (if in-stack? (rtlgen/stack-push!/1 contreg)))
  false)


(define (rtlgen/continuation-setup/saved! state cont)
  (define (bad-cont)
    (internal-error "Unexpected CALL continuation [saved!]" cont))
  (cond
   ((LOOKUP/? cont)
    (if (not state)
	(rtlgen/stack-push!/1 (rtlgen/boxed-continuation state))
	(let ((temp-reg (rtlgen/new-reg)))
	  (rtlgen/assign! temp-reg (rtlgen/boxed-continuation state))
	  (rtlgen/bop-stack-pointer! (rtlgen/state/stmt/size state))
	  (rtlgen/stack-push!/1 temp-reg)))
    false)
   ((CALL/%stack-closure-ref? cont)
    ;; This assumes that (a) it is the continuation variable and (b) it is at
    ;; the base of the frame.
    (let ((offset
	   (CALL/%stack-closure-ref/index cont)))
      (rtlgen/bop-stack-pointer! offset)
      false))
   ((CALL/%make-stack-closure? cont)
    (rtlgen/continuation-is-stack-closure state cont bad-cont #F #T))
   (else (bad-cont))))

(define (rtlgen/setup-stack-closure! state cont)
  (let* ((size  (rtlgen/state/stmt/size state))
	 (elts  (call/%make-stack-closure/values cont))
	 (size* (length elts)))

    (define (is-continuation-lookup? form)
      (and (LOOKUP/? form)
	   (continuation-variable? (lookup/name form))))

    (define (is-continuation-stack-ref? form)
      (and (CALL/%stack-closure-ref? form)
	   (continuation-variable?
	    (quote/text (call/%stack-closure-ref/name form)))))

    (define (returning-with-stack-arguments?)
      ;; The pushed values are all parameters, not saved values as this is a
      ;; reduction or return.
      (let ((lambda-slot (call/%make-stack-closure/lambda-expression cont)))
	(or (is-continuation-stack-ref? lambda-slot)
	    (is-continuation-lookup? lambda-slot))))

    (define (overwrite elts)
      (define (elt->reg elt)
	(rtlgen/->register
	 (rtlgen/expr (rtlgen/state/->expr state '(ANY))
		      elt)))
      (let ((elt-regs
	     (cond (*rtlgen/pre-load-stack-frame?*
		    (make-list (length elts) #F))
		   ((null? elts) '())
		   (else
		    (cons #F (map elt->reg (cdr elts)))))))
	(do ((frame-offset 0 (+ frame-offset 1))
	     (stack-offset (- size 1) (- stack-offset 1))
	     (elt-regs elt-regs (cdr elt-regs))
	     (elts elts (cdr elts)))
	    ((null? elts))
	    (cond ((and (CALL/%stack-closure-ref? (car elts))
			(CALL/%stack-closure-ref/index=? (car elts)
							 frame-offset)))
		  ((and (zero? frame-offset)
			(not (is-continuation-lookup? (car elts)))
			(not (returning-with-stack-arguments?)))
		   (internal-error "Unexpected previous continuation (1)" cont))
		  ((and (is-continuation-lookup? (car elts))
			(not (zero? frame-offset))
			(internal-error "Continuation saved at non-0 slot"
					cont)))
		  (else
		   (let* ((loc (or (car elt-regs)
				   (elt->reg (car elts)))))
		     (rtlgen/emit!/1
		      (rtlgen/write-stack-loc loc stack-offset))))))))

    (cond ((not (or (is-continuation-stack-ref? (first elts))
		    (is-continuation-lookup? (first elts))
		    (returning-with-stack-arguments?)))
	   (internal-error "Unexpected previous continuation (2)" cont))
	  ((and (> size* size) *rtlgen/pre-load-stack-frame?*)
	   (overwrite (list-head elts size))
	   (rtlgen/stack-push!
	    (rtlgen/expr* state (list-tail elts size))))
	  ((> size* size)
	   (let* ((values (rtlgen/expr* state (list-tail elts size)))
		  (regs   (map rtlgen/->register values)))
	     (overwrite (list-head elts size))
	     (rtlgen/stack-push! regs)))
	  (else
	   (overwrite elts)
	   (rtlgen/bop-stack-pointer! (- size size*))))))

;;;; RTL generation of expressions and pseudo-expressions

(define-macro (define-rtl-generator/expr keyword bindings . body)
  (let ((proc-name (symbol-append 'RTLGEN/ keyword '/EXPR)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler state) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name STATE FORM)
	     ;; FORM is in scope in BODY
	     (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	       ,code)))))))

(define-rtl-generator/expr LOOKUP (state name)
  (let ((place  (rtlgen/binding/find name (rtlgen/state/env state))))
    (cond ((not place)
	   (free-var-error name))
	  ((eq? place (rtlgen/state/continuation state))
	   (rtlgen/expr/simple-value state (rtlgen/boxed-continuation state)))
	  ((eq? place (rtlgen/state/closure state))
	   (rtlgen/expr/simple-value state (rtlgen/boxed-closure state)))
	  (else
	   (rtlgen/expr/simple-value state (rtlgen/binding/place place))))))

(define map-any-%unassigneds
  (let ((trap (make-unassigned-reference-trap)))
    (lambda (object)
      (cond ((pair? object)
	     (cons
	      (map-any-%unassigneds (car object))
	      (map-any-%unassigneds (cdr object))))
	    ((vector? object)
	     (vector-map object map-any-%unassigneds))
	    ((eq? object %unassigned)
	     (unmap-reference-trap trap))
	    (else object)))))

(define-rtl-generator/expr QUOTE (state object)
  (rtlgen/expr/simple-value
   state
   (if (eq? object %unassigned)
       (rtlgen/unassigned-object)
       `(CONSTANT ,(if (eq? object %unspecific)
		       unspecific
		       (map-any-%unassigneds object))))))

(define (rtlgen/expr/simple-value state loc)
  (let ((target  (rtlgen/state/expr/target state)))
    (case (car target)
      ((ANY)
       loc)
      ((REGISTER)
       (rtlgen/assign! target loc)
       target)
      ((PREDICATE)
       (rtlgen/branch/false? state loc))
      ((NONE)
       ;;(internal-error "Unexpected target kind for value" state)
       loc)
      (else
       (internal-error "Unknown target kind" state)))))

(define-rtl-generator/expr LET (state bindings body)
  (rtlgen/let* state bindings body rtlgen/expr rtlgen/state/expr/new-env))

(define-rtl-generator/expr IF (state pred conseq alt)
  (let ((state*
	 (if (eq? (car (rtlgen/state/expr/target state)) 'ANY)
	     (rtlgen/state/->expr state (rtlgen/new-reg))
	     state)))
    (rtlgen/if* state* pred conseq alt rtlgen/pseudo-stmt
		(not (eq? (car (rtlgen/state/expr/target state*))
			  'PREDICATE)))
    (let ((target (rtlgen/state/expr/target state*)))
      (and (eq? (car target) 'REGISTER)
	   target))))

(define (rtlgen/pseudo-stmt state expr)
  (let* ((target (rtlgen/state/expr/target state))
	 (result (rtlgen/expr state expr)))
    (case (car target)
      ((REGISTER)
       (if (not (equal? result target))
	   (internal-error "Non-register result when register demanded"
			   target result)))
      ((PREDICATE)
       (if result
	   (internal-error "Result for predicate found" target result)))
      ((NONE))
      (else
       (internal-error "Illegal expression predicate target" target)))))

(define-rtl-generator/expr CALL (state rator cont #!rest rands)
  (define (illegal message)
    (internal-error message form))
  (cond ((not (equal? cont '(QUOTE #F)))
	 (illegal "CALL expression with non-false continuation"))
	((not (and (QUOTE/? rator)
		   (pseudo-simple-operator? (quote/text rator))))
	 (illegal "CALL expression with non-simple operator"))
	(else
	 (let ((rator (quote/text rator)))
	   (cond ((eq? rator %make-trivial-closure)
		  (rtlgen/expr/make-trivial-closure state (car rands)))
		 ((eq? rator %make-heap-closure)
		  (rtlgen/expr/make-closure state rands))
		 ((eq? rator %stack-closure-ref)
		  (rtlgen/expr/stack-closure-ref state rands))
		 ((eq? rator %make-return-address)
		  (rtlgen/expr/make-return-address state (car rands)))
		 ((eq? rator %variable-read-cache)
		  (rtlgen/variable-cache state (cadr rands) 'VARIABLE-CACHE))
		 ((eq? rator %variable-write-cache)
		  (rtlgen/variable-cache state (cadr rands) 'ASSIGNMENT-CACHE))
		 ((eq? rator %make-stack-closure)
		  (illegal "expression call to %make-stack-closure"))
		 (else
		  (let* ((rands*  (rtlgen/expr* state rands))
			 (target  (rtlgen/state/expr/target state)))
		    (case (car target)
		      ((ANY REGISTER)
		       (rtlgen/open-code/value state rands* rator form))
		      ((PREDICATE)
		       (rtlgen/open-code/pred state rands* rator form))
		      ((NONE)
		       (rtlgen/open-code/stmt state rands* rator form))
		      (else
		       (internal-error "Unknown value destination"
				       target
				       form))))))))))


(define (rtlgen/variable-cache state name keyword)
  (if (not (QUOTE/? name))
      (internal-error "Unexpected variable cache name" name))
  (rtlgen/value-assignment state `(,keyword ,(quote/text name))))

(define (rtlgen/expr/make-return-address state rand)
  state					; ignored
  (rtlgen/quick&dirty/forbid-interrupt-check! rand)
  (rtlgen/continuation-label->object
   (rtlgen/enqueue-object! rand 'CONTINUATION)))  

(define (rtlgen/expr/make-trivial-closure state rand)
  (define (finish! entry-label)
    (let ((label-reg (rtlgen/new-reg)))
      (rtlgen/assign! label-reg `(ENTRY:PROCEDURE ,entry-label))
      (rtlgen/value-assignment state (rtlgen/entry->object label-reg))))
  (cond ((LOOKUP/? rand)
	 (finish!
	  (rtlgen/enqueue-delayed-object! (lookup/name rand) 'TRIVIAL-CLOSURE)))
	((LAMBDA/? rand)
	 (finish! (rtlgen/enqueue-object! rand 'TRIVIAL-CLOSURE)))
	(else
	 (internal-error "Unexpected argument to make-trivial-closure" rand))))

(define (rtlgen/enqueue-object! object kind)
  (let ((label* (rtlgen/new-name kind)))
    (rtlgen/enqueue! (rtlgen/descriptor/make kind label* object))
    label*))

(define (rtlgen/enqueue-delayed-object! name kind)
  (let ((place (assq name *rtlgen/delayed-objects*)))
    (if (not place)
	(internal-error "Unknown binding for operand" name kind))
    (let* ((desc   (cdr place))
	   (label  (rtlgen/descriptor/label desc)))
      (cond ((not label)
	     (let ((label* (car place)))
	       (set-rtlgen/descriptor/kind! desc kind)
	       (set-rtlgen/descriptor/label! desc label*)
	       (rtlgen/enqueue! desc)
	       label*))
	    ((not (eq? (rtlgen/descriptor/kind desc) kind))
	     (internal-error "Inconsistent usage"
			     (rtlgen/descriptor/object desc)
			     (rtlgen/descriptor/kind desc)
			     kind))
	    (else
	     label)))))

(define (rtlgen/find-delayed-object name)
  ;; Lookup by name, result is an rtlgen/descriptor
  (let ((result (assq name *rtlgen/delayed-objects*)))
    (if (not result)
	(internal-error "rtlgen/find-delayed-object: not found" name)
	(cdr result))))

(define (rtlgen/expr/make-closure state rands)
  (if (or (null? rands)
	  (null? (cdr rands))
	  (not (LAMBDA/? (first rands))))
      (internal-error "Unexpected argument to rtlgen/expr/make-closure"))
  ;; (second rands) is closure name vector, ignored
  (rtlgen/make-closure* state
			(lambda-list/arity-info
			 (cdr (lambda/formals (first rands))))
			(rtlgen/enqueue-object! (first rands) 'CLOSURE)
			(rtlgen/expr* state (cddr rands))))

(define (rtlgen/make-closure* state arity-info label elts)
  (let ((clos  (rtlgen/new-reg))
	(nelts (length elts)))
    (rtlgen/declare-allocation! (+ (rtlgen/closure-prefix-size) nelts))
    (rtlgen/assign! clos
		    `(CONS-CLOSURE (ENTRY:PROCEDURE ,label)
				   ,(car arity-info)
				   ,(cadr arity-info)
				   ,nelts))
    (do ((elts elts (cdr elts))
	 (offset (rtlgen/closure-first-offset) (+ offset 1)))
	((null? elts) 'DONE)
      (rtlgen/emit!/1
       `(ASSIGN (OFFSET ,clos (MACHINE-CONSTANT ,offset))
		,(rtlgen/->register (car elts)))))
    (rtlgen/value-assignment state (rtlgen/entry->object clos))))

(define (rtlgen/expr state expr)
  ;; returns result-location
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((LOOKUP) (rtlgen/lookup/expr state expr))
    ((QUOTE)  (rtlgen/quote/expr state expr))
    ((CALL)   (rtlgen/call/expr state expr))
    ((IF)     (rtlgen/if/expr state expr))
    ((LET)    (rtlgen/let/expr state expr))
    ((LAMBDA BEGIN LETREC DECLARE)
     (internal-error "Illegal expression" expr))
    (else     (illegal expr))))

(define (rtlgen/expr* state exprs)
  ;; returns list of result-locations
  (let ((state (rtlgen/state/->expr state '(ANY))))
    (let loop ((exprs   exprs)
	       (results '()))
      (if (null? exprs)
	  (reverse! results)
	  (loop (cdr exprs)
		(cons (rtlgen/expr state (car exprs))
		      results))))))

(define (rtlgen/remember new old)
  old					; ignored
  new)

(define (rtlgen/new-name prefix)
  (generate-uninterned-symbol prefix))

;;;;  States
;;
;;  States contain the contextual information needed to translate a piece
;;  of KMP code into RTL.  There are statement states (for reductions)
;;  and expression states (for open-coded subproblems).  States are
;;  initially set up at procedure (continuation etc) entry.
;;  RTLGEN/STATE/->EXPR is the only way to construct an expression
;;  state.  It builds on some existing (statement or expression)
;;  state.
;;
;;  The ENV is a map from names to machine places.  RTLGEN/STATE/ENV
;;  retrives the state and (RTLGEN/STATE/NEW-ENV/variant state env)
;;  returns a new state like the old but with a different ENV.
;;
;;  CLOSURE and CONTINUATION are set to the bindings for the heap closure
;;  and continuation parameters, or #F if that parameter is absent
;;  (e.g. continuations are not themselves passed a continuation).
;;  These bindings are also in ENV.  The binding is to an RTL register
;;  containing to the RAW object, which may have been loaded from
;;  either the stack or standard registers.
;;
;;  Statements are compiled in the context of a stack frame.  SIZE is the
;;  number of elements on the stack, INCLUDING the continuation and
;;  closure pointer if these are on the stack.
;;
;;  Expressions are compiled in the context of a target.  A target may be
;;  any of the following:
;;   (ANY)              any location will do
;;   (NONE)             the value is not required
;;   (REGISTER number)  target this register
;;   (PREDICATE (true-label count) (false-label count))
;;     `target' is a predicate.  The count slots are initially 0 and are
;;     updated to count the number of branches to the true and false
;;     labels.

(define-structure (rtlgen/state/stmt
		   (conc-name rtlgen/state/stmt/)
		   (constructor rtlgen/state/stmt/make))
  (env '() read-only true)
  (continuation #F read-only true)
  (closure      #F read-only true)
  (size false read-only true))

(define-structure (rtlgen/state/expr
		   (conc-name rtlgen/state/expr/)
		   (constructor %rtlgen/state/expr/make))
  (env '() read-only true)
  (continuation #F read-only true)
  (closure      #F read-only true)
  (target false read-only true))

;; RTLGEN/STATE/{ENV,CONTINUATION,CLOSURE} all depend on the fact that
;; both states have the same layout and that there is no error
;; checking by default.  Otherwise they could be written to dispatch.

(define-integrable (rtlgen/state/env state)
  (rtlgen/state/stmt/env state))

(define-integrable (rtlgen/state/continuation state)
  (rtlgen/state/stmt/continuation state))

(define-integrable (rtlgen/state/closure state)
  (rtlgen/state/stmt/closure state))

(define (rtlgen/state/reference-to-cont state)
  (if (rtlgen/state/continuation state)
      (rtlgen/binding/place (rtlgen/state/continuation state))
      (internal-error "No continuation in this state " state)))

(define (rtlgen/state/reference-to-closure state)
  (if (rtlgen/state/closure state)
      (rtlgen/binding/place (rtlgen/state/closure state))
      (internal-error "No continuation in this state " state)))

(define-integrable (rtlgen/state/->expr state target)
  (%rtlgen/state/expr/make (rtlgen/state/env state)
			   (rtlgen/state/continuation state)
			   (rtlgen/state/closure state)
			   target))

(define (rtlgen/state/stmt/new-env state env)
  (rtlgen/state/stmt/make env
			  (rtlgen/state/stmt/continuation state)
			  (rtlgen/state/stmt/closure state)
			  (rtlgen/state/stmt/size state)))

(define (rtlgen/state/expr/new-env state env)
  (%rtlgen/state/expr/make env
			   (rtlgen/state/expr/continuation state)
			   (rtlgen/state/expr/closure state)
			   (rtlgen/state/expr/target state)))

(define (rtlgen/state/stmt/guaranteed-size state)
  (or (and (rtlgen/state/stmt? state) (rtlgen/state/stmt/size state))
      (internal-error "Cannot find stack frame size" state)))
    
;; In the state structures, ENV is a list of bindings:

(define-structure (rtlgen/binding
		   (conc-name rtlgen/binding/)
		   (constructor rtlgen/binding/make)
		   (print-procedure
		    (standard-unparser-method 'RTLGEN/BINDING
		      (lambda (binding port)
			(write-char #\space port)
			(write (rtlgen/binding/name binding) port)))))
  (name  #F read-only true)
  (place #F read-only true)		; Where it is currently
  (home  #F read-only true))

(define (rtlgen/binding/find name env)
  (let loop ((env env))
    (cond ((null? env) #F)
	  ((eq? name (rtlgen/binding/name (car env)))
	   (car env))
	  (else (loop (cdr env))))))

;;;; Open coding

(define *open-coders*
  (make-eq-hash-table))

(define-integrable (rtlgen/get-open-coder rator)
  (let ((open-coder  (hash-table/get *open-coders* rator false)))
    (if (not open-coder)
	(internal-error "No open coder known" rator)
	open-coder)))

(define-integrable (rtlgen/get-open-coder/checked rator rands)
  (let ((open-coder (rtlgen/get-open-coder rator)))
    (if (and (rtlgen/open-coder/nargs open-coder)
	     (not (= (length rands) (rtlgen/open-coder/nargs open-coder))))
	(user-error "Wrong number of arguments" rator)
	open-coder)))


;; KLUDGE.  Used for passing the form to selected open-coders
(define *rtlgen/current-form* #F)

(define-integrable (rtlgen/open-code/common state rands rator form select-kind)
  (let ((open-coder  (rtlgen/get-open-coder/checked rator rands)))
    (if (rtlgen/open-coder/requires-form? open-coder)
	(fluid-let ((*rtlgen/current-form* form))
	  ((select-kind open-coder) state rands open-coder))
	((select-kind open-coder) state rands open-coder))))

(define (rtlgen/open-code/pred state rands rator form)
  ;; No meaningful value
  (rtlgen/open-code/common state rands rator form rtlgen/open-coder/pred))

(define (rtlgen/open-code/stmt state rands rator form)
  ;; No meaningful value
  (rtlgen/open-code/common state rands rator form rtlgen/open-coder/stmt))

(define (rtlgen/open-code/value state rands rator form)
  ;; Returns location of result
  (rtlgen/open-code/common state rands rator form rtlgen/open-coder/value))


(define (rtlgen/open-code/out-of-line cont-label rator)
  ;; No meaningful value
  (let ((open-coder  (hash-table/get *open-coders* rator false)))
    (cond ((not open-coder)
	   (internal-error "No open coder known" rator))
	  (else
	   ((rtlgen/open-coder/outl open-coder) cont-label open-coder)))))

(define (rtlgen/open-code/special cont-label rator rands)
  ;; No meaningful value
  (let ((open-coder  (rtlgen/get-open-coder/checked rator rands)))
    ((rtlgen/open-coder/special open-coder) cont-label rands open-coder)))


(define-structure (rtlgen/open-coder
		   (conc-name rtlgen/open-coder/)
		   (constructor rtlgen/open-coder/make))
  (rator false read-only true)
  (nargs false read-only true)
  (value false read-only true)
  (stmt false read-only true)
  (pred false read-only true)
  (outl false read-only true)
  (special false read-only true)
  ;; some opend coders need to inspect the CALL form:
  (requires-form? false read-only true))

(define (define-open-coder name-or-object nargs
	  vhandler shandler phandler ohandler sphandler
	  #!optional requires-form?)
  (let ((rator
	 (if (known-operator? name-or-object)
	     name-or-object
	     (let ((prim (make-primitive-procedure name-or-object nargs)))
	       (if (not (known-operator? prim))
		   ;; Applicat makes these into %primitive-apply
		   (warn "Unknown operator: open coder will never be called"
			 name-or-object))
	       prim))))
    (hash-table/put!
     *open-coders*
     rator
     (rtlgen/open-coder/make rator nargs
			     vhandler shandler phandler
			     ohandler sphandler
			     (if (default-object? requires-form?)
				 #F
				 requires-form?)))))

(define (rtlgen/no-predicate-open-coder state rands open-coder)
  state rands				; ignored
  (internal-error "Statement operation used as predicate"
		  (rtlgen/open-coder/rator open-coder))
  #F)

(define (rtlgen/no-stmt-open-coder state rands open-coder)
  state rands				; ignored
  (internal-error "Predicate/value operation used as statement"
		  (rtlgen/open-coder/rator open-coder)))

(define (rtlgen/no-value-open-coder state rands open-coder)
  state rands				; ignored
  (internal-error "Statement operation used as value"
		  (rtlgen/open-coder/rator open-coder)))

(define (rtlgen/no-out-of-line-open-coder cont-label open-coder)
  cont-label			; ignored
  (internal-error "Attempt to call open-coded operation"
		  (rtlgen/open-coder/rator open-coder)))

(define (rtlgen/no-special-open-coder cont-label rator rands open-coder)
  cont-label rator rands		; ignored
  (internal-error "Attempt to call open-coded operation"
		  (rtlgen/open-coder/rator open-coder)))

(define (define-open-coder/pred name-or-object nargs handler)
  (define-open-coder name-or-object nargs
    (rtlgen/pred->value handler)
    rtlgen/no-stmt-open-coder
    handler
    rtlgen/no-out-of-line-open-coder
    rtlgen/no-special-open-coder))

(define (define-open-coder/stmt name-or-object nargs handler)
  (define-open-coder name-or-object nargs
    rtlgen/no-value-open-coder
    handler
    rtlgen/no-predicate-open-coder
    rtlgen/no-out-of-line-open-coder
    rtlgen/no-special-open-coder))

(define (define-open-coder/value name-or-object nargs handler)
  (define-open-coder name-or-object nargs
    handler
    rtlgen/no-stmt-open-coder
    (rtlgen/value->pred handler)
    rtlgen/no-out-of-line-open-coder
    rtlgen/no-special-open-coder))

(define (define-open-coder/out-of-line name-or-object nargs handler)
  (define-open-coder name-or-object nargs
    (rtlgen/out-of-line->value handler)
    (rtlgen/out-of-line->stmt handler)
    (rtlgen/out-of-line->pred handler)
    handler
    rtlgen/no-special-open-coder
    'REQUIRES-FORM))

(define (define-open-coder/special name-or-object nargs handler)
  (define-open-coder name-or-object nargs
    (rtlgen/special->value handler)
    (rtlgen/special->stmt handler)
    (rtlgen/special->pred handler)
    rtlgen/no-out-of-line-open-coder
    handler
    'REQUIRES-FORM))

(define (rtlgen/pred->value handler)
  (lambda (state rands open-coder)
    (let* ((target (rtlgen/state/expr/target state))
	   (target* (case (car target)
		      ((ANY)
		       (rtlgen/new-reg))
		      ((REGISTER)
		       target)
		      (else
		       (internal-error "Unexpected value target" target
				       (rtlgen/open-coder/rator
					open-coder))))))
      (let* ((true-label  (rtlgen/new-name 'TRUE))
	     (false-label (rtlgen/new-name 'FALSE))
	     (tl          (list true-label 0))
	     (fl          (list false-label 0)))
	(handler (rtlgen/state/->expr  state  `(PREDICATE ,tl ,fl))
		 rands open-coder)
	(let ((true-label-taken?   (not (zero? (cadr tl))))
	      (false-label-taken?  (not (zero? (cadr fl)))))
	  (cond ((and true-label-taken? false-label-taken?)
		 (let ((merge-label  (rtlgen/new-name 'MERGE)))
		   (rtlgen/assign!*
		    `((LABEL ,true-label)
		      (ASSIGN ,target* (CONSTANT ,#t))
		      (JUMP ,merge-label)
		      (LABEL ,false-label)
		      (ASSIGN ,target* (CONSTANT ,#f))
		      (LABEL ,merge-label)))))
		(true-label-taken?
		 (rtlgen/assign!* `((LABEL ,true-label)
				    (ASSIGN ,target* (CONSTANT ,#T)))))
		(false-label-taken?
		 (rtlgen/assign!* `((LABEL ,false-label)
				    (ASSIGN ,target* (CONSTANT ,#F)))))
		(else
		 (internal-error "Neither branch taken"
				 (rtlgen/open-coder/rator open-coder)
				 rands)))))
      target*)))

(define (rtlgen/value->pred handler)
  (lambda (state rands open-coder)
    (rtlgen/branch/false? state
			  (handler (rtlgen/state/->expr state '(ANY))
				   rands open-coder))))

(define (rtlgen/with-preservation state code-gen-1 code-gen-2)
  (rtlgen/stack-allocation/protect	; /compatible ?
   (lambda ()
     (call-with-values
	 (lambda () (rtlgen/preserve-state state))
       (lambda (gen-prefix gen-suffix)
	 (let* ((cont-label (rtlgen/new-name 'CONT))
		(frame-size (if (not *rtlgen/frame-size*)
				0
				(- *rtlgen/frame-size* 1)))
		(dbg-info
		 (rtlgen/dbg-expression->continuation 
		  (code-rewrite/original-form/previous *rtlgen/current-form*)
		  cont-label
		  frame-size)))
	   (gen-prefix)
	   (code-gen-1 cont-label)
	   (rtlgen/emit!/1
	    `(RETURN-ADDRESS ,cont-label
			     ,dbg-info
			     (MACHINE-CONSTANT ,frame-size)
			     (MACHINE-CONSTANT 1)))
	   (let ((result (code-gen-2 state)))
	     (gen-suffix)
	     result)))))))

(define (rtlgen/dbg-expression->continuation info label frame-size)
  frame-size				; ignored
  (and (new-dbg-expression? info)
       (let ((outer (new-dbg-expression/outer info))
	     (inner (new-dbg-expression/source-code info)))
	 (and outer
	      inner
	      (let ((cont
		     (new-dbg-continuation/make
		      (cond ((scode/sequence? outer) 'SEQUENCE-ELEMENT)
			    ((and (scode/conditional? outer)
				  (eq? (scode/conditional-predicate outer)
				       inner))
			     'CONDITIONAL-PREDICATE)
			    (else 'COMBINATION-ELEMENT))
		      outer
		      inner)))
		(set-new-dbg-continuation/label! cont label)
		cont)))))

(define (rtlgen/out-of-line->pred handler)
  (rtlgen/value->pred (rtlgen/out-of-line->value handler)))

#|
(define (rtlgen/out-of-line->stmt handler)
  ;; /compatible
  (lambda (state rands open-coder)
    (rtlgen/with-preservation
     state
     (lambda (cont-label)
       (rtlgen/stack-push!
	(cons (rtlgen/continuation-label->object cont-label)
	      (reverse rands)))
       (handler cont-label open-coder))
     (lambda (state)
       state				; ignored
       unspecific))))

(define (rtlgen/out-of-line->value handler)
  ;; /compatible
  (lambda (state rands open-coder)
    (rtlgen/with-preservation
     state
     (lambda (cont-label)
       (rtlgen/stack-push!
	(cons (rtlgen/continuation-label->object cont-label)
	      (reverse rands)))
       (handler cont-label open-coder))
     (lambda (state)
       (rtlgen/value-assignment state (rtlgen/reference-to-val))))))	   
|#

(define (rtlgen/out-of-line->stmt handler)
  (lambda (state rands open-coder)
    (rtlgen/with-preservation
     state
     (lambda (cont-label)
       (rtlgen/expr-results->call-registers state rands)
       (handler cont-label open-coder))
     (lambda (state)
       state				; ignored
       unspecific))))

(define (rtlgen/out-of-line->value handler)
  (lambda (state rands open-coder)
    (rtlgen/with-preservation
     state
     (lambda (cont-label)
       (rtlgen/expr-results->call-registers state rands)
       (handler cont-label open-coder))
     (lambda (state)
       (rtlgen/value-assignment state (rtlgen/reference-to-val))))))	   

(define (rtlgen/special->pred handler)
  (rtlgen/value->pred (rtlgen/special->value handler)))
  
(define (rtlgen/special->stmt handler)
  (lambda (state rands open-coder)
    (rtlgen/with-preservation
     state
     (lambda (cont-label)
       (handler cont-label rands open-coder))
     (lambda (state)
       state				; ignored
       unspecific))))

(define (rtlgen/special->value handler)
  (lambda (state rands open-coder)
    (rtlgen/with-preservation
     state
     (lambda (cont-label)
       (handler cont-label rands open-coder))
     (lambda (state)
       (rtlgen/value-assignment state (rtlgen/reference-to-val))))))

;;;; Open-coded predicates

;;; These open codings do not do anything about type and range checking.
;;; Such things are assumed to have been done by an earlier stage.

(let* ((simple-value-tester
	(lambda (rtlgen/branch/<preference>)
	  (lambda (name rtl-pred compile-time-pred?)
	    (define-open-coder/pred name 1
	      (lambda (state rands open-coder)
		open-coder		; ignored
		(let ((rand (car rands)))
		  (cond ((or (not (rtlgen/constant? rand))
			     (not *rtlgen/fold-simple-value-tests?*))
			 (let* ((rand* (rtlgen/->register rand)))
			   (rtlgen/branch/<preference>
			    state  `(PRED-1-ARG ,rtl-pred ,rand*))))
			((compile-time-pred? (rtlgen/constant-value rand))
			 (rtlgen/branch/true state))
			(else
			 (rtlgen/branch/false state)))))))))
       (define-simple-value-test
	 (simple-value-tester rtlgen/branch/likely))
       (define-simple-value-test/inverted
	 (simple-value-tester rtlgen/branch/unlikely)))

  (define-simple-value-test/inverted 'NULL?  'NULL?   null?)
  (define-simple-value-test/inverted 'NOT    'FALSE?  not)
  (define-simple-value-test/inverted 'FALSE? 'FALSE?  false?)
  (define-simple-value-test 'FIXNUM?         'FIXNUM?       fixnum?)
  (define-simple-value-test 'INDEX-FIXNUM?   'INDEX-FIXNUM? index-fixnum?))

(let ((define-simple-tag-test
	(lambda (name tag)
	  (define-open-coder/pred name 1
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let ((rand (car rands)))
		(cond ((or (not (rtlgen/constant? rand))
			   (not *rtlgen/fold-tag-predicates?*))
		       (let* ((rand*  (rtlgen/->register rand))
			      (rand** (rtlgen/new-reg)))
			 (rtlgen/assign! rand** `(OBJECT->TYPE ,rand*))
			 (rtlgen/branch/likely state
					       `(TYPE-TEST ,rand** ,tag))))
		      ((object-type? tag (rtlgen/constant-value rand))
		       (rtlgen/branch/true state))
		      (else
		       (rtlgen/branch/false state)))))))))
  (define-simple-tag-test 'CELL?       (machine-tag 'CELL))
  (define-simple-tag-test 'CHAR?       (machine-tag 'CHARACTER))
  (define-simple-tag-test 'PAIR?       (machine-tag 'PAIR))
  (define-simple-tag-test 'VECTOR?     (machine-tag 'VECTOR))
  (define-simple-tag-test '%RECORD?    (machine-tag 'RECORD))
  (define-simple-tag-test 'STRING?     (machine-tag 'STRING))
  (define-simple-tag-test 'BIT-STRING? (machine-tag 'VECTOR-1B))
  (define-simple-tag-test 'FLONUM?     (machine-tag 'FLONUM))
  (define-simple-tag-test %compiled-entry? (machine-tag 'COMPILED-ENTRY)))

(define-open-coder/pred 'EQ? 2
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let ((rand1 (car rands))
	  (rand2 (cadr rands)))
      (cond ((or (not (rtlgen/constant? rand1))
		 (not (rtlgen/constant? rand2))
		 (not *rtlgen/fold-tag-predicates?*))
	     (let* ((rand1* (rtlgen/->register rand1))
		    (rand2* (rtlgen/->register rand2)))
	       (rtlgen/branch/unlikely state `(EQ-TEST ,rand1* ,rand2*))))
	    ((eq? (rtlgen/constant-value rand1) (rtlgen/constant-value rand2))
	     (rtlgen/branch/true state))
	    (else
	     (rtlgen/branch/false state))))))

(define-open-coder/pred %unassigned? 1
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let* ((rand1  (rtlgen/->register (car rands)))
	   (rand2  (rtlgen/->register (rtlgen/unassigned-object))))
      (rtlgen/branch/unlikely state `(EQ-TEST ,rand1 ,rand2)))))

(define-open-coder/pred %reference-trap? 1
  (let ((tag (machine-tag 'REFERENCE-TRAP)))
    (lambda (state rands open-coder)
      open-coder			; ignored
      (let* ((rand  (rtlgen/->register (car rands)))
	     (temp  (rtlgen/new-reg)))
	(rtlgen/assign! temp `(OBJECT->TYPE ,rand))
	(rtlgen/branch/unlikely state `(TYPE-TEST ,temp ,tag))))))

(define-open-coder/pred 'OBJECT-TYPE? 2
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let* ((tag  (car rands))
	   (obj  (rtlgen/->register (second rands)))
	   (obj* (rtlgen/new-reg)))
      (rtlgen/assign! obj* `(OBJECT->TYPE ,obj))
      (cond ((rtlgen/constant? tag)
	     (rtlgen/branch/likely
	      state
	      `(TYPE-TEST ,obj* ,(rtlgen/constant-value tag))))
	    (else
	     (let* ((tag*  (rtlgen/->register tag))
		    (tag** (rtlgen/new-reg)))
	       (rtlgen/assign! tag** `(OBJECT->DATUM ,tag*))
	       (rtlgen/branch/likely state `(EQ-TEST ,obj* ,tag**))))))))


(define-open-coder/pred %compiled-entry-maximum-arity? 2
  (lambda (state rands open-coder)
    open-coder
    (let* ((arity  (rtlgen/->register (first rands)))
	   (obj    (rtlgen/->register (second rands)))
	   (obj*   (if (rtlgen/tagged-entry-points?) (rtlgen/new-reg) obj))
	   (arity* (rtlgen/new-reg)))
      (if (rtlgen/tagged-entry-points?)
	  (rtlgen/assign! obj* `(OBJECT->ADDRESS ,obj)))
      (rtlgen/assign! arity* `(BYTE-OFFSET ,obj* (MACHINE-CONSTANT -3)))
      (rtlgen/branch/likely state `(EQ-TEST ,arity* ,arity)))))

(define-integrable (rtlgen/constant? syllable)
  (and (pair? syllable)
       (eq? (car syllable) 'CONSTANT)))

(define-integrable (rtlgen/constant-value syllable)
  (cadr syllable))

(define-integrable (rtlgen/integer-constant? syllable)
  (and (rtlgen/constant? syllable)
       (exact-integer? (rtlgen/constant-value syllable))
       (rtlgen/constant-value syllable)))

(define-integrable (rtlgen/vector-constant? syllable)
  (and (rtlgen/constant? syllable)
       (vector? (rtlgen/constant-value syllable))
       (rtlgen/constant-value syllable)))

(define-open-coder/pred %small-fixnum? 2
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let* ((value  (rtlgen/->register (car rands)))
	   (nbits  (cadr rands)))
      (if (not (rtlgen/constant? nbits))
	  (internal-error "small-fixnum? needs constant nbits" nbits))
      (rtlgen/branch/likely
       state
       `(PRED-2-ARGS SMALL-FIXNUM?
		     ,value
		     (MACHINE-CONSTANT ,(rtlgen/constant-value nbits)))))))

(let ((define-fixnum-predicate
	(lambda (proc name rtlgen/branch)
	  (define-open-coder/pred proc 2
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let* ((rand1 (rtlgen/->register (car rands)))
		     (rand2 (rtlgen/->register (cadr rands))))
		(rtlgen/branch
		 state
		 `(FIXNUM-PRED-2-ARGS ,name ,rand1 ,rand2))))))))
  (define-fixnum-predicate fix:= 'EQUAL-FIXNUM?
    rtlgen/branch/unlikely)
  (define-fixnum-predicate fix:< 'LESS-THAN-FIXNUM?
    rtlgen/branch/unpredictable)
  (define-fixnum-predicate fix:> 'GREATER-THAN-FIXNUM?
    rtlgen/branch/unpredictable))

(define-open-coder/pred %word-less-than-unsigned? 2
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let* ((rand1 (rtlgen/->register (first rands)))
	   (rand2 (rtlgen/->register (second rands))))
      (rtlgen/branch/likely
       state
       `(PRED-2-ARGS WORD-LESS-THAN-UNSIGNED? ,rand1 ,rand2)))))

(let ((define-flonum-predicate
	(lambda (proc name rtlgen/branch)
	  (define-open-coder/pred proc 2
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let* ((rand1 (rtlgen/->register (car rands)))
		     (rand2 (rtlgen/->register (cadr rands)))
		     (flo1 (rtlgen/new-reg))
		     (flo2 (rtlgen/new-reg)))
		(rtlgen/assign! flo1 `(OBJECT->FLOAT ,rand1))
		(rtlgen/assign! flo2 `(OBJECT->FLOAT ,rand2))
		(rtlgen/branch state
			       `(FLONUM-PRED-2-ARGS ,name ,flo1 ,flo2))))))))
  (define-flonum-predicate flo:= 'FLONUM-EQUAL?
    rtlgen/branch/unlikely)
  (define-flonum-predicate flo:< 'FLONUM-LESS?
    rtlgen/branch/unpredictable)
  (define-flonum-predicate flo:> 'FLONUM-GREATER?
    rtlgen/branch/unpredictable))

#|
;; These don't work, because the operands are evaluated by this point,
;; and one of the operands is (LOOKUP ,cache-name) where cache-name
;; is unbound!

(let ((define-reference-to-cache
	(lambda (%variable-cache keyword)
	  (define-open-coder/value %variable-cache 2
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let ((name (second rands)))
		(if (not (QUOTE/? name))
		    (internal-error "Unexpected variable cache name" name))
		(rtlgen/value-assignment state `(,keyword ,(cadr name)))))))))

  (define-reference-to-cache %variable-read-cache 'VARIABLE-CACHE)
  (define-reference-to-cache %variable-write-cache 'ASSIGNMENT-CACHE))
|#

(for-each
 (lambda (prim-name)
   (define-open-coder/value prim-name 1
     (lambda (state rands open-coder)
       open-coder			; ignored
       (let ((rand  (rtlgen/->register (first rands))))
	 (rtlgen/value-assignment state `(OBJECT->TYPE ,rand))))))
 '(OBJECT-TYPE
   PRIMITIVE-OBJECT-TYPE))

(define-open-coder/value 'OBJECT-DATUM 1
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let ((rand  (rtlgen/->register (first rands))))
      (rtlgen/value-assignment state `(OBJECT->DATUM ,rand)))))

(define-open-coder/value 'PRIMITIVE-OBJECT-SET-TYPE 2
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let ((tag  (first rands))
	  (obj  (rtlgen/->register (second rands))))
      (let ((obj* (rtlgen/new-reg)))
	(rtlgen/assign! obj* `(OBJECT->DATUM ,obj))
	(cond ((rtlgen/constant? tag)
	       (rtlgen/value-assignment
		state
		`(CONS-NON-POINTER
		  (MACHINE-CONSTANT ,(rtlgen/constant-value tag))
		  ,obj*)))
	      (else
	       (let* ((tag*  (rtlgen/->register tag))
		      (tag** (rtlgen/new-reg)))
		 (rtlgen/assign! tag** `(OBJECT->DATUM ,tag*))
		 (rtlgen/value-assignment
		  state
		  `(CONS-NON-POINTER ,tag** ,obj*)))))))))

(define (rtlgen/cons state rands tag)
  (rtlgen/heap-push! rands)
  (rtlgen/value-assignment
   state
   `(CONS-POINTER
     ,tag
     ,(rtlgen/->register
       `(OFFSET-ADDRESS ,(rtlgen/reference-to-free)
			(MACHINE-CONSTANT ,(- 0 (length rands))))))))

(let ((define-tagged-allocator
	(lambda (name arity tag profile-name)
	  (define-open-coder/value name arity
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (rtlgen/emit!/profile profile-name 1)
	      (rtlgen/cons state rands `(MACHINE-CONSTANT ,tag)))))))
  (define-tagged-allocator 'MAKE-CELL 1 (machine-tag 'CELL) 'CELL)
  (define-tagged-allocator %make-static-binding 1 (machine-tag 'CELL) #F)
  (define-tagged-allocator 'CONS 2 (machine-tag 'PAIR) 'CONS)
  (define-tagged-allocator %cons 2 (machine-tag 'PAIR) 'CONS)
  (define-tagged-allocator %make-entity 2 (machine-tag 'ENTITY) 'MAKE-ENTITY))

(define-open-coder/value %make-cell 2
  (let ((tag (machine-tag 'CELL)))
    (lambda (state rands open-coder)
      open-coder			; ignored
      (rtlgen/emit!/profile 'CELL 1)
      (rtlgen/cons state (list (first rands)) `(MACHINE-CONSTANT ,tag)))))

(define-open-coder/value %make-promise 1
  (let ((tag (machine-tag 'DELAYED)))
    (lambda (state rands open-coder)
      open-coder			; ignored
      (rtlgen/cons state
		   (cons `(CONSTANT 0) rands)
		   `(MACHINE-CONSTANT ,tag)))))

(let ((define-vector-allocator
	(lambda (name tag)
	  (define-open-coder/value name false
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (rtlgen/cons state
			   (cons `(CONSTANT ,(length rands)) rands)
			   `(MACHINE-CONSTANT ,tag)))))))
  (define-vector-allocator 'VECTOR  (machine-tag 'VECTOR))
  (define-vector-allocator %vector  (machine-tag 'VECTOR))
  (define-vector-allocator '%RECORD (machine-tag 'RECORD)))

(define-open-coder/value 'SYSTEM-PAIR-CONS 3
  (lambda (state rands open-coder)
    open-coder				; ignored
    (rtlgen/emit!/profile 'SYSTEM-PAIR-CONS 1)
    (rtlgen/cons state
		 (cdr rands)
		 (let ((tag (car rands)))
		   (if (rtlgen/constant? tag)
		       `(MACHINE-CONSTANT ,(rtlgen/constant-value tag))
		       (rtlgen/->register tag))))))

(define-open-coder/value 'STRING-ALLOCATE 1
  (let ((string-tag (machine-tag 'STRING))
	(nmv-tag    (machine-tag 'MANIFEST-NM-VECTOR)))
    (lambda (state rands open-coder)
      open-coder			; ignored
      (let ((char-len (rtlgen/allocate-length (first rands) 'STRING-ALLOCATE)))
	(let* ((free       (rtlgen/reference-to-free))
	       (result     (rtlgen/value-assignment
			    state
			    `(CONS-POINTER (MACHINE-CONSTANT ,string-tag)
					   ,free)))
	       (word-len   (rtlgen/chars->words char-len))
	       (nmv-header (rtlgen/new-reg))
	       (slen       (rtlgen/new-reg))
	       (zero       (rtlgen/new-reg)))
	  (rtlgen/declare-allocation! (+ word-len 2))
	  (rtlgen/assign!*
	   `((ASSIGN ,nmv-header
		     (CONS-NON-POINTER (MACHINE-CONSTANT ,nmv-tag)
				       (MACHINE-CONSTANT ,(+ word-len 1))))
	     (ASSIGN (OFFSET ,free (MACHINE-CONSTANT 0)) ,nmv-header)
	     (ASSIGN ,slen (CONSTANT ,char-len))
	     (ASSIGN (OFFSET ,free (MACHINE-CONSTANT 1)) ,slen)
	     (ASSIGN ,free
		     (OFFSET-ADDRESS ,free
				     (MACHINE-CONSTANT ,(+ word-len 2))))
	     (ASSIGN ,zero (MACHINE-CONSTANT 0))
	     (ASSIGN (OFFSET ,free (MACHINE-CONSTANT -1)) ,zero)))
	  result)))))

(define-open-coder/value 'FLOATING-VECTOR-CONS 1
  ;; (flo:vector-cons <small-known-integer>)
  (let ((fv-tag  (machine-tag 'FLOATING-POINT-VECTOR))
	(nmv-tag (machine-tag 'MANIFEST-NM-VECTOR)))
    (lambda (state rands open-coder)
      open-coder			; ignored
      (rtlgen/floating-align-free)
      (let* ((free   (rtlgen/reference-to-free))
	     (result (rtlgen/value-assignment
		      state
		      `(CONS-POINTER (MACHINE-CONSTANT ,fv-tag)
				     ,free)))
	     (len  (rtlgen/allocate-length (first rands) 'FLOATING-VECTOR-CONS))
	     (word-len   (rtlgen/fp->words len))
	     (nmv-header (rtlgen/new-reg)))
	(rtlgen/declare-allocation! (+ word-len 1))
	(rtlgen/assign!*
	 `((ASSIGN ,nmv-header
		   (CONS-NON-POINTER (MACHINE-CONSTANT ,nmv-tag)
				     (MACHINE-CONSTANT ,word-len)))
	   (ASSIGN (OFFSET ,free (MACHINE-CONSTANT 0)) ,nmv-header)
	   (ASSIGN ,free
		   (OFFSET-ADDRESS ,free
				   (MACHINE-CONSTANT
				    ,(+ word-len 1))))))
	result))))

(define-open-coder/value 'VECTOR-CONS 2
  (let ((vector-tag (machine-tag 'VECTOR)))
    (lambda (state rands open-coder)
      open-coder			; ignored
      (let ((len  (rtlgen/allocate-length (first rands) 'VECTOR-CONS))
	    (fill (rtlgen/->register (second rands))))
	(if (> len *vector-cons-max-open-coded-length*)
	    (internal-error "Open coding VECTOR-CONS with too large a length"
			    len))
	(rtlgen/cons state
		     (cons `(CONSTANT ,len) (make-list len fill))
		     `(MACHINE-CONSTANT ,vector-tag))))))

;; *** STRING-ALLOCATE, FLOATING-VECTOR-CONS, and perhaps VECTOR-CONS
;; should always be in-lined, even when the length argument is not known.
;; They can do a late back out when there is no space, much like generic
;; arithmetic backs out when the operands are not appropriate fixnums. ***

(define (rtlgen/allocate-length len proc)
  (if (not (rtlgen/integer-constant? len))
      (internal-error
       "Open coding allocation primitive with non-constant/non-integer length"
       len proc))
  (rtlgen/constant-value len))

(define-open-coder/value %variable-cell-ref 1
  (lambda (state rands open-coder)
    open-coder
    (let ((cell (rtlgen/->register (first rands))))
      (rtlgen/value-assignment state `(OFFSET ,cell (MACHINE-CONSTANT 0))))))

(define-open-coder/value %static-binding-ref 2
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let ((name (second rands)))
      (if (not (rtlgen/constant? name))
	  (internal-error "Unexpected name to static-binding-ref" name))
      (let ((cell (rtlgen/->register
		   `(STATIC-CELL ,(rtlgen/constant-value name)))))
	(rtlgen/value-assignment state
				 `(OFFSET ,cell (MACHINE-CONSTANT 0)))))))

#|
;; This is not done this way because stack closures are handled specially,
;; with RTL registers assigned early to their elements to allow painless
;; stack reformatting later.
;; In particular, %stack-closure-ref cannot be open-coded in the normal
;; way because it wants to examine the rands BEFORE rtl generation.

(define-open-coder/value %stack-closure-ref 3
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let ((closure (rtlgen/->register (first rands)))
	  (offset  (second rands)))
      (if (not (rtlgen/integer-constant? offset))
	  (internal-error "Non-constant index to stack-closure-ref" offset))
      (rtlgen/value-assignment
       state
       `(OFFSET ,closure
		(MACHINE-CONSTANT ,(rtlgen/constant-value offset)))))))
|#

(define (rtlgen/expr/stack-closure-ref state rands)
  (let ((name (third rands)))
    (if (not (QUOTE/? name))
	(internal-error "Unexpected name to stack-closure-ref" rands))
    (let* ((name*  (quote/text name))
	   (place  (rtlgen/binding/find name* (rtlgen/state/env state))))
      (if (not place)
	  (internal-error "stack binding not found" name*)
	  (rtlgen/expr/simple-value state (rtlgen/binding/place place))))))

(define (rtlgen/fixed-selection state tag rand offset)
  tag					; ignored
  (let* ((rand    (rtlgen/->register rand))
	 (address (rtlgen/new-reg)))
    (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
    (rtlgen/value-assignment state
			     `(OFFSET ,address (MACHINE-CONSTANT ,offset)))))

(let ((define-fixed-selector
	(lambda (name tag offset arity)
	  (define-open-coder/value name arity
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (rtlgen/fixed-selection state tag (first rands) offset))))))
  (define-fixed-selector 'CELL-CONTENTS     (machine-tag 'CELL) 0 1)
  (define-fixed-selector %cell-ref          (machine-tag 'CELL) 0 2)
  (define-fixed-selector %car               (machine-tag 'PAIR) 0 1)
  (define-fixed-selector %cdr               (machine-tag 'PAIR) 1 1)
  ;;(define-fixed-selector 'CAR               (machine-tag 'PAIR) 0 1)
  ;;(define-fixed-selector 'CDR               (machine-tag 'PAIR) 1 1)
  (define-fixed-selector 'SYSTEM-PAIR-CAR   false 0 1)
  (define-fixed-selector 'SYSTEM-PAIR-CDR   false 1 1)
  (define-fixed-selector 'SYSTEM-HUNK3-CXR0 false 0 1)
  (define-fixed-selector 'SYSTEM-HUNK3-CXR1 false 1 1)
  (define-fixed-selector 'SYSTEM-HUNK3-CXR2 false 2 1))

(let ((define-indexed-selector
	(lambda (name tag offset arity)
	  (define-open-coder/value name arity
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let ((index (second rands)))
		(cond ((rtlgen/integer-constant? index)
		       (rtlgen/fixed-selection
			state
			tag
			(first rands)
			(+ offset (rtlgen/constant-value index))))
		      ((rtlgen/indexed-loads? 'WORD)
		       ;; This allows CSE of the offset-address
		       (let* ((rand    (rtlgen/->register (first rands)))
			      (index*  (rtlgen/->register index))
			      (address (rtlgen/new-reg))
			      (ptr     (rtlgen/new-reg)))
			 (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
			 (rtlgen/assign!
			  ptr
			  `(OFFSET-ADDRESS ,address
					   (MACHINE-CONSTANT ,offset)))
			 (rtlgen/value-assignment state
						  `(OFFSET ,ptr ,index*))))
		      (else
		       (let* ((rand    (rtlgen/->register (first rands)))
			      (index*  (rtlgen/->register index))
			      (address (rtlgen/new-reg))
			      (ptr     (rtlgen/new-reg)))
			 (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
			 (rtlgen/assign! ptr
					 `(OFFSET-ADDRESS ,address ,index*))
			 (rtlgen/value-assignment
			  state
			  `(OFFSET ,ptr (MACHINE-CONSTANT ,offset))))))))))))
  ;;(define-indexed-selector 'VECTOR-REF (machine-tag 'VECTOR) 1 2)
  (define-indexed-selector %vector-ref (machine-tag 'VECTOR) 1 2)
  ;;(define-indexed-selector '%RECORD-REF (machine-tag 'RECORD) 1 2)
  (define-indexed-selector %%RECORD-REF (machine-tag 'RECORD) 1 2)
  ;; NOTE: This assumes that the result of the following two is always
  ;; an object.  If it isn't it could be incorrectly preserved, and...
  (define-indexed-selector 'SYSTEM-VECTOR-REF false 1 2)
  (define-indexed-selector 'PRIMITIVE-OBJECT-REF false 0 2))

(define-open-coder/value %heap-closure-ref 3
  (let ((offset (rtlgen/closure-first-offset))
	(closure-tag  (machine-tag 'COMPILED-ENTRY)))
    (lambda (state rands open-coder)
      open-coder			; ignored
      (let ((vector (rtlgen/vector-constant? (second rands)))
	    (name   (third rands)))
	(if (and vector
		 (rtlgen/constant? (third rands)))
	    (let ((index (vector-index vector (rtlgen/constant-value name))))
	      (if (rtlgen/tagged-closures?)
		  (rtlgen/fixed-selection state
					  closure-tag
					  (first rands)
					  (+ offset index))
		  (rtlgen/value-assignment
		   state
		   `(OFFSET ,(rtlgen/->register (first rands))
			    (MACHINE-CONSTANT
			     ,(+ offset index))))))
	    (internal-error "%heap-closure-ref: non-constant specifier"
			    rands))))))

;; NOTE: These do not use rtlgen/assign! because the length field
;; may not be an object, and the preservation code assumes that
;; the OFFSET address syllable always denotes an object.

(let* ((fixnum-tag (machine-tag 'POSITIVE-FIXNUM))
       (define-fixnumized-selector/tagged
	 (lambda (name tag off)
	   tag
	   (define-open-coder/value name 1
	     (lambda (state rands open-coder)
	       open-coder		; ignored
	       (let* ((rand    (rtlgen/->register (first rands)))
		      (address (rtlgen/new-reg))
		      (field   (rtlgen/new-reg))
		      (datum   (rtlgen/new-reg)))
		 (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
		 (rtlgen/assign!*
		  (list
		   `(ASSIGN ,field (OFFSET ,address (MACHINE-CONSTANT ,off)))
		   `(ASSIGN ,datum (OBJECT->DATUM ,field))))
		 (rtlgen/value-assignment
		  state
		  `(CONS-NON-POINTER (MACHINE-CONSTANT ,fixnum-tag)
				     ,datum)))))))
       (define-fixnumized-selector
	 (lambda (name tag off)
	   tag
	   (define-open-coder/value name 1
	     (lambda (state rands open-coder)
	       open-coder		; ignored
	       (let* ((rand (rtlgen/->register (car rands)))
		      (address (rtlgen/new-reg))
		      (field (rtlgen/new-reg)))
		 (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
		 (rtlgen/assign! field `(OFFSET ,address (MACHINE-CONSTANT ,off)))
		 (rtlgen/value-assignment
		  state
		  `(CONS-NON-POINTER (MACHINE-CONSTANT ,fixnum-tag)
				     ,field))))))))
  ;; Primitives VECTOR-LENGTH %RECORD-LENGTH STRING-LENGTH BIT-STRING-LENGTH
  ;; are calls the microcode (i.e. signal errors), so do not appear here.
  (define-fixnumized-selector/tagged %vector-length  (machine-tag 'VECTOR) 0)
  (define-fixnumized-selector/tagged %%RECORD-LENGTH (machine-tag 'RECORD) 0)
  (define-fixnumized-selector/tagged 'SYSTEM-VECTOR-SIZE false 0)
  (define-fixnumized-selector %STRING-LENGTH     (machine-tag 'STRING)    1)
  (define-fixnumized-selector %BIT-STRING-LENGTH (machine-tag 'VECTOR-1B) 1))

(define-open-coder/value %FLOATING-VECTOR-LENGTH 1 ; 'FLOATING-VECTOR-LENGTH
  (let ((factor (rtlgen/fp->words 1))
	(tag (machine-tag 'POSITIVE-FIXNUM)))
    (cond ((= factor 1)
	   (lambda (state rands open-coder)
	     open-coder			; ignored
	     (let* ((rand    (rtlgen/->register (first rands)))
		    (address (rtlgen/new-reg))
		    (field   (rtlgen/new-reg))
		    (datum   (rtlgen/new-reg)))
	       (rtlgen/assign!*
		(list
		 `(ASSIGN ,address (OBJECT->ADDRESS ,rand))
		 `(ASSIGN ,field   (OFFSET ,address (MACHINE-CONSTANT 0)))
		 `(ASSIGN ,datum   (OBJECT->DATUM ,field))))
	       (rtlgen/value-assignment
		state
		`(CONS-NON-POINTER (MACHINE-CONSTANT ,tag) ,datum)))))
	  ((power-of-two? factor)
	   => (lambda (shift)
		(lambda (state rands open-coder)
		  open-coder		; ignored
		  (let* ((rand     (rtlgen/->register (first rands)))
			 (address  (rtlgen/new-reg))
			 (field    (rtlgen/new-reg))
			 (datum    (rtlgen/new-reg))
			 (constant (rtlgen/new-reg))
			 (datum2   (rtlgen/new-reg)))
		    (rtlgen/assign!*
		     (list
		      `(ASSIGN ,address (OBJECT->ADDRESS ,rand))
		      `(ASSIGN ,field   (OFFSET ,address (MACHINE-CONSTANT 0)))
		      `(ASSIGN ,datum   (OBJECT->DATUM ,field))
		      `(ASSIGN ,constant (CONSTANT ,(- 0 shift)))
		      `(ASSIGN ,datum2 (FIXNUM-2-ARGS FIXNUM-LSH ,datum ,constant #F))))
		    (rtlgen/value-assignment
		     state
		     `(CONS-NON-POINTER (MACHINE-CONSTANT ,tag) ,datum2))))))
	  (else
	   (internal-error
	    "Floating-point values have unexpected size in words" factor)))))

(let ((define-fixnum-primitive/1
	(lambda (prim-name operation-name)
	  (define-open-coder/value prim-name 1
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let ((rand (rtlgen/->register (first rands))))
		(rtlgen/value-assignment state
		 `(FIXNUM-1-ARG ,operation-name ,rand #F)))))))
      (define-fixnum-primitive/2
	(lambda (prim-name operation-name)
	  (define-open-coder/value prim-name 2
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let* ((rand1 (rtlgen/->register (first rands)))
		     (rand2 (rtlgen/->register (second rands))))
		(rtlgen/value-assignment state
		 `(FIXNUM-2-ARGS ,operation-name
				 ,rand1 ,rand2 #F))))))))
  #| DIVIDE-FIXNUM GCD-FIXNUM |#
  (define-fixnum-primitive/2 'PLUS-FIXNUM   'PLUS-FIXNUM)
  (define-fixnum-primitive/2 'MINUS-FIXNUM  'MINUS-FIXNUM)
  (define-fixnum-primitive/2 'MULTIPLY-FIXNUM  'MULTIPLY-FIXNUM)
  (define-fixnum-primitive/2 'FIXNUM-QUOTIENT  'FIXNUM-QUOTIENT)
  (define-fixnum-primitive/2 'FIXNUM-REMAINDER 'FIXNUM-REMAINDER)
  (define-fixnum-primitive/2 'FIXNUM-ANDC 'FIXNUM-ANDC)
  (define-fixnum-primitive/2 'FIXNUM-AND  'FIXNUM-AND)
  (define-fixnum-primitive/2 'FIXNUM-OR   'FIXNUM-OR)
  (define-fixnum-primitive/2 'FIXNUM-XOR  'FIXNUM-XOR)
  (define-fixnum-primitive/2 'FIXNUM-LSH  'FIXNUM-LSH)
  (define-fixnum-primitive/1 'ONE-PLUS-FIXNUM       'ONE-PLUS-FIXNUM)
  (define-fixnum-primitive/1 'MINUS-ONE-PLUS-FIXNUM 'MINUS-ONE-PLUS-FIXNUM)
  (define-fixnum-primitive/1 'FIXNUM-NOT 'FIXNUM-NOT))

(let ((define-flonum-primitive/1
	(lambda (prim-name operation)
	  (define-open-coder/value prim-name 1
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let* ((rand (rtlgen/->register (first rands)))
		     (flo  (rtlgen/new-reg)))
		(rtlgen/assign! flo `(OBJECT->FLOAT ,rand))
		(rtlgen/value-assignment
		 state
		 `(FLOAT->OBJECT
		   ,(rtlgen/->register
		     `(FLONUM-1-ARG ,operation ,flo #F)))))))))
      (define-flonum-primitive/2
	(lambda (prim-name operation)
	  (define-open-coder/value prim-name 2
	    (lambda (state rands open-coder)
	      open-coder		; ignored
	      (let* ((rand1 (rtlgen/->register (first rands)))
		     (rand2 (rtlgen/->register (second rands)))
		     (flo1 (rtlgen/new-reg))
		     (flo2 (rtlgen/new-reg)))
		(rtlgen/assign! flo1 `(OBJECT->FLOAT ,rand1))
		(rtlgen/assign! flo2 `(OBJECT->FLOAT ,rand2))
		(rtlgen/value-assignment
		 state
		 `(FLOAT->OBJECT
		   ,(rtlgen/->register
		     `(FLONUM-2-ARGS ,operation ,flo1 ,flo2 #F))))))))))

  (define-flonum-primitive/1 'FLONUM-ABS  'FLONUM-ABS)
  (define-flonum-primitive/1 'FLONUM-ACOS 'FLONUM-ACOS)
  (define-flonum-primitive/1 'FLONUM-ASIN 'FLONUM-ASIN)
  (define-flonum-primitive/1 'FLONUM-ATAN 'FLONUM-ATAN)
  (define-flonum-primitive/1 'FLONUM-CEILING 'FLONUM-CEILING)
  (define-flonum-primitive/1 'FLONUM-CEILING->EXACT 'FLONUM-CEILING->EXACT)
  (define-flonum-primitive/1 'FLONUM-COS 'FLONUM-COS)
  (define-flonum-primitive/1 'FLONUM-EXP 'FLONUM-EXP)
  (define-flonum-primitive/1 'FLONUM-FLOOR 'FLONUM-FLOOR)
  (define-flonum-primitive/1 'FLONUM-FLOOR->EXACT 'FLONUM-FLOOR->EXACT)
  (define-flonum-primitive/1 'FLONUM-LOG 'FLONUM-LOG)
  (define-flonum-primitive/1 'FLONUM-NEGATE 'FLONUM-NEGATE)
  (define-flonum-primitive/1 'FLONUM-NORMALIZE 'FLONUM-NORMALIZE)
  (define-flonum-primitive/1 'FLONUM-ROUND 'FLONUM-ROUND)
  (define-flonum-primitive/1 'FLONUM-ROUND->EXACT 'FLONUM-ROUND->EXACT)
  (define-flonum-primitive/1 'FLONUM-SIN  'FLONUM-SIN)
  (define-flonum-primitive/1 'FLONUM-SQRT 'FLONUM-SQRT)
  (define-flonum-primitive/1 'FLONUM-TAN  'FLONUM-TAN)
  (define-flonum-primitive/1 'FLONUM-TRUNCATE 'FLONUM-TRUNCATE) 
  (define-flonum-primitive/1 'FLONUM-TRUNCATE->EXACT 'FLONUM-TRUNCATE->EXACT)

  (define-flonum-primitive/2 'FLONUM-ADD   'FLONUM-ADD)
  (define-flonum-primitive/2 'FLONUM-ATAN2 'FLONUM-ATAN2)
  (define-flonum-primitive/2 'FLONUM-DENORMALIZE 'FLONUM-DENORMALIZE)
  (define-flonum-primitive/2 'FLONUM-DIVIDE   'FLONUM-DIVIDE)
  (define-flonum-primitive/2 'FLONUM-EXPT     'FLONUM-EXPT)
  (define-flonum-primitive/2 'FLONUM-MULTIPLY 'FLONUM-MULTIPLY)
  (define-flonum-primitive/2 'FLONUM-SUBTRACT 'FLONUM-SUBTRACT))


(define-open-coder/value %fixnum->flonum 1
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let* ((rand (rtlgen/->register (first rands))))
      (rtlgen/value-assignment
       state
       `(FLOAT->OBJECT
	 ,(rtlgen/->register
	   `(FLONUM-1-ARG FIXNUM->FLONUM ,rand #F)))))))

(let ((char-tag   (machine-tag 'CHARACTER))
      (fixnum-tag (machine-tag 'POSITIVE-FIXNUM)))
  (let ((define-datum-conversion
	  (lambda (name output-tag)
	    (define-open-coder/value name 1
	      (lambda (state rands open-coder)
		open-coder		; ignored
		(let* ((rand* (rtlgen/->register (first rands)))
		       (temp  (rtlgen/new-reg)))
		  (rtlgen/assign! temp `(OBJECT->DATUM ,rand*))
		  (rtlgen/value-assignment
		   state
		   `(CONS-NON-POINTER (MACHINE-CONSTANT ,output-tag)
				      ,temp)))))))

	(define-masked-datum-conversion
	  (lambda (name mask)
	    (define-open-coder/value name 1
	      (lambda (state rands open-coder)
		open-coder		; ignored
		(let* ((rand*    (rtlgen/->register (first rands)))
		       (temp     (rtlgen/new-reg))
		       (mask-reg (rtlgen/new-reg))
		       (masked   (rtlgen/new-reg)))
		  (rtlgen/assign!*
		   `((ASSIGN ,temp (OBJECT->DATUM ,rand*))
		     (ASSIGN ,mask-reg (CONSTANT ,mask))
		     (ASSIGN ,masked
			     (FIXNUM-2-ARGS FIXNUM-AND ,temp ,mask-reg #F))))
		  (rtlgen/value-assignment
		   state
		   `(CONS-NON-POINTER (MACHINE-CONSTANT ,fixnum-tag)
				      ,masked))))))))

    (define-datum-conversion  'INTEGER->CHAR char-tag)
    (define-datum-conversion  'ASCII->CHAR   char-tag)
    (define-masked-datum-conversion 'CHAR->ASCII #xff)
    (define-masked-datum-conversion 'CHAR-CODE   #x7f)
    (define-datum-conversion  'CHAR->INTEGER fixnum-tag)))

(let* ((off (rtlgen/words->chars 2))
       (define-string-reference
	 (lambda (name tag)
	   (define-open-coder/value name 2
	     (lambda (state rands open-coder)
	       open-coder		; ignored
	       (let* ((index   (second rands))
		      (rand    (rtlgen/->register (first rands)))
		      (address (rtlgen/new-reg))
		      (byte    (rtlgen/new-reg)))
		 (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
		 (cond ((rtlgen/constant? index)
			(let ((index* (rtlgen/constant-value index)))
			  (rtlgen/assign! byte
					  `(BYTE-OFFSET ,address
							(MACHINE-CONSTANT
							 ,(+ off index*))))))
		       ((rtlgen/indexed-loads? 'BYTE)
			(let* ((index* (rtlgen/->register index))
			       (ptr    (rtlgen/new-reg)))
			  (rtlgen/assign!
			   ptr
			   `(BYTE-OFFSET-ADDRESS ,address
						 (MACHINE-CONSTANT ,off)))
			  (rtlgen/assign! byte `(BYTE-OFFSET ,ptr ,index*))))
		       (else
			(let* ((index* (rtlgen/->register index))
			       (ptr    (rtlgen/new-reg)))
			  (rtlgen/assign!
			   ptr
			   `(BYTE-OFFSET-ADDRESS ,address ,index*))
			  (rtlgen/assign!
			   byte
			   `(BYTE-OFFSET ,ptr 
					 (MACHINE-CONSTANT ,off))))))
		 (rtlgen/value-assignment
		  state
		  `(CONS-NON-POINTER (MACHINE-CONSTANT ,tag) ,byte))))))))
  ;; Primitives VECTOR-8B-REF STRING-REF are used to signal errors
  (define-string-reference %VECTOR-8B-REF (machine-tag 'POSITIVE-FIXNUM))
  (define-string-reference %STRING-REF    (machine-tag 'CHARACTER)))

(define-open-coder/value %FLOATING-VECTOR-REF 2 ;'FLOATING-VECTOR-REF 2
  (let ((factor (rtlgen/fp->words 1)))
    (if (= factor 1)
	(lambda (state rands open-coder)
	  open-coder			; ignored
	  (let* ((index   (second rands))
		 (rand    (rtlgen/->register (first rands)))
		 (address (rtlgen/new-reg))
		 (float   (rtlgen/new-reg)))
	    (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
	    (cond ((rtlgen/constant? index)
		   (let ((index* (rtlgen/constant-value index)))
		     (rtlgen/assign! float
				     `(FLOAT-OFFSET ,address
						    (MACHINE-CONSTANT
						     ,(+ 1 index*))))))
		  ((rtlgen/indexed-loads? 'FLOAT)
		   (let* ((index* (rtlgen/->register index))
			  (ptr    (rtlgen/new-reg)))
		     (rtlgen/assign!
		      ptr
		      `(FLOAT-OFFSET-ADDRESS ,address (MACHINE-CONSTANT 1)))
		     (rtlgen/assign! float `(FLOAT-OFFSET ,ptr ,index*))))
		  (else
		   (let* ((index* (rtlgen/->register index))
			  (ptr    (rtlgen/new-reg)))
		     (rtlgen/assign!
		      ptr
		      `(FLOAT-OFFSET-ADDRESS ,address ,index*))
		     (rtlgen/assign!
		      float
		      `(FLOAT-OFFSET ,ptr (MACHINE-CONSTANT 1))))))
	    (rtlgen/value-assignment state `(FLOAT->OBJECT ,float))))
	(lambda (state rands open-coder)
	  open-coder			; ignored
	  (let* ((index   (second rands))
		 (rand    (rtlgen/->register (first rands)))
		 (address (rtlgen/new-reg))
		 (ptr     (rtlgen/new-reg))
		 (float   (rtlgen/new-reg)))
	    (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
	    (rtlgen/assign! ptr `(OFFSET-ADDRESS ,address (MACHINE-CONSTANT 1)))
	    (cond ((rtlgen/constant? index)
		   (let ((index* (rtlgen/constant-value index)))
		     (rtlgen/assign!
		      float
		      `(FLOAT-OFFSET ,ptr (MACHINE-CONSTANT ,index*)))))
		  ((rtlgen/indexed-loads? 'FLOAT)
		   (let ((index* (rtlgen/->register index)))
		     (rtlgen/assign! float `(FLOAT-OFFSET ,ptr ,index*))))
		  (else
		   (let* ((index* (rtlgen/->register index))
			  (ptr2   (rtlgen/new-reg)))
		     (rtlgen/assign! ptr2
				     `(FLOAT-OFFSET-ADDRESS ,ptr ,index*))
		     (rtlgen/assign!
		      float
		      `(FLOAT-OFFSET ,ptr2 (MACHINE-CONSTANT 0))))))
	    (rtlgen/value-assignment state `(FLOAT->OBJECT ,float)))))))

(define (rtlgen/fixed-mutation rands offset)
  (let* ((rand    (rtlgen/->register (first rands)))
	 (value   (rtlgen/->register (second rands)))
	 (address (rtlgen/new-reg)))
    (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
    (rtlgen/emit!/1
     `(ASSIGN (OFFSET ,address (MACHINE-CONSTANT ,offset))
	      ,value))))

(define-open-coder/stmt %variable-cell-set! 2
  (lambda (state rands open-coder)
    state open-coder			; ignored
    (let* ((cell  (rtlgen/->register (first rands)))
	   (value (rtlgen/->register (second rands))))
      (rtlgen/emit!/1 `(ASSIGN (OFFSET ,cell (MACHINE-CONSTANT 0))
			       ,value)))))

(define-open-coder/stmt %static-binding-set! 3
  (lambda (state rands open-coder)
    state open-coder			; ignored
    (let ((name (third rands)))
      (if (not (rtlgen/constant? name))
	  (internal-error "Unexpected name to static-binding-set!" name))
      (let ((cell  (rtlgen/->register
		    `(STATIC-CELL ,(rtlgen/constant-value name))))
	    (value (rtlgen/->register (second rands))))
	(rtlgen/emit!/1
	 `(ASSIGN (OFFSET ,cell (MACHINE-CONSTANT 0)) ,value))))))

(define-open-coder/stmt %profile-data 1
  (lambda (state rands open-coder)
    state open-coder			; ignored
    (let ((data (first rands)))
      (if (not (rtlgen/constant? data))
	  (internal-error "Profile data must be constant" data))
      (rtlgen/emit!/1
       `(PROFILE-DATA (CONSTANT ,(rtlgen/constant-value data)))))))

(let ((define-fixed-mutator
	(lambda (name tag offset arity)
	  tag				; unused
	  (define-open-coder/stmt name arity
	    (lambda (state rands open-coder)
	      state open-coder		; ignored
	      (rtlgen/fixed-mutation rands offset))))))
  (define-fixed-mutator 'SET-CELL-CONTENTS! (machine-tag 'CELL) 0 2)
  (define-fixed-mutator %cell-set! (machine-tag 'CELL) 0 3)
  ;; Primitives SET-CAR! and SET-CDR! are used to signal errors
  (define-fixed-mutator %set-car!  (machine-tag 'PAIR) 0 2)
  (define-fixed-mutator %set-cdr!  (machine-tag 'PAIR) 1 2)
  (define-fixed-mutator 'SYSTEM-PAIR-SET-CAR!  false 0 2)
  (define-fixed-mutator 'SYSTEM-PAIR-SET-CDR!  false 1 2)
  (define-fixed-mutator 'SYSTEM-HUNK3-SET-CXR0!  false 0 2)
  (define-fixed-mutator 'SYSTEM-HUNK3-SET-CXR1!  false 1 2)
  (define-fixed-mutator 'SYSTEM-HUNK3-SET-CXR2!  false 2 2)
  (define-fixed-mutator 'SET-STRING-LENGTH! (machine-tag 'STRING) 1 2))

(let ((define-indexed-mutator
	(lambda (name tag offset arity)
	  tag				; unused
	  (define-open-coder/stmt name arity
	    (lambda (state rands open-coder)
	      state open-coder		; ignored
	      (let ((index (second rands)))
		(cond ((rtlgen/constant? index)
		       (rtlgen/fixed-mutation
			(list (first rands) (third rands))
			(+ offset (rtlgen/constant-value index))))
		      ((rtlgen/indexed-stores? 'WORD)
		       (let* ((rand    (rtlgen/->register (first rands)))
			      (index*  (rtlgen/->register index))
			      (value   (rtlgen/->register (third rands)))
			      (address (rtlgen/new-reg))
			      (ptr (rtlgen/new-reg)))
			 (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
			 (rtlgen/assign!
			  ptr
			  `(OFFSET-ADDRESS ,address
					   (MACHINE-CONSTANT ,offset)))
			 (rtlgen/emit!/1
			  `(ASSIGN (OFFSET ,ptr ,index*) ,value))))
		      (else
		       (let* ((rand    (rtlgen/->register (first rands)))
			      (index*  (rtlgen/->register index))
			      (value   (rtlgen/->register (third rands)))
			      (address (rtlgen/new-reg))
			      (ptr (rtlgen/new-reg)))
			 (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
			 (rtlgen/assign! ptr
					 `(OFFSET-ADDRESS ,address ,index*))
			 (rtlgen/emit!/1
			  `(ASSIGN (OFFSET ,ptr (MACHINE-CONSTANT ,offset))
				   ,value)))))))))))
  ;; Primitives VECTOR-SET! and %RECORD-SET! used to signal errors
  (define-indexed-mutator %vector-set!  (machine-tag 'VECTOR) 1 3)
  (define-indexed-mutator %%RECORD-SET! (machine-tag 'RECORD) 1 3)
  (define-indexed-mutator 'PRIMITIVE-OBJECT-SET! false 0 3))

(define-open-coder/stmt %heap-closure-set! 4
  (let ((offset (rtlgen/closure-first-offset))
	(closure-tag  (machine-tag 'COMPILED-ENTRY)))
    closure-tag
    (lambda (state rands open-coder)
      open-coder			; ignored
      (let ((vector (rtlgen/vector-constant? (second rands)))
	    (name   (fourth rands)))
	(if (and vector (rtlgen/constant? name))
	    (let ((index (vector-index vector (rtlgen/constant-value name))))
	      (if (rtlgen/tagged-closures?)
		  (rtlgen/fixed-mutation
		   (list (first rands) (third rands))
		   (+ offset index))
		  (rtlgen/emit!/1
		   `(ASSIGN (OFFSET ,(rtlgen/->register (car rands))
				    (MACHINE-CONSTANT ,(+ offset index)))
			    ,(rtlgen/->register (third rands))))))
	    (internal-error "%heap-closure-set!: non-constant specifier"
			    rands))))))

(let* ((off (rtlgen/words->chars 2))
       (define-string-mutation
	 (lambda (name)
	   (define-open-coder/stmt name 3
	     (lambda (state rands open-coder)
	       state open-coder		; ignored
	       (let* ((index   (second rands))
		      (rand    (rtlgen/->register (first rands)))
		      (address (rtlgen/new-reg))
		      (value   (rtlgen/->register (third rands)))
		      (byte    (rtlgen/new-reg)))
		 (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
		 (rtlgen/assign! byte `(OBJECT->DATUM ,value))
		 (cond ((rtlgen/constant? index)
			(let* ((index* (rtlgen/constant-value index)))
			  (rtlgen/emit!/1
			   `(ASSIGN (BYTE-OFFSET ,address
						 (MACHINE-CONSTANT
						  ,(+ off index*)))
				    ,byte))))
		       ((rtlgen/indexed-stores? 'BYTE)
			(let* ((index* (rtlgen/->register index))
			       (ptr    (rtlgen/new-reg)))
			  (rtlgen/assign!
			   ptr
			   `(BYTE-OFFSET-ADDRESS ,address
						 (MACHINE-CONSTANT ,off)))
			  (rtlgen/emit!/1
			   `(ASSIGN (BYTE-OFFSET ,ptr ,index*) ,byte))))
		       (else
			(let* ((index* (rtlgen/->register index))
			       (ptr    (rtlgen/new-reg)))
			  (rtlgen/assign!
			   ptr
			   `(BYTE-OFFSET-ADDRESS ,address ,index*))
			  (rtlgen/emit!/1
			   `(ASSIGN (BYTE-OFFSET ,ptr (MACHINE-CONSTANT ,off))
				    ,byte)))))))))))
  ;; Primitives VECTOR-8B-SET! STRING-SET!
  (define-string-mutation %VECTOR-8B-SET!)
  (define-string-mutation %STRING-SET!))

(define-open-coder/stmt %FLOATING-VECTOR-SET! 3 ;'FLOATING-VECTOR-SET! 3
  (let ((factor (rtlgen/fp->words 1)))
    (if (= factor 1)
	(lambda (state rands open-coder)
	  state open-coder		; ignored
	  (let* ((index   (second rands))
		 (rand    (rtlgen/->register (first rands)))
		 (address (rtlgen/new-reg))
		 (value   (rtlgen/->register (third rands)))
		 (float   (rtlgen/new-reg)))
	    (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
	    (rtlgen/assign! float `(OBJECT->FLOAT ,value))
	    (cond ((rtlgen/constant? index)
		   (let ((index* (rtlgen/constant-value index)))
		     (rtlgen/emit!/1
		      `(ASSIGN (FLOAT-OFFSET ,address
					     (MACHINE-CONSTANT ,(+ 1 index*)))
			       ,float))))
		  ((rtlgen/indexed-stores? 'FLOAT)
		   (let* ((index* (rtlgen/->register index))
			  (ptr    (rtlgen/new-reg)))
		     (rtlgen/assign!
		      ptr
		      `(FLOAT-OFFSET-ADDRESS ,address (MACHINE-CONSTANT 1)))
		     (rtlgen/emit!/1
		      `(ASSIGN (FLOAT-OFFSET ,ptr ,index*) ,float))))
		  (else
		   (let* ((index* (rtlgen/->register index))
			  (ptr    (rtlgen/new-reg)))
		     (rtlgen/assign! ptr
				     `(FLOAT-OFFSET-ADDRESS ,address ,index*))
		     (rtlgen/emit!/1
		      `(ASSIGN (FLOAT-OFFSET ,ptr (MACHINE-CONSTANT 1))
			       ,float)))))))
	(lambda (state rands open-coder)
	  state open-coder		; ignored
	  (let* ((index   (second rands))
		 (rand    (rtlgen/->register (first rands)))
		 (address (rtlgen/new-reg))
		 (ptr     (rtlgen/new-reg))
		 (value   (rtlgen/->register (third rands)))
		 (float   (rtlgen/new-reg)))
	    (rtlgen/assign! address `(OBJECT->ADDRESS ,rand))
	    (rtlgen/assign! ptr `(OFFSET-ADDRESS ,address
						 (MACHINE-CONSTANT 1)))
	    (rtlgen/assign! float `(OBJECT->FLOAT ,value))
	    (cond ((rtlgen/constant? index)
		   (let ((index* (rtlgen/constant-value index)))
		     (rtlgen/emit!/1
		      `(ASSIGN (FLOAT-OFFSET ,ptr
					     (MACHINE-CONSTANT ,index*))
			       ,float))))
		  ((rtlgen/indexed-stores? 'FLOAT)
		   (let ((index* (rtlgen/->register index)))
		     (rtlgen/emit!/1
		      `(ASSIGN (FLOAT-OFFSET ,ptr ,index*) ,float))))
		  (else
		   (let* ((index* (rtlgen/->register index))
			  (ptr2 (rtlgen/new-reg)))
		     (rtlgen/assign! ptr2
				     `(FLOAT-OFFSET-ADDRESS ,ptr ,index*))
		     (rtlgen/emit!/1
		      `(ASSIGN (FLOAT-OFFSET ,ptr2 (MACHINE-CONSTANT 0))
			       ,float))))))))))

;;;; Miscellaneous system primitives

(define-open-coder/pred 'HEAP-AVAILABLE? 1
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let* ((free   (rtlgen/reference-to-free))
	   (memtop (rtlgen/->register (rtlgen/fetch-memtop)))
	   (rand   (rtlgen/->register (first rands)))
	   (temp1  (rtlgen/new-reg))
	   (temp2  (rtlgen/new-reg)))
      (rtlgen/assign!*
       `((ASSIGN ,temp1 (OBJECT->DATUM ,rand))
	 (ASSIGN ,temp2 (OFFSET-ADDRESS ,free ,temp1))))
      (rtlgen/branch/likely
       state
       `(FIXNUM-PRED-2-ARGS LESS-THAN-FIXNUM? ,temp2 ,memtop)))))

(define-open-coder/value 'PRIMITIVE-GET-FREE 1
  (lambda (state rands open-coder)
    open-coder				; ignored
    (let* ((free (rtlgen/reference-to-free))
	   (rand (rtlgen/->register (first rands)))
	   (temp (rtlgen/new-reg)))
      (rtlgen/assign! temp `(OBJECT->DATUM ,rand))
      (rtlgen/value-assignment state `(CONS-POINTER ,temp ,free)))))

(define-open-coder/stmt 'PRIMITIVE-INCREMENT-FREE 1
  (lambda (state rands open-coder)
    state open-coder			; ignored
    (let* ((free (rtlgen/reference-to-free))
	   (rand (rtlgen/->register (first rands)))
	   (temp (rtlgen/new-reg)))
      (rtlgen/assign!*
       `((ASSIGN ,temp (OBJECT->DATUM ,rand))
	 (ASSIGN ,free (OFFSET-ADDRESS ,free ,temp)))))))

(define-open-coder/value 'GET-INTERRUPT-ENABLES 0
  (let ((tag (machine-tag 'POSITIVE-FIXNUM)))
    (lambda (state rands open-coder)
      open-coder rands			; ignored
      (let ((int-mask (rtlgen/->register (rtlgen/fetch-int-mask))))
	(rtlgen/value-assignment
	 state
	 `(CONS-NON-POINTER (MACHINE-CONSTANT ,tag) ,int-mask))))))

(define-open-coder/value %fetch-environment 0
  (lambda (state rands open-coder)
    rands open-coder			; ignored
    (rtlgen/value-assignment state (rtlgen/fetch-environment))))

;;;; Out of line hooks

(let ((define-out-of-line-primitive
	(lambda (operator prim-name arity)
	  (let ((primitive (make-primitive-procedure prim-name)))
	    (define-open-coder/out-of-line operator arity
	      (lambda (cont-label open-coder)
		open-coder		; ignored
		(rtlgen/emit!/1
		 `(INVOCATION:SPECIAL-PRIMITIVE ,(+ arity 1)
						,cont-label
						,primitive))))))))
  (define-out-of-line-primitive %+ '&+ 2)
  (define-out-of-line-primitive %- '&- 2)
  (define-out-of-line-primitive %* '&* 2)
  (define-out-of-line-primitive %/ '&/ 2)
  (define-out-of-line-primitive %quotient  'QUOTIENT 2)
  (define-out-of-line-primitive %remainder 'REMAINDER 2)
  (define-out-of-line-primitive %= '&= 2)
  (define-out-of-line-primitive %< '&< 2)
  (define-out-of-line-primitive %> '&> 2)
  (define-out-of-line-primitive %string-allocate 'STRING-ALLOCATE 1)
  (define-out-of-line-primitive %floating-vector-cons 'FLOATING-VECTOR-CONS 1)
  (define-out-of-line-primitive %vector-cons 'VECTOR-CONS 2)

  (define-out-of-line-primitive 'SET-INTERRUPT-ENABLES! 'SET-INTERRUPT-ENABLES! 1))

(let ((define-variable-ref
	(lambda (operator safe?)
	  (define-open-coder/special operator 1
	    (lambda (cont-label rands open-coder)
	      open-coder		; ignored
	      (let ((cell     (rtlgen/->register (first rands)))
		    (cell-loc (rtlgen/interpreter-call/argument-home 1)))
		(rtlgen/assign!*
		 (list `(ASSIGN ,cell-loc ,cell)
		       `(INTERPRETER-CALL:CACHE-REFERENCE ,cont-label
							  ,cell-loc
							  ,safe?)))))))))
  (define-variable-ref %hook-variable-cell-ref false)
  (define-variable-ref %hook-safe-variable-cell-ref true))

(define-open-coder/special %hook-variable-cell-set! 2
  (lambda (cont-label rands open-coder)
    open-coder				; ignored
    (let ((cell      (rtlgen/->register (first rands)))
	  (value     (rtlgen/->register (second rands)))
	  (cell-loc  (rtlgen/interpreter-call/argument-home 1))
	  (value-loc (rtlgen/interpreter-call/argument-home 2)))
      (rtlgen/assign!*
       (list `(ASSIGN ,value-loc ,value)
	     `(ASSIGN ,cell-loc ,cell)
	     `(INTERPRETER-CALL:CACHE-ASSIGNMENT ,cont-label
						 ,cell-loc
						 ,value-loc))))))

(let ((unexpected
       (lambda all
	 (let ((open-coder (car (last-pair all))))
	   (internal-error "Unexpected operator"
			   (rtlgen/open-coder/rator open-coder))))))

  (for-each
   (lambda (operation)
     (define-open-coder operation false
       unexpected unexpected unexpected unexpected unexpected))
   ;; These are rewritten by earlier stages or handled specially.
   ;; They should never be found.
   (list #|%vector-index|#
         %variable-cache-ref %variable-cache-set!
	 %safe-variable-cache-ref %stack-closure-ref
	 %internal-apply %internal-apply-unchecked
	 %primitive-apply %invoke-continuation
	 %invoke-operator-cache %invoke-remote-cache
	 %make-read-variable-cache %make-write-variable-cache
	 %make-operator-variable-cache %fetch-continuation
	 %fetch-stack-closure %make-stack-closure
	 %*define %execute %*define* %*make-environment
	 %copy-program %*lookup %*set! %*unassigned?
	 ;; Replaced for compatibility
	 %make-heap-closure %make-trivial-closure)))

#|
;; Missing:

|#

(define (call/%stack-closure-ref/unparse expr receiver)
  (let ((vector  (CALL/%stack-closure-ref/offset expr))
	(name    (CALL/%stack-closure-ref/name expr)))
    (if (and (QUOTE/? vector)
	     (QUOTE/? name))
	(let ((v  (quote/text vector))
	      (n  (quote/text name)))
	  (if (and (vector? v) (symbol? n))
	      (receiver v n))))))

(define (CALL/%stack-closure-ref/index expr)
  (call/%stack-closure-ref/unparse expr vector-index))

(define (CALL/%stack-closure-ref/index=? expr value)
  (call/%stack-closure-ref/unparse
   expr
   (lambda (v n)
     (and (vector? v)
	  (< -1 value (vector-length v))
	  (eq? (vector-ref v value) n)))))

;;;; Patterns

(define rtlgen/?lambda-list (->pattern-variable 'LAMBDA-LIST))
(define rtlgen/?frame-var (->pattern-variable 'FRAME-VAR))
(define rtlgen/?frame-vector (->pattern-variable 'FRAME-VECTOR))
(define rtlgen/?frame-vector* (->pattern-variable 'FRAME-VECTOR*))
(define rtlgen/?continuation-body (->pattern-variable 'CONTINUATION-BODY))
(define rtlgen/?rator (->pattern-variable 'RATOR))
(define rtlgen/?return-address (->pattern-variable 'RETURN-ADDRESS))
(define rtlgen/?closure-elts (->pattern-variable 'CLOSURE-ELTS))
(define rtlgen/?closure-elts* (->pattern-variable 'CLOSURE-ELTS*))
(define rtlgen/?rands (->pattern-variable 'RANDS))
(define rtlgen/?cont-name (->pattern-variable 'CONT-NAME))
(define rtlgen/?env-name (->pattern-variable 'ENV-NAME))
(define rtlgen/?body (->pattern-variable 'BODY))
(define rtlgen/?closed-vars (->pattern-variable 'CLOSED-VARS))
(define rtlgen/?closed-over-env-var
  (->pattern-variable 'CLOSED-OVER-ENV-VAR))

(define rtlgen/?closure-name (->pattern-variable 'CLOSURE-NAME))
(define rtlgen/?offset (->pattern-variable 'OFFSET))
(define rtlgen/?var-name (->pattern-variable 'VAR-NAME))

(define rtlgen/?lambda-expression (->pattern-variable 'LAMBDA-EXPRESSION))

(define rtlgen/continuation-pattern
  `(LAMBDA ,rtlgen/?lambda-list
     (LET ((,rtlgen/?frame-var
	    (CALL (QUOTE ,%fetch-stack-closure)
		  (QUOTE #F)
		  (QUOTE ,rtlgen/?frame-vector))))
       ,rtlgen/?continuation-body)))

(define rtlgen/stack-overwrite-pattern
  `(CALL (QUOTE ,%stack-closure-ref)
	 (QUOTE #F)
	 (LOOKUP ,rtlgen/?closure-name)
	 (QUOTE ,rtlgen/?offset)
	 (QUOTE ,rtlgen/?var-name)))

(define rtlgen/outer-expression-pattern
  `(LAMBDA (,rtlgen/?cont-name ,rtlgen/?env-name)
     ,rtlgen/?body))

(define rtlgen/top-level-trivial-closure-pattern
  `(CALL (QUOTE ,%invoke-continuation)
	 (LOOKUP ,rtlgen/?cont-name)
	 (CALL (QUOTE ,%make-trivial-closure)
	       (QUOTE #F)
	       ,rtlgen/?lambda-expression)))

(define rtlgen/top-level-heap-closure-pattern
  `(CALL (QUOTE ,%invoke-continuation)
	 (LOOKUP ,rtlgen/?cont-name)
	 (CALL (QUOTE ,%make-heap-closure)
	       (QUOTE #F)
	       ,rtlgen/?lambda-expression
	       ,rtlgen/?closed-vars
	       ,rtlgen/?closed-over-env-var)))

(define rtlgen/extended-call-pattern
  `(CALL (LAMBDA (,rtlgen/?cont-name)
	   (CALL (QUOTE ,rtlgen/?rator)
		 (CALL (QUOTE ,%make-stack-closure)
		       (QUOTE #F)
		       (QUOTE #F)
		       (QUOTE ,rtlgen/?frame-vector*)
		       (LOOKUP ,rtlgen/?cont-name)
		       ,@rtlgen/?closure-elts*)
		 ,@rtlgen/?rands))
	 (CALL (QUOTE ,%make-stack-closure)
	       (QUOTE #F)
	       ,rtlgen/?return-address
	       (QUOTE ,rtlgen/?frame-vector)
	       ,@rtlgen/?closure-elts)))

(define rtlgen/make-stack-closure-handler-pattern
  `(CALL ',%make-stack-closure
	 '#F
	 ,rtlgen/?lambda-expression
	 (QUOTE ,rtlgen/?frame-vector*)
	 ,rtlgen/?return-address
	 ,@rtlgen/?closure-elts*))

(define rtlgen/lambda-expr-pattern
  `(LAMBDA ,rtlgen/?lambda-list ,rtlgen/?body))

(define rtlgen/call-lambda-with-stack-closure-pattern
  `(CALL (LAMBDA (,rtlgen/?cont-name) ,rtlgen/?body)
	 (CALL ',%make-stack-closure
	       '#F
	       ,rtlgen/?lambda-expression
	       (QUOTE ,rtlgen/?frame-vector*)
	       ,rtlgen/?return-address
	       ,@rtlgen/?closure-elts*)))

;; Kludges

(define rtlgen/primitive-is-apply-like?
  (let ((apply-like-primitives
	 (map make-primitive-procedure
	      '(apply
		within-control-point scode-eval force
		execute-at-new-state-point return-to-application
		with-stack-marker with-interrupt-mask
		with-interrupts-reduced with-history-disabled))))
    (lambda (primitive)
      (memq primitive apply-like-primitives))))

(define (rtlgen/global-call-not-worth-interrupt-check? name+arity)
  ;; Some global procedures are known to the compiler and not worth an
  ;; interrupt check because we know that there cannot be a loop
  ;; without interrupt checks between that procedure and this one.
  (memq (first (quote/text name+arity))
	'(%COMPILED-CODE-SUPPORT:SIGNAL-ERROR-IN-PRIMITIVE
	  %COMPILED-CODE-SUPPORT:NONRESTARTABLE-CONTINUATION
	  COERCE-TO-COMPILED-PROCEDURE
	  ERROR:BAD-RANGE-ARGUMENT
	  ERROR:WRONG-TYPE-ARGUMENT
	  ERROR:WRONG-TYPE-DATUM)))

(define *rtlgen/omit-internal-interrupt-checks?* #T)

(define (rtlgen/omit-interrupt-check? procedure-name)
  (and *rtlgen/omit-internal-interrupt-checks?*
       (rtlgen/procedure-as-label? procedure-name)))

(define (rtlgen/procedure-as-label? procedure-name)
  (define (like? pattern)
    (let ((s-pat  (symbol-name pattern))
	  (s-lab  (symbol-name procedure-name)))
      (and (> (string-length s-lab) (string-length s-pat))
	   (substring=? s-pat 0 (string-length s-pat)
			s-lab 0 (string-length s-pat)))))
  (or (like? 'alt-)
      (like? 'cons-)
      (like? 'next-)
      (like? 'receiver-)))

(define (rtlgen/quick&dirty/forbid-interrupt-check! form)
  (form-map/put! *rtlgen/quick&dirty-interrupt-check-map* form #T))

(define (rtlgen/quick&dirty/forbid-interrupt-check? form)
  (form-map/get *rtlgen/quick&dirty-interrupt-check-map* form #F))



(define (rtlgen/check-declarations declarations)
  (define (check-declaration declaration)
    (if (and (pair? declaration)
	     (memq (car declaration) *rtlgen/valid-remaining-declarations*))
	unspecific
	(user-warning "Unused declaration" declaration)))
  (for-each check-declaration declarations))

(define *rtlgen/valid-remaining-declarations*
  '())

#|
;; New RTL:

(INVOCATION:REGISTER 0 #F (REGISTER n) #F (MACHINE-CONSTANT nregs))
(INVOCATION:PROCEDURE 0 cont-label label (MACHINE-CONSTANT nregs))
;; if cont-label is not false, this is expected to set up the
;; continuation in the standard location (register or top of stack)

(INVOCATION:NEW-APPLY
 frame-size cont-label (REGISTER dest) (MACHINE-CONSTANT nregs))
;; if cont-label is not false, this is expected to set up the
;; continuation in the standard location (register or top of stack)

(RETURN-ADDRESS label (MACHINE-CONSTANT n) (MACHINE-CONSTANT m))
   n --> number of items saved on the stack
   m --> arity
(PROCEDURE label (MACHINE-CONSTANT frame-size))
(TRIVIAL-CLOSURE label (MACHINE-CONSTANT min) (MACHINE-CONSTANT max))
(CLOSURE label (MACHINE-CONSTANT n))
(EXPRESSION label)

(INTERRUPT-CHECK:CLOSURE intrpt? heap? stack? (MACHINE-CONSTANT fs))
(INTERRUPT-CHECK:PROCEDURE intrpt? heap? stack? label (MACHINE-CONSTANT fs))
(INTERRUPT-CHECK:CONTINUATION intrpt? heap? stack? label (MACHINE-CONSTANT fs))
;; fs is the frame size, including the continuation and the
;; self-reference (heap closures only)

(ASSIGN (REGISTER n) (ALIGN-FLOAT (REGISTER m))) ; float alignment
(ASSIGN (REGISTER n) (STATIC-CELL name)) 	 ; static binding
(ASSIGN (REGISTER n)				 ; type & range check
	(PRED-2-ARGS SMALL-FIXNUM?
		     (REGISTER m)
		     (MACHINE-CONSTANT nbits)))
(PRESERVE (REGISTER n) <how>)
(RESTORE (REGISTER m) <expression> <how>)

;; where how is one of SAVE, IF-AVAILABLE, and RECOMPUTE

|#
