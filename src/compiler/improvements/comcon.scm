;;; This alternative version of `combination/constant!' attempts to
;;; keep the data structures more consistent.  It doesn't seem to be
;;; needed yet.

(define (combination/constant! combination rvalue)
  (let ((continuation (combination/continuation combination)))
    (for-each (lambda (continuation)
		(set-continuation/combinations!
		 continuation
		 (delq! combination (continuation/combinations continuation)))
		(set-continuation/returns!
		 continuation
		 (cons combination (continuation/returns continuation))))
	      (rvalue-values continuation))
    (for-each (lambda (operator)
		(if (rvalue/procedure? operator)
		    (delete-procedure-application! operator combination)))
	      (rvalue-values (combination/operator combination)))
    (maybe-kill-application-procedure! combination)
    (set-application-type! combination 'RETURN)
    (set-application-operator! combination continuation)
    (set-application-operands! combination (list rvalue))
    (let ((push (combination/continuation-push combination)))
      (if (and push (rvalue-known-value continuation))
	  (set-virtual-continuation/type! (virtual-return-operator push)
					  continuation-type/effect)))))

(define (maybe-kill-application-procedure! application)
  (let ((operator (rvalue-known-value (application-operator application))))
    (if (and operator
	     (rvalue/procedure? operator)
	     (procedure-always-known-operator? operator)
	     (null? (procedure-applications operator)))
	(kill-procedure! operator))))

(define (kill-procedure! procedure)
  (set! *procedures* (delq! procedure *procedures*))
  (let ((block (procedure-block procedure)))
    (set! *blocks* (delq! block *blocks*))
    (let ((parent (block-parent block)))
      (set-block-children! parent (delq! block (block-children parent))))
    ;; This should probably be accomplished by a codewalk, but for
    ;; current purposes it's adequate.
    (for-each kill-application! (block-applications block))))

(define (kill-application! application)
  (set! *applications* (delq! application *applications*))
  (for-each (lambda (operator)
	      (if (rvalue/procedure? operator)
		  (delete-procedure-application! operator application)))
	    (rvalue-values (application-operator application)))
  (if (application/combination? application)
      (for-each (lambda (continuation)
		  (delete-continuation/combination! continuation application))
		(rvalue-values (combination/continuation application))))
  (maybe-kill-application-procedure! application))

(define (delete-procedure-application! procedure combination)
  (let ((applications (delq! combination (procedure-applications procedure))))
    (set-procedure-applications! procedure applications)
    (if (null? applications)
	(set-procedure-always-known-operator?! procedure false))))

(define (delete-continuation/combination! continuation combination)
  (let ((combinations
	 (delq! combination (continuation/combinations continuation))))
    (set-continuation/combinations! continuation combinations)
    (if (and (null? combinations)
	     (null? (continuation/returns continuation)))
	(set-procedure-always-known-operator?! continuation false))))