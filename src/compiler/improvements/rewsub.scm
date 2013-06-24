;;; This code should be incorporated in a separate pass.  It finds
;;; subproblems that contain combinations that have been rewritten as
;;; returns (e.g. constant folding), and rewrites them so that they
;;; reflect the new code.

;;; This is a partial solution which works provided that "fgopt/order"
;;; uses `new-subproblem-rvalue' instead of `subproblem-rvalue'.  A
;;; better solution is to rewrite the subproblems and replace them in
;;; the parallel, then update the application's operator/operand slots
;;; to reflect the new rvalues.  Then everything will be consistent.

(define (rewrite-parallel-subproblems parallel)
  (let ((application (parallel-application parallel))
	(subproblems (parallel-subproblems parallel)))
    (if (application/combination? application)
	(begin
	  (set-application-operator! application
				     (new-subproblem-rvalue (car subproblems)))
	  (set-application-operands!
	   application
	   (cons (car (application-operands application))
		 (map new-subproblem-rvalue (cdr subproblems))))))))

(define (new-subproblem-rvalue subproblem)
  (if (subproblem-simplified? subproblem)
      (return/operand
       (car (continuation/returns (subproblem-continuation subproblem))))
      (subproblem-rvalue subproblem)))

(define (subproblem-simplified? subproblem)
  (and (subproblem-canonical? subproblem)
       (let ((continuation (subproblem-continuation subproblem)))
	 (and (continuation/always-known-operator? continuation)
	      (let ((returns (continuation/returns continuation)))
		(and (not (null? returns))
		     (null? (cdr returns))
		     (return/continuation-push (car returns))))))))