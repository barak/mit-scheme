;;; This alternative version of the assignment generation code (for
;;; "fgopt/reuse") attempts to pop things off the stack as soon as
;;; possible.

(define (generate-assignments nodes rest)
  (define (make-assignments nodes pushed registers)
    (if (null? nodes)
	(begin
	  (if (not (and (null? pushed) (null? registers)))
	      (error "unprocessed pending assignments" pushed registers))
	  rest)
	(let ((last-dependent (find-last-dependent (car nodes) (cdr nodes))))
	  (if last-dependent
	      (let ((entry (cons last-dependent (car nodes)))
		    (continue
		     (lambda (continuation-type pushed registers)
		       (linearize-subproblem!
			continuation-type
			(node-value (car nodes))
			(deallocate-registers nodes pushed registers)))))
		(if (nodes-simple? (cdr nodes))
		    (continue continuation-type/register
			      pushed
			      (cons entry registers))
		    (continue continuation-type/push
			      (cons entry pushed)
			      registers)))
	      (trivial-assignment
	       (car nodes)
	       (deallocate-registers nodes pushed registers))))))

  (define (deallocate-registers nodes pushed registers)
    (with-values
	(lambda ()
	  (discriminate-items registers
	    (lambda (register)
	      (eq? (car register) (car nodes)))))
      (lambda (deallocated-registers allocated-registers)
	(let loop ((registers registers))
	  (if (null? registers)
	      (deallocate-pushed nodes pushed allocated-registers)
	      (let ((node (cdar registers)))
		(overwrite node
			   (subproblem-continuation (node-value node))
			   (loop (cdr registers)))))))))

  (define (deallocate-pushed nodes pushed registers)
    (let loop ((pushed pushed))
      (let ((continue
	     (lambda () (make-assignments (cdr nodes) pushed registers))))
	(cond ((null? pushed)
	       (continue))
	      ((not (car pushed))
	       (let skip-empty ((pushed (cdr pushed)) (offset 1))
		 (if (or (null? pushed)
			 (car pushed))
		     (scfg*node->node! (make-stack-adjustment offset)
				       (loop pushed))
		     (skip-empty (cdr pushed) (1+ offset)))))
	      ((eq? (car nodes) (caar pushed))
	       (overwrite (cdar pushed) 0 (loop (cdr pushed))))
	      (else
	       (let loop ((pushed* (cdr pushed)) (index 1))
		 (if (null? pushed*)
		     (continue)
		     (let ((rest (lambda () (loop (cdr pushed*) (1+ index)))))
		       (if (and (car pushed*) (eq? (car nodes) (caar pushed*)))
			   (let ((node (cdar pushed*)))
			     (set-car! pushed* false)
			     (overwrite node index (rest)))
			   (rest))))))))))

  (make-assignments nodes '() '()))

(define (find-last-dependent node nodes)
  (let ((target (node-target node)))
    (let loop ((nodes nodes) (dependent false))
      (if (null? nodes)
	  dependent
	  (loop (cdr nodes)
		(let ((node (car nodes)))
		  (if (memq target (node-original-dependencies node))
		      node
		      dependent)))))))

(define (nodes-simple? nodes)
  (for-all? (cdr nodes)
    (lambda (node) (subproblem-simple? (node-value node)))))

(define (trivial-assignment node rest)
  (if (node/noop? node)
      rest
      (let ((subproblem (node-value node)))
	(linearize-subproblem! continuation-type/register
			       subproblem
			       (overwrite node
					  (subproblem-continuation subproblem)
					  rest)))))

(define (overwrite node source rest)
  (scfg*node->node!
   (make-stack-overwrite (subproblem-context (node-value node))
			 (node-target node)
			 source)
   rest))

;;; base/ctypes

(define-snode stack-adjustment
  offset)

(define (make-stack-adjustment offset)
  (snode->scfg (make-snode stack-adjustment-tag offset)))

(define-integrable (node/stack-adjustment? node)
  (eq? (tagged-vector/tag node) stack-adjustment-tag))

(define-snode stack-overwrite
  context
  target
  source)

(define (make-stack-overwrite block target source)
  (snode->scfg (make-snode stack-overwrite-tag block target source)))

(define-integrable (node/stack-overwrite? node)
  (eq? (tagged-vector/tag node) stack-overwrite-tag))

;;; base/subprb

(define (continuation*? object)
  (or (virtual-continuation? object)
      (continuation? object)))

;;; rtlgen/rgstmt

(define (generate/stack-overwrite stack-overwrite)
  (let ((target
	 (stack-overwrite-locative (stack-overwrite-context stack-overwrite)
				   (stack-overwrite-target stack-overwrite)))
	(source (stack-overwrite-source stack-overwrite)))
    (cond ((continuation*? source)
	   (rtl:make-assignment
	    target
	    (rtl:make-fetch (continuation*/register continuation))))
	  ((exact-nonnegative-integer? source)
	   (if (zero? source)
	       (rtl:make-pop target)
	       (rtl:make-assignment
		target
		(rtl:make-fetch
		 (stack-locative-offset (rtl:make-fetch stack-pointer)
					source)))))
	  (else
	   (error "Illegal stack-overwrite source" source)))))

(define (generate/stack-adjustment stack-adjustment)
  (rtl:make-assignment
   register:stack-pointer
   (rtl:make-address
    (stack-locative-offset (rtl:make-fetch stack-pointer) offset))))