; Test for dynamic links.
;
; The LOOP procedure doesn't statically know whether it has to pop L
; off the stack, or only L1, when it returns, so it uses a dynamic
; link, a pointer to the caller's stack frame.

(declare (usual-integrations))

(let ()
  (declare (no-type-checks))
  (define (list-copy l)
    (let loop ((l1 l))
      (if (pair? l1)
	  (cons (car l1) (loop (cdr l1)))
	  '())))
  ((ucode-primitive exit-with-value 1)
   (fix:- (car ((identity list-copy) '(42 123 456 789)))
	  42)))
