; Test for INVOCATION-PREFIX:MOVE-FRAME-UP with a known parent.
;
; The LOOP procedure knows how much it has to pop when making a tail
; call to itself; however, the stack frame it creates is so large that
; the compiler chooses not to reuse the parent frame but instead use
; INVOCATION-PREFIX:MOVE-FRAME-UP to replace it.

(declare (usual-integrations))

(let ()
  (define (fib n x y z f)
    (let loop
	((a 0) (b 1)
	 (x x) (y y) (z z) (p x) (q y) (r z) (u x) (v y) (w z)
	 (i 0))
      (if (< i n)
	  (let ((p (f y z p))
		(q (f z x q))
		(r (f x y r)))
	    (loop b (+ a b) u v w p q r x y z (+ i 1)))
	  (cons b (vector x y z)))))
  ((ucode-primitive exit-with-value 1)
   (fix:- (car ((identity fib) 20 0 1 2 (lambda (a b c) b c a)))
	  10946)))
