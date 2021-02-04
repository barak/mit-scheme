; Test of uuo links with closure values.

(declare (usual-integrations))

(prelude)

(define (f x)
  (lambda (y)
    (fix:* x y)))

(define g
  (f 5))

(define (h x)
  (g (car (cdr x))))

((ucode-primitive exit-with-value 1)
 (fix:- (h (cons 1 (cons 2 '())))
	10))
