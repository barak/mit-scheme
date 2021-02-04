; Test of uuo links.

(declare (usual-integrations))

(prelude)

(define (f x)
  (fix:* x 2))

(define (g x)
  (f (car (cdr x))))

((ucode-primitive exit-with-value 1)
 (fix:- (g (cons 1 (cons 2 '())))
	4))
