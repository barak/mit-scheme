; Test of uuo links with closure values before/after GC.

(declare (usual-integrations))

(prelude)

(define (f x)
  (lambda (y)
    (fix:* x y)))

(define g
  (f 5))

(define (h x)
  (g (car (cdr x))))

(let ((x ((f 3) 5)))
  ((ucode-primitive garbage-collect 1) #x1000)
  ((ucode-primitive exit-with-value 1)
   (fix:- (h (cons 1 (cons 3 '())))
	  x)))
