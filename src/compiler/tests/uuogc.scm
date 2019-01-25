; Test of uuo links, before/after garbage collection.

(declare (usual-integrations))

(prelude)

(define (f x)
  (fix:* x 2))

(define (g x)
  (f (car (cdr x))))

(let ((x (f 2)))
  ((ucode-primitive garbage-collect 1) #x1000)
  ((ucode-primitive exit-with-value 1)
   (fix:- (g (cons 1 (cons 2 '())))
	  x)))
