; Trivial test to confirm closures work.

(declare (usual-integrations))

(let ((x (let ((y 5)) (identity (lambda () y)))))
  ((ucode-primitive exit-with-value 1)
   (fix:- (x) 5)))
