; Test of creating a closure before GC and then using it after.

(declare (usual-integrations))

(let ((x (let ((y 5)) (identity (lambda () y)))))
  ((ucode-primitive garbage-collect 1) #x1000)
  ((ucode-primitive exit-with-value 1)
   (fix:- (x) 5)))
