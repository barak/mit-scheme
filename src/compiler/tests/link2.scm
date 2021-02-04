(declare (usual-integrations))

(prelude)

(define x 0)

(define (f) x)

((ucode-primitive exit-with-value 1)
 (f))
