(define-library (foo foo)
  (import (scheme base))
  (export <foo>
	  foo?)
  (begin
    (define-record-type <foo>
	(make-foo)
	foo?)))