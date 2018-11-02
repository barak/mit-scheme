(define-library (foo mumble)
  (import (scheme base))
  (export foo-mumble?
	  make-foo-mumble
	  foo-mumble-a
	  foo-mumble-b)
  (begin
    (define-record-type <foo-mumble>
	(make-foo-mumble a b)
	foo-mumble?
      (a foo-mumble-a)
      (b foo-mumble-b))))

(define-library (foo bletch)
  (import (scheme base))
  (export foo-bletch?
	  make-foo-bletch
	  foo-bletch-thing)
  (begin
    (define-record-type <foo-bletch>
	(make-foo-bletch thing)
	foo-bletch?
      (thing foo-bletch-thing))))

(define-library (foo grumble)
  (import (scheme base))
  (export foo-grumble?
	  make-foo-grumble
	  foo-grumble-a
	  foo-grumble-b)
  (begin
    (define-record-type <foo-grumble>
	(make-foo-grumble a b)
	foo-grumble?
      (a foo-grumble-a)
      (b foo-grumble-b))))

(define-library (foo quux)
  (import (scheme base))
  (export foo-quux?
	  make-foo-quux
	  foo-quux-a
	  foo-quux-b)
  (begin
    (define-record-type <foo-quux>
	(make-foo-quux a b)
	foo-quux?
      (a foo-quux-a)
      (b foo-quux-b))))