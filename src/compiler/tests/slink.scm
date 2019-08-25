; Test for static links.
;
; The LOOP procedure doesn't know how far up the stack F will be, so
; the generated code passes an extra stack environment pointer
; `argument' on the stack.  The extra (let ((x ...)) x) frame obviates
; the need for a dynamic link, because LOOP never has to pop MAP's
; arguments off the stack -- only its own argument.

(declare (usual-integrations))

(let ()
  (define (map f l)
    (let ((x
	   (let loop ((l1 l))
	     (if (pair? l1)
		 (cons (f (car l1)) (loop (cdr l1)))
		 '()))))
      x))
  (let ((l ((identity map) (lambda (x) (fix:- 0 x)) '(1 2 3))))
    ((ucode-primitive exit-with-value 1)
     (fix:or (fix:+ (car l) 1)
	     (fix:or (fix:+ (cadr l) 2)
		     (fix:+ (caddr l) 3))))))
