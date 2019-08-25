; Test for static and dynamic links.
;
; The LOOP procedure doesn't statically know whether it has to pop F
; and L off the stack, or only L1, when it returns, so it uses a
; dynamic link -- a pointer to the parent stack frame.  It also doesn't
; know where F was on the stack originally, so it passes around a
; static link to its own stack environment to get at F.

(declare (usual-integrations))

(let ()
  (define (map f l)
    (let loop ((l1 l))
      (if (pair? l1)
	  (cons (f (car l1)) (loop (cdr l1)))
	  '())))
  ((ucode-primitive exit-with-value 1)
   (fix:- (car ((identity map) (lambda (x) x) '(42 123 456 789)))
	  42)))
