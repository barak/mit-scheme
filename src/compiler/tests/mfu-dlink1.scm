; Test for INVOCATION-PREFIX:MOVE-FRAME-UP with dynamic link and small frame.
;
; The LOOP procedure doesn't statically know whether it must pop off
; the arguments to COPY or only the argument to LOOP when it performs
; tail calls to IF-PROPER or IF-IMPROPER.

(declare (usual-integrations))

(let ()
  (define (copy list if-proper if-improper)
    (let loop ((list list))
      (cond ((pair? list) (cons (car list) (loop (cdr list))))
	    ((null? list) (if-proper))
	    (else (if-improper list)))))
  ((identity copy)
   (identity '(1 2 3 . 0))
   (lambda () ((ucode-primitive exit-with-value 1) 1))
   (ucode-primitive exit-with-value 1)))
