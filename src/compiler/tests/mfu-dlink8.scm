; Test for INVOCATION-PREFIX:MOVE-FRAME-UP with dynamic link and large frame.
;
; The LOOP procedure doesn't statically know whether it must pop off
; the arguments to COPY or only the argument to LOOP when it performs
; tail calls to IF-PROPER or IF-IMPROPER.  When it calls IF-IMPROPER,
; it creates a large frame and then moves it up to where the dynamic
; link specified the parent started.

(declare (usual-integrations))

(let ()
  (define (copy list if-proper if-improper)
    (let loop ((list list))
      (cond ((pair? list) (cons (car list) (loop (cdr list))))
	    ((null? list) (if-proper))
	    (else (if-improper list list list list list list list list)))))
  ((identity copy)
   (identity '(1 2 3 . 0))
   (lambda () ((ucode-primitive exit-with-value 1) 0))
   (lambda (a b c d e f g h)
     ((ucode-primitive exit-with-value 1)
      (fix:or (fix:or (fix:or a b) (fix:or c d))
	      (fix:or (fix:or e f) (fix:or g h)))))))
