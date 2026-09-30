;;; Driver.
;;;
;;; Times each benchmark in CPU milliseconds, not wall clock: wall charges
;;; us for whatever else the machine is doing.  Wall is reported alongside
;;; so a disturbed run is visible rather than silently believed.
;;;
;;; process-time-clock has 10ms granularity on Linux, so each benchmark is
;;; first calibrated to a repeat count that takes about a second, putting
;;; quantisation under 1%.  Calibrating per binary rather than fixing the
;;; counts means the same source serves the interpreted and compiled
;;; phases, which differ by two orders of magnitude, and the native and
;;; SVM flavours, which differ by one.  Results are therefore reported per
;;; iteration, which is comparable across binaries; the repeat count is
;;; printed too, so the work done is on the record.

(define target-ms 1000)
(define reps 3)

(define (time-it n thunk)               ; -> cpu-ms
  (let ((t0 (process-time-clock)))
    (repeat n thunk)
    (- (process-time-clock) t0)))

(define (calibrate thunk)               ; -> iterations for ~target-ms
  (let loop ((n 1))
    (let ((dt (time-it n thunk)))
      (cond ((>= dt (quotient target-ms 4))
             (max 1 (round (/ (* n target-ms) (max dt 1)))))
            ((> n 100000000) n)         ; give up rather than spin forever
            (else (loop (* n 4)))))))

(define (bench name thunk)
  (let ((n (calibrate thunk)))
    (let loop ((i 0) (best-cpu #f) (best-real #f))
      (if (= i reps)
          (begin (display name)
                 (display " ") (display (exact->inexact (/ best-cpu n)))
                 (display " ") (display (exact->inexact (/ best-real n)))
                 (display " ") (display n)
                 (newline))
          (let* ((p0 (process-time-clock))
                 (r0 (real-time-clock))
                 (ignore (repeat n thunk))
                 (dp (- (process-time-clock) p0))
                 (dr (- (real-time-clock) r0)))
            ignore
            (loop (+ i 1)
                  (if (or (not best-cpu) (< dp best-cpu)) dp best-cpu)
                  (if (or (not best-real) (< dr best-real)) dr best-real)))))))

(newline)
(bench "fib"     (lambda () (fib 27)))
(bench "tak"     (lambda () (tak 20 14 6)))
(bench "nqueens" (lambda () (nqueens 9)))
(bench "bignum"  (lambda () (bignum-work 9000)))
(bench "flonum"  (lambda () (flonum-work 1500000)))
(bench "string"  (lambda () (string-work 200000)))
(bench "list"    (lambda () (list-work 250000)))
(bench "vector"  (lambda () (vector-work 2000000)))
(bench "alloc"   (lambda () (alloc-work 2000000)))
