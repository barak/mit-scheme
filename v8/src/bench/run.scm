(define (time-command thunk-maker)
  (define (say . stuff) (for-each display stuff))
  (let ((n 5))
    (let loop ((i n) (times '()) (value '?))
      (if (> i 0)
	  (let ((thunk (thunk-maker)))
	    (let ((start  (runtime)))	; process time - gc process time
	      (let ((value (thunk)))
		(let ((end (runtime)))
		  (loop (- i 1) (cons (- end start) times) value)))))
	  (fluid-let ((flonum-unparser-cutoff '(absolute 3)))
	    (let* ((sum   (reduce + 0 times))
		   (mean  (/ sum n))
		   (serr  (reduce + 0 (map (lambda (x) (abs (- mean x)))
					   times)))
		   (merr  (/ serr n)))
	      (say "\nTime:  "  mean  "  mean error "  merr)
	      (say "  ("  (round->exact (* 100 (/ merr (+ mean 1e-6))))  "%)")
	      (say "\nTimes: "  times)
	      value))))))

(define (make-env)
  (the-environment))

(define (benchmark-file file-name)
  (newline)
  (display "Benchmark:  ")
  (display file-name)
  (write-line (time-command (lambda ()
			      (fluid-let ((load/suppress-loading-message? #f))
				(let ((env  (make-env)))
				  (load "library" env)
				  (load file-name env))))))
  (newline))

(print-gc-statistics)

(for-each benchmark-file
	  '("boyer"
	    "browse"
	    "conform"
	    "cpstak"
	    "ctak"
	    "dderiv"
	    "deriv"
	    "destruct"
	    "div"
	    ;;"earley"
	    "fcomp"
	    "fib"
	    ;;"flatten"
	    "matmul1"
	    "matmul2"
	    "peval"
	    "puzzle"
	    "tak"
	    "takl"
	    "traverse"
	    "triangle"
	    "wttree"))
