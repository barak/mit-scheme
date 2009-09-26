(define (fill-file-with-random-integers n-bits-in-file
					n-bits-per-integer
					n-progress-dots
					filename)
  (if (not (= 0 (remainder n-bits-in-file n-bits-per-integer)))
      (error:bad-range-argument n-bits-in-file
				'FILL-FILE-WITH-RANDOM-INTEGERS))
  (if (not (= 0 (remainder n-bits-per-integer 8)))
      (error:bad-range-argument n-bits-per-integer
				'FILL-FILE-WITH-RANDOM-INTEGERS))
  (call-with-output-file filename
    (lambda (port)
      (let ((modulus (expt 2 n-bits-per-integer))
	    (j-limit (quotient n-bits-in-file n-bits-per-integer))
	    (i-limit (quotient n-bits-per-integer 8)))
	(let ((j-dot (quotient j-limit n-progress-dots))
	      (buffer (make-string i-limit)))
	  (do ((j 0 (+ j 1)))
	      ((= j j-limit))
	    (if (= 0 (remainder j j-dot))
		(write-char #\.))
	    (do ((i 0 (+ i 1))
		 (n (random modulus) (quotient n #x100)))
		((= i i-limit))
	      (vector-8b-set! buffer i (remainder n #x100)))
	    (write-string buffer port)))))))