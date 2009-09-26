(define (test-encoder n-packets packet-length text? filename
		      initialize finalize update)
  (call-with-output-file filename
    (lambda (port)
      (let ((context (initialize port text?))
	    (n-packets (random n-packets)))
	(do ((i 0 (+ i 1)))
	    ((= i n-packets))
	  (let ((packet-length (random packet-length)))
	    (write i)
	    (write-char #\space)
	    (write packet-length)
	    (write-char #\space)
	    (let ((packet 
		   (if text?
		       (random-text-string packet-length)
		       (random-byte-vector packet-length))))
	      (write packet)
	      (newline)
	      (update context packet 0 packet-length))))
	(finalize context)))))

(define (random-text-string length)
  (let ((string (make-string length))
	(n-text (string-length text-characters)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i length))
      (string-set! string i (string-ref text-characters (random n-text))))
    string))

(define text-characters
  (list->string
   (append '(#\tab #\newline)
	   (char-set-members char-set:graphic))))

(define (test-codec n-packets packet-length text? filename
		      encode:initialize encode:finalize encode:update
		      decode:initialize decode:finalize decode:update)
  (let ((packets (make-test-vector n-packets packet-length text?)))
    (let ((n-packets (vector-length packets)))
      (call-with-output-file (pathname-new-type filename "clear1")
	(lambda (port)
	  (do ((i 0 (+ i 1)))
	      ((= i n-packets))
	    (write-string (vector-ref packets i) port))))
      (call-with-output-file (pathname-new-type filename "encoded")
	(lambda (port)
	  (let ((context (encode:initialize port text?)))
	    (do ((i 0 (+ i 1)))
		((= i n-packets))
	      (let ((packet (vector-ref packets i)))
		(encode:update context packet 0 (string-length packet))))
	    (encode:finalize context))))))
  (retest-decoder text? filename
		  decode:initialize decode:finalize decode:update))

(define (make-test-vector n-packets packet-length text?)
  (let ((n-packets (random n-packets)))
    (let ((packets (make-vector n-packets)))
      (do ((i 0 (+ i 1)))
	  ((= i n-packets))
	(vector-set! packets i
		     (let ((packet-length (random packet-length)))
		       (if text?
			   (random-text-string packet-length)
			   (random-byte-vector packet-length)))))
      packets)))

(define (retest-codec text? filename
		      encode:initialize encode:finalize encode:update
		      decode:initialize decode:finalize decode:update)
  (call-with-input-file (pathname-new-type filename "clear1")
    (lambda (input-port)
      (call-with-output-file (pathname-new-type filename "encoded")
	(lambda (output-port)
	  (let ((context (encode:initialize output-port text?))
		(buffer (make-string 37)))
	    (let loop ()
	      (let ((n-read (read-string! buffer input-port)))
		(if (fix:> n-read 0)
		    (begin
		      (encode:update context buffer 0 n-read)
		      (loop)))))
	    (encode:finalize context))))))
  (retest-decoder text? filename
		  decode:initialize decode:finalize decode:update))

(define (retest-decoder text? filename
			decode:initialize decode:finalize decode:update)
  (let ((pn3 (pathname-new-type filename "clear2")))
    (call-with-input-file (pathname-new-type filename "encoded")
      (lambda (input-port)
	(call-with-output-file pn3
	  (lambda (output-port)
	    (let ((context (decode:initialize output-port text?))
		  (buffer (make-string 41)))
	      (let loop ()
		(let ((n-read (read-string! buffer input-port)))
		  (if (fix:> n-read 0)
		      (begin
			(decode:update context buffer 0 n-read)
			(loop)))))
	      (decode:finalize context))))))
    (call-with-input-file (pathname-new-type filename "clear1")
      (lambda (p1)
	(call-with-input-file pn3
	  (lambda (p3)
	    (let loop ()
	      (let ((c1 (read-char p1))
		    (c3 (read-char p3)))
		(if (eof-object? c1)
		    (if (eof-object? c3)
			unspecific
			(error "Output file longer."))
		    (if (eof-object? c3)
			(error "Output file shorter.")
			(if (char=? c1 c3)
			    (loop)
			    (error "Files don't match."))))))))))))