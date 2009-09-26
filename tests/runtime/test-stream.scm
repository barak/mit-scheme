(define (file-stream filename)
  (let ((port (open-input-file filename)))
    (let loop ()
      (let ((char (read-char port)))
	(if (eof-object? char)
	    (begin
	      (close-port port)
	      '())
	    (cons-stream char (loop)))))))

(define (run-tests filename)
  (toggle-gc-notification!)
  (stream-length (file-stream filename))
  (stream-length (stream-map (lambda (x) x) (file-stream filename)))
  (stream-for-each (lambda (x) x) (file-stream filename))
  (toggle-gc-notification!))

(run-tests "~/gunk/receiver/chip1/receiver/qrm-1.sim")