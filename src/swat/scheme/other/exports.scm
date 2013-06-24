;; Make export list

(define (make-export-list candidates)
  ;; Call this on the names in the SWAT load environmnet to discover the
  ;; useful names.
  (define xlib-names (map second (read-file "xlibCONSTANTS.scm")))

  (define (xlib-name? n) (memq n xlib-names))
  (define (internal-name? n)
    (let ((s (symbol-name n)))
      (or (string-find-next-char s #\%))))

  (sort (list-transform-negative candidates
	  (lambda (name)
	    (or (internal-name? name)
		(xlib-name? name))))
	symbol<?))
