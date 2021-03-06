(define (findex-names names #!optional port)
  (for-each (lambda (name)
              (write-string "@findex " port)
              (write name port)
              (newline port))
            names))

(define (max-column-size names)
  (fold (lambda (name acc)
	  (max (string-length (symbol->string name))
	       acc))
	0
	names))

(define (multitable n-columns names #!optional port)
  (let ((fraction
	 (parameterize ((param:flonum-printer-cutoff '(absolute 2)))
	   (number->string (inexact (/ 1 n-columns))))))
    (write-string "@multitable @columnfractions" port)
    (do ((i 0 (+ i 1)))
	((not (< i n-columns)))
      (write-string " " port)
      (write-string fraction port))
    (newline port)
    (multitable-rows n-columns names port)
    (write-string "@end multitable" port)
    (newline port)))

(define (multitable-rows n-columns names #!optional port)
  (for-each (lambda (group)
	      (write-string "@item @nicode{" port)
	      (write (car group) port)
	      (write-string "}" port)
	      (newline port)
	      (for-each (lambda (name)
			  (write-string "@tab @nicode{" port)
			  (write name port)
			  (write-string "}" port)
			  (newline port))
			(cdr group)))
	    (group-by-columns names n-columns)))

(define (group-by-columns names n-columns)
  (let loop ((n (length names)) (names names) (groups '()))
    (if (> n n-columns)
	(loop (- n n-columns)
	      (drop names n-columns)
	      (cons (take names n-columns) groups))
	(reverse
	 (if (> n 0)
	     (cons names groups)
	     groups)))))