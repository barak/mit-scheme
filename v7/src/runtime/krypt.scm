;;; -*- Scheme -*-

(define TS 256)				; Actual table size to use

(define-structure (key (conc-name key/)
		       (constructor %make-key))
  state-table
  index-i
  index-j)

(define (make-key)
  (%make-key
    (make-vector ts)
    #f
    #f))

(define (rcm-keyinit key)
  (let loop ((i 0))
    (if (fix:< i ts)
	(begin
	  (vector-set! (key/state-table key) i i)
	  (loop (fix:1+ i)))
	(begin
	  (set-key/index-i! key 0)
	  (set-key/index-j! key 0)))))

(define (rcm-key key kbuf)
  (let ((m (string-length kbuf)))
    (let loop ((i 0)
	       (j 0)
	       (k 0))
      (if (fix:< i ts)
	  (begin
	    (let ((s (key/state-table key)))
	      (let* ((j (fix:remainder (fix:+ (fix:+ j 1)
					      (fix:+ (vector-ref s i)
						     (char->ascii (string-ref kbuf k)))) ts))
		     (t (vector-ref s i)))
		(vector-set! s i (vector-ref s j))
		(vector-set! s j t)
		(loop (fix:1+ i) j (fix:remainder (fix:1+ k) m)))))))))

(define (rcm key n buf)
  (let ((i (key/index-i key))
	(j (key/index-j key)))
    (let ((s (key/state-table key)))
      (let loop ((k 0)
		 (i i)
		 (j j))
	(if (fix:< k n)
	    (begin
	      (let* ((i (fix:remainder (fix:1+ i) ts))
		     (j (fix:remainder (fix:+ j (vector-ref s i)) ts))
		     (t (vector-ref s i)))
		(vector-set! s i (vector-ref s j))
		(vector-set! s j t)
		(string-set! buf k 
			     (ascii->char 
			      (fix:xor (char->ascii (string-ref buf k))
				       (vector-ref s (fix:remainder 
						      (fix:+ (fix:1+ (vector-ref s i))
							     (vector-ref s j)) ts)))))
#|
		(let ((buf-k-bitstr
		       (unsigned-integer->bit-string 
			8 (char->ascii (string-ref buf k))))
		      (xor-string
		       (unsigned-integer->bit-string
			8 (vector-ref s (modulo (+ 1 (vector-ref s i) 
						   (vector-ref s j)) ts)))))
		  (string-set! buf k (ascii->char 
				      (bit-string->unsigned-integer
				       (bit-string-xor buf-k-bitstr xor-string)))))
|#
		(loop (1+ k) i j)))
	    (begin
	      (set-key/index-i! key i)
	      (set-key/index-j! key j)))))))

(define kryptid "This file krypted ")

(define (get-krypt-time-string)
  (let ((the-time (get-decoded-time)))
    (string-append
     (vector-ref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
		 (decoded-time/day-of-week the-time))
     " "
     (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
			  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
	       (-1+ (decoded-time/month the-time)))
     " "
     (write-to-string (decoded-time/day the-time))
     " "
     (write-to-string (decoded-time/hour the-time))
     ":"
     (write-to-string (decoded-time/minute the-time))
     ":"
     (write-to-string (decoded-time/second the-time))
     " "
     (write-to-string (decoded-time/year the-time)))))
#|
(define (get-krypt-time-string)
  "Mon Mar 30 17:21:44 1992")
|#

(define (update-checksum cs block)
  (let ((block-length (string-length block)))
    (let loop ((i 0)
	       (checksum cs))
      (if (< i block-length)
	  (loop (fix:1+ i) (fix:+ checksum (char->ascii (string-ref block i))))
	  (fix:remainder checksum 256)))))

(define (encrypt input-string password)
  (let ((checksum 0)
	(output-string "")
	(header (string-append kryptid (get-krypt-time-string) "\n")))
    (let ((key1 (make-key)))
      (rcm-keyinit key1)
      (rcm-key key1 header)
      (rcm-key key1 password)
      (let ((passwordmac (make-string 5 #\NUL)))
	(rcm key1 5 passwordmac)
	(set! output-string (string-append header passwordmac)))
      (let loop ((rest input-string)
		 (output-string-list '()))
	(if (>= (string-length rest) 256)
	    (let ((current-block (string-head rest 256))
		  (new-rest (string-tail rest 256)))
;	      (set! checksum (+ checksum (reduce + 0 (map char->ascii (string->list current-block)))))
	      (set! checksum (update-checksum checksum current-block))
	      (rcm key1 (string-length current-block) current-block)
	      (loop new-rest (cons current-block output-string-list)))
	    (begin
;	      (set! checksum (+ checksum (reduce + 0 (map char->ascii (string->list rest)))))
	      (set! checksum (update-checksum checksum rest))
	      (rcm key1 (string-length rest) rest)
	      (set! output-string 
		    (apply string-append (cons output-string (reverse (cons rest output-string-list))))))))
      (let ((check-char (ascii->char (modulo (- checksum) 256))))
	(let ((cc-string (char->string check-char)))
	  (rcm key1 1 cc-string)
	  (set! output-string (string-append output-string cc-string))))
      output-string)))

(define (decrypt input-string password #!optional password-error checksum-error)
  (let* ((header-length (+ (string-length kryptid) 25))
	 (header (string-head input-string header-length))
	 (text1 (string-tail input-string header-length))
	 (pwordmac (string-head text1 5))
	 (encrypted-text (string-tail text1 5)))
    (let ((key1 (make-key))
	  (checksum 0)
	  (output-string ""))
      (rcm-keyinit key1)
      (rcm-key key1 header)
      (rcm-key key1 password)
      (let ((passwordmac (make-string 5 #\NUL)))
	(rcm key1 5 passwordmac)
	(if (string=? passwordmac pwordmac)
	    (begin
	      (let loop ((rest encrypted-text)
			 (output-string-list '()))
		(if (>= (string-length rest) 256)
		    (let ((current-block (string-head rest 256))
			  (new-rest (string-tail rest 256)))
		      (rcm key1 (string-length current-block) current-block)
;		      (set! checksum (+ checksum (reduce + 0 (map char->ascii (string->list current-block)))))
		      (set! checksum (update-checksum checksum current-block))
		      (loop new-rest (cons current-block output-string-list)))
		    (begin
		      (rcm key1 (string-length rest) rest)
;		      (set! checksum (+ checksum (reduce + 0 (map char->ascii (string->list rest)))))
		      (set! checksum (update-checksum checksum rest))
		      (let ((foo (apply string-append (reverse (cons rest output-string-list)))))
			(set! output-string (string-head foo (-1+ (string-length foo))))))))
	      (if (not (= (modulo checksum 256) 0))
		  (if (default-object? checksum-error)
		      (error "krypt: Checksum error.")
		      (checksum-error output-string))
		  output-string))
	    (if (default-object? password-error)
		(error "krypt: Password error.")
		(password-error)))))))

	    
	      
	      
	      
	      
