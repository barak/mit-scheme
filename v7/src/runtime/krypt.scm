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
    (if (< i ts)
	(begin
	  (vector-set! (key/state-table key) i i)
	  (loop (1+ i)))
	(begin
	  (set-key/index-i! key 0)
	  (set-key/index-j! key 0)))))

(define (rcm-key key kbuf)
  (let ((m (string-length kbuf)))
    (let loop ((i 0)
	       (j 0)
	       (k 0))
      (if (< i ts)
	  (begin
	    (let ((s (key/state-table key)))
	      (let* ((j (modulo (+ j 1 
				   (vector-ref s i)
				   (char->ascii (string-ref kbuf k))) ts))
		     (t (vector-ref s i)))
		(vector-set! s i (vector-ref s j))
		(vector-set! s j t)
		(loop (1+ i) j (modulo (1+ k) m)))))))))

(define (rcm key n buf)
  (let ((i (key/index-i key))
	(j (key/index-j key)))
    (let ((s (key/state-table key)))
      (let loop ((k 0)
		 (i i)
		 (j j))
	(if (< k n)
	    (begin
	      (let* ((i (modulo (1+ i) ts))
		     (j (modulo (+ j (vector-ref s i)) ts))
		     (t (vector-ref s i)))
		(vector-set! s i (vector-ref s j))
		(vector-set! s j t)
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
  "Thu Mar 19 19:13:45 1992")
|#
(define (encrypt input-string password)
  (let ((checksum 0)
	(output-string "")
	(header (string-append kryptid (get-krypt-time-string) "\n")))
    (set! output-string (string-append output-string header))
    (let ((key1 (make-key)))
      (rcm-keyinit key1)
      (rcm-key key1 header)
      (rcm-key key1 password)
      (let ((passwordmac (list->string (map ascii->char '(0 0 0 0 0)))))
	(rcm key1 5 passwordmac)
	(set! output-string (string-append output-string passwordmac)))
      (let loop ((rest input-string))
	(if (>= (string-length rest) 256)
	    (let ((current-block (string-head rest 256))
		  (new-rest (string-tail rest 256)))
	      (set! checksum (+ checksum 
				(apply + (map char->ascii (string->list current-block)))))
	      (rcm key1 (string-length current-block) current-block)
	      (set! output-string (string-append output-string current-block))
	      (loop new-rest))
	    (begin
	      (set! checksum (+ checksum 
				(apply + (map char->ascii (string->list rest)))))
	      (rcm key1 (string-length rest) rest)
	      (set! output-string (string-append output-string rest)))))
      (let ((check-char (ascii->char (modulo (- checksum) 256))))
	(let ((cc-string (char->string check-char)))
	  (rcm key1 1 cc-string)
	  (set! output-string (string-append output-string cc-string))))
      output-string)))

