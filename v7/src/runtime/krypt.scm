;;; -*- Scheme -*-

(declare (usual-integrations))

(define-integrable ts 256)				; Actual table size to use

(define-structure (krypt-key (conc-name krypt-key/)
			     (constructor %make-krypt-key))
  state-table
  index-i
  index-j)

(define (make-krypt-key)
  (%make-krypt-key
    (make-vector ts)
    #f
    #f))

(define (rcm-keyinit key)
  (let loop ((i 0))
    (if (fix:< i ts)
	(begin
	  (vector-set! (krypt-key/state-table key) i i)
	  (loop (fix:1+ i)))
	(begin
	  (set-krypt-key/index-i! key 0)
	  (set-krypt-key/index-j! key 0)))))

(define (rcm-key key kbuf)
  (let ((m (string-length kbuf)))
    (let loop ((i 0)
	       (j 0)
	       (k 0))
      (if (fix:< i ts)
	  (begin
	    (let ((s (krypt-key/state-table key)))
	      (let* ((j (fix:remainder (fix:+ (fix:+ j 1)
					      (fix:+ (vector-ref s i)
						     (vector-8b-ref kbuf k)))
				       ts))
		     (t (vector-ref s i)))
		(vector-set! s i (vector-ref s j))
		(vector-set! s j t)
		(loop (fix:1+ i) j (fix:remainder (fix:1+ k) m)))))))))

#|
(define-integrable (inc-mod i ts)
  (if (fix:< i ts) 
      i
      (fix:- i ts)))
|#
(define-integrable (inc-mod i ts)
  (fix:remainder i ts))

(define-integrable (rcm key n buf)
  (rcm-iter key n buf 0))

(define (rcm-iter key n buf start-index)
  (let ((i (krypt-key/index-i key))
	(j (krypt-key/index-j key))
	(s (krypt-key/state-table key))
	(end-index (fix:+ n start-index)))
    (let loop ((k start-index)
	       (i i)
	       (j j))
      (if (fix:< k end-index)
	  (begin
	    (let* ((i (inc-mod (fix:1+ i) ts))
		   (j (inc-mod (fix:+ j (vector-ref s i)) ts))
		   (t (vector-ref s i)))
	      (vector-set! s i (vector-ref s j))
	      (vector-set! s j t)
	      (vector-8b-set! buf k
			      (fix:xor (vector-8b-ref buf k)
				       (vector-ref s (inc-mod
						      (fix:+ (fix:1+ (vector-ref s i))
							     (vector-ref s j)) 
						      ts))))
	      (loop (fix:1+ k) i j)))
	  (begin
	    (set-krypt-key/index-i! key i)
	    (set-krypt-key/index-j! key j))))))

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
     (string-pad-left (write-to-string (decoded-time/day the-time)) 2)
     " "
     (string-pad-left (write-to-string (decoded-time/hour the-time)) 2)
     ":"
     (string-pad-left (write-to-string (decoded-time/minute the-time)) 2 #\0)
     ":"
     (string-pad-left (write-to-string (decoded-time/second the-time)) 2 #\0)
     " "
     (write-to-string (decoded-time/year the-time)))))
#|
(define (get-krypt-time-string)
  "Mon Mar 30 17:21:44 1992")
|#

(define (update-checksum cs block index length)
  (let ((end-index (fix:+ index length)))
    (let loop ((i index)
	       (checksum cs))
      (if (fix:< i end-index)
	  (loop (fix:1+ i) (fix:+ checksum (vector-8b-ref block i)))
	  (fix:remainder checksum 256)))))

(define (encrypt input-string password)
  (let* ((checksum 0)
	 (header (string-append kryptid (get-krypt-time-string) "\n"))
	 (hlen (string-length header))
	 (output-string (make-string (fix:+ 6 (fix:+ hlen (string-length input-string)))))
	 (end-index (fix:- (string-length output-string) ts)))
    (let ((key1 (make-krypt-key)))
      (rcm-keyinit key1)
      (rcm-key key1 header)
      (rcm-key key1 password)
      (let ((passwordmac (make-string 5 #\NUL)))
	(rcm key1 5 passwordmac)
	(substring-move-left! header 0 hlen output-string 0)
	(substring-move-left! passwordmac 0 5 output-string hlen)
	(substring-move-left! input-string 0 (string-length input-string) output-string (fix:+ hlen 5)))
      (let loop ((index (fix:+ hlen 5)))
	(if (fix:< index end-index)
	    (begin
	      (set! checksum (update-checksum checksum output-string index ts))
	      (rcm-iter key1 ts output-string index)
	      (loop (fix:+ index ts)))
	    (let ((count (fix:- (string-length output-string) (fix:1+ index))))
	      (set! checksum (update-checksum checksum output-string index count))
	      (rcm-iter key1 count output-string index))))
      (let ((check-char (ascii->char (modulo (- checksum) ts))))
	(let ((cc-string (char->string check-char)))
	  (rcm key1 1 cc-string)
	  (string-set! output-string (fix:-1+ (string-length output-string)) (string-ref cc-string 0))))
      output-string)))

(define (decrypt input-string password #!optional password-error checksum-error)
  (let* ((header-length (+ (string-length kryptid) 25))
	 (header (string-head input-string header-length))
	 (pwordmac (substring input-string header-length (fix:+ header-length 5)))
	 (output-string (string-tail input-string (fix:+ header-length 5)))
	 (end-index (fix:- (string-length output-string) ts))
	 (key1 (make-krypt-key))
	 (checksum 0))
      (rcm-keyinit key1)
      (rcm-key key1 header)
      (rcm-key key1 password)
      (let ((passwordmac (make-string 5 #\NUL)))
	(rcm key1 5 passwordmac)
	(if (string=? passwordmac pwordmac)
	    (begin
	      (let loop ((index 0))
		(if (fix:< index end-index)
		    (begin
		      (rcm-iter key1 ts output-string index)
		      (set! checksum (update-checksum checksum output-string index ts))
		      (loop (fix:+ index ts)))
		    (let ((count (fix:- (string-length output-string) index)))
		      (rcm-iter key1 count output-string index)
		      (set! checksum (update-checksum checksum output-string index count)))))
	      (if (not (= (modulo checksum 256) 0))
		  (if (default-object? checksum-error)
		      (error "krypt: Checksum error.")
		      (checksum-error output-string))
		  (begin
		    (set-string-length! output-string (fix:-1+ (string-length output-string)))
		    output-string)))
	    (if (default-object? password-error)
		(error "krypt: Password error.")
		(password-error))))))

	    
	      
	      
	      
	      
