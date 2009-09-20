#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; String I/O Ports (SRFI-6)
;;; package: (runtime string-i/o-port)

(declare (usual-integrations))

;;;; Input as characters

(define (with-input-from-string string thunk)
  (with-input-from-port (open-input-string string) thunk))

(define (call-with-input-string string procedure)
  (procedure (open-input-string string)))

(define (open-input-string string #!optional start end)
  (cond ((string? string)
	 (receive (start end)
	     (check-index-limits start end (string-length string)
				 'OPEN-INPUT-STRING)
	   (make-port narrow-input-type
		      (make-internal-input-state string start end))))
	((wide-string? string)
	 (receive (start end)
	     (check-index-limits start end (wide-string-length string)
				 'OPEN-INPUT-STRING)
	   (make-port wide-input-type
		      (make-internal-input-state string start end))))
	((external-string? string)
	 (receive (start end)
	     (check-index-limits start end (xstring-length string)
				 'OPEN-INPUT-STRING)
	   (make-port external-input-type
		      (make-external-input-state string start end))))
	(else
	 (error:not-string string 'OPEN-INPUT-STRING))))

(define (check-index-limits start end limit caller)
  (let ((end
	 (if (or (default-object? end) (not end))
	     limit
	     (begin
	       (guarantee-exact-nonnegative-integer end caller)
	       (if (not (<= end limit))
		   (error:bad-range-argument end caller))
	       end))))
    (values (if (or (default-object? start) (not start))
		0
		(begin
		  (guarantee-exact-nonnegative-integer start caller)
		  (if (not (<= start end))
		      (error:bad-range-argument start caller))
		  start))
	    end)))

(define (make-string-in-type peek-char read-char unread-char)
  (make-port-type `((CHAR-READY? ,string-in/char-ready?)
		    (EOF? ,internal-in/eof?)
		    (PEEK-CHAR ,peek-char)
		    (READ-CHAR ,read-char)
		    (READ-SUBSTRING ,internal-in/read-substring)
		    (UNREAD-CHAR ,unread-char)
		    (WRITE-SELF ,string-in/write-self))
		  #f))

(define (make-internal-input-state string start end)
  (make-iistate string start end start))

(define-structure iistate
  (string #f read-only #t)
  (start #f read-only #t)
  (end #f read-only #t)
  next)

(define (string-in/char-ready? port)
  port
  #t)

(define (string-in/write-self port output-port)
  port
  (write-string " from string" output-port))

(define (internal-in/eof? port)
  (let ((ss (port/state port)))
    (not (fix:< (iistate-next ss) (iistate-end ss)))))

(define (internal-in/read-substring port string start end)
  (let ((ss (port/state port)))
    (let ((n
	   (move-chars! (iistate-string ss) (iistate-next ss) (iistate-end ss)
			string start end)))
      (set-iistate-next! ss (fix:+ (iistate-next ss) n))
      n)))

(define (make-narrow-input-type)
  (make-string-in-type narrow-in/peek-char
		       narrow-in/read-char
		       narrow-in/unread-char))

(define (narrow-in/peek-char port)
  (let ((ss (port/state port)))
    (if (fix:< (iistate-next ss) (iistate-end ss))
	(string-ref (iistate-string ss) (iistate-next ss))
	(make-eof-object port))))

(define (narrow-in/read-char port)
  (let ((ss (port/state port)))
    (if (fix:< (iistate-next ss) (iistate-end ss))
	(let ((char (string-ref (iistate-string ss) (iistate-next ss))))
	  (set-iistate-next! ss (fix:+ (iistate-next ss) 1))
	  char)
	(make-eof-object port))))

(define (narrow-in/unread-char port char)
  (let ((ss (port/state port)))
    (if (not (fix:< (iistate-start ss) (iistate-next ss)))
	(error "No char to unread:" port))
    (let ((prev (fix:- (iistate-next ss) 1)))
      (if (not (char=? char (string-ref (iistate-string ss) prev)))
	  (error "Unread char incorrect:" char))
      (set-iistate-next! ss prev))))

(define (make-wide-input-type)
  (make-string-in-type wide-in/peek-char
		       wide-in/read-char
		       wide-in/unread-char))

(define (wide-in/peek-char port)
  (let ((ss (port/state port)))
    (if (fix:< (iistate-next ss) (iistate-end ss))
	(wide-string-ref (iistate-string ss) (iistate-next ss))
	(make-eof-object port))))

(define (wide-in/read-char port)
  (let ((ss (port/state port)))
    (if (fix:< (iistate-next ss) (iistate-end ss))
	(let ((char (wide-string-ref (iistate-string ss) (iistate-next ss))))
	  (set-iistate-next! ss (fix:+ (iistate-next ss) 1))
	  char)
	(make-eof-object port))))

(define (wide-in/unread-char port char)
  (let ((ss (port/state port)))
    (if (not (fix:< (iistate-start ss) (iistate-next ss)))
	(error "No char to unread:" port))
    (let ((prev (fix:- (iistate-next ss) 1)))
      (if (not (char=? char (wide-string-ref (iistate-string ss) prev)))
	  (error "Unread char incorrect:" char))
      (set-iistate-next! ss prev))))

(define (make-external-input-type)
  (make-port-type
   `((CHAR-READY? ,string-in/char-ready?)
     (EOF? ,external-in/eof?)
     (PEEK-CHAR ,external-in/peek-char)
     (READ-CHAR ,external-in/read-char)
     (READ-SUBSTRING ,external-in/read-substring)
     (UNREAD-CHAR ,external-in/unread-char)
     (WRITE-SELF ,string-in/write-self))
   #f))

(define (make-external-input-state string start end)
  (make-xistate (external-string-source string start end) #f #f))

(define-structure xistate
  (source #f read-only #t)
  unread)

(define (external-in/eof? port)
  (let ((xs (port/state port)))
    (and (not (xistate-unread xs))
	 (not ((xistate-source xs))))))

(define (external-in/peek-char port)
  (let ((xs (port/state port)))
    (or (xistate-unread xs)
	(let ((char ((xistate-source xs))))
	  (set-xistate-unread! xs char)
	  char))))

(define (external-in/read-char port)
  (let ((xs (port/state port)))
    (let ((unread (xistate-unread xs)))
      (if unread
	  (begin
	    (set-xistate-unread! xs #f)
	    unread)
	  ((xistate-source xs))))))

(define (external-in/unread-char port char)
  (let ((xs (port/state port)))
    (if (xistate-unread xs)
	(error "Can't unread two chars."))
    (set-xistate-unread! xs char)))

(define (external-in/read-substring port string start end)
  (source->sink! (xistate-source (port/state port))
		 (string-sink string start end)))

(define (move-chars! string start end string* start* end*)
  (let ((n (min (- end start) (- end* start*))))
    (let ((end (+ start n))
	  (end* (+ start* n)))
      (cond ((wide-string? string)
	     (source->sink! (wide-string-source string start end)
			    (string-sink string* start* end*)))
	    ((wide-string? string*)
	     (source->sink! (string-source string start end)
			    (wide-string-sink string* start* end*)))
	    (else
	     (xsubstring-move! string start end string* start*)
	     n)))))

(define (source->sink! source sink)
  (let loop ((n 0))
    (if (sink (source))
	(loop (+ n 1))
	n)))

(define (string-source string start end)
  (cond ((string? string) (narrow-string-source string start end))
	((wide-string? string) (wide-string-source string start end))
	((external-string? string) (external-string-source string start end))
	(else (error:not-string string #f))))

(define (string-sink string start end)
  (cond ((string? string) (narrow-string-sink string start end))
	((wide-string? string) (wide-string-sink string start end))
	((external-string? string) (external-string-sink string start end))
	(else (error:not-string string #f))))

(define (narrow-string-source string start end)
  (lambda ()
    (and (fix:< start end)
	 (let ((char (string-ref string start)))
	   (set! start (fix:+ start 1))
	   char))))

(define (narrow-string-sink string start end)
  (lambda (char)
    (and char
	 (begin
	   (if (not (fix:< (char->integer char) #x100))
	       (error:not-8-bit-char char))
	   (and (fix:< start end)
		(begin
		  (string-set! string start char)
		  (set! start (+ start 1))
		  #t))))))

(define (wide-string-source string start end)
  (lambda ()
    (and (fix:< start end)
	 (let ((char (wide-string-ref string start)))
	   (set! start (fix:+ start 1))
	   char))))

(define (wide-string-sink string start end)
  (lambda (char)
    (and char
	 (fix:< start end)
	 (begin
	   (wide-string-set! string start char)
	   (set! start (+ start 1))
	   #t))))

(define (external-string-source string start end)
  (let ((buffer (make-string #x1000))
	(bi #x1000)
	(next start))
    (lambda ()
      (and (< next end)
	   (begin
	     (if (fix:>= bi #x1000)
		 (begin
		   (xsubstring-move! string next (min (+ next #x1000) end)
				     buffer 0)
		   (set! bi 0)))
	     (let ((char (string-ref buffer bi)))
	       (set! bi (fix:+ bi 1))
	       (set! next (+ next 1))
	       char))))))

(define (external-string-sink string start end)
  (let ((buffer (make-string #x1000))
	(bi 0))
    (lambda (char)
      (if char
	  (begin
	    (if (not (fix:< (char->integer char) #x100))
		(error:not-8-bit-char char))
	    (and (< start end)
		 (begin
		   (string-set! buffer bi char)
		   (set! bi (fix:+ bi 1))
		   (set! start (+ start 1))
		   (if (fix:= bi #x1000)
		       (begin
			 (xsubstring-move! buffer 0 bi string (- start bi))
			 (set! bi 0)))
		   #t)))
	  (begin
	    (xsubstring-move! buffer 0 bi string (- start bi))
	    (set! bi 0)
	    #f)))))

;;;; Input as byte vector

(define (call-with-input-octets octets procedure)
  (procedure (open-input-octets octets)))

(define (open-input-octets octets #!optional start end)
  (guarantee-xstring octets 'OPEN-INPUT-OCTETS)
  (receive (start end)
      (check-index-limits start end (xstring-length octets) 'OPEN-INPUT-OCTETS)
    (let ((port
	   (make-generic-i/o-port (make-octets-source octets start end)
				  #f
				  octets-input-type)))
      (port/set-coding port 'ISO-8859-1)
      (port/set-line-ending port 'NEWLINE)
      port)))

(define (make-octets-source string start end)
  (let ((index start))
    (make-non-channel-port-source
     (lambda ()
       (< index end))
     (lambda (string* start* end*)
       (let ((n (min (- end index) (- end* start*))))
	 (let ((limit (+ index n)))
	   (xsubstring-move! string index limit string* start*)
	   (set! index limit))
	 n)))))

(define (make-octets-input-type)
  (make-port-type `((WRITE-SELF
		     ,(lambda (port output-port)
			port
			(write-string " from byte vector" output-port))))
		  (generic-i/o-port-type #t #f)))

;;;; Output as characters

(define (open-narrow-output-string)
  (make-port narrow-output-type (make-ostate (make-string 16) 0 0)))

(define (open-wide-output-string)
  (make-port wide-output-type (make-ostate (make-wide-string 16) 0 0)))

(define (get-output-string port)
  ((port/operation port 'EXTRACT-OUTPUT) port))

(define (get-output-string! port)
  ((port/operation port 'EXTRACT-OUTPUT!) port))

(define (call-with-narrow-output-string generator)
  (let ((port (open-narrow-output-string)))
    (generator port)
    (get-output-string port)))

(define (call-with-wide-output-string generator)
  (let ((port (open-wide-output-string)))
    (generator port)
    (get-output-string port)))

(define (call-with-truncated-output-string limit generator)
  (let ((port (open-narrow-output-string)))
    (let ((truncated? (call-with-truncated-output-port limit port generator)))
      (cons truncated? (get-output-string port)))))

(define (with-output-to-string thunk)
  (call-with-narrow-output-string
    (lambda (port)
      (with-output-to-port port thunk))))

(define (with-output-to-truncated-string limit thunk)
  (call-with-truncated-output-string limit
    (lambda (port)
      (with-output-to-port port thunk))))

(define (make-narrow-output-type)
  (make-string-out-type narrow-out/write-char
			narrow-out/extract-output
			narrow-out/extract-output!))

(define (narrow-out/write-char port char)
  (if (not (fix:< (char->integer char) #x100))
      (error:not-8-bit-char char))
  (let ((os (port/state port)))
    (maybe-grow-buffer os 1)
    (string-set! (ostate-buffer os) (ostate-index os) char)
    (set-ostate-index! os (fix:+ (ostate-index os) 1))
    (set-ostate-column! os (new-column char (ostate-column os)))
    1))

(define (narrow-out/extract-output port)
  (let ((os (port/state port)))
    (string-head (ostate-buffer os) (ostate-index os))))

(define (narrow-out/extract-output! port)
  (let* ((os (port/state port))
	 (output (string-head! (ostate-buffer os) (ostate-index os))))
    (reset-buffer! os)
    output))

(define (make-wide-output-type)
  (make-string-out-type wide-out/write-char
			wide-out/extract-output
			wide-out/extract-output!))

(define (wide-out/write-char port char)
  (let ((os (port/state port)))
    (maybe-grow-buffer os 1)
    (wide-string-set! (ostate-buffer os) (ostate-index os) char)
    (set-ostate-index! os (fix:+ (ostate-index os) 1))
    (set-ostate-column! os (new-column char (ostate-column os)))
    1))

(define (wide-out/extract-output port)
  (let ((os (port/state port)))
    (wide-substring (ostate-buffer os) 0 (ostate-index os))))

(define (wide-out/extract-output! port)
  (let ((os (port/state port)))
    (let ((output (wide-substring (ostate-buffer os) 0 (ostate-index os))))
      (reset-buffer! os)
      output)))

(define (make-string-out-type write-char extract-output extract-output!)
  (make-port-type `((WRITE-CHAR ,write-char)
		    (WRITE-SUBSTRING ,string-out/write-substring)
		    (EXTRACT-OUTPUT ,extract-output)
		    (EXTRACT-OUTPUT! ,extract-output!)
		    (OUTPUT-COLUMN ,string-out/output-column)
		    (POSITION ,string-out/position)
		    (WRITE-SELF ,string-out/write-self))
		  #f))

(define-structure ostate
  buffer
  index
  column)

(define (string-out/output-column port)
  (ostate-column (port/state port)))

(define (string-out/position port)
  (ostate-index (port/state port)))

(define (string-out/write-self port output-port)
  port
  (write-string " to string" output-port))

(define (string-out/write-substring port string start end)
  (let ((os (port/state port))
	(n (- end start)))
    (maybe-grow-buffer os n)
    (let* ((start* (ostate-index os))
	   (end* (+ start* n)))
      (move-chars! string start end (ostate-buffer os) start* end*)
      (set-ostate-index! os end*))
    (update-column-for-substring! os n)
    n))

(define (maybe-grow-buffer os n)
  (let ((buffer (ostate-buffer os))
	(n (+ (ostate-index os) n)))
    (let ((m
	   (if (wide-string? buffer)
	       (wide-string-length buffer)
	       (string-length buffer))))
      (if (< m n)
	  (let ((buffer*
		 (let ((m*
			(let loop ((m (+ m m)))
			  (if (< m n)
			      (loop (+ m m))
			      m))))
		   (if (wide-string? buffer)
		       (make-wide-string m*)
		       (make-string m*)))))
	    (move-chars! buffer 0 (ostate-index os)
			 buffer* 0 (ostate-index os))
	    (set-ostate-buffer! os buffer*))))))

(define (reset-buffer! os)
  (set-ostate-buffer! os
		      (if (wide-string? (ostate-buffer os))
			  (make-wide-string 16)
			  (make-string 16)))
  (set-ostate-index! os 0)
  (set-ostate-column! os 0))

(define (new-column char column)
  (case char
    ((#\newline) 0)
    ((#\tab) (fix:+ column (fix:- 8 (fix:remainder column 8))))
    (else (fix:+ column 1))))

(define (update-column-for-substring! os n)
  (let ((string (ostate-buffer os))
	(end (ostate-index os)))
    (let ((start (- (ostate-index os) n)))
      (letrec
	  ((loop
	    (lambda (i column)
	      (if (< i end)
		  (loop (+ i 1)
			(new-column (if (wide-string? string)
					(wide-string-ref string i)
					(string-ref string i))
				    column))
		  (set-ostate-column! os column)))))
	(let ((nl (find-newline string start end)))
	  (if nl
	      (loop (+ nl 1) 0)
	      (loop start (ostate-column os))))))))

(define (find-newline string start end)
  (if (wide-string? string)
      (let loop ((index end))
	(and (fix:> index start)
	     (let ((index (fix:- index 1)))
	       (if (char=? (wide-string-ref string index) #\newline)
		   index
		   (loop index)))))
      (xsubstring-find-previous-char string start end #\newline)))

;;;; Output as octets

(define (call-with-output-octets generator)
  (let ((port (open-output-octets)))
    (generator port)
    (get-output-string port)))

(define (open-output-octets)
  (let ((port
	 (let ((os (make-ostate (make-vector-8b 16) 0 #f)))
	   (make-generic-i/o-port #f
				  (make-byte-sink os)
				  octets-output-type
				  os))))
    (port/set-line-ending port 'NEWLINE)
    port))

(define (make-byte-sink os)
  (make-non-channel-port-sink
   (lambda (octets start end)
     (let ((index (ostate-index os)))
       (let ((n (fix:+ index (fix:- end start))))
	 (let ((buffer (ostate-buffer os)))
	   (if (fix:> n (vector-8b-length buffer))
	       (set-ostate-buffer!
		os
		(let ((new
		       (make-vector-8b
			(let loop ((m (vector-8b-length buffer)))
			  (if (fix:>= m n)
			      m
			      (loop (fix:+ m m)))))))
		  (substring-move! buffer 0 index new 0)
		  new))))
	 (substring-move! octets start end (ostate-buffer os) index)
	 (set-ostate-index! os n)
	 (fix:- end start))))))

(define (make-octets-output-type)
  (make-port-type `((EXTRACT-OUTPUT ,octets-out/extract-output)
		    (EXTRACT-OUTPUT! ,octets-out/extract-output!)
		    (POSITION ,octets-out/position)
		    (WRITE-SELF ,octets-out/write-self))
		  (generic-i/o-port-type #f #t)))

(define (octets-out/extract-output port)
  (output-port/flush-output port)
  (let ((os (output-octets-port/os port)))
    (string-head (ostate-buffer os) (ostate-index os))))

(define (octets-out/extract-output! port)
  (output-port/flush-output port)
  (let* ((os (output-octets-port/os port))
	 (output (string-head! (ostate-buffer os) (ostate-index os))))
    (set-ostate-buffer! os (make-vector-8b 16))
    (set-ostate-index! os 0)
    output))

(define (octets-out/position port)
  (output-port/flush-output port)
  (ostate-index (output-octets-port/os port)))

(define (octets-out/write-self port output-port)
  port
  (write-string " to byte vector" output-port))

(define narrow-input-type)
(define wide-input-type)
(define external-input-type)
(define octets-input-type)
(define narrow-output-type)
(define wide-output-type)
(define octets-output-type)
(define output-octets-port/os)

(define (initialize-package!)
  (set! narrow-input-type (make-narrow-input-type))
  (set! wide-input-type (make-wide-input-type))
  (set! external-input-type (make-external-input-type))
  (set! octets-input-type (make-octets-input-type))
  (set! narrow-output-type (make-narrow-output-type))
  (set! wide-output-type (make-wide-output-type))
  (set! octets-output-type (make-octets-output-type))
  (set! output-octets-port/os (generic-i/o-port-accessor 0))
  unspecific)