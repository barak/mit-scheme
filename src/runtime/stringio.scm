#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;; obsolete
(define (with-input-from-string string thunk)
  (with-input-from-port (open-input-string string) thunk))

(define (call-with-input-string string procedure)
  (procedure (open-input-string string)))

(define (open-input-string string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'open-input-string))
	 (start (fix:start-index start end 'open-input-string)))
    (make-textual-port string-input-type
		       (make-istate string start end start))))

(define-structure istate
  (string #f read-only #t)
  (start #f read-only #t)
  (end #f read-only #t)
  next)

(define (make-string-input-type)
  (make-textual-port-type `((char-ready? ,string-in/char-ready?)
			    (eof? ,string-in/eof?)
			    (peek-char ,string-in/peek-char)
			    (read-char ,string-in/read-char)
			    (read-substring ,string-in/read-substring)
			    (unread-char ,string-in/unread-char)
			    (write-self ,string-in/write-self))
			  #f))

(define (string-in/char-ready? port)
  port
  #t)

(define (string-in/eof? port)
  (let ((ss (textual-port-state port)))
    (not (fix:< (istate-next ss) (istate-end ss)))))

(define (string-in/peek-char port)
  (let ((ss (textual-port-state port)))
    (if (fix:< (istate-next ss) (istate-end ss))
	(string-ref (istate-string ss) (istate-next ss))
	(make-eof-object port))))

(define (string-in/read-char port)
  (let ((ss (textual-port-state port)))
    (if (fix:< (istate-next ss) (istate-end ss))
	(let ((char (string-ref (istate-string ss) (istate-next ss))))
	  (set-istate-next! ss (fix:+ (istate-next ss) 1))
	  char)
	(make-eof-object port))))

(define (string-in/read-substring port string start end)
  (let ((ss (textual-port-state port)))
    (let ((string* (istate-string ss))
	  (start* (istate-next ss))
	  (end* (istate-end ss)))
      (let ((n (fix:min (fix:- end start) (fix:- end* start*))))
	(string-copy! string* start* string start (fix:+ start n))
	(set-istate-next! ss (fix:+ start* n))
	n))))

(define (string-in/unread-char port char)
  (let ((ss (textual-port-state port)))
    (if (not (fix:< (istate-start ss) (istate-next ss)))
	(error "No char to unread:" port))
    (let ((prev (fix:- (istate-next ss) 1)))
      (if (not (char=? char (string-ref (istate-string ss) prev)))
	  (error "Unread char incorrect:" char))
      (set-istate-next! ss prev))))

(define (string-in/write-self port output-port)
  port
  (write-string " from string" output-port))

;;;; Input as byte vector

(define (call-with-input-octets octets procedure)
  (procedure (open-input-octets octets)))

(define (open-input-octets octets #!optional start end)
  (let* ((end (fix:end-index end (string-length octets) 'open-input-octets))
	 (start (fix:start-index start end 'open-input-octets))
	 (port
	  (make-generic-i/o-port (make-octets-source octets start end)
				 #f
				 'open-input-octets
				 octets-input-type)))
    (port/set-coding port 'binary)
    (port/set-line-ending port 'binary)
    port))

(define (make-octets-source string start end)
  (let ((index start))
    (make-non-channel-input-source
     (lambda ()
       (fix:< index end))
     (lambda (bv start* end*)
       (let ((n (fix:min (fix:- end index) (fix:- end* start*))))
	 (let ((limit (fix:+ index n)))
	   (do ((i index (fix:+ i 1))
		(j start* (fix:+ j 1)))
	       ((not (fix:< i limit))
		(set! index i))
	     (bytevector-u8-set! bv j (char->integer (string-ref string i)))))
	 n)))))

(define (make-octets-input-type)
  (make-textual-port-type
   `((write-self
      ,(lambda (port output-port)
	 port
	 (write-string " from byte vector" output-port))))
   (generic-i/o-port-type #t #f)))

;;;; Output as characters

(define (open-output-string)
  (make-output-string (make-ustring 16)))

(define (get-output-string port)
  ((port/operation port 'extract-output) port))

(define (get-output-string! port)
  ((port/operation port 'extract-output!) port))

(define (call-with-output-string generator)
  (let ((port (open-output-string)))
    (generator port)
    (get-output-string port)))

(define (call-with-truncated-output-string limit generator)
  (let ((port (open-output-string)))
    (let ((truncated? (call-with-truncated-output-port limit port generator)))
      (cons truncated? (get-output-string port)))))

;; deprecated
(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (port)
      (with-output-to-port port thunk))))

;; deprecated
(define (with-output-to-truncated-string limit thunk)
  (call-with-truncated-output-string limit
    (lambda (port)
      (with-output-to-port port thunk))))

(define (make-output-string buffer)
  (make-textual-port string-output-type (make-ostate buffer 0 0)))

(define-structure ostate
  buffer
  index
  column)

(define (make-string-output-type)
  (make-textual-port-type `((write-char ,string-out/write-char)
			    (write-substring ,string-out/write-substring)
			    (extract-output ,string-out/extract-output)
			    (extract-output! ,string-out/extract-output!)
			    (output-column ,string-out/output-column)
			    (position ,string-out/position)
			    (write-self ,string-out/write-self))
			  #f))

(define (string-out/write-char port char)
  (let ((os (textual-port-state port)))
    (maybe-grow-buffer os 1)
    (string-set! (ostate-buffer os) (ostate-index os) char)
    (set-ostate-index! os (fix:+ (ostate-index os) 1))
    (set-ostate-column! os (new-column char (ostate-column os)))
    1))

(define (string-out/write-substring port string start end)
  (let ((os (textual-port-state port))
	(n (fix:- end start)))
    (maybe-grow-buffer os n)
    (string-copy! (ostate-buffer os) (ostate-index os) string start end)
    (set-ostate-index! os (fix:+ (ostate-index os) n))
    (update-column-for-substring! os n)
    n))

(define (string-out/extract-output port)
  (let ((os (textual-port-state port)))
    (string-copy (ostate-buffer os) 0 (ostate-index os))))

(define (string-out/extract-output! port)
  (let* ((os (textual-port-state port))
	 (output (string-copy (ostate-buffer os) 0 (ostate-index os))))
    (reset-buffer! os)
    output))

(define (string-out/output-column port)
  (ostate-column (textual-port-state port)))

(define (string-out/position port)
  (ostate-index (textual-port-state port)))

(define (string-out/write-self port output-port)
  port
  (write-string " to string" output-port))

(define (maybe-grow-buffer os n)
  (let ((buffer (ostate-buffer os))
	(n (fix:+ (ostate-index os) n)))
    (let ((m (string-length buffer)))
      (if (fix:< m n)
	  (let ((buffer*
		 (make-ustring
		  (let loop ((m (fix:+ m m)))
		    (if (fix:< m n)
			(loop (fix:+ m m))
			m)))))
	    (string-copy! buffer* 0 buffer 0 (ostate-index os))
	    (set-ostate-buffer! os buffer*))))))

(define (reset-buffer! os)
  (set-ostate-buffer! os (make-ustring 16))
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
    (let ((start (fix:- (ostate-index os) n)))
      (letrec
	  ((loop
	    (lambda (i column)
	      (if (fix:< i end)
		  (loop (fix:+ i 1)
			(new-column (string-ref string i) column))
		  (set-ostate-column! os column)))))
	(let ((nl (find-newline string start end)))
	  (if nl
	      (loop (fix:+ nl 1) 0)
	      (loop start (ostate-column os))))))))

(define (find-newline string start end)
  (substring-find-next-char string start end #\newline))

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
				  'open-output-octets
				  octets-output-type
				  os))))
    (port/set-line-ending port 'newline)
    port))

(define (make-byte-sink os)
  (make-non-channel-output-sink
   (lambda (bv start end)
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
	 (let ((buffer (ostate-buffer os)))
	   (do ((i start (fix:+ i 1))
		(j index (fix:+ j 1)))
	       ((not (fix:< i end)))
	     (vector-8b-set! buffer j (bytevector-u8-ref bv j))))
	 (set-ostate-index! os n)
	 (fix:- end start))))))

(define (make-octets-output-type)
  (make-textual-port-type `((EXTRACT-OUTPUT ,octets-out/extract-output)
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
	 (output (string-head (ostate-buffer os) (ostate-index os))))
    (set-ostate-buffer! os (make-vector-8b 16))
    (set-ostate-index! os 0)
    output))

(define (octets-out/position port)
  (output-port/flush-output port)
  (ostate-index (output-octets-port/os port)))

(define (octets-out/write-self port output-port)
  port
  (write-string " to byte vector" output-port))

(define string-input-type)
(define octets-input-type)
(define string-output-type)
(define octets-output-type)
(define output-octets-port/os)
(add-boot-init!
 (lambda ()
   (set! string-input-type (make-string-input-type))
   (set! octets-input-type (make-octets-input-type))
   (set! string-output-type (make-string-output-type))
   (set! octets-output-type (make-octets-output-type))
   (set! output-octets-port/os (generic-i/o-port-accessor 0))
   unspecific))