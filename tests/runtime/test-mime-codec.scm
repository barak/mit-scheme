#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Tests of MIME codecs

(declare (usual-integrations))

(load-option 'mime-codec)

(define (test-encoder n-packets packet-length text? filename
		      binary-codec? initialize finalize update)
  (call-with-output-file filename
    (lambda (port)
      (let ((context (initialize port text?))
	    (n-packets (random n-packets)))
	(do ((i 0 (+ i 1)))
	    ((= i n-packets))
	  (let ((packet-length (random packet-length)))
	    (write i port)
	    (write-char #\space port)
	    (write packet-length port)
	    (write-char #\space port)
	    (let ((packet (make-test-packet packet-length text? binary-codec?)))
	      (write packet port)
	      (newline port)
	      (update context packet 0 packet-length))))
	(finalize context)))))

(define (make-test-packet packet-length text? binary-codec?)
  (cond (binary-codec? (random-bytevector packet-length))
	(text? (random-text-string packet-length))
	(else (random-byte-vector packet-length))))

(define (random-text-string length)
  (let ((builder (string-builder))
	(n-text (string-length text-characters)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i length)))
      (builder (string-ref text-characters (random n-text))))
    (builder 'immutable)))

(define (random-byte-vector length)
  (let ((bv (random-bytevector length))
	(builder (string-builder)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i length)))
      (builder (integer->char (bytevector-u8-ref bv i))))
    (builder 'immutable)))

(define text-characters
  (list->string
   (append '(#\tab #\newline)
	   (char-set-members char-set:graphic))))

(define (test-codec n-packets packet-length text? filename binary-codec?
		    encode:initialize encode:finalize encode:update
		    decode:initialize decode:finalize decode:update)
  (let ((packets
	 (make-test-vector n-packets packet-length text? binary-codec?)))
    (if binary-codec?
	(begin
	  (call-with-binary-output-file (pathname-new-type filename "clear1")
	    (lambda (port)
	      (vector-for-each (lambda (packet)
				 (write-bytevector packet port))
			       packets)))
	  (call-with-output-file (pathname-new-type filename "encoded")
	    (lambda (port)
	      (let ((context (encode:initialize port text?)))
		(vector-for-each (lambda (packet)
				   (encode:update context packet))
				 packets)
		(encode:finalize context)))))
	(begin
	  (call-with-output-file (pathname-new-type filename "clear1")
	    (lambda (port)
	      (vector-for-each (lambda (packet)
				 (write-string packet port))
			       packets)))
	  (call-with-output-file (pathname-new-type filename "encoded")
	    (lambda (port)
	      (let ((context (encode:initialize port text?)))
		(vector-for-each (lambda (packet)
				   (encode:update context packet))
				 packets)
		(encode:finalize context)))))))
  (retest-decoder text? filename binary-codec?
		  decode:initialize decode:finalize decode:update))

(define (make-test-vector n-packets packet-length text? binary-codec?)
  (let ((n-packets (random n-packets))
	(builder (vector-builder)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n-packets)))
      (builder
       (make-test-packet (random packet-length)
			 text?
			 binary-codec?)))
    (builder)))

(define (retest-codec text? filename binary-codec?
		      encode:initialize encode:finalize encode:update
		      decode:initialize decode:finalize decode:update)
  (if binary-codec?
      (call-with-binary-input-file (pathname-new-type filename "clear1")
	(lambda (input-port)
	  (call-with-output-file (pathname-new-type filename "encoded")
	    (lambda (output-port)
	      (let ((context (encode:initialize output-port text?)))
		(let loop ()
		  (let ((bv (read-bytevector 37 input-port)))
		    (if (not (eof-object? bv))
			(begin
			  (encode:update context bv)
			  (loop)))))
		(encode:finalize context))))))
      (call-with-input-file (pathname-new-type filename "clear1")
	(lambda (input-port)
	  (call-with-output-file (pathname-new-type filename "encoded")
	    (lambda (output-port)
	      (let ((context (encode:initialize output-port text?)))
		(let loop ()
		  (let ((string (read-string 37 input-port)))
		    (if (not (eof-object? string))
			(begin
			  (encode:update context string)
			  (loop)))))
		(encode:finalize context)))))))
  (retest-decoder text? filename binary-codec?
		  decode:initialize decode:finalize decode:update))

(define (retest-decoder text? filename binary-codec?
			decode:initialize decode:finalize decode:update)
  (let ((pn3 (pathname-new-type filename "clear2")))
    (if binary-codec?
	(begin
	  (call-with-input-file (pathname-new-type filename "encoded")
	    (lambda (input-port)
	      (call-with-binary-output-file pn3
		(lambda (output-port)
		  (let ((context (decode:initialize output-port text?)))
		    (let loop ()
		      (let ((string (read-string 41 input-port)))
			(if (not (eof-object? string))
			    (begin
			      (decode:update context string)
			      (loop)))))
		    (decode:finalize context))))))
	  (call-with-binary-input-file (pathname-new-type filename "clear1")
	    (lambda (p1)
	      (call-with-binary-input-file pn3
		(lambda (p3)
		  (let loop ()
		    (let ((b1 (read-u8 p1))
			  (b3 (read-u8 p3)))
		      (if (eof-object? b1)
			  (if (eof-object? b3)
			      unspecific
			      (error "Output file longer."))
			  (if (eof-object? b3)
			      (error "Output file shorter.")
			      (if (fix:= b1 b3)
				  (loop)
				  (error "Files don't match.")))))))))))
	(begin
	  (call-with-input-file (pathname-new-type filename "encoded")
	    (lambda (input-port)
	      (call-with-output-file pn3
		(lambda (output-port)
		  (let ((context (decode:initialize output-port text?)))
		    (let loop ()
		      (let ((string (read-string 41 input-port)))
			(if (not (eof-object? string))
			    (begin
			      (decode:update context string)
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
				  (error "Files don't match."))))))))))))))

(define (for-each-setting procedure)
  (procedure 20 1024 #t)
  (procedure 20 1024 #f))

(define (define-mime-codec-tests name binary-codec?
	  encode:initialize encode:finalize encode:update
	  decode:initialize decode:finalize decode:update)
  (for-each-setting
   (lambda (n-packets packet-length text?)
     (define-test (symbol 'encode '- name
			  '/ (if text? 'text 'binary)
			  '/ n-packets
			  '/ packet-length)
       (lambda ()
	 (call-with-temporary-file-pathname
	   (lambda (pathname)
	     (test-encoder
	      n-packets packet-length text? pathname
	      binary-codec? encode:initialize encode:finalize encode:update)))))
     (define-test (symbol 'codec '- name
			  '/ (if text? 'text 'binary)
			  '/ n-packets
			  '/ packet-length)
       (lambda ()
	 (call-with-temporary-file-pathname
	   (lambda (pathname)
	     (test-codec
	      n-packets packet-length text? pathname binary-codec?
	      encode:initialize encode:finalize encode:update
	      decode:initialize decode:finalize decode:update))))))))

(define-mime-codec-tests 'BASE64
  #t
  encode-base64:initialize
  encode-base64:finalize
  encode-base64:update
  decode-base64:initialize
  decode-base64:finalize
  decode-base64:update)

#;
(define-mime-codec-tests 'BINHEX40
  #t
  encode-binhex40:initialize
  encode-binhex40:finalize
  encode-binhex40:update
  decode-binhex40:initialize
  decode-binhex40:finalize
  decode-binhex40:update)

(define-mime-codec-tests 'QUOTED-PRINTABLE
  #f
  encode-quoted-printable:initialize
  encode-quoted-printable:finalize
  encode-quoted-printable:update
  decode-quoted-printable:initialize
  decode-quoted-printable:finalize
  decode-quoted-printable:update)

#;
(define-mime-codec-tests 'UUE
  #t
  encode-uue:initialize
  encode-uue:finalize
  encode-uue:update
  decode-uue:initialize
  decode-uue:finalize
  decode-uue:update)