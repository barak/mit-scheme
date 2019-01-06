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

;;;; MIME support

(declare (usual-integrations))

(define (make-decoding-port-type update finalize)
  (make-textual-port-type
   `((write-char
      ,(lambda (port char)
	 (update (textual-port-state port) (string char) 0 1)
	 1))
     (write-substring
      ,(lambda (port string start end)
	 (if (string? string)
	     (begin
	       (update (textual-port-state port) string start end)
	       (fix:- end start))
	     (generic-port-operation:write-substring port string start end))))
     (close-output
      ,(lambda (port)
	 (finalize (textual-port-state port)))))
   #f))

(define condition-type:decode-mime
  (make-condition-type 'decode-mime condition-type:simple-error '() #f))

;;;; Encode quoted-printable

;;; Hair from two things: (1) delaying the decision to encode trailing
;;; whitespace until we see what comes after it on the line; and (2)
;;; an incremental line-breaking algorithm.

(define-structure (qp-encoding-context
		   (conc-name qp-encoding-context/)
		   (constructor encode-quoted-printable:initialize
				(port text?)))
  (port #f read-only #t)
  (text? #f read-only #t)
  ;; Either #F, or an LWSP input that may or may not need to be
  ;; encoded, depending on subsequent input.
  (pending-lwsp #f)
  ;; An exact integer between 0 and 75 inclusive, recording the number
  ;; of characters that have been written on the current output line.
  (column 0)
  ;; Either #F, or an output string that may or may not fit on the
  ;; current output line, depending on subsequent output.
  (pending-output #f))

(define (encode-quoted-printable:finalize context)
  (encode-qp-pending-lwsp context #f 'input-end)
  (write-qp-pending-output context #t))

(define (encode-quoted-printable:update context string #!optional start end)
  (let* ((caller 'encode-quoted-printable:update)
	 (end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    (if (qp-encoding-context/text? context)
	(let loop ((start start))
	  (let ((i (string-find-next-char string #\newline start end)))
	    (if i
		(begin
		  (encode-qp context string start i 'line-end)
		  (loop (fix:+ i 1)))
		(encode-qp context string start end 'partial))))
	(encode-qp context string start end 'partial))))

(define (encode-qp context string start end type)
  (encode-qp-pending-lwsp context (fix:< start end) type)
  (let loop ((start start))
    (cond ((fix:< start end)
	   (let ((char (string-ref string start))
		 (start (fix:+ start 1)))
	     (cond ((not (char-lwsp? char))
		    (if (char-in-set? char char-set:qp-encoded)
			(write-qp-encoded context char)
			(write-qp-clear context char))
		    (loop start))
		   ((and (eq? type 'partial)
			 (not (fix:< start end)))
		    (set-qp-encoding-context/pending-lwsp! context char))
		   (else
		    (if (fix:< start end)
			(write-qp-clear context char)
			(write-qp-encoded context char))
		    (loop start)))))
	  ((eq? type 'line-end)
	   (write-qp-hard-break context)))))

(define (encode-qp-pending-lwsp context packet-not-empty? type)
  (let ((pending (qp-encoding-context/pending-lwsp context)))
    (if pending
	(cond (packet-not-empty?
	       (set-qp-encoding-context/pending-lwsp! context #f)
	       (write-qp-clear context pending))
	      ((not (eq? type 'partial))
	       (set-qp-encoding-context/pending-lwsp! context #f)
	       (write-qp-encoded context pending))))))

(define (write-qp-clear context char)
  (write-qp-pending-output context #f)
  (let ((port (qp-encoding-context/port context))
	(column (qp-encoding-context/column context)))
    (cond ((fix:< column 75)
	   (write-char char port)
	   (set-qp-encoding-context/column! context (fix:+ column 1)))
	  ((not (qp-encoding-context/text? context))
	   (write-qp-soft-break context)
	   (write-char char port)
	   (set-qp-encoding-context/column! context 1))
	  (else
	   (set-qp-encoding-context/pending-output! context (string char))))))

(define (write-qp-encoded context char)
  (write-qp-pending-output context #f)
  (let ((port (qp-encoding-context/port context))
	(column (qp-encoding-context/column context))
	(d (char->integer char)))
    (let ((c1 (digit->char (fix:lsh d -4) 16))
	  (c2 (digit->char (fix:and d #x0F) 16)))
      (if (fix:= column 73)
	  (set-qp-encoding-context/pending-output! context (string #\= c1 c2))
	  (begin
	    (if (fix:> column 73)
		(write-qp-soft-break context))
	    (write-char #\= port)
	    (write-char c1 port)
	    (write-char c2 port)
	    (set-qp-encoding-context/column!
	     context
	     (fix:+ (qp-encoding-context/column context) 3)))))))

(define (write-qp-hard-break context)
  (write-qp-pending-output context #t)
  (newline (qp-encoding-context/port context))
  (set-qp-encoding-context/column! context 0))

(define (write-qp-pending-output context newline?)
  (let ((pending (qp-encoding-context/pending-output context)))
    (if pending
	(begin
	  (if (not newline?)
	      (write-qp-soft-break context))
	  (write-string pending (qp-encoding-context/port context))
	  (set-qp-encoding-context/pending-output! context #f)
	  (set-qp-encoding-context/column!
	   context
	   (fix:+ (qp-encoding-context/column context)
		  (string-length pending)))))))

(define (write-qp-soft-break context)
  (let ((port (qp-encoding-context/port context)))
    (write-char #\= port)
    (newline port))
  (set-qp-encoding-context/column! context 0))

;;;; Decode quoted-printable

;;; This decoder is unbelievably hairy.  The hair is due to the fact
;;; that the input to the decoder is arbitrarily packetized, and the
;;; encoder really wants to operate on units of input lines.  The
;;; strategy is that we process as much of the input packet as
;;; possible, then save enough state to continue when the next packet
;;; comes along.

(define-structure (qp-decoding-context
		   (conc-name qp-decoding-context/)
		   (constructor decode-quoted-printable:initialize
				(port text?)))
  (port #f read-only #t)
  (text? #f read-only #t)
  ;; Pending input that can't be processed until more input is
  ;; available.  Can take on one of the following values:
  ;; * #F means no pending input.
  ;; * A string, consisting entirely of LWSP characters, is whitespace
  ;;   that appeared at the end of an input packet.  We are waiting to
  ;;   see if it is followed by a newline, meaning it is to be
  ;;   discarded.  Otherwise it is part of the output.
  ;; * The character #\=, meaning that the equals-sign character has
  ;;   been seen and we need more characters to decide what to do with
  ;;   it.
  ;; * A hexadecimal-digit character (0-9, A-F), meaning that an
  ;;   equals sign and that character have been seen, and we are
  ;;   waiting for the second hexadecimal digit to arrive.
  (pending #f))

(define (decode-quoted-printable:finalize context)
  (decode-qp context "" 0 0 'input-end))

(define (decode-quoted-printable:update context string #!optional start end)
  (let* ((caller 'decode-quoted-printable:update)
	 (end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    (let loop ((start start))
      (let ((i (string-find-next-char string #\newline start end)))
	(if i
	    (begin
	      (decode-qp context
			 string start (skip-lwsp-backwards string start i)
			 'line-end)
	      (loop (fix:+ i 1)))
	    (decode-qp context string start end 'partial))))))

(define (call-with-decode-quoted-printable-output-port port text? generator)
  (let ((port (make-decode-quoted-printable-port port text?)))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define (make-decode-quoted-printable-port port text?)
  (make-textual-port decode-quoted-printable-port-type
		     (decode-quoted-printable:initialize port text?)))

(define decode-quoted-printable-port-type
  (make-decoding-port-type decode-quoted-printable:update
			   decode-quoted-printable:finalize))

(define (decode-qp context string start end type)
  (let ((port (qp-decoding-context/port context))
	(end* (skip-lwsp-backwards string start end)))

    (define (loop start)
      (let ((i
	     (string-find-next-char-in-set string char-set:qp-encoded
					   start end*)))
	(if i
	    (begin
	      (write-string string port start i)
	      (if (char=? (string-ref string i) #\=)
		  (handle-equals (fix:+ i 1))
		  ;; RFC 2045 recommends dropping illegal encoded char.
		  (loop (fix:+ i 1))))
	    (begin
	      (write-string string port start end*)
	      (finish)))))

    (define (handle-equals start)
      (if (fix:< (fix:+ start 1) end*)
	  (loop (decode-qp-hex context
			       (string-ref string start)
			       (string-ref string (fix:+ start 1))
			       (fix:+ start 2)))
	  (begin
	    (if (fix:< start end*)
		(let ((char (string-ref string start)))
		  (if (char->digit char 16)
		      (set-qp-decoding-context/pending! context char)
		      ;; Illegal: RFC 2045 recommends leaving as is.
		      (begin
			(write-char #\= port)
			(write-char char port))))
		(set-qp-decoding-context/pending! context #\=))
	    (finish))))

    (define (finish)
      (let ((pending (qp-decoding-context/pending context)))
	(set-qp-decoding-context/pending! context #f)
	(cond ((eq? type 'partial)
	       (set-qp-decoding-context/pending!
		context
		(decode-qp-pending-string pending string end* end)))
	      ((not pending)
	       (if (eq? type 'line-end)
		   ;; Hard line break.
		   (newline port)))
	      ((eqv? pending #\=)
	       (if (eq? type 'line-end)
		   unspecific		; Soft line break.
		   ;; Illegal: RFC 2045 recommends leaving as is.
		   (write-char #\= port)))
	      ((char? pending)
	       ;; Illegal: RFC 2045 recommends leaving as is.
	       (write-char #\= port)
	       (write-char pending port))
	      ((string? pending)
	       ;; Trailing whitespace: discard.
	       unspecific)
	      (else (error "Illegal PENDING value:" pending)))))

    (let ((pending (qp-decoding-context/pending context)))
      (if (and pending (fix:< start end*))
	  (begin
	    (set-qp-decoding-context/pending! context #f)
	    (cond ((eqv? pending #\=)
		   (handle-equals start))
		  ((char? pending)
		   (loop (decode-qp-hex context
					pending
					(string-ref string start)
					(fix:+ start 1))))
		  ((string? pending)
		   (write-string pending port)
		   (loop start))
		  (else (error "Illegal PENDING value:" pending))))
	  (loop start)))))

(define (decode-qp-pending-string pending string start end)
  (if (fix:< start end)
      (if pending
	  (string-append pending (substring string start end))
	  (substring string start end))
      pending))

(define char-set:qp-encoded
  (char-set-invert
   (char-set-union (char-set-difference (ascii-range->char-set #x21 #x7F)
					(char-set #\=))
		   (char-set #\space #\tab))))

(define (char-lwsp? char)
  (or (char=? #\space char)
      (char=? #\tab char)))

(define (skip-lwsp-backwards string start end)
  (let loop ((end end))
    (if (and (fix:< start end)
	     (char-lwsp? (string-ref string (fix:- end 1))))
	(loop (fix:- end 1))
	end)))

(define (decode-qp-hex context c1 c2 start)
  (let ((port (qp-decoding-context/port context)))
    (let ((char
	   (let ((d1 (char->digit c1 16))
		 (d2 (char->digit c2 16)))
	     (and (fix:< d1 #x10)
		  (fix:< d2 #x10)
		  (integer->char (fix:or (fix:lsh d1 4) d2))))))
      (if char
	  (begin
	    (write-char char port)
	    start)
	  ;; This case is illegal.  RFC 2045 recommends
	  ;; leaving it unconverted.
	  (begin
	    (write-char #\= port)
	    (write-char c1 port)
	    (fix:- start 1))))))

;;;; Encode BASE64

(define-structure (base64-encoding-context
		   (conc-name base64-encoding-context/)
		   (constructor encode-base64:initialize (port text?)))
  (port #f read-only #t)
  (text? #f read-only #t)
  (buffer (make-bytevector 48) read-only #t)
  (index 0))

(define (encode-base64:finalize context)
  (write-base64-line context))

(define (encode-base64:update context bytes #!optional start end)
  (let* ((caller 'encode-base64:update)
	 (end (fix:end-index end (bytevector-length bytes) caller))
	 (start (fix:start-index start end caller)))
    (if (base64-encoding-context/text? context)
	(let loop ((start start))
	  (let ((index
		 (let find-newline ((index start))
		   (and (fix:< index end)
			(if (fix:= cp:newline (bytevector-u8-ref bytes index))
			    index
			    (find-newline (fix:+ index 1)))))))
	    (if index
		(begin
		  (encode-base64 context bytes start index)
		  (encode-base64 context bv:crlf 0 2)
		  (loop (fix:+ index 1)))
		(encode-base64 context bytes start end))))
	(encode-base64 context bytes start end))))

(define (encode-base64 context bytes start end)
  (let ((buffer (base64-encoding-context/buffer context)))
    (let loop ((start start))
      (if (fix:< start end)
	  (let ((i (base64-encoding-context/index context)))
	    (let ((start* (fix:min end (fix:+ start (fix:- 48 i)))))
	      (let ((i (bytevector-copy! buffer i bytes start start*)))
		(set-base64-encoding-context/index! context i)
		(if (fix:= i 48)
		    (write-base64-line context)))
	      (loop start*)))))))

(define (write-base64-line context)
  (let ((buffer (base64-encoding-context/buffer context))
	(end (base64-encoding-context/index context))
	(port (base64-encoding-context/port context)))
    (if (fix:> end 0)
	(begin
	  (let ((write-digit
		 (lambda (d)
		   (write-char (base64:digit->char (fix:and #x3F d)) port))))
	    (let loop ((start 0))
	      (let ((n (fix:- end start)))
		(cond ((fix:>= n 3)
		       (let ((d1 (bytevector-u8-ref buffer start))
			     (d2 (bytevector-u8-ref buffer (fix:+ start 1)))
			     (d3 (bytevector-u8-ref buffer (fix:+ start 2))))
			 (write-digit (fix:lsh d1 -2))
			 (write-digit (fix:or (fix:lsh d1 4) (fix:lsh d2 -4)))
			 (write-digit (fix:or (fix:lsh d2 2) (fix:lsh d3 -6)))
			 (write-digit d3))
		       (loop (fix:+ start 3)))
		      ((fix:= n 2)
		       (let ((d1 (bytevector-u8-ref buffer start))
			     (d2 (bytevector-u8-ref buffer (fix:+ start 1))))
			 (write-digit (fix:lsh d1 -2))
			 (write-digit (fix:or (fix:lsh d1 4) (fix:lsh d2 -4)))
			 (write-digit (fix:lsh d2 2)))
		       (write-char #\= port))
		      ((fix:= n 1)
		       (let ((d1 (bytevector-u8-ref buffer start)))
			 (write-digit (fix:lsh d1 -2))
			 (write-digit (fix:lsh d1 4)))
		       (write-char #\= port)
		       (write-char #\= port))))))
	  (newline port)
	  (set-base64-encoding-context/index! context 0)))))

;;;; Decode BASE64

(define-structure (base64-decoding-context
		   (conc-name base64-decoding-context/)
		   (constructor decode-base64:initialize (port text?)))
  (port #f read-only #t)
  (text? #f read-only #t)
  (input-buffer (make-string 4) read-only #t)
  (input-index 0)
  ;; Ugh bletch.  Add state to look for line starting with NON-BASE64
  ;; character, and stop decoding there.  This works around problem
  ;; that arises when mail-processing agents randomly glue text on the
  ;; end of a MIME message.
  (input-state 'line-start)
  (output-buffer (make-bytevector 3) read-only #t)
  (pending-return? #f))

(define (decode-base64:finalize context)
  (if (fix:> (base64-decoding-context/input-index context) 0)
      (error:decode-base64 "BASE64 input length is not a multiple of 4."))
  (if (base64-decoding-context/pending-return? context)
      (write-u8 cp:return (base64-decoding-context/port context))))

(define (decode-base64:update context string #!optional start end)
  (let* ((caller 'decode-base64:update)
	 (end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    (if (not (eq? 'finished (base64-decoding-context/input-state context)))
	(let ((buffer (base64-decoding-context/input-buffer context)))
	  (let loop
	      ((start start)
	       (index (base64-decoding-context/input-index context))
	       (state (base64-decoding-context/input-state context)))
	    (let ((done
		   (lambda (state)
		     (set-base64-decoding-context/input-index! context index)
		     (set-base64-decoding-context/input-state! context state))))
	      (if (fix:< start end)
		  (let* ((char (string-ref string start))
			 (continue
			  (lambda (index)
			    (loop (fix:+ start 1)
				  index
				  (if (char=? char #\newline)
				      'line-start
				      'in-line)))))
		    (if (or (char=? char #\=)
			    (fix:< (base64:char->digit char) #x40))
			(begin
			  (string-set! buffer index char)
			  (if (fix:< index 3)
			      (continue (fix:+ index 1))
			      (begin
				(decode-base64-quantum context)
				(continue 0))))
			(if (eq? state 'line-start)
			    (done 'finished)
			    (continue index))))
		  (done state))))))))

(define (call-with-decode-base64-output-port port text? generator)
  (let ((port (make-decode-base64-port port text?)))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define (make-decode-base64-port port text?)
  (make-textual-port decode-base64-port-type
		     (decode-base64:initialize port text?)))

(define decode-base64-port-type
  (make-decoding-port-type decode-base64:update decode-base64:finalize))

(define (decode-base64-quantum context)
  (let ((input (base64-decoding-context/input-buffer context))
	(output (base64-decoding-context/output-buffer context))
	(port (base64-decoding-context/port context)))
    (let ((n (decode-base64-quantum-1 input output)))
      (if (base64-decoding-context/text? context)
	  (let loop
	      ((index 0)
	       (pending? (base64-decoding-context/pending-return? context)))
	    (if (fix:< index n)
		(let ((u8 (bytevector-u8-ref output index)))
		  (if pending?
		      (if (fix:= cp:newline u8)
			  (begin
			    (write-u8 u8 port)
			    (loop (fix:+ index 1) #f))
			  (begin
			    (write-u8 cp:return port)
			    (loop index #f)))
		      (if (fix:= cp:return u8)
			  (loop (fix:+ index 1) #t)
			  (begin
			    (write-u8 u8 port)
			    (loop (fix:+ index 1) #f)))))
		(set-base64-decoding-context/pending-return?! context
							      pending?)))
	  (write-bytevector output port 0 n)))))

(define (decode-base64-quantum-1 input output)
  (let ((d1 (decode-base64-char input 0))
	(d2 (decode-base64-char input 1)))
    (cond ((not (char=? (string-ref input 3) #\=))
	   (let ((n
		  (fix:+ (fix:+ (fix:lsh d1 18)
				(fix:lsh d2 12))
			 (fix:+ (fix:lsh (decode-base64-char input 2) 6)
				(decode-base64-char input 3)))))
	     (bytevector-u8-set! output 0 (fix:lsh n -16))
	     (bytevector-u8-set! output 1 (fix:and #xFF (fix:lsh n -8)))
	     (bytevector-u8-set! output 2 (fix:and #xFF n))
	     3))
	  ((not (char=? (string-ref input 2) #\=))
	   (let ((n
		  (fix:+ (fix:+ (fix:lsh d1 10) (fix:lsh d2 4))
			 (fix:lsh (decode-base64-char input 2) -2))))
	     (bytevector-u8-set! output 0 (fix:lsh n -8))
	     (bytevector-u8-set! output 1 (fix:and #xFF n)))
	   2)
	  (else
	   (bytevector-u8-set! output 0 (fix:+ (fix:lsh d1 2) (fix:lsh d2 -4)))
	   1))))

(define (decode-base64-char input index)
  (let ((digit (base64:char->digit (string-ref input index))))
    (if (fix:> digit #x40)
	(error:decode-base64 "Misplaced #\\= in BASE64 input."))
    digit))

(define (base64:char->digit char)
  (let ((cp (char->integer char)))
    (if (fix:< cp #x80)
	(bytevector-u8-ref base64:char->digit-table cp)
	#xFF)))

(define (base64:digit->char digit)
  (string-ref base64:digit->char-table digit))

(define base64:char->digit-table)
(define base64:digit->char-table)
(let ((char-table (make-bytevector #x80 #xFF))
      (digit-table (make-string #x40)))

  (define (do-range low high value)
    (do-char low value)
    (if (fix:< low high)
	(do-range (fix:+ low 1) high (fix:+ value 1))))

  (define (do-char code value)
    (bytevector-u8-set! char-table code value)
    (string-set! digit-table value (integer->char code)))

  (do-range (char->integer #\A) (char->integer #\Z) 0)
  (do-range (char->integer #\a) (char->integer #\z) 26)
  (do-range (char->integer #\0) (char->integer #\9) 52)
  (do-char (char->integer #\+) 62)
  (do-char (char->integer #\/) 63)
  (set! base64:char->digit-table char-table)
  (set! base64:digit->char-table digit-table)
  unspecific)

(define condition-type:decode-base64
  (make-condition-type 'decode-base64 condition-type:decode-mime '() #f))

(define error:decode-base64
  (let ((signal
	 (condition-signaller condition-type:decode-base64
			      '(message irritants)
			      standard-error-handler)))
    (lambda (message . irritants)
      (signal message irritants))))

(define-integrable cp:newline (char->integer #\newline))
(define-integrable cp:return (char->integer #\return))

(define bv:crlf
  (let ((bv (make-bytevector 2)))
    (bytevector-u8-set! bv 0 cp:return)
    (bytevector-u8-set! bv 1 cp:newline)
    bv))

;;;; Decode BinHex 4.0

(define-structure (binhex40-decoding-context
		   (conc-name binhex40-decoding-context/)
		   (constructor make-binhex40-decoding-context (port)))
  (port #f read-only #t)
  (state 'seeking-comment)
  (line-buffer "")
  (input-buffer (make-string 4) read-only #t)
  (input-index 0)
  (output-buffer (make-bytevector 3) read-only #t))

(define (decode-binhex40:initialize port text?)
  text?					;ignored
  (make-binhex40-decoding-context
   (make-binhex40-run-length-decoding-port
    (make-binhex40-deconstructing-port port))))

(define (decode-binhex40:update context string #!optional start end)
  (let* ((caller 'decode-binhex40:update)
	 (end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    (let ((state (binhex40-decoding-context/state context)))
      (case (binhex40-decoding-context/state context)
	((seeking-comment)
	 (decode-binhex40-seeking-comment context string start end))
	((decoding)
	 (decode-binhex40-decoding context string start end))
	((ignoring)
	 unspecific)
	(else
	 (error "Illegal decoder state:" state))))))

(define (decode-binhex40:finalize context)
  (let ((state (binhex40-decoding-context/state context)))
    (case (binhex40-decoding-context/state context)
      ((seeking-comment)
       (error:decode-binhex40 "Missing BinHex 4.0 initial comment line."))
      ((decoding)
       (error:decode-binhex40 "Missing BinHex 4.0 terminating character."))
      ((ignoring)
       (close-output-port (binhex40-decoding-context/port context)))
      (else
       (error "Illegal decoder state:" state)))))

(define (call-with-decode-binhex40-output-port port text? generator)
  (let ((port (make-decode-binhex40-port port text?)))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define (make-decode-binhex40-port port text?)
  (make-textual-port decode-binhex40-port-type
		     (decode-binhex40:initialize port text?)))

(define decode-binhex40-port-type
  (make-decoding-port-type decode-binhex40:update decode-binhex40:finalize))

(define condition-type:decode-binhex40
  (make-condition-type 'decode-binhex40 condition-type:decode-mime '() #f))

(define error:decode-binhex40
  (let ((signal
	 (condition-signaller condition-type:decode-binhex40
			      '(message irritants)
			      standard-error-handler)))
    (lambda (message . irritants)
      (signal message irritants))))

(define (decode-binhex40-seeking-comment context string start end)
  (let loop
      ((s
	(string-append (binhex40-decoding-context/line-buffer context)
		       (substring string start end))))
    (let ((result (regsexp-match-string binhex40-header-regexp s)))
      (if result
	  (begin
	    (set-binhex40-decoding-context/state! context 'decoding)
	    (set-binhex40-decoding-context/line-buffer! context #f)
	    (decode-binhex40:update context s (cadr result)))
	  (set-binhex40-decoding-context/line-buffer! context s)))))

(define binhex40-header-regexp
  (compile-regsexp
   '(seq (* (char-in "\r\n\t "))
	 "(This file must be converted with BinHex"
	 (* (any-char))
	 (char-in "\r\n")
	 (* (char-in "\r\n\t "))
	 ":")))

(define (decode-binhex40-decoding context string start end)
  (let ((buffer (binhex40-decoding-context/input-buffer context)))
    (let loop
	((start start)
	 (index (binhex40-decoding-context/input-index context)))
      (if (fix:< start end)
	  (let ((char (string-ref string start))
		(start (fix:+ start 1)))
	    (cond ((char=? char #\:)
		   (if (fix:> index 0)
		       (begin
			 (string-set! buffer index char)
			 (decode-binhex40-quantum context)))
		   (set-binhex40-decoding-context/state! context 'ignoring))
		  ((fix:< (binhex40:char->digit char) #x40)
		   (string-set! buffer index char)
		   (if (fix:< index 3)
		       (loop start (fix:+ index 1))
		       (begin
			 (decode-binhex40-quantum context)
			 (loop start 0))))
		  (else
		   (loop start index))))
	  (set-binhex40-decoding-context/input-index! context index)))))

(define (decode-binhex40-quantum context)
  (let ((input (binhex40-decoding-context/input-buffer context))
	(output (binhex40-decoding-context/output-buffer context))
	(port (binhex40-decoding-context/port context)))
    (write-bytevector output
		      0
		      (decode-binhex40-quantum-1 input output)
		      port)))

(define (decode-binhex40-quantum-1 input output)
  (let ((d1 (decode-binhex40-char input 0))
	(d2 (decode-binhex40-char input 1)))
    (cond ((char=? (string-ref input 2) #\:)
	   (bytevector-u8-set! output 0 (fix:+ (fix:lsh d1 2) (fix:lsh d2 -4)))
	   1)
	  ((char=? (string-ref input 3) #\:)
	   (let ((n
		  (fix:+ (fix:+ (fix:lsh d1 10) (fix:lsh d2 4))
			 (fix:lsh (decode-binhex40-char input 2) -2))))
	     (bytevector-u8-set! output 0 (fix:lsh n -8))
	     (bytevector-u8-set! output 1 (fix:and #xFF n)))
	   2)
	  (else
	   (let ((n
		  (fix:+ (fix:+ (fix:lsh d1 18)
				(fix:lsh d2 12))
			 (fix:+ (fix:lsh (decode-binhex40-char input 2) 6)
				(decode-binhex40-char input 3)))))
	     (bytevector-u8-set! output 0 (fix:lsh n -16))
	     (bytevector-u8-set! output 1 (fix:and #xFF (fix:lsh n -8)))
	     (bytevector-u8-set! output 2 (fix:and #xFF n))
	     3)))))

(define (decode-binhex40-char input index)
  (let ((digit (binhex40:char->digit (string-ref input index))))
    (if (fix:>= digit #x40)
	(error:decode-binhex40 "Illegal character in BinHex 4.0 input stream:"
			       (string-ref input index)))
    digit))

(define (binhex40:char->digit char)
  (let ((cp (char->integer char)))
    (if (fix:< cp #x80)
	(bytevector-u8-ref binhex40:char->digit-table cp)
	#xFF)))

(define binhex40:digit->char-table
  "!\"#$%&\'()*+,-012345689@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdefhijklmpqr")

(define-deferred binhex40:char->digit-table
  (let ((table (make-bytevector #x80 #xFF)))
    (do ((digit 0 (fix:+ digit 1)))
	((not (fix:< digit #x40)))
      (bytevector-u8-set! table
			  (char->integer
			   (string-ref binhex40:digit->char-table digit))
			  digit))
    table))

;;;; BinHex 4.0 run-length decoding

(define (make-binhex40-run-length-decoding-port port)
  (make-binary-port #f (make-binhex-run-length-decoding-sink port)))

(define (make-binhex-run-length-decoding-sink port)
  (let ((marker #x90)
	(marker-seen? #f)
	(byte* #f))

    (define (write-bytes bytes start end)
      (do ((i start (fix:+ i 1)))
	  ((not (fix:< i end)))
	(write-byte (bytevector-u8-ref bytes i))))

    (define (write-byte byte)
      (cond (marker-seen?
	     (cond ((fix:= byte 0)
		    (if byte* (write-u8 byte* port))
		    (set! byte* marker))
		   (byte*
		    (do ((i 0 (fix:+ i 1)))
			((not (fix:< i byte)))
		      (write-u8 byte* port))
		    (set! byte* #f)))
	     (set! marker-seen? #f))
	    ((fix:= byte marker)
	     (set! marker-seen? #t))
	    (else
	     (if byte* (write-u8 byte* port))
	     (set! byte* byte)))
      unspecific)

    (define (close)
      (if byte*
	  (begin
	    (write-u8 byte* port)
	    (set! byte* #f)))
      (if marker-seen?
	  (begin
	    (write-u8 marker port)
	    (set! marker-seen? #f)))
      (close-output-port port))

    (make-non-channel-output-sink write-bytes close)))

;;;; BinHex 4.0 deconstruction

(define (make-binhex40-deconstructing-port port)
  (make-binary-port #f (make-binhex40-deconstructing-sink port)))

(define (make-binhex40-deconstructing-sink port)
  (let ((state 'reading-header)
	(header-length)
	(header #f)
	(index 0)
	(data-length))

    (define (write-bytes bytes start end)
      (do ((i start (fix:+ i 1)))
	  ((not (fix:< i end)))
	(case state
	  ((reading-header) (reading-header (bytevector-u8-ref bytes i)))
	  ((copying-data) (copying-data (bytevector-u8-ref bytes i)))
	  ((skipping-tail) (skipping-tail)))))

    (define (reading-header byte)
      (cond ((= index 0)
	     (set! header-length (+ 22 byte))
	     (set! header (make-bytevector header-length))
	     (set! index 1))
	    ((< index header-length)
	     (bytevector-u8-set! header index byte)
	     (set! index (+ index 1)))
	    (else
	     (set! data-length (read-data-length (fix:- header-length 10)))
	     (set! index 0)
	     (set! state 'copying-data))))

    (define (copying-data byte)
      (if (< index data-length)
	  (begin
	    (write-u8 byte port)
	    (set! index (+ index 1)))
	  (begin
	    (set! index 0)
	    (set! data-length (+ (read-data-length (fix:- header-length 6)) 4))
	    (set! state 'skipping-tail))))

    (define (skipping-tail)
      (if (< index data-length)
	  (set! index (+ index 1))
	  (set! state 'finished)))

    (define (read-data-length index)
      (+ (* (bytevector-u8-ref header index) #x1000000)
	 (* (bytevector-u8-ref header (+ index 1)) #x10000)
	 (* (bytevector-u8-ref header (+ index 2)) #x100)
	 (bytevector-u8-ref header (+ index 3))))

    (define (close)
      (close-output-port port))

    (make-non-channel-output-sink write-bytes close)))

;;;; Decode uuencode

(define (decode-uue:initialize port text?)
  text?
  (let ((state 'begin)
	(builder (string-builder))
	(output-buffer (make-bytevector 3)))

    (define (update string start end)
      (if (and (not (eq? state 'finished))
	       (fix:< start end))
	  (let ((nl (string-find-next-char string #\newline start end)))
	    (if nl
		(begin
		  (builder (string-slice string start nl))
		  (let ((line (builder 'immutable)))
		    (builder 'reset!)
		    (process-line line))
		  (update string (fix:+ nl 1) end))
		(builder (string-slice string start end))))))

    (define (process-line line)
      (if (not (fix:> (string-length line) 0))
	  (error:decode-uue "Empty line not allowed."))
      (case state
	((begin) (process-begin-line line))
	((normal) (process-normal-line line))
	((zero) (process-zero-line line))
	((end) (process-end-line line))
	(else (error "Illegal state in uuencode decoder:" state))))

    (define (process-begin-line line)
      (if (not (regsexp-match-string decode-uue:begin-line-regsexp line))
	  (error:decode-uue "Malformed \"begin\" line:" line))
      (set! state 'normal))

    (define (process-normal-line line)
      (let ((n (uudecode-char (string-ref line 0))))
	(if (not (and (fix:>= n 0)
		      (fix:<= n 45)
		      (fix:>= (fix:- (string-length line) 1)
			      (fix:* (fix:quotient (fix:+ n 2) 3) 4))))
	    (error:decode-uue "Malformed line length:" n))
	(let per-quantum ((i 0) (start 1))
	  (if (fix:< i n)
	      (let ((i* (fix:+ i 3)))
		(uudecode-quantum line start output-buffer)
		(if (fix:<= i* n)
		    (begin
		      (write-bytevector output-buffer port)
		      (per-quantum i* (fix:+ start 4)))
		    (write-bytevector output-buffer port 0 (fix:- n i))))))
	(cond ((fix:= n 0) (set! state 'end))
	      ((fix:< n 45) (set! state 'zero)))))

    (define (process-zero-line line)
      (let ((n (uudecode-char (string-ref line 0))))
	(if (not (fix:= n 0))
	    (error:decode-uue "Expected zero-length line:" n)))
      (set! state 'end))

    (define (process-end-line line)
      (if (not (string=? line "end"))
	  (error:decode-uue "Malformed \"end\" line:" line))
      (set! state 'finished))

    (define (finalize)
      (if (not (eq? state 'finished))
	  (error:decode-uue "Can't finalize unfinished decoding.")))

    (make-uudecode-ctx update finalize)))

(define (decode-uue:update context string #!optional start end)
  (let* ((caller 'decode-uu3:update)
	 (end (fix:end-index end (string-length string) caller))
	 (start (fix:start-index start end caller)))
    ((uudecode-ctx-update context) string start end)))

(define (decode-uue:finalize context)
  ((uudecode-ctx-finalize context)))

(define-record-type <uudecode-ctx>
    (make-uudecode-ctx update finalize)
    uudecode-ctx?
  (update uudecode-ctx-update)
  (finalize uudecode-ctx-finalize))

(define (uudecode-quantum string start buffer)
  (let ((n0 (uudecode-char (string-ref string start)))
	(n1 (uudecode-char (string-ref string (fix:+ start 1))))
	(n2 (uudecode-char (string-ref string (fix:+ start 2))))
	(n3 (uudecode-char (string-ref string (fix:+ start 3)))))
    (bytevector-u8-set! buffer 0
			(fix:or (fix:lsh n0 2)
				(fix:lsh n1 -4)))
    (bytevector-u8-set! buffer 1
			(fix:or (fix:lsh (fix:and n1 #x0F) 4)
				(fix:lsh n2 -2)))
    (bytevector-u8-set! buffer 2
			(fix:or (fix:lsh (fix:and n2 #x03) 6)
				n3))))

(define (uudecode-char char)
  (let ((n (char->integer char)))
    (if (not (and (fix:>= n #x20) (fix:< n #x80)))
	(error "Illegal uuencode char:" char))
    (fix:and (fix:- n #x20) #x3F)))

(define decode-uue:begin-line-regsexp
  (compile-regsexp
   '(seq (line-start)
	 "begin"
	 (+ #\space)
	 (+ (char-in (48 . 56)))
	 (+ #\space)
	 (+ (any-char))
	 (line-end))))

(define (call-with-decode-uue-output-port port text? generator)
  (let ((port (make-decode-uue-port port text?)))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define (make-decode-uue-port port text?)
  (make-textual-port decode-uue-port-type (decode-uue:initialize port text?)))

(define decode-uue-port-type
  (make-decoding-port-type decode-uue:update decode-uue:finalize))

(define condition-type:decode-uue
  (make-condition-type 'decode-uue condition-type:decode-mime '() #f))

(define error:decode-uue
  (let ((signal
	 (condition-signaller condition-type:decode-uue
			      '(message irritants)
			      standard-error-handler)))
    (lambda (message . irritants)
      (signal message irritants))))