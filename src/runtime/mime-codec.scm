#| -*-Scheme-*-

$Id: mime-codec.scm,v 14.14 2003/02/14 18:28:33 cph Exp $

Copyright 2000, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; MIME support

(declare (usual-integrations))

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
  (encode-qp-pending-lwsp context #f 'INPUT-END)
  (write-qp-pending-output context #t))

(define (encode-quoted-printable:update context string start end)
  (if (qp-encoding-context/text? context)
      (let loop ((start start))
	(let ((i (substring-find-next-char string start end #\newline)))
	  (if i
	      (begin
		(encode-qp context string start i 'LINE-END)
		(loop (fix:+ i 1)))
	      (encode-qp context string start end 'PARTIAL))))
      (encode-qp context string start end 'PARTIAL)))

(define (encode-qp context string start end type)
  (encode-qp-pending-lwsp context (fix:< start end) type)
  (let loop ((start start))
    (cond ((fix:< start end)
	   (let ((char (string-ref string start))
		 (start (fix:+ start 1)))
	     (cond ((not (char-lwsp? char))
		    (if (char-set-member? char-set:qp-encoded char)
			(write-qp-encoded context char)
			(write-qp-clear context char))
		    (loop start))
		   ((and (eq? type 'PARTIAL)
			 (not (fix:< start end)))
		    (set-qp-encoding-context/pending-lwsp! context char))
		   (else
		    (if (fix:< start end)
			(write-qp-clear context char)
			(write-qp-encoded context char))
		    (loop start)))))
	  ((eq? type 'LINE-END)
	   (write-qp-hard-break context)))))

(define (encode-qp-pending-lwsp context packet-not-empty? type)
  (let ((pending (qp-encoding-context/pending-lwsp context)))
    (if pending
	(cond (packet-not-empty?
	       (set-qp-encoding-context/pending-lwsp! context #f)
	       (write-qp-clear context pending))
	      ((not (eq? type 'PARTIAL))
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
    (let ((c1 (hex-digit->char (fix:lsh d -4)))
	  (c2 (hex-digit->char (fix:and d #x0F))))
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

(define (call-with-decode-quoted-printable-output-port port text? generator)
  (let ((port (make-decode-quoted-printable-port port text?)))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define (make-decode-quoted-printable-port port text?)
  (make-port decode-quoted-printable-port-type
	     (decode-quoted-printable:initialize port text?)))

(define decode-quoted-printable-port-type
  (make-port-type
   `((WRITE-SUBSTRING
      ,(lambda (port string start end)
	 (decode-quoted-printable:update (port/state port) string start end)))
     (CLOSE-OUTPUT
      ,(lambda (port)
	 (decode-quoted-printable:finalize (port/state port)))))
   #f))

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
  (decode-qp context "" 0 0 'INPUT-END))

(define (decode-quoted-printable:update context string start end)
  (let loop ((start start))
    (let ((i (substring-find-next-char string start end #\newline)))
      (if i
	  (begin
	    (decode-qp context
		       string start (skip-lwsp-backwards string start i)
		       'LINE-END)
	    (loop (fix:+ i 1)))
	  (decode-qp context string start end 'PARTIAL)))))

(define (decode-qp context string start end type)
  (let ((port (qp-decoding-context/port context))
	(end* (skip-lwsp-backwards string start end)))

    (define (loop start)
      (let ((i
	     (substring-find-next-char-in-set string start end*
					      char-set:qp-encoded)))
	(if i
	    (begin
	      (write-substring string start i port)
	      (if (char=? (string-ref string i) #\=)
		  (handle-equals (fix:+ i 1))
		  ;; RFC 2045 recommends dropping illegal encoded char.
		  (loop (fix:+ i 1))))
	    (begin
	      (write-substring string start end* port)
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
		  (if (char-hex-digit? char)
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
	(cond ((eq? type 'PARTIAL)
	       (set-qp-decoding-context/pending!
		context
		(decode-qp-pending-string pending string end* end)))
	      ((not pending)
	       (if (eq? type 'LINE-END)
		   ;; Hard line break.
		   (newline port)))
	      ((eqv? pending #\=)
	       (if (eq? type 'LINE-END)
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
	  (let ((s
		 (make-string
		  (fix:+ (string-length pending) (fix:- end start)))))
	    (substring-move! string start end
			     s (string-move! pending s 0))
	    s)
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
	   (let ((d1 (char->hex-digit c1))
		 (d2 (char->hex-digit c2)))
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

(define-integrable (char-hex-digit? char)
  (fix:< (char->hex-digit char) #x10))

(define-integrable (char->hex-digit char)
  (vector-8b-ref hex-char-table (char->integer char)))

(define-integrable (hex-digit->char digit)
  (string-ref hex-digit-table digit))

(define hex-char-table)
(define hex-digit-table)
(let ((char-table (make-string 256 (integer->char #xff)))
      (digit-table (make-string 16)))
  (define (do-range low high value)
    (do-char low value)
    (if (fix:< low high)
	(do-range (fix:+ low 1) high (fix:+ value 1))))
  (define (do-char code value)
    (vector-8b-set! char-table code value)
    (vector-8b-set! digit-table value code))
  (do-range (char->integer #\0) (char->integer #\9) 0)
  (do-range (char->integer #\a) (char->integer #\f) 10)
  (do-range (char->integer #\A) (char->integer #\F) 10)
  (set! hex-char-table char-table)
  (set! hex-digit-table digit-table)
  unspecific)

;;;; Encode BASE64

(define-structure (base64-encoding-context
		   (conc-name base64-encoding-context/)
		   (constructor encode-base64:initialize (port text?)))
  (port #f read-only #t)
  (text? #f read-only #t)
  (buffer (make-string 48) read-only #t)
  (index 0))

(define (encode-base64:finalize context)
  (write-base64-line context))

(define (encode-base64:update context string start end)
  (if (base64-encoding-context/text? context)
      (let loop ((start start))
	(let ((index (substring-find-next-char string start end #\newline)))
	  (if index
	      (begin
		(encode-base64 context string start index)
		(encode-base64 context "\r\n" 0 2)
		(loop (fix:+ index 1)))
	      (encode-base64 context string start end))))
      (encode-base64 context string start end)))

(define (encode-base64 context string start end)
  (let ((buffer (base64-encoding-context/buffer context)))
    (let loop ((start start))
      (if (fix:< start end)
	  (let ((i (base64-encoding-context/index context)))
	    (let ((start* (fix:min end (fix:+ start (fix:- 48 i)))))
	      (let ((i (substring-move! string start start* buffer i)))
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
		   (write-char (string-ref base64-digit-table (fix:and #x3F d))
			       port))))
	    (let loop ((start 0))
	      (let ((n (fix:- end start)))
		(cond ((fix:>= n 3)
		       (let ((d1 (vector-8b-ref buffer start))
			     (d2 (vector-8b-ref buffer (fix:+ start 1)))
			     (d3 (vector-8b-ref buffer (fix:+ start 2))))
			 (write-digit (fix:lsh d1 -2))
			 (write-digit (fix:or (fix:lsh d1 4) (fix:lsh d2 -4)))
			 (write-digit (fix:or (fix:lsh d2 2) (fix:lsh d3 -6)))
			 (write-digit d3))
		       (loop (fix:+ start 3)))
		      ((fix:= n 2)
		       (let ((d1 (vector-8b-ref buffer start))
			     (d2 (vector-8b-ref buffer (fix:+ start 1))))
			 (write-digit (fix:lsh d1 -2))
			 (write-digit (fix:or (fix:lsh d1 4) (fix:lsh d2 -4)))
			 (write-digit (fix:lsh d2 2)))
		       (write-char #\= port))
		      ((fix:= n 1)
		       (let ((d1 (vector-8b-ref buffer start)))
			 (write-digit (fix:lsh d1 -2))
			 (write-digit (fix:lsh d1 4)))
		       (write-char #\= port)
		       (write-char #\= port))))))
	  (newline port)
	  (set-base64-encoding-context/index! context 0)))))

;;;; Decode BASE64

(define (call-with-decode-base64-output-port port text? generator)
  (let ((port (make-decode-base64-port port text?)))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define (make-decode-base64-port port text?)
  (make-port decode-base64-port-type (decode-base64:initialize port text?)))

(define decode-base64-port-type
  (make-port-type
   `((WRITE-SUBSTRING
      ,(lambda (port string start end)
	 (decode-base64:update (port/state port) string start end)))
     (CLOSE-OUTPUT
      ,(lambda (port)
	 (decode-base64:finalize (port/state port)))))
   #f))

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
  (input-state 'LINE-START)
  (output-buffer (make-string 3) read-only #t)
  (pending-return? #f))

(define (decode-base64:finalize context)
  (if (fix:> (base64-decoding-context/input-index context) 0)
      (error "BASE64 input length is not a multiple of 4."))
  (if (base64-decoding-context/pending-return? context)
      (write-char #\return (base64-decoding-context/port context))))

(define (decode-base64:update context string start end)
  (if (not (eq? 'FINISHED (base64-decoding-context/input-state context)))
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
				    'LINE-START
				    'IN-LINE)))))
		  (if (or (char=? char #\=)
			  (fix:< (vector-8b-ref base64-char-table
						(char->integer char))
				 #x40))
		      (begin
			(string-set! buffer index char)
			(if (fix:< index 3)
			    (continue (fix:+ index 1))
			    (begin
			      (decode-base64-quantum context)
			      (continue 0))))
		      (if (eq? state 'LINE-START)
			  (done 'FINISHED)
			  (continue index))))
		(done state)))))))

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
		(let ((char (string-ref output index)))
		  (if pending?
		      (if (char=? char #\linefeed)
			  (begin
			    (newline port)
			    (loop (fix:+ index 1) #f))
			  (begin
			    (write-char #\return port)
			    (loop index #f)))
		      (if (char=? char #\return)
			  (loop (fix:+ index 1) #t)
			  (begin
			    (write-char char port)
			    (loop (fix:+ index 1) #f)))))
		(set-base64-decoding-context/pending-return?! context
							      pending?)))
	  (write-substring output 0 n port)))))

(define (decode-base64-quantum-1 input output)
  (let ((d1 (decode-base64-char input 0))
	(d2 (decode-base64-char input 1)))
    (cond ((not (char=? (string-ref input 3) #\=))
	   (let ((n
		  (fix:+ (fix:+ (fix:lsh d1 18)
				(fix:lsh d2 12))
			 (fix:+ (fix:lsh (decode-base64-char input 2) 6)
				(decode-base64-char input 3)))))
	     (vector-8b-set! output 0 (fix:lsh n -16))
	     (vector-8b-set! output 1 (fix:and #xFF (fix:lsh n -8)))
	     (vector-8b-set! output 2 (fix:and #xFF n))
	     3))
	  ((not (char=? (string-ref input 2) #\=))
	   (let ((n
		  (fix:+ (fix:+ (fix:lsh d1 10) (fix:lsh d2 4))
			 (fix:lsh (decode-base64-char input 2) -2))))
	     (vector-8b-set! output 0 (fix:lsh n -8))
	     (vector-8b-set! output 1 (fix:and #xFF n)))
	   2)
	  (else
	   (vector-8b-set! output 0 (fix:+ (fix:lsh d1 2) (fix:lsh d2 -4)))
	   1))))

(define (decode-base64-char input index)
  (let ((digit (vector-8b-ref base64-char-table (vector-8b-ref input index))))
    (if (fix:> digit #x40)
	(error "Misplaced #\= in BASE64 input."))
    digit))

(define base64-char-table)
(define base64-digit-table)
(let ((char-table (make-string 256 (integer->char #xff)))
      (digit-table (make-string 64)))
  (define (do-range low high value)
    (do-char low value)
    (if (fix:< low high)
	(do-range (fix:+ low 1) high (fix:+ value 1))))
  (define (do-char code value)
    (vector-8b-set! char-table code value)
    (vector-8b-set! digit-table value code))
  (do-range (char->integer #\A) (char->integer #\Z) 0)
  (do-range (char->integer #\a) (char->integer #\z) 26)
  (do-range (char->integer #\0) (char->integer #\9) 52)
  (do-char (char->integer #\+) 62)
  (do-char (char->integer #\/) 63)
  (set! base64-char-table char-table)
  (set! base64-digit-table digit-table)
  unspecific)

;;;; Decode BinHex 4.0

(define (call-with-decode-binhex40-output-port port text? generator)
  (let ((port (make-decode-binhex40-port port text?)))
    (let ((v (generator port)))
      (close-output-port port)
      v)))

(define (make-decode-binhex40-port port text?)
  (make-port decode-binhex40-port-type
	     (decode-binhex40:initialize port text?)))

(define decode-binhex40-port-type
  (make-port-type
   `((WRITE-SUBSTRING
      ,(lambda (port string start end)
	 (decode-binhex40:update (port/state port) string start end)))
     (CLOSE-OUTPUT
      ,(lambda (port)
	 (decode-binhex40:finalize (port/state port)))))
   #f))

(define-structure (binhex40-decoding-context
		   (conc-name binhex40-decoding-context/)
		   (constructor make-binhex40-decoding-context (port)))
  (port #f read-only #t)
  (state 'SEEKING-COMMENT)
  (line-buffer "")
  (input-buffer (make-string 4) read-only #t)
  (input-index 0)
  (output-buffer (make-string 3) read-only #t))

(define (decode-binhex40:initialize port text?)
  text?					;ignored
  (make-binhex40-decoding-context
   (make-binhex40-run-length-decoding-port
    (make-binhex40-deconstructing-port port))))

(define (decode-binhex40:finalize context)
  (let ((state (binhex40-decoding-context/state context)))
    (case (binhex40-decoding-context/state context)
      ((SEEKING-COMMENT)
       (error "Missing BinHex 4.0 initial comment line."))
      ((DECODING)
       (error "Missing BinHex 4.0 terminating character."))
      ((IGNORING)
       (close-output-port (binhex40-decoding-context/port context)))
      (else
       (error "Illegal decoder state:" state)))))

(define (decode-binhex40:update context string start end)
  (let ((state (binhex40-decoding-context/state context)))
    (case (binhex40-decoding-context/state context)
      ((SEEKING-COMMENT)
       (decode-binhex40-seeking-comment context string start end))
      ((DECODING)
       (decode-binhex40-decoding context string start end))
      ((IGNORING)
       unspecific)
      (else
       (error "Illegal decoder state:" state)))))

(define (decode-binhex40-seeking-comment context string start end)
  (let loop
      ((s
	(string-append (binhex40-decoding-context/line-buffer context)
		       (substring string start end))))
    (let ((regs (re-string-match binhex40-header-regexp s)))
      (if regs
	  (begin
	    (set-binhex40-decoding-context/state! context 'DECODING)
	    (set-binhex40-decoding-context/line-buffer! context #f)
	    (decode-binhex40:update context s
				    (re-match-end-index 0 regs)
				    (string-length s)))
	  (set-binhex40-decoding-context/line-buffer! context s)))))

(define binhex40-header-regexp
  "[\r\n\t ]*(This file must be converted with BinHex.*[\r\n][\r\n\t ]*:")

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
		   (set-binhex40-decoding-context/state! context 'IGNORING))
		  ((fix:< (vector-8b-ref binhex40-char-table
					 (char->integer char))
			  #x40)
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
    (write-substring output 0
		     (decode-binhex40-quantum-1 input output)
		     port)))

(define (decode-binhex40-quantum-1 input output)
  (let ((d1 (decode-binhex40-char input 0))
	(d2 (decode-binhex40-char input 1)))
    (cond ((char=? (string-ref input 2) #\:)
	   (vector-8b-set! output 0 (fix:+ (fix:lsh d1 2) (fix:lsh d2 -4)))
	   1)
	  ((char=? (string-ref input 3) #\:)
	   (let ((n
		  (fix:+ (fix:+ (fix:lsh d1 10) (fix:lsh d2 4))
			 (fix:lsh (decode-binhex40-char input 2) -2))))
	     (vector-8b-set! output 0 (fix:lsh n -8))
	     (vector-8b-set! output 1 (fix:and #xFF n)))
	   2)
	  (else
	   (let ((n
		  (fix:+ (fix:+ (fix:lsh d1 18)
				(fix:lsh d2 12))
			 (fix:+ (fix:lsh (decode-binhex40-char input 2) 6)
				(decode-binhex40-char input 3)))))
	     (vector-8b-set! output 0 (fix:lsh n -16))
	     (vector-8b-set! output 1 (fix:and #xFF (fix:lsh n -8)))
	     (vector-8b-set! output 2 (fix:and #xFF n))
	     3)))))

(define (decode-binhex40-char input index)
  (let ((digit
	 (vector-8b-ref binhex40-char-table (vector-8b-ref input index))))
    (if (fix:> digit #x40)
	(error "Illegal character in BinHex 4.0 input stream:"
	       (string-ref input index)))
    digit))

(define binhex40-digit-table
  "!\"#$%&\'()*+,-012345689@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdefhijklmpqr")

(define binhex40-char-table
  (make-string 256 (integer->char #xff)))

(do ((code 0 (fix:+ code 1)))
    ((fix:= code 64))
  (vector-8b-set! binhex40-char-table
		  (vector-8b-ref binhex40-digit-table code)
		  code))

;;;; BinHex 4.0 run-length decoding

(define (make-binhex40-run-length-decoding-port port)
  (make-port binhex40-run-length-decoding-port-type
	     (make-binhex40-rld-state port)))

(define binhex40-run-length-decoding-port-type
  (make-port-type
   `((WRITE-CHAR
      ,(lambda (port char)
	 (let ((state (port/state port)))
	   (let ((port (binhex40-rld-state/port state))
		 (char* (binhex40-rld-state/char state)))
	     (cond ((binhex40-rld-state/marker-seen? state)
		    (let ((n (char->integer char)))
		      (cond ((fix:= n 0)
			     (if char* (write-char char* port))
			     (set-binhex40-rld-state/char!
			      state binhex40-rld-marker))
			    (char*
			     (do ((i 0 (fix:+ i 1)))
				 ((fix:= i n))
			       (write-char char* port))
			     (set-binhex40-rld-state/char! state #f))))
		    (set-binhex40-rld-state/marker-seen?! state #f))
		   ((char=? char binhex40-rld-marker)
		    (set-binhex40-rld-state/marker-seen?! state #t))
		   (else
		    (if char* (write-char char* port))
		    (set-binhex40-rld-state/char! state char)))))))
     (CLOSE-OUTPUT
      ,(lambda (port)
	 (let ((state (port/state port)))
	   (let ((port (binhex40-rld-state/port state))
		 (char* (binhex40-rld-state/char state)))
	     (if char*
		 (begin
		   (write-char char* port)
		   (set-binhex40-rld-state/char! state #f)))
	     (if (binhex40-rld-state/marker-seen? state)
		 (begin
		   (write-char binhex40-rld-marker port)
		   (set-binhex40-rld-state/marker-seen?! state #f)))
	     (close-output-port port))))))
   #f))

(define-structure (binhex40-rld-state
		   (conc-name binhex40-rld-state/)
		   (constructor make-binhex40-rld-state (port)))
  (port #f read-only #t)
  (char #f)
  (marker-seen? #f))

(define-integrable binhex40-rld-marker
  (integer->char #x90))

;;;; BinHex 4.0 deconstruction

(define (make-binhex40-deconstructing-port port)
  (make-port binhex40-deconstructing-port-type
	     (make-binhex40-decon port)))

(define binhex40-deconstructing-port-type
  (make-port-type
   `((WRITE-CHAR
      ,(lambda (port char)
	 (case (binhex40-decon/state (port/state port))
	   ((READING-HEADER) (binhex40-decon-reading-header port char))
	   ((COPYING-DATA) (binhex40-decon-copying-data port char))
	   ((SKIPPING-TAIL) (binhex40-decon-skipping-tail port))
	   ((FINISHED) unspecific)
	   (else (error "Illegal state in BinHex 4.0 deconstructor.")))))
     (CLOSE-OUTPUT
      ,(lambda (port)
	 (if (not (eq? (binhex40-decon/state (port/state port)) 'FINISHED))
	     (error "Premature EOF in BinHex 4.0 stream.")))))
   #f))

(define (binhex40-decon-reading-header port char)
  (let ((state (port/state port)))
    (let ((index (binhex40-decon/index state)))
      (if (fix:= index 0)
	  (begin
	    (set-binhex40-decon/header!
	     state (make-string (fix:+ 22 (char->integer char))))
	    (set-binhex40-decon/index! state 1))
	  (let ((header (binhex40-decon/header state)))
	    (string-set! header index char)
	    (let ((index (fix:+ index 1)))
	      (if (fix:< index (string-length header))
		  (set-binhex40-decon/index! state index)
		  (begin
		    (set-binhex40-decon/data-length!
		     state
		     (binhex40-4byte header (fix:- (string-length header) 10)))
		    (set-binhex40-decon/index! state 0)
		    (set-binhex40-decon/state! state 'COPYING-DATA)))))))))

(define (binhex40-decon-copying-data port char)
  (let ((state (port/state port)))
    (write-char char (binhex40-decon/port state))
    (let ((index (+ (binhex40-decon/index state) 1)))
      (if (< index (binhex40-decon/data-length state))
	  (set-binhex40-decon/index! state index)
	  (begin
	    (set-binhex40-decon/index! state 0)
	    (set-binhex40-decon/data-length!
	     state
	     (+ (let ((header (binhex40-decon/header state)))
		  (binhex40-4byte header (fix:- (string-length header) 6)))
		4))
	    (set-binhex40-decon/state! state 'SKIPPING-TAIL))))))

(define (binhex40-decon-skipping-tail port)
  (let ((state (port/state port)))
    (let ((index (+ (binhex40-decon/index state) 1)))
      (set-binhex40-decon/index! state index)
      (if (>= index (binhex40-decon/data-length state))
	  (set-binhex40-decon/state! state 'FINISHED)))))

(define-structure (binhex40-decon (conc-name binhex40-decon/)
				  (constructor make-binhex40-decon (port)))
  (port #f read-only #t)
  (state 'READING-HEADER)
  (header #f)
  (index 0)
  (data-length))

(define (binhex40-4byte string index)
  (+ (* (vector-8b-ref string index) #x1000000)
     (* (vector-8b-ref string (fix:+ index 1)) #x10000)
     (* (vector-8b-ref string (fix:+ index 2)) #x100)
     (vector-8b-ref string (fix:+ index 3))))