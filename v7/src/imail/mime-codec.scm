;;; -*-Scheme-*-
;;;
;;; $Id: mime-codec.scm,v 1.8 2000/06/07 18:37:25 cph Exp $
;;;
;;; Copyright (c) 2000 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; IMAIL mail reader: MIME support

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
  (let ((port (qp-encoding-context/port context))
	(text? (qp-encoding-context/text? context)))
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
	     (write-qp-hard-break context))))))

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
  ;; Either #F, or a string.  If a string, the string will entirely
  ;; consist of LWSP characters.  This is whitespace that appeared at
  ;; the end of an input packet.  We are waiting to see if it is
  ;; followed by a newline, meaning it is to be discarded, or
  ;; otherwise is part of the output.
  (pending #f)
  ;; Either #F, 'EQUALS, or a character.  If not #F, it indicates that
  ;; a packet ended with an unfinished = sequence that we can't decode
  ;; until we get more characters.  The symbol 'EQUALS means we saw
  ;; the equals sign but nothing else.  A character means we saw the
  ;; equals sign and that character.
  (partial #f))

(define (decode-quoted-printable:finalize context)
  (decode-qp-pending context 'INPUT-END))

(define (decode-quoted-printable:update context string start end)
  (let loop ((start start))
    (if (fix:< start end)
	(let ((i (substring-find-next-char string start end #\newline)))
	  (if i
	      (begin
		(let ((i (skip-lwsp-backwards string start i)))
		  (cond ((fix:< start i)
			 (decode-qp-pending context 'PARTIAL)
			 (decode-qp context string start i 'LINE-END))
			((not (decode-qp-pending context 'LINE-END))
			 (decode-qp context "" 0 0 'LINE-END))))
		(loop (fix:+ i 1)))
	      (let ((end* (skip-lwsp-backwards string start end)))
		(if (fix:< start end*)
		    (begin
		      (decode-qp-pending context 'PARTIAL)
		      (decode-qp context string start end* 'PARTIAL)))
		(if (fix:< end* end)
		    (set-qp-decoding-context/pending!
		     context
		     (let ((string (substring string end* end))
			   (pending (qp-decoding-context/pending context)))
		       (if pending
			   (string-append pending string)
			   string))))))))))

(define (decode-qp-pending context type)
  (let ((pending (qp-decoding-context/pending context)))
    (and pending
	 (begin
	   (set-qp-decoding-context/pending! context #f)
	   (decode-qp context pending 0
		      (if (eq? type 'PARTIAL) (string-length pending) 0)
		      type)
	   #t))))

(define (decode-qp context string start end type)
  (let ((port (qp-decoding-context/port context)))
    (let loop ((start (decode-qp-partial context string start end type)))
      (let ((i
	     (substring-find-next-char-in-set
	      string start end char-set:qp-encoded)))
	(if i
	    (begin
	      (if (fix:< start i)
		  (write-substring string start i port))
	      (cond ((not (char=? (string-ref string i) #\=))
		     ;; This case is illegal.  RFC 2045 recommends
		     ;; dropping the char altogether.
		     (loop (fix:+ i 1)))
		    ((fix:< (fix:+ i 2) end)
		     (loop
		      (fix:+ (fix:+ i 1)
			     (decode-qp-hex-octet
			      context
			      (string-ref string (fix:+ i 1))
			      (string-ref string (fix:+ i 2))))))
		    ((eq? type 'PARTIAL)
		     (set-qp-decoding-context/partial!
		      context
		      (if (fix:< (fix:+ i 1) end)
			  (string-ref string (fix:+ i 1))
			  'EQUALS)))
		    ((fix:< (fix:+ i 1) end)
		     ;; This case is illegal.  RFC 2045 recommends
		     ;; leaving it unconverted.
		     (write-char #\= port)
		     (write-char (string-ref string (fix:+ i 1)) port))
		    ((eq? type 'INPUT-END)
		     ;; This case is illegal.  RFC 2045 recommends
		     ;; leaving it unconverted.
		     (write-char #\= port))
		    (else
		     ;; This is a soft line break.
		     unspecific)))
	    (begin
	      (if (fix:< start end)
		  (write-substring string start end port))
	      (if (eq? type 'LINE-END)
		  (if (qp-decoding-context/text? context)
		      (if (eq? (qp-decoding-context/partial context) 'EQUALS)
			  ;; This is a soft line break.
			  (set-qp-decoding-context/partial! context #f)
			  ;; This is a hard line break.
			  (newline port))
		      ;; I think this is illegal (RFC 2045 doesn't
		      ;; say).  Most sensible thing to do is treat it
		      ;; like a soft line break.
		      unspecific))))))))

(define char-set:qp-encoded
  (char-set-invert
   (char-set-union (char-set-difference (ascii-range->char-set #x21 #x7F)
					(char-set #\=))
		   char-set:lwsp)))

(define (decode-qp-partial context string start end type)
  (let ((partial (qp-decoding-context/partial context)))
    (cond ((not (and partial (fix:< start end)))
	   (if (and partial (not (eq? type 'PARTIAL)))
	       (let ((port (qp-decoding-context/port context)))
		 ;; If PARTIAL is a character, this is illegal.
		 ;; Otherwise, this is a soft line break.
		 (cond ((char? partial)
			;; Illegal.
			(write-char #\= port)
			(write-char partial port)
			(set-qp-decoding-context/partial! context #f))
		       ((eq? type 'INPUT-END)
			;; Illegal.
			(write-char #\= port)
			(set-qp-decoding-context/partial! context #f))
		       (else
			;; Soft line break.
			unspecific))))
	   start)
	  ((eq? partial 'EQUALS)
	   (if (fix:< (fix:+ start 1) end)
	       (begin
		 (set-qp-decoding-context/partial! context #f)
		 (fix:+ start
			(decode-qp-hex-octet
			 context
			 (string-ref string start)
			 (string-ref string (fix:+ start 1)))))
	       (begin
		 (set-qp-decoding-context/partial! context
						   (string-ref string start))
		 (fix:+ start 1))))
	  (else
	   (set-qp-decoding-context/partial! context #f)
	   (fix:+ start
		  (fix:- (decode-qp-hex-octet context
					      partial
					      (string-ref string start))
			 1))))))

(define (decode-qp-hex-octet context c1 c2)
  (let ((port (qp-decoding-context/port context)))
    (let ((char
	   (let ((d1 (char->digit c1 16))
		 (d2 (char->digit c2 16)))
	     (and d1 d2
		  (integer->char (fix:or (fix:lsh d1 4) d2))))))
      (if char
	  (begin
	    (write-char char port)
	    2)
	  ;; This case is illegal.  RFC 2045 recommends
	  ;; leaving it unconverted.
	  (begin
	    (write-char #\= port)
	    (write-char c1 port)
	    1)))))

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

(define-structure (base64-decoding-context
		   (conc-name base64-decoding-context/)
		   (constructor decode-base64:initialize (port text?)))
  (port #f read-only #t)
  (text? #f read-only #t)
  (input-buffer (make-string 4) read-only #t)
  (input-index 0)
  (output-buffer (make-string 3) read-only #t)
  (pending-return? #f))

(define (decode-base64:finalize context)
  (if (fix:> (base64-decoding-context/input-index context) 0)
      (error "BASE64 input length is not a multiple of 4."))
  (if (base64-decoding-context/pending-return? context)
      (write-char #\return (base64-decoding-context/port context))))

(define (decode-base64:update context string start end)
  (let ((buffer (base64-decoding-context/input-buffer context)))
    (let loop
	((start start)
	 (index (base64-decoding-context/input-index context)))
      (if (fix:< start end)
	  (let ((char (string-ref string start))
		(start (fix:+ start 1)))
	    (if (or (char=? char #\=)
		    (fix:< (vector-8b-ref base64-char-table
					  (char->integer char))
			   #x40))
		(begin
		  (string-set! buffer index char)
		  (if (fix:< index 3)
		      (loop start (fix:+ index 1))
		      (begin
			(decode-base64-quantum context)
			(loop start 0))))
		(loop start index)))
	  (set-base64-decoding-context/input-index! context index)))))

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