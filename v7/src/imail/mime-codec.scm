;;; -*-Scheme-*-
;;;
;;; $Id: mime-codec.scm,v 1.4 2000/06/01 18:21:07 cph Exp $
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

;;;; Decode quoted-printable

(define (decode-quoted-printable-string string)
  (decode-quoted-printable-substring string 0 (string-length string)))

(define (decode-quoted-printable-substring string start end)
  (with-string-output-port
    (lambda (port)
      (let loop ((start start))
	(let ((i (substring-find-next-char string start end #\newline)))
	  (if i
	      (begin
		(if (decode-quoted-printable-line string start i port)
		    (newline port))
		(loop (fix:+ i 1)))
	      (decode-quoted-printable-line string start end port)))))))

(define (decode-quoted-printable-line string start end port)
  (let ((end (skip-lwsp-backwards string start end)))
    (let loop ((start start))
      (let ((i
	     (substring-find-next-char-in-set
	      string start end char-set:qp-encoded)))
	(if i
	    (begin
	      (write-substring string start i port)
	      (cond ((decode-qp-hex-octet string i end)
		     => (lambda (char)
			  (write-char char port)
			  (loop (fix:+ i 3))))
		    ((char=? (string-ref string i) #\=)
		     (if (fix:< (fix:+ i 1) end)
			 ;; This case is illegal.  RFC 2045 recommends
			 ;; leaving it unconverted.
			 (begin
			   (write-char (string-ref string i) port)
			   (write-char (string-ref string (fix:+ i 1)) port)
			   (loop (fix:+ i 2)))
			 ;; Soft line break.
			 #f))
		    (else
		     ;; This case is illegal.  RFC 2045 recommends
		     ;; dropping the char altogether.
		     (loop (fix:+ i 1)))))
	    (begin
	      (write-substring string start end port)
	      ;; Hard line break.
	      #t))))))

(define (decode-qp-hex-octet string start end)
  (and (fix:<= (fix:+ start 3) end)
       (let ((d1 (char->digit (string-ref string (fix:+ start 1)) 16))
	     (d2 (char->digit (string-ref string (fix:+ start 2)) 16)))
	 (and d1 d2
	      (integer->char (fix:+ (fix:* 16 d1) d2))))))

(define char-set:qp-encoded
  (char-set-invert
   (char-set-union (char-set-difference (ascii-range->char-set #x21 #x7F)
					(char-set #\=))
		   char-set:lwsp)))

;;;; Decode BASE64

(define (decode-base64-binary-string string)
  (decode-base64-binary-substring string 0 (string-length string)))

(define (decode-base64-binary-substring string start end)
  (decode-base64-internal string start end
    (lambda (port)
      (lambda (char)
	(write-char char port)))))

(define (decode-base64-text-string string pending-return?)
  (decode-base64-substring string 0 (string-length string) pending-return?))

(define (decode-base64-text-substring string start end pending-return?)
  (decode-base64-internal string start end
    (lambda (port)
      (lambda (char)
	(if pending-return?
	    (case char
	      ((#\linefeed)
	       (set! pending-return? #f)
	       (newline port))
	      ((#\return)
	       (write-char #\return port))
	      (else
	       (set! pending-return? #f)
	       (write-char #\return port)))
	    (if (char=? char #\return)
		(set! pending-return? #t)
		(write-char char port))))))
  pending-return?)

(define (decode-base64-internal string start end make-output)
  (let ((input (string->input-port string start end)))
    (with-string-output-port
      (lambda (output)
	(let ((input
	       (lambda (index)
		 (let loop ((char (read-char port)))
		   (cond ((eof-object? char)
			  (if (not (fix:= index 0))
			      (error "Premature EOF from BASE64 port."))
			  #f)
			 ((let ((digit
				 (vector-8b-ref base64-char-table
						(char->integer char))))
			    (and (fix:< digit #x40)
				 digit)))
			 ((char=? char #\=)
			  (if (not (or (fix:= index 2) (fix:= index 3)))
			      (error "Misplaced #\= from BASE64 port."))
			  #f)
			 (else
			  (loop (read-char port)))))))
	      (output (make-output output)))
	  (let loop ()
	    (if (decode-base64-quantum input output)
		(loop))))))))

(define (decode-base64-quantum input output)
  (let ((d1 (input 0))
	(output
	 (lambda (n)
	   (output (integer->char (fix:and n #xff))))))
    (and d1
	 (let* ((d2 (input 1))
		(d3 (input 2))
		(d4 (input 3)))
	   (if d4
	       (if d3
		   (let ((n
			  (fix:+ (fix:+ (fix:lsh d1 18)
					(fix:lsh d2 12))
				 (fix:+ (fix:lsh d3 6)
					d4))))
		     (output (fix:lsh n -16))
		     (output (fix:lsh n -8))
		     (output n)
		     #t)
		   (error "Misplaced #\= from BASE64 port."))
	       (begin
		 (if d3
		     (let ((n
			    (fix:+ (fix:+ (fix:lsh d1 10)
					  (fix:lsh d2 4))
				   (fix:lsh d3 -2))))
		       (output (fix:lsh n -8))
		       (output n))
		     (output (fix:+ (fix:lsh d1 2)
				    (fix:lsh d2 -4))))
		 #f))))))

(define base64-char-table
  (let ((table (make-string 256 (integer->char #xff))))
    (define (do-range low high value)
      (vector-8b-set! table low value)
      (if (fix:< low high)
	  (do-range (fix:+ low 1) high (fix:+ value 1))))
    (do-range (char->integer #\A) (char->integer #\Z) 0)
    (do-range (char->integer #\a) (char->integer #\z) 26)
    (do-range (char->integer #\0) (char->integer #\9) 52)
    (vector-8b-set! table (char->integer #\+) 62)
    (vector-8b-set! table (char->integer #\/) 63)
    table))