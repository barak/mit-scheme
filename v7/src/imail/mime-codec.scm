;;; -*-Scheme-*-
;;;
;;; $Id: mime-codec.scm,v 1.3 2000/05/30 18:32:56 cph Exp $
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

(define (decode-base64-string string)
  (decode-base64-substring string 0 (string-length string)))

(define (decode-base64-substring string start end)
  (with-string-output-port
    (lambda (port)
      (let loop ((start start))
	(let ((i (substring-find-next-char string start end #\newline)))
	  (if i
	      (begin
		(decode-base64-line string start i port)
		(loop (fix:+ i 1)))
	      (decode-base64-line string start end port)))))))

(define (decode-base64-line string start end port)
  (let ((end (skip-lwsp-backwards string start end)))
    (if (not (let ((n (fix:- end start)))
	       (and (fix:<= n 76)
		    (fix:= 0 (fix:remainder n 4)))))
	(error:bad-range-argument end 'DECODE-BASE64-LINE))
    (let loop ((start start))
      (if (fix:< start end)
	  (begin
	    (decode-base64-quantum string start port)
	    (loop (fix:+ start 4)))))))

(define (decode-base64-quantum string start port)
  (let ((c1 (string-ref string start))
	(c2 (string-ref string (fix:+ start 1)))
	(c3 (string-ref string (fix:+ start 2)))
	(c4 (string-ref string (fix:+ start 3))))
    (if (char=? c4 #\=)
	(if (char=? c3 #\=)
	    (write-octet (fix:+ (fix:lsh (base64-char->digit c1) 2)
				(fix:lsh (base64-char->digit c2) -4))
			 port)
	    (let ((n
		   (fix:+ (fix:lsh (base64-char->digit c1) 10)
			  (fix:+ (fix:lsh (base64-char->digit c2) 4)
				 (fix:lsh (base64-char->digit c3) -2)))))
	      (write-octet (fix:lsh n -8) port)
	      (write-octet n port)))
	(let ((n
	       (fix:+ (fix:lsh (base64-char->digit c1) 18)
		      (fix:+ (fix:lsh (base64-char->digit c2) 12)
			     (fix:+ (fix:lsh (base64-char->digit c3) 6)
				    (base64-char->digit c4))))))
	  (write-octet (fix:lsh n -16) port)
	  (write-octet (fix:lsh n -8) port)
	  (write-octet n port)))))

(define-integrable (write-octet n port)
  (write-char (integer->char (fix:and n #xff)) port))

(define-integrable (base64-char->digit char)
  (vector-8b-ref base64-char-table (char->integer char)))

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