;;; -*-Scheme-*-
;;;
;;; $Id: mime-codec.scm,v 1.1 2000/05/26 18:45:44 cph Exp $
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

(define (decode-quoted-printable-line line start end port)
  (let ((end
	 (let loop ((end end))
	   (if (and (fix:< start end)
		    (char-lwsp? (string-ref line (fix:- end 1))))
	       (loop (fix:- end 1))
	       end))))
    (let loop ((start start))
      (let ((i
	     (substring-find-next-char-in-set
	      line start end char-set:qp-encoded)))
	(if i
	    (begin
	      (write-substring line start i port)
	      (cond ((decode-qp-hex-octet line i end)
		     => (lambda (char)
			  (write-char char port)
			  (loop (fix:+ i 3))))
		    ((char=? (string-ref line i) #\=)
		     (if (fix:< (fix:+ i 1) end)
			 ;; This case is illegal.  RFC 2045 recommends
			 ;; leaving it unconverted.
			 (begin
			   (write-char (string-ref line i) port)
			   (write-char (string-ref line (fix:+ i 1)) port)
			   (loop (fix:+ i 2)))
			 ;; Soft line break.
			 #f))
		    (else
		     ;; This case is illegal.  RFC 2045 recommends
		     ;; dropping the char altogether.
		     (loop (fix:+ i 1)))))
	    (begin
	      (write-substring line start end port)
	      ;; Hard line break.
	      #t))))))

(define (decode-qp-hex-octet string start end)
  (and (fix:<= (fix:+ start 3) end)
       (let ((d1 (char->digit (string-ref string (fix:+ start 1)) 16))
	     (d2 (char->digit (string-ref string (fix:+ start 2)) 16)))
	 (and d1 d2
	      (integer->char (fix:+ (fix:* 4 d1) d2))))))

(define char-set:qp-encoded
  (char-set-invert
   (char-set-union (char-set-difference (ascii-range->char-set #x21 #x7F)
					(char-set #\=))
		   char-set:lwsp)))