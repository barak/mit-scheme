;;; -*-Scheme-*-
;;;
;;; $Id: url.scm,v 1.8 2000/07/02 05:09:21 cph Exp $
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

;;;; URL Encoding

(declare (usual-integrations))

(define url:char-set:safe (string->char-set "$-_.+"))
(define url:char-set:extra (string->char-set "!*'(),"))
(define url:char-set:national (string->char-set "{}|\\^~[]`"))
(define url:char-set:punctuation (string->char-set "<>#%\""))
(define url:char-set:reserved (string->char-set ";/?:@&="))

(define url:char-set:unreserved
  (char-set-union char-set:alphanumeric
		  url:char-set:safe
		  url:char-set:extra))

(define url:char-set:unescaped
  (char-set-union url:char-set:unreserved
		  url:char-set:reserved))

(define url:char-set:escaped
  (char-set-invert url:char-set:unescaped))

(define url:rexp:escape
  (let ((char-set:hex (string->char-set "0123456789ABCDEFabcdef")))
    (rexp-sequence "%" char-set:hex char-set:hex)))

(define url:rexp:uchar
  (rexp-alternatives url:char-set:unreserved url:rexp:escape))

(define url:rexp:xchar
  (rexp-alternatives url:char-set:unescaped url:rexp:escape))

(define url:rexp:hostname
  (let ((tail
	 (rexp-optional
	  (rexp*
	   (char-set-union char-set:alphanumeric (string->char-set "-")))
	  char-set:alphanumeric)))
    (rexp-sequence (rexp* char-set:alphanumeric tail ".")
		   char-set:alphabetic
		   tail)))

(define url:rexp:hostnumber
  (let ((n (rexp+ char-set:numeric)))
    (rexp-sequence n "." n "." n "." n)))

(define url:rexp:host
  (rexp-alternatives url:rexp:hostname url:rexp:hostnumber))

(define url:rexp:hostport
  (rexp-sequence url:rexp:host (rexp-optional ":" (rexp+ char-set:numeric))))

(define (url:string-encoded? string)
  (url:substring-encoded? string 0 (string-length string)))

(define (url:encode-string string)
  (url:encode-substring string 0 (string-length string)))

(define (url:decode-string string)
  (url:decode-substring string 0 (string-length string)))

(define url:substring-encoded?
  (let ((pattern (rexp-compile url:rexp:xchar)))
    (lambda (string start end)
      (let ((regs (re-substring-match pattern string start end)))
	(and regs
	     (fix:= end (re-match-end-index 0 regs)))))))

(define (url:encode-substring string start end)
  (let ((n-to-encode
	 (let loop ((start start) (n-to-encode 0))
	   (let ((index
		  (substring-find-next-char-in-set string start end
						   url:char-set:escaped)))
	     (if index
		 (loop (fix:+ index 1) (fix:+ n-to-encode 1))
		 n-to-encode)))))
    (if (fix:= 0 n-to-encode)
	(substring string start end)
	(let ((encoded
	       (make-string (fix:+ (fix:- end start) (fix:* 2 n-to-encode))))
	      (digits "0123456789ABCDEF"))
	  (let loop ((start start) (i 0))
	    (let ((index
		   (substring-find-next-char-in-set string start end
						    url:char-set:escaped)))
	      (if index
		  (begin
		    (substring-move! string start index encoded i)
		    (let ((i (fix:+ i (fix:- index start)))
			  (code (vector-8b-ref string index)))
		      (string-set! encoded i #\%)
		      (string-set! encoded
				   (fix:+ i 1)
				   (string-ref digits (fix:lsh code -4)))
		      (string-set! encoded
				   (fix:+ i 2)
				   (string-ref digits (fix:and code #x0F)))
		      (loop (fix:+ index 1) (fix:+ i 3))))
		  (substring-move! string start end encoded i))))
	  encoded))))

(define (url:decode-substring string start end)
  (let ((patt (rexp-compile url:rexp:escape)))
    (let ((n-encoded
	   (let loop ((start start) (n-encoded 0))
	     (let ((regs (re-substring-search-forward patt string start end)))
	       (if regs
		   (loop (re-match-end-index 0 regs) (fix:+ n-encoded 1))
		   n-encoded)))))
      (if (fix:= 0 n-encoded)
	  (substring string start end)
	  (let ((decoded
		 (make-string (fix:- (fix:- end start) (fix:* 2 n-encoded)))))
	    (let loop ((start start) (i 0))
	      (let ((regs (re-substring-search-forward patt string start end)))
		(if regs
		    (let ((index (re-match-start-index 0 regs)))
		      (substring-move! string start index decoded i)
		      (let ((i (fix:+ i (fix:- index start))))
			(vector-8b-set!
			 decoded i
			 (substring->number string
					     (fix:+ index 1)
					     (fix:+ index 3)
					     16))
			(loop (fix:+ index 3) (fix:+ i 1))))
		    (substring-move! string start end decoded i))))
	    decoded)))))