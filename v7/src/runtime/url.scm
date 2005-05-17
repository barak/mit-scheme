#| -*-Scheme-*-

$Id: url.scm,v 1.16 2005/05/17 05:22:51 cph Exp $

Copyright 2000,2001,2003,2004,2005 Massachusetts Institute of Technology

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

;;;; URL Encoding

(declare (usual-integrations))

(define url:char-set:lowalpha)
(define url:char-set:alpha)
(define url:char-set:digit)
(define url:char-set:alphadigit)
(define url:char-set:safe)
(define url:char-set:extra)
(define url:char-set:national)
(define url:char-set:punctuation)
(define url:char-set:reserved)
(define url:char-set:hex)
(define url:char-set:unreserved)
(define url:char-set:unescaped)
(define url:char-set:escaped)

(define (initialize-package!)
  (set! url:char-set:lowalpha (string->char-set "abcdefghijklmnopqrstuvwxyz"))
  (set! url:char-set:alpha
	(char-set-union url:char-set:lowalpha
			(string->char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
  (set! url:char-set:digit (string->char-set "0123456789"))
  (set! url:char-set:alphadigit
	(char-set-union url:char-set:alpha url:char-set:digit))
  (set! url:char-set:safe (string->char-set "$-_.+"))
  (set! url:char-set:extra (string->char-set "!*'(),"))
  (set! url:char-set:national (string->char-set "{}|\\^~[]`"))
  (set! url:char-set:punctuation (string->char-set "<>#%\""))
  (set! url:char-set:reserved (string->char-set ";/?:@&="))
  (set! url:char-set:hex (string->char-set "0123456789abcdefABCDEF"))
  (set! url:char-set:unreserved
	(char-set-union url:char-set:alphadigit
			url:char-set:safe
			url:char-set:extra))
  (set! url:char-set:unescaped
	(char-set-union url:char-set:unreserved
			url:char-set:reserved))
  (set! url:char-set:escaped
	(char-set-invert url:char-set:unescaped))
  unspecific)

(define url:match:escape
  (*matcher
   (seq "%"
	(char-set url:char-set:hex)
	(char-set url:char-set:hex))))

(define url:match:uchar
  (*matcher
   (alt (char-set url:char-set:unreserved)
	url:match:escape)))

(define url:match:xchar
  (*matcher
   (alt (char-set url:char-set:unescaped)
	url:match:escape)))

(define url:parse:hostport
  (*parser
   (seq (match url:match:host)
	(alt (map string->number
		  (seq (noise ":")
		       (match (+ (char-set url:char-set:digit)))))
	     (values #f)))))

(define url:match:host
  (*matcher (alt url:match:hostname url:match:hostnumber)))

(define url:match:hostname
  (let ((match-tail
	 (*matcher
	  (* (alt (char-set url:char-set:alphadigit)
		  (seq (+ #\-)
		       (char-set url:char-set:alphadigit)))))))
    (*matcher
     (seq (* (seq (char-set url:char-set:alphadigit)
		  match-tail
		  "."))
	  (char-set url:char-set:alpha)
	  match-tail))))

(define url:match:hostnumber
  (*matcher
   (seq (+ (char-set url:char-set:digit))
	"."
	(+ (char-set url:char-set:digit))
	"."
	(+ (char-set url:char-set:digit))
	"."
	(+ (char-set url:char-set:digit)))))

(define (url:string-encoded? string)
  (url:substring-encoded? string 0 (string-length string)))

(define url:substring-encoded?
  (let ((matcher (*matcher (complete (* url:match:xchar)))))
    (lambda (string start end)
      (matcher (string->parser-buffer string start end)))))

(define (url:encode-string string)
  (url:encode-substring string 0 (string-length string)))

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

(define (url:decode-string string)
  (url:decode-substring string 0 (string-length string)))

(define (url:decode-substring string start end)
  (let ((n-encoded
	 (let loop ((start start) (n-encoded 0))
	   (let ((index (substring-find-next-char string start end #\%)))
	     (if index
		 (loop (fix:+ index 1) (fix:+ n-encoded 1))
		 n-encoded))))
	(lose
	 (lambda ()
	   (error "Malformed encoded URL string:"
		  (substring string start end)))))
    (if (fix:= 0 n-encoded)
	(substring string start end)
	(let ((decoded
	       (make-string (fix:- (fix:- end start) (fix:* 2 n-encoded)))))
	  (let loop ((start start) (i 0))
	    (let ((index (substring-find-next-char string start end #\%)))
	      (if index
		  (begin
		    (if (not (fix:<= (fix:+ index 3) end))
			(lose))
		    (let ((k
			   (substring->number string
					      (fix:+ index 1)
					      (fix:+ index 3)
					      16))
			  (i* (fix:+ i (fix:- index start))))
		      (if (not k)
			  (lose))
		      (substring-move! string start index decoded i)
		      (vector-8b-set! decoded i* k)
		      (loop (fix:+ index 3) (fix:+ i* 1))))
		  (substring-move! string start end decoded i))))
	  decoded))))