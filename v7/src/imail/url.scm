;;; -*-Scheme-*-
;;;
;;; $Id: url.scm,v 1.3 2000/04/12 03:47:51 cph Exp $
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
(define url:char-set:national (string->char-set "{}|\^~[]`"))
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

(define url:regexp:escape
  "%[0-9A-Fa-f][0-9A-Fa-f]")

(define url:regexp:uchar
  (regexp-group (char-set->regexp url:char-set:unreserved)
		url:regexp:escape))

(define url:regexp:xchar
  (regexp-group (char-set->regexp url:char-set:unescaped)
		url:regexp:escape))

(define url:regexp:hostname
  (let ((c1 (char-set->regexp char-set:alphanumeric)))
    (let ((tail
	   (regexp-group
	    ""
	    (string-append
	     (char-set->regexp
	      (char-set-union char-set:alphanumeric (string->char-set "-")))
	     "*"
	     c1))))
      (string-append (regexp-group (string-append c1 tail "."))
		     "*"
		     (char-set->regexp char-set:alphabetic)
		     tail))))

(define url:regexp:hostnumber
  "[0-9]+.[0-9]+.[0-9]+.[0-9]+")

(define url:regexp:host
  (regexp-group url:regexp:hostname
		url:regexp:hostnumber))

(define url:regexp:hostport
  (string-append url:regexp:host (regexp-group ":[0-9]+") "?"))

(define (url:string-encoded? string)
  (url:substring-encoded? string 0 (string-length string)))

(define (url:encode-string string)
  (url:encode-substring string 0 (string-length string)))

(define (url:decode-string string)
  (url:decode-substring string 0 (string-length string)))

(define url:substring-encoded?
  (let ((pattern
	 (re-compile-pattern
	  (string-append
	   (regexp-group (char-set->regexp url:char-set:unescaped)
			 url:regexp:escape)
	   "*")
	  #f)))
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
		    (substring-move-left! string start index encoded i)
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
		  (substring-move-left! string start end
					encoded i))))
	  encoded))))

(define (url:decode-substring string start end)
  (let ((n-encoded
	 (let loop ((start start) (n-encoded 0))
	   (let ((regs
		  (re-substring-search-forward url:regexp:escape
					       string start end)))
	     (if regs
		 (loop (re-match-end-index 0 regs) (fix:+ n-encoded 1))
		 n-encoded)))))
    (if (fix:= 0 n-encoded)
	(substring string start end)
	(let ((decoded
	       (make-string (fix:- (fix:- end start) (fix:* 2 n-encoded)))))
	  (let loop ((start start) (i 0))
	    (let ((regs
		   (re-substring-search-forward url:regexp:escape
						string start end)))
	      (if regs
		  (let ((index (re-match-start-index 0 regs)))
		    (substring-move-left! string start index decoded i)
		    (let ((i (fix:+ i (fix:- index start))))
		      (vector-8b-set!
		       decoded i
		       (substring->number string
					   (fix:+ index 1)
					   (fix:+ index 3)
					   16))
		      (loop (fix:+ index 3) (fix:+ i 1))))
		  (substring-move-left! string start end decoded i))))
	  decoded))))