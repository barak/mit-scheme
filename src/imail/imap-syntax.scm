;;; -*-Scheme-*-
;;;
;;; $Id: imap-syntax.scm,v 1.18 2001/10/13 05:54:33 cph Exp $
;;;
;;; Copyright (c) 2000, 2001 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; IMAP Syntax

(declare (usual-integrations))

(define imap:char-set:char
  (ascii-range->char-set #x01 #x80))

(define imap:char-set:ctl
  (char-set-union (ascii-range->char-set #x00 #x20)
		  (char-set #\rubout)))

(define imap:char-set:list-wildcards
  (char-set #\% #\*))

(define imap:char-set:quoted-specials
  (char-set #\" #\\))

(define imap:char-set:text-char
  (char-set-difference imap:char-set:char
		       (char-set #\return #\linefeed)))

(define imap:char-set:quoted-char
  (char-set-difference imap:char-set:text-char
		       imap:char-set:quoted-specials))

(define imap:char-set:atom-char
  (char-set-difference imap:char-set:char
		       (char-set-union (char-set #\( #\) #\{ #\space)
				       imap:char-set:ctl
				       imap:char-set:list-wildcards
				       imap:char-set:quoted-specials)))

(define imap:char-set:tag-char
  (char-set-difference imap:char-set:atom-char
		       (char-set #\+)))

(define imap:char-set:achar
  (char-set-union url:char-set:unreserved (string->char-set "&=~")))

(define (imap:atom-char? char)
  (char-set-member? imap:char-set:atom-char char))

(define (imap:quoted-special? char)
  (char-set-member? imap:char-set:quoted-specials char))

(define (imap:quoted-char? char)
  (char-set-member? imap:char-set:quoted-char char))

(define ((string-matching-procedure matcher) string)
  (matcher (string->parser-buffer string)))

(define imap:string-may-be-quoted?
  (string-matching-procedure
   (*matcher (complete (* (char-set imap:char-set:text-char))))))

(define imap:tag-string?
  (string-matching-procedure
   (*matcher (complete (+ (char-set imap:char-set:tag-char))))))

(define (imap:server-parser allow-auth?)
  (let ((parse-user/auth
	 (if allow-auth?
	     (let ((parse-auth
		    (*parser
		     (seq (noise (string-ci ";auth="))
			  (alt (match "*")
			       imap:parse:achar+)))))
	       (*parser
		(alt (seq (alt (seq imap:parse:achar+
				    (alt parse-auth (values #f)))
			       (seq (alt imap:parse:achar+ (values #f))
				    parse-auth))
			  "@")
		     (values #f #f))))
	     (*parser
	      (alt (seq imap:parse:achar+ "@")
		   (values #f))))))
    (*parser
     (seq parse-user/auth
	  url:parse:hostport))))

(define imap:parse:achar+
  (*parser
   (map url:decode-string
	(match (+ (alt (char-set imap:char-set:achar)
		       url:match:escape))))))

(define imap:parse:enc-mailbox
  (let ((imap:char-set:bchar
	 (char-set-union imap:char-set:achar (string->char-set ":@/"))))
    (*parser
     (map url:decode-string
	  (match (+ (alt (char-set imap:char-set:bchar)
			 url:match:escape)))))))

(define imap:parse:section
  (*parser
   (encapsulate vector->list
     (alt imap:parse:section-text
	  (seq (? (seq imap:parse:nz-number
		       (* (seq "." imap:parse:nz-number))))
	       (? (seq "."
		       (alt imap:parse:section-text
			    (map intern (match (string-ci "mime")))))))))))

(define imap:parse:section-text
  (*parser
   (alt (map intern
	     (match (alt (string-ci "header")
			 (string-ci "text"))))
	(seq (map intern
		  (match (seq (string-ci "header.fields")
			      (? (string-ci ".not")))))
	     " ("
	     imap:parse:astring
	     (* (seq " " imap:parse:astring))
	     ")"))))

(define imap:parse:nz-number
  (let ((char-set:1-9 (char-set-difference char-set:numeric (char-set #\0))))
    (*parser
     (map string->number
	  (match (seq (char-set char-set:1-9)
		      (* (char-set char-set:numeric))))))))

(define imap:parse:astring
  (*parser (alt imap:parse:atom imap:parse:string)))

(define imap:parse:atom
  (*parser (match (+ (char-set imap:char-set:atom-char)))))

(define imap:parse:string
  (*parser (alt imap:parse:quoted-string imap:parse:literal)))

(define imap:parse:quoted-string
  (*parser
   (seq #\"
	(map decode-quoted-string
	     (match (* (alt (char-set imap:char-set:quoted-char)
			    (seq (char #\\)
				 (char-set imap:char-set:quoted-specials))))))
	#\")))

(define (decode-quoted-string string)
  (let ((end (string-length string)))
    (let ((n-quotes
	   (let loop ((start 0) (n-quotes 0))
	     (if (fix:< start end)
		 (let ((index (substring-find-next-char string start end #\\)))
		   (if index
		       (loop (fix:+ index 2) (fix:+ n-quotes 1))
		       n-quotes))
		 n-quotes))))
      (let ((end* (fix:- end n-quotes)))
	(let ((string* (make-string end*)))
	  (let loop ((start 0) (start* 0))
	    (if (fix:< start end)
		(let ((index (substring-find-next-char string start end #\\)))
		  (if index
		      (let ((index*
			     (substring-move! string start index
					      string* start*)))
			(string-set! string* index*
				     (string-ref string (fix:+ index 1)))
			(loop (fix:+ index 2) (fix:+ index* 1)))
		      (substring-move! string start end string* start*)))))
	  string*)))))

(define (imap:parse:literal buffer)
  (let ((p (get-parser-buffer-pointer buffer)))
    (let ((v
	   ((*parser
	     (seq "{" (match (+ (char-set char-set:numeric))) "}\r\n"))
	    buffer)))
      (and v
	   (let ((n (string->number (vector-ref v 0)))
		 (p2 (get-parser-buffer-pointer buffer)))
	     (let loop ((i 0))
	       (cond ((= i n)
		      (get-parser-buffer-tail buffer p2))
		     ((read-parser-buffer-char buffer)
		      (loop (+ i 1)))
		     (else
		      (set-parser-buffer-pointer! buffer p)
		      #f))))))))

;;;; Mailbox-name encoding (modified UTF-7)

(define (imap:encode-mailbox-name string #!optional start end)
  (let* ((start (if (default-object? start) 0 start))
	 (end (if (default-object? end) (string-length string) end))
	 (n
	  (let loop ((start start) (n 0))
	    (let ((index
		   (substring-find-next-char-in-set
		    string start end imap:char-set:mailbox-name-encoded)))
	      (if index
		  (let ((n (fix:+ n (fix:+ (fix:- index start) 2))))
		    (let ((index*
			   (or (substring-find-next-char-in-set
				string (fix:+ index 1) end
				imap:char-set:mailbox-name-unencoded)
			       end)))
		      (loop index*
			    (fix:+ n
				   (let ((m (fix:- index* index)))
				     (if (and (fix:= m 1)
					      (char=? (string-ref string index)
						      #\&))
					 0
					 (integer-ceiling (fix:* 8 m) 6)))))))
		  (fix:+ n (fix:- end start)))))))
    (let ((s (make-string n)))
      (let loop ((start start) (j 0))
	(let ((index
	       (substring-find-next-char-in-set
		string start end imap:char-set:mailbox-name-encoded)))
	  (if index
	      (let ((j (substring-move! string start index s j)))
		(string-set! s j #\&)
		(let ((j (fix:+ j 1))
		      (index*
		       (or (substring-find-next-char-in-set
			    string (fix:+ index 1) end
			    imap:char-set:mailbox-name-unencoded)
			   end)))
		  (let ((j
			 (if (and (fix:= (fix:- index* index) 1)
				  (char=? (string-ref string index) #\&))
			     j
			     (encode-mailbox-name-1 string index index* s j))))
		    (string-set! s j #\-)
		    (loop index* (fix:+ j 1)))))
	      (substring-move! string start end s j))))
      s)))

(define (imap:decode-mailbox-name string #!optional start end)
  (let* ((start (if (default-object? start) 0 start))
	 (end (if (default-object? end) (string-length string) end))
	 (lose
	  (lambda ()
	    (error "Malformed encoded mailbox name:"
		   (substring string start end)))))
    (let ((n
	   (let loop ((start start) (n 0))
	     (let ((index (substring-find-next-char string start end #\&)))
	       (if index
		   (let ((index*
			  (substring-find-next-char string (fix:+ index 1) end
						    #\-)))
		     (if (not index*) (lose))
		     (loop (fix:+ index* 1)
			   (fix:+ (fix:+ n (fix:- index start))
				  (let ((m (fix:- index* (fix:+ index 1))))
				    (if (fix:= m 1)
					1
					(let ((q (fix:quotient m 4))
					      (r (fix:remainder m 4)))
					  (fix:+ (fix:* 3 q)
						 (case r
						   ((0) 0)
						   ((2) 1)
						   ((3) 2)
						   (else (lose))))))))))
		   (fix:+ n (fix:- end start)))))))
      (let ((s (make-string n)))
	(let loop ((start start) (j 0))
	  (let ((index (substring-find-next-char string start end #\&)))
	    (if index
		(let ((index*
		       (substring-find-next-char string (fix:+ index 1) end
						 #\-)))
		  (if (not index*) (lose))
		  (let ((j (substring-move! string start index s j))
			(m (fix:- index* index)))
		    (if (fix:= m 1)
			(begin
			  (string-set! s j #\&)
			  (loop (fix:+ index* 1) (fix:+ j 1)))
			(loop (fix:+ index* 1)
			      (decode-mailbox-name-1 string
						     (fix:+ index 1)
						     index*
						     s
						     j
						     lose)))))
		(substring-move! string start end s j))))
	s))))

(define (encode-mailbox-name-1 string start end s j)
  (let ((write
	 (lambda (j v)
	   (string-set! s j
			(vector-8b-ref base64-digit-table
				       (fix:and #x3f v))))))
    (let loop ((start start) (j j))
      (case (fix:- end start)
	((0)
	 j)
	((1)
	 (let ((d0 (string-ref string start)))
	   (write j (fix:lsh d0 -2))
	   (write (fix:+ j 1) (fix:lsh d0 4)))
	 (fix:+ j 2))
	((2)
	 (let ((d0 (string-ref string start))
	       (d1 (string-ref string (fix:+ start 1))))
	   (write j (fix:lsh d0 -2))
	   (write (fix:+ j 1) (fix:+ (fix:lsh d0 4) (fix:lsh d1 -4)))
	   (write (fix:+ j 2) (fix:lsh d1 2)))
	 (fix:+ j 3))
	(else
	 (let ((d0 (string-ref string start))
	       (d1 (string-ref string (fix:+ start 1)))
	       (d2 (string-ref string (fix:+ start 2))))
	   (write j (fix:lsh d0 -2))
	   (write (fix:+ j 1) (fix:+ (fix:lsh d0 4) (fix:lsh d1 -4)))
	   (write (fix:+ j 2) (fix:+ (fix:lsh d1 2) (fix:lsh d2 -6)))
	   (write (fix:+ j 3) d2)
	   (loop (fix:+ start 3) (fix:+ j 4))))))))

(define (decode-mailbox-name-1 string start end s j lose)
  (let ((read (lambda (i) (decode-base64-char (vector-8b-ref string i))))
	(write (lambda (j v) (vector-8b-set! s j v))))
    (let loop ((start start) (j j))
      (case (fix:- end start)
	((0)
	 j)
	((1)
	 (lose))
	((2)
	 (let ((d0 (read start))
	       (d1 (read (fix:+ start 1))))
	   (write j
		  (fix:+ (fix:lsh d0 2)
			 (fix:lsh d1 -4))))
	 (fix:+ j 1))
	((3)
	 (let ((d0 (read start))
	       (d1 (read (fix:+ start 1)))
	       (d2 (read (fix:+ start 2))))
	   (write j
		  (fix:+ (fix:lsh d0 2)
			 (fix:lsh d1 -4)))
	   (write (fix:+ j 1)
		  (fix:+ (fix:lsh (fix:and #x0f d1) 4)
			 (fix:lsh d2 -2))))
	 (fix:+ j 2))
	(else
	 (let ((d0 (read start))
	       (d1 (read (fix:+ start 1)))
	       (d2 (read (fix:+ start 2)))
	       (d3 (read (fix:+ start 3))))
	   (write j
		  (fix:+ (fix:lsh d0 2)
			 (fix:lsh d1 -4)))
	   (write (fix:+ j 1)
		  (fix:+ (fix:lsh (fix:and #x0f d1) 4)
			 (fix:lsh d2 -2)))
	   (write (fix:+ j 2)
		  (fix:+ (fix:lsh (fix:and #x03 d2) 6)
			 d3)))
	 (loop (fix:+ start 4) (fix:+ j 3)))))))

(define imap:char-set:mailbox-name-encoded
  (char-set-union char-set:not-graphic (char-set #\&)))

(define imap:char-set:mailbox-name-unencoded
  (char-set-invert imap:char-set:mailbox-name-encoded))

(define (decode-base64-char byte)
  (let ((digit (vector-8b-ref base64-char-table byte)))
    (if (>= digit #x40)
	(error "Character not a base64 component:" (integer->char byte)))
    digit))  

(define base64-char-table)
(define base64-digit-table)
(let ((char-table (make-string 256 (integer->char #xff)))
      (digit-table (make-string 64)))
  (let ((do-single
	 (lambda (index value)
	   (vector-8b-set! char-table index value)
	   (vector-8b-set! digit-table value index))))
    (letrec
	((do-range
	  (lambda (low high value)
	    (do-single low value)
	    (if (fix:< low high)
		(do-range (fix:+ low 1) high (fix:+ value 1))))))
      (do-range (char->integer #\A) (char->integer #\Z) 0)
      (do-range (char->integer #\a) (char->integer #\z) 26)
      (do-range (char->integer #\0) (char->integer #\9) 52)
      (do-single (char->integer #\+) 62)
      (do-single (char->integer #\,) 63)))
  (set! base64-char-table char-table)
  (set! base64-digit-table digit-table)
  unspecific)

;;;; Formatted output

(define (imap:write-quoted-string string port)
  (imap:write-quoted-substring string 0 (string-length string) port))

(define (imap:write-quoted-substring string start end port)
  (imap-transcript-write-char #\" port)
  (let loop ((start start))
    (if (fix:< start end)
	(let ((char (string-ref string start)))
	  (if (or (char=? char #\\) (char=? char #\"))
	      (imap-transcript-write-char #\\ port))
	  (imap-transcript-write-char char port)
	  (loop (fix:+ start 1)))))
  (imap-transcript-write-char #\" port))

(define (imap:write-literal-string-header string port)
  (imap:write-literal-substring-header string 0 (string-length string) port))

(define (imap:write-literal-substring-header string start end port)
  (imap-transcript-write-char #\{ port)
  (imap-transcript-write
   (+ (- end start) (length (substring-search-all "\n" string start end)))
   port)
  (imap-transcript-write-char #\} port)
  (imap-transcript-write-char #\return port)
  (imap-transcript-write-char #\linefeed port))

(define (imap:write-literal-string-body string port)
  (imap:write-literal-substring-body string 0 (string-length string) port))

(define (imap:write-literal-substring-body string start end port)
  ;; Translate newlines back to network line endings.
  (let loop ((start start))
    (if (fix:<= start end)
	(let ((index (substring-find-next-char string start end #\newline)))
	  (if index
	      (begin
		(imap-transcript-write-substring string start index port)
		(imap-transcript-write-char #\return port)
		(imap-transcript-write-char #\linefeed port)
		(loop (fix:+ index 1)))
	      (imap-transcript-write-substring string start end port))))))

(define (imap:universal-time->date-time time)
  (imap:decoded-time->date-time (universal-time->global-decoded-time time)))

(define (imap:decoded-time->date-time dt)
  (let ((2digit
	 (lambda (n)
	   (string-pad-left (number->string n) 2 #\0))))
    (string-append (string-pad-left (number->string (decoded-time/day dt)) 2)
		   "-"
		   (month/short-string (decoded-time/month dt))
		   "-"
		   (number->string (decoded-time/year dt))
		   " "
		   (2digit (decoded-time/hour dt))
		   ":"
		   (2digit (decoded-time/minute dt))
		   ":"
		   (2digit (decoded-time/second dt))
		   " "
		   (time-zone->string (decoded-time/zone dt)))))