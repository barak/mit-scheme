;;; -*-Scheme-*-
;;;
;;; $Id: imap-syntax.scm,v 1.16 2000/07/05 03:25:35 cph Exp $
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

;;;; IMAP Syntax

(declare (usual-integrations))

(define imap:char-set:achar
  (char-set-union url:char-set:unreserved (string->char-set "&=~")))

(define imap:match:achar+
  (rexp-matcher
   (rexp+ (rexp-alternatives imap:char-set:achar url:rexp:escape))))

(define imap:match:bchar+
  (rexp-matcher
   (rexp+ (rexp-alternatives (char-set-union imap:char-set:achar
					     (string->char-set ":@/"))
			     url:rexp:escape))))

(define imap:char-set:quoted-specials
  (char-set #\" #\\))

(define (imap:quoted-special? char)
  (char-set-member? imap:char-set:quoted-specials char))

(define imap:char-set:list-wildcards
  (char-set #\% #\*))

(define imap:char-set:char
  (ascii-range->char-set #x01 #x80))

(define imap:char-set:ctl
  (char-set-union (ascii-range->char-set #x00 #x20)
		  (char-set #\rubout)))

(define imap:char-set:atom-char
  (char-set-difference imap:char-set:char
		       (char-set-union (char-set #\( #\) #\{ #\space)
				       imap:char-set:ctl
				       imap:char-set:list-wildcards
				       imap:char-set:quoted-specials)))

(define (imap:atom-char? char)
  (char-set-member? imap:char-set:atom-char char))

(define imap:char-set:text-char
  (char-set-difference imap:char-set:char
		       (char-set #\return #\linefeed)))

(define imap:char-set:not-text-char
  (char-set-invert imap:char-set:text-char))

(define (imap:string-may-be-quoted? string)
  (not (string-find-next-char-in-set string imap:char-set:not-text-char)))

(define imap:char-set:quoted-char
  (char-set-difference imap:char-set:text-char
		       imap:char-set:quoted-specials))

(define (imap:quoted-char? char)
  (char-set-member? imap:char-set:quoted-char char))

(define imap:char-set:base64
  (char-set-union char-set:alphanumeric
		  (char-set #\+ #\/)))

(define imap:char-set:tag-char
  (char-set-difference imap:char-set:atom-char
		       (char-set #\+)))

(define imap:match:atom
  (rexp-matcher (rexp+ imap:char-set:atom-char)))

(define imap:match:text
  (rexp-matcher (rexp+ imap:char-set:text-char)))

(define imap:match:tag
  (rexp-matcher (rexp+ imap:char-set:tag-char)))

(define imap:match:base64
  (rexp-matcher
   (rexp-sequence
    (rexp* imap:char-set:base64
	   imap:char-set:base64
	   imap:char-set:base64
	   imap:char-set:base64)
    (rexp-optional
     (rexp-alternatives
      (rexp-sequence imap:char-set:base64
		     imap:char-set:base64
		     "==")
      (rexp-sequence imap:char-set:base64
		     imap:char-set:base64
		     imap:char-set:base64
		     "="))))))

(define imap:match:quoted-string
  (rexp-matcher
   (rexp-sequence "\""
		  (rexp* (rexp-alternatives
			  imap:char-set:quoted-char
			  (rexp-sequence "\\" imap:char-set:quoted-specials)))
		  "\"")))

(define (imap:match:literal string start end)
  (let ((regs (re-substring-match "{\\([0-9]+\\)}\r\n" string start end)))
    (and regs
	 (let ((index
		(fix:+ (re-match-end-index 0 regs)
		       (substring->number string
					  (re-match-start-index 1 regs)
					  (re-match-end-index 1 regs)))))
	   (and (fix:<= index end)
		index)))))

(define imap:match:string
  (alternatives-matcher imap:match:quoted-string
			imap:match:literal))

(define imap:match:astring
  (alternatives-matcher imap:match:atom
			imap:match:string))

(define imap:match:number
  (rexp-matcher (rexp+ char-set:numeric)))

(define imap:match:nz-number
  (rexp-matcher
   (rexp-sequence (char-set-difference char-set:numeric (char-set #\0))
		  (rexp* char-set:numeric))))

(define imap:match:date
  (let ((date-text
	 (rexp-matcher
	  (rexp-sequence
	   (rexp-sequence (rexp-optional (char-set #\1 #\2 #\3))
			  char-set:numeric)
	   "-"
	   (apply rexp-alternatives
		  (map rexp-case-fold
		       '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
			       "Aug" "Sep" "Oct" "Nov" "Dec")))
	   "-"
	   (rexp-sequence char-set:numeric
			  char-set:numeric
			  char-set:numeric
			  char-set:numeric)))))
    (alternatives-matcher date-text
			  (sequence-matcher (string-matcher "\"")
					    date-text
					    (string-matcher "\"")))))

(define imap:parse:section-text
  (alternatives-parser
   (simple-parser (alternatives-matcher
		   (ci-string-matcher "header")
		   (ci-string-matcher "text"))
		  'KEYWORD)
   (sequence-parser
    (simple-parser (sequence-matcher
		    (ci-string-matcher "header.fields")
		    (optional-matcher
		     (ci-string-matcher ".not")))
		   'KEYWORD)
    (noise-parser (string-matcher " ("))
    (predicated-parser (list-parser imap:match:astring
				    (string-matcher " ")
				    'HEADERS)
		       (lambda (pv) (pair? (parser-token pv 'HEADERS))))
    (noise-parser (string-matcher ")")))))

(define imap:parse:section
  (encapsulating-parser
   (alternatives-parser
    imap:parse:section-text
    (sequence-parser
     (list-parser imap:match:nz-number (string-matcher ".") 'NUMBER)
     (optional-parser
      (noise-parser (string-matcher "."))
      (alternatives-parser
       imap:parse:section-text
       (simple-parser (ci-string-matcher "mime") 'KEYWORD)))))
   (lambda (pv)
     (map* (let ((keyword (parser-token pv 'KEYWORD)))
	     (if keyword
		 (cons (intern keyword)
		       (or (parser-token pv 'HEADERS) '()))
		 '()))
	   string->number
	   (or (parser-token pv 'NUMBER) '())))
   'SECTION))

(define imap:match:set
  (let ((range
	 (let ((number
		(alternatives-matcher imap:match:nz-number
				      (string-matcher "*"))))
	   (alternatives-matcher number
				 (sequence-matcher number ":" number)))))
    (sequence-matcher range
		      (*-matcher (string-matcher ",") range))))

(define imap:match:search-key
  (let ((m
	 (lambda (keyword . arguments)
	   (apply sequence-matcher
		  (ci-string-matcher keyword)
		  (map (lambda (argument)
			 (sequence-matcher (string-matcher " ")
					   argument))
		       arguments))))
	;; Kludge: self reference.
	(imap:match:search-key
	 (lambda (string start end)
	   (imap:match:search-key string start end))))
    (alternatives-matcher
     (m "all")
     (m "answered")
     (m "bcc"		imap:match:astring)
     (m "before"	imap:match:date)
     (m "body"		imap:match:astring)
     (m "cc"		imap:match:astring)
     (m "deleted")
     (m "draft")
     (m "flagged")
     (m "from"		imap:match:astring)
     (m "header"	imap:match:astring imap:match:astring)
     (m "keyword"	imap:match:atom)
     (m "larger"	imap:match:number)
     (m "new")
     (m "not"		imap:match:search-key)
     (m "old")
     (m "on"		imap:match:date)
     (m "or"		imap:match:search-key imap:match:search-key)
     (m "recent")
     (m "seen")
     (m "sentbefore"	imap:match:date)
     (m "senton"	imap:match:date)
     (m "sentsince"	imap:match:date)
     (m "since"		imap:match:date)
     (m "smaller"	imap:match:number)
     (m "subject"	imap:match:astring)
     (m "text"		imap:match:astring)
     (m "to"		imap:match:astring)
     (m "uid"		imap:match:set)
     (m "unanswered")
     (m "undeleted")
     (m "undraft")
     (m "unflagged")
     (m "unkeyword"	imap:match:atom)
     (m "unseen")
     imap:match:set
     (sequence-matcher (string-matcher "(")
		       imap:match:search-key
		       (string-matcher ")")))))

(define imap:match:search-program
  (sequence-matcher
   (optional-matcher (ci-string-matcher "charset ")
		     imap:match:astring
		     (string-matcher " "))
   imap:match:search-key))

;;;; URL parser

(define (url:decoding-parser match-encoded keyword)
  (decoding-parser match-encoded
		   url:decode-substring
		   (simple-parser (lambda (string start end)
				    string start
				    end)
				  keyword)))

(define (imap:server-parser allow-auth?)
  (sequence-parser
   (optional-parser
    (sequence-parser
     (let ((parse-user-id (url:decoding-parser imap:match:achar+ 'USER-ID)))
       (if allow-auth?
	   (let ((parse-auth
		  (sequence-parser
		   (noise-parser (ci-string-matcher ";auth="))
		   (alternatives-parser
		    (simple-parser (string-matcher "*") 'AUTH-TYPE)
		    (url:decoding-parser imap:match:achar+ 'AUTH-TYPE)))))
	     (alternatives-parser
	      (sequence-parser parse-user-id
			       (optional-parser parse-auth))
	      (sequence-parser (optional-parser parse-user-id)
			       parse-auth)))
	   parse-user-id))
     (noise-parser (string-matcher "@"))))
   (simple-parser (rexp-matcher url:rexp:host) 'HOST)
   (optional-parser
    (noise-parser (string-matcher ":"))
    (simple-parser (rexp-matcher (rexp+ char-set:numeric)) 'PORT))))

(define imap:parse:server
  (imap:server-parser #t))

(define imap:parse:mailboxlist
  (sequence-parser
   (optional-parser
    (url:decoding-parser imap:match:bchar+ 'MAILBOX-LIST))
   (noise-parser (ci-string-matcher ";type="))
   (simple-parser (alternatives-matcher (ci-string-matcher "list")
					(ci-string-matcher "lsub"))
		  'LIST-TYPE)))

(define imap:parse:enc-mailbox
  (url:decoding-parser imap:match:bchar+ 'MAILBOX))

(define imap:parse:uidvalidity
  (sequence-parser (noise-parser (ci-string-matcher ";uidvalidity="))
		   (simple-parser imap:match:nz-number 'UID-VALIDITY)))

(define imap:parse:messagelist
  (sequence-parser imap:parse:enc-mailbox
		   (optional-parser
		    (url:decoding-parser imap:match:bchar+ 'SEARCH-PROGRAM))
		   (optional-parser imap:parse:uidvalidity)))

(define imap:parse:messagepart
  (sequence-parser imap:parse:enc-mailbox
		   (optional-parser imap:parse:uidvalidity)
		   (noise-parser (ci-string-matcher "/;uid="))
		   (simple-parser imap:match:nz-number 'UID)
		   (optional-parser
		    (noise-parser (ci-string-matcher "/;section="))
		    (decoding-parser imap:match:bchar+
				     url:decode-substring
				     imap:parse:section))))

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