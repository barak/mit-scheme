;;; -*-Scheme-*-
;;;
;;; $Id: imail-imap-url.scm,v 1.7 2000/04/14 17:58:23 cph Exp $
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

;;;; IMAIL mail reader: IMAP back end

(declare (usual-integrations))

(define-class <imap-url> (<url>)
  (user-id define accessor)
  (auth-type define accessor)
  (host define accessor)
  (port define accessor))

(define-class (<imap-mailbox-url>
	       (constructor make-imap-mailbox-url
			    (user-id auth-type host port
				     mailbox uid-validity uid section)))
    (<imap-url>)
  (mailbox define accessor)
  (uid-validity define accessor)
  (uid define accessor)
  (section define accessor))

(define-class (<imap-search-url>
	       (constructor make-imap-search-url
			    (user-id auth-type host port
				     mailbox search-program uid-validity)))
    (<imap-url>)
  (mailbox define accessor)
  (search-program define accessor)
  (uid-validity define accessor))

(define-class (<imap-list-url>
	       (constructor make-imap-list-url
			    (user-id auth-type host port
				     mailbox-list list-type)))
    (<imap-url>)
  (mailbox-list define accessor)
  (list-type define accessor))

(define-url-protocol "imap" <imap-url>
  (lambda (string)
    (parse-imap-url string)))

(define (parse-imap-url string)
  (let ((string (url:decode-string string))
	(lose (lambda () (error:bad-range-argument string 'PARSE-IMAP-URL))))
    (if (not (string-prefix? "//" string))
	(lose))
    (let ((end (string-length string)))
      (let ((slash (substring-find-next-char string 2 end)))
	(if (not slash) (lose))
	(let ((pv1 (imap:parse:server string 0 slash)))
	  (if (not pv1) (lose))
	  (let ((start (fix:+ slash 1)))
	    (cond ((imap:parse:messagepart string start end)
		   =>
		   (lambda (pv2)
		     (make-imap-mailbox-url (parser-token pv1 'USER-ID)
					    (parser-token pv1 'AUTH-TYPE)
					    (parser-token pv1 'HOST)
					    (parser-token pv1 'PORT)
					    (parser-token pv2 'MAILBOX)
					    (parser-token pv2 'UID-VALIDITY)
					    (parser-token pv2 'UID)
					    (parser-token pv2 'SECTION))))
		  ((imap:parse:messagelist string start end)
		   =>
		   (lambda (pv2)
		     (make-imap-search-url (parser-token pv1 'USER-ID)
					   (parser-token pv1 'AUTH-TYPE)
					   (parser-token pv1 'HOST)
					   (parser-token pv1 'PORT)
					   (parser-token pv2 'MAILBOX)
					   (parser-token pv2 'SEARCH-PROGRAM)
					   (parser-token pv2 'UID-VALIDITY))))
		  ((imap:parse:mailboxlist string start end)
		   =>
		   (lambda (pv2)
		     (make-imap-list-url (parser-token pv1 'USER-ID)
					 (parser-token pv1 'AUTH-TYPE)
					 (parser-token pv1 'HOST)
					 (parser-token pv1 'PORT)
					 (parser-token pv2 'MAILBOX-LIST)
					 (parser-token pv2 'LIST-TYPE))))
		  (else (lose)))))))))

;;;; Parser language

;;; A parser is a procedure that accepts a substring as three
;;; arguments and returns one of two values.  If the parser
;;; successfully parses the substring, it returns a pair whose car is
;;; an index into the substring indicating how much of the substring
;;; was parsed, and whose cdr is an alist of keyword/token pairs.  If
;;; the parser fails, it returns #F.

(define (parser-token parser-value keyword)
  (let ((entry (assq keyword (cdr parser-value))))
    (and entry
	 (cdr entry))))

(define (parse-never string start end)
  string start end
  #f)

(define (parse-always string start end)
  string end
  (list start))

(define (trivial-parser match)
  (lambda (string start end)
    (let ((i (match string start end)))
      (and i
	   (list i)))))

(define (simple-parser match keyword)
  (lambda (string start end)
    (let ((i (match string start end)))
      (and i
	   (list i (cons keyword (substring string start i)))))))

(define (prefix-parser match-prefix match-body keyword)
  (wrapped-parser match-prefix match-body parse-always keyword))

(define (suffix-parser match-body match-suffix keyword)
  (wrapped-parser parse-always match-body match-suffix keyword))

(define (wrapped-parser match-prefix match-body match-suffix keyword)
  (lambda (string start end)
    (let ((i1 (match-prefix string start end)))
      (and i1
	   (let ((i2 (match-body string i1 end)))
	     (and i2
		  (let ((i3 (match-suffix string i2 end)))
		    (and i3
			 (list i3
			       (cons keyword
				     (substring string i1 i2)))))))))))

(define (complete-parser parse)
  (lambda (string start end)
    (let ((pv (parse string start end)))
      (and pv
	   (fix:= (car pv) end)
	   pv))))

(define (optional-parser parse)
  (lambda (string start end)
    (or (parse string start end)
	(list start))))

(define (sequence-parser . parsers)
  (if (pair? parsers)
      (lambda (string start end)
	(let loop ((parsers parsers) (start start))
	  (let ((pv1 ((car parsers) string start end)))
	    (and pv1
		 (if (pair? (cdr parsers))
		     (let ((pv2 (loop (cdr parsers) (car pv1))))
		       (and pv2
			    (cons (car pv2) (append (cdr pv1) (cdr pv2)))))
		     pv1)))))
      parse-always))

(define (alternatives-parser . parsers)
  (if (pair? parsers)
      (if (pair? (cdr parsers))
	  (lambda (string start end)
	    (let loop ((parsers parsers))
	      (or ((car parsers) string start end)
		  (and (pair? (cdr parsers))
		       (loop (cdr parsers))))))
	  (car parsers))
      parse-never))

;;;; Matcher language

;;; A matcher is a procedure that accepts a substring as three
;;; arguments and returns one of two values.  If the matcher
;;; successfully matches the substring, it returns an index into the
;;; substring indicating how much of the substring was matched.  If
;;; the matcher fails, it returns #F.

(define (match-never string start end)
  string start end
  #f)

(define (match-always string start end)
  string end
  start)

(define (rexp-matcher pattern)
  (let ((pattern (rexp-compile pattern)))
    (lambda (string start end)
      (let ((regs (re-substring-match pattern string start end)))
	(and regs
	     (re-match-end-index 0 regs))))))

(define (string-matcher pattern)
  (let ((pl (string-length pattern)))
    (lambda (string start end)
      (and (substring-prefix? pattern 0 pl string start end)
	   (fix:+ start pl)))))

(define (ci-string-matcher pattern)
  (let ((pl (string-length pattern)))
    (lambda (string start end)
      (and (substring-prefix-ci? pattern 0 pl string start end)
	   (fix:+ start pl)))))

(define (optional-matcher . matchers)
  (let ((matcher (apply sequence-matcher matchers)))
    (lambda (string start end)
      (or (matcher string start end)
	  start))))

(define (alternatives-matcher . matchers)
  (if (pair? matchers)
      (if (pair? (cdr matchers))
	  (lambda (string start end)
	    (let loop ((matchers matchers))
	      (or ((car matchers) string start end)
		  (and (pair? (cdr matchers))
		       (loop (cdr matchers))))))
	  (car matchers))
      match-never))

(define (sequence-matcher . matchers)
  (if (pair? matchers)
      (if (pair? (cdr matchers))
	  (lambda (string start end)
	    (let loop ((matchers matchers) (start start))
	      (let ((i ((car matchers) string start end)))
		(and i
		     (if (pair? (cdr matchers))
			 (loop (cdr matchers) i)
			 i)))))
	  (car matchers))
      match-always))

(define (*-matcher . matchers)
  (let ((matcher (apply sequence-matcher matchers)))
    (lambda (string start end)
      (let loop ((start start))
	(let ((i (matcher string start end)))
	  (if i
	      (loop i)
	      start))))))

(define (+-matcher . matchers)
  (let ((matcher (apply sequence-matcher matchers)))
    (sequence-matcher matcher (*-matcher matcher))))

;;;; IMAP URL parser

(define imap:char-set:quoted-specials
  (char-set #\" #\\))

(define imap:char-set:list-wildcards
  (char-set #\% #\*))

(define imap:char-set:atom-char
  (char-set-invert
   (char-set-union (char-set #\( #\) #\{ #\space #\rubout)
		   imap:char-set:quoted-specials
		   imap:char-set:list-wildcards
		   (ascii-range->char-set #x00 #x20))))

(define imap:match:atom
  (rexp-matcher (rexp+ imap:char-set:atom-char)))

(define imap:match:quoted-string
  (rexp-matcher
   (rexp-sequence "\""
		  (rexp* (rexp-alternatives
			  (char-set-difference
			   (char-set-difference
			    (ascii-range->char-set #x01 #x80)
			    (char-set #\return #\linefeed))
			   imap:char-set:quoted-specials)
			  (rexp-sequence "\\" imap:char-set:quoted-specials)))
		  "\"")))

(define (imap:match:literal string start end)
  (let ((regs (re-substring-match "{\\([0-9]+\\)}\r\n" string start end)))
    (and regs
	 (let ((index
		(+ (re-match-end-index 0 regs)
		   (substring->number string
				      (re-match-start-index 1 regs)
				      (re-match-end-index 1 regs)))))
	   (and (<= index end)
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

(define imap:match:section-text
  (alternatives-matcher
   (ci-string-matcher "header")
   (sequence-matcher (ci-string-matcher "header.fields")
		     (optional-matcher (ci-string-matcher ".not"))
		     (string-matcher " ")
		     (string-matcher "(")
		     (+-matcher imap:match:astring)
		     (string-matcher ")"))
   (ci-string-matcher "text")))

(define imap:match:section
  (alternatives-matcher
   imap:match:section-text
   (sequence-matcher imap:match:nz-number
		     (*-matcher (string-matcher ".")
				imap:match:nz-number)
		     (optional-matcher (string-matcher ".")
				       (alternatives-matcher
					imap:match:section-text
					(ci-string-matcher "mime"))))))

(define imap:parse:server
  (complete-parser
   (sequence-parser
    (optional-parser
     (let ((parse-user-id
	    (simple-parser imap:match:astring
			   'USER-ID))
	   (parse-auth
	    (prefix-parser (ci-string-matcher ";auth=")
			   (alternatives-matcher
			    (string-matcher "*")
			    imap:match:atom)
			   'AUTH-TYPE)))
       (sequence-parser
	(alternatives-parser
	 (sequence-parser parse-user-id
			  (optional-parser parse-auth))
	 (sequence-parser (optional-parser parse-user-id)
			  parse-auth))
	(trivial-parser (string-matcher "@")))))
    (simple-parser (rexp-matcher url:rexp:host)
		   'HOST)
    (optional-parser
     (prefix-parser (string-matcher ":")
		    (rexp-matcher (rexp+ char-set:numeric))
		    'PORT)))))

(define imap:parse:mailboxlist
  (complete-parser
   (sequence-parser
    (simple-parser
     (optional-matcher
      (alternatives-matcher
       (rexp-matcher
	(rexp+
	 (char-set-union imap:char-set:atom-char
			 imap:char-set:list-wildcards)))
       imap:match:string))
     'MAILBOX-LIST)
    (prefix-parser (ci-string-matcher ";type=")
		   (alternatives-matcher (ci-string-matcher "list")
					 (ci-string-matcher "lsub"))
		   'LIST-TYPE))))

(define imap:parse:mailbox
  (simple-parser imap:match:astring
		 'MAILBOX))

(define imap:parse:uidvalidity
  (optional-parser (prefix-parser (ci-string-matcher ";uidvalidity=")
				  imap:match:nz-number
				  'UID-VALIDITY)))

(define imap:parse:messagelist
  (complete-parser
   (sequence-parser imap:parse:mailbox
		    (optional-parser
		     (simple-parser imap:match:search-program
				    'SEARCH-PROGRAM))
		    imap:parse:uidvalidity)))

(define imap:parse:messagepart
  (complete-parser
   (sequence-parser imap:parse:mailbox
		    imap:parse:uidvalidity
		    (prefix-parser (ci-string-matcher "/;uid=")
				   imap:match:nz-number
				   'UID)
		    (prefix-parser (ci-string-matcher "/;section=")
				   imap:match:section
				   'SECTION))))