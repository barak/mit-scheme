;;; -*-Scheme-*-
;;;
;;; $Id: imap-syntax.scm,v 1.1 2000/04/18 21:30:57 cph Exp $
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

(define (url:decoding-parser match-encoded match-decoded keyword)
  (decoding-parser match-encoded url:decode-substring match-decoded keyword))

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

(define imap:parse:server
  (sequence-parser
   (optional-parser
    (let ((parse-user-id
	   (url:decoding-parser imap:match:achar+
				imap:match:astring
				'USER-ID))
	  (parse-auth
	   (sequence-parser
	    (noise-parser (ci-string-matcher ";auth="))
	    (alternatives-parser
	     (simple-parser (string-matcher "*") 'AUTH-TYPE)
	     (url:decoding-parser imap:match:achar+
				  imap:match:atom
				  'AUTH-TYPE)))))
      (sequence-parser
       (alternatives-parser
	(sequence-parser parse-user-id
			 (optional-parser parse-auth))
	(sequence-parser (optional-parser parse-user-id)
			 parse-auth))
       (noise-parser (string-matcher "@")))))
   (simple-parser (rexp-matcher url:rexp:host) 'HOST)
   (optional-parser
    (noise-parser (string-matcher ":"))
    (simple-parser (rexp-matcher (rexp+ char-set:numeric)) 'PORT))))

(define imap:parse:mailboxlist
  (sequence-parser
   (optional-parser
    (url:decoding-parser imap:match:bchar+
			 (alternatives-matcher
			  (rexp-matcher
			   (rexp+
			    (char-set-union imap:char-set:atom-char
					    imap:char-set:list-wildcards)))
			  imap:match:string)
			 'MAILBOX-LIST))
   (noise-parser (ci-string-matcher ";type="))
   (simple-parser (alternatives-matcher (ci-string-matcher "list")
					(ci-string-matcher "lsub"))
		  'LIST-TYPE)))

(define imap:parse:enc-mailbox
  (url:decoding-parser imap:match:bchar+ imap:match:astring 'MAILBOX))

(define imap:parse:uidvalidity
  (sequence-parser (noise-parser (ci-string-matcher ";uidvalidity="))
		   (simple-parser imap:match:nz-number 'UID-VALIDITY)))

(define imap:parse:messagelist
  (sequence-parser imap:parse:enc-mailbox
		   (optional-parser
		    (url:decoding-parser imap:match:bchar+
					 imap:match:search-program
					 'SEARCH-PROGRAM))
		   (optional-parser imap:parse:uidvalidity)))

(define imap:parse:messagepart
  (sequence-parser imap:parse:enc-mailbox
		   (optional-parser imap:parse:uidvalidity)
		   (noise-parser (ci-string-matcher "/;uid="))
		   (simple-parser imap:match:nz-number 'UID)
		   (optional-parser
		    (noise-parser (ci-string-matcher "/;section="))
		    (url:decoding-parser imap:match:bchar+
					 imap:match:section
					 'SECTION))))

(define imap:parse:simple-message
  (sequence-parser imap:parse:enc-mailbox
		   (noise-parser (ci-string-matcher "/;uid="))
		   (simple-parser imap:match:nz-number 'UID)))