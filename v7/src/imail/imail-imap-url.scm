;;; -*-Scheme-*-
;;;
;;; $Id: imail-imap-url.scm,v 1.2 2000/04/12 03:56:33 cph Exp $
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
  (userid define accessor)
  (auth-type define accessor)
  (hostname define accessor)
  (port define accessor))

(define-class (<imap-mailbox-url>
	       (constructor make-imap-mailbox-url
			    (userid auth-type hostname port
				    mailbox uid-validity uid section)))
    (<imap-url>)
  (mailbox define accessor)
  (uid-validity define accessor)
  (uid define accessor)
  (section define accessor))

(define-class (<imap-search-url>
	       (constructor make-imap-search-url
			    (userid auth-type hostname port
				    mailbox search-program uid-validity)))
    (<imap-url>)
  (mailbox define accessor)
  (search-program define accessor)
  (uid-validity define accessor))

(define-class (<imap-list-url>
	       (constructor make-imap-list-url
			    (userid auth-type hostname port
				    mailbox-list list-type)))
    (<imap-url>)
  (mailbox-list define accessor)
  (list-type define accessor))

(define-url-protocol "imap" <imap-url>
  (lambda (string)
    (parse-imap-url string)))

(define (parse-imap-url string)
  (let ((lose (lambda () (error:bad-range-argument string 'PARSE-IMAP-URL))))
    (if (not (string-prefix? "//" string))
	(lose))
    (let ((end (string-length string)))
      (let ((slash (substring-find-next-char string 2 end)))
	(if (not slash) (lose))
	(call-with-values (lambda () (parse-imap-url:server string 2 slash))
	  (lambda (userid auth-type hostname port)
	    (parse-imap-url:command string (fix:+ slash 1) end
	      (lambda (mailbox uid-validity uid section)
		(make-imap-mailbox-url userid auth-type hostname port
				       mailbox uid-validity uid section))
	      (lambda (mailbox search-program uid-validity)
		(make-imap-search-url userid auth-type hostname port
				      mailbox search-program uid-validity))
	      (lambda (mailbox-list list-type)
		(make-imap-list-url userid auth-type hostname port
				    mailbox-list list-type)))))))))

(define (parse-imap-url:server string start end)
  ???)

(define (parse-imap-url:command string start end if-mailbox if-search if-list)
  ???)

;;;; Matcher language

(define (regexp-matcher pattern)
  (let ((pattern (re-compile-pattern pattern #f)))
    (lambda (string start end)
      (let ((regs (re-substring-match pattern string start end)))
	(and regs
	     (re-match-end-index 0 regs))))))

(define (optional-matcher matcher)
  (lambda (string start end)
    (or (matcher string start end)
	start)))

(define (alternate-matcher . matchers)
  (lambda (string start end)
    (let loop ((matchers matchers))
      (and (pair? matchers)
	   (or ((car matchers) string start end)
	       (loop (cdr matchers)))))))

(define (sequential-matcher . matchers)
  (lambda (string start end)
    (let loop ((matchers matchers) (start start))
      (if (pair? matchers)
	  (let ((start* ((car matchers) string start end)))
	    (and start*
		 (loop (cdr matchers) start*)))
	  start))))

(define imap:char-set:quoted-specials
  (char-set #\" #\\))

(define imap:char-set:list-wildcards
  (char-set #\% #\*))

(define imap:char-set:atom-specials
  (char-set-union (char-set #\( #\) #\{ #\space #\rubout)
		  imap:char-set:quoted-specials
		  imap:char-set:list-wildcards
		  (ascii-range->char-set #x00 #x20)))

(define imap:char-set:atom-char
  (char-set-invert imap:char-set:atom-specials))

(define imap:char-set:text-char
  (char-set-difference (ascii-range->char-set #x01 #x80)
		       (char-set #\return #\linefeed)))

(define imap:match-atom
  (regexp-matcher
   (string-append (char-set->regexp imap:char-set:atom-char)
		  "+")))

(define imap:match-quoted-string
  (regexp-matcher
   (string-append
    "\""
    (regexp-group (char-set->regexp
		   (char-set-difference imap:char-set:text-char
					imap:char-set:quoted-specials))
		  (string-append
		   "\\\\"
		   (char-set->regexp imap:char-set:quoted-specials)))
    "*\"")))

(define (imap:match-literal string start end)
  (let ((regs (re-substring-match "{\\([0-9]+\\)}\r\n" string start end)))
    (and regs
	 (let ((index
		(+ (re-match-end-index 0 regs)
		   (substring->number string
				      (re-match-start-index 1 regs)
				      (re-match-end-index 1 regs)))))
	   (and (<= index end)
		index)))))

(define (imap:match-astring string start end)
  (or (imap:match-atom string start end)
      (imap:match-string string start end)))

(define (imap:match-string string start end)
  (or (imap:match-quoted-string string start end)
      (imap:match-literal string start end)))

(define imap:char-set:achar
  (char-set-union url:char-set:unreserved
		  (string->char-set "&=~")))

(define imap:regexp:achar
  (regexp-group (char-set->regexp imap:char-set:achar)
		url:regexp:escape))

(define imap:regexp:achar+
  (string-append imap:regexp:achar "+"))

(define imap:char-set:bchar
  (char-set-union imap:char-set:achar
		  (string->char-set ":@/")))

(define imap:regexp:bchar
  (regexp-group (char-set->regexp imap:char-set:bchar)
		url:regexp:escape))

(define imap:regexp:bchar+
  (string-append imap:regexp:bchar "+"))

(define imap:regexp:enc-auth-type imap:regexp:achar+)
(define imap:regexp:enc-list-mailbox imap:regexp:bchar+)
(define imap:regexp:enc-mailbox imap:regexp:bchar+)
(define imap:regexp:enc-search imap:regexp:bchar+)
(define imap:regexp:enc-section imap:regexp:bchar+)
(define imap:regexp:enc-user imap:regexp:achar+)

(define imap:regexp:iauth
  (string-append ";AUTH=" (regexp-group "\\*" imap:regexp:enc-auth-type)))

(define (regexp-optional regexp)
  (string-append (regexp-group regexp) "?"))

(define imap:regexp:iuserauth
  (regexp-group (string-append imap:regexp:enc-user
			       (regexp-optional imap:regexp:iauth))
		(string-append (regexp-optional imap:regexp:enc-user)
			       imap:regexp:iauth)))

(define imap:regexp:iserver
  (string-append (regexp-optional (string-append imap:regexp:iuserauth "@"))
		 url:regexp:hostport))

(define imap:regexp:imailboxlist
  (string-append (regexp-optional imap:regexp:enc-list-mailbox)
		 ";TYPE="
		 (regexp-group "LIST" "LSUB")))

(define imap:regexp:nz-number
  "[1-9][0-9]*")

(define imap:regexp:uidvalidity
  (string-append ";UIDVALIDITY=" imap:regexp:nz-number))

(define imap:regexp:iuid
  (string-append ";UID=" imap:regexp:nz-number))

(define imap:regexp:imessagelist
  (string-append imap:regexp:enc-mailbox
		 (regexp-optional (string-append "\\?" imap:regexp:enc-search))
		 (regexp-optional imap:regexp:uidvalidity)))

(define imap:regexp:imessagepart
  (string-append imap:regexp:enc-mailbox
		 (regexp-optional imap:regexp:uidvalidity)
		 imap:regexp:iuid
		 (regexp-optional
		  (string-append "/;SECTION=" imap:regexp:enc-section))))
		 