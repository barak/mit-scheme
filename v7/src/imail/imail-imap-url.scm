;;; -*-Scheme-*-
;;;
;;; $Id: imail-imap-url.scm,v 1.3 2000/04/13 16:40:23 cph Exp $
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

(define (rexp-matcher pattern)
  (let ((pattern (rexp-compile pattern)))
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
  (rexp-matcher (rexp+ imap:char-set:atom-char)))

(define imap:match-quoted-string
  (rexp-matcher
   (rexp-sequence "\""
		  (rexp* (rexp-alternatives
			  (char-set-difference imap:char-set:text-char
					       imap:char-set:quoted-specials)
			  (rexp-sequence "\\" imap:char-set:quoted-specials)))
		  "\"")))

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

(define imap:rexp:achar+
  (rexp+ (rexp-alternatives (char-set-union url:char-set:unreserved
					    (string->char-set "&=~"))
			    url:rexp:escape)))

(define imap:rexp:bchar+
  (rexp+ (rexp-alternatives (char-set-union imap:char-set:achar
					    (string->char-set ":@/"))
			    url:rexp:escape)))

(define imap:rexp:enc-auth-type imap:rexp:achar+)
(define imap:rexp:enc-list-mailbox imap:rexp:bchar+)
(define imap:rexp:enc-mailbox imap:rexp:bchar+)
(define imap:rexp:enc-search imap:rexp:bchar+)
(define imap:rexp:enc-section imap:rexp:bchar+)
(define imap:rexp:enc-user imap:rexp:achar+)

(define imap:rexp:iauth
  (rexp-sequence ";AUTH=" (regexp-alternatives "*" imap:rexp:enc-auth-type)))

(define imap:rexp:iuserauth
  (rexp-alternatives (rexp-sequence imap:rexp:enc-user
				    (rexp-optional imap:rexp:iauth))
		     (rexp-sequence (rexp-optional imap:rexp:enc-user)
				    imap:rexp:iauth)))

(define imap:rexp:iserver
  (rexp-sequence (rexp-optional (rexp-sequence imap:rexp:iuserauth "@"))
		 url:rexp:hostport))

(define imap:rexp:imailboxlist
  (rexp-sequence (rexp-optional imap:rexp:enc-list-mailbox)
		 ";TYPE="
		 (rexp-alternatives "LIST" "LSUB")))

(define imap:rexp:nz-number
  (rexp-sequence (char-set-difference char-set:numeric (char-set #\0))
		 (rexp* char-set:numeric)))

(define imap:rexp:uidvalidity
  (rexp-sequence ";UIDVALIDITY=" imap:rexp:nz-number))

(define imap:rexp:iuid
  (rexp-sequence ";UID=" imap:rexp:nz-number))

(define imap:rexp:imessagelist
  (rexp-sequence imap:rexp:enc-mailbox
		 (rexp-optional (rexp-sequence "?" imap:rexp:enc-search))
		 (rexp-optional imap:rexp:uidvalidity)))

(define imap:rexp:imessagepart
  (rexp-sequence imap:rexp:enc-mailbox
		 (rexp-optional imap:rexp:uidvalidity)
		 imap:rexp:iuid
		 (rexp-optional
		  (rexp-sequence "/;SECTION=" imap:rexp:enc-section))))
		 