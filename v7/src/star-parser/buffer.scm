;;; -*-Scheme-*-
;;;
;;; $Id: buffer.scm,v 1.1 2001/06/26 18:03:09 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
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

;;;; Parser-buffer abstraction

(declare (usual-integrations))

;;;; Parser buffer abstraction

(define-structure parser-buffer
  ;; The string buffer, as a substring:
  string
  start
  end
  ;; The offset of the string buffer within the character stream.
  ;; This is always zero if SOURCE is #F.
  base-offset
  ;; Our current position in the buffer.
  index
  ;; A procedure that is used to replenish the buffer when the
  ;; buffered characters are used up.  The procedure takes three
  ;; arguments, (STRING START END), and attempts to fill the
  ;; corresponding substring, returning the number of characters
  ;; actually written.  If SOURCE is #F, the buffered characters are
  ;; the entire stream.
  source
  ;; True if there are no more characters past END.
  at-end?)

;;; The two basic kinds of buffers: substring and source.  A substring
;;; buffer is one that reads from a pre-filled substring.  A source
;;; buffer is one that reads from an unbuffered source of unbounded
;;; length.

(define (substring->parser-buffer string start end)
  (make-parser-buffer string start end 0 start #f #t))

(define (source->parser-buffer source)
  (make-parser-buffer (make-string min-length) 0 0 0 0 source #f))

(define-integrable min-length 256)

(define (string->parser-buffer string)
  (substring->parser-buffer string 0 (string-length string)))

(define (input-port->parser-buffer port)
  (source->parser-buffer
   (lambda (string start end)
     (read-substring! string start end port))))

(define (get-parser-buffer-pointer buffer)
  ;; Get an object that represents the current buffer pointer.
  (+ (parser-buffer-base-offset buffer)
     (parser-buffer-index buffer)))

(define (set-parser-buffer-pointer! buffer p)
  ;; Move the buffer pointer to the location represented by P.  P must
  ;; be an object that was previously returned by GET-PARSER-BUFFER-POINTER.
  ;; The buffer pointer may only be moved to the left.
  (let ((p* (- p (parser-buffer-base-offset buffer))))
    (if (not (<= (parser-buffer-start buffer) p* (parser-buffer-index buffer)))
	(error:bad-range-argument p 'SET-PARSER-BUFFER-POINTER!))
    (set-parser-buffer-index! buffer p*)))

(define (decrement-parser-buffer-pointer buffer)
  ;; Decrement the buffer pointer by one.
  (if (fix:< (parser-buffer-start buffer) (parser-buffer-index buffer))
      (set-parser-buffer-index! buffer (fix:- (parser-buffer-index buffer) 1))
      (error "Can't decrement buffer pointer:" buffer)))

(define (get-parser-buffer-tail buffer p)
  ;; P must be a buffer pointer previously returned by
  ;; GET-PARSER-BUFFER-POINTER.  Return the string of characters
  ;; between P and the current buffer pointer.
  (let ((p* (- p (parser-buffer-base-offset buffer))))
    (if (not (<= (parser-buffer-start buffer) p* (parser-buffer-index buffer)))
	(error:bad-range-argument p 'GET-PARSER-BUFFER-TAIL))
    (substring (parser-buffer-string buffer)
	       p*
	       (parser-buffer-index buffer))))

(let-syntax
    ((char-matcher
      (lambda (name test)
	`(DEFINE (,(symbol-append 'MATCH-PARSER-BUFFER- name) BUFFER REFERENCE)
	   (AND (LET ((CHAR (PEEK-PARSER-BUFFER-CHAR BUFFER)))
		  (AND CHAR
		       ,test))
		(BEGIN
		  (SET-PARSER-BUFFER-INDEX! BUFFER
					    (FIX:+ (PARSER-BUFFER-INDEX BUFFER)
						   1))
		  #T))))))
  (char-matcher char (char=? char reference))
  (char-matcher char-ci (char-ci=? char reference))
  (char-matcher not-char (not (char=? char reference)))
  (char-matcher not-char-ci (not (char-ci=? char reference)))
  (char-matcher char-in-set (char-set-member? reference char)))

(let-syntax
    ((string-matcher
      (lambda (suffix)
	(let ((name
	       (intern (string-append "match-parser-buffer-string" suffix)))
	      (match-substring
	       (intern
		(string-append "match-parser-buffer-substring" suffix))))
	  `(DEFINE (,name BUFFER STRING)
	     (,match-substring BUFFER STRING 0 (STRING-LENGTH STRING)))))))
  (string-matcher "")
  (string-matcher "-ci")
  (string-matcher "-no-advance")
  (string-matcher "-ci-no-advance"))

(let-syntax
    ((substring-matcher
      (lambda (suffix)
	`(DEFINE (,(intern
		    (string-append "match-parser-buffer-substring" suffix))
		  BUFFER STRING START END)
	   (LET ((N (FIX:- END START)))
	     (AND (GUARANTEE-BUFFER-CHARS BUFFER N)
		  (,(intern (string-append "substring" suffix "=?"))
		   STRING START END
		   (PARSER-BUFFER-STRING BUFFER)
		   (PARSER-BUFFER-INDEX BUFFER)
		   (FIX:+ (PARSER-BUFFER-INDEX BUFFER) N))
		  (BEGIN
		    (SET-PARSER-BUFFER-INDEX!
		     BUFFER
		     (FIX:+ (PARSER-BUFFER-INDEX BUFFER) N))
		    #T)))))))
  (substring-matcher "")
  (substring-matcher "-ci"))

(let-syntax
    ((substring-matcher
      (lambda (suffix)
	`(DEFINE (,(intern
		    (string-append "match-parser-buffer-substring"
				   suffix
				   "-no-advance"))
		  BUFFER STRING START END)
	   (LET ((N (FIX:- END START)))
	     (AND (GUARANTEE-BUFFER-CHARS BUFFER N)
		  (,(intern (string-append "char" suffix "=?"))
		   STRING START END
		   (PARSER-BUFFER-STRING BUFFER)
		   (PARSER-BUFFER-INDEX BUFFER)
		   (FIX:+ (PARSER-BUFFER-INDEX BUFFER) N))))))))
  (substring-matcher "")
  (substring-matcher "-ci"))

(define (read-parser-buffer-char buffer)
  ;; Attempt to read the next character from BUFFER, starting at the
  ;; buffer pointer.  If there is a character available, increment the
  ;; buffer pointer and return the character.  If there are no more
  ;; characters available, return #F and leave the buffer pointer
  ;; unchanged.
  (let ((char (peek-parser-buffer-char buffer)))
    (if char
	(set-parser-buffer-index! buffer
				  (fix:+ (parser-buffer-index buffer) 1)))
    char))

(define (peek-parser-buffer-char buffer)
  ;; Attempt to read the next character from BUFFER, starting at the
  ;; buffer pointer.  If there is a character available, return it,
  ;; otherwise return #F.  The buffer pointer is unaffected in either
  ;; case.
  (and (guarantee-buffer-chars buffer 1)
       (string-ref (parser-buffer-string buffer)
		   (parser-buffer-index buffer))))

(define (guarantee-buffer-chars buffer n)
  (let ((min-end (fix:+ (parser-buffer-index buffer) n))
	(end (parser-buffer-end buffer)))
    (or (fix:<= min-end end)
	(and (not (parser-buffer-at-end? buffer))
	     (begin
	       (let* ((string (parser-buffer-string buffer))
		      (max-end (string-length string))
		      (max-end*
		       (let loop ((max-end* max-end))
			 (if (fix:<= min-end max-end*)
			     max-end*
			     (loop (fix:* max-end* 2))))))
		 (if (fix:> max-end* max-end)
		     (let ((string* (make-string max-end*)))
		       (string-move! string string* 0)
		       (set-parser-buffer-string! buffer string*))))
	       (let ((n-read
		      (let ((string (parser-buffer-string buffer)))
			((parser-buffer-source buffer)
			 string end (string-length string)))))
		 (if (fix:> n-read 0)
		     (let ((end (fix:+ end n-read)))
		       (set-parser-buffer-end! buffer end)
		       (fix:<= min-end end))
		     (begin
		       (set-parser-buffer-at-end?! buffer #t)
		       #f))))))))

(define (discard-parser-buffer-head! buffer)
  ;; Tell the buffer that it is safe to discard all characters to the
  ;; left of the current buffer pointer.  We promise not to backtrack
  ;; from here, and the buffer is allowed to enforce the promise.
  (if (parser-buffer-source buffer)
      (let ((string (parser-buffer-string buffer))
	    (index (parser-buffer-index buffer))
	    (end (parser-buffer-end buffer)))
	(if (fix:< 0 index)
	    (let* ((end* (fix:- end index))
		   (string*
		    (let ((n (string-length string)))
		      (if (and (fix:> n min-length)
			       (fix:<= end* (fix:quotient n 4)))
			  (make-string (fix:quotient n 2))
			  string))))
	      (without-interrupts
	       (lambda ()
		 (substring-move! string index end string* 0)
		 (set-parser-buffer-string! buffer string*)
		 (set-parser-buffer-index! buffer 0)
		 (set-parser-buffer-end! buffer end*)
		 (set-parser-buffer-base-offset!
		  buffer
		  (+ (parser-buffer-base-offset buffer) index)))))))
      (set-parser-buffer-start! buffer (parser-buffer-index buffer))))