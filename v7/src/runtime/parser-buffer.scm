#| -*-Scheme-*-

$Id: parser-buffer.scm,v 1.10 2003/10/11 04:00:17 cph Exp $

Copyright 2001,2002,2003 Massachusetts Institute of Technology

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
  at-end?
  ;; The number of newlines to the left of the current position.
  line)

;;; The two basic kinds of buffers: substring and source.  A substring
;;; buffer is one that reads from a pre-filled substring.  A source
;;; buffer is one that reads from an unbuffered source of unbounded
;;; length.

(define (substring->parser-buffer string start end)
  (make-parser-buffer string start end 0 start #f #t 0))

(define (source->parser-buffer source)
  (make-parser-buffer (make-string min-length) 0 0 0 0 source #f 0))

(define-integrable min-length 256)

(define (string->parser-buffer string)
  (substring->parser-buffer string 0 (string-length string)))

(define (input-port->parser-buffer port)
  (source->parser-buffer
   (lambda (string start end)
     (read-substring! string start end port))))

(define-structure parser-buffer-pointer
  (index #f read-only #t)
  (line #f read-only #t))

(define (get-parser-buffer-pointer buffer)
  ;; Get an object that represents the current position.
  (make-parser-buffer-pointer (+ (parser-buffer-base-offset buffer)
				 (parser-buffer-index buffer))
			      (parser-buffer-line buffer)))

(define (set-parser-buffer-pointer! buffer p)
  ;; Move the current position to P, which must be an object that was
  ;; previously returned by GET-PARSER-BUFFER-POINTER.  The position
  ;; may only be moved to the left.
  (set-parser-buffer-index! buffer (pointer->index p buffer))
  (set-parser-buffer-line! buffer (parser-buffer-pointer-line p)))

(define (get-parser-buffer-tail buffer p)
  (call-with-parser-buffer-tail buffer p substring))

(define (call-with-parser-buffer-tail buffer p procedure)
  ;; P must be a buffer pointer previously returned by
  ;; GET-PARSER-BUFFER-POINTER.  Call PROCEDURE on the substring
  ;; between P and the current buffer pointer.
  (procedure (parser-buffer-string buffer)
	     (pointer->index p buffer)
	     (parser-buffer-index buffer)))

(define (pointer->index p buffer)
  (if (parser-buffer-pointer? p)
      (let ((p*
	     (- (parser-buffer-pointer-index p)
		(parser-buffer-base-offset buffer))))
	(if (<= (parser-buffer-start buffer) p* (parser-buffer-index buffer))
	    p*
	    (error:bad-range-argument p 'POINTER->INDEX)))
      (error:wrong-type-argument p "parser-buffer pointer" 'POINTER->INDEX)))

(define (parser-buffer-position-string object)
  (let ((pointer
	 (if (parser-buffer-pointer? object)
	     object
	     (get-parser-buffer-pointer object))))
    (string-append
     "line "
     (number->string (+ (parser-buffer-pointer-line pointer) 1))
     ", char "
     (number->string (+ (parser-buffer-pointer-index pointer) 1)))))

(define (read-parser-buffer-char buffer)
  ;; Attempt to read the next character from BUFFER, starting at the
  ;; current position.  If there is a character available, increment
  ;; the position and return the character.  If there are no more
  ;; characters available, return #F and leave the position unchanged.
  (and (guarantee-buffer-chars buffer 1)
       (let ((char
	      (string-ref (parser-buffer-string buffer)
			  (parser-buffer-index buffer))))
	 (increment-buffer-index! buffer char)
	 char)))

(define (peek-parser-buffer-char buffer)
  ;; Attempt to read the next character from BUFFER, starting at the
  ;; current position.  If there is a character available, return it,
  ;; otherwise return #F.  The position is unaffected in either case.
  (and (guarantee-buffer-chars buffer 1)
       (string-ref (parser-buffer-string buffer)
		   (parser-buffer-index buffer))))

(define (parser-buffer-ref buffer index)
  (if (not (index-fixnum? index))
      (error:wrong-type-argument index "index" 'PARSER-BUFFER-REF))
  (and (guarantee-buffer-chars buffer (fix:+ index 1))
       (string-ref (parser-buffer-string buffer)
		   (fix:+ (parser-buffer-index buffer) index))))

(define-syntax char-matcher
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form))
	   (test
	    (make-syntactic-closure environment '(REFERENCE CHAR)
	      (caddr form))))
       `(BEGIN
	  (DEFINE (,(symbol-append 'MATCH-PARSER-BUFFER- name '-NO-ADVANCE)
		   BUFFER REFERENCE)
	    (AND (GUARANTEE-BUFFER-CHARS BUFFER 1)
		 (LET ((CHAR
			(STRING-REF (PARSER-BUFFER-STRING BUFFER)
				    (PARSER-BUFFER-INDEX BUFFER))))
		   (DECLARE (INTEGRATE CHAR))
		   ,test)))
	  (DEFINE (,(symbol-append 'MATCH-PARSER-BUFFER- name)
		   BUFFER REFERENCE)
	    (AND (GUARANTEE-BUFFER-CHARS BUFFER 1)
		 (LET ((CHAR
			(STRING-REF (PARSER-BUFFER-STRING BUFFER)
				    (PARSER-BUFFER-INDEX BUFFER))))
		   (AND ,test
			(BEGIN
			  (INCREMENT-BUFFER-INDEX! BUFFER CHAR)
			  #T))))))))))

(char-matcher char (char=? char reference))
(char-matcher char-ci (char-ci=? char reference))
(char-matcher not-char (not (char=? char reference)))
(char-matcher not-char-ci (not (char-ci=? char reference)))
(char-matcher char-in-set (char-set-member? reference char))

(define (match-utf8-char-in-alphabet buffer alphabet)
  (let ((p (get-parser-buffer-pointer buffer)))
    (if (let ((char
	       (read-utf8-char-from-source
		(lambda ()
		  (let ((char (read-parser-buffer-char buffer)))
		    (and char
			 (char->integer char)))))))
	  (and (not (eof-object? char))
	       (char-in-alphabet? char alphabet)))
	#t
	(begin
	  (set-parser-buffer-pointer! buffer p)
	  #f))))

(define-syntax string-matcher
  (sc-macro-transformer
   (lambda (form environment)
     (let ((suffix (cadr form)))
       `(DEFINE (,(intern
		   (string-append "match-parser-buffer-string" suffix))
		 BUFFER STRING)
	  (,(close-syntax
	     (intern
	      (string-append "match-parser-buffer-substring" suffix))
	     environment)
	   BUFFER STRING 0 (STRING-LENGTH STRING)))))))

(string-matcher "")
(string-matcher "-ci")
(string-matcher "-no-advance")
(string-matcher "-ci-no-advance")

(define-syntax substring-matcher
  (sc-macro-transformer
   (lambda (form environment)
     (let ((suffix (cadr form)))
       `(DEFINE (,(intern
		   (string-append "match-parser-buffer-substring" suffix))
		 BUFFER STRING START END)
	  (LET ((N (FIX:- END START)))
	    (AND (GUARANTEE-BUFFER-CHARS BUFFER N)
		 (,(close-syntax
		    (intern (string-append "substring" suffix "=?"))
		    environment)
		  STRING START END
		  (PARSER-BUFFER-STRING BUFFER)
		  (PARSER-BUFFER-INDEX BUFFER)
		  (FIX:+ (PARSER-BUFFER-INDEX BUFFER) N))
		 (BEGIN
		   (BUFFER-INDEX+N! BUFFER N)
		   #T))))))))

(substring-matcher "")
(substring-matcher "-ci")

(define-syntax substring-matcher-no-advance
  (sc-macro-transformer
   (lambda (form environment)
     (let ((suffix (cadr form)))
       `(DEFINE (,(intern
		   (string-append "match-parser-buffer-substring"
				  suffix
				  "-no-advance"))
		 BUFFER STRING START END)
	  (LET ((N (FIX:- END START)))
	    (AND (GUARANTEE-BUFFER-CHARS BUFFER N)
		 (,(close-syntax
		    (intern (string-append "substring" suffix "=?"))
		    environment)
		  STRING START END
		  (PARSER-BUFFER-STRING BUFFER)
		  (PARSER-BUFFER-INDEX BUFFER)
		  (FIX:+ (PARSER-BUFFER-INDEX BUFFER) N)))))))))

(substring-matcher-no-advance "")
(substring-matcher-no-advance "-ci")

(define-integrable (increment-buffer-index! buffer char)
  (set-parser-buffer-index! buffer (fix:+ (parser-buffer-index buffer) 1))
  (if (char=? char #\newline)
      (set-parser-buffer-line! buffer (fix:+ (parser-buffer-line buffer) 1))))

(define (buffer-index+n! buffer n)
  (let ((i (parser-buffer-index buffer))
	(s (parser-buffer-string buffer)))
    (let ((j (fix:+ i n)))
      (do ((i i (fix:+ i 1)))
	  ((fix:= i j))
	(if (char=? (string-ref s i) #\newline)
	    (set-parser-buffer-line! buffer
				     (fix:+ (parser-buffer-line buffer) 1))))
      (set-parser-buffer-index! buffer j))))

(define-integrable (guarantee-buffer-chars buffer n)
  (or (fix:<= (fix:+ (parser-buffer-index buffer) n)
	      (parser-buffer-end buffer))
      (guarantee-buffer-chars-1 buffer n)))

(define (guarantee-buffer-chars-1 buffer n)
  (let ((min-end (fix:+ (parser-buffer-index buffer) n))
	(end (parser-buffer-end buffer)))
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
		   #f)))))))

(define (discard-parser-buffer-head! buffer)
  ;; Tell the buffer that it is safe to discard all characters to the
  ;; left of the current position.
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