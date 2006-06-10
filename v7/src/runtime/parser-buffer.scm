#| -*-Scheme-*-

$Id: parser-buffer.scm,v 1.16 2006/06/10 04:06:47 cph Exp $

Copyright 2001,2002,2003,2004,2006 Massachusetts Institute of Technology

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
  ;; This is always zero if PORT is #F.
  base-offset
  ;; Our current position in the buffer.
  index
  ;; An input port that is used to replenish the buffer when the
  ;; buffered characters are used up.  If PORT is #F, the buffered
  ;; characters are the entire stream.
  port
  ;; True if there are no more characters past END.
  at-end?
  ;; The number of newlines to the left of the current position.
  line)

;;; The two basic kinds of buffers: string and port.  A string buffer
;;; is one that reads from a pre-filled string.  A port buffer is one
;;; that reads from an input port.

(define (string->parser-buffer string #!optional start end)
  (if (string? string)
      (let ((string (string->wide-string string start end)))
	(make-parser-buffer string 0 (%wide-string-length string) 0 0 #f #t 0))
      (begin
	(guarantee-wide-string string 'STRING->PARSER-BUFFER)
	(let* ((end
		(if (or (default-object? end) (not end))
		    (%wide-string-length string)
		    (guarantee-substring-end-index end
						   (%wide-string-length string)
						   'STRING->PARSER-BUFFER)))
	       (start
		(if (or (default-object? start) (not start))
		    0
		    (guarantee-substring-start-index start end
						     'STRING->PARSER-BUFFER))))
	  (make-parser-buffer string start end 0 0 #f #t 0)))))

(define (input-port->parser-buffer port)
  (guarantee-input-port port 'INPUT-PORT->PARSER-BUFFER)
  (make-parser-buffer (make-wide-string min-length) 0 0 0 0 port #f 0))

(define-integrable min-length 256)

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
  (call-with-parser-buffer-tail buffer p wide-string->utf8-string))

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

(define (parser-buffer-error ptr msg . irritants)
  (apply error
	 (string-append msg
			" at "
			(parser-buffer-position-string ptr)
			(if (pair? irritants) ":" "."))
	 irritants))

(define (read-parser-buffer-char buffer)
  ;; Attempt to read the next character from BUFFER, starting at the
  ;; current position.  If there is a character available, increment
  ;; the position and return the character.  If there are no more
  ;; characters available, return #F and leave the position unchanged.
  (and (guarantee-buffer-chars buffer 1)
       (let ((char
	      (%wide-string-ref (parser-buffer-string buffer)
				(parser-buffer-index buffer))))
	 (increment-buffer-index! buffer char)
	 char)))

(define (peek-parser-buffer-char buffer)
  ;; Attempt to read the next character from BUFFER, starting at the
  ;; current position.  If there is a character available, return it,
  ;; otherwise return #F.  The position is unaffected in either case.
  (and (guarantee-buffer-chars buffer 1)
       (%wide-string-ref (parser-buffer-string buffer)
			 (parser-buffer-index buffer))))

(define (parser-buffer-ref buffer index)
  (if (not (index-fixnum? index))
      (error:wrong-type-argument index "index" 'PARSER-BUFFER-REF))
  (and (guarantee-buffer-chars buffer (fix:+ index 1))
       (%wide-string-ref (parser-buffer-string buffer)
			 (fix:+ (parser-buffer-index buffer) index))))

(define (match-parser-buffer-char buffer char)
  (match-char buffer char char=?))

(define (match-parser-buffer-not-char buffer char)
  (match-char-not buffer char char=?))

(define (match-parser-buffer-char-no-advance buffer char)
  (match-char-no-advance buffer char char=?))

(define (match-parser-buffer-not-char-no-advance buffer char)
  (match-char-not-no-advance buffer char char=?))

(define (match-parser-buffer-char-ci buffer char)
  (match-char buffer char char-ci=?))

(define (match-parser-buffer-not-char-ci buffer char)
  (match-char-not buffer char char-ci=?))

(define (match-parser-buffer-char-ci-no-advance buffer char)
  (match-char-no-advance buffer char char-ci=?))

(define (match-parser-buffer-not-char-ci-no-advance buffer char)
  (match-char-not-no-advance buffer char char-ci=?))

(define (match-parser-buffer-char-in-set buffer set)
  (match-char buffer set char-in-set?))

(define (match-parser-buffer-char-not-in-set buffer set)
  (match-char-not buffer set char-in-set?))

(define (match-parser-buffer-char-in-set-no-advance buffer set)
  (match-char-no-advance buffer set char-in-set?))

(define (match-parser-buffer-char-not-in-set-no-advance buffer set)
  (match-char-not-no-advance buffer set char-in-set?))

(define-integrable (char-in-set? char set)
  (char-set-member? set char))

(define (match-parser-buffer-char-in-alphabet buffer alphabet)
  (match-char buffer alphabet char-in-alphabet?))

(define (match-parser-buffer-char-not-in-alphabet buffer alphabet)
  (match-char-not buffer alphabet char-in-alphabet?))

(define (match-parser-buffer-char-in-alphabet-no-advance buffer alphabet)
  (match-char-no-advance buffer alphabet char-in-alphabet?))

(define (match-parser-buffer-char-not-in-alphabet-no-advance buffer alphabet)
  (match-char-not-no-advance buffer alphabet char-in-alphabet?))

(define-integrable (match-char buffer reference compare)
  (and (guarantee-buffer-chars buffer 1)
       (let ((char
	      (%wide-string-ref (parser-buffer-string buffer)
				(parser-buffer-index buffer))))
	 (and (compare char reference)
	      (begin
		(increment-buffer-index! buffer char)
		#t)))))

(define-integrable (match-char-no-advance buffer reference compare)
  (and (guarantee-buffer-chars buffer 1)
       (compare (%wide-string-ref (parser-buffer-string buffer)
				  (parser-buffer-index buffer))
		reference)))

(define-integrable (match-char-not buffer reference compare)
  (match-char buffer reference
	      (lambda (c1 c2)
		(declare (integrate c1 c2))
		(not (compare c1 c2)))))

(define-integrable (match-char-not-no-advance buffer reference compare)
  (match-char-no-advance buffer reference
			 (lambda (c1 c2)
			   (declare (integrate c1 c2))
			   (not (compare c1 c2)))))

(define (match-parser-buffer-string buffer string)
  (match-string buffer string match-substring-loop char=?))

(define (match-parser-buffer-string-ci buffer string)
  (match-string buffer string match-substring-loop char-ci=?))

(define (match-parser-buffer-string-no-advance buffer string)
  (match-string buffer string match-substring-loop-na char=?))

(define (match-parser-buffer-string-ci-no-advance buffer string)
  (match-string buffer string match-substring-loop-na char-ci=?))

(define-integrable (match-string buffer string loop compare)
  (cond ((wide-string? string)
	 (let ((v (wide-string-contents string)))
	   (let ((n (vector-length v)))
	     (loop buffer v 0 n compare vector-ref))))
	((string? string)
	 (let ((n (string-length string)))
	   (loop buffer string 0 n compare string-ref)))
	(else
	 (error:wrong-type-argument string "string" #f))))

(define (match-parser-buffer-substring buffer string start end)
  (match-substring buffer string start end match-substring-loop char=?))

(define (match-parser-buffer-substring-ci buffer string start end)
  (match-substring buffer string start end match-substring-loop char-ci=?))

(define (match-parser-buffer-substring-no-advance buffer string start end)
  (match-substring buffer string start end match-substring-loop-na char=?))

(define (match-parser-buffer-substring-ci-no-advance buffer string start end)
  (match-substring buffer string start end match-substring-loop-na char-ci=?))

(define-integrable (match-substring buffer string start end loop compare)
  (cond ((wide-string? string)
	 (let ((v (wide-string-contents string)))
	   (loop buffer v start end compare vector-ref)))
	((string? string)
	 (loop buffer string start end compare string-ref))
	(else
	 (error:wrong-type-argument string "string" #f))))

(define-integrable (match-substring-loop buffer string start end
					 compare extract)
  (and (guarantee-buffer-chars buffer (fix:- end start))
       (let ((bv (wide-string-contents (parser-buffer-string buffer))))
	 (let loop
	     ((i start)
	      (bi (parser-buffer-index buffer))
	      (bl (parser-buffer-line buffer)))
	   (if (fix:< i end)
	       (and (compare (extract string i) (vector-ref bv bi))
		    (loop (fix:+ i 1)
			  (fix:+ bi 1)
			  (if (char=? (vector-ref bv bi) #\newline)
			      (fix:+ bl 1)
			      bl)))
	       (begin
		 (set-parser-buffer-index! buffer bi)
		 (set-parser-buffer-line! buffer bl)
		 #t))))))

(define-integrable (match-substring-loop-na buffer string start end
					    compare extract)
  (and (guarantee-buffer-chars buffer (fix:- end start))
       (let ((bv (wide-string-contents (parser-buffer-string buffer))))
	 (let loop ((i start) (bi (parser-buffer-index buffer)))
	   (if (fix:< i end)
	       (and (compare (extract string i) (vector-ref bv bi))
		    (loop (fix:+ i 1) (fix:+ bi 1)))
	       #t)))))

(define-integrable (increment-buffer-index! buffer char)
  (set-parser-buffer-index! buffer (fix:+ (parser-buffer-index buffer) 1))
  (if (char=? char #\newline)
      (set-parser-buffer-line! buffer (fix:+ (parser-buffer-line buffer) 1))))

(define (buffer-index+n! buffer n)
  (let ((i (parser-buffer-index buffer))
	(v (wide-string-contents (parser-buffer-string buffer))))
    (let ((j (fix:+ i n)))
      (let loop ((i i) (n (parser-buffer-line buffer)))
	(if (fix:< i j)
	    (loop (fix:+ i 1)
		  (if (char=? (vector-ref v i) #\newline) (fix:+ n 1) n))
	    (set-parser-buffer-line! buffer n)))
      (set-parser-buffer-index! buffer j))))

(define (discard-parser-buffer-head! buffer)
  ;; Tell the buffer that it is safe to discard all characters to the
  ;; left of the current position.
  (if (parser-buffer-port buffer)
      (let ((string (parser-buffer-string buffer))
	    (index (parser-buffer-index buffer))
	    (end (parser-buffer-end buffer)))
	(if (fix:< 0 index)
	    (let* ((end* (fix:- end index))
		   (string*
		    (let ((n (%wide-string-length string)))
		      (if (and (fix:> n min-length)
			       (fix:<= end* (fix:quotient n 4)))
			  (make-wide-string (fix:quotient n 2))
			  string))))
	      (without-interrupts
	       (lambda ()
		 (subvector-move-left! (wide-string-contents string) index end
				       (wide-string-contents string*) 0)
		 (set-parser-buffer-string! buffer string*)
		 (set-parser-buffer-index! buffer 0)
		 (set-parser-buffer-end! buffer end*)
		 (set-parser-buffer-base-offset!
		  buffer
		  (+ (parser-buffer-base-offset buffer) index)))))))
      (set-parser-buffer-start! buffer (parser-buffer-index buffer))))

(define-integrable (guarantee-buffer-chars buffer n)
  (or (fix:<= (fix:+ (parser-buffer-index buffer) n)
	      (parser-buffer-end buffer))
      (guarantee-buffer-chars-1 buffer n)))

(define (guarantee-buffer-chars-1 buffer n)
  ;; Don't read more characters than are needed.  The XML parser
  ;; depends on this when doing its character-code detection.
  (and (not (parser-buffer-at-end? buffer))
       (let ((min-end (fix:+ (parser-buffer-index buffer) n))
	     (end (parser-buffer-end buffer)))
	 (let* ((string (parser-buffer-string buffer))
		(v1 (wide-string-contents string))
		(max-end (vector-length v1))
		(max-end*
		 (let loop ((max-end* max-end))
		   (if (fix:<= min-end max-end*)
		       max-end*
		       (loop (fix:* max-end* 2))))))
	   (if (fix:> max-end* max-end)
	       (let ((string* (make-wide-string max-end*)))
		 (let ((v2 (wide-string-contents string*)))
		   (do ((i 0 (fix:+ i 1)))
		       ((not (fix:< i end)))
		     (vector-set! v2 i (vector-ref v1 i))))
		 (set-parser-buffer-string! buffer string*))))
	 (let ((port (parser-buffer-port buffer))
	       (string (parser-buffer-string buffer)))
	   (port/with-input-blocking-mode port 'BLOCKING
	     (lambda ()
	       (let loop ((end end))
		 (if (fix:< end min-end)
		     (let ((n-read
			    (input-port/read-wide-substring!
			     port string end min-end)))
		       (if (fix:> n-read 0)
			   (let ((end (fix:+ end n-read)))
			     (set-parser-buffer-end! buffer end)
			     (loop end))
			   (begin
			     (set-parser-buffer-at-end?! buffer #t)
			     #f)))
		     #t))))))))