#| -*-Scheme-*-

$Id: 49fbfca87ed53016d66e784ca09ceaec540fc25c $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; RCS Parser

(declare (usual-integrations))

(define (rcs/read-file filename #!optional text?)
  (call-with-input-file filename
    (lambda (port)
      (parse-rcstext port (if (default-object? text?) true text?)))))

(define (rcs/read-head filename)
  (call-with-input-file filename
    (lambda (port)
      (parse-head (make-line-port port)))))

(define (parse-rcstext port text?)
  (let ((line-port (make-line-port port)))
    (let* ((admin (parse-admin line-port))
	   (deltas (parse-deltas line-port))
	   (description (parse-desc line-port))
	   (deltatexts
	    (if text? (parse-deltatexts line-port (eq? true text?)) '()))
	   (num->delta (make-delta-map deltas deltatexts text?)))
      (make-rcstext (and (vector-ref admin 0)
			 (num->delta (vector-ref admin 0) #t))
		    (and (vector-ref admin 1)
			 (num->delta (vector-ref admin 1) #t))
		    (vector-ref admin 2)
		    (map (lambda (element)
			   (cons (car element)
				 (num->delta (cdr element) #f)))
			 (vector-ref admin 3))
		    (map (lambda (element)
			   (cons (car element)
				 (num->delta (cdr element) #t)))
			 (vector-ref admin 4))
		    (vector-ref admin 5)
		    (vector-ref admin 6)
		    (vector-ref admin 7)
		    description))))

(define (make-delta-map deltas deltatexts text?)
  (let ((table (make-string-hash-table)))
    (for-each (lambda (delta)
		(let ((key (vector-ref delta 0)))
		  (let ((entry (hash-table/get table key false)))
		    (if entry
			(error "duplicate delta entry" delta entry)))
		  (hash-table/put! table key
				   (make-delta key
					       (vector-ref delta 1)
					       (vector-ref delta 2)
					       (vector-ref delta 3)
					       (vector-ref delta 4)
					       (vector-ref delta 5)
					       false
					       false))))
	      deltas)
    (for-each (lambda (deltatext)
		(let ((key (vector-ref deltatext 0)))
		  (let ((delta (hash-table/get table key false)))
		    (if (not delta)
			(error "missing delta entry" deltatext))
		    (set-delta/log! delta (vector-ref deltatext 1))
		    (set-delta/text! delta (vector-ref deltatext 2)))))
	      deltatexts)
    (let ((num->delta
	   (lambda (key error?)
	     (let ((delta (hash-table/get table key false)))
	       (if (and (not delta) error?)
		   (error "unknown delta number" key))
	       delta))))
      (hash-table/for-each table
	(lambda (key delta)
	  key
	  (if (and text? (not (delta/log delta)))
	      (error "missing deltatext entry" delta))
	  (let loop ((branches (delta/branches delta)))
	    (if (pair? branches)
		(begin
		  (set-car! branches (num->delta (car branches) #t))
		  (loop (cdr branches)))))
	  (let ((next (delta/next delta)))
	    (if next
		(set-delta/next! delta (num->delta next #t))))))
      num->delta)))

(define (parse-admin line-port)
  (let* ((head (parse-head line-port))
	 (branch (parse-optional line-port "branch" '(num)))
	 (access-list (parse-required line-port "access" '(* id)))
	 (symbols (parse-required line-port "symbols" '(* id colon num)))
	 (locks (parse-required line-port "locks" '(* id colon num)))
	 (strict (parse-optional line-port "strict" '()))
	 (comment (parse-optional line-port "comment" '(? string)))
	 (expand (parse-optional line-port "expand" '(? string))))
    (discard-newphrases line-port)
    (vector head
	    (and branch
		 (not (null? (cdr branch)))
		 (rcs-num-string (cadr branch)))
	    (map rcs-id-string (cdr access-list))
	    (rcs-id-alist (cdr symbols))
	    (rcs-id-alist (cdr locks))
	    (and strict true)
	    (and comment
		 (not (null? (cdr comment)))
		 (rcs-string-contents (cadr comment)))
	    (and expand
		 (not (null? (cdr expand)))
		 (rcs-string-contents (cadr expand))))))

(define (parse-head line-port)
  (let ((head (parse-required line-port "head" '(num))))
    (and (not (null? (cdr head)))
	 (rcs-num-string (cadr head)))))

(define (rcs-id-alist symbols)
  (if (null? symbols)
      '()
      (cons (cons (rcs-id-string (car symbols))
		  (rcs-num-string (caddr symbols)))
	    (rcs-id-alist (cdddr symbols)))))

(define (parse-deltas line-port)
  (let ((delta (parse-delta line-port)))
    (if delta
	(cons delta (parse-deltas line-port))
	'())))

(define (parse-delta line-port)
  (let ((number (parse-optional line-port 'num '())))
    (and number
	 (let* ((date (parse-required line-port "date" '(num)))
		(author (parse-required line-port "author" '(id)))
		(state (parse-required line-port "state" '(? id)))
		(branches (parse-required line-port "branches" '(* num)))
		(next (parse-required line-port "next" '(? num))))
	   (discard-newphrases line-port)
	   (vector (rcs-num-string (car number))
		   (rcs-date (cadr date))
		   (rcs-id-string (cadr author))
		   (and (not (null? (cdr state)))
			(rcs-id-string (cadr state)))
		   (map rcs-num-string (cdr branches))
		   (and (not (null? (cdr next)))
			(rcs-num-string (cadr next))))))))

(define-integrable (rcs-date num)
  (apply date/make (number->integer-list (rcs-num-string num))))

(define (number->integer-list string)
  (let ((end (string-length string)))
    (let loop ((start 0) (index 0))
      (cond ((= index end)
	     (if (= start end)
		 (error "Trailing decimal in number"))
	     (list (string->number (substring string start end))))
	    ((char=? #\. (string-ref string index))
	     (cons (string->number (substring string start index))
		   (let ((start (1+ index)))
		     (loop start start))))
	    (else
	     (loop start (1+ index)))))))

(define (parse-desc line-port)
  (rcs-string-contents (cadr (parse-required line-port "desc" '(string)))))

(define (parse-deltatexts line-port text?)
  (let loop ()
    (let ((deltatext (parse-deltatext line-port text?)))
      (if deltatext
	  (cons deltatext (loop))
	  '()))))

(define (parse-deltatext line-port text?)
  (let ((number (parse-optional line-port 'num '())))
    (and number
	 (let ((log (parse-required line-port "log" '(string))))
	   (let loop ()
	     (let ((text (parse-optional line-port "text" '(string))))
	       (if text
		   (vector (rcs-num-string (car number))
			   (rcs-string-contents (cadr log))
			   (and text? (rcs-string-contents (cadr text))))
		   (begin
		     (parse-required line-port 'id '(* word))
		     (loop)))))))))

(define (parse-required line-port head tail)
  (let ((line (line-read line-port)))
    (if (not (and (rcs-match head (car line))
		  (rcs-match tail (cdr line))))
	(error "ill-formed RCS file" head tail line))
    line))

(define (parse-optional line-port head tail)
  (let ((line (line-peek line-port)))
    (and line
	 (rcs-match head (car line))
	 (begin
	   (line-discard line-port)
	   (if (not (rcs-match tail (cdr line)))
	       (error "ill-formed RCS file" head tail line))
	   line))))

(define (discard-newphrases line-port)
  (let ((line (line-peek line-port)))
    (if (and line
	     (rcs-match 'id (car line))
	     (not (string=? "desc" (rcs-id-string (car line))))
	     (rcs-match '(* word) (cdr line)))
	(begin
	  (line-discard line-port)
	  (discard-newphrases line-port)))))

(define (rcs-match pattern instance)
  (cond ((string? pattern)
	 (and (rcs-id? instance)
	      (string=? pattern (rcs-id-string instance))))
	((symbol? pattern)
	 (case pattern
	   ((id) (rcs-id? instance))
	   ((string) (rcs-string? instance))
	   ((num) (rcs-num? instance))
	   ((colon) (rcs-colon? instance))
	   ((semicolon) (rcs-semicolon? instance))
	   ((word) (rcs-word? instance))
	   (else (error "ill-formed pattern" pattern))))
	((list? pattern)
	 (if (null? pattern)
	     (null? instance)
	     (case (car pattern)
	       ((?)
		(or (null? instance)
		    (rcs-match-list (cdr pattern) instance null?)))
	       ((*)
		(let loop ((instance instance))
		  (or (null? instance)
		      (rcs-match-list (cdr pattern) instance loop))))
	       ((+)
		(letrec ((loop
			  (lambda (instance)
			    (or (null? instance)
				(rcs-match-list (cdr pattern)
						instance
						loop)))))
		  (rcs-match-list (cdr pattern) instance loop)))
	       (else
		(rcs-match-list pattern instance null?)))))
	(else
	 (error "ill-formed pattern" pattern))))

(define (rcs-match-list pattern instance if-match)
  (let loop ((pattern pattern) (instance instance))
    (if (null? pattern)
	(if-match instance)
	(and (pair? instance)
	     (rcs-match (car pattern) (car instance))
	     (loop (cdr pattern) (cdr instance))))))

(define (make-line-port port)
  (cons 'EMPTY port))

(define (line-peek line-port)
  (if (eq? 'EMPTY (car line-port))
      (set-car! line-port (parse-line (cdr line-port))))
  (car line-port))

(define (line-discard line-port)
  (if (car line-port)
      (set-car! line-port 'EMPTY)))

(define (line-read line-port)
  (let ((line (line-peek line-port)))
    (line-discard line-port)
    line))

(define (parse-line port)
  (let ((word (parse-word port)))
    (cond ((null? word)
	   false)
	  ((rcs-id? word)
	   (let ((string (rcs-id-string word)))
	     (if (or (string=? "desc" string)
		     (string=? "log" string)
		     (string=? "text" string))
		 (let ((string (parse-word port)))
		   (if (not (rcs-string? string))
		       (error "illegal word sequence" word string))
		   (list word string))
		 (cons word
		       (let loop ()
			 (let ((word (parse-word port)))
			   (if (rcs-semicolon? word)
			       '()
			       (cons word (loop)))))))))
	  ((rcs-num? word)
	   (list word))
	  (else
	   (error "illegal line-starting word" word)))))

(define (parse-word port)
  (skip-whitespace port)
  (let ((char (input-port/peek-char port)))
    (if (eof-object? char)
	'()
	((vector-ref parse-word/dispatch-table (char->ascii char)) port))))

(define skip-whitespace
  (let ((delimiters
	 (char-set-invert
	  (char-set-union (ascii-range->char-set #o010 #o016)
			  (ascii-range->char-set #o040 #o041)))))
    (lambda (port)
      (input-port/discard-chars port delimiters))))

(define parse-string
  (let ((delimiters (char-set #\@)))
    (lambda (port)
      (input-port/discard-char port)
      (let ((strings
	     (let loop ()
	       (let ((head (input-port/read-string port delimiters)))
		 (let ((char (input-port/peek-char port)))
		   (if (eof-object? char)
		       (error "end of file while reading string"))
		   (input-port/discard-char port)
		   (let ((char* (input-port/peek-char port)))
		     (if (eq? char char*)
			 (begin
			   (input-port/discard-char port)
			   (cons head (cons "@" (loop))))
			 (list head))))))))
	(make-rcs-string
	 (if (null? (cdr strings))
	     (car strings)
	     (apply string-append strings)))))))

(define parse-id
  (let ((delimiters
	 (char-set-invert
	  (char-set-difference
	   (char-set-union (ascii-range->char-set #o041 #o177)
			   (ascii-range->char-set #o240 #o400))
	   (char-set #\$ #\, #\. #\: #\; #\@)))))
    (lambda (port)
      (make-rcs-id (input-port/read-string port delimiters)))))

(define parse-num
  (let ((delimiters
	 (char-set-invert (char-set-union char-set:numeric (char-set #\.)))))
    (lambda (port)
      (make-rcs-num (input-port/read-string port delimiters)))))

(define (parse-colon port)
  (input-port/discard-char port)
  (make-rcs-colon))

(define (parse-semicolon port)
  (input-port/discard-char port)
  (make-rcs-semicolon))

(define parse-word/dispatch-table)

(define (initialize-dispatch-table!)
  (set! parse-word/dispatch-table
	(make-vector 256
		     (lambda (port)
		       (error "illegal word-starting character" port))))
  (subvector-fill! parse-word/dispatch-table #o101 #o133 parse-id)
  (subvector-fill! parse-word/dispatch-table #o141 #o173 parse-id)
  (subvector-fill! parse-word/dispatch-table #o300 #o327 parse-id)
  (subvector-fill! parse-word/dispatch-table #o330 #o366 parse-id)
  (subvector-fill! parse-word/dispatch-table #o370 #o400 parse-id)
  (subvector-fill! parse-word/dispatch-table #o060 #o072 parse-num)
  (vector-set! parse-word/dispatch-table (char->ascii #\@) parse-string)
  (vector-set! parse-word/dispatch-table (char->ascii #\:) parse-colon)
  (vector-set! parse-word/dispatch-table (char->ascii #\;) parse-semicolon))

(initialize-dispatch-table!)

(define (rcs-word? object)
  (and (pair? object)
       (memq (car object) '(IDENTIFIER STRING NUMBER COLON SEMICOLON))))

(define-integrable (make-rcs-id string)
  (cons 'IDENTIFIER string))

(define (rcs-id? word)
  (and (pair? word)
       (eq? 'IDENTIFIER (car word))))

(define-integrable (rcs-id-string rcs-id)
  (cdr rcs-id))

(define-integrable (make-rcs-string contents)
  (cons 'STRING contents))

(define (rcs-string? word)
  (and (pair? word)
       (eq? 'STRING (car word))))

(define-integrable (rcs-string-contents rcs-string)
  (cdr rcs-string))

(define-integrable (make-rcs-num string)
  (cons 'NUMBER string))

(define (rcs-num? word)
  (and (pair? word)
       (eq? 'NUMBER (car word))))

(define-integrable (rcs-num-string rcs-num)
  (cdr rcs-num))

(define-integrable (make-rcs-colon)
  '(COLON))

(define (rcs-colon? word)
  (and (pair? word)
       (eq? 'COLON (car word))))

(define-integrable (make-rcs-semicolon)
  '(SEMICOLON))

(define (rcs-semicolon? word)
  (and (pair? word)
       (eq? 'SEMICOLON (car word))))