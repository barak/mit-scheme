;;; -*-Scheme-*-
;;;
;;; $Id: imail-util.scm,v 1.32 2001/03/18 06:27:47 cph Exp $
;;;
;;; Copyright (c) 1999-2001 Massachusetts Institute of Technology
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

;;;; IMAIL mail reader: utilities

(declare (usual-integrations))

(define (guarantee-index index procedure)
  (if (not (index-fixnum? index))
      (error:wrong-type-argument index "index" procedure)))

(define (source->list source)
  (let ((item (source)))
    (if (eof-object? item)
	'()
	(let ((head (list item)))
	  (let loop ((prev head))
	    (let ((item (source)))
	      (if (eof-object? item)
		  head
		  (let ((this (list item)))
		    (set-cdr! prev this)
		    (loop this)))))))))

(define (list->source items)
  (lambda ()
    (if (pair? items)
	(let ((item (car items)))
	  (set! items (cdr items))
	  item)
	(make-eof-object #f))))

(define (cut-list! items predicate)
  (if (or (not (pair? items)) (predicate (car items)))
      (values '() items)
      (let loop ((prev items) (this (cdr items)))
	(if (or (not (pair? this)) (predicate (car this)))
	    (begin
	      (set-cdr! prev '())
	      (values items this))
	    (loop this (cdr this))))))

(define (burst-list items predicate)
  (let loop ((items items) (groups '()))
    (if (pair? items)
	(let find-next ((items (cdr items)) (group (list (car items))))
	  (if (and (pair? items) (not (predicate (car items))))
	      (find-next (cdr items) (cons (car items) group))
	      (loop items (cons (reverse! group) groups))))
	(reverse! groups))))

(define (count-matching-items items predicate)
  (let loop ((items items) (count 0))
    (if (pair? items)
	(loop (cdr items)
	      (if (predicate (car items))
		  (fix:+ count 1)
		  count))
	count)))

(define (remove-duplicates items predicate)
  (let loop ((items items) (items* '()))
    (if (pair? items)
	(loop (cdr items)
	      (if (let loop ((items* (cdr items)))
		    (and (pair? items*)
			 (or (predicate (car items) (car items*))
			     (loop (cdr items*)))))
		  items*
		  (cons (car items) items*)))
	(reverse! items*))))

(define (remove-duplicates! items predicate)
  (define (trim-initial-segment items)
    (cond ((pair? items)
	   (if (test-item items)
	       (trim-initial-segment (cdr items))
	       (begin
		 (locate-initial-segment items (cdr items))
		 items)))
	  ((null? items) items)
	  (else (lose))))

  (define (locate-initial-segment prev this)
    (cond ((pair? this)
	   (if (test-item this)
	       (set-cdr! prev (trim-initial-segment (cdr this)))
	       (locate-initial-segment this (cdr this))))
	  ((not (null? this)) (lose))))

  (define (test-item items)
    (let loop ((items* (cdr items)))
      (and (pair? items*)
	   (or (predicate (car items) (car items*))
	       (loop (cdr items*))))))

  (define (lose)
    (error:wrong-type-argument items "list" 'REMOVE-DUPLICATES!))

  (trim-initial-segment items))

;; The cryptic LWSP means Linear White SPace.  We use it because it
;; is the terminology from RFC 822.

(define (char-lwsp? char)
  (or (char=? #\space char)
      (char=? #\tab char)))

(define char-set:lwsp
  (char-set #\space #\tab))

(define (skip-lwsp-backwards string start end)
  (let loop ((end end))
    (if (and (fix:< start end)
	     (char-lwsp? (string-ref string (fix:- end 1))))
	(loop (fix:- end 1))
	end)))

(define (quote-lines lines)
  (map (lambda (line)
	 (string-append "\t" line))
       lines))

(define (unquote-lines lines)
  (map (lambda (line)
	 (if (and (fix:> (string-length line) 0)
		  (char=? #\tab (string-ref line 0)))
	     (string-tail line 1)
	     (error "Unquoted line:" line)))
       lines))

(define (string->lines string #!optional line-ending)
  (substring->lines string 0 (string-length string)
		    (if (default-object? line-ending) "\n" line-ending)))

(define (substring->lines string start end #!optional line-ending)
  (let ((line-ending (if (default-object? line-ending) "\n" line-ending))
	(n (string-length line-ending)))
    (let ((indexes (substring-search-all line-ending string start end)))
      (if (pair? indexes)
	  (begin
	    (let loop ((start start) (indexes indexes))
	      (let ((start* (fix:+ (car indexes) n)))
		(set-car! indexes (substring string start (car indexes)))
		(cond ((pair? (cdr indexes))
		       (loop start* (cdr indexes)))
		      ((fix:< start* end)
		       (set-cdr! indexes
				 (list (substring string start* end)))))))
	    indexes)
	  (list (if (and (fix:= start 0)
			 (fix:= end (string-length string)))
		    string
		    (substring string start end)))))))

(define (lines->string lines #!optional line-ending)
  (decorated-string-append "" ""
			   (if (default-object? line-ending) "\n" line-ending)
			   lines))

(define (read-lines port)
  (source->list (lambda () (read-line port))))

(define (read-header-lines port)
  (source->list
   (lambda ()
     (let ((line (read-required-line port)))
       (if (string-null? line)
	   (make-eof-object port)
	   line)))))

(define (read-required-line port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	(error "Premature end of file:" port))
    line))

(define (abbreviate-exact-nonnegative-integer n k)
  (if (< n (expt 10 (- k 1)))
      (string-append (string-pad-left (number->string n) (- k 1)) " ")
      (let ((s
	     (fluid-let ((flonum-unparser-cutoff `(RELATIVE ,k ENGINEERING)))
	       (number->string (exact->inexact n)))))
	(let ((regs (re-string-match "\\([0-9.]+\\)e\\([0-9]+\\)" s)))
	  (let ((mantissa (re-match-extract s regs 1))
		(exponent (string->number (re-match-extract s regs 2))))
	    (if (> exponent 12)
		(make-string k #\+)
		(string-append
		 (let ((l (string-length mantissa))
		       (k (- k 1)))
		   (cond ((< l k)
			  (string-pad-left mantissa k))
			 ((= l k)
			  mantissa)
			 ((char=? #\. (string-ref mantissa (- k 1)))
			  (string-append " " (string-head mantissa (- k 1))))
			 (else
			  (string-head mantissa k))))
		 (case exponent
		   ((0) " ")
		   ((3) "k")
		   ((6) "M")
		   ((9) "G")
		   ((12) "T")))))))))

(define (exact-nonnegative-integer-digits n)
  (let loop ((j 1) (k 10))
    (if (< n k)
	j
	(loop (+ j 1) (* k 10)))))

(define (burst-comma-list-string string)
  (list-transform-negative (map string-trim (burst-string string #\, #f))
    string-null?))

(define (string-greatest-common-prefix strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index* (string-match-forward string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (string-greatest-common-prefix-ci strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index* (string-match-forward-ci string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (string-n-newlines string)
  (substring-n-newlines string 0 (string-length string)))

(define (substring-n-newlines string start end)
  (let loop ((start start) (n 0))
    (let ((index (substring-find-next-char string start end #\newline)))
      (if index
	  (loop (fix:+ index 1) (fix:+ n 1))
	  n))))

;;;; Broken-pipe handler

(define (handle-broken-pipe handler thunk)
  (bind-condition-handler (list condition-type:system-call-error
				condition-type:derived-port-error)
      (lambda (condition)
	(if (broken-pipe? condition)
	    (handler condition)))
    thunk))

(define (broken-pipe? condition)
  (cond ((eq? (condition/type condition) condition-type:system-call-error)
	 (and (eq? (system-call-name condition) 'WRITE)
	      (eq? (system-call-error condition) 'BROKEN-PIPE)))
	((eq? (condition/type condition) condition-type:derived-port-error)
	 (broken-pipe? (derived-port-condition condition)))
	(else #f)))

(define system-call-name
  (condition-accessor condition-type:system-call-error 'SYSTEM-CALL))

(define system-call-error
  (condition-accessor condition-type:system-call-error 'ERROR-TYPE))

(define derived-port-condition
  (condition-accessor condition-type:derived-port-error 'CONDITION))

;;;; Filename Completion

(define (pathname-complete-string pathname filter
				  if-unique if-not-unique if-not-found)
  (let ((pathname (merge-pathnames pathname))
	(if-directory
	 (lambda (pathname)
	   (if-not-unique pathname
			  (lambda () (filtered-list pathname filter))))))
    (cond ((not (safe-file-directory? (directory-pathname pathname)))
	   (if-not-found))
	  ((string-null? (file-namestring pathname))
	   (if-directory pathname))
	  (else
	   (let ((pathnames (filtered-completions pathname filter)))
	     (cond ((not (pair? pathnames))
		    (if-not-found))
		   ((pair? (cdr pathnames))
		    (if-not-unique (->pathname
				    (string-greatest-common-prefix
				     (map ->namestring pathnames)))
				   (lambda () pathnames)))
		   ((string-null? (file-namestring (car pathnames)))
		    (if-directory (car pathnames)))
		   (else
		    (if-unique (car pathnames)))))))))

(define (pathname-completions-list pathname filter)
  (filtered-completions (merge-pathnames pathname) filter))

(define (filtered-completions pathname filter)
  (let* ((directory (directory-namestring pathname))
	 (prefix (file-namestring pathname))
	 (channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read-matching channel prefix)))
	(if name
	    (loop (filter-result (string-append directory name) filter result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (filtered-list pathname filter)
  (let* ((directory (directory-namestring pathname))
	 (channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (filter-result (string-append directory name) filter result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (filter-result filename filter result)
  (let ((pathname (parse-namestring filename #f #f)))
    (cond ((safe-file-directory? pathname)
	   (cons (pathname-as-directory pathname) result))
	  ((filter pathname) (cons pathname result))
	  (else result))))

(define (safe-file-directory? pathname)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:file-error
				   condition-type:port-error)
	 (lambda (condition)
	   condition
	   (k #f))
       (lambda ()
	 (file-directory? pathname))))))

;;;; Extended-string input port

(define (call-with-input-xstring xstring receiver)
  (let ((port (open-xstring-input-port xstring)))
    (let ((value (receiver port)))
      (close-port port)
      value)))

(define (open-xstring-input-port xstring)
  (let ((state (make-xstring-input-state xstring)))
    (read-xstring-buffer state)
    (make-port xstring-input-type state)))

(define-structure (xstring-input-state
		   (constructor make-xstring-input-state (xstring))
		   (conc-name xstring-input-state/))
  (xstring #f)
  (position 0)
  (buffer (make-string 512) read-only #t)
  (buffer-start 0)
  (buffer-end 0))

(define (xstring-port/xstring port)
  (xstring-input-state/xstring (port/state port)))

(define (xstring-port/position port)
  (xstring-input-state/position (port/state port)))

(define (read-xstring-buffer state)
  (let ((xstring (xstring-input-state/xstring state))
	(start (xstring-input-state/buffer-end state)))
    (let ((xend (external-string-length xstring)))
      (and (< start xend)
	   (let* ((buffer (xstring-input-state/buffer state))
		  (end (min (+ start (string-length buffer)) xend)))
	     (without-interrupts
	      (lambda ()
		(set-xstring-input-state/buffer-start! state start)
		(set-xstring-input-state/buffer-end! state end)
		(xsubstring-move! xstring start end buffer 0)))
	     #t)))))

(define xstring-input-type
  (make-port-type
   (let ((peek
	  (lambda (port)
	    (let ((state (port/state port)))
	      (let ((position (xstring-input-state/position state)))
		(if (or (< position (xstring-input-state/buffer-end state))
			(read-xstring-buffer state))
		    (string-ref (xstring-input-state/buffer state)
				(- position
				   (xstring-input-state/buffer-start state)))
		    (make-eof-object port))))))
	 (xlength
	  (lambda (state)
	    (external-string-length (xstring-input-state/xstring state)))))
     `((READ-CHAR
	,(lambda (port)
	   (let ((char (peek port))
		 (state (port/state port)))
	     (if (char? char)
		 (set-xstring-input-state/position!
		  state
		  (+ (xstring-input-state/position state) 1)))
	     char)))
       (PEEK-CHAR ,peek)
       (LENGTH ,(lambda (port) (xlength (port/state port))))
       (EOF?
	,(lambda (port)
	   (let ((state (port/state port)))
	     (< (xstring-input-state/position state) (xlength state)))))
       (CLOSE
	,(lambda (port)
	   (let ((state (port/state port)))
	     (without-interrupts
	      (lambda ()
		(set-xstring-input-state/xstring! state #f)
		(set-xstring-input-state/position! state 0)
		(set-xstring-input-state/buffer-start! state 0)
		(set-xstring-input-state/buffer-end! state 0))))))))
   #f))