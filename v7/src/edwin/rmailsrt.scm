;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/rmailsrt.scm,v 1.6 1991/11/26 21:18:56 bal Exp $
;;;
;;;	Copyright (c) 1991 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; RMAIL Sorting Utilities

(declare (usual-integrations))

;; GNUS compatible key bindings.
(define-key 'rmail (list #\C-c #\C-s #\C-d) 'rmail-sort-by-date)
(define-key 'rmail (list #\C-c #\C-s #\C-s) 'rmail-sort-by-subject)
(define-key 'rmail (list #\C-c #\C-s #\C-a) 'rmail-sort-by-author)
(define-key 'rmail (list #\C-c #\C-s #\C-r) 'rmail-sort-by-recipient)
(define-key 'rmail (list #\C-c #\C-s #\C-l) 'rmail-sort-by-size-lines)

(define-command rmail-sort-by-date 
  "Sort messages of current Rmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages 
     reverse
     (lambda (memo)
       (fetch-first-field "date" (msg-memo/start memo) (msg-memo/end memo)))
     (lambda (x y)
       (string<? (rmail-sortable-date-string x)
		 (rmail-sortable-date-string y))))))

(define-command rmail-sort-by-subject 
  "Sort messages of current Rmail file by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages 
     reverse
     (let ((re-pattern (re-compile-pattern "^\\(re:[ \t]+\\)*" true)))
       (lambda (memo)
	 (let ((key
		(or (fetch-first-field "subject" 
				       (msg-memo/start memo)
				       (msg-memo/end memo))
		    "")))
	   ;; Remove `Re:'
	   (if (re-match-string-forward re-pattern true false key)
	       (string-tail key (re-match-end-index 0))
	       key))))
     string<?)))

(define-command rmail-sort-by-author 
  "Sort messages of current Rmail file by author.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages 
     reverse
     (lambda (memo)
       (let ((start (msg-memo/start memo))
	     (end (msg-memo/end memo)))
	 (mail-strip-quoted-names
	  (or (fetch-first-field "from" start end)
	      (fetch-first-field "sender" start end)))))
     string<?)))

(define-command rmail-sort-by-recipient 
  "Sort messages of current Rmail file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages 
     reverse
     (lambda (memo)
       (let ((start (msg-memo/start memo))
	     (end (msg-memo/end memo)))
	 (mail-strip-quoted-names
	  (or (fetch-first-field "to" start end)
	      (fetch-first-field "apparently-to" start end)))))
     string<?)))

(define-command rmail-sort-by-size-lines 
  "Sort messages of current Rmail file by message size.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages
     reverse
     (lambda (memo) (count-lines (msg-memo/start memo) (msg-memo/end memo)))
     <)))

(define rmail-sort-messages
  (lambda (reverse keyfunc cmpfunc)
    (let* ((current-msg-num (msg-memo/number (current-msg-memo)))
	   (nummsg (-1+ (msg-memo/number (last-msg-memo))))
	   (sort-vect (make-vector (1+ nummsg))))
      (message "Finding sort keys...")
      (widen)
      (let loop ((n 0)
		 (the-memo (msg-memo/first (current-msg-memo))))
	(let ((next (msg-memo/next the-memo)))
	  (if (= 9 (modulo n 10))
	      (message "Finding sort keys..." (1+ n)))
	  (vector-set! 
	   sort-vect n
	   (list (keyfunc the-memo)
		 (extract-string
		  (msg-memo/start the-memo)
		  (msg-memo/end the-memo))
		 the-memo))
	  (if next (loop (1+ n) next))))
      (if reverse
	  (set! sort-vect
		(list->vector (reverse (vector->list sort-vect)))))
      (sort! sort-vect
	     (lambda (x y)
	       (cmpfunc (car x) (car y))))
      (message "Reordering buffer...")
      (set-buffer-writeable! (current-buffer))
      (delete-string
       (msg-memo/start (msg-memo/first (current-msg-memo)))
       (msg-memo/end (msg-memo/last (current-msg-memo))))
      (let loop ((n 0)
		 (previous false)
		 (the-memo (caddr (vector-ref sort-vect 0)))
		 (next (if (>= nummsg 2)
			   (caddr (vector-ref sort-vect 1))
			   false)))
	(set-msg-memo/previous! the-memo previous)
	(set-msg-memo/next! the-memo next)
	(if (< n nummsg)
	    (begin
	      (insert-string (cadr (vector-ref sort-vect n)))
	      (if (= 9 (modulo n 10))
		  (message "reordering buffer..." (1+ n)))
	      (loop (1+ n) the-memo next
		    (if (< (1+ n) nummsg)
			(caddr (vector-ref sort-vect (1+ n)))
			false)))
	    (insert-string (cadr (vector-ref sort-vect n)))))
      (set-buffer-read-only! (current-buffer))
      (set-buffer-msg-memo! (current-buffer) false)
      (memoize-buffer (current-buffer))
      (show-message (current-buffer) current-msg-num))))

;; Copy of the function gnus-comparable-date in gnus.el

(define rmail-sortable-date-string
  (lambda (date)
    (let ((month '(("JAN" . " 1")("FEB" . " 2")("MAR" . " 3")
				 ("APR" . " 4")("MAY" . " 5")("JUN" . " 6")
				 ("JUL" . " 7")("AUG" . " 8")("SEP" . " 9")
				 ("OCT" . "10")("NOV" . "11")("DEC" . "12")))
	  (date (or date "")))
    ;; Can understand the following styles:
    ;; (1) 14 Apr 89 03:20:12 GMT
    ;; (2) Fri, 17 Mar 89 4:01:33 GMT
    ;;
    ;; added [ ]+ to the regexp to handle date string put out
    ;; by hx.lcs.mit.edu (they use 2 spaces instead of 1)
      (if (re-search-string-forward
	   (re-compile-pattern
	    "\\([0-9]+\\) \\([^ ,]+\\) \\([0-9]+\\)[ ]+\\([0-9:]+\\)" true)
	   true false date)
	  (string-append
	   ;; Year
	   (let ((year
		  (string->number
		   (substring date
			      (re-match-start-index 3)
			      (re-match-end-index 3)))))
	     (let ((y1 (modulo year 100)))
	       (string-pad-left (number->string y1) 2)))
	   ;; Month
	   (cdr
	    (assoc
	     (string-upcase
	      (substring (substring date
				    (re-match-start-index 2)
				    (re-match-end-index 2))
			 0 3))
	     month))
	   ;; Day
	   (let ((day
		  (substring date
			     (re-match-start-index 1)
			     (re-match-end-index 1))))
	     (string-pad-left day 2 #\0))
	   ;; Time
	   (substring date (re-match-start-index 4) (re-match-end-index 4)))
      ;; Cannot understand DATE string.
	  date))))

(define mail-string-delete
  (lambda (string start end)
    (string-append
     (string-head string start)
     (string-tail string end))))

(define mail-strip-quoted-names
  (lambda (address)
    (let ((pos))
      (if (re-search-string-forward (re-compile-pattern "\\`[ \t\n]*" true)
				   true false address)
	  (set! address (string-tail address (re-match-end-index 0))))
      ;; strip surrounding whitespace
      (if (re-search-string-forward (re-compile-pattern "[ \t\n]*\\'" true)
				   true false address)
	  (set! address (string-head address (re-match-start-index 0))))
      (let loop ()
       (let ((the-pattern 
	      (re-compile-pattern
	       "[ \t]*(\\([^)\"\\]\\|\\\\.\\|\\\\\n\\)*)" true)))
	 (set! pos (re-search-string-forward the-pattern true false address))
	 (if pos
	     (begin
	       (set! address (mail-string-delete
			      address 
			      (re-match-start-index 0)
			      (re-match-end-index 0)))
	       (loop)))))
     ;; strip `quoted' names (This is supposed to hack `"Foo Bar" <bar@host>')
     (let loop ((the-pos 0))
       (let ((the-pattern
	      (re-compile-pattern
	       "[ \t]*\"\\([^\"\\]\\|\\\\.\\|\\\\\n\\)*\"[ \t\n]*"
	       true)))
	 (set! pos
	       (re-match-substring-forward the-pattern true false address
					   the-pos (string-length address)))
	 (if pos
	     (if (and (> (string-length address) (re-match-end-index 0))
		      (char=? (string-ref address (re-match-end-index 0)) #\@))
		 (loop pos)
		 (begin
		   (set! address
			 (mail-string-delete address
					     the-pos (re-match-end-index 0)))
		   (loop the-pos))))))
     ;; Retain only part of address in <> delims, if there is such a thing.
     (let loop ()
       (let ((the-pattern
	      (re-compile-pattern
	       "\\(,\\|\\`\\)[^,]*<\\([^>,]*>\\)"
	       true)))
	 (set! pos (re-search-string-forward the-pattern true false address))
	 (if pos
	     (let ((junk-beg (re-match-end-index 1))
		   (junk-end (re-match-start-index 2))
		   (close (re-match-end-index 0)))
	       (set! address (mail-string-delete address (-1+ close) close))
	       (set! address (mail-string-delete address junk-beg junk-end))
	       (loop)))))
     address)))
