;;; -*-Scheme-*-
;;;
;;;	$Id: regexp.scm,v 1.69 1997/03/04 06:43:23 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-97 Massachusetts Institute of Technology
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

;;;; Regular Expressions

(declare (usual-integrations))

(define registers (make-vector 20))
(define hash-of-false (object-hash false))
(define match-group hash-of-false)

(define-integrable (re-match-start-index i)
  (vector-ref registers i))

(define-integrable (re-match-end-index i)
  (vector-ref registers (+ i 10)))

(define (re-match-start i)
  (guarantee-re-register i 'RE-MATCH-START)
  (let ((index (re-match-start-index i)))
    (and index
	 (make-mark (re-match-group) index))))

(define (re-match-end i)
  (guarantee-re-register i 'RE-MATCH-END)
  (let ((index (re-match-end-index i)))
    (and index
	 (make-mark (re-match-group) index))))

(define (guarantee-re-register i operator)
  (if (not (and (exact-nonnegative-integer? i) (< i 10)))
      (error:wrong-type-argument i "RE register" operator)))

(define (re-match-group)
  (let ((group (object-unhash match-group)))
    (if (not group)
	(error "No match group"))
    group))

(define (re-match-data)
  (let ((group (object-unhash match-group)))
    (cons group
	  (if group
	      (let ((v (make-vector 20 false)))
		(do ((i 0 (+ i 1)))
		    ((= i 20))
		  (let ((index (vector-ref registers i)))
		    (if index
			(vector-set!
			 v i
			 ;; Start marks are right-inserting,
			 ;; end marks are left-inserting.
			 (make-permanent-mark group index (>= i 10))))))
		v)
	      (vector-copy registers)))))

(define (set-re-match-data! data)
  (let ((group (car data))
	(marks (cdr data)))
    (set! match-group (if group (group-hash-number group) hash-of-false))
    (set! registers
	  (if group
	      (vector-map marks
			  (lambda (mark)
			    (and mark
				 (let ((index (mark-index mark)))
				   (mark-temporary! mark)
				   index))))
	      marks)))
  unspecific)

(define (preserving-match-data thunk)
  (let ((data unspecific))
    (unwind-protect (lambda () (set! data (re-match-data)) unspecific)
		    thunk
		    (lambda () (set-re-match-data! data)))))

(define-integrable (syntax-table-argument syntax-table)
  (syntax-table/entries (or syntax-table standard-syntax-table)))

(define (replace-match replacement #!optional preserve-case? literal?)
  (let ((start (re-match-start 0))
	(end (mark-left-inserting-copy (re-match-end 0))))
    (let ((replacement
	   (let ((replacement
		  (if (and (not (default-object? literal?)) literal?)
		      replacement
		      (re-substitute-registers replacement))))
	     (if (and (not (default-object? preserve-case?)) preserve-case?)
		 ;; Emacs uses a more complicated algorithm here,
		 ;; which breaks the replaced string into words,
		 ;; makes the decision based on examining all the
		 ;; words, and then changes each word in the
		 ;; replacement to match the pattern.
		 (let ((replaced (extract-string start end)))
		   (cond ((string-capitalized? replaced)
			  (string-capitalize replacement))
			 ((string-upper-case? replaced)
			  (string-upcase replacement))
			 (else replacement)))
		 replacement))))
      (delete-string start end)
      (insert-string replacement start))
    (mark-temporary! end)
    end))

(define (re-substitute-registers pattern)
  (let ((end (string-length pattern)))
    (if (substring-find-next-char pattern 0 end #\\)
	(apply
	 string-append
	 (let loop ((start 0))
	   (let ((slash (substring-find-next-char pattern start end #\\)))
	     (if slash
		 (cons (substring pattern start slash)
		       (let ((next (+ slash 1)))
			 (cons (let ((char
				      (if (< next end)
					  (string-ref pattern next)
					  #\\)))
				 (let ((n
					(if (char=? #\& char)
					    0
					    (char->digit char))))
				   (cond ((not n)
					  (string char))
					 ((not (re-match-start-index n))
					  (error "No match for register" n))
					 (else
					  (extract-string
					   (re-match-start n)
					   (re-match-end n))))))
			       (if (< next end)
				   (loop (+ next 1))
				   '()))))
		 (list (substring pattern start end))))))
	pattern)))

(define (delete-match)
  (let ((group (re-match-group))
	(start (re-match-start-index 0)))
    (group-delete! group start (re-match-end-index 0))
    (make-mark group start)))

(define (re-search-buffer-forward regexp syntax-table group start end)
  (let ((index
	 ((ucode-primitive re-search-buffer-forward)
	  (compiled-regexp/byte-stream regexp)
	  (compiled-regexp/translation-table regexp)
	  (syntax-table-argument syntax-table)
	  registers group start end)))
    (set! match-group (compute-match-group group index))
    index))

(define (re-search-buffer-backward regexp syntax-table group start end)
  (let ((index
	 ((ucode-primitive re-search-buffer-backward)
	  (compiled-regexp/byte-stream regexp)
	  (compiled-regexp/translation-table regexp)
	  (syntax-table-argument syntax-table)
	  registers group start end)))
    (set! match-group (compute-match-group group index))
    index))

(define (re-match-buffer-forward regexp syntax-table group start end)
  (let ((index
	 ((ucode-primitive re-match-buffer)
	  (compiled-regexp/byte-stream regexp)
	  (compiled-regexp/translation-table regexp)
	  (syntax-table-argument syntax-table)
	  registers group start end)))
    (set! match-group (compute-match-group group index))
    index))

(define (compute-match-group group index)
  (if index
      (group-hash-number group)
      hash-of-false))

(define (re-match-string-forward regexp syntax-table string)
  (re-match-substring-forward regexp syntax-table
			      string 0 (string-length string)))

(define (re-match-substring-forward regexp syntax-table string start end)
  (set! match-group hash-of-false)
  ((ucode-primitive re-match-substring)
   (compiled-regexp/byte-stream regexp)
   (compiled-regexp/translation-table regexp)
   (syntax-table-argument syntax-table)
   registers string start end))

(define (re-search-string-forward regexp syntax-table string)
  (re-search-substring-forward regexp syntax-table
			       string 0 (string-length string)))

(define (re-search-substring-forward regexp syntax-table string start end)
  (set! match-group hash-of-false)
  ((ucode-primitive re-search-substring-forward)
   (compiled-regexp/byte-stream regexp)
   (compiled-regexp/translation-table regexp)
   (syntax-table-argument syntax-table)
   registers string start end))

(define (re-search-string-backward regexp syntax-table string)
  (re-search-substring-backward regexp syntax-table
				string 0 (string-length string)))

(define (re-search-substring-backward regexp syntax-table string start end)
  (set! match-group hash-of-false)
  ((ucode-primitive re-search-substring-backward)
   (compiled-regexp/byte-stream regexp)
   (compiled-regexp/translation-table regexp)
   (syntax-table-argument syntax-table)
   registers string start end))

(define-macro (default-end-mark start end)
  `(IF (DEFAULT-OBJECT? ,end)
       (GROUP-END ,start)
       (BEGIN
	 (IF (NOT (MARK<= ,start ,end))
	     (ERROR "Marks incorrectly related:" ,start ,end))
	 ,end)))

(define-macro (default-start-mark start end)
  `(IF (DEFAULT-OBJECT? ,start)
       (GROUP-START ,end)
       (BEGIN
	 (IF (NOT (MARK<= ,start ,end))
	     (ERROR "Marks incorrectly related:" ,start ,end))
	 ,start)))

(define-macro (default-case-fold-search case-fold-search mark)
  `(IF (DEFAULT-OBJECT? ,case-fold-search)
       (GROUP-CASE-FOLD-SEARCH (MARK-GROUP ,mark))
       ,case-fold-search))

(define (search-forward string start #!optional end case-fold-search)
  (%re-search string start (default-end-mark start end)
	      (default-case-fold-search case-fold-search start)
	      re-compile-string
	      re-search-buffer-forward))

(define (search-backward string end #!optional start case-fold-search)
  (%re-search string (default-start-mark start end) end
	      (default-case-fold-search case-fold-search end)
	      re-compile-string
	      re-search-buffer-backward))

(define (re-search-forward regexp start #!optional end case-fold-search)
  (%re-search regexp start (default-end-mark start end)
	      (default-case-fold-search case-fold-search start)
	      re-compile-pattern
	      re-search-buffer-forward))

(define (re-search-backward regexp end #!optional start case-fold-search)
  (%re-search regexp (default-start-mark start end) end
	      (default-case-fold-search case-fold-search end)
	      re-compile-pattern
	      re-search-buffer-backward))

(define (%re-search string start end case-fold-search compile-string search)
  (let ((group (mark-group start)))
    (let ((index
	   (search (if (compiled-regexp? string)
		       string
		       (compile-string string case-fold-search))
		   (group-syntax-table group)
		   group
		   (mark-index start)
		   (mark-index end))))
      (and index
	   (make-mark group index)))))

(define (re-match-forward regexp start #!optional end case-fold-search)
  (let ((end (default-end-mark start end))
	(case-fold-search (default-case-fold-search case-fold-search start))
	(group (mark-group start)))
    (let ((index
	   (re-match-buffer-forward (if (compiled-regexp? regexp)
					regexp
					(re-compile-pattern regexp
							    case-fold-search))
				    (group-syntax-table group)
				    group
				    (mark-index start)
				    (mark-index end))))
      (and index
	   (make-mark group index)))))

(define (re-string-match regexp string #!optional case-fold syntax-table)
  (let ((case-fold (if (default-object? case-fold) #f case-fold))
	(syntax-table (if (default-object? syntax-table) #f syntax-table)))
    (re-match-string-forward (if (compiled-regexp? regexp)
				 regexp
				 (re-compile-pattern regexp case-fold))
			     syntax-table
			     string)))

(define (re-substring-match regexp string start end
			    #!optional case-fold syntax-table)
  (let ((case-fold (if (default-object? case-fold) #f case-fold))
	(syntax-table (if (default-object? syntax-table) #f syntax-table)))
    (re-match-substring-forward (if (compiled-regexp? regexp)
				    regexp
				    (re-compile-pattern regexp case-fold))
				syntax-table
				string start end)))

(define (re-string-search regexp string #!optional case-fold syntax-table)
  (let ((case-fold (if (default-object? case-fold) #f case-fold))
	(syntax-table (if (default-object? syntax-table) #f syntax-table)))
    (re-search-string-forward (if (compiled-regexp? regexp)
				  regexp
				  (re-compile-pattern regexp case-fold))
			      syntax-table
			      string)))

(define (re-substring-search regexp string start end
			    #!optional case-fold syntax-table)
  (let ((case-fold (if (default-object? case-fold) #f case-fold))
	(syntax-table (if (default-object? syntax-table) #f syntax-table)))
    (re-search-substring-forward (if (compiled-regexp? regexp)
				     regexp
				     (re-compile-pattern regexp case-fold))
				 syntax-table
				 string start end)))

(define (regexp-group alternatives)
  (let ((alternatives
	 (list-transform-positive alternatives identity-procedure)))
    (if (null? alternatives)
	"\\(\\)"
	(apply string-append
	       (cons "\\("
		     (let loop ((alternatives alternatives))
		       (cons (car alternatives)
			     (if (null? (cdr alternatives))
				 (list "\\)")
				 (cons "\\|" (loop (cdr alternatives)))))))))))