;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/strtab.scm,v 1.41 1989/04/28 22:53:31 cph Rel $
;;;
;;;	Copyright (c) 1985, 1989 Massachusetts Institute of Technology
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

;;;; String Tables

(declare (usual-integrations))

(define-structure (string-table (constructor %make-string-table))
  vector
  size)

(define (make-string-table #!optional initial-size)
  (%make-string-table (make-vector (if (default-object? initial-size)
				       16
				       initial-size))
		      0))

(define (alist->string-table alist)
  (let ((v
	 (list->vector
	  (sort alist (lambda (x y) (string-ci<? (car x) (car y)))))))
    (%make-string-table v (vector-length v))))

(define-integrable make-string-table-entry cons)
(define-integrable string-table-entry-string car)
(define-integrable string-table-entry-value cdr)
(define-integrable set-string-table-entry-string! set-car!)
(define-integrable set-string-table-entry-value! set-cdr!)

(define (string-table-search table string1 if-found if-not-found)
  (let ((vector (string-table-vector table)))
    (let loop ((low 0) (high (-1+ (string-table-size table))))
      (if (< high low)
	  (if-not-found low)
	  (let ((index (quotient (+ high low) 2)))
	    (let ((entry (vector-ref vector index)))
	      (string-compare-ci string1 (string-table-entry-string entry)
		(lambda () (if-found index entry))
		(lambda () (loop low (-1+ index)))
		(lambda () (loop (1+ index) high)))))))))

(define (string-table-get table string #!optional if-not-found)
  (string-table-search table string
    (lambda (index entry)
      index				;ignore
      (string-table-entry-value entry))
    (if (default-object? if-not-found)
	(lambda (index) index false)
	if-not-found)))

(define (string-table-put! table string value)
  (string-table-search table string
    (lambda (index entry)
      index				;ignore
      (set-string-table-entry-string! entry string)
      (set-string-table-entry-value! entry value))
    (lambda (index)
      (let ((vector (string-table-vector table))
	    (size (string-table-size table))
	    (entry (make-string-table-entry string value)))
	(let ((max-size (vector-length vector)))
	  (if (= size max-size)
	      (let ((new-vector (vector-grow vector (* max-size 2))))
		(set-string-table-vector! table new-vector)
		(set! vector new-vector)))
	  (subvector-move-right! vector index size vector (1+ index))
	  (vector-set! vector index entry))
	(set-string-table-size! table (1+ size))))))

(define (string-table-remove! table string)
  (string-table-search table string
    (lambda (index entry)
      entry				;ignore
      (let ((vector (string-table-vector table))
	    (size (string-table-size table)))
	(subvector-move-left! vector (1+ index) size vector index)
	(let ((new-size (-1+ size)))
	  (vector-set! vector new-size '())
	  (set-string-table-size! table new-size)))
      true)
    (lambda (index) index false)))

(define (string-table-complete table string
			       if-unique if-not-unique if-not-found)
  (%string-table-complete table string
    if-unique
    (lambda (close-match gcs lower upper)
      (if-not-unique
       (substring close-match 0 gcs)
       (lambda ()
	 (let loop ((index lower))
	   (if (= index upper)
	       '()
	       (cons (string-table-entry-string
		      (vector-ref (string-table-vector table) index))
		     (loop (1+ index))))))))
    if-not-found))

(define (string-table-completions table string)
  (%string-table-complete table string
    list
    (lambda (close-match gcs lower upper)
      close-match gcs			;ignore
      (let loop ((index lower))
	(if (= index upper)
	    '()
	    (cons (string-table-entry-string
		   (vector-ref (string-table-vector table) index))
		  (loop (1+ index))))))
    (lambda ()
      '())))

(define (%string-table-complete table string
				if-unique if-not-unique if-not-found)
  (let ((size (string-length string))
	(table-size (string-table-size table))
	(entry-string
	 (lambda (index)
	   (string-table-entry-string
	    (vector-ref (string-table-vector table) index)))))
    (let ((perform-search
	   (lambda (index)
	     (let ((close-match (entry-string index)))
	       (let ((match-entry
		      (lambda (index)
			(string-match-forward-ci close-match
						 (entry-string index)))))
		 (define (scan-up gcs receiver)
		   (let loop ((gcs gcs) (index (1+ index)))
		     (if (= index table-size)
			 (receiver gcs table-size)
			 (let ((match (match-entry index)))
			   (if (< match size)
			       (receiver gcs index)
			       (loop (min gcs match) (1+ index)))))))
		 (define (scan-down gcs receiver)
		   (let loop ((gcs gcs) (index index))
		     (if (zero? index)
			 (receiver gcs 0)
			 (let ((new-index (-1+ index)))
			   (let ((match (match-entry new-index)))
			     (if (< match size)
				 (receiver gcs index)
				 (loop (min gcs match) new-index)))))))
		 (if (string-prefix-ci? string close-match)
		     (scan-up (string-length close-match)
		       (lambda (gcs upper)
			 (scan-down gcs
			   (lambda (gcs lower)
			     (if (= lower (-1+ upper))
				 (if-unique (entry-string lower))
				 (if-not-unique close-match
						gcs lower upper))))))
		     (if-not-found)))))))
      (string-table-search table string
	(lambda (index entry)
	  entry				;ignore
	  (perform-search index))
	(lambda (index)
	  (if (= index table-size)
	      (if-not-found)
	      (perform-search index)))))))

(define (string-table-apropos table string)
  (let ((end (string-table-size table)))
    (let loop ((index 0))
      (if (= index end)
	  '()
	  (let ((entry (vector-ref (string-table-vector table) index)))
	    (if (substring-ci? string (string-table-entry-string entry))
		(cons (string-table-entry-value entry) (loop (1+ index)))
		(loop (1+ index))))))))

(define (substring-ci? string1 string2)
  (or (string-null? string1)
      (let ((char (string-ref string1 0))
	    (end1 (string-length string1))
	    (end2 (string-length string2)))
	(define (loop start2)
	  (let ((index (substring-find-next-char-ci string2 start2 end2 char)))
	    (and index
		 (if (= (-1+ end1)
			(substring-match-forward-ci string1 1 end1
						    string2 (1+ index) end2))
		     index
		     (loop (1+ index))))))
	(loop 0))))