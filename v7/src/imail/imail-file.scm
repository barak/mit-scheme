;;; -*-Scheme-*-
;;;
;;; $Id: imail-file.scm,v 1.30 2000/05/20 19:39:14 cph Exp $
;;;
;;; Copyright (c) 1999-2000 Massachusetts Institute of Technology
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

;;;; IMAIL mail reader: file-based folder support

(declare (usual-integrations))

;;;; URL

(define-class <file-url> (<url>)
  (pathname define accessor))

(define-method url-body ((url <file-url>))
  (pathname->short-name (file-url-pathname url)))

(define-method url-presentation-name ((url <file-url>))
  (file-namestring (file-url-pathname url)))

(define ((file-url-completer filter)
	 string if-unique if-not-unique if-not-found)
  (pathname-complete-string (short-name->pathname string) filter
    (lambda (string)
      (if-unique (pathname->short-name string)))
    (lambda (prefix get-completions)
      (if-not-unique (pathname->short-name prefix)
		     (lambda () (map pathname->short-name (get-completions)))))
    if-not-found))

(define ((file-url-completions filter) string)
  (map pathname->short-name
       (pathname-completions-list (short-name->pathname string) filter)))

(define (file-suffix-filter suffix)
  (let ((suffix (string-append "." suffix)))
    (let ((l (string-length suffix)))
      (lambda (pathname)
	(let ((string (file-namestring pathname)))
	  (let ((i (string-search-forward suffix string)))
	    (and i
		 (fix:> i 0)
		 (let ((i (fix:+ i l)))
		   (or (fix:= i (string-length string))
		       (char=? #\. (string-ref string i)))))))))))

;;;; Server operations

(define-method %delete-folder ((url <file-url>))
  (delete-file (file-url-pathname url)))

;;; The next method only works when operating on two URLs of the same
;;; class, and is restricted to cases where RENAME-FILE works.

(define-computed-method %rename-folder ((uc1 <file-url>) (uc2 <file-url>))
  (and (eq? uc1 uc2)
       (lambda (url new-url)
	 (rename-file (file-url-pathname url) (file-url-pathname new-url)))))

(define-method available-folder-names ((url <file-url>))
  url
  (error "Unimplemented operation:" 'AVAILABLE-FOLDER-NAMES))

;;;; Folder

(define-class (<file-folder> (predicate)) (<folder>)
  (messages define standard
	    accessor %file-folder-messages
	    initial-value 'UNKNOWN)
  (file-modification-time define standard
			  initial-value #f)
  (file-modification-count define standard
			   initial-value #f))

(define (file-folder-messages folder)
  (if (eq? 'UNKNOWN (%file-folder-messages folder))
      (revert-file-folder folder))
  (%file-folder-messages folder))

(define-generic revert-file-folder (folder))

(define (file-folder-pathname folder)
  (file-url-pathname (folder-url folder)))

(define-method close-folder ((folder <file-folder>))
  (without-interrupts
   (lambda ()
     (let ((messages (%file-folder-messages folder)))
       (if (not (eq? 'UNKNOWN messages))
	   (begin
	     (set-file-folder-messages! folder 'UNKNOWN)
	     (for-each detach-message! messages)))))))

(define-method %folder-valid? ((folder <file-folder>))
  (file-exists? (file-folder-pathname folder)))

(define-method folder-length ((folder <file-folder>))
  (length (file-folder-messages folder)))

(define-method %get-message ((folder <file-folder>) index)
  (list-ref (file-folder-messages folder) index))

(define-method %append-message ((message <message>) (url <file-url>))
  (let ((folder (get-memoized-folder url)))
    (if folder
	(let ((message (make-message-copy message folder)))
	  (without-interrupts
	   (lambda ()
	     (set-file-folder-messages!
	      folder
	      (let ((messages (file-folder-messages folder)))
		(if (pair? messages)
		    (begin
		      (let loop
			  ((prev messages)
			   (this (cdr messages))
			   (index 1))
			(if (pair? this)
			    (loop this (cdr this) (fix:+ index 1))
			    (begin
			      (attach-message! message folder index)
			      (set-cdr! prev (list message)))))
		      messages)
		    (begin
		      (attach-message! message folder 0)
		      (list message))))))))
	(append-message-to-file message url))))

(define-generic make-message-copy (message folder))
(define-generic append-message-to-file (message url))

(define-method expunge-deleted-messages ((folder <file-folder>))
  (without-interrupts
   (lambda ()
     (let find-first ((messages (file-folder-messages folder)) (prev #f))
       (if (pair? messages)
	   (if (message-deleted? (car messages))
	       (let loop
		   ((messages messages)
		    (prev prev)
		    (index (message-index (car messages))))
		 (if (pair? messages)
		     (let ((next (cdr messages)))
		       (if (message-deleted? (car messages))
			   (begin
			     (detach-message! (car messages))
			     (if prev
				 (set-cdr! prev next)
				 (set-file-folder-messages! folder next))
			     (folder-modified! folder 'EXPUNGE index)
			     (loop next prev index))
			   (begin
			     (set-message-index! (car messages) index)
			     (loop (cdr messages) messages (+ index 1)))))))
	       (find-first (cdr messages) messages)))))))

(define-method search-folder ((folder <file-folder>) criteria)
  (cond ((string? criteria)
	 (let ((n (folder-length folder)))
	   (let loop ((index 0) (winners '()))
	     (if (< index n)
		 (loop (+ index 1)
		       (if (let ((message (get-message folder index)))
			     (or (string-search-forward
				  criteria
				  (header-fields->string
				   (message-header-fields message)))
				 (string-search-forward
				  criteria
				  (message-body message))))
			   (cons index winners)
			   winners))
		 (reverse! winners)))))
	(else
	 (error:wrong-type-argument criteria
				    "search criteria"
				    'SEARCH-FOLDER))))

(define-method folder-sync-status ((folder <file-folder>))
  (let ((sync-time (file-folder-file-modification-time folder))
	(sync-count (file-folder-file-modification-count folder))
	(current-count (folder-modification-count folder))
	(current-time (file-modification-time (file-folder-pathname folder))))
    (if (and sync-time sync-count)
	(if current-time
	    (if (= sync-time current-time)
		(if (= sync-count current-count)
		    'SYNCHRONIZED
		    'FOLDER-MODIFIED)
		(if (= sync-count current-count)
		    'PERSISTENT-MODIFIED
		    'BOTH-MODIFIED))
	    'PERSISTENT-DELETED)
	'UNSYNCHRONIZED)))

(define (synchronize-file-folder-write folder writer)
  (let ((pathname (file-folder-pathname folder)))
    (let loop ()
      (let ((count (folder-modification-count folder)))
	(writer folder pathname)
	(let ((t (file-modification-time pathname)))
	  (if (and t (= count (folder-modification-count folder)))
	      (begin
		(set-file-folder-file-modification-count! folder count)
		(set-file-folder-file-modification-time! folder t))
	      (loop)))))))

(define (synchronize-file-folder-read folder reader)
  (let ((pathname (file-folder-pathname folder)))
    (let loop ()
      (let ((t (file-modification-time pathname)))
	(reader folder pathname)
	(if (= t (file-modification-time pathname))
	    (begin
	      (set-file-folder-file-modification-time! folder t)
	      (set-file-folder-file-modification-count!
	       folder
	       (folder-modification-count folder)))
	    (loop))))))

(define-method discard-folder-cache ((folder <file-folder>))
  (close-folder folder))

(define-method probe-folder ((folder <file-folder>))
  folder
  unspecific)