;;; -*-Scheme-*-
;;;
;;; $Id: imail-file.scm,v 1.12 2000/05/02 21:42:07 cph Exp $
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

;;;; Server operations

(define-method %delete-folder ((url <file-url>))
  (delete-file (file-url-pathname url)))

;;; The next two methods only work when operating on two URLs of the
;;; same class.  Otherwise, it's necessary to do format conversions;
;;; this is handled at a higher level in the class heirarchy.

(define-computed-method %move-folder ((uc1 <file-url>) (uc2 <file-url>))
  (and (eq? uc1 uc2)
       (lambda (url new-url)
	 ;; **** Not really correct -- must handle cases where RENAME-FILE
	 ;; fails, such as moving across file systems under unix.
	 (rename-file (file-url-pathname url) (file-url-pathname new-url)))))

(define-computed-method %copy-folder ((uc1 <file-url>) (uc2 <file-url>))
  (and (eq? uc1 uc2)
       (lambda (url new-url)
	 (copy-file (file-url-pathname url) (file-url-pathname new-url)))))

(define-method available-folder-names ((url <file-url>))
  url
  (error "Unimplemented operation:" 'AVAILABLE-FOLDER-NAMES))

;;;; Folder

(define-class <file-folder> (<folder>)
  (messages define standard initial-value '())
  (modification-time define standard initial-value #f))

(define (file-folder-pathname folder)
  (file-url-pathname (folder-url folder)))

(define (update-file-folder-modification-time! folder)
  (set-file-folder-modification-time!
   folder
   (file-modification-time (file-folder-pathname folder)))
  (folder-not-modified! folder))

(define-method %close-folder ((folder <file-folder>))
  folder
  unspecific)

(define-method %folder-valid? ((folder <file-folder>))
  (file-exists? (file-folder-pathname folder)))

(define-method folder-length ((folder <file-folder>))
  (length (file-folder-messages folder)))

(define-method %get-message ((folder <file-folder>) index)
  (list-ref (file-folder-messages folder) index))

(define-method %append-message ((folder <file-folder>) message)
  (let ((message (attach-message message folder)))
    (without-interrupts
     (lambda ()
       (set-file-folder-messages!
	folder
	(let ((messages (file-folder-messages folder)))
	  (if (pair? messages)
	      (begin
		(let loop ((prev messages) (this (cdr messages)) (index 1))
		  (if (pair? this)
		      (loop this (cdr this) (fix:+ index 1))
		      (begin
			(set-message-index! message index)
			(set-cdr! prev (list message)))))
		messages)
	      (begin
		(set-message-index! message 0)
		(list message)))))
       (message-modified! message)))))

(define-method expunge-deleted-messages ((folder <file-folder>))
  (without-interrupts
   (lambda ()
     (let loop
	 ((messages (file-folder-messages folder))
	  (index 0)
	  (messages* '()))
       (cond ((not (pair? messages))
	      (set-file-folder-messages! folder (reverse! messages*)))
	     ((message-deleted? (car messages))
	      (detach-message (car messages))
	      (folder-modified! folder)
	      (loop (cdr messages) index messages*))
	     (else
	      (if (not (eqv? index (message-index (car messages))))
		  (begin
		    (set-message-index! (car messages) index)
		    (message-modified! (car messages))))
	      (loop (cdr messages)
		    (fix:+ index 1)
		    (cons (car messages) messages*))))))))

(define-method search-folder ((folder <file-folder>) criteria)
  (cond ((string? criteria)
	 (let ((n (folder-length folder)))
	   (let loop ((index 0) (winners '()))
	     (if (< index n)
		 (loop (+ index 1)
		       (if (string-search-forward
			    criteria
			    (message->string (get-message folder index)))
			   (cons index winners)
			   winners))
		 (reverse! winners)))))
	(else
	 (error:wrong-type-argument criteria
				    "search criteria"
				    'SEARCH-FOLDER))))

(define-method synchronize-folder ((folder <file-folder>))
  folder
  unspecific)

(define-method %save-folder ((folder <file-folder>))
  (%write-folder folder (folder-url folder)))

(define-method %maybe-revert-folder ((folder <file-folder>) resolve-conflict)
  (if (if (eqv? (file-folder-modification-time folder)
		(file-modification-time (file-folder-pathname folder)))
	  (or (not (folder-modified? folder))
	      (resolve-conflict folder))
	  (folder-modified? folder))
      (%revert-folder folder)))