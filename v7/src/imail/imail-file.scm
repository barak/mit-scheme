;;; -*-Scheme-*-
;;;
;;; $Id: imail-file.scm,v 1.2 2000/01/14 18:09:04 cph Exp $
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

(define-method subscribed-folder-names ((url <file-url>))
  url
  (error "Unimplemented operation:" 'SUBSCRIBED-FOLDER-NAMES))

;;;; Folder

(define-class <file-folder> (<folder>)
  (url accessor folder-url)
  (messages define standard))

(define-method %folder-valid? ((folder <file-folder>))
  (file-exists? (file-url-pathname (folder-url folder))))

(define-method count-messages ((folder <file-folder>))
  (length (file-folder-messages folder)))

(define-method %get-message ((folder <file-folder>) index)
  (list-ref (file-folder-messages folder) index))

(define-method %insert-message ((folder <file-folder>) index message)
  (let ((message (copy-message message))
	(messages (file-folder-messages folder)))
    (if (fix:= 0 index)
	(set-file-folder-messages! folder (cons message messages))
	(let loop ((index* 1) (prev messages) (this (cdr messages)))
	  (if (fix:= index index*)
	      (set-cdr! prev (cons message this))
	      (loop (fix:+ index* 1) this (cdr this)))))))

(define-method %append-message ((folder <file-folder>) message)
  (set-file-folder-messages! folder
			     (append! (file-folder-messages folder)
				      (list (copy-message message)))))

(define-method expunge-deleted-messages ((folder <file-folder>))
  (set-file-folder-messages!
   folder
   (list-transform-negative (file-folder-messages folder) message-deleted?)))

(define-method search-folder ((folder <file-folder>) criteria)
  folder criteria
  (error "Unimplemented operation:" 'SEARCH-FOLDER))

(define-method synchronize-folder ((folder <file-folder>))
  folder
  unspecific)

(define-method subscribe-folder ((folder <file-folder>))
  folder
  (error "Unimplemented operation:" 'SUBSCRIBE-FOLDER))

(define-method unsubscribe-folder ((folder <file-folder>))
  folder
  (error "Unimplemented operation:" 'UNSUBSCRIBE-FOLDER))