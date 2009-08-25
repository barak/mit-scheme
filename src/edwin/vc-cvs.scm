#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Version Control: CVS

(declare (usual-integrations))

(define vc-type:cvs
  (make-vc-type 'CVS "CVS" "\$Id\$"))

(define (cvs-master? master)
  (eq? vc-type:cvs (vc-master-type master)))

(define (cvs-directory workfile)
  (subdirectory-pathname workfile "CVS"))

(define (get-cvs-workfile-revision master error?)
  (let ((tokens (find-cvs-entry master)))
    (if tokens
	(cadr tokens)
	(and error?
	     (error "Workfile has no version:" (vc-master-workfile master))))))

(define (find-cvs-entry master)
  (let ((pathname (vc-master-pathname master)))
    (read-cached-value-1 master 'CVS-ENTRY pathname
      (lambda (time)
	time
	(%find-cvs-entry pathname (vc-master-workfile master))))))

(define (%find-cvs-entry pathname workfile)
  (let ((line
	 (find-cvs-line pathname
			(string-append "/" (file-namestring workfile) "/"))))
    (and line
	 (let ((tokens (cdr (burst-string line #\/ #f))))
	   (and (fix:= 5 (length tokens))
		tokens)))))

(define (cvs-workfile-protected? workfile)
  (string-prefix? "-r-"
		  (file-attributes/mode-string (file-attributes workfile))))

(define (cvs-file-edited? master)
  (let ((pathname
	 (merge-pathnames "Baserev"
			  (directory-pathname (vc-master-pathname master)))))
    (read-cached-value-1 master 'CVS-FILE-EDITED? pathname
      (lambda (time)
	time
	(find-cvs-line pathname
		       (string-append
			"B"
			(file-namestring (vc-master-workfile master))
			"/"))))))

(define (find-cvs-line pathname prefix)
  (and (file-readable? pathname)
       (call-with-input-file pathname
	 (lambda (port)
	   (let loop ()
	     (let ((line (read-line port)))
	       (and (not (eof-object? line))
		    (if (string-prefix? prefix line)
			line
			(loop)))))))))

(define (cvs-status master)
  (if (vc-cvs-stay-local? master)
      (if (vc-backend-workfile-modified? master)
	  'LOCALLY-MODIFIED
	  'UP-TO-DATE)
      (get-cvs-status master
	(lambda (m)
	  (if (re-search-forward "^File: [^ \t]+[ \t]+Status: \\(.*\\)" m)
	      (convert-cvs-status
	       (extract-string (re-match-start 1) (re-match-end 1)))
	      'UNKNOWN)))))

(define (cvs-default-revision master)
  (get-cvs-status master
    (lambda (m)
      (and (re-search-forward cvs-status-regexp m)
	   (extract-string (re-match-start 2) (re-match-end 2))))))

(define cvs-status-regexp
  "\\(RCS Version\\|RCS Revision\\|Repository revision\\):[ \t]+\\([0-9.]+\\)")

(define (get-cvs-status master parse-output)
  (vc-run-command master
		  `((BUFFER " *vc-status*"))
		  "cvs" "status"
		  (file-pathname (vc-master-workfile master)))
  (parse-output (buffer-start (find-or-create-buffer " *vc-status*"))))

(define (convert-cvs-status status)
  (cond ((string-ci=? status "Up-to-date")
	 'UP-TO-DATE)
	((string-ci=? status "Locally Modified")
	 'LOCALLY-MODIFIED)
	((or (string-ci=? status "Locally Added")
	     (string-ci=? status "New file!"))
	 'LOCALLY-ADDED)
	((string-ci=? status "Locally Removed")
	 'LOCALLY-REMOVED)
	((or (string-ci=? status "Needs Checkout")
	     (string-ci=? status "Needs Patch"))
	 'NEEDS-CHECKOUT)
	((string-ci=? status "Needs Merge")
	 'NEEDS-MERGE)
	((or (string-ci=? status "File had conflicts on merge")
	     (string-ci=? status "Unresolved Conflict"))
	 'UNRESOLVED-CONFLICT)
	(else
	 'UNKNOWN)))

(define (cvs-rev-switch revision)
  (and revision
       (list "-r" revision)))

(define (vc-cvs-stay-local? master)
  (ref-variable vc-cvs-stay-local (vc-workfile-buffer master #f)))

(define (vc-cvs-workfile-mtime-string master)
  (read-cached-value-2 master 'CVS-MTIME-STRING
		       (vc-master-pathname master)
		       (vc-master-workfile master)
    (lambda (tm tw)
      (and tm tw
	   (let ((entry (find-cvs-entry master)))
	     (and entry
		  (caddr entry)))))))

(define (set-vc-cvs-workfile-mtime-string! master tm tw modified?)
  (if (and tm tw (not modified?))
      (begin
	;; This breaks the READ-CACHED-VALUE-2 abstraction:
	(vc-master-put! master 'CVS-MTIME-STRING
			(vector (file-time->global-ctime-string tw) tm tw))
	(let ((buffer (pathname->buffer (vc-master-workfile master))))
	  (if buffer
	      (vc-mode-line master buffer))))))

(define-vc-type-operation 'RELEASE vc-type:cvs
  (lambda ()
    (and (= 0 (vc-run-command #f '() "cvs" "-v"))
	 (re-search-forward "^Concurrent Versions System (CVS) \\([0-9.]+\\)"
			    (buffer-start (get-vc-command-buffer)))
	 (extract-string (re-match-start 1) (re-match-end 1)))))

(define-vc-type-operation 'FIND-MASTER vc-type:cvs
  (lambda (workfile control-dir)
    (let ((entries-file (merge-pathnames "Entries" control-dir)))
      (and (%find-cvs-entry entries-file workfile)
	   (make-vc-master vc-type:cvs entries-file workfile)))))

(define-vc-type-operation 'VALID? vc-type:cvs
  (lambda (master)
    (get-cvs-workfile-revision master #f)))

(define-vc-type-operation 'DEFAULT-REVISION vc-type:cvs
  (lambda (master)
    (cvs-default-revision master)))

(define-vc-type-operation 'WORKFILE-REVISION vc-type:cvs
  (lambda (master)
    (get-cvs-workfile-revision master #t)))

(define-vc-type-operation 'LOCKING-USER vc-type:cvs
  (lambda (master revision)
    ;; The workfile is "locked" if it is modified.
    ;; We consider the workfile's owner to be the locker.
    (and (or (not revision)
	     (equal? revision (vc-backend-workfile-revision master)))
	 (or (not
	      (let ((t1 (file-modification-time (vc-master-workfile master)))
		    (t2 (vc-cvs-workfile-mtime-string master)))
		(and t1 t2
		     (string=? (file-time->global-ctime-string t1) t2))))
	     (cvs-file-edited? master))
         (let ((attributes (file-attributes (vc-master-workfile master))))
           (and attributes
                (unix/uid->string (file-attributes/uid attributes)))))))

(define-vc-type-operation 'WORKFILE-MODIFIED? vc-type:cvs
  (lambda (master)
    (read-cached-value-2 master 'MODIFIED?
			 (vc-master-pathname master)
			 (vc-master-workfile master)
      (lambda (tm tw)
	(if (and tm tw
		 (let ((ts (vc-cvs-workfile-mtime-string master)))
		   (and ts
			(string=? ts (file-time->global-ctime-string tw)))))
	    #f
	    (or (vc-cvs-stay-local? master)
		(let ((modified? (vc-backend-diff master #f #f #t)))
		  (set-vc-cvs-workfile-mtime-string! master tm tw modified?)
		  modified?)))))))

(define-vc-type-operation 'NEXT-ACTION vc-type:cvs
  (lambda (master)
    (case (cvs-status master)
      ((UP-TO-DATE)
       (if (or (vc-workfile-buffer-modified? master)
	       (cvs-file-edited? master))
	   'CHECKIN
	   'UNMODIFIED))
      ((NEEDS-CHECKOUT NEEDS-MERGE) 'MERGE)
      ((LOCALLY-MODIFIED LOCALLY-ADDED LOCALLY-REMOVED) 'CHECKIN)
      ((UNRESOLVED-CONFLICT) 'RESOLVE-CONFLICT)
      (else
       (error "Unable to determine CVS status of file:"
	      (vc-master-workfile master))))))

(define-vc-type-operation 'KEEP-WORKFILES? vc-type:cvs
  (lambda (master)
    master
    #t))

(define-vc-type-operation 'WORKFILE-STATUS-STRING vc-type:cvs
  (lambda (master)
    (case (cvs-status master)
      ((LOCALLY-MODIFIED) "modified")
      ((LOCALLY-ADDED) "added")
      ((NEEDS-CHECKOUT) "patch")
      ((NEEDS-MERGE) "merge")
      ((UNRESOLVED-CONFLICT) "conflict")
      (else #f))))

(define-vc-type-operation 'CONTROL-DIRECTORY vc-type:cvs
  (lambda (directory)
    (let ((cd (cvs-directory directory)))
      (and (file-directory? cd)
	   cd))))

(define-vc-type-operation 'STEAL vc-type:cvs
  (lambda (master revision)
    master revision
    (error "You cannot steal a CVS lock; there are no CVS locks to steal.")))

(define-vc-type-operation 'REGISTER vc-type:cvs
  (lambda (workfile revision comment keep?)
    revision keep?			;always keep file.
    (with-vc-command-message workfile "Registering"
      (lambda ()
	(vc-run-command workfile '() "cvs" "add"
			"-m" comment
			(file-pathname workfile))))))

(define-vc-type-operation 'CHECKOUT vc-type:cvs
  (lambda (master revision lock? workfile)
    (let ((workfile* (file-pathname (vc-master-workfile master))))
      (with-vc-command-message master "Checking out"
	(lambda ()
	  (cond (workfile
		 ;; CVS makes it difficult to check a file out into
		 ;; anything but the working file.
		 (delete-file-no-errors workfile)
		 (vc-run-shell-command master '() "cvs" "update" "-p"
				       (cvs-rev-switch revision)
				       workfile*
				       ">"
				       workfile))
		(revision
		 (vc-run-command master '() "cvs" (and lock? "-w") "update"
				 (cvs-rev-switch revision)
				 workfile*))
		(else
		 (vc-run-command master '() "cvs" "edit" workfile*))))))))

(define-vc-type-operation 'CHECKIN vc-type:cvs
  (lambda (master revision comment keep?)
    keep?
    (with-vc-command-message master "Checking in"
      (lambda ()
	(bind-condition-handler (list condition-type:editor-error)
	    (lambda (condition)
	      condition
	      (if (eq? 'NEEDS-MERGE (cvs-status master))
		  ;; The CVS output will be on top of this message.
		  (error "Type C-x 0 C-x C-q to merge in changes.")))
	  (lambda ()
	    ;; Explicit check-in to the trunk requires a double check-in
	    ;; (first unexplicit) (CVS-1.3).  [This is copied from Emacs
	    ;; 20.6, but I don't understand it. -- CPH]
	    (if (and revision
		     (not (equal? revision
				  (vc-backend-workfile-revision master)))
		     (trunk-revision? revision))
		(vc-run-command master '() "cvs" "commit"
				"-m" "#intermediate"
				(file-pathname (vc-master-workfile master))))
	    (vc-run-command master '() "cvs" "commit"
			    (cvs-rev-switch revision)
			    "-m" comment
			    (file-pathname (vc-master-workfile master)))))
	;; If this was an explicit check-in, remove the sticky tag.
	(if revision
	    (vc-run-command master '() "cvs" "update" "-A"
			    (file-pathname (vc-master-workfile master))))))))

(define-vc-type-operation 'REVERT vc-type:cvs
  (lambda (master)
    (with-vc-command-message master "Reverting"
      (lambda ()
	(let ((workfile (vc-master-workfile master)))
	  (if (cvs-file-edited? master)
	      (vc-run-command master '() "cvs" "unedit"
			      (file-pathname workfile))
	      (begin
		(delete-file-no-errors workfile)
		(vc-run-command master '() "cvs" "update"
				(file-pathname workfile)))))))))

(define-vc-type-operation 'DIFF vc-type:cvs
  (lambda (master rev1 rev2 simple?)
    (= 1
       (vc-run-command master
		       (get-vc-diff-options simple?)
		       "cvs"
		       "diff"
		       (if simple?
			   (and (diff-brief-available?) "--brief")
			   (gc-vc-diff-switches master))
		       (and rev1 (string-append "-r" rev1))
		       (and rev2 (string-append "-r" rev2))
		       (file-pathname (vc-master-workfile master))))))

(define-vc-type-operation 'PRINT-LOG vc-type:cvs
  (lambda (master)
    (vc-run-command master '() "cvs" "log"
		    (file-pathname (vc-master-workfile master)))))

(define-vc-type-operation 'CHECK-LOG-ENTRY vc-type:cvs
  (lambda (master log-buffer)
    master log-buffer
    unspecific))

(define-vc-type-operation 'CHECK-HEADERS vc-type:cvs
  (lambda (master buffer)
    master
    (check-rcs-headers buffer)))

(define (cvs-backend-merge-news master)
  (with-vc-command-message master "Merging changes into"
    (lambda ()
      (let ((workfile (vc-master-workfile master)))
	(vc-run-command master '() "cvs" "update" (file-pathname workfile))
	(let ((buffer (get-vc-command-buffer))
	      (fn (re-quote-string (file-namestring workfile))))
	  (cond ((re-search-forward
		  (string-append "^\\([CMUP]\\) " fn)
		  (buffer-start buffer))
		 (char=? #\C (extract-right-char (re-match-start 0))))
		((re-search-forward
		  (string-append fn
				 " already contains the differences between ")
		  (buffer-start buffer))
		 ;; Special case: file contents in sync with repository
		 ;; anyhow:
		 #f)
		(else
		 (pop-up-buffer buffer #f)
		 (error "Couldn't analyze cvs update result."))))))))