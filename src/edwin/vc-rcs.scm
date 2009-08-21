#| -*-Scheme-*-

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

;;;; Version Control: RCS

(declare (usual-integrations))

(define vc-type:rcs
  ;; Splitting up string constant prevents RCS from expanding this
  ;; keyword.
  (make-vc-type 'RCS "RCS" "\$Id\$"))

(define (rcs-master? master)
  (eq? vc-type:rcs (vc-master-type master)))

(define (rcs-directory workfile)
  (subdirectory-pathname workfile "RCS"))

(define (get-rcs-admin master)
  (let ((pathname (vc-master-pathname master)))
    (read-cached-value-1 master 'RCS-ADMIN pathname
			 (lambda (time) time (parse-rcs-admin pathname)))))

(define (check-rcs-headers buffer)
  (re-search-forward (string-append "\\$[A-Za-z\300-\326\330-\366\370-\377]+"
				    "\\(: [\t -#%-\176\240-\377]*\\)?\\$")
		     (buffer-start buffer)
		     (buffer-end buffer)))

(define (rcs-rev-switch switch revision)
  (if revision
      (string-append switch revision)
      switch))

(define (rcs-mtime-switch master)
  (and (ref-variable vc-rcs-preserve-mod-times
		     (pathname->buffer (->workfile master)))
       "-M"))

(define-vc-type-operation 'RELEASE vc-type:rcs
  (lambda ()
    (and (= 0 (vc-run-command #f '() "rcs" "-V"))
	 (re-search-forward "^RCS version \\([0-9.]+ *.*\\)"
			    (buffer-start (get-vc-command-buffer)))
	 (extract-string (re-match-start 1) (re-match-end 1)))))

(define-vc-type-operation 'FIND-MASTER vc-type:rcs
  (lambda (workfile control-dir)
    (let ((try
	   (lambda (transform)
	     (let ((master-file (transform workfile)))
	       (and (file-exists? master-file)
		    (make-vc-master vc-type:rcs master-file workfile)))))
	  (in-control-dir
	   (lambda (pathname)
	     (merge-pathnames (file-pathname pathname) control-dir)))
	  (rcs-file
	   (lambda (pathname)
	     (merge-pathnames (string-append (file-namestring pathname) ",v")
			      (directory-pathname pathname)))))
      (or (try (lambda (workfile) (rcs-file (in-control-dir workfile))))
	  (try in-control-dir)
	  (try rcs-file)))))

(define-vc-type-operation 'VALID? vc-type:rcs
  (lambda (master)
    (file-exists? (vc-master-pathname master))))

(define-vc-type-operation 'DEFAULT-REVISION vc-type:rcs
  (lambda (master)
    (let ((delta (rcs-find-delta (get-rcs-admin master) #f #f)))
      (and delta
	   (rcs-delta/number delta)))))

(define-vc-type-operation 'WORKFILE-REVISION vc-type:rcs
  (lambda (master)
    (let ((workfile (vc-master-workfile master)))
      (read-cached-value-1 master 'RCS-WORKFILE-REVISION workfile
	(lambda (time)
	  time
	  (let ((parse-buffer
		 (lambda (buffer)
		   (let ((start (buffer-start buffer))
			 (end (buffer-end buffer)))
		     (let ((find-keyword
			    (lambda (keyword)
			      (let ((mark
				     (search-forward
				      (string-append "$" keyword ":")
				      start end #f)))
				(and mark
				     (skip-chars-forward " " mark end #f)))))
			   (get-revision
			    (lambda (start)
			      (let ((end
				     (skip-chars-forward "0-9." start end)))
				(and (mark< start end)
				     (let ((revision
					    (extract-string start end)))
				       (let ((length
					      (rcs-number-length revision)))
					 (and (> length 2)
					      (even? length)
					      (rcs-number-head revision
							       (- length 1)
							       #f)))))))))
		       (cond ((or (find-keyword "Id") (find-keyword "Header"))
			      => (lambda (mark)
				   (get-revision
				    (skip-chars-forward
				     " "
				     (skip-chars-forward "^ " mark end)
				     end))))
			     ((find-keyword "Revision") => get-revision)
			     (else #f)))))))
	    (let ((buffer (pathname->buffer workfile)))
	      (if buffer
		  (parse-buffer buffer)
		  (call-with-temporary-buffer " *VC-temp*"
		    (lambda (buffer)
		      (catch-file-errors (lambda (condition) condition #f)
			(lambda ()
			  (read-buffer buffer workfile #f)
			  (parse-buffer buffer)))))))))))))

(define-vc-type-operation 'WORKFILE-MODIFIED? vc-type:rcs
  (lambda (master)
    (read-cached-value-2 master 'MODIFIED?
			 (vc-master-pathname master)
			 (vc-master-workfile master)
      (lambda (tm tw)
	tm tw
	(vc-backend-diff master #f #f #t)))))

(define-vc-type-operation 'NEXT-ACTION vc-type:rcs
  (lambda (master)
    (let ((owner (vc-backend-locking-user master #f)))
      (cond ((not owner) 'CHECKOUT)
	    ((string=? owner (current-user-name)) 'CHECKIN)
	    (else 'STEAL-LOCK)))))

(define-vc-type-operation 'KEEP-WORKFILES? vc-type:rcs
  (lambda (master)
    (ref-variable vc-keep-workfiles (vc-workfile-buffer master #f))))

(define-vc-type-operation 'WORKFILE-STATUS-STRING vc-type:rcs
  (lambda (master)
    (vc-backend-locking-user master #f)))

(define-vc-type-operation 'LOCKING-USER vc-type:rcs
  (lambda (master revision)
    (let ((admin (get-rcs-admin master)))
      (let ((delta
	     (rcs-find-delta admin
			     (or revision
				 (vc-backend-workfile-revision master))
			     #f)))
	(and delta
	     (let loop ((locks (rcs-admin/locks admin)))
	       (and (not (null? locks))
		    (if (eq? delta (cdar locks))
			(caar locks)
			(loop (cdr locks))))))))))

(define-vc-type-operation 'CONTROL-DIRECTORY vc-type:rcs
  (lambda (directory)
    (let ((cd (rcs-directory directory)))
      (and (file-directory? cd)
	   (any (lambda (pathname)
		  (string-suffix? ",v" (file-namestring pathname)))
		(directory-read cd))
	   cd))))

(define-vc-type-operation 'REGISTER vc-type:rcs
  (lambda (workfile revision comment keep?)
    (with-vc-command-message workfile "Registering"
      (lambda ()
	(vc-run-command workfile '() "ci"
			(and (vc-release? vc-type:rcs "5.6.4") "-i")
			(rcs-rev-switch (cond ((not keep?) "-r")
					      ((eq? 'LOCK keep?) "-l")
					      (else "-u"))
					revision)
			(rcs-mtime-switch workfile)
			(string-append "-t-" comment)
			workfile)))))

(define-vc-type-operation 'CHECKOUT vc-type:rcs
  (lambda (master revision lock? workfile)
    (let ((revision (or revision (vc-backend-workfile-revision master))))
      (with-vc-command-message master "Checking out"
	(lambda ()
	  (if workfile
	      ;; RCS makes it difficult to check a file out into anything
	      ;; but the working file.
	      (begin
		(delete-file-no-errors workfile)
		(vc-run-shell-command master '() "co"
				      (rcs-rev-switch "-p" revision)
				      (vc-master-workfile master)
				      ">"
				      workfile)
		(set-file-modes! workfile (if lock? #o644 #o444)))
	      (vc-run-command master '() "co"
			      (rcs-rev-switch (if lock? "-l" "-r") revision)
			      (rcs-mtime-switch master)
			      (vc-master-workfile master))))))))

(define-vc-type-operation 'CHECKIN vc-type:rcs
  (lambda (master revision comment keep?)
    (with-vc-command-message master "Checking in"
      (lambda ()
	(vc-run-command master '() "ci"
			;; If available, use the secure check-in option.
			(and (vc-release? vc-type:rcs "5.6.4") "-j")
			(rcs-rev-switch (if keep? "-u" "-r") revision)
			(rcs-mtime-switch master)
			(string-append "-m" comment)
			(vc-master-workfile master))))))

(define-vc-type-operation 'REVERT vc-type:rcs
  (lambda (master)
    (with-vc-command-message master "Reverting"
      (lambda ()
	(vc-run-command master '() "co"
			"-f" "-u"
			(rcs-mtime-switch master)
			(vc-master-workfile master))))))

(define-vc-type-operation 'STEAL vc-type:rcs
  (lambda (master revision)
    (if (not (vc-release? vc-type:rcs "5.6.2"))
	(error "Unable to steal locks with this version of RCS."))
    (let ((revision (or revision (vc-backend-workfile-revision master))))
      (with-vc-command-message master "Stealing lock on"
	(lambda ()
	  (vc-run-command master '() "rcs"
			  "-M"
			  (rcs-rev-switch "-u" revision)
			  (rcs-rev-switch "-l" revision)
			  (vc-master-workfile master)))))))

(define-vc-type-operation 'DIFF vc-type:rcs
  (lambda (master rev1 rev2 simple?)
    (= 1
       (vc-run-command master
		       (get-vc-diff-options simple?)
		       "rcsdiff"
		       "-q"
		       (if (and rev1 rev2)
			   (list (string-append "-r" rev1)
				 (string-append "-r" rev2))
			   (let ((rev
				  (or rev1 rev2
				      (vc-backend-workfile-revision master))))
			     (and rev
				  (string-append "-r" rev))))
		       (if simple?
			   (and (diff-brief-available?) "--brief")
			   (gc-vc-diff-switches master))
		       (vc-master-workfile master)))))

(define-vc-type-operation 'PRINT-LOG vc-type:rcs
  (lambda (master)
    (vc-run-command master '() "rlog" (vc-master-workfile master))))

(define-vc-type-operation 'CHECK-LOG-ENTRY vc-type:rcs
  (lambda (master log-buffer)
    master log-buffer
    unspecific))

(define-vc-type-operation 'CHECK-HEADERS vc-type:rcs
  (lambda (master buffer)
    master
    (check-rcs-headers buffer)))