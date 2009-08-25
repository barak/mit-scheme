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

;;;; Version Control: Bazaar

(declare (usual-integrations))

(define vc-type:bzr
  (make-vc-type 'BZR "bzr" "\$Id\$"))

(define-vc-type-operation 'RELEASE vc-type:bzr
  (lambda ()
    (and (= 0 (vc-run-command #f '() "bzr" "--version"))
	 (let ((m (buffer-start (get-vc-command-buffer))))
	   (re-match-forward "Bazaar (bzr) \\(.+\\)$"
			     m
			     (line-end m 0)))
	 (extract-string (re-match-start 1) (re-match-end 1)))))

(define-vc-type-operation 'CONTROL-DIRECTORY vc-type:bzr
  (lambda (directory)
    (let ((cd (subdirectory-pathname directory ".bzr")))
      (if (file-directory? cd)
	  cd
	  'SEARCH-PARENT))))

(define-vc-type-operation 'FIND-MASTER vc-type:bzr
  (lambda (workfile control-dir)
    (let ((master
	   (make-vc-master vc-type:bzr
			   (merge-pathnames "README" control-dir)
			   workfile)))
      (and (%bzr-master-valid? master)
	   master))))

(define-vc-type-operation 'VALID? vc-type:bzr
  (lambda (master)
    (%bzr-master-valid? master)))

(define (%bzr-master-valid? master)
  (%bzr-workfile-cache master 'WORKFILE-VERSIONED? %bzr-workfile-versioned?))

(define-vc-type-operation 'DEFAULT-REVISION vc-type:bzr
  (lambda (master)
    master
    #f))

(define-vc-type-operation 'WORKFILE-REVISION vc-type:bzr
  (lambda (master)
    (bzr-workfile-revision master)))

(define-vc-type-operation 'LOCKING-USER vc-type:bzr
  (lambda (master revision)
    revision				;ignore
    ;; The workfile is "locked" if it is modified.
    ;; We consider the workfile's owner to be the locker.
    (let ((status (get-bzr-status master)))
      (and status
	   (bzr-status-modified? status)
	   (unix/uid->string
	    (file-attributes/uid
	     (file-attributes (vc-master-workfile master))))))))

(define (bzr-workfile-revision master)
  (let ((result
	 (%bzr-cached-command master 'WORKFILE-REVISION
			      "log" "--limit=1" "--line"
			      (file-namestring (vc-master-workfile master)))))
    (and result
	 (let ((regs (re-string-match "\\([0-9]+\\): \\([^ ]+\\) " result)))
	   (and regs
		(re-match-extract result regs 1))))))

(define-vc-type-operation 'WORKFILE-MODIFIED? vc-type:bzr
  (lambda (master)
    (let ((status (get-bzr-status master)))
      (and status
	   (bzr-status-modified? status)))))

(define-vc-type-operation 'NEXT-ACTION vc-type:bzr
  (lambda (master)
    (let ((status (get-bzr-status master #t)))
      (let ((type (bzr-status-mod-type status)))
	(case type
	  ((UNMODIFIED)
	   (let ((type (bzr-status-type status)))
	     (case type
	       ((VERSIONED)
		(if (vc-workfile-buffer-modified? master)
		    'CHECKIN
		    'UNMODIFIED))
	       ((UNVERSIONED UNKNOWN) #f)
	       ((RENAMED) 'CHECKIN)
	       ((CONFLICTED) 'RESOLVE-CONFLICT)
	       ((PENDING-MERGE) 'PENDING-MERGE)
	       (else (error "Unknown Bazaar status type:" type)))))
	  ((CREATED DELETED KIND-CHANGED MODIFIED) 'CHECKIN)
	  (else (error "Unknown Bazaar status type:" type)))))))

(define-vc-type-operation 'KEEP-WORKFILES? vc-type:bzr
  (lambda (master)
    master
    #t))

(define-vc-type-operation 'WORKFILE-STATUS-STRING vc-type:bzr
  (lambda (master)
    (let ((status (get-bzr-status master)))
      (and status
	   (let ((type (bzr-status-type status)))
	     (case type
	       ((VERSIONED)
		(case (bzr-status-mod-type status)
		  ((CREATED) "created")
		  ((DELETED) "deleted")
		  ((KIND-CHANGED) "kind-changed")
		  ((MODIFIED) "modified")
		  (else #f)))
	       ((UNVERSIONED) "unversioned")
	       ((RENAMED) "renamed")
	       ((UNKNOWN) "unknown")
	       ((CONFLICTED) "conflicted")
	       ((PENDING-MERGE) "pending-merge")
	       (else #f)))))))

(define-vc-type-operation 'REGISTER vc-type:bzr
  (lambda (workfile revision comment keep?)
    revision comment keep?
    (with-vc-command-message workfile "Registering"
      (lambda ()
	(vc-run-command workfile '() "bzr" "add" (file-pathname workfile))))))

(define-vc-type-operation 'CHECKOUT vc-type:bzr
  (lambda (master revision lock? workfile)
    lock?
    (let ((workfile* (file-pathname (vc-master-workfile master))))
      (with-vc-command-message master "Checking out"
	(lambda ()
	  (cond (workfile
		 (delete-file-no-errors workfile)
		 (vc-run-shell-command master '() "bzr" "cat"
				       (bzr-rev-switch revision)
				       workfile*
				       ">"
				       workfile))
		(else
		 (vc-run-command master '() "bzr" "update"
				 (bzr-rev-switch revision)
				 workfile*))))))))

(define-vc-type-operation 'CHECKIN vc-type:bzr
  (lambda (master revision comment keep?)
    keep?
    (with-vc-command-message master "Checking in"
      (lambda ()
	(vc-run-command master '() "bzr" "commit"
			(bzr-rev-switch revision)
			"--message" comment
			(file-pathname (vc-master-workfile master)))))))

(define-vc-type-operation 'REVERT vc-type:bzr
  (lambda (master)
    (with-vc-command-message master "Reverting"
      (lambda ()
	(vc-run-command master '() "bzr" "revert"
			(file-pathname (vc-master-workfile master)))))))

(define-vc-type-operation 'STEAL vc-type:bzr
  (lambda (master revision)
    master revision
    (error "There are no Bazaar locks to steal.")))

(define-vc-type-operation 'DIFF vc-type:bzr
  (lambda (master rev1 rev2 simple?)
    (vc-run-command master
		    (get-vc-diff-options simple?)
		    "bzr"
		    "diff"
		    (and (not simple?)
			 (decorated-string-append "--diff-options="
						  " "
						  ""
						  (gc-vc-diff-switches master)))
		    (and (or rev1 rev2)
			 (if (and rev1 rev2)
			     (string-append "-r" rev1 ".." rev2)
			     (string-append "-r" (or rev1 rev2) "..")))
		    (file-pathname (vc-master-workfile master)))
    (> (buffer-length (get-vc-diff-buffer simple?)) 0)))

(define-vc-type-operation 'PRINT-LOG vc-type:bzr
  (lambda (master)
    (vc-run-command master '() "bzr" "log"
		    (file-pathname (vc-master-workfile master)))))

(define-vc-type-operation 'CHECK-LOG-ENTRY vc-type:bzr
  (lambda (master log-buffer)
    master log-buffer
    unspecific))

(define-vc-type-operation 'CHECK-HEADERS vc-type:bzr
  (lambda (master buffer)
    master buffer
    #f))

(define-vc-type-operation 'MODE-LINE-STATUS vc-type:bzr
  (lambda (master buffer)
    buffer
    (if (vc-backend-workfile-modified? master)
	" bzr **"
	" bzr --")))

(define (bzr-rev-switch revision)
  (and revision
       (list "-r" revision)))

(define (bzr-directory workfile)
  (let ((dir (merge-pathnames (directory-pathname workfile)))
	(bzr (pathname-as-directory ".bzr")))
    (let loop ((path (pathname-directory dir)))
      (let ((dir* (merge-pathnames bzr (pathname-new-directory dir path))))
	(cond ((file-directory? dir*) dir*)
	      ((pair? (cdr path)) (loop (except-last-pair path)))
	      (else #f))))))

(define (%bzr-workfile-versioned? workfile)
  (%bzr-ls-test workfile "--versioned"))

(define (%bzr-workfile-ignored? workfile)
  (%bzr-ls-test workfile "--ignored"))

(define (%bzr-ls-test workfile option)
  (let ((result (%bzr-run-command workfile "ls" "--non-recursive" option)))
    (and result
	 (re-string-search-forward (string-append "^"
						  (re-quote-string
						   (file-namestring workfile))
						  "$")
				   result))))

(define (%bzr-cached-command master key command . args)
  (%bzr-workfile-cache master key
    (lambda (workfile)
      (apply %bzr-run-command workfile command args))))

(define (%bzr-run-command workfile command . args)
  (let ((directory (directory-pathname workfile)))
    (let ((program (os/find-program "bzr" directory #!default #f)))
      (and program
	   (let ((port (open-output-string)))
	     (let ((status
		    (run-synchronous-subprocess
		     program
		     (cons command args)
		     'output port
		     'working-directory directory)))
	       (and (eqv? status 0)
		    (get-output-string port))))))))

(define (%bzr-workfile-cache master key procedure)
  (let ((workfile (vc-master-workfile master)))
    (read-cached-value-1 master key workfile
      (lambda (time)
	time
	(procedure workfile)))))

(define (get-bzr-status master #!optional required?)
  (%bzr-workfile-cache master 'GET-STATUS
    (lambda (workfile)
      (or (parse-bzr-status
	   (%bzr-run-command workfile "status" "--short"
			     (file-namestring workfile)))
	  (cond ((%bzr-master-valid? master)
		 (make-bzr-status 'VERSIONED 'UNMODIFIED #f))
		(else
		 (if (if (default-object? required?) #f required?)
		     (error "Unable to determine Bazaar status of file:"
			    workfile))
		 #f))))))

(define (parse-bzr-status status)
  (and status
       (not (string-null? status))
       (let ((regs (re-string-match "[ +---R?CP][ NDKM][ *] " status #f)))
	 (and regs
	      (make-bzr-status
	       (decode-bzr-status-0 (string-ref status 0))
	       (decode-bzr-status-1 (string-ref status 1))
	       (decode-bzr-status-2 (string-ref status 2)))))))

(define-record-type <bzr-status>
    (make-bzr-status type mod-type execute-changed?)
    bzr-status?
  (type bzr-status-type)
  (mod-type bzr-status-mod-type)
  (execute-changed? bzr-status-execute-changed?))

(define (bzr-status-modified? status)
  (not (eq? (bzr-status-mod-type status) 'UNMODIFIED)))

(define (decode-bzr-status-0 char)
  (case char
    ((#\space #\+) 'VERSIONED)
    ((#\-) 'UNVERSIONED)
    ((#\R) 'RENAMED)
    ((#\?) 'UNKNOWN)
    ((#\C) 'CONFLICTED)
    ((#\P) 'PENDING-MERGE)
    (else (error "Unknown status char 0:" char))))

(define (decode-bzr-status-1 char)
  (case char
    ((#\space) 'UNMODIFIED)
    ((#\N) 'CREATED)
    ((#\D) 'DELETED)
    ((#\K) 'KIND-CHANGED)
    ((#\M) 'MODIFIED)
    (else (error "Unknown status char 1:" char))))

(define (decode-bzr-status-2 char)
  (case char
    ((#\space) #f)
    ((#\*) #t)
    (else (error "Unknown status char 2:" char))))