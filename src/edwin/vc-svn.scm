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

;;;; Version Control: Subversion

(declare (usual-integrations))

(define vc-type:svn
  (make-vc-type 'SVN "SVN" "\$Id\$"))

(define-vc-type-operation 'RELEASE vc-type:svn
  (lambda ()
    (and (= 0 (vc-run-command #f '() "svn" "--version"))
	 (re-search-forward "svn, version \\([0-9.]+\\)"
			    (buffer-start (get-vc-command-buffer)))
	 (extract-string (re-match-start 1) (re-match-end 1)))))

(define-vc-type-operation 'CONTROL-DIRECTORY vc-type:svn
  (lambda (directory)
    (let ((cd (svn-directory directory)))
      (and (file-directory? cd)
	   cd))))

(define-vc-type-operation 'FIND-MASTER vc-type:svn
  (lambda (workfile control-dir)
    (and (not (let ((output (%get-svn-status workfile)))
		(or (not output)
		    (string-null? output)
		    (string-prefix? "?" output)
		    (string-prefix? "I" output))))
	 (make-vc-master vc-type:svn
			 (merge-pathnames "entries" control-dir)
			 workfile))))

(define (svn-directory workfile)
  (subdirectory-pathname workfile ".svn"))

(define-vc-type-operation 'VALID? vc-type:svn
  (lambda (master)
    (let ((status (get-svn-status (vc-master-workfile master))))
      (and status
	   (svn-status-working-revision status)))))

(define-vc-type-operation 'DEFAULT-REVISION vc-type:svn
  (lambda (master)
    (let ((workfile (vc-master-workfile master)))
      (let ((status (get-svn-status workfile #f)))
	(and status
	     (svn-status-working-revision status))))))

(define-vc-type-operation 'WORKFILE-REVISION vc-type:svn
  (lambda (master)
    (let ((status (get-svn-status master #f)))
      (and status
	   (svn-status-last-change-revision status)))))

(define-vc-type-operation 'LOCKING-USER vc-type:svn
  (lambda (master revision)
    ;; The workfile is "locked" if it is modified.
    ;; We consider the workfile's owner to be the locker.
    (let ((workfile (vc-master-workfile master)))
      (let ((status (get-svn-status workfile)))
	(and status
	     (or (not revision)
		 (equal? revision (svn-status-last-change-revision status)))
	     (svn-status-modified? status)
	     (unix/uid->string
	      (file-attributes/uid (file-attributes workfile))))))))

(define-vc-type-operation 'WORKFILE-MODIFIED? vc-type:svn
  (lambda (master)
    (let ((status (get-svn-status master)))
      (and status
	   (svn-status-modified? status)))))

(define (svn-status-modified? status)
  (memq (svn-status-type status)
	'(ADDED CONFLICTED DELETED MERGED MODIFIED REPLACED)))

(define-vc-type-operation 'NEXT-ACTION vc-type:svn
  (lambda (master)
    (let ((status (get-svn-status master #t)))
      (let ((type (svn-status-type status)))
	(case type
	  ((UNMODIFIED)
	   (if (vc-workfile-buffer-modified? master)
	       'CHECKIN
	       'UNMODIFIED))
	  ((MODIFIED ADDED DELETED REPLACED) 'CHECKIN)
	  ((CONFLICTED) 'RESOLVE-CONFLICT)
	  ((MISSING) 'CHECKOUT)
	  (else (error "Unknown SVN status type:" type)))))))

(define-vc-type-operation 'KEEP-WORKFILES? vc-type:svn
  (lambda (master)
    master
    #t))

(define-vc-type-operation 'WORKFILE-STATUS-STRING vc-type:svn
  (lambda (master)
    (let ((status (get-svn-status master)))
      (and status
	   (let ((type (svn-status-type status)))
	     (case type
	       ((ADDED) "added")
	       ((CONFLICTED) "conflicted")
	       ((DELETED) "deleted")
	       ((MERGED) "merged")
	       ((MODIFIED) "modified")
	       ((REPLACED) "replaced")
	       ((MISSING) "missing")
	       (else #f)))))))

(define-vc-type-operation 'REGISTER vc-type:svn
  (lambda (workfile revision comment keep?)
    revision comment keep?
    (with-vc-command-message workfile "Registering"
      (lambda ()
	(vc-run-command workfile '() "svn" "add" (file-pathname workfile))))))

(define-vc-type-operation 'CHECKOUT vc-type:svn
  (lambda (master revision lock? workfile)
    lock?
    (let ((workfile* (file-pathname (vc-master-workfile master))))
      (with-vc-command-message master "Checking out"
	(lambda ()
	  (cond (workfile
		 (delete-file-no-errors workfile)
		 (vc-run-shell-command master '() "svn" "cat"
				       (svn-rev-switch revision)
				       workfile*
				       ">"
				       workfile))
		(else
		 (vc-run-command master '() "svn" "update"
				 (svn-rev-switch revision)
				 workfile*))))))))

(define-vc-type-operation 'CHECKIN vc-type:svn
  (lambda (master revision comment keep?)
    keep?
    (with-vc-command-message master "Checking in"
      (lambda ()
	(vc-run-command master '() "svn" "commit"
			(svn-rev-switch revision)
			"--message" comment
			(file-pathname (vc-master-workfile master)))))))

(define-vc-type-operation 'REVERT vc-type:svn
  (lambda (master)
    (with-vc-command-message master "Reverting"
      (lambda ()
	(vc-run-command master '() "svn" "revert"
			(file-pathname (vc-master-workfile master)))))))

(define-vc-type-operation 'STEAL vc-type:svn
  (lambda (master revision)
    master revision
    (error "There are no Subversion locks to steal.")))

(define-vc-type-operation 'DIFF vc-type:svn
  (lambda (master rev1 rev2 simple?)
    (vc-run-command master
		    (get-vc-diff-options simple?)
		    "svn"
		    "diff"
		    (if simple?
			#f
			(let loop ((switches (gc-vc-diff-switches master)))
			  (if (pair? switches)
			      (cons* "-x" (car switches)
				     (loop (cdr switches)))
			      '())))
		    (and rev1 (string-append "-r" rev1))
		    (and rev2 (string-append "-r" rev2))
		    (file-pathname (vc-master-workfile master)))
    (> (buffer-length (get-vc-diff-buffer simple?)) 0)))

(define-vc-type-operation 'PRINT-LOG vc-type:svn
  (lambda (master)
    (vc-run-command master '() "svn" "log"
		    (file-pathname (vc-master-workfile master)))))

(define-vc-type-operation 'CHECK-LOG-ENTRY vc-type:svn
  (lambda (master log-buffer)
    master log-buffer
    unspecific))

(define-vc-type-operation 'CHECK-HEADERS vc-type:svn
  (lambda (master buffer)
    master
    (check-rcs-headers buffer)))

(define (svn-rev-switch revision)
  (and revision
       (list "-r" revision)))

(define (get-svn-status workfile #!optional required?)
  (let ((workfile
	 (if (vc-master? workfile)
	     (vc-master-workfile workfile)
	     workfile)))
    (let ((status (parse-svn-status (%get-svn-status workfile))))
      (if (and (not status) (if (default-object? required?) #f required?))
	  (error "Unable to determine SVN status of file:" workfile))
      status)))

(define (%get-svn-status workfile)
  (let ((directory (directory-pathname workfile)))
    (let ((program (os/find-program "svn" directory #!default #f)))
      (and program
	   (let ((port (open-output-string)))
	     (let ((status
		    (run-synchronous-subprocess
		     program
		     (list "status" "--verbose" (file-namestring workfile))
		     'output port
		     'working-directory directory)))
	       (and (eqv? status 0)
		    (get-output-string port))))))))

(define (parse-svn-status status)
  (and status
       (not (string-null? status))
       (let ((type (decode-svn-status-0 (string-ref status 0))))
	 (if (or (eq? type 'UNVERSIONED)
		 (eq? type 'IGNORED))
	     type
	     (let ((regs (re-string-match svn-status-regexp status #f)))
	       (and regs
		    (make-svn-status
		     type
		     (decode-svn-status-1 (string-ref status 1))
		     (decode-svn-status-2 (string-ref status 2))
		     (decode-svn-status-3 (string-ref status 3))
		     (decode-svn-status-4 (string-ref status 4))
		     (decode-svn-status-5 (string-ref status 5))
		     (decode-svn-status-7 (string-ref status 7))
		     (decode-svn-working-revision
		      (re-match-extract status regs 1))
		     (decode-svn-last-change-revision
		      (re-match-extract status regs 2))
		     (re-match-extract status regs 3))))))))

(define svn-status-regexp
  (string-append ".[ CM][ L][ +][ S][ KOTB] [ *]"
		 " +\\([0-9]+\\|-\\|\\?\\)"
		 " +\\([0-9]+\\|\\?\\)"
		 " +\\([^ ]+\\)"
		 " +"))

(define-record-type <svn-status>
    (make-svn-status type properties locked? history? switched? lock-token
		     updated? working-revision
		     last-change-revision last-change-author)
    svn-status?
  (type svn-status-type)
  (properties svn-status-properties)
  (locked? svn-status-locked?)
  (history? svn-status-history?)
  (switched? svn-status-switched?)
  (lock-token svn-status-lock-token)
  (updated? svn-status-updated?)
  (working-revision svn-status-working-revision)
  (last-change-revision svn-status-last-change-revision)
  (last-change-author svn-status-last-change-author))

(define (decode-svn-status-0 char)
  (case char
    ((#\space) 'UNMODIFIED)
    ((#\A) 'ADDED)
    ((#\C) 'CONFLICTED)
    ((#\D) 'DELETED)
    ((#\G) 'MERGED)
    ((#\I) 'IGNORED)
    ((#\M) 'MODIFIED)
    ((#\R) 'REPLACED)
    ((#\X) 'USED-BY-EXTERNALS)
    ((#\?) 'UNVERSIONED)
    ((#\!) 'MISSING)
    ((#\~) 'OBSTRUCTED)
    (else (error "Unknown status char 0:" char))))

(define (decode-svn-status-1 char)
  (case char
    ((#\space) 'UNMODIFIED)
    ((#\C) 'CONFLICTED)
    ((#\M) 'MODIFIED)
    (else (error "Unknown status char 1:" char))))

(define (decode-svn-status-2 char)
  (case char
    ((#\space) #f)
    ((#\L) #t)
    (else (error "Unknown status char 2:" char))))

(define (decode-svn-status-3 char)
  (case char
    ((#\space) #f)
    ((#\+) #t)
    (else (error "Unknown status char 3:" char))))

(define (decode-svn-status-4 char)
  (case char
    ((#\space) #f)
    ((#\S) #t)
    (else (error "Unknown status char 4:" char))))

(define (decode-svn-status-5 char)
  (case char
    ((#\space) #f)
    ((#\K) 'PRESENT)
    ((#\O) 'ABSENT)
    ((#\T) 'STOLEN)
    ((#\B) 'BROKEN)
    (else (error "Unknown status char 5:" char))))

(define (decode-svn-status-7 char)
  (case char
    ((#\space) #f)
    ((#\*) #t)
    (else (error "Unknown status char 7:" char))))

(define (decode-svn-working-revision string)
  (if (string=? string "?")
      #f
      string))

(define (decode-svn-last-change-revision string)
  (if (string=? string "?")
      "0"
      string))