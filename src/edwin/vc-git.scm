#| -*-Scheme-*-

$Id: 5156290a598b708aecbb1cf072df0b4ff1371663 $

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

;;;; Version Control: git

(declare (usual-integrations))

(define vc-type:git
  (make-vc-type 'GIT "git" "\$Id\$"))

(define-vc-type-operation 'RELEASE vc-type:git
  (lambda ()
    (and (= 0 (vc-run-command #f '() "git" "--version"))
	 (let ((m (buffer-start (get-vc-command-buffer))))
	   (re-match-forward "git version \\(.+\\)$"
			     m
			     (line-end m 0)))
	 (extract-string (re-match-start 1) (re-match-end 1)))))

(define-vc-type-operation 'CONTROL-DIRECTORY vc-type:git
  (lambda (directory)
    (let ((cd (subdirectory-pathname directory ".git")))
      (if (file-directory? cd)
	  cd
	  'SEARCH-PARENT))))

(define-vc-type-operation 'FIND-MASTER vc-type:git
  (lambda (workfile control-dir)
    (and (%git-workfile-versioned? workfile)
	 (make-vc-master vc-type:git
			 (merge-pathnames "description" control-dir)
			 workfile))))

(define-vc-type-operation 'VALID? vc-type:git
  (lambda (master)
    (%git-workfile-versioned? (vc-master-workfile master))))

(define-vc-type-operation 'DEFAULT-REVISION vc-type:git
  (lambda (master)
    master
    #f))

(define-vc-type-operation 'WORKFILE-REVISION vc-type:git
  (lambda (master)
    (let ((result
	   (%git-run-command (vc-master-workfile master)
			     "symbolic-ref" "HEAD")))
      (and result
	   (let ((regs
		  (re-string-match "^\\(refs/heads/\\)?\\(.+\\)$" result)))
	     (if regs
		 (re-match-extract result regs 2)
		 result))))))

(define-vc-type-operation 'LOCKING-USER vc-type:git
  (lambda (master revision)
    revision				;ignore
    ;; The workfile is "locked" if it is modified.
    ;; We consider the workfile's owner to be the locker.
    (and (%git-workfile-modified? (vc-master-workfile master))
	 (unix/uid->string
	  (file-attributes/uid
	   (file-attributes (vc-master-workfile master)))))))

(define-vc-type-operation 'WORKFILE-MODIFIED? vc-type:git
  (lambda (master)
    (%git-workfile-modified? (vc-master-workfile master))))

(define-vc-type-operation 'NEXT-ACTION vc-type:git
  (lambda (master)
    (let ((status (%git-workfile-status (vc-master-workfile master))))
      (case status
	((UNVERSIONED UNKNOWN) #f)
	((UNMODIFIED)
	 (if (vc-workfile-buffer-modified? master)
	     'CHECKIN
	     'UNMODIFIED))
	((ADDED COPIED DELETED MODIFIED RENAMED TYPE-CHANGED) 'CHECKIN)
	((UNMERGED) 'PENDING-MERGE)
	(else (error "Unknown git status type:" status))))))

(define-vc-type-operation 'KEEP-WORKFILES? vc-type:git
  (lambda (master)
    master
    #t))

(define-vc-type-operation 'WORKFILE-STATUS-STRING vc-type:git
  (lambda (master)
    (let ((status (%git-workfile-status (vc-master-workfile master))))
      (if (eq? status 'UNMODIFIED)
	  #f
	  (symbol->string status)))))

(define-vc-type-operation 'REGISTER vc-type:git
  (lambda (workfile revision comment keep?)
    revision comment keep?
    (with-vc-command-message workfile "Registering"
      (lambda ()
	(vc-run-command workfile '() "git" "add" "--"
			(file-pathname workfile))))))

(define-vc-type-operation 'CHECKOUT vc-type:git
  (lambda (master revision lock? output-file)
    lock?
    (let ((workfile (file-pathname (vc-master-workfile master))))
      (with-vc-command-message master "Checking out"
	(lambda ()
	  (cond (output-file
		 (delete-file-no-errors output-file)
		 (vc-run-shell-command master '() "git" "show"
				       (string-append
					(or revision "HEAD")
					":"
					(enough-namestring
					 workfile
					 (directory-pathname
					  (vc-master-pathname master))))
				       ">"
				       output-file))
		(else
		 (vc-run-command master '() "git" "checkout"
				 (or revision "HEAD")
				 "--" workfile))))))))

(define-vc-type-operation 'CHECKIN vc-type:git
  (lambda (master revision comment keep?)
    revision keep?
    (with-vc-command-message master "Checking in"
      (lambda ()
	(vc-run-command master '() "git" "commit"
			"--message" comment
			(file-pathname (vc-master-workfile master)))))))

(define-vc-type-operation 'REVERT vc-type:git
  (lambda (master)
    (with-vc-command-message master "Reverting"
      (lambda ()
	(vc-run-command master '() "git" "checkout" "HEAD"
			"--" (file-pathname (vc-master-workfile master)))))))

(define-vc-type-operation 'STEAL vc-type:git
  (lambda (master revision)
    master revision
    (error "There are no git locks to steal.")))

(define-vc-type-operation 'DIFF vc-type:git
  (lambda (master rev1 rev2 simple?)
    (if (and rev1 rev2)
	(vc-run-command master (get-vc-diff-options simple?)
			"git" "diff-tree"
			"--exit-code" "-p"
			(and (not simple?)
			     (ref-variable git-diff-switches
					   (vc-workfile-buffer master #f)))
			rev1 rev2
			"--" (file-pathname (vc-master-workfile master)))
	(vc-run-command master (get-vc-diff-options simple?)
			"git" "diff-index"
			"--exit-code" "-p"
			(and (not simple?)
			     (ref-variable git-diff-switches
					   (vc-workfile-buffer master #f)))
			(or rev1 "HEAD")
			"--" (file-pathname (vc-master-workfile master))))
    (> (buffer-length (get-vc-diff-buffer simple?)) 0)))

(define-variable git-diff-switches
  "A list of strings specifying switches to pass to the `git diff' command."
  '()
  list-of-strings?)

(define-vc-type-operation 'PRINT-LOG vc-type:git
  (lambda (master)
    (vc-run-command master '() "git" "log" "--follow" "--name-status"
		    "--" (file-pathname (vc-master-workfile master)))))

(define-vc-type-operation 'CHECK-LOG-ENTRY vc-type:git
  (lambda (master log-buffer)
    master log-buffer
    unspecific))

(define-vc-type-operation 'CHECK-HEADERS vc-type:git
  (lambda (master buffer)
    master buffer
    #f))

(define (%git-workfile-status workfile)
  (if (%git-run-command workfile "add" "--refresh" "--"
			(file-namestring workfile))
      (let ((result
	     (%git-run-command workfile "diff-index" "-z" "HEAD" "--"
			       (file-namestring workfile))))
	(cond ((not result) 'UNKNOWN)
	      ((string-null? result) 'UNMODIFIED)
	      (else
	       (let ((regs
		      (re-string-match
		       "^:[0-7]+ [0-7]+ [0-9a-f]+ [0-9a-f]+ \\(.\\)[0-9]*\000"
		       result)))
		 (and regs
		      (let ((status
			     (string-ref (re-match-extract result regs 1)
					 0)))
			(case status
			  ((#\A) 'ADDED)
			  ((#\C) 'COPIED)
			  ((#\D) 'DELETED)
			  ((#\M) 'MODIFIED)
			  ((#\R) 'RENAMED)
			  ((#\T) 'TYPE-CHANGED)
			  ((#\U) 'UNMERGED)
			  (else (error "Unknown status:" status)))))))))
      'UNVERSIONED))

(define (%git-workfile-versioned? workfile)
  (not (memq (%git-workfile-status workfile) '(UNKNOWN UNVERSIONED))))

(define (%git-workfile-modified? workfile)
  (not (eq? (%git-workfile-status workfile) 'UNMODIFIED)))

(define (%git-run-command workfile command . args)
  (let ((directory (directory-pathname workfile)))
    (let ((program (os/find-program "git" directory #!default #f)))
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