;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/manual.scm,v 1.1 1991/09/17 20:36:42 arthur Exp $
;;;
;;;	Copyright (c) 1991 Massachusetts Institute of Technology
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

;;;; Display Manual Pages

(declare (usual-integrations))

;;; MAKE THIS A LOAD-OPTION!

;;; THIS PAGE SHOULD BE IN $se/paths.scm!

(define manual-program false)

;; Prefix for directories containing formatted manual pages.  Append a
;; section-number or section-name to get a directory name.
(define manual-formatted-dir-prefix false)

(set! manual-program
      (if (file-exists? "/usr/bin/man")
	  "/usr/bin/man"
	  "/usr/ucb/man"))

;; Note that /usr/man/cat is not really right for this on sysV; nothing is,
;; judging by the list of directories below.  You can't get the dir
;; for a section by appending the section number to any one prefix.
;; But it turns out that a string that's wrong does no harm here.
(set! manual-formatted-dir-prefix
      (if (file-exists? "/usr/man/cat.C")  ;; Check for Xenix.
	  "/usr/man/cat."
	  "/usr/man/cat"))

;; IS THIS GOING TO BE TOO SLOW?  I DID THIS TO AVOID HAVING TO
;; CLASSIFY BY OPERATING SYSTEM.

;; List of directories containing formatted manual pages.
(set! manual-formatted-dirlist
      (list-transform-positive
	  '("/usr/catman/u_man/man1" "/usr/catman/u_man/man6"
	    "/usr/catman/p_man/man2" "/usr/catman/p_man/man3"
	    "/usr/catman/p_man/man4" "/usr/catman/p_man/man5"
	    "/usr/catman/a_man/man1" "/usr/catman/a_man/man7"
	    "/usr/catman/a_man/man8" "/usr/catman/local"
	    "/usr/man/cat1"          "/usr/man/cat2"
	    "/usr/man/cat3"          "/usr/man/cat4"
	    "/usr/man/cat5"          "/usr/man/cat6"
	    "/usr/man/cat7"          "/usr/man/cat8"
	    "/usr/man/catl"          "/usr/man/catn"
	    "/usr/man/cat.C"         "/usr/man/cat.CP"
	    "/usr/man/cat.CT"        "/usr/man/cat.DOS/"
	    "/usr/man/cat.F"         "/usr/man/cat.HW"
	    "/usr/man/cat.M/"        "/usr/man/cat.S"
	    "/usr/man/cat.LOCAL"     "/usr/man/cat1"
	    "/usr/man/cat2"          "/usr/man/cat3"
	    "/usr/man/cat4"          "/usr/man/cat5"
	    "/usr/man/cat6"          "/usr/man/cat7"
	    "/usr/man/cat1m"         "/usr/man/cat8"
	    "/usr/local/man/cat1"    "/usr/local/man/cat2"
	    "/usr/local/man/cat3"    "/usr/local/man/cat4"
	    "/usr/local/man/cat5"    "/usr/local/man/cat6"
	    "/usr/local/man/cat7"    "/usr/local/man/cat1m"
	    "/usr/local/man/cat8"    "/usr/contrib/man/cat1"
	    "/usr/contrib/man/cat2"  "/usr/contrib/man/cat3"
	    "/usr/contrib/man/cat4"  "/usr/contrib/man/cat5"
	    "/usr/contrib/man/cat6"  "/usr/contrib/man/cat7"
	    "/usr/contrib/man/cat1m" "/usr/contrib/man/cat8")
	file-exists?))

(define-variable manual-entry-reuse-buffer?
  "If true, MANUAL-ENTRY uses buffer *Manual Entry* for all entries.
Otherwise, a new buffer is created for each topic."
  true
  boolean?)

(define (manual-string-match pattern string)
  (re-match-string-forward
   (re-compile-pattern pattern false)
   true
   false
   string))

(define-command manual-entry
  "Display the Unix manual entry for TOPIC.
TOPIC is either the title of the entry, or has the form TITLE(SECTION)
where SECTION is the desired section of the manual, as in `tty(4)'."
  "sManual entry (topic): "
  (lambda (topic #!optional section)
    (if (and (default-object? section)
	     (manual-string-match
	      "\\`[ \t]*\\([^( \t]+\\)[ \t]*(\\(.+\\))[ \t]*\\'"
	      topic))
	(begin
	  (set! section
		(substring topic (match-beginning 2) (match-end 2)))
	  (set! topic
		(substring topic (match-beginning 1) (match-end 1))))
	(set! section false))
    (let* ((section-suffix
	    (if section (string-append "(" section ")") ""))
	   (buffer-name
	    (if (ref-variable manual-entry-reuse-buffer?)
		"*Manual Entry*"
		(string-append
		 "*"
		 topic
		 section-suffix
		 " Manual Entry*"))))
      (if
       (with-output-to-temporary-buffer buffer-name
	 (lambda ()
	   (let ((buffer (find-buffer buffer-name)))
	     (with-selected-buffer buffer
	       (lambda ()
		 (message "Looking for formatted entry for "
			  topic
			  section-suffix
			  "...")
		 (let (
					;WHAT SHOULD I DO ABOUT THIS? :
		       (case-fold-search nil))
		   (cond ((and section
			       (let ((prefix manual-formatted-dir-prefix))
				 (or (let ((name (string-append
						  prefix
						  (substring section 0 1)
						  "/"
						  topic "." section)))
				       (and (file-exists? name)
					    name))
				     (let ((name (string-append
						  prefix
						  section
						  "/"
						  topic "." section)))
				       (and (file-exists? name)
					    name)))))
			  => insert-man-file)
			 (else
			  (let loop ((dirlist manual-formatted-dirlist))
			    (if (not (null? dirlist))
				(let ((directory (car dirlist)))
				  (let ((name (string-append
					       directory "/" topic "."
					       (or section
						   (substring
						    directory
						    (1+ (or (manual-string-match
							     "\\.[^./]*$" directory)
							    -2)))))))
				    (if (file-exists? name)
					(insert-man-file name)
					(call-with-current-continuation
					 (lambda (ignore-error)
					   (bind-condition-handler
					       condition-type:file-error
					       (lambda (condition)
						 (ignore-error unspecific))
					     (lambda ()
					       (let loop ((completions
							   (filename-completions-list
							    (pathname-new-directory
							     (->pathname (string-append topic "." (or section "")))
							     directory))))
						 (if (not (null? completions))
						     (begin
						       (insert-man-file (string-append directory "/" (car completions)))
						       (loop (cdr completions))))))))))
				    (loop (cdr dirlist))))))))
		   (if (= (buffer-size) 0)
		       (progn
			(message "No formatted entry, invoking man "
				 (if section (string-append section " ") "")
				 topic
				 "...")
			(if section
			    (call-process manual-program nil t nil section topic)
			    (call-process manual-program nil t nil topic))))
		   (if (< (buffer-size) 80)
		       (let ((start (buffer-start buffer)))
			 (buffer-not-modified! buffer)
			 (extract-string start (line-end start 0)))
		       (begin
			 (message "Cleaning manual entry for %s..." topic)
			 (nuke-nroff-bs buffer)
			 (buffer-not-modified! buffer)
			 (set-buffer-read-only! buffer)
			 (message "Manual page ready")
			 false))))))))
       (begin
	 (kill-buffer buffer-name)
	 (editor-failure error-string))))))

(define (nuke-nroff-bs buffer)
  ;; Nuke underlining and overstriking (only by the same letter)
  (let ((start (buffer-start buffer))
	(end (buffer-end buffer)))
    (let loop ((point
		(search-forward "\b" start end false)))
      (if point
	  (let* ((two-back (mark- point 2))
		 (preceding (extract-right-char two-back))
		 (following (extract-right-char point)))
	    (cond ((char=? preceding following)
		   ;; x\bx
		   (region-delete! (make-region point two-back)))
		  ((char=? preceding #\_)
		   ;; _\b
		   (region-delete! (make-region point two-back)))
		  ((char=? following #\_)
		   ;; \b_
		   (region-delete!
		    (make-region (mark-1+ point) (mark1+ point)))))
	    (loop (search-forward "\b" point end false)))))

    ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
    (let ((pattern "^ *\\([A-Za-z][-_A-Za-z0-9]*([0-9A-Z]+)\\).*\\1$"))
      (let loop ((point
		  (re-search-forward pattern start end false)))
	(if point
	    (begin
	      (replace-match "" false true)
	      (loop (re-search-forward pattern point end false))))))

    ;; Nuke footers: "Printed 12/3/85	27 April 1981	1"
    (let ((pattern
	   (cond ((eq? system-type 'hpux)
		  "^[ \t]*Hewlett-Packard\\(\\| Company\\)[ \t]*- [0-9]* -.*$")
		 ((eq? system-type 'usg-unix-v)
		  "^ *Page [0-9]*.*(printed [0-9/]*)$")
		 (else
		  "^\\(Printed\\|Sun Release\\) [0-9].*[0-9]$"))))
      (let loop ((point
		  (re-search-forward pattern start end false)))
	(if point
	    (begin (replace-match "" false true)
		   (loop (re-search-forward pattern point end false))))))

    ;; Crunch blank lines
    (let ((pattern "\n\n\n\n*"))
      (let loop ((point
		  (re-search-forward pattern start end false)))
	(if point
	    (begin (replace-match "\n\n" false true)
		   (loop (re-search-forward pattern point end false))))))

    ;; Nuke blanks lines at start.
    (region-delete!
     (make-region start
		  (skip-chars-forward "\n" start end 'limit)))))

(define (insert-man-file name)
  (let ((buffer (current-buffer))
	(pathname (->pathname name)))
    (if (or (string=? "Z" (pathname-type pathname))
	    (manual-string-match "/cat[0-9][a-z]?\\.Z/" name))
	(call-process "zcat" name t nil)
	(if (string=? "z" (pathname-type pathname))
	    (call-process "pcat" nil t nil name)
	    (insert-file (buffer-end buffer) name)))
    (set-buffer-point! buffer (buffer-end buffer))))