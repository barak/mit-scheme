#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/6001/edextra.scm,v 1.7 1992/09/04 21:45:20 nick Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; 6.001: Edwin Extensions

(declare (usual-integrations))

(load-edwin-library 'PRINT)

(define-command print-graphics
  "Print out the last displayed picture."
  '()
  (lambda ()
    (call-with-last-picture-file
     (lambda (filename)
       (if filename
	   (begin
	     (message "Spooling...")
	     (shell-command
	      false false false false
	      (string-append "/users/u6001/bin/print-pgm.sh "
			     filename
			     " "
			     (print/assemble-switches "Scheme Picture" '())))
	     (append-message "done"))
	   (editor-error "No picture to print!"))))))

(environment-link-name '(edwin)
		       '(student pictures)
		       'call-with-last-picture-file)

(define (restore-focus-to-editor)
  (let ((screen (selected-screen)))
    (if (xterm-screen/grab-focus! screen)
	(xterm-screen/flush! screen))))

(environment-link-name '(student pictures)
		       '(edwin)
		       'restore-focus-to-editor)



;;;; EDWIN Command "Load Problem Set"

(declare (usual-integrations))
(using-syntax (access edwin-syntax-table (->environment '(edwin)))
(in-package (->environment '(edwin))

;;; Wired-in pathnames

;;; We look in the "psn" subdir for problem set n
(define pset-dir "~u6001/psets/")
(define pset-list-file (merge-pathnames "probsets.scm" pset-dir))
(define student-dir "~u6001/work/")

;;; The structure "problem-sets" must be loaded from pset-list-file whenever
;;; the set of available problem sets changes, or when the default
;;; problem set changes.  Files should appear with name and extension, but
;;; without device, directory, or version; these will be supplied
;;; automatically.
;;;
;;; Example problem-sets variable:

;(define problem-sets
;  `(1 (1  (load&reference "ps1-c-curve.scm" "ps1-debug.scm"))
;      (2  (copy "ps2-ans.scm") (load&reference "ps2-primes.scm"))
;      (3  (copy "ps3-ans.scm")
;	  (load&reference "ps3-squares.scm" "ps3-tri.scm"))
;      (4  (copy "ps4-ans.scm") (load&reference "ps4-doctor.scm")
;	  (select "ps4-ans.scm"))
;      (5  (copy "ps5-ans.scm")
;	  (load&reference "ps5-graph.scm" "ps5-imp.scm" "ps5-res.scm"))
;      (6  (copy "ps6-mods.scm") (load&reference "ps6-adv.scm"))
;      (7  (copy "ps7-ans.scm")
;	  (load&reference "ps7-ps.scm" "ps7-psutil.scm" "ps7-ratnum.scm"))
;      (8  (copy "ps8-mods.scm") (load&reference "ps8-mceval.scm"))))

;;; Data abstraction for the "problem-sets" object:

(define problem-sets/default-ps car)
(define problem-sets/psets cdr)
(define psets/first-pset car)
(define psets/rest-psets cdr)
(define psets/empty? null?)
(define pset/ps car)
(define pset/groups cdr)
(define (groups/files-to-copy groups)
  (let ((any (assq 'copy groups)))
    (if any (cdr any) '())))
(define (groups/files-to-load groups)
  (let ((any (assq 'load groups)))
    (if any (cdr any) '())))
(define (groups/files-to-reference groups)
  (let ((any (assq 'reference groups)))
    (if any (cdr any) '())))
(define (groups/files-to-load&reference groups)
  (let ((any (assq 'load&reference groups)))
    (if any (cdr any) '())))
(define (groups/buffer-to-select groups)
  (let ((any (assq 'select groups)))
    (if any (cadr any) '())))
(define (groups/all-files groups)
  (merge-lists (groups/files-to-copy groups)
	       (groups/files-to-load groups)
	       (groups/files-to-reference groups)
	       (groups/files-to-load&reference groups)))


;;; Procedure to get the "files" object corresponding to a particular
;;; problem set.  Runs error-handler (which should never return) if
;;; the problem set number is not listed in the "problem-sets" object.

(define (ps-groups ps error-handler)
  (let loop ((remaining-psets (problem-sets/psets problem-sets)))
    (if (psets/empty? remaining-psets)
	(error-handler)
	(let ((first-ps (psets/first-pset remaining-psets)))
	  (if (string=? ps (->string (pset/ps first-ps)))
	      (pset/groups first-ps)
	      (loop (psets/rest-psets remaining-psets)))))))

;;; Horribly inefficient procedure to merge lists, ensuring that no member
;;; is repeated in the resulting list.
(define (merge-lists . lists)
  (let ((one-list (apply append lists)))
    (let loop ((remaining one-list)
	       (accumulated '()))
      (if (null? remaining)
	  accumulated
	  (let ((first (car remaining))
		(rest (cdr remaining)))
	    (if (memq first rest)
		(loop rest accumulated)
		(loop rest (cons first accumulated))))))))

;;; Returns #t iff files all exist in directory.
(define (files-all-exist? files directory)
  (let loop ((files files))
    (or (null? files)
	(and (file-exists?
	      (merge-pathnames directory (->pathname (car files))))
	     (loop (cdr files))))))

;;; Return the string representation of a number.
(define (number->string number)
  (with-output-to-string (lambda () (write number))))

;;; Return the number represented by string.  Note that even if string does not
;;; represent a number, string->number will convert it to whatever object READ
;;; would when presented with the contents of that string as input.  Therefore,
;;; it may be necessary to test to see if the result is a number.
(define (string->number string)
  (with-input-from-string string read))



(define (->string object)
  (if (string? object)
      object
      (with-output-to-string (lambda () (display object)))))

(define-command Load-Problem-Set
  "Load a 6.001 problem set."
  ()
  (lambda ()
  (begin
    (load pset-list-file (->environment '(edwin)))
   (let* ((default-ps (problem-sets/default-ps problem-sets))
	  (ps (prompt-for-string "Load Problem Set" (->string default-ps)))
	  )
     (let* ((error-handler
	     (lambda () (editor-error "There doesn't appear to be a problem set "
				      ps
				      " installed; ask a TA for help.")))
	    (groups (ps-groups ps error-handler))
	    (pset-path (merge-pathnames (string-append "ps" (->string ps) "/") 
					pset-dir)))
       (or (files-all-exist? (groups/all-files groups) pset-path)
	   (error-handler))
       (map (lambda (file)
	      (find-file (merge-pathnames pset-path
					  (->pathname file))))
	    (groups/files-to-reference groups))
       (map (lambda (file)
	      (let ((filename (merge-pathnames pset-path
					       (->pathname file))))
		(message "Evaluating file " (->namestring filename))
		(load filename (->environment '(student)))
		(append-message " -- done")))
	    (groups/files-to-load groups))
       (map (lambda (file)
	      (let ((filename (merge-pathnames pset-path
					       (->pathname file))))
		(message "Evaluating file " (->namestring filename))
		(load filename (->environment '(student)))
		(append-message " -- done")
		(find-file filename)))
	    (groups/files-to-load&reference groups))
       (map (lambda (file)
	      (let ((source-file (merge-pathnames pset-path (->pathname file)))
		    (dest-file (merge-pathnames student-dir (->pathname file))))
		(message "Copying file " (->namestring file) " to working area")
		(let ((buffer (find-buffer (->namestring dest-file))))
		  (if buffer (kill-buffer buffer)))
		(find-file source-file)
		(let ((buffer (current-buffer)))
		  (set-buffer-writeable! buffer)
		  (set-visited-pathname buffer dest-file)
		  (write-buffer buffer))
		(append-message " -- done")
		(find-file dest-file)))
	    (groups/files-to-copy groups))
       )))))
))

;;; Edwin Variables:
;;; scheme-environment: '(edwin)
;;; scheme-syntax-table: 'edwin-syntax-table
;;; End:
