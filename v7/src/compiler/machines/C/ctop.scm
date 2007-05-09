#| -*-Scheme-*-

$Id: ctop.scm,v 1.27 2007/05/09 02:05:50 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; C-output fake assembler and linker
;;; package: (compiler top-level)

(declare (usual-integrations))

;;;; Exports to the compiler

(define compiled-output-extension "c")
(define compiler:invoke-c-compiler? #t)
(define compiler:invoke-verbose? #t)

(define (compiler-file-output object pathname)
  (let ((pair (vector-ref object 1)))
    (call-with-output-file pathname
      (lambda (port)
	(c:write-group (cdr pair) port)))
    (if compiler:invoke-c-compiler? (c-compile pathname))))

(define (compile-data-from-file obj pathname)
  (let ((res (stringify-data obj (merge-pathnames pathname))))
    ;; Make output palatable to compiler-file-output
    (vector #f (cons #f res))))

(define (compiler-output->procedure compiler-output environment)
  (finish-c-compilation
   compiler-output
   (lambda (shared-library-pathname)
     (load shared-library-pathname environment))))

(define (compiler-output->compiled-expression compiler-output)
  (finish-c-compilation compiler-output fasload-liarc-object-file))

(define (compile-scode/internal/hook action)
  (if (not (eq? *info-output-filename* 'KEEP))
      (action)
      (fluid-let ((*info-output-filename*
		   (pathname-new-type (compiler-temporary-file-pathname)
				      "inf")))
	(action))))

(define (cross-compile-bin-file input . more)
  input more				; ignored
  (error "cross-compile-bin-file: Meaningless"))

(define (optimize-linear-lap lap-program)
  lap-program)

(define (compiler-temporary-file-pathname)
  (let ((pathname (temporary-file-pathname)))
    (if (file-exists? pathname)
	(delete-file pathname))
    (if (pathname-type pathname)
	(pathname-new-name
	 (pathname-new-type pathname #f)
	 (string-append (pathname-name pathname)
			"_"
			(pathname-type pathname)))
	pathname)))

(define (finish-c-compilation compiler-output action)
  (let* ((file (compiler-temporary-file-pathname))
	 (filec (pathname-new-type file "c")))
    (dynamic-wind
     (lambda () #f)
     (lambda ()
       (fluid-let ((compiler:invoke-c-compiler? #t))
	 (compiler-file-output compiler-output filec)
	 (action (pathname-new-type file (c-output-extension)))))
     (lambda ()
       (for-each (lambda (type)
		   (let ((f (pathname-new-type file type)))
		     (if (file-exists? f)
			 (delete-file f))))
		 (list "c" "o"
		       ;; Can't delete this because it is mapped...
		       ;; (c-output-extension)
		       ))))))

(define (c-compile pathname)
  (let ((run
	 (lambda tokens
	   (let ((command
		  (decorated-string-append
		   "" " " ""
		   (map (lambda (token)
			  (let ((s
				 (if (pathname? token)
				     (enough-namestring token)
				     token)))
			    (if (string-find-next-char s #\space)
				(string-append "\"" s "\"")
				s)))
			tokens))))
	     (maybe-with-notification compiler:invoke-verbose?
				      (lambda (port)
					(write-string "Executing " port)
					(write command port))
	       (lambda ()
		 (run-shell-command command)))))))
    (run (system-library-pathname "liarc-cc")
	 (pathname-new-type pathname "o")
	 pathname)
    (run (system-library-pathname "liarc-ld")
	 (pathname-new-type pathname (c-output-extension))
	 (pathname-new-type pathname "o"))))

(define (maybe-with-notification flag message thunk)
  (if flag
      (with-notification message thunk)
      (thunk)))

(define (c-output-extension)
  "so")

(define (recursive-compilation-results)
  (sort *recursive-compilation-results*
	(lambda (x y)
	  (< (vector-ref x 0)
	     (vector-ref y 0)))))

;; Global variables for assembler and linker

(define *recursive-compilation-results*)
(define *shared-namestring*)

;; First set: phase/rtl-generation
;; Last used: phase/link
(define *block-label*)
(define *disambiguator*)

(define *start-label*)

;; First set: phase/lap-generation
;; Last used: phase/info-generation-2
(define *external-labels*)
(define *special-labels*)

;; First set: phase/lap-generation
;; Last used: phase/output-generation ???
(define *invoke-interface*)
(define *used-invoke-primitive*)
(define *use-jump-execute-chache*)
(define *use-pop-return*)
(define *purification-root-object*)

;; First set: phase/assemble
;; Last used: phase/output-generation
(define *C-code-name*)
(define *C-data-name*)
(define *ntags*)
(define *labels*)
(define *code*)
(define *proxy*)

;; First set: phase/output-generation
(define *result*)

(define (assemble&link info-output-pathname)
  (phase/assemble info-output-pathname)
  (if info-output-pathname
      (phase/info-generation-2 *labels* info-output-pathname))
  (phase/output-generation)
  *result*)

(define (wrap-lap entry-label some-lap)
  (set! *start-label* entry-label)
  (LAP ,@(if *procedure-result?*
	     (LAP)
	     (lap:make-entry-point entry-label *block-label*))
       ,@some-lap))

(define (bind-assembler&linker-top-level-variables thunk)
  (fluid-let ((*recursive-compilation-results* '())
	      (*shared-namestring* #f))
    (thunk)))

(define (bind-assembler&linker-variables thunk)
  (fluid-let ((current-register-list)
	      (free-assignments)
	      (free-references)
	      (free-uuo-links)
	      (global-uuo-links)
	      (label-num)
	      (labels)
	      (objects)
	      (permanent-register-list)
	      (*block-label*)
	      (*disambiguator*)
	      (*start-label*)
	      (*external-labels*)
	      (*special-labels*)
	      (*invoke-interface*)
	      (*used-invoke-primitive*)
	      (*use-jump-execute-chache*)
	      (*use-pop-return*)
	      (*purification-root-object*)
	      (*end-of-block-code*)
	      (*C-code-name*)
	      (*C-data-name*)
	      (*ntags*)
	      (*labels*)
	      (*code*)
	      (*proxy*))
    (thunk)))

(define (assembler&linker-reset!)
  (set! *recursive-compilation-results* '())
  (set! current-register-list)
  (set! free-assignments)
  (set! free-references)
  (set! free-uuo-links)
  (set! global-uuo-links)
  (set! label-num)
  (set! labels)
  (set! objects)
  (set! permanent-register-list)
  (set! *block-label*)
  (set! *disambiguator*)
  (set! *start-label*)
  (set! *external-labels*)
  (set! *special-labels*)
  (set! *invoke-interface*)
  (set! *used-invoke-primitive*)
  (set! *use-jump-execute-chache*)
  (set! *use-pop-return*)
  (set! *purification-root-object*)
  (set! *end-of-block-code*)
  (set! *C-code-name*)
  (set! *C-data-name*)
  (set! *ntags*)
  (set! *labels*)
  (set! *code*)
  (set! *proxy*)
  unspecific)

(define (initialize-back-end!)
  (set! current-register-list '())
  (set! free-assignments (make-table))
  (set! free-references (make-table))
  (set! free-uuo-links (list 'FOO))
  (set! global-uuo-links (list 'BAR))
  (set! label-num 0)
  (set! labels '())
  (set! objects (make-table))
  (set! permanent-register-list '())
  (set! *block-label* (generate-label))
  (set! *disambiguator*
	(if (zero? *recursive-compilation-number*)
	    ""
	    (string-append (number->string *recursive-compilation-number*)
			   "_")))
  (set! *external-labels* '())
  (set! *special-labels* (make-special-labels))
  (set! *invoke-interface* 'INFINITY)
  (set! *used-invoke-primitive* #f)
  (set! *use-jump-execute-chache* #f)
  (set! *use-pop-return* #f)
  (set! *purification-root-object* #f)
  (set! *end-of-block-code* (LAP))
  unspecific)

(define (phase/assemble pathname)
  (compiler-phase
   "Pseudo-Assembly"			; garbage collection
   (lambda ()
     (with-values
	 (lambda ()
	   (stringify
	    (if (not (zero? *recursive-compilation-number*))
		(string-append
		 "_"
		 (number->string *recursive-compilation-number*))
		"")
	    (last-reference *start-label*)
	    (last-reference *lap*)
	    (cond ((eq? pathname 'RECURSIVE)
		   (cons *info-output-filename*
			 *recursive-compilation-number*))
		  ((eq? pathname 'KEEP)
		   (if (zero? *recursive-compilation-number*)
		       "foo.bar"
		       (cons "foo.bar" *recursive-compilation-number*)))
		  (else
		   pathname))))
       (lambda (code-name data-name ntags labels code proxy)
	 (set! *C-code-name* code-name)
	 (set! *C-data-name* data-name)
	 (set! *ntags* ntags)
	 (set! *labels* labels)
	 (set! *code* code)
	 (set! *proxy* proxy)
	 unspecific)))))

(define (phase/output-generation)
  (if (not (null? *ic-procedure-headers*))
      (error "phase/output-generation: Can't hack IC procedures"))

  (set! *result*
	(if *procedure-result?*
	    (let* ((linking-info *subprocedure-linking-info*)
		   (translate-label
		    (lambda (label)
		      (let ((place (assq label *labels*)))
			(if (not place)
			    (error "translate-label: Not found" label)
			    (cdr place)))))
		   (translate-symbol
		    (lambda (index)
		      (translate-label (vector-ref linking-info index))))
		   (index *recursive-compilation-number*)
		   (name (fake-compiled-block-name index)))
	      (let ((fcb
		     (make-fake-compiled-block name
					       *C-code-name* ; tag
					       *C-code-name* ; c-proc
					       *C-data-name* ; d-proc
					       *code* 	     ; c-code
					       index
					       *ntags*
					       *proxy*))
		    (lab (translate-label *entry-label*)))
		(cons (make-fake-compiled-procedure name lab fcb lab)
		      (vector
		       fcb
		       (translate-symbol 0)
		       (translate-symbol 1)
		       (translate-symbol 2)))))
	    (cons *C-code-name*
		  *code*)))

  (if (not compiler:preserve-data-structures?)
      (begin
	(set! *subprocedure-linking-info*)
	(set! *labels*)
	(set! *block-label*)
	(set! *entry-label*)
	(set! *ic-procedure-headers*)
	(set! *code*)
	(set! *proxy*)
	unspecific)))

(define (phase/info-generation-2 labels pathname)
  (info-generation-2 labels pathname))

(define (info-generation-2 labels pathname)
  (compiler-phase "Debugging Information Generation"
    (lambda ()
      (let ((info
	     (info-generation-phase-3
	      (last-reference *dbg-expression*)
	      (last-reference *dbg-procedures*)
	      (last-reference *dbg-continuations*)
	      labels
	      (last-reference *external-labels*))))
	(cond ((eq? pathname 'KEEP)	; for dynamic execution
	       info)
	      ((eq? pathname 'RECURSIVE) ; recursive compilation
	       (set! *recursive-compilation-results*
		     (cons (vector *recursive-compilation-number*
				   info
				   #f)
			   *recursive-compilation-results*))
	       unspecific)
	      (else
	       (compiler:dump-info-file
		(let ((others (recursive-compilation-results)))
		  (if (null? others)
		      info
		      (list->vector
		       (cons info
			     (map (lambda (other) (vector-ref other 1))
				  others)))))
		pathname)
	       *info-output-filename*))))))

(define (compiler:dump-bci-file binf pathname)
  (let ((bci-path (pathname-new-type pathname "bci")))
    (split-inf-structure! binf #f)
    (dump-compressed binf bci-path)))

(define (dump-compressed object path)
  (call-with-temporary-filename
    (lambda (temp)
      (fasdump object temp #t)
      (compress temp path))))

(define compiler:dump-info-file compiler:dump-bci-file)

;; This defintion exported to compiler to handle losing C name restrictions

(define (canonicalize-label-name prefix)
  (if (string-null? prefix)
      "empty_string"
      (let* ((str (if (char-alphabetic? (string-ref prefix 0))
		      (string-copy prefix)
		      (string-append "Z_" prefix)))
	     (len (string-length str)))
	(do ((i 0 (1+ i)))
	    ((>= i len) str)
	  (let ((char (string-ref str i)))
	    (if (not (char-alphanumeric? char))
		(string-set! str i
			     (case char
			       ((#\?) #\P)
			       ((#\!) #\B)
			       (else #\_)))))))))
