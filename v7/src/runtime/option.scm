#| -*-Scheme-*-

$Id: option.scm,v 14.29 1994/09/30 02:37:48 adams Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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

;;;; Option Loader
;;; package: (runtime options)

(declare (usual-integrations))

(define *initial-options-file*  #F)

(define loaded-options  '())


(define (initial-options-file-pathname)
  (define (library-file? library-internal-path)
    (let* ((library    (library-directory-pathname ""))
	   (pathname   (merge-pathnames library-internal-path library)))
      (and (file-exists? pathname)
	   pathname)))
  (or *initial-options-file*
      (get-environment-variable "MITSCHEME_LOAD_OPTIONS")
      (library-file? "options.db")
      (library-file? "options/options.db")
      (error "Cannot locate an options database")
      "options.db"))


(define (load-option name)
  
  (define (eval-filename form)
    (eval form system-global-environment))

  (define (process-descriptor descriptor)
    (let ((environment (package/environment (find-package (car descriptor)))))
      (for-each
	  (lambda (filename-form)
	    (let ((filename  (eval-filename filename-form)))
	      (cond 
	       (((ucode-primitive initialize-c-compiled-block 1)
		 (string-append "runtime_" filename))
		=> (lambda (obj)
		     (purify obj)
		     (scode-eval obj environment)))
	       (else
		(let ((path (merge-pathnames filename (library-directory-pathname "options"))))
		  (with-working-directory-pathname
		   (directory-pathname path)
		   (lambda ()
		     (load path
			   environment
			   syntax-table/system-internal
			   true))))))))
	(cddr descriptor))
      (eval (cadr descriptor) environment)))
  
  (define (load-entry entry)
    (for-each process-descriptor (cdr entry))
    (set! loaded-options (cons name loaded-options))
    unspecific)

  (define (file-loop options-file)
    (let ((options (with-input-from-file options-file read)))
      (verify-options-syntax options options-file)
      (cond ((assq name (cdr options)) => load-entry)
	    ((car options)
	     (file-loop
	      (merge-pathnames (eval-filename (car options))
			       (library-directory-pathname ""))))
	    (else
	     (error "Unknown option name:" name)))))

  (define (verify-options-syntax options filename)
    (define (verify-entry thing)
      (if (not (and (pair? thing)
		    (symbol? (car thing))
		    (list? (cdr thing))))
	  (error "Bad entry in options database" filename thing)))
    (if (and (pair? options)
	     (list? (cdr options)))
	(for-each verify-entry (cdr options))
	(error "Bad options database" filename options)))


  (if (not (memq name loaded-options))
      (file-loop  (initial-options-file-pathname)))
  name)


(define (library-directory-pathname name)
  (or (system-library-directory-pathname name)
      (library-directory-pathname
       (error:file-operation name
			     "find"
			     "directory"
			     "no such directory in system library path"
			     library-directory-pathname
			     (list name)))))


(define (declare-shared-library shared-library thunk)
  (let ((thunk-valid?
	 (lambda (thunk)
	   (not (condition? (ignore-errors thunk))))))
    (add-event-receiver!
     event:after-restore
     (lambda ()
       (if (not (thunk-valid? thunk))
	   (fluid-let ((load/suppress-loading-message? true))
	     (load (merge-pathnames
		    (library-directory-pathname "shared")
		    shared-library))))))))
