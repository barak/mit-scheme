#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/etc/pack.scm,v 1.5 1992/05/23 00:10:32 jinx Exp $

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

;;;; Binary file packer, to be loaded in (runtime load)

(declare (usual-integrations))

(define (pack-binaries output files-lists)
  (define (->string pathname-or-string)
    (if (string? pathname-or-string)
        pathname-or-string
        (->namestring pathname-or-string)))

  (define (make-load-wrapper output files-lists)
    (syntax
     `((in-package 
         (->environment '(runtime load))
         (lambda (environment-to-load)
           (if (not load/loading?)
               (error "packed-wrapper: Evaluated when not loaded!")
               (let ((pathname load/current-pathname))
                 (set! load/after-load-hooks
                       (cons (lambda ()
                               (load-packed-binaries
                                 pathname
                                 ,(->string output)
				 ,(length files-lists)
                                 environment-to-load))
                             load/after-load-hooks))))))
       (the-environment))
     system-global-syntax-table))

  (if (and (not (string? output))
           (not (pathname? output)))
      (error "pack-binaries: Bad output file" output))
  (if (null? files-lists)
      (error "pack-binaries: No files"))

  (let* ((pathnames-lists
	  (map (lambda (files)
		 (let ((dir (car files)))
		   (cons dir
			 (with-working-directory-pathname dir
			   (lambda ()
			     (map
			      (lambda (file)
				(let ((pathname (->pathname file)))
				  (if (not (file-exists? pathname))
				      (error "pack-binaries: Cannot find" file)
				      pathname)))
				  (cdr files)))))))
	       files-lists))
         (wrapper (make-load-wrapper output files-lists)))

    (with-binary-output-file
      output
      (lambda (channel)
        (channel-fasdump wrapper channel false)
	(for-each (lambda (pathnames)
		    (with-working-directory-pathname (car pathnames)
		      (lambda ()
			(channel-fasdump (map (lambda (pathname)
						(cons (->string pathname)
						      (fasload pathname)))
					      (cdr pathnames))
					 channel
					 false))))
		  pathnames-lists)))))

;;;; Utilities and installation

(define (with-binary-output-file file action)
  (with-binary-file-channel file action
    open-binary-output-file
    output-port/channel
    'with-binary-output-file))

(define channel-fasdump
  (make-primitive-procedure 'primitive-fasdump 3))

;;; Link to global

(let ((system-global-environment '())
      (this-environment (the-environment)))
  (if (not (environment-bound? system-global-environment
                               'pack-binaries))
      (environment-link-name system-global-environment this-environment
                             'pack-binaries)))