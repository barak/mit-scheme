#| -*-Scheme-*-

$Id$

Copyright (c) 1992-93 Massachusetts Institute of Technology

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
  (if (null? files-lists)
      (error:bad-range-argument files-lists 'PACK-BINARIES))
  (with-binary-output-file (->pathname output)
    (lambda (channel)
      (channel-fasdump
       (syntax
	`((lambda (environment-to-load)
	    (load/push-hook!
	     (let ((pathname (current-load-pathname)))
	       (lambda ()
		 ((access load-packed-binaries
			  (->environment '(runtime load)))
		  pathname
		  ,(->namestring output)
		  ,(length files-lists)
		  environment-to-load)))))
	  (the-environment))
	system-global-syntax-table)
       channel
       #f)
      (for-each
       (lambda (files)
	 (with-working-directory-pathname (car files)
	   (lambda ()
	     (channel-fasdump (map (lambda (file)
				     (cons (->namestring file)
					   (fasload (->pathname file))))
				   (cdr files))
			      channel
			      #f))))
       files-lists))))

(define (with-binary-output-file file action)
  (with-binary-file-channel file action
    open-binary-output-file
    output-port/channel
    'WITH-BINARY-OUTPUT-FILE))

(define channel-fasdump
  (make-primitive-procedure 'PRIMITIVE-FASDUMP 3))

(if (not (environment-bound? system-global-environment 'PACK-BINARIES))
    (environment-link-name system-global-environment
			   (the-environment)
			   'PACK-BINARIES))