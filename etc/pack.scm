#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/etc/pack.scm,v 1.4 1992/05/22 23:22:41 jinx Exp $

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

;; This code has interrupt windows because it does not use the
;; channel stuff from the runtime system.
;; In addition, the channel will not be closed if it is lost and gc'd.

(define open-binary-input-file
  (let ((open-file
         (make-primitive-procedure 'file-open-input-channel 1)))
    (lambda (file-name)
      (open-file (->namestring (->truename (->pathname file-name)))))))

(define close-binary-input-channel
  (let ((channel-close (make-primitive-procedure 'channel-close 1)))
    (lambda (channel)
      (channel-close channel))))

(define open-binary-output-file
  (let ((open-file
         (make-primitive-procedure 'file-open-output-channel 1)))
    (lambda (file-name)
      (open-file (->namestring (->pathname file-name))))))

(define close-binary-output-channel
  (let ((channel-close (make-primitive-procedure 'channel-close 1)))
    (lambda (channel)
      (channel-close channel))))

(define (with-binary-file file action open close name)
  (let ((channel false))
    (dynamic-wind
     (lambda ()
       (if channel
           (error "cannot re-enter with-binary-file" name)))
     (lambda ()
       (set! channel (open file))
       (action channel))
     (lambda ()
       (if (and channel
                (not (eq? channel true)))
           (begin
             (close channel)
             (set! channel true)))))))

(define (with-binary-input-file file action)
  (with-binary-file file action
    open-binary-input-file
    close-binary-input-channel
    action))

(define (with-binary-output-file file action)
  (with-binary-file file action
    open-binary-output-file
    close-binary-output-channel
    action))

(define channel-fasdump
  (make-primitive-procedure 'primitive-fasdump 3))

(define channel-fasload
  (make-primitive-procedure 'binary-fasload 1))

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
                               (unpack-binaries-and-load 
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

(define (unpack-binaries-and-load pathname fname strings environment)
  (define (find-filename fname alist)
    (define (compatible? path1 path2)
      (and (equal? (pathname-directory path1)
                   (pathname-directory path2))
           (equal? (pathname-name path1)
                   (pathname-name path2))
           (or (equal? (pathname-type path1) (pathname-type path2))
               (and (member (pathname-type path1) '(#f "bin" "com"))
                    (member (pathname-type path2) '(#f "bin" "com"))))))

    (let ((path (->pathname fname)))
      (let loop ((alist alist))
        (and (not (null? alist))
             (if (compatible? path (cadar alist))
                 (car alist)
                 (loop (cdr alist)))))))

  (define (process-bunch alist)
    (let* ((real-load load)
	   (new-load
	    (lambda (fname #!optional env syntax-table purify?)
	      (let ((env (if (default-object? env)
			     environment
			     env))
		    (st (if (default-object? syntax-table)
			    default-object
			    syntax-table))
		    (purify? (if (default-object? purify?)
				 default-object
				 purify?)))
		(let ((place (find-filename fname alist)))
		  (if (not place)
		      (real-load fname env st purify?)
		      (let ((scode (caddr place)))
			(if (not load/suppress-loading-message?)
			    (begin
			      (newline)
			      (display ";Pseudo-loading ")
			      (display (->namestring (->pathname fname)))
			      (display "...")))
			(if (and purify? (not (eq? purify? default-object)))
			    (purify (load/purification-root scode)))
			(extended-scode-eval scode env))))))))
      (fluid-let ((load new-load)
		  (flush-purification-queue! (lambda () 'done)))
        (new-load (caar alist))))
    (flush-purification-queue!))

  (with-binary-input-file (->truename pathname)
    (lambda (channel)
      ;; Dismiss header.
      (channel-fasload channel)
      (let ((process-next-bunch
	     (lambda ()
	       (process-bunch
		(map (lambda (pair)
		       (list (car pair)
			     (->pathname (car pair))
			     (cdr pair)))
		     (channel-fasload channel))))))

	(cond ((number? strings)
	       (do ((count strings (-1+ count)))
		   ((= count 1)
		    (process-next-bunch))
		 (process-next-bunch)))
	      ((eq? strings true)
	       (process-next-bunch))
	      (else
	       (do ((i (length strings) (-1+ i))
		    (strings strings (cdr strings))
		    (alist '()
			   (cons (list (car strings)
				       (->pathname (car strings))
				       (channel-fasload channel))
				 alist)))
		   ((zero? i)
		    (process-bunch (reverse! alist))))))))))

;;; Link to global

(let ((system-global-environment '())
      (this-environment (the-environment)))
  (if (not (environment-bound? system-global-environment
                               'pack-binaries))
      (environment-link-name system-global-environment this-environment
                             'pack-binaries))
  (if (not (environment-bound? system-global-environment
                               'unpack-binaries-and-load))
      (environment-link-name system-global-environment this-environment
                             'unpack-binaries-and-load)))