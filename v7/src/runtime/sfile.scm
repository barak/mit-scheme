#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/sfile.scm,v 14.8 1991/11/04 20:29:58 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Simple File Operations
;;; package: ()

(declare (usual-integrations))

(define (file-exists? filename)
  ((ucode-primitive file-exists? 1) (->namestring (merge-pathnames filename))))

(define (rename-file from to)
  ((ucode-primitive file-rename) (->namestring (merge-pathnames from))
				 (->namestring (merge-pathnames to))))

(define (delete-file filename)
  ((ucode-primitive file-remove) (->namestring (merge-pathnames filename))))

(define (copy-file from to)
  (let ((input-filename (->namestring (merge-pathnames from)))
	(output-filename (->namestring (merge-pathnames to))))
    (let ((input-channel false)
	  (output-channel false))
      (dynamic-wind
       (lambda ()
	 (set! input-channel (file-open-input-channel input-filename))
	 (set! output-channel
	       (begin
		 ((ucode-primitive file-remove-link 1) output-filename)
		 (file-open-output-channel output-filename)))
	 unspecific)
       (lambda ()
	 (let ((source-length (file-length input-channel))
	       (buffer-length 8192))
	   (if (zero? source-length)
	       0
	       (let* ((buffer (make-string buffer-length))
		      (transfer
		       (lambda (length)
			 (let ((n-read
				(channel-read-block input-channel
						    buffer
						    0
						    length)))
			   (if (positive? n-read)
			       (channel-write-block output-channel
						    buffer
						    0
						    n-read))
			   n-read))))
		 (let loop ((source-length source-length))
		   (if (< source-length buffer-length)
		       (transfer source-length)
		       (let ((n-read (transfer buffer-length)))
			 (if (= n-read buffer-length)
			     (+ (loop (- source-length buffer-length))
				buffer-length)
			     n-read))))))))
       (lambda ()
	 (if output-channel (channel-close output-channel))
	 (if input-channel (channel-close input-channel)))))))