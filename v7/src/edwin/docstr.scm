#| -*-Scheme-*-

$Id: docstr.scm,v 1.2 1994/11/20 05:06:04 cph Exp $

Copyright (c) 1993-94 Massachusetts Institute of Technology

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
MIT in each case.

NOTE: Parts of this program (Edwin) were created by translation
from corresponding parts of GNU Emacs.  Users should be aware that
the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
of that license should have been included along with this file.
|#

;;;; Documentation Strings

(declare (usual-integrations))

(define *external-doc-strings?* true)
(define *external-doc-strings-file* false)
(define *doc-strings* false)
(define *doc-string-posn* 0)
(define *doc-string-channel* false)
(define *doc-string-buffer* false)

(define (doc-string->posn name str)
  (if (not *external-doc-strings?*)
      str
      (let ((nlen (string-length name))
	    (dslen (string-length str))
	    (slen (if (not *doc-strings*)
		      0
		      (string-length *doc-strings*)))
	    (posn *doc-string-posn*))
	(let* ((next (fix:+ posn nlen))
	       (end (fix:+ next (fix:+ dslen 6))))
	  (if (> end slen)
	      (let ((new (string-allocate
			  (max end
			       (if (fix:zero? slen)
				   4096
				   (fix:+ slen (fix:quotient slen 2)))))))
		(if *doc-strings*
		    (substring-move-right! *doc-strings* 0 posn new 0))
		(set! *doc-strings* new)))
	  (let ((doc-strings *doc-strings*))
	    (vector-8b-set! doc-strings posn (fix:remainder dslen 256))
	    (vector-8b-set! doc-strings
			    (fix:+ posn 1)
			    (fix:quotient dslen 256))
	    (string-set! doc-strings (fix:+ posn 2) #\Newline)
	    (substring-move-right! name 0 nlen doc-strings (fix:+ posn 3))
	    (string-set! doc-strings (fix:+ next 3) #\Newline)
	    (substring-move-right! str 0 dslen doc-strings (fix:+ next 4))
	    (string-set! doc-strings (fix:- end 2) #\Newline)
	    (string-set! doc-strings (fix:- end 1) #\Newline)
	    (set! *doc-string-posn* end)
	    posn)))))

(define-integrable doc-string-buffer-length 512)

(define (->doc-string name posn)
  (define (out-of-range)
    (editor-error "->doc-string: Out of range argument" posn))

  (define (fill-buffer channel buffer posn blen)
    (let fill-loop ((posn posn))
      (if (fix:< posn blen)
	  (let ((n (channel-read-block channel buffer posn blen)))
	    (fill-loop (fix:+ posn n))))))

  (define (verify-and-extract buffer nlen dslen nposn)
    (let ((nend (fix:+ nposn nlen)))
      (if (not (string=? (substring buffer nposn nend) name))
	  (editor-error "->doc-string: Inconsistency" posn)
	  (let ((dstart (fix:+ nend 1)))
	    (substring buffer dstart (fix:+ dstart dslen))))))

  (cond ((string? posn)
	 posn)
	((not (fix:fixnum? posn))
	 (editor-error "->doc-string: Wrong type argument" posn))
	((fix:< posn 0)
	 (out-of-range))
	(*doc-strings*
	 (let ((slen (string-length *doc-strings*))
	       (nlen (string-length name)))
	   (if (fix:> (fix:+ posn 2) slen)
	       (out-of-range))
	   (let ((dslen
		  (fix:+ (vector-8b-ref *doc-strings* posn)
			 (fix:lsh (vector-8b-ref *doc-strings* (fix:+ posn 1))
				  8))))
	     (if (fix:> (fix:+ (fix:+ posn 6) (fix:+ nlen dslen)) slen)
		 (out-of-range)
		 (verify-and-extract *doc-strings* nlen dslen
				     (fix:+ posn 3))))))
	(else
	 (guarantee-doc-string-state)
	 (let* ((channel *doc-string-channel*)
		(buffer *doc-string-buffer*)
		(flen (channel-file-length channel))
		(nlen (string-length name))
		(delta (fix:- flen (fix:+ posn 2))))
	   (if (fix:< delta 0)
	       (out-of-range))
	   (channel-file-set-position channel posn)
	   (let ((blen (min doc-string-buffer-length delta)))
	     (fill-buffer channel buffer 0 blen)
	     (let* ((dslen (fix:+ (vector-8b-ref buffer 0)
				  (fix:lsh (vector-8b-ref buffer 1)
					   8)))
		    (end (fix:+ (fix:+ dslen nlen) 6)))
	       (cond ((not (fix:> end blen))
		      (verify-and-extract buffer nlen dslen 3))
		     ((fix:> (fix:+ end posn) flen)
		      (out-of-range))
		     (else
		      (let* ((rlen (fix:+ (fix:+ nlen dslen) 1))
			     (result (string-allocate rlen)))
			(substring-move-right! buffer 3 blen result 0)
			(fill-buffer channel result (fix:- blen 3) rlen)
			(verify-and-extract result nlen dslen 0))))))))))

(define (dump-doc-strings output #!optional permanent)
  (if (not *doc-strings*)
      (error "dump-doc-strings: No doc strings to dump!"))
  (set! *external-doc-strings-file*
	(if (or (default-object? permanent)
		(not permanent))
	    output
	    permanent))	
  (set-string-length! *doc-strings* *doc-string-posn*)
  (call-with-binary-output-file
   output
   (lambda (port)
     (output-port/write-string port *doc-strings*)))
  (set! *external-doc-strings?* false)
  (set! *doc-string-posn* 0)
  (set! *doc-strings* false)
  unspecific)

(define (guarantee-doc-string-state)
  (if (not *doc-string-buffer*)
      (set! *doc-string-buffer* (string-allocate doc-string-buffer-length)))
  (cond (*doc-string-channel*)
	((not *external-doc-strings-file*)
	 (editor-error
	  "guarantee-doc-string-channel: Undeclared doc-string file"))
	(else
	 (let ((doc-strings
		(if (or (pathname-absolute? *external-doc-strings-file*)
			(file-exists? *external-doc-strings-file*))
		    *external-doc-strings-file*
		    (merge-pathnames *external-doc-strings-file*
				     (edwin-etc-directory)))))
	   (if (not (file-exists? doc-strings))
	       (editor-error
		"guarantee-doc-string-channel: Non-existent doc-string file")
	       (begin
		 (set! *doc-string-channel*
		       (file-open-input-channel
			(->namestring doc-strings)))
		 unspecific))))))

(add-event-receiver! event:after-restart
		     (lambda ()
		       (set! *doc-string-channel* false)))