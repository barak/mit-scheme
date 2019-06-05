#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Input/Output Utilities
;;; package: (runtime primitive-io)

(declare (usual-integrations))

(define open-channels)
(define open-directories)

(define (initialize-package!)
  (set! open-channels
	(make-gc-finalizer (ucode-primitive channel-close 1)
			   channel?
			   channel-descriptor
			   set-channel-descriptor!))
  (set! open-directories
	(make-gc-finalizer (ucode-primitive new-directory-close 1)
			   directory-channel?
			   directory-channel/descriptor
			   set-directory-channel/descriptor!))
  (initialize-select-registry!)
  (reset-dld-handles!)
  (add-event-receiver! event:after-restore reset-dld-handles!)
  unspecific)

(define-structure (channel (constructor %make-channel))
  ;; This structure serves two purposes.  First, because a descriptor
  ;; is a non-pointer, it is necessary to store it in an allocated
  ;; object in order to determine when all references to it have been
  ;; dropped.  Second, the structure provides a type predicate.
  descriptor
  (type #f read-only #t)
  port)

(define-guarantee channel "I/O channel")

(define (make-channel d)
  (open-channel (lambda (p) (system-pair-set-cdr! p d))))

(define (open-channel procedure)
  (make-gc-finalized-object open-channels procedure
    (lambda (d)
      (%make-channel d (descriptor-type-name d) #f))))

(define (descriptor->channel descriptor)
  (search-gc-finalizer open-channels
    (lambda (channel)
      (fix:= descriptor (channel-descriptor channel)))))

(define (descriptor-type-name descriptor)
  (let ((name ((ucode-primitive channel-type-name 1) descriptor)))
    (and name
	 (intern name))))

(define-integrable (channel-type=unknown? channel)
  (false? (channel-type channel)))

(define-integrable (channel-type=file? channel)
  (eq? 'file (channel-type channel)))

(define-integrable (channel-type=directory? channel)
  (eq? 'directory (channel-type channel)))

(define (channel-type=terminal? channel)
  (let ((type (channel-type channel)))
    (or (eq? 'terminal type)
	(eq? 'unix-pty-master type))))

(define (channel-close channel)
  (with-gc-finalizer-lock open-channels
    (lambda ()
      (if (channel-open? channel)
	  (begin
	    (%deregister-io-descriptor (channel-descriptor-for-select channel))
	    (remove-from-locked-gc-finalizer! open-channels channel))))))

(define-integrable (channel-open? channel)
  (if (channel-descriptor channel) #t #f))

(define-integrable (channel-closed? channel)
  (if (channel-descriptor channel) #f #t))

(define (close-all-open-files)
  (close-all-open-channels channel-type=file?))

(define (close-all-open-channels #!optional filter)
  (let ((filter (if (default-object? filter) #f filter)))
    (for-each (lambda (channel)
		(if (or (not filter) (filter channel))
		    (let ((port (channel-port channel)))
		      (if port
			  (close-port port)
			  (channel-close channel)))))
	      (all-open-channels))
    (if (not filter)
	(remove-all-from-gc-finalizer! open-channels))))

(define (all-open-channels)
  (gc-finalizer-elements open-channels))

;;;; Channel Primitives

(define (port-error-test operator operands)
  ;; If the performance of this `memq' is a problem, change this to
  ;; use a string hash table based on the primitive name.
  (and (memq operator channel-primitives)
       (not (null? operands))
       (let ((descriptor (car operands)))
	 (and (exact-nonnegative-integer? descriptor)
	      (let ((channel (descriptor->channel descriptor)))
		(and channel
		     (channel-port channel)))))))

(define channel-primitives
  (list (ucode-primitive channel-blocking 1)
	(ucode-primitive channel-blocking? 1)
	(ucode-primitive channel-close 1)
	(ucode-primitive channel-descriptor 1)
	(ucode-primitive channel-nonblocking 1)
	(ucode-primitive channel-read 4)
	(ucode-primitive channel-synchronize 1)
	(ucode-primitive channel-write 4)
	(ucode-primitive file-length-new 1)
	(ucode-primitive file-position 1)
	(ucode-primitive file-set-position 2)
	(ucode-primitive pty-master-continue 1)
	(ucode-primitive pty-master-interrupt 1)
	(ucode-primitive pty-master-kill 1)
	(ucode-primitive pty-master-quit 1)
	(ucode-primitive pty-master-send-signal 2)
	(ucode-primitive pty-master-stop 1)
	(ucode-primitive terminal-buffered 1)
	(ucode-primitive terminal-buffered? 1)
	(ucode-primitive terminal-cooked-output 1)
	(ucode-primitive terminal-cooked-output? 1)
	(ucode-primitive terminal-drain-output 1)
	(ucode-primitive terminal-flush-input 1)
	(ucode-primitive terminal-flush-output 1)
	(ucode-primitive terminal-get-ispeed 1)
	(ucode-primitive terminal-get-ospeed 1)
	(ucode-primitive terminal-set-ispeed 2)
	(ucode-primitive terminal-set-ospeed 2)
	(ucode-primitive terminal-get-state 1)
	(ucode-primitive terminal-nonbuffered 1)
	(ucode-primitive terminal-raw-output 1)
	(ucode-primitive terminal-set-state 2)))

(define (channel-read channel buffer start end)
  (let loop ()
    (let ((n (%channel-read channel buffer start end)))
      (if (eq? n #t)
	  (if (channel-blocking? channel)
	      (loop)
	      #f)
	  n))))

(define (%channel-read channel buffer start end)
  (let ((do-read
	 (lambda ()
	   ((ucode-primitive channel-read 4)
	    (channel-descriptor channel)
	    buffer
	    start
	    end))))
    (declare (integrate-operator do-read))
    (if (and have-select? (not (channel-type=file? channel)))
	(let ((result (test-for-io-on-channel channel 'read
					      (channel-blocking? channel))))
	  (case result
	    ((read hangup error) (do-read))
	    ((#f) #f)
	    ((process-status-change interrupt) #t)
	    (else (error "Unexpected test-for-io-on-channel value:" result))))
	(do-read))))

(define (channel-write channel buffer start end)
  (let loop ()
    (let ((n (%channel-write channel buffer start end)))
      (if (eq? n #t)
	  (if (channel-blocking? channel)
	      (loop)
	      #f)
	  n))))

(define (%channel-write channel buffer start end)
  (let ((do-write
	 (lambda ()
	   ((ucode-primitive channel-write 4)
	    (channel-descriptor channel)
	    buffer
	    start
	    end))))
    (declare (integrate-operator do-write))
    (if (and have-select? (not (channel-type=file? channel)))
	(let ((result (test-for-io-on-channel channel 'write
					      (channel-blocking? channel))))
	  (case result
	    ((write hangup error) (do-write))
	    ((#f) 0)
	    ((process-status-change interrupt) #t)
	    (else (error "Unexpected test-for-io-on-channel value:" result))))
	(do-write))))

(define (channel-read-block channel buffer start end)
  (let loop ()
    (or (channel-read channel buffer start end)
	(loop))))

(define (channel-write-block channel buffer start end)
  (let loop ((start start) (n-left (- end start)))
    (let ((n (channel-write channel buffer start end)))
      (cond ((not n) (loop start n-left))
	    ((< n n-left) (loop (+ start n) (- n-left n)))))))

(define (channel-write-byte-block channel byte)
  (channel-write-block channel (bytevector byte) 0 1))

(define (channel-blocking? channel)
  ((ucode-primitive channel-blocking? 1) (channel-descriptor channel)))

(define (channel-blocking channel)
  ((ucode-primitive channel-blocking 1) (channel-descriptor channel)))

(define (channel-nonblocking channel)
  ((ucode-primitive channel-nonblocking 1) (channel-descriptor channel)))

(define (with-channel-blocking channel blocking? thunk)
  (if (channel-open? channel)
      (let ((blocking-outside?))
	(dynamic-wind
	 (lambda ()
	   (if (channel-open? channel)
	       (begin
		 (set! blocking-outside? (channel-blocking? channel))
		 (if blocking?
		     (channel-blocking channel)
		     (channel-nonblocking channel)))))
	 thunk
	 (lambda ()
	   (if (channel-open? channel)
	       (begin
		 (set! blocking? (channel-blocking? channel))
		 (if blocking-outside?
		     (channel-blocking channel)
		     (channel-nonblocking channel)))))))
      (thunk)))

(define (channel-table)
  (with-gc-finalizer-lock open-channels
    (lambda ()
      (let ((descriptors ((ucode-primitive channel-table 0))))
	(and descriptors
	     (vector-map descriptor->channel descriptors))))))

(define (channel-synchronize channel)
  ((ucode-primitive channel-synchronize 1) (channel-descriptor channel)))

;;;; File Primitives

(define (file-open primitive operator filename)
  (let ((channel
	 (open-channel
	  (lambda (p)
	    (primitive (string-for-primitive filename) p)))))
    (if (or (channel-type=directory? channel)
	    (channel-type=unknown? channel))
	(let ((reason
	       (if (channel-type=directory? channel)
		   "Is a directory"
		   "Unknown file type")))
	  (channel-close channel)
	  (file-open primitive
		     operator
		     (error:file-operation 0 "open" "file" reason
					   operator (list filename))))
	channel)))

(define (file-open-input-channel filename)
  (file-open (ucode-primitive new-file-open-input-channel 2)
	     file-open-input-channel
	     filename))

(define (file-open-output-channel filename)
  (file-open (ucode-primitive new-file-open-output-channel 2)
	     file-open-output-channel
	     filename))

(define (file-open-exclusive-output-channel filename)
  (file-open (ucode-primitive new-file-open-exclusive-output-channel 2)
	     file-open-exclusive-output-channel
	     filename))

(define (file-open-io-channel filename)
  (file-open (ucode-primitive new-file-open-io-channel 2)
	     file-open-io-channel
	     filename))

(define (file-open-append-channel filename)
  (file-open (ucode-primitive new-file-open-append-channel 2)
	     file-open-append-channel
	     filename))

(define (channel-file-length channel)
  ((ucode-primitive file-length-new 1) (channel-descriptor channel)))

(define (channel-file-position channel)
  ((ucode-primitive file-position 1) (channel-descriptor channel)))

(define (channel-file-set-position channel position)
  ((ucode-primitive file-set-position 2) (channel-descriptor channel)
					 position))

(define (channel-file-truncate channel length)
  ((ucode-primitive file-truncate 2) (channel-descriptor channel) length))

(define (make-pipe)
  (let* ((writer)
	 (reader
	  (open-channel
	   (lambda (reader-pair)
	     (set! writer
		   (open-channel
		    (lambda (writer-pair)
		      ((ucode-primitive new-make-pipe 2)
		       reader-pair
		       writer-pair))))))))
    (values reader writer)))

;;;; Terminal Primitives

(define (tty-input-channel)
  (make-channel ((ucode-primitive tty-input-channel 0))))

(define (tty-output-channel)
  (make-channel ((ucode-primitive tty-output-channel 0))))

(define (tty-error-channel)
  (make-channel ((ucode-primitive tty-error-channel 0))))

(define (terminal-get-state channel)
  ((ucode-primitive terminal-get-state 1) (channel-descriptor channel)))

(define (terminal-set-state channel state)
  ((ucode-primitive terminal-set-state 2) (channel-descriptor channel) state))

(define (terminal-cooked-input? channel)
  ((ucode-primitive terminal-buffered? 1) (channel-descriptor channel)))

(define (terminal-cooked-input channel)
  ((ucode-primitive terminal-buffered 1) (channel-descriptor channel)))

(define (terminal-raw-input channel)
  ((ucode-primitive terminal-nonbuffered 1) (channel-descriptor channel)))

(define (terminal-cooked-output? channel)
  ((ucode-primitive terminal-cooked-output? 1) (channel-descriptor channel)))

(define (terminal-cooked-output channel)
  ((ucode-primitive terminal-cooked-output 1) (channel-descriptor channel)))

(define (terminal-raw-output channel)
  ((ucode-primitive terminal-raw-output 1) (channel-descriptor channel)))

(define (terminal-flush-input channel)
  ((ucode-primitive terminal-flush-input 1) (channel-descriptor channel)))

(define (terminal-flush-output channel)
  ((ucode-primitive terminal-flush-output 1) (channel-descriptor channel)))

(define (terminal-drain-output channel)
  ((ucode-primitive terminal-drain-output 1) (channel-descriptor channel)))

(define (terminal-input-baud-rate channel)
  ((ucode-primitive baud-index->rate 1)
   ((ucode-primitive terminal-get-ispeed 1) (channel-descriptor channel))))

(define (terminal-output-baud-rate channel)
  ((ucode-primitive baud-index->rate 1)
   ((ucode-primitive terminal-get-ospeed 1) (channel-descriptor channel))))

(define (set-terminal-input-baud-rate! channel baud)
  ((ucode-primitive terminal-set-ispeed 2)
   (channel-descriptor channel)
   ((ucode-primitive baud-rate->index 1) baud)))

(define (set-terminal-output-baud-rate! channel baud)
  ((ucode-primitive terminal-set-ospeed 2)
   (channel-descriptor channel)
   ((ucode-primitive baud-rate->index 1) baud)))

;;;; PTY Master Primitives

(define (open-pty-master)
  (without-interruption
   (lambda ()
     (let ((result ((ucode-primitive open-pty-master 0))))
       (values (make-channel (vector-ref result 0))
	       (string-from-primitive (vector-ref result 1))
	       (string-from-primitive (vector-ref result 2)))))))

(define (pty-master-send-signal channel signal)
  ((ucode-primitive pty-master-send-signal 2) (channel-descriptor channel)
					      signal))

(define (pty-master-kill channel)
  ((ucode-primitive pty-master-kill 1) (channel-descriptor channel)))

(define (pty-master-stop channel)
  ((ucode-primitive pty-master-stop 1) (channel-descriptor channel)))

(define (pty-master-continue channel)
  ((ucode-primitive pty-master-continue 1) (channel-descriptor channel)))

(define (pty-master-interrupt channel)
  ((ucode-primitive pty-master-interrupt 1) (channel-descriptor channel)))

(define (pty-master-quit channel)
  ((ucode-primitive pty-master-quit 1) (channel-descriptor channel)))

(define (pty-master-hangup channel)
  ((ucode-primitive pty-master-hangup 1) (channel-descriptor channel)))

;;;; Directory Primitives

(define-structure (directory-channel (conc-name directory-channel/))
  descriptor)

(define-guarantee directory-channel "directory channel")

(define (directory-channel-open name)
  (without-interruption
   (lambda ()
     (add-to-gc-finalizer! open-directories
			   (make-directory-channel
			    ((ucode-primitive new-directory-open 1)
			     (string-for-primitive name)))))))

(define (directory-channel-close channel)
  (remove-from-gc-finalizer! open-directories channel))

(define (directory-channel-read channel)
  (string-from-primitive
   ((ucode-primitive new-directory-read 1)
    (directory-channel/descriptor channel))))

(define (directory-channel-read-matching channel prefix)
  (string-from-primitive
   ((ucode-primitive new-directory-read-matching 2)
    (directory-channel/descriptor channel)
    (string-for-primitive prefix))))

;;;; Select registry

(define have-select?)
(define select-registry-finalizer)
(define select-registry-result-vectors)

(define (initialize-select-registry!)
  (set! have-select? ((ucode-primitive have-select? 0)))
  (set! select-registry-finalizer
	(make-gc-finalizer (ucode-primitive deallocate-select-registry 1)
			   select-registry?
			   select-registry-handle
			   set-select-registry-handle!))
  (let ((reset-rv!
	 (lambda ()
	   (set! select-registry-result-vectors '())
	   unspecific)))
    (reset-rv!)
    (add-event-receiver! event:after-restart reset-rv!))
  (add-event-receiver! event:after-restore
    (lambda ()
      (set! have-select? ((ucode-primitive have-select? 0)))
      unspecific)))

(define-structure (select-registry
		   (constructor %make-select-registry (handle)))
  handle
  (length #f))

(define (make-select-registry)
  (without-interruption
   (lambda ()
     (add-to-gc-finalizer! select-registry-finalizer
			   (%make-select-registry
			    ((ucode-primitive allocate-select-registry 0)))))))

(define (add-to-select-registry! registry descriptor mode)
  ((ucode-primitive add-to-select-registry 3)
   (select-registry-handle registry)
   descriptor
   (encode-select-registry-mode mode))
  (set-select-registry-length! registry #f))

(define (remove-from-select-registry! registry descriptor mode)
  ((ucode-primitive remove-from-select-registry 3)
   (select-registry-handle registry)
   descriptor
   (encode-select-registry-mode mode))
  (set-select-registry-length! registry #f))

(define (test-for-io-on-channel channel mode #!optional block?)
  (test-for-io-on-descriptor (channel-descriptor-for-select channel)
			     (if (default-object? block?)
				 (channel-blocking? channel)
				 block?)
			     mode))

(define (channel-has-input? channel)
  (let loop ()
    (let ((mode (test-select-descriptor (channel-descriptor-for-select channel)
					'read)))
      (if (pair? mode)
	  (or (eq? (car mode) 'read)
	      (eq? (car mode) 'read/write))
	  (loop)))))

(define-integrable (channel-descriptor-for-select channel)
  ((ucode-primitive channel-descriptor 1) (channel-descriptor channel)))

(define (test-for-io-on-descriptor descriptor block? mode)
  (or (let ((rmode (test-select-descriptor descriptor mode)))
	(if (pair? rmode)
	    (simplify-select-registry-mode rmode)
	    rmode))
      (and block?
	   (block-on-io-descriptor descriptor mode))))

(define (test-select-descriptor descriptor mode)
  (let ((result
	 ((ucode-primitive test-select-descriptor 3)
	  descriptor
	  #f
	  (encode-select-registry-mode mode))))
    (cond ((>= result 0) (decode-select-registry-mode result))
	  ((= result -1) 'interrupt)
	  ((= result -2)
	   (handle-subprocess-status-change)
	   'process-status-change)
	  (else
	   (error "Illegal result from TEST-SELECT-DESCRIPTOR:" result)))))

(define (encode-select-registry-mode mode)
  (case mode
    ((read) 1)
    ((write) 2)
    ((read/write) 3)
    (else (error:bad-range-argument mode 'encode-select-registry-mode))))

(define (decode-select-registry-mode mode)
  (cons (if (select-registry-mode-read? mode)
	    (if (select-registry-mode-write? mode) 'read/write 'read)
	    (if (select-registry-mode-write? mode) 'write #f))
	(let ((tail
	       (if (select-registry-mode-hangup? mode)
		   (list 'hangup)
		   '())))
	  (if (select-registry-mode-error? mode)
	      (cons 'error tail)
	      tail))))

(define (simplify-select-registry-mode mode)
  (cond ((memq 'hangup (cdr mode)) 'hangup)
	((memq 'error (cdr mode)) 'error)
	(else (car mode))))

(define-integrable (select-registry-mode-read? mode)
  (fix:= 1 (fix:and 1 mode)))

(define-integrable (select-registry-mode-write? mode)
  (fix:= 2 (fix:and 2 mode)))

(define-integrable (select-registry-mode-error? mode)
  (fix:= 4 (fix:and 4 mode)))

(define-integrable (select-registry-mode-hangup? mode)
  (fix:= 8 (fix:and 8 mode)))

(define (test-select-registry registry block?)
  (receive (vfd vmode) (allocate-select-registry-result-vectors registry)
    (let ((result
	   ((ucode-primitive test-select-registry 4)
	    (select-registry-handle registry)
	    block?
	    vfd
	    vmode)))
      (if (> result 0)
	  (begin
	    (do ((i 0 (fix:+ i 1)))
		((fix:= i result))
	      (vector-set!
	       vmode i
	       (simplify-select-registry-mode
		(decode-select-registry-mode (vector-ref vmode i)))))
	    (vector result vfd vmode))
	  (begin
	    (deallocate-select-registry-result-vectors vfd vmode)
	    (cond ((= 0 result) #f)
		  ((= -1 result) 'interrupt)
		  ((= -2 result) 'process-status-change)
		  (else
		   (error "Illegal result from TEST-SELECT-REGISTRY:"
			  result))))))))

(define (allocate-select-registry-result-vectors registry)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((n
	   (or (select-registry-length registry)
	       (let ((rl
		      ((ucode-primitive select-registry-length 1)
		       (select-registry-handle registry))))
		 (set-select-registry-length! registry rl)
		 rl))))
      (let loop ((rv select-registry-result-vectors))
	(if (pair? rv)
	    (let ((vfd (caar rv))
		  (vmode (cdar rv)))
	      (if (and vfd (fix:<= n (vector-length vfd)))
		  (begin
		    (set-car! (car rv) #f)
		    (set-cdr! (car rv) #f)
		    (set-interrupt-enables! interrupt-mask)
		    (values vfd vmode))
		  (loop (cdr rv))))
	    (let loop ((m 16))
	      (if (fix:< n m)
		  (begin
		    (set-interrupt-enables! interrupt-mask)
		    (values (make-vector m) (make-vector m)))
		  (loop (fix:* m 2)))))))))

(define (deallocate-select-registry-result-vectors vfd vmode)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let loop ((rv select-registry-result-vectors))
      (if (pair? rv)
	  (if (caar rv)
	      (loop (cdr rv))
	      (begin
		(set-car! (car rv) vfd)
		(set-cdr! (car rv) vmode)))
	  (set! select-registry-result-vectors
		(cons (cons vfd vmode) select-registry-result-vectors))))
    (set-interrupt-enables! interrupt-mask)))

;;;; Interface to dynamic loader

(define-structure dld-handle
  (pathname #f read-only #t)
  address)

(define-guarantee dld-handle "dynamic-loader handle")

(define (dld-handle-valid? handle)
  (guarantee-dld-handle handle 'dld-handle-valid?)
  (if (dld-handle-address handle) #t #f))

(define (guarantee-valid-dld-handle object #!optional caller)
  (guarantee-dld-handle object caller)
  (if (not (dld-handle-address object))
      (error:bad-range-argument object
				(if (default-object? caller) #f caller))))

(define (dld-get-scheme-handle)
  (dld-load-file #f))

(define (dld-load-file pathname)
  (let ((p (weak-cons #f #f)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda ()
       ((ucode-primitive dld-load-file 2)
	(and pathname (string-for-primitive (->namestring pathname)))
	p)
       (let ((handle (make-dld-handle pathname (weak-cdr p))))
	 (with-thread-mutex-lock dld-handles-mutex
	  (lambda ()
	    (set! dld-handles (cons handle dld-handles))
	    (weak-set-car! p #t)
	    unspecific))
	 handle))
     (lambda ()
       (if (and (not (weak-pair/car? p)) (weak-cdr p))
	   (begin
	     ((ucode-primitive dld-unload-file 1) (weak-cdr p))
	     (weak-set-cdr! p #f)))))))

(define dld-handles)
(define dld-handles-mutex)

(define (reset-dld-handles!)
  (set! dld-handles '())
  (set! dld-handles-mutex (make-thread-mutex))
  unspecific)

(define (dld-unload-file handle)
  (guarantee-dld-handle handle 'dld-unload-file)
  (with-thread-mutex-lock dld-handles-mutex
   (lambda ()
     (%dld-unload-file handle)
     (set! dld-handles (delq! handle dld-handles))
     unspecific)))

(define (%dld-unload-file handle)
  (let ((address (dld-handle-address handle)))
    (if address
	(begin
	  ((ucode-primitive dld-unload-file 1) address)
	  (set-dld-handle-address! handle #f)))))

(define (dld-lookup-symbol handle name)
  (guarantee-dld-handle handle 'dld-lookup-symbol)
  (guarantee string? name 'dld-lookup-symbol)
  ((ucode-primitive dld-lookup-symbol 2)
   (dld-handle-address handle)
   (string-for-primitive name)))

(define (dld-loaded-file? pathname)
  (find-dld-handle
   (lambda (handle)
     (let ((pathname* (dld-handle-pathname handle)))
       (and pathname*
	    (pathname=? pathname* pathname))))))

(define (find-dld-handle predicate)
  (with-thread-mutex-lock dld-handles-mutex
    (lambda ()
      (find predicate dld-handles))))

(define (all-dld-handles)
  (with-thread-mutex-lock dld-handles-mutex
    (lambda ()
      (list-copy dld-handles))))