#| -*-Scheme-*-

$Id: io.scm,v 14.79 2004/02/16 05:36:50 cph Exp $

Copyright 1986,1987,1988,1990,1991,1993 Massachusetts Institute of Technology
Copyright 1994,1995,1998,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2002,2003,2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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
  (initialize-select-registry!))

(define-structure (channel (constructor %make-channel))
  ;; This structure serves two purposes.  First, because a descriptor
  ;; is a non-pointer, it is necessary to store it in an allocated
  ;; object in order to determine when all references to it have been
  ;; dropped.  Second, the structure provides a type predicate.
  descriptor
  (type #f read-only #t)
  port)

(define (make-channel d)
  (open-channel (lambda (p) (system-pair-set-cdr! p d) #t)))

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
  (eq? 'FILE (channel-type channel)))

(define-integrable (channel-type=directory? channel)
  (eq? 'DIRECTORY (channel-type channel)))

(define (channel-type=terminal? channel)
  (let ((type (channel-type channel)))
    (or (eq? 'TERMINAL type)
	(eq? 'UNIX-PTY-MASTER type)
	(eq? 'OS/2-CONSOLE type))))

(define (channel-close channel)
  (without-interrupts
   (lambda ()
     (if (channel-open? channel)
	 (remove-from-gc-finalizer! open-channels channel)))))

(define-integrable (channel-open? channel)
  (channel-descriptor channel))

(define-integrable (channel-closed? channel)
  (not (channel-descriptor channel)))

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
  (let ((do-read
	 (lambda ()
	   ((ucode-primitive channel-read 4)
	    (channel-descriptor channel)
	    (if (external-string? buffer)
		(external-string-descriptor buffer)
		buffer)
	    start
	    end))))
    (declare (integrate-operator do-read))
    (if (and have-select? (not (channel-type=file? channel)))
	(with-thread-events-blocked
	  (lambda ()
	    (let ((do-test
		   (lambda (k)
		     (let ((result (test-for-io-on-channel channel 'READ)))
		       (case result
			 ((READ HANGUP ERROR) (do-read))
			 ((PROCESS-STATUS-CHANGE)
			  (handle-subprocess-status-change)
			  (if (channel-closed? channel) 0 (k)))
			 (else (k)))))))
	      (if (channel-blocking? channel)
		  (let loop () (do-test loop))
		  (do-test (lambda () #f))))))
	(do-read))))

(define (channel-write channel buffer start end)
  (let ((do-write
	 (lambda ()
	   ((ucode-primitive channel-write 4)
	    (channel-descriptor channel)
	    (if (external-string? buffer)
		(external-string-descriptor buffer)
		buffer)
	    start
	    end))))
    (declare (integrate-operator do-write))
    (if (and have-select? (not (channel-type=file? channel)))
	(with-thread-events-blocked
	  (lambda ()
	    (let ((do-test
		   (lambda (k)
		     (let ((result (test-for-io-on-channel channel 'WRITE)))
		       (case result
			 ((WRITE HANGUP ERROR) (do-write))
			 ((PROCESS-STATUS-CHANGE)
			  (handle-subprocess-status-change)
			  (if (channel-closed? channel) 0 (k)))
			 (else (k)))))))
	      (if (channel-blocking? channel)
		  (let loop () (do-test loop))
		  (do-test (lambda () #f))))))
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
  (let ((bytes (make-string 1)))
    (vector-8b-set! bytes 0 byte)
    (channel-write-block channel bytes 0 1)))

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
  (without-interrupts
   (lambda ()
     (let ((descriptors ((ucode-primitive channel-table 0))))
       (and descriptors
	    (vector-map (lambda (descriptor)
			  (or (descriptor->channel descriptor)
			      (make-channel descriptor)))
			descriptors))))))

;;;; File Primitives

(define (file-open primitive filename)
  (let ((channel (open-channel (lambda (p) (primitive filename p)))))
    (if (or (channel-type=directory? channel)
	    (channel-type=unknown? channel))
	(begin
	  (channel-close channel)
	  (error:bad-range-argument filename primitive)))
    channel))

(define (file-open-input-channel filename)
  (file-open (ucode-primitive new-file-open-input-channel 2) filename))

(define (file-open-output-channel filename)
  (file-open (ucode-primitive new-file-open-output-channel 2) filename))

(define (file-open-io-channel filename)
  (file-open (ucode-primitive new-file-open-io-channel 2) filename))

(define (file-open-append-channel filename)
  (file-open (ucode-primitive new-file-open-append-channel 2) filename))

(define (channel-file-length channel)
  ((ucode-primitive file-length-new 1) (channel-descriptor channel)))

(define (channel-file-position channel)
  ((ucode-primitive file-position 1) (channel-descriptor channel)))

(define (channel-file-set-position channel position)
  ((ucode-primitive file-set-position 2) (channel-descriptor channel)
					 position))

(define (make-pipe)
  (without-interrupts
   (lambda ()
     (let ((pipe ((ucode-primitive make-pipe 0))))
       (values (make-channel (car pipe))
	       (make-channel (cdr pipe)))))))

;;;; Terminal Primitives

(define (tty-input-channel)
  (without-interrupts
   (lambda ()
     (make-channel ((ucode-primitive tty-input-channel 0))))))

(define (tty-output-channel)
  (without-interrupts
   (lambda ()
     (make-channel ((ucode-primitive tty-output-channel 0))))))

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
  (without-interrupts
   (lambda ()
     (let ((result ((ucode-primitive open-pty-master 0))))
       (values (make-channel (vector-ref result 0))
	       (vector-ref result 1)
	       (vector-ref result 2))))))

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

(define (directory-channel-open name)
  (without-interrupts
   (lambda ()
     (add-to-gc-finalizer! open-directories
			   (make-directory-channel
			    ((ucode-primitive new-directory-open 1) name))))))

(define (directory-channel-close channel)
  (remove-from-gc-finalizer! open-directories channel))

(define (directory-channel-read channel)
  ((ucode-primitive new-directory-read 1)
   (directory-channel/descriptor channel)))

(define (directory-channel-read-matching channel prefix)
  ((ucode-primitive new-directory-read-matching 2)
   (directory-channel/descriptor channel)
   prefix))

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
  (without-interrupts
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

(define (test-for-io-on-channel channel mode)
  (test-for-io-on-descriptor (channel-descriptor-for-select channel)
			     (channel-blocking? channel)
			     mode))

(define (channel-has-input? channel)
  (let ((descriptor (channel-descriptor-for-select channel)))
    (let loop ()
      (let ((mode (test-select-descriptor descriptor #f 'READ)))
	(if (pair? mode)
	    (or (eq? (car mode) 'READ)
		(eq? (car mode) 'READ/WRITE))
	    (begin
	      (if (eq? mode 'PROCESS-STATUS-CHANGE)
		  (handle-subprocess-status-change))
	      (loop)))))))

(define-integrable (channel-descriptor-for-select channel)
  ((ucode-primitive channel-descriptor 1) (channel-descriptor channel)))

(define (test-for-io-on-descriptor descriptor block? mode)
  (or (let ((rmode (test-select-descriptor descriptor #f mode)))
	(if (pair? rmode)
	    (simplify-select-registry-mode rmode)
	    rmode))
      (and block?
	   (block-on-io-descriptor descriptor mode))))

(define (test-select-descriptor descriptor block? mode)
  (let ((result
	 ((ucode-primitive test-select-descriptor 3)
	  descriptor
	  block?
	  (encode-select-registry-mode mode))))
    (cond ((>= result 0) (decode-select-registry-mode result))
	  ((= result -1) 'INTERRUPT)
	  ((= result -2)
	   (subprocess-global-status-tick)
	   'PROCESS-STATUS-CHANGE)
	  (else
	   (error "Illegal result from TEST-SELECT-DESCRIPTOR:" result)))))

(define (encode-select-registry-mode mode)
  (case mode
    ((READ) 1)
    ((WRITE) 2)
    ((READ/WRITE) 3)
    (else (error:bad-range-argument mode 'ENCODE-SELECT-REGISTRY-MODE))))

(define (decode-select-registry-mode mode)
  (cons (if (select-registry-mode-read? mode)
	    (if (select-registry-mode-write? mode) 'READ/WRITE 'READ)
	    (if (select-registry-mode-write? mode) 'WRITE #f))
	(let ((tail
	       (if (select-registry-mode-hangup? mode)
		   (list 'HANGUP)
		   '())))
	  (if (select-registry-mode-error? mode)
	      (cons 'ERROR tail)
	      tail))))

(define (simplify-select-registry-mode mode)
  (cond ((memq 'HANGUP (cdr mode)) 'HANGUP)
	((memq 'ERROR (cdr mode)) 'ERROR)
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
		  ((= -1 result) 'INTERRUPT)
		  ((= -2 result)
		   (subprocess-global-status-tick)
		   'PROCESS-STATUS-CHANGE)
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