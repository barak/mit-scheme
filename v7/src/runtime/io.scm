#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/io.scm,v 14.8 1990/08/16 20:09:57 cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Input/Output Utilities
;;; package: (runtime primitive-io)

(declare (usual-integrations))

(define open-channels-list)
(define traversing?)

(define (initialize-package!)
  (set! open-channels-list (list 'OPEN-CHANNELS-LIST))
  (set! traversing? false)
  (add-gc-daemon! close-lost-open-files-daemon)
  (add-event-receiver! event:after-restore primitive-io/reset!))

(define-structure (channel (constructor %make-channel))
  ;; This structure serves two purposes.  First, because a descriptor
  ;; is a non-pointer, it is necessary to store it in an allocated
  ;; object in order to determine when all references to it have been
  ;; dropped.  Second, the structure provides a type predicate.
  descriptor
  (type false read-only true))

(define (make-channel descriptor)
  ;; Make sure that interrupts are disabled before `descriptor' is
  ;; created until after this procedure returns.
  (let ((channel
	 (%make-channel
	  descriptor
	  (let ((type ((ucode-primitive channel-type 1) descriptor))
		(types
		 '#(#F FILE PIPE FIFO TERMINAL PTY-MASTER
		       UNIX-STREAM-SOCKET TCP-STREAM-SOCKET)))
	    (and (< type (vector-length types))
		 (vector-ref types type))))))
    (with-absolutely-no-interrupts
     (lambda ()
       (set-cdr! open-channels-list
		 (cons (system-pair-cons (ucode-type weak-cons)
					 channel
					 descriptor)
		       (cdr open-channels-list)))))
    channel))

(define (descriptor->channel descriptor)
  (or (let loop ((channels (cdr open-channels-list)))
	(and (not (null? channels))
	     (if (= descriptor (system-pair-cdr (car channels)))
		 (system-pair-car (car channels))
		 (loop (cdr channels)))))
      (make-channel descriptor)))

(define-integrable (channel-type=file? channel)
  (eq? 'FILE (channel-type channel)))

(define-integrable (channel-type=terminal? channel)
  (eq? 'TERMINAL (channel-type channel)))

(define-integrable (channel-type=pty-master? channel)
  (eq? 'PTY-MASTER (channel-type channel)))

(define (channel-close channel)
  ;; This is locked from interrupts, but GC can occur since the
  ;; procedure itself hangs on to the channel until the last moment,
  ;; when it returns the channel's name.  The list will not be spliced
  ;; by the daemon behind its back because of the traversing? flag.
  (fluid-let ((traversing? true))
    (without-interrupts
     (lambda ()
       (if (channel-descriptor channel)
	   (begin
	     ((ucode-primitive channel-close 1) (channel-descriptor channel))
	     (set-channel-descriptor! channel false)
	     (let loop
		 ((l1 open-channels-list)
		  (l2 (cdr open-channels-list)))
	       (cond ((null? l2)
		      (set! traversing? false)
		      (error "CHANNEL-CLOSE: lost channel" channel))
		     ((eq? channel (system-pair-car (car l2)))
		      (set-cdr! l1 (cdr l2)))
		     (else
		      (loop l2 (cdr l2)))))))))))

(define (close-all-open-files)
  (close-all-open-files-internal (ucode-primitive channel-close 1)))

(define (primitive-io/reset!)
  ;; This is invoked after disk-restoring.  It "cleans" the new runtime system.
  (close-all-open-files-internal (lambda (ignore) ignore)))

(define (close-all-open-files-internal action)
  (fluid-let ((traversing? true))
    (without-interrupts
     (lambda ()
       (let loop ((l (cdr open-channels-list)))
	 (if (not (null? l))
	     (begin
	       (let ((channel (system-pair-car (car l))))
		 (if channel
		     (set-channel-descriptor! channel false)))
	       (action (system-pair-cdr (car l)))
	       (let ((l (cdr l)))
		 (set-cdr! open-channels-list l)
		 (loop l)))))))))

;;; This is the daemon which closes files which no one points to.
;;; Runs with GC, and lower priority interrupts, disabled.
;;; It is unsafe because of the (unnecessary) consing by the
;;; interpreter while it executes the loop.

;;; Replaced by a primitive installed below.
#|
(define (close-lost-open-files-daemon)
  (if (not traversing?)
      (let loop ((l1 open-channels-list) (l2 (cdr open-channels-list)))
	(cond ((null? l2)
	       true)
	      ((system-pair-car (car l2))
	       (loop l2 (cdr l2)))
	      (else
	       ((ucode-primitive channel-close 1) (system-pair-cdr (car l2)))
	       (set-cdr! l1 (cdr l2))
	       (loop l1 (cdr l1)))))))
|#
(define (close-lost-open-files-daemon)
  (if (not traversing?)
      ((ucode-primitive close-lost-open-files 1) open-channels-list)))

;;;; Wrapped Primitives

(define (channel-read channel buffer start end)
  ((ucode-primitive channel-read 4) (channel-descriptor channel)
				    buffer start end))

(define (channel-read-block channel buffer start end)
  (let loop ()
    (or (channel-read channel buffer start end)
	(loop))))

(define (channel-write channel buffer start end)
  ((ucode-primitive channel-write 4) (channel-descriptor channel)
				     buffer start end))

(define (channel-write-block channel buffer start end)
  (let loop ((start start) (n-left (- end start)))
    (let ((n (channel-write channel buffer start end)))
      (cond ((not n) (loop start n-left))
	    ((< n n-left) (loop (+ start n) (- n-left n)))))))

(define (channel-write-string-block channel string)
  (channel-write-block channel string 0 (string-length string)))

(define (channel-write-char-block channel char)
  (channel-write-block channel (string char) 0 1))

(define (channel-blocking? channel)
  ((ucode-primitive channel-blocking? 1) (channel-descriptor channel)))

(define (channel-blocking channel)
  ((ucode-primitive channel-blocking 1) (channel-descriptor channel)))

(define (channel-nonblocking channel)
  ((ucode-primitive channel-nonblocking 1) (channel-descriptor channel)))

(define (with-channel-blocking channel blocking? thunk)
  (let ((blocking-outside?))
    (dynamic-wind
     (lambda ()
       (set! blocking-outside? (channel-blocking? channel))
       (if blocking?
	   (channel-blocking channel)
	   (channel-nonblocking channel)))
     thunk
     (lambda ()
       (set! blocking? (channel-blocking? channel))
       (if blocking-outside?
	   (channel-blocking channel)
	   (channel-nonblocking channel))))))

(define (channel-table)
  (fluid-let ((traversing? true))
    (without-interrupts
     (lambda ()
       (let ((descriptors ((ucode-primitive channel-table 0))))
	 (and descriptors
	      (vector-map descriptors descriptor->channel)))))))

(define (file-open-input-channel filename)
  (without-interrupts
   (lambda ()
     (make-channel ((ucode-primitive file-open-input-channel 1) filename)))))

(define (file-open-output-channel filename)
  ((ucode-primitive file-remove-link 1) filename)
  (without-interrupts
   (lambda ()
     (make-channel ((ucode-primitive file-open-output-channel 1) filename)))))

(define (file-open-io-channel filename)
  (without-interrupts
   (lambda ()
     (make-channel ((ucode-primitive file-open-io-channel 1) filename)))))

(define (file-open-append-channel filename)
  (without-interrupts
   (lambda ()
     (make-channel ((ucode-primitive file-open-append-channel 1) filename)))))

(define (tty-input-channel)
  (without-interrupts
   (lambda ()
     (make-channel ((ucode-primitive tty-input-channel 0))))))

(define (tty-output-channel)
  (without-interrupts
   (lambda ()
     (make-channel ((ucode-primitive tty-output-channel 0))))))

(define (file-length channel)
  ((ucode-primitive file-length-new 1) (channel-descriptor channel)))

(define (file-position channel)
  ((ucode-primitive file-position 1) (channel-descriptor channel)))

(define (file-set-position channel position)
  ((ucode-primitive file-set-position 2) (channel-descriptor channel)
					 position))

(define (terminal-buffered? channel)
  ((ucode-primitive terminal-buffered? 1) (channel-descriptor channel)))

(define (terminal-buffered channel)
  ((ucode-primitive terminal-buffered 1) (channel-descriptor channel)))

(define (terminal-nonbuffered channel)
  ((ucode-primitive terminal-nonbuffered 1) (channel-descriptor channel)))

(define (terminal-flush-input channel)
  ((ucode-primitive terminal-flush-input 1) (channel-descriptor channel)))

(define (terminal-flush-output channel)
  ((ucode-primitive terminal-flush-output 1) (channel-descriptor channel)))

(define (terminal-drain-output channel)
  ((ucode-primitive terminal-drain-output 1) (channel-descriptor channel)))

(define (open-pty-master)
  (without-interrupts
   (lambda ()
     (let ((result ((ucode-primitive open-pty-master 0))))
       (if (not result)
	   (error "unable to open pty master"))
       (values (make-channel (vector-ref result 0))
	       (vector-ref result 1)
	       (vector-ref result 2))))))

(define (pty-master-send-signal channel signal)
  ((ucode-primitive pty-master-send-signal 2) (channel-descriptor channel)
					      signal))

;;;; File Copying

(define (copy-file from to)
  (file-copy (canonicalize-input-filename from)
	     (canonicalize-output-filename to)))

(define (file-copy input-filename output-filename)
  (let ((input-channel false)
	(output-channel false))
    (dynamic-wind
     (lambda ()
       (set! input-channel (file-open-input-channel input-filename))
       (set! output-channel (file-open-output-channel output-filename)))
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
       (if input-channel (channel-close input-channel))))))

;;;; Buffered Output

(define-structure (output-buffer
		   (conc-name output-buffer/)
		   (constructor %make-output-buffer))
  (channel false read-only true)
  string
  position)

(define-integrable (make-output-buffer channel buffer-size)
  (%make-output-buffer channel (make-string buffer-size) 0))

(define (output-buffer/close buffer)
  (output-buffer/drain-block buffer)
  (channel-close (output-buffer/channel buffer)))

(define (output-buffer/size buffer)
  (string-length (output-buffer/string buffer)))

(define (output-buffer/set-size buffer buffer-size)
  (if (> (output-buffer/position buffer) buffer-size)
      (let loop () (if (>= (output-buffer/drain buffer) buffer-size) (loop))))
  (let ((position (output-buffer/position buffer))
	(string (make-string buffer-size)))
    (substring-move-left! (output-buffer/string buffer) 0 position string 0)
    (set-output-buffer/string! buffer string)
    (if (= position buffer-size) (output-buffer/drain buffer))))

(define (output-buffer/drain buffer)
  (let ((position (output-buffer/position buffer)))
    (if (zero? position)
	0
	(let ((channel (output-buffer/channel buffer))
	      (string (output-buffer/string buffer)))
	  (let ((n (channel-write channel string 0 position)))
	    (cond ((or (not n) (zero? n)) position)
		  ((< n position)
		   (let ((position* (- position n)))
		     (substring-move-left! string n position string 0)
		     (set-output-buffer/position! buffer position*)
		     position*))
		  (else
		   (set-output-buffer/position! buffer 0)
		   0)))))))

(define (output-buffer/flush buffer)
  (set-output-buffer/position! buffer 0))

(define (output-buffer/write-substring buffer string start end)
  (if (= start end)
      0
      (let loop ((start start) (n-left (- end start)) (n-previous 0))
	(let ((string* (output-buffer/string buffer))
	      (position (output-buffer/position buffer)))
	  (let ((length (string-length string*))
		(position* (+ position n-left)))
	    (cond ((<= position* length)
		   (substring-move-left! string start end string* position)
		   (set-output-buffer/position! buffer position*)
		   (if (= position* length) (output-buffer/drain buffer))
		   (+ n-previous n-left))
		  ((< position length)
		   (let ((room (- length position)))
		     (let ((end (+ start room))
			   (n-previous (+ n-previous room)))
		       (substring-move-left! string start end string* position)
		       (set-output-buffer/position! buffer length)
		       (if (< (output-buffer/drain buffer) length)
			   (loop end (- n-left room) n-previous)
			   n-previous))))
		  (else
		   (if (< (output-buffer/drain buffer) length)
		       (loop start n-left n-previous)
		       n-previous))))))))

(define (output-buffer/write-char buffer char)
  (let* ((string (output-buffer/string buffer))
	 (length (string-length string)))
    (and (or (< (output-buffer/position buffer) length)
	     (< (output-buffer/drain buffer) length))
	 (let ((position (output-buffer/position buffer)))
	   (string-set! string position char)
	   (let ((position (1+ position)))
	     (set-output-buffer/position! buffer position)
	     (if (= position length) (output-buffer/drain buffer))
	     true)))))

(define (output-buffer/drain-block buffer)
  (let loop ()
    (if (not (zero? (output-buffer/drain buffer)))
	(loop))))

(define (output-buffer/write-string-block buffer string)
  (output-buffer/write-substring-block buffer string 0 (string-length string)))

(define (output-buffer/write-substring-block buffer string start end)
  (let loop ((start start) (n-left (- end start)))
    (let ((n (output-buffer/write-substring buffer string start end)))
      (if (< n n-left)
	  (loop (+ start n) (- n-left n))))))

(define (output-buffer/write-char-block buffer char)
  (let loop ()
    (if (not (output-buffer/write-char buffer char))
	(loop))))

;;;; Buffered Input

(define-structure (input-buffer
		   (conc-name input-buffer/)
		   (constructor %make-input-buffer))
  (channel false read-only true)
  string
  start-index
  end-index)

(define (make-input-buffer channel buffer-size)
  (%make-input-buffer channel
		      (make-string buffer-size)
		      buffer-size
		      buffer-size))

(define (input-buffer/close buffer)
  (set-input-buffer/end-index! buffer 0)
  (channel-close (input-buffer/channel buffer)))

(define (input-buffer/size buffer)
  (string-length (input-buffer/string buffer)))

(define (input-buffer/set-size buffer buffer-size)
  ;; If the buffer's contents will not fit with the new size, the
  ;; oldest part of it is discarded.
  (let ((start-index (input-buffer/start-index buffer))
	(end-index (input-buffer/end-index buffer))
	(string (make-string buffer-size)))
    (substring-move-left! (input-buffer/string buffer)
			  (max start-index (- end-index buffer-size))
			  end-index
			  string
			  0)
    (set-input-buffer/string! buffer string)
    (set-input-buffer/start-index! buffer 0)
    (set-input-buffer/end-index! buffer (- end-index start-index))))

(define (input-buffer/flush buffer)
  (let ((end-index (input-buffer/end-index buffer)))
    (if (< (input-buffer/start-index buffer) end-index)
	(set-input-buffer/start-index! buffer end-index))))

(define (input-buffer/chars-available buffer)
  (- (input-buffer/end-index buffer) (input-buffer/start-index buffer)))

(define (input-buffer/chars-remaining buffer)
  (let ((channel (input-buffer/channel buffer)))
    (and (channel-type=file? channel)
	 (let ((n (- (file-length channel) (file-position channel))))
	   (and (not (negative? n))
		(+ (input-buffer/chars-available buffer) n))))))

(define (input-buffer/char-ready? buffer interval)
  (let ((fill
	 (if (positive? interval)
	     (lambda ()
	       (let ((timeout (+ (real-time-clock) interval)))
		 (let loop ()
		   (cond ((input-buffer/fill buffer) true)
			 ((< (real-time-clock) timeout) (loop))
			 (else false)))))
	     (lambda ()
	       (input-buffer/fill buffer)))))
    (char-ready? buffer
      (lambda (buffer)
	(let ((channel (input-buffer/channel buffer)))
	  (case (channel-blocking? channel)
	    ((#F) (fill))
	    ((#T) (with-channel-blocking channel false fill))
	    (else false)))))))

(define (char-ready? buffer fill)
  (let ((end-index (input-buffer/end-index buffer)))
    (cond ((< (input-buffer/start-index buffer) end-index) true)
	  ((zero? (input-buffer/end-index buffer)) false)
	  (else (fill buffer)))))

(define (input-buffer/fill buffer)
  (let ((end-index
	 (let ((string (input-buffer/string buffer)))
	   (channel-read (input-buffer/channel buffer)
			 string 0 (string-length string)))))
    (and end-index
	 (begin
	   (set-input-buffer/start-index! buffer 0)
	   (set-input-buffer/end-index! buffer end-index)
	   (not (zero? end-index))))))

(define (input-buffer/read-char buffer)
  (let ((start-index (input-buffer/start-index buffer))
	(end-index (input-buffer/end-index buffer)))
    (if (< start-index end-index)
	(begin
	  (set-input-buffer/start-index! buffer (1+ start-index))
	  (string-ref (input-buffer/string buffer) start-index))
	(and (not (zero? end-index))
	     (input-buffer/fill buffer)
	     (begin
	       (set-input-buffer/start-index! buffer 1)
	       (string-ref (input-buffer/string buffer) 0))))))

(define (input-buffer/peek-char buffer)
  (let ((start-index (input-buffer/start-index buffer))
	(end-index (input-buffer/end-index buffer)))
    (if (< start-index end-index)
	(string-ref (input-buffer/string buffer) start-index)
	(and (not (zero? end-index))
	     (input-buffer/fill buffer)
	     (string-ref (input-buffer/string buffer) 0)))))

(define (input-buffer/discard-char buffer)
  (let ((start-index (input-buffer/start-index buffer)))
    (if (< start-index (input-buffer/end-index buffer))
	(set-input-buffer/start-index! buffer (1+ start-index)))))

(define (input-buffer/read-substring buffer string start end)
  (let ((start-index (input-buffer/start-index buffer))
	(end-index (input-buffer/end-index buffer)))
    (cond ((< start-index end-index)
	   (let ((string* (input-buffer/string buffer))
		 (available (- end-index start-index))
		 (needed (- end start)))
	     (if (>= available needed)
		 (begin
		   (let ((end-index (+ start-index needed)))
		     (substring-move-left! string* start-index end-index
					   string start)
		     (set-input-buffer/start-index! buffer end-index))
		   needed)
		 (begin
		   (substring-move-left! string* start-index end-index
					 string start)
		   (set-input-buffer/start-index! buffer end-index)
		   (+ available
		      (or (channel-read (input-buffer/channel buffer)
					string
					(+ start available)
					end)
			  0))))))
	  ((zero? end-index)
	   0)
	  (else
	   (channel-read (input-buffer/channel buffer) string start end)))))

(define (input-buffer/read-until-delimiter buffer delimiters)
  (and (char-ready? buffer input-buffer/fill)
       (let ((string (input-buffer/string buffer)))
	 (let loop ()
	   (let ((start-index (input-buffer/start-index buffer))
		 (end-index (input-buffer/end-index buffer)))
	     (let ((delimiter-index
		    (substring-find-next-char-in-set string
						     start-index
						     end-index
						     delimiters)))
	       (if delimiter-index
		   (let ((head (substring string start-index delimiter-index)))
		     (set-input-buffer/start-index! buffer delimiter-index)
		     head)
		   (let ((head (substring string start-index end-index)))
		     (set-input-buffer/start-index! buffer end-index)
		     (if (input-buffer/fill buffer)
			 (string-append head (loop))
			 head)))))))))

(define (input-buffer/discard-until-delimiter buffer delimiters)
  (if (char-ready? buffer input-buffer/fill)
      (let ((string (input-buffer/string buffer)))
	(let loop ()
	  (let ((end-index (input-buffer/end-index buffer)))
	    (let ((delimiter-index
		   (substring-find-next-char-in-set
		    string
		    (input-buffer/start-index buffer)
		    end-index
		    delimiters)))
	      (if delimiter-index
		  (set-input-buffer/start-index! buffer delimiter-index)
		  (begin
		    (set-input-buffer/start-index! buffer end-index)
		    (if (input-buffer/fill buffer)
			(loop))))))))))