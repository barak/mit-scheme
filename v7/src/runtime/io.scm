#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/io.scm,v 14.30 1992/04/16 05:12:27 jinx Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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
(define open-directories-list)

(define (initialize-package!)
  (set! open-channels-list (list 'OPEN-CHANNELS-LIST))
  (set! traversing? false)
  (add-gc-daemon! close-lost-open-files-daemon)
  (set! open-directories-list (make-protection-list))
  (add-gc-daemon! close-lost-open-directories-daemon)
  (add-event-receiver! event:after-restore primitive-io/reset!))

(define-structure (channel (constructor %make-channel))
  ;; This structure serves two purposes.  First, because a descriptor
  ;; is a non-pointer, it is necessary to store it in an allocated
  ;; object in order to determine when all references to it have been
  ;; dropped.  Second, the structure provides a type predicate.
  descriptor
  (type false read-only true)
  port)

(define (make-channel descriptor)
  ;; Make sure that interrupts are disabled before `descriptor' is
  ;; created until after this procedure returns.
  (let ((channel
	 (%make-channel
	  descriptor
	  (let ((type ((ucode-primitive channel-type 1) descriptor))
		(types
		 '#(#F FILE PIPE FIFO TERMINAL PTY-MASTER
		       UNIX-STREAM-SOCKET TCP-STREAM-SOCKET
		       TCP-SERVER-SOCKET DIRECTORY CHARACTER-DEVICE
		       BLOCK-DEVICE)))
	    (and (< type (vector-length types))
		 (vector-ref types type)))
	  false)))
    (with-absolutely-no-interrupts
     (lambda ()
       (set-cdr! open-channels-list
		 (cons (system-pair-cons (ucode-type weak-cons)
					 channel
					 descriptor)
		       (cdr open-channels-list)))))
    channel))

(define (descriptor->channel descriptor)
  (let loop ((channels (cdr open-channels-list)))
    (and (not (null? channels))
	 (if (fix:= descriptor (system-pair-cdr (car channels)))
	     (system-pair-car (car channels))
	     (loop (cdr channels))))))

(define-integrable (channel-type=unknown? channel)
  (false? (channel-type channel)))

(define-integrable (channel-type=file? channel)
  (eq? 'FILE (channel-type channel)))

(define-integrable (channel-type=terminal? channel)
  (eq? 'TERMINAL (channel-type channel)))

(define-integrable (channel-type=pty-master? channel)
  (eq? 'PTY-MASTER (channel-type channel)))

(define-integrable (channel-type=directory? channel)
  (eq? 'DIRECTORY (channel-type channel)))

(define-integrable (channel-type=character-device? channel)
  (eq? 'CHARACTER-DEVICE (channel-type channel)))

(define-integrable (channel-type=block-device? channel)
  (eq? 'BLOCK-DEVICE (channel-type channel)))

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

(define-integrable (channel-open? channel)
  (channel-descriptor channel))

(define-integrable (channel-closed? channel)
  (not (channel-descriptor channel)))

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
	(ucode-primitive channel-nonblocking 1)
	(ucode-primitive channel-read 4)
	(ucode-primitive channel-register 1)
	(ucode-primitive channel-registered? 1)
	(ucode-primitive channel-select-then-read 4)
	(ucode-primitive channel-unregister 1)
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
	(ucode-primitive terminal-get-state 1)
	(ucode-primitive terminal-nonbuffered 1)
	(ucode-primitive terminal-raw-output 1)
	(ucode-primitive terminal-set-state 2)))

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

(define (channel-registered? channel)
  ((ucode-primitive channel-registered? 1) (channel-descriptor channel)))

(define (channel-register channel)
  ((ucode-primitive channel-register 1) (channel-descriptor channel)))

(define (channel-unregister channel)
  ((ucode-primitive channel-unregister 1) (channel-descriptor channel)))

(define (channel-select-then-read channel buffer start end)
  ((ucode-primitive channel-select-then-read 4) (channel-descriptor channel)
						buffer start end))

(define (channel-table)
  (fluid-let ((traversing? true))
    (without-interrupts
     (lambda ()
       (let ((descriptors ((ucode-primitive channel-table 0))))
	 (and descriptors
	      (vector-map descriptors
		(lambda (descriptor)
		  (or (descriptor->channel descriptor)
		      (make-channel descriptor))))))))))

;;;; File Primitives

(define (file-open primitive filename)
  (let ((channel
	 (without-interrupts
	  (lambda ()
	    (make-channel (primitive filename))))))
    (if (or (channel-type=directory? channel)
	    (channel-type=unknown? channel))
	(begin
	  (channel-close channel)
	  (error:bad-range-argument filename primitive)))
    channel))

(define (file-open-input-channel filename)
  (file-open (ucode-primitive file-open-input-channel 1) filename))

(define (file-open-output-channel filename)
  (file-open (ucode-primitive file-open-output-channel 1) filename))

(define (file-open-io-channel filename)
  (file-open (ucode-primitive file-open-io-channel 1) filename))

(define (file-open-append-channel filename)
  (file-open (ucode-primitive file-open-append-channel 1) filename))

(define (file-length channel)
  ((ucode-primitive file-length-new 1) (channel-descriptor channel)))

(define (file-position channel)
  ((ucode-primitive file-position 1) (channel-descriptor channel)))

(define (file-set-position channel position)
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
     (let ((descriptor ((ucode-primitive new-directory-open 1) name)))
       (let ((channel (make-directory-channel descriptor)))
	 (add-to-protection-list! open-directories-list channel descriptor)
	 channel)))))

(define (directory-channel-close channel)
  (without-interrupts
   (lambda ()
     (let ((descriptor (directory-channel/descriptor channel)))
       (if descriptor
	   (begin
	     ((ucode-primitive new-directory-close 1) descriptor)
	     (set-directory-channel/descriptor! channel false)
	     (remove-from-protection-list! open-directories-list channel)))))))

(define (close-lost-open-directories-daemon)
  (clean-lost-protected-objects open-directories-list
				(ucode-primitive new-directory-close 1)))

(define (directory-channel-read channel)
  ((ucode-primitive new-directory-read 1)
   (directory-channel/descriptor channel)))

(define (directory-channel-read-matching channel prefix)
  ((ucode-primitive new-directory-read-matching 2)
   (directory-channel/descriptor channel)
   prefix))

;;;; Protection lists

;;; These will cause problems on interpreted systems, due to the
;;; consing of the interpreter.  For now we'll only run this compiled.

(define (make-protection-list)
  (list 'PROTECTION-LIST))

(define (add-to-protection-list! list scheme-object microcode-object)
  (with-absolutely-no-interrupts
   (lambda ()
     (set-cdr! list
	       (cons (weak-cons scheme-object microcode-object)
		     (cdr list))))))

(define (remove-from-protection-list! list scheme-object)
  (with-absolutely-no-interrupts
   (lambda ()
     (let loop ((associations (cdr list)) (previous list))
       (if (not (null? associations))
	   (if (eq? scheme-object (weak-pair/car? (car associations)))
	       (set-cdr! previous (cdr associations))
	       (loop (cdr associations) associations)))))))

(define (clean-lost-protected-objects list cleaner)
  (let loop ((associations (cdr list)) (previous list))
    (if (not (null? associations))
	(if (weak-pair/car? (car associations))
	    (loop (cdr associations) associations)
	    (begin
	      (cleaner (weak-cdr (car associations)))
	      (let ((next (cdr associations)))
		(set-cdr! previous next)
		(loop next previous)))))))

(define (search-protection-list list microcode-object)
  (let loop ((associations (cdr list)))
    (and (not (null? associations))
	 (if (eq? microcode-object (system-pair-cdr (car associations)))
	     (system-pair-car (car associations))
	     (loop (cdr associations))))))

;;;; Buffered Output

(define-structure (output-buffer
		   (conc-name output-buffer/)
		   (constructor %make-output-buffer))
  (channel false read-only true)
  string
  position
  line-translation			; string that newline maps to
  logical-size)

(define (output-buffer-sizes translation buffer-size)
  (let ((logical-size
	 (if (and translation (fix:< buffer-size 1))
	     1
	     buffer-size)))
    (values logical-size
	    (if (not translation)
		logical-size
		(fix:+ logical-size
		       (fix:- (string-length translation) 1))))))

(define (make-output-buffer channel buffer-size #!optional line-translation)
  (let ((translation (and (not (default-object? line-translation))
			  line-translation)))
    (with-values
	(lambda ()
	  (output-buffer-sizes translation
			       buffer-size))
      (lambda (logical-size string-size)
	(%make-output-buffer channel
			     (and (fix:> string-size 0) (make-string string-size))
			     0
			     translation
			     logical-size)))))

(define (output-buffer/close buffer)
  (output-buffer/drain-block buffer)
  (channel-close (output-buffer/channel buffer)))

(define (output-buffer/size buffer)
  (output-buffer/logical-size buffer))

(define (output-buffer/set-size buffer buffer-size)
  (output-buffer/drain-block buffer)
  (with-values
      (lambda ()
	(output-buffer-sizes (output-buffer/line-translation buffer)
			     buffer-size))
    (lambda (logical-size string-size)
      (set-output-buffer/logical-size! buffer logical-size)
      (set-output-buffer/string!
       buffer
       (and (fix:> string-size 0) (make-string string-size))))))

(define output-buffer/buffered-chars
  output-buffer/position)

(define (output-buffer/drain buffer)
  (let ((string (output-buffer/string buffer))
	(position (output-buffer/position buffer)))
    (if (or (not string) (zero? position))
	0
	(let ((n (channel-write
		  (output-buffer/channel buffer)
		  string
		  0
		  (let ((logical-size (output-buffer/logical-size buffer)))
		    (if (fix:> position logical-size)
			logical-size
			position)))))
	  (cond ((or (not n) (fix:= n 0))
		 position)
		((fix:< n position)
		 (let ((position* (fix:- position n)))
		   (substring-move-left! string n position string 0)
		   (set-output-buffer/position! buffer position*)
		   position*))
		(else
		 (set-output-buffer/position! buffer 0)
		 0))))))

(define (output-buffer/flush buffer)
  (set-output-buffer/position! buffer 0))

(define (output-buffer/write-substring buffer string start end)
  (define (output-buffer/write-buffered-substring start end)
    (let loop ((start start) (n-left (fix:- end start)) (n-previous 0))
      (let ((string* (output-buffer/string buffer))
	    (position (output-buffer/position buffer)))
	(let ((max-position (output-buffer/logical-size buffer))
	      (position* (fix:+ position n-left)))
	  (cond ((fix:<= position* max-position)
		 (substring-move-left! string start end string* position)
		 (set-output-buffer/position! buffer position*)
		 (if (fix:= position* max-position)
		     (output-buffer/drain buffer))
		 (fix:+ n-previous n-left))
		((fix:< position max-position)
		 (let ((room (fix:- max-position position)))
		   (let ((end (fix:+ start room))
			 (n-previous (fix:+ n-previous room)))
		     (substring-move-left! string start end
					   string* position)
		     (set-output-buffer/position! buffer max-position)
		     (if (fix:< (output-buffer/drain buffer) max-position)
			 (loop end (fix:- n-left room) n-previous)
			 n-previous))))
		(else
		 (if (fix:< (output-buffer/drain buffer) max-position)
		     (loop start n-left n-previous)
		     n-previous)))))))

  ;; This transfers the end-of-line string atomically.  In this way,
  ;; as far as the Scheme program is concerned, either the newline has
  ;; been completely buffered/written, or it has not at all.

  (define (output-buffer/write-translated-newline)
    (let ((translation (output-buffer/line-translation buffer))
	  (string (output-buffer/string buffer))
	  (posn (output-buffer/position buffer)))
      (let ((tlen (string-length translation)))
	(and (fix:<= tlen (fix:- (string-length string) posn))
	     (begin
	       (substring-move-left! translation 0 tlen string posn)
	       (set-output-buffer/position! buffer (fix:+ posn tlen))
	       true)))))
  
  (define (find-next-newline posn)
    (and (fix:< posn end)
	 (if (char=? (string-ref string posn) #\Newline)
	     posn
	     (find-next-newline (fix:+ posn 1)))))

  (cond ((fix:= start end)
	 0)
	((not (output-buffer/string buffer))
	 (or (channel-write (output-buffer/channel buffer) string start end)
	     0))
	((not (output-buffer/line-translation buffer))
	 (output-buffer/write-buffered-substring start end))
	(else
	 (letrec ((write-newline
		   (lambda (posn)
		     (and (output-buffer/write-translated-newline)
			  (let ((next (fix:+ posn 1)))
			    (if (fix:= next end)
				1
				(fix:+ 1
				       (or (write-segment
					    next
					    (find-next-newline next))
					   0)))))))
		  (write-segment
		   (lambda (start posn)
		     (cond ((not posn)
			    (output-buffer/write-buffered-substring start end))
			   ((fix:= posn start)
			    (write-newline posn))
			   (else
			    (let ((delta (fix:- posn start))
				  (n-written
				   (output-buffer/write-buffered-substring
				    start posn)))
			      (and n-written
				   (if (fix:< n-written delta)
				       n-written
				       (fix:+ n-written
					      (or (write-newline posn)
						  0))))))))))

	   (write-segment start (find-next-newline start))))))

(define (output-buffer/drain-block buffer)
  (let loop ()
    (if (not (fix:= (output-buffer/drain buffer) 0))
	(loop))))

(define (output-buffer/write-substring-block buffer string start end)
  (do ((start start
	      (fix:+ start
		     (output-buffer/write-substring buffer string start end))))
      ((fix:>= start end))))

(define (output-buffer/write-char-block buffer char)
  (output-buffer/write-substring-block buffer (string char) 0 1))

(define (output-buffer/write-string-block buffer string)
  (output-buffer/write-substring-block buffer string 0 (string-length string)))

;;;; Buffered Input

(define-structure (input-buffer
		   (conc-name input-buffer/)
		   (constructor %make-input-buffer))
  (channel false read-only true)
  string
  start-index
  ;; END-INDEX is zero iff CHANNEL is closed.
  end-index
  line-translation			; string that maps to newline
  real-end)

(define (input-buffer-size translation buffer-size)
  (cond ((not translation)
	 (if (fix:< buffer-size 1)
	     1
	     buffer-size))
	((fix:< buffer-size (string-length translation))
	 (string-length translation))
	(else
	 buffer-size)))

(define (make-input-buffer channel buffer-size #!optional line-translation)
  (let* ((translation (and (not (default-object? line-translation))
			   line-translation))
	 (string-size (input-buffer-size translation buffer-size)))
    (%make-input-buffer channel
			(make-string string-size)
			string-size
			string-size
			translation
			string-size)))

(define (input-buffer/close buffer)
  (set-input-buffer/end-index! buffer 0)
  (channel-close (input-buffer/channel buffer)))

(define (input-buffer/size buffer)
  (string-length (input-buffer/string buffer)))

(define (input-buffer/set-size buffer buffer-size)
  ;; Returns the actual buffer size, which may be different from the arg.
  ;; Discards any buffered characters.
  (if (not (fix:= (input-buffer/end-index buffer) 0))
      (let ((string-size 
	     (input-buffer-size (input-buffer/line-translation buffer)
				buffer-size)))
	(let ((old-string (input-buffer/string buffer))
	      (delta (fix:- (input-buffer/real-end buffer)
			    (input-buffer/end-index buffer))))
	  (set-input-buffer/string! buffer (make-string string-size))
	  (let ((logical-end
		 (if (fix:zero? delta)
		     string-size
		     (let ((logical-end (fix:- string-size delta)))
		       (substring-move-left! old-string
					     (input-buffer/end-index buffer)
					     (input-buffer/real-end buffer)
					     (input-buffer/string buffer)
					     logical-end)
		       logical-end))))
	    (set-input-buffer/start-index! buffer logical-end)
	    (set-input-buffer/end-index! buffer logical-end)
	    (set-input-buffer/real-end! buffer string-size)
	    string-size)))))

(define (input-buffer/flush buffer)
  (set-input-buffer/start-index! buffer (input-buffer/end-index buffer)))

(define (input-buffer/buffered-chars buffer)
  (fix:- (input-buffer/end-index buffer) (input-buffer/start-index buffer)))

(define (input-buffer/chars-remaining buffer)
  (let ((channel (input-buffer/channel buffer)))
    (and (channel-open? channel)
	 (channel-type=file? channel)
	 (not (input-buffer/line-translation buffer)) ; Can't tell otherwise
	 (let ((n (fix:- (file-length channel) (file-position channel))))
	   (and (fix:>= n 0)
		(fix:+ (input-buffer/buffered-chars buffer) n))))))

(define (input-buffer/char-ready? buffer interval)
  (char-ready? buffer
    (lambda (buffer)
      (let ((channel (input-buffer/channel buffer)))
	(and (channel-open? channel)
	     (with-channel-blocking channel false
	       (lambda ()
		 (if (positive? interval)
		     (let ((timeout (+ (real-time-clock) interval)))
		       (let loop ()
			 (let ((n (input-buffer/fill buffer)))
			   (if n
			       (fix:> n 0)
			       (and (< (real-time-clock) timeout)
				    (loop))))))
		     (input-buffer/fill* buffer)))))))))

(define (char-ready? buffer fill)
  (let ((end-index (input-buffer/end-index buffer)))
    (cond ((fix:= (input-buffer/end-index buffer) 0) false)
	  ((fix:< (input-buffer/start-index buffer) end-index) true)
	  (else (fill buffer)))))

(define (input-buffer/eof? buffer)
  ;; This returns true iff it knows that it is at EOF.
  ;; If BUFFER is non-blocking with no input available, it returns false.
  (and (not (input-buffer/char-ready? buffer 0))
       (fix:= (input-buffer/end-index buffer) 0)))

(define (input-buffer/fill buffer)
  (let ((channel (input-buffer/channel buffer)))
    (if (channel-closed? channel)
	0
	(let ((delta (fix:- (input-buffer/real-end buffer)
			    (input-buffer/end-index buffer)))
	      (string (input-buffer/string buffer)))
	  (if (not (fix:zero? delta))
	      (substring-move-left! string
				    (input-buffer/end-index buffer)
				    (input-buffer/real-end buffer)
				    string
				    0))
	  (let ((n-read
		 (channel-read channel string delta (string-length string))))
	    (and n-read
		 (let ((end-index (fix:+ delta n-read)))
		   (set-input-buffer/start-index! buffer 0)
		   (set-input-buffer/end-index! buffer end-index)
		   (set-input-buffer/real-end! buffer end-index)
		   (cond ((and (input-buffer/line-translation buffer)
			       (not (fix:= end-index 0)))
			  (input-buffer/translate! buffer))
			 ((fix:= n-read 0)
			  (channel-close channel)
			  end-index)
			 (else
			  end-index)))))))))

(define-integrable (input-buffer/fill* buffer)
  (let ((n (input-buffer/fill buffer)))
    (and n
	 (fix:> n 0))))

;;;; Input line termination translation

(define (input-buffer/translate! buffer)
  (with-values
      (lambda ()
	(substring/input-translate! (input-buffer/string buffer)
				    (input-buffer/line-translation buffer)
				    0
				    (input-buffer/real-end buffer)))
    (lambda (logical-end real-end)
      (set-input-buffer/end-index! buffer logical-end)
      (set-input-buffer/real-end! buffer real-end)
      logical-end)))

;; This maps a multi-character (perhaps only 1) sequence into a single
;; newline character.

(define (substring/input-translate! string translation start end)
  (let ((tlen (string-length translation))
	(match (vector-8b-ref translation 0)))

    (define (verify position)
      (or (fix:< tlen 2)
	  (let ((next (fix:+ position 1)))
	    (if (not (fix:< next end))
		'TOO-SHORT
		(and (fix:= (vector-8b-ref translation 1)
			    (vector-8b-ref string next))
		     (or (fix:= tlen 2)
			 (let verify-loop ((tpos 2) (spos (fix:+ next 1)))
			   (cond ((not (fix:< tpos tlen))
				  true)
				 ((not (fix:< spos end))
				  'TOO-SHORT)
				 ((not (fix:= (vector-8b-ref translation tpos)
					      (vector-8b-ref string spos)))
				  false)
				 (else
				  (verify-loop (fix:+ tpos 1)
					       (fix:+ spos 1)))))))))))

    (define (clobber-loop target source)
      ;; Found one match, continue looking at source
      (string-set! string target #\Newline)
      (let find-next ((target (fix:+ target 1)) (source source))
	(cond ((not (fix:< source end))
	       ;; Finished after doing some clobbering.
	       ;; Real and virtual pointer in sync.
	       (values target target))
	      ((not (fix:= match (vector-8b-ref string source)))
	       (vector-8b-set! string target
			       (vector-8b-ref string source))
	       (find-next (fix:+ target 1) (fix:+ source 1)))
	      (else
	       (case (verify source)
		 ((#f)
		  (vector-8b-set! string target
				  (vector-8b-ref string source))
		  (find-next (fix:+ target 1) (fix:+ source 1)))
		 ((TOO-SHORT)
		  ;; Pointers not in sync, since the buffer ends
		  ;; in what appears to be the middle of a
		  ;; translation sequence
		  (let copy-loop ((target* target) (source source))
		    (if (not (fix:< source end))
			(values target target*)
			(begin
			  (vector-8b-set! string target*
					  (vector-8b-ref string source))
			  (copy-loop (fix:+ target* 1) (fix:+ source 1))))))
		 (else
		  (clobber-loop target (fix:+ source tlen))))))))

    (define (find-loop position)
      (cond ((not (fix:< position end))
	     (values position position))
	    ((not (fix:= match (vector-8b-ref string position)))
	     (find-loop (fix:+ position 1)))
	    (else
	     (case (verify position)
	       ((#f)
		(find-loop (fix:+ position 1)))
	       ((TOO-SHORT)
		(values position end))
	       (else
		(clobber-loop position (fix:+ position tlen)))))))

    (find-loop start)))

(define (input-buffer/read-char buffer)
  (let ((start-index (input-buffer/start-index buffer))
	(end-index (input-buffer/end-index buffer)))
    (cond ((fix:< start-index end-index)
	   (set-input-buffer/start-index! buffer (fix:+ start-index 1))
	   (string-ref (input-buffer/string buffer) start-index))
	  ((fix:= end-index 0)
	   eof-object)
	  (else
	   (let ((n (input-buffer/fill buffer)))
	     (cond ((not n) false)
		   ((fix:= n 0) eof-object)
		   (else
		    (set-input-buffer/start-index! buffer 1)
		    (string-ref (input-buffer/string buffer) 0))))))))

(define (input-buffer/peek-char buffer)
  (let ((start-index (input-buffer/start-index buffer))
	(end-index (input-buffer/end-index buffer)))
    (cond ((fix:< start-index end-index)
	   (string-ref (input-buffer/string buffer) start-index))
	  ((fix:= end-index 0)
	   eof-object)
	  (else
	   (let ((n (input-buffer/fill buffer)))
	     (cond ((not n) false)
		   ((fix:= n 0) eof-object)
		   (else (string-ref (input-buffer/string buffer) 0))))))))

(define (input-buffer/discard-char buffer)
  (let ((start-index (input-buffer/start-index buffer)))
    (if (fix:< start-index (input-buffer/end-index buffer))
	(set-input-buffer/start-index! buffer (fix:+ start-index 1)))))

(define (input-buffer/read-substring buffer string start end)
  (define (read-directly start end)
    (if (not (input-buffer/line-translation buffer))
	(channel-read (input-buffer/channel buffer) string start end)
	(let ((next (input-buffer/fill buffer)))
	  (and next
	       (transfer-input-buffer start end)))))

  (define (transfer-input-buffer start end)
    (let ((start-index (input-buffer/start-index buffer))
	  (end-index (input-buffer/end-index buffer)))
      (cond ((fix:< start-index end-index)
	     (let ((string* (input-buffer/string buffer))
		   (available (fix:- end-index start-index))
		   (needed (fix:- end start)))
	       (if (fix:>= available needed)
		   (begin
		     (let ((end-index (fix:+ start-index needed)))
		       (substring-move-left! string* start-index end-index
					     string start)
		       (set-input-buffer/start-index! buffer end-index))
		     needed)
		   (begin
		     (substring-move-left! string* start-index end-index
					   string start)
		     (set-input-buffer/start-index! buffer end-index)
		     (fix:+ available
			    (or (and (channel-open? (input-buffer/channel buffer))
				     (read-directly (fix:+ start available)
						    end))
				0))))))
	    ((or (fix:= end-index 0)
		 (channel-closed? channel))
	     0)
	    (else
	     (read-directly start end)))))

  (transfer-input-buffer start end))

(define (input-buffer/read-until-delimiter buffer delimiters)
  (let ((channel (input-buffer/channel buffer)))
    (if (and (channel-open? channel)
	     (char-ready? buffer input-buffer/fill-block))
	(apply string-append
	       (let ((string (input-buffer/string buffer)))
		 (let loop ()
		   (let ((start (input-buffer/start-index buffer))
			 (end (input-buffer/end-index buffer)))
		     (let ((delimiter
			    (substring-find-next-char-in-set string start end
							     delimiters)))
		       (if delimiter
			   (let ((head (substring string start delimiter)))
			     (set-input-buffer/start-index! buffer delimiter)
			     (list head))
			   (let ((head (substring string start end)))
			     (set-input-buffer/start-index! buffer end)
			     (cons head
				   (if (input-buffer/fill-block buffer)
				       (loop)
				       '())))))))))
	eof-object)))

(define (input-buffer/discard-until-delimiter buffer delimiters)
  (let ((channel (input-buffer/channel buffer)))
    (if (and (channel-open? channel)
	     (char-ready? buffer input-buffer/fill-block))
	(let ((string (input-buffer/string buffer)))
	  (let loop ()
	    (let ((end-index (input-buffer/end-index buffer)))
	      (let ((index
		     (substring-find-next-char-in-set
		      string
		      (input-buffer/start-index buffer)
		      end-index
		      delimiters)))
		(if index
		    (set-input-buffer/start-index! buffer index)
		    (begin
		      (set-input-buffer/start-index! buffer end-index)
		      (if (input-buffer/fill-block buffer)
			  (loop)))))))))))

(define (input-buffer/fill-block buffer)
  (fix:> (let loop () (or (input-buffer/fill buffer) (loop))) 0))

(define (input-buffer/buffer-contents buffer)
  (and (fix:< (input-buffer/start-index buffer)
	      (input-buffer/end-index buffer))
       (substring (input-buffer/string buffer)
		  (input-buffer/start-index buffer)
		  (input-buffer/end-index buffer))))

(define (input-buffer/set-buffer-contents buffer contents)
  (let ((contents-size (string-length contents)))
    (if (fix:> contents-size 0)
	(let ((string (input-buffer/string buffer)))
	  (if (fix:> contents-size (string-length string))
	      (input-buffer/set-size buffer contents-size))
	  (substring-move-left! contents 0 contents-size string 0)
	  (set-input-buffer/start-index! buffer 0)
	  (set-input-buffer/end-index! buffer contents-size)))))