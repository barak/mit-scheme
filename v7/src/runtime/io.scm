#| -*-Scheme-*-

$Id: io.scm,v 14.68 2003/01/22 18:43:05 cph Exp $

Copyright 1986,1987,1988,1990,1991,1993 Massachusetts Institute of Technology
Copyright 1994,1995,1998,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2002,2003 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Input/Output Utilities
;;; package: (runtime primitive-io)

(declare (usual-integrations))

(define open-channels-list)
(define open-directories)

(define (initialize-package!)
  (set! open-channels-list (list 'OPEN-CHANNELS-LIST))
  (add-gc-daemon! close-lost-open-files-daemon)
  (set! open-directories
	(make-gc-finalizer (ucode-primitive new-directory-close 1)))
  (add-event-receiver! event:after-restore
    (lambda ()
      (close-all-open-channels-internal (lambda (ignore) ignore))))
  (initialize-select-registry!))

(define-structure (channel (constructor %make-channel))
  ;; This structure serves two purposes.  First, because a descriptor
  ;; is a non-pointer, it is necessary to store it in an allocated
  ;; object in order to determine when all references to it have been
  ;; dropped.  Second, the structure provides a type predicate.
  descriptor
  (type #f read-only #t)
  port)

(define (open-channel procedure)
  ;; A bunch of hair to permit microcode descriptors be opened with
  ;; interrupts turned on, yet not leave a dangling descriptor around
  ;; if the open is interrupted before the runtime system's data
  ;; structures are updated.
  (let ((p (system-pair-cons (ucode-type weak-cons) #f #f)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda ()
       (and (procedure p)
	    (make-channel-1 p)))
     (lambda ()
       (if (and (not (system-pair-car p)) (system-pair-cdr p))
	   (begin
	     ((ucode-primitive channel-close 1) (system-pair-cdr p))
	     (system-pair-set-cdr! p #f)))))))

(define (make-channel descriptor)
  (make-channel-1 (system-pair-cons (ucode-type weak-cons) #f descriptor)))

(define (make-channel-1 p)
  (let ((channel
	 (let ((d (system-pair-cdr p)))
	   (%make-channel d (descriptor-type-name d) #f))))
    (without-interrupts
     (lambda ()
       (system-pair-set-car! p channel)
       (set-cdr! open-channels-list (cons p (cdr open-channels-list)))))
    channel))

(define (descriptor->channel descriptor)
  (let loop ((channels (cdr open-channels-list)))
    (and (not (null? channels))
	 (if (fix:= descriptor (system-pair-cdr (car channels)))
	     (system-pair-car (car channels))
	     (loop (cdr channels))))))

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
     (if (channel-descriptor channel)
	 (begin
	   ((ucode-primitive channel-close 1) (channel-descriptor channel))
	   (set-channel-descriptor! channel #f)
	   (let loop
	       ((l1 open-channels-list)
		(l2 (cdr open-channels-list)))
	     (cond ((null? l2)
		    (error "CHANNEL-CLOSE: lost channel" channel))
		   ((eq? channel (system-pair-car (car l2)))
		    (set-cdr! l1 (cdr l2)))
		   (else
		    (loop l2 (cdr l2))))))))))

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
	(close-all-open-channels-internal (ucode-primitive channel-close 1)))))

(define (all-open-channels)
  (without-interrupts
   (lambda ()
     (let loop ((l (cdr open-channels-list)) (result '()))
       (if (null? l)
	   result
	   (loop (cdr l) (cons (system-pair-car (car l)) result)))))))

(define (close-all-open-channels-internal action)
  (without-interrupts
   (lambda ()
     (let loop ((l (cdr open-channels-list)))
       (if (not (null? l))
	   (begin
	     (let ((channel (system-pair-car (car l))))
	       (if channel
		   (set-channel-descriptor! channel #f)))
	     (action (system-pair-cdr (car l)))
	     (let ((l (cdr l)))
	       (set-cdr! open-channels-list l)
	       (loop l))))))))

(define (close-lost-open-files-daemon)
  ;; This is the daemon that closes files that no one points to.
  (let loop ((l1 open-channels-list) (l2 (cdr open-channels-list)))
    (cond ((null? l2)
	   unspecific)
	  ((system-pair-car (car l2))
	   (loop l2 (cdr l2)))
	  (else
	   ((ucode-primitive channel-close 1) (system-pair-cdr (car l2)))
	   (set-cdr! l1 (cdr l2))
	   (loop l1 (cdr l1))))))

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
			 ((READ ERROR) (do-read))
			 ((HANGUP) 0)
			 ((PROCESS-STATUS-CHANGE)
			  (handle-subprocess-status-change)
			  (if (channel-closed? channel) 0 (k)))
			 (else (k)))))))
	      (if (channel-blocking? channel)
		  (let loop () (do-test loop))
		  (do-test (lambda () #f))))))
	(do-read))))

(define (channel-read-block channel buffer start end)
  (let loop ()
    (or (channel-read channel buffer start end)
	(loop))))

(define (test-for-io-on-channel channel mode)
  (test-for-io-on-descriptor (channel-descriptor-for-select channel)
			     (channel-blocking? channel)
			     mode))

(define (test-for-io-on-descriptor descriptor block? mode)
  (if block?
      (or (test-select-descriptor descriptor #f mode)
	  (block-on-io-descriptor descriptor mode))
      (test-select-descriptor descriptor #f mode)))

(define-integrable (channel-descriptor-for-select channel)
  ((ucode-primitive channel-descriptor 1) (channel-descriptor channel)))

(define (channel-write channel buffer start end)
  ((ucode-primitive channel-write 4) (channel-descriptor channel)
				     (if (external-string? buffer)
					 (external-string-descriptor buffer)
					 buffer)
				     start
				     end))

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
     (let ((descriptor ((ucode-primitive new-directory-open 1) name)))
       (let ((channel (make-directory-channel descriptor)))
	 (add-to-gc-finalizer! open-directories channel descriptor)
	 channel)))))

(define (directory-channel-close channel)
  (without-interrupts
   (lambda ()
     (if (directory-channel/descriptor channel)
	 (begin
	   (remove-from-gc-finalizer! open-directories channel)
	   (set-directory-channel/descriptor! channel #f))))))

(define (directory-channel-read channel)
  ((ucode-primitive new-directory-read 1)
   (directory-channel/descriptor channel)))

(define (directory-channel-read-matching channel prefix)
  ((ucode-primitive new-directory-read-matching 2)
   (directory-channel/descriptor channel)
   prefix))

;;;; Buffered Output

(define-structure (output-buffer
		   (conc-name output-buffer/)
		   (constructor %make-output-buffer))
  (channel #f read-only #t)
  string
  position
  line-translation			; string that newline maps to
  logical-size
  closed?
  column)

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
  (let ((translation
	 (if (or (default-object? line-translation)
		 ;; Kludge because of DEFAULT-OBJECT?:
		 (eq? 'DEFAULT line-translation))
	     (if (eq? 'TCP-STREAM-SOCKET (channel-type channel))
		 "\r\n"
		 (os/default-end-of-line-translation))
	     (if (and (string? line-translation)
		      (string=? "\n" line-translation))
		 #f
		 line-translation))))
    (with-values (lambda () (output-buffer-sizes translation buffer-size))
      (lambda (logical-size string-size)
	(%make-output-buffer channel
			     (and (fix:> string-size 0)
				  (make-string string-size))
			     0
			     translation
			     logical-size
			     #f
			     0)))))

(define (output-buffer/close buffer associated-buffer)
  (output-buffer/drain-block buffer)
  (without-interrupts
   (lambda ()
     (set-output-buffer/closed?! buffer #t)
     (let ((channel (output-buffer/channel buffer)))
       (if (not (and (input-buffer? associated-buffer)
		     (eq? channel (input-buffer/channel associated-buffer))
		     (input-buffer/open? associated-buffer)))
	   (channel-close channel))))))

(define-integrable (output-buffer/open? buffer)
  (not (output-buffer/closed? buffer)))

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

(define (output-buffer/write-substring buffer string start end)
  (let ((name 'OUTPUT-BUFFER/WRITE-SUBSTRING))
    (if (output-buffer/closed? buffer)
	(error:bad-range-argument buffer name))
    (cond ((string? string)
	   (if (not (index-fixnum? start))
	       (error:wrong-type-argument start "string index" name))
	   (if (not (index-fixnum? end))
	       (error:wrong-type-argument end "string index" name))
	   (if (not (fix:<= end (string-length string)))
	       (error:bad-range-argument end name))
	   (cond ((fix:< start end)
		  (output-buffer/write-substring-1 buffer string start end))
		 ((fix:= start end) 0)
		 (else (error:bad-range-argument start name))))
	  ((external-string? string)
	   (if (not (exact-nonnegative-integer? start))
	       (error:wrong-type-argument start "exact nonnegative integer"
					  name))
	   (if (not (exact-nonnegative-integer? end))
	       (error:wrong-type-argument end "exact nonnegative integer"
					  name))
	   (if (not (<= end (external-string-length string)))
	       (error:bad-range-argument end name))
	   (cond ((< start end)
		  (output-buffer/write-xsubstring buffer string start end))
		 ((= start end) 0)
		 (else (error:bad-range-argument start name))))
	  (else
	   (error:wrong-type-argument string "string" name)))))

(define (output-buffer/write-xsubstring buffer string start end)
  (cond ((output-buffer/line-translation buffer)
	 (let* ((n 65536)
		(b (make-string n)))
	   (let loop ((index start))
	     (if (< index end)
		 (let ((n-to-write (min (- end index) n)))
		   (xsubstring-move! string index (+ index n-to-write) b 0)
		   (let ((n-written
			  (output-buffer/write-substring-1 buffer
							   b 0 n-to-write)))
		     (let ((index* (+ n-written index)))
		       (if (< n-written n-to-write)
			   (- index* start)
			   (loop index*)))))
		 (- index start)))))
	((and (output-buffer/string buffer)
	      (<= (- end start)
		  (fix:- (output-buffer/logical-size buffer)
			 (output-buffer/position buffer))))
	 (xsubstring-move! string start end
			   (output-buffer/string buffer)
			   (output-buffer/position buffer))
	 (set-output-buffer/position! buffer
				      (fix:+ (output-buffer/position buffer)
					     (- end start))))
	(else
	 (output-buffer/drain-block buffer)
	 (or (channel-write (output-buffer/channel buffer) string start end)
	     0))))

(define (output-buffer/write-substring-1 buffer string start end)
  (define (write-buffered start end n-previous)
    (if (fix:< start end)
	(let loop ((start start) (n-previous n-previous))
	  (let ((n-left (fix:- end start))
		(max-posn (output-buffer/logical-size buffer)))
	    (let ((room (fix:- max-posn (output-buffer/position buffer))))
	      (cond ((fix:>= room n-left)
		     (add-to-buffer string start end)
		     (if (fix:= n-left room)
			 (output-buffer/drain buffer))
		     (fix:+ n-previous n-left))
		    ((fix:> room 0)
		     (let ((new-start (fix:+ start room))
			   (n-previous (fix:+ n-previous room)))
		       (add-to-buffer string start new-start)
		       (if (fix:< (output-buffer/drain buffer) max-posn)
			   (loop new-start n-previous)
			   n-previous)))
		    (else
		     (if (fix:< (output-buffer/drain buffer) max-posn)
			 (loop start n-previous)
			 n-previous))))))
	n-previous))

  (define (write-newline)
    ;; This transfers the end-of-line string atomically.  In this way,
    ;; as far as the Scheme program is concerned, either the newline
    ;; has been completely buffered/written, or it has not at all.
    (let ((translation (output-buffer/line-translation buffer)))
      (let ((tlen (string-length translation)))
	(let loop ()
	  (let ((posn (output-buffer/position buffer)))
	    (if (fix:<= tlen
			(fix:- (string-length (output-buffer/string buffer))
			       posn))
		(begin
		  (add-to-buffer translation 0 tlen)
		  #t)
		(and (fix:< (output-buffer/drain buffer) posn)
		     (loop))))))))

  (define (add-to-buffer string start end)
    (let ((posn (output-buffer/position buffer)))
      (substring-move! string start end (output-buffer/string buffer) posn)
      (set-output-buffer/position! buffer (fix:+ posn (fix:- end start)))))

  (let ((n-written
	 (cond ((not (output-buffer/string buffer))
		(or (channel-write (output-buffer/channel buffer)
				   string start end)
		    0))
	       ((not (output-buffer/line-translation buffer))
		(write-buffered start end 0))
	       (else
		(let loop ((start start) (n-prev 0))
		  (let find-newline ((index start))
		    (cond ((fix:= index end)
			   (write-buffered start end n-prev))
			  ((not (char=? (string-ref string index) #\newline))
			   (find-newline (fix:+ index 1)))
			  (else
			   (let ((n-prev* (write-buffered start index n-prev)))
			     (if (or (fix:< n-prev*
					    (fix:+ n-prev (fix:- start index)))
				     (not (write-newline)))
				 n-prev*
				 (loop (fix:+ index 1)
				       (fix:+ n-prev* 1))))))))))))
    (set-output-buffer/column!
     buffer
     (let* ((end (fix:+ start n-written))
	    (nl (substring-find-previous-char string start end #\newline)))
       (if nl
	   (count-columns string (fix:+ nl 1) end 0)
	   (count-columns string start end (output-buffer/column buffer)))))
    n-written))

(define (count-columns string start end column)
  ;; This simple-minded algorithm works only for a limited subset of
  ;; US-ASCII.  Doing a better job quickly gets very hairy.
  (do ((start start (fix:+ start 1))
       (column column
	       (fix:+ column
		      (if (char=? #\tab (string-ref string start))
			  (fix:- 8 (fix:remainder column 8))
			  1))))
      ((fix:= start end) column)))

(define (output-buffer/drain buffer)
  (let ((string (output-buffer/string buffer))
	(position (output-buffer/position buffer)))
    (if (or (not string) (zero? position) (output-buffer/closed? buffer))
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
		   (substring-move! string n position string 0)
		   (set-output-buffer/position! buffer position*)
		   position*))
		(else
		 (set-output-buffer/position! buffer 0)
		 0))))))

(define (output-buffer/flush buffer)
  (set-output-buffer/position! buffer 0))

(define (output-buffer/drain-block buffer)
  (let loop ()
    (if (not (fix:= (output-buffer/drain buffer) 0))
	(loop))))

(define (output-buffer/write-substring-block buffer string start end)
  (do ((start start
	      (+ start
		 (output-buffer/write-substring buffer string start end))))
      ((>= start end))))

(define (output-buffer/write-char-block buffer char)
  (output-buffer/write-substring-block buffer (string char) 0 1))

;;;; Buffered Input

(define-structure (input-buffer
		   (conc-name input-buffer/)
		   (constructor %make-input-buffer))
  (channel #f read-only #t)
  string
  start-index
  end-index
  line-translation			; string that maps to newline
  ;; REAL-END is zero iff the buffer is closed.
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
  (let* ((translation
	  (if (or (default-object? line-translation)
		  ;; Kludge because of DEFAULT-OBJECT?:
		  (eq? 'DEFAULT line-translation))
	      (if (eq? 'TCP-STREAM-SOCKET (channel-type channel))
		  "\r\n"
		  (os/default-end-of-line-translation))
	      (if (and (string? line-translation)
		       (string=? "\n" line-translation))
		  #f
		  line-translation)))
	 (string-size (input-buffer-size translation buffer-size)))
    (%make-input-buffer channel
			(make-string string-size)
			string-size
			string-size
			translation
			string-size)))

(define (input-buffer/close buffer associated-buffer)
  (without-interrupts
   (lambda ()
     (set-input-buffer/real-end! buffer 0)
     (let ((channel (input-buffer/channel buffer)))
       (if (not (and (output-buffer? associated-buffer)
		     (eq? channel (output-buffer/channel associated-buffer))
		     (output-buffer/open? associated-buffer)))
	   (channel-close channel))))))

(define-integrable (input-buffer/closed? buffer)
  (fix:= 0 (input-buffer/real-end buffer)))

(define-integrable (input-buffer/open? buffer)
  (not (input-buffer/closed? buffer)))

(define (input-buffer/size buffer)
  (string-length (input-buffer/string buffer)))

(define (input-buffer/set-size buffer buffer-size)
  ;; Returns the actual buffer size, which may be different from the arg.
  ;; Discards any buffered characters.
  (without-interrupts
   (lambda ()
     (if (input-buffer/closed? buffer)
	 0
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
			  (substring-move! old-string
					   (input-buffer/end-index buffer)
					   (input-buffer/real-end buffer)
					   (input-buffer/string buffer)
					   logical-end)
			  logical-end))))
	       (set-input-buffer/start-index! buffer logical-end)
	       (set-input-buffer/end-index! buffer logical-end)
	       (set-input-buffer/real-end! buffer string-size)
	       string-size)))))))

(define (input-buffer/flush buffer)
  (without-interrupts
   (lambda ()
     (set-input-buffer/start-index! buffer (input-buffer/end-index buffer)))))

(define (input-buffer/buffered-chars buffer)
  (without-interrupts
   (lambda ()
     (fix:- (input-buffer/end-index buffer)
	    (input-buffer/start-index buffer)))))

(define (input-buffer/fill buffer)
  ;; Assumption:
  ;; (and (input-buffer/open? buffer)
  ;;      (fix:= (input-buffer/start-index buffer)
  ;;             (input-buffer/end-index buffer)))
  (let ((delta
	 (fix:- (input-buffer/real-end buffer)
		(input-buffer/end-index buffer)))
	(string (input-buffer/string buffer)))
    (if (not (fix:= delta 0))
	(substring-move! string
			 (input-buffer/end-index buffer)
			 (input-buffer/real-end buffer)
			 string
			 0))
    (let ((n-read
	   (channel-read (input-buffer/channel buffer)
			 string delta (string-length string))))
      (and n-read
	   (input-buffer/after-fill! buffer (fix:+ delta n-read))))))

(define (input-buffer/after-fill! buffer end-index)
  (set-input-buffer/start-index! buffer 0)
  (set-input-buffer/end-index! buffer end-index)
  (set-input-buffer/real-end! buffer end-index)
  (if (and (input-buffer/line-translation buffer)
	   (not (fix:= end-index 0)))
      (input-buffer/translate! buffer)
      end-index))

(define-integrable (input-buffer/fill* buffer)
  (let ((n (input-buffer/fill buffer)))
    (and n
	 (fix:> n 0))))

(define (input-buffer/chars-remaining buffer)
  (without-interrupts
   (lambda ()
     (and (input-buffer/open? buffer)
	  (not (input-buffer/line-translation buffer))
	  (let ((channel (input-buffer/channel buffer)))
	    (and (channel-type=file? channel)
		 (let ((n
			(fix:- (channel-file-length channel)
			       (channel-file-position channel))))
		   (and (fix:>= n 0)
			(fix:+ (input-buffer/buffered-chars buffer) n)))))))))

(define (input-buffer/char-ready? buffer interval)
  (without-interrupts
   (lambda ()
     (char-ready? buffer
       (lambda (buffer)
	 (with-channel-blocking (input-buffer/channel buffer) #f
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
  (and (input-buffer/open? buffer)
       (or (fix:< (input-buffer/start-index buffer)
		  (input-buffer/end-index buffer))
	   (fill buffer))))

(define (input-buffer/eof? buffer)
  ;; This returns #t iff it knows that it is at EOF.
  ;; If BUFFER is non-blocking with no input available, it returns #f.
  (and (not (input-buffer/char-ready? buffer 0))
       (input-buffer/closed? buffer)))

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
      (and (fix:> logical-end 0) logical-end))))

(define (substring/input-translate! string translation start end)
  ;; This maps a multi-character (perhaps only 1) sequence into a
  ;; single newline character.
  (let ((tlen (string-length translation))
	(match (string-ref translation 0)))

    (define (find-loop index)
      (cond ((fix:= index end)
	     (values index index))
	    ((char=? match (string-ref string index))
	     (case (verify index)
	       ((#F) (find-loop (fix:+ index 1)))
	       ((TOO-SHORT) (values index end))
	       (else (clobber-loop index (fix:+ index tlen)))))
	    (else
	     (find-loop (fix:+ index 1)))))

    (define verify
      (if (fix:= tlen 2)
	  (lambda (index)
	    (let ((index (fix:+ index 1)))
	      (if (fix:= index end)
		  'TOO-SHORT
		  (char=? (string-ref translation 1)
			  (string-ref string index)))))
	  (lambda (index)
	    (let loop ((tind 1) (index (fix:+ index 1)))
	      (cond ((fix:= tind tlen)
		     #t)
		    ((fix:= index end)
		     'TOO-SHORT)
		    (else
		     (and (char=? (string-ref translation tind)
				  (string-ref string index))
			  (loop (fix:+ tind 1)
				(fix:+ index 1)))))))))

    (define (clobber-loop target source)
      ;; Found one match, continue looking at source
      (string-set! string target #\newline)
      (let find-next ((target (fix:+ target 1)) (source source))
	(cond ((fix:= source end)
	       ;; Pointers in sync.
	       (values target target))
	      ((char=? match (string-ref string source))
	       (case (verify source)
		 ((#F)
		  (string-set! string target (string-ref string source))
		  (find-next (fix:+ target 1) (fix:+ source 1)))
		 ((TOO-SHORT)
		  ;; Pointers not in sync: buffer ends in what might
		  ;; be the middle of a translation sequence.
		  (do ((target* target (fix:+ target* 1))
		       (source source (fix:+ source 1)))
		      ((fix:= source end)
		       (values target target*))
		    (string-set! string target* (string-ref string source))))
		 (else
		  (clobber-loop target (fix:+ source tlen)))))
	      (else
	       (string-set! string target (string-ref string source))
	       (find-next (fix:+ target 1) (fix:+ source 1))))))

    (find-loop start)))

(define (input-buffer/read-char buffer)
  (without-interrupts
   (lambda ()
     (let ((start-index (input-buffer/start-index buffer)))
       (cond ((fix:< start-index (input-buffer/end-index buffer))
	      (set-input-buffer/start-index! buffer (fix:+ start-index 1))
	      (string-ref (input-buffer/string buffer) start-index))
	     ((input-buffer/closed? buffer)
	      eof-object)
	     (else
	      (let ((n (input-buffer/fill buffer)))
		(cond ((not n) #f)
		      ((fix:= n 0) eof-object)
		      (else
		       (set-input-buffer/start-index! buffer 1)
		       (string-ref (input-buffer/string buffer) 0))))))))))

(define (input-buffer/peek-char buffer)
  (without-interrupts
   (lambda ()
     (let ((start-index (input-buffer/start-index buffer)))
       (cond ((fix:< start-index (input-buffer/end-index buffer))
	      (string-ref (input-buffer/string buffer) start-index))
	     ((input-buffer/closed? buffer)
	      eof-object)
	     (else
	      (let ((n (input-buffer/fill buffer)))
		(cond ((not n) #f)
		      ((fix:= n 0) eof-object)
		      (else
		       (string-ref (input-buffer/string buffer) 0))))))))))

(define (input-buffer/discard-char buffer)
  (without-interrupts
   (lambda ()
     (let ((start-index (input-buffer/start-index buffer)))
       (cond ((fix:< start-index (input-buffer/end-index buffer))
	      (set-input-buffer/start-index! buffer (fix:+ start-index 1)))
	     ((input-buffer/open? buffer)
	      (if (let ((n (input-buffer/fill buffer)))
		    (and n
			 (not (fix:= n 0))))
		  (set-input-buffer/start-index! buffer 1))))))))

(define (input-buffer/read-substring buffer string start end)
  (define (transfer-input-buffer index)
    (let ((bstart (input-buffer/start-index buffer))
	  (bend (input-buffer/end-index buffer)))
      (cond ((fix:< bstart bend)
	     (let ((bstring (input-buffer/string buffer))
		   (available (fix:- bend bstart))
		   (needed (- end index)))
	       (if (>= available needed)
		   (begin
		     (let ((bend (fix:+ bstart needed)))
		       (substring-move! bstring bstart bend string index)
		       (set-input-buffer/start-index! buffer bend))
		     end)
		   (begin
		     (substring-move! bstring bstart bend string index)
		     (set-input-buffer/start-index! buffer bend)
		     (if (input-buffer/char-ready? buffer 0)
			 (transfer-input-buffer (+ index available))
			 (+ index available))))))
	    ((input-buffer/closed? buffer)
	     index)
	    (else
	     (read-directly index)))))

  (define (read-directly index)
    (if (and (not (input-buffer/line-translation buffer))
	     (>= (- end index) (input-buffer/size buffer)))
	(let ((n
	       (channel-read (input-buffer/channel buffer) string index end)))
	  (if n
	      (+ index n)
	      (and (not (= index start)) index)))
	(if (input-buffer/fill buffer)
	    (transfer-input-buffer index)
	    (and (not (= index start)) index))))

  (without-interrupts
   (lambda ()
     (let ((index (transfer-input-buffer start)))
       (and index
	    (- index start))))))

(define (input-buffer/read-until-delimiter buffer delimiters)
  (without-interrupts
   (lambda ()
     (if (and (input-buffer/open? buffer)
	      (char-ready? buffer input-buffer/fill-block))
	 (apply string-append
		(let ((string (input-buffer/string buffer)))
		  (let loop ()
		    (let ((start (input-buffer/start-index buffer))
			  (end (input-buffer/end-index buffer)))
		      (let ((delimiter
			     (substring-find-next-char-in-set
			      string start end delimiters)))
			(if delimiter
			    (let ((head (substring string start delimiter)))
			      (set-input-buffer/start-index! buffer
							     delimiter)
			      (list head))
			    (let ((head (substring string start end)))
			      (set-input-buffer/start-index! buffer end)
			      (cons head
				    (if (input-buffer/fill-block buffer)
					(loop)
					'())))))))))
	 eof-object))))

(define (input-buffer/discard-until-delimiter buffer delimiters)
  (without-interrupts
   (lambda ()
     (if (and (input-buffer/open? buffer)
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
			   (loop))))))))))))

(define (input-buffer/fill-block buffer)
  (fix:> (let loop () (or (input-buffer/fill buffer) (loop))) 0))

(define (input-buffer/buffer-contents buffer)
  (without-interrupts
   (lambda ()
     (and (fix:< (input-buffer/start-index buffer)
		 (input-buffer/end-index buffer))
	  (substring (input-buffer/string buffer)
		     (input-buffer/start-index buffer)
		     (input-buffer/end-index buffer))))))

(define (input-buffer/set-buffer-contents buffer contents)
  (without-interrupts
   (lambda ()
     (let ((contents-size (string-length contents)))
       (if (fix:> contents-size 0)
	   (let ((string (input-buffer/string buffer)))
	     (if (fix:> contents-size (string-length string))
		 (input-buffer/set-size buffer contents-size))
	     (substring-move! contents 0 contents-size string 0)
	     (input-buffer/after-fill! buffer contents-size)))))))

;;;; Select registry

(define have-select?)
(define select-registry-finalizer)
(define select-registry-result-vectors)

(define (initialize-select-registry!)
  (set! have-select? ((ucode-primitive have-select? 0)))
  (set! select-registry-finalizer
	(make-gc-finalizer (ucode-primitive deallocate-select-registry 1)))
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
     (let ((handle ((ucode-primitive allocate-select-registry 0))))
       (let ((registry (%make-select-registry handle)))
	 (add-to-gc-finalizer! select-registry-finalizer registry handle)
	 registry)))))

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

(define (test-select-descriptor descriptor block? mode)
  (let ((result
	 ((ucode-primitive test-select-descriptor 3)
	  descriptor
	  block?
	  (encode-select-registry-mode mode))))
    (if (>= result 0)
	(cond ((fix:= 8 (fix:and 8 result)) 'HANGUP)
	      ((fix:= 4 (fix:and 4 result)) 'ERROR)
	      (else
	       (if (fix:= 1 (fix:and 1 result))
		   (if (fix:= 2 (fix:and 2 result)) 'READ/WRITE 'READ)
		   (if (fix:= 2 (fix:and 2 result)) 'WRITE #f))))
	(case result
	  ((-1) 'INTERRUPT)
	  ((-2)
	   (subprocess-global-status-tick)
	   'PROCESS-STATUS-CHANGE)
	  (else
	   (error "Illegal result from TEST-SELECT-DESCRIPTOR:" result))))))

(define (encode-select-registry-mode mode)
  (case mode
    ((READ) 1)
    ((WRITE) 2)
    ((READ/WRITE) 3)
    (else (error:bad-range-argument mode 'ENCODE-SELECT-REGISTRY-MODE))))

(define (test-select-registry registry block?)
  (receive (vr vw) (allocate-select-registry-result-vectors registry)
    (let ((result
	   ((ucode-primitive test-select-registry 4)
	    (select-registry-handle registry)
	    block?
	    vr
	    vw)))
      (if (> result 0)
	  (cons vr vw)
	  (begin
	    (deallocate-select-registry-result-vectors vr vw)
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
	    (let ((vr (caar rv))
		  (vw (cdar rv)))
	      (if (and vr (fix:< n (vector-length vr)))
		  (begin
		    (set-car! (car rv) #f)
		    (set-cdr! (car rv) #f)
		    (set-interrupt-enables! interrupt-mask)
		    (values vr vw))
		  (loop (cdr rv))))
	    (let loop ((m 16))
	      (if (fix:< n m)
		  (begin
		    (set-interrupt-enables! interrupt-mask)
		    (values (make-vector m) (make-vector m)))
		  (loop (fix:* m 2)))))))))

(define (deallocate-select-registry-result-vectors vr vw)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let loop ((rv select-registry-result-vectors))
      (if (pair? rv)
	  (if (caar rv)
	      (loop (cdr rv))
	      (begin
		(set-car! (car rv) vr)
		(set-cdr! (car rv) vw)))
	  (set! select-registry-result-vectors
		(cons (cons vr vw) select-registry-result-vectors))))
    (set-interrupt-enables! interrupt-mask)))