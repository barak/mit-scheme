#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Binary I/O ports
;;; package: (runtime port)

(declare (usual-integrations))

(define-record-type <binary-port>
    (make-binary-port input-buffer output-buffer)
    binary-port?
  (input-buffer port-input-buffer)
  (output-buffer port-output-buffer))

(define (make-binary-input-port source caller)
  (let ((port
	 (make-binary-port (make-input-buffer source caller)
			   #f)))
    (set-source/sink-port! source port)
    port))

(define (make-binary-output-port sink caller)
  (let ((port
	 (make-binary-port #f
			   (make-output-buffer sink caller))))
    (set-source/sink-port! sink port)
    port))

(define (make-binary-i/o-port source sink caller)
  (let ((port
	 (make-binary-port (make-input-buffer source caller)
			   (make-output-buffer sink caller))))
    (set-source/sink-port! source port)
    (set-source/sink-port! sink port)
    port))

(define (binary-input-port? object)
  (and (binary-port? object)
       (port-input-buffer object)
       #t))

(define (binary-output-port? object)
  (and (binary-port? object)
       (port-output-buffer object)
       #t))

(define (binary-i/o-port? object)
  (and (binary-port? object)
       (port-input-buffer object)
       (port-output-buffer object)
       #t))

(add-boot-init!
 (lambda ()
   (register-predicate! binary-input-port? 'binary-input-port
			'<= binary-port?)
   (register-predicate! binary-output-port? 'binary-output-port
			'<= binary-port?)
   (register-predicate! binary-i/o-port? 'binary-i/o-port
			'<= (list binary-input-port? binary-output-port?))))

;;;; Bytevector input ports

(define (open-input-bytevector bytevector #!optional start end)
  (let* ((end
	  (if (default-object? end)
	      (bytevector-length bytevector)
	      (begin
		(guarantee index-fixnum? end 'open-input-bytevector)
		(if (not (fix:<= end (bytevector-length bytevector)))
		    (error:bad-range-argument end 'open-input-bytevector))
		end)))
	 (start
	  (if (default-object? start)
	      0
	      (begin
		(guarantee index-fixnum? start 'open-input-bytevector)
		(if (not (fix:<= start end))
		    (error:bad-range-argument start 'open-input-bytevector))
		start))))
    (make-binary-input-port
     (make-non-channel-input-source
      (lambda ()
	(fix:<= start end))
      (lambda (bv bs be)
	(if (fix:< bs be)
	    (let ((n (fix:min (fix:- end start) (fix:- be bs))))
	      (let ((start* (fix:+ start n)))
		(bytevector-copy! bv bs bytevector start start*)
		(set! start start*))
	      n)
	    0)))
     'open-input-bytevector)))

;;;; Bytevector output ports

(define (open-output-bytevector #!optional initial-size)
  (let* ((size
	  (if (or (default-object? initial-size)
		  (eqv? 0 initial-size))
	      64
	      (begin
		(guarantee index-fixnum? initial-size 'open-input-bytevector)
		initial-size)))
	 (bytevector (make-bytevector size))
	 (index 0))
    (make-binary-output-port
     (make-non-channel-output-sink
      (lambda (bv bs be)
	(let ((index* (fix:+ index (fix:- be bs))))
	  (let ((size*
		 (let loop ((size* size))
		   (if (fix:<= index* size*)
		       size*
		       (loop (fix:* 2 size*))))))
	    (if (fix:> size* size)
		(let ((bytevector* (make-bytevector size*)))
		  (bytevector-copy! bytevector* 0 bytevector 0 index)
		  (set! size size*)
		  (set! bytevector bytevector*))))
	  (bytevector-copy! bytevector index bv bs be)
	  (set! index index*)
	  (fix:- be bs)))
      bytevector-output-port-tag
      (lambda ()
	(bytevector-copy bytevector 0 index)))
     'open-output-bytevector)))

(define (get-output-bytevector port)
  (guarantee bytevector-output-port? port 'get-output-bytevector)
  ((output-sink-custom-ref (buffer-source/sink (port-output-buffer port)) 1)))

(define (bytevector-output-port? object)
  (and (binary-output-port? object)
       (let ((sink (buffer-source/sink (port-output-buffer object))))
	 (and (fix:= (output-sink-custom-length sink) 2)
	      (eq? bytevector-output-port-tag
		   (output-sink-custom-ref sink 0))))))

(define bytevector-output-port-tag
  (list 'bytevector-output-port-tag))

(add-boot-init!
 (lambda ()
   (register-predicate! bytevector-output-port? 'bytevector-output-port
			'<= binary-output-port?)))

;;;; Closing operations

(define (close-binary-port port)
  (let ((ib (port-input-buffer port))
	(ob (port-output-buffer port)))
    (if ib (close-input-buffer ib))
    (if ob (close-output-buffer ob))
    (let ((ic (and ib (buffer-channel ib)))
	  (oc (and ob (buffer-channel ob))))
      (cond (ic
	     (channel-close ic)
	     (if (and oc (not (eqv? oc ic)))
		 (channel-close oc)))
	    (oc
	     (channel-close oc))))))

(define (close-binary-input-port port)
  (let ((ib (port-input-buffer port)))
    (if (not ib)
	(error:not-a binary-input-port? port 'close-input-port))
    (close-input-buffer ib)
    (let ((ic (buffer-channel ib)))
      (if (and ic
	       (let ((ob (port-output-buffer port)))
		 (or (not ob)
		     (and (eqv? ic (buffer-channel ob))
			  (buffer-marked-closed? ob)))))
	  (channel-close ic)))))

(define (close-binary-output-port port)
  (let ((ob (port-output-buffer port)))
    (if (not ob)
	(error:not-a binary-output-port? port 'close-output-port))
    (close-output-buffer ob)
    (let ((oc (buffer-channel ob)))
      (if (and oc
	       (let ((ib (port-input-buffer port)))
		 (or (not ib)
		     (and (eqv? oc (buffer-channel ib))
			  (buffer-marked-closed? ib)))))
	  (channel-close oc)))))

;;;; Input operations

(define (binary-input-port-open? port)
  (let ((ib (port-input-buffer port)))
    (if (not ib)
	(error:not-a binary-input-port? port 'input-port-open?))
    (buffer-open? ib)))

(define (check-input-port port caller)
  (let* ((port (if (default-object? port) (current-input-port) port))
	 (ib (port-input-buffer port)))
    (if (not ib)
	(error:not-a binary-input-port? port caller))
    (if (not (buffer-open? ib))
	(error:bad-range-argument port caller))
    ib))

(define (u8-ready? #!optional port)
  (let ((ib (check-input-port port 'u8-ready?)))
    (or (not (eq? 'unfilled (input-buffer-state ib 'u8-ready?)))
	(and (source-has-bytes? (buffer-source/sink ib))
	     (fill-input-buffer! ib)
	     #t))))

(define (read-u8 #!optional port)
  (let ((ib (check-input-port port 'read-u8)))
    (let loop ((state (input-buffer-state ib 'read-u8)))
      (case state
	((filled)
	 (let* ((start (buffer-start ib))
		(byte (bytevector-u8-ref (buffer-bytes ib) start)))
	   (set-buffer-start! ib (fix:+ start 1))
	   byte))
	((unfilled) (loop (fill-input-buffer! ib)))
	((eof) (eof-object))
	(else #f)))))

(define (peek-u8 #!optional port)
  (let ((ib (check-input-port port 'peek-u8)))
    (let loop ((state (input-buffer-state ib 'peek-u8)))
      (case state
	((filled)
	 (bytevector-u8-ref (buffer-bytes ib) (buffer-start ib)))
	((unfilled) (loop (fill-input-buffer! ib)))
	((eof) (eof-object))
	(else #f)))))

(define (binary-input-port:buffer-contents port)
  (let ((ib (check-input-port port 'input-port-buffer-contents)))
    (if (eq? 'filled (input-buffer-state ib 'input-port-buffer-contents))
	(bytevector-copy (buffer-bytes ib)
			 (buffer-start ib)
			 (buffer-end ib))
	(make-bytevector 0))))

(define (binary-input-port:set-buffer-contents! port contents)
  (let ((ib (check-input-port port 'set-input-port-buffer-contents!)))
    (if (eq? 'unfilled (input-buffer-state ib 'set-input-port-buffer-contents!))
	(let ((bv (buffer-bytes ib)))
	  (let ((n (fix:min (bytevector-length contents) input-buffer-length)))
	    (bytevector-copy! bv 0 contents 0 n)
	    (set-buffer-start! ib 0)
	    (set-buffer-end! ib n))))))

(define (read-bytevector k #!optional port)
  (guarantee index-fixnum? k 'read-bytevector)
  (let ((ib (check-input-port port 'read-bytevector)))
    (if (fix:> k 0)
	(let ((bytevector (make-bytevector k)))
	  (let ((n (%read-bytevector! ib bytevector 0 k 'read-bytevector)))
	    (cond ((or (not n) (eof-object? n)) n)
		  ((fix:< n k) (bytevector-copy bytevector 0 n))
		  (else bytevector))))
	(make-bytevector 0))))

(define (read-bytevector! bytevector #!optional port start end)
  (let ((ib (check-input-port port 'read-bytevector!))
	(end
	 (if (default-object? end)
	     (bytevector-length bytevector)
	     (begin
	       (guarantee index-fixnum? end 'read-bytevector!)
	       (if (not (fix:<= end (bytevector-length bytevector)))
		   (error:bad-range-argument end 'read-bytevector))
	       end))))
    (let ((start
	   (if (default-object? start)
	       0
	       (begin
		 (guarantee index-fixnum? start 'read-bytevector!)
		 (if (not (fix:<= start end))
		     (error:bad-range-argument start 'read-bytevector!))
		 start))))
      (if (fix:< start end)
	  (%read-bytevector! ib bytevector start end 'read-bytevector!)
	  0))))

(define (%read-bytevector! ib bytevector start end caller)

  (define (read-from-buffer index)
    (let ((bv (buffer-bytes ib))
	  (bs (buffer-start ib))
	  (be (buffer-end ib)))
      (let ((n (fix:min (fix:- end index) (fix:- be bs))))
	(let ((bs* (fix:+ bs n)))
	  (bytevector-copy! bytevector index bv bs bs*)
	  (set-buffer-start! ib bs*)
	  (fix:+ index n)))))

  (define (read-from-source index)
    ;; Always read at least page-size bytes; use the buffer if the caller
    ;; wants less than that.
    (if (fix:< (fix:- end index) page-size)
	(case (fill-input-buffer! ib)
	  ((filled) (fix:- (read-from-buffer index) start))
	  ((eof) (eof index))
	  (else (would-block index)))
	(let ((n (read-bytes! ib bytevector index end)))
	  (cond ((not n) (would-block index))
		((fix:> n 0) (fix:- (fix:+ index n) start))
		(else (eof index))))))

  (define (eof index)
    (if (fix:< start index) (fix:- index start) (eof-object)))

  (define (would-block index)
    (if (fix:< start index) (fix:- index start) #f))

  (case (input-buffer-state ib caller)
    ((filled)
     (let ((index* (read-from-buffer start)))
       (if (fix:< index* end)
	   (read-from-source index*)
	   (fix:- end start))))
    ((unfilled) (read-from-source start))
    (else (eof-object))))

;;;; Input buffers

(define-integrable input-buffer-length page-size)

(define (make-input-buffer source caller)
  (guarantee input-source? source caller)
  (make-buffer source input-buffer-length))

(define (input-buffer-marked-eof? ib)
  (eq? 'eof (buffer-override ib)))

(define (mark-input-buffer-eof! ib)
  (set-buffer-override! ib 'eof))

(define (close-input-buffer ib)
  (if (not (buffer-marked-closed? ib))
      (begin
	(close-buffer ib)
	(mark-buffer-closed! ib))))

(define (input-buffer-state ib caller)
  (if (buffer-marked-closed? ib)
      (error:bad-range-argument (buffer-port ib) caller))
  (cond ((input-buffer-marked-eof? ib) 'eof)
	((fix:< (buffer-start ib) (buffer-end ib)) 'filled)
	(else 'unfilled)))

(define (fill-input-buffer! ib)
  ;; assert (eq? 'unfilled (input-buffer-state ib caller))
  (let ((n (read-bytes! ib (buffer-bytes ib) 0 input-buffer-length)))
    (set-buffer-start! ib 0)
    (set-buffer-end! ib (or n 0))
    (cond ((not n) #f)
	  ((fix:> n 0) 'filled)
	  (else 'eof))))

(define (read-bytes! ib bv bs be)
  ;; assert (eq? 'unfilled (input-buffer-state ib caller))
  (let ((n (source-read-bytes! (buffer-source/sink ib) bv bs be)))
    (if (eqv? n 0)
	(mark-input-buffer-eof! ib))
    n))

;;;; Output operations

(define (binary-output-port-open? port)
  (let ((ob (port-output-buffer port)))
    (if (not ob)
	(error:not-a binary-output-port? port 'output-port-open?))
    (buffer-open? ob)))

(define (check-output-port port caller)
  (let* ((port (if (default-object? port) (current-output-port) port))
	 (ob (port-output-buffer port)))
    (if (not ob)
	(error:not-a binary-output-port? port caller))
    (if (not (buffer-open? ob))
	(error:bad-range-argument port caller))
    ob))

(define (flush-binary-output-port #!optional port)
  (flush-output-buffer (check-output-port port 'flush-output-port)))

(define (write-u8 byte #!optional port)
  (guarantee byte? byte 'write-u8)
  (let ((ob (check-output-port port 'write-u8)))
    (let ((write
	   (lambda ()
	     (let ((bi (buffer-end ob)))
	       (bytevector-u8-set! (buffer-bytes ob) bi byte)
	       (set-buffer-end! ob (fix:+ bi 1))
	       1))))
      (if (fix:> (output-buffer-available ob 'write-u8) 0)
	  (write)
	  (let ((n (drain-output-buffer ob)))
	    (if (and n (fix:> n 0))
		(write)
		n))))))

(define (write-bytevector bytevector #!optional port start end)
  (let ((ob (check-output-port port 'write-bytevector))
	(end
	 (if (default-object? end)
	     (bytevector-length bytevector)
	     (begin
	       (guarantee index-fixnum? end 'read-bytevector!)
	       (if (not (fix:<= end (bytevector-length bytevector)))
		   (error:bad-range-argument end 'read-bytevector))
	       end))))
    (let ((start
	   (if (default-object? start)
	       0
	       (begin
		 (guarantee index-fixnum? start 'read-bytevector!)
		 (if (not (fix:<= start end))
		     (error:bad-range-argument start 'read-bytevector!))
		 start)))
	  (bv (buffer-bytes ob)))

      (define (loop index)
	(let ((remaining (fix:- end index))
	      (available (output-buffer-available ob 'write-bytevector)))
	  (cond ((fix:<= remaining available)
		 (fix:- (write-to-buffer index remaining) start))
		((fix:> available 0)
		 (let ((index* (write-to-buffer index available)))
		   (let ((n (drain-output-buffer ob)))
		     (if (and n (fix:> n 0))
			 (if (fix:< n output-buffer-length)
			     ;; partial drain
			     (loop index*)
			     ;; full drain
			     (write-to-sink index))
			 ;; no progress was made
			 (fix:- index* start)))))
		(else
		 (write-to-sink start)))))

      (define (write-to-buffer index n)
	(let ((bi (buffer-end ob))
	      (index* (fix:+ index n)))
	  (bytevector-copy! bv bi bytevector index index*)
	  (set-buffer-end! ob (fix:+ bi n))
	  index*))

      (define (write-to-sink index)
	(let ((n
	       (sink-write-bytes (buffer-source/sink ob) bytevector index end)))
	  (if (and n (fix:> n 0))
	      (let ((index* (fix:+ index n)))
		(if (fix:< index* end)
		    (write-to-sink index*)
		    (fix:- end start)))
	      (if (fix:< start index)
		  (fix:- index start)
		  n))))

      (loop start))))

;;;; Output buffers

(define-integrable output-buffer-length page-size)

(define (make-output-buffer sink caller)
  (guarantee output-sink? sink caller)
  (make-buffer sink output-buffer-length))

(define (close-output-buffer ob)
  (if (not (buffer-marked-closed? ob))
      (begin
	(flush-output-buffer ob)
	(close-buffer ob)
	(mark-buffer-closed! ob))))

(define (output-buffer-available ob caller)
  (if (buffer-marked-closed? ob)
      (error:bad-range-argument (buffer-port ob) caller))
  (fix:- output-buffer-length
	 (let ((bv (buffer-bytes ob))
	       (bs (buffer-start ob))
	       (be (buffer-end ob)))
	   (if (fix:> bs 0)
	       (let ((be* (fix:- be bs)))
		 (bytevector-copy! bv 0 bv bs be)
		 (set-buffer-start! ob 0)
		 (set-buffer-end! ob be*)
		 be*)
	       be))))

(define (drain-output-buffer ob)
  ;; assert (fix:= 0 (output-buffer-available ob caller))
  ;; implies
  ;;   (and (fix:= (buffer-start ob) 0)
  ;;        (fix:= (buffer-end ob) output-buffer-length))
  (let ((bv (buffer-bytes ob))
	(be (buffer-end ob))
	(sink (buffer-source/sink ob)))
    (let loop ((bi 0))
      (let ((n (sink-write-bytes sink bv bi be)))
	(cond ((and n (fix:> n 0))
	       (let ((bi* (fix:+ bi n)))
		 (if (fix:< bi* be)
		     (begin
		       (set-buffer-start! ob bi*)
		       (loop bi*))
		     (begin
		       (set-buffer-start! ob 0)
		       (set-buffer-end! ob 0)
		       be))))
	      ((fix:> bi 0)
	       (bytevector-copy! bv 0 bv bi be)
	       (set-buffer-start! ob 0)
	       (set-buffer-end! ob (fix:- be bi))
	       bi)
	      (else n))))))

(define (flush-output-buffer ob)
  (let ((bv (buffer-bytes ob))
	(bs (buffer-start ob))
	(be (buffer-end ob))
	(sink (buffer-source/sink ob)))
    (if (fix:< bs be)
	(let loop ((bi bs))
	  (let ((n (sink-write-bytes sink bv bi be)))
	    (if (and n (fix:> n 0))
		(let ((bi* (fix:+ bi n)))
		  (if (fix:< bi* be)
		      (begin
			(set-buffer-start! ob bi*)
			(loop bi*))
		      (begin
			(set-buffer-start! ob 0)
			(set-buffer-end! ob 0)
			output-buffer-length)))
		(fix:- bi bs))))
	0)))

;;;; Buffers

(define-integrable page-size #x1000)

(define-record-type <buffer>
    (%make-buffer source/sink bytes start end override)
    buffer?
  (source/sink buffer-source/sink)
  (bytes buffer-bytes)
  (start buffer-start set-buffer-start!)
  (end buffer-end set-buffer-end!)
  (override buffer-override set-buffer-override!))

(define (make-buffer source/sink buffer-length)
  (%make-buffer source/sink (make-bytevector buffer-length) 0 0 #f))

(define (buffer-channel buffer)
  (source/sink-channel (buffer-source/sink buffer)))

(define (buffer-port buffer)
  ((source/sink-operation:get-port (buffer-source/sink buffer))))

(define (buffer-marked-eof? buffer)
  (eq? 'eof (buffer-override buffer)))

(define (mark-buffer-eof! buffer)
  (set-buffer-override! buffer 'eof))

(define (buffer-marked-closed? buffer)
  (eq? 'closed (buffer-override buffer)))

(define (mark-buffer-closed! buffer)
  (set-buffer-override! buffer 'closed))

(define (buffer-open? buffer)
  (and (not (buffer-marked-closed? buffer))
       (or ((source/sink-operation:open? (buffer-source/sink buffer)))
	   (begin
	     (mark-buffer-closed! buffer)
	     #f))))

(define (close-buffer buffer)
  ((source/sink-operation:close (buffer-source/sink buffer))))

;;;; Sources and sinks

(define-record-type <source/sink>
    (make-source/sink flavor channel get-port set-port! open? close custom)
    source/sink?
  (flavor source/sink-flavor)
  (channel source/sink-channel)
  (get-port source/sink-operation:get-port)
  (set-port! source/sink-operation:set-port!)
  (open? source/sink-operation:open?)
  (close source/sink-operation:close)
  (custom source/sink-custom))

(define (make-channel-ss flavor channel . custom)
  (make-source/sink flavor
		    (lambda () channel)
		    (lambda () (channel-port channel))
		    (lambda (port) (set-channel-port! channel port))
		    (lambda () (channel-open? channel))
		    (lambda () unspecific)
		    (list->vector custom)))

(define (make-non-channel-ss flavor . custom)
  (let ((port #f)
	(open? #t))
    (make-source/sink flavor
		      (lambda () #f)
		      (lambda () port)
		      (lambda (port*) (set! port port*) unspecific)
		      (lambda () open?)
		      (lambda () (set! open? #f) unspecific)
		      (list->vector custom))))

(define (set-source/sink-port! source/sink port)
  ((source/sink-operation:set-port! source/sink) port))

(define (input-source? object)
  (and (source/sink? object)
       (eq? 'source (source/sink-flavor object))))

(define (output-sink? object)
  (and (source/sink? object)
       (eq? 'sink (source/sink-flavor object))))

(add-boot-init!
 (lambda ()
   (register-predicate! input-source? 'input-source '<= source/sink?)
   (register-predicate! output-sink? 'output-sink '<= source/sink?)))

(define (make-channel-input-source channel)
  (make-channel-ss 'source
		   channel
		   (lambda () (channel-has-input? channel))
		   (lambda (bv bs be) (channel-read channel bv bs be))))

(define (make-non-channel-input-source has-bytes? read-bytes! . custom)
  (apply make-non-channel-ss 'source has-bytes? read-bytes! custom))

(define (source-has-bytes? source)
  ((vector-ref (source/sink-custom source) 0)))

(define (source-read-bytes! source bv bs be)
  ((vector-ref (source/sink-custom source) 1) bv bs be))

(define (input-source-custom-length source)
  (fix:- (vector-length (source/sink-custom source)) 2))

(define (input-source-custom-ref source index)
  (vector-ref (source/sink-custom source) (fix:+ index 2)))

(define (make-channel-output-sink channel)
  (make-channel-ss 'sink
		   channel
		   (lambda (bv bs be) (channel-write channel bv bs be))))

(define (make-non-channel-output-sink write-bytes . custom)
  (apply make-non-channel-ss 'sink write-bytes custom))

(define (sink-write-bytes sink bv bs be)
  ((vector-ref (source/sink-custom sink) 0) bv bs be))

(define (output-sink-custom-length sink)
  (fix:- (vector-length (source/sink-custom sink)) 1))

(define (output-sink-custom-ref sink index)
  (vector-ref (source/sink-custom sink) (fix:+ index 1)))