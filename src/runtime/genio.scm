#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Generic I/O Ports
;;; package: (runtime generic-i/o-port)

(declare (usual-integrations)
	 (integrate-external "port"))

(define (make-generic-i/o-port source sink #!optional type . extra-state)
  (if (not (or source sink))
      (error "Missing arguments."))
  (let ((port
	 (make-port (if (default-object? type)
			(generic-i/o-port-type (source-type source)
					       (sink-type sink))
			type)
		    (apply make-gstate source sink 'TEXT 'TEXT extra-state))))
    (let ((ib (port-input-buffer port)))
      (if ib
	  ((source/set-port (input-buffer-source ib)) port)))
    (let ((ob (port-output-buffer port)))
      (if ob
	  ((sink/set-port (output-buffer-sink ob)) port)))
    port))

(define (source-type source)
  (cond ((not source) #f)
	((or (channel? source) ((source/get-channel source))) 'CHANNEL)
	(else #t)))

(define (sink-type sink)
  (cond ((not sink) #f)
	((or (channel? sink) ((sink/get-channel sink))) 'CHANNEL)
	(else #t)))

(define (generic-i/o-port-type source sink)
  (case source
    ((#F)
     (case sink
       ((#F) generic-type00)
       ((CHANNEL) generic-type02)
       (else generic-type01)))
    ((CHANNEL)
     (case sink
       ((#F) generic-type20)
       ((CHANNEL) generic-type22)
       (else generic-type21)))
    (else
     (case sink
       ((#F) generic-type10)
       ((CHANNEL) generic-type12)
       (else generic-type11)))))

(define-structure (gstate (constructor %make-gstate))
  (input-buffer #f read-only #t)
  (output-buffer #f read-only #t)
  coding
  line-ending
  (extra #f read-only #t))

(define (make-gstate source sink coder-name normalizer-name . extra)
  (%make-gstate (and source
		     (make-input-buffer (->source source 'MAKE-GSTATE)
					coder-name
					normalizer-name))
		(and sink
		     (make-output-buffer (->sink sink 'MAKE-GSTATE)
					 coder-name
					 normalizer-name))
		coder-name
		normalizer-name
		(list->vector extra)))

(define-integrable (port-input-buffer port)
  (gstate-input-buffer (port/%state port)))

(define-integrable (port-output-buffer port)
  (gstate-output-buffer (port/%state port)))

(define (generic-i/o-port-accessor index)
  (guarantee-index-fixnum index 'GENERIC-I/O-PORT-ACCESSOR)
  (lambda (port)
    (let ((extra (gstate-extra (port/%state port))))
      (if (not (fix:< index (vector-length extra)))
	  (error "Accessor index out of range:" index))
      (vector-ref extra index))))

(define (generic-i/o-port-modifier index)
  (guarantee-index-fixnum index 'GENERIC-I/O-PORT-MODIFIER)
  (lambda (port object)
    (let ((extra (gstate-extra (port/%state port))))
      (if (not (fix:< index (vector-length extra)))
	  (error "Accessor index out of range:" index))
      (vector-set! extra index object))))

(define (initialize-package!)
  (let ((ops:in1
	 `((CHAR-READY? ,generic-io/char-ready?)
	   (CLOSE-INPUT ,generic-io/close-input)
	   (EOF? ,generic-io/eof?)
	   (INPUT-LINE ,generic-io/input-line)
	   (INPUT-OPEN? ,generic-io/input-open?)
	   (PEEK-CHAR ,generic-io/peek-char)
	   (READ-CHAR ,generic-io/read-char)
	   (READ-SUBSTRING ,generic-io/read-substring)
	   (UNREAD-CHAR ,generic-io/unread-char)))
	(ops:in2
	 `((INPUT-BLOCKING-MODE ,generic-io/input-blocking-mode)
	   (INPUT-CHANNEL ,generic-io/input-channel)
	   (INPUT-TERMINAL-MODE ,generic-io/input-terminal-mode)
	   (SET-INPUT-BLOCKING-MODE ,generic-io/set-input-blocking-mode)
	   (SET-INPUT-TERMINAL-MODE ,generic-io/set-input-terminal-mode)))
	(ops:out1
	 `((BUFFERED-OUTPUT-BYTES ,generic-io/buffered-output-bytes)
	   (BYTES-WRITTEN ,generic-io/bytes-written)
	   (CLOSE-OUTPUT ,generic-io/close-output)
	   (FLUSH-OUTPUT ,generic-io/flush-output)
	   (OUTPUT-COLUMN ,generic-io/output-column)
	   (OUTPUT-OPEN? ,generic-io/output-open?)
	   (WRITE-CHAR ,generic-io/write-char)
	   (WRITE-SUBSTRING ,generic-io/write-substring)))
	(ops:out2
	 `((OUTPUT-BLOCKING-MODE ,generic-io/output-blocking-mode)
	   (OUTPUT-CHANNEL ,generic-io/output-channel)
	   (OUTPUT-TERMINAL-MODE ,generic-io/output-terminal-mode)
	   (SET-OUTPUT-BLOCKING-MODE ,generic-io/set-output-blocking-mode)
	   (SET-OUTPUT-TERMINAL-MODE ,generic-io/set-output-terminal-mode)
	   (SYNCHRONIZE-OUTPUT ,generic-io/synchronize-output)))
	(other-operations
	 `((CLOSE ,generic-io/close)
	   (CODING ,generic-io/coding)
	   (KNOWN-CODING? ,generic-io/known-coding?)
	   (KNOWN-CODINGS ,generic-io/known-codings)
	   (KNOWN-LINE-ENDING? ,generic-io/known-line-ending?)
	   (KNOWN-LINE-ENDINGS ,generic-io/known-line-endings)
	   (LINE-ENDING ,generic-io/line-ending)
	   (OPEN? ,generic-io/open?)
	   (SET-CODING ,generic-io/set-coding)
	   (SET-LINE-ENDING ,generic-io/set-line-ending)
	   (SUPPORTS-CODING? ,generic-io/supports-coding?)
	   (WRITE-SELF ,generic-io/write-self))))
    (let ((make-type
	   (lambda ops
	     (make-port-type (append (apply append ops)
				     other-operations)
			     #f))))
      (set! generic-type00 (make-type))
      (set! generic-type10 (make-type ops:in1))
      (set! generic-type20 (make-type ops:in1 ops:in2))
      (set! generic-type01 (make-type ops:out1))
      (set! generic-type02 (make-type ops:out1 ops:out2))
      (set! generic-type11 (make-type ops:in1 ops:out1))
      (set! generic-type21 (make-type ops:in1 ops:in2 ops:out1))
      (set! generic-type12 (make-type ops:in1 ops:out1 ops:out2))
      (set! generic-type22 (make-type ops:in1 ops:in2 ops:out1 ops:out2))))
  (initialize-name-maps!)
  (initialize-conditions!))

(define generic-type00)
(define generic-type10)
(define generic-type20)
(define generic-type01)
(define generic-type02)
(define generic-type11)
(define generic-type21)
(define generic-type12)
(define generic-type22)

;;;; Input operations

(define (generic-io/char-ready? port)
  (buffer-has-input? (port-input-buffer port)))

(define (generic-io/peek-char port)
  (let* ((ib (port-input-buffer port))
	 (line (input-buffer-line ib))
	 (char (generic-io/read-char port)))
    (if (char? char)
	;; Undo effect of read-char.
	(begin
	  (set-input-buffer-line! ib line)
	  (set-input-buffer-start! ib (input-buffer-prev ib))))
    char))

(define (generic-io/read-char port)
  (let ((ib (port-input-buffer port)))
    (reset-prev-char ib)
    (let loop ()
      (or (read-next-char ib)
	  (let ((r (fill-input-buffer ib)))
	    (case r
	      ((OK) (loop))
	      ((WOULD-BLOCK) #f)
	      ((EOF) (eof-object))
	      (else (error "Unknown result:" r))))))))

(define (generic-io/unread-char port char)
  (let ((ib (port-input-buffer port)))
    (let ((bp (input-buffer-prev ib)))
      (if (not (fix:< bp (input-buffer-start ib)))
	  (error "No char to unread:" port))
      ;; If unreading a newline, decrement the line count.
      (if (char=? char #\newline)
	  (set-input-buffer-line! ib (fix:- (input-buffer-line ib) 1)))
      (set-input-buffer-start! ib bp))))

(define (generic-io/read-substring port string start end)
  (read-substring (port-input-buffer port) string start end))

(define (generic-io/input-line port)
  (input-buffer-line (port-input-buffer port)))

(define (generic-io/eof? port)
  (input-buffer-at-eof? (port-input-buffer port)))

(define (generic-io/input-channel port)
  (let ((ib (port-input-buffer port)))
    (if (not ib)
	(error:bad-range-argument port #f))
    (input-buffer-channel ib)))

(define (generic-io/input-blocking-mode port)
  (let ((channel (generic-io/input-channel port)))
    (if channel
	(if (channel-blocking? channel) 'BLOCKING 'NONBLOCKING)
	#f)))

(define (generic-io/set-input-blocking-mode port mode)
  (let ((channel (generic-io/input-channel port)))
    (if channel
	(case mode
	  ((BLOCKING) (channel-blocking channel))
	  ((NONBLOCKING) (channel-nonblocking channel))
	  (else (error:wrong-type-datum mode "blocking mode"))))))

(define (generic-io/input-terminal-mode port)
  (let ((channel (generic-io/input-channel port)))
    (if (and channel (channel-type=terminal? channel))
	(if (terminal-cooked-input? channel) 'COOKED 'RAW)
	#f)))

(define (generic-io/set-input-terminal-mode port mode)
  (let ((channel (generic-io/input-channel port)))
    (if (and channel (channel-type=terminal? channel))
	(case mode
	  ((COOKED) (terminal-cooked-input channel))
	  ((RAW) (terminal-raw-input channel))
	  ((#F) unspecific)
	  (else (error:wrong-type-datum mode "terminal mode"))))))

;;;; Output operations

(define (generic-io/write-char port char)
  (let ((ob (port-output-buffer port)))
    (let loop ()
      (if (write-next-char ob char)
	  1
	  (let ((n (drain-output-buffer ob)))
	    (if (and n (fix:> n 0))
		(loop)
		n))))))

(define (generic-io/write-substring port string start end)
  (write-substring (port-output-buffer port) string start end))

(define (generic-io/flush-output port)
  (force-drain-output-buffer (port-output-buffer port)))

(define (generic-io/output-column port)
  (output-buffer-column (port-output-buffer port)))

(define (generic-io/output-channel port)
  (let ((ob (port-output-buffer port)))
    (if (not ob)
	(error:bad-range-argument port #f))
    (output-buffer-channel ob)))

(define (generic-io/output-blocking-mode port)
  (let ((channel (generic-io/output-channel port)))
    (if channel
	(if (channel-blocking? channel) 'BLOCKING 'NONBLOCKING)
	#f)))

(define (generic-io/set-output-blocking-mode port mode)
  (let ((channel (generic-io/output-channel port)))
    (if channel
	(case mode
	  ((BLOCKING) (channel-blocking channel))
	  ((NONBLOCKING) (channel-nonblocking channel))
	  (else (error:wrong-type-datum mode "blocking mode"))))))

(define (generic-io/output-terminal-mode port)
  (let ((channel (generic-io/output-channel port)))
    (if (and channel (channel-type=terminal? channel))
	(if (terminal-cooked-output? channel) 'COOKED 'RAW)
	#f)))

(define (generic-io/set-output-terminal-mode port mode)
  (let ((channel (generic-io/output-channel port)))
    (if (and channel (channel-type=terminal? channel))
	(case mode
	  ((COOKED) (terminal-cooked-output channel))
	  ((RAW) (terminal-raw-output channel))
	  ((#F) unspecific)
	  (else (error:wrong-type-datum mode "terminal mode"))))))

(define (generic-io/synchronize-output port)
  (let ((channel (generic-io/output-channel port)))
    (if channel
	(channel-synchronize channel))))

(define (generic-io/buffered-output-bytes port)
  (output-buffer-start (port-output-buffer port)))

(define (generic-io/bytes-written port)
  (output-buffer-total (port-output-buffer port)))

;;;; Non-specific operations

(define (generic-io/close port)
  (maybe-close-input port)
  (maybe-close-output port)
  (maybe-close-channels port))

(define (generic-io/close-output port)
  (maybe-close-output port)
  (maybe-close-channels port))

(define (generic-io/close-input port)
  (maybe-close-input port)
  (maybe-close-channels port))

(define (maybe-close-input port)
  (let ((ib (port-input-buffer port)))
    (if ib
	(close-input-buffer ib))))

(define (maybe-close-output port)
  (let ((ob (port-output-buffer port)))
    (if ob
	(close-output-buffer ob))))

(define (maybe-close-channels port)
  (let ((ib (port-input-buffer port))
	(ob (port-output-buffer port)))
    (let ((ic (and ib (input-buffer-channel ib)))
	  (oc (and ob (output-buffer-channel ob))))
      (if (and ic (eq? ic oc))
	  (if (and (not (%input-buffer-open? ib))
		   (not (%output-buffer-open? ob)))
	      (channel-close ic))
	  (begin
	    (if (and ic (not (%input-buffer-open? ib)))
		(channel-close ic))
	    (if (and oc (not (%output-buffer-open? ob)))
		(channel-close oc)))))))

(define (generic-io/output-open? port)
  (let ((ob (port-output-buffer port)))
    (and ob
	 (output-buffer-open? ob))))

(define (generic-io/input-open? port)
  (let ((ib (port-input-buffer port)))
    (and ib
	 (input-buffer-open? ib))))

(define (generic-io/open? port)
  (and (let ((ib (port-input-buffer port)))
	 (if ib
	     (input-buffer-open? ib)
	     #t))
       (let ((ob (port-output-buffer port)))
	 (if ob
	     (output-buffer-open? ob)
	     #t))))

(define (generic-io/write-self port output-port)
  (cond ((i/o-port? port)
	 (write-string " for channels: " output-port)
	 (write (generic-io/input-channel port) output-port)
	 (write-string " " output-port)
	 (write (generic-io/output-channel port) output-port))
	((input-port? port)
	 (write-string " for channel: " output-port)
	 (write (generic-io/input-channel port) output-port))
	((output-port? port)
	 (write-string " for channel: " output-port)
	 (write (generic-io/output-channel port) output-port))))

(define (generic-io/supports-coding? port)
  port
  #t)

(define (generic-io/coding port)
  (gstate-coding (port/%state port)))

(define (generic-io/set-coding port name)
  (let ((state (port/%state port)))
    (let ((ib (gstate-input-buffer state)))
      (if ib
	  (set-input-buffer-coding! ib name)))
    (let ((ob (gstate-output-buffer state)))
      (if ob
	  (set-output-buffer-coding! ob name)))
    (set-gstate-coding! state name)))

(define (generic-io/known-coding? port coding)
  (and (if (input-port? port) (known-input-port-coding? coding) #t)
       (if (output-port? port) (known-output-port-coding? coding) #t)))

(define (generic-io/known-codings port)
  (cond ((i/o-port? port)
	 (eq-intersection (known-input-port-codings)
			  (known-output-port-codings)))
	((input-port? port) (known-input-port-codings))
	((output-port? port) (known-output-port-codings))
	(else '())))

(define (generic-io/line-ending port)
  (gstate-line-ending (port/%state port)))

(define (generic-io/set-line-ending port name)
  (let ((state (port/%state port)))
    (let ((ib (gstate-input-buffer state)))
      (if ib
	  (set-input-buffer-line-ending!
	   ib
	   (line-ending (input-buffer-channel ib) name #f))))
    (let ((ob (gstate-output-buffer state)))
      (if ob
	  (set-output-buffer-line-ending!
	   ob
	   (line-ending (output-buffer-channel ob) name #t))))
    (set-gstate-line-ending! state name)))

(define (generic-io/known-line-ending? port line-ending)
  (and (if (input-port? port) (known-input-line-ending? line-ending) #t)
       (if (output-port? port) (known-output-line-ending? line-ending) #t)))

(define (generic-io/known-line-endings port)
  (cond ((i/o-port? port)
	 (eq-intersection (known-input-line-endings)
			  (known-output-line-endings)))
	((input-port? port) (known-input-line-endings))
	((output-port? port) (known-output-line-endings))
	(else '())))

(define (line-ending channel name for-output?)
  (guarantee-symbol name #f)
  (if (and for-output?
	   (known-input-line-ending? name)
	   (not (known-output-line-ending? name)))
      (if (and channel (eq? (channel-type channel) 'TCP-STREAM-SOCKET))
	  'CRLF
	  (default-line-ending))
      name))

(define (eq-intersection a b)
  (let loop ((a a))
    (cond ((not (pair? a)) '())
	  ((memq (car a) b) (cons (car a) (loop (cdr a))))
	  (else (loop (cdr a))))))

;;;; Name maps

(define-syntax define-name-map
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL) (cdr form))
	 (let ((sing (cadr form)))
	   (let ((plur (symbol sing 'S))
		 (proc (symbol 'DEFINE- sing)))
	     (let ((rev (symbol plur '-REVERSE))
		   (aliases (symbol sing '-ALIASES))
		   (aproc (symbol proc '-ALIAS)))
	       `(BEGIN
		  (DEFINE ,plur '())
		  (DEFINE ,rev)
		  (DEFINE ,aliases '())
		  (DEFINE (,proc NAME ,sing)
		    (SET! ,plur (CONS (CONS NAME ,sing) ,plur))
		    NAME)
		  (DEFINE (,(symbol proc '/POST-BOOT) NAME ,sing)
		    (LET ((OLD (HASH-TABLE/GET ,plur NAME #F)))
		      (IF OLD
			  (HASH-TABLE/REMOVE! ,rev OLD)))
		    (HASH-TABLE/PUT! ,plur NAME ,sing)
		    (HASH-TABLE/PUT! ,rev ,sing NAME))
		  (DEFINE (,aproc NAME ALIAS)
		    (SET! ,aliases (CONS (CONS NAME ALIAS) ,aliases))
		    NAME)
		  (DEFINE (,(symbol aproc '/POST-BOOT) NAME ALIAS)
		    (HASH-TABLE/PUT! ,aliases NAME ALIAS))
		  (DEFINE (,(symbol 'NAME-> sing) NAME)
		    (LET LOOP ((NAME NAME))
		      (LET ((ALIAS (HASH-TABLE/GET ,aliases NAME #F)))
			(COND ((SYMBOL? ALIAS) (LOOP ALIAS))
			      ((PROCEDURE? ALIAS) (LOOP (ALIAS)))
			      ((HASH-TABLE/GET ,plur NAME #F))
			      (else (ERROR:BAD-RANGE-ARGUMENT NAME #F))))))))))
	 (ill-formed-syntax form)))))

(define-name-map decoder)
(define-name-map encoder)
(define-name-map sizer)
(define-name-map normalizer)
(define-name-map denormalizer)

(define (known-input-port-coding? name)
  (or (hash-table/get decoder-aliases name #f)
      (hash-table/get decoders name #f)))

(define (known-input-port-codings)
  (append (hash-table/key-list decoder-aliases)
	  (hash-table/key-list decoders)))

(define (known-output-port-coding? name)
  (or (hash-table/get encoder-aliases name #f)
      (hash-table/get encoders name #f)))

(define (known-output-port-codings)
  (append (hash-table/key-list encoder-aliases)
	  (hash-table/key-list encoders)))

(define (known-input-line-ending? name)
  (or (hash-table/get normalizer-aliases name #f)
      (hash-table/get normalizers name #f)))

(define (known-input-line-endings)
  (append (hash-table/key-list normalizer-aliases)
	  (hash-table/key-list normalizers)))

(define (known-output-line-ending? name)
  (or (hash-table/get denormalizer-aliases name #f)
      (hash-table/get denormalizers name #f)))

(define (known-output-line-endings)
  (append (hash-table/key-list denormalizer-aliases)
	  (hash-table/key-list denormalizers)))

(define (initialize-name-maps!)
  (let ((convert-reverse
	 (lambda (alist)
	   (let ((table (make-strong-eq-hash-table)))
	     (for-each (lambda (n.d)
			 (hash-table/put! table (cdr n.d) (car n.d)))
		       alist)
	     table)))
	(convert-forward
	 (lambda (alist)
	   (let ((table (make-strong-eq-hash-table)))
	     (for-each (lambda (n.d)
			 (hash-table/put! table (car n.d) (cdr n.d)))
		       alist)
	     table))))
    (let-syntax
	((initialize-name-map
	  (sc-macro-transformer
	   (lambda (form environment)
	     environment
	     (if (syntax-match? '(SYMBOL) (cdr form))
		 (let ((sing (cadr form)))
		   (let ((plur (symbol sing 'S))
			 (aliases (symbol sing '-ALIASES))
			 (proc (symbol 'DEFINE- sing)))
		     (let ((aproc (symbol proc '-ALIAS)))
		       `(BEGIN
			  (SET! ,(symbol plur '-REVERSE)
				(CONVERT-REVERSE ,plur))
			  (SET! ,plur (CONVERT-FORWARD ,plur))
			  (SET! ,proc ,(symbol proc '/POST-BOOT))
			  (SET! ,aliases (CONVERT-FORWARD ,aliases))
			  (SET! ,aproc ,(symbol aproc '/POST-BOOT))))))
		 (ill-formed-syntax form))))))
      (initialize-name-map decoder)
      (initialize-name-map encoder)
      (initialize-name-map sizer)
      (initialize-name-map normalizer)
      (initialize-name-map denormalizer)))
  (set! binary-decoder (name->decoder 'BINARY))
  (set! binary-encoder (name->encoder 'BINARY))
  (set! binary-sizer (name->sizer 'BINARY))
  (set! binary-normalizer (name->normalizer 'BINARY))
  (set! binary-denormalizer (name->denormalizer 'BINARY))
  unspecific)

(define binary-decoder)
(define binary-encoder)
(define binary-sizer)
(define binary-normalizer)
(define binary-denormalizer)

(define (define-coding-aliases name aliases)
  (for-each (lambda (alias)
	      (define-decoder-alias alias name)
	      (define-encoder-alias alias name)
	      (define-sizer-alias alias name))
	    aliases))

(define (primary-input-port-codings)
  (cons 'US-ASCII (hash-table/key-list decoders)))

(define (primary-output-port-codings)
  (cons 'US-ASCII (hash-table/key-list encoders)))

;;;; Byte sources

(define-structure (source (constructor make-gsource) (conc-name source/))
  (get-channel #f read-only #t)
  (get-port #f read-only #t)
  (set-port #f read-only #t)
  (open? #f read-only #t)
  (close #f read-only #t)
  (has-bytes? #f read-only #t)
  (read #f read-only #t))

(define-guarantee source "byte source")

(define (->source object #!optional caller)
  (if (channel? object)
      (make-channel-source object)
      (begin
	(guarantee-source object caller)
	object)))

(define (make-channel-source channel)
  (make-gsource (lambda () channel)
		(lambda () (channel-port channel))
		(lambda (port) (set-channel-port! channel port))
		(lambda () (channel-open? channel))
		(lambda () ;; channel-close provided by maybe-close-channels
		  unspecific)
		(lambda () (channel-has-input? channel))
		(lambda (string start end)
		  (channel-read channel string start end))))

(define (make-non-channel-port-source has-bytes? read-bytes)
  (let ((port #f)
	(open? #t))
    (make-gsource (lambda () #f)
		  (lambda () port)
		  (lambda (port*) (set! port port*) unspecific)
		  (lambda () open?)
		  (lambda () (set! open? #f) unspecific)
		  has-bytes?
		  read-bytes)))

;;;; Byte Sinks

(define-structure (sink (constructor make-gsink) (conc-name sink/))
  (get-channel #f read-only #t)
  (get-port #f read-only #t)
  (set-port #f read-only #t)
  (open? #f read-only #t)
  (close #f read-only #t)
  (write #f read-only #t))

(define-guarantee sink "byte sink")

(define (->sink object #!optional caller)
  (if (channel? object)
      (make-channel-sink object)
      (begin
	(guarantee-sink object caller)
	object)))

(define (make-channel-sink channel)
  (make-gsink (lambda () channel)
	      (lambda () (channel-port channel))
	      (lambda (port) (set-channel-port! channel port))
	      (lambda () (channel-open? channel))
	      (lambda () ;; channel-close provided by maybe-close-channels
		unspecific)
	      (lambda (string start end)
		(channel-write channel string start end))))

(define (make-non-channel-port-sink write-bytes)
  (let ((port #f)
	(open? #t))
    (make-gsink (lambda () #f)
		(lambda () port)
		(lambda (port*) (set! port port*) unspecific)
		(lambda () open?)
		(lambda () (set! open? #f) unspecific)
		write-bytes)))

;;;; Input buffer

(define-integrable page-size #x1000)
(define-integrable max-char-bytes 4)

(define-integrable byte-buffer-length
  (fix:+ page-size
	 (fix:- (fix:* max-char-bytes 4) 1)))

(define-structure (input-buffer (constructor %make-input-buffer))
  (source #f read-only #t)
  (bytes #f read-only #t)
  prev
  start
  end
  decode
  normalize
  line
  compute-encoded-character-size)

(define (make-input-buffer source coder-name normalizer-name)
  (%make-input-buffer source
		      (make-string byte-buffer-length)
		      byte-buffer-length
		      byte-buffer-length
		      byte-buffer-length
		      (name->decoder coder-name)
		      (name->normalizer
		       (line-ending ((source/get-channel source))
				    normalizer-name
				    #f))
		      0
		      (name->sizer coder-name)))

(define (input-buffer-open? ib)
  (and (%input-buffer-open? ib)
       ((source/open? (input-buffer-source ib)))))

(define (%input-buffer-open? ib)
  (fix:>= (input-buffer-end ib) 0))

(define (clear-input-buffer ib)
  (set-input-buffer-prev! ib byte-buffer-length)
  (set-input-buffer-start! ib byte-buffer-length)
  (set-input-buffer-end! ib byte-buffer-length))

(define (close-input-buffer ib)
  ((source/close (input-buffer-source ib)))
  (set-input-buffer-line! ib -1)
  (set-input-buffer-prev! ib -1)
  (set-input-buffer-start! ib -1)
  (set-input-buffer-end! ib -1))

(define (input-buffer-channel ib)
  ((source/get-channel (input-buffer-source ib))))

(define (input-buffer-port ib)
  ((source/get-port (input-buffer-source ib))))

(define (input-buffer-at-eof? ib)
  (or (fix:<= (input-buffer-end ib) 0)
      (and (fix:= (input-buffer-prev ib) 0)
	   (fix:= (input-buffer-start ib) (input-buffer-end ib)))))

(define (input-buffer-encoded-character-size ib char)
  ((input-buffer-compute-encoded-character-size ib) ib char))

(define (read-next-char ib)
  (let ((char ((input-buffer-normalize ib) ib)))
    (if (and (char? char)
	     (char=? char #\newline))
	(let ((line (input-buffer-line ib)))
	  (if line
	      (set-input-buffer-line! ib (fix:+ line 1)))))
    char))

(define (decode-char ib)
  (and (fix:< (input-buffer-start ib) (input-buffer-end ib))
       (let ((cp ((input-buffer-decode ib) ib)))
	 (and cp
	      (integer->char cp)))))

(define (reset-prev-char ib)
  (set-input-buffer-prev! ib (input-buffer-start ib)))

(define (set-input-buffer-coding! ib coding)
  (reset-prev-char ib)
  (set-input-buffer-decode! ib (name->decoder coding)))

(define (set-input-buffer-line-ending! ib name)
  (reset-prev-char ib)
  (set-input-buffer-normalize! ib (name->normalizer name)))

(define (input-buffer-using-binary-normalizer? ib)
  (eq? (input-buffer-normalize ib) binary-normalizer))

(define (input-buffer-contents ib)
  (substring (input-buffer-bytes ib)
	     (input-buffer-start ib)
	     (input-buffer-end ib)))

(define (set-input-buffer-contents! ib contents)
  (guarantee-string contents 'SET-INPUT-BUFFER-CONTENTS!)
  (let ((bv (input-buffer-bytes ib)))
    (let ((n (fix:min (string-length contents) (string-length bv))))
      (substring-move! contents 0 n bv 0)
      (set-input-buffer-prev! ib 0)
      (set-input-buffer-start! ib 0)
      (set-input-buffer-end! ib n))))

(define (input-buffer-free-bytes ib)
  (fix:- (input-buffer-end ib)
	 (input-buffer-start ib)))

(define (fill-input-buffer ib)
  (if (input-buffer-at-eof? ib)
      'EOF
      (let ((n (read-bytes ib)))
	(cond ((not n) 'WOULD-BLOCK)
	      ((fix:> n 0) 'OK)
	      (else 'EOF)))))

(define (buffer-has-input? ib)
  (or (next-char-ready? ib)
      (input-buffer-at-eof? ib)
      (and ((source/has-bytes? (input-buffer-source ib)))
	   (begin
	     (read-bytes ib)
	     (next-char-ready? ib)))))

(define (next-char-ready? ib)
  (let ((bl (input-buffer-line ib))
	(bs (input-buffer-start ib)))
    (and (read-next-char ib)
	 (begin
	   (set-input-buffer-line! ib bl)
	   (set-input-buffer-start! ib bs)
	   #t))))

(define (read-bytes ib)
  ;; assumption: (not (input-buffer-at-eof? ib))
  (reset-prev-char ib)
  (let ((bv (input-buffer-bytes ib)))
    (let ((do-read
	   (lambda (be)
	     (let ((be* (fix:+ be page-size)))
	       (if (not (fix:<= be* (vector-8b-length bv)))
		   (error "Input buffer overflow:" ib))
	       ((source/read (input-buffer-source ib)) bv be be*)))))
      (let ((bs (input-buffer-start ib))
	    (be (input-buffer-end ib)))
	(if (fix:< bs be)
	    (begin
	      (if (fix:> bs 0)
		  (do ((i bs (fix:+ i 1))
		       (j 0 (fix:+ j 1)))
		      ((not (fix:< i be))
		       (set-input-buffer-prev! ib 0)
		       (set-input-buffer-start! ib 0)
		       (set-input-buffer-end! ib j))
		    (string-set! bv j (string-ref bv i))))
	      (let ((be (input-buffer-end ib)))
		(let ((n (do-read be)))
		  (if n
		      (set-input-buffer-end! ib (fix:+ be n)))
		  n)))
	    (let ((n (do-read 0)))
	      (if n
		  (begin
		    (set-input-buffer-prev! ib 0)
		    (set-input-buffer-start! ib 0)
		    (set-input-buffer-end! ib n)))
	      n))))))

(define (read-substring ib string start end)
  (reset-prev-char ib)
  (cond ((string? string)
	 (if (input-buffer-in-8-bit-mode? ib)
	     (let ((bv (input-buffer-bytes ib))
		   (bs (input-buffer-start ib))
		   (be (input-buffer-end ib)))
	       (if (fix:< bs be)
		   (let ((n (fix:min (fix:- be bs) (fix:- end start))))
		     (let ((be (fix:+ bs n)))
		       (%substring-move! bv bs be string start)
		       (set-input-buffer-prev! ib be)
		       (set-input-buffer-start! ib be)
		       n))
		   ((source/read (input-buffer-source ib)) string start end)))
	     (read-to-8-bit ib string start end)))
	((wide-string? string)
	 (let ((v (wide-string-contents string)))
	   (let loop ((i start))
	     (cond ((not (fix:< i end))
		    (fix:- i start))
		   ((read-next-char ib)
		    => (lambda (char)
			 (vector-set! v i char)
			 (loop (fix:+ i 1))))
		   ((fix:> i start)
		    (fix:- i start))
		   (else
		    (let ((r (fill-input-buffer ib)))
		      (case r
			((OK) (loop i))
			((WOULD-BLOCK) #f)
			((EOF) 0)
			(else (error "Unknown result:" r)))))))))
	((external-string? string)
	 (if (input-buffer-in-8-bit-mode? ib)
	     (let ((bv (input-buffer-bytes ib))
		   (bs (input-buffer-start ib))
		   (be (input-buffer-end ib)))
	       (if (fix:< bs be)
		   (let ((n (min (fix:- be bs) (- end start))))
		     (let ((be (fix:+ bs n)))
		       (xsubstring-move! bv bs be string start)
		       (set-input-buffer-prev! ib be)
		       (set-input-buffer-start! ib be)
		       n))
		   ((source/read (input-buffer-source ib)) string start end)))
	     (let ((bounce (make-string page-size))
		   (be (min page-size (- end start))))
	       (let ((n (read-to-8-bit ib bounce 0 be)))
		 (if (and n (fix:> n 0))
		     (xsubstring-move! bounce 0 n string start))
		 n))))
	(else
	 (error:not-string string 'INPUT-PORT/READ-SUBSTRING!))))

(define (input-buffer-in-8-bit-mode? ib)
  (and (eq? (input-buffer-decode ib) binary-decoder)
       (eq? (input-buffer-normalize ib) binary-normalizer)))

(define (read-to-8-bit ib string start end)
  (let ((n
	 (let loop ((i start))
	   (if (fix:< i end)
	       (let ((char (read-next-char ib)))
		 (if char
		     (if (fix:< (char->integer char) #x100)
			 (begin
			   (string-set! string i char)
			   (loop (fix:+ i 1)))
			 (error "Character too large for 8-bit string:" char))
		     (fix:- i start)))
	       (fix:- i start)))))
    (if (fix:> n 0)
	n
	(let ((r (fill-input-buffer ib)))
	  (case r
	    ((OK) (read-to-8-bit ib string start end))
	    ((WOULD-BLOCK) #f)
	    ((EOF) 0)
	    (else (error "Unknown result:" r)))))))

;;;; Output buffer

(define-structure (output-buffer (constructor %make-output-buffer))
  (sink #f read-only #t)
  (bytes #f read-only #t)
  start
  total
  encode
  denormalize
  column)

(define (make-output-buffer sink coder-name normalizer-name)
  (%make-output-buffer sink
		       (make-string byte-buffer-length)
		       0
		       0
		       (name->encoder coder-name)
		       (name->denormalizer
			(line-ending ((sink/get-channel sink))
				     normalizer-name
				     #t))
		       0))

(define (output-buffer-open? ob)
  (and (%output-buffer-open? ob)
       ((sink/open? (output-buffer-sink ob)))))

(define (%output-buffer-open? ob)
  (fix:>= (output-buffer-start ob) 0))

(define (close-output-buffer ob)
  (if (output-buffer-open? ob)
      (begin
	(force-drain-output-buffer ob)
	((sink/close (output-buffer-sink ob)))
	(set-output-buffer-start! ob -1))))

(define (output-buffer-channel ob)
  ((sink/get-channel (output-buffer-sink ob))))

(define (output-buffer-port ob)
  ((sink/get-port (output-buffer-sink ob))))

(define-integrable (output-buffer-end ob)
  (string-length (output-buffer-bytes ob)))

(define (flush-output-buffer buffer)
  (set-output-buffer-start! buffer 0))

(define (force-drain-output-buffer ob)
  (let ((channel (output-buffer-channel ob))
	(drain-buffer
	 (lambda ()
	   (let loop ()
	     (drain-output-buffer ob)
	     (if (fix:> (output-buffer-start ob) 0)
		 (loop))))))
    (if channel
	(with-channel-blocking channel #t drain-buffer)
	(drain-buffer))))

(define (drain-output-buffer ob)
  (let ((bs (output-buffer-start ob)))
    (if (fix:> bs 0)
	(let ((bv (output-buffer-bytes ob)))
	  (let ((n
		 ((sink/write (output-buffer-sink ob))
		  bv
		  0
		  (fix:min bs page-size))))
	    (if (and n (fix:> n 0))
		(do ((bi n (fix:+ bi 1))
		     (bj 0 (fix:+ bj 1)))
		    ((not (fix:< bi bs))
		     (set-output-buffer-start! ob bj))
		  (vector-8b-set! bv bj (vector-8b-ref bv bi))))
	    n))
	0)))

(define (write-next-char ob char)
  (and (fix:< (output-buffer-start ob) page-size)
       (begin
	 ((output-buffer-denormalize ob) ob char)
	 (if (char=? char #\newline)
	     (set-output-buffer-column! ob 0)
	     (let ((column (output-buffer-column ob)))
	       (if column
		   (set-output-buffer-column!
		    ob
		    (cond ((char=? char #\tab)
			   (fix:+ column (fix:- 8 (fix:remainder column 8))))
			  ((and (fix:<= #x20 (char->integer char))
				(fix:<= (char->integer char) #x7E))
			   (fix:+ column 1))
			  (else #f))))))
	 #t)))

(define (output-buffer-in-8-bit-mode? ob)
  (and (eq? (output-buffer-encode ob) binary-encoder)
       (eq? (output-buffer-denormalize ob) binary-denormalizer)))

(define (output-buffer-using-binary-denormalizer? ob)
  (eq? (output-buffer-denormalize ob) binary-denormalizer))

(define (encode-char ob char)
  (let ((n-bytes ((output-buffer-encode ob) ob (char->integer char))))
    (set-output-buffer-start! ob (fix:+ (output-buffer-start ob) n-bytes))
    (set-output-buffer-total! ob (fix:+ (output-buffer-total ob) n-bytes))))

(define (set-output-buffer-coding! ob coding)
  (set-output-buffer-encode! ob (name->encoder coding)))

(define (set-output-buffer-line-ending! ob name)
  (set-output-buffer-denormalize! ob (name->denormalizer name)))

(define (write-substring ob string start end)
  (cond ((string? string)
	 (let loop ((i start))
	   (if (fix:< i end)
	       (if (write-next-char ob (string-ref string i))
		   (loop (fix:+ i 1))
		   (let ((n (drain-output-buffer ob)))
		     (cond ((not n) (and (fix:> i start) (fix:- i start)))
			   ((fix:> n 0) (loop i))
			   (else (fix:- i start)))))
	       (fix:- end start))))
	((wide-string? string)
	 (let ((v (wide-string-contents string)))
	   (let loop ((i start))
	     (if (fix:< i end)
		 (if (write-next-char ob (vector-ref v i))
		     (loop (fix:+ i 1))
		     (let ((n (drain-output-buffer ob)))
		       (cond ((not n) (and (fix:> i start) (fix:- i start)))
			     ((fix:> n 0) (loop i))
			     (else (fix:- i start)))))
		 (fix:- end start)))))
	((external-string? string)
	 (let ((bounce (make-string #x1000)))
	   (let loop ((i start))
	     (if (< i end)
		 (let ((n (min (- end i) #x1000)))
		   (xsubstring-move! string i (+ i n) bounce 0)
		   (let ((m (write-substring ob bounce 0 n)))
		     (cond ((not m)
			    (and (> i start)
				 (- i start)))
			   ((fix:> m 0)
			    (if (fix:< m n)
				(- (+ i m) start)
				(loop (+ i n))))
			   (else (- i start)))))
		 (- end start)))))
	(else
	 (error:not-string string 'OUTPUT-PORT/WRITE-SUBSTRING))))

;;;; 8-bit codecs

(define-decoder 'ISO-8859-1
  (lambda (ib)
    (let ((cp (vector-8b-ref (input-buffer-bytes ib) (input-buffer-start ib))))
      (set-input-buffer-start! ib (fix:+ (input-buffer-start ib) 1))
      cp)))

(define-encoder 'ISO-8859-1
  (lambda (ob cp)
    (if (not (fix:< cp #x100))
	(error:char-encoding ob cp))
    (vector-8b-set! (output-buffer-bytes ob) (output-buffer-start ob) cp)
    1))

(define-sizer 'ISO-8859-1
  (lambda (ib cp)
    ib cp
    1))

(define-coding-aliases 'ISO-8859-1
  '(ISO_8859-1:1987 ISO-IR-100 ISO_8859-1 LATIN1 L1 IBM819 CP819 CSISOLATIN1))

(define-coding-aliases 'ISO-8859-1
  '(BINARY TEXT))

(define-coding-aliases 'ISO-8859-1
  ;; Treat US-ASCII like ISO-8859-1.
  '(US-ASCII ANSI_X3.4-1968 ISO-IR-6 ANSI_X3.4-1986 ISO_646.IRV:1991 ASCII
	     ISO646-US US IBM367 CP367 CSASCII))

(define-syntax define-8-bit-codecs
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL + DATUM) (cdr form))
	 (let ((name (cadr form))
	       (start (caddr form))
	       (code-points (cdddr form)))
	   `(BEGIN
	      (DEFINE-DECODER ',name
		(LET ((TABLE
		       #(,@(let loop ((i 0))
			     (if (fix:< i start)
				 (cons i (loop (fix:+ i 1)))
				 code-points)))))
		  (LAMBDA (IB)
		    (DECODE-8-BIT IB TABLE))))
	      (DEFINE-ENCODER ',name
		(RECEIVE (LHS RHS) (REVERSE-ISO-8859-MAP ,start ',code-points)
		  (LAMBDA (OB CP)
		    (ENCODE-8-BIT OB CP ,start LHS RHS))))
	      (DEFINE-SIZER-ALIAS ',name 'ISO-8859-1)))
	 (ill-formed-syntax form)))))

(define (decode-8-bit ib table)
  (let ((cp
	 (vector-ref table
		     (vector-8b-ref (input-buffer-bytes ib)
				    (input-buffer-start ib)))))
    (if cp
	(begin
	  (set-input-buffer-start! ib (fix:+ (input-buffer-start ib) 1))
	  cp)
	(error:char-decoding ib))))

(define (encode-8-bit ob cp start map-lhs map-rhs)
  (vector-8b-set! (input-buffer-bytes ob)
		  (input-buffer-start ob)
		  (if (fix:< cp start)
		      cp
		      (let loop ((low 0) (high (vector-length map-lhs)))
			(if (not (fix:< low high))
			    (error:char-encoding ob cp))
			(let ((i (fix:quotient (fix:+ low high) 2)))
			  (cond ((fix:< cp (vector-ref map-lhs i))
				 (loop low i))
				((fix:> cp (vector-ref map-lhs i))
				 (loop (fix:+ i 1) high))
				(else
				 (vector-8b-ref map-rhs i)))))))
  1)

(define (reverse-iso-8859-map start code-points)
  (let ((n (length code-points)))
    (let ((lhs (make-vector n))
	  (rhs (make-vector-8b n)))
      (do ((alist (sort (let loop ((code-points code-points) (i start))
			  (if (pair? code-points)
			      (if (car code-points)
				  (cons (cons (car code-points) i)
					(loop (cdr code-points) (fix:+ i 1)))
				  (loop (cdr code-points) (fix:+ i 1)))
			      '()))
		    (lambda (a b)
		      (fix:< (car a) (car b))))
		  (cdr alist))
	   (i 0 (fix:+ i 1)))
	  ((not (pair? alist)))
	(vector-set! lhs i (caar alist))
	(vector-8b-set! rhs i (cdar alist)))
      (values lhs rhs))))

(define-8-bit-codecs iso-8859-2 #xA1
  #x0104 #x02D8 #x0141 #x00A4 #x013D #x015A #x00A7 #x00A8
  #x0160 #x015E #x0164 #x0179 #x00AD #x017D #x017B #x00B0
  #x0105 #x02DB #x0142 #x00B4 #x013E #x015B #x02C7 #x00B8
  #x0161 #x015F #x0165 #x017A #x02DD #x017E #x017C #x0154
  #x00C1 #x00C2 #x0102 #x00C4 #x0139 #x0106 #x00C7 #x010C
  #x00C9 #x0118 #x00CB #x011A #x00CD #x00CE #x010E #x0110
  #x0143 #x0147 #x00D3 #x00D4 #x0150 #x00D6 #x00D7 #x0158
  #x016E #x00DA #x0170 #x00DC #x00DD #x0162 #x00DF #x0155
  #x00E1 #x00E2 #x0103 #x00E4 #x013A #x0107 #x00E7 #x010D
  #x00E9 #x0119 #x00EB #x011B #x00ED #x00EE #x010F #x0111
  #x0144 #x0148 #x00F3 #x00F4 #x0151 #x00F6 #x00F7 #x0159
  #x016F #x00FA #x0171 #x00FC #x00FD #x0163 #x02D9)

(define-coding-aliases 'ISO-8859-2
  '(ISO_8859-2:1987 ISO-IR-101 ISO_8859-2 LATIN2 L2 CSISOLATIN2))

(define-8-bit-codecs iso-8859-3 #xA1
  #x0126 #x02D8 #x00A3 #x00A4 #f     #x0124 #x00A7 #x00A8
  #x0130 #x015E #x011E #x0134 #x00AD #f     #x017B #x00B0
  #x0127 #x00B2 #x00B3 #x00B4 #x00B5 #x0125 #x00B7 #x00B8
  #x0131 #x015F #x011F #x0135 #x00BD #f     #x017C #x00C0
  #x00C1 #x00C2 #f     #x00C4 #x010A #x0108 #x00C7 #x00C8
  #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF #f
  #x00D1 #x00D2 #x00D3 #x00D4 #x0120 #x00D6 #x00D7 #x011C
  #x00D9 #x00DA #x00DB #x00DC #x016C #x015C #x00DF #x00E0
  #x00E1 #x00E2 #f     #x00E4 #x010B #x0109 #x00E7 #x00E8
  #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF #f
  #x00F1 #x00F2 #x00F3 #x00F4 #x0121 #x00F6 #x00F7 #x011D
  #x00F9 #x00FA #x00FB #x00FC #x016D #x015D #x02D9)

(define-coding-aliases 'ISO-8859-3
  '(ISO_8859-3:1988 ISO-IR-109 ISO_8859-3 LATIN3 L3 CSISOLATIN3))

(define-8-bit-codecs iso-8859-4 #xA1
  #x0104 #x0138 #x0156 #x00A4 #x0128 #x013B #x00A7 #x00A8
  #x0160 #x0112 #x0122 #x0166 #x00AD #x017D #x00AF #x00B0
  #x0105 #x02DB #x0157 #x00B4 #x0129 #x013C #x02C7 #x00B8
  #x0161 #x0113 #x0123 #x0167 #x014A #x017E #x014B #x0100
  #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x012E #x010C
  #x00C9 #x0118 #x00CB #x0116 #x00CD #x00CE #x012A #x0110
  #x0145 #x014C #x0136 #x00D4 #x00D5 #x00D6 #x00D7 #x00D8
  #x0172 #x00DA #x00DB #x00DC #x0168 #x016A #x00DF #x0101
  #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x012F #x010D
  #x00E9 #x0119 #x00EB #x0117 #x00ED #x00EE #x012B #x0111
  #x0146 #x014D #x0137 #x00F4 #x00F5 #x00F6 #x00F7 #x00F8
  #x0173 #x00FA #x00FB #x00FC #x0169 #x016B #x02D9)

(define-coding-aliases 'ISO-8859-4
  '(ISO_8859-4:1988 ISO-IR-110 ISO_8859-4 LATIN4 L4 CSISOLATIN4))

(define-8-bit-codecs iso-8859-5 #xA1
  #x0401 #x0402 #x0403 #x0404 #x0405 #x0406 #x0407 #x0408
  #x0409 #x040A #x040B #x040C #x00AD #x040E #x040F #x0410
  #x0411 #x0412 #x0413 #x0414 #x0415 #x0416 #x0417 #x0418
  #x0419 #x041A #x041B #x041C #x041D #x041E #x041F #x0420
  #x0421 #x0422 #x0423 #x0424 #x0425 #x0426 #x0427 #x0428
  #x0429 #x042A #x042B #x042C #x042D #x042E #x042F #x0430
  #x0431 #x0432 #x0433 #x0434 #x0435 #x0436 #x0437 #x0438
  #x0439 #x043A #x043B #x043C #x043D #x043E #x043F #x0440
  #x0441 #x0442 #x0443 #x0444 #x0445 #x0446 #x0447 #x0448
  #x0449 #x044A #x044B #x044C #x044D #x044E #x044F #x2116
  #x0451 #x0452 #x0453 #x0454 #x0455 #x0456 #x0457 #x0458
  #x0459 #x045A #x045B #x045C #x00A7 #x045E #x045F)

(define-coding-aliases 'ISO-8859-5
  '(ISO_8859-5:1988 ISO-IR-144 ISO_8859-5 CYRILLIC CSISOLATINCYRILLIC))

(define-8-bit-codecs iso-8859-6 #xA1
  #f     #f     #f     #x00A4 #f     #f     #f     #f
  #f     #f     #f     #x060C #x00AD #f     #f     #f
  #f     #f     #f     #f     #f     #f     #f     #f
  #f     #f     #x061B #f     #f     #f     #x061F #f
  #x0621 #x0622 #x0623 #x0624 #x0625 #x0626 #x0627 #x0628
  #x0629 #x062A #x062B #x062C #x062D #x062E #x062F #x0630
  #x0631 #x0632 #x0633 #x0634 #x0635 #x0636 #x0637 #x0638
  #x0639 #x063A #f     #f     #f     #f     #f     #x0640
  #x0641 #x0642 #x0643 #x0644 #x0645 #x0646 #x0647 #x0648
  #x0649 #x064A #x064B #x064C #x064D #x064E #x064F #x0650
  #x0651 #x0652 #f     #f     #f     #f     #f     #f
  #f     #f     #f     #f     #f     #f     #f)

(define-coding-aliases 'ISO-8859-6
  '(ISO_8859-6:1987 ISO-IR-127 ISO_8859-6 ECMA-114 ASMO-708 ARABIC
		    CSISOLATINARABIC))

(define-8-bit-codecs iso-8859-7 #xA1
  #x2018 #x2019 #x00A3 #f     #f     #x00A6 #x00A7 #x00A8
  #x00A9 #f     #x00AB #x00AC #x00AD #f     #x2015 #x00B0
  #x00B1 #x00B2 #x00B3 #x0384 #x0385 #x0386 #x00B7 #x0388
  #x0389 #x038A #x00BB #x038C #x00BD #x038E #x038F #x0390
  #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397 #x0398
  #x0399 #x039A #x039B #x039C #x039D #x039E #x039F #x03A0
  #x03A1 #f     #x03A3 #x03A4 #x03A5 #x03A6 #x03A7 #x03A8
  #x03A9 #x03AA #x03AB #x03AC #x03AD #x03AE #x03AF #x03B0
  #x03B1 #x03B2 #x03B3 #x03B4 #x03B5 #x03B6 #x03B7 #x03B8
  #x03B9 #x03BA #x03BB #x03BC #x03BD #x03BE #x03BF #x03C0
  #x03C1 #x03C2 #x03C3 #x03C4 #x03C5 #x03C6 #x03C7 #x03C8
  #x03C9 #x03CA #x03CB #x03CC #x03CD #x03CE #f)

(define-coding-aliases 'ISO-8859-7
  '(ISO_8859-7:1987 ISO-IR-126 ISO_8859-7 ELOT_928 ECMA-118 GREEK GREEK8
		    CSISOLATINGREEK))

(define-8-bit-codecs iso-8859-8 #xA1
  #f     #x00A2 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7 #x00A8
  #x00A9 #x00D7 #x00AB #x00AC #x00AD #x00AE #x00AF #x00B0
  #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7 #x00B8
  #x00B9 #x00F7 #x00BB #x00BC #x00BD #x00BE #f     #f
  #f     #f     #f     #f     #f     #f     #f     #f
  #f     #f     #f     #f     #f     #f     #f     #f
  #f     #f     #f     #f     #f     #f     #f     #f
  #f     #f     #f     #f     #f     #f     #x2017 #x05D0
  #x05D1 #x05D2 #x05D3 #x05D4 #x05D5 #x05D6 #x05D7 #x05D8
  #x05D9 #x05DA #x05DB #x05DC #x05DD #x05DE #x05DF #x05E0
  #x05E1 #x05E2 #x05E3 #x05E4 #x05E5 #x05E6 #x05E7 #x05E8
  #x05E9 #x05EA #f     #f     #x200E #x200F #f)

(define-coding-aliases 'ISO-8859-8
  '(ISO_8859-8:1988 ISO-IR-138 ISO_8859-8 HEBREW CSISOLATINHEBREW))

(define-8-bit-codecs iso-8859-9 #xA1
  #x00A1 #x00A2 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7 #x00A8
  #x00A9 #x00AA #x00AB #x00AC #x00AD #x00AE #x00AF #x00B0
  #x00B1 #x00B2 #x00B3 #x00B4 #x00B5 #x00B6 #x00B7 #x00B8
  #x00B9 #x00BA #x00BB #x00BC #x00BD #x00BE #x00BF #x00C0
  #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7 #x00C8
  #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF #x011E
  #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x00D7 #x00D8
  #x00D9 #x00DA #x00DB #x00DC #x0130 #x015E #x00DF #x00E0
  #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7 #x00E8
  #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF #x011F
  #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x00F7 #x00F8
  #x00F9 #x00FA #x00FB #x00FC #x0131 #x015F #x00FF)

(define-coding-aliases 'ISO-8859-9
  '(ISO_8859-9:1989 ISO-IR-148 ISO_8859-9 LATIN5 L5 CSISOLATIN5))

(define-8-bit-codecs iso-8859-10 #xA1
  #x0104 #x0112 #x0122 #x012A #x0128 #x0136 #x00A7 #x013B
  #x0110 #x0160 #x0166 #x017D #x00AD #x016A #x014A #x00B0
  #x0105 #x0113 #x0123 #x012B #x0129 #x0137 #x00B7 #x013C
  #x0111 #x0161 #x0167 #x017E #x2015 #x016B #x014B #x0100
  #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x012E #x010C
  #x00C9 #x0118 #x00CB #x0116 #x00CD #x00CE #x00CF #x00D0
  #x0145 #x014C #x00D3 #x00D4 #x00D5 #x00D6 #x0168 #x00D8
  #x0172 #x00DA #x00DB #x00DC #x00DD #x00DE #x00DF #x0101
  #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x012F #x010D
  #x00E9 #x0119 #x00EB #x0117 #x00ED #x00EE #x00EF #x00F0
  #x0146 #x014D #x00F3 #x00F4 #x00F5 #x00F6 #x0169 #x00F8
  #x0173 #x00FA #x00FB #x00FC #x00FD #x00FE #x0138)

(define-coding-aliases 'ISO-8859-10
  '(ISO-IR-157 L6 ISO_8859-10:1992 CSISOLATIN6 LATIN6))

(define-8-bit-codecs iso-8859-11 #xA1
  #x0E01 #x0E02 #x0E03 #x0E04 #x0E05 #x0E06 #x0E07 #x0E08
  #x0E09 #x0E0A #x0E0B #x0E0C #x0E0D #x0E0E #x0E0F #x0E10
  #x0E11 #x0E12 #x0E13 #x0E14 #x0E15 #x0E16 #x0E17 #x0E18
  #x0E19 #x0E1A #x0E1B #x0E1C #x0E1D #x0E1E #x0E1F #x0E20
  #x0E21 #x0E22 #x0E23 #x0E24 #x0E25 #x0E26 #x0E27 #x0E28
  #x0E29 #x0E2A #x0E2B #x0E2C #x0E2D #x0E2E #x0E2F #x0E30
  #x0E31 #x0E32 #x0E33 #x0E34 #x0E35 #x0E36 #x0E37 #x0E38
  #x0E39 #x0E3A #f     #f     #f     #f     #x0E3F #x0E40
  #x0E41 #x0E42 #x0E43 #x0E44 #x0E45 #x0E46 #x0E47 #x0E48
  #x0E49 #x0E4A #x0E4B #x0E4C #x0E4D #x0E4E #x0E4F #x0E50
  #x0E51 #x0E52 #x0E53 #x0E54 #x0E55 #x0E56 #x0E57 #x0E58
  #x0E59 #x0E5A #x0E5B #f     #f     #f     #f)

(define-8-bit-codecs iso-8859-13 #xA1
  #x201D #x00A2 #x00A3 #x00A4 #x201E #x00A6 #x00A7 #x00D8
  #x00A9 #x0156 #x00AB #x00AC #x00AD #x00AE #x00C6 #x00B0
  #x00B1 #x00B2 #x00B3 #x201C #x00B5 #x00B6 #x00B7 #x00F8
  #x00B9 #x0157 #x00BB #x00BC #x00BD #x00BE #x00E6 #x0104
  #x012E #x0100 #x0106 #x00C4 #x00C5 #x0118 #x0112 #x010C
  #x00C9 #x0179 #x0116 #x0122 #x0136 #x012A #x013B #x0160
  #x0143 #x0145 #x00D3 #x014C #x00D5 #x00D6 #x00D7 #x0172
  #x0141 #x015A #x016A #x00DC #x017B #x017D #x00DF #x0105
  #x012F #x0101 #x0107 #x00E4 #x00E5 #x0119 #x0113 #x010D
  #x00E9 #x017A #x0117 #x0123 #x0137 #x012B #x013C #x0161
  #x0144 #x0146 #x00F3 #x014D #x00F5 #x00F6 #x00F7 #x0173
  #x0142 #x015B #x016B #x00FC #x017C #x017E #x2019)

(define-8-bit-codecs iso-8859-14 #xA1
  #x1E02 #x1E03 #x00A3 #x010A #x010B #x1E0A #x00A7 #x1E80
  #x00A9 #x1E82 #x1E0B #x1EF2 #x00AD #x00AE #x0178 #x1E1E
  #x1E1F #x0120 #x0121 #x1E40 #x1E41 #x00B6 #x1E56 #x1E81
  #x1E57 #x1E83 #x1E60 #x1EF3 #x1E84 #x1E85 #x1E61 #x00C0
  #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7 #x00C8
  #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF #x0174
  #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x1E6A #x00D8
  #x00D9 #x00DA #x00DB #x00DC #x00DD #x0176 #x00DF #x00E0
  #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7 #x00E8
  #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF #x0175
  #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x1E6B #x00F8
  #x00F9 #x00FA #x00FB #x00FC #x00FD #x0177 #x00FF)

(define-coding-aliases 'ISO-8859-14
  '(ISO-IR-199 ISO_8859-14:1998 ISO_8859-14 LATIN8 ISO-CELTIC L8))

(define-8-bit-codecs iso-8859-15 #xA1
  #x00A1 #x00A2 #x00A3 #x20AC #x00A5 #x0160 #x00A7 #x0161
  #x00A9 #x00AA #x00AB #x00AC #x00AD #x00AE #x00AF #x00B0
  #x00B1 #x00B2 #x00B3 #x017D #x00B5 #x00B6 #x00B7 #x017E
  #x00B9 #x00BA #x00BB #x0152 #x0153 #x0178 #x00BF #x00C0
  #x00C1 #x00C2 #x00C3 #x00C4 #x00C5 #x00C6 #x00C7 #x00C8
  #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF #x00D0
  #x00D1 #x00D2 #x00D3 #x00D4 #x00D5 #x00D6 #x00D7 #x00D8
  #x00D9 #x00DA #x00DB #x00DC #x00DD #x00DE #x00DF #x00E0
  #x00E1 #x00E2 #x00E3 #x00E4 #x00E5 #x00E6 #x00E7 #x00E8
  #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF #x00F0
  #x00F1 #x00F2 #x00F3 #x00F4 #x00F5 #x00F6 #x00F7 #x00F8
  #x00F9 #x00FA #x00FB #x00FC #x00FD #x00FE #x00FF)

(define-coding-aliases 'ISO-8859-15
  '(ISO_8859-15 LATIN-9))

(define-8-bit-codecs iso-8859-16 #xA1
  #x0104 #x0105 #x0141 #x20AC #x201E #x0160 #x00A7 #x0161
  #x00A9 #x0218 #x00AB #x0179 #x00AD #x017A #x017B #x00B0
  #x00B1 #x010C #x0142 #x017D #x201D #x00B6 #x00B7 #x017E
  #x010D #x0219 #x00BB #x0152 #x0153 #x0178 #x017C #x00C0
  #x00C1 #x00C2 #x0102 #x00C4 #x0106 #x00C6 #x00C7 #x00C8
  #x00C9 #x00CA #x00CB #x00CC #x00CD #x00CE #x00CF #x0110
  #x0143 #x00D2 #x00D3 #x00D4 #x0150 #x00D6 #x015A #x0170
  #x00D9 #x00DA #x00DB #x00DC #x0118 #x021A #x00DF #x00E0
  #x00E1 #x00E2 #x0103 #x00E4 #x0107 #x00E6 #x00E7 #x00E8
  #x00E9 #x00EA #x00EB #x00EC #x00ED #x00EE #x00EF #x0111
  #x0144 #x00F2 #x00F3 #x00F4 #x0151 #x00F6 #x015B #x0171
  #x00F9 #x00FA #x00FB #x00FC #x0119 #x021B #x00FF)

(define-coding-aliases 'ISO-8859-16
  '(ISO-IR-226 ISO_8859-16:2001 ISO_8859-16 LATIN10 L10))

(define-8-bit-codecs windows-1250 #x80
  #x20ac #f     #x201a #f     #x201e #x2026 #x2020 #x2021
  #f     #x2030 #x0160 #x2039 #x015a #x0164 #x017d #x0179
  #f     #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #f     #x2122 #x0161 #x203a #x015b #x0165 #x017e #x017a
  #x00a0 #x02c7 #x02d8 #x0141 #x00a4 #x0104 #x00a6 #x00a7
  #x00a8 #x00a9 #x015e #x00ab #x00ac #x00ad #x00ae #x017b
  #x00b0 #x00b1 #x02db #x0142 #x00b4 #x00b5 #x00b6 #x00b7
  #x00b8 #x0105 #x015f #x00bb #x013d #x02dd #x013e #x017c
  #x0154 #x00c1 #x00c2 #x0102 #x00c4 #x0139 #x0106 #x00c7
  #x010c #x00c9 #x0118 #x00cb #x011a #x00cd #x00ce #x010e
  #x0110 #x0143 #x0147 #x00d3 #x00d4 #x0150 #x00d6 #x00d7
  #x0158 #x016e #x00da #x0170 #x00dc #x00dd #x0162 #x00df
  #x0155 #x00e1 #x00e2 #x0103 #x00e4 #x013a #x0107 #x00e7
  #x010d #x00e9 #x0119 #x00eb #x011b #x00ed #x00ee #x010f
  #x0111 #x0144 #x0148 #x00f3 #x00f4 #x0151 #x00f6 #x00f7
  #x0159 #x016f #x00fa #x0171 #x00fc #x00fd #x0163 #x02d9)

(define-8-bit-codecs windows-1251 #x80
  #x0402 #x0403 #x201a #x0453 #x201e #x2026 #x2020 #x2021
  #x20ac #x2030 #x0409 #x2039 #x040a #x040c #x040b #x040f
  #x0452 #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #f     #x2122 #x0459 #x203a #x045a #x045c #x045b #x045f
  #x00a0 #x040e #x045e #x0408 #x00a4 #x0490 #x00a6 #x00a7
  #x0401 #x00a9 #x0404 #x00ab #x00ac #x00ad #x00ae #x0407
  #x00b0 #x00b1 #x0406 #x0456 #x0491 #x00b5 #x00b6 #x00b7
  #x0451 #x2116 #x0454 #x00bb #x0458 #x0405 #x0455 #x0457
  #x0410 #x0411 #x0412 #x0413 #x0414 #x0415 #x0416 #x0417
  #x0418 #x0419 #x041a #x041b #x041c #x041d #x041e #x041f
  #x0420 #x0421 #x0422 #x0423 #x0424 #x0425 #x0426 #x0427
  #x0428 #x0429 #x042a #x042b #x042c #x042d #x042e #x042f
  #x0430 #x0431 #x0432 #x0433 #x0434 #x0435 #x0436 #x0437
  #x0438 #x0439 #x043a #x043b #x043c #x043d #x043e #x043f
  #x0440 #x0441 #x0442 #x0443 #x0444 #x0445 #x0446 #x0447
  #x0448 #x0449 #x044a #x044b #x044c #x044d #x044e #x044f)

(define-8-bit-codecs windows-1252 #x80
  #x20ac #f     #x201a #x0192 #x201e #x2026 #x2020 #x2021
  #x02c6 #x2030 #x0160 #x2039 #x0152 #f     #x017d #f
  #f     #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #x02dc #x2122 #x0161 #x203a #x0153 #f     #x017e #x0178
  #x00a0 #x00a1 #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
  #x00a8 #x00a9 #x00aa #x00ab #x00ac #x00ad #x00ae #x00af
  #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
  #x00b8 #x00b9 #x00ba #x00bb #x00bc #x00bd #x00be #x00bf
  #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7
  #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
  #x00d0 #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7
  #x00d8 #x00d9 #x00da #x00db #x00dc #x00dd #x00de #x00df
  #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
  #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
  #x00f0 #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7
  #x00f8 #x00f9 #x00fa #x00fb #x00fc #x00fd #x00fe #x00ff)

(define-8-bit-codecs windows-1253 #x80
  #x20ac #f     #x201a #x0192 #x201e #x2026 #x2020 #x2021
  #f     #x2030 #f     #x2039 #f     #f     #f     #f
  #f     #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #f     #x2122 #f     #x203a #f     #f     #f     #f
  #x00a0 #x0385 #x0386 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
  #x00a8 #x00a9 #f     #x00ab #x00ac #x00ad #x00ae #x2015
  #x00b0 #x00b1 #x00b2 #x00b3 #x0384 #x00b5 #x00b6 #x00b7
  #x0388 #x0389 #x038a #x00bb #x038c #x00bd #x038e #x038f
  #x0390 #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397
  #x0398 #x0399 #x039a #x039b #x039c #x039d #x039e #x039f
  #x03a0 #x03a1 #f     #x03a3 #x03a4 #x03a5 #x03a6 #x03a7
  #x03a8 #x03a9 #x03aa #x03ab #x03ac #x03ad #x03ae #x03af
  #x03b0 #x03b1 #x03b2 #x03b3 #x03b4 #x03b5 #x03b6 #x03b7
  #x03b8 #x03b9 #x03ba #x03bb #x03bc #x03bd #x03be #x03bf
  #x03c0 #x03c1 #x03c2 #x03c3 #x03c4 #x03c5 #x03c6 #x03c7
  #x03c8 #x03c9 #x03ca #x03cb #x03cc #x03cd #x03ce #f)

(define-8-bit-codecs windows-1254 #x80
  #x20ac #f     #x201a #x0192 #x201e #x2026 #x2020 #x2021
  #x02c6 #x2030 #x0160 #x2039 #x0152 #f     #f     #f
  #f     #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #x02dc #x2122 #x0161 #x203a #x0153 #f     #f     #x0178
  #x00a0 #x00a1 #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
  #x00a8 #x00a9 #x00aa #x00ab #x00ac #x00ad #x00ae #x00af
  #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
  #x00b8 #x00b9 #x00ba #x00bb #x00bc #x00bd #x00be #x00bf
  #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7
  #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
  #x011e #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7
  #x00d8 #x00d9 #x00da #x00db #x00dc #x0130 #x015e #x00df
  #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
  #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
  #x011f #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7
  #x00f8 #x00f9 #x00fa #x00fb #x00fc #x0131 #x015f #x00ff)

(define-8-bit-codecs windows-1255 #x80
  #x20ac #f     #x201a #x0192 #x201e #x2026 #x2020 #x2021
  #x02c6 #x2030 #f     #x2039 #f     #f     #f     #f
  #f     #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #x02dc #x2122 #f     #x203a #f     #f     #f     #f
  #x00a0 #x00a1 #x00a2 #x00a3 #x20aa #x00a5 #x00a6 #x00a7
  #x00a8 #x00a9 #x00d7 #x00ab #x00ac #x00ad #x00ae #x00af
  #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
  #x00b8 #x00b9 #x00f7 #x00bb #x00bc #x00bd #x00be #x00bf
  #x05b0 #x05b1 #x05b2 #x05b3 #x05b4 #x05b5 #x05b6 #x05b7
  #x05b8 #x05b9 #f     #x05bb #x05bc #x05bd #x05be #x05bf
  #x05c0 #x05c1 #x05c2 #x05c3 #x05f0 #x05f1 #x05f2 #x05f3
  #x05f4 #f     #f     #f     #f     #f     #f     #f
  #x05d0 #x05d1 #x05d2 #x05d3 #x05d4 #x05d5 #x05d6 #x05d7
  #x05d8 #x05d9 #x05da #x05db #x05dc #x05dd #x05de #x05df
  #x05e0 #x05e1 #x05e2 #x05e3 #x05e4 #x05e5 #x05e6 #x05e7
  #x05e8 #x05e9 #x05ea #f     #f     #x200e #x200f #f)

(define-8-bit-codecs windows-1256 #x80
  #x20ac #x067e #x201a #x0192 #x201e #x2026 #x2020 #x2021
  #x02c6 #x2030 #x0679 #x2039 #x0152 #x0686 #x0698 #x0688
  #x06af #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #x06a9 #x2122 #x0691 #x203a #x0153 #x200c #x200d #x06ba
  #x00a0 #x060c #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
  #x00a8 #x00a9 #x06be #x00ab #x00ac #x00ad #x00ae #x00af
  #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
  #x00b8 #x00b9 #x061b #x00bb #x00bc #x00bd #x00be #x061f
  #x06c1 #x0621 #x0622 #x0623 #x0624 #x0625 #x0626 #x0627
  #x0628 #x0629 #x062a #x062b #x062c #x062d #x062e #x062f
  #x0630 #x0631 #x0632 #x0633 #x0634 #x0635 #x0636 #x00d7
  #x0637 #x0638 #x0639 #x063a #x0640 #x0641 #x0642 #x0643
  #x00e0 #x0644 #x00e2 #x0645 #x0646 #x0647 #x0648 #x00e7
  #x00e8 #x00e9 #x00ea #x00eb #x0649 #x064a #x00ee #x00ef
  #x064b #x064c #x064d #x064e #x00f4 #x064f #x0650 #x00f7
  #x0651 #x00f9 #x0652 #x00fb #x00fc #x200e #x200f #x06d2)

(define-8-bit-codecs windows-1257 #x80
  #x20ac #f     #x201a #f     #x201e #x2026 #x2020 #x2021
  #f     #x2030 #f     #x2039 #f     #x00a8 #x02c7 #x00b8
  #f     #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #f     #x2122 #f     #x203a #f     #x00af #x02db #f
  #x00a0 #f     #x00a2 #x00a3 #x00a4 #f     #x00a6 #x00a7
  #x00d8 #x00a9 #x0156 #x00ab #x00ac #x00ad #x00ae #x00c6
  #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
  #x00f8 #x00b9 #x0157 #x00bb #x00bc #x00bd #x00be #x00e6
  #x0104 #x012e #x0100 #x0106 #x00c4 #x00c5 #x0118 #x0112
  #x010c #x00c9 #x0179 #x0116 #x0122 #x0136 #x012a #x013b
  #x0160 #x0143 #x0145 #x00d3 #x014c #x00d5 #x00d6 #x00d7
  #x0172 #x0141 #x015a #x016a #x00dc #x017b #x017d #x00df
  #x0105 #x012f #x0101 #x0107 #x00e4 #x00e5 #x0119 #x0113
  #x010d #x00e9 #x017a #x0117 #x0123 #x0137 #x012b #x013c
  #x0161 #x0144 #x0146 #x00f3 #x014d #x00f5 #x00f6 #x00f7
  #x0173 #x0142 #x015b #x016b #x00fc #x017c #x017e #x02d9)

(define-8-bit-codecs windows-1258 #x80
  #x20ac #f     #x201a #x0192 #x201e #x2026 #x2020 #x2021
  #x02c6 #x2030 #f     #x2039 #x0152 #f     #f     #f
  #f     #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #x02dc #x2122 #f     #x203a #x0153 #f     #f     #x0178
  #x00a0 #x00a1 #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
  #x00a8 #x00a9 #x00aa #x00ab #x00ac #x00ad #x00ae #x00af
  #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
  #x00b8 #x00b9 #x00ba #x00bb #x00bc #x00bd #x00be #x00bf
  #x00c0 #x00c1 #x00c2 #x0102 #x00c4 #x00c5 #x00c6 #x00c7
  #x00c8 #x00c9 #x00ca #x00cb #x0300 #x00cd #x00ce #x00cf
  #x0110 #x00d1 #x0309 #x00d3 #x00d4 #x01a0 #x00d6 #x00d7
  #x00d8 #x00d9 #x00da #x00db #x00dc #x01af #x0303 #x00df
  #x00e0 #x00e1 #x00e2 #x0103 #x00e4 #x00e5 #x00e6 #x00e7
  #x00e8 #x00e9 #x00ea #x00eb #x0301 #x00ed #x00ee #x00ef
  #x0111 #x00f1 #x0323 #x00f3 #x00f4 #x01a1 #x00f6 #x00f7
  #x00f8 #x00f9 #x00fa #x00fb #x00fc #x01b0 #x20ab #x00ff)

(define-8-bit-codecs windows-874 #x80
  #x20ac #f     #f     #f     #f     #x2026 #f     #f
  #f     #f     #f     #f     #f     #f     #f     #f
  #f     #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
  #f     #f     #f     #f     #f     #f     #f     #f
  #x00a0 #x0e01 #x0e02 #x0e03 #x0e04 #x0e05 #x0e06 #x0e07
  #x0e08 #x0e09 #x0e0a #x0e0b #x0e0c #x0e0d #x0e0e #x0e0f
  #x0e10 #x0e11 #x0e12 #x0e13 #x0e14 #x0e15 #x0e16 #x0e17
  #x0e18 #x0e19 #x0e1a #x0e1b #x0e1c #x0e1d #x0e1e #x0e1f
  #x0e20 #x0e21 #x0e22 #x0e23 #x0e24 #x0e25 #x0e26 #x0e27
  #x0e28 #x0e29 #x0e2a #x0e2b #x0e2c #x0e2d #x0e2e #x0e2f
  #x0e30 #x0e31 #x0e32 #x0e33 #x0e34 #x0e35 #x0e36 #x0e37
  #x0e38 #x0e39 #x0e3a #f     #f     #f     #f     #x0e3f
  #x0e40 #x0e41 #x0e42 #x0e43 #x0e44 #x0e45 #x0e46 #x0e47
  #x0e48 #x0e49 #x0e4a #x0e4b #x0e4c #x0e4d #x0e4e #x0e4f
  #x0e50 #x0e51 #x0e52 #x0e53 #x0e54 #x0e55 #x0e56 #x0e57
  #x0e58 #x0e59 #x0e5a #x0e5b #f     #f     #f     #f)

#|
(define (read-iso-8859-directory directory)
  (let ((directory (pathname-as-directory directory)))
    (let loop ((pathnames (directory-read directory)))
      (if (pair? pathnames)
	  (let ((pathname (car pathnames)))
	    (let ((name (pathname-name pathname)))
	      (if (re-string-match "\\`8859-[0-9]+\\'" name)
		  (cons (list (intern (string-append "ISO-" name))
			      (read-iso-8859-file pathname))
			(loop (cdr pathnames)))
		  (loop (cdr pathnames)))))
	  '()))))

(define (read-iso-8859-file pathname)
  (call-with-input-file pathname
    (lambda (port)
      (let ((v (make-vector #x100 #f))
	    (re
	     (rexp-compile
	      (let ((hex (string->char-set "0123456789abcdefABCDEF")))
		(rexp-sequence (rexp-string-start)
			       "0x" (rexp-group hex hex)
			       "\t0x" (rexp-group hex hex hex hex)
			       "\t"))))
	    (hex
	     (lambda (line regs i)
	       (string->number (re-match-extract line regs i) 16))))
	(let loop ()
	  (let ((line (read-line port)))
	    (if (not (eof-object? line))
		(let ((regs (re-string-match re line)))
		  (if regs
		      (let ((i (hex line regs 1))
			    (j (hex line regs 2)))
			(let ((c (integer->char j)))
			  (if (vector-ref v i)
			      (error "Character defined:" i c)
			      (vector-set! v i c)))))
		  (loop)))))
	v))))
|#

;;;; Unicode codecs

(define-decoder 'UTF-8
  (lambda (ib)

    (define-integrable (done cp bs)
      (set-input-buffer-start! ib bs)
      cp)

    (let ((bv (input-buffer-bytes ib))
	  (bs (input-buffer-start ib)))
      (let ((b0 (get-byte bv bs 0)))
	(cond ((fix:< b0 #x80)
	       (done b0 (fix:+ bs 1)))
	      ((fix:< b0 #xE0)
	       (and (fix:<= (fix:+ bs 2) (input-buffer-end ib))
		    (let ((b1 (get-byte bv bs 1)))
		      (if (and (fix:> b0 #xC1)
			       (trailing-byte? b1))
			  (done (fix:or (extract b0 #x1F 6)
					(extract b1 #x3F 0))
				(fix:+ bs 2))
			  (error:char-decoding ib)))))
	      ((fix:< b0 #xF0)
	       (and (fix:<= (fix:+ bs 3) (input-buffer-end ib))
		    (let ((b1 (get-byte bv bs 1))
			  (b2 (get-byte bv bs 2)))
		      (if (and (or (fix:> b0 #xE0) (fix:> b1 #x9F))
			       (trailing-byte? b1)
			       (trailing-byte? b2))
			  (let ((cp
				 (fix:or (fix:or (extract b0 #x0F 12)
						 (extract b1 #x3F 6))
					 (extract b2 #x3F 0))))
			    (if (illegal-low? cp)
				(error:char-decoding ib)
				(done cp (fix:+ bs 3))))
			  (error:char-decoding ib)))))
	      ((fix:< b0 #xF8)
	       (and (fix:<= (fix:+ bs 4) (input-buffer-end ib))
		    (let ((b1 (get-byte bv bs 1))
			  (b2 (get-byte bv bs 2))
			  (b3 (get-byte bv bs 3)))
		      (if (and (or (fix:> b0 #xF0) (fix:> b1 #x8F))
			       (trailing-byte? b1)
			       (trailing-byte? b2)
			       (trailing-byte? b3))
			  (let ((cp
				 (fix:or (fix:or (extract b0 #x07 18)
						 (extract b1 #x3F 12))
					 (fix:or (extract b2 #x3F 6)
						 (extract b3 #x3F 0)))))
			    (if (fix:< cp #x110000)
				(done cp (fix:+ bs 4))
				(error:char-decoding ib)))
			  (error:char-decoding ib)))))
	      (else
	       (error:char-decoding ib)))))))

(define-encoder 'UTF-8
  (lambda (ob cp)
    (let ((bv (output-buffer-bytes ob))
	  (bs (output-buffer-start ob)))

      (define-integrable (initial-byte n-bits offset)
	(fix:or (fix:and (fix:lsh #xFF (fix:+ n-bits 1)) #xFF)
		(fix:lsh cp (fix:- 0 offset))))

      (define-integrable (trailing-byte offset)
	(fix:or #x80 (fix:and (fix:lsh cp (fix:- 0 offset)) #x3F)))

      (cond ((fix:< cp #x00000080)
	     (put-byte bv bs 0 cp)
	     1)
	    ((fix:< cp #x00000800)
	     (put-byte bv bs 0 (initial-byte 5 6))
	     (put-byte bv bs 1 (trailing-byte 0))
	     2)
	    ((fix:< cp #x00010000)
	     (put-byte bv bs 0 (initial-byte 4 12))
	     (put-byte bv bs 1 (trailing-byte 6))
	     (put-byte bv bs 2 (trailing-byte 0))
	     3)
	    ((fix:< cp #x00110000)
	     (put-byte bv bs 0 (initial-byte 3 18))
	     (put-byte bv bs 1 (trailing-byte 12))
	     (put-byte bv bs 2 (trailing-byte 6))
	     (put-byte bv bs 3 (trailing-byte 0))
	     4)
	    (else
	     (error:char-encoding ob cp))))))

(define-sizer 'UTF-8
  (lambda (ib cp)
    (cond ((fix:< cp #x00000080) 1)
	  ((fix:< cp #x00000800) 2)
	  ((fix:< cp #x00010000) 3)
	  ((fix:< cp #x00110000) 4)
	  (else (error:char-encoding ib cp)))))

(define-integrable (get-byte bv base offset)
  (vector-8b-ref bv (fix:+ base offset)))

(define-integrable (put-byte bv base offset byte)
  (vector-8b-set! bv (fix:+ base offset) byte))

(define-integrable (extract b m n)
  (fix:lsh (fix:and b m) n))

(define-integrable (trailing-byte? b)
  (fix:= (fix:and #xC0 b) #x80))

(define-integrable (illegal-low? n)
  (or (fix:= (fix:and #xF800 n) #xD800)
      (fix:= (fix:and #xFFFE n) #xFFFE)))

(let ((alias (lambda () (if (host-big-endian?) 'UTF-16BE 'UTF-16LE))))
  (define-decoder-alias 'UTF-16 alias)
  (define-encoder-alias 'UTF-16 alias))

(define-decoder 'UTF-16BE (lambda (ib) (decode-utf-16 ib be-bytes->digit16)))
(define-decoder 'UTF-16LE (lambda (ib) (decode-utf-16 ib le-bytes->digit16)))

(define-integrable (decode-utf-16 ib combine)

  (define-integrable (done cp bs)
    (set-input-buffer-start! ib bs)
    cp)

  (let ((bv (input-buffer-bytes ib))
	(bs (input-buffer-start ib)))
    (and (fix:<= (fix:+ bs 2) (input-buffer-end ib))
	 (let ((d0
		(combine (get-byte bv bs 0)
			 (get-byte bv bs 1))))
	   (if (utf16-high-surrogate? d0)
	       (and (fix:<= (fix:+ bs 4) (input-buffer-end ib))
		    (let ((d1
			   (combine (get-byte bv bs 2)
				    (get-byte bv bs 3))))
		      (if (utf16-low-surrogate? d1)
			  (done (combine-utf16-surrogates d0 d1) (fix:+ bs 4))
			  (error:char-decoding ib))))
	       (if (illegal-low? d0)
		   (error:char-decoding ib)
		   (done d0 (fix:+ bs 2))))))))

(define-encoder 'UTF-16BE
  (lambda (ob cp)
    (encode-utf-16 ob cp high-byte low-byte)))

(define-encoder 'UTF-16LE
  (lambda (ob cp)
    (encode-utf-16 ob cp low-byte high-byte)))

(define-integrable (encode-utf-16 ob cp first-byte second-byte)
  (let ((bv (output-buffer-bytes ob))
	(bs (output-buffer-start ob)))
    (cond ((fix:< cp #x10000)
	   (put-byte bv bs 0 (first-byte cp))
	   (put-byte bv bs 1 (second-byte cp))
	   2)
	  ((fix:< cp #x110000)
	   (receive (h l) (split-into-utf16-surrogates cp)
	     (put-byte bv bs 0 (first-byte h))
	     (put-byte bv bs 1 (second-byte h))
	     (put-byte bv bs 2 (first-byte l))
	     (put-byte bv bs 3 (second-byte l)))
	   4)
	  (else
	   (error:char-encoding ob cp)))))

(define-sizer 'UTF-16
  (lambda (ib cp)
    (cond ((fix:< cp #x00010000) 2)
	  ((fix:< cp #x00110000) 4)
	  (else (error:char-encoding ib cp)))))
(define-sizer-alias 'UTF-16BE 'UTF-16)
(define-sizer-alias 'UTF-16LE 'UTF-16)

(define-integrable (be-bytes->digit16 b0 b1) (fix:or (fix:lsh b0 8) b1))
(define-integrable (le-bytes->digit16 b0 b1) (fix:or b0 (fix:lsh b1 8)))
(define-integrable (high-byte d) (fix:lsh d -8))
(define-integrable (low-byte d) (fix:and d #xFF))

(let ((alias
       (lambda ()
	 (if (host-big-endian?)
	     'UTF-32BE
	     'UTF-32LE))))
  (define-decoder-alias 'UTF-32 alias)
  (define-encoder-alias 'UTF-32 alias))

(define-decoder 'UTF-32BE
  (lambda (ib)
    (let ((bv (input-buffer-bytes ib))
	  (bs (input-buffer-start ib)))
      (and (fix:<= (fix:+ bs 4) (input-buffer-end ib))
	   (let ((cp
		  (+ (* (get-byte bv bs 0) #x1000000)
		     (* (get-byte bv bs 1) #x10000)
		     (* (get-byte bv bs 2) #x100)
		     (get-byte bv bs 3))))
	     (if (unicode-scalar-value? cp)
		 (begin
		   (set-input-buffer-start! ib (fix:+ bs 4))
		   cp)
		 (error:char-decoding ib)))))))

(define-decoder 'UTF-32LE
  (lambda (ib)
    (let ((bv (input-buffer-bytes ib))
	  (bs (input-buffer-start ib)))
      (and (fix:<= (fix:+ bs 4) (input-buffer-end ib))
	   (let ((cp
		  (+ (* (get-byte bv bs 3) #x1000000)
		     (* (get-byte bv bs 2) #x10000)
		     (* (get-byte bv bs 1) #x100)
		     (get-byte bv bs 0))))
	     (if (unicode-scalar-value? cp)
		 (begin
		   (set-input-buffer-start! ib (fix:+ bs 4))
		   cp)
		 (error:char-decoding ib)))))))

(define-encoder 'UTF-32BE
  (lambda (ob cp)
    (if (fix:< cp #x110000)
	(let ((bv (output-buffer-bytes ob))
	      (bs (output-buffer-start ob)))
	  (put-byte bv bs 0 #x00)
	  (put-byte bv bs 1 (fix:and (fix:lsh cp -16) #xFF))
	  (put-byte bv bs 2 (fix:and (fix:lsh cp -8) #xFF))
	  (put-byte bv bs 3 (fix:and cp #xFF))
	  4)
	(error:char-encoding ob cp))))

(define-encoder 'UTF-32LE
  (lambda (ob cp)
    (if (fix:< cp #x110000)
	(let ((bv (output-buffer-bytes ob))
	      (bs (output-buffer-start ob)))
	  (put-byte bv bs 0 (fix:and cp #xFF))
	  (put-byte bv bs 1 (fix:and (fix:lsh cp -8) #xFF))
	  (put-byte bv bs 2 (fix:and (fix:lsh cp -16) #xFF))
	  (put-byte bv bs 3 #x00)
	  4)
	(error:char-encoding ob cp))))

(define-sizer 'UTF-32
  (lambda (ib cp)
    (cond ((fix:< cp #x110000) 4)
	  (else (error:char-encoding ib cp)))))
(define-sizer-alias 'UTF-32BE 'UTF-32)
(define-sizer-alias 'UTF-32LE 'UTF-32)

;;;; Normalizers

(define-normalizer 'NEWLINE
  (lambda (ib)
    (decode-char ib)))

(define-denormalizer 'NEWLINE
  (lambda (ob char)
    (encode-char ob char)))

(define-normalizer 'CR
  (lambda (ib)
    (let ((c0 (decode-char ib)))
      (if (eq? c0 #\U+000D)
	  #\newline
	  c0))))

(define-denormalizer 'CR
  (lambda (ob char)
    (encode-char ob (if (char=? char #\newline) #\U+000D char))))

(define-normalizer 'CRLF
  (lambda (ib)
    (let* ((bs0 (input-buffer-start ib))
	   (c0 (decode-char ib)))
      (if (eq? c0 #\U+000D)
	  (let* ((bs1 (input-buffer-start ib))
		 (c1 (decode-char ib)))
	    (case c1
	      ((#\U+000A)
	       #\newline)
	      ((#f)
	       (set-input-buffer-start! ib bs0)
	       #f)
	      (else
	       (set-input-buffer-start! ib bs1)
	       c0)))
	  c0))))

(define-denormalizer 'CRLF
  (lambda (ob char)
    (if (char=? char #\newline)
	(begin
	  (encode-char ob #\U+000D)
	  (encode-char ob #\U+000A))
	(encode-char ob char))))

(define-normalizer 'XML-1.0
  (lambda (ib)
    (let* ((bs0 (input-buffer-start ib))
	   (c0 (decode-char ib)))
      (case c0
	((#\U+000D)
	 (let* ((bs1 (input-buffer-start ib))
		(c1 (decode-char ib)))
	   (case c1
	     ((#\U+000A)
	      #\U+000A)
	     ((#f)
	      (set-input-buffer-start! ib bs0)
	      #f)
	     (else
	      (set-input-buffer-start! ib bs1)
	      #\U+000A))))
	(else c0)))))

(define-normalizer 'XML-1.1
  (lambda (ib)
    (let* ((bs0 (input-buffer-start ib))
	   (c0 (decode-char ib)))
      (case c0
	((#\U+000D)
	 (let* ((bs1 (input-buffer-start ib))
		(c1 (decode-char ib)))
	   (case c1
	     ((#\U+000A #\U+0085)
	      #\U+000A)
	     ((#f)
	      (set-input-buffer-start! ib bs0)
	      #f)
	     (else
	      (set-input-buffer-start! ib bs1)
	      #\U+000A))))
	((#\U+0085 #\U+2028) #\U+000A)
	(else c0)))))

(define-normalizer-alias 'TEXT 'XML-1.0)
(define-normalizer-alias 'LF 'NEWLINE)
(define-denormalizer-alias 'LF 'NEWLINE)
(define-normalizer-alias 'BINARY 'NEWLINE)
(define-denormalizer-alias 'BINARY 'NEWLINE)
(define-normalizer-alias 'HTTP 'XML-1.0)
(define-denormalizer-alias 'HTTP 'CRLF)

;;;; Conditions

(define condition-type:char-decoding-error)
(define condition-type:char-encoding-error)
(define error:char-decoding)
(define error:char-encoding)

(define (initialize-conditions!)
  (set! condition-type:char-decoding-error
	(make-condition-type 'CHAR-DECODING-ERROR condition-type:port-error '()
	  (lambda (condition port)
	    (write-string "The input port " port)
	    (write (access-condition condition 'PORT) port)
	    (write-string " was unable to decode a character." port)
	    (newline port))))
  (set! error:char-decoding
	(condition-signaller condition-type:char-decoding-error
			     '(PORT)
			     standard-error-handler))
  (set! condition-type:char-encoding-error
	(make-condition-type 'CHAR-ENCODING-ERROR condition-type:port-error
	    '(CHAR)
	  (lambda (condition port)
	    (write-string "The output port " port)
	    (write (access-condition condition 'PORT) port)
	    (write-string " was unable to encode the character " port)
	    (write (access-condition condition 'CHAR) port)
	    (newline port))))
  (set! error:char-encoding
	(condition-signaller condition-type:char-encoding-error
			     '(PORT CHAR)
			     standard-error-handler))
  unspecific)