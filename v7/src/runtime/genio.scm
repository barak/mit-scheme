#| -*-Scheme-*-

$Id: genio.scm,v 1.29 2004/02/25 20:59:29 cph Exp $

Copyright 1991,1993,1995,1996,1999,2002 Massachusetts Institute of Technology
Copyright 2003,2004 Massachusetts Institute of Technology

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

;;;; Generic I/O Ports
;;; package: (runtime generic-i/o-port)

(declare (usual-integrations))

(define (make-generic-i/o-port input-channel output-channel)
  (if (not (or input-channel output-channel))
      (error "Missing channel arguments."))
  (let ((port
	 (make-port (cond ((not input-channel) generic-output-type)
			  ((not output-channel) generic-input-type)
			  (else generic-i/o-type))
		    (make-gstate input-channel output-channel 'TEXT))))
    (if input-channel (set-channel-port! input-channel port))
    (if output-channel (set-channel-port! output-channel port))
    port))

(define-structure (gstate (type vector) (constructor #f))
  ;; Changes to this structure must be copied to "fileio.scm" and
  ;; "ttyio.scm".
  (input-buffer #f read-only #t)
  (output-buffer #f read-only #t)
  coding
  line-ending)

(define (make-gstate input-channel output-channel type . extra)
  (list->vector
   (cons* (and input-channel (make-input-buffer-1 input-channel type))
	  (and output-channel (make-output-buffer-1 output-channel type))
	  type
	  type
	  extra)))

(define-integrable (port-input-buffer port)
  (gstate-input-buffer (port/state port)))

(define-integrable (port-output-buffer port)
  (gstate-output-buffer (port/state port)))

(define (initialize-package!)
  (let ((input-operations
	 `((CHAR-READY? ,generic-io/char-ready?)
	   (CLOSE-INPUT ,generic-io/close-input)
	   (EOF? ,generic-io/eof?)
	   (INPUT-BLOCKING-MODE ,generic-io/input-blocking-mode)
	   (INPUT-CHANNEL ,generic-io/input-channel)
	   (INPUT-OPEN? ,generic-io/input-open?)
	   (INPUT-TERMINAL-MODE ,generic-io/input-terminal-mode)
	   (READ-CHAR ,generic-io/read-char)
	   (READ-EXTERNAL-SUBSTRING ,generic-io/read-external-substring)
	   (READ-SUBSTRING ,generic-io/read-substring)
	   (READ-WIDE-SUBSTRING ,generic-io/read-wide-substring)
	   (SET-INPUT-BLOCKING-MODE ,generic-io/set-input-blocking-mode)
	   (SET-INPUT-TERMINAL-MODE ,generic-io/set-input-terminal-mode)))
	(output-operations
	 `((BUFFERED-OUTPUT-BYTES ,generic-io/buffered-output-bytes)
	   (CLOSE-OUTPUT ,generic-io/close-output)
	   (FLUSH-OUTPUT ,generic-io/flush-output)
	   (OUTPUT-BLOCKING-MODE ,generic-io/output-blocking-mode)
	   (OUTPUT-CHANNEL ,generic-io/output-channel)
	   (OUTPUT-OPEN? ,generic-io/output-open?)
	   (OUTPUT-TERMINAL-MODE ,generic-io/output-terminal-mode)
	   (SET-OUTPUT-BLOCKING-MODE ,generic-io/set-output-blocking-mode)
	   (SET-OUTPUT-TERMINAL-MODE ,generic-io/set-output-terminal-mode)
	   (WRITE-CHAR ,generic-io/write-char)
	   (WRITE-EXTERNAL-SUBSTRING ,generic-io/write-external-substring)
	   (WRITE-SUBSTRING ,generic-io/write-substring)
	   (WRITE-WIDE-SUBSTRING ,generic-io/write-wide-substring)))
	(other-operations
	 `((CLOSE ,generic-io/close)
	   (CODING ,generic-io/coding)
	   (KNOWN-CODING? ,generic-io/known-coding?)
	   (KNOWN-CODINGS ,generic-io/known-codings)
	   (KNOWN-LINE-ENDING? ,generic-io/known-line-ending?)
	   (KNOWN-LINE-ENDINGS ,generic-io/known-line-endings)
	   (LINE-ENDING ,generic-io/line-ending)
	   (SET-CODING ,generic-io/set-coding)
	   (SET-LINE-ENDING ,generic-io/set-line-ending)
	   (WRITE-SELF ,generic-io/write-self))))
    (set! generic-input-type
	  (make-port-type (append input-operations
				  other-operations)
			  #f))
    (set! generic-output-type
	  (make-port-type (append output-operations
				  other-operations)
			  #f))
    (set! generic-i/o-type
	  (make-port-type (append input-operations
				  output-operations
				  other-operations)
			  #f)))
  (initialize-name-maps!)
  (initialize-conditions!))

(define generic-input-type)
(define generic-output-type)
(define generic-i/o-type)

;;;; Input operations

(define (generic-io/char-ready? port)
  (buffer-has-input? (port-input-buffer port)))

(define (generic-io/read-char port)
  (let ((ib (port-input-buffer port)))
    (let loop ()
      (or (read-next-char ib)
	  (let ((r (fill-input-buffer ib)))
	    (case r
	      ((OK) (loop))
	      ((WOULD-BLOCK) #f)
	      ((EOF) (make-eof-object port))
	      (else (error "Unknown result:" r))))))))

(define (generic-io/read-substring port string start end)
  (read-substring:string (port-input-buffer port) string start end))

(define (generic-io/read-wide-substring port string start end)
  (read-substring:wide-string (port-input-buffer port) string start end))

(define (generic-io/read-external-substring port string start end)
  (read-substring:external-string (port-input-buffer port) string start end))

(define-integrable (generic-io/eof? port)
  (input-buffer-at-eof? (port-input-buffer port)))

(define (generic-io/input-channel port)
  (let ((ib (port-input-buffer port)))
    (if (not ib)
	(error:bad-range-argument port #f))
    (input-buffer-channel ib)))

(define (generic-io/input-blocking-mode port)
  (if (channel-blocking? (generic-io/input-channel port))
      'BLOCKING
      'NONBLOCKING))

(define (generic-io/set-input-blocking-mode port mode)
  (case mode
    ((BLOCKING) (channel-blocking (generic-io/input-channel port)))
    ((NONBLOCKING) (channel-nonblocking (generic-io/input-channel port)))
    (else (error:wrong-type-datum mode "blocking mode"))))

(define (generic-io/input-terminal-mode port)
  (let ((channel (generic-io/input-channel port)))
    (cond ((not (channel-type=terminal? channel)) #f)
	  ((terminal-cooked-input? channel) 'COOKED)
	  (else 'RAW))))

(define (generic-io/set-input-terminal-mode port mode)
  (let ((channel (generic-io/input-channel port)))
    (if (channel-type=terminal? channel)
	(case mode
	  ((COOKED) (terminal-cooked-input channel))
	  ((RAW) (terminal-raw-input channel))
	  ((#F) unspecific)
	  (else (error:wrong-type-datum mode "terminal mode")))
	unspecific)))

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
  (write-substring:string (port-output-buffer port) string start end))

(define (generic-io/write-wide-substring port string start end)
  (write-substring:wide-string (port-output-buffer port) string start end))

(define (generic-io/write-external-substring port string start end)
  (write-substring:external-string (port-output-buffer port) string start end))

(define (generic-io/flush-output port)
  (force-drain-output-buffer (port-output-buffer port)))

(define (generic-io/output-channel port)
  (let ((ob (port-output-buffer port)))
    (if (not ob)
	(error:bad-range-argument port #f))
    (output-buffer-channel ob)))

(define (generic-io/output-blocking-mode port)
  (if (channel-blocking? (generic-io/output-channel port))
      'BLOCKING
      'NONBLOCKING))

(define (generic-io/set-output-blocking-mode port mode)
  (case mode
    ((BLOCKING) (channel-blocking (generic-io/output-channel port)))
    ((NONBLOCKING) (channel-nonblocking (generic-io/output-channel port)))
    (else (error:wrong-type-datum mode "blocking mode"))))

(define (generic-io/output-terminal-mode port)
  (let ((channel (generic-io/output-channel port)))
    (cond ((not (channel-type=terminal? channel)) #f)
	  ((terminal-cooked-output? channel) 'COOKED)
	  (else 'RAW))))

(define (generic-io/set-output-terminal-mode port mode)
  (let ((channel (generic-io/output-channel port)))
    (if (channel-type=terminal? channel)
	(case mode
	  ((COOKED) (terminal-cooked-output (generic-io/output-channel port)))
	  ((RAW) (terminal-raw-output (generic-io/output-channel port)))
	  ((#F) unspecific)
	  (else (error:wrong-type-datum mode "terminal mode")))
	unspecific)))

(define (generic-io/buffered-output-bytes port)
  (output-buffer-start (port-output-buffer port)))

;;;; Non-specific operations

(define (generic-io/close port)
  (generic-io/close-input port)
  (generic-io/close-output port))

(define (generic-io/close-output port)
  (let ((ob (port-output-buffer port)))
    (if ob
	(close-output-buffer ob))))

(define (generic-io/close-input port)
  (let ((ib (port-input-buffer port)))
    (if ib
	(close-input-buffer ib))))

(define (generic-io/output-open? port)
  (let ((ob (port-output-buffer port)))
    (and ob
	 (output-buffer-open? ob))))

(define (generic-io/input-open? port)
  (let ((ib (port-input-buffer port)))
    (and ib
	 (input-buffer-open? ib))))

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
	 (write (generic-io/output-channel port) output-port))
	(else
	 (write-string " for channel" output-port))))

(define (generic-io/coding port)
  (gstate-coding (port/state port)))

(define (generic-io/set-coding port name)
  (let ((state (port/state port)))
    (let ((ib (gstate-input-buffer state)))
      (if ib
	  (set-input-buffer-coding! ib name)))
    (let ((ob (gstate-output-buffer state)))
      (if ob
	  (set-output-buffer-coding! ob name)))
    (set-gstate-coding! state name)))

(define (generic-io/known-coding? port coding)
  (and (if (input-port? port) (known-input-coding? coding) #t)
       (if (output-port? port) (known-output-coding? coding) #t)))

(define (generic-io/known-codings port)
  (cond ((i/o-port? port)
	 (eq-intersection (known-input-codings)
			  (known-output-codings)))
	((input-port? port) (known-input-codings))
	(else (known-output-codings))))

(define (generic-io/line-ending port)
  (gstate-line-ending (port/state port)))

(define (generic-io/set-line-ending port name)
  (let ((state (port/state port)))
    (let ((ib (gstate-input-buffer state))
	  (ob (gstate-output-buffer state)))
      (if ib
	  (set-input-buffer-line-ending!
	   ib
	   (line-ending (input-buffer-channel ib) name #f)))
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
	(else (known-output-line-endings))))

(define (line-ending channel name for-output?)
  (guarantee-symbol name #f)
  (if (or (eq? name 'TEXT)
	  (and for-output?
	       (known-input-line-ending? name)
	       (not (known-output-line-ending? name))))
      (if (eq? 'TCP-STREAM-SOCKET (channel-type channel))
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
	   (let ((plur (symbol-append sing 'S))
		 (proc (symbol-append 'DEFINE- sing)))
	     (let ((rev (symbol-append plur '-REVERSE))
		   (aliases (symbol-append sing '-ALIASES))
		   (aproc (symbol-append proc '-ALIAS)))
	       `(BEGIN
		  (DEFINE ,plur '())
		  (DEFINE ,rev)
		  (DEFINE ,aliases '())
		  (DEFINE (,proc NAME ,sing)
		    (SET! ,plur (CONS (CONS NAME ,sing) ,plur))
		    NAME)
		  (DEFINE (,(symbol-append proc '/POST-BOOT) NAME ,sing)
		    (LET ((OLD (HASH-TABLE/GET ,plur NAME #F)))
		      (IF OLD
			  (HASH-TABLE/REMOVE! ,rev OLD)))
		    (HASH-TABLE/PUT! ,plur NAME ,sing))
		  (DEFINE (,aproc NAME ALIAS)
		    (SET! ,aliases (CONS (CONS NAME ALIAS) ,aliases))
		    NAME)
		  (DEFINE (,(symbol-append aproc '/POST-BOOT) NAME ALIAS)
		    (HASH-TABLE/PUT! ,aliases NAME ALIAS))
		  (DEFINE (,(symbol-append 'NAME-> sing) NAME)
		    (LET LOOP ((NAME NAME))
		      (LET ((ALIAS (HASH-TABLE/GET ,aliases NAME #F)))
			(COND ((SYMBOL? ALIAS) (LOOP ALIAS))
			      ((PROCEDURE? ALIAS) (LOOP (ALIAS)))
			      ((HASH-TABLE/GET ,plur NAME #F))
			      (else (ERROR:BAD-RANGE-ARGUMENT NAME #F))))))))))
	 (ill-formed-syntax form)))))

(define-name-map decoder)
(define-name-map encoder)
(define-name-map normalizer)
(define-name-map denormalizer)

(define (known-input-coding? name)
  (or (hash-table/get decoder-aliases name #f)
      (hash-table/get decoders name #f)))

(define (known-input-codings)
  (append (hash-table/key-list decoder-aliases)
	  (hash-table/key-list decoders)))

(define (known-output-coding? name)
  (or (hash-table/get encoder-aliases name #f)
      (hash-table/get encoders name #f)))

(define (known-output-codings)
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
	   (let ((table (make-eq-hash-table)))
	     (for-each (lambda (n.d)
			 (hash-table/put! table (cdr n.d) (car n.d)))
		       alist)
	     table)))
	(convert-forward
	 (lambda (alist)
	   (let ((table (make-eq-hash-table)))
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
		   (let ((plur (symbol-append sing 'S))
			 (aliases (symbol-append sing '-ALIASES))
			 (proc (symbol-append 'DEFINE- sing)))
		     (let ((aproc (symbol-append proc '-ALIAS)))
		       `(BEGIN
			  (SET! ,(symbol-append plur '-REVERSE)
				(CONVERT-REVERSE ,plur))
			  (SET! ,plur (CONVERT-FORWARD ,plur))
			  (SET! ,proc ,(symbol-append proc '/POST-BOOT))
			  (SET! ,aliases (CONVERT-FORWARD ,aliases))
			  (SET! ,aproc ,(symbol-append aproc '/POST-BOOT))))))
		 (ill-formed-syntax form))))))
      (initialize-name-map decoder)
      (initialize-name-map encoder)
      (initialize-name-map normalizer)
      (initialize-name-map denormalizer)))
  (set! binary-decoder (name->decoder 'ISO-8859-1))
  (set! binary-encoder (name->encoder 'ISO-8859-1))
  (set! binary-normalizer (name->normalizer 'BINARY))
  (set! binary-denormalizer (name->denormalizer 'BINARY))
  unspecific)

(define binary-decoder)
(define binary-encoder)
(define binary-normalizer)
(define binary-denormalizer)

;;;; Input buffer

(define-integrable page-size #x1000)
(define-integrable max-char-bytes 4)

(define-integrable byte-buffer-length
  (fix:+ page-size
	 (fix:- (fix:* max-char-bytes 2) 1)))

(define-structure (input-buffer (constructor %make-input-buffer))
  (channel #f read-only #t)
  (bytes #f read-only #t)
  start
  end
  decode
  normalize)

(define (make-input-buffer channel)
  (make-input-buffer-1 channel 'TEXT))

(define (make-binary-input-buffer channel)
  (make-input-buffer-1 channel 'BINARY))

(define (make-input-buffer-1 channel type)
  (%make-input-buffer channel
		      (make-string byte-buffer-length)
		      byte-buffer-length
		      byte-buffer-length
		      (name->decoder type)
		      (name->normalizer (line-ending channel type #f))))

(define-integrable (input-buffer-open? ib)
  (channel-open? (input-buffer-channel ib)))

(define (close-input-buffer ib)
  (set-input-buffer-start! ib 0)
  (set-input-buffer-end! ib 0)
  (channel-close (input-buffer-channel ib)))

(define-integrable (input-buffer-port ib)
  (channel-port (input-buffer-channel ib)))

(define-integrable (input-buffer-at-eof? ib)
  (fix:= (input-buffer-end ib) 0))

(define-integrable (input-buffer-byte-count ib)
  (fix:- (input-buffer-end ib) (input-buffer-start ib)))

(define (read-next-char ib)
  ((input-buffer-normalize ib) ib))

(define (decode-char ib)
  (and (fix:< (input-buffer-start ib) (input-buffer-end ib))
       (let ((cp ((input-buffer-decode ib) ib)))
	 (and cp
	      (integer->char cp)))))

(define (fill-input-buffer ib)
  (if (input-buffer-at-eof? ib)
      'EOF
      (begin
	(justify-input-buffer ib)
	(let loop ()
	  (let ((n (read-bytes ib)))
	    (cond ((not n) 'WOULD-BLOCK)
		  ((fix:> n 0) 'OK)
		  (else 'EOF)))))))

(define (buffer-has-input? ib)
  (let ((bs (input-buffer-start ib)))
    (if (read-next-char ib)
	(begin
	  (set-input-buffer-start! ib bs)
	  #t)
	(and (not (input-buffer-at-eof? ib))
	     (channel-has-input? (input-buffer-channel ib))
	     (begin
	       (justify-input-buffer ib)
	       (read-bytes ib)
	       (let ((bs (input-buffer-start ib)))
		 (and (read-next-char ib)
		      (begin
			(set-input-buffer-start! ib bs)
			#t))))))))

(define (justify-input-buffer ib)
  (let ((bs (input-buffer-start ib))
	(be (input-buffer-end ib)))
    (if (and (fix:< 0 bs) (fix:< bs be))
	(let ((bv (input-buffer-bytes ib)))
	  (do ((i bs (fix:+ i 1))
	       (j 0 (fix:+ j 1)))
	      ((not (fix:< i be))
	       (set-input-buffer-start! ib 0)
	       (set-input-buffer-end! ib j)
	       j)
	    (string-set! bv j (string-ref bv i)))))))

(define (read-bytes ib)
  (let ((available (input-buffer-byte-count ib)))
    (let ((n
	   (channel-read (input-buffer-channel ib)
			 (input-buffer-bytes ib)
			 available
			 (fix:+ available page-size))))
      (if (and n (fix:> n 0))
	  (begin
	    (set-input-buffer-start! ib 0)
	    (set-input-buffer-end! ib (fix:+ available n))))
      n)))

(define (set-input-buffer-coding! ib coding)
  (set-input-buffer-decode! ib (name->decoder coding)))

(define (set-input-buffer-line-ending! ib name)
  (set-input-buffer-normalize! ib (name->normalizer name)))

(define (input-buffer-contents ib)
  (substring (input-buffer-bytes ib)
	     (input-buffer-start ib)
	     (input-buffer-end ib)))

(define (set-input-buffer-contents! ib contents)
  (guarantee-string contents 'SET-INPUT-BUFFER-CONTENTS!)
  (let ((bv (input-buffer-bytes ib)))
    (let ((n (fix:min (string-length contents) (string-length bv))))
      (substring-move! contents 0 n bv 0)
      (set-input-buffer-start! ib 0)
      (set-input-buffer-end! ib n))))

(define (read-substring:wide-string ib string start end)
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

(define (read-substring:string ib string start end)
  (if (input-buffer-in-8-bit-mode? ib)
      (let ((bv (input-buffer-bytes ib))
	    (bs (input-buffer-start ib))
	    (be (input-buffer-end ib)))
	(if (fix:< bs be)
	    (let ((n (fix:min (fix:- be bs) (fix:- end start))))
	      (let ((be (fix:+ bs n)))
		(%substring-move! bv bs be string start)
		(set-input-buffer-start! ib be)
		n))
	    (channel-read (input-buffer-channel ib) string start end)))
      (read-to-8-bit ib string start end)))

(define (read-substring:external-string ib string start end)
  (if (input-buffer-in-8-bit-mode? ib)
      (let ((bv (input-buffer-bytes ib))
	    (bs (input-buffer-start ib))
	    (be (input-buffer-end ib)))
	(if (fix:< bs be)
	    (let ((n (min (fix:- be bs) (- end start))))
	      (let ((be (fix:+ bs n)))
		(xsubstring-move! bv bs be string start)
		(set-input-buffer-start! ib be)
		n))
	    (channel-read (input-buffer-channel ib) string start end)))
      (let ((bounce (make-string page-size))
	    (be (min page-size (- end start))))
	(let ((n (read-to-8-bit ib bounce 0 be)))
	  (if (and n (fix:> n 0))
	      (substring-move! bounce 0 n string start))
	  n))))

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
  (channel #f read-only #t)
  (bytes #f read-only #t)
  start
  encode
  denormalize)

(define (make-output-buffer channel)
  (make-output-buffer-1 channel 'TEXT))

(define (make-binary-output-buffer channel)
  (make-output-buffer-1 channel 'BINARY))

(define (make-output-buffer-1 channel type)
  (%make-output-buffer channel
		       (make-string byte-buffer-length)
		       0
		       (name->encoder type)
		       (name->denormalizer (line-ending channel type #t))))

(define-integrable (output-buffer-open? ob)
  (channel-open? (output-buffer-channel ob)))

(define (close-output-buffer ob)
  (force-drain-output-buffer ob)
  (channel-close (output-buffer-channel ob)))

(define-integrable (output-buffer-port ob)
  (channel-port (output-buffer-channel ob)))

(define-integrable (output-buffer-end ob)
  (string-length (output-buffer-bytes ob)))

(define (flush-output-buffer buffer)
  (set-output-buffer-start! buffer 0))

(define (force-drain-output-buffer ob)
  (with-channel-blocking (output-buffer-channel ob) #t
    (lambda ()
      (let loop ()
	(drain-output-buffer ob)
	(if (fix:> (output-buffer-start ob) 0)
	    (loop))))))

(define (drain-output-buffer ob)
  (let ((bs (output-buffer-start ob)))
    (if (fix:> bs 0)
	(let ((bv (output-buffer-bytes ob)))
	  (let ((n
		 (channel-write (output-buffer-channel ob)
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
	 #t)))

(define (output-buffer-in-8-bit-mode? ib)
  (and (eq? (output-buffer-encode ib) binary-encoder)
       (eq? (output-buffer-denormalize ib) binary-denormalizer)))

(define (encode-char ob char)
  (set-output-buffer-start!
   ob
   (fix:+ (output-buffer-start ob)
	  ((output-buffer-encode ob) ob (char->integer char)))))

(define (set-output-buffer-coding! ib coding)
  (set-output-buffer-encode! ib (name->encoder coding)))

(define (set-output-buffer-line-ending! ib name)
  (set-output-buffer-denormalize! ib (name->denormalizer name)))

(define (write-substring:string ob string start end)
  (if (output-buffer-in-8-bit-mode? ob)
      (let ((bv (output-buffer-bytes ob))
	    (be (output-buffer-end ob)))
	(let loop ((i start) (bi (output-buffer-start ob)))
	  (if (fix:< i end)
	      (if (fix:< bi be)
		  (begin
		    (string-set! bv bi (string-ref string i))
		    (loop (fix:+ i 1) (fix:+ bi 1)))
		  (begin
		    (set-output-buffer-start! ob be)
		    (let ((n (drain-output-buffer ob)))
		      (cond ((not n) (and (fix:> i start) (fix:- i start)))
			    ((fix:> n 0) (loop i (output-buffer-start ob)))
			    (else (fix:- i start))))))
	      (begin
		(set-output-buffer-start! ob bi)
		(fix:- end start)))))
      (let loop ((i start))
	(if (fix:< i end)
	    (if (write-next-char ob (string-ref string i))
		(loop (fix:+ i 1))
		(let ((n (drain-output-buffer ob)))
		  (cond ((not n) (and (fix:> i start) (fix:- i start)))
			((fix:> n 0) (loop i))
			(else (fix:- i start)))))
	    (fix:- end start)))))

(define (write-substring:wide-string ob string start end)
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

(define (write-substring:external-string ob string start end)
  (let ((bounce (make-string #x1000)))
    (let loop ((i start))
      (if (< i end)
	  (let ((n (min (- end i) #x1000)))
	    (substring-move! string i (+ i n) bounce 0)
	    (let ((m (write-substring:string ob bounce 0 n)))
	      (cond ((not m)
		     (and (> i start)
			  (- i start)))
		    ((fix:> m 0)
		     (if (fix:< m n)
			 (- (+ i m) start)
			 (loop (+ i n))))
		    (else (- i start)))))
	  (- end start)))))

;;;; ISO-8859 codecs

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

(define-decoder-alias 'BINARY 'ISO-8859-1)
(define-encoder-alias 'BINARY 'ISO-8859-1)
(define-decoder-alias 'TEXT 'ISO-8859-1)
(define-encoder-alias 'TEXT 'ISO-8859-1)
(define-decoder-alias 'US-ASCII 'ISO-8859-1)
(define-encoder-alias 'ASCII 'ISO-8859-1)

(define-syntax define-iso-8859-map
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(+ DATUM) (cdr form))
	 (let ((name
		(intern
		 (string-append "iso-8859-" (number->string (cadr form))))))
	   (let ((decoding-map (symbol-append 'DECODING-MAP: name))
		 (encoding-map (symbol-append 'ENCODING-MAP: name)))
	     `(BEGIN
		(DEFINE-DECODER ',name
		  (LET ((,decoding-map
			 #(,@(let loop ((i 0))
			       (if (fix:= i #xA1)
				   (cddr form)
				   (cons i (loop (fix:+ i 1))))))))
		    (LAMBDA (IB)
		      (DECODE-ISO-8859 IB ,decoding-map))))
		(DEFINE-ENCODER ',name
		  (LET ((,encoding-map
			 (RECEIVE (LHS RHS)
			     (REVERSE-ISO-8859-MAP ',(cddr form))
			   (CONS LHS RHS))))
		    (LAMBDA (OB CP)
		      (ENCODE-ISO-8859 OB CP ,encoding-map)))))))
	 (ill-formed-syntax form)))))

(define (decode-iso-8859 ib table)
  (let ((cp
	 (vector-ref table
		     (vector-8b-ref (input-buffer-bytes ib)
				    (input-buffer-start ib)))))
    (if cp
	(begin
	  (set-input-buffer-start! ib (fix:+ (input-buffer-start ib) 1))
	  cp)
	(error:char-decoding ib))))

(define (encode-iso-8859 ob cp table)
  (vector-8b-set! (input-buffer-bytes ob)
		  (input-buffer-start ob)
		  (if (fix:< cp #xA1)
		      cp
		      (let ((lhs (car table)))
			(let loop ((low 0) (high (vector-length lhs)))
			  (if (not (fix:< low high))
			      (error:char-encoding ob cp))
			  (let ((i (fix:quotient (fix:+ low high) 2)))
			    (cond ((fix:< cp (vector-ref lhs i))
				   (loop low i))
				  ((fix:> cp (vector-ref lhs i))
				   (loop (fix:+ i 1) high))
				  (else
				   (vector-8b-ref (cdr table) i))))))))
  1)

(define (reverse-iso-8859-map code-points)
  (let ((lhs (make-vector #x5F))
	(rhs (make-string #x5F)))
    (do ((alist (sort (let loop ((code-points code-points) (i #xA1))
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
    (values lhs rhs)))

(define-iso-8859-map 2
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

(define-iso-8859-map 3
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

(define-iso-8859-map 4
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

(define-iso-8859-map 5
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

(define-iso-8859-map 6
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
  #f     #f     #f     #f     #f     #f     #f    )

(define-iso-8859-map 7
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
  #x03C9 #x03CA #x03CB #x03CC #x03CD #x03CE #f    )

(define-iso-8859-map 8
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
  #x05E9 #x05EA #f     #f     #x200E #x200F #f    )

(define-iso-8859-map 9
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

(define-iso-8859-map 10
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

(define-iso-8859-map 11
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
  #x0E59 #x0E5A #x0E5B #f     #f     #f     #f    )

(define-iso-8859-map 13
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

(define-iso-8859-map 14
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

(define-iso-8859-map 15
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

(define-iso-8859-map 16
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

(let ((alias
       (lambda ()
	 (if (host-big-endian?)
	     'UTF-16BE
	     'UTF-16LE))))
  (define-decoder-alias 'UTF-16 alias)
  (define-encoder-alias 'UTF-16 alias))

(define-decoder 'UTF-16BE
  (lambda (ib)
    (decode-utf-16 ib be-bytes->digit16)))

(define-decoder 'UTF-16LE
  (lambda (ib)
    (decode-utf-16 ib le-bytes->digit16)))

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
	   (if (high-surrogate? d0)
	       (and (fix:<= (fix:+ bs 4) (input-buffer-end ib))
		    (let ((d1
			   (combine (get-byte bv bs 2)
				    (get-byte bv bs 3))))
		      (if (low-surrogate? d1)
			  (done (combine-surrogates d0 d1) (fix:+ bs 4))
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
	   (let ((h (fix:or (fix:lsh (fix:- cp #x10000) -10) #xD800))
		 (l (fix:or (fix:and (fix:- cp #x10000) #x3FF) #xDC00)))
	     (put-byte bv bs 0 (first-byte h))
	     (put-byte bv bs 1 (second-byte h))
	     (put-byte bv bs 2 (first-byte l))
	     (put-byte bv bs 3 (second-byte l)))
	   4)
	  (else
	   (error:char-encoding ob cp)))))

(define-integrable (be-bytes->digit16 b0 b1) (fix:or (fix:lsh b0 8) b1))
(define-integrable (le-bytes->digit16 b0 b1) (fix:or b0 (fix:lsh b1 8)))
(define-integrable (high-byte d) (fix:lsh d -8))
(define-integrable (low-byte d) (fix:and d #xFF))
(define-integrable (high-surrogate? n) (fix:= (fix:and #xFC00 n) #xD800))
(define-integrable (low-surrogate? n) (fix:= (fix:and #xFC00 n) #xDC00))

(define-integrable (combine-surrogates n0 n1)
  (fix:+ (fix:or (extract n0 #x3FF 10)
		 (extract n1 #x3FF 0))
	 #x10000))

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
	     (if (unicode-code-point? cp)
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
	     (if (unicode-code-point? cp)
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

;;;; Normalizers

(define-normalizer 'NEWLINE
  (lambda (ib)
    (decode-char ib)))

(define-denormalizer 'NEWLINE
  (lambda (ob char)
    (encode-char ob char)))

(define-normalizer-alias 'LF 'NEWLINE)
(define-denormalizer-alias 'LF 'NEWLINE)
(define-normalizer-alias 'BINARY 'NEWLINE)
(define-denormalizer-alias 'BINARY 'NEWLINE)

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