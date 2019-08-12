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

;;;; Generic I/O Ports
;;; package: (runtime generic-i/o-port)

(declare (usual-integrations))

(define (make-generic-i/o-port binary-port #!optional type caller . extra-state)
  (let ((port
	 (make-textual-port (if (default-object? type)
				(generic-i/o-port-type
				 (source-type (binary-port-source binary-port))
				 (sink-type (binary-port-sink binary-port)))
				type)
			    (apply make-gstate binary-port 'text 'text caller
				   extra-state)
			    caller)))
    (let ((ib (port-input-buffer port)))
      (if ib
	  (set-input-buffer-port! ib port)))
    (let ((ob (port-output-buffer port)))
      (if ob
	  (set-output-buffer-port! ob port)))
    port))

(define (binary->textual-port binary-port)
  (make-generic-i/o-port binary-port (default-object) 'binary->textual-port))

(define (source-type source)
  (cond ((not source) #f)
	((input-source-channel source) 'channel)
	(else #t)))

(define (sink-type sink)
  (cond ((not sink) #f)
	((output-sink-channel sink) 'channel)
	(else #t)))

(define (generic-i/o-port-type source sink)
  (case source
    ((#f)
     (case sink
       ((#f) generic-type00)
       ((channel) generic-type02)
       (else generic-type01)))
    ((channel)
     (case sink
       ((#f) generic-type20)
       ((channel) generic-type22)
       (else generic-type21)))
    (else
     (case sink
       ((#f) generic-type10)
       ((channel) generic-type12)
       (else generic-type11)))))

(define (generic-i/o-port->binary-port port)
  (if (port-input-buffer port)
      (input-port->binary-port port)
      (output-port->binary-port port)))

(define (input-port->binary-port port)
  (input-buffer-binary-port (port-input-buffer port)))

(define (output-port->binary-port port)
  (output-buffer-binary-port (port-output-buffer port)))

(define (make-gstate binary-port coder-name normalizer-name caller . extra)
  (%make-gstate (and (binary-input-port? binary-port)
		     (make-input-buffer binary-port
					coder-name
					normalizer-name
					caller))
		(and (binary-output-port? binary-port)
		     (make-output-buffer binary-port
					 coder-name
					 normalizer-name
					 caller))
		coder-name
		normalizer-name
		(list->vector extra)))

(define-record-type <gstate>
    (%make-gstate input-buffer output-buffer coder-name normalizer-name extra)
    gstate?
  (input-buffer gstate-input-buffer set-gstate-input-buffer!)
  (output-buffer gstate-output-buffer set-gstate-output-buffer!)
  (coder-name gstate-coder-name
	      set-gstate-coder-name!)
  (normalizer-name gstate-normalizer-name
		   set-gstate-normalizer-name!)
  (extra gstate-extra))

(define (port-input-buffer port)
  (gstate-input-buffer (textual-port-state port)))

(define (port-output-buffer port)
  (gstate-output-buffer (textual-port-state port)))

(define (generic-i/o-port-accessor index)
  (guarantee index-fixnum? index 'generic-i/o-port-accessor)
  (lambda (port)
    (vector-ref (gstate-extra (textual-port-state port)) index)))

(define (generic-i/o-port-modifier index)
  (guarantee index-fixnum? index 'generic-i/o-port-modifier)
  (lambda (port object)
    (vector-set! (gstate-extra (textual-port-state port)) index object)))

(define (replace-binary-port! port binary-port)
  (let ((gstate (textual-port-state port)))
    (set-gstate-input-buffer!
     gstate
     (and (binary-input-port? binary-port)
	  (let ((buffer
		 (make-input-buffer binary-port
				    (gstate-coder-name gstate)
				    (gstate-normalizer-name gstate)
				    'replace-binary-port!)))

	    (set-input-buffer-port! buffer port)
	    buffer)))
    (set-gstate-output-buffer!
     gstate
     (and (binary-output-port? binary-port)
	  (let ((buffer
		 (make-output-buffer binary-port
				     (gstate-coder-name gstate)
				     (gstate-normalizer-name gstate)
				     'gstate-replace-binary-port!)))
	    (set-output-buffer-port! buffer port)
	    buffer)))))

(define generic-type00)
(define generic-type10)
(define generic-type20)
(define generic-type01)
(define generic-type02)
(define generic-type11)
(define generic-type21)
(define generic-type12)
(define generic-type22)
(add-boot-init!
 (lambda ()
   (let ((ops:in1
	  `((char-ready? ,generic-io/char-ready?)
	    (close-input ,generic-io/close-input)
	    (eof? ,generic-io/eof?)
	    (input-line ,generic-io/input-line)
	    (input-open? ,generic-io/input-open?)
	    (peek-char ,generic-io/peek-char)
	    (read-char ,generic-io/read-char)
	    (read-substring ,generic-io/read-substring)
	    (unread-char ,generic-io/unread-char)))
	 (ops:in2
	  `((input-channel ,generic-io/input-channel)))
	 (ops:out1
	  `((buffered-output-bytes ,generic-io/buffered-output-bytes)
	    (bytes-written ,generic-io/bytes-written)
	    (close-output ,generic-io/close-output)
	    (flush-output ,generic-io/flush-output)
	    (output-column ,generic-io/output-column)
	    (output-open? ,generic-io/output-open?)
	    (write-char ,generic-io/write-char)
	    (write-substring ,generic-io/write-substring)))
	 (ops:out2
	  `((output-channel ,generic-io/output-channel)
	    (synchronize-output ,generic-io/synchronize-output)))
	 (other-operations
	  `((char-set ,generic-io/char-set)
	    (close ,generic-io/close)
	    (coding ,generic-io/coding)
	    (known-coding? ,generic-io/known-coding?)
	    (known-codings ,generic-io/known-codings)
	    (known-line-ending? ,generic-io/known-line-ending?)
	    (known-line-endings ,generic-io/known-line-endings)
	    (line-ending ,generic-io/line-ending)
	    (open? ,generic-io/open?)
	    (set-coding ,generic-io/set-coding)
	    (set-line-ending ,generic-io/set-line-ending)
	    (supports-coding? ,generic-io/supports-coding?)
	    (write-self ,generic-io/write-self))))
     (let ((make-type
	    (lambda ops
	      (make-textual-port-type (append (apply append ops)
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
       (set! generic-type22 (make-type ops:in1 ops:in2 ops:out1 ops:out2))))))

;;;; Input operations

(define (generic-io/char-ready? port)
  (let ((ib (port-input-buffer port)))
    (or (input-buffer-peeked ib)
	(u8-ready? (input-buffer-binary-port ib)))))

(define (generic-io/peek-char port)
  (let ((ib (port-input-buffer port)))
    (or (input-buffer-peeked ib)
	(let ((char ((input-buffer-normalizer ib) ib)))
	  (if (char? char)
	      (set-input-buffer-peeked! ib char))
	  char))))

(define (generic-io/read-char port)
  (let ((ib (port-input-buffer port)))
    (let ((char (input-buffer-peeked ib)))
      (if char
	  (begin
	    (set-input-buffer-peeked! ib #f)
	    char)
	  (let ((char ((input-buffer-normalizer ib) ib)))
	    (if (eq? char #\newline)
		(set-input-buffer-line! ib (fix:+ (input-buffer-line ib) 1)))
	    char)))))

(define (generic-io/unread-char port char)
  (let ((ib (port-input-buffer port)))
    (guarantee char? char 'unread-char)
    (if (input-buffer-peeked ib)
	(error "Can't unread another char:" char (input-buffer-port ib)))
    (set-input-buffer-peeked! ib char)
    ;; If unreading a newline, decrement the line count.
    (if (char=? char #\newline)
	(set-input-buffer-line! ib (fix:- (input-buffer-line ib) 1)))))

(define (generic-io/read-substring port string start end)
  (let loop ((index start))
    (if (fix:< index end)
	(let ((char (generic-io/read-char port)))
	  (cond ((not char)
		 (if (fix:< start index)
		     (fix:- index start)
		     #f))
		((eof-object? char)
		 (fix:- index start))
		(else
		 (string-set! string index char)
		 (loop (fix:+ index 1)))))
	(fix:- end start))))

(define (generic-io/input-line port)
  (input-buffer-line (port-input-buffer port)))

(define (generic-io/eof? port)
  (input-buffer-at-eof? (port-input-buffer port)))

(define (generic-io/input-channel port)
  (let ((ib (port-input-buffer port)))
    (if (not ib)
	(error:bad-range-argument port #f))
    (input-buffer-channel ib)))

(define (generic-io/buffer-contents port)
  (binary-input-port-buffer-contents (input-port->binary-port port)))

(define (generic-io/set-buffer-contents port contents)
  (set-binary-input-port-buffer-contents! (input-port->binary-port port)
					  contents))

;;;; Output operations

(define (generic-io/write-char port char)
  (guarantee char? char)
  (write-next-char (port-output-buffer port) char))

(define (generic-io/write-substring port string start end)
  (let ((ob (port-output-buffer port)))
    (let loop ((index start))
      (if (fix:< index end)
	  (let ((n (write-next-char ob (string-ref string index))))
	    (cond ((and n (fix:> n 0)) (loop (fix:+ index 1)))
		  ((fix:< start index) (fix:- index start))
		  (else n)))
	  (fix:- end start)))))

(define (generic-io/flush-output port)
  (flush-binary-output-port (output-port->binary-port port)))

(define (generic-io/output-column port)
  (output-buffer-column (port-output-buffer port)))

(define (generic-io/output-channel port)
  (let ((ob (port-output-buffer port)))
    (if (not ob)
	(error:bad-range-argument port #f))
    (output-buffer-channel ob)))

(define (generic-io/synchronize-output port)
  (synchronize-binary-output-port (output-port->binary-port port)))

(define (generic-io/buffered-output-bytes port)
  (binary-output-port-buffered-byte-count (output-port->binary-port port)))

(define (generic-io/bytes-written port)
  (output-buffer-total (port-output-buffer port)))

;;;; Non-specific operations

(define (generic-io/close port)
  (let ((ib (port-input-buffer port))
	(ob (port-output-buffer port)))
    (cond ((and ib
		ob
		(eq? (input-buffer-binary-port ib)
		     (output-buffer-binary-port ob)))
	   (close-binary-port (input-buffer-binary-port ib)))
	  (ib (close-binary-input-port (input-buffer-binary-port ib)))
	  (ob (close-binary-output-port (output-buffer-binary-port ob))))))

(define (generic-io/close-input port)
  (close-binary-input-port (input-port->binary-port port)))

(define (generic-io/close-output port)
  (close-binary-output-port (output-port->binary-port port)))

(define (generic-io/open? port)
  (and (let ((ib (port-input-buffer port)))
	 (if ib
	     (input-buffer-open? ib)
	     #t))
       (let ((ob (port-output-buffer port)))
	 (if ob
	     (output-buffer-open? ob)
	     #t))))

(define (generic-io/input-open? port)
  (let ((ib (port-input-buffer port)))
    (and ib
	 (input-buffer-open? ib))))

(define (generic-io/output-open? port)
  (let ((ob (port-output-buffer port)))
    (and ob
	 (output-buffer-open? ob))))

(define (generic-io/write-self port output-port)
  (let ((ib (port-input-buffer port))
	(ob (port-output-buffer port)))
    (let ((ic (and ib (input-buffer-channel ib)))
	  (oc (and ob (output-buffer-channel ob))))
      (cond ((and ic oc (not (eq? ic oc)))
	     (write-string " for channels: " output-port)
	     (write ic output-port)
	     (write-string " " output-port)
	     (write oc output-port))
	    ((or ic oc)
	     (write-string " for channel: " output-port)
	     (write (or ic oc) output-port))))))

(define (generic-io/supports-coding? port)
  port
  #t)

(define (generic-io/coding port)
  (gstate-coder-name (textual-port-state port)))

(define (generic-io/set-coding port name)
  (let ((ib (port-input-buffer port)))
    (if ib
	(set-input-buffer-decoder! ib (name->decoder name 'port/set-coding))))
  (let ((ob (port-output-buffer port)))
    (if ob
	(set-output-buffer-encoder! ob
				    (name->encoder name 'port/set-coding))))
  (set-gstate-coder-name! (textual-port-state port) name))

(define (generic-io/known-coding? port coding)
  (and (if (input-port? port) (known-input-port-coding? coding) #t)
       (if (output-port? port) (known-output-port-coding? coding) #t)))

(define (generic-io/known-codings port)
  (cond ((i/o-port? port)
	 (lset-intersection eq?
			    (known-input-port-codings)
			    (known-output-port-codings)))
	((input-port? port) (known-input-port-codings))
	((output-port? port) (known-output-port-codings))
	(else '())))

(define (generic-io/line-ending port)
  (gstate-normalizer-name (textual-port-state port)))

(define (generic-io/set-line-ending port name)
  (let ((ib (port-input-buffer port)))
    (if ib
	(set-input-buffer-normalizer!
	 ib
	 (name->normalizer (line-ending (input-buffer-channel ib) name #f
					'port/set-line-ending)
			   'port/set-line-ending))))
  (let ((ob (port-output-buffer port)))
    (if ob
	(set-output-buffer-denormalizer!
	 ob
	 (name->denormalizer (line-ending (output-buffer-channel ob) name #t
					  'port/set-line-ending)
			     'port/set-line-ending))))
  (set-gstate-normalizer-name! (textual-port-state port) name))

(define (generic-io/known-line-ending? port line-ending)
  (and (if (input-port? port) (known-input-line-ending? line-ending) #t)
       (if (output-port? port) (known-output-line-ending? line-ending) #t)))

(define (generic-io/known-line-endings port)
  (cond ((i/o-port? port)
	 (lset-intersection eq?
			    (known-input-line-endings)
			    (known-output-line-endings)))
	((input-port? port) (known-input-line-endings))
	((output-port? port) (known-output-line-endings))
	(else '())))

(define (line-ending channel name for-output? caller)
  (guarantee symbol? name caller)
  (if (and for-output?
	   (known-input-line-ending? name)
	   (not (known-output-line-ending? name)))
      (if (and channel
	       (eq? (channel-type channel) 'tcp-stream-socket))
	  'crlf
	  (default-line-ending))
      name))

;;;; Name maps

(define-syntax define-name-map
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(symbol symbol) (cdr form))
	 (let ((sing (cadr form))
	       (deref (caddr form)))
	   (let ((plur (symbol sing 's))
		 (proc (symbol 'define- sing)))
	     (let ((rev (symbol plur '-reverse)))
	       `(begin
		  (define ,plur '())
		  (define ,rev)
		  (define (,proc name ,sing)
		    (set! ,plur (cons (cons name ,sing) ,plur))
		    name)
		  (define (,(symbol proc '/post-boot) name ,sing)
		    (let ((old (hash-table-ref/default ,plur name #f)))
		      (if old
			  (hash-table-delete! ,rev old)))
		    (hash-table-set! ,plur name ,sing)
		    (hash-table-set! ,rev ,sing name))
		  (define (,(symbol 'name-> sing) name #!optional caller)
		    (or (hash-table-ref/default ,plur (,deref name) #f)
			(error:bad-range-argument name caller)))))))
	 (ill-formed-syntax form)))))

(define-name-map decoder dereference-coding-alias)
(define-name-map encoder dereference-coding-alias)
(define-name-map normalizer dereference-line-ending-alias)
(define-name-map denormalizer dereference-line-ending-alias)

(define-syntax define-alias-map
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(symbol) (cdr form))
	 (let ((root (cadr form)))
	   (let ((aliases (symbol root '-aliases))
		 (aproc (symbol 'define- root '-alias)))
	     `(begin
		(define ,aliases '())
		(define (,aproc name alias)
		  (set! ,aliases (cons (cons name alias) ,aliases))
		  name)
		(define (,(symbol aproc '/post-boot) name alias)
		  (hash-table-set! ,aliases name alias))
		(define (,(symbol 'dereference- root '-alias) name)
		  (dereference-alias name ,aliases)))))
	 (ill-formed-syntax form)))))

(define-alias-map coding)
(define-alias-map line-ending)

(define (dereference-alias name table)
  (let ((alias (hash-table-ref/default table name #f)))
    (cond ((symbol? alias) (dereference-alias alias table))
	  ((procedure? alias) (dereference-alias (alias) table))
	  (else name))))

(define (known-input-port-coding? name)
  (or (hash-table-ref/default decoders name #f)
      (hash-table-ref/default coding-aliases name #f)))

(define (known-input-port-codings)
  (append (hash-table-keys decoders)
	  (hash-table-keys coding-aliases)))

(define (known-output-port-coding? name)
  (or (hash-table-ref/default encoders name #f)
      (hash-table-ref/default coding-aliases name #f)))

(define (known-output-port-codings)
  (append (hash-table-keys encoders)
	  (hash-table-keys coding-aliases)))

(define (known-input-line-ending? name)
  (or (hash-table-ref/default normalizers name #f)
      (hash-table-ref/default line-ending-aliases name #f)))

(define (known-input-line-endings)
  (append (hash-table-keys normalizers)
	  (hash-table-keys line-ending-aliases)))

(define (known-output-line-ending? name)
  (or (hash-table-ref/default denormalizers name #f)
      (hash-table-ref/default line-ending-aliases name #f)))

(define (known-output-line-endings)
  (append (hash-table-keys denormalizers)
	  (hash-table-keys line-ending-aliases)))

(define (generic-io/char-set port)
  (coder-char-set (gstate-coder-name (textual-port-state port))))

(define (coder-char-set coder-name)
  (case (dereference-coding-alias coder-name)
    ((iso-8859-1) char-set:iso-8859-1)
    ((iso-8859-2) char-set:iso-8859-2)
    ((iso-8859-3) char-set:iso-8859-3)
    ((iso-8859-4) char-set:iso-8859-4)
    ((iso-8859-5) char-set:iso-8859-5)
    ((iso-8859-6) char-set:iso-8859-6)
    ((iso-8859-7) char-set:iso-8859-7)
    ((iso-8859-8) char-set:iso-8859-8)
    ((iso-8859-9) char-set:iso-8859-9)
    ((iso-8859-10) char-set:iso-8859-10)
    ((iso-8859-11) char-set:iso-8859-11)
    ((iso-8859-13) char-set:iso-8859-13)
    ((iso-8859-14) char-set:iso-8859-14)
    ((iso-8859-15) char-set:iso-8859-15)
    ((iso-8859-16) char-set:iso-8859-16)
    ((windows-1250) char-set:windows-1250)
    ((windows-1251) char-set:windows-1251)
    ((windows-1252) char-set:windows-1252)
    ((windows-1253) char-set:windows-1253)
    ((windows-1254) char-set:windows-1254)
    ((windows-1255) char-set:windows-1255)
    ((windows-1256) char-set:windows-1256)
    ((windows-1257) char-set:windows-1257)
    ((windows-1258) char-set:windows-1258)
    ((windows-874) char-set:windows-874)
    ((utf-8 utf-16be utf-16le utf-32be utf-32le) char-set:unicode)
    (else (error "Unknown coder name:" coder-name))))

(define binary-decoder)
(define binary-encoder)
(define binary-normalizer)
(define binary-denormalizer)
(add-boot-init!
 (lambda ()
   (let ((convert-reverse
	  (lambda (alist)
	    (let ((table (make-strong-eq-hash-table)))
	      (for-each (lambda (n.d)
			  (hash-table-set! table (cdr n.d) (car n.d)))
			alist)
	      table)))
	 (convert-forward
	  (lambda (alist)
	    (let ((table (make-strong-eq-hash-table)))
	      (for-each (lambda (n.d)
			  (hash-table-set! table (car n.d) (cdr n.d)))
			alist)
	      table))))
     (let-syntax
	 ((initialize-name-map
	   (sc-macro-transformer
	    (lambda (form environment)
	      environment
	      (if (syntax-match? '(symbol) (cdr form))
		  (let ((sing (cadr form)))
		    (let ((plur (symbol sing 's))
			  (proc (symbol 'define- sing)))
		      `(begin
			 (set! ,(symbol plur '-reverse) (convert-reverse ,plur))
			 (set! ,plur (convert-forward ,plur))
			 (set! ,proc ,(symbol proc '/post-boot)))))
		  (ill-formed-syntax form))))))
       (initialize-name-map decoder)
       (initialize-name-map encoder)
       (initialize-name-map normalizer)
       (initialize-name-map denormalizer))
     (let-syntax
	 ((initialize-name-map
	   (sc-macro-transformer
	    (lambda (form environment)
	      environment
	      (if (syntax-match? '(symbol) (cdr form))
		  (let ((root (cadr form)))
		    (let ((aliases (symbol root '-aliases))
			  (proc (symbol 'define- root '-alias)))
		      `(begin
			 (set! ,aliases (convert-forward ,aliases))
			 (set! ,proc ,(symbol proc '/post-boot)))))
		  (ill-formed-syntax form))))))
       (initialize-name-map coding)
       (initialize-name-map line-ending)))
   (set! binary-decoder (name->decoder 'binary))
   (set! binary-encoder (name->encoder 'binary))
   (set! binary-normalizer (name->normalizer 'binary))
   (set! binary-denormalizer (name->denormalizer 'binary))
   unspecific))

(define (define-coding-aliases name aliases)
  (for-each (lambda (alias)
	      (define-coding-alias alias name))
	    aliases))

(define (primary-input-port-codings)
  (cons 'us-ascii (hash-table-keys decoders)))

(define (primary-output-port-codings)
  (cons 'us-ascii (hash-table-keys encoders)))

(define max-char-bytes 4)

;;;; Input buffer

(define (make-input-buffer binary-port coder-name normalizer-name caller)
  (%make-input-buffer binary-port
		      (name->decoder coder-name caller)
		      (name->normalizer
		       (line-ending (binary-input-port-channel binary-port)
				    normalizer-name
				    #f
				    caller)
		       caller)
		      (make-bytevector max-char-bytes)
		      #f
		      '()
		      0))

(define-record-type <input-buffer>
    (%make-input-buffer binary-port decoder normalizer
			bytes peeked decoded-chars line)
    input-buffer?
  (binary-port input-buffer-binary-port)
  (decoder input-buffer-decoder
	   set-input-buffer-decoder!)
  (normalizer input-buffer-normalizer
	      set-input-buffer-normalizer!)
  (bytes input-buffer-bytes)
  (peeked input-buffer-peeked
	  set-input-buffer-peeked!)
  (decoded-chars input-buffer-decoded-chars
		 set-input-buffer-decoded-chars!)
  (line input-buffer-line
	set-input-buffer-line!))

(define (input-buffer-open? ib)
  (binary-input-port-open? (input-buffer-binary-port ib)))

(define (input-buffer-channel ib)
  (input-source-channel (%input-buffer-source ib)))

(define (input-buffer-port ib)
  (input-source-port (%input-buffer-source ib)))

(define (set-input-buffer-port! ib port)
  (set-input-source-port! (%input-buffer-source ib) port))

(define (%input-buffer-source ib)
  (binary-input-port-source (input-buffer-binary-port ib)))

(define (input-buffer-at-eof? ib)
  (binary-input-port-at-eof? (input-buffer-binary-port ib)))

;; Next two for use only in normalizers.

(define (decode-char ib)
  (let ((chars (input-buffer-decoded-chars ib)))
    (if (pair? chars)
	(let ((char (car chars)))
	  (set-input-buffer-decoded-chars! ib (cdr chars))
	  char)
	(let ((u8 (peek-byte ib)))
	  (if (fix:fixnum? u8)
	      ((input-buffer-decoder ib) ib)
	      u8)))))

(define (unread-decoded-char ib char)
  (set-input-buffer-decoded-chars!
     ib
     (cons char (input-buffer-decoded-chars ib))))

;;; Next three for use only in decoders.

(define (peek-byte ib)
  (peek-u8 (input-buffer-binary-port ib)))

(define (read-byte ib)
  (read-u8 (input-buffer-binary-port ib)))

(define (read-bytes! ib start end)
  (let loop ((index start))
    (if (fix:< index end)
	(let ((n
	       (read-bytevector! (input-buffer-bytes ib)
				 (input-buffer-binary-port ib)
				 index
				 end)))
	  (if (not (and (fix:fixnum? n) (fix:> n 0)))
	      (error:char-decoding ib))
	  (loop (fix:+ index n))))))

;;;; Output buffer

(define (make-output-buffer binary-port coder-name normalizer-name caller)
  (%make-output-buffer binary-port
		       (name->encoder coder-name caller)
		       (name->denormalizer
			(line-ending (binary-output-port-channel binary-port)
				     normalizer-name
				     #t
				     caller)
			caller)
		       (make-bytevector max-char-bytes)
		       0
		       0
		       0))

(define-record-type <output-buffer>
    (%make-output-buffer binary-port encoder denormalizer
			 bytes line column total)
    output-buffer?
  (binary-port output-buffer-binary-port)
  (encoder output-buffer-encoder
	   set-output-buffer-encoder!)
  (denormalizer output-buffer-denormalizer
		set-output-buffer-denormalizer!)
  (bytes output-buffer-bytes)
  (line output-buffer-line
	set-output-buffer-line!)
  (column output-buffer-column
	  set-output-buffer-column!)
  (total output-buffer-total
	 set-output-buffer-total!))

(define (output-buffer-open? ob)
  (binary-output-port-open? (output-buffer-binary-port ob)))

(define (output-buffer-channel ob)
  (output-sink-channel (%output-buffer-sink ob)))

(define (output-buffer-port ob)
  (output-sink-port (%output-buffer-sink ob)))

(define (set-output-buffer-port! ob port)
  (set-output-sink-port! (%output-buffer-sink ob) port))

(define (%output-buffer-sink ob)
  (binary-output-port-sink (output-buffer-binary-port ob)))

;; Returns >0 if the character was written in its entirety.
;; Returns 0 if the character wasn't written at all.
;; Returns #f if the write would block.
;; Throws an error if there was a short write.
(define (write-next-char ob char)
  (let ((n ((output-buffer-denormalizer ob) ob char)))
    (if (and n (fix:> n 0))
	(if (char=? char #\newline)
	    (begin
	      (set-output-buffer-column! ob 0)
	      (set-output-buffer-line! ob (fix:+ (output-buffer-line ob) 1)))
	    (let ((column (output-buffer-column ob)))
	      (if column
		  (set-output-buffer-column!
		   ob
		   (cond ((char=? char #\tab)
			  (fix:+ column (fix:- 8 (fix:remainder column 8))))
			 ((and (fix:<= #x20 (char->integer char))
			       (fix:<= (char->integer char) #x7E))
			  (fix:+ column 1))
			 (else #f)))))))
    n))

;; For use only in denormalizers.
;; Returns 1 if the character was written in its entirety.
;; Returns 0 if the character wasn't written at all.
;; Returns #f if the write would block.
;; Throws an error if there was a short write.
(define (encode-char ob char)
  (let ((n ((output-buffer-encoder ob) ob char)))
    (let ((m
	   (write-bytevector (output-buffer-bytes ob)
			     (output-buffer-binary-port ob)
			     0
			     n)))
      (if (and m (fix:> m 0))
	  (begin
	    (if (fix:< m n)
		(error:char-encoding ob char))
	    (set-output-buffer-total! ob (fix:+ (output-buffer-total ob) n))
	    1)
	  m))))

;;;; 8-bit codecs

(define-decoder 'iso-8859-1
  (lambda (ib)
    (let ((sv (read-byte ib)))
      (if (fix:fixnum? sv)
	  (integer->char sv)
	  sv))))

(define-encoder 'iso-8859-1
  (lambda (ob char)
    (let ((cp (char->integer char)))
      (if (not (fix:< cp #x100))
	  (error:char-encoding ob cp))
      (bytevector-u8-set! (output-buffer-bytes ob) 0 cp))
    1))

(define-deferred char-set:iso-8859-1
  (char-set* (iota #x100)))

(define-coding-aliases 'iso-8859-1
  '(iso_8859-1:1987 iso-ir-100 iso_8859-1 latin1 l1 ibm819 cp819 csisolatin1))

(define-coding-aliases 'iso-8859-1
  '(binary))

(define-coding-aliases 'iso-8859-1
  ;; Treat US-ASCII like ISO-8859-1.
  '(us-ascii ansi_x3.4-1968 iso-ir-6 ansi_x3.4-1986 iso_646.irv:1991 ascii
	     iso646-us us ibm367 cp367 csascii))

(define-syntax define-8-bit-codecs
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(symbol + datum) (cdr form))
	 (let ((name (cadr form))
	       (start (caddr form))
	       (code-points (cdddr form)))
	   (let ((alist
		  (sort (filter-map (lambda (cp byte)
				      (and cp
					   (cons cp byte)))
				    code-points
				    (iota (length code-points) start))
			(lambda (a b)
			  (fix:< (car a) (car b))))))
	     (let ((lhs (list->vector (map car alist)))
		   (rhs (map cdr alist)))
	       `(begin
		  (define-decoder ',name
		    (let ((table
			   #(,@(map (lambda (cp)
				      (and cp
					   (integer->char cp)))
				    (let loop ((i 0))
				      (if (fix:< i start)
					  (cons i (loop (fix:+ i 1)))
					  code-points))))))
		      (lambda (ib)
			(decode-8-bit ib table))))
		  (define-encoder ',name
		    (let ((lhs ',lhs)
			  (rhs (apply bytevector ',rhs)))
		      (lambda (ob char)
			(encode-8-bit ob char ,start lhs rhs))))
		  (define-deferred ,(symbol 'char-set: name)
		    (char-set* ',(append (iota #x80)
					 (filter (lambda (cp) cp)
						 code-points))))))))
	 (ill-formed-syntax form)))))

(define (decode-8-bit ib table)
  (let ((u8 (read-byte ib)))
    (if (fix:fixnum? u8)
	(let ((char (vector-ref table u8)))
	  (if (not char)
	      (error:char-decoding ib))
	  char)
	u8)))

(define (encode-8-bit ob char start map-lhs map-rhs)
  (bytevector-u8-set! (output-buffer-bytes ob)
		      0
		      (let ((cp (char->integer char)))
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
				       (bytevector-u8-ref map-rhs i))))))))
  1)

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

(define-coding-aliases 'iso-8859-2
  '(iso_8859-2:1987 iso-ir-101 iso_8859-2 latin2 l2 csisolatin2))

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

(define-coding-aliases 'iso-8859-3
  '(iso_8859-3:1988 iso-ir-109 iso_8859-3 latin3 l3 csisolatin3))

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

(define-coding-aliases 'iso-8859-4
  '(iso_8859-4:1988 iso-ir-110 iso_8859-4 latin4 l4 csisolatin4))

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

(define-coding-aliases 'iso-8859-5
  '(iso_8859-5:1988 iso-ir-144 iso_8859-5 cyrillic csisolatincyrillic))

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

(define-coding-aliases 'iso-8859-6
  '(iso_8859-6:1987 iso-ir-127 iso_8859-6 ecma-114 asmo-708 arabic
		    csisolatinarabic))

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

(define-coding-aliases 'iso-8859-7
  '(iso_8859-7:1987 iso-ir-126 iso_8859-7 elot_928 ecma-118 greek greek8
		    csisolatingreek))

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

(define-coding-aliases 'iso-8859-8
  '(iso_8859-8:1988 iso-ir-138 iso_8859-8 hebrew csisolatinhebrew))

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

(define-coding-aliases 'iso-8859-9
  '(iso_8859-9:1989 iso-ir-148 iso_8859-9 latin5 l5 csisolatin5))

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

(define-coding-aliases 'iso-8859-10
  '(iso-ir-157 l6 iso_8859-10:1992 csisolatin6 latin6))

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

(define-coding-aliases 'iso-8859-14
  '(iso-ir-199 iso_8859-14:1998 iso_8859-14 latin8 iso-celtic l8))

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

(define-coding-aliases 'iso-8859-15
  '(iso_8859-15 latin-9))

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

(define-coding-aliases 'iso-8859-16
  '(iso-ir-226 iso_8859-16:2001 iso_8859-16 latin10 l10))

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

;;;; Unicode codecs

(define-coding-alias 'text 'utf-8)

(define-decoder 'utf-8
  (lambda (ib)
    (let ((n (initial-byte->utf8-char-length (peek-byte ib))))
      (read-bytes! ib 0 n)
      (decode-utf8-char (input-buffer-bytes ib) 0))))

(define-encoder 'utf-8
  (lambda (ob char)
    (encode-utf8-char! (output-buffer-bytes ob) 0 char)))

(define-coding-alias 'utf-16
  (lambda ()
    (if (host-big-endian?) 'utf-16be 'utf-16le)))

(define-decoder 'utf-16be
  (lambda (ib)
    (read-bytes! ib 0 2)
    (let ((n
	   (initial-u16->utf16-char-length
	    (bytevector-u16be-ref (input-buffer-bytes ib) 0))))
      (if (fix:> n 2)
	  (read-bytes! ib 2 n))
      (decode-utf16be-char (input-buffer-bytes ib) 0))))

(define-decoder 'utf-16le
  (lambda (ib)
    (read-bytes! ib 0 2)
    (let ((n
	   (initial-u16->utf16-char-length
	    (bytevector-u16le-ref (input-buffer-bytes ib) 0))))
      (if (fix:> n 2)
	  (read-bytes! ib 2 n))
      (decode-utf16le-char (input-buffer-bytes ib) 0))))

(define-encoder 'utf-16be
  (lambda (ob char)
    (encode-utf16be-char! (output-buffer-bytes ob) 0 char)))

(define-encoder 'utf-16le
  (lambda (ob char)
    (encode-utf16le-char! (output-buffer-bytes ob) 0 char)))

(define-coding-alias 'utf-32
  (lambda ()
    (if (host-big-endian?) 'utf-32be 'utf-32le)))

(define-decoder 'utf-32be
  (lambda (ib)
    (read-bytes! ib 0 4)
    (decode-utf32be-char (input-buffer-bytes ib) 0)))

(define-decoder 'utf-32le
  (lambda (ib)
    (read-bytes! ib 0 4)
    (decode-utf32le-char (input-buffer-bytes ib) 0)))

(define-encoder 'utf-32be
  (lambda (ob char)
    (encode-utf32be-char! (output-buffer-bytes ob) 0 char)))

(define-encoder 'utf-32le
  (lambda (ob char)
    (encode-utf32le-char! (output-buffer-bytes ob) 0 char)))

;;;; Normalizers

(define-normalizer 'newline
  (lambda (ib)
    (decode-char ib)))

(define-denormalizer 'newline
  (lambda (ob char)
    (encode-char ob char)))

(define-normalizer 'cr
  (lambda (ib)
    (let ((c0 (decode-char ib)))
      (if (eq? c0 #\u+000D)
	  #\newline
	  c0))))

(define-denormalizer 'cr
  (lambda (ob char)
    (encode-char ob (if (char=? char #\newline) #\u+000D char))))

(define-normalizer 'crlf
  (lambda (ib)
    (let ((c0 (decode-char ib)))
      (case c0
	((#\u+000D)
	 (let ((c1 (decode-char ib)))
	   (case c1
	     ((#\u+000A)
	      #\newline)
	     ((#f)
	      (unread-decoded-char ib c0)
	      #f)
	     (else
	      (unread-decoded-char ib c1)
	      c0))))
	(else c0)))))

(define-denormalizer 'crlf
  (lambda (ob char)
    (if (char=? char #\newline)
	(let ((n1 (encode-char ob #\u+000D)))
	  (if (eq? n1 1)
	      (let ((n2 (encode-char ob #\u+000A)))
		(if (not (eq? n2 1))
		    (error:char-encoding ob char))
		2)
	      n1))
	(encode-char ob char))))

(define-normalizer 'xml-1.0
  (lambda (ib)
    (let ((c0 (decode-char ib)))
      (case c0
	((#\u+000D)
	 (let ((c1 (decode-char ib)))
	   (case c1
	     ((#\u+000A)
	      #\newline)
	     ((#f)
	      (unread-decoded-char ib c0)
	      #f)
	     (else
	      (unread-decoded-char ib c1)
	      #\newline))))
	(else c0)))))

(define-denormalizer 'xml-1.0
  (lambda (ob char)
    (encode-char ob char)))

(define-normalizer 'xml-1.1
  (lambda (ib)
    (let ((c0 (decode-char ib)))
      (case c0
	((#\u+000D)
	 (let ((c1 (decode-char ib)))
	   (case c1
	     ((#\u+000A #\u+0085)
	      #\newline)
	     ((#f)
	      (unread-decoded-char ib c0)
	      #f)
	     (else
	      (unread-decoded-char ib c1)
	      #\newline))))
	((#\u+0085 #\u+2028) #\newline)
	(else c0)))))

(define-denormalizer 'xml-1.1
  (lambda (ob char)
    (encode-char ob char)))

(define-line-ending-alias 'text 'xml-1.0)
(define-line-ending-alias 'lf 'newline)
(define-line-ending-alias 'binary 'newline)
(define-line-ending-alias 'http 'xml-1.0)

;;;; Conditions

(define (error:char-decoding ib)
  (%error:char-decoding (input-buffer-port ib)))

(define (error:char-encoding ob cp)
  (%error:char-encoding (output-buffer-port ob) (integer->char cp)))

(define condition-type:char-decoding-error)
(define condition-type:char-encoding-error)
(define %error:char-decoding)
(define %error:char-encoding)
(add-boot-init!
 (lambda ()
   (set! condition-type:char-decoding-error
	 (make-condition-type 'char-decoding-error condition-type:port-error '()
	   (lambda (condition port)
	     (write-string "The input port " port)
	     (write (access-condition condition 'port) port)
	     (write-string " was unable to decode a character." port)
	     (newline port))))
   (set! %error:char-decoding
	 (condition-signaller condition-type:char-decoding-error
			      '(port)
			      standard-error-handler))
   (set! condition-type:char-encoding-error
	 (make-condition-type 'char-encoding-error condition-type:port-error
	     '(char)
	   (lambda (condition port)
	     (write-string "The output port " port)
	     (write (access-condition condition 'port) port)
	     (write-string " was unable to encode the character " port)
	     (write (access-condition condition 'char) port)
	     (newline port))))
   (set! %error:char-encoding
	 (condition-signaller condition-type:char-encoding-error
			      '(port char)
			      standard-error-handler))
   unspecific))