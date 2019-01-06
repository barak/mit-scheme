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

;;;; Compiled Code Information: Utilities
;;; package: (runtime compiler-info)

(declare (usual-integrations))

(define (compiled-code-block/dbg-info block demand-load?)
  (let ((wrapper (compiled-code-block/debugging-wrapper block)))
    (and wrapper
	 (or (debugging-wrapper/info wrapper)
	     (and demand-load?
		  (read-debugging-info wrapper))))))

(define (read-debugging-info wrapper)
  (let ((pathname (debugging-wrapper/pathname wrapper)))
    (and pathname
	 (let ((file-wrapper (read-binf-file pathname)))
	   (and file-wrapper
		(let ((file-wrapper (canonicalize-file-wrapper file-wrapper)))
		  (and file-wrapper
		       (let ((info (get-wrapped-dbg-info file-wrapper wrapper)))
			 (if info
			     (memoize-debugging-info! wrapper info))
			 info))))))))

(define (read-binf-file pathname)
  (let ((pathname (canonicalize-debug-info-pathname pathname)))
    (if (file-exists? pathname)
	(fasload-loader (->namestring pathname))
	(find-alternate-file-type pathname
				  `(("inf" . ,fasload-loader)
				    ("bif" . ,fasload-loader)
				    ("bci" . ,compressed-loader))))))

(define (find-alternate-file-type base-pathname alist)
  (let loop ((left alist) (time 0) (file #f) (receiver (lambda (x) x)))
    (if (null? left)
	(receiver file)
	(let ((file* (pathname-new-type base-pathname (caar left)))
	      (receiver* (cdar left)))
	  (if (not (file-exists? file*))
	      (loop (cdr left) time file receiver)
	      (let ((time* (file-modification-time-direct file*)))
		(if (> time* time)
		    (loop (cdr left) time* file* receiver*)
		    (loop (cdr left) time file receiver))))))))

(define (fasload-loader filename)
  (call-with-current-continuation
    (lambda (if-fail)
      (bind-condition-handler (list condition-type:fasload-error
				    condition-type:file-error
				    condition-type:bad-range-argument)
	  (lambda (condition) condition (if-fail #f))
	(lambda () (fasload filename #t))))))

(define (memoize-debugging-info! wrapper info)
  (without-interruption
   (lambda ()
     (set-debugging-wrapper/info! wrapper info)
     (add-to-population! wrappers-with-memoized-debugging-info wrapper))))

(define (discard-debugging-info!)
  (for-each-inhabitant wrappers-with-memoized-debugging-info
		       (lambda (wrapper)
			 (set-debugging-wrapper/info! wrapper #f)))
  (empty-population! wrappers-with-memoized-debugging-info))

(define-deferred wrappers-with-memoized-debugging-info
  (make-serial-population))

(add-boot-init!
 (lambda ()
   (add-secondary-gc-daemon! discard-debugging-info!)))

(define (compiled-entry/dbg-object entry #!optional demand-load?)
  (let ((block (compiled-entry/block entry))
	(offset (compiled-entry/offset entry)))
    (let ((dbg-info
	   (compiled-code-block/dbg-info block
					 (if (default-object? demand-load?)
					     #t
					     demand-load?))))
      (and dbg-info
	   (let ((find-procedure
		  (lambda ()
		    (vector-binary-search (dbg-info/procedures dbg-info)
					  <
					  dbg-procedure/label-offset
					  offset))))
	     (discriminate-compiled-entry entry
	       find-procedure
	       (lambda ()
		 (or (vector-binary-search (dbg-info/continuations dbg-info)
					   <
					   dbg-continuation/label-offset
					   offset)
		     (find-procedure)))
	       (lambda ()
		 (let ((expression (dbg-info/expression dbg-info)))
		   (if (= offset (dbg-expression/label-offset expression))
		       expression
		       (find-procedure))))
	       (lambda ()
		 (find-procedure))))))))

(define (compiled-entry/block entry)
  (cond ((compiled-code-block? entry)
	 entry)
	((compiled-closure? entry)
	 (compiled-entry/block (compiled-closure->entry entry)))
	(else
	 (compiled-code-address->block entry))))

(define (compiled-entry/offset entry)
  (if (compiled-closure? entry)
      (compiled-entry/offset (compiled-closure->entry entry))
      (compiled-code-address->offset entry)))

(define (compiled-entry/filename-and-index entry)
  (compiled-code-block/filename-and-index (compiled-entry/block entry)))

(define (compiled-code-block/filename-and-index block)
  (let ((wrapper (compiled-code-block/debugging-wrapper block)))
    (if wrapper
	(let ((pathname (debugging-wrapper/pathname wrapper)))
	  (if pathname
	      (values (canonicalize-debug-info-filename pathname)
		      (debugging-wrapper/index wrapper)
		      (debugging-wrapper/library-name wrapper))
	      (values #f #f #f)))
	(values #f #f #f))))

(define (dbg-labels/find-offset labels offset)
  (vector-binary-search labels < dbg-label/offset offset))

(define (fasload/update-debugging-info! value com-pathname)
  (cond ((compiled-code-address? value)
	 (fasload-update-internal (compiled-code-address->block value)
				  (let ((blocks
					 (load/purification-root value)))
				    (and (vector? blocks)
					 blocks))
				  0
				  com-pathname))
	((and (scode-comment? value)
	      (dbg-info-vector? (scode-comment-text value)))
	 (let ((blocks
		(dbg-info-vector/blocks-vector (scode-comment-text value))))
	   (fasload-update-internal (vector-ref blocks 0)
				    blocks
				    1
				    com-pathname)))))

(define (fasload-update-internal block blocks start com-pathname)
  (let ((wrapper (compiled-code-block/debugging-wrapper block)))
    (if wrapper
	(let ((pathname (debugging-wrapper/pathname wrapper)))
	  (if pathname
	      (let ((pathname*
		     (fasload-compute-pathname pathname com-pathname)))
		(set-debugging-wrapper/pathname! wrapper pathname*)
		(if blocks
		    (fasload-update-sub-blocks blocks start
					       pathname pathname*))))))))

(define (fasload-compute-pathname pathname com-pathname)
  (rewrite-directory
   (let ((pathname (merge-pathnames pathname))
	 (com-pathname (merge-pathnames com-pathname)))
     (if (and (equal? (pathname-name pathname)
		      (pathname-name com-pathname))
	      (not (equal? (pathname-type pathname)
			   (pathname-type com-pathname)))
	      (equal? (pathname-version pathname)
		      (pathname-version com-pathname)))
	 (pathname-new-type com-pathname (pathname-type pathname))
	 pathname))))

(define (fasload-update-sub-blocks blocks start pathname pathname*)
  (let ((n (vector-length blocks)))
    (do ((i start (fix:+ i 1)))
	((fix:= i n))
      (let ((wrapper
	     (compiled-code-block/debugging-wrapper (vector-ref blocks i))))
	(if (and wrapper
		 (debug-info-pathname? (debugging-wrapper/pathname wrapper))
		 (pathname=? (debugging-wrapper/pathname wrapper) pathname))
	    (set-debugging-wrapper/pathname! wrapper pathname*))))))

(define-deferred directory-rewriting-rules
  (make-settable-parameter '()))

(define (with-directory-rewriting-rule match replace thunk)
  (parameterize ((directory-rewriting-rules
		  (cons (cons (pathname-as-directory (merge-pathnames match))
			      replace)
			(directory-rewriting-rules))))
    (thunk)))

(define (add-directory-rewriting-rule! match replace)
  (let ((match (pathname-as-directory (merge-pathnames match))))
    (let ((rule
	   (find (lambda (rule)
		   (equal? (pathname-directory (car rule))
			   (pathname-directory match)))
		 (directory-rewriting-rules))))
      (if rule
	  (set-cdr! rule replace)
	  (directory-rewriting-rules
	   (cons (cons match replace)
		 (directory-rewriting-rules))))))
  unspecific)

(define (rewrite-directory pathname)
  (let ((rule
	 (find (lambda (rule)
		 (directory-prefix? (pathname-directory pathname)
				    (pathname-directory (car rule))))
	       (directory-rewriting-rules))))
    (->namestring
     (if rule
	 (merge-pathnames
	  (pathname-new-directory
	   (file-pathname pathname)
	   (cons 'relative
		 (list-tail (pathname-directory pathname)
			    (length (pathname-directory (car rule))))))
	  (cdr rule))
	 pathname))))

(define (directory-prefix? x y)
  (and (pair? x)
       (pair? y)
       (eq? (car x) (car y))
       (let loop ((x (cdr x)) (y (cdr y)))
	 (or (null? y)
	     (and (not (null? x))
		  (equal? (car x) (car y))
		  (loop (cdr x) (cdr y)))))))

(define (canonicalize-debug-info-filename filename)
  (->namestring (canonicalize-debug-info-pathname filename)))

(define (canonicalize-debug-info-pathname pathname)
  (merge-pathnames
   pathname
   (let ((value (get-environment-variable "MITSCHEME_INF_DIRECTORY")))
     (if value
	 (pathname-as-directory value)
	 (or (%find-library-directory pathname)
	     (system-library-directory-pathname))))))

(define (%find-library-directory pathname)
  (let ((dir (pathname-directory pathname)))
    (or (and (pair? dir)
	     (eq? 'relative (car dir))
	     (pair? (cdr dir))
	     (string? (cadr dir))
	     (let ((libdir (system-library-directory-pathname (cadr dir))))
	       (and libdir
		    (pathname-new-directory libdir
					    (except-last-pair
					     (pathname-directory libdir)))))))))

(define-integrable (dbg-block/layout-first-offset block)
  (let ((layout (dbg-block/layout block)))
    (and (pair? layout) (car layout))))

(define-integrable (dbg-block/layout-vector block)
  (let ((layout (dbg-block/layout block)))
    (if (pair? layout)
	(cdr layout)
	layout)))

(define (dbg-block/dynamic-link-index block)
  (vector-find-next-element (dbg-block/layout-vector block)
			    dbg-block-name/dynamic-link))

(define (dbg-block/ic-parent-index block)
  (vector-find-next-element (dbg-block/layout-vector block)
			    dbg-block-name/ic-parent))

(define (dbg-block/normal-closure-index block)
  (vector-find-next-element (dbg-block/layout-vector block)
			    dbg-block-name/normal-closure))

(define (dbg-block/return-address-index block)
  (vector-find-next-element (dbg-block/layout-vector block)
			    dbg-block-name/return-address))

(define (dbg-block/static-link-index block)
  (vector-find-next-element (dbg-block/layout-vector block)
			    dbg-block-name/static-link))

(define (dbg-block/find-name block name)
  (let ((layout (dbg-block/layout-vector block)))
    (let ((end (vector-length layout)))
      (let loop ((index 0))
	(and (< index end)
	     (if (let ((item (vector-ref layout index)))
		   (and (dbg-variable? item)
			(eq? name (dbg-variable/name item))))
		 index
		 (loop (1+ index))))))))

(define (compiled-procedure/name entry)
  (let ((procedure
	 (compiled-entry/dbg-object entry load-debugging-info-on-demand?)))
    (and procedure
	 (let ((name (dbg-procedure/name procedure)))
	   (or (special-form-procedure-name? name)
	       (symbol->string name))))))

(define load-debugging-info-on-demand?
  #f)

(define (special-form-procedure-name? name)
  (let ((association (assq name special-form-procedure-names)))
    (and association
	 (symbol->string (cdr association)))))

(define-deferred special-form-procedure-names
  `((,scode-lambda-name:unnamed . lambda)
    (,scode-lambda-name:internal-lambda . lambda)
    (,scode-lambda-name:let . let)
    (,scode-lambda-name:fluid-let . fluid-let)))

(define (compiled-procedure/lambda entry)
  (let ((procedure (compiled-entry/dbg-object entry)))
    (and procedure
	 (dbg-procedure/source-code procedure))))

(define (compiled-expression/scode entry)
  (let ((object (compiled-entry/dbg-object entry)))
    (or (and (dbg-procedure? object)
	     (let ((scode (dbg-procedure/source-code object)))
	       (and scode
		    (scode-lambda-body scode))))
	entry)))

;;;; UNCOMPRESS
;;;  A simple extractor for compressed binary info files.

(define (compressed-loader compressed-file)
  (call-with-temporary-file-pathname
   (lambda (temporary-file)
     (call-with-current-continuation
      (lambda (k)
	(uncompress-internal compressed-file
			     temporary-file
			     (lambda (message . irritants)
			       message irritants
			       (k #f)))
	(fasload-loader temporary-file))))))

(define (uncompress-internal ifile ofile if-fail)
  (call-with-binary-input-file (merge-pathnames ifile)
    (lambda (input)
	;; This may get more hairy as we up versions
      (if (read-compressed-file-marker input)
	  (call-with-binary-output-file (merge-pathnames ofile)
	    (lambda (output)
	      (uncompress-ports input output
				(fix:* (file-length ifile) 2))))
	  (if-fail "Not a recognized compressed file:" ifile)))))

(define (read-compressed-file-marker input)
  (let ((n (bytevector-length compressed-file-marker)))
    (let ((marker (read-bytevector n input)))
      (and marker
	   (not (eof-object? marker))
	   (bytevector=? marker compressed-file-marker)))))

(define (write-compressed-file-marker output)
  (write-bytevector compressed-file-marker output))

(define-deferred compressed-file-marker
  (string->utf8 "Compressed-B1-1.00"))

;; This version will uncompress any input that can be read in chunks by
;; calling SOURCE on a bytevector range.
;;
;; This version is written for speed:
;;
;;  . The main speed gain is from buffering the input.  This version
;;    is about 10 times faster than the above version on files, and about
;;    1.5 times faster than the above version called on custom input
;;    operations.
;;
;;  . PARSE-COMMAND interprets one `command' of compressed information.
;;
;;  . There is no assignment to local variables.  Instead the changeable
;;    state is passed as explicit state variables (a kind of functional
;;    style) and the procedures are tail-recursive so that the state
;;    is `single-threaded'.  This prevents the compiler from
;;    cellifying the variables.
;;
;;  . Some of the drudgery of passing all of the state is handed over
;;    to the compiler by making the procedures internal to PARSE-COMMAND.
;;
;;  . The main loop (PARSE-COMMAND) is `restartable'.  This allows the
;;    parsing operation to determine if enough input or output buffer is
;;    available before doing any copying, and if there is a problem it
;;    can tail-call into the handler (RETRY-WITH-BIGGER-OUTPUT-BUFFER
;;    and REFILL-INPUT-BUFFER-AND-RETRY) and that can tail call back
;;    into PARSE-COMMAND.
;;
;;  . Refilling the input buffer and testing for EOF is a bit funky.
;;    It relies on the fact that when we demand a refill we know how many
;;    bytes we require to (re)parse the command.  We are at EOF when
;;    we try to read some more data and there is none, and also there
;;    is no unprocessed input, in which case we just tail out of the
;;    loop.

(define (uncompress-ports input-port output-port #!optional buffer-size)
  (uncompress-kernel-by-blocks
   (lambda (bytes start end)
     (let ((n (read-bytevector! bytes input-port start end)))
       (if (or (not n) (eof-object? n))
	   0
	   n)))
   (lambda (bytes start end)
     (write-bytevector bytes output-port start end))
   (if (default-object? buffer-size) 4096 buffer-size)))

(define (uncompress-kernel-by-blocks source sink output-size)
  (define-integrable window-size #x1000)
  (define-integrable input-size #x1000)

  (let ((cp-table (make-vector window-size))
	(input-buffer (make-bytevector input-size)))

    (define-integrable (cp:++ cp)
      (fix:and (fix:+ cp 1)
	       (fix:- window-size 1)))

    (define-integrable (cp:- cp n)
      (fix:and (fix:+ (fix:- cp n) window-size)
	       (fix:- window-size 1)))

    (define (short-copy! to at from start end)
      (do ((i start (fix:+ i 1))
	   (j at (fix:+ j 1)))
	  ((not (fix:< i end)) j)
	(bytevector-u8-set! to j (bytevector-u8-ref from i))))

    (let parse-command
	((ip 0)
	 (ip-end 0)
	 (cp 0)
	 (output-buffer (make-bytevector output-size))
	 (op 0)
	 (output-size output-size))

      ;; Invariant:
      ;; (BYTEVECTOR-COPY OUTPUT-BUFFER IP IP-END) is unprocessed input.
      (define (retry-with-bigger-output-buffer)
	(let* ((new-size (fix:+ output-size (fix:lsh output-size -2)))
	       (output-buffer* (make-bytevector new-size)))
	  (bytevector-copy! output-buffer* 0 output-buffer)
	  (parse-command ip ip-end cp
			 output-buffer* op new-size)))

      (define (refill-input-buffer-and-retry needed)
	(let* ((ip-end* (short-copy! input-buffer 0 input-buffer ip ip-end))
	       (n (source input-buffer ip-end* input-size)))
	  (if (fix:> n 0)
	      (parse-command 0 (fix:+ ip-end* n) cp
			     output-buffer op output-size)
	      (begin
		(if (fix:< ip-end* needed)
		    (error "Compressed input ends too soon"
			   'uncompress-kernel-by-blocks))
		(sink output-buffer 0 op)
		op))))

      ;; Copy BYTE+1 bytes from input to output, and update CP-TABLE with their
      ;; indices in the output buffer.
      (define (literal-command byte)
	(let* ((ip (fix:+ ip 1))
	       (ip* (fix:+ ip (fix:+ byte 1)))
	       (op* (fix:+ op (fix:+ byte 1))))
	  (if (fix:<= op* output-size)
	      (if (fix:<= ip* ip-end)
		  (begin
		    (short-copy! output-buffer op input-buffer ip ip*)
		    (do ((op op (fix:+ op 1))
			 (cp cp (cp:++ cp)))
			((not (fix:< op op*))
			 (parse-command ip* ip-end cp
					output-buffer op* output-size))
		      (vector-set! cp-table cp op)))
		  (refill-input-buffer-and-retry (fix:+ byte 2)))
	      (retry-with-bigger-output-buffer))))

      ;; Copy bytes from earlier in the output to here.  Start and length are
      ;; encoded in BYTE and the next input byte.
      (define (copy-command byte)
	(let ((ip (fix:+ ip 1)))
	  (if (fix:< ip ip-end)
	      (let ((ostart
		     (vector-ref cp-table
				 (cp:- cp
				       (fix:or (fix:lsh (fix:and byte #x0F) 8)
					       (bytevector-u8-ref input-buffer
								  ip)))))
		    (length (fix:+ (fix:lsh byte -4) 1)))
		(let ((op* (fix:+ op length)))
		  (if (fix:<= op* output-size)
		      (begin
			(short-copy! output-buffer op
				     output-buffer ostart (fix:+ ostart length))
			(vector-set! cp-table cp op)
			(parse-command (fix:+ ip 1) ip-end (cp:++ cp)
				       output-buffer op* output-size))
		      (retry-with-bigger-output-buffer))))
	      (refill-input-buffer-and-retry 2))))

      (if (fix:< ip ip-end)
	  (let ((byte (bytevector-u8-ref input-buffer ip)))
	    (if (fix:< byte #x10)
		(literal-command byte)
		(copy-command byte)))
	  (refill-input-buffer-and-retry 0)))))