#| -*-Scheme-*-

$Id: infutl.scm,v 1.77 2008/08/24 23:31:07 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
(declare (integrate-external "infstr" "char"))

(define (initialize-package!)
  (set! special-form-procedure-names
	`((,lambda-tag:unnamed . LAMBDA)
	  (,lambda-tag:internal-lambda . LAMBDA)
	  (,lambda-tag:internal-lexpr . LAMBDA)
	  (,lambda-tag:let . LET)
	  (,lambda-tag:fluid-let . FLUID-LET)))
  (set! wrappers-with-memoized-debugging-info (make-population))
  (add-secondary-gc-daemon! discard-debugging-info!)
  (initialize-uncompressed-files!)
  (add-event-receiver! event:after-restore initialize-uncompressed-files!)
  (add-event-receiver! event:before-exit delete-uncompressed-files!)
  (add-gc-daemon! clean-uncompressed-files!))

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
		       (let ((info
			      (get-wrapped-dbg-info file-wrapper wrapper)))
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
				    ("bci" . ,(compressed-loader "bif")))))))

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

(define (memoize-debugging-info! wrapper info)
  (without-interrupts
   (lambda ()
     (set-debugging-wrapper/info! wrapper info)
     (add-to-population! wrappers-with-memoized-debugging-info wrapper))))

(define (discard-debugging-info!)
  (without-interrupts
   (lambda ()
     (map-over-population! wrappers-with-memoized-debugging-info
       (lambda (wrapper)
	 (set-debugging-wrapper/info! wrapper #f)))
     (set! wrappers-with-memoized-debugging-info (make-population))
     unspecific)))

(define wrappers-with-memoized-debugging-info)

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
		      (debugging-wrapper/index wrapper))
	      (values #f #f)))
	(values #f #f))))

(define (dbg-labels/find-offset labels offset)
  (vector-binary-search labels < dbg-label/offset offset))

(define (dbg-info-vector/blocks-vector info)
  (let ((items (dbg-info-vector/items info)))
    (cond ((vector? items) items)
	  ((and (pair? items)
		(pair? (cdr items))
		(vector? (cadr items)))
	   (cadr items))
	  (else (error "Illegal dbg-info-vector" info)))))

(define (dbg-info-vector/purification-root info)
  (let ((items (dbg-info-vector/items info)))
    (cond ((vector? items) #f)
	  ((and (pair? items)
		(eq? (car items) 'COMPILED-BY-PROCEDURES)
		(pair? (cdr items))
		(pair? (cddr items)))
	   (caddr items))
	  (else (error "Illegal dbg-info-vector" info)))))

(define (fasload/update-debugging-info! value com-pathname)
  (cond ((compiled-code-address? value)
	 (fasload-update-internal (compiled-code-address->block value)
				  (let ((blocks
					 (load/purification-root value)))
				    (and (vector? blocks)
					 blocks))
				  0
				  com-pathname))
	((and (comment? value)
	      (dbg-info-vector? (comment-text value)))
	 (let ((blocks (dbg-info-vector/blocks-vector (comment-text value))))
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

(define directory-rewriting-rules
  '())

(define (with-directory-rewriting-rule match replace thunk)
  (fluid-let ((directory-rewriting-rules
	       (cons (cons (pathname-as-directory (merge-pathnames match))
			   replace)
		     directory-rewriting-rules)))
    (thunk)))

(define (add-directory-rewriting-rule! match replace)
  (let ((match (pathname-as-directory (merge-pathnames match))))
    (let ((rule
	   (list-search-positive directory-rewriting-rules
	     (lambda (rule)
	       (equal? (pathname-directory (car rule))
		       (pathname-directory match))))))
      (if rule
	  (set-cdr! rule replace)
	  (set! directory-rewriting-rules
		(cons (cons match replace)
		      directory-rewriting-rules)))))
  unspecific)

(define (rewrite-directory pathname)
  (let ((rule
	 (list-search-positive directory-rewriting-rules
	   (lambda (rule)
	     (directory-prefix? (pathname-directory pathname)
				(pathname-directory (car rule)))))))
    (->namestring
     (if rule
	 (merge-pathnames
	  (pathname-new-directory
	   (file-pathname pathname)
	   (cons 'RELATIVE
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
	 (system-library-directory-pathname)))))

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
	       (symbol-name name))))))

(define load-debugging-info-on-demand?
  #f)

(define (special-form-procedure-name? name)
  (let ((association (assq name special-form-procedure-names)))
    (and association
	 (symbol-name (cdr association)))))

(define special-form-procedure-names)

(define (compiled-procedure/lambda entry)
  (let ((procedure (compiled-entry/dbg-object entry)))
    (and procedure
	 (dbg-procedure/source-code procedure))))

(define (compiled-expression/scode entry)
  (let ((object (compiled-entry/dbg-object entry)))
    (or (and (dbg-procedure? object)
	     (let ((scode (dbg-procedure/source-code object)))
	       (and scode
		    (lambda-body scode))))
	entry)))

;;; Support of BSM files

(define (read-labels descriptor)
  (cond ((debug-info-pathname? descriptor)
	 (let ((bsm (read-bsm-file descriptor)))
	   (and bsm ;; bsm are either vectors of pairs or vectors of vectors
		(if (vector? bsm)
		    (let ((first (and (not (zero? (vector-length bsm)))
				      (vector-ref bsm 0))))
		      (cond ((pair? first) bsm)
			    ((vector? first) first)
			    (else #f)))))))
	((and (pair? descriptor)
	      (debug-info-pathname? (car descriptor))
	      (exact-nonnegative-integer? (cdr descriptor)))
	 (let ((bsm (read-bsm-file (car descriptor))))
	   (and bsm
		(vector? bsm)
		(< (cdr descriptor) (vector-length bsm))
		(vector-ref bsm (cdr descriptor)))))
	(else #f)))

(define (read-bsm-file name)
  (let ((pathname
	 (let ((pathname
		(canonicalize-debug-info-pathname
		 (rewrite-directory (merge-pathnames name)))))
	   (if (file-exists? pathname)
	       pathname
	       (let loop ((types '("bsm" "bcs")))
		 (and (not (null? types))
		      (let ((pathname
			     (pathname-new-type pathname (car types))))
			(if (file-exists? pathname)
			    pathname
			    (loop (cdr types))))))))))
    (and pathname
	 (if (equal? "bcs" (pathname-type pathname))
	     ((compressed-loader "bsm") pathname)
	     (fasload-loader pathname)))))

;;;; Splitting of info structures

(define (inf->bif/bsm inffile)
  (let* ((infpath (merge-pathnames inffile))
	 (bifpath (pathname-new-type infpath "bif"))
	 (bsmpath (pathname-new-type infpath "bsm")))
    (let ((file-info (fasload infpath)))
      (inf-structure->bif/bsm file-info bifpath bsmpath))))

(define (inf-structure->bif/bsm file-info bifpath bsmpath)
  (let ((bifpath (merge-pathnames bifpath))
	(bsmpath (and bsmpath (merge-pathnames bsmpath))))
    (call-with-values (lambda () (split-inf-structure! file-info bsmpath))
      (lambda (file-wrapper bsm)
	(fasdump file-wrapper bifpath #t)
	(if bsmpath (fasdump bsm bsmpath #t))))))

(define (split-inf-structure! file-info bsmpath)
  (let ((file-wrapper (canonicalize-file-wrapper file-info))
	(bsmname (and bsmpath (->namestring bsmpath))))
    (if (not file-wrapper)
	(error "Unknown debugging-file format:" file-info))
    (let ((info (debugging-file-wrapper/info file-wrapper)))
      (let ((n (vector-length info)))
	(let ((bsm (make-vector n)))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i n))
	    (let ((dbg-info (vector-ref info i)))
	      (let ((labels (dbg-info/labels/desc dbg-info)))
		(vector-set! bsm i labels)
		(set-dbg-info/labels/desc! dbg-info
					   (and bsmname (cons bsmname i))))))
	  (values file-wrapper bsm))))))

;;;; UNCOMPRESS
;;;  A simple extractor for compressed binary info files.

(define-integrable window-size 4096)

(define (uncompress-ports input-port output-port #!optional buffer-size)
  (uncompress-kernel-by-blocks
   input-port output-port
   (if (default-object? buffer-size) 4096 buffer-size)
   input-port/read-substring!))

(define (uncompress-read-substring port buffer start end)
  (let loop ((i start))
    (if (fix:>= i end)
	(fix:- i start)
	(let ((char (read-char port)))
	  (if (not (char? char))
	      (fix:- i start)
	      (begin
		(string-set! buffer i char)
		(loop (fix:1+ i))))))))

;;  General version.
;;
;; . This version will uncompress any input that can be read a character at
;;   a time by applying parameter READ-CHAR to INPUT-PORT.  These do not
;;   necesarily have to be a port and a port operation, but that is
;;   the expected use.
;; . The EOF indicator returned by READ-CHAR must not be a character, which
;;   implies that EOF-OBJECT? and CHAR? are disjoint.

#|
(define (uncompress-kernel-by-chars input-port output-port buffer-size
				    read-char)
  (let ((buffer (make-string buffer-size))
	(cp-table (make-vector window-size)))

    (define (displacement->cp-index displacement cp)
      (let ((index (fix:- cp displacement)))
	(if (fix:< index 0) (fix:+ window-size index) index)))

    (define-integrable (cp:+ cp n)
      (fix:remainder (fix:+ cp n) window-size))

    (define-integrable (read-substring! start end)
      (let loop ((i start))
	(if (fix:>= i end)
	    (fix:- i start)
	    (begin
	      (string-set! buffer i (read-char input-port))
	      (loop (fix:1+ i))))))

    (define (grow-buffer!)
      (let* ((new-size (fix:+ buffer-size (fix:quotient buffer-size 4)))
	     (nbuffer (make-string new-size)))
	(substring-move! buffer 0 buffer-size nbuffer 0)
	(set! buffer-size new-size)
	(set! buffer nbuffer)
	unspecific))

    (define-integrable (guarantee-buffer nbp)
      (if (fix:> nbp buffer-size)
	  (grow-buffer!)))

    (let loop ((bp 0) (cp 0))
      (let ((char (read-char input-port)))
	(if (not (char? char))		; Assume EOF
	    (begin
	      (output-port/write-substring output-port buffer 0 bp)
	      bp)
	    (let ((byte (char->integer char)))
	      (if (fix:< byte 16)
		  (let ((length (fix:+ byte 1)))
		    (let ((nbp (fix:+ bp length))
			  (ncp (cp:+ cp length)))
		      (guarantee-buffer nbp)
		      (read-substring! bp nbp)
		      (do ((bp bp (fix:+ bp 1)) (cp cp (cp:+ cp 1)))
			  ((fix:= bp nbp))
			(vector-set! cp-table cp bp))
		      (loop nbp ncp)))
		  (let ((cpi (displacement->cp-index
			      (fix:+ (fix:* (fix:remainder byte 16) 256)
				     (char->integer (read-char input-port)))
			      cp))
			(length (fix:+ (fix:quotient byte 16) 1)))
		    (let ((bp* (vector-ref cp-table cpi))
			  (nbp (fix:+ bp length))
			  (ncp (cp:+ cp 1)))
		      (guarantee-buffer nbp)
		      (let ((end-bp* (fix:+ bp* length)))
			(do ((bp* bp* (fix:+ bp* 1))
			     (bp bp (fix:+ bp 1)))
			    ((not (fix:< bp* end-bp*)))
			  (vector-8b-set! buffer bp
					  (vector-8b-ref buffer bp*))))
		      (vector-set! cp-table cp bp)
		      (loop nbp ncp))))))))))
|#

;; This version will uncompress any input that can be read in chunks by
;; applying parameter READ-SUBSTRING to INPUT-PORT and a substring
;; reference.  These do not necesarily have to be a port and a port
;; operation, but that is the expected use.
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

(define (uncompress-kernel-by-blocks input-port output-port buffer-size
				     read-substring)
  (define-integrable input-size 4096)
  (let ((cp-table (make-vector window-size))
	(input-buffer (make-string input-size)))

    (define (displacement->cp-index displacement cp)
      (let ((index (fix:- cp displacement)))
	(if (fix:< index 0) (fix:+ window-size index) index)))

    (define-integrable (cp:+ cp n)
      (fix:remainder (fix:+ cp n) window-size))

    (define (short-substring-move! s1 start1 end1 s2 start2)
      (do ((i1 start1 (fix:+ i1 1))
    	   (i2 start2 (fix:+ i2 1)))
    	  ((fix:= i1 end1))
    	(string-set! s2 i2 (string-ref s1 i1))))

    (let parse-command ((bp 0) (cp 0) (ip 0) (ip-end 0)
			       (buffer (make-string buffer-size))
			       (buffer-size buffer-size))
      ;; Invariant: (SUBTRING BUFFER IP IP-END) is unprocessed input.
      (define (retry-with-bigger-output-buffer)
	(let* ((new-size (fix:+ buffer-size (fix:quotient buffer-size 4)))
	       (nbuffer (make-string new-size)))
	  (substring-move! buffer 0 buffer-size nbuffer 0)
	  (parse-command bp cp ip ip-end nbuffer new-size)))

      (define (refill-input-buffer-and-retry needed)
	(short-substring-move! input-buffer ip ip-end input-buffer 0)
	(let* ((left (fix:- ip-end ip))
	       (count (read-substring input-port input-buffer 
				      left input-size))
	       (total (fix:+ count left)))
	  (if (fix:= count 0)
	      (if (fix:< total needed)
		  (error "Compressed input ends too soon"
			 input-port 'UNCOMPRESS-KERNEL-BY-BLOCKS)
		  (finished))
	      (parse-command bp cp 0  total buffer buffer-size))))

      (define (finished)
	(output-port/write-substring output-port buffer 0 bp)
	bp)
  
      (define (literal-command byte)
	(let ((length (fix:+ byte 1))
	      (ip*    (fix:+ ip 1)))
	  (let ((nbp (fix:+ bp length))
		(ncp (cp:+ cp length))
		(nip (fix:+ ip* length)))
	    (if (fix:> nbp buffer-size)
		(retry-with-bigger-output-buffer)
		(if (fix:> nip ip-end)
		    (refill-input-buffer-and-retry (fix:+ length 1))
		    (begin
		      (short-substring-move! input-buffer ip* nip buffer bp)
		      (do ((bp bp (fix:+ bp 1)) (cp cp (cp:+ cp 1)))
			  ((fix:= bp nbp))
			(vector-set! cp-table cp bp))
		      (parse-command nbp ncp nip ip-end buffer
				     buffer-size)))))))

      (define (copy-command byte)
	(let ((ip* (fix:+ ip 1)))
	  (if (fix:>= ip* ip-end)
	      (refill-input-buffer-and-retry 2)
	      (let ((cpi (displacement->cp-index
			  (fix:+ (fix:* (fix:remainder byte 16) 256)
				 (vector-8b-ref input-buffer ip*))
			  cp))
		    (length (fix:+ (fix:quotient byte 16) 1)))
		(let ((bp* (vector-ref cp-table cpi))
		      (nbp (fix:+ bp length))
		      (ncp (cp:+ cp 1)))
		  (if (fix:> nbp buffer-size)
		      (retry-with-bigger-output-buffer)
		      (let ((end-bp* (fix:+ bp* length)))
			(short-substring-move! buffer bp* end-bp* buffer bp)
			(vector-set! cp-table cp bp)
			(parse-command nbp ncp (fix:+ ip 2) ip-end
				       buffer buffer-size))))))))

      (if (fix:>= ip ip-end)
	  (refill-input-buffer-and-retry 0)
	  (let ((byte  (vector-8b-ref input-buffer ip)))
	    (if (fix:< byte 16)
		(literal-command byte)
		(copy-command byte)))))))

(define (fasload-loader filename)
  (call-with-current-continuation
    (lambda (if-fail)
      (bind-condition-handler (list condition-type:fasload-error
				    condition-type:bad-range-argument)
	  (lambda (condition) condition (if-fail #f))
        (lambda () (fasload filename #t))))))

(define (compressed-loader uncompressed-type)
  (lambda (compressed-file)
    (lookup-uncompressed-file compressed-file fasload-loader
      (lambda ()
	(let ((load-compressed
	       (lambda (temporary-file)
		 (call-with-current-continuation
		  (lambda (k)
		    (uncompress-internal compressed-file
					 temporary-file
					 (lambda (message . irritants)
					   message irritants
					   (k #f)))
		    (fasload-loader temporary-file))))))
	  (case *save-uncompressed-files?*
	    ((#F)
	     (call-with-temporary-file-pathname load-compressed))
	    ((AUTOMATIC)
	     (call-with-uncompressed-file-pathname compressed-file
						   load-compressed))
	    (else
	     (call-with-temporary-file-pathname
	      (lambda (temporary-file)
		(let ((result (load-compressed temporary-file))
		      (uncompressed-file
		       (pathname-new-type compressed-file uncompressed-type)))
		  (delete-file-no-errors uncompressed-file)
		  (if (call-with-current-continuation
		       (lambda (k)
			 (bind-condition-handler
			     (list condition-type:file-error
				   condition-type:port-error)
			     (lambda (condition) condition (k #t))
			   (lambda ()
			     (rename-file temporary-file uncompressed-file)
			     #f))))
		      (call-with-current-continuation
		       (lambda (k)
			 (bind-condition-handler
			     (list condition-type:file-error
				   condition-type:port-error)
			     (lambda (condition) condition (k unspecific))
			   (lambda ()
			     (copy-file temporary-file uncompressed-file))))))
		  result))))))))))

(define (uncompress-internal ifile ofile if-fail)
  (call-with-binary-input-file (merge-pathnames ifile)
    (lambda (input)			       
      (let* ((file-marker "Compressed-B1-1.00")
	     (marker-size (string-length file-marker))
	     (actual-marker (make-string marker-size)))
	;; This may get more hairy as we up versions
	(if (and (fix:= (uncompress-read-substring input
						   actual-marker 0 marker-size)
			marker-size)
		 (string=? file-marker actual-marker))
	    (call-with-binary-output-file (merge-pathnames ofile)
   	      (lambda (output)					  
		(uncompress-ports input output (fix:* (file-length ifile) 2))))
	    (if-fail "Not a recognized compressed file:" ifile))))))

(define (lookup-uncompressed-file compressed-file if-found if-not-found)
  (dynamic-wind
   (lambda ()
     (set-car! uncompressed-files (+ (car uncompressed-files) 1)))
   (lambda ()
     (let loop ((entries (cdr uncompressed-files)))
       (cond ((null? entries)
	      (if-not-found))
	     ((and (pathname=? (caar entries) compressed-file)
		   ;; Avoid a subtle race condition with the GC daemon.
		   (let ((time (cddar entries)))
		     (set-cdr! (cdar entries) (real-time-clock))
		     (and time
			  (or (file-exists? (cadar entries))
			      (begin
				(set-cdr! (cdar entries) #f)
				#f))
			  (cddar entries))))
	      (dynamic-wind
	       (lambda () unspecific)
	       (lambda ()
		 (or (if-found (cadar entries))
		     (begin
		       (set-cdr! (cdar entries) #f)
		       (loop (cdr entries)))))
	       (lambda ()
		 (if (cddar entries)
		     (set-cdr! (cdar entries) (real-time-clock))))))
	     (else
	      (loop (cdr entries))))))
   (lambda ()
     (set-car! uncompressed-files (- (car uncompressed-files) 1)))))

(define (call-with-uncompressed-file-pathname compressed-file receiver)
  (let ((temporary-file (temporary-file-pathname)))
    (let ((entry
	   (cons compressed-file
		 (cons temporary-file (real-time-clock)))))
      (dynamic-wind
       (lambda () unspecific)
       (lambda ()
	 (without-interrupts
	  (lambda ()
	    (set-cdr! uncompressed-files
		      (cons entry (cdr uncompressed-files)))))
	 (receiver temporary-file))
       (lambda ()
	 (set-cdr! (cdr entry) (real-time-clock)))))))

(define (delete-uncompressed-files!)
  (do ((entries (cdr uncompressed-files) (cdr entries)))
      ((null? entries) unspecific)
    (deallocate-temporary-file (cadar entries))))

(define (clean-uncompressed-files!)
  (if (= 0 (car uncompressed-files))
      (let ((time (real-time-clock)))
	(let loop
	    ((entries (cdr uncompressed-files))
	     (prev uncompressed-files))
	  (if (not (null? entries))
	      (if (or (not (cddar entries))
		      (< (- time (cddar entries))
			 *uncompressed-file-lifetime*))
		  (loop (cdr entries) entries)
		  (begin
		    (set-cdr! prev (cdr entries))
		    (set-cdr! (cdar entries) #f)
		    (deallocate-temporary-file (cadar entries))
		    (loop (cdr entries) prev))))))))

(define (initialize-uncompressed-files!)
  (set! uncompressed-files (list 0))
  unspecific)

(define *save-uncompressed-files?* 'AUTOMATIC)
(define *uncompressed-file-lifetime* 300000)
(define uncompressed-files)