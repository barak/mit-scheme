#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/infutl.scm,v 1.25 1992/05/26 17:20:40 mhwu Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Compiled Code Information: Utilities
;;; package: (runtime compiler-info)

(declare (usual-integrations))
(declare (integrate-external "infstr"))

(define (initialize-package!)
  (set! special-form-procedure-names
	`((,lambda-tag:unnamed . LAMBDA)
	  (,lambda-tag:internal-lambda . LAMBDA)
	  (,lambda-tag:internal-lexpr . LAMBDA)
	  (,lambda-tag:let . LET)
	  (,lambda-tag:fluid-let . FLUID-LET)
	  (,lambda-tag:make-environment . MAKE-ENVIRONMENT)))
  (set! blocks-with-memoized-debugging-info (make-population))
  (add-secondary-gc-daemon! discard-debugging-info!))

(define (compiled-code-block/dbg-info block demand-load?)
  (let ((old-info (compiled-code-block/debugging-info block)))
    (cond ((dbg-info? old-info)
	   old-info)
	  ((and (pair? old-info) (dbg-info? (car old-info)))
	   (car old-info))
	  (demand-load?
	   (let ((dbg-info (read-debugging-info old-info)))
	     (if dbg-info (memoize-debugging-info! block dbg-info))
	     dbg-info))
	  (else
	   false))))

(define (discard-debugging-info!)
  (without-interrupts
   (lambda ()
     (map-over-population! blocks-with-memoized-debugging-info
			   discard-block-debugging-info!)
     (set! blocks-with-memoized-debugging-info (make-population))
     unspecific)))

(define (read-debugging-info descriptor)
  (cond ((string? descriptor)
	 (let ((binf (read-binf-file descriptor)))
	   (and binf
		(if (dbg-info? binf)
		    binf
		    (and (vector? binf)
			 (not (zero? (vector-length binf)))
			 (vector-ref binf 0))))))
	((and (pair? descriptor)
	      (string? (car descriptor))
	      (exact-nonnegative-integer? (cdr descriptor)))
	 (let ((binf (read-binf-file (car descriptor))))
	   (and binf
		(vector? binf)
		(< (cdr descriptor) (vector-length binf))
		(vector-ref binf (cdr descriptor)))))
	(else
	 false)))

(define (read-binf-file filename)
  (let ((pathname (merge-pathnames filename)))
    (if (file-exists? pathname)
	(fasload-loader (->namestring pathname))
	(find-alternate-file-type pathname
				  `(("binf" . ,fasload-loader)
				    ("inf" . ,fasload-loader)
				    ("bif" . ,fasload-loader)
				    ("bci" . ,compressed-loader))))))

(define (memoize-debugging-info! block dbg-info)
  (without-interrupts
   (lambda ()
     (let ((old-info (compiled-code-block/debugging-info block)))
       (if (not (and (pair? old-info) (dbg-info? (car old-info))))
	   (begin
	     (set-compiled-code-block/debugging-info! block
						      (cons dbg-info old-info))
	     (add-to-population! blocks-with-memoized-debugging-info
				 block)))))))

(define (un-memoize-debugging-info! block)
  (without-interrupts
   (lambda ()
     (discard-block-debugging-info! block)
     (remove-from-population! blocks-with-memoized-debugging-info block))))

(define (discard-block-debugging-info! block)
  (let ((old-info (compiled-code-block/debugging-info block)))
    (if (and (pair? old-info) (dbg-info? (car old-info)))
	(set-compiled-code-block/debugging-info! block (cdr old-info)))))

(define blocks-with-memoized-debugging-info)

(define (compiled-entry/dbg-object entry #!optional demand-load?)
  (let ((block (compiled-entry/block entry))
	(offset (compiled-entry/offset entry)))
    (let ((dbg-info
	   (compiled-code-block/dbg-info block
					 (if (default-object? demand-load?)
					     true
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
		 false)))))))

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

(define (compiled-entry/filename entry)
  (let loop
      ((info
	(compiled-code-block/debugging-info (compiled-entry/block entry))))
    (cond ((string? info) (values info false))
	  ((not (pair? info)) (values false false))
	  ((dbg-info? (car info)) (loop (cdr info)))
	  ((string? (car info))
	   (values (car info)
		   (and (exact-nonnegative-integer? (cdr info))
			(cdr info))))
	  (else (values false false)))))

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
    (cond ((vector? items) false)
	  ((and (pair? items)
		(eq? (car items) 'COMPILED-BY-PROCEDURES)
		(pair? (cdr items))
		(pair? (cddr items)))
	   (caddr items))
	  (else (error "Illegal dbg-info-vector" info)))))

(define (fasload/update-debugging-info! value com-pathname)
  (let ((process-block
	 (lambda (block)
	   (let ((binf-filename
		  (process-binf-filename
		   (compiled-code-block/debugging-info block)
		   com-pathname)))
	     (set-compiled-code-block/debugging-info! block binf-filename)
	     binf-filename))))
    (cond ((compiled-code-address? value)
	   (process-block (compiled-code-address->block value)))
	  ((and (comment? value)
		(dbg-info-vector? (comment-text value)))
	   (let ((blocks (dbg-info-vector/blocks-vector (comment-text value))))
	     (let ((binf-filename (process-block (vector-ref blocks 0)))
		   (end (vector-length blocks)))
	       (let loop ((index 1))
		 (if (< index end)
		     (begin
		       (set-car! (compiled-code-block/debugging-info
				  (vector-ref blocks index))
				 binf-filename)
		       (loop (1+ index)))))))))))

(define (process-binf-filename binf-filename com-pathname)
  (and binf-filename
       (->namestring
	(rewrite-directory
	 (let ((binf-pathname (merge-pathnames binf-filename))
	       (com-pathname (merge-pathnames com-pathname)))
	   (if (and (equal? (pathname-name binf-pathname)
			    (pathname-name com-pathname))
		    (not (equal? (pathname-type binf-pathname)
				 (pathname-type com-pathname)))
		    (equal? (pathname-version binf-pathname)
			    (pathname-version com-pathname)))
	       (pathname-new-type com-pathname (pathname-type binf-pathname))
	       binf-pathname))))))

(define directory-rewriting-rules
  '())

(define (add-directory-rewriting-rule! match replace)
  (let ((match (merge-pathnames match))
	(replace (merge-pathnames replace)))
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
    (if rule
	(pathname-new-directory
	 pathname
	 (append (pathname-directory (cdr rule))
		 (list-tail (pathname-directory pathname)
			    (length (pathname-directory (car rule))))))
	pathname)))

(define (directory-prefix? x y)
  (and (pair? x)
       (pair? y)
       (eq? (car x) (car y))
       (let loop ((x (cdr x)) (y (cdr y)))
	 (or (null? y)
	     (and (not (null? x))
		  (equal? (car x) (car y))
		  (loop (cdr x) (cdr y)))))))

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
  false)

(define (special-form-procedure-name? name)
  (let ((association (assq name special-form-procedure-names)))
    (and association
	 (symbol->string (cdr association)))))

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
  (cond ((string? descriptor)
	 (let ((bsm (read-bsm-file descriptor)))
	   (and bsm ;; bsm are either vectors of pairs or vectors of vectors
		(if (vector? bsm)
		    (let ((first (and (not (zero? (vector-length bsm)))
				      (vector-ref bsm 0))))
		      (cond ((pair? first) bsm)
			    ((vector? first) first)
			    (else false)))))))
	((and (pair? descriptor)
	      (string? (car descriptor))
	      (exact-nonnegative-integer? (cdr descriptor)))
	 (let ((bsm (read-bsm-file (car descriptor))))
	   (and bsm
		(vector? bsm)
		(< (cdr descriptor) (vector-length bsm))
		(vector-ref bsm (cdr descriptor)))))
	(else
	 false)))

(define (read-bsm-file name)
  (let ((filename (process-bsym-filename name)))
    (if (file-exists? filename)
	(fasload-loader filename)
	(let ((pathname (merge-pathnames filename)))
	  (find-alternate-file-type pathname
				    `(("bsm" . ,fasload-loader)
				      ("bcs" . compressed-loader)))))))	

(define (process-bsym-filename name)
  (->namestring
   (rewrite-directory (merge-pathnames name))))

;;; The conversion hack.

(define (inf->bif/bsm inffile)
  (let* ((infpath (merge-pathnames inffile))
	 (bifpath (pathname-new-type infpath "bif"))
	 (bsmpath (pathname-new-type infpath "bsm")))
    (let ((binf (fasload infpath)))
      (inf-structure->bif/bsm binf bifpath bsmpath))))

(define (inf-structure->bif/bsm binf bifpath bsmpath)
  (let* ((bifpath (merge-pathnames bifpath))
	 (bsmpath (merge-pathnames bsmpath))
	 (bsmname (->namestring bsmpath)))
    (cond ((dbg-info? binf)
	   (let ((labels (dbg-info/labels/desc binf)))
	     (set-dbg-info/labels/desc! binf bsmname)
	     (fasdump binf bifpath)
	     (fasdump labels bsmpath)))
	  ((vector? binf)
	   (let ((bsm (make-vector (vector-length binf))))
	     (let loop ((pos 0))
	       (if (fix:= pos (vector-length bsm))
		   (begin
		     (fasdump bsm bsmpath)
		     (fasdump binf bifpath))
		   (let ((dbg-info (vector-ref binf pos)))
		     (let ((labels (dbg-info/labels/desc dbg-info)))
		       (vector-set! bsm pos labels)
		       (set-dbg-info/labels/desc! dbg-info (cons bsmname pos))
		       (loop (fix:1+ pos))))))))
	  (else 
	   (error "Unknown inf file format" infpath)))))


;;; UNCOMPRESS: A simple extractor for compressed binary info files.

(define (uncompress ifile ofile)
  (define-integrable window-size 4096)
  (define (expand input-port output-channel buffer-size)
    (let ((buffer (make-string buffer-size))
	  (cp-table (make-vector window-size))
	  (port/read-char 
	   (or (input-port/operation/read-char input-port)
	       (error "Port doesn't support read-char" input-port)))
	  (port/read-substring
	   (or (input-port/operation input-port 'READ-SUBSTRING)
	       (error "Port doesn't support read-substring" input-port))))
      (define (displacement->cp-index displacement cp)
	(let ((index (fix:- cp displacement)))
	  (if (fix:< index 0) (fix:+ window-size index) index)))
      (define-integrable (cp:+ cp n)
	(fix:remainder (fix:+ cp n) window-size))
      (define-integrable (read-substring! buffer start end)
	(port/read-substring input-port buffer start end))
      (define (read-ascii)
	(let ((char (port/read-char input-port)))
	  (and (not (eof-object? char))
	       (char->ascii char))))
      (define (guarantee-buffer nbp)
	(if (fix:> nbp buffer-size)
	    (let* ((new-size (fix:+ buffer-size (fix:quotient buffer-size 4)))
		   (nbuffer (make-string new-size)))
	      (substring-move-right! buffer 0 buffer-size nbuffer 0)
	      (set! buffer-size new-size)
	      (set! buffer nbuffer))))

      (let loop ((bp 0) (cp 0) (byte (read-ascii)))
	(cond ((not byte)
	       (channel-write output-channel buffer 0 bp)
	       bp)
	      ((fix:< byte 16)
	       (let ((length (fix:+ byte 1)))
		 (let ((nbp (fix:+ bp length)) (ncp (cp:+ cp length)))
		   (guarantee-buffer nbp)
		   (read-substring! buffer bp nbp)
		   (do ((bp bp (fix:+ bp 1)) (cp cp (cp:+ cp 1)))
		       ((fix:= bp nbp))
		     (vector-set! cp-table cp bp))
		   (loop nbp ncp (read-ascii)))))
	      (else
	       (let ((cpi (displacement->cp-index
			   (fix:+ (fix:* (fix:remainder byte 16) 256)
				  (read-ascii))
			   cp))	
		     (length (fix:+ (fix:quotient byte 16) 1)))
		 (let ((bp* (vector-ref cp-table cpi))
		       (nbp (fix:+ bp length))
		       (ncp (cp:+ cp 1)))
		    (guarantee-buffer nbp)
		    (substring-move-right! buffer bp* (fix:+ bp* length)
					   buffer bp)
		    (vector-set! cp-table cp bp)
		    (loop nbp ncp (read-ascii)))))))))

  (let ((input (open-binary-input-file (merge-pathnames ifile))))
    (if (not (input-port? input))
	(error "UNCOMPRESS: error opening input" ifile))
    (let* ((file-marker "Compressed-B1-1.00")
	   (marker-size (string-length file-marker))
	   (actual-marker (make-string marker-size)))
      ;; This may get more hairy as we up versions
      (if (and (fix:= ((input-port/operation input 'read-substring)
		       input actual-marker 0 marker-size)
		      marker-size)
	       (string=? file-marker actual-marker))
	  (let ((output (file-open-output-channel
			 (->namestring (merge-pathnames ofile))))
		(size (file-attributes/length (file-attributes ifile))))
	    (expand input output (fix:* size 2))
	    (channel-close output)
	    (close-input-port input))
	  (error "Not a recognized compressed file" ifile)))))

(define (find-alternate-file-type base-pathname exts/receivers)
  (or (null? exts/receivers)
      (let ((file (pathname-new-type base-pathname (caar exts/receivers))))
	(if (file-exists? file)
	    ((cdar exts/receivers) (->namestring file))
	    (find-alternate-file-type base-pathname (cdr exts/receivers))))))

(define (fasload-loader filename)
  (call-with-current-continuation
    (lambda (if-fail)
      (bind-condition-handler (list condition-type:fasload-band)
        (lambda (condition) condition (if-fail false))
        (lambda () (fasload filename true))))))

(define (compressed-loader compressed-filename)
  (call-with-temporary-filename
    (lambda (uncompressed-filename)
      (uncompress compressed-filename uncompressed-filename)
      (fasload-loader uncompressed-filename))))
  
