#| -*-Scheme-*-

$Id: infutl.scm,v 1.63 2001/03/21 05:39:44 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
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
	  (,lambda-tag:fluid-let . FLUID-LET)
	  (,lambda-tag:make-environment . MAKE-ENVIRONMENT)))
  (set! blocks-with-memoized-debugging-info (make-population))
  (add-secondary-gc-daemon! discard-debugging-info!)
  (initialize-uncompressed-files!)
  (add-event-receiver! event:after-restore initialize-uncompressed-files!)
  (add-event-receiver! event:before-exit delete-uncompressed-files!)
  (add-gc-daemon! clean-uncompressed-files!))

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
	  (else #f))))

(define (discard-debugging-info!)
  (without-interrupts
   (lambda ()
     (map-over-population! blocks-with-memoized-debugging-info
			   discard-block-debugging-info!)
     (set! blocks-with-memoized-debugging-info (make-population))
     unspecific)))

(define (read-debugging-info descriptor)
  (cond ((debug-info-pathname? descriptor)
	 (let ((binf (read-binf-file descriptor)))
	   (and binf
		(if (dbg-info? binf)
		    binf
		    (and (vector? binf)
			 (not (zero? (vector-length binf)))
			 (vector-ref binf 0))))))
	((and (pair? descriptor)
	      (debug-info-pathname? (car descriptor))
	      (exact-nonnegative-integer? (cdr descriptor)))
	 (let ((binf (read-binf-file (car descriptor))))
	   (and binf
		(vector? binf)
		(< (cdr descriptor) (vector-length binf))
		(vector-ref binf (cdr descriptor)))))
	(else #f)))

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
  (let loop ((info (compiled-code-block/debugging-info block)))
    (cond ((debug-info-pathname? info)
	   (values (canonicalize-debug-info-filename info) #f))
	  ((not (pair? info)) (values #f #f))
	  ((dbg-info? (car info)) (loop (cdr info)))
	  ((debug-info-pathname? (car info))
	   (values (canonicalize-debug-info-filename (car info))
		   (and (exact-nonnegative-integer? (cdr info))
			(cdr info))))
	  (else (values #f #f)))))

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
  (let ((process-block
	 (lambda (block)
	   (let ((binf-filename
		  (process-binf-filename
		   (compiled-code-block/debugging-info block)
		   com-pathname)))
	     (set-compiled-code-block/debugging-info! block binf-filename)
	     binf-filename)))
	(process-subblocks
	 (lambda (blocks start binf-filename)
	   (let ((end (vector-length blocks)))
	     (let loop ((index start))
	       (if (< index end)
		   (begin
		     (set-car! (compiled-code-block/debugging-info
				(vector-ref blocks index))
			       binf-filename)
		     (loop (1+ index)))))))))

    (cond ((compiled-code-address? value)
	   (let ((binf-filename
		  (process-block (compiled-code-address->block value)))
		 (blocks (load/purification-root value)))
	     (if (vector? blocks)
		 (process-subblocks blocks 0 binf-filename))))
	  ((and (comment? value)
		(dbg-info-vector? (comment-text value)))
	   (let ((blocks (dbg-info-vector/blocks-vector (comment-text value))))
	     (process-subblocks blocks
				1
				(process-block (vector-ref blocks 0))))))))

(define (process-binf-filename binf-filename com-pathname)
  (and binf-filename
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
	      binf-pathname)))))

(define (debug-info-pathname? object)
  (or (pathname? object)
      (string? object)))

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
  (if (pathname-absolute? pathname)
      pathname
      (merge-pathnames
       pathname
       (let ((value
	      (get-environment-variable "MITSCHEME_INF_DIRECTORY")))
	 (if value
	     (pathname-as-directory value)
	     (system-library-directory-pathname "SRC"))))))

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
    (let ((binf (fasload infpath)))
      (inf-structure->bif/bsm binf bifpath bsmpath))))

(define (inf-structure->bif/bsm binf bifpath bsmpath)
  (let ((bifpath (merge-pathnames bifpath))
	(bsmpath (and bsmpath (merge-pathnames bsmpath))))
    (let ((bsm (split-inf-structure! binf bsmpath)))
      (fasdump binf bifpath #t)
      (if bsmpath
	  (fasdump bsm bsmpath #t)))))

(define (split-inf-structure! binf bsmpath)
  (let ((bsmname (and bsmpath (->namestring bsmpath))))
    (cond ((dbg-info? binf)
	   (let ((labels (dbg-info/labels/desc binf)))
	     (set-dbg-info/labels/desc! binf bsmname)
	     labels))
	  ((vector? binf)
	   (let ((n (vector-length binf)))
	     (let ((bsm (make-vector n)))
	       (do ((i 0 (fix:+ i 1)))
		   ((fix:= i n))
		 (let ((dbg-info (vector-ref binf i)))
		   (let ((labels (dbg-info/labels/desc dbg-info)))
		     (vector-set! bsm i labels)
		     (set-dbg-info/labels/desc!
		      dbg-info
		      (and bsmname (cons bsmname i))))))
	       bsm)))
	  (else 
	   (error "Unknown inf format:" binf)))))

;;;; UNCOMPRESS
;;;  A simple extractor for compressed binary info files.

(define-integrable window-size 4096)

(define (uncompress-ports input-port output-port #!optional buffer-size)
  (uncompress-kernel-by-blocks
   input-port output-port
   (if (default-object? buffer-size) 4096 buffer-size)
   (input-port/operation input-port 'READ-SUBSTRING)))

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
;;  . The main speed gain is from is by buffering the input.  This version
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
;;  . Some of the drudge in passing all of the state is handed over to the
;;    compiler by making the procedures internal to PARSE-COMMAND.
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
      (bind-condition-handler (list condition-type:fasload-band)
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
		   (cddar entries)
		   (or (file-exists? (cadar entries))
		       (begin
			 (set-cdr! (cdar entries) #f)
			 #f)))
	      (dynamic-wind
	       (lambda () unspecific)
	       (lambda () (if-found (cadar entries)))
	       (lambda () (set-cdr! (cdar entries) (real-time-clock)))))
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
		    (deallocate-temporary-file (cadar entries))
		    (loop (cdr entries) prev))))))))

(define (initialize-uncompressed-files!)
  (set! uncompressed-files (list 0))
  unspecific)

(define *save-uncompressed-files?* 'AUTOMATIC)
(define *uncompressed-file-lifetime* 300000)
(define uncompressed-files)