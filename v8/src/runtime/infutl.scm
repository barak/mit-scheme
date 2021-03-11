#| -*-Scheme-*-

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
  (let ((fasload-loader (cached-loader fasload-loader)))
    (set! inf-load-types
	  `(("inf" . ,fasload-loader)
	    ("bif" . ,fasload-loader)
	    ("bci" . ,(compressed-loader "bif" fasload-loader))))
    (set! bsm-load-types
	  `(("bsm" . ,fasload-loader)
	    ("bcs" . ,(compressed-loader "bsm" fasload-loader)))))
  (initialize-cached-files!)
  (initialize-uncompressed-files!)
  (add-event-receiver! event:after-restore initialize-uncompressed-files!)
  (add-event-receiver! event:before-exit delete-uncompressed-files!)
  (add-gc-daemon! clean-uncompressed-files!)
  (add-gc-daemon! clean-cached-files!))

(define inf-load-types)
(define bsm-load-types)

(define (compiled-module-eval module environment)
  (scode-eval (compiled-module/expression module) environment))

(define (compiled-code-block/dbg-descriptor block)
  (let ((info (compiled-code-block/debugging-info block)))
    (cond ((valid-dbg-descriptor? info)
	   info)
	  ((dbg-locator? info)
	   (cons info 0))
	  ((not (pair? info))
	   false)
	  ((valid-dbg-descriptor? (cdr info))
	   (cdr info))
	  ((dbg-locator? (cdr info))
	   (cons (cdr info) 0))
	  (else false))))

(define (compiled-code-block/dbg-info block demand-load?)
  (let ((old-info (compiled-code-block/debugging-info block)))
    (cond ((dbg-info? old-info)
	   old-info)
	  ((and (pair? old-info) (dbg-info? (car old-info)))
	   (car old-info))
	  (demand-load?
	   (let ((dbg-info (read-debugging-info
			    (compiled-code-block/dbg-descriptor block))))
	     (if dbg-info (memoize-debugging-info! block dbg-info))
	     dbg-info))
	  (else
	   false))))

(define (compiled-code-block/labels block demand-load?)
  (let ((info (compiled-code-block/dbg-info block demand-load?)))
    (and info
	 (let ((labels/desc (dbg-info/labels/desc info)))
	   (if (vector? labels/desc)
	       labels/desc
	       (let ((labels
		      (read-labels
		       (compiled-code-block/dbg-descriptor block))))
		 (and labels
		      (begin
			(set-dbg-info/labels/desc! info labels)
			labels))))))))

(define (discard-debugging-info!)
  (without-interrupts
   (lambda ()
     (map-over-population! blocks-with-memoized-debugging-info
			   discard-block-debugging-info!)
     (set! blocks-with-memoized-debugging-info (make-population))
     unspecific)))

(define (valid-dbg-descriptor? object)
  (and (pair? object)
       (dbg-locator? (car object))
       (index-fixnum? (cdr object))))

(define (read-debugging-info descriptor)
  (and (valid-dbg-descriptor? descriptor)
       (let ((binf (read-dbg-file (car descriptor) inf-load-types)))
	 (select-dbg-info descriptor binf))))

(define (read-labels descriptor)
  (and (valid-dbg-descriptor? descriptor)
       (let ((binf (read-dbg-file (car descriptor) bsm-load-types)))
	 (select-dbg-info descriptor binf))))
      
(define (select-dbg-info descriptor dbg-file-contents)
  (let ((locator  (car descriptor))
	(index    (cdr descriptor)))

    (define (complain message . other-irritants)
      (if (not (dbg-locator/status locator))
	  (begin
	    (apply warn
		   (string-append "Bad debugging information: " message ":")
		   locator
		   other-irritants)
	    (set-dbg-locator/status! locator 'BAD)))
      #F)

    (cond ((dbg-wrapper? dbg-file-contents)
	   (let ((compile-time (dbg-locator/timestamp locator))
		 (dbg-time     (dbg-wrapper/timestamp dbg-file-contents))
		 (objects      (dbg-wrapper/objects dbg-file-contents))
		 (version      (dbg-wrapper/format-version dbg-file-contents)))
	     (cond ((not (equal? compile-time dbg-time))
		    (complain "mismatched timestamps" compile-time dbg-time))
		   ((< version dbg-format:oldest-acceptable-version)
		    (complain "obsolete format version" version))
		   ((> version dbg-format:current-version)
		    (complain "future format version!" version))
		   ((or (not (vector? objects))
			(>= index (vector-length objects)))
		    (complain "vector problem" index))
		   (else
		    (vector-ref objects index)))))
	  ((false? dbg-file-contents)
	   #F)
	  (complain "not `wrapped'"))))

(define (read-dbg-file locator load-types)
  (let ((pathname
	 (canonicalize-debug-info-pathname (dbg-locator/file locator))))
    (find-alternate-file-type pathname load-types)))

(define (find-alternate-file-type base-pathname alist)
  (let loop ((left alist) (time 0) (file #f) (receiver (lambda (x t) t x)))
    (if (null? left)
	(receiver file time)
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
		   (if (and expression
			    (= offset
			       (dbg-expression/label-offset expression)))
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

(define (compiled-code-block/filename-and-index block)
  ;; Values (filename block-number), either may be #F. For the unparser.
  (let ((descriptor  (compiled-code-block/dbg-descriptor block)))
    (if descriptor
	(values (canonicalize-debug-info-pathname
		 (dbg-locator/file (car descriptor)))
		(cdr descriptor))
	(values false false))))

(define (compiled-entry/filename-and-index entry)
  (compiled-code-block/filename-and-index (compiled-entry/block entry)))

(define (dbg-labels/find-offset labels offset)
  (vector-binary-search labels < dbg-label/offset offset))

(define (fasload/update-debugging-info! value com-pathname)
  (cond ((or (compiled-code-address? value)
	     (and (comment? value)
		  (compiled-code-address? (comment-expression value))))
	 (warn "Recompile " com-pathname))
	((compiled-module? value)
	 (let* ((locator   (compiled-module/dbg-locator value))
		(pathname  (dbg-locator/file locator)))
	   (set-dbg-locator/file!
	    locator
	    (process-binf-filename pathname com-pathname))))
	(else unspecific)))

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

(define (dbg-block/find-variable block name)
  (let ((layout (dbg-block/variables block)))
    (let ((end (vector-length layout)))
      (let loop ((index 0))
	(and (< index end)
	     (let ((item (vector-ref layout index)))
	       (if (and (dbg-variable? item)
			(eq? name (dbg-variable/name item)))
		   item
		   (loop (+ index 1)))))))))

(define (compiled-procedure/name entry)
  (let ((procedure
	 (compiled-entry/dbg-object entry load-debugging-info-on-demand?)))
    (and procedure
	 (let ((name (dbg-procedure/name procedure)))
	   (or (special-form-procedure-name? name)
	       (symbol->string name))))))

(define (compiled-code-block/name block offset)
  ;; Try to come up with a name for BLOCK.  If there is one top-level
  ;; procedure, use its name.
  (define (top-level-proc-name proc)
    (and (dbg-block? (dbg-procedure/block proc))
	 (eq? 'IC (dbg-block/parent (dbg-procedure/block proc)))
	 (let ((name (dbg-procedure/name proc)))
	   (or (special-form-procedure-name? name)
	       name))))
  offset ; ignored
  (let ((dbg-info
	 (compiled-code-block/dbg-info block load-debugging-info-on-demand?)))
    (and dbg-info
	 (not (dbg-info/expression dbg-info)) ; top level or group compiled
	 (let ((procs (dbg-info/procedures dbg-info)))
	   (let loop ((i 0) (name #F))
	     (cond ((= i (vector-length procs))  name)
		   ((top-level-proc-name (vector-ref procs i))
		    => (lambda (name*)
			 (and (not name)
			      (loop (+ i 1) name*))))
		   (else (loop (+ i 1) name))))))))

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

;;;; Splitting of info structures

(define (inf->bif/bsm inffile)
  (let* ((infpath (merge-pathnames inffile))
	 (bifpath (pathname-new-type infpath "bif"))
	 (bsmpath (pathname-new-type infpath "bsm")))
    (let ((binf (fasload infpath)))
      (inf-structure->bif/bsm binf bifpath bsmpath))))

(define (inf-structure->bif/bsm binf bifpath bsmpath)
  (error "Needs fixing")
  (let ((bifpath (merge-pathnames bifpath))
	(bsmpath (and bsmpath (merge-pathnames bsmpath))))
    (let ((bsm (split-inf-structure! binf bsmpath)))
      (fasdump binf bifpath true)
      (if bsmpath
	  (fasdump bsm bsmpath true)))))

(define (split-inf-structure! binf replacement)
  (cond ((vector? binf)
	 (let ((n (vector-length binf)))
	   (let ((bsm (make-vector n)))
	     (do ((i 0 (fix:+ i 1)))
		 ((fix:= i n))
	       (let ((dbg-info (vector-ref binf i)))
		 (let ((labels (dbg-info/labels/desc dbg-info)))
		   (vector-set! bsm i labels)
		   (set-dbg-info/labels/desc! dbg-info replacement))))
	     bsm)))
	(else 
	 (error "Unknown inf format:" binf))))

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
	(substring-move-right! buffer 0 buffer-size nbuffer 0)
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
	  (substring-move-right! buffer 0 buffer-size nbuffer 0)
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

(define (fasload-without-errors filename)
  (call-with-current-continuation
    (lambda (if-fail)
      (bind-condition-handler (list condition-type:fasload-band)
	(lambda (condition) condition (if-fail false))
        (lambda () (fasload filename true))))))

(define (fasload-loader filename file-time)
  file-time				; ignored
  (fasload-without-errors filename))

(define (compressed-loader uncompressed-type uncompressed-loader)
  (lambda (compressed-file compressed-time)
    (lookup-uncompressed-file
     compressed-file compressed-time uncompressed-loader
     (lambda ()
       (define (load-compressed temporary-file)
	 (call-with-current-continuation
	  (lambda (k)
	    (uncompress-internal compressed-file
				 temporary-file
				 (lambda (message . irritants)
				   message irritants
				   (k #f)))
	    (uncompressed-loader 
	     temporary-file
	     (file-modification-time-direct temporary-file)))))
       (case *save-uncompressed-files?*
	 ((#F)
	  (call-with-temporary-file-pathname load-compressed))
	 ((AUTOMATIC)
	  (call-with-uncompressed-file-pathname compressed-file
						compressed-time
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
	       result)))))))))

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

(define-structure (file-entry
		   (type vector)
		   (conc-name file-entry/))
  compressed-name
  compressed-time
  uncompressed-name
  uncompressed-time
  last-use-time)

(define (lookup-uncompressed-file compressed-file compressed-time
				  if-found if-not-found)
  (dynamic-wind
   (lambda ()
     (set-car! uncompressed-files (+ (car uncompressed-files) 1)))
   (lambda ()
     (let loop ((entries (cdr uncompressed-files)))
       (if (null? entries)
	   (if-not-found)
	   (let ((entry (car entries)))
	     (if (and (pathname=? (file-entry/compressed-name entry)
				  compressed-file)
		      (file-entry/uncompressed-name entry)
		      (= (file-entry/compressed-time entry) compressed-time)
		      (or (file-exists? (file-entry/uncompressed-name entry))
			  (begin
			    (set-file-entry/uncompressed-name! entry #F)
			    #f)))
		 (dynamic-wind
		  (lambda () unspecific)
		  (lambda () (if-found (file-entry/uncompressed-name entry)
				       (file-entry/uncompressed-time entry)))
		  (lambda ()
		    (set-file-entry/last-use-time! entry (real-time-clock))))
		 (loop (cdr entries)))))))
   (lambda ()
     (set-car! uncompressed-files (- (car uncompressed-files) 1)))))

(define (call-with-uncompressed-file-pathname compressed-file compressed-time
					      receiver)
  (let ((temporary-file (temporary-file-pathname)))
    (let ((entry
	   (make-file-entry
	    compressed-file compressed-time
	    temporary-file  (file-modification-time-direct temporary-file)
	    (real-time-clock))))
      (dynamic-wind
       (lambda () unspecific)
       (lambda ()
	 (without-interrupts
	  (lambda ()
	    (set-cdr! uncompressed-files
		      (cons entry (cdr uncompressed-files)))))
	 (receiver temporary-file))
       (lambda ()
	 (set-file-entry/last-use-time! entry (real-time-clock)))))))

(define (delete-uncompressed-files!)
  (do ((entries (cdr uncompressed-files) (cdr entries)))
      ((null? entries) unspecific)
    (let ((name (file-entry/uncompressed-name (car entries))))
      (if name
	  (deallocate-temporary-file name)))))

(define (clean-uncompressed-files!)
  (if (= 0 (car uncompressed-files))
      (let ((time (real-time-clock)))
	(let loop
	    ((entries (cdr uncompressed-files))
	     (prev    uncompressed-files))
	  (if (pair? entries)
	      (let ((entry (car entries)))
		(if (or (not (file-entry/uncompressed-name entry))
			(< (- time (file-entry/last-use-time entry))
			   *uncompressed-file-lifetime*))
		    (loop (cdr entries) entries)
		    (begin
		      (set-cdr! prev (cdr entries))
		      (deallocate-temporary-file
		       (file-entry/uncompressed-name entry))
		      (loop (cdr entries) prev)))))))))

(define (initialize-uncompressed-files!)
  (set! uncompressed-files (list 0))
  unspecific)

(define *save-uncompressed-files?* 'AUTOMATIC)
(define *uncompressed-file-lifetime* 300000)
(define uncompressed-files)

(define ((cached-loader loader) filename time)
  (define (reload)
    (let ((object  (loader filename time)))
      (set-cdr! cached-files
		(cons (cons filename (weak-cons object time))
		      (cdr cached-files)))
      object))
  (if cached-files
      (let ((entry (assoc filename (cdr cached-files))))
	(if entry
	    (let ((object  (weak-car (cdr entry)))
		  (time*   (weak-cdr (cdr entry))))
	      (if (and object (= time time*))
		  object
		  (reload)))
	    (reload)))
      (loader filename time)))

(define (clean-cached-files!)
  (let loop ((items  (cdr cached-files))
	     (prev   cached-files))
    (cond ((null? items))
	  ((or (not (caar items))
	       (not (weak-car (cdar items))))
	   (set-cdr! prev (cdr items))
	   (loop (cdr items) prev))
	  (else
	   (loop (cdr items) (cdr prev))))))

(define (initialize-cached-files!)
  (set! cached-files (list #F)))

(define cached-files #F)