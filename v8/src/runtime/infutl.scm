#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/runtime/infutl.scm,v 1.3 1988/08/05 20:47:32 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Compiled Code Information
;;; package: (runtime compiler-info)

(declare (usual-integrations))

(define (initialize-package!)
  (make-value-cache uncached-block->compiler-info
    (lambda (compute-value flush-cache)
      (set! compiled-code-block->compiler-info compute-value)
      (set! flush-compiler-info! flush-cache))))

(define-integrable compiler-info-tag
  (string->symbol "#[COMPILER-INFO]"))

(define-integrable compiler-entries-tag
  (string->symbol "#[COMPILER-ENTRIES]"))

(define-structure (compiler-info (named compiler-info-tag))
  (procedures false read-only true)
  (continuations false read-only true)
  (labels false read-only true))

(define-structure (label-info (type vector))
  (name false read-only true)
  (offset false read-only true)
  (external? false read-only true))

;;; Yes, you could be clever and do a number of integrations in this file
;;; however, I don't think speed will be the problem.

;;; Currently, the info slot is in one of several formats:
;;;
;;; NULL -- There is no info.
;;;
;;; COMPILER-INFO -- Just the structure you see above.
;;;
;;; STRING -- The pathstring of the binf file.
;;;
;;; PAIR -- The CAR is the pathstring
;;;         The CDR is either COMPILER-INFO or a NUMBER
;;;	    indicating the offset into the binf file that
;;;	    you should use to find the info.

(define (block->info-slot-contents block if-found if-not-found)
  ;; Fetches the contents of the compiler-info slot in a block.
  ;; Calls if-not-found if there is no slot (block is manifest-closure).
  (if (compiled-code-block/manifest-closure? block)
      (if-not-found)
      (if-found (compiled-code-block/debugging-info block))))

(define (parse-info-slot-contents slot-contents
	  if-no-info
	  if-pathstring
	  if-info
	  if-pathstring-and-info
	  if-pathstring-and-offset)
  (cond ((null? slot-contents) (if-no-info))
	((compiler-info? slot-contents) (if-info slot-contents))
	((string? slot-contents) (if-pathstring slot-contents))
	((pair? slot-contents)
	 (if (string? (car slot-contents))
	     (cond ((compiler-info? (cdr slot-contents)) 
		    (if-pathstring-and-info (car slot-contents)
					    (cdr slot-contents)))
		   ((number? (cdr slot-contents))
		    (if-pathstring-and-offset (car slot-contents)
					      (cdr slot-contents)))
		   (else (if-no-info)))
	     (if-no-info)))
	(else (if-no-info))))

(define (info-slot-contents->pathstring slot-contents if-found if-not-found)
  ;; Attempts to get the string denoting the file that the compiler-info
  ;; is loaded from.
  (parse-info-slot-contents slot-contents
    if-not-found
    if-found
    (lambda (info) info (if-not-found))
    (lambda (pathstring info)
      info 
      (if-found pathstring))
    (lambda (pathstring offset)
      offset 
      (if-found pathstring))))

(define (info-slot-contents->compiler-info slot-contents if-found if-not-found)
  ;; Attempts to get the compiler info denoted by the contents of the
  ;; info slot.
  (parse-info-slot-contents slot-contents
    if-not-found
    (lambda (pathstring) 
      (on-demand-load pathstring #f if-found if-not-found))
    (lambda (info)
      (if-found info))
    (lambda (pathstring info) 
      pathstring
      (if-found info))
    (lambda (pathstring offset) 
      (on-demand-load pathstring offset if-found if-not-found))))

(define *compiler-info/load-on-demand?* #f)

(define (compiler-info/with-on-demand-loading thunk)
  (fluid-let ((*compiler-info/load-on-demand?* #t))
    (thunk)))

(define (compiler-info/without-on-demand-loading thunk)
  (fluid-let ((*compiler-info/load-on-demand?* #f))
    (thunk)))

;;; The binf file is either a compiler-info structure, or
;;; a vector with a compiler-info structure in it.

;;; If the binf file is a vector, the offset, obtained from the info slot
;;; in the block, will be the index of the vector slot containing the info.
;;; If there was no offset, the zeroth slot has the info in it.

(define (on-demand-load pathstring offset if-found if-not-found)
  (cond ((not *compiler-info/load-on-demand?*) (if-not-found))
	((not (file-exists? pathstring)) (if-not-found))
	(else (let ((object (fasload pathstring)))
		(if (null? offset)
		    (if (compiler-info? object)
			(if-found object)
			(if (and (vector? object)
				 (> (vector-length object) 0)
				 (compiler-info? (vector-ref object 0)))
			    (if-found (vector-ref object 0))
			    (if-not-found)))
		    (if (and (vector? object)
			     (< offset (vector-length object)))
			(let ((possible-info (vector-ref object offset)))
			  (if (compiler-info? possible-info)
			      (if-found possible-info)
			      (if-not-found)))
			(if-not-found)))))))

;; Uncached version will reload the binf file each time.

(define (block->info block info-hacker if-found if-not-found)
  (block->info-slot-contents block
      (lambda (contents)
	(info-hacker contents if-found if-not-found))
      if-not-found))

(define (uncached-block->compiler-info block if-found if-not-found)
  (block->info block info-slot-contents->compiler-info if-found if-not-found))

(define (compiled-code-block->pathstring block if-found if-not-found)
  (block->info block info-slot-contents->pathstring if-found if-not-found))

(define flush-compiler-info!)
(define compiled-code-block->compiler-info)

(define (make-value-cache function receiver)
  (let ((cache (make-1d-table)))

    (define (flush-cache!)
      (set! cache (make-1d-table))
      'flushed)

    (define (compute-value argument if-found if-not-found)
      (1d-table/lookup cache argument
	 if-found
	 (lambda ()
	   (function argument
	     (lambda (value)
	       (1d-table/put! cache argument value)
	       (if-found value))
	     if-not-found))))

    (receiver compute-value flush-cache!)))

(define (entry->info entry block-info-hacker if-found if-not-found)
  (compiled-entry->block-and-offset-indirect entry
     (lambda (block offset)
       offset
       (block-info-hacker block if-found if-not-found))
     if-not-found))

(define (compiled-entry->pathstring entry if-found if-not-found)
  (entry->info entry compiled-code-block->pathstring if-found if-not-found))

(define (compiled-entry->pathname entry if-found if-not-found)
  (compiled-entry->pathstring entry
    (lambda (pathstring)
      (if-found (string->pathname pathstring)))
    if-not-found))

(define (info-file object)
  (and (compiled-code-address? object)
       (pathname-name (compiled-entry->pathname object
						identity-procedure
						false-procedure))))

(define (compiled-entry->compiler-info entry if-found if-not-found)
  (entry->info entry compiled-code-block->compiler-info if-found if-not-found))

;;; This switch gets turned on when the implementation for
;;; INDIRECT-THROUGH-MANIFEST-CLOSURE is present.
;;; The mechanism for indirecting through a manifest closure
;;; is highly machine dependent.

(define *indirect-through-manifest-closure? #f)
(define indirect-through-manifest-closure)

(define (compiled-entry->block-and-offset entry 
					  if-block
					  if-manifest-closure
					  if-failed)
  (let ((block  (compiled-code-address->block entry))
	(offset (compiled-code-address->offset entry)))
    (if (compiled-code-block/manifest-closure? block)
	(if *indirect-through-manifest-closure?
	    (indirect-through-manifest-closure entry
	      (lambda (indirect-block indirect-offset)
		(if-manifest-closure
		 block offset indirect-block indirect-offset))
	       (lambda () (if-failed)))
	    (if-failed))
	(if-block block offset))))

(define (compiled-entry->block-and-offset-indirect 
	 entry if-found if-not-found)
  (compiled-entry->block-and-offset entry
    if-found
    (lambda (closure-block closure-offset block offset)
      closure-block closure-offset
      (if-found block offset))
    if-not-found))

(define (block-symbol-table block if-found if-not-found)
  (compiled-code-block->compiler-info block
    (lambda (info)
      (if-found (compiler-info/symbol-table info)))
    if-not-found))

(define (compiled-entry->name compiled-entry if-found if-not-found)
  (define (block-and-offset->name block offset)
    (block-symbol-table block
      (lambda (symbol-table)
	(sorted-vector/lookup symbol-table offset 
          (lambda (label-info)
	    (if-found (label-info-name label-info)))
	  if-not-found))
      if-not-found))

  (compiled-entry->block-and-offset compiled-entry
    block-and-offset->name
    (lambda (manifest-block manifest-offset block offset)
      manifest-block manifest-offset
      (block-and-offset->name block offset))
    if-not-found))

(define (compiler-info/symbol-table compiler-info)
  (make-sorted-vector (compiler-info-labels compiler-info)
		      (lambda (offset label-info)
			(= offset (label-info-offset label-info)))
		      (lambda (offset label-info)
			(< offset (label-info-offset label-info)))))

(define (lookup-label labels label-name if-found if-not-found)
  (let ((limit (vector-length labels)))
    (let loop ((index 0))
      (if (= index limit) 
	  (if-not-found)
	  (let ((this-label (vector-ref labels index)))
	    (if (string-ci=? label-name (label-info-name this-label))
		(if-found index this-label)
		(loop (1+ index))))))))

(define (label->offset labels name if-found if-not-found)
  (lookup-label labels name
    (lambda (vector-index label-info)
      vector-index
      (if-found (label-info-offset label-info)))
    if-not-found))

;;;; Binary Search

(define-structure (sorted-vector
		   (conc-name sorted-vector/)
		   (constructor %make-sorted-vector))
  (vector false read-only true)
  (key=? false read-only true)
  (key-compare false read-only true))

(define (make-sorted-vector vector key=? key<?)
  (%make-sorted-vector vector
			 key=?
			 (lambda (key entry if= if< if>)
			   ((cond ((key=? key entry) if=)
				  ((key<? key entry) if<)
				  (else if>))))))

(define (sorted-vector/find-element sorted-vector key)
  (let ((vector (sorted-vector/vector sorted-vector)))
    (vector-binary-search vector
			  key
			  (sorted-vector/key-compare sorted-vector)
			  (lambda (index) (vector-ref vector index))
			  (lambda () false))))

(define (sorted-vector/lookup sorted-vector key if-found if-not-found)
  (let ((vector (sorted-vector/vector sorted-vector)))
    (vector-binary-search vector
			  key
			  (sorted-vector/key-compare sorted-vector)
			  (lambda (index) (if-found (vector-ref vector index)))
			  (lambda () (if-not-found)))))

(define (sorted-vector/find-indices sorted-vector key if-found if-not-found)
  (vector-binary-search-range (sorted-vector/vector sorted-vector)
			      key
			      (sorted-vector/key=? sorted-vector)
			      (sorted-vector/key-compare sorted-vector)
			      if-found
			      if-not-found))

(define (sorted-vector/there-exists? sorted-vector key predicate)
  (sorted-vector/find-indices sorted-vector key
    (lambda (low high)
      (let ((vector (sorted-vector/vector sorted-vector)))
	(let loop ((index low))
	  (if (predicate (vector-ref vector index))
	      true
	      (and (< index high)
		   (loop (1+ index)))))))
    (lambda () false)))

(define (sorted-vector/for-each sorted-vector key procedure)
  (sorted-vector/find-indices sorted-vector key
    (lambda (low high)
      (let ((vector (sorted-vector/vector sorted-vector)))
	(let loop ((index low))
	  (procedure (vector-ref vector index))
	  (if (< index high)
	      (loop (1+ index))))))
    (lambda () unspecific)))

(define (vector-binary-search-range vector key key=? compare if-found
				    if-not-found)
  (vector-binary-search vector key compare
    (lambda (index)
      (if-found (let loop ((index index))
		  (if (zero? index)
		      index
		      (let ((index* (-1+ index)))
			(if (key=? key (vector-ref vector index*))
			    (loop index*)
			    index))))
		(let ((end (-1+ (vector-length vector))))
		  (let loop ((index index))
		    (if (= index end)
			index
			(let ((index* (1+ index)))
			  (if (key=? key (vector-ref vector index*))
			      (loop index*)
			      index)))))))
    if-not-found))

(define (vector-binary-search vector key compare if-found if-not-found)
  (let loop ((low 0) (high (-1+ (vector-length vector))))
    (if (< high low)
	(if-not-found)
	(let ((index (quotient (+ high low) 2)))
	  (compare key
		   (vector-ref vector index)
		   (lambda () (if-found index))
		   (lambda () (loop low (-1+ index)))
		   (lambda () (loop (1+ index) high)))))))

(define (vector-linear-search vector key compare if-found if-not-found)
  (let ((limit (length vector)))
    (let loop ((index 0))
      (if (> index limit)
	  (if-not-found)
	  (compare key 
		   (vector-ref vector index) 
		   (lambda () (if-found index))
		   (lambda () (loop (1+ index))))))))