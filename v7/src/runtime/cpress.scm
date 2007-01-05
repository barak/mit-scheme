#| -*-Scheme-*-

$Id: cpress.scm,v 1.15 2007/01/05 15:33:09 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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

;;;; Data Compressor

(declare (usual-integrations))

;; This declaration is worth up to 30% speedup
(declare
 (ignore-reference-traps
  (set root-nodes oldest-node newest-node window-filled? byte-buffer)))

;; This does not seem to make much difference:
(declare (ignore-reference-traps (set current-pointer current-bp command-bp)))

;;; This compression program is based on the algorithm described in
;;; "Data Compression with Finite Windows", by Edward R. Fiala and
;;; Daniel H. Greene, Xerox CSL-89-3.  A version of this paper
;;; appeared in "Communications of the Association for Computing
;;; Machinery", 32(1), 1989.

;;; This is a one-pass lossless substitution algorithm.  The algorithm
;;; works by finding large blocks of text in the input stream and
;;; replacing them with shorter references to earlier occurrences of
;;; identical text.  In order to limit the amount of memory needed by
;;; the compressor and expander, a sliding "window" is used to
;;; remember the input stream, and "copy" references may only refer to
;;; text within that window.

;;; The output stream of the compressor is a series of "commands", of
;;; which there are two kinds: "literal" and "copy".  A literal
;;; command specifies a sequence of bytes that appear in the input
;;; stream.  A copy command is a reference to some earlier part of the
;;; input stream, consisting of a length field and a relative pointer
;;; to the position of the referenced text.

;;; Fiala and Greene describe five algorithms, which they name A1, A2,
;;; B1, B2, and C2:

;;; A1 and B1 use a simple encoding of commands that is suitable for
;;; byte-addressed machines.  This encoding is adequate for many
;;; purposes but does not achieve the compression ratios of the other
;;; algorithms.

;;; A2 and B2 use a more complex encoding that results in a
;;; significantly better compression ratio.  The price is that the
;;; compression and expansion are slower than that achieved with A1
;;; and B2.

;;; C2's encoding is even more complex, and results in the best
;;; overall compression ratio.  The compression speed of C2 is the
;;; same as that of B2, and the expansion is about 25% slower.

;;; A1 and A2 encode the relative pointers in copy commands as
;;; positions in the input byte stream.  B1 and B2 encode these
;;; pointers as positions in the output command stream, which the
;;; expander then translates back into byte positions.  The B
;;; algorithms speed up compression by approximately a factor of three
;;; over their A counterparts, while slowing expansion slightly.  The
;;; reason that compression is so much faster is that the A algorithms
;;; require much more complex data structures to keep track of the
;;; information in the window.

;;; C2 is like B2, except that it encodes further information about
;;; the data structures it is using to represent the window, assumes
;;; that the expander reproduces those data structures, and takes
;;; advantage of that assumption to achieve shorter references.

;;; This program implements the window data structures required by
;;; the algorithms B1, B2, and C2.  The encoder, which appears below,
;;; determines the algorithm.

(define input-port)
(define output-port)

(define (compress ifile ofile)
  (call-with-binary-input-file (merge-pathnames ifile)
    (lambda (input)
      (call-with-binary-output-file (merge-pathnames ofile)
        (lambda (output)				      
	  (write-string "Compressed-B1-1.00" output)
	  (compress-ports input output))))))

(define (compress-ports input output)
  (fluid-let ((root-nodes (make-vector 256 false))
	      (oldest-node false)
	      (newest-node false)
	      (window-filled? false)
	      (compress-continuation)
	      (byte-buffer (make-byte-buffer))
	      (current-pointer 0)
	      (current-bp 0)
	      (command-bp 0)
	      (output-buffer (make-output-buffer))
	      (input-port input)
	      (output-port output))
    (call-with-current-continuation
     (lambda (continuation)
       (set! compress-continuation continuation)
       (idle)))
    (flush-output-buffer)))

(define (idle)
  ;; This is the top of the compression loop.  We've just emitted a
  ;; command.  If the next two bytes can be matched against some text
  ;; in the window, start a copy command, otherwise start a literal.
  (guarantee-buffer-space 2)
  (let ((node (match-first)))
    (if (not node)
	(generate-literal)
	(let ((node (match-next node 1)))
	  (if (not node)
	      (generate-literal)
	      (generate-copy node 2))))))

(define (generate-literal)
  (guarantee-buffer-space (fix:+ literal-max 2))
  (letrec
      ((loop
	(lambda (nb)
	  (let ((node (match-first)))
	    (if (not node)
		(continue nb)
		(let ((node (match-next node 1)))
		  (if (not node)
		      (continue nb)
		      (let ((node (match-next node 2)))
			(if (not node)
			    (begin
			      (unread-byte)
			      (continue nb))
			    (let ((nb*
				   (let ((cbp current-bp)
					 (nbp (node-bp node)))
				     (fix:- (if (fix:< cbp nbp)
						(fix:+ cbp buffer-size)
						cbp)
					    nbp))))
			      (if (fix:< nb* 3)
				  ;; Don't consider matches that
				  ;; would result in a copy that is
				  ;; copying from itself.
				  (begin
				    (unread-bytes 2)
				    (continue nb))
				  (begin
				    (write-literal nb)
				    (generate-copy node 3))))))))))))
       (continue
	(lambda (nb)
	  (increment-current-pointer)
	  (increment-bp)
	  (let ((nb (fix:+ nb 1)))
	    (if (fix:< nb literal-max)
		(loop nb)
		(begin
		  (write-literal nb)
		  (idle)))))))
    (increment-current-pointer)
    (increment-bp)
    (loop 1)))

(define (generate-copy node nb)
  (guarantee-buffer-space copy-max)
  (let ((copy-pointer current-pointer))
    (let ((finish
	   (lambda (nb pointer bp)
	     (let ((nb*
		    (fix:- (let ((bp* command-bp))
			     (if (fix:< bp* bp)
				 (fix:+ bp* buffer-size)
				 bp*))
			   bp))
		   (do-copy
		    (lambda (nb)
		      (write-copy nb pointer copy-pointer)
		      (increment-current-pointer)
		      (idle))))
	       ;; NB is the number of bytes that we want to write a
	       ;; copy command for; NB* is the number of bytes between
	       ;; the start of the copy and the current position.  If
	       ;; NB* is less than NB, we must truncate the copy in
	       ;; order to prevent it from copying from itself.  If
	       ;; NB* is 1, then don't copy -- it's too short.
	       (if (fix:<= nb nb*)
		   (do-copy nb)
		   (begin
		     (unread-bytes (fix:- nb nb*))
		     (if (fix:= nb* 1)
			 (generate-literal)
			 (do-copy nb*))))))))
      (let loop ((node node) (nb nb))
	(let ((pointer (node-pointer node))
	      (bp (node-bp node)))
	  (if (not (byte-ready?))
	      (finish nb pointer bp)
	      (let ((node* (match-next node nb)))
		(if (not node*)
		    (finish nb pointer bp)
		    (let ((nb (fix:+ nb 1)))
		      (if (fix:< nb copy-max)
			  (loop node* nb)
			  (if (eq? node node*)
			      (finish nb pointer bp)
			      (let ((pointer (node-pointer node*))
				    (bp (node-bp node*)))
				(update-node-pointer node*)
				(finish nb pointer bp)))))))))))))

(define (match-first)
  (let ((byte (read-byte)))
    (let ((node (vector-ref root-nodes byte)))
      (if (not node)
	  (add-child false byte (make-node 0)))
      node)))

(define (match-next node nb)
  (let ((byte (peek-byte)))
    (if (fix:= (node-nb node) nb)
	(begin
	  (update-node-pointer node)
	  (let loop ((child (node-children node)))
	    (cond ((not child)
		   (add-child node byte (make-node 0))
		   false)
		  ((fix:= byte (node-byte child))
		   (discard-byte)
		   child)
		  (else
		   (loop (node-next child))))))
	(let ((byte* (node-ref node nb)))
	  (if (fix:= byte byte*)
	      (begin
		(discard-byte)
		node)
	      (begin
		(let ((parent (make-node nb)))
		  (replace-child node parent)
		  (add-child parent byte* node)
		  (add-child parent byte (make-node 0)))
		false))))))

;;;; PATRICIA Tree Database

(define root-nodes)
(define oldest-node)
(define newest-node)
(define window-filled?)

(define-structure (node (constructor %make-node (nb older)))
  ;; The parent of this node, or #F for a root node.
  (parent false)

  ;; The children of this node.  Either #F for no children, or the
  ;; first child.  The remaining children are accessed through the
  ;; NODE-NEXT fields.  A node will never have exactly one child.
  (children false)

  ;; The adjacent siblings of this node, or #F if none.
  (previous false)
  (next false)

  ;; The first byte of the substring between the parent and this node.
  (byte false)

  ;; The number of bytes in the string represented by this node,
  ;; counting down from the root of the tree.
  (nb 0)

  ;; The adjacent nodes in the node pointer ordering.  The OLDER node
  ;; has less recent POINTER and BP, while the newer node has more recent.
  (older false)
  (newer false)

  ;; The command pointer for this node.
  (pointer current-pointer)

  ;; The byte pointer for this node.
  (bp current-bp))

(define (make-node nb)
  (let ((node (%make-node nb newest-node)))
    (if newest-node
	(set-node-newer! newest-node node)
	(set! oldest-node node))
    (set! newest-node node)
    node))

(define (update-node-pointer node)
  (set-node-pointer! node current-pointer)
  (set-node-bp! node current-bp)
  (let ((older (node-older node))
	(newer (node-newer node)))
    (if newer
	(begin
	  (set-node-older! newer older)
	  (if older
	      (set-node-newer! older newer)
	      (set! oldest-node newer))
	  (set-node-newer! node false)
	  (set-node-older! node newest-node)
	  (set-node-newer! newest-node node)
	  (set! newest-node node)
	  unspecific))))

(define (add-child parent byte child)
  (set-node-parent! child parent)
  (set-node-byte! child byte)
  (if parent
      (let ((sibling (node-children parent)))
	(set-node-next! child sibling)
	(if sibling (set-node-previous! sibling child))
	(set-node-children! parent child))
      (vector-set! root-nodes byte child)))

(define (replace-child child child*)
  (let ((parent (node-parent child))
	(byte (node-byte child)))
    (set-node-parent! child* parent)
    (set-node-byte! child* byte)
    (if parent
	(begin
	  (let ((previous (node-previous child)))
	    (set-node-previous! child* previous)
	    (if previous
		(set-node-next! previous child*)
		(set-node-children! parent child*)))
	  (let ((next (node-next child)))
	    (set-node-next! child* next)
	    (if next
		(set-node-previous! next child*))))
	(vector-set! root-nodes byte child*))))

(define (set-oldest-node node pointer)
  (let ((node
	 (do ((node node (node-newer node)))
	     ((not (fix:= (node-pointer node) pointer)) node))))
    (if (not (eq? node oldest-node))
	(let ((older (node-older node)))
	  (set-node-older! node false)
	  (set! oldest-node node)
	  ;; We don't have to do anything complicated to delete a node.
	  ;; If the node has any children, we know that they are also
	  ;; being deleted, because a descendant cannot be newer than
	  ;; its ancestor.  However, we want to avoid deleting a child
	  ;; from its parent if the parent is also going to be deleted,
	  ;; so we first mark each of the nodes being deleted, and then
	  ;; only do the deletion if the parent is not marked.
	  (do ((node older (node-older node)))
	      ((not node))
	    (set-node-nb! node false))
	  (do ((node older (node-older node)))
	      ((not node))
	    (let ((parent (node-parent node)))
	      (cond ((not parent)
		     (vector-set! root-nodes (node-byte node) false))
		    ((node-nb parent)
		     (delete-child parent node))))
	    (set-node-nb! node true))
	  unspecific))))

(define (delete-child parent child)
  (let ((previous (node-previous child))
	(next (node-next child)))
    (if next
	(set-node-previous! next previous))
    (if previous
	(set-node-next! previous next)
	(set-node-children! parent next)))
  (let ((child (node-children parent)))
    ;; If only one child remains, splice out PARENT.
    (if (not (node-next child))
	(begin
	  (replace-child parent child)
	  (let ((older (node-older parent))
		(newer (node-newer parent)))
	    (if older
		(set-node-newer! older newer))
	    (if newer
		(set-node-older! newer older))
	    (if (eq? parent oldest-node)
		(set! oldest-node newer))
	    (if (eq? parent newest-node)
		(set! newest-node older))
	    unspecific)))))

;;;; The Byte Buffer

;;; Maximum number of bytes that the byte buffer can hold.
;;; The optimal size for this buffer is
;;;  (+ (* COPY-MAX POINTER-MAX) BUFFER-READ)
(define-integrable buffer-size 69632)
(define-integrable buffer-size-optimal? true)

;;; When input is needed from the input port, we attempt to read this
;;; many bytes all at once.  It is assumed that BUFFER-SIZE is an
;;; integral multiple of this number.
(define-integrable buffer-read 4096)

(define compress-continuation)
(define byte-buffer)

(define-structure (bb (constructor make-byte-buffer ()))
  (vector (make-string buffer-size) read-only true)
  (ptr 0)
  (end 0)
  (eof? false))

(define (byte-ready?)
  (let ((bb byte-buffer))
    (if (fix:= (bb-ptr bb) (bb-end bb))
	(guarantee-buffer-data bb true)
	true)))

(define (read-byte)
  ;; Get a byte from the byte buffer.  If we are reading bytes in the
  ;; process of generating a copy command, NODE is the current
  ;; position in the copy, otherwise it is #F.  If we encounter EOF
  ;; while reading this byte, NODE is used to emit the final command.
  (let ((bb byte-buffer))
    (let ((byte (%peek-byte bb)))
      (%discard-byte bb)
      byte)))

(define (peek-byte)
  (%peek-byte byte-buffer))

(define (discard-byte)
  (%discard-byte byte-buffer))

(declare (integrate-operator %peek-byte %discard-byte))

(define (%peek-byte bb)
  (if (fix:= (bb-ptr bb) (bb-end bb))
      (guarantee-buffer-data bb false))
  (vector-8b-ref (bb-vector bb) (bb-ptr bb)))

(define (%discard-byte bb)
  (set-bb-ptr! bb
	       (if (fix:= (bb-ptr bb) (fix:- buffer-size 1))
		   0
		   (fix:+ (bb-ptr bb) 1))))

(define (unread-byte)
  (let ((bb byte-buffer))
    (set-bb-ptr! bb
		 (if (fix:= (bb-ptr bb) 0)
		     (fix:- buffer-size 1)
		     (fix:- (bb-ptr bb) 1)))))

(define (unread-bytes nb)
  (let ((bb byte-buffer))
    (set-bb-ptr! bb
		 (let ((ptr (fix:- (bb-ptr bb) nb)))
		   (if (fix:< ptr 0)
		       (fix:+ ptr buffer-size)
		       ptr)))))

(define (node-ref node nb)
  ;; Read byte NB in the string for NODE.
  (vector-8b-ref (bb-vector byte-buffer)
		 (let ((bp (fix:+ (node-bp node) nb)))
		   (if (fix:< bp buffer-size)
		       bp
		       (fix:- bp buffer-size)))))

(define (guarantee-buffer-data bb probe?)
  ;; We have read all of the bytes in the buffer, so it's time to get
  ;; some more.  If PROBE? is false and we're at EOF, do a non-local
  ;; exit to finish the compression.  If the last read was short, that
  ;; means we are now at EOF.
  (if (bb-eof? bb)
      (if probe?
	  false
	  (compress-finished))
      (let* ((end (bb-end bb))
	     (end* (fix:+ end buffer-read)))
	;; Calls to GUARANTEE-BUFFER-SPACE make sure that this read will
	;; not overwrite any data that we are still using.  Otherwise, we
	;; might have to invalidate some nodes here, and that would
	;; consequently make the program more complicated because we
	;; couldn't be sure that any nodes we were holding were valid
	;; across a call to READ-BYTE.
	(let ((nb
	       (input-port/read-substring! input-port
					   (bb-vector bb) end end*)))
	  (cond ((not nb)
		 (error "Input port must be in blocking mode:" input-port)
		 false)
		((fix:= nb buffer-read)
		 ;; A full block was read.
		 (set-bb-end! bb (if (fix:= end* buffer-size) 0 end*))
		 true)
		((fix:= nb 0)
		 ;; We're at EOF.
		 (if probe?
		     (begin
		       (set-bb-eof?! bb true)
		       false)
		     (compress-finished)))
		((and (fix:< 0 nb) (fix:< nb buffer-read))
		 ;; A partial block was read, meaning that
		 ;; this is the last block.  Set BB-EOF? to
		 ;; indicate that there is no more data after
		 ;; this block is exhausted.
		 (set-bb-eof?! bb true)
		 (set-bb-end! bb (fix:+ end nb))
		 true)
		(else
		 (error "Illegal result from read:" nb buffer-read)
		 false))))))

(define (compress-finished)
  ;; This is called from GUARANTEE-BUFFER-DATA when EOF is
  ;; encountered.  If any data remains in the buffer which has not yet
  ;; been emitted as a literal or copy, it is emitted as a literal.
  (let ((bp command-bp)
	(ptr (bb-ptr byte-buffer)))
    (if (not (fix:= ptr bp))
	(let loop
	    ((nb (fix:- (if (fix:< bp ptr) ptr (fix:+ ptr buffer-size)) bp)))
	  (if (fix:<= nb literal-max)
	      (write-literal nb)
	      (begin
		(write-literal literal-max)
		(loop (fix:- nb literal-max)))))))
  (compress-continuation unspecific))

(define (guarantee-buffer-space nb)
  ;; Make sure that the byte buffer has enough space to hold NB bytes.
  ;; If necessary, invalidate old commands until this is true.  If the
  ;; buffer size is optimal, this is never necessary, because the
  ;; buffer is big enough to hold all of the commands in the window.
  (if (and (not buffer-size-optimal?)
	   oldest-node)
      (let ((end (bb-end byte-buffer)))
	(if (fix:< (let ((bp command-bp))
		   (fix:- (if (fix:<= bp end)
			    end
			    (fix:+ end buffer-size))
			bp))
		 nb)
	    (let ((start (node-bp oldest-node))
		  (nb (if (fix:< buffer-read nb) nb buffer-read)))
	      (if (fix:< (fix:- (if (fix:< end start)
				start
				(fix:+ start buffer-size))
			    end)
		       nb)
		  (let ((node
			 (let ((end
				(let ((end (fix:+ end nb)))
				  (if (fix:< end buffer-size)
				      end
				      (fix:- end buffer-size)))))
			   (if (fix:< start end)
			       (do ((node oldest-node (node-newer node)))
				   ((not
				     (let ((bp (node-bp node)))
				       (and (fix:<= start bp)
					    (fix:< bp end))))
				    node))
			       (do ((node oldest-node (node-newer node)))
				   ((not
				     (let ((bp (node-bp node)))
				       (or (and (fix:<= start bp)
						(fix:< bp buffer-size))
					   (fix:< bp end))))
				    node))))))
		    (set-oldest-node node
				     (node-pointer (node-older node))))))))))

;;;; The Encoder
;;;  This is the B1 encoder of Fiala and Greene.

;;; Maximum length of a literal.
(define-integrable literal-max 16)

;;; Maximum length of a copy.
(define-integrable copy-max 16)

;;; Maximum displacement of a copy, in "pointers".  Consequently, this
;;; is the size of the compression window in pointers.
(define-integrable pointer-max 4096)

;;; Current "pointer" in input stream.  The pointer is updated at each
;;; literal character and copy command.
(define current-pointer)

;;; Starting position of current command in byte buffer.
(define current-bp)
(define command-bp)

(define (write-literal nb)
  ;; Output a literal command of length NB, which is greater than zero
  ;; and at most LITERAL-MAX.
  (write-byte (fix:- nb 1))
  (let ((string (bb-vector byte-buffer))
	(start command-bp))
    (let ((end (fix:+ start nb)))
      (if (fix:<= end buffer-size)
	  (begin
	    (write-bytes string start end)
	    (set! command-bp (if (fix:= end buffer-size) 0 end)))
	  (let ((end (fix:- end buffer-size)))
	    (write-bytes string start buffer-size)
	    (write-bytes string 0 end)
	    (set! command-bp end)))))
  unspecific)

(define (write-copy nb pointer copy-pointer)
  ;; Output a copy command of length NB, which is greater than one
  ;; and at most COPY-MAX.  POINTER is the pointer of the text being
  ;; copied, while COPY-POINTER is the pointer of the copy command
  ;; being emitted.
  (let ((length (fix:* (fix:- nb 1) 16))
	(displacement
	 (fix:- (if (fix:<= pointer copy-pointer)
		    copy-pointer
		    (fix:+ copy-pointer pointer-max))
		pointer)))
    (if (fix:< displacement 256)
	(begin
	  (write-byte length)
	  (write-byte displacement))
	(begin
	  (write-byte (fix:+ length (fix:quotient displacement 256)))
	  (write-byte (fix:remainder displacement 256)))))
  (let ((bp
	 (let ((bp (fix:+ current-bp nb)))
	   (if (fix:< bp buffer-size)
	       bp
	       (fix:- bp buffer-size)))))
    (set! current-bp bp)
    (set! command-bp bp))
  unspecific)

(define (increment-bp)
  (set! current-bp
	(let ((bp (fix:+ current-bp 1)))
	  (if (fix:= bp buffer-size)
	      0
	      bp)))
  unspecific)

(define (increment-current-pointer)
  (let ((pointer
	 (let ((pointer (fix:+ current-pointer 1)))
	   (if (fix:= pointer pointer-max)
	       (begin
		 (set! window-filled? true)
		 0)
	       pointer))))
    (set! current-pointer pointer)
    ;; Invalidate any nodes that refer to the previous command with
    ;; number POINTER.  If WINDOW-FILLED? is false, we haven't yet
    ;; generated enough commands for such nodes to exist.
    (if window-filled?
	(set-oldest-node oldest-node pointer))))

(define output-buffer)

(define (make-output-buffer)
  (cons 0 (make-string 4096)))

(define (write-byte byte)
  (let ((ob output-buffer))
    (let ((index (car ob)))
      (vector-8b-set! (cdr ob) index byte)
      (if (fix:= index 4095)
	  (begin
	    (output-port/write-string output-port (cdr ob))
	    (set-car! ob 0))
	  (set-car! ob (fix:+ index 1))))))

(define (write-bytes string start end)
  (let ((ob output-buffer))
    (let ((index (car ob)))
      (let ((new-index (fix:+ index (fix:- end start))))
	(if (fix:< new-index 4096)
	    (begin
	      (let ((buffer (cdr ob)))
		(do ((start start (fix:+ start 1))
		     (index index (fix:+ index 1)))
		    ((fix:= start end))
		  (vector-8b-set! buffer index (vector-8b-ref string start))))
	      (set-car! ob new-index))
	    (do ((start start (fix:+ start 1)))
		((fix:= start end))
	      (write-byte (vector-8b-ref string start))))))))

(define (flush-output-buffer)
  (let ((ob output-buffer))
    (if (fix:< 0 (car ob))
	(output-port/write-substring output-port (cdr ob) 0 (car ob))))
  (output-port/flush-output output-port))

(define (uncompress ifile ofile)
  (uncompress-internal ifile ofile
    (lambda (message . irritants)
      (error message irritants))))