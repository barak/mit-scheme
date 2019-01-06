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

;;;; Data Compressor

(declare (usual-integrations))

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

(define (compress ifile ofile)
  (call-with-binary-input-file (merge-pathnames ifile)
    (lambda (input)
      (call-with-binary-output-file (merge-pathnames ofile)
        (lambda (output)
	  (write-compressed-file-marker output)
	  (compress-ports input output))))))

(define-structure (compression-state
		   (conc-name #f)
		   (constructor make-compression-state
				(root-nodes byte-buffer output-buffer
					    input-port output-port)))
  root-nodes
  (oldest-node #f)
  (newest-node #f)
  (window-filled? #f)
  (compress-continuation unspecific)
  byte-buffer

  ;; Current "pointer" in input stream.  The pointer is updated at each
  ;; literal character and copy command.
  (current-pointer 0)

  ;; Starting position of current command in byte buffer.
  (current-bp 0)
  (command-bp 0)

  output-buffer
  input-port
  output-port)

(define (compress-ports input output)
  (let ((state
	 (make-compression-state (make-vector 256 #f)
				 (make-byte-buffer)
				 (make-output-buffer)
				 input output)))
    (call-with-current-continuation
     (lambda (continuation)
       (set-compress-continuation! state continuation)
       (idle state)))
    (flush-output-buffer state)))

(define (idle state)
  ;; This is the top of the compression loop.  We've just emitted a
  ;; command.  If the next two bytes can be matched against some text
  ;; in the window, start a copy command, otherwise start a literal.
  (guarantee-buffer-space state 2)
  (let ((node (match-first state)))
    (if (not node)
	(generate-literal state)
	(let ((node (match-next state node 1)))
	  (if (not node)
	      (generate-literal state)
	      (generate-copy state node 2))))))

(define (generate-literal state)
  (guarantee-buffer-space state (fix:+ literal-max 2))
  (letrec
      ((loop
	(lambda (nb)
	  (let ((node (match-first state)))
	    (if (not node)
		(continue nb)
		(let ((node (match-next state node 1)))
		  (if (not node)
		      (continue nb)
		      (let ((node (match-next state node 2)))
			(if (not node)
			    (begin
			      (unread-byte state)
			      (continue nb))
			    (let ((nb*
				   (let ((cbp (current-bp state))
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
				    (unread-bytes state 2)
				    (continue nb))
				  (begin
				    (write-literal state nb)
				    (generate-copy state node 3))))))))))))
       (continue
	(lambda (nb)
	  (increment-current-pointer state)
	  (increment-bp state)
	  (let ((nb (fix:+ nb 1)))
	    (if (fix:< nb literal-max)
		(loop nb)
		(begin
		  (write-literal state nb)
		  (idle state)))))))
    (increment-current-pointer state)
    (increment-bp state)
    (loop 1)))

(define (generate-copy state node nb)
  (guarantee-buffer-space state copy-max)
  (let ((copy-pointer (current-pointer state)))
    (let ((finish
	   (lambda (nb pointer bp)
	     (let ((nb*
		    (fix:- (let ((bp* (command-bp state)))
			     (if (fix:< bp* bp)
				 (fix:+ bp* buffer-size)
				 bp*))
			   bp))
		   (do-copy
		    (lambda (nb)
		      (write-copy state nb pointer copy-pointer)
		      (increment-current-pointer state)
		      (idle state))))
	       ;; NB is the number of bytes that we want to write a
	       ;; copy command for; NB* is the number of bytes between
	       ;; the start of the copy and the current position.  If
	       ;; NB* is less than NB, we must truncate the copy in
	       ;; order to prevent it from copying from itself.  If
	       ;; NB* is 1, then don't copy -- it's too short.
	       (if (fix:<= nb nb*)
		   (do-copy nb)
		   (begin
		     (unread-bytes state (fix:- nb nb*))
		     (if (fix:= nb* 1)
			 (generate-literal state)
			 (do-copy nb*))))))))
      (let loop ((node node) (nb nb))
	(let ((pointer (node-pointer node))
	      (bp (node-bp node)))
	  (if (not (byte-ready? state))
	      (finish nb pointer bp)
	      (let ((node* (match-next state node nb)))
		(if (not node*)
		    (finish nb pointer bp)
		    (let ((nb (fix:+ nb 1)))
		      (if (fix:< nb copy-max)
			  (loop node* nb)
			  (if (eq? node node*)
			      (finish nb pointer bp)
			      (let ((pointer (node-pointer node*))
				    (bp (node-bp node*)))
				(update-node-pointer state node*)
				(finish nb pointer bp)))))))))))))

(define (match-first state)
  (let ((byte (read-byte state)))
    (let ((node (vector-ref (root-nodes state) byte)))
      (if (not node)
	  (add-child state #f byte (make-node state 0)))
      node)))

(define (match-next state node nb)
  (let ((byte (peek-byte state)))
    (if (fix:= (node-nb node) nb)
	(begin
	  (update-node-pointer state node)
	  (let loop ((child (node-children node)))
	    (cond ((not child)
		   (add-child state node byte (make-node state 0))
		   #f)
		  ((fix:= byte (node-byte child))
		   (discard-byte state)
		   child)
		  (else
		   (loop (node-next child))))))
	(let ((byte* (node-ref node nb state)))
	  (if (fix:= byte byte*)
	      (begin
		(discard-byte state)
		node)
	      (begin
		(let ((parent (make-node state nb)))
		  (replace-child state node parent)
		  (add-child state parent byte* node)
		  (add-child state parent byte (make-node state 0)))
		#f))))))

;;;; PATRICIA Tree Database

(define-structure (node (constructor %make-node (nb older pointer bp)))
  ;; The parent of this node, or #F for a root node.
  (parent #f)

  ;; The children of this node.  Either #F for no children, or the
  ;; first child.  The remaining children are accessed through the
  ;; NODE-NEXT fields.  A node will never have exactly one child.
  (children #f)

  ;; The adjacent siblings of this node, or #F if none.
  (previous #f)
  (next #f)

  ;; The first byte of the substring between the parent and this node.
  (byte #f)

  ;; The number of bytes in the string represented by this node,
  ;; counting down from the root of the tree.
  (nb 0)

  ;; The adjacent nodes in the node pointer ordering.  The OLDER node
  ;; has less recent POINTER and BP, while the newer node has more recent.
  (older #f)
  (newer #f)

  ;; The command pointer for this node.
  pointer

  ;; The byte pointer for this node.
  bp)

(define (make-node state nb)
  (let ((node (%make-node nb (newest-node state)
			  (current-pointer state) (current-bp state))))
    (if (newest-node state)
	(set-node-newer! (newest-node state) node)
	(set-oldest-node! state node))
    (set-newest-node! state node)
    node))

(define (update-node-pointer state node)
  (set-node-pointer! node (current-pointer state))
  (set-node-bp! node (current-bp state))
  (let ((older (node-older node))
	(newer (node-newer node)))
    (if newer
	(begin
	  (set-node-older! newer older)
	  (if older
	      (set-node-newer! older newer)
	      (set-oldest-node! state newer))
	  (set-node-newer! node #f)
	  (set-node-older! node (newest-node state))
	  (set-node-newer! (newest-node state) node)
	  (set-newest-node! state node)
	  unspecific))))

(define (add-child state parent byte child)
  (set-node-parent! child parent)
  (set-node-byte! child byte)
  (if parent
      (let ((sibling (node-children parent)))
	(set-node-next! child sibling)
	(if sibling (set-node-previous! sibling child))
	(set-node-children! parent child))
      (vector-set! (root-nodes state) byte child)))

(define (replace-child state child child*)
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
	(vector-set! (root-nodes state) byte child*))))

(define (set-oldest state node pointer)
  (let ((node
	 (do ((node node (node-newer node)))
	     ((not (fix:= (node-pointer node) pointer)) node))))
    (if (not (eq? node oldest-node))
	(let ((older (node-older node)))
	  (set-node-older! node #f)
	  (set-oldest-node! state node)
	  ;; We don't have to do anything complicated to delete a node.
	  ;; If the node has any children, we know that they are also
	  ;; being deleted, because a descendant cannot be newer than
	  ;; its ancestor.  However, we want to avoid deleting a child
	  ;; from its parent if the parent is also going to be deleted,
	  ;; so we first mark each of the nodes being deleted, and then
	  ;; only do the deletion if the parent is not marked.
	  (do ((node older (node-older node)))
	      ((not node))
	    (set-node-nb! node #f))
	  (do ((node older (node-older node)))
	      ((not node))
	    (let ((parent (node-parent node)))
	      (cond ((not parent)
		     (vector-set! (root-nodes state) (node-byte node) #f))
		    ((node-nb parent)
		     (delete-child state parent node))))
	    (set-node-nb! node #t))
	  unspecific))))

(define (delete-child state parent child)
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
	  (replace-child state parent child)
	  (let ((older (node-older parent))
		(newer (node-newer parent)))
	    (if older
		(set-node-newer! older newer))
	    (if newer
		(set-node-older! newer older))
	    (if (eq? parent (oldest-node state))
		(set-oldest-node! state newer))
	    (if (eq? parent (newest-node state))
		(set-newest-node! state older))
	    unspecific)))))

;;;; The Byte Buffer

;;; Maximum number of bytes that the byte buffer can hold.
;;; The optimal size for this buffer is
;;;  (+ (* COPY-MAX POINTER-MAX) BUFFER-READ)
(define-integrable buffer-size 69632)
(define-integrable buffer-size-optimal? #t)

;;; When input is needed from the input port, we attempt to read this
;;; many bytes all at once.  It is assumed that BUFFER-SIZE is an
;;; integral multiple of this number.
(define-integrable buffer-read 4096)

(define-structure (bb (constructor make-byte-buffer ()))
  (vector (make-bytevector buffer-size) read-only #t)
  (ptr 0)
  (end 0)
  (eof? #f))

(define (byte-ready? state)
  (let ((bb (byte-buffer state)))
    (if (fix:= (bb-ptr bb) (bb-end bb))
	(guarantee-buffer-data state bb #t)
	#t)))

(define (read-byte state)
  ;; Get a byte from the byte buffer.  If we are reading bytes in the
  ;; process of generating a copy command, NODE is the current
  ;; position in the copy, otherwise it is #F.  If we encounter EOF
  ;; while reading this byte, NODE is used to emit the final command.
  (let ((bb (byte-buffer state)))
    (let ((byte (%peek-byte state bb)))
      (%discard-byte bb)
      byte)))

(define (peek-byte state)
  (%peek-byte state (byte-buffer state)))

(define (discard-byte state)
  (%discard-byte (byte-buffer state)))

(declare (integrate-operator %peek-byte %discard-byte))

(define (%peek-byte state bb)
  (if (fix:= (bb-ptr bb) (bb-end bb))
      (guarantee-buffer-data state bb #f))
  (bytevector-u8-ref (bb-vector bb) (bb-ptr bb)))

(define (%discard-byte bb)
  (set-bb-ptr! bb
	       (if (fix:= (bb-ptr bb) (fix:- buffer-size 1))
		   0
		   (fix:+ (bb-ptr bb) 1))))

(define (unread-byte state)
  (let ((bb (byte-buffer state)))
    (set-bb-ptr! bb
		 (if (fix:= (bb-ptr bb) 0)
		     (fix:- buffer-size 1)
		     (fix:- (bb-ptr bb) 1)))))

(define (unread-bytes state nb)
  (let ((bb (byte-buffer state)))
    (set-bb-ptr! bb
		 (let ((ptr (fix:- (bb-ptr bb) nb)))
		   (if (fix:< ptr 0)
		       (fix:+ ptr buffer-size)
		       ptr)))))

(define (node-ref node nb state)
  ;; Read byte NB in the string for NODE.
  (bytevector-u8-ref (bb-vector (byte-buffer state))
		     (let ((bp (fix:+ (node-bp node) nb)))
		       (if (fix:< bp buffer-size)
			   bp
			   (fix:- bp buffer-size)))))

(define (guarantee-buffer-data state bb probe?)
  ;; We have read all of the bytes in the buffer, so it's time to get
  ;; some more.  If PROBE? is false and we're at EOF, do a non-local
  ;; exit to finish the compression.  If the last read was short, that
  ;; means we are now at EOF.
  (if (bb-eof? bb)
      (if probe?
	  #f
	  (compress-finished state))
      (let* ((end (bb-end bb))
	     (end* (fix:+ end buffer-read)))
	;; Calls to GUARANTEE-BUFFER-SPACE make sure that this read will
	;; not overwrite any data that we are still using.  Otherwise, we
	;; might have to invalidate some nodes here, and that would
	;; consequently make the program more complicated because we
	;; couldn't be sure that any nodes we were holding were valid
	;; across a call to READ-BYTE.
	(let ((nb
	       (read-bytevector! (bb-vector bb) (input-port state) end end*)))
	  (cond ((not nb)
		 (error "Input port must be in blocking mode:"
			(input-port state))
		 #f)
		((eof-object? nb)
		 (if probe?
		     (begin
		       (set-bb-eof?! bb #t)
		       #f)
		     (compress-finished state)))
		((fix:= nb buffer-read)
		 ;; A full block was read.
		 (set-bb-end! bb (if (fix:= end* buffer-size) 0 end*))
		 #t)
		((and (fix:< 0 nb) (fix:< nb buffer-read))
		 ;; A partial block was read, meaning that
		 ;; this is the last block.  Set BB-EOF? to
		 ;; indicate that there is no more data after
		 ;; this block is exhausted.
		 (set-bb-eof?! bb #t)
		 (set-bb-end! bb (fix:+ end nb))
		 #t)
		(else
		 (error "Illegal result from read:" nb buffer-read)
		 #f))))))

(define (compress-finished state)
  ;; This is called from GUARANTEE-BUFFER-DATA when EOF is
  ;; encountered.  If any data remains in the buffer which has not yet
  ;; been emitted as a literal or copy, it is emitted as a literal.
  (let ((bp (command-bp state))
	(ptr (bb-ptr (byte-buffer state))))
    (if (not (fix:= ptr bp))
	(let loop
	    ((nb (fix:- (if (fix:< bp ptr) ptr (fix:+ ptr buffer-size)) bp)))
	  (if (fix:<= nb literal-max)
	      (write-literal state nb)
	      (begin
		(write-literal state literal-max)
		(loop (fix:- nb literal-max)))))))
  ((compress-continuation state) unspecific))

(define (guarantee-buffer-space state nb)
  ;; Make sure that the byte buffer has enough space to hold NB bytes.
  ;; If necessary, invalidate old commands until this is true.  If the
  ;; buffer size is optimal, this is never necessary, because the
  ;; buffer is big enough to hold all of the commands in the window.
  (declare (ignore state nb))
  (if (and (not buffer-size-optimal?)
	   oldest)
      (let ((end (bb-end (byte-buffer state)))
	    (oldest (oldest-node state)))
	(if (fix:< (let ((bp (command-bp state)))
		     (fix:- (if (fix:<= bp end)
				end
				(fix:+ end buffer-size))
			    bp))
		   nb)
	    (let ((start (node-bp oldest))
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
			       (do ((node oldest (node-newer node)))
				   ((not
				     (let ((bp (node-bp node)))
				       (and (fix:<= start bp)
					    (fix:< bp end))))
				    node))
			       (do ((node oldest (node-newer node)))
				   ((not
				     (let ((bp (node-bp node)))
				       (or (and (fix:<= start bp)
						(fix:< bp buffer-size))
					   (fix:< bp end))))
				    node))))))
		    (set-oldest state node
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

(define (write-literal state nb)
  ;; Output a literal command of length NB, which is greater than zero
  ;; and at most LITERAL-MAX.
  (write-byte state (fix:- nb 1))
  (let ((string (bb-vector (byte-buffer state)))
	(start (command-bp state)))
    (let ((end (fix:+ start nb)))
      (if (fix:<= end buffer-size)
	  (begin
	    (write-bytes state string start end)
	    (set-command-bp! state (if (fix:= end buffer-size) 0 end)))
	  (let ((end (fix:- end buffer-size)))
	    (write-bytes state string start buffer-size)
	    (write-bytes state string 0 end)
	    (set-command-bp! state end)))))
  unspecific)

(define (write-copy state nb pointer copy-pointer)
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
	  (write-byte state length)
	  (write-byte state displacement))
	(begin
	  (write-byte state (fix:+ length (fix:quotient displacement 256)))
	  (write-byte state (fix:remainder displacement 256)))))
  (let ((bp
	 (let ((bp (fix:+ (current-bp state) nb)))
	   (if (fix:< bp buffer-size)
	       bp
	       (fix:- bp buffer-size)))))
    (set-current-bp! state bp)
    (set-command-bp! state bp))
  unspecific)

(define (increment-bp state)
  (set-current-bp! state
		   (let ((bp (fix:+ (current-bp state) 1)))
		     (if (fix:= bp buffer-size)
			 0
			 bp)))
  unspecific)

(define (increment-current-pointer state)
  (let ((pointer
	 (let ((pointer (fix:+ (current-pointer state) 1)))
	   (if (fix:= pointer pointer-max)
	       (begin
		 (set-window-filled?! state #t)
		 0)
	       pointer))))
    (set-current-pointer! state pointer)
    ;; Invalidate any nodes that refer to the previous command with
    ;; number POINTER.  If WINDOW-FILLED? is false, we haven't yet
    ;; generated enough commands for such nodes to exist.
    (if (window-filled? state)
	(set-oldest state (oldest-node state) pointer))))

(define (make-output-buffer)
  (cons 0 (make-bytevector 4096)))

(define (write-byte state byte)
  (let ((ob (output-buffer state)))
    (let ((index (car ob)))
      (bytevector-u8-set! (cdr ob) index byte)
      (if (fix:= index 4095)
	  (begin
	    (write-bytevector (cdr ob) (output-port state))
	    (set-car! ob 0))
	  (set-car! ob (fix:+ index 1))))))

(define (write-bytes state string start end)
  (let ((ob (output-buffer state)))
    (let ((index (car ob)))
      (let ((new-index (fix:+ index (fix:- end start))))
	(if (fix:< new-index 4096)
	    (begin
	      (let ((buffer (cdr ob)))
		(do ((start start (fix:+ start 1))
		     (index index (fix:+ index 1)))
		    ((fix:= start end))
		  (bytevector-u8-set! buffer index
				      (bytevector-u8-ref string start))))
	      (set-car! ob new-index))
	    (do ((start start (fix:+ start 1)))
		((fix:= start end))
	      (write-byte state (bytevector-u8-ref string start))))))))

(define (flush-output-buffer state)
  (let ((ob (output-buffer state))
	(op (output-port state)))
    (if (fix:< 0 (car ob))
	(write-bytevector (cdr ob) op 0 (car ob)))
    (flush-output-port op)))

(define (uncompress ifile ofile)
  (uncompress-internal ifile ofile
    (lambda (message . irritants)
      (error message irritants))))