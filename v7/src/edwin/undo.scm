;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/undo.scm,v 1.45 1989/04/28 22:54:12 cph Rel $
;;;
;;;	Copyright (c) 1985, 1989 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Undo, translated from the GNU Emacs implementation in C.

(declare (usual-integrations))

;;;; Basic Record Keeping

(define-integrable initial-undo-records 8)
(define-integrable initial-undo-chars 128)
(define-integrable maximum-undo-records 512)
(define-integrable maximum-undo-chars 8192)

(define-structure (undo-data)
  records				; vector of records
  next-record				; position in vector
  chars					; string of characters
  next-char				; position in string
  )

(define-structure (undo-record
		   (type vector)
		   (constructor %make-undo-record ()))
  (type false)
  (start false)
  (length false))

(define-integrable (undo-records-ref records index)
  (or (vector-ref records index)
      (let ((new-record (%make-undo-record)))
	(vector-set! records index new-record)
	new-record)))

(define last-undo-group false)
(define last-undo-record false)

(define (enable-group-undo! group)
  (without-interrupts
   (lambda ()
     (set-group-undo-data!
      group
      (make-undo-data (let ((records (make-vector initial-undo-records false)))
			(mark-not-undoable!
			 (let ((max-index (-1+ initial-undo-records)))
			   (undo-records-ref records max-index)))
			records)
		      0
		      (string-allocate initial-undo-chars)
		      0)))))

(define (new-undo! undo-data type group start length)
  (let ((records (undo-data-records undo-data))
	(index (undo-data-next-record undo-data)))
    (let ((undo-record (undo-records-ref records index)))
      (set-undo-record-type! undo-record type)
      (set-undo-record-start! undo-record start)
      (set-undo-record-length! undo-record length)
      (set! last-undo-record undo-record))
    (let ((next (1+ index)))
      (cond ((< next (vector-length records))
	     (mark-not-undoable! (undo-records-ref records next))
	     (set-undo-data-next-record! undo-data next))
	    ((>= next maximum-undo-records)
	     (mark-not-undoable! (vector-ref records 0))
	     (set-undo-data-next-record! undo-data 0))
	    (else
	     (let ((new-records (make-vector maximum-undo-records false))
		   (length (vector-length records))
		   (new-record (%make-undo-record))
		   (max-record (%make-undo-record)))
	       (subvector-move-right! records 0 length new-records 0)
	       (mark-not-undoable! new-record)
	       (mark-not-undoable! max-record)
	       (vector-set! new-records length new-record)
	       (vector-set! new-records (-1+ maximum-undo-records) max-record)
	       (set-undo-data-records! undo-data new-records)
	       (set-undo-data-next-record! undo-data next))))))
  (set! last-undo-group group)
  (if (not (eq? 'BOUNDARY type))
      (set! last-undone-record -1)))

(define-integrable (mark-not-undoable! record)
  (set-undo-record-type! record 'NOT-UNDOABLE))

(define (undo-store-substring! undo-data string start end)
  (let loop ((start start))
    (let ((chars (undo-data-chars undo-data))
	  (i (undo-data-next-char undo-data)))
      (let ((room (- (string-length chars) i))
	    (needed (- end start)))
	(cond ((> room needed)
	       (substring-move-right! string start end chars i)
	       (set-undo-data-next-char! undo-data (+ i needed))
	       (set! number-chars-left (- number-chars-left needed)))
	      ((= room needed)
	       (substring-move-right! string start end chars i)
	       (set-undo-data-next-char! undo-data 0)
	       (set! number-chars-left (- number-chars-left needed)))
	      ((< (string-length chars) maximum-undo-chars)
	       (let ((new-chars (string-allocate maximum-undo-chars)))
		 (substring-move-right! chars 0 i new-chars 0)
		 (set-undo-data-chars! undo-data new-chars))
	       (set! number-chars-left
		     (+ (- maximum-undo-chars (string-length chars))
			number-chars-left))
	       (loop start))
	      (else
	       (let ((new-start (+ start room)))
		 (substring-move-right! string start new-start chars i)
		 (set-undo-data-next-char! undo-data 0)
		 (set! number-chars-left (- number-chars-left room))
		 (loop new-start))))))))

;;;; External Recording Hooks

;;; These assume that they are called before the regular recording
;;; daemons, for the following reason: to check the old status of the
;;; GROUP-MODIFIED? flag before the buffer daemon updates it.

(define (undo-record-insertion! group start end)
  (let ((undo-data (group-undo-data group)))
    (if undo-data
	(begin
	  (if (not (eq? group last-undo-group))
	      (begin
		(undo-mark-previous! undo-data
				     'BOUNDARY
				     group
				     (mark-index (group-point group)))
		(set! last-undo-record false)))
	  (if (not (group-modified? group))
	      (new-undo! undo-data 'UNMODIFY group start 0))
	  (let ((last last-undo-record)
		(length (- end start)))
	    (if (and last
		     (eq? 'DELETE (undo-record-type last))
		     (= start
			(+ (undo-record-start last)
			   (undo-record-length last))))
		(set-undo-record-length! last
					 (+ length (undo-record-length last)))
		(new-undo! undo-data 'DELETE group start length)))))))

(define (undo-record-deletion! group start end)
  (let ((undo-data (group-undo-data group)))
    (if undo-data
	(begin
	  (if (not (eq? group last-undo-group))
	      (begin
		(undo-mark-previous! undo-data
				     'BOUNDARY
				     group
				     (mark-index (group-point group)))
		(set! last-undo-record false)))
	  (if (not (group-modified? group))
	      (new-undo! undo-data 'UNMODIFY group start 0))
	  (let ((last last-undo-record)
		(length (- end start)))
	    (if (and last
		     (eq? 'INSERT (undo-record-type last))
		     (= start (undo-record-start last)))
		(set-undo-record-length! last
					 (+ length (undo-record-length last)))
		(new-undo! undo-data 'INSERT group start length)))
	  (let ((text (group-text group))
		(gap-start (group-gap-start group))
		(length (group-gap-length group)))
	    (cond ((<= end gap-start)
		   (undo-store-substring! undo-data text start end))
		  ((>= start gap-start)
		   (undo-store-substring! undo-data
					  text
					  (+ start length)
					  (+ end length)))
		  (else
		   (undo-store-substring! undo-data text start gap-start)
		   (undo-store-substring! undo-data
					  text
					  (group-gap-end group)
					  (+ end length)))))))))

(define (undo-boundary! point)
  (without-interrupts
   (lambda ()
     (let ((group (mark-group point)))
       (let ((undo-data (group-undo-data group)))
	 (if undo-data
	     (undo-mark-previous! undo-data
				  'BOUNDARY
				  group
				  (mark-index point))))))))

(define (undo-done! point)
  (without-interrupts
   (lambda ()
     (let ((group (mark-group point)))
       (let ((undo-data (group-undo-data group)))
	 (if undo-data
	     (undo-mark-previous! undo-data
				  'NOT-UNDOABLE
				  group
				  (mark-index point))))))))

(define-integrable (undo-mark-previous! undo-data type group start)
  (let ((records (undo-data-records undo-data)))
    (let ((index
	   (let ((next (undo-data-next-record undo-data)))
	     (-1+ (if (zero? next)
		      (vector-length records)
		      next)))))
      (let ((record (vector-ref records index)))
	(if record
	    (if (not (eq? type (undo-record-type record)))
		(new-undo! undo-data type group start 0))
	    (begin
	      (vector-set! records index (%make-undo-record))
	      (new-undo! undo-data type group start 0)))))))

;;;; Undo Command

;;; This is used to determine if we have switched buffers since the
;;; last Undo command.  Actually, this may be an artifact of RMS'
;;; implementation since there should not be any way to switch buffers
;;; between two Undo commands in this editor.
(define last-undone-buffer)

;;; These keep track of the state of the Undo command, so that
;;; subsequent invocations know where to start from.
(define last-undone-record)
(define last-undone-char)

;;; This counts the total number of records that have been undone, so
;;; that it can be compared to the total number of records, to
;;; determine if we have run out of records.
(define number-records-undone)

;;; This says how many chars of undo are left.  It is initialized by
;;; the Undo command to the length of the chars string, and used, like
;;; NUMBER-RECORDS-UNDONE, to determine if we have run out of undo
;;; data.  This, however, is kept up to date by NEW-UNDO because there
;;; is no NOT-UNDOABLE boundary in the chars array to tell us where
;;; the chars end.
(define number-chars-left 0)

;;; Some error messages:

(define cant-undo-more
  "Cannot undo more: changes have been made since the last undo")

(define no-more-undo
  "No further undo information available")

(define outside-visible-range
  "Changes to be undone are outside the visible portion of buffer")

(define undo-command-tag "Undo")

(define-command undo
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  "p"
  (lambda (argument)
    (if (positive? argument)
	(let ((buffer (current-buffer)))
	  (let ((undo-data (group-undo-data (buffer-group buffer))))
	    (if (not undo-data)
		(editor-error "Undo information not kept for this buffer"))
	    (without-interrupts
	     (lambda ()
	       (command-message-receive undo-command-tag
		 (lambda ()
		   (if (or (not (eq? last-undone-buffer buffer))
			   (= -1 last-undone-record))
		       (editor-error cant-undo-more)))
		 (lambda ()
		   (set! last-undone-buffer buffer)
		   (set! number-records-undone 0)
		   (set! number-chars-left
			 (string-length (undo-data-chars undo-data)))
		   (set! last-undone-record (undo-data-next-record undo-data))
		   (set! last-undone-char (undo-data-next-char undo-data))
		   ;; This accounts for the boundary that is inserted
		   ;; just before this command is called.
		   (set! argument (1+ argument))
		   unspecific))
	       (undo-n-records undo-data
			       buffer
			       (count-records-to-undo undo-data argument))))
	    (set-command-message! undo-command-tag)
	    (temporary-message "Undo!"))))))

(define (count-records-to-undo undo-data argument)
  (let ((records (undo-data-records undo-data)))
    (let find-nth-boundary ((argument argument) (i last-undone-record) (n 0))
      (let find-boundary ((i i) (n n) (any-records? false))
	(let ((i (-1+ (if (zero? i) (vector-length records) i)))
	      (n (1+ n)))
	  (set! number-records-undone (1+ number-records-undone))
	  (if (> number-records-undone (vector-length records))
	      (editor-error no-more-undo)
	      (case (undo-record-type (vector-ref records i))
		((BOUNDARY)
		 (if (= argument 1)
		     n
		     (find-nth-boundary (-1+ argument) i n)))
		((NOT-UNDOABLE)
		 (if (and (= argument 1) any-records?)
		     ;; In this case treat it as if there were a
		     ;; BOUNDARY just in front of this record.
		     (-1+ n)
		     (editor-error no-more-undo)))
		((INSERT)
		 (set! number-chars-left
		       (- number-chars-left
			  (undo-record-length (vector-ref records i))))
		 (if (negative? number-chars-left)
		     (editor-error no-more-undo)
		     (find-boundary i n true)))
		(else
		 (find-boundary i n true)))))))))

(define (undo-n-records undo-data buffer n)
  (let ((group (buffer-group buffer))
	(records (undo-data-records undo-data))
	(chars (undo-data-chars undo-data)))
    (let loop ((n n))
      (if (positive? n)
	  (let ((ir (-1+ (if (zero? last-undone-record)
			     (vector-length records)
			     last-undone-record))))
	    (let ((type (undo-record-type (vector-ref records ir)))
		  (start (undo-record-start (vector-ref records ir)))
		  (length (undo-record-length (vector-ref records ir))))
	      (cond ((eq? 'DELETE type)
		     (let ((end (+ start length)))
		       (if (or (< start (group-start-index group))
			       (> end (group-end-index group)))
			   (editor-error outside-visible-range))
		       (group-delete! group start end))
		     (set-current-point! (make-mark group start)))
		    ((eq? 'INSERT type)
		     (if (or (< start (group-start-index group))
			     (> start (group-end-index group)))
			 (editor-error outside-visible-range))
		     (set-current-point! (make-mark group start))
		     (let ((ic (- last-undone-char length)))
		       (if (not (negative? ic))
			   (begin
			     (group-insert-substring! group start
						      chars ic
						      last-undone-char)
			     (set! last-undone-char ic))
			   (let ((l (string-length chars)))
			     (let ((ic* (+ l ic)))
			       (group-insert-substring! group start
							chars ic* l)
			       (group-insert-substring! group (- start ic)
							chars 0
							last-undone-char)
			       (set! last-undone-char ic*))))))
		    ((eq? 'UNMODIFY type)
		     (buffer-not-modified! buffer))
		    ((eq? 'BOUNDARY type) 'DONE)
		    (else (error "Losing undo record type" type))))
	    (set! last-undone-record ir)
	    (loop (-1+ n)))))))