;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Undo, translated from the GNU Emacs implementation in C.

(declare (usual-integrations)
	 )
(using-syntax edwin-syntax-table

(define enable-group-undo!)
(define undo-record-insertion!)
(define undo-record-deletion!)
(define undo-boundary!)
(define undo-done!)

(define undo-package
  (make-environment

(declare (integrate initial-undo-records initial-undo-chars
		    maximum-undo-records maximum-undo-chars))

(define initial-undo-records 8)
(define initial-undo-chars 128)
(define maximum-undo-records 512)
(define maximum-undo-chars 8192)

(define-named-structure "Undo-Data"
  records				; vector of records
  next-record				; position in vector
  chars					; string of characters
  next-char				; position in string
  )

(declare (integrate %make-undo-record undo-record-index:type
		    undo-record-index:start undo-record-index:length
		    undo-record-type undo-record-start undo-record-length
		    mark-not-undoable!))

(define (%make-undo-record)
  (vector-cons 3 #!FALSE))

(define undo-record-index:type 0)
(define undo-record-index:start 1)
(define undo-record-index:length 2)

(define (undo-record-type undo-record)
  (declare (integrate undo-record))
  (vector-ref undo-record 0))

(define (undo-record-start undo-record)
  (declare (integrate undo-record))
  (vector-ref undo-record 1))

(define (undo-record-length undo-record)
  (declare (integrate undo-record))
  (vector-ref undo-record 2))

(define (undo-records-ref records index)
  (or (vector-ref records index)
      (let ((new-record (%make-undo-record)))
	(vector-set! records index new-record)
	new-record)))

;;;; Basic Record Keeping

(define last-undo-group #!FALSE)
(define last-undo-record #!FALSE)

(set! enable-group-undo!
(named-lambda (enable-group-undo! group)
  (without-interrupts
   (lambda ()
     (let ((undo-data (%make-undo-data))
	   (records (vector-cons initial-undo-records #!FALSE)))
       (mark-not-undoable! records (-1+ initial-undo-records))
       (vector-set! undo-data undo-data-index:records records)
       (vector-set! undo-data undo-data-index:next-record 0)
       (vector-set! undo-data undo-data-index:chars
		    (string-allocate initial-undo-chars))
       (vector-set! undo-data undo-data-index:next-char 0)
       (set-group-undo-data! group undo-data))))))

(define (new-undo! undo-data type group start length)
  (let ((undo-record (undo-records-ref (undo-data-records undo-data)
				       (undo-data-next-record undo-data))))
    (let ((next (1+ (undo-data-next-record undo-data))))
      (cond ((< next (vector-length (undo-data-records undo-data)))
	     (vector-set! undo-data undo-data-index:next-record next))
	    ((>= next maximum-undo-records)
	     (vector-set! undo-data undo-data-index:next-record 0))
	    (else
	     (let ((records (undo-data-records undo-data))
		   (new-records (vector-cons maximum-undo-records #!FALSE)))
	       (subvector-move-right! records 0 (vector-length records)
				      new-records 0)
	       (mark-not-undoable! new-records (-1+ maximum-undo-records))
	       (vector-set! undo-data undo-data-index:records new-records)
	       (vector-set! undo-data undo-data-index:next-record next)))))
    (mark-not-undoable! (undo-data-records undo-data)
			(undo-data-next-record undo-data))
    (vector-set! undo-record undo-record-index:type type)
    (vector-set! undo-record undo-record-index:start start)
    (vector-set! undo-record undo-record-index:length length)
    (set! last-undo-record undo-record))
  (set! last-undo-group group)
  (if (not (eq? 'BOUNDARY type))
      (set! last-undone-record -1)))

(define (mark-not-undoable! records index)
  (declare (integrate records index))
  (vector-set! (undo-records-ref records index)
	       undo-record-index:type 'NOT-UNDOABLE))

(define (undo-store-chars! undo-data group start end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(length (group-gap-length group)))
    (cond ((<= end gap-start)
	   (undo-store-substring! undo-data text start end))
	  ((>= start gap-start)
	   (undo-store-substring! undo-data text (+ start length)
				   (+ end length)))
	  (else
	   (undo-store-substring! undo-data text start gap-start)
	   (undo-store-substring! undo-data text (group-gap-end group)
				   (+ end length))))))

(define (undo-store-substring! undo-data string start end)
  (let ((chars (undo-data-chars undo-data))
	(i (undo-data-next-char undo-data)))
    (let ((room (- (string-length chars) i))
	  (needed (- end start)))
      (cond ((> room needed)
	     (substring-move-right! string start end chars i)
	     (vector-set! undo-data undo-data-index:next-char (+ i needed))
	     (set! number-chars-left (- number-chars-left needed)))
	    ((= room needed)
	     (substring-move-right! string start end chars i)
	     (vector-set! undo-data undo-data-index:next-char 0)
	     (set! number-chars-left (- number-chars-left needed)))
	    ((< (string-length chars) maximum-undo-chars)
	     (let ((new-chars (string-allocate maximum-undo-chars)))
	       (substring-move-right! chars 0 i new-chars 0)
	       (vector-set! undo-data undo-data-index:chars new-chars))
	     (set! number-chars-left
		   (+ (- maximum-undo-chars (string-length chars))
		      number-chars-left))
	     (undo-store-substring! undo-data string start end))
	    (else
	     (let ((new-start (+ start room)))
	       (substring-move-right! string start new-start chars i)
	       (vector-set! undo-data undo-data-index:next-char 0)
	       (set! number-chars-left (- number-chars-left room))
	       (undo-store-substring! undo-data string new-start end)))))))

;;;; External Recording Hooks

;;; These assume that they are called before the regular recording
;;; daemons, for the following reason:  to check the old status of the
;;; GROUP-MODIFIED? flag before the buffer daemon updates it.

(set! undo-record-insertion!
(named-lambda (undo-record-insertion! group start end)
  (let ((undo-data (group-undo-data group)))
    (if undo-data
	(begin
	 (if (not (eq? group last-undo-group))
	     (begin (undo-mark-previous! undo-data 'BOUNDARY group
					 (mark-index (group-point group)))
		    (set! last-undo-record #!FALSE)))
	 (if (not (group-modified? group))
	     (new-undo! undo-data 'UNMODIFY group start 0))
	 (let ((length (- end start)))
	   (if (and last-undo-record
		    (eq? 'DELETE (undo-record-type last-undo-record))
		    (= start (+ (undo-record-start last-undo-record)
				(undo-record-length last-undo-record))))
	       (vector-set! last-undo-record undo-record-index:length
			    (+ length (undo-record-length last-undo-record)))
	       (new-undo! undo-data 'DELETE group start length))))))))

(set! undo-record-deletion!
(named-lambda (undo-record-deletion! group start end)
  (let ((undo-data (group-undo-data group)))
    (if undo-data
	(begin
	 (if (not (eq? group last-undo-group))
	     (begin (undo-mark-previous! undo-data 'BOUNDARY group
					 (mark-index (group-point group)))
		    (set! last-undo-record #!FALSE)))
	 (if (not (group-modified? group))
	     (new-undo! undo-data 'UNMODIFY group start 0))
	 (let ((length (- end start)))
	   (if (and last-undo-record
		    (eq? 'INSERT (undo-record-type last-undo-record))
		    (= start (undo-record-start last-undo-record)))
	       (vector-set! last-undo-record undo-record-index:length
			    (+ length (undo-record-length last-undo-record)))
	       (new-undo! undo-data 'INSERT group start length)))
	 (undo-store-chars! undo-data group start end))))))

(set! undo-boundary!
(named-lambda (undo-boundary! point)
  (without-interrupts
   (lambda ()
     (let ((group (mark-group point)))
       (let ((undo-data (group-undo-data group)))
	 (if undo-data
	     (undo-mark-previous! undo-data 'BOUNDARY group
				  (mark-index point)))))))))

(set! undo-done!
(named-lambda (undo-done! point)
  (without-interrupts
   (lambda ()
     (let ((group (mark-group point)))
       (let ((undo-data (group-undo-data group)))
	 (if undo-data
	     (undo-mark-previous! undo-data 'NOT-UNDOABLE group
				  (mark-index point)))))))))

(define (undo-mark-previous! undo-data type group start)
  (let ((record
	 (let ((records (undo-data-records undo-data))
	       (next (undo-data-next-record undo-data)))
	   (undo-records-ref records
			     (-1+ (if (zero? next)
				      (vector-length records)
				      next))))))
    (if (not (eq? type (undo-record-type record)))
	(new-undo! undo-data type group start 0))))

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

(define-command ("Undo" (argument 1))
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
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
		 (set! argument (1+ argument))))
	     (undo-n-records undo-data
			     buffer
			     (count-records-to-undo undo-data argument))))
	  (set-command-message! undo-command-tag)
	  (temporary-message "Undo!")))))

(define (count-records-to-undo undo-data argument)
  (let ((records (undo-data-records undo-data)))
    (define (find-nth-previous-boundary argument i n)
      (define (find-previous-boundary i n any-records?)
	(let ((i (-1+ (if (zero? i) (vector-length records) i)))
	      (n (1+ n)))
	  (set! number-records-undone (1+ number-records-undone))
	  (if (> number-records-undone (vector-length records))
	      (editor-error no-more-undo)
	      (case (undo-record-type (vector-ref records i))
		((BOUNDARY)
		 (if (= argument 1)
		     n
		     (find-nth-previous-boundary (-1+ argument) i n)))
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
		     (find-previous-boundary i n #!TRUE)))
		(else
		 (find-previous-boundary i n #!TRUE))))))
      (find-previous-boundary i n #!FALSE))
    (find-nth-previous-boundary argument last-undone-record 0)))

(define (undo-n-records undo-data buffer n)
  (let ((group (buffer-group buffer))
	(records (undo-data-records undo-data))
	(chars (undo-data-chars undo-data)))
    (define (loop n)
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
			   (begin (group-insert-substring! group start
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
		    (else
		     (error "Losing undo record type" type))))
	    (set! last-undone-record ir)
	    (loop (-1+ n)))))
    (loop n)))

;;; end UNDO-PACKAGE
)))

;;; Edwin Variables:
;;; Scheme Environment: (access undo-package edwin-package)
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
