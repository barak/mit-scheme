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

;;;; Kill Commands

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define (delete-region mark)
  (if (not mark)
      (editor-error "Delete exceeds buffer bounds")
      (delete-string mark (current-point))))

(define (kill-region mark)
  (if (not mark)
      (editor-error "Kill exceeds buffer bounds")
      (kill-string mark (current-point))))

(define (copy-region mark)
  (if (not mark)
      (editor-error "Copy exceeds buffer bounds")
      (copy-string mark (current-point))))

(define (kill-string mark #!optional point)
  (if (unassigned? point) (set! point (current-point)))
  (kill-ring-save (extract-string mark point)
		  (mark<= point mark))
  (delete-string mark point))

(define (copy-string mark #!optional point)
  (if (unassigned? point) (set! point (current-point)))
  (kill-ring-save (extract-string mark point)
		  (mark<= point mark)))

(define (unkill string)
  (let ((end (current-point)))
    (let ((start (mark-right-inserting end)))
      (insert-string string end)
      (set-current-point! start))
    (push-current-mark! end)))

(define (unkill-reversed string)
  (let ((end (current-point)))
    (push-current-mark! (mark-right-inserting end))
    (insert-string string end)))

(define append-next-kill-tag
  "Append Next Kill")

(define (kill-ring-save string forward?)
  (let ((ring (current-kill-ring)))
    (command-message-receive append-next-kill-tag
      (lambda ()
	(if (ring-empty? ring) (editor-error "No previous kill"))
	(ring-set! ring 0
		   (if forward?
		       (string-append (ring-ref ring 0) string)
		       (string-append string (ring-ref ring 0)))))
      (lambda ()
	(ring-push! ring string))))
  (set-command-message! append-next-kill-tag))

(define-command ("^R Append Next Kill" argument)
  "Cause following command, if kill, to append to previous kill."
  (set-command-message! append-next-kill-tag))

;;;; Deletion

(define-command ("^R Backward Delete Character" argument)
  "Delete character before point.
With argument, kills several characters (saving them).
Negative args kill characters forward."
  (if (not argument)
      (delete-region (mark-1+ (current-point)))
      (kill-region (mark- (current-point) argument))))

(define-command ("^R Delete Character" argument)
  "Delete character after point.
With argument, kill than many characters (saving them).
Negative args kill characters backward."
  (if (not argument)
      (delete-region (mark1+ (current-point)))
      (kill-region (mark+ (current-point) argument))))

(define-command ("^R Kill Line" argument)
  "Kill to end of line, or kill an end of line.
At the end of a line (only blanks following) kill through the newline.
Otherwise, kill the rest of the line but not the newline.  
With argument (positive or negative), kill specified number of lines.
An argument of zero means kill to beginning of line, nothing if at beginning.
Killed text is pushed onto the kill ring for retrieval."
  (let ((point (current-point)))
    (kill-region
     (cond ((not argument)
	    (let ((end (line-end point 0)))
	      (if (region-blank? (make-region point end))
		  (mark1+ end)
		  end)))
	   ((positive? argument)
	    (and (not (group-end? point))
		 (line-start point argument 'LIMIT)))
	   ((zero? argument)
	    (line-start point 0))
	   (else
	    (and (not (group-start? point))
		 (line-start point
			     (if (line-start? point)
				 argument
				 (1+ argument))
			     'LIMIT)))))))

(define-command ("^R Backward Delete Hacking Tabs" argument)
  "Delete character before point, turning tabs into spaces.
Rather than deleting a whole tab, the tab is converted into the
appropriate number of spaces and then one space is deleted."
  (define (back n)
    (let ((m1 (mark- (current-point) n 'LIMIT)))
      (if (not (char-search-backward #\Tab (current-point) m1))
	  m1
	  (begin (convert-tab-to-spaces! (re-match-start 0))
		 (back n)))))
  (define (forth n)
    (let ((m1 (mark+ (current-point) n 'LIMIT)))
      (if (not (char-search-forward #\Tab (current-point) m1))
	  m1
	  (begin (convert-tab-to-spaces! (re-match-start 0))
		 (forth n)))))
  (cond ((not argument)
	 (if (char-match-backward #\Tab)
	     (convert-tab-to-spaces! (mark-1+ (current-point))))
	 (delete-region (mark-1+ (current-point))))
	((positive? argument)
	 (kill-region (back argument)))
	((negative? argument)
	 (kill-region (forth (- argument))))))

(define (convert-tab-to-spaces! m1)
  (let ((at-point? (mark= m1 (current-point)))
	(m2 (mark-left-inserting (mark1+ m1))))
    (define (perform-replacement)
      (let ((n (- (mark-column m2) (mark-column m1))))
	(delete-string m1 m2)
	(insert-string (make-string n #\Space) m2)))
    (if at-point?
	(let ((start (mark-right-inserting m1)))
	  (perform-replacement)
	  (set-current-point! start))
	(perform-replacement))))

;;;; Un/Killing

(define-command ("^R Kill Region" argument)
  "Kill from point to mark.
Use \\[^R Un-Kill] and \\[^R Un-Kill Pop] to get it back."
  (kill-region (current-mark)))

(define-command ("^R Copy Region" argument)
  "Stick region into kill-ring without killing it.
Like killing and getting back, but doesn't mark buffer modified."
  (copy-region (current-mark))
  (temporary-message "Region saved"))

(define un-kill-tag
  "Un-kill")

(define-command ("^R Un-Kill" (argument 1))
  "Re-insert the last stuff killed.
Puts point after it and the mark before it.
A positive argument N says un-kill the N'th most recent
string of killed stuff (1 = most recent).  A null
argument (just C-U) means leave point before, mark after."
  (let ((ring (current-kill-ring)))
    (define (pop-loop n)
      (if (> n 1)
	  (begin (ring-pop! ring)
		 (pop-loop (-1+ n)))))
    (if (ring-empty? ring) (editor-error "Nothing to un-kill"))
    (cond ((command-argument-multiplier-only?)
	   (unkill (ring-ref ring 0)))
	  ((positive? argument)
	   (pop-loop argument)
	   (unkill-reversed (ring-ref ring 0)))))
  (set-command-message! un-kill-tag))

(define-command ("^R Un-kill Pop" (argument 1))
  "Correct after \\[^R Un-Kill] to use an earlier kill.
Requires that the region contain the most recent killed stuff,
as it does immediately after using \\[^R Un-Kill].
It is deleted and replaced with the previous killed stuff,
which is rotated to the front of the kill ring.
With 0 as argument, just deletes the region with no replacement,
but the region must still match the last killed stuff."
  (command-message-receive un-kill-tag
    (lambda ()
      (let ((ring (current-kill-ring))
	    (point (current-point)))
	(if (or (ring-empty? ring)
		(not (match-string (ring-ref ring 0) (current-mark) point)))
	    (editor-error "Region does not match last kill"))
	(delete-string (pop-current-mark!) point)
	(if (not (zero? argument))
	    (begin (ring-pop! ring)
		   (unkill-reversed (ring-ref ring 0))))))
    (lambda ()
      (editor-error "No previous un-kill to replace")))
  (set-command-message! un-kill-tag))

;;;; Marks

(define-variable "Mark Ring Maximum"
  "The maximum number of marks that are saved on the mark ring.
This variable is only noticed when a buffer is created, so changing
it later will not affect existing buffers."
  16)

(define-command ("^R Set/Pop Mark" argument)
  "Sets or pops the mark.
With no C-U's, pushes point as the mark.
With one C-U, pops the mark into point.
With two C-U's, pops the mark and throws it away."
  (let ((n (command-argument-multiplier-exponent)))
    (cond ((zero? n) (push-current-mark! (current-point)))
	  ((= n 1) (set-current-point! (pop-current-mark!)))
	  ((= n 2) (pop-current-mark!))
	  (else (editor-error)))))

(define-command ("^R Mark Beginning" argument)
  "Set mark at beginning of buffer."
  (push-current-mark! (buffer-start (current-buffer))))

(define-command ("^R Mark End" argument)
  "Set mark at end of buffer."
  (push-current-mark! (buffer-end (current-buffer))))

(define-command ("^R Mark Whole Buffer" argument)
  "Set point at beginning and mark at end of buffer.
Pushes the old point on the mark first, so two pops restore it.
With argument, puts point at end and mark at beginning."
  (push-current-mark! (current-point))
  ((if (not argument) set-current-region! set-current-region-reversed!)
   (buffer-region (current-buffer))))

(define-command ("^R Exchange Point and Mark" argument)
  "Exchange positions of point and mark."
  (let ((point (current-point))
	(mark (current-mark)))
    (if (not mark) (editor-error "No mark to exchange"))
    (set-current-point! mark)
    (set-current-mark! point)))

;;;; Q-Registers

(define-command ("^R Get Q-reg" argument)
  "Get contents of Q-reg (reads name from tty).
Usually leaves the pointer before, and the mark after, the text.
With argument, puts point after and mark before."
  (not-implemented))

(define-command ("^R Put Q-reg" argument)
  "Put point to mark into Q-reg (reads name from tty).
With an argument, the text is also deleted."
  (not-implemented))

;;;; Transposition

(define-command ("^R Transpose Characters" (argument 1))
  "Transpose the characters before and after the cursor.
With a positive argument it transposes the characters before and after
the cursor, moves right, and repeats the specified number of times,
dragging the character to the left of the cursor right.

With a negative argument, it transposes the two characters to the left
of the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.

With a zero argument, it transposes the characters at point and mark.

At the end of a line, with no argument, the preceding two characters
are transposed."
  (cond ((and (= argument 1) (line-end? (current-point)))
	 (twiddle-characters (mark-1+ (current-point) 'ERROR)
			     (current-point)))
	((positive? argument)
	 (twiddle-characters (current-point)
			     (mark+ (current-point) argument 'ERROR)))
	((negative? argument)
	 (twiddle-characters (current-point)
			     (mark- (current-point) (1+ (- argument)) 'ERROR)))
	(else
	 (let ((m1 (mark-right-inserting (current-point)))
	       (m2 (mark-right-inserting (current-mark))))
	   (let ((r1 (region-extract!
		      (make-region (current-point)
				   (mark1+ (current-point) 'ERROR))))
		 (r2 (region-extract!
		      (make-region (current-mark)
				   (mark1+ (current-mark) 'ERROR)))))
	     (region-insert! m1 r2)
	     (region-insert! m2 r1))
	   (set-current-point! m1)
	   (set-current-mark! m2)))))

(define (twiddle-characters m1 m2)
  (let ((m* (mark-left-inserting m2)))
    (region-insert! m* (region-extract! (make-region (mark-1+ m1 'ERROR) m1)))
    (set-current-point! m*)))

(define-command ("^R Transpose Regions" argument)
  "Transpose regions defined by point and last 3 marks.
To transpose two non-overlapping regions, set the mark successively at three
of the four boundaries, put point at the fourth, and call this function.
On return, the cursor and saved marks retain their original order, but are
adjusted to delineate the interchanged regions.  Thus two consecutive
calls to this function will leave the buffer unchanged."
  (not-implemented))

;;; end USING-SYNTAX
)