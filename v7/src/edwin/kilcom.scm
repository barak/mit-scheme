;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/kilcom.scm,v 1.62 1991/05/02 01:13:23 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989-91 Massachusetts Institute of Technology
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

;;;; Kill Commands

(declare (usual-integrations))

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
  (let ((point (if (default-object? point) (current-point) point)))
    (kill-ring-save (extract-string mark point) (mark<= point mark))
    (delete-string mark point)))

(define (copy-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (kill-ring-save (extract-string mark point) (mark<= point mark))))

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

(define-command append-next-kill
  "Cause following command, if kill, to append to previous kill."
  ()
  (lambda ()
    (set-command-message! append-next-kill-tag)))

;;;; Deletion

(define-command delete-backward-char
  "Delete character before point.
With argument, kills several characters (saving them).
Negative args kill characters forward."
  "P"
  (lambda (argument)
    (if (not argument)
	(delete-region (mark-1+ (current-point)))
	(kill-region
	 (mark- (current-point) (command-argument-value argument))))))

(define-command delete-char
  "Delete character after point.
With argument, kill than many characters (saving them).
Negative args kill characters backward."
  "P"
  (lambda (argument)
    (if (not argument)
	(delete-region (mark1+ (current-point)))
	(kill-region
	 (mark+ (current-point) (command-argument-value argument))))))

(define-command kill-line
  "Kill to end of line, or kill an end of line.
At the end of a line (only blanks following) kill through the newline.
Otherwise, kill the rest of the line but not the newline.  
With argument (positive or negative), kill specified number of lines.
An argument of zero means kill to beginning of line, nothing if at beginning.
Killed text is pushed onto the kill ring for retrieval."
  "P"
  (lambda (argument)
    (let ((argument (command-argument-value argument))
	  (point (current-point)))
      (kill-region
       (cond ((not argument)
	      (let ((end (line-end point 0)))
		(if (and (region-blank? (make-region point end))
			 (not (group-end? point)))
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
			       'LIMIT))))))))

(define-command backward-delete-char-untabify
  "Delete character before point, turning tabs into spaces.
Rather than deleting a whole tab, the tab is converted into the
appropriate number of spaces and then one space is deleted."
  "P"
  (lambda (argument)
    (define (back n)
      (let ((point (current-point)))
	(let ((m1 (mark- point n 'LIMIT)))
	  (let ((tab (char-search-backward #\tab point m1)))
	    (if (not tab)
		m1
		(begin
		  (convert-tab-to-spaces! tab)
		  (back n)))))))
    (define (forth n)
      (let ((point (current-point)))
	(let ((m1 (mark+ point n 'LIMIT)))
	  (let ((tab (char-search-forward #\tab point m1)))
	    (if (not tab)
		m1
		(begin
		  (convert-tab-to-spaces! (mark-1+ tab))
		  (forth n)))))))
    (let ((argument (command-argument-value argument)))
      (cond ((not argument)
	     (let ((point (current-point)))
	       (if (char-match-backward #\Tab point)
		   (convert-tab-to-spaces! (mark-1+ point))))
	     (delete-region (mark-1+ (current-point))))
	    ((positive? argument)
	     (kill-region (back argument)))
	    ((negative? argument)
	     (kill-region (forth (- argument))))))))

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

(define-command kill-region
  "Kill from point to mark.
Use \\[yank] and \\[yank-pop] to get it back."
  "m"
  kill-region)

(define-command copy-region-as-kill
  "Stick region into kill-ring without killing it.
Like killing and getting back, but doesn't mark buffer modified."
  ()
  (lambda ()
    (copy-region (current-mark))
    (temporary-message "Region saved")))

(define un-kill-tag
  "Un-kill")

(define-command yank
  "Re-insert the last stuff killed.
Puts point after it and the mark before it.
A positive argument N says un-kill the N'th most recent
string of killed stuff (1 = most recent).  A null
argument (just C-U) means leave point before, mark after."
  "P"
  (lambda (argument)
    (let ((ring (current-kill-ring)))
      (define (pop-loop n)
	(if (> n 1)
	    (begin (ring-pop! ring)
		   (pop-loop (-1+ n)))))
      (if (ring-empty? ring) (editor-error "Nothing to un-kill"))
      (if (command-argument-multiplier-only? argument)
	  (unkill (ring-ref ring 0))
	  (let ((argument (command-argument-numeric-value argument)))
	    (if (positive? argument)
		(begin
		  (pop-loop argument)
		  (unkill-reversed (ring-ref ring 0)))))))
    (set-command-message! un-kill-tag)))

(define-command yank-pop
  "Correct after \\[yank] to use an earlier kill.
Requires that the region contain the most recent killed stuff,
as it does immediately after using \\[yank].
It is deleted and replaced with the previous killed stuff,
which is rotated to the front of the kill ring.
With 0 as argument, just deletes the region with no replacement,
but the region must still match the last killed stuff."
  "p"
  (lambda (argument)
    (command-message-receive un-kill-tag
      (lambda ()
	(let ((ring (current-kill-ring))
	      (point (current-point)))
	  (if (or (ring-empty? ring)
		  (not
		   (let ((string (ring-ref ring 0))
			 (mark (current-mark)))
		     (if (mark< mark point)
			 (match-forward string mark point false)
			 (match-forward string point mark false)))))
	      (editor-error "Region does not match last kill"))
	  (delete-string (pop-current-mark!) point)
	  (if (not (zero? argument))
	      (begin
		(ring-pop! ring)
		(unkill-reversed (ring-ref ring 0))))))
      (lambda ()
	(editor-error "No previous un-kill to replace")))
    (set-command-message! un-kill-tag)))

;;;; Marks

(define-variable mark-ring-maximum
  "The maximum number of marks that are saved on the mark ring.
This variable is only noticed when a buffer is created, so changing
it later will not affect existing buffers."
  16)

(define-command set-mark-command
  "Sets or pops the mark.
With no \\[universal-argument]'s, pushes point as the mark.
With one \\[universal-argument], pops the mark into point.
With two \\[universal-argument]'s, pops the mark and throws it away."
  "P"
  (lambda (argument)
    (case (and (command-argument-multiplier-only? argument)
	       (command-argument-value argument))
      ((4) (set-current-point! (pop-current-mark!)))
      ((16) (pop-current-mark!))
      (else (push-current-mark! (current-point))))))

(define-command mark-beginning-of-buffer
  "Set mark at beginning of buffer."
  ()
  (lambda ()
    (push-current-mark! (buffer-start (current-buffer)))))

(define-command mark-end-of-buffer
  "Set mark at end of buffer."
  ()
  (lambda ()
    (push-current-mark! (buffer-end (current-buffer)))))

(define-command mark-whole-buffer
  "Set point at beginning and mark at end of buffer.
Pushes the old point on the mark first, so two pops restore it.
With argument, puts point at end and mark at beginning."
  "P"
  (lambda (argument)
    (push-current-mark! (current-point))
    ((if (not argument) set-current-region! set-current-region-reversed!)
     (buffer-region (current-buffer)))))

(define-command exchange-point-and-mark
  "Exchange positions of point and mark."
  ()
  (lambda ()
    (let ((point (current-point))
	  (mark (current-mark)))
      (if (not mark) (editor-error "No mark to exchange"))
      (set-current-point! mark)
      (set-current-mark! point))))

;;;; Transposition

(define-command transpose-chars
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
  "p"
  (lambda (argument)
    (cond ((and (= argument 1) (line-end? (current-point)))
	   (twiddle-characters (mark-1+ (current-point) 'ERROR)
			       (current-point)))
	  ((positive? argument)
	   (twiddle-characters (current-point)
			       (mark+ (current-point) argument 'ERROR)))
	  ((negative? argument)
	   (twiddle-characters
	    (current-point)
	    (mark- (current-point) (1+ (- argument)) 'ERROR)))
	  (else
	   (let ((m1 (mark-right-inserting (current-point)))
		 (m2 (mark-right-inserting (current-mark))))
	     (if (not (mark= m1 m2))
		 (begin
		   (let ((c1 (extract-right-char m1))
			 (c2 (extract-right-char m2)))
		     (delete-right-char m1)
		     (delete-right-char m2)
		     (insert-char c2 m1)
		     (insert-char c1 m2))
		   (set-current-point! m1)
		   (set-current-mark! m2))))))))

(define (twiddle-characters m1 m2)
  (let ((m* (mark-left-inserting m2)))
    (let ((char (extract-left-char m1)))
      (delete-left-char m1)
      (insert-char char m*))
    (set-current-point! m*)))