;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/kilcom.scm,v 1.63 1991/05/10 04:57:24 cph Exp $
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

;;;; Deletion

(define (delete-region mark)
  (if (not mark)
      (editor-error "Delete exceeds buffer bounds"))
  (delete-string mark (current-point)))

(define (kill-region mark)
  (if (not mark)
      (editor-error "Kill exceeds buffer bounds"))
  (kill-string mark (current-point)))

(define-command delete-region
  "Delete the text between point and mark."
  "*r"
  (lambda (region)
    (region-delete! region)))

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

;;;; Killing

(define-variable kill-ring-max
  "Maximum length of kill ring before oldest elements are thrown away."
  30
  exact-nonnegative-integer?)

(define-variable kill-ring
  "List of killed text sequences."
  '())

(define-variable kill-ring-yank-pointer
  "The tail of the kill ring whose car is the last thing yanked."
  '())

(define-command kill-region
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)"
  "*m\nd"
  (lambda (mark point)
    (kill-string mark point)))

(define-command copy-region-as-kill
  "Save the region as if killed, but don't kill it."
  "m\nd"
  (lambda (mark point)
    (copy-string mark point)
    (temporary-message "Region saved")))

(define-command append-next-kill
  "Cause following command, if kill, to append to previous kill."
  ()
  (lambda ()
    (set-command-message! append-next-kill-tag)))

(define (kill-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (kill-ring-save (extract-string mark point) (mark<= point mark))
    (delete-string mark point)))

(define (copy-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (kill-ring-save (extract-string mark point) (mark<= point mark))))

(define (kill-ring-save string forward?)
  (let ((strings (ref-variable kill-ring)))
    (command-message-receive append-next-kill-tag
      (lambda ()
	(if (null? strings)
	    (editor-error "No previous kill"))
	(set-car! strings
		  (if forward?
		      (string-append (car strings) string)
		      (string-append string (car strings))))
	(set-variable! kill-ring-yank-pointer strings))
      (lambda ()
	(let ((strings
	       (let ((kill-ring-max (ref-variable kill-ring-max)))
		 (if (zero? kill-ring-max)
		     '()
		     (let ((strings (cons string strings)))
		       (if (> (length strings) kill-ring-max)
			   (set-cdr! (list-tail strings (- kill-ring-max 1))
				     '()))
		       strings)))))
	  (set-variable! kill-ring strings)
	  (set-variable! kill-ring-yank-pointer strings)))))
  (set-command-message! append-next-kill-tag))

(define append-next-kill-tag
  "Append Next Kill")

;;;; Yanking

(define-command yank
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.
With just \\[universal-argument] as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  "*P"
  (lambda (argument)
    (yank (if (command-argument-multiplier-only? argument)
	      0
	      (- (command-argument-numeric-value argument) 1))
	  (command-argument-multiplier-only? argument)
	  push-current-mark!)))

(define-command yank-pop
  "Replace just-yanked stretch of killed-text with a different stretch.
This command is allowed only immediately after a \\[yank] or a \\[yank-pop].
At such a time, the region contains a stretch of reinserted
previously-killed text.  \\[yank-pop] deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument n, the n'th previous kill is inserted.
If n is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  "*p"
  (lambda (argument)
    (command-message-receive un-kill-tag
      (lambda () unspecific)
      (lambda () (editor-error "Previous command was not a yank")))
    (yank argument
	  (let ((point (current-point))
		(mark (current-mark)))
	    (let ((before? (mark< point mark)))
	      (delete-string point mark)
	      before?))
	  set-current-mark!)))

(define (yank offset before? set-current-mark!)
  ((ref-command rotate-yank-pointer) offset)
  (let* ((start (mark-right-inserting-copy (current-point)))
	 (end (mark-left-inserting-copy start)))
    (insert-string (car (ref-variable kill-ring-yank-pointer)) start)
    (mark-temporary! end)
    (mark-temporary! start)
    (if before?
	(begin (set-current-mark! end) (set-current-point! start))
	(begin (set-current-mark! start) (set-current-point! end))))
  (set-command-message! un-kill-tag))

(define un-kill-tag
  "Un-kill")

(define-command rotate-yank-pointer
  "Rotate the yanking point in the kill ring."
  "p"
  (lambda (argument)
    (let ((kill-ring (ref-variable kill-ring)))
      (if (null? kill-ring)
	  (editor-error "Kill ring is empty"))
      (set-variable!
       kill-ring-yank-pointer
       (list-tail kill-ring
		  (modulo (+ argument
			     (let ((kill-ring-yank-pointer
				    (ref-variable kill-ring-yank-pointer)))
			       (let loop ((l kill-ring) (n 0))
				 (cond ((null? l) 0)
				       ((eq? l kill-ring-yank-pointer) n)
				       (else (loop (cdr l) (+ n 1)))))))
			  (length kill-ring)))))))

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