;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/image.scm,v 1.126 1990/11/02 03:24:25 cph Rel $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Display Imaging

(declare (usual-integrations))

;;; Display imaging is the process by which strings are converted into
;;; an image which can be displayed on a screen.  The IMAGE
;;; abstraction, implemented here, captures that process.  Given a
;;; string, it is capable of generating another string which is the
;;; visual representation of that string.  In addition, it retains the
;;; ability to associate indices into the string with columns in the
;;; representation.

;;; *** One important note: the image abstraction will not "correctly"
;;; display strings that contain newlines.  Currently, a newline in
;;; such a string will be represented by the string "^J" (or perhaps
;;; "^M").  This is so because images are intended to be used on a
;;; per-line basis; that is, the string should be for a single line.

;;; Images are implemented in terms of another abstraction, called a
;;; PARSE, which describes how characters in the string are displayed.
;;; Most characters are represented by themselves (these are called
;;; "graphic" characters), but others (called "non-graphic"
;;; characters) are represented by strings of graphic characters.

;;; A parse, then, is a list of alternating index/string pairs.  The
;;; index is the position of the next non-graphic character in the
;;; string, and the following string is its representation.  If two or
;;; more non-graphic characters are adjacent, then the list contains a
;;; single index, followed by the representations of each of the
;;; non-graphic characters, in succession.  Finally, if the
;;; non-graphic characters appear at the beginning of the string, then
;;; the index is omitted altogether.

;;; This representation has a number of advantages.

;;; [] Most of the time, there are no non-graphic characters in the
;;;    string; then the parse is the empty list.

;;; [] Adjacent non-graphic characters (tabs) are common in indented
;;;    Lisp code; this representation optimizes specially for this
;;;    case.

;;; [] The association of string indices and image columns is very
;;;    straightforward.

(define-structure (image (type vector) (constructor false))
  (string false read-only true)
  (start-index false read-only true)
  (start-column false read-only true)
  (parse false read-only true)
  (column-size false read-only true))

(define (make-null-image)
  (vector "" 0 0 '() 0))

(define-integrable (string->image string start-column)
  (string-head->image string 0 start-column))

(define (string-head->image string start start-column)
  (parse-substring-for-image string start (string-length string) start-column
    (lambda (parse column-size)
      (vector string start start-column parse column-size))))

(define (image-index-size image)
  (fix:- (string-length (image-string image)) (image-start-index image)))

(define (image-direct-output-insert-char! image char)
  (vector-set! image 0 (string-append-char (vector-ref image 0) char))
  (vector-set! image 4 (fix:1+ (vector-ref image 4))))

(define (image-direct-output-insert-substring! image string start end)
  (vector-set! image 0
	       (string-append-substring (vector-ref image 0)
					string start end))
  (vector-set! image 4 (fix:+ (vector-ref image 4) (fix:- end start))))

(define (image-representation image)
  (let ((string (image-string image))
	(result (string-allocate (image-column-size image))))
    (let ((string-end (string-length string)))
      (let loop
	  ((parse (image-parse image))
	   (string-start (image-start-index image))
	   (result-start 0))
	(cond ((null? parse)
	       (substring-move-left! string string-start string-end
				     result result-start))
	      ((string? (car parse))
	       (let ((size (string-length (car parse))))
		 (substring-move-left! (car parse) 0 size result result-start)
		 (loop (cdr parse)
		       (fix:1+ string-start)
		       (fix:+ result-start size))))
	      ((number? (car parse))
	       (substring-move-left! string string-start (car parse)
				     result result-start)
	       (loop (cdr parse)
		     (car parse)
		     (fix:+ result-start (fix:- (car parse) string-start))))
	      (else
	       (error "Bad parse element" (car parse))))))
    result))

(define (image-index->column image index)
  (let loop
      ((parse (image-parse image))
       (start (image-start-index image))
       (column (image-start-column image)))
    (cond ((null? parse)
	   (fix:+ column (fix:- index start)))
	  ((string? (car parse))
	   (if (fix:= index start)
	       column
	       (loop (cdr parse)
		     (fix:1+ start)
		     (fix:+ column (string-length (car parse))))))
	  ((number? (car parse))
	   (if (fix:> index (car parse))
	       (loop (cdr parse)
		     (car parse)
		     (fix:+ column (fix:- (car parse) start)))
	       (fix:+ column (fix:- index start))))
	  (else
	   (error "Bad parse element" (car parse))))))

(define (image-column->index image column)
  ;; If COLUMN falls in the middle of a multi-column character, the
  ;; index returned is that of the character.  Thinking of the index
  ;; as a pointer between characters, the value is the pointer to the
  ;; left of the multi-column character.  Only if COLUMN reaches
  ;; across the character will the right-hand pointer be returned.
  ;; Various things depend on this.
  (let loop
      ((parse (image-parse image))
       (start (image-start-index image))
       (c (image-start-column image)))
    (cond ((null? parse)
	   (fix:+ start (fix:- column c)))
	  ((string? (car parse))
	   (let ((new-c (fix:+ c (string-length (car parse)))))
	     (if (fix:< column new-c)
		 start
		 (loop (cdr parse) (fix:1+ start) new-c))))
	  ((number? (car parse))
	   (let ((new-c (fix:+ c (fix:- (car parse) start))))
	     (if (fix:< column new-c)
		 (fix:+ start (fix:- column c))
		 (loop (cdr parse) (car parse) new-c))))
	  (else
	   (error "Bad parse element" (car parse))))))

;;;; String Operations

(define (string-representation string start-column)
  (substring-representation string 0 (string-length string) start-column))

(define (substring-representation string start end start-column)
  (let ((result
	 (string-allocate
	  (fix:- (substring-column-length string start end start-column)
	     start-column))))
    (let loop ((start start) (column start-column) (rindex 0))
      (let* ((index
	      (substring-find-next-char-in-set string start end
					       char-set:not-graphic))
	     (copy-representation!
	      (lambda (column rindex)
		(let* ((representation
			(char-representation (string-ref string index) column))
		       (size (string-length representation)))
		  (substring-move-right! representation 0 size result rindex)
		  (loop (fix:1+ index)
			(fix:+ column size)
			(fix:+ rindex size))))))
	(cond ((not index)
	       (substring-move-right! string start end result rindex)
	       result)
	      ((fix:= start index)
	       (copy-representation! column rindex))
	      (else
	       (substring-move-right! string start index result rindex)
	       (let ((size (fix:- index start)))
		 (copy-representation! (fix:+ column size)
				       (fix:+ rindex size)))))))))

(define (string-column-length string start-column)
  (substring-column-length string 0 (string-length string) start-column))

(define (string-index->column string start-column index)
  (fix:+ start-column (substring-column-length string 0 index start-column)))

(define (substring-column-length string start end start-column)
  (let loop ((i start) (c start-column))
    (let ((index
	   (substring-find-next-char-in-set string i end
					    char-set:not-graphic)))
      (if (not index)
	  (fix:+ c (fix:- end i))
	  (loop (fix:1+ index)
		(let ((c (fix:+ c (fix:- index i))))
		  (fix:+ c
			 (char-column-length (string-ref string index)
					     c))))))))

(define (string-column->index string start-column column if-lose)
  (substring-column->index string 0 (string-length string) start-column
			   column if-lose))

(define (substring-column->index string start end start-column column
				 #!optional if-lose)
  ;; If COLUMN falls in the middle of a multi-column character, the
  ;; index returned is that of the character.  Thinking of the index
  ;; as a pointer between characters, the value is the pointer to the
  ;; left of the multi-column character.  Only if COLUMN reaches
  ;; across the character will the right-hand pointer be returned.
  ;; Various things depend on this.
  (if (fix:zero? column)
      start
      (let loop ((i start) (c start-column) (left (fix:- column start-column)))
	(let ((index
	       (substring-find-next-char-in-set string i end
						char-set:not-graphic)))
	  (if (not index)
	      (let ((n (fix:- end i)))
		(cond ((not (fix:> left n)) (fix:+ i left))
		      ((default-object? if-lose) end)
		      (else (if-lose (fix:+ c n)))))
	      (let ((n (fix:- index i)))
		(if (fix:> left n)
		    (let ((c (fix:+ c n))
			  (left (fix:- left n)))
		      (let ((n
			     (char-column-length (string-ref string index) c)))
			(cond ((fix:< left n) index)
			      ((fix:= left n) (fix:1+ index))
			      (else
			       (loop (fix:1+ index)
				     (fix:+ c n)
				     (fix:- left n))))))
		    (fix:+ i left))))))))

;;;; Parsing

(define (parse-substring-for-image string start end start-column receiver)
  (let ((column-size))
    (let ((parse
	   (let loop ((start start) (column start-column))
	     (let ((index
		    (substring-find-next-char-in-set string start end
						     char-set:not-graphic)))
	       (if (not index)
		   (begin
		     (set! column-size (fix:+ column (fix:- end start)))
		     '())
		   (let ((column (fix:+ column (fix:- index start))))
		     (let ((representation
			    (char-representation (string-ref string index)
						 column)))
		       (let ((parse
			      (loop (fix:1+ index)
				    (fix:+ column
					   (string-length representation)))))
			 (if (fix:= index start)
			     (cons representation parse)
			     (cons index (cons representation parse)))))))))))
      (receiver parse column-size))))

(define char-column-length)
(define char-representation)
(let ((tab-display-images
       #("        " "       " "      " "     " "    " "   " "  " " "))
      (display-images
       #("^@" "^A" "^B" "^C" "^D" "^E" "^F" "^G"
	 "^H" "^I" "^J" "^K" "^L" "^M" "^N" "^O"
	 "^P" "^Q" "^R" "^S" "^T" "^U" "^V" "^W"
	 "^X" "^Y" "^Z" "^[" "^\\" "^]" "^^" "^_"
	 " " "!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/"
	 "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ":" ";" "<" "=" ">" "?"
	 "@" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O"
	 "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "[" "\\" "]" "^" "_"
	 "`" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
	 "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "{" "|" "}" "~" "^?"
	 "\200" "\201" "\202" "\203" "\204" "\205" "\206" "\207"
	 "\210" "\211" "\212" "\213" "\214" "\215" "\216" "\217"
	 "\220" "\221" "\222" "\223" "\224" "\225" "\226" "\227"
	 "\230" "\231" "\232" "\233" "\234" "\235" "\236" "\237"
	 "\240" "\241" "\242" "\243" "\244" "\245" "\246" "\247"
	 "\250" "\251" "\252" "\253" "\254" "\255" "\256" "\257"
	 "\260" "\261" "\262" "\263" "\264" "\265" "\266" "\267"
	 "\270" "\271" "\272" "\273" "\274" "\275" "\276" "\277"
	 "\300" "\301" "\302" "\303" "\304" "\305" "\306" "\307"
	 "\310" "\311" "\312" "\313" "\314" "\315" "\316" "\317"
	 "\320" "\321" "\322" "\323" "\324" "\325" "\326" "\327"
	 "\330" "\331" "\332" "\333" "\334" "\335" "\336" "\337"
	 "\340" "\341" "\342" "\343" "\344" "\345" "\346" "\347"
	 "\350" "\351" "\352" "\353" "\354" "\355" "\356" "\357"
	 "\360" "\361" "\362" "\363" "\364" "\365" "\366" "\367"
	 "\370" "\371" "\372" "\373" "\374" "\375" "\376" "\377")))
  (set! char-representation
	(lambda (char column)
	  (if (char=? char #\tab)
	      (vector-ref tab-display-images (fix:remainder column 8))
	      (vector-ref display-images (char->integer char)))))
  (let ((tab-display-lengths (vector-map tab-display-images string-length))
	(display-lengths (vector-map display-images string-length)))
    (set! char-column-length
	  (lambda (char column)
	    (if (char=? char #\tab)
		(vector-ref tab-display-lengths (fix:remainder column 8))
		(vector-ref display-lengths (char->integer char)))))
    unspecific))