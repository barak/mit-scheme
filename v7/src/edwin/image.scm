;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/image.scm,v 1.122 1989/03/14 08:00:53 cph Exp $
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
;;; such a string will be represented by the string "^N" (or perhaps
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
  (parse false read-only true)
  (column-size false read-only true))

(define (make-null-image)
  (vector "" '() 0))

(define (make-image string)
  (parse-string-for-image string
    (lambda (parse column-size)
      (vector string parse column-size))))

(define-integrable (image-index-size image)
  (string-length (image-string image)))

(define (image-direct-output-insert-char! image char)
  (vector-set! image 0 (string-append-char (vector-ref image 0) char))
  (vector-set! image 2 (1+ (vector-ref image 2)))
  unspecific)

(define (image-direct-output-insert-substring! image string start end)
  (vector-set! image 0
	       (string-append-substring (vector-ref image 0)
					string start end))
  (vector-set! image 2 (+ (vector-ref image 2) (- end start)))
  unspecific)

(define (image-representation image)
  (let ((string (image-string image))
	(result-end (image-column-size image)))
    (let ((string-end (string-length string))
	  (result (string-allocate result-end)))
      (let loop ((parse (image-parse image)) (string-start 0) (result-start 0))
	(cond ((null? parse)
	       (substring-move-right! string string-start string-end
				      result result-start))
	      ((string? (car parse))
	       (let ((size (string-length (car parse))))
		 (substring-move-right! (car parse) 0 size result result-start)
		 (loop (cdr parse) (1+ string-start) (+ result-start size))))
	      ((number? (car parse))
	       (substring-move-right! string string-start (car parse)
				      result result-start)
	       (loop (cdr parse)
		     (car parse)
		     (+ result-start (- (car parse) string-start))))
	      (else
	       (error "Bad parse element" (car parse)))))
      result)))

(define (image-index->column image index)
  (let loop ((parse (image-parse image)) (start 0) (column 0))
    (cond ((null? parse)
	   (+ column (- index start)))
	  ((string? (car parse))
	   (if (= index start)
	       column
	       (loop (cdr parse)
		     (1+ start)
		     (+ column (string-length (car parse))))))
	  ((number? (car parse))
	   (if (<= index (car parse))
	       (+ column (- index start))
	       (loop (cdr parse)
		     (car parse)
		     (+ column (- (car parse) start)))))
	  (else
	   (error "Bad parse element" (car parse))))))

(define (image-column->index image column)
  (let loop ((parse (image-parse image)) (start 0) (c 0))
    (cond ((null? parse)
	   (+ start (- column c)))
	  ((string? (car parse))
	   (let ((new-c (+ c (string-length (car parse)))))
	     (if (< column new-c)
		 start
		 (loop (cdr parse) (1+ start) new-c))))
	  ((number? (car parse))
	   (let ((new-c (+ c (- (car parse) start))))
	     (if (< column new-c)
		 (+ start (- column c))
		 (loop (cdr parse) (car parse) new-c))))
	  (else
	   (error "Bad parse element" (car parse))))))

;;;; Parsing

(define (parse-string-for-image string receiver)
  (parse-substring-for-image string 0 (string-length string) receiver))

(define (string-column-length string start-column)
  (substring-column-length string 0 (string-length string) start-column))

(define (string-index->column string start-column index)
  (+ start-column (substring-column-length string 0 index start-column)))

(define (string-column->index string start-column column if-lose)
  (substring-column->index string 0 (string-length string) start-column
			   column if-lose))

(define (char-column-length char start-column)
  (string-length (char-representation char start-column)))

(define parse-substring-for-image)
(define substring-column-length)
(define substring-column->index)
(define char-representation)
(let ()

(set! parse-substring-for-image
(named-lambda (parse-substring-for-image string start end receiver)
  (define (loop start column receiver)
    (let ((index (substring-find-next-char-in-set string start end
						  char-set:not-graphic)))
      (if (not index)
	  (receiver '() (+ column (- end start)))
	  (let ((column (+ column (- index start))))
	    (let ((representation (char-rep string index column)))
	      (loop (1+ index)
		    (+ column (string-length representation))
		    (lambda (parse column-size)
		      (receiver (if (= index start)				    (cons representation parse)
				    (cons index (cons representation parse)))
				column-size))))))))
  (loop start 0 receiver)))

(set! substring-column-length
(named-lambda (substring-column-length string start end start-column)
  (define (loop i c)
    (let ((index (substring-find-next-char-in-set string i end
						  char-set:not-graphic)))
      (if (not index)
	  (+ c (- end i))
	  (let ((c (+ c (- index i))))
	    (loop (1+ index)
		  (+ c (string-length (char-rep string index c))))))))
  (loop start start-column)))

(set! substring-column->index
(named-lambda (substring-column->index string start end start-column
				       column #!optional if-lose)
  (define (loop i c left)
    (let ((index (substring-find-next-char-in-set string i end
						  char-set:not-graphic)))
      (if (not index)
	  (let ((n (- end i)))
	    (cond ((<= left n) (+ i left))
		  ((default-object? if-lose) end)
		  (else (if-lose (+ c n)))))
	  (let ((n (- index i)))
	    (if (<= left n)
		(+ i left)
		(let ((c (+ c n)) (left (- left n)))
		  (let ((n (string-length (char-rep string index c))))
		    (cond ((< left n) index)
			  ((= left n) (1+ index))
			  (else (loop (1+ index) (+ c n) (- left n)))))))))))
  (if (zero? column)
      start
      (loop start start-column (- column start-column)))))

(define-integrable (char-rep string index column)
  (char-representation (string-ref string index) column))

(set! char-representation
(named-lambda (char-representation char column)
  (if (char=? char #\Tab)
      (vector-ref tab-display-images (remainder column 8))
      (vector-ref display-images (char->ascii char)))))

(define tab-display-images
  #("        " "       " "      " "     " "    " "   " "  " " "))

(define display-images
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
    "\370" "\371" "\372" "\373" "\374" "\375" "\376" "\377"))

)