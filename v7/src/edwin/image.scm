;;; -*-Scheme-*-
;;;
;;;	$Id: image.scm,v 1.133 1995/02/16 21:59:01 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-95 Massachusetts Institute of Technology
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

(define (group-columns group start end column tab-width char-image-strings)
  (let ((text       (group-text group))
	(gap-start  (group-gap-start group))
	(gap-end    (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-columns text start end column
			      tab-width char-image-strings))
	  ((fix:<= gap-start start)
	   (substring-columns text
			      (fix:+ start gap-length)
			      (fix:+ end gap-length)
			      column
			      tab-width
			      char-image-strings))
	  (else
	   (substring-columns text
			      gap-end
			      (fix:+ end gap-length)
			      (substring-columns text start gap-start
						 column tab-width
						 char-image-strings)
			      tab-width
			      char-image-strings)))))

(define (string-columns string column tab-width char-image-strings)
  (substring-columns string 0 (string-length string) column tab-width
		     char-image-strings))

(define (substring-columns string start end column tab-width
			   char-image-strings)
  (if tab-width
      (do ((index start (fix:+ index 1))
	   (column column
		   (fix:+ column
			  (let ((ascii (vector-8b-ref string index)))
			    (if (fix:= ascii (char->integer #\tab))
				(fix:- tab-width
				       (fix:remainder column tab-width))
				(string-length
				 (vector-ref char-image-strings ascii)))))))
	  ((fix:= index end) column))
      (do ((index start (fix:+ index 1))
	   (column column
		   (fix:+ column
			  (string-length
			   (vector-ref char-image-strings
				       (vector-8b-ref string index))))))
	  ((fix:= index end) column))))

;;(define-integrable char-image-lengths
;;  '#(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;;     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;;     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;;     1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2
;;     4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
;;     4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
;;     4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
;;     4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4))

(define default-char-image-strings
  '#("^@" "^A" "^B" "^C" "^D" "^E" "^F" "^G"
     "^H" "^I" "^J" "^K" "^L" "^M" "^N" "^O"
     "^P" "^Q" "^R" "^S" "^T" "^U" "^V" "^W"
     "^X" "^Y" "^Z" "^[" "^\\" "^]" "^^" "^_"
     " " "!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/"
     "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ":" ";" "<" "=" ">" "?"
     "@" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O"
     "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "[" "\\" "]" "^" "_"
     "`" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
     "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "{" "|" "}" "~" "^?"
     "\\200" "\\201" "\\202" "\\203" "\\204" "\\205" "\\206" "\\207"
     "\\210" "\\211" "\\212" "\\213" "\\214" "\\215" "\\216" "\\217"
     "\\220" "\\221" "\\222" "\\223" "\\224" "\\225" "\\226" "\\227"
     "\\230" "\\231" "\\232" "\\233" "\\234" "\\235" "\\236" "\\237"
     "\\240" "\\241" "\\242" "\\243" "\\244" "\\245" "\\246" "\\247"
     "\\250" "\\251" "\\252" "\\253" "\\254" "\\255" "\\256" "\\257"
     "\\260" "\\261" "\\262" "\\263" "\\264" "\\265" "\\266" "\\267"
     "\\270" "\\271" "\\272" "\\273" "\\274" "\\275" "\\276" "\\277"
     "\\300" "\\301" "\\302" "\\303" "\\304" "\\305" "\\306" "\\307"
     "\\310" "\\311" "\\312" "\\313" "\\314" "\\315" "\\316" "\\317"
     "\\320" "\\321" "\\322" "\\323" "\\324" "\\325" "\\326" "\\327"
     "\\330" "\\331" "\\332" "\\333" "\\334" "\\335" "\\336" "\\337"
     "\\340" "\\341" "\\342" "\\343" "\\344" "\\345" "\\346" "\\347"
     "\\350" "\\351" "\\352" "\\353" "\\354" "\\355" "\\356" "\\357"
     "\\360" "\\361" "\\362" "\\363" "\\364" "\\365" "\\366" "\\367"
     "\\370" "\\371" "\\372" "\\373" "\\374" "\\375" "\\376" "\\377"))

(define default-char-image-strings/ascii
  '#("[NUL]" "[SOH]" "[STX]" "[ETX]" "[EOT]" "[ENQ]" "[ACK]" "[BEL]"
     "[BS]"  "[HT]"  "[NL]"  "[VT]" "[Page]" "[CR]"  "[SO]"  "[SI]"
     "[DLE]" "[DC1]" "[DC2]" "[DC3]" "[DC4]" "[NAK]" "[SYN]" "[ETB]"
     "[CAN]" "[EM]"  "[SUB]" "[ESC]" "[FS]"  "[GS]"  "[RS]"  "[US]"
     " " "!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/"
     "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ":" ";" "<" "=" ">" "?"
     "@" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O"
     "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "[" "\\" "]" "^" "_"
     "`" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
     "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "{" "|" "}" "~" "^?"
     "\\200" "\\201" "\\202" "\\203" "\\204" "\\205" "\\206" "\\207"
     "\\210" "\\211" "\\212" "\\213" "\\214" "\\215" "\\216" "\\217"
     "\\220" "\\221" "\\222" "\\223" "\\224" "\\225" "\\226" "\\227"
     "\\230" "\\231" "\\232" "\\233" "\\234" "\\235" "\\236" "\\237"
     "\\240" "\\241" "\\242" "\\243" "\\244" "\\245" "\\246" "\\247"
     "\\250" "\\251" "\\252" "\\253" "\\254" "\\255" "\\256" "\\257"
     "\\260" "\\261" "\\262" "\\263" "\\264" "\\265" "\\266" "\\267"
     "\\270" "\\271" "\\272" "\\273" "\\274" "\\275" "\\276" "\\277"
     "\\300" "\\301" "\\302" "\\303" "\\304" "\\305" "\\306" "\\307"
     "\\310" "\\311" "\\312" "\\313" "\\314" "\\315" "\\316" "\\317"
     "\\320" "\\321" "\\322" "\\323" "\\324" "\\325" "\\326" "\\327"
     "\\330" "\\331" "\\332" "\\333" "\\334" "\\335" "\\336" "\\337"
     "\\340" "\\341" "\\342" "\\343" "\\344" "\\345" "\\346" "\\347"
     "\\350" "\\351" "\\352" "\\353" "\\354" "\\355" "\\356" "\\357"
     "\\360" "\\361" "\\362" "\\363" "\\364" "\\365" "\\366" "\\367"
     "\\370" "\\371" "\\372" "\\373" "\\374" "\\375" "\\376" "\\377"))


(define (group-line-columns group start end column
			    tab-width char-image-strings)
  ;; Like GROUP-COLUMNS, but stops at line end.
  (let ((text       (group-text group))
	(gap-start  (group-gap-start group))
	(gap-end    (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-line-columns text start end column
				   tab-width char-image-strings))
	  ((fix:<= gap-start start)
	   (let ((i&c
		  (substring-line-columns text
					  (fix:+ start gap-length)
					  (fix:+ end gap-length)
					  column
					  tab-width
					  char-image-strings)))
	     (cons (fix:- (car i&c) gap-length) (cdr i&c))))
	  (else
	   (let ((i&c
		  (substring-line-columns text start gap-start
					  column tab-width
					  char-image-strings)))
	     (if (fix:< (car i&c) gap-start)
		 i&c
		 (let ((i&c
			(substring-line-columns text
						gap-end
						(fix:+ end gap-length)
						(cdr i&c)
						tab-width
						char-image-strings)))
		   (cons (fix:- (car i&c) gap-length) (cdr i&c)))))))))

(define (string-line-columns string column tab-width char-image-strings)
  (substring-line-columns string 0 (string-length string) column tab-width
			  char-image-strings))

(define (substring-line-columns string start end column tab-width
				char-image-strings)
  (if tab-width
      (let loop ((index start) (column column))
	(if (fix:= index end)
	    (cons index column)
	    (let ((ascii (vector-8b-ref string index)))
	      (if (fix:= ascii (char->integer #\newline))
		  (cons index column)
		  (loop (fix:+ index 1)
			(fix:+ column
			       (if (fix:= ascii (char->integer #\tab))
				   (fix:- tab-width
					  (fix:remainder column tab-width))
				   (string-length
				    (vector-ref char-image-strings
						ascii)))))))))
      (let loop ((index start) (column column))
	(if (fix:= index end)
	    (cons index column)
	    (let ((ascii (vector-8b-ref string index)))
	      (if (fix:= ascii (char->integer #\newline))
		  (cons index column)
		  (loop (fix:+ index 1)
			(fix:+ column
			       (string-length
				(vector-ref char-image-strings ascii))))))))))

(define (group-column->index group start end start-column column tab-width
			     char-image-strings)
  (let ((text       (group-text group))
	(gap-start  (group-gap-start group))
	(gap-end    (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-column->index text start end start-column column
				    tab-width char-image-strings))
	  ((fix:<= gap-start start)
	   (let ((result
		  (substring-column->index text
					   (fix:+ start gap-length)
					   (fix:+ end gap-length)
					   start-column
					   column
					   tab-width
					   char-image-strings)))
	     (vector-set! result 0 (fix:- (vector-ref result 0) gap-length))
	     result))
	  (else
	   (let ((result
		  (substring-column->index text start gap-start
					   start-column column tab-width
					   char-image-strings)))
	     (if (and (fix:< (vector-ref result 1) column)
		      (fix:= (vector-ref result 0) gap-start))
		 (let ((result
			(substring-column->index text
						 gap-end
						 (fix:+ end gap-length)
						 (fix:+ (vector-ref result 1)
							(vector-ref result 2))
						 column
						 tab-width
						 char-image-strings)))
		   (vector-set! result 0
				(fix:- (vector-ref result 0) gap-length))
		   result)
		 result))))))

(define (substring-column->index string start end start-column column
				 tab-width char-image-strings)
  ;; If COLUMN falls in the middle of a multi-column character, the
  ;; index returned is that of the character.  Thinking of the index
  ;; as a pointer between characters, the value is the pointer to the
  ;; left of the multi-column character.  Only if COLUMN reaches
  ;; across the character will the right-hand pointer be returned.
  ;; Various things depend on this.
  (if tab-width
      (let loop ((index start) (c start-column))
	(if (or (fix:= c column)
		(fix:= index end)
		(fix:= (char->integer #\newline) (vector-8b-ref string index)))
	    (vector index c 0)
	    (let ((c
		   (fix:+ c
			  (let ((ascii (vector-8b-ref string index)))
			    (if (fix:= ascii (char->integer #\tab))
				(fix:- tab-width (fix:remainder c tab-width))
				(string-length
				 (vector-ref char-image-strings ascii)))))))
	      (if (fix:> c column)
		  (vector index column (fix:- c column))
		  (loop (fix:+ index 1) c)))))
      (let loop ((index start) (c start-column))
	(if (or (fix:= c column)
		(fix:= index end)
		(fix:= (char->integer #\newline) (vector-8b-ref string index)))
	    (vector index c 0)
	    (let ((c
		   (fix:+ c
			  (string-length
			   (vector-ref char-image-strings
				       (vector-8b-ref string index))))))
	      (if (fix:> c column)
		  (vector index column (fix:- c column))
		  (loop (fix:+ index 1) c)))))))

(define (substring-image! string string-start string-end
			  image image-start image-end
			  tab-width column-offset results
			  char-image-strings)
  (let loop ((string-index string-start) (image-index image-start))
    (if (or (fix:= image-index image-end)
	    (fix:= string-index string-end))
	(begin
	  (vector-set! results 0 string-index)
	  (vector-set! results 1 image-index)
	  (vector-set! results 2 0))
	(let ((ascii (vector-8b-ref string string-index))
	      (partial
	       (lambda (partial)
		 (vector-set! results 0 string-index)
		 (vector-set! results 1 image-end)
		 (vector-set! results 2 partial))))
	  (if (and (fix:= ascii (char->integer #\tab)) tab-width)
	      (let ((n
		     (fix:- tab-width
			    (fix:remainder (fix:+ column-offset
						  image-index)
					   tab-width))))
		(let ((end (fix:+ image-index n)))
		  (if (fix:<= end image-end)
		      (begin
			(do ((image-index image-index
					  (fix:+ image-index 1)))
			    ((fix:= image-index end))
			  (string-set! image image-index #\space))
			(loop (fix:+ string-index 1) end))
		      (begin
			(do ((image-index image-index
					  (fix:+ image-index 1)))
			    ((fix:= image-index image-end))
			  (string-set! image image-index #\space))
			(partial (fix:- end image-end))))))
	      (let* ((image-string  (vector-ref char-image-strings ascii))
		     (image-len     (string-length image-string)))
		(string-set! image image-index (string-ref image-string 0))
		(if (fix:= image-len 1)
		    (loop (fix:+ string-index 1) (fix:+ image-index 1))
		    (if (fix:< (fix:+ image-index image-len) image-end)
			(let copy-image-loop ((i 1))
			  (string-set! image (fix:+ image-index i)
				       (string-ref image-string i))
			  (if (fix:= (fix:+ i 1) image-len)
			      (loop (fix:+ string-index 1)
				    (fix:+ image-index image-len))
			      (copy-image-loop (fix:+ i 1))))
			(let copy-image-loop ((i 1))
			  (cond ((fix:= i image-len)
				 (loop (fix:+ string-index 1)
				       (fix:+ image-index image-len)))
				((fix:= (fix:+ image-index i) image-end)
				 (partial (fix:- image-len i)))
				(else
				 (string-set! image (fix:+ image-index i)
					      (string-ref image-string i))
				 (copy-image-loop (fix:+ i 1)))))))))))))

(define (string-image string start-column tab-width char-image-strings)
  (substring-image string 0 (string-length string) start-column tab-width
		   char-image-strings))

(define (substring-image string start end start-column tab-width
			 char-image-strings)
  (let ((columns
	 (fix:- (substring-columns string start end start-column tab-width
				   char-image-strings)
		start-column)))
    (let ((image (make-string columns)))
      (substring-image! string start end
			image 0 columns
			tab-width start-column substring-image-results
			char-image-strings)
      image)))

(define substring-image-results
  (make-vector 3))

(define (group-image! group start end
		      image image-start image-end
		      tab-width column-offset results
		      char-image-strings)
  (let ((text       (group-text group))
	(gap-start  (group-gap-start group))
	(gap-end    (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-image! text start end
			     image image-start image-end
			     tab-width column-offset results
			     char-image-strings))
	  ((fix:<= gap-start start)
	   (substring-image! text
			     (fix:+ start gap-length) (fix:+ end gap-length)
			     image image-start image-end
			     tab-width column-offset results
			     char-image-strings)
	   (vector-set! results 0 (fix:- (vector-ref results 0) gap-length)))
	  (else
	   (substring-image! text start gap-start
			     image image-start image-end
			     tab-width column-offset results
			     char-image-strings)
	   (if (fix:< (vector-ref results 1) image-end)
	       (begin
		 (substring-image! text gap-end (fix:+ end gap-length)
				   image (vector-ref results 1) image-end
				   tab-width column-offset results
				   char-image-strings)
		 (vector-set! results 0
			      (fix:- (vector-ref results 0) gap-length))))))))

(define (partial-image! char n image image-start image-end tab-width
			char-image-strings)
  ;; Assume that (< IMAGE-START IMAGE-END) and that N is less than the
  ;; total width of the image for the character.
  (let ((ascii (char->integer char)))
    (if (and (fix:= ascii (char->integer #\tab)) tab-width)
	(let ((end
	       (let ((end (fix:+ image-start n)))
		 (if (fix:< end image-end) end image-end))))
	  (do ((image-index image-start (fix:+ image-index 1)))
	      ((fix:= image-index end))
	    (string-set! image image-index #\space)))
	(let ((picture (vector-ref char-image-strings ascii)))
	  (let ((end
		 (let ((end (fix:+ image-start n)))
		   (if (fix:< end image-end) end image-end))))
	    (string-set! image image-start (string-ref picture 1))
	    (let loop ((i           (fix:- (string-length picture) n))
		       (image-index image-start))
	      (if (fix:< image-index end)
		  (begin
		    (string-set! image image-index (string-ref picture i))
		    (loop (fix:+ i 1) (fix:+ image-index 1))))))))))

