;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/modefs.scm,v 1.114 1989/03/14 08:01:33 cph Exp $
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

;;;; Fundamental Mode

(declare (usual-integrations))

(define-command ("Fundamental Mode")
  "Make the current mode be Fundamental Mode.
All normal editing modes are defined relative to this mode."
  (set-current-major-mode! fundamental-mode))

(define-major-mode "Fundamental" #F
  "Major mode not specialized for anything in particular.
Most other major modes are defined by comparison to this one."
  (if (ref-variable "Fundamental Mode Hook")
      ((ref-variable "Fundamental Mode Hook"))))

(define-variable "Fundamental Mode Hook"
  "If not false, a thunk to call when entering Fundamental mode."
  false)

(define-variable "Editor Default Mode"
  "The default major mode for new buffers."
  fundamental-mode)

(define-variable "File Type to Major Mode"
  "Specifies the major mode for new buffers based on file type.
This is an alist, the cars of which are pathname types,
and the cdrs of which are major modes."
  `(("ASM" . ,(name->mode "Midas"))
    ("C" . ,(name->mode "C"))
    ("PAS" . ,(name->mode "Pascal"))
    ("S" . ,(name->mode "Scheme"))
    ("SCM" . ,(name->mode "Scheme"))
    ("TXI" . ,(name->mode "Texinfo"))
    ("TXT" . ,(name->mode "Text"))))

(define-default-key "Fundamental" "^R Bad Command")

(define-key "Fundamental" char-set:graphic "^R Insert Self")
(define-key "Fundamental" char-set:numeric "^R Autoargument Digit")
(define-key "Fundamental" #\- "^R Auto Negative Argument")

(define-key "Fundamental" #\Tab "^R Indent for Tab")
(define-key "Fundamental" #\Linefeed "^R Indent New Line")
(define-key "Fundamental" #\Page "^R New Window")
(define-key "Fundamental" #\Return "^R Newline")
(define-key "Fundamental" #\Altmode "^R Prefix Meta")
(define-key "Fundamental" #\Rubout "^R Backward Delete Character")

(define-key "Fundamental" #\C-Space "^R Set/Pop Mark")
;!"#$
(define-key "Fundamental" #\C-% "Replace String")
;'()*+,
(define-key "Fundamental" #\C-- "^R Negative Argument")
(define-key "Fundamental" #\C-. "Tags Loop Continue")
;/
(define-key "Fundamental" #\C-0 "^R Argument Digit")
(define-key "Fundamental" #\C-1 "^R Argument Digit")
(define-key "Fundamental" #\C-2 "^R Argument Digit")
(define-key "Fundamental" #\C-3 "^R Argument Digit")
(define-key "Fundamental" #\C-4 "^R Argument Digit")
(define-key "Fundamental" #\C-5 "^R Argument Digit")
(define-key "Fundamental" #\C-6 "^R Argument Digit")
(define-key "Fundamental" #\C-7 "^R Argument Digit")
(define-key "Fundamental" #\C-8 "^R Argument Digit")
(define-key "Fundamental" #\C-9 "^R Argument Digit")
;:
(define-key "Fundamental" #\C-\; "^R Indent for Comment")
(define-key "Fundamental" #\C-< "^R Mark Beginning")
(define-key "Fundamental" #\C-= "What Cursor Position")
(define-key "Fundamental" #\C-> "^R Mark End")
;?
(define-key "Fundamental" #\C-@ "^R Set/Pop Mark")
(define-key "Fundamental" #\C-A "^R Beginning of Line")
(define-key "Fundamental" #\C-B "^R Backward Character")
;C
(define-key "Fundamental" #\C-D "^R Delete Character")
(define-key "Fundamental" #\C-E "^R End of Line")
(define-key "Fundamental" #\C-F "^R Forward Character")
;GHIJ
(define-key "Fundamental" #\C-K "^R Kill Line")
;LM
(define-key "Fundamental" #\C-N "^R Down Real Line")
(define-key "Fundamental" #\C-O "^R Open Line")
(define-key "Fundamental" #\C-P "^R Up Real Line")
(define-key "Fundamental" #\C-Q "^R Quoted Insert")
(define-key "Fundamental" #\C-R "^R Reverse Search")
(define-key "Fundamental" #\C-S "^R Incremental Search")
(define-key "Fundamental" #\C-T "^R Transpose Characters")
(define-key "Fundamental" #\C-U "^R Universal Argument")
(define-key "Fundamental" #\C-V "^R Next Screen")
(define-key "Fundamental" #\C-W "^R Kill Region")
(define-prefix-key "Fundamental" #\C-X "^R Prefix Character")
(define-key "Fundamental" #\C-Y "^R Un-Kill")
(define-key "Fundamental" #\C-Z "^R Prefix Control-Meta")
;[\
(define-key "Fundamental" #\C-\] "Abort Recursive Edit")
(define-key "Fundamental" #\C-^ "^R Prefix Control")
(define-key "Fundamental" #\C-_ "Undo")
;`{|}~
(define-key "Fundamental" #\C-Rubout "^R Backward Delete Hacking Tabs")

(define-key "Fundamental" #\M-Backspace "^R Mark Definition")
(define-key "Fundamental" #\M-Tab "^R Tab")
(define-key "Fundamental" #\M-Linefeed "^R Indent New Comment Line")
(define-key "Fundamental" #\M-Page "^R Twiddle Buffers")
(define-key "Fundamental" #\M-Return "^R Back to Indentation")
;Altmode
(define-key "Fundamental" #\M-Space "^R Just One Space")
;!"#$
(define-key "Fundamental" #\M-% "Query Replace")
;'()*
(define-key "Fundamental" #\M-+ "Pascal Filer")
(define-key "Fundamental" #\M-, "Pascal Emulator")
(define-key "Fundamental" #\M-- "^R Autoargument")
(define-key "Fundamental" #\M-. "Find Tag")
(define-key "Fundamental" #\M-/ "Describe Command")
(define-key "Fundamental" #\M-0 "^R Autoargument")
(define-key "Fundamental" #\M-1 "^R Autoargument")
(define-key "Fundamental" #\M-2 "^R Autoargument")
(define-key "Fundamental" #\M-3 "^R Autoargument")
(define-key "Fundamental" #\M-4 "^R Autoargument")
(define-key "Fundamental" #\M-5 "^R Autoargument")
(define-key "Fundamental" #\M-6 "^R Autoargument")
(define-key "Fundamental" #\M-7 "^R Autoargument")
(define-key "Fundamental" #\M-8 "^R Autoargument")
(define-key "Fundamental" #\M-9 "^R Autoargument")
;:
(define-key "Fundamental" #\M-\; "^R Indent for Comment")
(define-key "Fundamental" #\M-< "^R Goto Beginning")
(define-key "Fundamental" #\M-= "^R Count Lines Region")
(define-key "Fundamental" #\M-> "^R Goto End")
(define-key "Fundamental" #\M-? "Describe Command")
(define-key "Fundamental" #\M-@ "^R Mark Word")
(define-key "Fundamental" #\M-A "^R Backward Sentence")
(define-key "Fundamental" #\M-B "^R Backward Word")
(define-key "Fundamental" #\M-C "^R Uppercase Initial")
(define-key "Fundamental" #\M-D "^R Kill Word")
(define-key "Fundamental" #\M-E "^R Forward Sentence")
(define-key "Fundamental" #\M-F "^R Forward Word")
;(define-key "Fundamental" #\M-G "^R Fill Region")
(define-key "Fundamental" #\M-H "^R Mark Paragraph")
(define-key "Fundamental" #\M-I "^R Tab to Tab Stop")
(define-key "Fundamental" #\M-J "^R Indent New Comment Line")
(define-key "Fundamental" #\M-K "^R Kill Sentence")
(define-key "Fundamental" #\M-L "^R Lowercase Word")
(define-key "Fundamental" #\M-M "^R Back to Indentation")
;NOP
(define-key "Fundamental" #\M-Q "^R Fill Paragraph")
(define-key "Fundamental" #\M-R "^R Move to Screen Edge")
;S
(define-key "Fundamental" #\M-T "^R Transpose Words")
(define-key "Fundamental" #\M-U "^R Uppercase Word")
(define-key "Fundamental" #\M-V "^R Previous Screen")
(define-key "Fundamental" #\M-W "^R Copy Region")
(define-key "Fundamental" #\M-X "^R Extended Command")
(define-key "Fundamental" #\M-Y "^R Un-Kill Pop")
;Z
(define-key "Fundamental" #\M-\[ "^R Backward Paragraph")
(define-key "Fundamental" #\M-\\ "^R Delete Horizontal Space")
(define-key "Fundamental" #\M-\] "^R Forward Paragraph")
(define-key "Fundamental" #\M-^ "^R Delete Indentation")
;_`{|}
(define-key "Fundamental" #\M-~ "^R Buffer Not Modified")
(define-key "Fundamental" #\M-Rubout "^R Backward Kill Word")

(define-key "Fundamental" #\C-M-Space "^R Mark Sexp")
(define-key "Fundamental" #\C-M-0 "^R Argument Digit")
(define-key "Fundamental" #\C-M-1 "^R Argument Digit")
(define-key "Fundamental" #\C-M-2 "^R Argument Digit")
(define-key "Fundamental" #\C-M-3 "^R Argument Digit")
(define-key "Fundamental" #\C-M-4 "^R Argument Digit")
(define-key "Fundamental" #\C-M-5 "^R Argument Digit")
(define-key "Fundamental" #\C-M-6 "^R Argument Digit")
(define-key "Fundamental" #\C-M-7 "^R Argument Digit")
(define-key "Fundamental" #\C-M-8 "^R Argument Digit")
(define-key "Fundamental" #\C-M-9 "^R Argument Digit")
(define-key "Fundamental" #\C-M-- "^R Negative Argument")

(define-key "Fundamental" #\C-M-\\ "^R Indent Region")
(define-key "Fundamental" #\C-M-^ "^R Delete Indentation")
(define-key "Fundamental" #\C-M-\( "^R Backward Up List")
(define-key "Fundamental" #\C-M-\) "^R Forward Up List")
(define-key "Fundamental" #\C-M-@ "^R Mark Sexp")
(define-key "Fundamental" #\C-M-\; "^R Kill Comment")

(define-key "Fundamental" #\C-M-A "^R Beginning of Definition")
(define-key "Fundamental" #\C-M-B "^R Backward Sexp")
(define-key "Fundamental" #\C-M-C "^R Exit")
(define-key "Fundamental" #\C-M-D "^R Forward Down List")
(define-key "Fundamental" #\C-M-E "^R End of Definition")
(define-key "Fundamental" #\C-M-F "^R Forward Sexp")
;GHIJ
(define-key "Fundamental" #\C-M-K "^R Kill Sexp")
;LM
(define-key "Fundamental" #\C-M-N "^R Forward List")
(define-key "Fundamental" #\C-M-O "^R Split Line")
(define-key "Fundamental" #\C-M-P "^R Backward List")
;Q
(define-key "Fundamental" #\C-M-R "^R Reposition Window")
;S
(define-key "Fundamental" #\C-M-T "^R Transpose Sexps")
(define-key "Fundamental" #\C-M-U "^R Backward Up List")
(define-key "Fundamental" #\C-M-V "^R Scroll Other Window")
(define-key "Fundamental" #\C-M-W "^R Append Next Kill")
;XYZ
(define-key "Fundamental" #\C-M-Rubout "^R Backward Kill Sexp")

;Backspace
(define-key "Fundamental" '(#\C-X #\Tab) "^R Indent Rigidly")
;Linefeed
(define-key "Fundamental" '(#\C-X #\Page) "^R Lowercase Region")
;Return,Altmode
;A
(define-key "Fundamental" '(#\C-X #\C-B) "List Buffers")
;C
(define-key "Fundamental" '(#\C-X #\C-D) "List Directory")
(define-key "Fundamental" '(#\C-X #\C-E) "^R Evaluate Previous Sexp")
(define-key "Fundamental" '(#\C-X #\C-F) "Find File")
;GHIJKLM
(define-key "Fundamental" '(#\C-X #\C-N) "^R Set Goal Column")
(define-key "Fundamental" '(#\C-X #\C-O) "^R Delete Blank Lines")
(define-key "Fundamental" '(#\C-X #\C-P) "^R Mark Page")
(define-key "Fundamental" '(#\C-X #\C-Q) "Toggle Read Only")
;R
(define-key "Fundamental" '(#\C-X #\C-S) "^R Save File")
(define-key "Fundamental" '(#\C-X #\C-T) "^R Transpose Lines")
(define-key "Fundamental" '(#\C-X #\C-U) "^R Uppercase Region")
(define-key "Fundamental" '(#\C-X #\C-V) "^R Find Alternate File")
(define-key "Fundamental" '(#\C-X #\C-W) "Write File")
(define-key "Fundamental" '(#\C-X #\C-X) "^R Exchange Point and Mark")
(define-key "Fundamental" '(#\C-X #\C-Z) "^R Return to Superior")
;!"#$%&'
(define-key "Fundamental" '(#\C-X #\() "Start Keyboard Macro")
(define-key "Fundamental" '(#\C-X #\)) "End Keyboard Macro")
;*+,-
(define-key "Fundamental" '(#\C-X #\.) "^R Set Fill Prefix")
(define-key "Fundamental" '(#\C-X #\/) "Point to Register")
(define-key "Fundamental" '(#\C-X #\0) "^R Delete Window")
(define-key "Fundamental" '(#\C-X #\1) "^R Delete Other Windows")
(define-key "Fundamental" '(#\C-X #\2) "^R Split Window Vertically")
(define-key "Fundamental" '(#\C-X #\3) "Kill Pop Up Buffer")
(define-prefix-key "Fundamental" '(#\C-X #\4) "^R Prefix Character")
(define-key "Fundamental" '(#\C-X #\4 #\.) "Find Tag Other Window")
(define-key "Fundamental" '(#\C-X #\4 #\B) "Select Buffer Other Window")
(define-key "Fundamental" '(#\C-X #\4 #\D) "Dired Other Window")
(define-key "Fundamental" '(#\C-X #\4 #\F) "Find File Other Window")
(define-key "Fundamental" '(#\C-X #\5) "^R Split Window Horizontally")
;:
(define-key "Fundamental" '(#\C-X #\;) "^R Set Comment Column")
;<
(define-key "Fundamental" '(#\C-X #\=) "What Cursor Position")
;>?A
(define-key "Fundamental" '(#\C-X #\B) "Select Buffer")
;C
(define-key "Fundamental" '(#\C-X #\D) "Dired")
(define-key "Fundamental" '(#\C-X #\E) "Call Last Keyboard Macro")
(define-key "Fundamental" '(#\C-X #\F) "^R Set Fill Column")
(define-key "Fundamental" '(#\C-X #\G) "Insert Register")
(define-key "Fundamental" '(#\C-X #\H) "^R Mark Whole Buffer")
(define-key "Fundamental" '(#\C-X #\I) "Insert File")
(define-key "Fundamental" '(#\C-X #\J) "Register to Point")
(define-key "Fundamental" '(#\C-X #\K) "Kill Buffer")
(define-key "Fundamental" '(#\C-X #\L) "^R Count Lines Page")
;M
;(define-key "Fundamental" '(#\C-X #\N) "^R Narrow Bounds to Region")
(define-key "Fundamental" '(#\C-X #\O) "^R Other Window")
;(define-key "Fundamental" '(#\C-X #\P) "^R Narrow Bounds to Page")
(define-key "Fundamental" '(#\C-X #\Q) "Keyboard Macro Query")
(define-key "Fundamental" '(#\C-X #\R) "Copy Rectangle to Register")
(define-key "Fundamental" '(#\C-X #\S) "Save Some Buffers")
;(define-key "Fundamental" '(#\C-X #\T) "^R Transpose Regions")
(define-key "Fundamental" '(#\C-X #\U) "Undo")
(define-key "Fundamental" '(#\C-X #\V) "^R Screen Video")
(define-key "Fundamental" '(#\C-X #\W) "^R Widen Bounds")
(define-key "Fundamental" '(#\C-X #\X) "Copy to Register")
;Y
(define-key "Fundamental" '(#\C-X #\Z) "^R Scheme")
(define-key "Fundamental" '(#\C-X #\[) "^R Previous Page")
;\
(define-key "Fundamental" '(#\C-X #\]) "^R Next Page")
(define-key "Fundamental" '(#\C-X #\^) "^R Enlarge Window Vertically")
;_`
(define-key "Fundamental" '(#\C-X #\{) "^R Shrink Window Horizontally")
;|
(define-key "Fundamental" '(#\C-X #\}) "^R Enlarge Window Horizontally")
;~
(define-key "Fundamental" '(#\C-X #\Rubout) "^R Backward Kill Sentence")