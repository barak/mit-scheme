#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/decls.scm,v 1.10 1989/08/29 21:39:43 cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Edwin: Syntaxing Declarations

(declare (usual-integrations))

(let* ((scm-file (lambda (file) (string-append file ".scm")))
       (bin-file (lambda (file) (string-append file ".bin")))
       (bin-time (lambda (file) (file-modification-time (bin-file file))))
       (sf-dependent
	(lambda (syntax-table)
	  (lambda (source . dependencies)
	    (let ((reasons
		   (let ((source-time (bin-time source)))
		     (append
		      (if (not (file-processed? source "scm" "bin"))
			  (list (scm-file source))
			  '())
		      (map bin-file
			   (list-transform-positive dependencies
			     (if source-time
				 (lambda (dependency)
				   (< source-time (bin-time dependency)))
				 (lambda (dependency)
				   dependency ;ignore
				   true))))))))
	      (if (not (null? reasons))
		  (begin
		    (newline)
		    (write-string "Processing ")
		    (write source)
		    (write-string " because of:")
		    (for-each (lambda (reason)
				(write-char #\space)
				(write reason))
			      reasons)
		    (fluid-let ((sf/default-syntax-table
				 (lexical-reference (->environment '(EDWIN))
						    syntax-table))
				(sf/default-declarations
				 (map (lambda (dependency)
					`(integrate-external ,dependency))
				      dependencies)))
		      (sf source))))))))
       (sf-global (sf-dependent 'syntax-table/system-internal))
       (sf-edwin (sf-dependent 'edwin-syntax-table))
       (sf-class (sf-dependent 'class-syntax-table)))
  (for-each sf-global
	    '("bufinp"
	      "bufott"
	      "bufout"
	      "class"
	      "clscon"
	      "clsmac"
	      "comtab"
	      "cterm"
	      "display"
	      "entity"
	      "image"
	      "macros"
	      "make"
	      "nvector"
	      "paths"
	      "rename"
	      "rgxcmp"
	      "ring"
	      "screen"
	      "search"
	      "simple"
	      "strpad"
	      "strtab"
	      "utils"
	      "winout"
	      "winren"
	      "xform"
	      "xterm"))
  (for-each sf-edwin
	    '("argred"
	      "autold"
	      "autosv"
	      "basic"
	      "bufcom"
	      "buffer"
	      "bufmnu"
	      "bufset"
	      "c-mode"
	      "calias"
	      "cinden"
	      "comman"
	      "comred"
	      "curren"
	      "debug"
	      "debuge"
	      "dired"
	      "ed-ffi"
	      "editor"
	      "edtstr"
	      "evlcom"
	      "filcom"
	      "fileio"
	      "fill"
	      "hlpcom"
	      "info"
	      "input"
	      "intmod"
	      "iserch"
	      "keymap"
	      "kilcom"
	      "kmacro"
	      "lincom"
	      "linden"
	      "loadef"
	      "lspcom"
	      "midas"
	      "modefs"
	      "modes"
	      "modlin"
	      "motcom"
	      "pasmod"
	      "prompt"
	      "reccom"
	      "regcom"
	      "regexp"
	      "replaz"
	      "schmod"
	      "sercom"
	      "struct"
	      "syntax"
	      "tags"
	      "texcom"
	      "things"
	      "tparse"
	      "tximod"
	      "undo"
	      "unix"
	      "wincom"
	      "xcom"))
  (for-each sf-class
	    '("comwin"
	      "modwin"
	      "buffrm"
	      "edtfrm"
	      "winmis"
	      "rescrn"))
  (sf-edwin "grpops" "struct")
  (sf-edwin "regops" "struct")
  (sf-edwin "motion" "struct")
  (sf-class "window" "class")
  (sf-class "utlwin" "window" "class")
  (sf-class "linwin" "window" "class")
  (sf-class "bufwin" "window" "class" "struct")
  (sf-class "bufwfs" "bufwin" "window" "class" "struct")
  (sf-class "bufwiu" "bufwin" "window" "class" "struct")
  (sf-class "bufwmc" "bufwin" "window" "class" "struct"))