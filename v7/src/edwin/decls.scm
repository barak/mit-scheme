#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/decls.scm,v 1.37 1992/05/12 15:45:23 mhwu Exp $

Copyright (c) 1989-92 Massachusetts Institute of Technology

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
	    '("ansi"
	      "class"
	      "clscon"
	      "clsmac"
	      "display"
	      "key"
	      "macros"
	      "make"
	      "nvector"
	      "paths"
	      "rename"
	      "rgxcmp"
	      "ring"
	      "strpad"
	      "strtab"
	      "termcap"
	      "utils"
	      "winren"
	      "xform"
	      "xterm"))
  (sf-global "tterm" "termcap")
  (let ((includes '("struct" "comman" "modes" "buffer" "edtstr")))
    (let loop ((files includes) (includes '()))
      (if (not (null? files))
	  (begin
	    (apply sf-edwin (car files) includes)
	    (loop (cdr files) (cons (car files) includes)))))
    (for-each (lambda (filename)
		(apply sf-edwin filename includes))
	      '("argred"
		"autold"
		"autosv"
		"basic"
		"bochser"
		"bochsmod"
		"bufcom"
		"bufinp"
		"bufmnu"
		"bufout"
		"bufset"
		"c-mode"
		"calias"
		"cinden"
		"comint"
		"compile"
		"comtab"
		"comred"
		"curren"
		"debug"
		"debuge"
		"dired"
		"dos"
		"dosproc"
		"ed-ffi"
		"editor"
		"evlcom"
		"filcom"
		"fileio"
		"fill"
		"grpops"
		"hlpcom"
		"image"
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
		"malias"
		"manual"
		"midas"
		"modefs"
		"modlin"
		"motcom"
		"motion"
		"notify"
		"outline"
		"occur"
		"pasmod"
		"print"
		"process"
		"prompt"
		"rcs"
		"reccom"
		"regcom"
		"regexp"
		"regops"
		"replaz"
		"rmail"
		"rmailsrt"
		"rmailsum"
		"schmod"
		"scrcom"
		"screen"
		"search"
		"sendmail"
		"sercom"
		"shell"
		"simple"
		"syntax"
		"tags"
		"techinfo"
		"telnet"
		"texcom"
		"things"
		"tparse"
		"tximod"
		"undo"
		"unix"
		"wincom"
		"winout"
		"xcom")))
  (for-each sf-class
	    '("comwin"
	      "modwin"
	      "edtfrm"))
  (sf-class "window" "class")
  (sf-class "utlwin" "window" "class")
  (sf-class "bufwin" "utlwin" "window" "class" "buffer" "struct")
  (sf-class "bufwfs" "bufwin" "utlwin" "window" "class" "buffer" "struct")
  (sf-class "bufwiu" "bufwin" "utlwin" "window" "class" "buffer" "struct")
  (sf-class "bufwmc" "bufwin" "utlwin" "window" "class" "buffer" "struct")
  (sf-class "buffrm" "bufwin" "window" "class" "struct"))
