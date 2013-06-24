#| -*-Scheme-*-

$Id: decls.scm,v 1.73 2003/02/14 18:28:11 cph Exp $

Copyright (c) 1989-2001 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Edwin: Syntaxing Declarations

(declare (usual-integrations))

(let* ((scm-file (lambda (file) (string-append file ".scm")))
       (bin-file (lambda (file) (string-append file ".bin")))
       (bin-time (lambda (file) (file-modification-time (bin-file file))))
       (sf-dependent
	(lambda (environment)
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
				   (let ((bin-time (bin-time dependency)))
				     (or (not bin-time)
					 (< source-time bin-time))))
				 (lambda (dependency)
				   dependency ;ignore
				   true))))))))
	      (if (not (null? reasons))
		  (begin
		    (fresh-line)
		    (write-string "Processing ")
		    (write source)
		    (write-string " because of:")
		    (for-each (lambda (reason)
				(write-char #\space)
				(write reason))
			      reasons)
		    (newline)
		    (fluid-let ((sf/default-syntax-table environment)
				(sf/default-declarations
				 (map (lambda (dependency)
					`(integrate-external ,dependency))
				      dependencies)))
		      (sf source))))))))
       (sf-edwin (sf-dependent (->environment '(EDWIN))))
       (sf-class (sf-dependent (->environment '(EDWIN WINDOW)))))
  (for-each sf-edwin
	    '("ansi"
	      "bios"
	      "class"
	      "clscon"
	      "clsmac"
	      "comatch"
	      "display"
	      "key-w32"
	      "key-x11"
	      "macros"
	      "make"
	      "nntp"
	      "nvector"
	      "os2term"
	      "paths"
	      "rcsparse"
	      "rename"
	      "rfc822"
	      "ring"
	      "strpad"
	      "strtab"
	      "termcap"
	      "utils"
	      "win32"
	      "winren"
	      "xform"
	      "xterm"))
  (sf-edwin "tterm" "termcap")
  (let ((includes '("struct" "comman" "modes" "buffer" "edtstr")))
    (let loop ((files includes) (includes '()))
      (if (pair? files)
	  (begin
	    (apply sf-edwin (car files) includes)
	    (loop (cdr files) (cons (car files) includes)))))
    (for-each (lambda (filename)
		(apply sf-edwin filename includes))
	      '("abbrev"
		"argred"
		"artdebug"
		"autold"
		"autosv"
		"basic"
		;;"bochser"
		;;"bochsmod"
		"bufcom"
		"bufinp"
		"bufmnu"
		"bufout"
		"bufset"
		"c-mode"
		"calias"
		"cinden"
		"comhst"
		"comint"
		"compile"
		"comtab"
		"comred"
		"curren"
		"dabbrev"
		"debian-changelog"
		"debug"
		"debuge"
		"dired"
		"diros2"
		"dirunx"
		"dirw32"
		"docstr"
		"dos"
		"doscom"
		"dosfile"
		"dosproc"
		"dosshell"
		"ed-ffi"
		"editor"
		"evlcom"
		"eystep"
		"filcom"
		"fileio"
		"fill"
		"grpops"
		"hlpcom"
		"htmlmode"
		"image"
		"info"
		"input"
		"intmod"
		"iserch"
		"javamode"
		"keymap"
		"keyparse"
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
		"mousecom"
		"notify"
		"outline"
		"occur"
		"os2"
		"os2com"
		"pasmod"
		"print"
		"process"
		"prompt"
		"pwedit"
		"pwparse"
		;;"rcs"
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
		"snr"
		"sort"
		"syntax"
		"tagutl"
		"techinfo"
		"telnet"
		"texcom"
		"things"
		"tparse"
		"tximod"
		"txtprp"
		"undo"
		"unix"
		"vc"
		"verilog"
		"vhdl"
		"webster"
		"wincom"
		"winout"
		"xcom"
		"win32com"
		"xmodef")))
  (for-each sf-class
	    '("comwin"
	      "modwin"
	      "edtfrm"))
  (sf-class "window" "class")
  (sf-class "utlwin" "window" "class")
  (sf-class "bufwin" "window" "class" "buffer" "struct")
  (sf-class "bufwfs" "bufwin" "window" "class" "buffer" "struct")
  (sf-class "bufwiu" "bufwin" "window" "class" "buffer" "struct")
  (sf-class "bufwmc" "bufwin" "window" "class" "buffer" "struct")
  (sf-class "buffrm" "bufwin" "window" "class" "struct"))