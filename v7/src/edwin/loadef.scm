;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/loadef.scm,v 1.7 1990/10/03 04:55:26 cph Rel $
;;;
;;;	Copyright (c) 1986, 1989, 1990 Massachusetts Institute of Technology
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

;;;; Autoload Definitions

(declare (usual-integrations))

;;;; Various Libraries

(define-library 'INFO
  '("info" (EDWIN INFO)))

(define-variable info-enable-edit
  "If true, the \\[info-edit] command in Info can edit the current node."
  false)

(define-variable info-enable-active-nodes
  "If true, allows Info to execute Scheme code associated with nodes.
The Scheme code is executed when the node is selected."
  true)

(define-variable info-directory
  "If not false, default directory for Info documentation files.
Otherwise the standard directory is used."
  false)

(define-variable info-previous-search
  "Default search string for Info \\[info-search] command to search for."
  false)

(define-autoload-command 'info 'INFO
  "Create a buffer for Info, the documentation browser program.")

(define-library 'DIRED
  '("dired" (EDWIN DIRED)))

(define-variable list-directory-unpacked
  "If not false, \\[list-directory] puts one file on each line.
Normally it packs many onto a line.
This has no effect if \\[list-directory] is invoked with an argument."
  false)

(define-autoload-command 'dired 'DIRED
  "Edit a directory.  You type the directory name.")

(define-autoload-command 'dired-other-window 'DIRED
  "Edit a directory in another window.  You type the directory name.")

(define-autoload-command 'list-directory 'DIRED
  "Generate a directory listing.")

(define-autoload-procedure 'make-dired-buffer '(EDWIN DIRED) 'DIRED)

(define-library 'RECTANGLE-COMMANDS
  '("reccom" (EDWIN RECTANGLE)))

(define-autoload-command 'kill-rectangle 'RECTANGLE-COMMANDS
  "Delete rectangle with corners at point and mark; save as last killed one.")

(define-autoload-command 'delete-rectangle 'RECTANGLE-COMMANDS
  "Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends.")

(define-autoload-command 'open-rectangle 'RECTANGLE-COMMANDS
  "Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle.")

(define-autoload-command 'clear-rectangle 'RECTANGLE-COMMANDS
  "Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks.")

(define-autoload-command 'yank-rectangle 'RECTANGLE-COMMANDS
  "Yank the last killed rectangle with upper left corner at point.")

(define-autoload-procedure 'delete-rectangle '(EDWIN RECTANGLE)
  'RECTANGLE-COMMANDS)

(define-autoload-procedure 'yank-rectangle '(EDWIN RECTANGLE)
  'RECTANGLE-COMMANDS)

(define-library 'COMMAND-SUMMARY
  '("keymap" (EDWIN COMMAND-SUMMARY)))

(define-autoload-command 'make-command-summary 'COMMAND-SUMMARY
  "Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first.")

(define-autoload-command 'describe-bindings 'COMMAND-SUMMARY
  "Show a list of all defined keys, and their definitions.
The list is put in a buffer, which is displayed.")

;;;; Tags Package

(define-library 'TAGS
  '("tags" (EDWIN TAGS)))

(define-variable tags-table-pathname
  "Pathname of current tags table."
  false)

(define-autoload-command 'visit-tags-table 'TAGS
  "Tell tags commands to use a given tags table file.")

(define-autoload-command 'find-tag 'TAGS
  "Find tag (in current tags table) whose name contains a given string.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 With argument, searches for the next tag in the tags table that matches
the string used in the previous Find Tag.")

(define-autoload-command 'find-tag-other-window 'TAGS
  "Like \\[find-tag], but selects buffer in another window.")

(define-autoload-command 'tags-search 'TAGS
  "Search through all files listed in tag table for a given string.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].")

(define-autoload-command 're-tags-search 'TAGS
  "Search through all files listed in tag table for a given regexp.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].")

(define-autoload-command 'tags-query-replace 'TAGS
  "Query replace a given string with another one though all files listed
in tag table.  If you exit (C-G or Altmode), you can resume the query
replace with the command \\[tags-loop-continue].")

(define-autoload-command 'tags-loop-continue 'TAGS
  "Continue last \\[tags-search] or \\[tags-query-replace] command.")

;;;; Major Mode Libraries

(define-library 'MIDAS-MODE
  '("midas" (EDWIN)))

(define-autoload-major-mode 'midas 'fundamental "Midas" 'MIDAS-MODE
  "Major mode for editing assembly code.")

(define-autoload-command 'midas-mode 'MIDAS-MODE
  "Enter Midas mode.")

(define-variable midas-mode-hook
  "An event distributor that is invoked when entering Midas mode."
  (make-event-distributor))

(define-library 'PASCAL-MODE
  '("pasmod" (EDWIN)))

(define-autoload-major-mode 'pascal 'fundamental "Pascal" 'PASCAL-MODE
  "Major mode specialized for editing Pascal code.")

(define-autoload-command 'pascal-mode 'PASCAL-MODE
  "Enter Pascal mode.")

(define-variable pascal-mode-hook
  "An event distributor that is invoked when entering Pascal mode."
  (make-event-distributor))

(define-variable pascal-shift-increment
  "Indentation increment for Pascal Shift commands."
  2)

(define-variable pascal-indentation-keywords
  "These keywords cause the lines below them to be indented to the right.
This must be a regular expression, or #F to disable the option."
  false)

(define-library 'TEXINFO-MODE
  '("tximod" (EDWIN)))

(define-autoload-major-mode 'texinfo 'text "Texinfo" 'TEXINFO-MODE
  "Major mode for editing texinfo files.
These are files that are input for TeX and also to be turned
into Info files by \\[texinfo-format-buffer].
These files must be written in a very restricted and
modified version of TeX input format.")

(define-autoload-command 'texinfo-mode 'TEXINFO-MODE
  "Make the current mode be Texinfo mode.")

(define-variable texinfo-mode-hook
  "An event distributor that is invoked when entering Texinfo mode."
  (make-event-distributor))

(define-library 'C-MODE
  '("c-mode" (EDWIN))
  '("cinden" (EDWIN C-INDENTATION)))

(define-autoload-major-mode 'c 'fundamental "C" 'C-MODE
  "Major mode for editing C code.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments are delimited with /* ... */.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
The characters { } ; : correct indentation when typed.

Variables controlling indentation style:
 c-auto-newline
    Non-false means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-brace-offset
    Extra indentation for line if it starts with an open brace.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or default.")

(define-autoload-command 'c-mode 'C-MODE
  "Enter C mode.")

(define-variable c-mode-hook
  "An event distributor that is invoked when entering C mode."
  (make-event-distributor))

(define-variable c-indent-level
  "Indentation of C statements with respect to containing block."
  2)

(define-variable c-brace-offset
  "Extra indentation for braces, compared with other text in same context."
  0)

(define-variable c-brace-imaginary-offset
  "Imagined indentation of a C open brace that actually follows a statement."
  0)

(define-variable c-argdecl-indent
  "Indentation level of declarations of C function arguments."
  5)

(define-variable c-label-offset
  "Offset of C label lines and case statements relative to usual indentation."
  -2)

(define-variable c-continued-statement-offset
  "Extra indent for lines not starting new statements."
  2)

(define-variable c-auto-newline
  "Non-false means automatically newline before and after braces,
and after colons and semicolons, inserted in C code."
  false)