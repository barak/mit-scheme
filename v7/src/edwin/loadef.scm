;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/loadef.scm,v 1.1 1989/03/14 08:08:54 cph Exp $
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

;;;; Autoload Definitions

(declare (usual-integrations))

;;;; Various Libraries

(define-library 'INFO
  '("info" (EDWIN INFO)))

(define-variable "Info Enable Edit"
  "If true, the \\[^R Info Edit] command in Info can edit the current node."
  false)

(define-variable "Info Enable Active Nodes"
  "If true, allows Info to execute Scheme code associated with nodes.
The Scheme code is executed when the node is selected."
  true)

(define-variable "Info Directory"
  "Default directory pathname for Info documentation files."
  edwin-info-directory)
(define-variable "Info Previous Search"
  "Default search string for Info \\[^R Info Search] command to search for."
  false)

(define-variable "Info Tag Table Start" "")
(define-variable "Info Tag Table End" "")

(define-autoload-command "Info" 'INFO
  "Create a buffer for Info, the documentation browser program.")

(define-library 'DIRED
  '("dired" (EDWIN DIRED)))

(define-variable "List Directory Unpacked"
  "If not false, \\[List Directory] puts one file on each line.
Normally it packs many onto a line.
This has no effect if \\[List Directory] is invoked with an argument."
  false)

(define-autoload-command "Dired" 'DIRED
  "Edit a directory.  You type the directory name.")

(define-autoload-command "Dired Other Window" 'DIRED
  "Edit a directory in another window.  You type the directory name.")

(define-autoload-command "List Directory" 'DIRED
  "Generate a directory listing.")

(define-library 'RECTANGLE-COMMANDS
  '("reccom" (EDWIN RECTANGLE)))

(define-autoload-command "Kill Rectangle" 'RECTANGLE-COMMANDS
  "Delete rectangle with corners at point and mark; save as last killed one.")

(define-autoload-command "Delete Rectangle" 'RECTANGLE-COMMANDS
  "Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends.")

(define-autoload-command "Open Rectangle" 'RECTANGLE-COMMANDS
  "Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle.")

(define-autoload-command "Clear Rectangle" 'RECTANGLE-COMMANDS
  "Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks.")

(define-autoload-command "Yank Rectangle" 'RECTANGLE-COMMANDS
  "Yank the last killed rectangle with upper left corner at point.")

(define-autoload-procedure '(EDWIN RECTANGLE) 'delete-rectangle
  'RECTANGLE-COMMANDS)

(define-autoload-procedure '(EDWIN RECTANGLE) 'yank-rectangle
  'RECTANGLE-COMMANDS)

(define-library 'COMMAND-SUMMARY
  '("keymap" (EDWIN COMMAND-SUMMARY)))

(define-autoload-command "Make Command Summary" 'COMMAND-SUMMARY
  "Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first.")
;;;; Tags Package

(define-library 'TAGS
  '("tags" (EDWIN TAGS)))

(define-variable "Tags Table Pathname"
  "Pathname of current tags table."
  false)

(define-autoload-command "Visit Tags Table" 'TAGS
  "Tell tags commands to use a given tags table file.")

(define-autoload-command "Find Tag" 'TAGS
  "Find tag (in current tags table) whose name contains a given string.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 With argument, searches for the next tag in the tags table that matches
the string used in the previous Find Tag.")

(define-autoload-command "Find Tag Other Window" 'TAGS
  "Like \\[Find Tag], but selects buffer in another window.")

(define-autoload-command "Generate Tags Table" 'TAGS
  "Generate a tags table from a files list of Scheme files.
 A files list is a file containing only strings which are file names.
 The generated tags table has the same name as the files list, except that
the file type is TAG.")

(define-autoload-command "Tags Search" 'TAGS
  "Search through all files listed in tag table for a given string.
Stops when a match is found.
To continue searching for next match, use command \\[Tags Loop Continue].")

(define-autoload-command "RE Tags Search" 'TAGS
  "Search through all files listed in tag table for a given regexp.
Stops when a match is found.
To continue searching for next match, use command \\[Tags Loop Continue].")

(define-autoload-command "Tags Query Replace" 'TAGS
  "Query replace a given string with another one though all files listed
in tag table.  If you exit (C-G or Altmode), you can resume the query
replace with the command \\[Tags Loop Continue].")

(define-autoload-command "Tags Loop Continue" 'TAGS
  "Continue last \\[Tags Search] or \\[Tags Query Replace] command.")

;;;; Major Mode Libraries

(define-library 'MIDAS-MODE
  '("midas" (EDWIN)))

(define-autoload-major-mode "Midas" "Fundamental" 'MIDAS-MODE
  "Major mode for editing assembly code.")

(define-autoload-command "Midas Mode" 'MIDAS-MODE
  "Enter Midas mode.")

(define-variable "Midas Mode Hook"
  "If not false, a thunk to call when entering Midas mode."
  false)

(define-library 'PASCAL-MODE
  '("pasmod" (EDWIN)))

(define-autoload-major-mode "Pascal" "Fundamental" 'PASCAL-MODE
  "Major mode specialized for editing Pascal code.")

(define-autoload-command "Pascal Mode" 'PASCAL-MODE
  "Enter Pascal mode.")

(define-variable "Pascal Mode Hook"
  "If not false, a thunk to call when entering Pascal mode."
  false)

(define-variable "Pascal Shift Increment"
  "Indentation increment for Pascal Shift commands."
  2)

(define-variable "Pascal Indentation Keywords"
  "These keywords cause the lines below them to be indented to the right.
This must be a regular expression, or #F to disable the option."
  false)

(define-library 'TEXINFO-MODE
  '("tximod" (EDWIN)))

(define-autoload-major-mode "Texinfo" "Text" 'TEXINFO-MODE
  "Major mode for editing texinfo files.
These are files that are input for TeX and also to be turned
into Info files by \\[Texinfo Format Buffer].
These files must be written in a very restricted and
modified version of TeX input format.")

(define-autoload-command "Texinfo Mode" 'TEXINFO-MODE
  "Make the current mode be Texinfo mode.")

(define-variable "Texinfo Mode Hook"
  "A procedure to be called when Texinfo mode is entered, or false."
  false)

(define-library 'C-MODE
  '("c-mode" (EDWIN))
  '("cinden" (EDWIN C-INDENTATION)))

(define-autoload-major-mode "C" "Fundamental" 'C-MODE
  "Major mode for editing C code.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments are delimited with /* ... */.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
The characters { } ; : correct indentation when typed.

Variables controlling indentation style:
 C Auto Newline
    Non-false means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 C Indent Level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 C Continued Statement Offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 C Brace Offset
    Extra indentation for line if it starts with an open brace.
 C Brace Imaginary Offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 C Argdecl Indent
    Indentation level of declarations of C function arguments.
 C Label Offset
    Extra indentation for line that is a label, or case or default.")

(define-autoload-command "C Mode" 'C-MODE
  "Enter C mode.")

(define-variable "C Mode Hook"
  "If not false, a thunk to call when entering C mode."
  false)

(define-variable "C Indent Level"
  "Indentation of C statements with respect to containing block."
  2)

(define-variable "C Brace Offset"
  "Extra indentation for braces, compared with other text in same context."
  0)

(define-variable "C Brace Imaginary Offset"
  "Imagined indentation of a C open brace that actually follows a statement."
  0)

(define-variable "C Argdecl Indent"
  "Indentation level of declarations of C function arguments."
  5)

(define-variable "C Label Offset"
  "Offset of C label lines and case statements relative to usual indentation."
  -2)

(define-variable "C Continued Statement Offset"
  "Extra indent for lines not starting new statements."
  2)

(define-variable "C Auto Newline"
  "Non-false means automatically newline before and after braces,
and after colons and semicolons, inserted in C code."
  false)