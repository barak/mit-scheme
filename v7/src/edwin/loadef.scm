;;; -*-Scheme-*-
;;;
;;;	$Id: loadef.scm,v 1.24 1993/08/22 20:47:32 gjr Exp $
;;;
;;;	Copyright (c) 1986, 1989-1993 Massachusetts Institute of Technology
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

;;;; Major Mode Libraries
(define-library 'TECHINFO-MODE
  '("techinfo" (EDWIN)))

(define-autoload-major-mode 'techinfo 'fundamental "TechInfo" 'TECHINFO-MODE
  "Mode for accessing the TechInfo database.")

(define-autoload-command 'techinfo 'TECHINFO-MODE
  "Enter TechInfo mode.")

(define-library 'TELNET-MODE
  '("telnet" (EDWIN)))

(define-autoload-major-mode 'telnet 'comint "Telnet" 'TELNET-MODE
  "Major mode for interacting with the Telnet program.")

(define-autoload-command 'telnet 'TELNET-MODE
  "Telnet to remote host.")

(define-variable telnet-mode-hook
  "An event distributor that is invoked when entering Telnet mode."
  (make-event-distributor))

(define-library 'RMAIL-SUMMARY-MODE
  '("rmailsum" (EDWIN RMAIL)))

(define-autoload-major-mode 'rmail-summary 'fundamental "RMAIL-Summary"
  'RMAIL-SUMMARY-MODE "Summary mode for RMAIL.")

(define-autoload-command 'rmail-summary 'RMAIL-SUMMARY-MODE
  "Enter RMAIL Summary mode.")

(define-autoload-command 'rmail-summary-by-recipients 'RMAIL-SUMMARY-MODE
  "Enter RMAIL Summary mode for specified recipients.")

(define-variable rmail-summary-mode-hook
  "An event distributor that is invoked when entering RMAIL Summary mode."
  (make-event-distributor))

(define-variable-per-buffer rmail-summary-buffer
  "Corresponding RMAIL-summary buffer for an RMAIL buffer.
FALSE means buffer has no summary buffer."
  false
  (lambda (x) (or (not x) (buffer? x))))

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

;;;; Other Libraries

(define-library 'manual
  '("manual" (EDWIN)))

(define-autoload-command 'manual-entry 'MANUAL
  "Display UNIX man page.")

(define-variable manual-entry-reuse-buffer?
  "If true, MANUAL-ENTRY uses buffer *Manual-Entry* for all entries.
Otherwise, a new buffer is created for each topic."
  false
  boolean?)

(define-variable manual-command
  "A string containing the manual page formatting command.  
Section (if any) and topic strings are appended (with space separators)
and the resulting string is provided to a shell running in a subprocess."
  false
  string-or-false?)

(define-library 'print
  '("print" (EDWIN)))

(define-variable lpr-procedure
  "Procedure that spools some text to the printer, or #F for the default.
Procedure is called with four arguments: a region to be printed, a flag
indicating that the text should be printed with page headers, a title string
to appear in the header lines and on the title page, and the buffer in which
the text was originally stored (for editor variable references).  If this
variable's value is #F, the text is printed using LPR-COMMAND."
  false
  (lambda (object) (or (not object) (procedure? object))))

(define-variable lpr-command
  "Shell command for printing a file"
  "lpr"
  string?)

(define-variable lpr-switches
  "List of strings to pass as extra switch args to lpr when it is invoked."
  '()
  list-of-strings?)

(define lpr-prompt-for-name?
  ;; If true, lpr commands prompt for a name to appear on the title page.
  false)

(define lpr-print-not-special?
  ;; If true, the print-* commands are just like the lpr-* commands.
  false)

(define-autoload-command 'lpr-buffer 'PRINT
  "Print buffer contents with Unix command `lpr'.")

(define-autoload-command 'print-buffer 'PRINT
  "Print buffer contents as with Unix command `lpr -p'.")

(define-autoload-command 'lpr-region 'PRINT
  "Print region contents as with Unix command `lpr'.")

(define-autoload-command 'print-region 'PRINT
  "Print region contents as with Unix command `lpr -p'.")

(define-library 'SORT
  '("sort" (EDWIN)))

(define-autoload-command 'sort-lines 'SORT
  "Sort lines by their text.")

(define-autoload-command 'sort-pages 'SORT
  "Sort pages by their text.")

(define-autoload-command 'sort-paragraphs 'SORT
  "Sort paragraphs by their text.")

(define-autoload-command 'sort-fields 'SORT
  "Sort lines by the text of a field.")

(define-autoload-command 'sort-numeric-fields 'SORT
  "Sort lines by the numeric value of a field.")

(define-autoload-command 'sort-columns 'SORT
  "Sort lines by the text in a range of columns.")

;;;; DOS-specific commands

(if (memq (intern microcode-id/operating-system-name) '(dos nt))
    (begin
      (define-library 'DOSCOM
	'("doscom" (EDWIN DOSJOB)))
      (define-autoload-command 'shell-command 'DOSCOM
	"Execute string COMMAND in inferior shell; display output, if any.")
      (define-autoload-command 'shell-command-on-region 'DOSCOM
	"Execute string COMMAND in inferior shell with region as input.")
      (define-autoload-procedure 'shell-command '(EDWIN DOSJOB)
	'DOSCOM)

      (define-library 'DOSSHELL
	'("dosshell" (EDWIN DOSJOB)))
      (define-autoload-major-mode 'pseudo-shell 'fundamental "Pseudo Shell"
	'DOSSHELL
	"Major mode for executing DOS commands.")
      (define-autoload-command 'shell 'DOSSHELL
	"Run an inferior pseudo shell, with I/O through buffer *shell*.")))