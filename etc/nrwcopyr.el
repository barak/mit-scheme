;;; -*- Emacs-Lisp -*-

;; Emacs Lisp function to narrow the current buffer so that the MIT Scheme
;; copyright notice -- and nothing else -- disappears.  Works for Scheme,
;; C, and Midas (assembler) files.  -- by Arthur Gleckler

;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;     2005, 2006, 2007, 2008, 2009, 2010, 2011 Massachusetts
;;     Institute of Technology
;; 
;; This material was developed by the Scheme project at the Massachusetts
;; Institute of Technology, Department of Electrical Engineering and
;; Computer Science.  Permission to copy this software, to redistribute
;; it, and to use it for any purpose is granted, subject to the following
;; restrictions and understandings.
;; 
;; 1. Any copy made of this software must include this copyright notice
;; in full.
;; 
;; 2. Users of this software agree to make their best efforts (a) to
;; return to the MIT Scheme project any improvements or extensions that
;; they make, so that these may be included in future releases; and (b)
;; to inform MIT of noteworthy uses of this software.
;; 
;; 3. All materials developed as a consequence of the use of this
;; software shall duly acknowledge such use, in accordance with the usual
;; standards of acknowledging credit in academic research.
;; 
;; 4. MIT has made no warrantee or representation that the operation of
;; this software will be error-free, and MIT is under no obligation to
;; provide any services, by way of maintenance, update, or otherwise.
;; 
;; 5. In conjunction with products arising from the use of this material,
;; there shall be no use of the name of the Massachusetts Institute of
;; Technology nor of any adaptation thereof in any advertising,
;; promotional, or sales literature without prior written consent from
;; MIT in each case.

;; Examples of use:
;;
;;   Install narrow-without-copyright on "C-x ," in Scheme and C modes.
;;  
;;     (define-key scheme-mode-map "\^x," 'narrow-without-copyright)
;;     (define-key c-mode-map "\^x," 'narrow-without-copyright)
;;  
;;   Cause narrow-without-copyright to happen whenever a buffer is brought into
;;   Scheme or C mode.  When an MIT Scheme file is loaded into a buffer, the
;;   copyright will then automatically disappear.
;;  
;;     (setq scheme-mode-hook 'narrow-without-copyright)
;;     (setq c-mode-hook 'narrow-without-copyright)

(defun narrow-without-copyright ()
  "Narrow the current buffer to the part after the MIT Scheme copyright notice,
if there is one."
  (interactive)
  (let ((copyright-pattern
	 "Copyright ([cC]) [0-9---, ]* Massachusetts Institute of Technology"))
    (save-excursion
      (widen)
      (goto-char 1)
      (cond ((looking-at "#| -\\*-Scheme-\\*-")
	     (if (re-search-forward copyright-pattern (point-max) t)
		 (if (re-search-forward "|#[ \t\n]*" (point-max) t)
		     (narrow-to-region (point) (point-max)))))
	    ((looking-at "/\\* -\\*-C-\\*-")
	     (if (re-search-forward copyright-pattern (point-max) t)
		 (if (re-search-forward "\\*/[ \t\n]*" (point-max) t)
		     (narrow-to-region (point) (point-max)))))
	    ((looking-at ";* -\\*-Scheme-\\*-")
	     (if (re-search-forward copyright-pattern (point-max) t)
		 (let ((end-of-comment
			(re-search-forward
			 "MIT in each case.\n;*[ \t\n]*" (point-max) t)))
		   (if end-of-comment
		       (narrow-to-region (point) (point-max))))))
	    ((looking-at "#* -\\*-Midas-\\*-")
	     (if (re-search-forward copyright-pattern (point-max) t)
		 (let ((end-of-comment
			(re-search-forward
			 "MIT in each case.\n#*[ \t\n]*" (point-max) t))))))))))


;;; Make Emacs automagically byte-compile this file when it is saved.
;;;
;;; Local Variables:
;;; write-file-hooks: ((lambda () (if (fboundp 'auto-compile) (auto-compile))))
;;; End:
