#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unxprm.scm,v 1.12 1991/05/09 03:22:24 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Miscellaneous Unix Primitives
;;; package: ()

(declare (usual-integrations))

(define (file-directory? filename)
  ((ucode-primitive file-directory?)
   (pathname->string (pathname->absolute-pathname (->pathname filename)))))

(define (file-symbolic-link? filename)
  ((ucode-primitive file-symlink?)
   (pathname->string (pathname->absolute-pathname (->pathname filename)))))

(define (file-modes filename)
  ((ucode-primitive file-modes)
   (pathname->string (pathname->absolute-pathname (->pathname filename)))))

(define-integrable (set-file-modes! filename modes)
  ((ucode-primitive set-file-modes!) (canonicalize-input-filename filename)
				     modes))

(define (unix/file-access filename amode)
  ((ucode-primitive file-access)
   (pathname->string (pathname->absolute-pathname (->pathname filename)))
   amode))

(define (file-writable? filename)
  (let ((pathname (pathname->absolute-pathname (->pathname filename))))
    (let ((filename (pathname->string pathname)))
      (or ((ucode-primitive file-access) filename 2)
	  (and (not ((ucode-primitive file-exists?) filename))
	       ((ucode-primitive file-access)
		(pathname-directory-string pathname)
		2))))))

(define (file-attributes filename)
  ((ucode-primitive file-attributes)
   (pathname->string (pathname->absolute-pathname (->pathname filename)))))

(define (file-attributes-indirect filename)
  ((ucode-primitive file-attributes-indirect)
   (pathname->string (pathname->absolute-pathname (->pathname filename)))))

(define-structure (file-attributes
		   (type vector)
		   (constructor false)
		   (conc-name file-attributes/))
  (type false read-only true)
  (n-links false read-only true)
  (uid false read-only true)
  (gid false read-only true)
  (access-time false read-only true)
  (modification-time false read-only true)
  (change-time false read-only true)
  (length false read-only true)
  (mode-string false read-only true)
  (inode-number false read-only true))

(define (file-modification-time filename)
  ((ucode-primitive file-mod-time-indirect 1)
   (pathname->string (pathname->absolute-pathname (->pathname filename)))))

(define-integrable get-environment-variable
  (ucode-primitive get-environment-variable))

(define (unix/user-home-directory user-name)
  (let ((directory (ucode-primitive get-user-home-directory)))
    (if (not directory)
	(error "Can't find user's home directory:" user-name))
    directory))

(define (unix/current-home-directory)
  (or (get-environment-variable "HOME")
      (get-user-home-directory (unix/current-user-name))))

(define-integrable unix/current-user-name
  (ucode-primitive current-user-name))

(define-integrable unix/current-uid
  (ucode-primitive current-uid))

(define-integrable unix/current-gid
  (ucode-primitive current-gid))

(define-integrable unix/current-file-time
  (ucode-primitive current-file-time))

(define-integrable unix/file-time->string
  (ucode-primitive file-time->string))

(define (unix/uid->string uid)
  (or ((ucode-primitive uid->string) uid)
      (number->string uid 10)))

(define (unix/gid->string gid)
  (or ((ucode-primitive gid->string) gid)
      (number->string gid 10)))

(define-integrable unix/system
  (ucode-primitive system))

(define (file-touch filename)
  (let ((filename
	 (pathname->string
	  (let ((pathname (pathname->absolute-pathname (->pathname filename))))
	    (if (let ((version (pathname-version pathname)))
		  (or (not version)
		      (exact-integer? version)))
		pathname
		(or (pathname->input-truename pathname)
		    (pathname-new-version pathname false)))))))
    (let ((result ((ucode-primitive file-touch) filename)))
      (if (string? result)
	  (error:file-touch filename result))
      result)))

(define (make-directory name)
  ((ucode-primitive directory-make)
   (pathname->string
    (pathname-as-directory
     (pathname->absolute-pathname (->pathname name))))))