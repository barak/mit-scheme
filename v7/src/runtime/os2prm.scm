#| -*-Scheme-*-

$Id: os2prm.scm,v 1.29 1996/05/03 07:40:33 cph Exp $

Copyright (c) 1994-96 Massachusetts Institute of Technology

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

;;;; Miscellaneous OS/2 Primitives
;;; package: ()

(declare (usual-integrations))

(define (file-directory? filename)
  ((ucode-primitive file-directory? 1)
   (->namestring (merge-pathnames filename))))

(define (file-symbolic-link? filename)
  ((ucode-primitive file-symlink? 1)
   (->namestring (merge-pathnames filename))))

(define (file-access filename amode)
  ((ucode-primitive file-access 2)
   (->namestring (merge-pathnames filename))
   amode))

(define (file-readable? filename)
  (file-access filename 4))

(define (file-writable? filename)
  ((ucode-primitive file-access 2)
   (let ((pathname (merge-pathnames filename)))
     (let ((filename (->namestring pathname)))
       (if ((ucode-primitive file-exists? 1) filename)
	   filename
	   (directory-namestring pathname))))
   2))

(define (file-executable? filename)
  (file-access filename 1))

(define (make-directory name)
  ((ucode-primitive directory-make 1)
   (->namestring (pathname-as-directory (merge-pathnames name)))))

(define (delete-directory name)
  ((ucode-primitive directory-delete 1)
   (->namestring (pathname-as-directory (merge-pathnames name)))))

(define (file-modes filename)
  ((ucode-primitive file-attributes 1)
   (->namestring (merge-pathnames filename))))

(define (set-file-modes! filename modes)
  ((ucode-primitive set-file-attributes! 2)
   (->namestring (merge-pathnames filename))
   modes))

(define-integrable os2-file-mode/read-only #x01)
(define-integrable os2-file-mode/hidden    #x02)
(define-integrable os2-file-mode/system    #x04)
(define-integrable os2-file-mode/directory #x10)
(define-integrable os2-file-mode/archived  #x20)

(define (file-length filename)
  ((ucode-primitive file-length 1)
   (->namestring (merge-pathnames filename))))

(define (file-modification-time filename)
  ((ucode-primitive file-mod-time 1)
   (->namestring (merge-pathnames filename))))
(define file-modification-time-direct file-modification-time)
(define file-modification-time-indirect file-modification-time)

(define (file-access-time filename)
  ((ucode-primitive file-access-time 1)
   (->namestring (merge-pathnames filename))))
(define file-access-time-direct file-access-time)
(define file-access-time-indirect file-access-time)

(define (set-file-times! filename access-time modification-time)
  ((ucode-primitive set-file-times! 3)
   (->namestring (merge-pathnames filename))
   access-time
   modification-time))

(define (decode-file-time time)
  (let* ((twosecs (remainder time 32)) (time (quotient time 32))
	 (minutes (remainder time 64)) (time (quotient time 64))
	 (hours   (remainder time 32)) (time (quotient time 32))
	 (day     (remainder time 32)) (time (quotient time 32))
	 (month   (remainder time 16)) (year (quotient time 16)))
    (make-decoded-time (* twosecs 2) minutes hours day month (+ 1980 year))))

(define (encode-file-time dt)
  (let ((f (lambda (i j k) (+ (* i j) k))))
    (f (f (f (f (f (let ((year (decoded-time/year dt)))
		     (if (< year 1980)
			 (error "Can't encode years earlier than 1980:" year))
		     year)
		   16 (decoded-time/month dt))
		32 (decoded-time/day dt))
	     32 (decoded-time/hour dt))
	  64 (decoded-time/minute dt))
       32 (quotient (decoded-time/second dt) 2))))

(define (file-time->universal-time time)
  (encode-universal-time (decode-file-time time)))

(define (universal-time->file-time time)
  (encode-file-time (decode-universal-time time)))

(define (file-attributes filename)
  ((ucode-primitive file-info 1)
   (->namestring (merge-pathnames filename))))
(define file-attributes-direct file-attributes)
(define file-attributes-indirect file-attributes)

(define-structure (file-attributes (type vector)
				   (constructor #f)
				   (conc-name file-attributes/))
  (type #f read-only #t)
  (access-time #f read-only #t)
  (modification-time #f read-only #t)
  (change-time #f read-only #t)
  (length #f read-only #t)
  (mode-string #f read-only #t)
  (modes #f read-only #t)
  (allocated-length #f read-only #t))

(define (file-attributes/n-links attributes) attributes 1)

(define (file-touch filename)
  ((ucode-primitive file-touch 1) (->namestring (merge-pathnames filename))))

(define (get-environment-variable name)
  ((ucode-primitive get-environment-variable 1) name))

(define (temporary-file-pathname)
  (let ((root
	 (let ((directory (temporary-directory-pathname)))
	   (merge-pathnames
	    (if (dos/fs-long-filenames? directory)
		(string-append
		 "sch"
		 (string-pad-left (number->string (os2/current-pid)) 6 #\0))
		"_scm_tmp")
	    directory))))
    (let loop ((ext 0))
      (let ((pathname (pathname-new-type root (number->string ext))))
	(if (allocate-temporary-file pathname)
	    pathname
	    (begin
	      (if (> ext 999)
		  (error "Can't find unique temporary pathname:" root))
	      (loop (+ ext 1))))))))

(define (temporary-directory-pathname)
  (let ((try-directory
	 (lambda (directory)
	   (let ((directory
		  (pathname-as-directory (merge-pathnames directory))))
	     (and (file-directory? directory)
		  (file-writable? directory)
		  directory)))))
    (let ((try-variable
	   (lambda (name)
	     (let ((value (get-environment-variable name)))
	       (and value
		    (try-directory value))))))
      (or (try-variable "TEMP")
	  (try-variable "TMP")
	  (try-directory "\\tmp")
	  (try-directory "c:\\")
	  (try-directory ".")
	  (try-directory "\\")
	  (error "Can't find temporary directory.")))))

(define-integrable os2/current-pid
  (ucode-primitive current-pid 0))

(define (current-home-directory)
  (let ((home (get-environment-variable "HOME")))
    (if home
	(pathname-as-directory (merge-pathnames home))
	(user-home-directory (current-user-name)))))

(define (current-user-name)
  (or (get-environment-variable "USER")
      "nouser"))

(define (user-home-directory user-name)
  (or (and user-name
	   (let ((directory (get-environment-variable "USERDIR")))
	     (and directory
		  (pathname-as-directory
		   (pathname-new-name
		    (pathname-as-directory (merge-pathnames directory))
		    user-name)))))
      (merge-pathnames "\\")))

(define (dos/fs-drive-type pathname)
  (let ((type
	 ((ucode-primitive drive-type 1)
	  (pathname-device (merge-pathnames pathname)))))
    (let ((colon (string-find-next-char type #\:)))
      (if colon
	  (cons (string-head type colon) (string-tail type (fix:+ colon 1)))
	  (cons type "")))))

(define (dos/fs-long-filenames? pathname)
  (not (string-ci=? "fat" (car (dos/fs-drive-type pathname)))))

(define (os/file-end-of-line-translation pathname)
  (let ((type (dos/fs-drive-type pathname)))
    ;; "ext2" is the Linux ext2 file-system driver.  "NFS" is the IBM
    ;; TCP/IP NFS driver, which we further qualify by examining the
    ;; mount info -- if the directory starts with a "/", we assume
    ;; that it is a unix system.
    (if (or (string=? "ext2" (car type))
	    (and (string=? "NFS" (car type))
		 (let* ((mount (cdr type))
			(colon (string-find-next-char mount #\:)))
		   (and colon
			(fix:< (fix:+ colon 1) (string-length mount))
			(char=? #\/ (string-ref mount (fix:+ colon 1)))))))
	#f
	"\r\n")))

(define (os/default-end-of-line-translation)
  "\r\n")

(define (copy-file from to)
  ((ucode-primitive os2-copy-file 2) (->namestring (merge-pathnames from))
				     (->namestring (merge-pathnames to))))

(define (init-file-specifier->pathname specifier)

  (define (read-fat-init-file-map port)
    (let loop ((result '()))
      (let ((item (read port)))
	(if (eof-object? item)
	    result
	    (begin
	      (if (not (and (pair? item)
			    (init-file-specifier? (car item))
			    (string? (cdr item))))
		  (error "Malformed init-file map item:" item))
	      (loop (cons item result)))))))

  (define (generate-fat-init-file directory)
    (let loop ((index 1))
      (let ((filename
	     (string-append "ini"
			    (string-pad-left (number->string index) 5 #\0)
			    ".dat")))
	(if (file-exists? (merge-pathnames filename directory))
	    (loop (+ index 1))
	    filename))))

  (guarantee-init-file-specifier specifier 'INIT-FILE-SPECIFIER->PATHNAME)
  (let ((long-base (merge-pathnames ".mit-scheme/" (user-homedir-pathname))))
    (if (dos/fs-long-filenames? long-base)
	(if (null? specifier)
	    (directory-pathname-as-file long-base)
	    (merge-pathnames
	     (apply string-append
		    (cons (car specifier)
			  (append-map (lambda (string) (list "/" string))
				      (cdr specifier))))
	     long-base))
	(let ((short-base
	       (merge-pathnames "mitschem.ini/" (user-homedir-pathname))))
	  (let ((file-map-pathname (merge-pathnames "filemap.dat" short-base)))
	    (let ((port #f))
	      (dynamic-wind
	       (lambda ()
		 (set! port (open-i/o-file file-map-pathname))
		 unspecific)
	       (lambda ()
		 (merge-pathnames
		  (or (let ((entry
			     (assoc specifier (read-fat-init-file-map port))))
			(and entry
			     (cdr entry)))
		      (let ((filename (generate-fat-init-file short-base)))
			(let ((channel (port/output-channel port)))
			  (channel-file-set-position
			   channel
			   (channel-file-length channel)))
			(write (cons specifier filename) port)
			(newline port)
			filename))
		  short-base))
	       (lambda ()
		 (if port
		     (begin
		       (close-port port)
		       (set! port #f)
		       unspecific))))))))))

(define (initialize-system-primitives!)
  (discard-select-registry-result-vectors!)
  (add-event-receiver! event:after-restart
		       discard-select-registry-result-vectors!))

(define os2/select-registry-lub)
(define select-registry-result-vectors)

(define (discard-select-registry-result-vectors!)
  (set! os2/select-registry-lub ((ucode-primitive os2-select-registry-lub 0)))
  (set! select-registry-result-vectors '())
  unspecific)

(define (allocate-select-registry-result-vector)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((v
	   (let loop ((rv select-registry-result-vectors))
	     (cond ((null? rv)
		    (make-string os2/select-registry-lub))
		   ((car rv)
		    => (lambda (v) (set-car! rv #f) v))
		   (else
		    (loop (cdr rv)))))))
      (set-interrupt-enables! interrupt-mask)
      v)))

(define (deallocate-select-registry-result-vector v)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let loop ((rv select-registry-result-vectors))
      (cond ((null? rv)
	     (set! select-registry-result-vectors
		   (cons v select-registry-result-vectors)))
	    ((car rv)
	     (loop (cdr rv)))
	    (else
	     (set-car! rv v))))
    (set-interrupt-enables! interrupt-mask))
  unspecific)

(define (make-select-registry . descriptors)
  (let ((registry (make-string os2/select-registry-lub)))
    (vector-8b-fill! registry 0 os2/select-registry-lub 0)
    (do ((descriptors descriptors (cdr descriptors)))
	((null? descriptors))
      (add-to-select-registry! registry (car descriptors)))
    registry))

(define (os2/guarantee-select-descriptor descriptor procedure)
  (if (not (and (fix:fixnum? descriptor)
		(fix:<= 0 descriptor)
		(fix:< descriptor os2/select-registry-lub)))
      (error:wrong-type-argument descriptor "select descriptor" procedure))
  descriptor)

(define (add-to-select-registry! registry descriptor)
  (os2/guarantee-select-descriptor descriptor 'ADD-TO-SELECT-REGISTRY!)
  (vector-8b-set! registry descriptor 1))

(define (remove-from-select-registry! registry descriptor)
  (os2/guarantee-select-descriptor descriptor 'REMOVE-FROM-SELECT-REGISTRY!)
  (vector-8b-set! registry descriptor 0))

(define (select-descriptor descriptor block?)
  (vector-ref os2/select-result-values
	      ((ucode-primitive os2-select-descriptor 2) descriptor block?)))

(define (select-registry-test registry block?)
  (let ((result-vector (allocate-select-registry-result-vector)))
    (let ((result
	   ((ucode-primitive os2-select-registry-test 3) registry
							 result-vector
							 block?)))
      (if (fix:= result 0)
	  (let loop
	      ((index (fix:- os2/select-registry-lub 1))
	       (descriptors '()))
	    (let ((descriptors
		   (if (fix:= 0 (vector-8b-ref result-vector index))
		       descriptors
		       (cons index descriptors))))
	      (if (fix:= 0 index)
		  (begin
		    (deallocate-select-registry-result-vector result-vector)
		    descriptors)
		  (loop (fix:- index 1) descriptors))))
	  (begin
	    (deallocate-select-registry-result-vector result-vector)
	    (vector-ref os2/select-result-values result))))))

(define os2/select-result-values
  '#(INPUT-AVAILABLE #F INTERRUPT PROCESS-STATUS-CHANGE))