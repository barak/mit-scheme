;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Input

(declare (usual-integrations))

;;;; Input Ports

(define input-port-tag
  "Input Port")

(define (input-port? object)
  (and (environment? object)
       (not (lexical-unreferenceable? object ':type))
       (eq? (access :type object) input-port-tag)))

(define eof-object
  "EOF Object")

(define (eof-object? object)
  (eq? object eof-object))

(define *current-input-port*)

(define (current-input-port)
  *current-input-port*)

(define (with-input-from-port port thunk)
  (if (not (input-port? port)) (error "Bad input port" port))
  (fluid-let ((*current-input-port* port))
    (thunk)))

(define (with-input-from-file input-specifier thunk)
  (define new-port (open-input-file input-specifier))
  (define old-port)
  (dynamic-wind (lambda ()
		  (set! old-port
			(set! *current-input-port*
			      (set! new-port))))
		thunk
		(lambda ()
		  (let ((port))
		    ;; Only SET! is guaranteed to do the right thing with
		    ;; an unassigned value.  Binding may not work right.
		    (set! port (set! *current-input-port* (set! old-port)))
		    (if (not (unassigned? port))
			(close-input-port port))))))

(define (call-with-input-file input-specifier receiver)
  (let ((port (open-input-file input-specifier)))
    (let ((value (receiver port)))
      (close-input-port port)
      value)))

(define (close-input-port port)
  ((access :close port)))

;;;; Console Input Port

(define console-input-port)
(let ()

(define tty-read-char
  (make-primitive-procedure 'TTY-READ-CHAR))

(define tty-read-char-immediate
  (make-primitive-procedure 'TTY-READ-CHAR-IMMEDIATE))

(define tty-read-char-ready?
  (make-primitive-procedure 'TTY-READ-CHAR-READY?))

(define tty-read-finish
  (make-primitive-procedure 'TTY-READ-FINISH))

(define (read-start-hook)
  'DONE)

(define (read-finish-hook)
  'DONE)

(set! console-input-port
      (make-environment

(define :type input-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Console input port"))))

(define (:close)
  'DONE)

(define character-buffer
  #!FALSE)

(define (:peek-char)
  (or character-buffer
      (begin (set! character-buffer (tty-read-char))
	     character-buffer)))

(define (:discard-char)
  (set! character-buffer #!FALSE))

(define (:read-char)
  (if character-buffer
      (set! character-buffer #!FALSE)
      (tty-read-char)))

(define (:read-string delimiters)
  (define (loop)
    (if (char-set-member? delimiters (:peek-char))
	'()
	(let ((char (:read-char)))
	  (cons char (loop)))))
  (list->string (loop)))

(define (:discard-chars delimiters)
  (define (loop)
    (if (not (char-set-member? delimiters (:peek-char)))
	(begin (:discard-char)
	       (loop))))
  (loop))

(define (:peek-char-immediate)
  (or character-buffer
      (begin (set! character-buffer (tty-read-char-immediate))
	     character-buffer)))

(define (:read-char-immediate)
  (if character-buffer
      (set! character-buffer #!FALSE)
      (tty-read-char-immediate)))

(define (:char-ready? delay)
  (or character-buffer
      (tty-read-char-ready? delay)))

(define (:read-start!)
  (read-start-hook))

(define :read-finish!
  (let ()
    (define (read-finish-loop)
      (if (and (:char-ready? 0)
	       (char-whitespace? (:peek-char)))
	  (begin (:discard-char)
		 (read-finish-loop))))
    (lambda ()
      (tty-read-finish)
      (read-finish-loop)
      (read-finish-hook))))

;;; end CONSOLE-INPUT-PORT.
))

)

(set! *current-input-port* console-input-port)

;;;; File Input Ports

(define open-input-file)
(let ()

(define file-fill-input-buffer
  (make-primitive-procedure 'FILE-FILL-INPUT-BUFFER))

(define file-length
  (make-primitive-procedure 'FILE-LENGTH))

(define file-port-buffer-size
  512)

(set! open-input-file
(named-lambda (open-input-file filename)
  (let ((file-channel ((access open-input-channel primitive-io)
		       (canonicalize-input-filename filename))))

(define :type input-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Buffered input port for file: ")
     (write ((access channel-name primitive-io) file-channel)))))

(define (:pathname)
  (->pathname filename))

(define (:truename)
  (->pathname ((access channel-name primitive-io) file-channel)))

(define (:length)
  (file-length file-channel))

(define buffer #!FALSE)
(define start-index 0)
(define end-index -1)

(define (refill-buffer!)
  (if (not buffer) (set! buffer (string-allocate file-port-buffer-size)))
  (set! start-index 0)
  (set! end-index (file-fill-input-buffer file-channel buffer))
  (zero? end-index))

(define (:char-ready? delay)
  (not (zero? end-index)))

(define (:close)
  (set! end-index 0)
  (set! buffer #!FALSE)
  ((access close-physical-channel primitive-io) file-channel))

(define (:peek-char)
  (if (< start-index end-index)
      (string-ref buffer start-index)
      (and (not (zero? end-index))
	   (not (refill-buffer!))
	   (string-ref buffer 0))))

(define (:discard-char)
  (set! start-index (1+ start-index)))

(define (:read-char)
  (if (< start-index end-index)
      (string-ref buffer (set! start-index (1+ start-index)))
      (and (not (zero? end-index))
	   (not (refill-buffer!))
	   (begin (set! start-index 1)
		  (string-ref buffer 0)))))

(define (:read-string delimiters)
  (define (loop)
    (let ((index
	   (substring-find-next-char-in-set buffer start-index end-index
					    delimiters)))
      (if index
	  (substring buffer (set! start-index index) index)
	  (let ((head (substring buffer start-index end-index)))
	    (if (refill-buffer!)
		head
		(let ((tail (loop))
		      (head-length (string-length head)))
		  (let ((result (string-allocate (+ head-length
						    (string-length tail)))))
		    (substring-move-right! head 0 head-length
					   result 0)
		    (substring-move-right! tail 0 (string-length tail)
					   result head-length)
		    result)))))))
  (and (or (< start-index end-index)
	   (and (not (zero? end-index))
		(not (refill-buffer!))))
       (loop)))

(define (:discard-chars delimiters)
  (define (loop)
    (let ((index
	   (substring-find-next-char-in-set buffer start-index end-index
					    delimiters)))
      (cond (index (set! start-index index))
	    ((not (refill-buffer!)) (loop)))))
  (if (or (< start-index end-index)
	  (and (not (zero? end-index))
	       (not (refill-buffer!))))
      (loop)))

(define (:rest->string)
  (define (read-rest)
    (set! end-index 0)
    (loop))

  (define (loop)
    (let ((buffer (string-allocate file-port-buffer-size)))
      (let ((n (file-fill-input-buffer file-channel buffer)))
	(cond ((zero? n) '())
	      ((< n file-port-buffer-size)
	       (set-string-length! buffer n)
	       (list buffer))
	      (else (cons buffer (loop)))))))

  (if (zero? end-index)
      (error "End of file -- :REST->STRING"))
  (cond ((= -1 end-index)
	 (let ((l (:length)))
	   (if l
	       (let ((buffer (string-allocate l)))
		 (set! end-index 0)
		 (file-fill-input-buffer file-channel buffer)
		 buffer)
	       (apply string-append (read-rest)))))
	((< start-index end-index)
	 (let ((first (substring buffer start-index end-index)))
	   (apply string-append
		  (cons first
			(read-rest)))))
	(else
	 (apply string-append (read-rest)))))

(the-environment))))

)

;;;; String Input Ports

(define (with-input-from-string string thunk)
  (fluid-let ((*current-input-port* (string->input-port string)))
    (thunk)))

(define (string->input-port string #!optional start end)
  (cond ((unassigned? start)
	 (set! start 0)
	 (set! end (string-length string)))
	((unassigned? end)
	 (set! end (string-length string))))

(define :type input-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Input port for string"))))

(define (:char-ready? delay)
  (< start end))

(define (:close) 'DONE)

(define (:peek-char)
  (and (< start end)
       (string-ref string start)))

(define (:discard-char)
  (set! start (1+ start)))

(define (:read-char)
  (and (< start end)
       (string-ref string (set! start (1+ start)))))

(define (:read-string delimiters)
  (and (< start end)
       (let ((index
	      (substring-find-next-char-in-set string start end delimiters)))
	 (if index
	     (substring string (set! start index) index)
	     (substring string start end)))))

(define (:discard-chars delimiters)
  (if (< start end)
      (set! start
	    (or (substring-find-next-char-in-set string start end delimiters)
		end))))

;;; end STRING->INPUT-PORT.
(the-environment))

;;;; Input Procedures

(define (peek-char #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (or ((if (lexical-unreferenceable? port ':peek-char-immediate)
	   (access :peek-char port)
	   (access :peek-char-immediate port)))
      eof-object))

(define (read-char #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (or ((if (lexical-unreferenceable? port ':read-char-immediate)
	   (access :read-char port)
	   (access :read-char-immediate port)))
      eof-object))

(define (read-string delimiters #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (or ((access :read-string port) delimiters)
      eof-object))

(define (read #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (if (not (lexical-unreferenceable? port ':read-start!))
      ((access :read-start! port)))
  (let ((object ((access *parse-object parser-package) port)))
    (if (not (lexical-unreferenceable? port ':read-finish!))
	((access :read-finish! port)))
    object))

;;; **** The DELAY option for this operation works only for the
;;; console port.  Since it is a kludge, it is probably OK.

(define (read-char-ready? #!optional port delay)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (cond ((unassigned? delay) (set! delay 0))
	((not (and (integer? delay) (>= delay 0))) (error "Bad delay" delay)))
  ((access :char-ready? port) delay))

(define (read-char-no-hang #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (and ((access :char-ready? port) 0)
       (read-char port)))

(define load)
(define load-noisily)
(define load-noisily? #!FALSE)
(define read-file)
(let ()

(define default-pathname
  (make-pathname #!FALSE #!FALSE #!FALSE #!FALSE 'NEWEST))

;;; This crufty piece of code, once it decides which file to load,
;;; does `file-exists?' on that file at least three times!!

(define (basic-load filename environment)
  (define (kernel filename)
    (let ((pathname
	   (let ((pathname (->pathname filename)))
	     (or (pathname->input-truename pathname)
		 (let ((pathname (merge-pathnames pathname default-pathname)))
		     (if (pathname-type pathname)
			 (pathname->input-truename pathname)
			 (or (pathname->input-truename
			      (pathname-new-type pathname "bin"))
			     (pathname->input-truename
			      (pathname-new-type pathname "scm")))))
		 (error "No such file" pathname)))))
      (if (call-with-input-file pathname
	    (lambda (port)
	      (= 250 (char->ascii (peek-char port)))))
	  (scode-load pathname)
	  (sexp-load pathname))))

  (define (sexp-load filename)
    (call-with-input-file filename
      (lambda (port)
	(define (load-loop previous-object)
	  (let ((object (read port)))
	    (if (eof-object? object)
		previous-object
		(let ((value (eval object environment)))
		  (if load-noisily? (begin (newline) (write value)))
		  (load-loop value)))))
	(load-loop *the-non-printing-object*))))

  (define (scode-load filename)
    (scode-eval (fasload filename) environment))

  (if (pair? filename)
      (for-each kernel filename)
      (kernel filename)))

(set! load
(named-lambda (load filename #!optional environment)
  (if (unassigned? environment) (set! environment (rep-environment)))
  (basic-load filename environment)))

(set! load-noisily
(named-lambda (load-noisily filename #!optional environment)
  (if (unassigned? environment) (set! environment (rep-environment)))
  (fluid-let ((load-noisily? #!TRUE))
    (basic-load filename environment))))

(set! read-file
(named-lambda (read-file filename)
  (let ((name (pathname->input-truename
	       (merge-pathnames (->pathname filename) default-pathname))))
    (if name
	(call-with-input-file name
	  (access *parse-objects-until-eof parser-package))
	(error "Read-file: No such file" name)))))
)

(define fasload)
(let ()

(define binary-fasload
  (make-primitive-procedure 'BINARY-FASLOAD))

(set! fasload
(named-lambda (fasload filename)
  (set! filename (canonicalize-input-filename filename))
  (let ((port (rep-output-port)))
    (newline port)
    (write-string "FASLoading " port)
    (write filename port)
    (let ((value (binary-fasload filename)))
      (write-string " -- done" port)
      value))))

)

(define transcript-on
  (let ((photo-open (make-primitive-procedure 'PHOTO-OPEN)))
    (named-lambda (transcript-on filename)
      (if (not (photo-open (canonicalize-output-filename filename)))
	  (error "Transcript file already open: TRANSCRIPT-ON" filename))
      *the-non-printing-object*)))

(define transcript-off
  (let ((photo-close (make-primitive-procedure 'PHOTO-CLOSE)))
    (named-lambda (transcript-off)
      (if (not (photo-close))
	  (error "Transcript file already closed: TRANSCRIPT-OFF"))
      *the-non-printing-object*)))