;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/output.scm,v 13.46 1987/06/17 21:03:20 cph Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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

;;;; Output

(declare (usual-integrations))

;;;; Output Ports

(define output-port-tag
  "Output Port")

(define (output-port? object)
  (and (environment? object)
       (not (lexical-unreferenceable? object ':TYPE))
       (eq? (access :type object) output-port-tag)))

(define *current-output-port*)

(define (current-output-port)
  *current-output-port*)

(define (with-output-to-port port thunk)
  (if (not (output-port? port)) (error "Bad output port" port))
  (fluid-let ((*current-output-port* port))
    (thunk)))

(define (with-output-to-file output-specifier thunk)
  (define new-port (open-output-file output-specifier))
  (define old-port)
  (dynamic-wind (lambda ()
		  (set! old-port
			(set! *current-output-port*
			      (set! new-port))))
		thunk
		(lambda ()
		  (let ((port))
		    ;; Only SET! is guaranteed to do the right thing with
		    ;; an unassigned value.  Binding may not work right.
		    (set! port (set! *current-output-port* (set! old-port)))
		    (if (not (unassigned? port))
			(close-output-port port))))))

(define (call-with-output-file output-specifier receiver)
  (let ((port (open-output-file output-specifier)))
    (let ((value (receiver port)))
      (close-output-port port)
      value)))

(define (close-output-port port)
  ((access :close port)))

;;;; Console Output Port

(define beep
  (make-primitive-procedure 'TTY-BEEP))

(define (screen-clear)
  ((access :clear-screen console-output-port))
  ((access :flush-output console-output-port)))

(define console-output-port)
(let ()

(define tty-write-char
  (make-primitive-procedure 'TTY-WRITE-CHAR))

(define tty-write-string
  (make-primitive-procedure 'TTY-WRITE-STRING))

(define tty-flush-output
  (make-primitive-procedure 'TTY-FLUSH-OUTPUT))

(define tty-clear
  (make-primitive-procedure 'TTY-CLEAR))

(set! console-output-port
      (make-environment

(define :type output-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Console output port"))))

(define (:close) 'DONE)
(define :write-char tty-write-char)
(define :write-string tty-write-string)
(define :flush-output tty-flush-output)
(define :clear-screen tty-clear)

(define (:x-size)
  (access printer-width implementation-dependencies))

(define (:y-size)
  (access printer-length implementation-dependencies))

;;; end CONSOLE-OUTPUT-PORT.
))

)

(set! *current-output-port* console-output-port)

;;; File Output Ports

(define open-output-file)
(let ()
#|
(declare (integrate-primitive-procedures file-write-char file-write-string))
|#
(define file-write-char
  (make-primitive-procedure 'FILE-WRITE-CHAR))

(define file-write-string
  (make-primitive-procedure 'FILE-WRITE-STRING))

(set! open-output-file
(named-lambda (open-output-file filename)
  (make-file-output-port
   ((access open-output-channel primitive-io)
    (canonicalize-output-filename filename)))))

(define (make-file-output-port file-channel)

(define :type output-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Output port for file: ")
     (write ((access channel-name primitive-io) file-channel)))))

(define (:close)
  ((access close-physical-channel primitive-io) file-channel))

(define (:write-char char)
  (file-write-char char file-channel))

(define (:write-string string)
  (file-write-string string file-channel))

(define (:flush-output) 'DONE)
(define (:x-size) false)
(define (:y-size) false)

;;; end MAKE-FILE-OUTPUT-PORT.
(the-environment))

)

;;;; String Output Ports

(define (write-to-string object #!optional max)
  (if (unassigned? max) (set! max false))
  (if (not max)
      (with-output-to-string
       (lambda ()
	 (write object)))
      (with-output-to-truncated-string max
	(lambda ()
	  (write object)))))

(define (with-output-to-string thunk)
  (let ((port (string-output-port)))
    (fluid-let ((*current-output-port* port))
      (thunk))
    ((access :value port))))

(define (string-output-port)

(define :type output-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Output port to string"))))

(define accumulator '())

(define (:value)
  (let ((string (apply string-append (reverse! accumulator))))
    (set! accumulator (list string))
    string))

(define (:write-char char)
  (set! accumulator (cons (char->string char) accumulator)))

(define (:write-string string)
  (set! accumulator (cons string accumulator)))

(define (:close) 'DONE)
(define (:flush-output) 'DONE)
(define (:x-size) false)
(define (:y-size) false)

;;; end STRING-OUTPUT-PORT.
(the-environment))

(define (with-output-to-truncated-string maxsize thunk)
  (call-with-current-continuation
   (lambda (return)

(define :type output-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Output port to truncated string"))))

(define accumulator '())
(define counter maxsize)

(define (:write-char char)
  (:write-string (char->string char)))

(define (:write-string string)
  (set! accumulator (cons string accumulator))
  (set! counter (- counter (string-length string)))
  (if (negative? counter)
      (return (cons true 
		    (substring (apply string-append (reverse! accumulator))
			       0 maxsize)))))

(define (:close) 'DONE)
(define (:flush-output) 'DONE)
(define (:x-size) false)
(define (:y-size) false)

(fluid-let ((*current-output-port* (the-environment)))
  (thunk))
(cons false (apply string-append (reverse! accumulator)))

;;; end WITH-OUTPUT-TO-TRUNCATED-STRING.
)))

;;;; Output Procedures

(define (newline #!optional port)
  (cond ((unassigned? port) (set! port *current-output-port*))
	((not (output-port? port)) (error "Bad output port" port)))
  ((access :write-char port) char:newline)
  ((access :flush-output port))
  *the-non-printing-object*)

(define (write-char char #!optional port)
  (cond ((unassigned? port) (set! port *current-output-port*))
	((not (output-port? port)) (error "Bad output port" port)))
  ((access :write-char port) char)
  ((access :flush-output port))
  *the-non-printing-object*)

(define (write-string string #!optional port)
  (cond ((unassigned? port) (set! port *current-output-port*))
	((not (output-port? port)) (error "Bad output port" port)))
  ((access :write-string port) string)
  ((access :flush-output port))
  *the-non-printing-object*)

(define (unparse-with-brackets thunk)
  ((access unparse-with-brackets unparser-package) thunk))

(define non-printing-object?
  (let ((objects
	 (list *the-non-printing-object*
	       undefined-conditional-branch
	       (vector-ref (get-fixed-objects-vector)
			   (fixed-objects-vector-slot 'NON-OBJECT)))))
    (named-lambda (non-printing-object? object)
      (and (not (future? object))
	   (memq object objects)))))

(define display)
(define write)
(define write-line)

(let ((make-unparser
       (lambda (handler)
	 (lambda (object #!optional port)
	   (if (not (non-printing-object? object))
	       (if (unassigned? port)
		   (handler object *current-output-port*)
		   (with-output-to-port port
		     (lambda ()
		       (handler object port)))))
	   *the-non-printing-object*))))
  (set! display
    (make-unparser
     (lambda (object port)
       (if (and (not (future? object))
		(string? object))
	   ((access :write-string port) object)
	   ((access unparse-object unparser-package) object port false))
       ((access :flush-output port)))))
  (set! write
    (make-unparser
     (lambda (object port)
       ((access unparse-object unparser-package) object port true)
       ((access :flush-output port)))))
  (set! write-line
    (make-unparser
     (lambda (object port)
	((access :write-char port) char:newline)
	((access unparse-object unparser-package) object port true)
	((access :flush-output port))))))