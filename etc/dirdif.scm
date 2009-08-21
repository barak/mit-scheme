#| -*-Scheme-*-

Copyright (c) 1992, 2000 Massachusetts Institute of Technology

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

;;;; Directory comparator

(declare (usual-integrations))

(define input-buffer/channel 
  (access input-buffer/channel (->environment '(runtime generic-i/o-port))))

(define (find-diff old new)
  (let ((delimiters (char-set))
	(copy-command (case (intern (microcode-identification-item
				     'OS-NAME-STRING))
			((unix)
			 "cpx")
			((dos)
			 "copy")
			(else
			 (error "find-diff: Unknown OS"))))
        (copy-commands '())
        (diff-commands '()))
    
    (define (dos-command command file1 file2)
      (display command)
      (display " ")
      (display (->namestring file1))
      (display " ")
      (display (->namestring file2))
      (newline))
    
    (define (file-identical? old new)
      (call-with-input-file 
        old
        (lambda (old)
          (call-with-input-file
            new
            (lambda (new)
              (and (= (file-length 
                        (input-buffer/channel (vector-ref (port/state old) 
                                                          0)))
                      (file-length
                        (input-buffer/channel (vector-ref (port/state new)
                                                          0))))
                   (string=? (read-string delimiters old)
                             (read-string delimiters new))))))))

    (define (dos-copy source dest)
      (set! copy-commands
            (cons (lambda ()
                    (dos-command copy-command source dest))
                  copy-commands))
      unspecific)
    
    (define (dos-diff old new)
      (if (not (file-identical? old new))
          (set! diff-commands
                (cons (lambda ()
                        (dos-command "diff" old new))
                      diff-commands)))
      unspecific)

    (let ((old (pathname-as-directory (->pathname old)))
          (new (pathname-as-directory (->pathname new))))
      (for-each (lambda (path)
                  (let ((path*
                          (pathname-new-directory path 
                                                  (pathname-directory new))))
                    
                    (cond ((member (pathname-name path) '("." "..")))
                          ((not (file-exists? path*))
                           (dos-copy path path*))
                          ((not (member (pathname-type path) '("obj" "exe")))
                           (dos-diff path path*)))))
                (directory-read old)))
    
    (for-each (lambda (command) (command)) (reverse! copy-commands))
    (for-each (lambda (command) (command)) (reverse! diff-commands))))