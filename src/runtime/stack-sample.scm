;;; -*- Mode: Scheme -*-

;;; Copyright (c) 2009, 2010, Taylor R. Campbell.
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;;; Rudimentary Stack-Sampling Profiler
;;; package: (runtime stack-sampler)

(declare (usual-integrations))

;;; This rudimentary stack sampler periodically interrupts the program
;;; and records two numbers for each interrupted compiled[*] entry
;;; found on the stack:
;;;
;;; 1. The `sampled' count, which is the number of times that the
;;;    compiled entry was the interrupted one.  This tells you how
;;;    often a particular part of the code is hit, which approximately
;;;    tells you how important it is for that code to be fast.
;;;
;;; 2. The `waiting' count, which is the number of times that the
;;;    compiled entry was found on the stack as the return address of a
;;;    continuation.  This tells you how much often the sampler hit
;;;    something involved in computing a particular expression, which
;;;    approximately tells you how much time is spent computing that
;;;    expression.
;;;
;;; To sample the evaluation an expression <expression>, interrupting
;;; at every <sample-interval> millisceonds, and then to display the
;;; results and yield its value, evaluate
;;;
;;;   (WITH-STACK-SAMPLING <sample-interval> (LAMBDA () <expression>)).
;;;
;;; A slightly more sophisticated profiler might record a directed
;;; graph of edges from `callers' to `callees' labelled by the number
;;; of times the edge was found on the stack (really, not edges from
;;; callers to callees, but edges from continuations, labelled by the
;;; number of times that one continuation was found as that of a
;;; subsubproblem of the subproblem of the other continuation).  This
;;; is not such a sophisticated profiler.
;;;
;;; This sampler uses the full-blown stack parser, which is a fairly
;;; heavy-weight abstraction not really fit for use in high-frequency
;;; sampling when really only the return addresses on the stack and
;;; their debugging information are important, not any dynamic state.
;;; Probably as a consequence of this, programs run significantly
;;; slower while being sampled.
;;;
;;; This sampler uses the Scheme thread timer to sample, which means
;;; that it samples only at procedure call and return sites, and the
;;; time between samples may vary significantly.  Another approach
;;; would be to use an operating system timer to sample, but then the
;;; stack may be in an arbitrary horrible state (the Scheme ABI does
;;; not have nice frame pointers), so the best we could do is find
;;; where the PC is.
;;;
;;; Finally, this thing is pretty clumsy.  Please feel free to rewrite
;;; this profiler to improve the interface or to expose the information
;;; with an API worth exposing.
;;;
;;; [*] Yes, this works only in compiled code.  It is not clear how to
;;; identify points in interpreted code when recording samples.  But
;;; if your code runs too slowly interpreted, the first step should be
;;; to compile it, not to profile it, because that will always make it
;;; run faster without requiring you to change your code.

;;;; Miscellaneous Kludgerosity

(define (compiled-entry? object)
  (object-type? (ucode-type COMPILED-ENTRY) object))

(define event-return-address 'UNINITIALIZED)

(define (initialize-package!)
  (let ((blocked? (block-thread-events)))
    (signal-thread-event (current-thread)
      (lambda ()
        (call-with-current-continuation
          (lambda (continuation)
            (set! event-return-address
                  (let ((stack-frame
                         ;; Total kludge here.  If thread.scm changes,
                         ;; this will have to change too.  Note that
                         ;; this magic subproblem skippage is not
                         ;; isolated to here -- it must be done in
                         ;; FIND-FIRST-SUBPROBLEM, too, because here we
                         ;; use SUSPEND-CURRENT-THREAD to force the
                         ;; event to run, while during sampling the
                         ;; event is run by a timer interrupt, whose
                         ;; continuation looks different.
                         (stack-frame/next-subproblem
                          (continuation/first-subproblem continuation))))
                    (and (eq? stack-frame-type/compiled-return-address
                              (stack-frame/type stack-frame))
                         (stack-frame/return-address stack-frame))))))))
    (do () ((not (eq? event-return-address 'UNINITIALIZED)))
      (suspend-current-thread))
    (if (not blocked?)
        (unblock-thread-events))))

(define stack-sampler:debug-internal-errors? #f)
(define stack-sampler:show-expressions? #t)

;;;; Running with Stack Sampling

(define (run-with-stack-sampling sample-interval thunk)
  (let ((profile (make-profile))
        (timer-registration #t))
    (define (register-event)
      (if timer-registration
          (set! timer-registration
                (register-timer-event sample-interval
                  (lambda ()
                    (call-with-current-continuation
                      (lambda (continuation)
                        (carefully-record-sample profile continuation)
                        (register-event))))))))
    (define (deregister-event)
      (deregister-timer-event timer-registration)
      (set! timer-registration #f))
    (values (with-simple-restart 'ABORT "Abort stack sampling."
              (lambda ()
                (dynamic-wind
                 register-event
                 (lambda () (with-stack-sampling-continuation thunk))
                 deregister-event)))
            profile)))

(define (carefully-record-sample profile continuation)
  (with-simple-restart 'CONTINUE "Ignore the sample."
    (lambda ()
      (let ((ignore (first-bound-restart))) ;silly
        (define (go) (record-sample profile continuation))
        (if stack-sampler:debug-internal-errors?
            (go)
            (bind-condition-handler (list condition-type:error)
                (lambda (condition)
                  (write-notification-line
                   (lambda (output-port)
                     (write-string "Error in stack sampler: " output-port)
                     (write-condition-report condition output-port)))
                  (invoke-restart ignore))
              go))))))

(define (stack-sampler-interrupt-stack-frame? stack-frame)
  (let ((return-address event-return-address))
    (and (compiled-return-address? return-address)
         (eq? stack-frame-type/compiled-return-address
              (stack-frame/type stack-frame))
         (eq? event-return-address (stack-frame/return-address stack-frame)))))

(define stack-sampling-return-address #f)

(define (stack-sampling-stack-frame? stack-frame)
  (let ((return-address stack-sampling-return-address))
    (and (compiled-return-address? return-address)
         (eq? stack-frame-type/compiled-return-address
              (stack-frame/type stack-frame))
         (eq? return-address (stack-frame/return-address stack-frame)))))

(define (with-stack-sampling-continuation thunk)
  ;; Calling IDENTITY-PROCEDURE here creates a continuation with a
  ;; return address unique to this code, which we use to determine
  ;; where to stop walking down the stack while sampling.
  (identity-procedure
   (call-with-current-continuation
     (lambda (continuation)
       (let ((stack-frame (continuation/first-subproblem continuation)))
         (if (eq? stack-frame-type/compiled-return-address
                  (stack-frame/type stack-frame))
             (fluid-let ((stack-sampling-return-address
                          (stack-frame/return-address stack-frame)))
               (thunk))
             (thunk)))))))

;;;; Profile Data

(define-structure (profile
                   (conc-name profile.)
                   (constructor make-profile ()))
  (entries (make-strong-eq-hash-table) read-only #t))

(define-structure (entry
                   (conc-name entry.)
                   (constructor make-entry
                                (return-address
                                 expression
                                 subexpression
                                 environment-names)))
  (sampled-count 0)
  (waiting-count 0)
  (time-stamp #f)
  (return-address #f read-only #t)
  (expression #f read-only #t)
  (subexpression #f read-only #t)
  (environment-names #f read-only #t))

(define (record-sample profile continuation)
  (let ((time-stamp (real-time-clock))
        (stack-frame
         (find-first-subproblem (continuation->stack-frame continuation))))
    (if stack-frame
        (begin
          (record-sampled profile stack-frame time-stamp)
          (let loop ((stack-frame stack-frame))
            (let ((stack-frame (find-next-subproblem stack-frame)))
              (if (and stack-frame
                       (not (stack-sampling-stack-frame? stack-frame)))
                  (begin (record-waiting profile stack-frame time-stamp)
                         (loop stack-frame)))))))))

(define (find-first-subproblem stack-frame)
  (let loop ((next (stack-frame/skip-non-subproblems stack-frame)))
    (cond ((stack-sampler-interrupt-stack-frame? next)
           ;; Another kludge about the internals of thread.scm.
           (cond ((stack-frame/next-subproblem next) => find-next-subproblem)
                 (else #f)))
          ((stack-frame/next-subproblem next) => loop)
          (else (find-subproblem stack-frame)))))

(define (find-subproblem stack-frame)
  (if (compiled-entry? (stack-frame/return-address stack-frame))
      stack-frame
      (find-next-subproblem stack-frame)))

(define (find-next-subproblem stack-frame)
  (cond ((stack-frame/next-subproblem stack-frame) => find-subproblem)
        (else #f)))

(define (record-sampled profile stack-frame time-stamp)
  time-stamp                            ;ignore
  (let ((entry (intern-entry profile stack-frame)))
    (if entry
        (set-entry.sampled-count! entry (+ 1 (entry.sampled-count entry))))))

(define (record-waiting profile stack-frame time-stamp)
  (let ((entry (intern-entry profile stack-frame)))
    (if (and entry (not (eqv? time-stamp (entry.time-stamp entry))))
        (begin
          (set-entry.waiting-count! entry (+ 1 (entry.waiting-count entry)))
          (set-entry.time-stamp! entry time-stamp)))))

(define (intern-entry profile stack-frame)
  (let ((return-address (stack-frame/return-address stack-frame)))
    (if (compiled-entry? return-address)
        (let ((return-address
               (if (compiled-closure? return-address)
                   (compiled-closure->entry return-address)
                   return-address)))
          (hash-table/intern! (profile.entries profile) return-address
            (lambda ()
              (receive (expression environment subexpression)
                       (stack-frame/debugging-info stack-frame)
                (make-entry return-address
                            expression
                            subexpression
                            (environment-ancestry-names environment))))))
        ;; What to do for interpreted code?  Fetch the debugging
        ;; information and use the expression, subexpression, and
        ;; environment ancestry names as the key?
        #f)))

;;;; Display

(define (with-stack-sampling sample-interval thunk)
  (receive (value profile)
      (with-notification (lambda (output-port)
                           (write-string "Stack-sampling" output-port))
        (lambda ()
          (run-with-stack-sampling sample-interval thunk)))
    (write-notification-line
     (lambda (output-port)
       (display-profile profile output-port)))
    value))

(define (display-profile profile output-port)
  (let ((entries (hash-table/datum-list (profile.entries profile))))
    (define (sortem entry.count)
      (sort (delete-matching-items entries
              (lambda (e) (zero? (entry.count e))))
            (lambda (a b) (< (entry.count a) (entry.count b)))))
    (let ((sampled (sortem entry.sampled-count))
          (waiting (sortem entry.waiting-count)))
      (let ((total-sampled (reduce + 0 (map entry.sampled-count sampled)))
            (total-waiting (reduce + 0 (map entry.waiting-count waiting))))
        (define (d title entries total selector)
          (display-profile-entries title entries total selector output-port))
        (write total-sampled output-port)
        (display " samples" output-port)
        (newline output-port)
        (d "Waiting" waiting total-waiting entry.waiting-count)
        (d "Sampled" sampled total-sampled entry.sampled-count)))))

(define (display-profile-entries title entries total entry.count output-port)
  total                                 ;ignore
  (newline output-port)
  (display "*** " output-port)
  (display title output-port)
  (newline output-port)
  (newline output-port)
  (for-each (lambda (count-string entry)
              (write-string count-string output-port)
              (write-string " sample" output-port)
              (if (not (= 1 (entry.count entry)))
                  (write-char #\s output-port))
              (write-string " in " output-port)
              (let ((environment-names (entry.environment-names entry)))
                (if (pair? environment-names)
                    (show-environment-names environment-names output-port)
                    (write (entry.return-address entry) output-port)))
              (if stack-sampler:show-expressions?
                  (begin
                    (write-char #\: output-port)
                    (newline output-port)
                    (show-expression (entry.expression entry)
                                     (entry.subexpression entry)
                                     output-port)))
              (newline output-port))
            (entry-count-strings entries entry.count)
            entries))

(define (entry-count-strings entries entry.count)
  (let ((count-strings
         (map (lambda (entry) (number->string (entry.count entry))) entries)))
    (map (let ((width (reduce max 0 (map string-length count-strings))))
           (lambda (count-string)
             (string-pad-left count-string width #\space)))
         count-strings)))

(define (environment-ancestry-names environment)
  (let recur ((environment environment))
    (if (environment? environment)      ;Idle paranoia?
        (let ((package (environment->package environment)))
          (if package
              (list (package/name package))
              (let ((name (environment-procedure-name environment))
                    (names
                     (if (environment-has-parent? environment)
                         (recur (environment-parent environment))
                         '())))
                (if name
                    (cons (cond ((special-form-procedure-name? name)
                                 => (lambda (rename) (list (intern rename))))
                                (else name))
                          names)
                    names))))
        '())))

(define (show-environment-names environment-names output-port)
  (if (pair? environment-names)
      (write-string
       (decorated-string-append "" ", " ""
         (map write-to-string (reverse environment-names)))
       output-port)))

(define (show-expression expression subexpression output-port)
  (write-string " evaluating " output-port)
  (cond ((invalid-expression-description expression)
         => (lambda (description)
              (write-string description output-port)
              (newline output-port)))
        ((or (debugging-info/undefined-expression? subexpression)
             (debugging-info/unknown-expression? subexpression))
         (newline output-port)
         (profile-pp expression output-port))
        (else
         (newline output-port)
         (profile-pp subexpression output-port)
         (write-string " for ### in" output-port)
         (newline output-port)
         (profile-pp
          (unsyntax-with-substitutions
           expression
           (list (cons subexpression (string->symbol "###"))))
          output-port))))

(define (invalid-expression-description expression)
  (cond ((debugging-info/compiled-code? expression)
         ;++ Should this display the compiled entry itself?
         "compiled code")
        ((debugging-info/undefined-expression? expression)
         "undefined expression")
        (else #f)))

(define (profile-pp expression output-port)
  ;; Random parametrization.
  (fluid-let ((*unparser-list-breadth-limit* 5)
              (*unparser-list-depth-limit* 3)
              (*unparser-string-length-limit* 40)
              (*unparse-primitives-by-name?* #t)
              (*pp-save-vertical-space?* #t)
              (*pp-default-as-code?* #t))
    (pp expression output-port)))