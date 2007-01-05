#| -*-Scheme-*-

$Id: pcsdisp.scm,v 1.7 2007/01/05 15:33:09 cph Exp $

Copyright 1993,1999,2006 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; PC Sampling Display routines (pre-cursor to PC Sample SWAT frobs)
;;; package: (pc-sample display)

(declare (usual-integrations))

(define (initialize-package!)
  (install))

(define-primitives
  (get-primitive-name 1)
  )

;;; Aesthetics

(define (pc-sample/status/display)
  (pc-sample/status/display/header "")
  (pc-sample/builtin/status/display	'SUBHEADER)
  (pc-sample/utility/status/display	'SUBHEADER)
  (pc-sample/primitive/status/display	'SUBHEADER)
  (pc-sample/code-block/status/display	'SUBHEADER)
  (pc-sample/interp-proc/status/display	'SUBHEADER)
  (pc-sample/prob-comp/status/display	'SUBHEADER)
  (pc-sample/UFO/status/display		'SUBHEADER)
  unspecific)

;; Status Displayers

(define pc-sample/builtin/status/display)
(define pc-sample/utility/status/display)
(define pc-sample/primitive/status/display)
(define pc-sample/code-block/status/display)
(define pc-sample/interp-proc/status/display)
(define pc-sample/prob-comp/status/display)
(define pc-sample/UFO/status/display) 

(define (generate:pc-sample/status/displayer header-string display-proc)
  (lambda (#!optional subheader?)
    ((if (or (default-object? subheader?) (not subheader?)) ; display header
	 pc-sample/status/display/header
	 pc-sample/status/display/subheader)
     header-string)
    (display-proc)
    (pc-sample/status/display/header/delimiter)
    unspecific))

(define-integrable (pc-sample/status/display/header/delimiter)
  (display "\n;============================================================="))

(define-integrable (pc-sample/status/display/subheader/delimiter)
  (display "\n;------------------------------------------------------"))

(define-integrable (pc-sample/status/display/title-root-string)
  (display " PC Sampling status:"))

(define-integrable (pc-sample/status/display/header title-prefix-string)
  (pc-sample/status/display/header/delimiter)
  (display (string-append "\n; " title-prefix-string))
  (pc-sample/status/display/title-root-string)
  (pc-sample/status/display/header/delimiter))
  
(define-integrable (pc-sample/status/display/subheader subheader-title-string)
  (display (string-append "\n; " subheader-title-string "..."))
  (pc-sample/status/display/subheader/delimiter))

(define (install-status-displayers)
  (set! pc-sample/builtin/status/display     (generate:pc-sample/status/displayer
		 "Hand Assembled Procedure (a.k.a. ``Built-In'') "
        pc-sample/builtin/display))

  (set! pc-sample/utility/status/display     (generate:pc-sample/status/displayer
                 "Utility System Subroutine "
        pc-sample/utility/display))

  (set! pc-sample/primitive/status/display   (generate:pc-sample/status/displayer
                 "Primitive Procedure "
        pc-sample/primitive/display))

  (set! pc-sample/code-block/status/display  (generate:pc-sample/status/displayer
		 "Compiled Procedure (a.k.a. ``Code Block'') "
        pc-sample/code-block/display))

  (set! pc-sample/interp-proc/status/display (generate:pc-sample/status/displayer
                 "Interpreted Procedure (a.k.a. ``Interp-Proc'') "
        pc-sample/interp-proc/display))

  (set! pc-sample/prob-comp/status/display   (generate:pc-sample/status/displayer
		 "Probably Compiled Function, Not Observably Residence Designated\n;    (a.k.a. ``Prob Comp FNORD!'') "
        pc-sample/prob-comp/display))

  (set! pc-sample/UFO/status/display         (generate:pc-sample/status/displayer
		 "Unidentifiable Function Object (a.k.a. ``UFO'') "
        pc-sample/UFO/display))
  )

;; Structure [table] Displayers

(define pc-sample/builtin/display)
(define pc-sample/utility/display)
(define pc-sample/primitive/display)
(define pc-sample/code-block/display)
(define pc-sample/interp-proc/display)
(define pc-sample/prob-comp/display)
(define pc-sample/UFO/display)

(define (generate:pc-sample/table/displayer display-acater)
  (lambda ()
    (let ((displayee (display-acater)))
      (cond ((string? displayee)
	     (newline)
	     (display displayee))
	    ((vector? displayee)	; spec., #(sample-list BTW-string)
	     (display-sample-list (vector-ref displayee 0))
	     (display             (vector-ref displayee 1)))
	    (else
	     (display-sample-list displayee))))))

(define (display-sample-list sample-list) ; not integrated so can play w/ it
  (fluid-let ((*pp-default-as-code?* #T)) ; for now: just pp as code, but
    (pp sample-list)))			  ; maybe opt for wizzy graphics later

(define (install-displayers)
  (set! pc-sample/builtin/display     (generate:pc-sample/table/displayer
        pc-sample/builtin/display-acate))

  (set! pc-sample/utility/display     (generate:pc-sample/table/displayer
        pc-sample/utility/display-acate))

  (set! pc-sample/primitive/display   (generate:pc-sample/table/displayer
        pc-sample/primitive/display-acate))

  (set! pc-sample/code-block/display  (generate:pc-sample/table/displayer
        pc-sample/code-block/display-acate))

  (set! pc-sample/interp-proc/display (generate:pc-sample/table/displayer
        pc-sample/interp-proc/display-acate))

  (set! pc-sample/prob-comp/display   (generate:pc-sample/table/displayer
        pc-sample/prob-comp/display-acate))

  (set! pc-sample/UFO/display         (generate:pc-sample/table/displayer
        pc-sample/UFO/display-acate))
  )

;; Display-acaters (i.e., make a widget presentable for human readable display)
;;                 All display-acaters are presently *not* integrable so we
;;                 can interavtively play with them to explore display options.

(define *display-acation-status* #F)	; FLUID optional arg

(define (with-pc-sample-displayacation-status displayacation-status thunk)
  (fluid-let ((*display-acation-status* displayacation-status))
    (thunk)))

(define (pc-sample/builtin/display-acate)
  (pc-sample/indexed-vector-table/display-acate 
   pc-sample/status/builtin-table
   pc-sample/builtin-table
   "Built-Ins"
   'BUILTIN
   'BUILTIN-FNORD!
   get-builtin-name))

(define (pc-sample/utility/display-acate)
  (pc-sample/indexed-vector-table/display-acate
   pc-sample/status/utility-table
   pc-sample/utility-table
   "Utilities"
   'UTILITY
   'UTILITY-FNORD!
   get-utility-name))

(define (pc-sample/primitive/display-acate)
  (pc-sample/indexed-vector-table/display-acate
   pc-sample/status/primitive-table
   pc-sample/primitive-table
   "Primitives"
   'PRIMITIVE
   'PRIMITIVE-FNORD!
   get-primitive-name))

(define (pc-sample/indexed-vector-table/display-acate
	 pc-sample/status/mumble-table
	 pc-sample/mumble-table
	 mumble-string
	 mumble-ID
	 mumble-ID-fnord!
	 get-mumble-name)
  (cond ((if *display-acation-status*
	     (pc-sample/status/mumble-table *display-acation-status*)
	     (pc-sample/mumble-table))
	 =>
	 (lambda (mumble-tbl)
	   (let ((count-acc   0.)
		 (disp-stack '()))
	     (do ((index (-1+ (vector-length mumble-tbl)) (-1+ index)))
		 ((negative? index)
		  (if (null? disp-stack)
		      (string-append 
		       "; ++++ No " mumble-string "s Sampled Yet ++++")
		      `(,mumble-ID-fnord!
			,count-acc
			,@(sort-sample-list disp-stack))))
	       (let ((count (vector-ref mumble-tbl index)))
		 (cond ((not (flo:zero? count))
			(set! count-acc (flo:+ count count-acc))
			(set! disp-stack
			      `((,count
				 ,mumble-ID ,index ,(get-mumble-name index))
				. ,disp-stack)))))))))
	(else
	 (string-append "; **** [" mumble-string " Table Uninitialized]."))))

(define (pc-sample/code-block/display-acate)
  (let ((BTW-string 
	 (string-append
	  "\n"
	  ";..............................................................\n"
	  "; BTW:  Code Block Buffer Status --\n"
	  ";        "
	  "((plen . pslk)"
	  " (hlen . hslk))\n"
	  ";      = "
	  (write-to-string
	   (if *display-acation-status*
	       (pc-sample/status/code-block-buffer/status
		                                      *display-acation-status*)
	       (pc-sample/code-block-buffer/status))))))
    (if (code-block-profiling-disabled?)
	(no-code-blocks-of-sort "" BTW-string #F)
	(let* ((purified-count-cell (make-cell 0.))
	       ( heathen-count-cell (make-cell 0.))
	       (display-acated-p&h-lists
		(map (lambda (table label cable) ; 8 tables: 4 purified + 4 not
		       (vector->list
			(vector-map (lambda (elt)
				      (let* ((coblx (profile-hash-table-car elt))
					     (datum (profile-hash-table-cdr elt))
					     (count 
					      (code-block-profile-datum/count datum))
					     (name-list
					      (code-block/name/display-acate  coblx)))
					(set-cell-contents! cable
							    (flo:+ count
								   (cell-contents cable)))
					`(,count ,label ,coblx ,@name-list)))
				    table)))
		     (vector->list
		      (if *display-acation-status*
			  (pc-sample/status/code-block-table
			   *display-acation-status*)
			  (pc-sample/code-block-table)))
		     '((CODE-BLOCK PURIFIED COM-PROC)
		       (CODE-BLOCK PURIFIED DBG-INFO)
		       (CODE-BLOCK PURIFIED RAW-COBL)
		       (CODE-BLOCK PURIFIED TRAMPOLINE)
		       (CODE-BLOCK  HEATHEN COM-PROC)
		       (CODE-BLOCK  HEATHEN DBG-INFO)
		       (CODE-BLOCK  HEATHEN RAW-COBL)
		       (CODE-BLOCK  HEATHEN TRAMPOLINE)
		       )
		     `(,purified-count-cell ,purified-count-cell
		       ,purified-count-cell ,purified-count-cell
		        ,heathen-count-cell  ,heathen-count-cell
			,heathen-count-cell  ,heathen-count-cell
			)
		     ))
	       (display-acated-purified-list 
		`(,@(first  display-acated-p&h-lists)
		  ,@(second display-acated-p&h-lists)
		  ,@(third  display-acated-p&h-lists)
		  ,@(fourth display-acated-p&h-lists)
		  ))
	       (display-acated-heathen-list
		`(,@(fifth   display-acated-p&h-lists)
		  ,@(sixth   display-acated-p&h-lists)
		  ,@(seventh display-acated-p&h-lists)
		  ,@(eighth  display-acated-p&h-lists)
		  )))
	  (cond ((and (null? display-acated-purified-list)
		      (null? display-acated-heathen-list))
		 (no-code-blocks-of-sort "" BTW-string #F))
		((null? display-acated-heathen-list)
		 `#((PURIFIED-FNORD!
		     ,(cell-contents purified-count-cell)
		     ,@(sort-sample-list display-acated-purified-list))
		    ,(no-code-blocks-of-sort "Heathen"  BTW-string 'BTW)))
		((null? display-acated-purified-list)
		 `#((HEATHEN-FNORD! 
		     ,(cell-contents heathen-count-cell)
		     ,@(sort-sample-list display-acated-heathen-list))
		    ,(no-code-blocks-of-sort "Purified" BTW-string 'BTW)))
		(else
		 `#(#((PURIFIED-FNORD!
		       ,(cell-contents purified-count-cell)
		       ,@(sort-sample-list display-acated-purified-list))
		      (HEATHEN-FNORD! 
		       ,(cell-contents heathen-count-cell)
		       ,@(sort-sample-list display-acated-heathen-list)))
		    ,BTW-string)))))))

(define (compiled-entry-pointer? object) ; should live in /scheme/src/runtime/udata.scm
  (and (compiled-code-address?   object)
       (eq? (compiled-entry-type object) 'COMPILED-ENTRY)))

(define (compiled-procedure-entry?   obj) ; should live in /scheme/src/runtime/udata.scm
  (and (compiled-code-address?       obj)
       (or (compiled-procedure?      obj)
	   (compiled-return-address? obj)
	   (compiled-entry-pointer?  obj))))

(define *announce-trampoline-sightings?* #F)

(define (code-block/name/display-acate coblx) ; not integrable so can frob it
  (with-values
      (lambda ()
	(cond ((compiled-code-block?                      coblx)
	       (if (compiled-code-block/trampoline?       coblx)
		   (if (trampoline/return-to-interpreter? coblx)
		       (values 'RETURN_TO_INTERPRETER        69)
		       (values 'ABNORMAL_COMPILED_CODE_BLOCK 42))
		   (compiled-code-block/filename-and-index coblx)))
	      ((compiled-code-address?                    coblx)
	       (compiled-entry/filename-and-index         coblx))
	      (else
	       (values '<--- '<debugging-info>))))
    (lambda (filename offset)
      `(,(cond ((compiled-procedure-entry?                            coblx)
		(lambda/name/display-acate (compiled-procedure/lambda coblx)))
	       ((compiled-code-block/trampoline?                      coblx)
		(cond (*announce-trampoline-sightings?*
		       (newline)
		       (newline)
		       (display ";;;; ========== TRAMPOLINE ========== ")(display filename)
		       (newline)
		       (newline)))
		'-*-TRAMPOLINE-*-)
	       (else			; compiled-expr [loading], debugging-info, compclo
		(unsyntax/truthfully/sublist 5 (if (compiled-expression?      coblx)
						   (compiled-expression/scode coblx)
						   coblx))))
	,(if (null? filename) 
	     "[Not file-defined (i.e., interactively defined?)]"
	     filename)
	,(if (and (null? filename) (null? offset))
	     235
	     offset
	     )))))

(define-integrable (no-code-blocks-of-sort ID-string BTW-string BTW?)
  (string-append
   (if BTW? "\n" "")
   (if (string-null? ID-string)
       (if (code-block-profiling-disabled?)
	   "; **** [Code Block Profile Buffers Uninitialized]."
	   "; +++ No Code Blocks Sampled Yet +++")
       (string-append
	";~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
	"; +++ No " ID-string " Code Blocks Sampled Yet +++"))
   BTW-string))



(define (pc-sample/purified-trampoline/display-acate)
  (pc-sample/trampoline/display-acate 'PURIFIED 'PURIFIED-FNORD! "Purified" 0))

(define (pc-sample/heathen-trampoline/display-acate)
  (pc-sample/trampoline/display-acate 'HEATHEN   'HEATHEN-FNORD! "Heathen"  1))

(define-integrable (pc-sample/trampoline/display-acate ID ID-fnord! ID-string
						       pure/heathen-index)
  ;; Straightforwardly derived from full code-block display-ication...
  (let ((complete-code-block-display-acation
	 (pc-sample/code-block/display-acate)))
    (cond ((string? complete-code-block-display-acation)
	   (no-trampolines-of-sort ID-string))
	  ((vector? complete-code-block-display-acation)
	   (let* ((samples (vector-ref complete-code-block-display-acation 0))
		  (tramps
		   (cond ((vector? samples) ; #(tagged-pures tagged-heathens)
			  (filter-sorted-sample-list-by-label
			   `(CODE-BLOCK ,ID TRAMPOLINE)
			   (cddr (vector-ref samples pure/heathen-index))))
			 ;; Invariant: samples is tagged pair
			 ((eq? (car samples) ID-fnord!)
			  (filter-sorted-sample-list-by-label
			   `(CODE-BLOCK ,ID TRAMPOLINE)
			   (cddr samples)))
			 (else '())))
		  ;; tally # samples
		  (tramp-tally (apply + (map second tramps))))

	     (if (null? tramps)
		 (no-trampolines-of-sort ID-string)
		 `(,ID-fnord! ,tramp-tally ,@tramps))))
	  (else
	   (error "Unrecognized format from PC-SAMPLE/CODE-BLOCK/DISPLAY-ACATE"
		  complete-code-block-display-acation)))))

(define-integrable (filter-sorted-sample-list-by-label label sorted-sample-list)
  (list-transform-positive sorted-sample-list
    (lambda (elt)
      (equal? (second elt) label))))	; (# label ...)

(define-integrable (no-trampolines-of-sort ID-string)
  (string-append
   ";~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
   "; +++ No " ID-string " Trampolines Sampled Yet +++\n"
   ))
  

(define (pc-sample/interp-proc/display-acate)
  (let ((BTW-string 
	 (string-append
	  "\n"
	  ";..............................................................\n"
	  "; BTW:  Interp-Proc Buffer Status (length . slack) = "
	  (write-to-string 
	   (if *display-acation-status*
	       (pc-sample/status/interp-proc-buffer/status
		                                      *display-acation-status*)
	       (pc-sample/interp-proc-buffer/status))))))
    (if (interp-proc-profiling-disabled?)
	(string-append "; **** [Interp-Proc Profile Buffers Uninitialized]."
			BTW-string) 
	(let* ((tally 0.)
	       (display-acated-list
		(vector->list
		 (vector-map 
		  (lambda (elt)
		    (let* ((lambx (profile-hash-table-car elt))
			   (datum (profile-hash-table-cdr elt))
			   (count (interp-proc-profile-datum/count datum))
			   (name  (lambda/name/display-acate       lambx)))
		      (set! tally (flo:+ count tally))
		      `(,count INTERP-PROC ,lambx ,name)))
		  (if *display-acation-status*
		      (pc-sample/status/interp-proc-table
		       *display-acation-status*)
		      (pc-sample/interp-proc-table))))))
	  (if (null? display-acated-list)
	      (string-append "; +++ No Interp-Procs Sampled Yet +++"
			     BTW-string)
	      `#((INTERP-PROC-FNORD! ,tally
				     ,@(sort-sample-list display-acated-list))
		 ,BTW-string))))))

(define (lambda/name/display-acate lambx) ; not integrable so can play w/ it
  (if (meaningfully-named-lambda? lambx)
      (lambda-components* lambx
	(lambda (name required optional rest body)
	  body				; ignore
	  `(,name
	    ,@required
	    ,@(if (null? optional) '() `(#!OPTIONAL ,@optional))
	    . ,(if rest rest '()))))
      (unsyntax/truthfully/sublist 5 lambx)))

(define (unsyntax/truthfully/sublist lngth scode)
  (let ((lst (unsyntax/truthfully scode)))
    (if (not lst)
	'(-?-)
	(sublist lst 0 (-1+ (min lngth (length lst)))))))

(define (unsyntax/truthfully scode)
  (let ((un-env (->environment '(runtime unsyntaxer))))
    (fluid-let (((access unsyntaxer:macroize?      un-env) false)
      		((access unsyntaxer:show-comments? un-env) false))
      (unsyntax scode))))



(define (meaningfully-named-lambda? x)	; not integrated so can play w/ it
  (and (lambda? x)
       (not (nonmeaningful-lambda-name? (lambda-name x)))))

(define *nonmeaningful-procedure-names*	; exported for FLUID-LET-itude
  (list 'LOOP 'DO-LOOP 'ITER 'RECUR 'WALK 'SCAN 'TRAVERSE 'ACCUMULATE 'ACC
	'FOO 'BAR 'BAZ 'QUUX 'FOOBAR
	'SNAFU 'FROB 'FROBNITZ 'FROBNICATE
	'MUMBLE 'GRUMBLE 'FUMBLE 'TUMBLE
	'F 'G 'H 'J 'K
	'FNORD 'FNORD! 'IGNORE 'PUNT
	))

(define (nonmeaningful-lambda-name? raw-name) ; not integrated so can frob
  (or (uninterned-symbol? raw-name)
      (special-form-procedure-name? raw-name)
      (memq raw-name *nonmeaningful-procedure-names*)))


(define (pc-sample/prob-comp/display-acate)
  (trivial-ate-table
      (if *display-acation-status*
	  (pc-sample/status/prob-comp-table *display-acation-status*)
	  (pc-sample/prob-comp-table))
      '(PROB-COMP PURIFIED)
      '(PROB-COMP  HEATHEN)
      'PROB-COMP-FNORD!
      "Probably Compiled FNORD!"
      "; **** [Prob Comp FNORD! Counters Uninitialized]."))

(define (pc-sample/UFO/display-acate)
  (trivial-ate-table
      (if *display-acation-status*
	  (pc-sample/status/UFO-table *display-acation-status*)
	  (pc-sample/UFO-table))
      '(UFO HYPERSPACE)
      '(UFO CYBERSPACE)
      'UFO-FNORD!
      "UFO"
      (string-append "; **** [UFO Sightings Uninitialized] "
		     "(Project Blue Book Cancelled?).")))

(define (trivial-ate-table count-vector type-0 type-1 widget-ID-fnord!
			                              widget-ID-string
							 uninit-string)
  (if count-vector
      (let* ((count-0 (vector-ref count-vector 0))
	     (count-1 (vector-ref count-vector 1))
	     (no-0s?  (flo:zero?         count-0))
	     (no-1s?  (flo:zero?         count-1)))
	(if (and no-0s?
		 no-1s?)
	    (string-append "; +++ No " widget-ID-string "s Sampled Yet +++")
	    (let ((tally (flo:+ count-0 count-1))
		  (display-acated-list
		   (cond (no-0s? `((,count-1 ,type-1)))
			 (no-1s? `((,count-0 ,type-0)))
			 (else   `((,count-0 ,type-0)
				   (,count-1 ,type-1))))))
	      `(,widget-ID-fnord! ,tally
				  ,@(sort-sample-list display-acated-list)))))
      uninit-string))

(declare (integrate-operator trivial-ate-table))

(define-integrable (sort-sample-list sample-list)
  (sort sample-list			; sample-list := ((flonum ...)...)
	(lambda (sample1 sample2)
	  (flo:> (car sample1)
		 (car sample2)))))

;;; Tabulations

(define (pc-sample/status/table . display-acaters)
  ;; defaulted optional rest args
  (let* ((real-display-acaters
	  (if (null? display-acaters)	; no opt rest arg
	      (list pc-sample/builtin/display-acate
	            pc-sample/utility/display-acate
	            pc-sample/primitive/display-acate
		    pc-sample/code-block/display-acate
		    pc-sample/interp-proc/display-acate
		    pc-sample/prob-comp/display-acate
		    pc-sample/UFO/display-acate)
	      display-acaters))
	 ;; Lie: should store sample interval in the table some how. Sigh.
	 (sample-interval (pc-sample/sample-interval))
	 (tally 0.)
	 ;; Do (apply append (map (.\ (dcr-thunk) ...) real-dcrs))
	 (display-acatees
	  (map (lambda (dcr-thunk)
		 (let* ((raw-display-acatee (dcr-thunk))
			(half-baked-display-acatee
			 (cond ((string? raw-display-acatee)
				'(FNORD! 0.))
			       ((vector? raw-display-acatee)
				;; spec., #(sample-list BTW-string)
				(vector-ref raw-display-acatee 0))
			       (else        raw-display-acatee   ))))
		   ;; Cook half-baked display-acatee
		   (cond ((pair?   half-baked-display-acatee)
			  (set! tally 
				(+ (second half-baked-display-acatee) tally))
			  (cddr    half-baked-display-acatee)) ; de-fnord-ize
			 ((vector? half-baked-display-acatee)
			  ;; e.g., #((purified...)(heathen...))
			  ;; Do (apply append (map cdr lst))
			  (cddr (reduce-right
				 (lambda (l r)
				   (let ((l-count (second l))
					 (r-count (second r)))
				     (set! tally
					   (flo:+ (flo:+ l-count
							 r-count) ; Grrr
						  tally))
				     `(FNORD! 0. ,@(cddr l) ,@(cddr r))))
				 '(FNORD!-TO-CDR-IF-NULL-HALF-BAKED-DISPEES)
				 (vector->list half-baked-display-acatee))))
			 (else
			  (error "Unknown display-acatee format"
				 half-baked-display-acatee)))))
	       real-display-acaters))
	 (merged-status (reduce-right append '() display-acatees)) ; flatten
	 (sorted-status (sort-sample-list merged-status))
	 (percent-sorted-status
	  (map (lambda (ntry)
		 `(,(percenticate  (car ntry) tally)
		   ,(relevanticate (car ntry) tally sample-interval)
		   ,@ntry))
	       sorted-status)))
#|
    ;; Reality check...
    ;; Do: (apply + (map car lst))... reality check...
    (let ((total-count (apply + (map car sorted-status))))
      (cond ((not (flo:= total-count tally))
	     (warn "; Damned total-count != tally. Foo." total-count tally))))
|#
    (display-sample-list percent-sorted-status)))


(define *pc-sample/status/table/decimal-pump* 100000.)	; want 5 decimal places

(define-integrable (percenticate numer denom)
  ;; Standard hack: pump up the numerator, round it, then deflate result.
  (let ((pumped-percentage
	 (flo:/ (flo:* (flo:* numer 100.) 		      ; percent-icate
		       *pc-sample/status/table/decimal-pump*) ; decimal pump
		denom)))
    (flo:/ (flo:round pumped-percentage)
	   *pc-sample/status/table/decimal-pump*)))
					     
(define-integrable (relevanticate numer denom interval)
  `#(,numer ,denom ,(make-rectangular (/ (flo:round->exact numer)
					 (flo:round->exact denom))
				      interval)))


(define-integrable       (pc-sample/builtin/status/table)
  (pc-sample/status/table pc-sample/builtin/display-acate))

(define-integrable       (pc-sample/utility/status/table)
  (pc-sample/status/table pc-sample/utility/display-acate))

(define-integrable       (pc-sample/primitive/status/table)
  (pc-sample/status/table pc-sample/primitive/display-acate))

(define-integrable       (pc-sample/code-block/status/table)
  (pc-sample/status/table pc-sample/code-block/display-acate))

(define-integrable       (pc-sample/interp-proc/status/table)
  (pc-sample/status/table pc-sample/interp-proc/display-acate))

(define-integrable       (pc-sample/prob-comp/status/table)
  (pc-sample/status/table pc-sample/prob-comp/display-acate))

(define-integrable       (pc-sample/UFO/status/table)
  (pc-sample/status/table pc-sample/UFO/display-acate))


(define-integrable       (pc-sample/purified-trampoline/status/table)
  (pc-sample/status/table pc-sample/purified-trampoline/display-acate))

(define-integrable       (pc-sample/heathen-trampoline/status/table)
  (pc-sample/status/table pc-sample/heathen-trampoline/display-acate))


;;; Default status displayer

(define *pc-sample/default-status-displayer*)

(define   (with-pc-sample-default-status-displayer  status-displayer    thunk)
  (fluid-let ((*pc-sample/default-status-displayer* status-displayer)) (thunk)))

(define (install-default-status-displayer)
  (set! *pc-sample/default-status-displayer* pc-sample/status/table)
  )

;;; Install

(define (install)
  (install-displayers)			; NB: Must load this before status-disp
  (install-status-displayers)
  (install-default-status-displayer)
  )

;;; fini
