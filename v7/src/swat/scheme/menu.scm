;;; -*- Scheme -*-

;;; <MenuItem> class

(define *all-menus* '())

(define (MenuItem.Menu item)
  (MenuRecord.Menu (MenuItem.MenuRecord item)))

(define (find-menu-record menu)
  (let loop ((prev #F)
	     (rest *all-menus*))
    (cond ((null? rest)
	   (error "Find-Menu-Record: Can't find record" menu))
	  ((null? (weak-car rest))
	   (if prev
	       (weak-set-cdr! prev (weak-cdr rest))
	       (set! *all-menus* (weak-cdr rest)))
	   (loop prev (weak-cdr rest)))
	  ((eq? (MenuRecord.Menu (weak-car rest)) menu)
	   (weak-car rest))
	  (else (loop rest (weak-cdr rest))))))

(define (add-sub-menu menu sub-menu . options)
  (set-TKWidget%.do-not-gc-protect! sub-menu #T)
  (UIObj-protect-from-gc! sub-menu menu) ; Keep daddy alive ...
  ;; Above must happen *before* add-child!, since the menu may already
  ;; have a drawing surface and then the sub-menu would get it
  ;; immediately and the protection wouldn't be removed.  Get it?
  (add-child! menu sub-menu)
  (apply add-to-menu menu 'cascade
	 '-menu (lambda () (tk-widget->pathname sub-menu))
	 options))

(define (menuitem-set-callback! me proc)
  (if (not (MenuItem.Index me))
      (error "SET-CALLBACK!: menu item deleted!" me))
  (set-MenuItem.%callback! me proc)
  (ask-widget (MenuItem.Menu me)
	      `(entryconfigure
		,(MenuItem.Index me)
		-command
		,(string-append "SchemeCallBack "
				(number->string
				 (hash proc *our-hash-table*))))))

(define (delete-menuitem! item)
  (let ((menu-record (MenuItem.MenuRecord item))
	(index (MenuItem.Index item)))
    (let loop ((rest (MenuRecord.Items menu-record))
	       (count 0)
	       (prev #F))
      (cond ((null? rest)
	     (ask-widget (MenuRecord.Menu menu-record)
			 `(delete ,(MenuItem.index item)))
	     (set-MenuItem.index! item #F)
	     'DONE)
	    ((eq? (car rest) item)
	     (if (not (= count Index))
		 (error "Delete-MenuItem!: Inconsistent count"
			count index me))
	     
	     (if prev
		 (set-cdr! prev (cdr rest))
		 (set-MenuRecord.Items! menu-record (cdr rest)))
	     (loop (cdr rest) (+ count 1) prev))
	    ((= count index)
	     (error "Delete-MenuItem!: Missing item" count index me))
	    (else
	     (if (> count index)
		 (Set-MenuItem.Index! (car rest) (- count 1)))
	     (loop (cdr rest) (+ count 1) rest))))))

(define (menuitem-ask-widget me command)
  ;; For example:
  ;; (Ask-Widget me `(configure -label "George"))
  ;; becomes
  ;; (Ask-Widget menu `(entryconfigure index -label "George"))
  (if (or (eq? (car command) 'configure)
	  (string=? (car command) "configure"))
      (ask-widget (MenuItem.Menu me)
		  `(entryconfigure ,(MenuItem.Index me)
				   ,@(cdr command)))
      (error "MenuItem-Ask-Widget: must be configure command"
	     me command)))

(define (add-to-menu menu which-kind . options)
  ;; Which-Kind should be 'CHECKBUTTON, 'COMMAND,
  ;; 'RADIOBUTTON, or 'SEPARATOR.  Cascades are made using
  ;; Add-Sub-Menu, above
  (let ((menu-record (find-menu-record menu)))
    (let ((items (MenuRecord.Items Menu-Record)))
      (let ((new-item
	     (make-menuitem menuitem-ask-widget
			    'invalid
			    menuitem-set-callback!
			    menu-record
			    '()
			    (length items))))
	(ask-widget menu `(add ,which-kind ,@options))
	(set-MenuRecord.Items! Menu-Record
			       (append! items (list new-item)))
	new-item))))

