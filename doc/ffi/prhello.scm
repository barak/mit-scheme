#| -*-Scheme-*-

This is Havoc Pennington's Hello World example from GGAD, in the raw
FFI.  Note that no arrangements have been made to de-register the
callbacks. |#

(declare (usual-integrations))

(C-include "prhello")

(define (hello)
  (C-call "gtk_init" 0 null-alien)
  (let ((window (let ((alien (make-alien '|GtkWidget|)))
		  (C-call "gtk_window_new" alien
			  (C-enum "GTK_WINDOW_TOPLEVEL"))
		  (if (alien-null? alien) (error "Could not create window."))
		  alien))
	(button (let ((alien (make-alien '|GtkWidget|)))
		  (C-call "gtk_button_new" alien)
		  (if (alien-null? alien) (error "Could not create button."))
		  alien))
	(label (let ((alien (make-alien '|GtkWidget|)))
		 (C-call "gtk_label_new" alien "Hello, World!")
		 (if (alien-null? alien) (error "Could not create label."))
		 alien)))
    (C-call "gtk_container_add" button label)
    (C-call "gtk_container_add" window button)
    (C-call "gtk_window_set_title" window "Hello")
    (C-call "gtk_container_set_border_width" button 10)
    (let ((counter 0))
      (C-call "g_signal_connect" window "delete_event"
	      (C-callback "delete_event")	;trampoline
	      (C-callback			;callback ID
	       (lambda (w e)
		 (outf-console ";Delete me "(- 2 counter)" times.\n")
		 (set! counter (1+ counter))
		 ;; Three or more is the charm.
		 (if (> counter 2)
		     (begin
		       (C-call "gtk_main_quit")
		       0)
		     1))))
      (C-call "g_signal_connect" button "clicked"
	      (C-callback "clicked")	;trampoline
	      (C-callback			;callback ID
	       (lambda (w)
		 (let ((gstring (make-alien '(* |gchar|))))
		   (C-call "gtk_label_get_text" gstring label)
		   (let ((text (c-peek-cstring gstring)))
		     (C-call "gtk_label_set_text" label
			     (list->string (reverse! (string->list text))))))
		 unspecific))))
    (C-call "gtk_widget_show_all" window)
    (C-call "gtk_main")
    window))