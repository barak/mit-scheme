;;;-*-Scheme-*-

(C-include "ffi-test")

(define (test-ffi)
  (let* ((struct (malloc (c-sizeof "TestStruct") '|TestStruct|))
	 (string "input string")
	 (pi (* 4 (atan 1 1)))
	 (chars (malloc (1+ (* (c-sizeof "char") (string-length string)))
			'(* char))))
    (C->= struct "TestStruct first" (char->ascii #\A))
    (C->= struct "TestStruct second" pi)
    (C->= struct "TestStruct third" (char->ascii #\C))
    (c-poke-string chars string)
    (C->= struct "TestStruct fourth" chars)
    (C-call "test_register_double"
	    (C-callback "test_double_callback")
	    (C-callback (lambda (d) (* d pi))))
    (list
     (let ((d (C-call "test_double" pi struct)))
       (assert-equal (* pi pi pi) d))
     (assert-equal (number->string (* 2 (string-length string)))
		   (let* ((alien (make-alien-to-free
				  '(* char)
				  (lambda (retval)
				    (C-call "test_string" retval
					    string struct))))
			  (new (c-peek-cstring alien)))
		     (free alien)
		     new)))))