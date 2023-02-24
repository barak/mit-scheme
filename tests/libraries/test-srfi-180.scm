#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Tests of SRFI 180 implementation

;; See <https://seriot.ch/projects/parsing_json.html> for the explanatory
;; article "Parsing JSON is a Minefield".

(declare (usual-integrations))

(import-to-top-level-environment! (the-environment)
				  '(srfi 180))

(define-record-type <condition-expectation>
    (%make-condition-expectation predicate message irritants)
    condition-expectation?
  (predicate condition-expectation-predicate)
  (message condition-expectation-message)
  (irritants condition-expectation-irritants))

(define (expect-condition predicate #!optional message irritants)
  (%make-condition-expectation predicate message irritants))

(define expected-hex-digit
  (expect-condition json-error? "Expected hex digit:"))

(define expected-high-surrogate
  (expect-condition json-error? "Expected Unicode high surrogate:"))

(define expected-low-surrogate
  (expect-condition json-error? "Expected Unicode low surrogate:"))

(define expected-string-unicode-escape
  (expect-condition json-error? "Expected \\u but got:"))

(define expected-string-char
  (expect-condition json-error? "Expected JSON string char:"))

(define expected-string-escape
  (expect-condition json-error? "Expected JSON string escape:"))

(define invalid-initial-char
  (expect-condition json-error? "Invalid JSON initial character:"))

(define (invalid-token-in-state token state)
  (expect-condition json-error? "Invalid token in state:" (list token state)))

(define invalid-number-syntax
  (expect-condition json-error? "Invalid number syntax:"))

(define premature-eof
  (expect-condition json-error? "Premature EOF."))

(define char-encoding-error
  (expect-condition
   (condition-predicate condition-type:char-decoding-error)))

(define test-expectations
  `(("y_array_arraysWithSpaces"
     (json-generator (array-start array-start array-end array-end))
     (#t #(#())))
    ("y_array_empty"
     (json-generator (array-start array-end))
     (#t #()))
    ("y_array_empty-string"
     (json-generator (array-start "" array-end))
     (#t #("")))
    ("y_array_ending_with_newline"
     (json-generator (array-start "a" array-end))
     (#t #("a")))
    ("y_array_false"
     (json-generator (array-start #f array-end))
     (#t #(#f)))
    ("y_array_heterogeneous"
     (json-generator (array-start null 1 "1"
                                  object-start object-end
                                  array-end))
     (#t #(null 1 "1" ())))
    ("y_array_null"
     (json-generator (array-start null array-end))
     (#t #(null)))
    ("y_array_with_1_and_newline"
     (json-generator (array-start 1 array-end))
     (json-lines-read ,(invalid-token-in-state 'eof 'array-comma))
     (#t #(1)))
    ("y_array_with_leading_space"
     (json-generator (array-start 1 array-end))
     (#t #(1)))
    ("y_array_with_several_null"
     (json-generator (array-start 1 null null null 2 array-end))
     (#t #(1 null null null 2)))
    ("y_array_with_trailing_space"
     (json-generator (array-start 2 array-end))
     (#t #(2)))
    ("y_number_0e+1"
     (json-generator (array-start 0. array-end))
     (#t #(0.)))
    ("y_number_0e1"
     (json-generator (array-start 0. array-end))
     (#t #(0.)))
    ("y_number_after_space"
     (json-generator (array-start 4 array-end))
     (#t #(4)))
    ("y_number_double_close_to_zero"
     (json-generator (array-start -1e-78 array-end))
     (#t #(-1e-78)))
    ("y_number_int_with_exp"
     (json-generator (array-start 200. array-end))
     (#t #(200.)))
    ("y_number"
     (json-generator (array-start 1.23e67 array-end))
     (#t #(1.23e67)))
    ("y_number_minus_zero"
     (json-generator (array-start 0 array-end))
     (#t #(0)))
    ("y_number_negative_int"
     (json-generator (array-start -123 array-end))
     (#t #(-123)))
    ("y_number_negative_one"
     (json-generator (array-start -1 array-end))
     (#t #(-1)))
    ("y_number_negative_zero"
     (json-generator (array-start 0 array-end))
     (#t #(0)))
    ("y_number_real_capital_e"
     (json-generator (array-start 1e22 array-end))
     (#t #(1e22)))
    ("y_number_real_capital_e_neg_exp"
     (json-generator (array-start .01 array-end))
     (#t #(.01)))
    ("y_number_real_capital_e_pos_exp"
     (json-generator (array-start 100. array-end))
     (#t #(100.)))
    ("y_number_real_exponent"
     (json-generator (array-start 1.23e47 array-end))
     (#t #(1.23e47)))
    ("y_number_real_fraction_exponent"
     (json-generator (array-start 1.23456e80 array-end))
     (#t #(1.23456e80)))
    ("y_number_real_neg_exp"
     (json-generator (array-start .01 array-end))
     (#t #(.01)))
    ("y_number_real_pos_exponent"
     (json-generator (array-start 100. array-end))
     (#t #(100.)))
    ("y_number_simple_int"
     (json-generator (array-start 123 array-end))
     (#t #(123)))
    ("y_number_simple_real"
     (json-generator (array-start 123.456789 array-end))
     (#t #(123.456789)))
    ("y_object_basic"
     (json-generator (object-start "asd" "sdf" object-end))
     (#t ((asd . "sdf"))))
    ("y_object_duplicated_key_and_value"
     (json-generator (object-start "a" "b" "a" "b" object-end))
     (#t ((a . "b") (a . "b"))))
    ("y_object_duplicated_key"
     (json-generator (object-start "a" "b" "a" "c" object-end))
     (#t ((a . "b") (a . "c"))))
    ("y_object_empty"
     (json-generator (object-start object-end))
     (#t ()))
    ("y_object_empty_key"
     (json-generator (object-start "" 0 object-end))
     (#t ((|| . 0))))
    ("y_object_escaped_null_in_key"
     (json-generator (object-start "foo\x0;bar" 42 object-end))
     (#t ((|foo\x0;bar| . 42))))
    ("y_object_extreme_numbers"
     (json-generator (object-start "min" -1e28 "max" 1e28 object-end))
     (#t ((min . -1e28) (max . 1e28))))
    ("y_object"
     (json-generator (object-start "asd" "sdf" "dfg" "fgh" object-end))
     (#t ((asd . "sdf") (dfg . "fgh"))))
    ("y_object_long_strings"
     (json-generator
      (object-start "x"
                    array-start
                    object-start
                    "id"
                    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                    object-end
                    array-end
                    "id"
                    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                    object-end))
     (#t
      ((x . #(((id . "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))))
       (id . "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))))
    ("y_object_simple"
     (json-generator (object-start "a" array-start array-end object-end))
     (#t ((a . #()))))
    ("y_object_string_unicode"
     (json-generator (object-start "title" "Полтора Землекопа" object-end))
     (#t ((title . "Полтора Землекопа"))))
    ("y_object_with_newlines"
     (json-generator (object-start "a" "b" object-end))
     (json-lines-read ,(invalid-token-in-state 'eof 'object-first))
     (#t ((a . "b"))))
    ("y_string_1_2_3_bytes_UTF-8_sequences"
     (json-generator (array-start "`\x12a;\x12ab;" array-end))
     (#t #("`\x12a;\x12ab;")))
    ("y_string_accepted_surrogate_pair"
     (json-generator (array-start "\x10437;" array-end))
     (#t #("\x10437;")))
    ("y_string_accepted_surrogate_pairs"
     (json-generator (array-start "\x1f639;\x1f48d;" array-end))
     (#t #("\x1f639;\x1f48d;")))
    ("y_string_allowed_escapes"
     (json-generator (array-start "\"\\/\b\xc;\n\r\t" array-end))
     (#t #("\"\\/\b\xc;\n\r\t")))
    ("y_string_backslash_and_u_escaped_zero"
     (json-generator (array-start "\\u0000" array-end))
     (#t #("\\u0000")))
    ("y_string_backslash_doublequotes"
     (json-generator (array-start "\"" array-end))
     (#t #("\"")))
    ("y_string_comments"
     (json-generator (array-start "a/*b*/c/*d//e" array-end))
     (#t #("a/*b*/c/*d//e")))
    ("y_string_double_escape_a"
     (json-generator (array-start "\\a" array-end))
     (#t #("\\a")))
    ("y_string_double_escape_n"
     (json-generator (array-start "\\n" array-end))
     (#t #("\\n")))
    ("y_string_escaped_control_character"
     (json-generator (array-start "\x12;" array-end))
     (#t #("\x12;")))
    ("y_string_escaped_noncharacter"
     (json-generator (array-start "\xffff;" array-end))
     (#t #("\xffff;")))
    ("y_string_in_array"
     (json-generator (array-start "asd" array-end))
     (#t #("asd")))
    ("y_string_in_array_with_leading_space"
     (json-generator (array-start "asd" array-end))
     (#t #("asd")))
    ("y_string_last_surrogates_1_and_2"
     (json-generator (array-start "\x10ffff;" array-end))
     (#t #("\x10ffff;")))
    ("y_string_nbsp_uescaped"
     (json-generator (array-start "new line" array-end))
     (#t #("new line")))
    ("y_string_nonCharacterInUTF-8_U+10FFFF"
     (json-generator (array-start "\x10ffff;" array-end))
     (#t #("\x10ffff;")))
    ("y_string_nonCharacterInUTF-8_U+FFFF"
     (json-generator (array-start "\xffff;" array-end))
     (#t #("\xffff;")))
    ("y_string_null_escape"
     (json-generator (array-start "\x0;" array-end))
     (#t #("\x0;")))
    ("y_string_one-byte-utf-8"
     (json-generator (array-start "," array-end))
     (#t #(",")))
    ("y_string_pi"
     (json-generator (array-start "\x3c0;" array-end))
     (#t #("\x3c0;")))
    ("y_string_reservedCharacterInUTF-8_U+1BFFF"
     (json-generator (array-start "\x1bfff;" array-end))
     (#t #("\x1bfff;")))
    ("y_string_simple_ascii"
     (json-generator (array-start "asd " array-end))
     (#t #("asd ")))
    ("y_string_space"
     (json-generator (" "))
     (#t " "))
    ("y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF"
     (json-generator (array-start "\x1d11e;" array-end))
     (#t #("\x1d11e;")))
    ("y_string_three-byte-utf-8"
     (json-generator (array-start "\x821;" array-end))
     (#t #("\x821;")))
    ("y_string_two-byte-utf-8"
     (json-generator (array-start "\x123;" array-end))
     (#t #("\x123;")))
    ("y_string_u+2028_line_sep"
     (json-generator (array-start "\x2028;" array-end))
     (#t #("\x2028;")))
    ("y_string_u+2029_par_sep"
     (json-generator (array-start "\x2029;" array-end))
     (#t #("\x2029;")))
    ("y_string_uescaped_newline"
     (json-generator (array-start "new\nline" array-end))
     (#t #("new\nline")))
    ("y_string_uEscape"
     (json-generator (array-start "a\x30af;\x30ea;\x30b9;" array-end))
     (#t #("a\x30af;\x30ea;\x30b9;")))
    ("y_string_unescaped_char_delete"
     (json-generator (array-start "\x7f;" array-end))
     (#t #("\x7f;")))
    ("y_string_unicode_2"
     (json-generator (array-start "\x2342;\x3234;\x2342;" array-end))
     (#t #("\x2342;\x3234;\x2342;")))
    ("y_string_unicodeEscapedBackslash"
     (json-generator (array-start "\\" array-end))
     (#t #("\\")))
    ("y_string_unicode_escaped_double_quote"
     (json-generator (array-start "\"" array-end))
     (#t #("\"")))
    ("y_string_unicode"
     (json-generator (array-start "\xa66d;" array-end))
     (#t #("\xa66d;")))
    ("y_string_unicode_U+10FFFE_nonchar"
     (json-generator (array-start "\x10fffe;" array-end))
     (#t #("\x10fffe;")))
    ("y_string_unicode_U+1FFFE_nonchar"
     (json-generator (array-start "\x1fffe;" array-end))
     (#t #("\x1fffe;")))
    ("y_string_unicode_U+200B_ZERO_WIDTH_SPACE"
     (json-generator (array-start "\x200b;" array-end))
     (#t #("\x200b;")))
    ("y_string_unicode_U+2064_invisible_plus"
     (json-generator (array-start "\x2064;" array-end))
     (#t #("\x2064;")))
    ("y_string_unicode_U+FDD0_nonchar"
     (json-generator (array-start "\xfdd0;" array-end))
     (#t #("\xfdd0;")))
    ("y_string_unicode_U+FFFE_nonchar"
     (json-generator (array-start "\xfffe;" array-end))
     (#t #("\xfffe;")))
    ("y_string_utf8"
     (json-generator (array-start "\x20ac;\x1d11e;" array-end))
     (#t #("\x20ac;\x1d11e;")))
    ("y_string_with_del_character"
     (json-generator (array-start "a\x7f;a" array-end))
     (#t #("a\x7f;a")))
    ("y_structure_lonely_false"
     (json-generator (#f))
     (#t #f))
    ("y_structure_lonely_int"
     (json-generator (42))
     (#t 42))
    ("y_structure_lonely_negative_real"
     (json-generator (-.1))
     (#t -.1))
    ("y_structure_lonely_null"
     (json-generator (null))
     (#t null))
    ("y_structure_lonely_string"
     (json-generator ("asd"))
     (#t "asd"))
    ("y_structure_lonely_true"
     (json-generator (#t))
     (#t #t))
    ("y_structure_string_empty"
     (json-generator (""))
     (#t ""))
    ("y_structure_trailing_newline"
     (json-generator (array-start "a" array-end))
     (#t #("a")))
    ("y_structure_true_in_array"
     (json-generator (array-start #t array-end))
     (#t #(#t)))
    ("y_structure_whitespace_array"
     (json-generator (array-start array-end))
     (#t #()))
    ("n_array_1_true_without_comma"
     (#t ,(invalid-token-in-state 'non-string 'array-comma)))
    ("n_array_a_invalid_utf8"
     (#t ,invalid-initial-char))
    ("n_array_colon_instead_of_comma"
     (#t ,(invalid-token-in-state 'colon 'array-comma)))
    ("n_array_comma_after_close"
     (#t ,(invalid-token-in-state 'comma 'final)))
    ("n_array_comma_and_number"
     (#t ,(invalid-token-in-state 'comma 'array-first)))
    ("n_array_double_comma"
     (#t ,(invalid-token-in-state 'comma 'array-next)))
    ("n_array_double_extra_comma"
     (#t ,(invalid-token-in-state 'comma 'array-next)))
    ("n_array_extra_close"
     (#t ,(invalid-token-in-state 'array-end 'final)))
    ("n_array_extra_comma"
     (#t ,(invalid-token-in-state 'array-end 'array-next)))
    ("n_array_incomplete"
     (#t ,(invalid-token-in-state 'eof 'array-comma)))
    ("n_array_incomplete_invalid_value"
     (#t ,invalid-initial-char))
    ("n_array_inner_array_no_comma"
     (#t ,(invalid-token-in-state 'array-start 'array-comma)))
    ("n_array_invalid_utf8"
     (#t ,invalid-initial-char))
    ("n_array_items_separated_by_semicolon"
     (#t ,(invalid-token-in-state 'colon 'array-comma)))
    ("n_array_just_comma"
     (#t ,(invalid-token-in-state 'comma 'array-first)))
    ("n_array_just_minus"
     (#t ,invalid-number-syntax))
    ("n_array_missing_value"
     (#t ,(invalid-token-in-state 'comma 'array-first)))
    ("n_array_newlines_unclosed"
     (#t ,(invalid-token-in-state 'eof 'array-next)))
    ("n_array_number_and_comma"
     (#t ,(invalid-token-in-state 'array-end 'array-next)))
    ("n_array_number_and_several_commas"
     (#t ,(invalid-token-in-state 'comma 'array-next)))
    ("n_array_spaces_vertical_tab_formfeed"
     (#t ,expected-string-char))
    ("n_array_star_inside"
     (#t ,invalid-initial-char))
    ("n_array_unclosed"
     (#t ,(invalid-token-in-state 'eof 'array-comma)))
    ("n_array_unclosed_trailing_comma"
     (#t ,(invalid-token-in-state 'eof 'array-next)))
    ("n_array_unclosed_with_new_lines"
     (json-lines-read ,(invalid-token-in-state 'eof 'array-next))
     (#t ,(invalid-token-in-state 'eof 'array-comma)))
    ("n_array_unclosed_with_object_inside"
     (#t ,(invalid-token-in-state 'eof 'array-comma)))
    ("n_incomplete_false"
     (#t ,(expect-condition json-error? "Expected e but got:" '(#\]))))
    ("n_incomplete_null"
     (#t ,(expect-condition json-error? "Expected l but got:" '(#\]))))
    ("n_incomplete_true"
     (#t ,(expect-condition json-error? "Expected e but got:" '(#\]))))
    ("n_multidigit_number_then_00"
     (#t ,invalid-initial-char))
    ("n_number_++"
     (#t ,invalid-initial-char))
    ("n_number_+1"
     (#t ,invalid-initial-char))
    ("n_number_+Inf"
     (#t ,invalid-initial-char))
    ("n_number_-01"
     (#t ,invalid-number-syntax))
    ("n_number_-NaN"
     (#t ,invalid-number-syntax))
    ("n_number_.-1"
     (#t ,invalid-initial-char))
    ("n_number_.2e-3"
     (#t ,invalid-initial-char))
    ("n_number_0.1.2"
     (#t ,invalid-number-syntax))
    ("n_number_0.3e"
     (#t ,invalid-number-syntax))
    ("n_number_0.3e+"
     (#t ,invalid-number-syntax))
    ("n_number_0.e1"
     (#t ,invalid-number-syntax))
    ("n_number_0_capital_E"
     (#t ,invalid-number-syntax))
    ("n_number_0_capital_E+"
     (#t ,invalid-number-syntax))
    ("n_number_0e"
     (#t ,invalid-number-syntax))
    ("n_number_0e+"
     (#t ,invalid-number-syntax))
    ("n_number_1.0e"
     (#t ,invalid-number-syntax))
    ("n_number_1.0e+"
     (#t ,invalid-number-syntax))
    ("n_number_1.0e-"
     (#t ,invalid-number-syntax))
    ("n_number_1_000"
     (#t ,invalid-number-syntax))
    ("n_number_1eE2"
     (#t ,invalid-number-syntax))
    ("n_number_2.e+3"
     (#t ,invalid-number-syntax))
    ("n_number_2.e-3"
     (#t ,invalid-number-syntax))
    ("n_number_2.e3"
     (#t ,invalid-number-syntax))
    ("n_number_9.e+"
     (#t ,invalid-number-syntax))
    ("n_number_Inf"
     (#t ,invalid-initial-char))
    ("n_number_NaN"
     (#t ,invalid-initial-char))
    ("n_number_U+FF11_fullwidth_digit_one"
     (#t ,invalid-initial-char))
    ("n_number_expression"
     (#t ,invalid-number-syntax))
    ("n_number_hex_1_digit"
     (#t ,invalid-initial-char))
    ("n_number_hex_2_digits"
     (#t ,invalid-initial-char))
    ("n_number_infinity"
     (#t ,invalid-initial-char))
    ("n_number_invalid+-"
     (#t ,invalid-number-syntax))
    ("n_number_invalid-negative-real"
     (#t ,(expect-condition json-error? "Expected a but got:" '(#\o))))
    ("n_number_invalid-utf-8-in-bigger-int"
     (#t ,char-encoding-error))
    ("n_number_invalid-utf-8-in-exponent"
     (#t ,char-encoding-error))
    ("n_number_invalid-utf-8-in-int"
     (#t ,invalid-initial-char))
    ("n_number_minus_infinity"
     (#t ,invalid-number-syntax))
    ("n_number_minus_sign_with_trailing_garbage"
     (#t ,invalid-number-syntax))
    ("n_number_minus_space_1"
     (#t ,invalid-number-syntax))
    ("n_number_neg_int_starting_with_zero"
     (#t ,invalid-number-syntax))
    ("n_number_neg_real_without_int_part"
     (#t ,invalid-number-syntax))
    ("n_number_neg_with_garbage_at_end"
     (#t ,invalid-initial-char))
    ("n_number_real_garbage_after_e"
     (#t ,invalid-number-syntax))
    ("n_number_real_with_invalid_utf8_after_e"
     (#t ,char-encoding-error))
    ("n_number_real_without_fractional_part"
     (#t ,invalid-number-syntax))
    ("n_number_starting_with_dot"
     (#t ,invalid-initial-char))
    ("n_number_with_alpha"
     (#t ,invalid-initial-char))
    ("n_number_with_alpha_char"
     (#t ,invalid-initial-char))
    ("n_number_with_leading_zero"
     (#t ,invalid-number-syntax))
    ("n_object_bad_value"
     (#t ,(expect-condition json-error? "Expected e but got:" '(#\t))))
    ("n_object_bracket_key"
     (#t ,(invalid-token-in-state 'array-start 'object-first)))
    ("n_object_comma_instead_of_colon"
     (#t ,(invalid-token-in-state 'comma 'object-colon)))
    ("n_object_double_colon"
     (#t ,(invalid-token-in-state 'colon 'object-value)))
    ("n_object_emoji"
     (#t ,invalid-initial-char))
    ("n_object_garbage_at_end"
     (#t ,(invalid-token-in-state 'non-string 'object-comma)))
    ("n_object_key_with_single_quotes"
     (#t ,invalid-initial-char))
    ("n_object_lone_continuation_byte_in_key_and_trailing_comma"
     (#t ,(invalid-token-in-state 'object-end 'object-next)))
    ("n_object_missing_colon"
     (#t ,invalid-initial-char))
    ("n_object_missing_key"
     (#t ,(invalid-token-in-state 'colon 'object-first)))
    ("n_object_missing_semicolon"
     (#t ,(invalid-token-in-state 'string 'object-colon)))
    ("n_object_missing_value"
     (#t ,(invalid-token-in-state 'eof 'object-value)))
    ("n_object_no-colon"
     (#t ,(invalid-token-in-state 'eof 'object-colon)))
    ("n_object_non_string_key"
     (#t ,(invalid-token-in-state 'non-string 'object-first)))
    ("n_object_non_string_key_but_huge_number_instead"
     (#t ,invalid-number-syntax))
    ("n_object_repeated_null_null"
     (#t ,(invalid-token-in-state 'non-string 'object-first)))
    ("n_object_several_trailing_commas"
     (#t ,(invalid-token-in-state 'comma 'object-next)))
    ("n_object_single_quote"
     (#t ,invalid-initial-char))
    ("n_object_trailing_comma"
     (#t ,(invalid-token-in-state 'object-end 'object-next)))
    ("n_object_trailing_comment"
     (#t ,invalid-initial-char))
    ("n_object_trailing_comment_open"
     (#t ,invalid-initial-char))
    ("n_object_trailing_comment_slash_open"
     (#t ,invalid-initial-char))
    ("n_object_trailing_comment_slash_open_incomplete"
     (#t ,invalid-initial-char))
    ("n_object_two_commas_in_a_row"
     (#t ,(invalid-token-in-state 'comma 'object-next)))
    ("n_object_unquoted_key"
     (#t ,invalid-initial-char))
    ("n_object_unterminated-value"
     (#t ,expected-string-char))
    ("n_object_with_single_string"
     (#t ,(invalid-token-in-state 'object-end 'object-colon)))
    ("n_object_with_trailing_garbage"
     (#t ,invalid-initial-char))
    ("n_single_space"
     (json-generator ())
     (#t ,premature-eof))
    ("n_string_1_surrogate_then_escape"
     (#t ,expected-string-unicode-escape))
    ("n_string_1_surrogate_then_escape_u"
     (#t ,expected-hex-digit))
    ("n_string_1_surrogate_then_escape_u1"
     (#t ,expected-hex-digit))
    ("n_string_1_surrogate_then_escape_u1x"
     (#t ,expected-hex-digit))
    ("n_string_accentuated_char_no_quotes"
     (#t ,invalid-initial-char))
    ("n_string_backslash_00"
     (#t ,expected-string-escape))
    ("n_string_escape_x"
     (#t ,expected-string-escape))
    ("n_string_escaped_backslash_bad"
     (#t ,expected-string-char))
    ("n_string_escaped_ctrl_char_tab"
     (#t ,expected-string-escape))
    ("n_string_escaped_emoji"
     (#t ,expected-string-escape))
    ("n_string_incomplete_escape"
     (#t ,expected-string-char))
    ("n_string_incomplete_escaped_character"
     (#t ,expected-hex-digit))
    ("n_string_incomplete_surrogate"
     (#t ,expected-hex-digit))
    ("n_string_incomplete_surrogate_escape_invalid"
     (#t ,expected-low-surrogate))
    ("n_string_invalid-utf-8-in-escape"
     (#t ,expected-hex-digit))
    ("n_string_invalid_backslash_esc"
     (#t ,expected-string-escape))
    ("n_string_invalid_unicode_escape"
     (#t ,expected-hex-digit))
    ("n_string_invalid_utf8_after_escape"
     (#t ,expected-string-escape))
    ("n_string_leading_uescaped_thinspace"
     (#t ,invalid-initial-char))
    ("n_string_no_quotes_with_bad_escape"
     (#t ,invalid-initial-char))
    ("n_string_single_doublequote"
     (#t ,expected-string-char))
    ("n_string_single_quote"
     (#t ,invalid-initial-char))
    ("n_string_single_string_no_double_quotes"
     (#t ,invalid-initial-char))
    ("n_string_start_escape_unclosed"
     (#t ,expected-string-escape))
    ("n_string_unescaped_ctrl_char"
     (#t ,expected-string-char))
    ("n_string_unescaped_newline"
     (#t ,expected-string-char))
    ("n_string_unescaped_tab"
     (#t ,expected-string-char))
    ("n_string_unicode_CapitalU"
     (#t ,expected-string-escape))
    ("n_string_with_trailing_garbage"
     (#t ,invalid-initial-char))
    ("n_structure_100000_opening_arrays"
     (#t ,(invalid-token-in-state 'eof 'array-first)))
    ("n_structure_U+2060_word_joined"
     (#t ,invalid-initial-char))
    ("n_structure_UTF8_BOM_no_data"
     (#t ,invalid-initial-char))
    ("n_structure_angle_bracket_null"
     (#t ,invalid-initial-char))
    ("n_structure_array_trailing_garbage"
     (#t ,invalid-initial-char))
    ("n_structure_array_with_extra_array_close"
     (#t ,(invalid-token-in-state 'array-end 'final)))
    ("n_structure_array_with_unclosed_string"
     (#t ,expected-string-char))
    ("n_structure_ascii-unicode-identifier"
     (#t ,invalid-initial-char))
    ("n_structure_capitalized_True"
     (#t ,invalid-initial-char))
    ("n_structure_close_unopened_array"
     (#t ,(invalid-token-in-state 'array-end 'initial)))
    ("n_structure_comma_instead_of_closing_brace"
     (#t ,(invalid-token-in-state 'eof 'object-next)))
    ("n_structure_double_array"
     (#t ,(invalid-token-in-state 'array-start 'final)))
    ("n_structure_end_array"
     (#t ,(invalid-token-in-state 'array-end 'initial)))
    ("n_structure_incomplete_UTF8_BOM"
     (#t ,invalid-initial-char))
    ("n_structure_lone-invalid-utf-8"
     (#t ,char-encoding-error))
    ("n_structure_lone-open-bracket"
     (#t ,(invalid-token-in-state 'eof 'array-first)))
    ("n_structure_null-byte-outside-string"
     (#t ,invalid-initial-char))
    ("n_structure_number_with_trailing_garbage"
     (#t ,invalid-initial-char))
    ("n_structure_object_followed_by_closing_object"
     (#t ,(invalid-token-in-state 'object-end 'final)))
    ("n_structure_object_unclosed_no_value"
     (#t ,(invalid-token-in-state 'eof 'object-value)))
    ("n_structure_object_with_comment"
     (#t ,invalid-initial-char))
    ("n_structure_object_with_trailing_garbage"
     (#t ,(invalid-token-in-state 'string 'final)))
    ("n_structure_open_array_apostrophe"
     (#t ,invalid-initial-char))
    ("n_structure_open_array_comma"
     (#t ,(invalid-token-in-state 'comma 'array-first)))
    ("n_structure_open_array_object"
     (#t ,(invalid-token-in-state 'eof 'object-value)))
    ("n_structure_open_array_open_object"
     (#t ,(invalid-token-in-state 'eof 'object-first)))
    ("n_structure_open_array_open_string"
     (#t ,expected-string-char))
    ("n_structure_open_array_string"
     (#t ,(invalid-token-in-state 'eof 'array-comma)))
    ("n_structure_open_object"
     (#t ,(invalid-token-in-state 'eof 'object-first)))
    ("n_structure_open_object_close_array"
     (#t ,(invalid-token-in-state 'array-end 'object-first)))
    ("n_structure_open_object_comma"
     (#t ,(invalid-token-in-state 'comma 'object-first)))
    ("n_structure_open_object_open_array"
     (#t ,(invalid-token-in-state 'array-start 'object-first)))
    ("n_structure_open_object_open_string"
     (#t ,expected-string-char))
    ("n_structure_open_object_string_with_apostrophes"
     (#t ,invalid-initial-char))
    ("n_structure_open_open"
     (#t ,expected-string-escape))
    ("n_structure_no_data"
     (json-generator ())
     (#t ,premature-eof))
    ("n_structure_single_eacute"
     (#t ,char-encoding-error))
    ("n_structure_single_star"
     (#t ,invalid-initial-char))
    ("n_structure_trailing_#"
     (#t ,invalid-initial-char))
    ("n_structure_uescaped_LF_before_string"
     (#t ,invalid-initial-char))
    ("n_structure_unclosed_array"
     (#t ,(invalid-token-in-state 'eof 'array-comma)))
    ("n_structure_unclosed_array_partial_null"
     (#t ,(expect-condition json-error? "Expected l but got:"
                            (list (eof-object)))))
    ("n_structure_unclosed_array_unfinished_false"
     (#t ,(expect-condition json-error? "Expected e but got:"
                            (list (eof-object)))))
    ("n_structure_unclosed_array_unfinished_true"
     (#t ,(expect-condition json-error? "Expected e but got:"
                            (list (eof-object)))))
    ("n_structure_unclosed_object"
     (#t ,(invalid-token-in-state 'eof 'object-comma)))
    ("n_structure_unicode-identifier"
     (#t ,invalid-initial-char))
    ("n_structure_whitespace_U+2060_word_joiner"
     (#t ,invalid-initial-char))
    ("n_structure_whitespace_formfeed"
     (#t ,invalid-initial-char))
    ("i_object_key_lone_2nd_surrogate"
     (#t ,expected-high-surrogate))
    ("i_number_double_huge_neg_exp"
     (json-generator (array-start 0. array-end))
     (#t #(0.)))
    ("i_number_huge_exp"
     (#t ,invalid-number-syntax))
    ("i_number_neg_int_huge_exp"
     (#t ,invalid-number-syntax))
    ("i_number_pos_double_huge_exp"
     (#t ,invalid-number-syntax))
    ("i_number_real_neg_overflow"
     (#t ,invalid-number-syntax))
    ("i_number_real_pos_overflow"
     (#t ,invalid-number-syntax))
    ("i_number_real_underflow"
     (#t ,invalid-number-syntax))
    ("i_number_too_big_neg_int"
     (json-generator
      (array-start -123123123123123123123123123123 array-end))
     (#t #(-123123123123123123123123123123)))
    ("i_number_too_big_pos_int"
     (json-generator (array-start 100000000000000000000 array-end))
     (#t #(100000000000000000000)))
    ("i_number_very_big_negative_int"
     (json-generator
      (array-start -237462374673276894279832749832423479823246327846
                   array-end))
     (#t #(-237462374673276894279832749832423479823246327846)))
    ("i_string_1st_surrogate_but_2nd_missing"
     (#t ,expected-string-unicode-escape))
    ("i_string_1st_valid_surrogate_2nd_invalid"
     (#t ,expected-low-surrogate))
    ("i_string_UTF-16LE_with_BOM"
     (#t ,invalid-initial-char))
    ("i_string_UTF-8_invalid_sequence"
     (json-generator (array-start "\x65e5;\x448;\xfffd;" array-end))
     (#t #("\x65e5;\x448;\xfffd;")))
    ("i_string_UTF8_surrogate_U+D800"
     (json-generator (array-start "\xfffd;" array-end))
     (#t #("\xfffd;")))
    ("i_string_incomplete_surrogate_and_escape_valid"
     (#t ,expected-string-unicode-escape))
    ("i_string_incomplete_surrogate_pair"
     (#t ,expected-high-surrogate))
    ("i_string_incomplete_surrogates_escape_valid"
     (#t ,expected-low-surrogate))
    ("i_string_invalid_lonely_surrogate"
     (#t ,expected-string-unicode-escape))
    ("i_string_invalid_surrogate"
     (#t ,expected-string-unicode-escape))
    ("i_string_invalid_utf-8"
     (json-generator (array-start "\xfffd;" array-end))
     (#t #("\xfffd;")))
    ("i_string_inverted_surrogates_U+1D11E"
     (#t ,expected-high-surrogate))
    ("i_string_iso_latin_1"
     (#t ,expected-string-char))
    ("i_string_lone_second_surrogate"
     (#t ,expected-high-surrogate))
    ("i_string_lone_utf8_continuation_byte"
     (json-generator (array-start "\xfffd;" array-end))
     (#t #("\xfffd;")))
    ("i_string_not_in_unicode_range"
     (json-generator (array-start "\xfffd;" array-end))
     (#t #("\xfffd;")))
    ("i_string_overlong_sequence_2_bytes"
     (json-generator (array-start "\xfffd;\xfffd;" array-end))
     (#t #("\xfffd;\xfffd;")))
    ("i_string_overlong_sequence_6_bytes"
     (json-generator
      (array-start "\xfffd;\xfffd;\xfffd;\xfffd;\xfffd;\xfffd;" array-end))
     (#t #("\xfffd;\xfffd;\xfffd;\xfffd;\xfffd;\xfffd;")))
    ("i_string_overlong_sequence_6_bytes_null"
     (json-generator
      (array-start "\xfffd;\xfffd;\xfffd;\xfffd;\xfffd;\xfffd;" array-end))
     (#t #("\xfffd;\xfffd;\xfffd;\xfffd;\xfffd;\xfffd;")))
    ("i_string_truncated-utf-8"
     (#t ,expected-string-char))
    ("i_string_utf16BE_no_BOM"
     (#t ,invalid-initial-char))
    ("i_string_utf16LE_no_BOM"
     (#t ,invalid-initial-char))
    ("i_structure_UTF-8_BOM_empty_object"
     (#t ,invalid-initial-char))
    ("i_structure_500_nested_arrays"
     (json-generator
      ,(append (make-list 500 'array-start)
               (make-list 500 'array-end)))
     (#t
      ,(let loop ((n 1))
         (if (fx<? n 500)
             (vector (loop (fx+ n 1)))
             (vector)))))))

(define (proc-expectation expectation proc-name)
  (let ((p
	 (or (assq proc-name (cdr expectation))
	     (let ((l (last expectation)))
	       (and (eq? #t (car l))
		    l)))))
    (if (not p)
	(error "Unknown procedure name:" proc-name))
    (cadr p)))

(define (std-input-test-cases proc-name test-proc)
  (map (std-input-test-case proc-name test-proc)
       test-expectations))

(define ((std-input-test-case proc-name test-proc) entry)
  (let ((expected (proc-expectation entry proc-name))
	(pathname
	 (merge-pathnames (string-append (car entry) ".json")
			  test-data-directory)))

    (define (run-test)
      (call-with-input-file pathname test-proc))

    (if (condition-expectation? expected)
	(lambda ()
	  (assert-error run-test (condition-expectation-matcher expected)))
	(lambda ()
	  (assert-equal (run-test) expected)))))

(define test-data-directory
  (merge-pathnames "test-srfi-180-data/"
		   (directory-pathname (current-load-pathname))))

(define ((condition-expectation-matcher expected) actual)
  (and (condition? actual)
       (let ((predicate (condition-expectation-predicate expected))
             (message (condition-expectation-message expected))
             (irritants (condition-expectation-irritants expected))
             (object
              (if (r7rs-error-condition? actual)
                  (r7rs-error-condition-object actual)
                  actual)))
         (and (predicate object)
              (or (not (error-object? object))
                  (or (default-object? message)
                      (and (equal? (error-object-message object) message)
                           (or (default-object? irritants)
                               (equal? (error-object-irritants object)
                                       irritants)))))))))

(define-test 'json-generator
  (std-input-test-cases 'json-generator
   (lambda (port)
     (generator->list (json-generator port)))))

(define-test 'json-read
  (std-input-test-cases 'json-read json-read))

(define-test 'json-lines-read
  (std-input-test-cases 'json-lines-read json-lines-read))

(define-test 'json-sequence-read
  (std-input-test-cases 'json-sequence-read json-sequence-read))