#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; Unicode character database XML properties
;;;  (Not including Unihan properties.)

;;; Each entry is a list of three items:
;;; 1. The XML property name: a string, usually abbreviated.
;;; 2. The Scheme property name: a symbol, not abbreviated.
;;; 3. A type specifier: see the code for details.

("AHex" ascii-hex-digit boolean)
("Alpha" alphabetic boolean)
("Bidi_C" bidi-control boolean)
("Bidi_M" bidi-mirrored boolean)
("CE" composition-exclusion boolean)
("CI" case-ignorable boolean)
("CWCF" changes-when-case-folded boolean)
("CWCM" changes-when-case-mapped boolean)
("CWKCF" changes-when-nfkc-case-folded boolean)
("CWL" changes-when-lower-cased boolean)
("CWT" changes-when-title-cased boolean)
("CWU" changes-when-upper-cased boolean)
("Cased" cased boolean)
("Comp_Ex" full-composition-exclusion boolean)
("DI" default-ignorable-code-point boolean)
("Dash" dash boolean)
("Dep" deprecated boolean)
("Dia" diacritic boolean)
("Ext" extender boolean)
("FC_NFKC" fc-nfkc-closure code-point+)
("GCB" grapheme-cluster-break
 (enum "CN" "CR" "EB" "EBG" "EM" "EX" "GAZ" "L" "LF"
       "LV" "LVT" "PP" "RI" "SM" "T" "V" "XX" "ZWJ"))
("Gr_Base" grapheme-base boolean)
("Gr_Ext" grapheme-extend boolean)
("Gr_Link" grapheme-link boolean)
("Hex" hex-digit boolean)
("Hyphen" hyphen boolean)
("IDC" id-continue boolean)
("IDS" id-start boolean)
("IDSB" ids-binary-operator boolean)
("IDST" ids-trinary-operator boolean)
("Ideo" ideographic boolean)
("InMC" indic-matra-category
 (enum "Right"
       "Left"
       "Visual_Order_Left"
       "Left_And_Right"
       "Top"
       "Bottom"
       "Top_And_Bottom"
       "Top_And_Right"
       "Top_And_Left"
       "Top_And_Left_And_Right"
       "Bottom_And_Right"
       "Top_And_Bottom_And_Right"
       "Overstruck"
       "Invisible"
       "NA"))
("InPC" indic-positional-category
 (enum "Bottom"
       "Bottom_And_Right"
       "Left"
       "Left_And_Right"
       "NA"
       "Overstruck"
       "Right"
       "Top"
       "Top_And_Bottom"
       "Top_And_Bottom_And_Right"
       "Top_And_Left"
       "Top_And_Left_And_Right"
       "Top_And_Right"
       "Visual_Order_Left"))
("InSC" indic-syllabic-category
 (enum "Avagraha"
       "Bindu"
       "Brahmi_Joining_Number"
       "Cantillation_Mark"
       "Consonant"
       "Consonant_Dead"
       "Consonant_Final"
       "Consonant_Head_Letter"
       "Consonant_Killer"
       "Consonant_Medial"
       "Consonant_Placeholder"
       "Consonant_Preceding_Repha"
       "Consonant_Prefixed"
       "Consonant_Repha"
       "Consonant_Subjoined"
       "Consonant_Succeeding_Repha"
       "Consonant_With_Stacker"
       "Gemination_Mark"
       "Invisible_Stacker"
       "Joiner"
       "Modifying_Letter"
       "Non_Joiner"
       "Nukta"
       "Number"
       "Number_Joiner"
       "Other"
       "Pure_Killer"
       "Register_Shifter"
       "Syllable_Modifier"
       "Tone_Letter"
       "Tone_Mark"
       "Virama"
       "Visarga"
       "Vowel"
       "Vowel_Dependent"
       "Vowel_Independent"))
("JSN" jamo-short-name (regex "[A-Z]{0,3}"))
("Join_C" join-control boolean)
("LOE" logical-order-exception boolean)
("Lower" lower-case boolean)
("Math" math boolean)
("NChar" noncharactor-code-point boolean)
("NFC_QC" nfc-quick-check (enum ("Y" . yes) ("N" . no) ("M" . maybe)))
("NFD_QC" nfd-quick-check boolean)
("NFKC_CF" nfkc-case-fold code-point*)
("NFKC_QC" nfkc-quick-check (enum ("Y" . yes) ("N" . no) ("M" . maybe)))
("NFKD_QC" nfkd-quick-check boolean)
("OAlpha" other-alphabetic boolean)
("ODI" other-default-ignorable-code-point boolean)
("OGr_Ext" other-grapheme-extend boolean)
("OIDC" other-id-continue boolean)
("OIDS" other-id-start boolean)
("OLower" other-lower-case boolean)
("OMath" other-math boolean)
("OUpper" other-upper-case boolean)
("PCM" prepended-concatenation-mark boolean)
("Pat_Syn" pattern-syntax boolean)
("Pat_WS" pattern-whitespace boolean)
("QMark" quotation-mark boolean)
("Radical" radical boolean)
("SB" sentence-break
 (enum "AT" "CL" "CR" "EX" "FO" "LE" "LF" "LO" "NU" "SC" "SE"
       "SP" "ST" "UP" "XX"))
("SD" soft-dotted boolean)
("STerm" sentence-terminal boolean)
("Term" terminal-punctuation boolean)
("UIdeo" unified-ideograph boolean)
("Upper" upper-case boolean)
("VS" variation-selector boolean)
("WB" word-break
 (enum "CR" "DQ" "EB" "EBG" "EM" "EX" "Extend" "FO" "GAZ" "HL" "KA" "LE" "LF"
       "MB" "ML" "MN" "NL" "NU" "RI" "SQ" "XX" "ZWJ"))
("WSpace" whitespace boolean)
("XIDC" xid-continue boolean)
("XIDS" xid-start boolean)
("XO_NFC" expands-on-nfc boolean)
("XO_NFD" expands-on-nfd boolean)
("XO_NFKC" expands-on-nfkc boolean)
("XO_NFKD" expands-on-nfkd boolean)
("age" age
 (enum "1.1" "2.0" "2.1" "3.0" "3.1" "3.2" "4.0" "4.1" "5.0" "5.1" "5.2"
       "6.0" "6.1" "6.2" "6.3" "7.0" "8.0" "9.0" "unassigned"))
("bc" bidirectional-class
 (enum "AL" "AN" "B " "BN" "CS" "EN" "ES"  "ET" "FSI" "L"  "LRE" "LRI" "LRO"
       "NSM" "ON" "PDF" "PDI" "R"  "RLE" "RLI" "RLO" "S" "WS"))
("blk" block
 (enum  "Adlam"
	"Aegean_Numbers"
	"Ahom"
	"Alchemical"
	"Alphabetic_PF"
	"Anatolian_Hieroglyphs"
	"Ancient_Greek_Music"
	"Ancient_Greek_Numbers"
	"Ancient_Symbols"
	"Arabic"
	"Arabic_Ext_A"
	"Arabic_Math"
	"Arabic_PF_A"
	"Arabic_PF_B"
	"Arabic_Sup"
	"Armenian"
	"Arrows"
	"ASCII"
	"Avestan"
	"Balinese"
	"Bamum"
	"Bamum_Sup"
	"Bassa_Vah"
	"Batak"
	"Bengali"
	"Bhaiksuki"
	"Block_Elements"
	"Bopomofo"
	"Bopomofo_Ext"
	"Box_Drawing"
	"Brahmi"
	"Braille"
	"Buginese"
	"Buhid"
	"Byzantine_Music"
	"Carian"
	"Caucasian_Albanian"
	"Chakma"
	"Cham"
	"Cherokee"
	"Cherokee_Sup"
	"CJK"
	"CJK_Compat"
	"CJK_Compat_Forms"
	"CJK_Compat_Ideographs"
	"CJK_Compat_Ideographs_Sup"
	"CJK_Ext_A"
	"CJK_Ext_B"
	"CJK_Ext_C"
	"CJK_Ext_D"
	"CJK_Ext_E"
	"CJK_Radicals_Sup"
	"CJK_Strokes"
	"CJK_Symbols"
	"Compat_Jamo"
	"Control_Pictures"
	"Coptic"
	"Coptic_Epact_Numbers"
	"Counting_Rod"
	"Cuneiform"
	"Cuneiform_Numbers"
	"Currency_Symbols"
	"Cypriot_Syllabary"
	"Cyrillic"
	"Cyrillic_Ext_A"
	"Cyrillic_Ext_B"
	"Cyrillic_Ext_C"
	"Cyrillic_Sup"
	"Deseret"
	"Devanagari"
	"Devanagari_Ext"
	"Diacriticals"
	"Diacriticals_For_Symbols"
	"Diacriticals_Sup"
	"Diacriticals_Ext"
	"Dingbats"
	"Domino"
	"Duployan"
	"Early_Dynastic_Cuneiform"
	"Egyptian_Hieroglyphs"
	"Elbasan"
	"Emoticons"
	"Enclosed_Alphanum"
	"Enclosed_Alphanum_Sup"
	"Enclosed_CJK"
	"Enclosed_Ideographic_Sup"
	"Ethiopic"
	"Ethiopic_Ext"
	"Ethiopic_Ext_A"
	"Ethiopic_Sup"
	"Geometric_Shapes"
	"Geometric_Shapes_Ext"
	"Georgian"
	"Georgian_Sup"
	"Glagolitic"
	"Glagolitic_Sup"
	"Gothic"
	"Grantha"
	"Greek"
	"Greek_Ext"
	"Gujarati"
	"Gurmukhi"
	"Half_And_Full_Forms"
	"Half_Marks"
	"Hangul"
	"Hanunoo"
	"Hatran"
	"Hebrew"
	"High_PU_Surrogates"
	"High_Surrogates"
	"Hiragana"
	"IDC"
	"Ideographic_Symbols"
	"Imperial_Aramaic"
	"Indic_Number_Forms"
	"Inscriptional_Pahlavi"
	"Inscriptional_Parthian"
	"IPA_Ext"
	"Jamo"
	"Jamo_Ext_A"
	"Jamo_Ext_B"
	"Javanese"
	"Kaithi"
	"Kana_Sup"
	"Kanbun"
	"Kangxi"
	"Kannada"
	"Katakana"
	"Katakana_Ext"
	"Kayah_Li"
	"Kharoshthi"
	"Khmer"
	"Khmer_Symbols"
	"Khojki"
	"Khudawadi"
	"Lao"
	"Latin_1_Sup"
	"Latin_Ext_A"
	"Latin_Ext_Additional"
	"Latin_Ext_B"
	"Latin_Ext_C"
	"Latin_Ext_D"
	"Latin_Ext_E"
	"Lepcha"
	"Letterlike_Symbols"
	"Limbu"
	"Linear_A"
	"Linear_B_Ideograms"
	"Linear_B_Syllabary"
	"Lisu"
	"Low_Surrogates"
	"Lycian"
	"Lydian"
	"Mahajani"
	"Mahjong"
	"Malayalam"
	"Mandaic"
	"Manichaean"
	"Marchen"
	"Math_Alphanum"
	"Math_Operators"
	"Meetei_Mayek"
	"Meetei_Mayek_Ext"
	"Mende_Kikakui"
	"Meroitic_Cursive"
	"Meroitic_Hieroglyphs"
	"Miao"
	"Misc_Arrows"
	"Misc_Math_Symbols_A"
	"Misc_Math_Symbols_B"
	"Misc_Pictographs"
	"Misc_Symbols"
	"Misc_Technical"
	"Modi"
	"Modifier_Letters"
	"Modifier_Tone_Letters"
	"Mongolian"
	"Mongolian_Sup"
	"Mro"
	"Music"
	"Multani"
	"Myanmar"
	"Myanmar_Ext_A"
	"Myanmar_Ext_B"
	"Nabataean"
	"NB"
	"New_Tai_Lue"
	"Newa"
	"NKo"
	"Number_Forms"
	"OCR"
	"Ogham"
	"Ol_Chiki"
	"Old_Hungarian"
	"Old_Italic"
	"Old_North_Arabian"
	"Old_Permic"
	"Old_Persian"
	"Old_South_Arabian"
	"Old_Turkic"
	"Oriya"
	"Ornamental_Dingbats"
	"Osage"
	"Osmanya"
	"Pahawh_Hmong"
	"Palmyrene"
	"Pau_Cin_Hau"
	"Phags_Pa"
	"Phaistos"
	"Phoenician"
	"Phonetic_Ext"
	"Phonetic_Ext_Sup"
	"Playing_Cards"
	"Psalter_Pahlavi"
	"PUA"
	"Punctuation"
	"Rejang"
	"Rumi"
	"Runic"
	"Samaritan"
	"Saurashtra"
	"Sharada"
	"Shavian"
	"Shorthand_Format_Controls"
	"Siddham"
	"Sinhala"
	"Sinhala_Archaic_Numbers"
	"Small_Forms"
	"Sora_Sompeng"
	"Specials"
	"Sundanese"
	"Sundanese_Sup"
	"Sup_Arrows_A"
	"Sup_Arrows_B"
	"Sup_Arrows_C"
	"Sup_Math_Operators"
	"Sup_PUA_A"
	"Sup_PUA_B"
	"Sup_Punctuation"
	"Sup_Symbols_And_Pictographs"
	"Super_And_Sub"
	"Sutton_SignWriting"
	"Syloti_Nagri"
	"Syriac"
	"Tagalog"
	"Tagbanwa"
	"Tags"
	"Tai_Le"
	"Tai_Tham"
	"Tai_Viet"
	"Tai_Xuan_Jing"
	"Takri"
	"Tamil"
	"Tangut"
	"Tangut_Components"
	"Telugu"
	"Thaana"
	"Thai"
	"Tibetan"
	"Tifinagh"
	"Tirhuta"
	"Transport_And_Map"
	"UCAS"
	"UCAS_Ext"
	"Ugaritic"
	"Vai"
	"Vedic_Ext"
	"Vertical_Forms"
	"VS"
	"VS_Sup"
	"Warang_Citi"
	"Yi_Radicals"
	"Yi_Syllables"
	"Yijing"))
("bmg" mirror-image code-point?)
("bpb" bidi-paired-bracket code-point)
("bpt" bidi-paired-bracket-type (enum "o" "c" "n"))
("canonical-cm" canonical-composition-mapping u16 derived)
("canonical-dm" canonical-decomposition-mapping code-point* derived)
("ccc" combining-class ccc)
("cf" case-folding code-point+)
("dm" decomposition-mapping code-point*)
("dt" decomposition-type
 (enum ("can" . canonical)
       ("com" . unspecified)
       ("enc" . encircled)
       ("fin" . final-presentation)
       ("font" . font-variant)
       ("fra" . vulgar-fraction)
       ("init" . initial-presentation)
       ("iso" . isolated-presentation)
       ("med" . medial-presentation)
       ("nar" . narrow-compatibility)
       ("nb" . no-break-version)
       ("sml" . small-variant)
       ("sqr" . cjk-squared-font-variant)
       ("sub" . subscript)
       ("sup" . superscript)
       ("vert" . vertical-layout-presentation)
       ("wide" . wide-compatibility)
       ("none" . #f)))
("ea" east-asian-width (enum "A" "F" "H" "N" "Na" "W"))
("gc" general-category
 (enum ("Lu" . letter:uppercase)
       ("Ll" . letter:lowercase)
       ("Lt" . letter:titlecase)
       ("Lm" . letter:modifier)
       ("Lo" . letter:other)
       ("Mn" . mark:nonspacing)
       ("Mc" . mark:spacing-combining)
       ("Me" . mark:enclosing)
       ("Nd" . number:decimal-digit)
       ("Nl" . number:letter)
       ("No" . number:other)
       ("Pc" . punctuation:connector)
       ("Pd" . punctuation:dash)
       ("Ps" . punctuation:open)
       ("Pe" . punctuation:close)
       ("Pi" . punctuation:initial-quote)
       ("Pf" . punctuation:final-quote)
       ("Po" . punctuation:other)
       ("Sm" . symbol:math)
       ("Sc" . symbol:currency)
       ("Sk" . symbol:modifier)
       ("So" . symbol:other)
       ("Zs" . separator:space)
       ("Zl" . separator:line)
       ("Zp" . separator:paragraph)
       ("Cc" . other:control)
       ("Cf" . other:format)
       ("Cs" . other:surrogate)
       ("Co" . other:private-use)
       ("Cn" . other:not-assigned)))
("hst" hangul-syllable-type
 (enum ("L" . leading-jamo)
       ("LV" . lv-syllable)
       ("LVT" . lvt-syllable)
       ("T" . trailing-jamo)
       ("V" . vowel-jamo)
       ("NA" . #f)))
("isc" iso-10646-comment string)
("jg" joining-group
 (enum "African_Feh" "African_Noon" "African_Qaf"
       "Ain" "Alaph" "Alef" "Alef_Maqsurah"
       "Beh" "Beth" "Burushaski_Yeh_Barree"
       "Dal" "Dalath_Rish" "E"
       "Farsi_Yeh" "Fe"  "Feh" "Final_Semkath"
       "Gaf" "Gamal"
       "Hah" "Hamza_On_Heh_Goal" "He"
       "Heh" "Heh_Goal" "Heth"
       "Kaf" "Kaph" "Khaph" "Knotted_Heh"
       "Lam" "Lamadh"
       "Manichaean_Aleph"
       "Manichaean_Ayin"
       "Manichaean_Beth"
       "Manichaean_Daleth"
       "Manichaean_Dhamedh"
       "Manichaean_Five"
       "Manichaean_Gimel"
       "Manichaean_Heth"
       "Manichaean_Hundred"
       "Manichaean_Kaph"
       "Manichaean_Lamedh"
       "Manichaean_Mem"
       "Manichaean_Nun"
       "Manichaean_One"
       "Manichaean_Pe"
       "Manichaean_Qoph"
       "Manichaean_Resh"
       "Manichaean_Sadhe"
       "Manichaean_Samekh"
       "Manichaean_Taw"
       "Manichaean_Ten"
       "Manichaean_Teth"
       "Manichaean_Thamedh"
       "Manichaean_Twenty"
       "Manichaean_Waw"
       "Manichaean_Yodh"
       "Manichaean_Zayin"
       "Meem" "Mim"
       "No_Joining_Group" "Noon" "Nun" "Nya"
       "Pe" "Qaf" "Qaph" "Reh" "Reversed_Pe"
       "Rohingya_Yeh"
       "Sad" "Sadhe" "Seen" "Semkath" "Shin"
       "Straight_Waw"
       "Swash_Kaf" "Syriac_Waw" "Tah" "Taw"
       "Teh_Marbuta" "Teh_Marbuta_Goal" "Teth" "Waw" "Yeh"
       "Yeh_Barree" "Yeh_With_Tail" "Yudh"
       "Yudh_He" "Zain" "Zhain"))
("jt" joining-class (enum "U" "C" "T" "D" "L" "R"))
("lb" line-break
 (enum "AI" "AL" "B2" "BA" "BB" "BK" "CB" "CJ" "CL" "CM" "CP" "CR" "EB"
       "EM" "EX" "GL" "H2" "H3" "HL" "HY" "ID" "IN" "IS" "JL" "JT" "JV"
       "LF" "NL" "NS" "NU" "OP" "PO" "PR" "QU" "RI" "SA" "SG" "SP" "SY"
       "WJ" "XX" "ZW" "ZWJ"))
("lc" lower-case code-point+)
("na" name string)
("na1" name string)
("nt" numeric-type
 (enum ("None" . #f)
       ("De" . decimal)
       ("Di" . digit)
       ("Nu" . numeric)))
("nv" numeric-value rational-or-nan)
("sc" script
 (enum "Adlm" "Aghb" "Ahom" "Arab" "Armi" "Armn" "Avst"
       "Bali" "Bamu" "Bass" "Batk" "Beng" "Bhks"
       "Bopo" "Brah" "Brai" "Bugi" "Buhd"
       "Cakm" "Cans" "Cari" "Cham" "Cher" "Copt" "Cprt"
       "Cyrl"
       "Deva" "Dsrt" "Dupl"
       "Elba" "Egyp" "Ethi"
       "Geor" "Glag" "Goth" "Gran" "Grek" "Gujr" "Guru"
       "Hang" "Hani" "Hano" "Hatr" "Hebr" "Hira" "Hluw"
       "Hmng" "Hrkt" "Hung"
       "Ital"
       "Java"
       "Kali" "Kana" "Khar" "Khmr" "Khoj" "Knda" "Kthi"
       "Lana" "Laoo" "Latn" "Lepc" "Limb" "Lina" "Linb"
       "Lisu" "Lyci" "Lydi"
       "Mahj" "Mand" "Mani" "Marc"
       "Mend" "Merc" "Mero" "Mlym"
       "Modi" "Mong" "Mroo" "Mtei" "Mult" "Mymr"
       "Narb" "Nbat" "Newa" "Nkoo"
       "Ogam" "Olck" "Orkh" "Orya" "Osge" "Osma"
       "Palm" "Pauc" "Perm" "Phag" "Phli" "Phlp" "Phnx"
       "Plrd" "Prti"
       "Qaai"
       "Rjng" "Runr"
       "Samr" "Sarb" "Saur" "Sgnw" "Shaw" "Shrd" "Sidd"
       "Sind" "Sinh" "Sora" "Sund" "Sylo" "Syrc"
       "Tagb" "Takr" "Tale" "Talu" "Taml" "Tang" "Tavt"
       "Telu" "Tfng" "Tglg" "Thaa" "Thai" "Tibt" "Tirh"
       "Ugar"
       "Vaii"
       "Wara"
       "Xpeo" "Xsux"
       "Yiii"
       "Zinh" "Zyyy" "Zzzz"))
("scf" simple-case-folding code-point)
("scx" script-extension list-of-script)
("slc" simple-lower-case code-point)
("stc" simple-title-case code-point)
("suc" simple-upper-case code-point)
("tc" title-case code-point+)
("uc" upper-case code-point+)
