#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; XHTML entity definitions

(declare (usual-integrations))

(define html-entity-alist
  '((nbsp #\U+00A0)		; no-break space
    (iexcl #\U+00A1)		; inverted exclamation mark
    (cent #\U+00A2)		; cent sign
    (pound #\U+00A3)		; pound sign
    (curren #\U+00A4)		; currency sign
    (yen #\U+00A5)		; yen sign
    (brvbar #\U+00A6)		; broken bar
    (sect #\U+00A7)		; section sign
    (uml #\U+00A8)		; diaeresis
    (copy #\U+00A9)		; copyright sign
    (ordf #\U+00AA)		; feminine ordinal indicator
    (laquo #\U+00AB)		; left-pointing double angle quotation mark
    (not #\U+00AC)		; not sign
    (shy #\U+00AD)		; soft hyphen
    (reg #\U+00AE)		; registered sign
    (macr #\U+00AF)		; macron
    (deg #\U+00B0)		; degree sign
    (plusmn #\U+00B1)		; plus-minus sign
    (sup2 #\U+00B2)		; superscript two
    (sup3 #\U+00B3)		; superscript three
    (acute #\U+00B4)		; acute accent
    (micro #\U+00B5)		; micro sign
    (para #\U+00B6)		; pilcrow sign
    (middot #\U+00B7)		; middle dot
    (cedil #\U+00B8)		; cedilla
    (sup1 #\U+00B9)		; superscript one
    (ordm #\U+00BA)		; masculine ordinal indicator
    (raquo #\U+00BB)		; right-pointing double angle quotation mark
    (frac14 #\U+00BC)		; vulgar fraction one quarter
    (frac12 #\U+00BD)		; vulgar fraction one half
    (frac34 #\U+00BE)		; vulgar fraction three quarters
    (iquest #\U+00BF)		; inverted question mark
    (|Agrave| #\U+00C0)		; latin capital letter A with grave
    (|Aacute| #\U+00C1)		; latin capital letter A with acute
    (|Acirc| #\U+00C2)		; latin capital letter A with circumflex
    (|Atilde| #\U+00C3)		; latin capital letter A with tilde
    (|Auml| #\U+00C4)		; latin capital letter A with diaeresis
    (|Aring| #\U+00C5)		; latin capital letter A with ring above
    (|AElig| #\U+00C6)		; latin capital letter AE
    (|Ccedil| #\U+00C7)		; latin capital letter C with cedilla
    (|Egrave| #\U+00C8)		; latin capital letter E with grave
    (|Eacute| #\U+00C9)		; latin capital letter E with acute
    (|Ecirc| #\U+00CA)		; latin capital letter E with circumflex
    (|Euml| #\U+00CB)		; latin capital letter E with diaeresis
    (|Igrave| #\U+00CC)		; latin capital letter I with grave
    (|Iacute| #\U+00CD)		; latin capital letter I with acute
    (|Icirc| #\U+00CE)		; latin capital letter I with circumflex
    (|Iuml| #\U+00CF)		; latin capital letter I with diaeresis
    (|ETH| #\U+00D0)		; latin capital letter ETH
    (|Ntilde| #\U+00D1)		; latin capital letter N with tilde
    (|Ograve| #\U+00D2)		; latin capital letter O with grave
    (|Oacute| #\U+00D3)		; latin capital letter O with acute
    (|Ocirc| #\U+00D4)		; latin capital letter O with circumflex
    (|Otilde| #\U+00D5)		; latin capital letter O with tilde
    (|Ouml| #\U+00D6)		; latin capital letter O with diaeresis
    (times #\U+00D7)		; multiplication sign
    (|Oslash| #\U+00D8)		; latin capital letter O with stroke
    (|Ugrave| #\U+00D9)		; latin capital letter U with grave
    (|Uacute| #\U+00DA)		; latin capital letter U with acute
    (|Ucirc| #\U+00DB)		; latin capital letter U with circumflex
    (|Uuml| #\U+00DC)		; latin capital letter U with diaeresis
    (|Yacute| #\U+00DD)		; latin capital letter Y with acute
    (|THORN| #\U+00DE)		; latin capital letter THORN
    (szlig #\U+00DF)		; latin small letter sharp s
    (agrave #\U+00E0)		; latin small letter a with grave
    (aacute #\U+00E1)		; latin small letter a with acute
    (acirc #\U+00E2)		; latin small letter a with circumflex
    (atilde #\U+00E3)		; latin small letter a with tilde
    (auml #\U+00E4)		; latin small letter a with diaeresis
    (aring #\U+00E5)		; latin small letter a with ring above
    (aelig #\U+00E6)		; latin small letter ae
    (ccedil #\U+00E7)		; latin small letter c with cedilla
    (egrave #\U+00E8)		; latin small letter e with grave
    (eacute #\U+00E9)		; latin small letter e with acute
    (ecirc #\U+00EA)		; latin small letter e with circumflex
    (euml #\U+00EB)		; latin small letter e with diaeresis
    (igrave #\U+00EC)		; latin small letter i with grave
    (iacute #\U+00ED)		; latin small letter i with acute
    (icirc #\U+00EE)		; latin small letter i with circumflex
    (iuml #\U+00EF)		; latin small letter i with diaeresis
    (eth #\U+00F0)		; latin small letter eth
    (ntilde #\U+00F1)		; latin small letter n with tilde
    (ograve #\U+00F2)		; latin small letter o with grave
    (oacute #\U+00F3)		; latin small letter o with acute
    (ocirc #\U+00F4)		; latin small letter o with circumflex
    (otilde #\U+00F5)		; latin small letter o with tilde
    (ouml #\U+00F6)		; latin small letter o with diaeresis
    (divide #\U+00F7)		; division sign
    (oslash #\U+00F8)		; latin small letter o with stroke,
    (ugrave #\U+00F9)		; latin small letter u with grave
    (uacute #\U+00FA)		; latin small letter u with acute
    (ucirc #\U+00FB)		; latin small letter u with circumflex
    (uuml #\U+00FC)		; latin small letter u with diaeresis
    (yacute #\U+00FD)		; latin small letter y with acute
    (thorn #\U+00FE)		; latin small letter thorn
    (yuml #\U+00FF)		; latin small letter y with diaeresis
    (|OElig| #\U+0152)		; latin capital ligature OE
    (oelig #\U+0153)		; latin small ligature oe
    (|Scaron| #\U+0160)		; latin capital letter S with caron
    (scaron #\U+0161)		; latin small letter s with caron
    (|Yuml| #\U+0178)		; latin capital letter Y with diaeresis
    (circ #\U+02C6)		; modifier letter circumflex accent
    (tilde #\U+02DC)		; small tilde
    (ensp #\U+2002)		; en space
    (emsp #\U+2003)		; em space
    (thinsp #\U+2009)		; thin space
    (zwnj #\U+200C)		; zero width non-joiner
    (zwj #\U+200D)		; zero width joiner
    (lrm #\U+200E)		; left-to-right mark
    (rlm #\U+200F)		; right-to-left mark
    (ndash #\U+2013)		; en dash
    (mdash #\U+2014)		; em dash
    (lsquo #\U+2018)		; left single quotation mark
    (rsquo #\U+2019)		; right single quotation mark
    (sbquo #\U+201A)		; single low-9 quotation mark
    (ldquo #\U+201C)		; left double quotation mark
    (rdquo #\U+201D)		; right double quotation mark
    (bdquo #\U+201E)		; double low-9 quotation mark
    (dagger #\U+2020)		; dagger
    (|Dagger| #\U+2021)		; double dagger
    (permil #\U+2030)		; per mille sign
    (lsaquo #\U+2039)		; single left-pointing angle quotation mark
    (rsaquo #\U+203A)		; single right-pointing angle quotation mark
    (euro #\U+20AC)		; euro sign
    (fnof #\U+0192)		; latin small letter f with hook
    (|Alpha| #\U+0391)		; greek capital letter alpha
    (|Beta| #\U+0392)		; greek capital letter beta
    (|Gamma| #\U+0393)		; greek capital letter gamma
    (|Delta| #\U+0394)		; greek capital letter delta
    (|Epsilon| #\U+0395)	; greek capital letter epsilon
    (|Zeta| #\U+0396)		; greek capital letter zeta
    (|Eta| #\U+0397)		; greek capital letter eta
    (|Theta| #\U+0398)		; greek capital letter theta
    (|Iota| #\U+0399)		; greek capital letter iota
    (|Kappa| #\U+039A)		; greek capital letter kappa
    (|Lambda| #\U+039B)		; greek capital letter lamda
    (|Mu| #\U+039C)		; greek capital letter mu
    (|Nu| #\U+039D)		; greek capital letter nu
    (|Xi| #\U+039E)		; greek capital letter xi
    (|Omicron| #\U+039F)	; greek capital letter omicron
    (|Pi| #\U+03A0)		; greek capital letter pi
    (|Rho| #\U+03A1)		; greek capital letter rho
    (|Sigma| #\U+03A3)		; greek capital letter sigma
    (|Tau| #\U+03A4)		; greek capital letter tau
    (|Upsilon| #\U+03A5)	; greek capital letter upsilon
    (|Phi| #\U+03A6)		; greek capital letter phi
    (|Chi| #\U+03A7)		; greek capital letter chi
    (|Psi| #\U+03A8)		; greek capital letter psi
    (|Omega| #\U+03A9)		; greek capital letter omega
    (alpha #\U+03B1)		; greek small letter alpha
    (beta #\U+03B2)		; greek small letter beta
    (gamma #\U+03B3)		; greek small letter gamma
    (delta #\U+03B4)		; greek small letter delta
    (epsilon #\U+03B5)		; greek small letter epsilon
    (zeta #\U+03B6)		; greek small letter zeta
    (eta #\U+03B7)		; greek small letter eta
    (theta #\U+03B8)		; greek small letter theta
    (iota #\U+03B9)		; greek small letter iota
    (kappa #\U+03BA)		; greek small letter kappa
    (lambda #\U+03BB)		; greek small letter lamda
    (mu #\U+03BC)		; greek small letter mu
    (nu #\U+03BD)		; greek small letter nu
    (xi #\U+03BE)		; greek small letter xi
    (omicron #\U+03BF)		; greek small letter omicron
    (pi #\U+03C0)		; greek small letter pi
    (rho #\U+03C1)		; greek small letter rho
    (sigmaf #\U+03C2)		; greek small letter final sigma
    (sigma #\U+03C3)		; greek small letter sigma
    (tau #\U+03C4)		; greek small letter tau
    (upsilon #\U+03C5)		; greek small letter upsilon
    (phi #\U+03C6)		; greek small letter phi
    (chi #\U+03C7)		; greek small letter chi
    (psi #\U+03C8)		; greek small letter psi
    (omega #\U+03C9)		; greek small letter omega
    (thetasym #\U+03D1)		; greek theta symbol
    (upsih #\U+03D2)		; greek upsilon with hook symbol
    (piv #\U+03D6)		; greek pi symbol
    (bull #\U+2022)		; bullet
    (hellip #\U+2026)		; horizontal ellipsis
    (prime #\U+2032)		; prime
    (|Prime| #\U+2033)		; double prime
    (oline #\U+203E)		; overline
    (frasl #\U+2044)		; fraction slash
    (weierp #\U+2118)		; script capital P
    (image #\U+2111)		; black-letter capital I
    (real #\U+211C)		; black-letter capital R
    (trade #\U+2122)		; trade mark sign
    (alefsym #\U+2135)		; alef symbol
    (larr #\U+2190)		; leftwards arrow
    (uarr #\U+2191)		; upwards arrow
    (rarr #\U+2192)		; rightwards arrow
    (darr #\U+2193)		; downwards arrow
    (harr #\U+2194)		; left right arrow
    (crarr #\U+21B5)		; downwards arrow with corner leftwards
    (|lArr| #\U+21D0)		; leftwards double arrow
    (|uArr| #\U+21D1)		; upwards double arrow
    (|rArr| #\U+21D2)		; rightwards double arrow
    (|dArr| #\U+21D3)		; downwards double arrow
    (|hArr| #\U+21D4)		; left right double arrow
    (forall #\U+2200)		; for all
    (part #\U+2202)		; partial differential
    (exist #\U+2203)		; there exists
    (empty #\U+2205)		; empty set
    (nabla #\U+2207)		; nabla
    (isin #\U+2208)		; element of
    (notin #\U+2209)		; not an element of
    (ni #\U+220B)		; contains as member
    (prod #\U+220F)		; n-ary product
    (sum #\U+2211)		; n-ary summation
    (minus #\U+2212)		; minus sign
    (lowast #\U+2217)		; asterisk operator
    (radic #\U+221A)		; square root
    (prop #\U+221D)		; proportional to
    (infin #\U+221E)		; infinity
    (ang #\U+2220)		; angle
    (and #\U+2227)		; logical and
    (or #\U+2228)		; logical or
    (cap #\U+2229)		; intersection
    (cup #\U+222A)		; union
    (int #\U+222B)		; integral
    (there4 #\U+2234)		; therefore
    (sim #\U+223C)		; tilde operator
    (cong #\U+2245)		; approximately equal to
    (asymp #\U+2248)		; almost equal to
    (ne #\U+2260)		; not equal to
    (equiv #\U+2261)		; identical to
    (le #\U+2264)		; less-than or equal to
    (ge #\U+2265)		; greater-than or equal to
    (sub #\U+2282)		; subset of
    (sup #\U+2283)		; superset of
    (nsub #\U+2284)		; not a subset of
    (sube #\U+2286)		; subset of or equal to
    (supe #\U+2287)		; superset of or equal to
    (oplus #\U+2295)		; circled plus
    (otimes #\U+2297)		; circled times
    (perp #\U+22A5)		; up tack
    (sdot #\U+22C5)		; dot operator
    (lceil #\U+2308)		; left ceiling
    (rceil #\U+2309)		; right ceiling
    (lfloor #\U+230A)		; left floor
    (rfloor #\U+230B)		; right floor
    (lang #\U+2329)		; left-pointing angle bracket
    (rang #\U+232A)		; right-pointing angle bracket
    (loz #\U+25CA)		; lozenge
    (spades #\U+2660)		; black spade suit
    (clubs #\U+2663)		; black club suit
    (hearts #\U+2665)		; black heart suit
    (diams #\U+2666)		; black diamond suit
    ))

(define html-entities
  (map (lambda (b)
	 (make-xml-!entity
	  (car b)
	  (list (string-append "&#x"
			       (number->string (char->integer (cadr b)) 16)
			       ";"))))
       html-entity-alist))

(define html-char->name-map
  (let ((table (make-strong-eqv-hash-table)))
    (for-each (lambda (b) (hash-table/put! table (cadr b) (car b)))
	      html-entity-alist)
    (lambda (char)
      (hash-table/get table char #f))))