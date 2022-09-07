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

;;;; XHTML entity definitions

(declare (usual-integrations))

(define html-entity-alist
  '((nbsp #\x00A0)		; no-break space
    (iexcl #\x00A1)		; inverted exclamation mark
    (cent #\x00A2)		; cent sign
    (pound #\x00A3)		; pound sign
    (curren #\x00A4)		; currency sign
    (yen #\x00A5)		; yen sign
    (brvbar #\x00A6)		; broken bar
    (sect #\x00A7)		; section sign
    (uml #\x00A8)		; diaeresis
    (copy #\x00A9)		; copyright sign
    (ordf #\x00AA)		; feminine ordinal indicator
    (laquo #\x00AB)		; left-pointing double angle quotation mark
    (not #\x00AC)		; not sign
    (shy #\x00AD)		; soft hyphen
    (reg #\x00AE)		; registered sign
    (macr #\x00AF)		; macron
    (deg #\x00B0)		; degree sign
    (plusmn #\x00B1)		; plus-minus sign
    (sup2 #\x00B2)		; superscript two
    (sup3 #\x00B3)		; superscript three
    (acute #\x00B4)		; acute accent
    (micro #\x00B5)		; micro sign
    (para #\x00B6)		; pilcrow sign
    (middot #\x00B7)		; middle dot
    (cedil #\x00B8)		; cedilla
    (sup1 #\x00B9)		; superscript one
    (ordm #\x00BA)		; masculine ordinal indicator
    (raquo #\x00BB)		; right-pointing double angle quotation mark
    (frac14 #\x00BC)		; vulgar fraction one quarter
    (frac12 #\x00BD)		; vulgar fraction one half
    (frac34 #\x00BE)		; vulgar fraction three quarters
    (iquest #\x00BF)		; inverted question mark
    (|Agrave| #\x00C0)		; latin capital letter A with grave
    (|Aacute| #\x00C1)		; latin capital letter A with acute
    (|Acirc| #\x00C2)		; latin capital letter A with circumflex
    (|Atilde| #\x00C3)		; latin capital letter A with tilde
    (|Auml| #\x00C4)		; latin capital letter A with diaeresis
    (|Aring| #\x00C5)		; latin capital letter A with ring above
    (|AElig| #\x00C6)		; latin capital letter AE
    (|Ccedil| #\x00C7)		; latin capital letter C with cedilla
    (|Egrave| #\x00C8)		; latin capital letter E with grave
    (|Eacute| #\x00C9)		; latin capital letter E with acute
    (|Ecirc| #\x00CA)		; latin capital letter E with circumflex
    (|Euml| #\x00CB)		; latin capital letter E with diaeresis
    (|Igrave| #\x00CC)		; latin capital letter I with grave
    (|Iacute| #\x00CD)		; latin capital letter I with acute
    (|Icirc| #\x00CE)		; latin capital letter I with circumflex
    (|Iuml| #\x00CF)		; latin capital letter I with diaeresis
    (|ETH| #\x00D0)		; latin capital letter ETH
    (|Ntilde| #\x00D1)		; latin capital letter N with tilde
    (|Ograve| #\x00D2)		; latin capital letter O with grave
    (|Oacute| #\x00D3)		; latin capital letter O with acute
    (|Ocirc| #\x00D4)		; latin capital letter O with circumflex
    (|Otilde| #\x00D5)		; latin capital letter O with tilde
    (|Ouml| #\x00D6)		; latin capital letter O with diaeresis
    (times #\x00D7)		; multiplication sign
    (|Oslash| #\x00D8)		; latin capital letter O with stroke
    (|Ugrave| #\x00D9)		; latin capital letter U with grave
    (|Uacute| #\x00DA)		; latin capital letter U with acute
    (|Ucirc| #\x00DB)		; latin capital letter U with circumflex
    (|Uuml| #\x00DC)		; latin capital letter U with diaeresis
    (|Yacute| #\x00DD)		; latin capital letter Y with acute
    (|THORN| #\x00DE)		; latin capital letter THORN
    (szlig #\x00DF)		; latin small letter sharp s
    (agrave #\x00E0)		; latin small letter a with grave
    (aacute #\x00E1)		; latin small letter a with acute
    (acirc #\x00E2)		; latin small letter a with circumflex
    (atilde #\x00E3)		; latin small letter a with tilde
    (auml #\x00E4)		; latin small letter a with diaeresis
    (aring #\x00E5)		; latin small letter a with ring above
    (aelig #\x00E6)		; latin small letter ae
    (ccedil #\x00E7)		; latin small letter c with cedilla
    (egrave #\x00E8)		; latin small letter e with grave
    (eacute #\x00E9)		; latin small letter e with acute
    (ecirc #\x00EA)		; latin small letter e with circumflex
    (euml #\x00EB)		; latin small letter e with diaeresis
    (igrave #\x00EC)		; latin small letter i with grave
    (iacute #\x00ED)		; latin small letter i with acute
    (icirc #\x00EE)		; latin small letter i with circumflex
    (iuml #\x00EF)		; latin small letter i with diaeresis
    (eth #\x00F0)		; latin small letter eth
    (ntilde #\x00F1)		; latin small letter n with tilde
    (ograve #\x00F2)		; latin small letter o with grave
    (oacute #\x00F3)		; latin small letter o with acute
    (ocirc #\x00F4)		; latin small letter o with circumflex
    (otilde #\x00F5)		; latin small letter o with tilde
    (ouml #\x00F6)		; latin small letter o with diaeresis
    (divide #\x00F7)		; division sign
    (oslash #\x00F8)		; latin small letter o with stroke,
    (ugrave #\x00F9)		; latin small letter u with grave
    (uacute #\x00FA)		; latin small letter u with acute
    (ucirc #\x00FB)		; latin small letter u with circumflex
    (uuml #\x00FC)		; latin small letter u with diaeresis
    (yacute #\x00FD)		; latin small letter y with acute
    (thorn #\x00FE)		; latin small letter thorn
    (yuml #\x00FF)		; latin small letter y with diaeresis
    (|OElig| #\x0152)		; latin capital ligature OE
    (oelig #\x0153)		; latin small ligature oe
    (|Scaron| #\x0160)		; latin capital letter S with caron
    (scaron #\x0161)		; latin small letter s with caron
    (|Yuml| #\x0178)		; latin capital letter Y with diaeresis
    (circ #\x02C6)		; modifier letter circumflex accent
    (tilde #\x02DC)		; small tilde
    (ensp #\x2002)		; en space
    (emsp #\x2003)		; em space
    (thinsp #\x2009)		; thin space
    (zwnj #\x200C)		; zero width non-joiner
    (zwj #\x200D)		; zero width joiner
    (lrm #\x200E)		; left-to-right mark
    (rlm #\x200F)		; right-to-left mark
    (ndash #\x2013)		; en dash
    (mdash #\x2014)		; em dash
    (lsquo #\x2018)		; left single quotation mark
    (rsquo #\x2019)		; right single quotation mark
    (sbquo #\x201A)		; single low-9 quotation mark
    (ldquo #\x201C)		; left double quotation mark
    (rdquo #\x201D)		; right double quotation mark
    (bdquo #\x201E)		; double low-9 quotation mark
    (dagger #\x2020)		; dagger
    (|Dagger| #\x2021)		; double dagger
    (permil #\x2030)		; per mille sign
    (lsaquo #\x2039)		; single left-pointing angle quotation mark
    (rsaquo #\x203A)		; single right-pointing angle quotation mark
    (euro #\x20AC)		; euro sign
    (fnof #\x0192)		; latin small letter f with hook
    (|Alpha| #\x0391)		; greek capital letter alpha
    (|Beta| #\x0392)		; greek capital letter beta
    (|Gamma| #\x0393)		; greek capital letter gamma
    (|Delta| #\x0394)		; greek capital letter delta
    (|Epsilon| #\x0395)		; greek capital letter epsilon
    (|Zeta| #\x0396)		; greek capital letter zeta
    (|Eta| #\x0397)		; greek capital letter eta
    (|Theta| #\x0398)		; greek capital letter theta
    (|Iota| #\x0399)		; greek capital letter iota
    (|Kappa| #\x039A)		; greek capital letter kappa
    (|Lambda| #\x039B)		; greek capital letter lamda
    (|Mu| #\x039C)		; greek capital letter mu
    (|Nu| #\x039D)		; greek capital letter nu
    (|Xi| #\x039E)		; greek capital letter xi
    (|Omicron| #\x039F)	; greek capital letter omicron
    (|Pi| #\x03A0)		; greek capital letter pi
    (|Rho| #\x03A1)		; greek capital letter rho
    (|Sigma| #\x03A3)		; greek capital letter sigma
    (|Tau| #\x03A4)		; greek capital letter tau
    (|Upsilon| #\x03A5)	; greek capital letter upsilon
    (|Phi| #\x03A6)		; greek capital letter phi
    (|Chi| #\x03A7)		; greek capital letter chi
    (|Psi| #\x03A8)		; greek capital letter psi
    (|Omega| #\x03A9)		; greek capital letter omega
    (alpha #\x03B1)		; greek small letter alpha
    (beta #\x03B2)		; greek small letter beta
    (gamma #\x03B3)		; greek small letter gamma
    (delta #\x03B4)		; greek small letter delta
    (epsilon #\x03B5)		; greek small letter epsilon
    (zeta #\x03B6)		; greek small letter zeta
    (eta #\x03B7)		; greek small letter eta
    (theta #\x03B8)		; greek small letter theta
    (iota #\x03B9)		; greek small letter iota
    (kappa #\x03BA)		; greek small letter kappa
    (lambda #\x03BB)		; greek small letter lamda
    (mu #\x03BC)		; greek small letter mu
    (nu #\x03BD)		; greek small letter nu
    (xi #\x03BE)		; greek small letter xi
    (omicron #\x03BF)		; greek small letter omicron
    (pi #\x03C0)		; greek small letter pi
    (rho #\x03C1)		; greek small letter rho
    (sigmaf #\x03C2)		; greek small letter final sigma
    (sigma #\x03C3)		; greek small letter sigma
    (tau #\x03C4)		; greek small letter tau
    (upsilon #\x03C5)		; greek small letter upsilon
    (phi #\x03C6)		; greek small letter phi
    (chi #\x03C7)		; greek small letter chi
    (psi #\x03C8)		; greek small letter psi
    (omega #\x03C9)		; greek small letter omega
    (thetasym #\x03D1)		; greek theta symbol
    (upsih #\x03D2)		; greek upsilon with hook symbol
    (piv #\x03D6)		; greek pi symbol
    (bull #\x2022)		; bullet
    (hellip #\x2026)		; horizontal ellipsis
    (prime #\x2032)		; prime
    (|Prime| #\x2033)		; double prime
    (oline #\x203E)		; overline
    (frasl #\x2044)		; fraction slash
    (weierp #\x2118)		; script capital P
    (image #\x2111)		; black-letter capital I
    (real #\x211C)		; black-letter capital R
    (trade #\x2122)		; trade mark sign
    (alefsym #\x2135)		; alef symbol
    (larr #\x2190)		; leftwards arrow
    (uarr #\x2191)		; upwards arrow
    (rarr #\x2192)		; rightwards arrow
    (darr #\x2193)		; downwards arrow
    (harr #\x2194)		; left right arrow
    (crarr #\x21B5)		; downwards arrow with corner leftwards
    (|lArr| #\x21D0)		; leftwards double arrow
    (|uArr| #\x21D1)		; upwards double arrow
    (|rArr| #\x21D2)		; rightwards double arrow
    (|dArr| #\x21D3)		; downwards double arrow
    (|hArr| #\x21D4)		; left right double arrow
    (forall #\x2200)		; for all
    (part #\x2202)		; partial differential
    (exist #\x2203)		; there exists
    (empty #\x2205)		; empty set
    (nabla #\x2207)		; nabla
    (isin #\x2208)		; element of
    (notin #\x2209)		; not an element of
    (ni #\x220B)		; contains as member
    (prod #\x220F)		; n-ary product
    (sum #\x2211)		; n-ary summation
    (minus #\x2212)		; minus sign
    (lowast #\x2217)		; asterisk operator
    (radic #\x221A)		; square root
    (prop #\x221D)		; proportional to
    (infin #\x221E)		; infinity
    (ang #\x2220)		; angle
    (and #\x2227)		; logical and
    (or #\x2228)		; logical or
    (cap #\x2229)		; intersection
    (cup #\x222A)		; union
    (int #\x222B)		; integral
    (there4 #\x2234)		; therefore
    (sim #\x223C)		; tilde operator
    (cong #\x2245)		; approximately equal to
    (asymp #\x2248)		; almost equal to
    (ne #\x2260)		; not equal to
    (equiv #\x2261)		; identical to
    (le #\x2264)		; less-than or equal to
    (ge #\x2265)		; greater-than or equal to
    (sub #\x2282)		; subset of
    (sup #\x2283)		; superset of
    (nsub #\x2284)		; not a subset of
    (sube #\x2286)		; subset of or equal to
    (supe #\x2287)		; superset of or equal to
    (oplus #\x2295)		; circled plus
    (otimes #\x2297)		; circled times
    (perp #\x22A5)		; up tack
    (sdot #\x22C5)		; dot operator
    (lceil #\x2308)		; left ceiling
    (rceil #\x2309)		; right ceiling
    (lfloor #\x230A)		; left floor
    (rfloor #\x230B)		; right floor
    (lang #\x2329)		; left-pointing angle bracket
    (rang #\x232A)		; right-pointing angle bracket
    (loz #\x25CA)		; lozenge
    (spades #\x2660)		; black spade suit
    (clubs #\x2663)		; black club suit
    (hearts #\x2665)		; black heart suit
    (diams #\x2666)		; black diamond suit
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
    (for-each (lambda (b) (hash-table-set! table (cadr b) (car b)))
	      html-entity-alist)
    (lambda (char)
      (hash-table-ref/default table char #f))))