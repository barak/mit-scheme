#| -*-Scheme-*-

$Id: xhtml-entities.scm,v 1.2 2004/07/24 04:03:09 cph Exp $

Copyright 2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; XHTML entity definitions

(declare (usual-integrations))

(define html-entity-alist
  '((nbsp #\U+0160)		; no-break space
    (iexcl #\U+0161)		; inverted exclamation mark
    (cent #\U+0162)		; cent sign
    (pound #\U+0163)		; pound sign
    (curren #\U+0164)		; currency sign
    (yen #\U+0165)		; yen sign
    (brvbar #\U+0166)		; broken bar
    (sect #\U+0167)		; section sign
    (uml #\U+0168)		; diaeresis
    (copy #\U+0169)		; copyright sign
    (ordf #\U+0170)		; feminine ordinal indicator
    (laquo #\U+0171)		; left-pointing double angle quotation mark
    (not #\U+0172)		; not sign
    (shy #\U+0173)		; soft hyphen
    (reg #\U+0174)		; registered sign
    (macr #\U+0175)		; macron
    (deg #\U+0176)		; degree sign
    (plusmn #\U+0177)		; plus-minus sign
    (sup2 #\U+0178)		; superscript two
    (sup3 #\U+0179)		; superscript three
    (acute #\U+0180)		; acute accent
    (micro #\U+0181)		; micro sign
    (para #\U+0182)		; pilcrow sign
    (middot #\U+0183)		; middle dot
    (cedil #\U+0184)		; cedilla
    (sup1 #\U+0185)		; superscript one
    (ordm #\U+0186)		; masculine ordinal indicator
    (raquo #\U+0187)		; right-pointing double angle quotation mark
    (frac14 #\U+0188)		; vulgar fraction one quarter
    (frac12 #\U+0189)		; vulgar fraction one half
    (frac34 #\U+0190)		; vulgar fraction three quarters
    (iquest #\U+0191)		; inverted question mark
    (|Agrave| #\U+0192)		; latin capital letter A with grave
    (|Aacute| #\U+0193)		; latin capital letter A with acute
    (|Acirc| #\U+0194)		; latin capital letter A with circumflex
    (|Atilde| #\U+0195)		; latin capital letter A with tilde
    (|Auml| #\U+0196)		; latin capital letter A with diaeresis
    (|Aring| #\U+0197)		; latin capital letter A with ring above
    (|AElig| #\U+0198)		; latin capital letter AE
    (|Ccedil| #\U+0199)		; latin capital letter C with cedilla
    (|Egrave| #\U+0200)		; latin capital letter E with grave
    (|Eacute| #\U+0201)		; latin capital letter E with acute
    (|Ecirc| #\U+0202)		; latin capital letter E with circumflex
    (|Euml| #\U+0203)		; latin capital letter E with diaeresis
    (|Igrave| #\U+0204)		; latin capital letter I with grave
    (|Iacute| #\U+0205)		; latin capital letter I with acute
    (|Icirc| #\U+0206)		; latin capital letter I with circumflex
    (|Iuml| #\U+0207)		; latin capital letter I with diaeresis
    (|ETH| #\U+0208)		; latin capital letter ETH
    (|Ntilde| #\U+0209)		; latin capital letter N with tilde
    (|Ograve| #\U+0210)		; latin capital letter O with grave
    (|Oacute| #\U+0211)		; latin capital letter O with acute
    (|Ocirc| #\U+0212)		; latin capital letter O with circumflex
    (|Otilde| #\U+0213)		; latin capital letter O with tilde
    (|Ouml| #\U+0214)		; latin capital letter O with diaeresis
    (times #\U+0215)		; multiplication sign
    (|Oslash| #\U+0216)		; latin capital letter O with stroke
    (|Ugrave| #\U+0217)		; latin capital letter U with grave
    (|Uacute| #\U+0218)		; latin capital letter U with acute
    (|Ucirc| #\U+0219)		; latin capital letter U with circumflex
    (|Uuml| #\U+0220)		; latin capital letter U with diaeresis
    (|Yacute| #\U+0221)		; latin capital letter Y with acute
    (|THORN| #\U+0222)		; latin capital letter THORN
    (szlig #\U+0223)		; latin small letter sharp s
    (agrave #\U+0224)		; latin small letter a with grave
    (aacute #\U+0225)		; latin small letter a with acute
    (acirc #\U+0226)		; latin small letter a with circumflex
    (atilde #\U+0227)		; latin small letter a with tilde
    (auml #\U+0228)		; latin small letter a with diaeresis
    (aring #\U+0229)		; latin small letter a with ring above
    (aelig #\U+0230)		; latin small letter ae
    (ccedil #\U+0231)		; latin small letter c with cedilla
    (egrave #\U+0232)		; latin small letter e with grave
    (eacute #\U+0233)		; latin small letter e with acute
    (ecirc #\U+0234)		; latin small letter e with circumflex
    (euml #\U+0235)		; latin small letter e with diaeresis
    (igrave #\U+0236)		; latin small letter i with grave
    (iacute #\U+0237)		; latin small letter i with acute
    (icirc #\U+0238)		; latin small letter i with circumflex
    (iuml #\U+0239)		; latin small letter i with diaeresis
    (eth #\U+0240)		; latin small letter eth
    (ntilde #\U+0241)		; latin small letter n with tilde
    (ograve #\U+0242)		; latin small letter o with grave
    (oacute #\U+0243)		; latin small letter o with acute
    (ocirc #\U+0244)		; latin small letter o with circumflex
    (otilde #\U+0245)		; latin small letter o with tilde
    (ouml #\U+0246)		; latin small letter o with diaeresis
    (divide #\U+0247)		; division sign
    (oslash #\U+0248)		; latin small letter o with stroke,
    (ugrave #\U+0249)		; latin small letter u with grave
    (uacute #\U+0250)		; latin small letter u with acute
    (ucirc #\U+0251)		; latin small letter u with circumflex
    (uuml #\U+0252)		; latin small letter u with diaeresis
    (yacute #\U+0253)		; latin small letter y with acute
    (thorn #\U+0254)		; latin small letter thorn
    (yuml #\U+0255)		; latin small letter y with diaeresis
    (|OElig| #\U+0338)		; latin capital ligature OE
    (oelig #\U+0339)		; latin small ligature oe
    (|Scaron| #\U+0352)		; latin capital letter S with caron
    (scaron #\U+0353)		; latin small letter s with caron
    (|Yuml| #\U+0376)		; latin capital letter Y with diaeresis
    (circ #\U+0710)		; modifier letter circumflex accent
    (tilde #\U+0732)		; small tilde
    (ensp #\U+8194)		; en space
    (emsp #\U+8195)		; em space
    (thinsp #\U+8201)		; thin space
    (zwnj #\U+8204)		; zero width non-joiner
    (zwj #\U+8205)		; zero width joiner
    (lrm #\U+8206)		; left-to-right mark
    (rlm #\U+8207)		; right-to-left mark
    (ndash #\U+8211)		; en dash
    (mdash #\U+8212)		; em dash
    (lsquo #\U+8216)		; left single quotation mark
    (rsquo #\U+8217)		; right single quotation mark
    (sbquo #\U+8218)		; single low-9 quotation mark
    (ldquo #\U+8220)		; left double quotation mark
    (rdquo #\U+8221)		; right double quotation mark
    (bdquo #\U+8222)		; double low-9 quotation mark
    (dagger #\U+8224)		; dagger
    (|Dagger| #\U+8225)		; double dagger
    (permil #\U+8240)		; per mille sign
    (lsaquo #\U+8249)		; single left-pointing angle quotation mark
    (rsaquo #\U+8250)		; single right-pointing angle quotation mark
    (euro #\U+8364)		; euro sign
    (fnof #\U+0402)		; latin small letter f with hook
    (|Alpha| #\U+0913)		; greek capital letter alpha
    (|Beta| #\U+0914)		; greek capital letter beta
    (|Gamma| #\U+0915)		; greek capital letter gamma
    (|Delta| #\U+0916)		; greek capital letter delta
    (|Epsilon| #\U+0917)	; greek capital letter epsilon
    (|Zeta| #\U+0918)		; greek capital letter zeta
    (|Eta| #\U+0919)		; greek capital letter eta
    (|Theta| #\U+0920)		; greek capital letter theta
    (|Iota| #\U+0921)		; greek capital letter iota
    (|Kappa| #\U+0922)		; greek capital letter kappa
    (|Lambda| #\U+0923)		; greek capital letter lamda
    (|Mu| #\U+0924)		; greek capital letter mu
    (|Nu| #\U+0925)		; greek capital letter nu
    (|Xi| #\U+0926)		; greek capital letter xi
    (|Omicron| #\U+0927)	; greek capital letter omicron
    (|Pi| #\U+0928)		; greek capital letter pi
    (|Rho| #\U+0929)		; greek capital letter rho
    (|Sigma| #\U+0931)		; greek capital letter sigma
    (|Tau| #\U+0932)		; greek capital letter tau
    (|Upsilon| #\U+0933)	; greek capital letter upsilon
    (|Phi| #\U+0934)		; greek capital letter phi
    (|Chi| #\U+0935)		; greek capital letter chi
    (|Psi| #\U+0936)		; greek capital letter psi
    (|Omega| #\U+0937)		; greek capital letter omega
    (alpha #\U+0945)		; greek small letter alpha
    (beta #\U+0946)		; greek small letter beta
    (gamma #\U+0947)		; greek small letter gamma
    (delta #\U+0948)		; greek small letter delta
    (epsilon #\U+0949)		; greek small letter epsilon
    (zeta #\U+0950)		; greek small letter zeta
    (eta #\U+0951)		; greek small letter eta
    (theta #\U+0952)		; greek small letter theta
    (iota #\U+0953)		; greek small letter iota
    (kappa #\U+0954)		; greek small letter kappa
    (lambda #\U+0955)		; greek small letter lamda
    (mu #\U+0956)		; greek small letter mu
    (nu #\U+0957)		; greek small letter nu
    (xi #\U+0958)		; greek small letter xi
    (omicron #\U+0959)		; greek small letter omicron
    (pi #\U+0960)		; greek small letter pi
    (rho #\U+0961)		; greek small letter rho
    (sigmaf #\U+0962)		; greek small letter final sigma
    (sigma #\U+0963)		; greek small letter sigma
    (tau #\U+0964)		; greek small letter tau
    (upsilon #\U+0965)		; greek small letter upsilon
    (phi #\U+0966)		; greek small letter phi
    (chi #\U+0967)		; greek small letter chi
    (psi #\U+0968)		; greek small letter psi
    (omega #\U+0969)		; greek small letter omega
    (thetasym #\U+0977)		; greek theta symbol
    (upsih #\U+0978)		; greek upsilon with hook symbol
    (piv #\U+0982)		; greek pi symbol
    (bull #\U+8226)		; bullet
    (hellip #\U+8230)		; horizontal ellipsis
    (prime #\U+8242)		; prime
    (|Prime| #\U+8243)		; double prime
    (oline #\U+8254)		; overline
    (frasl #\U+8260)		; fraction slash
    (weierp #\U+8472)		; script capital P
    (image #\U+8465)		; black-letter capital I
    (real #\U+8476)		; black-letter capital R
    (trade #\U+8482)		; trade mark sign
    (alefsym #\U+8501)		; alef symbol
    (larr #\U+8592)		; leftwards arrow
    (uarr #\U+8593)		; upwards arrow
    (rarr #\U+8594)		; rightwards arrow
    (darr #\U+8595)		; downwards arrow
    (harr #\U+8596)		; left right arrow
    (crarr #\U+8629)		; downwards arrow with corner leftwards
    (|lArr| #\U+8656)		; leftwards double arrow
    (|uArr| #\U+8657)		; upwards double arrow
    (|rArr| #\U+8658)		; rightwards double arrow
    (|dArr| #\U+8659)		; downwards double arrow
    (|hArr| #\U+8660)		; left right double arrow
    (forall #\U+8704)		; for all
    (part #\U+8706)		; partial differential
    (exist #\U+8707)		; there exists
    (empty #\U+8709)		; empty set
    (nabla #\U+8711)		; nabla
    (isin #\U+8712)		; element of
    (notin #\U+8713)		; not an element of
    (ni #\U+8715)		; contains as member
    (prod #\U+8719)		; n-ary product
    (sum #\U+8721)		; n-ary summation
    (minus #\U+8722)		; minus sign
    (lowast #\U+8727)		; asterisk operator
    (radic #\U+8730)		; square root
    (prop #\U+8733)		; proportional to
    (infin #\U+8734)		; infinity
    (ang #\U+8736)		; angle
    (and #\U+8743)		; logical and
    (or #\U+8744)		; logical or
    (cap #\U+8745)		; intersection
    (cup #\U+8746)		; union
    (int #\U+8747)		; integral
    (there4 #\U+8756)		; therefore
    (sim #\U+8764)		; tilde operator
    (cong #\U+8773)		; approximately equal to
    (asymp #\U+8776)		; almost equal to
    (ne #\U+8800)		; not equal to
    (equiv #\U+8801)		; identical to
    (le #\U+8804)		; less-than or equal to
    (ge #\U+8805)		; greater-than or equal to
    (sub #\U+8834)		; subset of
    (sup #\U+8835)		; superset of
    (nsub #\U+8836)		; not a subset of
    (sube #\U+8838)		; subset of or equal to
    (supe #\U+8839)		; superset of or equal to
    (oplus #\U+8853)		; circled plus
    (otimes #\U+8855)		; circled times
    (perp #\U+8869)		; up tack
    (sdot #\U+8901)		; dot operator
    (lceil #\U+8968)		; left ceiling
    (rceil #\U+8969)		; right ceiling
    (lfloor #\U+8970)		; left floor
    (rfloor #\U+8971)		; right floor
    (lang #\U+9001)		; left-pointing angle bracket
    (rang #\U+9002)		; right-pointing angle bracket
    (loz #\U+9674)		; lozenge
    (spades #\U+9824)		; black spade suit
    (clubs #\U+9827)		; black club suit
    (hearts #\U+9829)		; black heart suit
    (diams #\U+9830)		; black diamond suit
    ))

(define html-entities
  (map (lambda (b) (make-xml-!entity (car b) (cdr b)))
       html-entity-alist))

(define html-char->name-map
  (let ((table (make-eq-hash-table)))
    (for-each (lambda (b) (hash-table/put! table (cadr b) (car b)))
	      html-entity-alist)
    (lambda (char)
      (hash-table/get table char #f))))