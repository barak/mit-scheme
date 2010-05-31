#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; XML characters

(declare (usual-integrations))

(define char-set:xml-base-char
  (scalar-values->char-set
   '((#x0041 . #x005B)
     (#x0061 . #x007B)
     (#x00C0 . #x00D7)
     (#x00D8 . #x00F7)
     (#x00F8 . #x0100)
     (#x0100 . #x0132)
     (#x0134 . #x013F)
     (#x0141 . #x0149)
     (#x014A . #x017F)
     (#x0180 . #x01C4)
     (#x01CD . #x01F1)
     (#x01F4 . #x01F6)
     (#x01FA . #x0218)
     (#x0250 . #x02A9)
     (#x02BB . #x02C2)
     #x0386
     (#x0388 . #x038B)
     #x038C
     (#x038E . #x03A2)
     (#x03A3 . #x03CF)
     (#x03D0 . #x03D7)
     #x03DA
     #x03DC
     #x03DE
     #x03E0
     (#x03E2 . #x03F4)
     (#x0401 . #x040D)
     (#x040E . #x0450)
     (#x0451 . #x045D)
     (#x045E . #x0482)
     (#x0490 . #x04C5)
     (#x04C7 . #x04C9)
     (#x04CB . #x04CD)
     (#x04D0 . #x04EC)
     (#x04EE . #x04F6)
     (#x04F8 . #x04FA)
     (#x0531 . #x0557)
     #x0559
     (#x0561 . #x0587)
     (#x05D0 . #x05EB)
     (#x05F0 . #x05F3)
     (#x0621 . #x063B)
     (#x0641 . #x064B)
     (#x0671 . #x06B8)
     (#x06BA . #x06BF)
     (#x06C0 . #x06CF)
     (#x06D0 . #x06D4)
     #x06D5
     (#x06E5 . #x06E7)
     (#x0905 . #x093A)
     #x093D
     (#x0958 . #x0962)
     (#x0985 . #x098D)
     (#x098F . #x0991)
     (#x0993 . #x09A9)
     (#x09AA . #x09B1)
     #x09B2
     (#x09B6 . #x09BA)
     (#x09DC . #x09DE)
     (#x09DF . #x09E2)
     (#x09F0 . #x09F2)
     (#x0A05 . #x0A0B)
     (#x0A0F . #x0A11)
     (#x0A13 . #x0A29)
     (#x0A2A . #x0A31)
     (#x0A32 . #x0A34)
     (#x0A35 . #x0A37)
     (#x0A38 . #x0A3A)
     (#x0A59 . #x0A5D)
     #x0A5E
     (#x0A72 . #x0A75)
     (#x0A85 . #x0A8C)
     #x0A8D
     (#x0A8F . #x0A92)
     (#x0A93 . #x0AA9)
     (#x0AAA . #x0AB1)
     (#x0AB2 . #x0AB4)
     (#x0AB5 . #x0ABA)
     #x0ABD
     #x0AE0
     (#x0B05 . #x0B0D)
     (#x0B0F . #x0B11)
     (#x0B13 . #x0B29)
     (#x0B2A . #x0B31)
     (#x0B32 . #x0B34)
     (#x0B36 . #x0B3A)
     #x0B3D
     (#x0B5C . #x0B5E)
     (#x0B5F . #x0B62)
     (#x0B85 . #x0B8B)
     (#x0B8E . #x0B91)
     (#x0B92 . #x0B96)
     (#x0B99 . #x0B9B)
     #x0B9C
     (#x0B9E . #x0BA0)
     (#x0BA3 . #x0BA5)
     (#x0BA8 . #x0BAB)
     (#x0BAE . #x0BB6)
     (#x0BB7 . #x0BBA)
     (#x0C05 . #x0C0D)
     (#x0C0E . #x0C11)
     (#x0C12 . #x0C29)
     (#x0C2A . #x0C34)
     (#x0C35 . #x0C3A)
     (#x0C60 . #x0C62)
     (#x0C85 . #x0C8D)
     (#x0C8E . #x0C91)
     (#x0C92 . #x0CA9)
     (#x0CAA . #x0CB4)
     (#x0CB5 . #x0CBA)
     #x0CDE
     (#x0CE0 . #x0CE2)
     (#x0D05 . #x0D0D)
     (#x0D0E . #x0D11)
     (#x0D12 . #x0D29)
     (#x0D2A . #x0D3A)
     (#x0D60 . #x0D62)
     (#x0E01 . #x0E2F)
     #x0E30
     (#x0E32 . #x0E34)
     (#x0E40 . #x0E46)
     (#x0E81 . #x0E83)
     #x0E84
     (#x0E87 . #x0E89)
     #x0E8A
     #x0E8D
     (#x0E94 . #x0E98)
     (#x0E99 . #x0EA0)
     (#x0EA1 . #x0EA4)
     #x0EA5
     #x0EA7
     (#x0EAA . #x0EAC)
     (#x0EAD . #x0EAF)
     #x0EB0
     (#x0EB2 . #x0EB4)
     #x0EBD
     (#x0EC0 . #x0EC5)
     (#x0F40 . #x0F48)
     (#x0F49 . #x0F6A)
     (#x10A0 . #x10C6)
     (#x10D0 . #x10F7)
     #x1100
     (#x1102 . #x1104)
     (#x1105 . #x1108)
     #x1109
     (#x110B . #x110D)
     (#x110E . #x1113)
     #x113C
     #x113E
     #x1140
     #x114C
     #x114E
     #x1150
     (#x1154 . #x1156)
     #x1159
     (#x115F . #x1162)
     #x1163
     #x1165
     #x1167
     #x1169
     (#x116D . #x116F)
     (#x1172 . #x1174)
     #x1175
     #x119E
     #x11A8
     #x11AB
     (#x11AE . #x11B0)
     (#x11B7 . #x11B9)
     #x11BA
     (#x11BC . #x11C3)
     #x11EB
     #x11F0
     #x11F9
     (#x1E00 . #x1E9C)
     (#x1EA0 . #x1EFA)
     (#x1F00 . #x1F16)
     (#x1F18 . #x1F1E)
     (#x1F20 . #x1F46)
     (#x1F48 . #x1F4E)
     (#x1F50 . #x1F58)
     #x1F59
     #x1F5B
     #x1F5D
     (#x1F5F . #x1F7E)
     (#x1F80 . #x1FB5)
     (#x1FB6 . #x1FBD)
     #x1FBE
     (#x1FC2 . #x1FC5)
     (#x1FC6 . #x1FCD)
     (#x1FD0 . #x1FD4)
     (#x1FD6 . #x1FDC)
     (#x1FE0 . #x1FED)
     (#x1FF2 . #x1FF5)
     (#x1FF6 . #x1FFD)
     #x2126
     (#x212A . #x212C)
     #x212E
     (#x2180 . #x2183)
     (#x3041 . #x3095)
     (#x30A1 . #x30FB)
     (#x3105 . #x312D)
     (#xAC00 . #xD7A4))))

(define char-set:xml-ideographic
  (scalar-values->char-set
   '(#x3007
     (#x3021 . #x302A)
     (#x4E00 . #x9FA6))))

(define char-set:xml-combining-char
  (scalar-values->char-set
   '((#x0300 . #x0346)
     (#x0360 . #x0362)
     (#x0483 . #x0487)
     (#x0591 . #x05A2)
     (#x05A3 . #x05BA)
     (#x05BB . #x05BE)
     #x05BF
     (#x05C1 . #x05C3)
     #x05C4
     (#x064B . #x0653)
     #x0670
     (#x06D6 . #x06DD)
     (#x06DD . #x06E0)
     (#x06E0 . #x06E5)
     (#x06E7 . #x06E9)
     (#x06EA . #x06EE)
     (#x0901 . #x0904)
     #x093C
     (#x093E . #x094D)
     #x094D
     (#x0951 . #x0955)
     (#x0962 . #x0964)
     (#x0981 . #x0984)
     #x09BC
     #x09BE
     #x09BF
     (#x09C0 . #x09C5)
     (#x09C7 . #x09C9)
     (#x09CB . #x09CE)
     #x09D7
     (#x09E2 . #x09E4)
     #x0A02
     #x0A3C
     #x0A3E
     #x0A3F
     (#x0A40 . #x0A43)
     (#x0A47 . #x0A49)
     (#x0A4B . #x0A4E)
     (#x0A70 . #x0A72)
     (#x0A81 . #x0A84)
     #x0ABC
     (#x0ABE . #x0AC6)
     (#x0AC7 . #x0ACA)
     (#x0ACB . #x0ACE)
     (#x0B01 . #x0B04)
     #x0B3C
     (#x0B3E . #x0B44)
     (#x0B47 . #x0B49)
     (#x0B4B . #x0B4E)
     (#x0B56 . #x0B58)
     (#x0B82 . #x0B84)
     (#x0BBE . #x0BC3)
     (#x0BC6 . #x0BC9)
     (#x0BCA . #x0BCE)
     #x0BD7
     (#x0C01 . #x0C04)
     (#x0C3E . #x0C45)
     (#x0C46 . #x0C49)
     (#x0C4A . #x0C4E)
     (#x0C55 . #x0C57)
     (#x0C82 . #x0C84)
     (#x0CBE . #x0CC5)
     (#x0CC6 . #x0CC9)
     (#x0CCA . #x0CCE)
     (#x0CD5 . #x0CD7)
     (#x0D02 . #x0D04)
     (#x0D3E . #x0D44)
     (#x0D46 . #x0D49)
     (#x0D4A . #x0D4E)
     #x0D57
     #x0E31
     (#x0E34 . #x0E3B)
     (#x0E47 . #x0E4F)
     #x0EB1
     (#x0EB4 . #x0EBA)
     (#x0EBB . #x0EBD)
     (#x0EC8 . #x0ECE)
     (#x0F18 . #x0F1A)
     #x0F35
     #x0F37
     #x0F39
     #x0F3E
     #x0F3F
     (#x0F71 . #x0F85)
     (#x0F86 . #x0F8C)
     (#x0F90 . #x0F96)
     #x0F97
     (#x0F99 . #x0FAE)
     (#x0FB1 . #x0FB8)
     #x0FB9
     (#x20D0 . #x20DD)
     #x20E1
     (#x302A . #x3030)
     #x3099
     #x309A)))

(define char-set:xml-digit
  (scalar-values->char-set
   '((#x0030 . #x003A)
     (#x0660 . #x066A)
     (#x06F0 . #x06FA)
     (#x0966 . #x0970)
     (#x09E6 . #x09F0)
     (#x0A66 . #x0A70)
     (#x0AE6 . #x0AF0)
     (#x0B66 . #x0B70)
     (#x0BE7 . #x0BF0)
     (#x0C66 . #x0C70)
     (#x0CE6 . #x0CF0)
     (#x0D66 . #x0D70)
     (#x0E50 . #x0E5A)
     (#x0ED0 . #x0EDA)
     (#x0F20 . #x0F2A))))

(define char-set:xml-extender
  (scalar-values->char-set
   '(#x00B7
     #x02D0
     #x02D1
     #x0387
     #x0640
     #x0E46
     #x0EC6
     #x3005
     (#x3031 . #x3036)
     (#x309D . #x309F)
     (#x30FC . #x30FF))))

(define char-set:xml-char
  (scalar-values->char-set
   '(#x0009
     #x000A
     #x000D
     (#x0020 . #xD800)
     (#xE000 . #xFFFE)
     (#x10000 . #x110000))))

(define char-set:char-data
  (char-set-difference char-set:xml-char
		       (string->char-set "<&")))

(define char-set:name-initial
  (char-set-union char-set:xml-base-char
		  char-set:xml-ideographic
		  (string->char-set "_:")))

(define char-set:name-subsequent		;[4]
  (char-set-union char-set:xml-base-char
		  char-set:xml-ideographic
		  char-set:xml-digit
		  char-set:xml-combining-char
		  char-set:xml-extender
		  (string->char-set ".-_:")))

(define char-set:ncname-initial
  (char-set-difference char-set:name-initial
		       (string->char-set ":")))

(define char-set:ncname-subsequent
  (char-set-difference char-set:name-subsequent
		       (string->char-set ":")))

(define char-set:xml-whitespace
  (char-set #\space #\tab #\return #\linefeed))