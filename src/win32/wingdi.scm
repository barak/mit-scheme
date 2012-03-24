#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

(declare (usual-integrations))

;;Binary raster ops
(define-integrable R2_BLACK            1 ) ;0
(define-integrable R2_NOTMERGEPEN      2 ) ;DPon
(define-integrable R2_MASKNOTPEN       3 ) ;DPna
(define-integrable R2_NOTCOPYPEN       4 ) ;PN
(define-integrable R2_MASKPENNOT       5 ) ;PDna
(define-integrable R2_NOT              6 ) ;Dn
(define-integrable R2_XORPEN           7 ) ;DPx
(define-integrable R2_NOTMASKPEN       8 ) ;DPan
(define-integrable R2_MASKPEN          9 ) ;DPa
(define-integrable R2_NOTXORPEN        10) ;DPxn
(define-integrable R2_NOP              11) ;D
(define-integrable R2_MERGENOTPEN      12) ;DPno
(define-integrable R2_COPYPEN          13) ;P
(define-integrable R2_MERGEPENNOT      14) ;PDno
(define-integrable R2_MERGEPEN         15) ;DPo
(define-integrable R2_WHITE            16) ;1
(define-integrable R2_LAST             16)

;; Ternary raster operations
(define-integrable SRCCOPY             #x00CC0020)  ;dest = source
(define-integrable SRCPAINT            #x00EE0086)  ;dest = source OR dest
(define-integrable SRCAND              #x008800C6)  ;dest = source AND dest
(define-integrable SRCINVERT           #x00660046)  ;dest = source XOR dest
(define-integrable SRCERASE            #x00440328)  ;dest = source AND (NOT dest )
(define-integrable NOTSRCCOPY          #x00330008)  ;dest = (NOT source)
(define-integrable NOTSRCERASE         #x001100A6)  ;dest = (NOT src) AND (NOT dest)
(define-integrable MERGECOPY           #x00C000CA)  ;dest = (source AND pattern)
(define-integrable MERGEPAINT          #x00BB0226)  ;dest = (NOT source) OR dest
(define-integrable PATCOPY             #x00F00021)  ;dest = pattern
(define-integrable PATPAINT            #x00FB0A09)  ;dest = DPSnoo
(define-integrable PATINVERT           #x005A0049)  ;dest = pattern XOR dest
(define-integrable DSTINVERT           #x00550009)  ;dest = (NOT dest)
(define-integrable BLACKNESS           #x00000042)  ;dest = BLACK
(define-integrable WHITENESS           #x00FF0062)  ;dest = WHITE

(define-integrable GDI_ERROR #xFFFFFFFF)
(define-integrable HGDI_ERROR #xFFFFFFFF)

;;Region Flags
(define-integrable ERROR               0)
(define-integrable NULLREGION          1)
(define-integrable SIMPLEREGION        2)
(define-integrable COMPLEXREGION       3)
(define-integrable RGN_ERROR ERROR)

;;CombineRgn() Styles
(define-integrable RGN_AND             1)
(define-integrable RGN_OR              2)
(define-integrable RGN_XOR             3)
(define-integrable RGN_DIFF            4)
(define-integrable RGN_COPY            5)
(define-integrable RGN_MIN             RGN_AND)
(define-integrable RGN_MAX             RGN_COPY)

;;StretchBlt() Modes
(define-integrable BLACKONWHITE                 1)
(define-integrable WHITEONBLACK                 2)
(define-integrable COLORONCOLOR                 3)
(define-integrable HALFTONE                     4)
(define-integrable MAXSTRETCHBLTMODE            4)

;;PolyFill() Modes
(define-integrable ALTERNATE                    1)
(define-integrable WINDING                      2)
(define-integrable POLYFILL_LAST                2)

;;Text Alignment Options
(define-integrable TA_NOUPDATECP                0)
(define-integrable TA_UPDATECP                  1)

(define-integrable TA_LEFT                      0)
(define-integrable TA_RIGHT                     2)
(define-integrable TA_CENTER                    6)

(define-integrable TA_TOP                       0)
(define-integrable TA_BOTTOM                    8)
(define-integrable TA_BASELINE                  24)
(define-integrable TA_MASK       (+ TA_BASELINE TA_CENTER TA_UPDATECP))

(define-integrable VTA_BASELINE TA_BASELINE)
(define-integrable VTA_LEFT     TA_BOTTOM)
(define-integrable VTA_RIGHT    TA_TOP)
(define-integrable VTA_CENTER   TA_CENTER)
(define-integrable VTA_BOTTOM   TA_RIGHT)
(define-integrable VTA_TOP      TA_LEFT)


(define-integrable ETO_GRAYED                   1)
(define-integrable ETO_OPAQUE                   2)
(define-integrable ETO_CLIPPED                  4)

(define-integrable ASPECT_FILTERING             #x0001)

;;Bounds Accumulation APIs

(define-integrable DCB_RESET       #x0001)
(define-integrable DCB_ACCUMULATE  #x0002)
(define-integrable DCB_DIRTY       DCB_ACCUMULATE)
(define-integrable DCB_SET         (+ DCB_RESET DCB_ACCUMULATE))
(define-integrable DCB_ENABLE      #x0004)
(define-integrable DCB_DISABLE     #x0008)

;;Metafile Functions
(define-integrable META_SETBKCOLOR              #x0201)
(define-integrable META_SETBKMODE               #x0102)
(define-integrable META_SETMAPMODE              #x0103)
(define-integrable META_SETROP2                 #x0104)
(define-integrable META_SETRELABS               #x0105)
(define-integrable META_SETPOLYFILLMODE         #x0106)
(define-integrable META_SETSTRETCHBLTMODE       #x0107)
(define-integrable META_SETTEXTCHAREXTRA        #x0108)
(define-integrable META_SETTEXTCOLOR            #x0209)
(define-integrable META_SETTEXTJUSTIFICATION    #x020A)
(define-integrable META_SETWINDOWORG            #x020B)
(define-integrable META_SETWINDOWEXT            #x020C)
(define-integrable META_SETVIEWPORTORG          #x020D)
(define-integrable META_SETVIEWPORTEXT          #x020E)
(define-integrable META_OFFSETWINDOWORG         #x020F)
(define-integrable META_SCALEWINDOWEXT          #x0410)
(define-integrable META_OFFSETVIEWPORTORG       #x0211)
(define-integrable META_SCALEVIEWPORTEXT        #x0412)
(define-integrable META_LINETO                  #x0213)
(define-integrable META_MOVETO                  #x0214)
(define-integrable META_EXCLUDECLIPRECT         #x0415)
(define-integrable META_INTERSECTCLIPRECT       #x0416)
(define-integrable META_ARC                     #x0817)
(define-integrable META_ELLIPSE                 #x0418)
(define-integrable META_FLOODFILL               #x0419)
(define-integrable META_PIE                     #x081A)
(define-integrable META_RECTANGLE               #x041B)
(define-integrable META_ROUNDRECT               #x061C)
(define-integrable META_PATBLT                  #x061D)
(define-integrable META_SAVEDC                  #x001E)
(define-integrable META_SETPIXEL                #x041F)
(define-integrable META_OFFSETCLIPRGN           #x0220)
(define-integrable META_TEXTOUT                 #x0521)
(define-integrable META_BITBLT                  #x0922)
(define-integrable META_STRETCHBLT              #x0B23)
(define-integrable META_POLYGON                 #x0324)
(define-integrable META_POLYLINE                #x0325)
(define-integrable META_ESCAPE                  #x0626)
(define-integrable META_RESTOREDC               #x0127)
(define-integrable META_FILLREGION              #x0228)
(define-integrable META_FRAMEREGION             #x0429)
(define-integrable META_INVERTREGION            #x012A)
(define-integrable META_PAINTREGION             #x012B)
(define-integrable META_SELECTCLIPREGION        #x012C)
(define-integrable META_SELECTOBJECT            #x012D)
(define-integrable META_SETTEXTALIGN            #x012E)
(define-integrable META_CHORD                   #x0830)
(define-integrable META_SETMAPPERFLAGS          #x0231)
(define-integrable META_EXTTEXTOUT              #x0a32)
(define-integrable META_SETDIBTODEV             #x0d33)
(define-integrable META_SELECTPALETTE           #x0234)
(define-integrable META_REALIZEPALETTE          #x0035)
(define-integrable META_ANIMATEPALETTE          #x0436)
(define-integrable META_SETPALENTRIES           #x0037)
(define-integrable META_POLYPOLYGON             #x0538)
(define-integrable META_RESIZEPALETTE           #x0139)
(define-integrable META_DIBBITBLT               #x0940)
(define-integrable META_DIBSTRETCHBLT           #x0b41)
(define-integrable META_DIBCREATEPATTERNBRUSH   #x0142)
(define-integrable META_STRETCHDIB              #x0f43)
(define-integrable META_EXTFLOODFILL            #x0548)
(define-integrable META_DELETEOBJECT            #x01f0)
(define-integrable META_CREATEPALETTE           #x00f7)
(define-integrable META_CREATEPATTERNBRUSH      #x01F9)
(define-integrable META_CREATEPENINDIRECT       #x02FA)
(define-integrable META_CREATEFONTINDIRECT      #x02FB)
(define-integrable META_CREATEBRUSHINDIRECT     #x02FC)
(define-integrable META_CREATEREGION            #x06FF)

;;GDI Escapes
(define-integrable NEWFRAME                     1)
(define-integrable ABORTDOC                     2)
(define-integrable NEXTBAND                     3)
(define-integrable SETCOLORTABLE                4)
(define-integrable GETCOLORTABLE                5)
(define-integrable FLUSHOUTPUT                  6)
(define-integrable DRAFTMODE                    7)
(define-integrable QUERYESCSUPPORT              8)
(define-integrable SETABORTPROC                 9)
(define-integrable STARTDOC                     10)
(define-integrable ENDDOC                       11)
(define-integrable GETPHYSPAGESIZE              12)
(define-integrable GETPRINTINGOFFSET            13)
(define-integrable GETSCALINGFACTOR             14)
(define-integrable MFCOMMENT                    15)
(define-integrable GETPENWIDTH                  16)
(define-integrable SETCOPYCOUNT                 17)
(define-integrable SELECTPAPERSOURCE            18)
(define-integrable DEVICEDATA                   19)
(define-integrable PASSTHROUGH                  19)
(define-integrable GETTECHNOLGY                 20)
(define-integrable GETTECHNOLOGY                20)
(define-integrable SETLINECAP                   21)
(define-integrable SETLINEJOIN                  22)
(define-integrable SETMITERLIMIT                23)
(define-integrable BANDINFO                     24)
(define-integrable DRAWPATTERNRECT              25)
(define-integrable GETVECTORPENSIZE             26)
(define-integrable GETVECTORBRUSHSIZE           27)
(define-integrable ENABLEDUPLEX                 28)
(define-integrable GETSETPAPERBINS              29)
(define-integrable GETSETPRINTORIENT            30)
(define-integrable ENUMPAPERBINS                31)
(define-integrable SETDIBSCALING                32)
(define-integrable EPSPRINTING                  33)
(define-integrable ENUMPAPERMETRICS             34)
(define-integrable GETSETPAPERMETRICS           35)
(define-integrable POSTSCRIPT_DATA              37)
(define-integrable POSTSCRIPT_IGNORE            38)
(define-integrable MOUSETRAILS                  39)

(define-integrable GETEXTENDEDTEXTMETRICS       256)
(define-integrable GETEXTENTTABLE               257)
(define-integrable GETPAIRKERNTABLE             258)
(define-integrable GETTRACKKERNTABLE            259)
(define-integrable EXTTEXTOUT                   512)
(define-integrable ENABLERELATIVEWIDTHS         768)
(define-integrable ENABLEPAIRKERNING            769)
(define-integrable SETKERNTRACK                 770)
(define-integrable SETALLJUSTVALUES             771)
(define-integrable SETCHARSET                   772)

(define-integrable STRETCHBLT                   2048)
(define-integrable GETSETSCREENPARAMS           3072)
(define-integrable BEGIN_PATH                   4096)
(define-integrable CLIP_TO_PATH                 4097)
(define-integrable END_PATH                     4098)
(define-integrable EXT_DEVICE_CAPS              4099)
(define-integrable RESTORE_CTM                  4100)
(define-integrable SAVE_CTM                     4101)
(define-integrable SET_ARC_DIRECTION            4102)
(define-integrable SET_BACKGROUND_COLOR         4103)
(define-integrable SET_POLY_MODE                4104)
(define-integrable SET_SCREEN_ANGLE             4105)
(define-integrable SET_SPREAD                   4106)
(define-integrable TRANSFORM_CTM                4107)
(define-integrable SET_CLIP_BOX                 4108)
(define-integrable SET_BOUNDS                   4109)
(define-integrable SET_MIRROR_MODE              4110)

;;Spooler Error Codes
(define-integrable SP_NOTREPORTED               #x4000)
(define-integrable SP_ERROR                     -1)
(define-integrable SP_APPABORT                  -2)
(define-integrable SP_USERABORT                 -3)
(define-integrable SP_OUTOFDISK                 -4)
(define-integrable SP_OUTOFMEMORY               -5)

(define-integrable PR_JOBSTATUS                 #x0000)

;;Object Definitions for EnumObjects
(define-integrable OBJ_PEN             1)
(define-integrable OBJ_BRUSH           2)
(define-integrable OBJ_DC              3)
(define-integrable OBJ_METADC          4)
(define-integrable OBJ_PAL             5)
(define-integrable OBJ_FONT            6)
(define-integrable OBJ_BITMAP          7)
(define-integrable OBJ_REGION          8)
(define-integrable OBJ_METAFILE        9)
(define-integrable OBJ_MEMDC           10)
(define-integrable OBJ_EXTPEN          11)
(define-integrable OBJ_ENHMETADC       12)
(define-integrable OBJ_ENHMETAFILE     13)

;;xform stuff
(define-integrable MWT_IDENTITY        1)
(define-integrable MWT_LEFTMULTIPLY    2)
(define-integrable MWT_RIGHTMULTIPLY   3)

(define-integrable MWT_MIN             MWT_IDENTITY)
(define-integrable MWT_MAX             MWT_RIGHTMULTIPLY)

;;constants for the biCompression field
(define-integrable BI_RGB        0)
(define-integrable BI_RLE8       1)
(define-integrable BI_RLE4       2)
(define-integrable BI_BITFIELDS  3)

;;tmPitchAntFamily flags
(define-integrable TMPF_FIXED_PITCH    #x01)
(define-integrable TMPF_VECTOR             #x02)
(define-integrable TMPF_DEVICE             #x08)
(define-integrable TMPF_TRUETYPE       #x04)

;;ntmFlags field flags
(define-integrable NTM_REGULAR     #x00000040)
(define-integrable NTM_BOLD        #x00000020)
(define-integrable NTM_ITALIC      #x00000001)

(define-integrable LF_FULLFACESIZE     64)

(define-integrable OUT_DEFAULT_PRECIS      0)
(define-integrable OUT_STRING_PRECIS       1)
(define-integrable OUT_CHARACTER_PRECIS    2)
(define-integrable OUT_STROKE_PRECIS       3)
(define-integrable OUT_TT_PRECIS           4)
(define-integrable OUT_DEVICE_PRECIS       5)
(define-integrable OUT_RASTER_PRECIS       6)
(define-integrable OUT_TT_ONLY_PRECIS      7)
(define-integrable OUT_OUTLINE_PRECIS      8)

(define-integrable CLIP_DEFAULT_PRECIS     0)
(define-integrable CLIP_CHARACTER_PRECIS   1)
(define-integrable CLIP_STROKE_PRECIS      2)
(define-integrable CLIP_MASK               #xf)
(define-integrable CLIP_LH_ANGLES          (* 1 16))
(define-integrable CLIP_TT_ALWAYS          (* 2 16))
(define-integrable CLIP_EMBEDDED           (* 8 16))

(define-integrable DEFAULT_QUALITY         0)
(define-integrable DRAFT_QUALITY           1)
(define-integrable PROOF_QUALITY           2)

(define-integrable DEFAULT_PITCH           0)
(define-integrable FIXED_PITCH             1)
(define-integrable VARIABLE_PITCH          2)

(define-integrable ANSI_CHARSET            0)
(define-integrable SYMBOL_CHARSET          2)
(define-integrable SHIFTJIS_CHARSET        128)
(define-integrable HANGEUL_CHARSET         129)
(define-integrable CHINESEBIG5_CHARSET     136)
(define-integrable OEM_CHARSET             255)

;;Font Families
(define-integrable FF_DONTCARE   (fix:lsh 0 4)) ;Don't care or don't know.
(define-integrable FF_ROMAN      (fix:lsh 1 4)) ;Variable stroke width, serifed.
						;Times Roman, Century Schoolbook, etc.
(define-integrable FF_SWISS      (fix:lsh 2 4)) ;Variable stroke width, sans-serifed.
						;Helvetica, Swiss, etc.
(define-integrable FF_MODERN     (fix:lsh 3 4)) ;Constant stroke width, serifed or sans-serifed.
						;Pica, Elite, Courier, etc.
(define-integrable FF_SCRIPT     (fix:lsh 4 4)) ;Cursive, etc.
(define-integrable FF_DECORATIVE (fix:lsh 5 4)) ;Old English, etc.

;;Font Weights
(define-integrable FW_DONTCARE         0)
(define-integrable FW_THIN             100)
(define-integrable FW_EXTRALIGHT       200)
(define-integrable FW_LIGHT            300)
(define-integrable FW_NORMAL           400)
(define-integrable FW_MEDIUM           500)
(define-integrable FW_SEMIBOLD         600)
(define-integrable FW_BOLD             700)
(define-integrable FW_EXTRABOLD        800)
(define-integrable FW_HEAVY            900)

(define-integrable FW_ULTRALIGHT       FW_EXTRALIGHT)
(define-integrable FW_REGULAR          FW_NORMAL)
(define-integrable FW_DEMIBOLD         FW_SEMIBOLD)
(define-integrable FW_ULTRABOLD        FW_EXTRABOLD)
(define-integrable FW_BLACK            FW_HEAVY)

(define-integrable PANOSE_COUNT               10)
(define-integrable PAN_FAMILYTYPE_INDEX        0)
(define-integrable PAN_SERIFSTYLE_INDEX        1)
(define-integrable PAN_WEIGHT_INDEX            2)
(define-integrable PAN_PROPORTION_INDEX        3)
(define-integrable PAN_CONTRAST_INDEX          4)
(define-integrable PAN_STROKEVARIATION_INDEX   5)
(define-integrable PAN_ARMSTYLE_INDEX          6)
(define-integrable PAN_LETTERFORM_INDEX        7)
(define-integrable PAN_MIDLINE_INDEX           8)
(define-integrable PAN_XHEIGHT_INDEX           9)

(define-integrable PAN_CULTURE_LATIN           0)

(define-integrable PAN_ANY                         0);Any
(define-integrable PAN_NO_FIT                      1);No Fit

(define-integrable PAN_FAMILY_TEXT_DISPLAY         2);Text and Display
(define-integrable PAN_FAMILY_SCRIPT               3);Script
(define-integrable PAN_FAMILY_DECORATIVE           4);Decorative
(define-integrable PAN_FAMILY_PICTORIAL            5);Pictorial

(define-integrable PAN_SERIF_COVE                  2);Cove
(define-integrable PAN_SERIF_OBTUSE_COVE           3);Obtuse Cove
(define-integrable PAN_SERIF_SQUARE_COVE           4);Square Cove
(define-integrable PAN_SERIF_OBTUSE_SQUARE_COVE    5);Obtuse Square Cove
(define-integrable PAN_SERIF_SQUARE                6);Square
(define-integrable PAN_SERIF_THIN                  7);Thin
(define-integrable PAN_SERIF_BONE                  8);Bone
(define-integrable PAN_SERIF_EXAGGERATED           9);Exaggerated
(define-integrable PAN_SERIF_TRIANGLE             10);Triangle
(define-integrable PAN_SERIF_NORMAL_SANS          11);Normal Sans
(define-integrable PAN_SERIF_OBTUSE_SANS          12);Obtuse Sans
(define-integrable PAN_SERIF_PERP_SANS            13);Prep Sans
(define-integrable PAN_SERIF_FLARED               14);Flared
(define-integrable PAN_SERIF_ROUNDED              15);Rounded

(define-integrable PAN_WEIGHT_VERY_LIGHT           2);Very Light
(define-integrable PAN_WEIGHT_LIGHT                3);Light
(define-integrable PAN_WEIGHT_THIN                 4);Thin
(define-integrable PAN_WEIGHT_BOOK                 5);Book
(define-integrable PAN_WEIGHT_MEDIUM               6);Medium
(define-integrable PAN_WEIGHT_DEMI                 7);Demi
(define-integrable PAN_WEIGHT_BOLD                 8);Bold
(define-integrable PAN_WEIGHT_HEAVY                9);Heavy
(define-integrable PAN_WEIGHT_BLACK               10);Black
(define-integrable PAN_WEIGHT_NORD                11);Nord

(define-integrable PAN_PROP_OLD_STYLE              2);Old Style
(define-integrable PAN_PROP_MODERN                 3);Modern
(define-integrable PAN_PROP_EVEN_WIDTH             4);Even Width
(define-integrable PAN_PROP_EXPANDED               5);Expanded
(define-integrable PAN_PROP_CONDENSED              6);Condensed
(define-integrable PAN_PROP_VERY_EXPANDED          7);Very Expanded
(define-integrable PAN_PROP_VERY_CONDENSED         8);Very Condensed
(define-integrable PAN_PROP_MONOSPACED             9);Monospaced

(define-integrable PAN_CONTRAST_NONE               2);None
(define-integrable PAN_CONTRAST_VERY_LOW           3);Very Low
(define-integrable PAN_CONTRAST_LOW                4);Low
(define-integrable PAN_CONTRAST_MEDIUM_LOW         5);Medium Low
(define-integrable PAN_CONTRAST_MEDIUM             6);Medium
(define-integrable PAN_CONTRAST_MEDIUM_HIGH        7);Mediim High
(define-integrable PAN_CONTRAST_HIGH               8);High
(define-integrable PAN_CONTRAST_VERY_HIGH          9);Very High

(define-integrable PAN_STROKE_GRADUAL_DIAG         2);Gradual/Diagonal
(define-integrable PAN_STROKE_GRADUAL_TRAN         3);Gradual/Transitional
(define-integrable PAN_STROKE_GRADUAL_VERT         4);Gradual/Vertical
(define-integrable PAN_STROKE_GRADUAL_HORZ         5);Gradual/Horizontal
(define-integrable PAN_STROKE_RAPID_VERT           6);Rapid/Vertical
(define-integrable PAN_STROKE_RAPID_HORZ           7);Rapid/Horizontal
(define-integrable PAN_STROKE_INSTANT_VERT         8);Instant/Vertical

(define-integrable PAN_STRAIGHT_ARMS_HORZ          2);Straight Arms/Horizontal
(define-integrable PAN_STRAIGHT_ARMS_WEDGE         3);Straight Arms/Wedge
(define-integrable PAN_STRAIGHT_ARMS_VERT          4);Straight Arms/Vertical
(define-integrable PAN_STRAIGHT_ARMS_SINGLE_SERIF  5);Straight Arms/Single-Serif
(define-integrable PAN_STRAIGHT_ARMS_DOUBLE_SERIF  6);Straight Arms/Double-Serif
(define-integrable PAN_BENT_ARMS_HORZ              7);Non-Straight Arms/Horizontal
(define-integrable PAN_BENT_ARMS_WEDGE             8);Non-Straight Arms/Wedge
(define-integrable PAN_BENT_ARMS_VERT              9);Non-Straight Arms/Vertical
(define-integrable PAN_BENT_ARMS_SINGLE_SERIF     10);Non-Straight Arms/Single-Serif
(define-integrable PAN_BENT_ARMS_DOUBLE_SERIF     11);Non-Straight Arms/Double-Serif

(define-integrable PAN_LETT_NORMAL_CONTACT         2);Normal/Contact
(define-integrable PAN_LETT_NORMAL_WEIGHTED        3);Normal/Weighted
(define-integrable PAN_LETT_NORMAL_BOXED           4);Normal/Boxed
(define-integrable PAN_LETT_NORMAL_FLATTENED       5);Normal/Flattened
(define-integrable PAN_LETT_NORMAL_ROUNDED         6);Normal/Rounded
(define-integrable PAN_LETT_NORMAL_OFF_CENTER      7);Normal/Off Center
(define-integrable PAN_LETT_NORMAL_SQUARE          8);Normal/Square
(define-integrable PAN_LETT_OBLIQUE_CONTACT        9);Oblique/Contact
(define-integrable PAN_LETT_OBLIQUE_WEIGHTED      10);Oblique/Weighted
(define-integrable PAN_LETT_OBLIQUE_BOXED         11);Oblique/Boxed
(define-integrable PAN_LETT_OBLIQUE_FLATTENED     12);Oblique/Flattened
(define-integrable PAN_LETT_OBLIQUE_ROUNDED       13);Oblique/Rounded
(define-integrable PAN_LETT_OBLIQUE_OFF_CENTER    14);Oblique/Off Center
(define-integrable PAN_LETT_OBLIQUE_SQUARE        15);Oblique/Square

(define-integrable PAN_MIDLINE_STANDARD_TRIMMED    2);Standard/Trimmed
(define-integrable PAN_MIDLINE_STANDARD_POINTED    3);Standard/Pointed
(define-integrable PAN_MIDLINE_STANDARD_SERIFED    4);Standard/Serifed
(define-integrable PAN_MIDLINE_HIGH_TRIMMED        5);High/Trimmed
(define-integrable PAN_MIDLINE_HIGH_POINTED        6);High/Pointed
(define-integrable PAN_MIDLINE_HIGH_SERIFED        7);High/Serifed
(define-integrable PAN_MIDLINE_CONSTANT_TRIMMED    8);Constant/Trimmed
(define-integrable PAN_MIDLINE_CONSTANT_POINTED    9);Constant/Pointed
(define-integrable PAN_MIDLINE_CONSTANT_SERIFED   10);Constant/Serifed
(define-integrable PAN_MIDLINE_LOW_TRIMMED        11);Low/Trimmed
(define-integrable PAN_MIDLINE_LOW_POINTED        12);Low/Pointed
(define-integrable PAN_MIDLINE_LOW_SERIFED        13);Low/Serifed

(define-integrable PAN_XHEIGHT_CONSTANT_SMALL      2);Constant/Small
(define-integrable PAN_XHEIGHT_CONSTANT_STD        3);Constant/Standard
(define-integrable PAN_XHEIGHT_CONSTANT_LARGE      4);Constant/Large
(define-integrable PAN_XHEIGHT_DUCKING_SMALL       5);Ducking/Small
(define-integrable PAN_XHEIGHT_DUCKING_STD         6);Ducking/Standard
(define-integrable PAN_XHEIGHT_DUCKING_LARGE       7);Ducking/Large

(define-integrable ELF_VENDOR_SIZE     4)

;;The extended logical font
;;An extension of the ENUMLOGFONT
(define-integrable ELF_VERSION         0)
(define-integrable ELF_CULTURE_LATIN   0)

;;EnumFonts Masks
(define-integrable RASTER_FONTTYPE     #x001)
(define-integrable DEVICE_FONTTYPE     #x002)
(define-integrable TRUETYPE_FONTTYPE   #x004)

;;palette entry flags
(define-integrable PC_RESERVED     #x01   );palette index used for animation
(define-integrable PC_EXPLICIT     #x02   );palette index is explicit to device
(define-integrable PC_NOCOLLAPSE   #x04   );do not match color to system palette

;;Background Modes
(define-integrable TRANSPARENT         1)
(define-integrable OPAQUE              2)
(define-integrable BKMODE_LAST         2)

;;Graphics Modes
(define-integrable GM_COMPATIBLE       1)
(define-integrable GM_ADVANCED         2)
(define-integrable GM_LAST             2)

;;PolyDraw and GetPath point types
(define-integrable PT_CLOSEFIGURE      #x01)
(define-integrable PT_LINETO           #x02)
(define-integrable PT_BEZIERTO         #x04)
(define-integrable PT_MOVETO           #x06)

;;Mapping Modes
(define-integrable MM_TEXT             1)
(define-integrable MM_LOMETRIC         2)
(define-integrable MM_HIMETRIC         3)
(define-integrable MM_LOENGLISH        4)
(define-integrable MM_HIENGLISH        5)
(define-integrable MM_TWIPS            6)
(define-integrable MM_ISOTROPIC        7)
(define-integrable MM_ANISOTROPIC      8)

;;Min and Max Mapping Mode values
(define-integrable MM_MIN              MM_TEXT)
(define-integrable MM_MAX              MM_ANISOTROPIC)
(define-integrable MM_MAX_FIXEDSCALE   MM_TWIPS)

;;Coordinate Modes
(define-integrable ABSOLUTE            1)
(define-integrable RELATIVE            2)

;;Stock Logical Objects
(define-integrable WHITE_BRUSH         0)
(define-integrable LTGRAY_BRUSH        1)
(define-integrable GRAY_BRUSH          2)
(define-integrable DKGRAY_BRUSH        3)
(define-integrable BLACK_BRUSH         4)
(define-integrable NULL_BRUSH          5)
(define-integrable HOLLOW_BRUSH        NULL_BRUSH)
(define-integrable WHITE_PEN           6)
(define-integrable BLACK_PEN           7)
(define-integrable NULL_PEN            8)
(define-integrable OEM_FIXED_FONT      10)
(define-integrable ANSI_FIXED_FONT     11)
(define-integrable ANSI_VAR_FONT       12)
(define-integrable SYSTEM_FONT         13)
(define-integrable DEVICE_DEFAULT_FONT 14)
(define-integrable DEFAULT_PALETTE     15)
(define-integrable SYSTEM_FIXED_FONT   16)
(define-integrable STOCK_LAST          16)

(define-integrable CLR_INVALID     #xFFFFFFFF)

;;Brush Styles
(define-integrable BS_SOLID            0)
(define-integrable BS_NULL             1)
(define-integrable BS_HOLLOW           BS_NULL)
(define-integrable BS_HATCHED          2)
(define-integrable BS_PATTERN          3)
(define-integrable BS_INDEXED          4)
(define-integrable BS_DIBPATTERN       5)
(define-integrable BS_DIBPATTERNPT     6)
(define-integrable BS_PATTERN8X8       7)

;;Hatch Styles
(define-integrable HS_HORIZONTAL       0)       ;-----
(define-integrable HS_VERTICAL         1)       ;|||||
(define-integrable HS_FDIAGONAL        2)       ;\\\\\
(define-integrable HS_BDIAGONAL        3)       ;/////
(define-integrable HS_CROSS            4)       ;+++++
(define-integrable HS_DIAGCROSS        5)       ;xxxxx
(define-integrable HS_FDIAGONAL1       6)
(define-integrable HS_BDIAGONAL1       7)
(define-integrable HS_SOLID            8)
(define-integrable HS_DENSE1           9)
(define-integrable HS_DENSE2           10)
(define-integrable HS_DENSE3           11)
(define-integrable HS_DENSE4           12)
(define-integrable HS_DENSE5           13)
(define-integrable HS_DENSE6           14)
(define-integrable HS_DENSE7           15)
(define-integrable HS_DENSE8           16)
(define-integrable HS_NOSHADE          17)
(define-integrable HS_HALFTONE         18)
(define-integrable HS_SOLIDCLR	       19)
(define-integrable HS_DITHEREDCLR      20)
(define-integrable HS_SOLIDTEXTCLR     21)
(define-integrable HS_DITHEREDTEXTCLR  22)
(define-integrable HS_SOLIDBKCLR       23)
(define-integrable HS_DITHEREDBKCLR    24)
(define-integrable HS_API_MAX          25)

;;Pen Styles
(define-integrable PS_SOLID            0)
(define-integrable PS_DASH             1)       ;-------
(define-integrable PS_DOT              2)       ;.......
(define-integrable PS_DASHDOT          3)       ;_._._._
(define-integrable PS_DASHDOTDOT       4)       ;_.._.._
(define-integrable PS_NULL             5)
(define-integrable PS_INSIDEFRAME      6)
(define-integrable PS_USERSTYLE        7)
(define-integrable PS_ALTERNATE        8)
(define-integrable PS_STYLE_MASK       #x0000000F)

(define-integrable PS_ENDCAP_ROUND     #x00000000)
(define-integrable PS_ENDCAP_SQUARE    #x00000100)
(define-integrable PS_ENDCAP_FLAT      #x00000200)
(define-integrable PS_ENDCAP_MASK      #x00000F00)

(define-integrable PS_JOIN_ROUND       #x00000000)
(define-integrable PS_JOIN_BEVEL       #x00001000)
(define-integrable PS_JOIN_MITER       #x00002000)
(define-integrable PS_JOIN_MASK        #x0000F000)

(define-integrable PS_COSMETIC         #x00000000)
(define-integrable PS_GEOMETRIC        #x00010000)
(define-integrable PS_TYPE_MASK        #x000F0000)

(define-integrable AD_COUNTERCLOCKWISE 1)
(define-integrable AD_CLOCKWISE        2)

;;Device Parameters for GetDeviceCaps
(define-integrable DRIVERVERSION 0)     ;Device driver version
(define-integrable TECHNOLOGY    2)     ;Device classification
(define-integrable HORZSIZE      4)     ;Horizontal size in millimeters
(define-integrable VERTSIZE      6)     ;Vertical size in millimeters
(define-integrable HORZRES       8)     ;Horizontal width in pixels
(define-integrable VERTRES       10)    ;Vertical width in pixels
(define-integrable BITSPIXEL     12)    ;Number of bits per pixel
(define-integrable PLANES        14)    ;Number of planes
(define-integrable NUMBRUSHES    16)    ;Number of brushes the device has
(define-integrable NUMPENS       18)    ;Number of pens the device has
(define-integrable NUMMARKERS    20)    ;Number of markers the device has
(define-integrable NUMFONTS      22)    ;Number of fonts the device has
(define-integrable NUMCOLORS     24)    ;Number of colors the device supports
(define-integrable PDEVICESIZE   26)    ;Size required for device descriptor
(define-integrable CURVECAPS     28)    ;Curve capabilities
(define-integrable LINECAPS      30)    ;Line capabilities
(define-integrable POLYGONALCAPS 32)    ;Polygonal capabilities
(define-integrable TEXTCAPS      34)    ;Text capabilities
(define-integrable CLIPCAPS      36)    ;Clipping capabilities
(define-integrable RASTERCAPS    38)    ;Bitblt capabilities
(define-integrable ASPECTX       40)    ;Length of the X leg
(define-integrable ASPECTY       42)    ;Length of the Y leg
(define-integrable ASPECTXY      44)    ;Length of the hypotenuse

(define-integrable LOGPIXELSX    88)    ;Logical pixels/inch in X
(define-integrable LOGPIXELSY    90)    ;Logical pixels/inch in Y

(define-integrable SIZEPALETTE  104)    ;Number of entries in physical palette
(define-integrable NUMRESERVED  106)    ;Number of reserved entries in palette
(define-integrable COLORRES     108)    ;Actual color resolution

;; Printing related DeviceCaps. These replace the appropriate Escapes
(define-integrable PHYSICALWIDTH   110) ;// Physical Width in device units
(define-integrable PHYSICALHEIGHT  111) ;// Physical Height in device units
(define-integrable PHYSICALOFFSETX 112) ;// Physical Printable Area x margin
(define-integrable PHYSICALOFFSETY 113) ;// Physical Printable Area y margin
(define-integrable SCALINGFACTORX  114) ;// Scaling factor x
(define-integrable SCALINGFACTORY  115) ;// Scaling factor y

;;;Device Capability Masks:

;;Device Technologies
(define-integrable DT_PLOTTER          0)   ;Vector plotter
(define-integrable DT_RASDISPLAY       1)   ;Raster display
(define-integrable DT_RASPRINTER       2)   ;Raster printer
(define-integrable DT_RASCAMERA        3)   ;Raster camera
(define-integrable DT_CHARSTREAM       4)   ;Character-stream, PLP
(define-integrable DT_METAFILE         5)   ;Metafile, VDM
(define-integrable DT_DISPFILE         6)   ;Display-file

;;Curve Capabilities
(define-integrable CC_NONE             0)   ;Curves not supported
(define-integrable CC_CIRCLES          1)   ;Can do circles
(define-integrable CC_PIE              2)   ;Can do pie wedges
(define-integrable CC_CHORD            4)   ;Can do chord arcs
(define-integrable CC_ELLIPSES         8)   ;Can do ellipese
(define-integrable CC_WIDE             16)  ;Can do wide lines
(define-integrable CC_STYLED           32)  ;Can do styled lines
(define-integrable CC_WIDESTYLED       64)  ;Can do wide styled lines
(define-integrable CC_INTERIORS        128) ;Can do interiors
(define-integrable CC_ROUNDRECT        256) ;

;;Line Capabilities
(define-integrable LC_NONE             0)   ;Lines not supported
(define-integrable LC_POLYLINE         2)   ;Can do polylines
(define-integrable LC_MARKER           4)   ;Can do markers
(define-integrable LC_POLYMARKER       8)   ;Can do polymarkers
(define-integrable LC_WIDE             16)  ;Can do wide lines
(define-integrable LC_STYLED           32)  ;Can do styled lines
(define-integrable LC_WIDESTYLED       64)  ;Can do wide styled lines
(define-integrable LC_INTERIORS        128) ;Can do interiors

;;Polygonal Capabilities
(define-integrable PC_NONE             0)   ;Polygonals not supported
(define-integrable PC_POLYGON          1)   ;Can do polygons
(define-integrable PC_RECTANGLE        2)   ;Can do rectangles
(define-integrable PC_WINDPOLYGON      4)   ;Can do winding polygons
(define-integrable PC_TRAPEZOID        4)   ;Can do trapezoids
(define-integrable PC_SCANLINE         8)   ;Can do scanlines
(define-integrable PC_WIDE             16)  ;Can do wide borders
(define-integrable PC_STYLED           32)  ;Can do styled borders
(define-integrable PC_WIDESTYLED       64)  ;Can do wide styled borders
(define-integrable PC_INTERIORS        128) ;Can do interiors

;;Polygonal Capabilities
(define-integrable CP_NONE             0)   ;No clipping of output
(define-integrable CP_RECTANGLE        1)   ;Output clipped to rects
(define-integrable CP_REGION           2)   ;

;;Text Capabilities
(define-integrable TC_OP_CHARACTER     #x00000001)  ;Can do OutputPrecision   CHARACTER
(define-integrable TC_OP_STROKE        #x00000002)  ;Can do OutputPrecision   STROKE
(define-integrable TC_CP_STROKE        #x00000004)  ;Can do ClipPrecision     STROKE
(define-integrable TC_CR_90            #x00000008)  ;Can do CharRotAbility    90
(define-integrable TC_CR_ANY           #x00000010)  ;Can do CharRotAbility    ANY
(define-integrable TC_SF_X_YINDEP      #x00000020)  ;Can do ScaleFreedom      X_YINDEPENDENT
(define-integrable TC_SA_DOUBLE        #x00000040)  ;Can do ScaleAbility      DOUBLE
(define-integrable TC_SA_INTEGER       #x00000080)  ;Can do ScaleAbility      INTEGER
(define-integrable TC_SA_CONTIN        #x00000100)  ;Can do ScaleAbility      CONTINUOUS
(define-integrable TC_EA_DOUBLE        #x00000200)  ;Can do EmboldenAbility   DOUBLE
(define-integrable TC_IA_ABLE          #x00000400)  ;Can do ItalisizeAbility  ABLE
(define-integrable TC_UA_ABLE          #x00000800)  ;Can do UnderlineAbility  ABLE
(define-integrable TC_SO_ABLE          #x00001000)  ;Can do StrikeOutAbility  ABLE
(define-integrable TC_RA_ABLE          #x00002000)  ;Can do RasterFontAble    ABLE
(define-integrable TC_VA_ABLE          #x00004000)  ;Can do VectorFontAble    ABLE
(define-integrable TC_RESERVED         #x00008000)
(define-integrable TC_SCROLLBLT        #x00010000)  ;do text scroll with blt

;;Raster Capabilities
(define-integrable RC_BITBLT           1)       ;Can do standard BLT.
(define-integrable RC_BANDING          2)       ;Device requires banding support
(define-integrable RC_SCALING          4)       ;Device requires scaling support
(define-integrable RC_BITMAP64         8)       ;Device can support >64K bitmap
(define-integrable RC_GDI20_OUTPUT     #x0010)      ;has 2.0 output calls
(define-integrable RC_GDI20_STATE      #x0020)
(define-integrable RC_SAVEBITMAP       #x0040)
(define-integrable RC_DI_BITMAP        #x0080)      ;supports DIB to memory
(define-integrable RC_PALETTE          #x0100)      ;supports a palette
(define-integrable RC_DIBTODEV         #x0200)      ;supports DIBitsToDevice
(define-integrable RC_BIGFONT          #x0400)      ;supports >64K fonts
(define-integrable RC_STRETCHBLT       #x0800)      ;supports StretchBlt
(define-integrable RC_FLOODFILL        #x1000)      ;supports FloodFill
(define-integrable RC_STRETCHDIB       #x2000)      ;supports StretchDIBits
(define-integrable RC_OP_DX_OUTPUT     #x4000)
(define-integrable RC_DEVBITS          #x8000)

;;DIB color table identifiers
(define-integrable DIB_RGB_COLORS      0) ;color table in RGBs
(define-integrable DIB_PAL_COLORS      1) ;color table in palette indices
(define-integrable DIB_PAL_INDICES     2) ;No color table indices into surf palette
(define-integrable DIB_PAL_PHYSINDICES 2) ;No color table indices into surf palette
(define-integrable DIB_PAL_LOGINDICES  4) ;No color table indices into DC palette

;;constants for Get/SetSystemPaletteUse()
(define-integrable SYSPAL_ERROR    0)
(define-integrable SYSPAL_STATIC   1)
(define-integrable SYSPAL_NOSTATIC 2)

;;constants for CreateDIBitmap
(define-integrable CBM_CREATEDIB   #x02)   ;create DIB bitmap
(define-integrable CBM_INIT        #x04)   ;initialize bitmap

;;ExtFloodFill style flags
(define-integrable  FLOODFILLBORDER   0)
(define-integrable  FLOODFILLSURFACE  1)

;;size of a device name string
(define-integrable CCHDEVICENAME 32)

;;size of a form name string
(define-integrable CCHFORMNAME 32)

;;current version of specification
(define-integrable DM_SPECVERSION #x320)

;;field selection bits
(define-integrable DM_ORIENTATION      #x0000001)
(define-integrable DM_PAPERSIZE        #x0000002)
(define-integrable DM_PAPERLENGTH      #x0000004)
(define-integrable DM_PAPERWIDTH       #x0000008)
(define-integrable DM_SCALE            #x0000010)
(define-integrable DM_COPIES           #x0000100)
(define-integrable DM_DEFAULTSOURCE    #x0000200)
(define-integrable DM_PRINTQUALITY     #x0000400)
(define-integrable DM_COLOR            #x0000800)
(define-integrable DM_DUPLEX           #x0001000)
(define-integrable DM_YRESOLUTION      #x0002000)
(define-integrable DM_TTOPTION         #x0004000)
(define-integrable DM_COLLATE          #x0008000)
(define-integrable DM_FORMNAME         #x0010000)

;;orientation selections
(define-integrable DMORIENT_PORTRAIT   1)
(define-integrable DMORIENT_LANDSCAPE  2)

;;paper selections
(define-integrable DMPAPER_LETTER      1)           ;Letter 8 1/2 x 11 in
(define-integrable DMPAPER_FIRST       DMPAPER_LETTER)
(define-integrable DMPAPER_LETTERSMALL 2)           ;Letter Small 8 1/2 x 11 in
(define-integrable DMPAPER_TABLOID     3)           ;Tabloid 11 x 17 in
(define-integrable DMPAPER_LEDGER      4)           ;Ledger 17 x 11 in
(define-integrable DMPAPER_LEGAL       5)           ;Legal 8 1/2 x 14 in
(define-integrable DMPAPER_STATEMENT   6)           ;Statement 5 1/2 x 8 1/2 in
(define-integrable DMPAPER_EXECUTIVE   7)           ;Executive 7 1/4 x 10 1/2 in
(define-integrable DMPAPER_A3          8)           ;A3 297 x 420 mm
(define-integrable DMPAPER_A4          9)           ;A4 210 x 297 mm
(define-integrable DMPAPER_A4SMALL     10)          ;A4 Small 210 x 297 mm
(define-integrable DMPAPER_A5          11)          ;A5 148 x 210 mm
(define-integrable DMPAPER_B4          12)          ;B4 250 x 354
(define-integrable DMPAPER_B5          13)          ;B5 182 x 257 mm
(define-integrable DMPAPER_FOLIO       14)          ;Folio 8 1/2 x 13 in
(define-integrable DMPAPER_QUARTO      15)          ;Quarto 215 x 275 mm
(define-integrable DMPAPER_10X14       16)          ;10x14 in
(define-integrable DMPAPER_11X17       17)          ;11x17 in
(define-integrable DMPAPER_NOTE        18)          ;Note 8 1/2 x 11 in
(define-integrable DMPAPER_ENV_9       19)          ;Envelope #9 3 7/8 x 8 7/8
(define-integrable DMPAPER_ENV_10      20)          ;Envelope #10 4 1/8 x 9 1/2
(define-integrable DMPAPER_ENV_11      21)          ;Envelope #11 4 1/2 x 10 3/8
(define-integrable DMPAPER_ENV_12      22)          ;Envelope #12 4 \276 x 11
(define-integrable DMPAPER_ENV_14      23)          ;Envelope #14 5 x 11 1/2
(define-integrable DMPAPER_CSHEET      24)          ;C size sheet
(define-integrable DMPAPER_DSHEET      25)          ;D size sheet
(define-integrable DMPAPER_ESHEET      26)          ;E size sheet
(define-integrable DMPAPER_ENV_DL      27)          ;Envelope DL 110 x 220mm
(define-integrable DMPAPER_ENV_C5      28)          ;Envelope C5 162 x 229 mm
(define-integrable DMPAPER_ENV_C3      29)          ;Envelope C3  324 x 458 mm
(define-integrable DMPAPER_ENV_C4      30)          ;Envelope C4  229 x 324 mm
(define-integrable DMPAPER_ENV_C6      31)          ;Envelope C6  114 x 162 mm
(define-integrable DMPAPER_ENV_C65     32)          ;Envelope C65 114 x 229 mm
(define-integrable DMPAPER_ENV_B4      33)          ;Envelope B4  250 x 353 mm
(define-integrable DMPAPER_ENV_B5      34)          ;Envelope B5  176 x 250 mm
(define-integrable DMPAPER_ENV_B6      35)          ;Envelope B6  176 x 125 mm
(define-integrable DMPAPER_ENV_ITALY   36)          ;Envelope 110 x 230 mm
(define-integrable DMPAPER_ENV_MONARCH 37)          ;Envelope Monarch 3.875 x 7.5 in
(define-integrable DMPAPER_ENV_PERSONAL 38)         ;6 3/4 Envelope 3 5/8 x 6 1/2 in
(define-integrable DMPAPER_FANFOLD_US  39)          ;US Std Fanfold 14 7/8 x 11 in
(define-integrable DMPAPER_FANFOLD_STD_GERMAN  40)  ;German Std Fanfold 8 1/2 x 12 in
(define-integrable DMPAPER_FANFOLD_LGL_GERMAN  41)  ;German Legal Fanfold 8 1/2 x 13 in

(define-integrable DMPAPER_LAST        DMPAPER_FANFOLD_LGL_GERMAN)

(define-integrable DMPAPER_USER        256)

;;bin selections
(define-integrable DMBIN_UPPER         1)
(define-integrable DMBIN_FIRST         DMBIN_UPPER)
(define-integrable DMBIN_ONLYONE       1)
(define-integrable DMBIN_LOWER         2)
(define-integrable DMBIN_MIDDLE        3)
(define-integrable DMBIN_MANUAL        4)
(define-integrable DMBIN_ENVELOPE      5)
(define-integrable DMBIN_ENVMANUAL     6)
(define-integrable DMBIN_AUTO          7)
(define-integrable DMBIN_TRACTOR       8)
(define-integrable DMBIN_SMALLFMT      9)
(define-integrable DMBIN_LARGEFMT      10)
(define-integrable DMBIN_LARGECAPACITY 11)
(define-integrable DMBIN_CASSETTE      14)
(define-integrable DMBIN_LAST          DMBIN_CASSETTE)

(define-integrable DMBIN_USER          256)     ;device specific bins start here

;;print qualities
(define-integrable DMRES_DRAFT         -1)
(define-integrable DMRES_LOW           -2)
(define-integrable DMRES_MEDIUM        -3)
(define-integrable DMRES_HIGH          -4)

;;color enable/disable for color printers
(define-integrable DMCOLOR_MONOCHROME  1)
(define-integrable DMCOLOR_COLOR       2)

;;duplex enable
(define-integrable DMDUP_SIMPLEX    1)
(define-integrable DMDUP_VERTICAL   2)
(define-integrable DMDUP_HORIZONTAL 3)

;;TrueType options
(define-integrable DMTT_BITMAP     1)       ;print TT fonts as graphics
(define-integrable DMTT_DOWNLOAD   2)       ;download TT fonts as soft fonts
(define-integrable DMTT_SUBDEV     3)       ;substitute device fonts for TT fonts

;;Collation selections
(define-integrable DMCOLLATE_FALSE  0)
(define-integrable DMCOLLATE_TRUE   1)

;;DEVMODE dmDisplayFlags flags
(define-integrable DM_GRAYSCALE  #x00000001)
(define-integrable DM_INTERLACED #x00000002)

;;GetRegionData/ExtCreateRegion
(define-integrable RDH_RECTANGLES  1)

;;GetGlyphOutline constants
(define-integrable GGO_NONE           0)
(define-integrable GGO_BITMAP         1)
(define-integrable GGO_NATIVE         2)

(define-integrable TT_POLYGON_TYPE   24)

(define-integrable TT_PRIM_LINE       1)
(define-integrable TT_PRIM_QSPLINE    2)

;;bits defined in wFlags of RASTERIZER_STATUS
(define-integrable TT_AVAILABLE    #x0001)
(define-integrable TT_ENABLED      #x0002)

;;mode selections for the device mode function
(define-integrable DM_UPDATE           1)
(define-integrable DM_COPY             2)
(define-integrable DM_PROMPT           4)
(define-integrable DM_MODIFY           8)

(define-integrable DM_IN_BUFFER        DM_MODIFY)
(define-integrable DM_IN_PROMPT        DM_PROMPT)
(define-integrable DM_OUT_BUFFER       DM_COPY)
(define-integrable DM_OUT_DEFAULT      DM_UPDATE)

;;device capabilities indices
(define-integrable DC_FIELDS           1)
(define-integrable DC_PAPERS           2)
(define-integrable DC_PAPERSIZE        3)
(define-integrable DC_MINEXTENT        4)
(define-integrable DC_MAXEXTENT        5)
(define-integrable DC_BINS             6)
(define-integrable DC_DUPLEX           7)
(define-integrable DC_SIZE             8)
(define-integrable DC_EXTRA            9)
(define-integrable DC_VERSION          10)
(define-integrable DC_DRIVER           11)
(define-integrable DC_BINNAMES         12)
(define-integrable DC_ENUMRESOLUTIONS  13)
(define-integrable DC_FILEDEPENDENCIES 14)
(define-integrable DC_TRUETYPE         15)
(define-integrable DC_PAPERNAMES       16)
(define-integrable DC_ORIENTATION      17)
(define-integrable DC_COPIES           18)

;;Flags value for COLORADJUSTMENT
(define-integrable CA_NEGATIVE                 #x0001)
(define-integrable CA_LOG_FILTER               #x0002)

;;IlluminantIndex values
(define-integrable ILLUMINANT_DEVICE_DEFAULT   0)
(define-integrable ILLUMINANT_A                1)
(define-integrable ILLUMINANT_B                2)
(define-integrable ILLUMINANT_C                3)
(define-integrable ILLUMINANT_D50              4)
(define-integrable ILLUMINANT_D55              5)
(define-integrable ILLUMINANT_D65              6)
(define-integrable ILLUMINANT_D75              7)
(define-integrable ILLUMINANT_F2               8)
(define-integrable ILLUMINANT_MAX_INDEX        ILLUMINANT_F2)

(define-integrable ILLUMINANT_TUNGSTEN         ILLUMINANT_A)
(define-integrable ILLUMINANT_DAYLIGHT         ILLUMINANT_C)
(define-integrable ILLUMINANT_FLUORESCENT      ILLUMINANT_F2)
(define-integrable ILLUMINANT_NTSC             ILLUMINANT_C)
;;
;;Min and max for RedGamma, GreenGamma, BlueGamma
(define-integrable RGB_GAMMA_MIN               02500)  ;(WORD)02500
(define-integrable RGB_GAMMA_MAX               65000)

;;Min and max for ReferenceBlack and ReferenceWhite
(define-integrable REFERENCE_WHITE_MIN         6000)	;(WORD)6000
(define-integrable REFERENCE_WHITE_MAX         10000)	;(WORD)10000
(define-integrable REFERENCE_BLACK_MIN         0)	;(WORD)0
(define-integrable REFERENCE_BLACK_MAX         4000)	;(WORD)4000

;;Min and max for Contrast, Brightness, Colorfulness, RedGreenTint
(define-integrable COLOR_ADJ_MIN               -100)	;(SHORT)-100
(define-integrable COLOR_ADJ_MAX               100)	;(SHORT)100

;;Enhanced metafile record types.
(define-integrable EMR_HEADER			1)
(define-integrable EMR_POLYBEZIER			2)
(define-integrable EMR_POLYGON			3)
(define-integrable EMR_POLYLINE			4)
(define-integrable EMR_POLYBEZIERTO		5)
(define-integrable EMR_POLYLINETO			6)
(define-integrable EMR_POLYPOLYLINE		7)
(define-integrable EMR_POLYPOLYGON			8)
(define-integrable EMR_SETWINDOWEXTEX		9)
(define-integrable EMR_SETWINDOWORGEX		10)
(define-integrable EMR_SETVIEWPORTEXTEX		11)
(define-integrable EMR_SETVIEWPORTORGEX		12)
(define-integrable EMR_SETBRUSHORGEX		13)
(define-integrable EMR_EOF				14)
(define-integrable EMR_SETPIXELV			15)
(define-integrable EMR_SETMAPPERFLAGS		16)
(define-integrable EMR_SETMAPMODE			17)
(define-integrable EMR_SETBKMODE			18)
(define-integrable EMR_SETPOLYFILLMODE		19)
(define-integrable EMR_SETROP2			20)
(define-integrable EMR_SETSTRETCHBLTMODE		21)
(define-integrable EMR_SETTEXTALIGN		22)
(define-integrable EMR_SETCOLORADJUSTMENT		23)
(define-integrable EMR_SETTEXTCOLOR		24)
(define-integrable EMR_SETBKCOLOR			25)
(define-integrable EMR_OFFSETCLIPRGN		26)
(define-integrable EMR_MOVETOEX			27)
(define-integrable EMR_SETMETARGN			28)
(define-integrable EMR_EXCLUDECLIPRECT		29)
(define-integrable EMR_INTERSECTCLIPRECT		30)
(define-integrable EMR_SCALEVIEWPORTEXTEX		31)
(define-integrable EMR_SCALEWINDOWEXTEX		32)
(define-integrable EMR_SAVEDC			33)
(define-integrable EMR_RESTOREDC			34)
(define-integrable EMR_SETWORLDTRANSFORM		35)
(define-integrable EMR_MODIFYWORLDTRANSFORM	36)
(define-integrable EMR_SELECTOBJECT		37)
(define-integrable EMR_CREATEPEN			38)
(define-integrable EMR_CREATEBRUSHINDIRECT		39)
(define-integrable EMR_DELETEOBJECT		40)
(define-integrable EMR_ANGLEARC			41)
(define-integrable EMR_ELLIPSE			42)
(define-integrable EMR_RECTANGLE			43)
(define-integrable EMR_ROUNDRECT			44)
(define-integrable EMR_ARC				45)
(define-integrable EMR_CHORD			46)
(define-integrable EMR_PIE				47)
(define-integrable EMR_SELECTPALETTE		48)
(define-integrable EMR_CREATEPALETTE		49)
(define-integrable EMR_SETPALETTEENTRIES		50)
(define-integrable EMR_RESIZEPALETTE		51)
(define-integrable EMR_REALIZEPALETTE		52)
(define-integrable EMR_EXTFLOODFILL		53)
(define-integrable EMR_LINETO			54)
(define-integrable EMR_ARCTO			55)
(define-integrable EMR_POLYDRAW			56)
(define-integrable EMR_SETARCDIRECTION		57)
(define-integrable EMR_SETMITERLIMIT		58)
(define-integrable EMR_BEGINPATH			59)
(define-integrable EMR_ENDPATH			60)
(define-integrable EMR_CLOSEFIGURE			61)
(define-integrable EMR_FILLPATH			62)
(define-integrable EMR_STROKEANDFILLPATH		63)
(define-integrable EMR_STROKEPATH			64)
(define-integrable EMR_FLATTENPATH			65)
(define-integrable EMR_WIDENPATH			66)
(define-integrable EMR_SELECTCLIPPATH		67)
(define-integrable EMR_ABORTPATH			68)

(define-integrable EMR_GDICOMMENT			70)
(define-integrable EMR_FILLRGN			71)
(define-integrable EMR_FRAMERGN			72)
(define-integrable EMR_INVERTRGN			73)
(define-integrable EMR_PAINTRGN			74)
(define-integrable EMR_EXTSELECTCLIPRGN		75)
(define-integrable EMR_BITBLT			76)
(define-integrable EMR_STRETCHBLT			77)
(define-integrable EMR_MASKBLT			78)
(define-integrable EMR_PLGBLT			79)
(define-integrable EMR_SETDIBITSTODEVICE		80)
(define-integrable EMR_STRETCHDIBITS		81)
(define-integrable EMR_EXTCREATEFONTINDIRECTW	82)
(define-integrable EMR_EXTTEXTOUTA   		83)
(define-integrable EMR_EXTTEXTOUTW   		84)
(define-integrable EMR_POLYBEZIER16		85)
(define-integrable EMR_POLYGON16			86)
(define-integrable EMR_POLYLINE16			87)
(define-integrable EMR_POLYBEZIERTO16		88)
(define-integrable EMR_POLYLINETO16		89)
(define-integrable EMR_POLYPOLYLINE16		90)
(define-integrable EMR_POLYPOLYGON16		91)
(define-integrable EMR_POLYDRAW16			92)
(define-integrable EMR_CREATEMONOBRUSH		93)
(define-integrable EMR_CREATEDIBPATTERNBRUSHPT	94)
(define-integrable EMR_EXTCREATEPEN		95)
(define-integrable EMR_POLYTEXTOUTA   		96)
(define-integrable EMR_POLYTEXTOUTW   		97)

(define-integrable EMR_MIN 1)
(define-integrable EMR_MAX 97)