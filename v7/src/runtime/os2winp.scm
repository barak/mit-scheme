#| -*-Scheme-*-

$Id: os2winp.scm,v 1.9 1995/05/20 10:18:14 cph Exp $

Copyright (c) 1995 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; OS/2 PM Interface -- Primitives
;;; package: (runtime os2-window-primitives)

(declare (usual-integrations))

(define-primitives
  (os2-clipboard-read-text 0)
  (os2-clipboard-write-text 1)
  (os2menu-create 3)
  (os2menu-destroy 1)
  (os2menu-get-item-attributes 4)
  (os2menu-insert-item 7)
  (os2menu-n-items 1)
  (os2menu-nth-item 2)
  (os2menu-remove-item 4)
  (os2menu-set-item-attributes 5)
  (os2pm-synchronize 0)
  (os2ps-bitblt 6)
  (os2ps-clear 5)
  (os2ps-create-bitmap 3)
  (os2ps-create-memory-ps 0)
  (os2ps-destroy-bitmap 1)
  (os2ps-destroy-memory-ps 1)
  (os2ps-draw-point 3)
  (os2ps-get-bitmap 1)
  (os2ps-get-bitmap-bits 5)
  (os2ps-get-bitmap-parameters 1)
  (os2ps-get-font-metrics 1)
  (os2ps-line 3)
  (os2ps-move-graphics-cursor 3)
  (os2ps-poly-line 3)
  (os2ps-poly-line-disjoint 3)
  (os2ps-query-capabilities 3)
  (os2ps-query-capability 2)
  (os2ps-reset-clip-rectangle 1)
  (os2ps-set-bitmap 2)
  (os2ps-set-bitmap-bits 5)
  (os2ps-set-clip-rectangle 5)
  (os2ps-set-colors 3)
  (os2ps-set-font 3)
  (os2ps-set-line-type 2)
  (os2ps-set-mix 2)
  (os2ps-text-width 4)
  (os2ps-write 6)
  (os2win-activate 1)
  (os2win-beep 2)
  (os2win-close 1)
  (os2win-close-event-qid 1)
  (os2win-console-wid 0)
  (os2win-desktop-height 0)
  (os2win-desktop-width 0)
  (os2win-event-ready? 2)
  (os2win-focus? 1)
  (os2win-frame-handle 1)
  (os2win-get-event 2)
  (os2win-get-frame-size 1)
  (os2win-get-pos 1)
  (os2win-get-size 1)
  (os2win-invalidate 5)
  (os2win-move-cursor 3)
  (os2win-open 2)
  (os2win-open-event-qid 0)
  (os2win-ps 1)
  (os2win-scroll 7)
  (os2win-set-grid 3)
  (os2win-set-pos 3)
  (os2win-set-size 3)
  (os2win-set-state 2)
  (os2win-set-title 2)
  (os2win-shape-cursor 4)
  (os2win-show 2)
  (os2win-show-cursor 2)
  (os2win-update-frame 2))

(define-integrable (event-type event) (vector-ref event 0))
(define-integrable (event-wid event) (vector-ref event 1))
(define-integrable (set-event-wid! event wid) (vector-set! event 1 wid))

(define-macro (define-event name type . slots)
  `(BEGIN
     (DEFINE-INTEGRABLE ,(symbol-append 'EVENT-TYPE: name) ,type)
     ,@(let loop ((slots slots) (index 2))
	 (if (null? slots)
	     '()
	     (cons `(DEFINE-INTEGRABLE
		      (,(symbol-append name '-EVENT/ (car slots)) EVENT)
		      (VECTOR-REF EVENT ,index))
		   (loop (cdr slots) (+ index 1)))))))

;; These must match "microcode/pros2pm.c"
(define-event button     0 number type x y flags)
(define-event close      1)
(define-event focus      2 gained?)
(define-event key        3 code flags repeat)
(define-event paint      4 xl xh yl yh)
(define-event resize     5 width height)
(define-event visibility 6 shown?)
(define-event command    7 code)
(define-event help       8 code)

(define-integrable number-of-event-types 9)

(define-integrable button-event-type:down 0)
(define-integrable button-event-type:up 1)
(define-integrable button-event-type:click 2)
(define-integrable button-event-type:double-click 3)

(define-structure (font-metrics (type vector) (conc-name font-metrics/))
  (width #f read-only #t)
  (height #f read-only #t)
  (descender #f read-only #t))

;;; Constants from OS/2 header file "pmwin.h":

(define-integrable CURSOR_SOLID		#x0000)
(define-integrable CURSOR_HALFTONE	#x0001)
(define-integrable CURSOR_FRAME		#x0002)
(define-integrable CURSOR_FLASH		#x0004)

(define-integrable VK_BUTTON1		#x01)
(define-integrable VK_BUTTON2		#x02)
(define-integrable VK_BUTTON3		#x03)
(define-integrable VK_BREAK		#x04)
(define-integrable VK_BACKSPACE		#x05)
(define-integrable VK_TAB		#x06)
(define-integrable VK_BACKTAB		#x07)
(define-integrable VK_NEWLINE		#x08)
(define-integrable VK_SHIFT		#x09)
(define-integrable VK_CTRL		#x0A)
(define-integrable VK_ALT		#x0B)
(define-integrable VK_ALTGRAF		#x0C)
(define-integrable VK_PAUSE		#x0D)
(define-integrable VK_CAPSLOCK		#x0E)
(define-integrable VK_ESC		#x0F)
(define-integrable VK_SPACE		#x10)
(define-integrable VK_PAGEUP		#x11)
(define-integrable VK_PAGEDOWN		#x12)
(define-integrable VK_END		#x13)
(define-integrable VK_HOME		#x14)
(define-integrable VK_LEFT		#x15)
(define-integrable VK_UP		#x16)
(define-integrable VK_RIGHT		#x17)
(define-integrable VK_DOWN		#x18)
(define-integrable VK_PRINTSCRN		#x19)
(define-integrable VK_INSERT		#x1A)
(define-integrable VK_DELETE		#x1B)
(define-integrable VK_SCRLLOCK		#x1C)
(define-integrable VK_NUMLOCK		#x1D)
(define-integrable VK_ENTER		#x1E)
(define-integrable VK_SYSRQ		#x1F)
(define-integrable VK_F1		#x20)
(define-integrable VK_F2		#x21)
(define-integrable VK_F3		#x22)
(define-integrable VK_F4		#x23)
(define-integrable VK_F5		#x24)
(define-integrable VK_F6		#x25)
(define-integrable VK_F7		#x26)
(define-integrable VK_F8		#x27)
(define-integrable VK_F9		#x28)
(define-integrable VK_F10		#x29)
(define-integrable VK_F11		#x2A)
(define-integrable VK_F12		#x2B)
(define-integrable VK_F13		#x2C)
(define-integrable VK_F14		#x2D)
(define-integrable VK_F15		#x2E)
(define-integrable VK_F16		#x2F)
(define-integrable VK_F17		#x30)
(define-integrable VK_F18		#x31)
(define-integrable VK_F19		#x32)
(define-integrable VK_F20		#x33)
(define-integrable VK_F21		#x34)
(define-integrable VK_F22		#x35)
(define-integrable VK_F23		#x36)
(define-integrable VK_F24		#x37)
(define-integrable VK_ENDDRAG		#x38)
(define-integrable VK_CLEAR		#x39)
(define-integrable VK_EREOF		#x3A)
(define-integrable VK_PA1		#x3B)
(define-integrable virtual-key-supremum #x3C)

(define-integrable KC_NONE		#x0000)
(define-integrable KC_CHAR		#x0001)
(define-integrable KC_VIRTUALKEY	#x0002)
(define-integrable KC_SCANCODE		#x0004)
(define-integrable KC_SHIFT		#x0008)
(define-integrable KC_CTRL		#x0010)
(define-integrable KC_ALT		#x0020)
(define-integrable KC_KEYUP		#x0040)
(define-integrable KC_PREVDOWN		#x0080)
(define-integrable KC_LONEKEY		#x0100)
(define-integrable KC_DEADKEY		#x0200)
(define-integrable KC_COMPOSITE		#x0400)
(define-integrable KC_INVALIDCOMP	#x0800)
(define-integrable KC_TOGGLE		#x1000)
(define-integrable KC_INVALIDCHAR	#x2000)

(define-integrable LINETYPE_DEFAULT       0)
(define-integrable LINETYPE_DOT           1)
(define-integrable LINETYPE_SHORTDASH     2)
(define-integrable LINETYPE_DASHDOT       3)
(define-integrable LINETYPE_DOUBLEDOT     4)
(define-integrable LINETYPE_LONGDASH      5)
(define-integrable LINETYPE_DASHDOUBLEDOT 6)
(define-integrable LINETYPE_SOLID         7)
(define-integrable LINETYPE_INVISIBLE     8)
(define-integrable LINETYPE_ALTERNATE     9)

(define-integrable FM_DEFAULT     0)
(define-integrable FM_OR          1)
(define-integrable FM_OVERPAINT   2)
(define-integrable FM_XOR         4)
(define-integrable FM_LEAVEALONE  5)
(define-integrable FM_AND         6)
(define-integrable FM_SUBTRACT    7)
(define-integrable FM_MASKSRCNOT  8)
(define-integrable FM_ZERO        9)
(define-integrable FM_NOTMERGESRC 10)
(define-integrable FM_NOTXORSRC   11)
(define-integrable FM_INVERT      12)
(define-integrable FM_MERGESRCNOT 13)
(define-integrable FM_NOTCOPYSRC  14)
(define-integrable FM_MERGENOTSRC 15)
(define-integrable FM_NOTMASKSRC  16)
(define-integrable FM_ONE         17)

(define-integrable window-state:top        0)
(define-integrable window-state:bottom     1)
(define-integrable window-state:show       2)
(define-integrable window-state:hide       3)
(define-integrable window-state:activate   4)
(define-integrable window-state:deactivate 5)
(define-integrable window-state:minimize   6)
(define-integrable window-state:maximize   7)
(define-integrable window-state:restore    8)

(define-integrable WS_VISIBLE      #x80000000)
(define-integrable WS_DISABLED     #x40000000)
(define-integrable WS_CLIPCHILDREN #x20000000)
(define-integrable WS_CLIPSIBLINGS #x10000000)
(define-integrable WS_PARENTCLIP   #x08000000)
(define-integrable WS_SAVEBITS     #x04000000)
(define-integrable WS_SYNCPAINT    #x02000000)
(define-integrable WS_MINIMIZED    #x01000000)
(define-integrable WS_MAXIMIZED    #x00800000)
(define-integrable WS_ANIMATE      #x00400000)

;; codes for OS2PS-QUERY-CAPABILITIES and OS2PS-QUERY-CAPABILITY
(define-integrable CAPS_FAMILY                     0)
(define-integrable CAPS_IO_CAPS                    1)
(define-integrable CAPS_TECHNOLOGY                 2)
(define-integrable CAPS_DRIVER_VERSION             3)
(define-integrable CAPS_WIDTH                      4) ;pels
(define-integrable CAPS_HEIGHT                     5) ;pels
(define-integrable CAPS_WIDTH_IN_CHARS             6)
(define-integrable CAPS_HEIGHT_IN_CHARS            7)
(define-integrable CAPS_HORIZONTAL_RESOLUTION      8) ;pels per meter
(define-integrable CAPS_VERTICAL_RESOLUTION        9) ;pels per meter
(define-integrable CAPS_CHAR_WIDTH                10) ;pels
(define-integrable CAPS_CHAR_HEIGHT               11) ;pels
(define-integrable CAPS_SMALL_CHAR_WIDTH          12) ;pels
(define-integrable CAPS_SMALL_CHAR_HEIGHT         13) ;pels
(define-integrable CAPS_COLORS                    14)
(define-integrable CAPS_COLOR_PLANES              15)
(define-integrable CAPS_COLOR_BITCOUNT            16)
(define-integrable CAPS_COLOR_TABLE_SUPPORT       17)
(define-integrable CAPS_MOUSE_BUTTONS             18)
(define-integrable CAPS_FOREGROUND_MIX_SUPPORT    19)
(define-integrable CAPS_BACKGROUND_MIX_SUPPORT    20)
(define-integrable CAPS_VIO_LOADABLE_FONTS        21)
(define-integrable CAPS_WINDOW_BYTE_ALIGNMENT     22)
(define-integrable CAPS_BITMAP_FORMATS            23)
(define-integrable CAPS_RASTER_CAPS               24)
(define-integrable CAPS_MARKER_HEIGHT             25) ;pels
(define-integrable CAPS_MARKER_WIDTH              26) ;pels
(define-integrable CAPS_DEVICE_FONTS              27)
(define-integrable CAPS_GRAPHICS_SUBSET           28)
(define-integrable CAPS_GRAPHICS_VERSION          29)
(define-integrable CAPS_GRAPHICS_VECTOR_SUBSET    30)
(define-integrable CAPS_DEVICE_WINDOWING          31)
(define-integrable CAPS_ADDITIONAL_GRAPHICS       32)
(define-integrable CAPS_PHYS_COLORS               33)
(define-integrable CAPS_COLOR_INDEX               34)
(define-integrable CAPS_GRAPHICS_CHAR_WIDTH       35)
(define-integrable CAPS_GRAPHICS_CHAR_HEIGHT      36)
(define-integrable CAPS_HORIZONTAL_FONT_RES       37)
(define-integrable CAPS_VERTICAL_FONT_RES         38)
(define-integrable CAPS_DEVICE_FONT_SIM           39)
(define-integrable CAPS_LINEWIDTH_THICK           40)
(define-integrable CAPS_DEVICE_POLYSET_POINTS     41)

;; Constants for CAPS_IO_CAPS
(define-integrable CAPS_IO_DUMMY       1)
(define-integrable CAPS_IO_SUPPORTS_OP 2)
(define-integrable CAPS_IO_SUPPORTS_IP 3)
(define-integrable CAPS_IO_SUPPORTS_IO 4)

;; Constants for CAPS_TECHNOLOGY
(define-integrable CAPS_TECH_UNKNOWN        0)
(define-integrable CAPS_TECH_VECTOR_PLOTTER 1)
(define-integrable CAPS_TECH_RASTER_DISPLAY 2)
(define-integrable CAPS_TECH_RASTER_PRINTER 3)
(define-integrable CAPS_TECH_RASTER_CAMERA  4)
(define-integrable CAPS_TECH_POSTSCRIPT     5)

;; Constants for CAPS_COLOR_TABLE_SUPPORT
(define-integrable CAPS_COLTABL_RGB_8      #x0001)
(define-integrable CAPS_COLTABL_RGB_8_PLUS #x0002)
(define-integrable CAPS_COLTABL_TRUE_MIX   #x0004)
(define-integrable CAPS_COLTABL_REALIZE    #x0008)

;; Constants for CAPS_FOREGROUND_MIX_SUPPORT
(define-integrable CAPS_FM_OR              #x0001)
(define-integrable CAPS_FM_OVERPAINT       #x0002)
(define-integrable CAPS_FM_XOR             #x0008)
(define-integrable CAPS_FM_LEAVEALONE      #x0010)
(define-integrable CAPS_FM_AND             #x0020)
(define-integrable CAPS_FM_GENERAL_BOOLEAN #x0040)

;; Constants for CAPS_BACKGROUND_MIX_SUPPORT
(define-integrable CAPS_BM_OR              #x0001)
(define-integrable CAPS_BM_OVERPAINT       #x0002)
(define-integrable CAPS_BM_XOR             #x0008)
(define-integrable CAPS_BM_LEAVEALONE      #x0010)
(define-integrable CAPS_BM_AND             #x0020)
(define-integrable CAPS_BM_GENERAL_BOOLEAN #x0040)
(define-integrable CAPS_BM_SRCTRANSPARENT  #x0080)
(define-integrable CAPS_BM_DESTTRANSPARENT #x0100)

;; Constants for CAPS_DEVICE_WINDOWING
(define-integrable CAPS_DEV_WINDOWING_SUPPORT 1)

;; Constants for CAPS_ADDITIONAL_GRAPHICS
(define-integrable CAPS_VDD_DDB_TRANSFER          #x0001)
(define-integrable CAPS_GRAPHICS_KERNING_SUPPORT  #x0002)
(define-integrable CAPS_FONT_OUTLINE_DEFAULT      #x0004)
(define-integrable CAPS_FONT_IMAGE_DEFAULT        #x0008)
;; bits represented by values #x0010 and #x0020 are reserved
(define-integrable CAPS_SCALED_DEFAULT_MARKERS    #x0040)
(define-integrable CAPS_COLOR_CURSOR_SUPPORT      #x0080)
(define-integrable CAPS_PALETTE_MANAGER           #x0100)
(define-integrable CAPS_COSMETIC_WIDELINE_SUPPORT #x0200)
(define-integrable CAPS_DIRECT_FILL               #x0400)
(define-integrable CAPS_REBUILD_FILLS             #x0800)
(define-integrable CAPS_CLIP_FILLS                #x1000)
(define-integrable CAPS_ENHANCED_FONTMETRICS      #x2000)
(define-integrable CAPS_TRANSFORM_SUPPORT         #x4000)

;; Constants for CAPS_WINDOW_BYTE_ALIGNMENT
(define-integrable CAPS_BYTE_ALIGN_REQUIRED     0)
(define-integrable CAPS_BYTE_ALIGN_RECOMMENDED  1)
(define-integrable CAPS_BYTE_ALIGN_NOT_REQUIRED 2)

;; Constants for CAPS_RASTER_CAPS
(define-integrable CAPS_RASTER_BITBLT         #x0001)
(define-integrable CAPS_RASTER_BANDING        #x0002)
(define-integrable CAPS_RASTER_BITBLT_SCALING #x0004)
(define-integrable CAPS_RASTER_SET_PEL        #x0010)
(define-integrable CAPS_RASTER_FONTS          #x0020)
(define-integrable CAPS_RASTER_FLOOD_FILL     #x0040)

;; Constants for OS2PS-BITBLT raster-op argument
(define-integrable ROP_SRCCOPY     #xCC)
(define-integrable ROP_SRCPAINT    #xEE)
(define-integrable ROP_SRCAND      #x88)
(define-integrable ROP_SRCINVERT   #x66)
(define-integrable ROP_SRCERASE    #x44)
(define-integrable ROP_NOTSRCCOPY  #x33)
(define-integrable ROP_NOTSRCERASE #x11)
(define-integrable ROP_MERGECOPY   #xC0)
(define-integrable ROP_MERGEPAINT  #xBB)
(define-integrable ROP_PATCOPY     #xF0)
(define-integrable ROP_PATPAINT    #xFB)
(define-integrable ROP_PATINVERT   #x5A)
(define-integrable ROP_DSTINVERT   #x55)
(define-integrable ROP_ZERO        #x00)
(define-integrable ROP_ONE         #xFF)
   
;; Constants for OS2PS-BITBLT options argument
(define-integrable BBO_OR            0)
(define-integrable BBO_AND           1)
(define-integrable BBO_IGNORE        2)
(define-integrable BBO_PAL_COLORS    4)
(define-integrable BBO_NO_COLOR_INFO 8)

;; Menu item positions:
(define-integrable MIT_END                    #xFFFF)
(define-integrable MIT_NONE                   #xFFFF)
(define-integrable MIT_MEMERROR               #xFFFF)
(define-integrable MIT_ERROR                  #xFFFF)
(define-integrable MIT_FIRST                  #xFFFE)
(define-integrable MIT_LAST                   #xFFFD)

;; Menu item styles:
(define-integrable MIS_TEXT                   #x0001)
(define-integrable MIS_BITMAP                 #x0002)
(define-integrable MIS_SEPARATOR              #x0004)
(define-integrable MIS_OWNERDRAW              #x0008)
(define-integrable MIS_SUBMENU                #x0010)
(define-integrable MIS_MULTMENU               #x0020) ;multiple choice submenu
(define-integrable MIS_SYSCOMMAND             #x0040)
(define-integrable MIS_HELP                   #x0080)
(define-integrable MIS_STATIC                 #x0100)
(define-integrable MIS_BUTTONSEPARATOR        #x0200)
(define-integrable MIS_BREAK                  #x0400)
(define-integrable MIS_BREAKSEPARATOR         #x0800)
(define-integrable MIS_GROUP                  #x1000) ;multiple choice group
;; In multiple choice submenus a style of 'single' denotes the item is
;; a radiobutton.  Absence of this style defaults the item to a
;; checkbox.
(define-integrable MIS_SINGLE                 #x2000)

;; Menu item attributes:
(define-integrable MIA_NODISMISS              #x0020)
(define-integrable MIA_FRAMED                 #x1000)
(define-integrable MIA_CHECKED                #x2000)
(define-integrable MIA_DISABLED               #x4000)
(define-integrable MIA_HILITED                #x8000)

(define-integrable FID_SYSMENU                #x8002)
(define-integrable FID_TITLEBAR               #x8003)
(define-integrable FID_MINMAX                 #x8004)
(define-integrable FID_MENU                   #x8005)
(define-integrable FID_VERTSCROLL             #x8006)
(define-integrable FID_HORZSCROLL             #x8007)
(define-integrable FID_CLIENT                 #x8008)

;; Menu control styles */
(define-integrable MS_ACTIONBAR               #x0001)
(define-integrable MS_TITLEBUTTON             #x0002)
(define-integrable MS_VERTICALFLIP            #x0004)
(define-integrable MS_CONDITIONALCASCADE      #x0040)

;; Frame window styles:
(define-integrable FCF_TITLEBAR               #x00000001)
(define-integrable FCF_SYSMENU                #x00000002)
(define-integrable FCF_MENU                   #x00000004)
(define-integrable FCF_SIZEBORDER             #x00000008)
(define-integrable FCF_MINBUTTON              #x00000010)
(define-integrable FCF_MAXBUTTON              #x00000020)
(define-integrable FCF_MINMAX                 #x00000030)
(define-integrable FCF_VERTSCROLL             #x00000040)
(define-integrable FCF_HORZSCROLL             #x00000080)
(define-integrable FCF_DLGBORDER              #x00000100)
(define-integrable FCF_BORDER                 #x00000200)
(define-integrable FCF_SHELLPOSITION          #x00000400)
(define-integrable FCF_TASKLIST               #x00000800)
(define-integrable FCF_NOBYTEALIGN            #x00001000)
(define-integrable FCF_NOMOVEWITHOWNER        #x00002000)
(define-integrable FCF_ICON                   #x00004000)
(define-integrable FCF_ACCELTABLE             #x00008000)
(define-integrable FCF_SYSMODAL               #x00010000)
(define-integrable FCF_SCREENALIGN            #x00020000)
(define-integrable FCF_MOUSEALIGN             #x00040000)
(define-integrable FCF_HIDEBUTTON             #x01000000)
(define-integrable FCF_HIDEMAX                #x01000020)
(define-integrable FCF_AUTOICON               #x40000000)
(define-integrable FCF_STANDARD               #x0000CC3F)