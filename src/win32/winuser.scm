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

;;Predefined Resource Types
(define-integrable RT_CURSOR           1)
(define-integrable RT_BITMAP           2)
(define-integrable RT_ICON             3)
(define-integrable RT_MENU             4)
(define-integrable RT_DIALOG           5)
(define-integrable RT_STRING           6)
(define-integrable RT_FONTDIR          7)
(define-integrable RT_FONT             8)
(define-integrable RT_ACCELERATOR      9)
(define-integrable RT_RCDATA           10)
(define-integrable RT_MESSAGETABLE     11)

(define-integrable DIFFERENCE          11)
(define-integrable RT_GROUP_CURSOR     (+ RT_CURSOR DIFFERENCE))
(define-integrable RT_GROUP_ICON       (+ RT_ICON DIFFERENCE))
(define-integrable RT_VERSION          16)
(define-integrable RT_DLGINCLUDE       17)

;;Scroll Bar Constants
(define-integrable SB_HORZ             0)
(define-integrable SB_VERT             1)
(define-integrable SB_CTL              2)
(define-integrable SB_BOTH             3)

;;Scroll Bar Commands
(define-integrable SB_LINEUP           0)
(define-integrable SB_LINELEFT         0)
(define-integrable SB_LINEDOWN         1)
(define-integrable SB_LINERIGHT        1)
(define-integrable SB_PAGEUP           2)
(define-integrable SB_PAGELEFT         2)
(define-integrable SB_PAGEDOWN         3)
(define-integrable SB_PAGERIGHT        3)
(define-integrable SB_THUMBPOSITION    4)
(define-integrable SB_THUMBTRACK       5)
(define-integrable SB_TOP              6)
(define-integrable SB_LEFT             6)
(define-integrable SB_BOTTOM           7)
(define-integrable SB_RIGHT            7)
(define-integrable SB_ENDSCROLL        8)

;;ShowWindow Commands
(define-integrable SW_HIDE             0)
(define-integrable SW_SHOWNORMAL       1)
(define-integrable SW_NORMAL           1)
(define-integrable SW_SHOWMINIMIZED    2)
(define-integrable SW_SHOWMAXIMIZED    3)
(define-integrable SW_MAXIMIZE         3)
(define-integrable SW_SHOWNOACTIVATE   4)
(define-integrable SW_SHOW             5)
(define-integrable SW_MINIMIZE         6)
(define-integrable SW_SHOWMINNOACTIVE  7)
(define-integrable SW_SHOWNA           8)
(define-integrable SW_RESTORE          9)
(define-integrable SW_SHOWDEFAULT      10)
(define-integrable SW_MAX              10)

;;Old ShowWindow Commands
(define-integrable HIDE_WINDOW         0)
(define-integrable SHOW_OPENWINDOW     1)
(define-integrable SHOW_ICONWINDOW     2)
(define-integrable SHOW_FULLSCREEN     3)
(define-integrable SHOW_OPENNOACTIVATE 4)

;;Identifiers for the WM_SHOWWINDOW message
(define-integrable SW_PARENTCLOSING    1)
(define-integrable SW_OTHERZOOM        2)
(define-integrable SW_PARENTOPENING    3)
(define-integrable SW_OTHERUNZOOM      4)

;;WM_KEYUP/DOWN/CHAR HIWORD(lParam) flags
(define-integrable KF_EXTENDED         #x0100)
(define-integrable KF_DLGMODE          #x0800)
(define-integrable KF_MENUMODE         #x1000)
(define-integrable KF_ALTDOWN          #x2000)
(define-integrable KF_REPEAT           #x4000)
(define-integrable KF_UP               #x8000)

;;Virtual Keys, Standard Set
(define-integrable VK_LBUTTON        #x01)
(define-integrable VK_RBUTTON        #x02)
(define-integrable VK_CANCEL         #x03)
(define-integrable VK_MBUTTON        #x04) ;NOT contiguous with L & RBUTTON

(define-integrable VK_BACK           #x08)
(define-integrable VK_TAB            #x09)

(define-integrable VK_CLEAR          #x0C)
(define-integrable VK_RETURN         #x0D)

(define-integrable VK_SHIFT          #x10)
(define-integrable VK_CONTROL        #x11)
(define-integrable VK_MENU           #x12)
(define-integrable VK_PAUSE          #x13)
(define-integrable VK_CAPITAL        #x14)

(define-integrable VK_ESCAPE         #x1B)

(define-integrable VK_SPACE          #x20)
(define-integrable VK_PRIOR          #x21)
(define-integrable VK_NEXT           #x22)
(define-integrable VK_END            #x23)
(define-integrable VK_HOME           #x24)
(define-integrable VK_LEFT           #x25)
(define-integrable VK_UP             #x26)
(define-integrable VK_RIGHT          #x27)
(define-integrable VK_DOWN           #x28)
(define-integrable VK_SELECT         #x29)
(define-integrable VK_PRINT          #x2A)
(define-integrable VK_EXECUTE        #x2B)
(define-integrable VK_SNAPSHOT       #x2C)
(define-integrable VK_INSERT         #x2D)
(define-integrable VK_DELETE         #x2E)
(define-integrable VK_HELP           #x2F)

;;VK_0 thru VK_9 are the same as ASCII '0' thru '9' (0x30 - 0x39)
;;VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' (0x41 - 0x5A)

(define-integrable VK_NUMPAD0        #x60)
(define-integrable VK_NUMPAD1        #x61)
(define-integrable VK_NUMPAD2        #x62)
(define-integrable VK_NUMPAD3        #x63)
(define-integrable VK_NUMPAD4        #x64)
(define-integrable VK_NUMPAD5        #x65)
(define-integrable VK_NUMPAD6        #x66)
(define-integrable VK_NUMPAD7        #x67)
(define-integrable VK_NUMPAD8        #x68)
(define-integrable VK_NUMPAD9        #x69)
(define-integrable VK_MULTIPLY       #x6A)
(define-integrable VK_ADD            #x6B)
(define-integrable VK_SEPARATOR      #x6C)
(define-integrable VK_SUBTRACT       #x6D)
(define-integrable VK_DECIMAL        #x6E)
(define-integrable VK_DIVIDE         #x6F)
(define-integrable VK_F1             #x70)
(define-integrable VK_F2             #x71)
(define-integrable VK_F3             #x72)
(define-integrable VK_F4             #x73)
(define-integrable VK_F5             #x74)
(define-integrable VK_F6             #x75)
(define-integrable VK_F7             #x76)
(define-integrable VK_F8             #x77)
(define-integrable VK_F9             #x78)
(define-integrable VK_F10            #x79)
(define-integrable VK_F11            #x7A)
(define-integrable VK_F12            #x7B)
(define-integrable VK_F13            #x7C)
(define-integrable VK_F14            #x7D)
(define-integrable VK_F15            #x7E)
(define-integrable VK_F16            #x7F)
(define-integrable VK_F17            #x80)
(define-integrable VK_F18            #x81)
(define-integrable VK_F19            #x82)
(define-integrable VK_F20            #x83)
(define-integrable VK_F21            #x84)
(define-integrable VK_F22            #x85)
(define-integrable VK_F23            #x86)
(define-integrable VK_F24            #x87)

(define-integrable VK_NUMLOCK        #x90)
(define-integrable VK_SCROLL         #x91)

;; VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
;; Used only as parameters to GetAsyncKeyState and GetKeyState.
;; No other API or message will distinguish left and right keys in this way.
(define-integrable VK_LSHIFT         #xA0)
(define-integrable VK_RSHIFT         #xA1)
(define-integrable VK_LCONTROL       #xA2)
(define-integrable VK_RCONTROL       #xA3)
(define-integrable VK_LMENU          #xA4)
(define-integrable VK_RMENU          #xA5)

(define-integrable VK_ATTN           #xF6)
(define-integrable VK_CRSEL          #xF7)
(define-integrable VK_EXSEL          #xF8)
(define-integrable VK_EREOF          #xF9)
(define-integrable VK_PLAY           #xFA)
(define-integrable VK_ZOOM           #xFB)
(define-integrable VK_NONAME         #xFC)
(define-integrable VK_PA1            #xFD)
(define-integrable VK_OEM_CLEAR      #xFE)

;; SetWindowsHook codes
(define-integrable WH_MIN              -1)
(define-integrable WH_MSGFILTER        -1)
(define-integrable WH_JOURNALRECORD    0)
(define-integrable WH_JOURNALPLAYBACK  1)
(define-integrable WH_KEYBOARD         2)
(define-integrable WH_GETMESSAGE       3)
(define-integrable WH_CALLWNDPROC      4)
(define-integrable WH_CBT              5)
(define-integrable WH_SYSMSGFILTER     6)
(define-integrable WH_MOUSE            7)
(define-integrable WH_HARDWARE         8)
(define-integrable WH_DEBUG            9)
(define-integrable WH_SHELL           10)
(define-integrable WH_FOREGROUNDIDLE  11)
(define-integrable WH_MAX             11)

;; * Hook Codes
(define-integrable HC_ACTION           0)
(define-integrable HC_GETNEXT          1)
(define-integrable HC_SKIP             2)
(define-integrable HC_NOREMOVE         3)
(define-integrable HC_NOREM            HC_NOREMOVE)
(define-integrable HC_SYSMODALON       4)
(define-integrable HC_SYSMODALOFF      5)

;; * CBT Hook Codes
(define-integrable HCBT_MOVESIZE       0)
(define-integrable HCBT_MINMAX         1)
(define-integrable HCBT_QS             2)
(define-integrable HCBT_CREATEWND      3)
(define-integrable HCBT_DESTROYWND     4)
(define-integrable HCBT_ACTIVATE       5)
(define-integrable HCBT_CLICKSKIPPED   6)
(define-integrable HCBT_KEYSKIPPED     7)
(define-integrable HCBT_SYSCOMMAND     8)
(define-integrable HCBT_SETFOCUS       9)

;; * WH_MSGFILTER Filter Proc Codes
(define-integrable MSGF_DIALOGBOX      0)
(define-integrable MSGF_MESSAGEBOX     1)
(define-integrable MSGF_MENU           2)
(define-integrable MSGF_MOVE           3)
(define-integrable MSGF_SIZE           4)
(define-integrable MSGF_SCROLLBAR      5)
(define-integrable MSGF_NEXTWINDOW     6)
(define-integrable MSGF_MAINLOOP       8)
(define-integrable MSGF_MAX            8)
(define-integrable MSGF_USER           4096)

;; * Shell support
(define-integrable HSHELL_WINDOWCREATED       1)
(define-integrable HSHELL_WINDOWDESTROYED     2)
(define-integrable HSHELL_ACTIVATESHELLWINDOW 3)

;; * Window Manager Hook Codes
(define-integrable WC_INIT             1)
(define-integrable WC_SWP              2)
(define-integrable WC_DEFWINDOWPROC    3)
(define-integrable WC_MINMAX           4)
(define-integrable WC_MOVE             5)
(define-integrable WC_SIZE             6)
(define-integrable WC_DRAWCAPTION      7)

;; * Keyboard Layout API
(define-integrable HKL_PREV            0)
(define-integrable HKL_NEXT            1)

(define-integrable KLF_ACTIVATE        #x00000001)
(define-integrable KLF_SUBSTITUTE_OK   #x00000002)
(define-integrable KLF_UNLOADPREVIOUS  #x00000004)
(define-integrable KLF_REORDER         #x00000008)

;; * Size of KeyboardLayoutName (number of characters), including nul terminator
(define-integrable KL_NAMELENGTH       9)

;; * Desktop-specific access flags
(define-integrable DESKTOP_ENUMWINDOWS         #x0001)
(define-integrable DESKTOP_CREATEWINDOW        #x0002)
(define-integrable DESKTOP_CREATEMENU          #x0004)
(define-integrable DESKTOP_HOOKCONTROL         #x0008)
(define-integrable DESKTOP_JOURNALRECORD       #x0010)
(define-integrable DESKTOP_JOURNALPLAYBACK     #x0020)
(define-integrable DESKTOP_ENUMERATE           #x0040)

;; * Windowstation-specific access flags
(define-integrable WINSTA_ENUMDESKTOPS         #x0001)
(define-integrable WINSTA_READATTRIBUTES       #x0002)
(define-integrable WINSTA_ACCESSCLIPBOARD      #x0004)
(define-integrable WINSTA_CREATEDESKTOP        #x0008)
(define-integrable WINSTA_WRITEATTRIBUTES      #x0010)
(define-integrable WINSTA_ACCESSGLOBALATOMS    #x0020)
(define-integrable WINSTA_EXITWINDOWS          #x0040)
(define-integrable WINSTA_ENUMERATE            #x0100)
(define-integrable WINSTA_READSCREEN           #x0200)

;; * window-specific access flags
(define-integrable WIN_ACCESSWINDOW            #x0001)

;; * menu-specific access flags
(define-integrable MENU_ACCESSMENU             #x0001)

;; * Window field offsets for GetWindowLong
(define-integrable GWL_WNDPROC         -4)
(define-integrable GWL_HINSTANCE       -6)
(define-integrable GWL_HWNDPARENT      -8)
(define-integrable GWL_STYLE           -16)
(define-integrable GWL_EXSTYLE         -20)
(define-integrable GWL_USERDATA        -21)
(define-integrable GWL_ID              -12)

;; * Class field offsets for GetClassLong
(define-integrable GCL_MENUNAME        -8)
(define-integrable GCL_HBRBACKGROUND   -10)
(define-integrable GCL_HCURSOR         -12)
(define-integrable GCL_HICON           -14)
(define-integrable GCL_HMODULE         -16)
(define-integrable GCL_CBWNDEXTRA      -18)
(define-integrable GCL_CBCLSEXTRA      -20)
(define-integrable GCL_WNDPROC         -24)
(define-integrable GCL_STYLE           -26)


;; * Window Messages

(define-integrable WM_NULL                         #x0000)
(define-integrable WM_CREATE                       #x0001)
(define-integrable WM_DESTROY                      #x0002)
(define-integrable WM_MOVE                         #x0003)
(define-integrable WM_SIZE                         #x0005)

(define-integrable WM_ACTIVATE                     #x0006)
;; * WM_ACTIVATE state values
(define-integrable     WA_INACTIVE     0)
(define-integrable     WA_ACTIVE       1)
(define-integrable     WA_CLICKACTIVE  2)

(define-integrable WM_SETFOCUS                     #x0007)
(define-integrable WM_KILLFOCUS                    #x0008)
(define-integrable WM_ENABLE                       #x000A)
(define-integrable WM_SETREDRAW                    #x000B)
(define-integrable WM_SETTEXT                      #x000C)
(define-integrable WM_GETTEXT                      #x000D)
(define-integrable WM_GETTEXTLENGTH                #x000E)
(define-integrable WM_PAINT                        #x000F)
(define-integrable WM_CLOSE                        #x0010)
(define-integrable WM_QUERYENDSESSION              #x0011)
(define-integrable WM_QUIT                         #x0012)
(define-integrable WM_QUERYOPEN                    #x0013)
(define-integrable WM_ERASEBKGND                   #x0014)
(define-integrable WM_SYSCOLORCHANGE               #x0015)
(define-integrable WM_ENDSESSION                   #x0016)
(define-integrable WM_SHOWWINDOW                   #x0018)
(define-integrable WM_WININICHANGE                 #x001A)
(define-integrable WM_DEVMODECHANGE                #x001B)
(define-integrable WM_ACTIVATEAPP                  #x001C)
(define-integrable WM_FONTCHANGE                   #x001D)
(define-integrable WM_TIMECHANGE                   #x001E)
(define-integrable WM_CANCELMODE                   #x001F)
(define-integrable WM_SETCURSOR                    #x0020)
(define-integrable WM_MOUSEACTIVATE                #x0021)
(define-integrable WM_CHILDACTIVATE                #x0022)
(define-integrable WM_QUEUESYNC                    #x0023)

(define-integrable WM_GETMINMAXINFO                #x0024)
;; * STRUCT POINTED TO BY WM_GETMINMAXINFO LPARAM

(define-integrable WM_PAINTICON                    #x0026)
(define-integrable WM_ICONERASEBKGND               #x0027)
(define-integrable WM_NEXTDLGCTL                   #x0028)
(define-integrable WM_SPOOLERSTATUS                #x002A)
(define-integrable WM_DRAWITEM                     #x002B)
(define-integrable WM_MEASUREITEM                  #x002C)
(define-integrable WM_DELETEITEM                   #x002D)
(define-integrable WM_VKEYTOITEM                   #x002E)
(define-integrable WM_CHARTOITEM                   #x002F)
(define-integrable WM_SETFONT                      #x0030)
(define-integrable WM_GETFONT                      #x0031)
(define-integrable WM_SETHOTKEY                    #x0032)
(define-integrable WM_GETHOTKEY                    #x0033)
(define-integrable WM_QUERYDRAGICON                #x0037)
(define-integrable WM_COMPAREITEM                  #x0039)
(define-integrable WM_COMPACTING                   #x0041)
(define-integrable WM_OTHERWINDOWCREATED           #x0042) ;  // NO LONGER SUPORTED
(define-integrable WM_OTHERWINDOWDESTROYED         #x0043) ;  // NO LONGER SUPORTED
(define-integrable WM_COMMNOTIFY                   #x0044) ;  // NO LONGER SUPORTED
(define-integrable WM_HOTKEYEVENT                  #x0045)
(define-integrable WM_WINDOWPOSCHANGING            #x0046)
(define-integrable WM_WINDOWPOSCHANGED             #x0047)

(define-integrable WM_POWER                        #x0048)

;; * WPARAM FOR WM_POWER WINDOW MESSAGE AND DRV_POWER DRIVER NOTIFICATION
(define-integrable PWR_OK              1)
(define-integrable PWR_FAIL           -1)
(define-integrable PWR_SUSPENDREQUEST  1)
(define-integrable PWR_SUSPENDRESUME   2)
(define-integrable PWR_CRITICALRESUME  3)

(define-integrable WM_COPYDATA                     #x004A)
(define-integrable WM_CANCELJOURNAL                #x004B)

(define-integrable WM_NCCREATE                     #x0081)
(define-integrable WM_NCDESTROY                    #x0082)
(define-integrable WM_NCCALCSIZE                   #x0083)
(define-integrable WM_NCHITTEST                    #x0084)
(define-integrable WM_NCPAINT                      #x0085)
(define-integrable WM_NCACTIVATE                   #x0086)
(define-integrable WM_GETDLGCODE                   #x0087)
(define-integrable WM_NCMOUSEMOVE                  #x00A0)
(define-integrable WM_NCLBUTTONDOWN                #x00A1)
(define-integrable WM_NCLBUTTONUP                  #x00A2)
(define-integrable WM_NCLBUTTONDBLCLK              #x00A3)
(define-integrable WM_NCRBUTTONDOWN                #x00A4)
(define-integrable WM_NCRBUTTONUP                  #x00A5)
(define-integrable WM_NCRBUTTONDBLCLK              #x00A6)
(define-integrable WM_NCMBUTTONDOWN                #x00A7)
(define-integrable WM_NCMBUTTONUP                  #x00A8)
(define-integrable WM_NCMBUTTONDBLCLK              #x00A9)

(define-integrable WM_KEYFIRST                     #x0100)
(define-integrable WM_KEYDOWN                      #x0100)
(define-integrable WM_KEYUP                        #x0101)
(define-integrable WM_CHAR                         #x0102)
(define-integrable WM_DEADCHAR                     #x0103)
(define-integrable WM_SYSKEYDOWN                   #x0104)
(define-integrable WM_SYSKEYUP                     #x0105)
(define-integrable WM_SYSCHAR                      #x0106)
(define-integrable WM_SYSDEADCHAR                  #x0107)
(define-integrable WM_KEYLAST                      #x0108)
(define-integrable WM_INITDIALOG                   #x0110)
(define-integrable WM_COMMAND                      #x0111)
(define-integrable WM_SYSCOMMAND                   #x0112)
(define-integrable WM_TIMER                        #x0113)
(define-integrable WM_HSCROLL                      #x0114)
(define-integrable WM_VSCROLL                      #x0115)
(define-integrable WM_INITMENU                     #x0116)
(define-integrable WM_INITMENUPOPUP                #x0117)
(define-integrable WM_MENUSELECT                   #x011F)
(define-integrable WM_MENUCHAR                     #x0120)
(define-integrable WM_ENTERIDLE                    #x0121)

(define-integrable WM_CTLCOLORMSGBOX               #x0132)
(define-integrable WM_CTLCOLOREDIT                 #x0133)
(define-integrable WM_CTLCOLORLISTBOX              #x0134)
(define-integrable WM_CTLCOLORBTN                  #x0135)
(define-integrable WM_CTLCOLORDLG                  #x0136)
(define-integrable WM_CTLCOLORSCROLLBAR            #x0137)
(define-integrable WM_CTLCOLORSTATIC               #x0138)

(define-integrable WM_MOUSEFIRST                   #x0200)
(define-integrable WM_MOUSEMOVE                    #x0200)
(define-integrable WM_LBUTTONDOWN                  #x0201)
(define-integrable WM_LBUTTONUP                    #x0202)
(define-integrable WM_LBUTTONDBLCLK                #x0203)
(define-integrable WM_RBUTTONDOWN                  #x0204)
(define-integrable WM_RBUTTONUP                    #x0205)
(define-integrable WM_RBUTTONDBLCLK                #x0206)
(define-integrable WM_MBUTTONDOWN                  #x0207)
(define-integrable WM_MBUTTONUP                    #x0208)
(define-integrable WM_MBUTTONDBLCLK                #x0209)
(define-integrable WM_MOUSELAST                    #x0209)

(define-integrable WM_PARENTNOTIFY                 #x0210)
(define-integrable WM_ENTERMENULOOP                #x0211)
(define-integrable WM_EXITMENULOOP                 #x0212)
(define-integrable WM_MDICREATE                    #x0220)
(define-integrable WM_MDIDESTROY                   #x0221)
(define-integrable WM_MDIACTIVATE                  #x0222)
(define-integrable WM_MDIRESTORE                   #x0223)
(define-integrable WM_MDINEXT                      #x0224)
(define-integrable WM_MDIMAXIMIZE                  #x0225)
(define-integrable WM_MDITILE                      #x0226)
(define-integrable WM_MDICASCADE                   #x0227)
(define-integrable WM_MDIICONARRANGE               #x0228)
(define-integrable WM_MDIGETACTIVE                 #x0229)
(define-integrable WM_MDISETMENU                   #x0230)
(define-integrable WM_DROPFILES                    #x0233)
(define-integrable WM_MDIREFRESHMENU               #x0234)

(define-integrable WM_CUT                          #x0300)
(define-integrable WM_COPY                         #x0301)
(define-integrable WM_PASTE                        #x0302)
(define-integrable WM_CLEAR                        #x0303)
(define-integrable WM_UNDO                         #x0304)
(define-integrable WM_RENDERFORMAT                 #x0305)
(define-integrable WM_RENDERALLFORMATS             #x0306)
(define-integrable WM_DESTROYCLIPBOARD             #x0307)
(define-integrable WM_DRAWCLIPBOARD                #x0308)
(define-integrable WM_PAINTCLIPBOARD               #x0309)
(define-integrable WM_VSCROLLCLIPBOARD             #x030A)
(define-integrable WM_SIZECLIPBOARD                #x030B)
(define-integrable WM_ASKCBFORMATNAME              #x030C)
(define-integrable WM_CHANGECBCHAIN                #x030D)
(define-integrable WM_HSCROLLCLIPBOARD             #x030E)
(define-integrable WM_QUERYNEWPALETTE              #x030F)
(define-integrable WM_PALETTEISCHANGING            #x0310)
(define-integrable WM_PALETTECHANGED               #x0311)
(define-integrable WM_HOTKEY                       #x0312)

(define-integrable WM_PENWINFIRST                  #x0380)
(define-integrable WM_PENWINLAST                   #x038F)




;; * NOTE: ALL MESSAGE NUMBERS BELOW 0X0400 ARE RESERVED.

;; * PRIVATE WINDOW MESSAGES START HERE:

(define-integrable WM_USER                         #x0400)

;; * WM_SYNCTASK COMMANDS
(define-integrable ST_BEGINSWP         0)
(define-integrable ST_ENDSWP           1)

;; * WM_NCHITTEST AND MOUSEHOOKSTRUCT MOUSE POSITION CODES
(define-integrable HTERROR             -2)
(define-integrable HTTRANSPARENT       -1)
(define-integrable HTNOWHERE           0)
(define-integrable HTCLIENT            1)
(define-integrable HTCAPTION           2)
(define-integrable HTSYSMENU           3)
(define-integrable HTGROWBOX           4)
(define-integrable HTSIZE              4)
(define-integrable HTMENU              5)
(define-integrable HTHSCROLL           6)
(define-integrable HTVSCROLL           7)
(define-integrable HTMINBUTTON         8)
(define-integrable HTMAXBUTTON         9)
(define-integrable HTLEFT              10)
(define-integrable HTRIGHT             11)
(define-integrable HTTOP               12)
(define-integrable HTTOPLEFT           13)
(define-integrable HTTOPRIGHT          14)
(define-integrable HTBOTTOM            15)
(define-integrable HTBOTTOMLEFT        16)
(define-integrable HTBOTTOMRIGHT       17)
(define-integrable HTBORDER            18)
(define-integrable HTREDUCE            8)
(define-integrable HTZOOM              9)
(define-integrable HTSIZEFIRST         10)
(define-integrable HTSIZELAST          17)

;; * SENDMESSAGETIMEOUT VALUES
(define-integrable SMTO_NORMAL         #x0000)
(define-integrable SMTO_BLOCK          #x0001)
(define-integrable SMTO_ABORTIFHUNG    #x0002)

;; * WM_MOUSEACTIVATE RETURN CODES
(define-integrable MA_ACTIVATE         1)
(define-integrable MA_ACTIVATEANDEAT   2)
(define-integrable MA_NOACTIVATE       3)
(define-integrable MA_NOACTIVATEANDEAT 4)

;; * WM_SIZE MESSAGE WPARAM VALUES
(define-integrable SIZE_RESTORED       0)
(define-integrable SIZE_MINIMIZED      1)
(define-integrable SIZE_MAXIMIZED      2)
(define-integrable SIZE_MAXSHOW        3)
(define-integrable SIZE_MAXHIDE        4)

;; * OBSOLETE CONSTANT NAMES
(define-integrable SIZENORMAL          0)
(define-integrable SIZEICONIC          1)
(define-integrable SIZEFULLSCREEN      2)
(define-integrable SIZEZOOMSHOW        3)
(define-integrable SIZEZOOMHIDE        4)

;; * WM_NCCALCSIZE "WINDOW VALID RECT" RETURN VALUES
(define-integrable WVR_ALIGNTOP        #x0010)
(define-integrable WVR_ALIGNLEFT       #x0020)
(define-integrable WVR_ALIGNBOTTOM     #x0040)
(define-integrable WVR_ALIGNRIGHT      #x0080)
(define-integrable WVR_HREDRAW         #x0100)
(define-integrable WVR_VREDRAW         #x0200)
(define-integrable WVR_REDRAW          (+ WVR_HREDRAW
                                          WVR_VREDRAW))
(define-integrable WVR_VALIDRECTS      #x0400)

;; * KEY STATE MASKS FOR MOUSE MESSAGES
(define-integrable MK_LBUTTON          #x0001)
(define-integrable MK_RBUTTON          #x0002)
(define-integrable MK_SHIFT            #x0004)
(define-integrable MK_CONTROL          #x0008)
(define-integrable MK_MBUTTON          #x0010)

;; * WINDOW STYLES
(define-integrable WS_OVERLAPPED       #x00000000)
(define-integrable WS_POPUP            #x80000000)
(define-integrable WS_CHILD            #x40000000)
(define-integrable WS_MINIMIZE         #x20000000)
(define-integrable WS_VISIBLE          #x10000000)
(define-integrable WS_DISABLED         #x08000000)
(define-integrable WS_CLIPSIBLINGS     #x04000000)
(define-integrable WS_CLIPCHILDREN     #x02000000)
(define-integrable WS_MAXIMIZE         #x01000000)
(define-integrable WS_CAPTION          #x00C00000)
(define-integrable WS_BORDER           #x00800000)
(define-integrable WS_DLGFRAME         #x00400000)
(define-integrable WS_VSCROLL          #x00200000)
(define-integrable WS_HSCROLL          #x00100000)
(define-integrable WS_SYSMENU          #x00080000)
(define-integrable WS_THICKFRAME       #x00040000)
(define-integrable WS_GROUP            #x00020000)
(define-integrable WS_TABSTOP          #x00010000)

(define-integrable WS_MINIMIZEBOX      #x00020000)
(define-integrable WS_MAXIMIZEBOX      #x00010000)

(define-integrable WS_TILED            WS_OVERLAPPED)
(define-integrable WS_ICONIC           WS_MINIMIZE)
(define-integrable WS_SIZEBOX          WS_THICKFRAME)
(define-integrable WS_TILEDWINDOW      (+ WS_OVERLAPPED WS_CAPTION WS_SYSMENU WS_THICKFRAME WS_MINIMIZEBOX WS_MAXIMIZEBOX))

;; * COMMON WINDOW STYLES
(define-integrable WS_OVERLAPPEDWINDOW (+ WS_OVERLAPPED 
                                          WS_CAPTION
                                          WS_SYSMENU
                                          WS_THICKFRAME
                                          WS_MINIMIZEBOX
                                          WS_MAXIMIZEBOX))

(define-integrable WS_POPUPWINDOW      (+ WS_POPUP
                                          WS_BORDER
                                          WS_SYSMENU))

(define-integrable WS_CHILDWINDOW      WS_CHILD)

;; * EXTENDED WINDOW STYLES
(define-integrable WS_EX_DLGMODALFRAME  #x00000001)
(define-integrable WS_EX_NOPARENTNOTIFY #x00000004)
(define-integrable WS_EX_TOPMOST        #x00000008)
(define-integrable WS_EX_ACCEPTFILES    #x00000010)
(define-integrable WS_EX_TRANSPARENT    #x00000020)

;; * CLASS STYLES
(define-integrable CS_VREDRAW          #x0001)
(define-integrable CS_HREDRAW          #x0002)
(define-integrable CS_KEYCVTWINDOW     #x0004)
(define-integrable CS_DBLCLKS          #x0008)
(define-integrable CS_OWNDC            #x0020)
(define-integrable CS_CLASSDC          #x0040)
(define-integrable CS_PARENTDC         #x0080)
(define-integrable CS_NOKEYCVT         #x0100)
(define-integrable CS_NOCLOSE          #x0200)
(define-integrable CS_SAVEBITS         #x0800)
(define-integrable CS_BYTEALIGNCLIENT  #X1000)
(define-integrable CS_BYTEALIGNWINDOW  #X2000)
(define-integrable CS_GLOBALCLASS      #X4000)

;; * PREDEFINED CLIPBOARD FORMATS
(define-integrable CF_TEXT             1)
(define-integrable CF_BITMAP           2)
(define-integrable CF_METAFILEPICT     3)
(define-integrable CF_SYLK             4)
(define-integrable CF_DIF              5)
(define-integrable CF_TIFF             6)
(define-integrable CF_OEMTEXT          7)
(define-integrable CF_DIB              8)
(define-integrable CF_PALETTE          9)
(define-integrable CF_PENDATA          10)
(define-integrable CF_RIFF             11)
(define-integrable CF_WAVE             12)
(define-integrable CF_UNICODETEXT      13)
(define-integrable CF_ENHMETAFILE      14)

(define-integrable CF_OWNERDISPLAY     #x0080)
(define-integrable CF_DSPTEXT          #x0081)
(define-integrable CF_DSPBITMAP        #x0082)
(define-integrable CF_DSPMETAFILEPICT  #x0083)
(define-integrable CF_DSPENHMETAFILE   #x008E)

;; * "PRIVATE" FORMATS DON'T GET GLOBALFREE'D
(define-integrable CF_PRIVATEFIRST     #x0200)
(define-integrable CF_PRIVATELAST      #x02FF)

;; * "GDIOBJ" FORMATS DO GET DELETEOBJECT'D
(define-integrable CF_GDIOBJFIRST      #x0300)
(define-integrable CF_GDIOBJLAST       #x03FF)

;; * DEFINES FOR THE FVIRT FIELD OF THE ACCELERATOR TABLE STRUCTURE.
(define-integrable FVIRTKEY  #t)
(define-integrable FNOINVERT #x02)
(define-integrable FSHIFT    #x04)
(define-integrable FCONTROL  #x08)
(define-integrable FALT      #X10)

(define-integrable WPF_SETMINPOSITION      #x0001)
(define-integrable WPF_RESTORETOMAXIMIZED  #x0002)

;; * OWNER DRAW CONTROL TYPES
(define-integrable ODT_MENU        1)
(define-integrable ODT_LISTBOX     2)
(define-integrable ODT_COMBOBOX    3)
(define-integrable ODT_BUTTON      4)

;; * OWNER DRAW ACTIONS
(define-integrable ODA_DRAWENTIRE  #x0001)
(define-integrable ODA_SELECT      #x0002)
(define-integrable ODA_FOCUS       #x0004)

;; * OWNER DRAW STATE
(define-integrable ODS_SELECTED    #x0001)
(define-integrable ODS_GRAYED      #x0002)
(define-integrable ODS_DISABLED    #x0004)
(define-integrable ODS_CHECKED     #x0008)
(define-integrable ODS_FOCUS       #x0010)

;; * PEEKMESSAGE OPTIONS
(define-integrable PM_NOREMOVE         #x0000)
(define-integrable PM_REMOVE           #x0001)
(define-integrable PM_NOYIELD          #x0002)

(define-integrable MOD_ALT         #x0001)
(define-integrable MOD_CONTROL     #x0002)
(define-integrable MOD_SHIFT       #x0004)

(define-integrable IDHOT_SNAPWINDOW  -1)   ;SHIFT-PRINTSCRN
(define-integrable IDHOT_SNAPDESKTOP -2)   ;PRINTSCRN

(define-integrable EWX_LOGOFF   0)
(define-integrable EWX_SHUTDOWN 1)
(define-integrable EWX_REBOOT   2)
(define-integrable EWX_FORCE    4)

;; * SPECIAL HWND VALUE FOR USE WITH POSTMESSAGE AND SENDMESSAGE
(define-integrable HWND_BROADCAST  #xFFFF) ;  ((HWND)0XFFFF)

(define-integrable CW_USEDEFAULT       #x80000000) ; ((UINT)0X80000000)

;; * SPECIAL VALUE FOR CREATEWINDOW, ET AL.
(define-integrable HWND_DESKTOP        0)          ; ((HWND)0)

;; * SetWindowPos Flags
(define-integrable SWP_NOSIZE          #x0001)
(define-integrable SWP_NOMOVE          #x0002)
(define-integrable SWP_NOZORDER        #x0004)
(define-integrable SWP_NOREDRAW        #x0008)
(define-integrable SWP_NOACTIVATE      #x0010)
(define-integrable SWP_FRAMECHANGED    #x0020) ;The frame changed: send WM_NCCALCSIZE
(define-integrable SWP_SHOWWINDOW      #x0040)
(define-integrable SWP_HIDEWINDOW      #x0080)
(define-integrable SWP_NOCOPYBITS      #x0100)
(define-integrable SWP_NOOWNERZORDER   #x0200) ;Don't do owner Z ordering

(define-integrable SWP_DRAWFRAME       SWP_FRAMECHANGED)
(define-integrable SWP_NOREPOSITION    SWP_NOOWNERZORDER)

(define-integrable HWND_TOP        0)  ;((HWND)0)
(define-integrable HWND_BOTTOM     1)  ;((HWND)1)
(define-integrable HWND_TOPMOST    -1)  ;((HWND)-1)
(define-integrable HWND_NOTOPMOST  -2)  ;((HWND)-2)

;; * Window extra byted needed for private dialog classes.
(define DLGWINDOWEXTRA 30)

(define-integrable MOUSEEVENTF_MOVE        #x0001) ;  // mouse move
(define-integrable MOUSEEVENTF_LEFTDOWN    #x0002) ;  // left button down
(define-integrable MOUSEEVENTF_LEFTUP      #x0004) ;  // left button up
(define-integrable MOUSEEVENTF_RIGHTDOWN   #x0008) ;  // right button down
(define-integrable MOUSEEVENTF_RIGHTUP     #x0010) ;  // right button up
(define-integrable MOUSEEVENTF_MIDDLEDOWN  #x0020) ;  // middle button down
(define-integrable MOUSEEVENTF_MIDDLEUP    #x0040) ;  // middle button up
(define-integrable MOUSEEVENTF_ABSOLUTE    #x8000) ;  // absolute move

;; * Queue status flags for GetQueueStatus and MsgWaitForMultipleObjects
(define-integrable QS_KEY           #x01)
(define-integrable QS_MOUSEMOVE     #x02)
(define-integrable QS_MOUSEBUTTON   #x04)
(define-integrable QS_POSTMESSAGE   #x08)
(define-integrable QS_TIMER         #x10)
(define-integrable QS_PAINT         #x20)
(define-integrable QS_SENDMESSAGE   #x40)
(define-integrable QS_HOTKEY        #x80)

(define-integrable QS_MOUSE        (+ QS_MOUSEMOVE
                                      QS_MOUSEBUTTON))

(define-integrable QS_INPUT        (+ QS_MOUSE
                                      QS_KEY))

(define-integrable QS_ALLEVENTS    (+ QS_INPUT
                                      QS_POSTMESSAGE
                                      QS_TIMER
                                      QS_PAINT
                                      QS_HOTKEY))

;; * GetSysInputMode return values
(define-integrable IMD_NONE                0)
(define-integrable IMD_MENU                1)
(define-integrable IMD_DIALOGBOX           2)
(define-integrable IMD_NEXTWINDOW          3)
(define-integrable IMD_SCROLLBAR           4)
(define-integrable IMD_TITLEBUTTONTRACK    5)
(define-integrable IMD_MOVESIZETRACK       6)
(define-integrable IMD_SYSERRDLG           7)
(define-integrable IMD_DRAGOBJECT          8)
(define-integrable IMD_DRAGDETECT          9)

;; * GetSystemMetrics codes
(define-integrable SM_CXSCREEN         0)
(define-integrable SM_CYSCREEN         1)
(define-integrable SM_CXVSCROLL        2)
(define-integrable SM_CYHSCROLL        3)
(define-integrable SM_CYCAPTION        4)
(define-integrable SM_CXBORDER         5)
(define-integrable SM_CYBORDER         6)
(define-integrable SM_CXDLGFRAME       7)
(define-integrable SM_CYDLGFRAME       8)
(define-integrable SM_CYVTHUMB         9)
(define-integrable SM_CXHTHUMB         10)
(define-integrable SM_CXICON           11)
(define-integrable SM_CYICON           12)
(define-integrable SM_CXCURSOR         13)
(define-integrable SM_CYCURSOR         14)
(define-integrable SM_CYMENU           15)
(define-integrable SM_CXFULLSCREEN     16)
(define-integrable SM_CYFULLSCREEN     17)
(define-integrable SM_CYKANJIWINDOW    18)
(define-integrable SM_MOUSEPRESENT     19)
(define-integrable SM_CYVSCROLL        20)
(define-integrable SM_CXHSCROLL        21)
(define-integrable SM_DEBUG            22)
(define-integrable SM_SWAPBUTTON       23)
(define-integrable SM_RESERVED1        24)
(define-integrable SM_RESERVED2        25)
(define-integrable SM_RESERVED3        26)
(define-integrable SM_RESERVED4        27)
(define-integrable SM_CXMIN            28)
(define-integrable SM_CYMIN            29)
(define-integrable SM_CXSIZE           30)
(define-integrable SM_CYSIZE           31)
(define-integrable SM_CXFRAME          32)
(define-integrable SM_CYFRAME          33)
(define-integrable SM_CXMINTRACK       34)
(define-integrable SM_CYMINTRACK       35)
(define-integrable SM_CXDOUBLECLK       36)
(define-integrable SM_CYDOUBLECLK       37)
(define-integrable SM_CXICONSPACING     38)
(define-integrable SM_CYICONSPACING     39)
(define-integrable SM_MENUDROPALIGNMENT 40)
(define-integrable SM_PENWINDOWS        41)
(define-integrable SM_DBCSENABLED       42)
(define-integrable SM_CMOUSEBUTTONS     43)
(define-integrable SM_CMETRICS          44)

;; * Flags for TrackPopupMenu
(define-integrable TPM_LEFTBUTTON  #x0000)
(define-integrable TPM_RIGHTBUTTON #x0002)
(define-integrable TPM_LEFTALIGN   #x0000)
(define-integrable TPM_CENTERALIGN #x0004)
(define-integrable TPM_RIGHTALIGN  #x0008)

;; * DrawText Format Flags
(define-integrable DT_TOP              #x0000)
(define-integrable DT_LEFT             #x0000)
(define-integrable DT_CENTER           #x0001)
(define-integrable DT_RIGHT            #x0002)
(define-integrable DT_VCENTER          #x0004)
(define-integrable DT_BOTTOM           #x0008)
(define-integrable DT_WORDBREAK        #x0010)
(define-integrable DT_SINGLELINE       #x0020)
(define-integrable DT_EXPANDTABS       #x0040)
(define-integrable DT_TABSTOP          #x0080)
(define-integrable DT_NOCLIP           #x0100)
(define-integrable DT_EXTERNALLEADING  #x0200)
(define-integrable DT_CALCRECT         #x0400)
(define-integrable DT_NOPREFIX         #x0800)
(define-integrable DT_INTERNAL         #x1000)

;; * GetDCEx flags
(define-integrable DCX_WINDOW           #x00000001)
(define-integrable DCX_CACHE            #x00000002)
(define-integrable DCX_NORESETATTRS     #x00000004)
(define-integrable DCX_CLIPCHILDREN     #x00000008)
(define-integrable DCX_CLIPSIBLINGS     #x00000010)
(define-integrable DCX_PARENTCLIP       #x00000020)

(define-integrable DCX_EXCLUDERGN       #x00000040)
(define-integrable DCX_INTERSECTRGN     #x00000080)

(define-integrable DCX_EXCLUDEUPDATE    #x00000100)
(define-integrable DCX_INTERSECTUPDATE  #x00000200)

(define-integrable DCX_LOCKWINDOWUPDATE #x00000400)

(define-integrable DCX_NORECOMPUTE      #x00100000)
(define-integrable DCX_VALIDATE         #x00200000)

;; * RedrawWindow flags
(define-integrable RDW_INVALIDATE          #x0001)
(define-integrable RDW_INTERNALPAINT       #x0002)
(define-integrable RDW_ERASE               #x0004)

(define-integrable RDW_VALIDATE            #x0008)
(define-integrable RDW_NOINTERNALPAINT     #x0010)
(define-integrable RDW_NOERASE             #x0020)

(define-integrable RDW_NOCHILDREN          #x0040)
(define-integrable RDW_ALLCHILDREN         #x0080)

(define-integrable RDW_UPDATENOW           #x0100)
(define-integrable RDW_ERASENOW            #x0200)

(define-integrable RDW_FRAME               #x0400)
(define-integrable RDW_NOFRAME             #x0800)

;; * LockWindowUpdate API
(define-integrable SW_SCROLLCHILDREN   #x0001) ;Scroll children within *lprcScroll.
(define-integrable SW_INVALIDATE       #x0002) ;Invalidate after scrolling
(define-integrable SW_ERASE            #x0004) ;If SW_INVALIDATE, don't send WM_ERASEBACKGROUND

;; * EnableScrollBar flags
(define-integrable ESB_ENABLE_BOTH     #x0000)
(define-integrable ESB_DISABLE_BOTH    #x0003)

(define-integrable ESB_DISABLE_LEFT    #x0001)
(define-integrable ESB_DISABLE_RIGHT   #x0002)

(define-integrable ESB_DISABLE_UP      #x0001)
(define-integrable ESB_DISABLE_DOWN    #x0002)

(define-integrable ESB_DISABLE_LTUP    ESB_DISABLE_LEFT)
(define-integrable ESB_DISABLE_RTDN    ESB_DISABLE_RIGHT)

;; * MessageBox Flags
(define-integrable MB_OK                       #x00000000)
(define-integrable MB_OKCANCEL                 #x00000001)
(define-integrable MB_ABORTRETRYIGNORE         #x00000002)
(define-integrable MB_YESNOCANCEL              #x00000003)
(define-integrable MB_YESNO                    #x00000004)
(define-integrable MB_RETRYCANCEL              #x00000005)

(define-integrable MB_ICONHAND                 #x00000010)
(define-integrable MB_ICONQUESTION             #x00000020)
(define-integrable MB_ICONEXCLAMATION          #x00000030)
(define-integrable MB_ICONASTERISK             #x00000040)

(define-integrable MB_ICONINFORMATION          MB_ICONASTERISK)
(define-integrable MB_ICONSTOP                 MB_ICONHAND)

(define-integrable MB_DEFBUTTON1               #x00000000)
(define-integrable MB_DEFBUTTON2               #x00000100)
(define-integrable MB_DEFBUTTON3               #x00000200)

(define-integrable MB_APPLMODAL                #x00000000)
(define-integrable MB_SYSTEMMODAL              #x00001000)
(define-integrable MB_TASKMODAL                #x00002000)

(define-integrable MB_NOFOCUS                  #x00008000)
(define-integrable MB_SETFOREGROUND            #x00010000)
(define-integrable MB_DEFAULT_DESKTOP_ONLY     #x00020000)

(define-integrable MB_TYPEMASK                 #x0000000F)
(define-integrable MB_ICONMASK                 #x000000F0)
(define-integrable MB_DEFMASK                  #x00000F00)
(define-integrable MB_MODEMASK                 #x00003000)
(define-integrable MB_MISCMASK                 #x0000C000)

;; * Color Types
(define-integrable CTLCOLOR_MSGBOX         0)
(define-integrable CTLCOLOR_EDIT           1)
(define-integrable CTLCOLOR_LISTBOX        2)
(define-integrable CTLCOLOR_BTN            3)
(define-integrable CTLCOLOR_DLG            4)
(define-integrable CTLCOLOR_SCROLLBAR      5)
(define-integrable CTLCOLOR_STATIC         6)
(define-integrable CTLCOLOR_MAX            8)  ;three bits max

(define-integrable COLOR_SCROLLBAR         0)
(define-integrable COLOR_BACKGROUND        1)
(define-integrable COLOR_ACTIVECAPTION     2)
(define-integrable COLOR_INACTIVECAPTION   3)
(define-integrable COLOR_MENU              4)
(define-integrable COLOR_WINDOW            5)
(define-integrable COLOR_WINDOWFRAME       6)
(define-integrable COLOR_MENUTEXT          7)
(define-integrable COLOR_WINDOWTEXT        8)
(define-integrable COLOR_CAPTIONTEXT       9)
(define-integrable COLOR_ACTIVEBORDER      10)
(define-integrable COLOR_INACTIVEBORDER    11)
(define-integrable COLOR_APPWORKSPACE      12)
(define-integrable COLOR_HIGHLIGHT         13)
(define-integrable COLOR_HIGHLIGHTTEXT     14)
(define-integrable COLOR_BTNFACE           15)
(define-integrable COLOR_BTNSHADOW         16)
(define-integrable COLOR_GRAYTEXT          17)
(define-integrable COLOR_BTNTEXT           18)
(define-integrable COLOR_INACTIVECAPTIONTEXT 19)
(define-integrable COLOR_BTNHIGHLIGHT      20)

;; * GetWindow Constants
(define-integrable GW_HWNDFIRST        0)
(define-integrable GW_HWNDLAST         1)
(define-integrable GW_HWNDNEXT         2)
(define-integrable GW_HWNDPREV         3)
(define-integrable GW_OWNER            4)
(define-integrable GW_CHILD            5)
(define-integrable GW_MAX              5)

;; * Menu flags for Add/Check/EnableMenuItem
(define-integrable MF_INSERT          #x00000000)
(define-integrable MF_CHANGE          #x00000080)
(define-integrable MF_APPEND          #x00000100)
(define-integrable MF_DELETE          #x00000200)
(define-integrable MF_REMOVE          #x00001000)

(define-integrable MF_BYCOMMAND       #x00000000)
(define-integrable MF_BYPOSITION      #x00000400)

(define-integrable MF_SEPARATOR       #x00000800)

(define-integrable MF_ENABLED         #x00000000)
(define-integrable MF_GRAYED          #x00000001)
(define-integrable MF_DISABLED        #x00000002)

(define-integrable MF_UNCHECKED       #x00000000)
(define-integrable MF_CHECKED         #x00000008)
(define-integrable MF_USECHECKBITMAPS #x00000200)

(define-integrable MF_STRING          #x00000000)
(define-integrable MF_BITMAP          #x00000004)
(define-integrable MF_OWNERDRAW       #x00000100)

(define-integrable MF_POPUP           #x00000010)
(define-integrable MF_MENUBARBREAK    #x00000020)
(define-integrable MF_MENUBREAK       #x00000040)

(define-integrable MF_UNHILITE        #x00000000)
(define-integrable MF_HILITE          #x00000080)

(define-integrable MF_SYSMENU         #x00002000)
(define-integrable MF_HELP            #x00004000)
(define-integrable MF_MOUSESELECT     #x00008000)

;; * System Menu Command Values
(define-integrable SC_SIZE         #xF000)
(define-integrable SC_MOVE         #xF010)
(define-integrable SC_MINIMIZE     #xF020)
(define-integrable SC_MAXIMIZE     #xF030)
(define-integrable SC_NEXTWINDOW   #xF040)
(define-integrable SC_PREVWINDOW   #xF050)
(define-integrable SC_CLOSE        #xF060)
(define-integrable SC_VSCROLL      #xF070)
(define-integrable SC_HSCROLL      #xF080)
(define-integrable SC_MOUSEMENU    #xF090)
(define-integrable SC_KEYMENU      #xF100)
(define-integrable SC_ARRANGE      #xF110)
(define-integrable SC_RESTORE      #xF120)
(define-integrable SC_TASKLIST     #xF130)
(define-integrable SC_SCREENSAVE   #xF140)
(define-integrable SC_HOTKEY       #xF150)

;; * Obsolete names
(define-integrable SC_ICON         SC_MINIMIZE)
(define-integrable SC_ZOOM         SC_MAXIMIZE)

;; * Standard Cursor IDs
(define-integrable IDC_ARROW           32512)
(define-integrable IDC_IBEAM           32513)
(define-integrable IDC_WAIT            32514)
(define-integrable IDC_CROSS           32515)
(define-integrable IDC_UPARROW         32516)
(define-integrable IDC_SIZE            32640)
(define-integrable IDC_ICON            32641)
(define-integrable IDC_SIZENWSE        32642)
(define-integrable IDC_SIZENESW        32643)
(define-integrable IDC_SIZEWE          32644)
(define-integrable IDC_SIZENS          32645)
(define-integrable IDC_SIZEALL         32646) ; //not in win3.1
(define-integrable IDC_NO              32648) ; //not in win3.1
(define-integrable IDC_APPSTARTING     32650) ; //not in win3.1

;; * OEM Resource Ordinal Numbers
(define-integrable OBM_CLOSE           32754)
(define-integrable OBM_UPARROW         32753)
(define-integrable OBM_DNARROW         32752)
(define-integrable OBM_RGARROW         32751)
(define-integrable OBM_LFARROW         32750)
(define-integrable OBM_REDUCE          32749)
(define-integrable OBM_ZOOM            32748)
(define-integrable OBM_RESTORE         32747)
(define-integrable OBM_REDUCED         32746)
(define-integrable OBM_ZOOMD           32745)
(define-integrable OBM_RESTORED        32744)
(define-integrable OBM_UPARROWD        32743)
(define-integrable OBM_DNARROWD        32742)
(define-integrable OBM_RGARROWD        32741)
(define-integrable OBM_LFARROWD        32740)
(define-integrable OBM_MNARROW         32739)
(define-integrable OBM_COMBO           32738)
(define-integrable OBM_UPARROWI        32737)
(define-integrable OBM_DNARROWI        32736)
(define-integrable OBM_RGARROWI        32735)
(define-integrable OBM_LFARROWI        32734)

(define-integrable OBM_OLD_CLOSE       32767)
(define-integrable OBM_SIZE            32766)
(define-integrable OBM_OLD_UPARROW     32765)
(define-integrable OBM_OLD_DNARROW     32764)
(define-integrable OBM_OLD_RGARROW     32763)
(define-integrable OBM_OLD_LFARROW     32762)
(define-integrable OBM_BTSIZE          32761)
(define-integrable OBM_CHECK           32760)
(define-integrable OBM_CHECKBOXES      32759)
(define-integrable OBM_BTNCORNERS      32758)
(define-integrable OBM_OLD_REDUCE      32757)
(define-integrable OBM_OLD_ZOOM        32756)
(define-integrable OBM_OLD_RESTORE     32755)

(define-integrable OCR_NORMAL          32512)
(define-integrable OCR_IBEAM           32513)
(define-integrable OCR_WAIT            32514)
(define-integrable OCR_CROSS           32515)
(define-integrable OCR_UP              32516)
(define-integrable OCR_SIZE            32640)
(define-integrable OCR_ICON            32641)
(define-integrable OCR_SIZENWSE        32642)
(define-integrable OCR_SIZENESW        32643)
(define-integrable OCR_SIZEWE          32644)
(define-integrable OCR_SIZENS          32645)
(define-integrable OCR_SIZEALL         32646)
(define-integrable OCR_ICOCUR          32647)
(define-integrable OCR_NO              32648) ;  //not in win3.1

(define-integrable OIC_SAMPLE          32512)
(define-integrable OIC_HAND            32513)
(define-integrable OIC_QUES            32514)
(define-integrable OIC_BANG            32515)
(define-integrable OIC_NOTE            32516)

(define-integrable ORD_LANGDRIVER    1)   ;The ordinal number for the entry point of

;; * Standard Icon IDs
(define-integrable IDI_APPLICATION   32512)
(define-integrable IDI_HAND          32513)
(define-integrable IDI_QUESTION      32514)
(define-integrable IDI_EXCLAMATION   32515)
(define-integrable IDI_ASTERISK      32516)

;; * Dialog Box Command IDs
(define-integrable IDOK                1)
(define-integrable IDCANCEL            2)
(define-integrable IDABORT             3)
(define-integrable IDRETRY             4)
(define-integrable IDIGNORE            5)
(define-integrable IDYES               6)
(define-integrable IDNO                7)

;; * Edit Control Styles
(define-integrable ES_LEFT             #x0000)
(define-integrable ES_CENTER           #x0001)
(define-integrable ES_RIGHT            #x0002)
(define-integrable ES_MULTILINE        #x0004)
(define-integrable ES_UPPERCASE        #x0008)
(define-integrable ES_LOWERCASE        #x0010)
(define-integrable ES_PASSWORD         #x0020)
(define-integrable ES_AUTOVSCROLL      #x0040)
(define-integrable ES_AUTOHSCROLL      #x0080)
(define-integrable ES_NOHIDESEL        #x0100)
(define-integrable ES_OEMCONVERT       #x0400)
(define-integrable ES_READONLY         #x0800)
(define-integrable ES_WANTRETURN       #x1000)


;; * Edit Control Notification Codes
(define-integrable EN_SETFOCUS         #x0100)
(define-integrable EN_KILLFOCUS        #x0200)
(define-integrable EN_CHANGE           #x0300)
(define-integrable EN_UPDATE           #x0400)
(define-integrable EN_ERRSPACE         #x0500)
(define-integrable EN_MAXTEXT          #x0501)
(define-integrable EN_HSCROLL          #x0601)
(define-integrable EN_VSCROLL          #x0602)

;; * Edit Control Messages
(define-integrable EM_GETSEL               #x00B0)
(define-integrable EM_SETSEL               #x00B1)
(define-integrable EM_GETRECT              #x00B2)
(define-integrable EM_SETRECT              #x00B3)
(define-integrable EM_SETRECTNP            #x00B4)
(define-integrable EM_SCROLL               #x00B5)
(define-integrable EM_LINESCROLL           #x00B6)
(define-integrable EM_SCROLLCARET          #x00B7)
(define-integrable EM_GETMODIFY            #x00B8)
(define-integrable EM_SETMODIFY            #x00B9)
(define-integrable EM_GETLINECOUNT         #x00BA)
(define-integrable EM_LINEINDEX            #x00BB)
(define-integrable EM_SETHANDLE            #x00BC)
(define-integrable EM_GETHANDLE            #x00BD)
(define-integrable EM_GETTHUMB             #x00BE)
(define-integrable EM_LINELENGTH           #x00C1)
(define-integrable EM_REPLACESEL           #x00C2)
(define-integrable EM_GETLINE              #x00C4)
(define-integrable EM_LIMITTEXT            #x00C5)
(define-integrable EM_CANUNDO              #x00C6)
(define-integrable EM_UNDO                 #x00C7)
(define-integrable EM_FMTLINES             #x00C8)
(define-integrable EM_LINEFROMCHAR         #x00C9)
(define-integrable EM_SETTABSTOPS          #x00CB)
(define-integrable EM_SETPASSWORDCHAR      #x00CC)
(define-integrable EM_EMPTYUNDOBUFFER      #x00CD)
(define-integrable EM_GETFIRSTVISIBLELINE  #x00CE)
(define-integrable EM_SETREADONLY          #x00CF)
(define-integrable EM_SETWORDBREAKPROC     #x00D0)
(define-integrable EM_GETWORDBREAKPROC     #x00D1)
(define-integrable EM_GETPASSWORDCHAR      #x00D2)

;; * EDITWORDBREAKPROC code values
(define-integrable WB_LEFT            0)
(define-integrable WB_RIGHT           1)
(define-integrable WB_ISDELIMITER     2)

;; * Button Control Styles
(define-integrable BS_PUSHBUTTON      #x00)
(define-integrable BS_DEFPUSHBUTTON   #x01)
(define-integrable BS_CHECKBOX        #x02)
(define-integrable BS_AUTOCHECKBOX    #x03)
(define-integrable BS_RADIOBUTTON     #x04)
(define-integrable BS_3STATE          #x05)
(define-integrable BS_AUTO3STATE      #x06)
(define-integrable BS_GROUPBOX        #x07)
(define-integrable BS_USERBUTTON      #x08)
(define-integrable BS_AUTORADIOBUTTON #x09)
(define-integrable BS_OWNERDRAW       #x0B)
(define-integrable BS_LEFTTEXT        #x20)

;; * User Button Notification Codes
(define-integrable BN_CLICKED         0)
(define-integrable BN_PAINT           1)
(define-integrable BN_HILITE          2)
(define-integrable BN_UNHILITE        3)
(define-integrable BN_DISABLE         4)
(define-integrable BN_DOUBLECLICKED   5)

;; * Button Control Messages
(define-integrable BM_GETCHECK        #x00F0)
(define-integrable BM_SETCHECK        #x00F1)
(define-integrable BM_GETSTATE        #x00F2)
(define-integrable BM_SETSTATE        #x00F3)
(define-integrable BM_SETSTYLE        #x00F4)

;; * Static Control Constants
(define-integrable SS_LEFT            #x00)
(define-integrable SS_CENTER          #x01)
(define-integrable SS_RIGHT           #x02)
(define-integrable SS_ICON            #x03)
(define-integrable SS_BLACKRECT       #x04)
(define-integrable SS_GRAYRECT        #x05)
(define-integrable SS_WHITERECT       #x06)
(define-integrable SS_BLACKFRAME      #x07)
(define-integrable SS_GRAYFRAME       #x08)
(define-integrable SS_WHITEFRAME      #x09)
(define-integrable SS_USERITEM        #x0A)
(define-integrable SS_SIMPLE          #x0B)
(define-integrable SS_LEFTNOWORDWRAP  #x0C)
(define-integrable SS_NOPREFIX        #x80)  ;Don't do "&" character translation

;; * Static Control Mesages
(define-integrable STM_SETICON        #x170)
(define-integrable STM_GETICON        #x171)
(define-integrable STM_MSGMAX         #x172)

;; * Dialog window class
(define-integrable WC_DIALOG       #x8002)  ;    (MAKEINTATOM(0x8002))

;; * Get/SetWindowWord/Long offsets for use with WC_DIALOG windows
(define-integrable DWL_MSGRESULT   0)
(define-integrable DWL_DLGPROC     4)
(define-integrable DWL_USER        8)

;; * DlgDirList, DlgDirListComboBox flags values
(define-integrable DDL_READWRITE       #x0000)
(define-integrable DDL_READONLY        #x0001)
(define-integrable DDL_HIDDEN          #x0002)
(define-integrable DDL_SYSTEM          #x0004)
(define-integrable DDL_DIRECTORY       #x0010)
(define-integrable DDL_ARCHIVE         #x0020)

(define-integrable DDL_POSTMSGS        #x2000)
(define-integrable DDL_DRIVES          #x4000)
(define-integrable DDL_EXCLUSIVE       #x8000)

;; * Dialog Styles
(define-integrable DS_ABSALIGN         #x01)
(define-integrable DS_SYSMODAL         #x02)
(define-integrable DS_LOCALEDIT        #x20)  ;Edit items get Local storage.
(define-integrable DS_SETFONT          #x40)  ;User specified font for Dlg controls
(define-integrable DS_MODALFRAME       #x80)  ;Can be combined with WS_CAPTION
(define-integrable DS_NOIDLEMSG        #x100) ;WM_ENTERIDLE message will not be sent
(define-integrable DS_SETFOREGROUND    #x200) ;   //not in win3.1

(define-integrable DM_GETDEFID         (+ WM_USER 0))
(define-integrable DM_SETDEFID         (+ WM_USER 1))

;; * Returned in HIWORD of DM_GETDEFID result if msg is supported
(define-integrable DC_HASDEFID         #x534B)

;; * Dialog Codes
(define-integrable DLGC_WANTARROWS     #x0001) ;Control wants arrow keys
(define-integrable DLGC_WANTTAB        #x0002) ;Control wants tab keys
(define-integrable DLGC_WANTALLKEYS    #x0004) ;Control wants all keys
(define-integrable DLGC_WANTMESSAGE    #x0004) ;Pass message to control
(define-integrable DLGC_HASSETSEL      #x0008) ;Understands EM_SETSEL message
(define-integrable DLGC_DEFPUSHBUTTON  #x0010) ;Default pushbutton
(define-integrable DLGC_UNDEFPUSHBUTTON #x0020) ;Non-default pushbutton
(define-integrable DLGC_RADIOBUTTON    #x0040) ;Radio button
(define-integrable DLGC_WANTCHARS      #x0080) ;Want WM_CHAR messages
(define-integrable DLGC_STATIC         #x0100) ;Static item: don't include
(define-integrable DLGC_BUTTON         #x2000) ;Button item: can be checked

(define-integrable LB_CTLCODE          0)

;; * Listbox Return Values
(define-integrable LB_OKAY             0)
(define-integrable LB_ERR              -1)
(define-integrable LB_ERRSPACE         -2)

;; * Listbox Notification Codes
(define-integrable LBN_ERRSPACE        -2)
(define-integrable LBN_SELCHANGE       1)
(define-integrable LBN_DBLCLK          2)
(define-integrable LBN_SELCANCEL       3)
(define-integrable LBN_SETFOCUS        4)
(define-integrable LBN_KILLFOCUS       5)

;; * Listbox messages
(define-integrable LB_ADDSTRING            #x0180)
(define-integrable LB_INSERTSTRING         #x0181)
(define-integrable LB_DELETESTRING         #x0182)
(define-integrable LB_SELITEMRANGEEX       #x0183)
(define-integrable LB_RESETCONTENT         #x0184)
(define-integrable LB_SETSEL               #x0185)
(define-integrable LB_SETCURSEL            #x0186)
(define-integrable LB_GETSEL               #x0187)
(define-integrable LB_GETCURSEL            #x0188)
(define-integrable LB_GETTEXT              #x0189)
(define-integrable LB_GETTEXTLEN           #x018A)
(define-integrable LB_GETCOUNT             #x018B)
(define-integrable LB_SELECTSTRING         #x018C)
(define-integrable LB_DIR                  #x018D)
(define-integrable LB_GETTOPINDEX          #x018E)
(define-integrable LB_FINDSTRING           #x018F)
(define-integrable LB_GETSELCOUNT          #x0190)
(define-integrable LB_GETSELITEMS          #x0191)
(define-integrable LB_SETTABSTOPS          #x0192)
(define-integrable LB_GETHORIZONTALEXTENT  #x0193)
(define-integrable LB_SETHORIZONTALEXTENT  #x0194)
(define-integrable LB_SETCOLUMNWIDTH       #x0195)
(define-integrable LB_ADDFILE              #x0196)
(define-integrable LB_SETTOPINDEX          #x0197)
(define-integrable LB_GETITEMRECT          #x0198)
(define-integrable LB_GETITEMDATA          #x0199)
(define-integrable LB_SETITEMDATA          #x019A)
(define-integrable LB_SELITEMRANGE         #x019B)
(define-integrable LB_SETANCHORINDEX       #x019C)
(define-integrable LB_GETANCHORINDEX       #x019D)
(define-integrable LB_SETCARETINDEX        #x019E)
(define-integrable LB_GETCARETINDEX        #x019F)
(define-integrable LB_SETITEMHEIGHT        #x01A0)
(define-integrable LB_GETITEMHEIGHT        #x01A1)
(define-integrable LB_FINDSTRINGEXACT      #x01A2)
(define-integrable LB_SETLOCALE            #x01A5)
(define-integrable LB_GETLOCALE            #x01A6)
(define-integrable LB_SETCOUNT             #x01A7)
(define-integrable LB_MSGMAX               #x01A8)

;; * Listbox Styles
(define-integrable LBS_NOTIFY            #x0001)
(define-integrable LBS_SORT              #x0002)
(define-integrable LBS_NOREDRAW          #x0004)
(define-integrable LBS_MULTIPLESEL       #x0008)
(define-integrable LBS_OWNERDRAWFIXED    #x0010)
(define-integrable LBS_OWNERDRAWVARIABLE #x0020)
(define-integrable LBS_HASSTRINGS        #x0040)
(define-integrable LBS_USETABSTOPS       #x0080)
(define-integrable LBS_NOINTEGRALHEIGHT  #x0100)
(define-integrable LBS_MULTICOLUMN       #x0200)
(define-integrable LBS_WANTKEYBOARDINPUT #x0400)
(define-integrable LBS_EXTENDEDSEL       #x0800)
(define-integrable LBS_DISABLENOSCROLL   #x1000)
(define-integrable LBS_NODATA            #x2000)
(define-integrable LBS_STANDARD          (+ LBS_NOTIFY LBS_SORT WS_VSCROLL WS_BORDER))

;; * Combo Box return Values
(define-integrable CB_OKAY             0)
(define-integrable CB_ERR              -1)
(define-integrable CB_ERRSPACE         -2)

;; * Combo Box Notification Codes
(define-integrable CBN_ERRSPACE        -1)
(define-integrable CBN_SELCHANGE       1)
(define-integrable CBN_DBLCLK          2)
(define-integrable CBN_SETFOCUS        3)
(define-integrable CBN_KILLFOCUS       4)
(define-integrable CBN_EDITCHANGE      5)
(define-integrable CBN_EDITUPDATE      6)
(define-integrable CBN_DROPDOWN        7)
(define-integrable CBN_CLOSEUP         8)
(define-integrable CBN_SELENDOK        9)
(define-integrable CBN_SELENDCANCEL    10)

;; * Combo Box styles
(define-integrable CBS_SIMPLE            #x0001)
(define-integrable CBS_DROPDOWN          #x0002)
(define-integrable CBS_DROPDOWNLIST      #x0003)
(define-integrable CBS_OWNERDRAWFIXED    #x0010)
(define-integrable CBS_OWNERDRAWVARIABLE #x0020)
(define-integrable CBS_AUTOHSCROLL       #x0040)
(define-integrable CBS_OEMCONVERT        #x0080)
(define-integrable CBS_SORT              #x0100)
(define-integrable CBS_HASSTRINGS        #x0200)
(define-integrable CBS_NOINTEGRALHEIGHT  #x0400)
(define-integrable CBS_DISABLENOSCROLL   #x0800)

;; * Combo Box messages
(define-integrable CB_GETEDITSEL               #x0140)
(define-integrable CB_LIMITTEXT                #x0141)
(define-integrable CB_SETEDITSEL               #x0142)
(define-integrable CB_ADDSTRING                #x0143)
(define-integrable CB_DELETESTRING             #x0144)
(define-integrable CB_DIR                      #x0145)
(define-integrable CB_GETCOUNT                 #x0146)
(define-integrable CB_GETCURSEL                #x0147)
(define-integrable CB_GETLBTEXT                #x0148)
(define-integrable CB_GETLBTEXTLEN             #x0149)
(define-integrable CB_INSERTSTRING             #x014A)
(define-integrable CB_RESETCONTENT             #x014B)
(define-integrable CB_FINDSTRING               #x014C)
(define-integrable CB_SELECTSTRING             #x014D)
(define-integrable CB_SETCURSEL                #x014E)
(define-integrable CB_SHOWDROPDOWN             #x014F)
(define-integrable CB_GETITEMDATA              #x0150)
(define-integrable CB_SETITEMDATA              #x0151)
(define-integrable CB_GETDROPPEDCONTROLRECT    #x0152)
(define-integrable CB_SETITEMHEIGHT            #x0153)
(define-integrable CB_GETITEMHEIGHT            #x0154)
(define-integrable CB_SETEXTENDEDUI            #x0155)
(define-integrable CB_GETEXTENDEDUI            #x0156)
(define-integrable CB_GETDROPPEDSTATE          #x0157)
(define-integrable CB_FINDSTRINGEXACT          #x0158)
(define-integrable CB_SETLOCALE                #x0159)
(define-integrable CB_GETLOCALE                #x015a)
(define-integrable CB_MSGMAX                   #x015b)

;; * Scroll Bar Styles
(define-integrable SBS_HORZ                    #x0000)
(define-integrable SBS_VERT                    #x0001)
(define-integrable SBS_TOPALIGN                #x0002)
(define-integrable SBS_LEFTALIGN               #x0002)
(define-integrable SBS_BOTTOMALIGN             #x0004)
(define-integrable SBS_RIGHTALIGN              #x0004)
(define-integrable SBS_SIZEBOXTOPLEFTALIGN     #x0002)
(define-integrable SBS_SIZEBOXBOTTOMRIGHTALIGN #x0004)
(define-integrable SBS_SIZEBOX                 #x0008)

;; * Scroll bar messages
(define-integrable SBM_SETPOS                  #x00E0)  ;   //not in win3.1
(define-integrable SBM_GETPOS                  #x00E1)  ;   //not in win3.1
(define-integrable SBM_SETRANGE                #x00E2)  ;   //not in win3.1
(define-integrable SBM_SETRANGEREDRAW          #x00E6)  ;   //not in win3.1
(define-integrable SBM_GETRANGE                #x00E3)  ;   //not in win3.1
(define-integrable SBM_ENABLE_ARROWS           #x00E4)  ;   //not in win3.1

;; * MDI client style bits
(define-integrable MDIS_ALLCHILDSTYLES    #x0001)

;; * wParam Flags for WM_MDITILE and WM_MDICASCADE messages.
(define-integrable MDITILE_VERTICAL       #x0000)  ;   //not in win3.1
(define-integrable MDITILE_HORIZONTAL     #x0001)  ;   //not in win3.1
(define-integrable MDITILE_SKIPDISABLED   #x0002)  ;   //not in win3.1

;; * Commands to pass to WinHelp
(define-integrable HELP_CONTEXT      #x0001) ;Display topic in ulTopic
(define-integrable HELP_QUIT         #x0002) ;Terminate help
(define-integrable HELP_INDEX        #x0003) ;Display index
(define-integrable HELP_CONTENTS     #x0003)
(define-integrable HELP_HELPONHELP   #x0004) ;Display help on using help
(define-integrable HELP_SETINDEX     #x0005) ;Set current Index for multi index help
(define-integrable HELP_SETCONTENTS  #x0005)
(define-integrable HELP_CONTEXTPOPUP #x0008)
(define-integrable HELP_FORCEFILE    #x0009)
(define-integrable HELP_KEY          #x0101) ;Display topic for keyword in offabData
(define-integrable HELP_COMMAND      #x0102)
(define-integrable HELP_PARTIALKEY   #x0105)
(define-integrable HELP_MULTIKEY     #x0201)
(define-integrable HELP_SETWINPOS    #x0203)

;; * Parameter for SystemParametersInfo
(define-integrable SPI_GETBEEP                 1)
(define-integrable SPI_SETBEEP                 2)
(define-integrable SPI_GETMOUSE                3)
(define-integrable SPI_SETMOUSE                4)
(define-integrable SPI_GETBORDER               5)
(define-integrable SPI_SETBORDER               6)
(define-integrable SPI_GETKEYBOARDSPEED       10)
(define-integrable SPI_SETKEYBOARDSPEED       11)
(define-integrable SPI_LANGDRIVER             12)
(define-integrable SPI_ICONHORIZONTALSPACING  13)
(define-integrable SPI_GETSCREENSAVETIMEOUT   14)
(define-integrable SPI_SETSCREENSAVETIMEOUT   15)
(define-integrable SPI_GETSCREENSAVEACTIVE    16)
(define-integrable SPI_SETSCREENSAVEACTIVE    17)
(define-integrable SPI_GETGRIDGRANULARITY     18)
(define-integrable SPI_SETGRIDGRANULARITY     19)
(define-integrable SPI_SETDESKWALLPAPER       20)
(define-integrable SPI_SETDESKPATTERN         21)
(define-integrable SPI_GETKEYBOARDDELAY       22)
(define-integrable SPI_SETKEYBOARDDELAY       23)
(define-integrable SPI_ICONVERTICALSPACING    24)
(define-integrable SPI_GETICONTITLEWRAP       25)
(define-integrable SPI_SETICONTITLEWRAP       26)
(define-integrable SPI_GETMENUDROPALIGNMENT   27)
(define-integrable SPI_SETMENUDROPALIGNMENT   28)
(define-integrable SPI_SETDOUBLECLKWIDTH      29)
(define-integrable SPI_SETDOUBLECLKHEIGHT     30)
(define-integrable SPI_GETICONTITLELOGFONT    31)
(define-integrable SPI_SETDOUBLECLICKTIME     32)
(define-integrable SPI_SETMOUSEBUTTONSWAP     33)
(define-integrable SPI_SETICONTITLELOGFONT    34)
(define-integrable SPI_GETFASTTASKSWITCH      35)
(define-integrable SPI_SETFASTTASKSWITCH      36)

;; * Flags
(define-integrable SPIF_UPDATEINIFILE    #x0001)
(define-integrable SPIF_SENDWININICHANGE #x0002)