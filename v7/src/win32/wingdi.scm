;;/*++ BUILD Version: 0004    // Increment this if a change has global effects
;;
;;Copyright (c) 1985-91, Microsoft Corporation
;;
;;Module Name:
;;
;;    wingdi.h
;;
;;Abstract:
;;
;;    Procedure declarations, constant definitions and macros for the GDI
;;    component.
;;
;;--*/
;;
;;#ifndef _WINGDI_
;;#define _WINGDI_
;;
;;#ifdef __cplusplus
;;extern "C" {
;;#endif
;;
;;#ifndef NOGDI
;;
;;#ifndef NORASTEROPS
;;
;;/* Binary raster ops */
(define-integrable R2_BLACK            1 ) ;  /*  0       */
(define-integrable R2_NOTMERGEPEN      2 ) ;  /* DPon     */
(define-integrable R2_MASKNOTPEN       3 ) ;  /* DPna     */
(define-integrable R2_NOTCOPYPEN       4 ) ;  /* PN       */
(define-integrable R2_MASKPENNOT       5 ) ;  /* PDna     */
(define-integrable R2_NOT              6 ) ;  /* Dn       */
(define-integrable R2_XORPEN           7 ) ;  /* DPx      */
(define-integrable R2_NOTMASKPEN       8 ) ;  /* DPan     */
(define-integrable R2_MASKPEN          9 ) ;  /* DPa      */
(define-integrable R2_NOTXORPEN        10) ;  /* DPxn     */
(define-integrable R2_NOP              11) ;  /* D        */
(define-integrable R2_MERGENOTPEN      12) ;  /* DPno     */
(define-integrable R2_COPYPEN          13) ;  /* P        */
(define-integrable R2_MERGEPENNOT      14) ;  /* PDno     */
(define-integrable R2_MERGEPEN         15) ;  /* DPo      */
(define-integrable R2_WHITE            16) ;  /*  1       */
(define-integrable R2_LAST             16)

;;/*  Ternary raster operations */
(define-integrable SRCCOPY             #x00CC0020)  ;/* dest = source                   */
(define-integrable SRCPAINT            #x00EE0086)  ;/* dest = source OR dest           */
(define-integrable SRCAND              #x008800C6)  ;/* dest = source AND dest          */
(define-integrable SRCINVERT           #x00660046)  ;/* dest = source XOR dest          */
(define-integrable SRCERASE            #x00440328)  ;/* dest = source AND (NOT dest )   */
(define-integrable NOTSRCCOPY          #x00330008)  ;/* dest = (NOT source)             */
(define-integrable NOTSRCERASE         #x001100A6)  ;/* dest = (NOT src) AND (NOT dest) */
(define-integrable MERGECOPY           #x00C000CA)  ;/* dest = (source AND pattern)     */
(define-integrable MERGEPAINT          #x00BB0226)  ;/* dest = (NOT source) OR dest     */
(define-integrable PATCOPY             #x00F00021)  ;/* dest = pattern                  */
(define-integrable PATPAINT            #x00FB0A09)  ;/* dest = DPSnoo                   */
(define-integrable PATINVERT           #x005A0049)  ;/* dest = pattern XOR dest         */
(define-integrable DSTINVERT           #x00550009)  ;/* dest = (NOT dest)               */
(define-integrable BLACKNESS           #x00000042)  ;/* dest = BLACK                    */
(define-integrable WHITENESS           #x00FF0062)  ;/* dest = WHITE                    */
;;#endif /* NORASTEROPS */
;;
(define-integrable GDI_ERROR #xFFFFFFFF)
(define-integrable HGDI_ERROR #xFFFFFFFF)  ;#define HGDI_ERROR ((HANDLE)(0xFFFFFFFFL))

;;/* Region Flags */
(define-integrable ERROR               0)
(define-integrable NULLREGION          1)
(define-integrable SIMPLEREGION        2)
(define-integrable COMPLEXREGION       3)
(define-integrable RGN_ERROR ERROR)

;;/* CombineRgn() Styles */
(define-integrable RGN_AND             1)
(define-integrable RGN_OR              2)
(define-integrable RGN_XOR             3)
(define-integrable RGN_DIFF            4)
(define-integrable RGN_COPY            5)
(define-integrable RGN_MIN             RGN_AND)
(define-integrable RGN_MAX             RGN_COPY)

;;/* StretchBlt() Modes */
(define-integrable BLACKONWHITE                 1)
(define-integrable WHITEONBLACK                 2)
(define-integrable COLORONCOLOR                 3)
(define-integrable HALFTONE                     4)
(define-integrable MAXSTRETCHBLTMODE            4)

;;/* PolyFill() Modes */
(define-integrable ALTERNATE                    1)
(define-integrable WINDING                      2)
(define-integrable POLYFILL_LAST                2)

;;/* Text Alignment Options */
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

;;/* Bounds Accumulation APIs */

(define-integrable DCB_RESET       #x0001)
(define-integrable DCB_ACCUMULATE  #x0002)
(define-integrable DCB_DIRTY       DCB_ACCUMULATE)
(define-integrable DCB_SET         (+ DCB_RESET DCB_ACCUMULATE))
(define-integrable DCB_ENABLE      #x0004)
(define-integrable DCB_DISABLE     #x0008)

;;#ifndef NOMETAFILE
;;
;;/* Metafile Functions */
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

;;#endif /* NOMETAFILE */
;;
;;/* GDI Escapes */
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

;;/* Spooler Error Codes */
(define-integrable SP_NOTREPORTED               #x4000)
(define-integrable SP_ERROR                     -1)
(define-integrable SP_APPABORT                  -2)
(define-integrable SP_USERABORT                 -3)
(define-integrable SP_OUTOFDISK                 -4)
(define-integrable SP_OUTOFMEMORY               -5)

(define-integrable PR_JOBSTATUS                 #x0000)

;;/* Object Definitions for EnumObjects() */
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

;;/* xform stuff */
(define-integrable MWT_IDENTITY        1)
(define-integrable MWT_LEFTMULTIPLY    2)
(define-integrable MWT_RIGHTMULTIPLY   3)

(define-integrable MWT_MIN             MWT_IDENTITY)
(define-integrable MWT_MAX             MWT_RIGHTMULTIPLY)

;;#define _XFORM_
;;typedef struct  tagXFORM
;;  {
;;    FLOAT   eM11;
;;    FLOAT   eM12;
;;    FLOAT   eM21;
;;    FLOAT   eM22;
;;    FLOAT   eDx;
;;    FLOAT   eDy;
;;  } XFORM, *PXFORM, FAR *LPXFORM;
;;
;;/* Bitmap Header Definition */
;;typedef struct tagBITMAP
;;  {
;;    LONG        bmType;
;;    LONG        bmWidth;
;;    LONG        bmHeight;
;;    LONG        bmWidthBytes;
;;    WORD        bmPlanes;
;;    WORD        bmBitsPixel;
;;    LPVOID      bmBits;
;;  } BITMAP, *PBITMAP, NEAR *NPBITMAP, FAR *LPBITMAP;
;;
;;typedef struct tagRGBTRIPLE {
;;        BYTE    rgbtBlue;
;;        BYTE    rgbtGreen;
;;        BYTE    rgbtRed;
;;} RGBTRIPLE;
;;
;;typedef struct tagRGBQUAD {
;;        BYTE    rgbBlue;
;;        BYTE    rgbGreen;
;;        BYTE    rgbRed;
;;        BYTE    rgbReserved;
;;} RGBQUAD;
;;
;;/* structures for defining DIBs */
;;typedef struct tagBITMAPCOREHEADER {
;;        DWORD   bcSize;                 /* used to get to color table */
;;        WORD    bcWidth;
;;        WORD    bcHeight;
;;        WORD    bcPlanes;
;;        WORD    bcBitCount;
;;} BITMAPCOREHEADER, FAR *LPBITMAPCOREHEADER, *PBITMAPCOREHEADER;
;;
;;
;;typedef struct tagBITMAPINFOHEADER{
;;        DWORD      biSize;
;;        LONG       biWidth;
;;        LONG       biHeight;
;;        WORD       biPlanes;
;;        WORD       biBitCount;
;;        DWORD      biCompression;
;;        DWORD      biSizeImage;
;;        LONG       biXPelsPerMeter;
;;        LONG       biYPelsPerMeter;
;;        DWORD      biClrUsed;
;;        DWORD      biClrImportant;
;;} BITMAPINFOHEADER, FAR *LPBITMAPINFOHEADER, *PBITMAPINFOHEADER;
;;
;;/* constants for the biCompression field */
(define-integrable BI_RGB        0)
(define-integrable BI_RLE8       1)
(define-integrable BI_RLE4       2)
(define-integrable BI_BITFIELDS  3)

;;typedef struct tagBITMAPINFO {
;;    BITMAPINFOHEADER    bmiHeader;
;;    RGBQUAD             bmiColors[1];
;;} BITMAPINFO, FAR *LPBITMAPINFO, *PBITMAPINFO;
;;
;;typedef struct tagBITMAPCOREINFO {
;;    BITMAPCOREHEADER    bmciHeader;
;;    RGBTRIPLE           bmciColors[1];
;;} BITMAPCOREINFO, FAR *LPBITMAPCOREINFO, *PBITMAPCOREINFO;
;;
;;#pragma pack(2)
;;typedef struct tagBITMAPFILEHEADER {
;;        WORD    bfType;
;;        DWORD   bfSize;
;;        WORD    bfReserved1;
;;        WORD    bfReserved2;
;;        DWORD   bfOffBits;
;;} BITMAPFILEHEADER, FAR *LPBITMAPFILEHEADER, *PBITMAPFILEHEADER;
;;#pragma pack()
;;
;;#define MAKEPOINTS(l)       (*((POINTS FAR *)&(l)))
;;
;;#ifndef NOMETAFILE
;;
;;/* Clipboard Metafile Picture Structure */
;;typedef struct tagHANDLETABLE
;;  {
;;    HGDIOBJ     objectHandle[1];
;;  } HANDLETABLE, *PHANDLETABLE, FAR *LPHANDLETABLE;
;;
;;typedef struct tagMETARECORD
;;  {
;;    DWORD       rdSize;
;;    WORD        rdFunction;
;;    WORD        rdParm[1];
;;  } METARECORD;
;;typedef struct tagMETARECORD UNALIGNED *PMETARECORD;
;;typedef struct tagMETARECORD UNALIGNED FAR *LPMETARECORD;
;;
;;typedef struct tagMETAFILEPICT
;;  {
;;    LONG        mm;
;;    LONG        xExt;
;;    LONG        yExt;
;;    HMETAFILE   hMF;
;;  } METAFILEPICT, FAR *LPMETAFILEPICT;
;;
;;#pragma pack(2)
;;typedef struct tagMETAHEADER
;;{
;;    WORD        mtType;
;;    WORD        mtHeaderSize;
;;    WORD        mtVersion;
;;    DWORD       mtSize;
;;    WORD        mtNoObjects;
;;    DWORD       mtMaxRecord;
;;    WORD        mtNoParameters;
;;} METAHEADER;
;;typedef struct tagMETAHEADER UNALIGNED *PMETAHEADER;
;;typedef struct tagMETAHEADER UNALIGNED FAR *LPMETAHEADER;
;;
;;#pragma pack()
;;
;;/* Enhanced Metafile structures */
;;typedef struct tagENHMETARECORD
;;{
;;    DWORD   iType;              // Record type MR_XXX
;;    DWORD   nSize;              // Record size in bytes
;;    DWORD   dParm[1];           // Parameters
;;} ENHMETARECORD, *PENHMETARECORD, *LPENHMETARECORD;
;;
;;typedef struct tagENHMETAHEADER
;;{
;;    DWORD   iType;              // Record type MR_METAFILE
;;    DWORD   nSize;              // Record size in bytes.  This may be greater
;;                                // than the sizeof(ENHMETAHEADER).
;;    RECTL   rclBounds;          // Inclusive-inclusive bounds in device units
;;    RECTL   rclFrame;           // Inclusive-inclusive Picture Frame of metafile in .01 mm units
;;    DWORD   dSignature;         // Signature.  Must be ENHMETA_SIGNATURE.
;;    DWORD   nVersion;           // Version number
;;    DWORD   nBytes;             // Size of the metafile in bytes
;;    DWORD   nRecords;           // Number of records in the metafile
;;    WORD    nHandles;           // Number of handles in the handle table
;;                                // Handle index zero is reserved.
;;    WORD    sReserved;          // Reserved.  Must be zero.
;;    DWORD   nDescription;       // Number of chars in the unicode description string
;;                                // This is 0 if there is no description string
;;    DWORD   offDescription;     // Offset to the metafile description record.
;;                                // This is 0 if there is no description string
;;    DWORD   nPalEntries;        // Number of entries in the metafile palette.
;;    SIZEL   szlDevice;          // Size of the reference device in pels
;;    SIZEL   szlMillimeters;     // Size of the reference device in millimeters
;;} ENHMETAHEADER, *PENHMETAHEADER, *LPENHMETAHEADER;
;;
;;#endif /* NOMETAFILE */
;;
;;#ifndef NOTEXTMETRIC
;;
;;/* tmPitchAntFamily flags */
(define-integrable TMPF_FIXED_PITCH    #x01)
(define-integrable TMPF_VECTOR             #x02)
(define-integrable TMPF_DEVICE             #x08)
(define-integrable TMPF_TRUETYPE       #x04)

;;typedef struct tagTEXTMETRICA
;;{
;;    LONG        tmHeight;
;;    LONG        tmAscent;
;;    LONG        tmDescent;
;;    LONG        tmInternalLeading;
;;    LONG        tmExternalLeading;
;;    LONG        tmAveCharWidth;
;;    LONG        tmMaxCharWidth;
;;    LONG        tmWeight;
;;    LONG        tmOverhang;
;;    LONG        tmDigitizedAspectX;
;;    LONG        tmDigitizedAspectY;
;;    BYTE        tmFirstChar;
;;    BYTE        tmLastChar;
;;    BYTE        tmDefaultChar;
;;    BYTE        tmBreakChar;
;;    BYTE        tmItalic;
;;    BYTE        tmUnderlined;
;;    BYTE        tmStruckOut;
;;    BYTE        tmPitchAndFamily;
;;    BYTE        tmCharSet;
;;} TEXTMETRICA, *PTEXTMETRICA, NEAR *NPTEXTMETRICA, FAR *LPTEXTMETRICA;
;;typedef struct tagTEXTMETRICW
;;{
;;    LONG        tmHeight;
;;    LONG        tmAscent;
;;    LONG        tmDescent;
;;    LONG        tmInternalLeading;
;;    LONG        tmExternalLeading;
;;    LONG        tmAveCharWidth;
;;    LONG        tmMaxCharWidth;
;;    LONG        tmWeight;
;;    LONG        tmOverhang;
;;    LONG        tmDigitizedAspectX;
;;    LONG        tmDigitizedAspectY;
;;    WCHAR       tmFirstChar;
;;    WCHAR       tmLastChar;
;;    WCHAR       tmDefaultChar;
;;    WCHAR       tmBreakChar;
;;    BYTE        tmItalic;
;;    BYTE        tmUnderlined;
;;    BYTE        tmStruckOut;
;;    BYTE        tmPitchAndFamily;
;;    BYTE        tmCharSet;
;;} TEXTMETRICW, *PTEXTMETRICW, NEAR *NPTEXTMETRICW, FAR *LPTEXTMETRICW;
;;#ifdef UNICODE
;;typedef TEXTMETRICW TEXTMETRIC;
;;typedef PTEXTMETRICW PTEXTMETRIC;
;;typedef NPTEXTMETRICW NPTEXTMETRIC;
;;typedef LPTEXTMETRICW LPTEXTMETRIC;
;;#else
;;typedef TEXTMETRICA TEXTMETRIC;
;;typedef PTEXTMETRICA PTEXTMETRIC;
;;typedef NPTEXTMETRICA NPTEXTMETRIC;
;;typedef LPTEXTMETRICA LPTEXTMETRIC;
;;#endif // UNICODE
;;
;;/* ntmFlags field flags */
(define-integrable NTM_REGULAR     #x00000040)
(define-integrable NTM_BOLD        #x00000020)
(define-integrable NTM_ITALIC      #x00000001)
;;
;;typedef struct tagNEWTEXTMETRICA
;;{
;;    LONG        tmHeight;
;;    LONG        tmAscent;
;;    LONG        tmDescent;
;;    LONG        tmInternalLeading;
;;    LONG        tmExternalLeading;
;;    LONG        tmAveCharWidth;
;;    LONG        tmMaxCharWidth;
;;    LONG        tmWeight;
;;    LONG        tmOverhang;
;;    LONG        tmDigitizedAspectX;
;;    LONG        tmDigitizedAspectY;
;;    BYTE        tmFirstChar;
;;    BYTE        tmLastChar;
;;    BYTE        tmDefaultChar;
;;    BYTE        tmBreakChar;
;;    BYTE        tmItalic;
;;    BYTE        tmUnderlined;
;;    BYTE        tmStruckOut;
;;    BYTE        tmPitchAndFamily;
;;    BYTE        tmCharSet;
;;    DWORD   ntmFlags;
;;    UINT    ntmSizeEM;
;;    UINT    ntmCellHeight;
;;    UINT    ntmAvgWidth;
;;} NEWTEXTMETRICA, *PNEWTEXTMETRICA, NEAR *NPNEWTEXTMETRICA, FAR *LPNEWTEXTMETRICA;
;;typedef struct tagNEWTEXTMETRICW
;;{
;;    LONG        tmHeight;
;;    LONG        tmAscent;
;;    LONG        tmDescent;
;;    LONG        tmInternalLeading;
;;    LONG        tmExternalLeading;
;;    LONG        tmAveCharWidth;
;;    LONG        tmMaxCharWidth;
;;    LONG        tmWeight;
;;    LONG        tmOverhang;
;;    LONG        tmDigitizedAspectX;
;;    LONG        tmDigitizedAspectY;
;;    WCHAR       tmFirstChar;
;;    WCHAR       tmLastChar;
;;    WCHAR       tmDefaultChar;
;;    WCHAR       tmBreakChar;
;;    BYTE        tmItalic;
;;    BYTE        tmUnderlined;
;;    BYTE        tmStruckOut;
;;    BYTE        tmPitchAndFamily;
;;    BYTE        tmCharSet;
;;    DWORD   ntmFlags;
;;    UINT    ntmSizeEM;
;;    UINT    ntmCellHeight;
;;    UINT    ntmAvgWidth;
;;} NEWTEXTMETRICW, *PNEWTEXTMETRICW, NEAR *NPNEWTEXTMETRICW, FAR *LPNEWTEXTMETRICW;
;;#ifdef UNICODE
;;typedef NEWTEXTMETRICW NEWTEXTMETRIC;
;;typedef PNEWTEXTMETRICW PNEWTEXTMETRIC;
;;typedef NPNEWTEXTMETRICW NPNEWTEXTMETRIC;
;;typedef LPNEWTEXTMETRICW LPNEWTEXTMETRIC;
;;#else
;;typedef NEWTEXTMETRICA NEWTEXTMETRIC;
;;typedef PNEWTEXTMETRICA PNEWTEXTMETRIC;
;;typedef NPNEWTEXTMETRICA NPNEWTEXTMETRIC;
;;typedef LPNEWTEXTMETRICA LPNEWTEXTMETRIC;
;;#endif // UNICODE
;;
;;#endif /* NOTEXTMETRIC */
;;/* GDI Logical Objects: */
;;
;;/* Pel Array */
;;typedef struct tagPELARRAY
;;  {
;;    LONG        paXCount;
;;    LONG        paYCount;
;;    LONG        paXExt;
;;    LONG        paYExt;
;;    BYTE        paRGBs;
;;  } PELARRAY, *PPELARRAY, NEAR *NPPELARRAY, FAR *LPPELARRAY;
;;
;;/* Logical Brush (or Pattern) */
;;typedef struct tagLOGBRUSH
;;  {
;;    UINT        lbStyle;
;;    COLORREF    lbColor;
;;    LONG        lbHatch;
;;  } LOGBRUSH, *PLOGBRUSH, NEAR *NPLOGBRUSH, FAR *LPLOGBRUSH;
;;
;;typedef LOGBRUSH            PATTERN;
;;typedef PATTERN             *PPATTERN;
;;typedef PATTERN NEAR        *NPPATTERN;
;;typedef PATTERN FAR         *LPPATTERN;
;;
;;/* Logical Pen */
;;typedef struct tagLOGPEN
;;  {
;;    UINT        lopnStyle;
;;    POINT       lopnWidth;
;;    COLORREF    lopnColor;
;;  } LOGPEN, *PLOGPEN, NEAR *NPLOGPEN, FAR *LPLOGPEN;
;;
;;typedef struct tagEXTLOGPEN {
;;    DWORD       elpPenStyle;
;;    DWORD       elpWidth;
;;    UINT        elpBrushStyle;
;;    COLORREF    elpColor;
;;    LONG        elpHatch;
;;    DWORD       elpNumEntries;
;;    DWORD       elpStyleEntry[1];
;;} EXTLOGPEN, *PEXTLOGPEN, NEAR *NPEXTLOGPEN, FAR *LPEXTLOGPEN;
;;
;;typedef struct tagPALETTEENTRY {
;;    BYTE        peRed;
;;    BYTE        peGreen;
;;    BYTE        peBlue;
;;    BYTE        peFlags;
;;} PALETTEENTRY, *PPALETTEENTRY, FAR *LPPALETTEENTRY;
;;
;;/* Logical Palette */
;;typedef struct tagLOGPALETTE {
;;    WORD        palVersion;
;;    WORD        palNumEntries;
;;    PALETTEENTRY        palPalEntry[1];
;;} LOGPALETTE, *PLOGPALETTE, NEAR *NPLOGPALETTE, FAR *LPLOGPALETTE;
;;
;;
;;/* Logical Font */
;;#define LF_FACESIZE         32
;;
;;typedef struct tagLOGFONTA
;;{
;;    LONG      lfHeight;
;;    LONG      lfWidth;
;;    LONG      lfEscapement;
;;    LONG      lfOrientation;
;;    LONG      lfWeight;
;;    BYTE      lfItalic;
;;    BYTE      lfUnderline;
;;    BYTE      lfStrikeOut;
;;    BYTE      lfCharSet;
;;    BYTE      lfOutPrecision;
;;    BYTE      lfClipPrecision;
;;    BYTE      lfQuality;
;;    BYTE      lfPitchAndFamily;
;;    CHAR      lfFaceName[LF_FACESIZE];
;;} LOGFONTA, *PLOGFONTA, NEAR *NPLOGFONTA, FAR *LPLOGFONTA;
;;typedef struct tagLOGFONTW
;;{
;;    LONG      lfHeight;
;;    LONG      lfWidth;
;;    LONG      lfEscapement;
;;    LONG      lfOrientation;
;;    LONG      lfWeight;
;;    BYTE      lfItalic;
;;    BYTE      lfUnderline;
;;    BYTE      lfStrikeOut;
;;    BYTE      lfCharSet;
;;    BYTE      lfOutPrecision;
;;    BYTE      lfClipPrecision;
;;    BYTE      lfQuality;
;;    BYTE      lfPitchAndFamily;
;;    WCHAR     lfFaceName[LF_FACESIZE];
;;} LOGFONTW, *PLOGFONTW, NEAR *NPLOGFONTW, FAR *LPLOGFONTW;
;;#ifdef UNICODE
;;typedef LOGFONTW LOGFONT;
;;typedef PLOGFONTW PLOGFONT;
;;typedef NPLOGFONTW NPLOGFONT;
;;typedef LPLOGFONTW LPLOGFONT;
;;#else
;;typedef LOGFONTA LOGFONT;
;;typedef PLOGFONTA PLOGFONT;
;;typedef NPLOGFONTA NPLOGFONT;
;;typedef LPLOGFONTA LPLOGFONT;
;;#endif // UNICODE
;;
(define-integrable LF_FULLFACESIZE     64)
;;
;;/* Structure passed to FONTENUMPROC */
;;typedef struct tagENUMLOGFONTA
;;{
;;    LOGFONTA elfLogFont;
;;    BYTE     elfFullName[LF_FULLFACESIZE];
;;    BYTE     elfStyle[LF_FACESIZE];
;;} ENUMLOGFONTA, FAR* LPENUMLOGFONTA;
;;/* Structure passed to FONTENUMPROC */
;;typedef struct tagENUMLOGFONTW
;;{
;;    LOGFONTW elfLogFont;
;;    WCHAR    elfFullName[LF_FULLFACESIZE];
;;    WCHAR    elfStyle[LF_FACESIZE];
;;} ENUMLOGFONTW, FAR* LPENUMLOGFONTW;
;;#ifdef UNICODE
;;typedef ENUMLOGFONTW ENUMLOGFONT;
;;typedef LPENUMLOGFONTW LPENUMLOGFONT;
;;#else
;;typedef ENUMLOGFONTA ENUMLOGFONT;
;;typedef LPENUMLOGFONTA LPENUMLOGFONT;
;;#endif // UNICODE
;;
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

;;/* Font Families */
(define-integrable FF_DONTCARE         (fix:lsh 0 4))  ;/* Don't care or don't know. */
(define-integrable FF_ROMAN            (fix:lsh 1 4))  ;/* Variable stroke width, serifed. */
                                            ;/* Times Roman, Century Schoolbook, etc. */
(define-integrable FF_SWISS            (fix:lsh 2 4))  ;/* Variable stroke width, sans-serifed. */
	                                    ;/* Helvetica, Swiss, etc. */
(define-integrable FF_MODERN           (fix:lsh 3 4))  ;/* Constant stroke width, serifed or sans-serifed. */
	                                    ;/* Pica, Elite, Courier, etc. */
(define-integrable FF_SCRIPT           (fix:lsh 4 4))  ;/* Cursive, etc. */
(define-integrable FF_DECORATIVE       (fix:lsh 5 4))  ;/* Old English, etc. */

;;/* Font Weights */
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

;;typedef struct tagPANOSE
;;{
;;    BYTE    bFamilyType;
;;    BYTE    bSerifStyle;
;;    BYTE    bWeight;
;;    BYTE    bProportion;
;;    BYTE    bContrast;
;;    BYTE    bStrokeVariation;
;;    BYTE    bArmStyle;
;;    BYTE    bLetterform;
;;    BYTE    bMidline;
;;    BYTE    bXHeight;
;;} PANOSE, * LPPANOSE;

(define-integrable PAN_ANY                         0);/* Any                            */
(define-integrable PAN_NO_FIT                      1);/* No Fit                         */

(define-integrable PAN_FAMILY_TEXT_DISPLAY         2);/* Text and Display               */
(define-integrable PAN_FAMILY_SCRIPT               3);/* Script                         */
(define-integrable PAN_FAMILY_DECORATIVE           4);/* Decorative                     */
(define-integrable PAN_FAMILY_PICTORIAL            5);/* Pictorial                      */

(define-integrable PAN_SERIF_COVE                  2);/* Cove                           */
(define-integrable PAN_SERIF_OBTUSE_COVE           3);/* Obtuse Cove                    */
(define-integrable PAN_SERIF_SQUARE_COVE           4);/* Square Cove                    */
(define-integrable PAN_SERIF_OBTUSE_SQUARE_COVE    5);/* Obtuse Square Cove             */
(define-integrable PAN_SERIF_SQUARE                6);/* Square                         */
(define-integrable PAN_SERIF_THIN                  7);/* Thin                           */
(define-integrable PAN_SERIF_BONE                  8);/* Bone                           */
(define-integrable PAN_SERIF_EXAGGERATED           9);/* Exaggerated                    */
(define-integrable PAN_SERIF_TRIANGLE             10);/* Triangle                       */
(define-integrable PAN_SERIF_NORMAL_SANS          11);/* Normal Sans                    */
(define-integrable PAN_SERIF_OBTUSE_SANS          12);/* Obtuse Sans                    */
(define-integrable PAN_SERIF_PERP_SANS            13);/* Prep Sans                      */
(define-integrable PAN_SERIF_FLARED               14);/* Flared                         */
(define-integrable PAN_SERIF_ROUNDED              15);/* Rounded                        */

(define-integrable PAN_WEIGHT_VERY_LIGHT           2);/* Very Light                     */
(define-integrable PAN_WEIGHT_LIGHT                3);/* Light                          */
(define-integrable PAN_WEIGHT_THIN                 4);/* Thin                           */
(define-integrable PAN_WEIGHT_BOOK                 5);/* Book                           */
(define-integrable PAN_WEIGHT_MEDIUM               6);/* Medium                         */
(define-integrable PAN_WEIGHT_DEMI                 7);/* Demi                           */
(define-integrable PAN_WEIGHT_BOLD                 8);/* Bold                           */
(define-integrable PAN_WEIGHT_HEAVY                9);/* Heavy                          */
(define-integrable PAN_WEIGHT_BLACK               10);/* Black                          */
(define-integrable PAN_WEIGHT_NORD                11);/* Nord                           */

(define-integrable PAN_PROP_OLD_STYLE              2);/* Old Style                      */
(define-integrable PAN_PROP_MODERN                 3);/* Modern                         */
(define-integrable PAN_PROP_EVEN_WIDTH             4);/* Even Width                     */
(define-integrable PAN_PROP_EXPANDED               5);/* Expanded                       */
(define-integrable PAN_PROP_CONDENSED              6);/* Condensed                      */
(define-integrable PAN_PROP_VERY_EXPANDED          7);/* Very Expanded                  */
(define-integrable PAN_PROP_VERY_CONDENSED         8);/* Very Condensed                 */
(define-integrable PAN_PROP_MONOSPACED             9);/* Monospaced                     */

(define-integrable PAN_CONTRAST_NONE               2);/* None                           */
(define-integrable PAN_CONTRAST_VERY_LOW           3);/* Very Low                       */
(define-integrable PAN_CONTRAST_LOW                4);/* Low                            */
(define-integrable PAN_CONTRAST_MEDIUM_LOW         5);/* Medium Low                     */
(define-integrable PAN_CONTRAST_MEDIUM             6);/* Medium                         */
(define-integrable PAN_CONTRAST_MEDIUM_HIGH        7);/* Mediim High                    */
(define-integrable PAN_CONTRAST_HIGH               8);/* High                           */
(define-integrable PAN_CONTRAST_VERY_HIGH          9);/* Very High                      */

(define-integrable PAN_STROKE_GRADUAL_DIAG         2);/* Gradual/Diagonal               */
(define-integrable PAN_STROKE_GRADUAL_TRAN         3);/* Gradual/Transitional           */
(define-integrable PAN_STROKE_GRADUAL_VERT         4);/* Gradual/Vertical               */
(define-integrable PAN_STROKE_GRADUAL_HORZ         5);/* Gradual/Horizontal             */
(define-integrable PAN_STROKE_RAPID_VERT           6);/* Rapid/Vertical                 */
(define-integrable PAN_STROKE_RAPID_HORZ           7);/* Rapid/Horizontal               */
(define-integrable PAN_STROKE_INSTANT_VERT         8);/* Instant/Vertical               */

(define-integrable PAN_STRAIGHT_ARMS_HORZ          2);/* Straight Arms/Horizontal       */
(define-integrable PAN_STRAIGHT_ARMS_WEDGE         3);/* Straight Arms/Wedge            */
(define-integrable PAN_STRAIGHT_ARMS_VERT          4);/* Straight Arms/Vertical         */
(define-integrable PAN_STRAIGHT_ARMS_SINGLE_SERIF  5);/* Straight Arms/Single-Serif     */
(define-integrable PAN_STRAIGHT_ARMS_DOUBLE_SERIF  6);/* Straight Arms/Double-Serif     */
(define-integrable PAN_BENT_ARMS_HORZ              7);/* Non-Straight Arms/Horizontal   */
(define-integrable PAN_BENT_ARMS_WEDGE             8);/* Non-Straight Arms/Wedge        */
(define-integrable PAN_BENT_ARMS_VERT              9);/* Non-Straight Arms/Vertical     */
(define-integrable PAN_BENT_ARMS_SINGLE_SERIF     10);/* Non-Straight Arms/Single-Serif */
(define-integrable PAN_BENT_ARMS_DOUBLE_SERIF     11);/* Non-Straight Arms/Double-Serif */

(define-integrable PAN_LETT_NORMAL_CONTACT         2);/* Normal/Contact                 */
(define-integrable PAN_LETT_NORMAL_WEIGHTED        3);/* Normal/Weighted                */
(define-integrable PAN_LETT_NORMAL_BOXED           4);/* Normal/Boxed                   */
(define-integrable PAN_LETT_NORMAL_FLATTENED       5);/* Normal/Flattened               */
(define-integrable PAN_LETT_NORMAL_ROUNDED         6);/* Normal/Rounded                 */
(define-integrable PAN_LETT_NORMAL_OFF_CENTER      7);/* Normal/Off Center              */
(define-integrable PAN_LETT_NORMAL_SQUARE          8);/* Normal/Square                  */
(define-integrable PAN_LETT_OBLIQUE_CONTACT        9);/* Oblique/Contact                */
(define-integrable PAN_LETT_OBLIQUE_WEIGHTED      10);/* Oblique/Weighted               */
(define-integrable PAN_LETT_OBLIQUE_BOXED         11);/* Oblique/Boxed                  */
(define-integrable PAN_LETT_OBLIQUE_FLATTENED     12);/* Oblique/Flattened              */
(define-integrable PAN_LETT_OBLIQUE_ROUNDED       13);/* Oblique/Rounded                */
(define-integrable PAN_LETT_OBLIQUE_OFF_CENTER    14);/* Oblique/Off Center             */
(define-integrable PAN_LETT_OBLIQUE_SQUARE        15);/* Oblique/Square                 */

(define-integrable PAN_MIDLINE_STANDARD_TRIMMED    2);/* Standard/Trimmed               */
(define-integrable PAN_MIDLINE_STANDARD_POINTED    3);/* Standard/Pointed               */
(define-integrable PAN_MIDLINE_STANDARD_SERIFED    4);/* Standard/Serifed               */
(define-integrable PAN_MIDLINE_HIGH_TRIMMED        5);/* High/Trimmed                   */
(define-integrable PAN_MIDLINE_HIGH_POINTED        6);/* High/Pointed                   */
(define-integrable PAN_MIDLINE_HIGH_SERIFED        7);/* High/Serifed                   */
(define-integrable PAN_MIDLINE_CONSTANT_TRIMMED    8);/* Constant/Trimmed               */
(define-integrable PAN_MIDLINE_CONSTANT_POINTED    9);/* Constant/Pointed               */
(define-integrable PAN_MIDLINE_CONSTANT_SERIFED   10);/* Constant/Serifed               */
(define-integrable PAN_MIDLINE_LOW_TRIMMED        11);/* Low/Trimmed                    */
(define-integrable PAN_MIDLINE_LOW_POINTED        12);/* Low/Pointed                    */
(define-integrable PAN_MIDLINE_LOW_SERIFED        13);/* Low/Serifed                    */

(define-integrable PAN_XHEIGHT_CONSTANT_SMALL      2);/* Constant/Small                 */
(define-integrable PAN_XHEIGHT_CONSTANT_STD        3);/* Constant/Standard              */
(define-integrable PAN_XHEIGHT_CONSTANT_LARGE      4);/* Constant/Large                 */
(define-integrable PAN_XHEIGHT_DUCKING_SMALL       5);/* Ducking/Small                  */
(define-integrable PAN_XHEIGHT_DUCKING_STD         6);/* Ducking/Standard               */
(define-integrable PAN_XHEIGHT_DUCKING_LARGE       7);/* Ducking/Large                  */


(define-integrable ELF_VENDOR_SIZE     4)

;;/* The extended logical font       */
;;/* An extension of the ENUMLOGFONT */

;;typedef struct tagEXTLOGFONTA {
;;    LOGFONTA    elfLogFont;
;;    BYTE        elfFullName[LF_FULLFACESIZE];
;;    BYTE        elfStyle[LF_FACESIZE];
;;    DWORD       elfVersion;     /* 0 for the first release of NT */
;;    DWORD       elfStyleSize;
;;    DWORD       elfMatch;
;;    DWORD       elfReserved;
;;    BYTE        elfVendorId[ELF_VENDOR_SIZE];
;;    DWORD       elfCulture;     /* 0 for Latin                   */
;;    PANOSE      elfPanose;
;;} EXTLOGFONTA, *PEXTLOGFONTA, NEAR *NPEXTLOGFONTA, FAR *LPEXTLOGFONTA;
;;typedef struct tagEXTLOGFONTW {
;;    LOGFONTW    elfLogFont;
;;    WCHAR       elfFullName[LF_FULLFACESIZE];
;;    WCHAR       elfStyle[LF_FACESIZE];
;;    DWORD       elfVersion;     /* 0 for the first release of NT */
;;    DWORD       elfStyleSize;
;;    DWORD       elfMatch;
;;    DWORD       elfReserved;
;;    BYTE        elfVendorId[ELF_VENDOR_SIZE];
;;    DWORD       elfCulture;     /* 0 for Latin                   */
;;    PANOSE      elfPanose;
;;} EXTLOGFONTW, *PEXTLOGFONTW, NEAR *NPEXTLOGFONTW, FAR *LPEXTLOGFONTW;
;;#ifdef UNICODE
;;typedef EXTLOGFONTW EXTLOGFONT;
;;typedef PEXTLOGFONTW PEXTLOGFONT;
;;typedef NPEXTLOGFONTW NPEXTLOGFONT;
;;typedef LPEXTLOGFONTW LPEXTLOGFONT;
;;#else
;;typedef EXTLOGFONTA EXTLOGFONT;
;;typedef PEXTLOGFONTA PEXTLOGFONT;
;;typedef NPEXTLOGFONTA NPEXTLOGFONT;
;;typedef LPEXTLOGFONTA LPEXTLOGFONT;
;;#endif // UNICODE
;;
;;
(define-integrable ELF_VERSION         0)
(define-integrable ELF_CULTURE_LATIN   0)
;;
;;/* EnumFonts Masks */
(define-integrable RASTER_FONTTYPE     #x0001)
(define-integrable DEVICE_FONTTYPE     #x002)
(define-integrable TRUETYPE_FONTTYPE   #x004)
;;
;;#define RGB(r,g,b)          ((COLORREF)(((BYTE)(r)|((WORD)(g)<<8))|(((DWORD)(BYTE)(b))<<16)))
;;#define PALETTERGB(r,g,b)   (0x02000000 | RGB(r,g,b))
;;#define PALETTEINDEX(i)     ((COLORREF)(0x01000000 | (DWORD)(WORD)(i)))
;;
;;/* palette entry flags */
;;
(define-integrable PC_RESERVED     #x01   );/* palette index used for animation */
(define-integrable PC_EXPLICIT     #x02   );/* palette index is explicit to device */
(define-integrable PC_NOCOLLAPSE   #x04   );/* do not match color to system palette */

;;#define GetRValue(rgb)      ((BYTE)(rgb))
;;#define GetGValue(rgb)      ((BYTE)(((WORD)(rgb)) >> 8))
;;#define GetBValue(rgb)      ((BYTE)((rgb)>>16))
;;
;;/* Background Modes */
(define-integrable TRANSPARENT         1)
(define-integrable OPAQUE              2)
(define-integrable BKMODE_LAST         2)

;;/* Graphics Modes */

(define-integrable GM_COMPATIBLE       1)
(define-integrable GM_ADVANCED         2)
(define-integrable GM_LAST             2)

;;/* PolyDraw and GetPath point types */
(define-integrable PT_CLOSEFIGURE      #x01)
(define-integrable PT_LINETO           #x02)
(define-integrable PT_BEZIERTO         #x04)
(define-integrable PT_MOVETO           #x06)

;;/* Mapping Modes */
(define-integrable MM_TEXT             1)
(define-integrable MM_LOMETRIC         2)
(define-integrable MM_HIMETRIC         3)
(define-integrable MM_LOENGLISH        4)
(define-integrable MM_HIENGLISH        5)
(define-integrable MM_TWIPS            6)
(define-integrable MM_ISOTROPIC        7)
(define-integrable MM_ANISOTROPIC      8)

;;/* Min and Max Mapping Mode values */
(define-integrable MM_MIN              MM_TEXT)
(define-integrable MM_MAX              MM_ANISOTROPIC)
(define-integrable MM_MAX_FIXEDSCALE   MM_TWIPS)

;;/* Coordinate Modes */
(define-integrable ABSOLUTE            1)
(define-integrable RELATIVE            2)

;;/* Stock Logical Objects */
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

;;/* Brush Styles */
(define-integrable BS_SOLID            0)
(define-integrable BS_NULL             1)
(define-integrable BS_HOLLOW           BS_NULL)
(define-integrable BS_HATCHED          2)
(define-integrable BS_PATTERN          3)
(define-integrable BS_INDEXED          4)
(define-integrable BS_DIBPATTERN       5)
(define-integrable BS_DIBPATTERNPT     6)
(define-integrable BS_PATTERN8X8       7)

;;/* Hatch Styles */
(define-integrable HS_HORIZONTAL       0)       ;/* ----- */
(define-integrable HS_VERTICAL         1)       ;/* ||||| */
(define-integrable HS_FDIAGONAL        2)       ;/* \\\\\ */
(define-integrable HS_BDIAGONAL        3)       ;/* ///// */
(define-integrable HS_CROSS            4)       ;/* +++++ */
(define-integrable HS_DIAGCROSS        5)       ;/* xxxxx */
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
(define-integrable HS_SOLIDCLR	    19)
(define-integrable HS_DITHEREDCLR	    20)
(define-integrable HS_SOLIDTEXTCLR     21)
(define-integrable HS_DITHEREDTEXTCLR  22)
(define-integrable HS_SOLIDBKCLR	    23)
(define-integrable HS_DITHEREDBKCLR    24)
(define-integrable HS_API_MAX	    25)

;;/* Pen Styles */
(define-integrable PS_SOLID            0)
(define-integrable PS_DASH             1)       ;/* -------  */
(define-integrable PS_DOT              2)       ;/* .......  */
(define-integrable PS_DASHDOT          3)       ;/* _._._._  */
(define-integrable PS_DASHDOTDOT       4)       ;/* _.._.._  */
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

;;/* Device Parameters for GetDeviceCaps() */
(define-integrable DRIVERVERSION 0)     ;/* Device driver version                    */
(define-integrable TECHNOLOGY    2)     ;/* Device classification                    */
(define-integrable HORZSIZE      4)     ;/* Horizontal size in millimeters           */
(define-integrable VERTSIZE      6)     ;/* Vertical size in millimeters             */
(define-integrable HORZRES       8)     ;/* Horizontal width in pixels               */
(define-integrable VERTRES       10)    ;/* Vertical width in pixels                 */
(define-integrable BITSPIXEL     12)    ;/* Number of bits per pixel                 */
(define-integrable PLANES        14)    ;/* Number of planes                         */
(define-integrable NUMBRUSHES    16)    ;/* Number of brushes the device has         */
(define-integrable NUMPENS       18)    ;/* Number of pens the device has            */
(define-integrable NUMMARKERS    20)    ;/* Number of markers the device has         */
(define-integrable NUMFONTS      22)    ;/* Number of fonts the device has           */
(define-integrable NUMCOLORS     24)    ;/* Number of colors the device supports     */
(define-integrable PDEVICESIZE   26)    ;/* Size required for device descriptor      */
(define-integrable CURVECAPS     28)    ;/* Curve capabilities                       */
(define-integrable LINECAPS      30)    ;/* Line capabilities                        */
(define-integrable POLYGONALCAPS 32)    ;/* Polygonal capabilities                   */
(define-integrable TEXTCAPS      34)    ;/* Text capabilities                        */
(define-integrable CLIPCAPS      36)    ;/* Clipping capabilities                    */
(define-integrable RASTERCAPS    38)    ;/* Bitblt capabilities                      */
(define-integrable ASPECTX       40)    ;/* Length of the X leg                      */
(define-integrable ASPECTY       42)    ;/* Length of the Y leg                      */
(define-integrable ASPECTXY      44)    ;/* Length of the hypotenuse                 */

(define-integrable LOGPIXELSX    88)    ;/* Logical pixels/inch in X                 */
(define-integrable LOGPIXELSY    90)    ;/* Logical pixels/inch in Y                 */

(define-integrable SIZEPALETTE  104)    ;/* Number of entries in physical palette    */
(define-integrable NUMRESERVED  106)    ;/* Number of reserved entries in palette    */
(define-integrable COLORRES     108)    ;/* Actual color resolution                  */


;;// Printing related DeviceCaps. These replace the appropriate Escapes

(define-integrable PHYSICALWIDTH   110) ;// Physical Width in device units
(define-integrable PHYSICALHEIGHT  111) ;// Physical Height in device units
(define-integrable PHYSICALOFFSETX 112) ;// Physical Printable Area x margin
(define-integrable PHYSICALOFFSETY 113) ;// Physical Printable Area y margin
(define-integrable SCALINGFACTORX  114) ;// Scaling factor x
(define-integrable SCALINGFACTORY  115) ;// Scaling factor y

;;#ifndef NOGDICAPMASKS
;;
;;;/* Device Capability Masks: */
;;
;;/* Device Technologies */
(define-integrable DT_PLOTTER          0)   ;/* Vector plotter                   */
(define-integrable DT_RASDISPLAY       1)   ;/* Raster display                   */
(define-integrable DT_RASPRINTER       2)   ;/* Raster printer                   */
(define-integrable DT_RASCAMERA        3)   ;/* Raster camera                    */
(define-integrable DT_CHARSTREAM       4)   ;/* Character-stream, PLP            */
(define-integrable DT_METAFILE         5)   ;/* Metafile, VDM                    */
(define-integrable DT_DISPFILE         6)   ;/* Display-file                     */
;;
;;/* Curve Capabilities */
(define-integrable CC_NONE             0)   ;/* Curves not supported             */
(define-integrable CC_CIRCLES          1)   ;/* Can do circles                   */
(define-integrable CC_PIE              2)   ;/* Can do pie wedges                */
(define-integrable CC_CHORD            4)   ;/* Can do chord arcs                */
(define-integrable CC_ELLIPSES         8)   ;/* Can do ellipese                  */
(define-integrable CC_WIDE             16)  ;/* Can do wide lines                */
(define-integrable CC_STYLED           32)  ;/* Can do styled lines              */
(define-integrable CC_WIDESTYLED       64)  ;/* Can do wide styled lines         */
(define-integrable CC_INTERIORS        128) ;/* Can do interiors                 */
(define-integrable CC_ROUNDRECT        256) ;/*                                  */
;;
;;/* Line Capabilities */
(define-integrable LC_NONE             0)   ;/* Lines not supported              */
(define-integrable LC_POLYLINE         2)   ;/* Can do polylines                 */
(define-integrable LC_MARKER           4)   ;/* Can do markers                   */
(define-integrable LC_POLYMARKER       8)   ;/* Can do polymarkers               */
(define-integrable LC_WIDE             16)  ;/* Can do wide lines                */
(define-integrable LC_STYLED           32)  ;/* Can do styled lines              */
(define-integrable LC_WIDESTYLED       64)  ;/* Can do wide styled lines         */
(define-integrable LC_INTERIORS        128) ;/* Can do interiors                 */
;;
;;/* Polygonal Capabilities */
(define-integrable PC_NONE             0)   ;/* Polygonals not supported         */
(define-integrable PC_POLYGON          1)   ;/* Can do polygons                  */
(define-integrable PC_RECTANGLE        2)   ;/* Can do rectangles                */
(define-integrable PC_WINDPOLYGON      4)   ;/* Can do winding polygons          */
(define-integrable PC_TRAPEZOID        4)   ;/* Can do trapezoids                */
(define-integrable PC_SCANLINE         8)   ;/* Can do scanlines                 */
(define-integrable PC_WIDE             16)  ;/* Can do wide borders              */
(define-integrable PC_STYLED           32)  ;/* Can do styled borders            */
(define-integrable PC_WIDESTYLED       64)  ;/* Can do wide styled borders       */
(define-integrable PC_INTERIORS        128) ;/* Can do interiors                 */
;;
;;/* Polygonal Capabilities */
(define-integrable CP_NONE             0)   ;/* No clipping of output            */
(define-integrable CP_RECTANGLE        1)   ;/* Output clipped to rects          */
(define-integrable CP_REGION           2)   ;/*                                  */

;;/* Text Capabilities */
(define-integrable TC_OP_CHARACTER     #x00000001)  ;/* Can do OutputPrecision   CHARACTER      */
(define-integrable TC_OP_STROKE        #x00000002)  ;/* Can do OutputPrecision   STROKE         */
(define-integrable TC_CP_STROKE        #x00000004)  ;/* Can do ClipPrecision     STROKE         */
(define-integrable TC_CR_90            #x00000008)  ;/* Can do CharRotAbility    90             */
(define-integrable TC_CR_ANY           #x00000010)  ;/* Can do CharRotAbility    ANY            */
(define-integrable TC_SF_X_YINDEP      #x00000020)  ;/* Can do ScaleFreedom      X_YINDEPENDENT */
(define-integrable TC_SA_DOUBLE        #x00000040)  ;/* Can do ScaleAbility      DOUBLE         */
(define-integrable TC_SA_INTEGER       #x00000080)  ;/* Can do ScaleAbility      INTEGER        */
(define-integrable TC_SA_CONTIN        #x00000100)  ;/* Can do ScaleAbility      CONTINUOUS     */
(define-integrable TC_EA_DOUBLE        #x00000200)  ;/* Can do EmboldenAbility   DOUBLE         */
(define-integrable TC_IA_ABLE          #x00000400)  ;/* Can do ItalisizeAbility  ABLE           */
(define-integrable TC_UA_ABLE          #x00000800)  ;/* Can do UnderlineAbility  ABLE           */
(define-integrable TC_SO_ABLE          #x00001000)  ;/* Can do StrikeOutAbility  ABLE           */
(define-integrable TC_RA_ABLE          #x00002000)  ;/* Can do RasterFontAble    ABLE           */
(define-integrable TC_VA_ABLE          #x00004000)  ;/* Can do VectorFontAble    ABLE           */
(define-integrable TC_RESERVED         #x00008000)
(define-integrable TC_SCROLLBLT        #x00010000)  ;/* do text scroll with blt                 */
;;
;;#endif /* NOGDICAPMASKS */
;;
;;/* Raster Capabilities */
;;#define RC_NONE
(define-integrable RC_BITBLT           1)       ;/* Can do standard BLT.             */
(define-integrable RC_BANDING          2)       ;/* Device requires banding support  */
(define-integrable RC_SCALING          4)       ;/* Device requires scaling support  */
(define-integrable RC_BITMAP64         8)       ;/* Device can support >64K bitmap   */
(define-integrable RC_GDI20_OUTPUT     #x0010)      ;/* has 2.0 output calls         */
(define-integrable RC_GDI20_STATE      #x0020)
(define-integrable RC_SAVEBITMAP       #x0040)
(define-integrable RC_DI_BITMAP        #x0080)      ;/* supports DIB to memory       */
(define-integrable RC_PALETTE          #x0100)      ;/* supports a palette           */
(define-integrable RC_DIBTODEV         #x0200)      ;/* supports DIBitsToDevice      */
(define-integrable RC_BIGFONT          #x0400)      ;/* supports >64K fonts          */
(define-integrable RC_STRETCHBLT       #x0800)      ;/* supports StretchBlt          */
(define-integrable RC_FLOODFILL        #x1000)      ;/* supports FloodFill           */
(define-integrable RC_STRETCHDIB       #x2000)      ;/* supports StretchDIBits       */
(define-integrable RC_OP_DX_OUTPUT     #x4000)
(define-integrable RC_DEVBITS          #x8000)
;;
;;/* DIB color table identifiers */
;;
(define-integrable DIB_RGB_COLORS      0) ;/* color table in RGBs */
(define-integrable DIB_PAL_COLORS      1) ;/* color table in palette indices */
(define-integrable DIB_PAL_INDICES     2) ;/* No color table indices into surf palette */
(define-integrable DIB_PAL_PHYSINDICES 2) ;/* No color table indices into surf palette */
(define-integrable DIB_PAL_LOGINDICES  4) ;/* No color table indices into DC palette */
;;
;;/* constants for Get/SetSystemPaletteUse() */
;;
(define-integrable SYSPAL_ERROR    0)
(define-integrable SYSPAL_STATIC   1)
(define-integrable SYSPAL_NOSTATIC 2)
;;
;;/* constants for CreateDIBitmap */
(define-integrable CBM_CREATEDIB   #x02)   ;/* create DIB bitmap */
(define-integrable CBM_INIT        #x04)   ;/* initialize bitmap */
;;
;;/* ExtFloodFill style flags */
(define-integrable  FLOODFILLBORDER   0)
(define-integrable  FLOODFILLSURFACE  1)
;;
;;/* size of a device name string */
(define-integrable CCHDEVICENAME 32)
;;
;;/* size of a form name string */
(define-integrable CCHFORMNAME 32)
;;
;;typedef struct _devicemodeA {
;;    BYTE   dmDeviceName[CCHDEVICENAME];
;;    WORD dmSpecVersion;
;;    WORD dmDriverVersion;
;;    WORD dmSize;
;;    WORD dmDriverExtra;
;;    DWORD dmFields;
;;    short dmOrientation;
;;    short dmPaperSize;
;;    short dmPaperLength;
;;    short dmPaperWidth;
;;    short dmScale;
;;    short dmCopies;
;;    short dmDefaultSource;
;;    short dmPrintQuality;
;;    short dmColor;
;;    short dmDuplex;
;;    short dmYResolution;
;;    short dmTTOption;
;;    short dmCollate;
;;    BYTE   dmFormName[CCHFORMNAME];
;;    WORD   dmUnusedPadding;
;;    DWORD  dmBitsPerPel;
;;    DWORD  dmPelsWidth;
;;    DWORD  dmPelsHeight;
;;    DWORD  dmDisplayFlags;
;;    DWORD  dmDisplayFrequency;
;;} DEVMODEA, *PDEVMODEA, *NPDEVMODEA, *LPDEVMODEA;
;;typedef struct _devicemodeW {
;;    WCHAR  dmDeviceName[CCHDEVICENAME];
;;    WORD dmSpecVersion;
;;    WORD dmDriverVersion;
;;    WORD dmSize;
;;    WORD dmDriverExtra;
;;    DWORD dmFields;
;;    short dmOrientation;
;;    short dmPaperSize;
;;    short dmPaperLength;
;;    short dmPaperWidth;
;;    short dmScale;
;;    short dmCopies;
;;    short dmDefaultSource;
;;    short dmPrintQuality;
;;    short dmColor;
;;    short dmDuplex;
;;    short dmYResolution;
;;    short dmTTOption;
;;    short dmCollate;
;;    WCHAR  dmFormName[CCHFORMNAME];
;;    WORD   dmUnusedPadding;
;;    DWORD  dmBitsPerPel;
;;    DWORD  dmPelsWidth;
;;    DWORD  dmPelsHeight;
;;    DWORD  dmDisplayFlags;
;;    DWORD  dmDisplayFrequency;
;;} DEVMODEW, *PDEVMODEW, *NPDEVMODEW, *LPDEVMODEW;
;;#ifdef UNICODE
;;typedef DEVMODEW DEVMODE;
;;typedef PDEVMODEW PDEVMODE;
;;typedef NPDEVMODEW NPDEVMODE;
;;typedef LPDEVMODEW LPDEVMODE;
;;#else
;;typedef DEVMODEA DEVMODE;
;;typedef PDEVMODEA PDEVMODE;
;;typedef NPDEVMODEA NPDEVMODE;
;;typedef LPDEVMODEA LPDEVMODE;
;;#endif // UNICODE
;;
;;/* current version of specification */
(define-integrable DM_SPECVERSION #x320)
;;
;;/* field selection bits */
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
;;
;;/* orientation selections */
(define-integrable DMORIENT_PORTRAIT   1)
(define-integrable DMORIENT_LANDSCAPE  2)
;;
;;/* paper selections */
(define-integrable DMPAPER_LETTER      1)           ;/* Letter 8 1/2 x 11 in               */
(define-integrable DMPAPER_FIRST       DMPAPER_LETTER)
(define-integrable DMPAPER_LETTERSMALL 2)           ;/* Letter Small 8 1/2 x 11 in         */
(define-integrable DMPAPER_TABLOID     3)           ;/* Tabloid 11 x 17 in                 */
(define-integrable DMPAPER_LEDGER      4)           ;/* Ledger 17 x 11 in                  */
(define-integrable DMPAPER_LEGAL       5)           ;/* Legal 8 1/2 x 14 in                */
(define-integrable DMPAPER_STATEMENT   6)           ;/* Statement 5 1/2 x 8 1/2 in         */
(define-integrable DMPAPER_EXECUTIVE   7)           ;/* Executive 7 1/4 x 10 1/2 in        */
(define-integrable DMPAPER_A3          8)           ;/* A3 297 x 420 mm                    */
(define-integrable DMPAPER_A4          9)           ;/* A4 210 x 297 mm                    */
(define-integrable DMPAPER_A4SMALL     10)          ;/* A4 Small 210 x 297 mm              */
(define-integrable DMPAPER_A5          11)          ;/* A5 148 x 210 mm                    */
(define-integrable DMPAPER_B4          12)          ;/* B4 250 x 354                       */
(define-integrable DMPAPER_B5          13)          ;/* B5 182 x 257 mm                    */
(define-integrable DMPAPER_FOLIO       14)          ;/* Folio 8 1/2 x 13 in                */
(define-integrable DMPAPER_QUARTO      15)          ;/* Quarto 215 x 275 mm                */
(define-integrable DMPAPER_10X14       16)          ;/* 10x14 in                           */
(define-integrable DMPAPER_11X17       17)          ;/* 11x17 in                           */
(define-integrable DMPAPER_NOTE        18)          ;/* Note 8 1/2 x 11 in                 */
(define-integrable DMPAPER_ENV_9       19)          ;/* Envelope #9 3 7/8 x 8 7/8          */
(define-integrable DMPAPER_ENV_10      20)          ;/* Envelope #10 4 1/8 x 9 1/2         */
(define-integrable DMPAPER_ENV_11      21)          ;/* Envelope #11 4 1/2 x 10 3/8        */
(define-integrable DMPAPER_ENV_12      22)          ;/* Envelope #12 4 \276 x 11           */
(define-integrable DMPAPER_ENV_14      23)          ;/* Envelope #14 5 x 11 1/2            */
(define-integrable DMPAPER_CSHEET      24)          ;/* C size sheet                       */
(define-integrable DMPAPER_DSHEET      25)          ;/* D size sheet                       */
(define-integrable DMPAPER_ESHEET      26)          ;/* E size sheet                       */
(define-integrable DMPAPER_ENV_DL      27)          ;/* Envelope DL 110 x 220mm            */
(define-integrable DMPAPER_ENV_C5      28)          ;/* Envelope C5 162 x 229 mm           */
(define-integrable DMPAPER_ENV_C3      29)          ;/* Envelope C3  324 x 458 mm          */
(define-integrable DMPAPER_ENV_C4      30)          ;/* Envelope C4  229 x 324 mm          */
(define-integrable DMPAPER_ENV_C6      31)          ;/* Envelope C6  114 x 162 mm          */
(define-integrable DMPAPER_ENV_C65     32)          ;/* Envelope C65 114 x 229 mm          */
(define-integrable DMPAPER_ENV_B4      33)          ;/* Envelope B4  250 x 353 mm          */
(define-integrable DMPAPER_ENV_B5      34)          ;/* Envelope B5  176 x 250 mm          */
(define-integrable DMPAPER_ENV_B6      35)          ;/* Envelope B6  176 x 125 mm          */
(define-integrable DMPAPER_ENV_ITALY   36)          ;/* Envelope 110 x 230 mm              */
(define-integrable DMPAPER_ENV_MONARCH 37)          ;/* Envelope Monarch 3.875 x 7.5 in    */
(define-integrable DMPAPER_ENV_PERSONAL 38)         ;/* 6 3/4 Envelope 3 5/8 x 6 1/2 in    */
(define-integrable DMPAPER_FANFOLD_US  39)          ;/* US Std Fanfold 14 7/8 x 11 in      */
(define-integrable DMPAPER_FANFOLD_STD_GERMAN  40)  ;/* German Std Fanfold 8 1/2 x 12 in   */
(define-integrable DMPAPER_FANFOLD_LGL_GERMAN  41)  ;/* German Legal Fanfold 8 1/2 x 13 in */

(define-integrable DMPAPER_LAST        DMPAPER_FANFOLD_LGL_GERMAN)

(define-integrable DMPAPER_USER        256)
;;
;;/* bin selections */
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

(define-integrable DMBIN_USER          256)     ;/* device specific bins start here */
;;
;;/* print qualities */
(define-integrable DMRES_DRAFT         -1)
(define-integrable DMRES_LOW           -2)
(define-integrable DMRES_MEDIUM        -3)
(define-integrable DMRES_HIGH          -4)
;;
;;/* color enable/disable for color printers */
(define-integrable DMCOLOR_MONOCHROME  1)
(define-integrable DMCOLOR_COLOR       2)
;;
;;/* duplex enable */
(define-integrable DMDUP_SIMPLEX    1)
(define-integrable DMDUP_VERTICAL   2)
(define-integrable DMDUP_HORIZONTAL 3)
;;
;;/* TrueType options */
(define-integrable DMTT_BITMAP     1)       ;/* print TT fonts as graphics */
(define-integrable DMTT_DOWNLOAD   2)       ;/* download TT fonts as soft fonts */
(define-integrable DMTT_SUBDEV     3)       ;/* substitute device fonts for TT fonts */
;;
;;/* Collation selections */
(define-integrable DMCOLLATE_FALSE  0)
(define-integrable DMCOLLATE_TRUE   1)
;;
;;/* DEVMODE dmDisplayFlags flags */
;;
(define-integrable DM_GRAYSCALE  #x00000001)
(define-integrable DM_INTERLACED #x00000002)
;;
;;/* GetRegionData/ExtCreateRegion */
;;
(define-integrable RDH_RECTANGLES  1)
;;
;;typedef struct _RGNDATAHEADER {
;;    DWORD   dwSize;
;;    DWORD   iType;
;;    DWORD   nCount;
;;    DWORD   nRgnSize;
;;    RECT    rcBound;
;;} RGNDATAHEADER, *PRGNDATAHEADER;
;;
;;typedef struct _RGNDATA {
;;    RGNDATAHEADER   rdh;
;;    char            Buffer[1];
;;} RGNDATA, *PRGNDATA, NEAR *NPRGNDATA, FAR *LPRGNDATA;
;;
;;
;;typedef struct _ABC {
;;    int     abcA;
;;    UINT    abcB;
;;    int     abcC;
;;} ABC, *PABC, NEAR *NPABC, FAR *LPABC;
;;
;;typedef struct _ABCFLOAT {
;;    FLOAT   abcfA;
;;    FLOAT   abcfB;
;;    FLOAT   abcfC;
;;} ABCFLOAT, *PABCFLOAT, NEAR *NPABCFLOAT, FAR *LPABCFLOAT;
;;
;;#ifndef NOTEXTMETRIC
;;
;;typedef struct _OUTLINETEXTMETRICA {
;;    UINT    otmSize;
;;    TEXTMETRICA otmTextMetrics;
;;    BYTE    otmFiller;
;;    PANOSE  otmPanoseNumber;
;;    UINT    otmfsSelection;
;;    UINT    otmfsType;
;;    UINT    otmsCharSlopeRise;
;;    UINT    otmsCharSlopeRun;
;;    UINT    otmItalicAngle;
;;    UINT    otmEMSquare;
;;    UINT    otmAscent;
;;     int    otmDescent;
;;     int    otmLineGap;
;;    UINT    otmsCapEmHeight;
;;    UINT    otmsXHeight;
;;    RECT    otmrcFontBox;
;;     int    otmMacAscent;
;;     int    otmMacDescent;
;;    UINT    otmMacLineGap;
;;    UINT    otmusMinimumPPEM;
;;    POINT   otmptSubscriptSize;
;;    POINT   otmptSubscriptOffset;
;;    POINT   otmptSuperscriptSize;
;;    POINT   otmptSuperscriptOffset;
;;    UINT    otmsStrikeoutSize;
;;     int    otmsStrikeoutPosition;
;;     int    otmsUnderscoreSize;
;;    UINT    otmsUnderscorePosition;
;;    PSTR    otmpFamilyName;
;;    PSTR    otmpFaceName;
;;    PSTR    otmpStyleName;
;;    PSTR    otmpFullName;
;;} OUTLINETEXTMETRICA, *POUTLINETEXTMETRICA, NEAR *NPOUTLINETEXTMETRICA, FAR *LPOUTLINETEXTMETRICA;
;;typedef struct _OUTLINETEXTMETRICW {
;;    UINT    otmSize;
;;    TEXTMETRICW otmTextMetrics;
;;    BYTE    otmFiller;
;;    PANOSE  otmPanoseNumber;
;;    UINT    otmfsSelection;
;;    UINT    otmfsType;
;;    UINT    otmsCharSlopeRise;
;;    UINT    otmsCharSlopeRun;
;;    UINT    otmItalicAngle;
;;    UINT    otmEMSquare;
;;    UINT    otmAscent;
;;     int    otmDescent;
;;     int    otmLineGap;
;;    UINT    otmsCapEmHeight;
;;    UINT    otmsXHeight;
;;    RECT    otmrcFontBox;
;;     int    otmMacAscent;
;;     int    otmMacDescent;
;;    UINT    otmMacLineGap;
;;    UINT    otmusMinimumPPEM;
;;    POINT   otmptSubscriptSize;
;;    POINT   otmptSubscriptOffset;
;;    POINT   otmptSuperscriptSize;
;;    POINT   otmptSuperscriptOffset;
;;    UINT    otmsStrikeoutSize;
;;     int    otmsStrikeoutPosition;
;;     int    otmsUnderscoreSize;
;;    UINT    otmsUnderscorePosition;
;;    PSTR    otmpFamilyName;
;;    PSTR    otmpFaceName;
;;    PSTR    otmpStyleName;
;;    PSTR    otmpFullName;
;;} OUTLINETEXTMETRICW, *POUTLINETEXTMETRICW, NEAR *NPOUTLINETEXTMETRICW, FAR *LPOUTLINETEXTMETRICW;
;;#ifdef UNICODE
;;typedef OUTLINETEXTMETRICW OUTLINETEXTMETRIC;
;;typedef POUTLINETEXTMETRICW POUTLINETEXTMETRIC;
;;typedef NPOUTLINETEXTMETRICW NPOUTLINETEXTMETRIC;
;;typedef LPOUTLINETEXTMETRICW LPOUTLINETEXTMETRIC;
;;#else
;;typedef OUTLINETEXTMETRICA OUTLINETEXTMETRIC;
;;typedef POUTLINETEXTMETRICA POUTLINETEXTMETRIC;
;;typedef NPOUTLINETEXTMETRICA NPOUTLINETEXTMETRIC;
;;typedef LPOUTLINETEXTMETRICA LPOUTLINETEXTMETRIC;
;;#endif // UNICODE
;;
;;#endif /* NOTEXTMETRIC */
;;
;;
;;typedef struct tagPOLYTEXTA
;;{
;;    int       x;
;;    int       y;
;;    UINT      n;
;;    LPCSTR    lpstr;
;;    UINT      uiFlags;
;;    RECT      rcl;
;;    int      *pdx;
;;} POLYTEXTA, *PPOLYTEXTA, NEAR *NPPOLYTEXTA, FAR *LPPOLYTEXTA;
;;typedef struct tagPOLYTEXTW
;;{
;;    int       x;
;;    int       y;
;;    UINT      n;
;;    LPCWSTR   lpstr;
;;    UINT      uiFlags;
;;    RECT      rcl;
;;    int      *pdx;
;;} POLYTEXTW, *PPOLYTEXTW, NEAR *NPPOLYTEXTW, FAR *LPPOLYTEXTW;
;;#ifdef UNICODE
;;typedef POLYTEXTW POLYTEXT;
;;typedef PPOLYTEXTW PPOLYTEXT;
;;typedef NPPOLYTEXTW NPPOLYTEXT;
;;typedef LPPOLYTEXTW LPPOLYTEXT;
;;#else
;;typedef POLYTEXTA POLYTEXT;
;;typedef PPOLYTEXTA PPOLYTEXT;
;;typedef NPPOLYTEXTA NPPOLYTEXT;
;;typedef LPPOLYTEXTA LPPOLYTEXT;
;;#endif // UNICODE
;;
;;typedef struct _FIXED {
;;    WORD    fract;
;;    short   value;
;;} FIXED;
;;
;;
;;typedef struct _MAT2 {
;;     FIXED  eM11;
;;     FIXED  eM12;
;;     FIXED  eM21;
;;     FIXED  eM22;
;;} MAT2, FAR *LPMAT2;
;;
;;
;;
;;typedef struct _GLYPHMETRICS {
;;    UINT    gmBlackBoxX;
;;    UINT    gmBlackBoxY;
;;    POINT   gmptGlyphOrigin;
;;    short   gmCellIncX;
;;    short   gmCellIncY;
;;} GLYPHMETRICS, FAR *LPGLYPHMETRICS;
;;
;;//  GetGlyphOutline constants
;;
(define-integrable GGO_NONE           0)
(define-integrable GGO_BITMAP         1)
(define-integrable GGO_NATIVE         2)

(define-integrable TT_POLYGON_TYPE   24)

(define-integrable TT_PRIM_LINE       1)
(define-integrable TT_PRIM_QSPLINE    2)

;;typedef struct tagPOINTFX
;;{
;;    FIXED x;
;;    FIXED y;
;;} POINTFX, FAR* LPPOINTFX;
;;
;;typedef struct tagTTPOLYCURVE
;;{
;;    WORD    wType;
;;    WORD    cpfx;
;;    POINTFX apfx[1];
;;} TTPOLYCURVE, FAR* LPTTPOLYCURVE;
;;
;;typedef struct tagTTPOLYGONHEADER
;;{
;;    DWORD   cb;
;;    DWORD   dwType;
;;    POINTFX pfxStart;
;;} TTPOLYGONHEADER, FAR* LPTTPOLYGONHEADER;
;;
;;
;;
;;typedef struct _RASTERIZER_STATUS {
;;    short   nSize;
;;    short   wFlags;
;;    short   nLanguageID;
;;} RASTERIZER_STATUS, FAR *LPRASTERIZER_STATUS;
;;
;;/* bits defined in wFlags of RASTERIZER_STATUS */
(define-integrable TT_AVAILABLE    #x0001)
(define-integrable TT_ENABLED      #x0002)
;;
;;#ifdef STRICT
;;#if !defined(NOTEXTMETRIC)
;;typedef int (CALLBACK* OLDFONTENUMPROC)(CONST LOGFONT *, CONST TEXTMETRIC *, DWORD, LPARAM);
;;#else
;;typedef int (CALLBACK* OLDFONTENUMPROC)(CONST LOGFONT * ,CONST VOID *, DWORD, LPARAM);
;;#endif
;;typedef int (CALLBACK* GOBJENUMPROC)(LPVOID, LPARAM);
;;typedef VOID (CALLBACK* LINEDDAPROC)(int, int, LPARAM);
;;#else
;;typedef FARPROC OLDFONTENUMPROC;
;;typedef FARPROC GOBJENUMPROC;
;;typedef FARPROC LINEDDAPROC;
;;#endif
;;
;;typedef OLDFONTENUMPROC FONTENUMPROC;
;;
;;int WINAPI AddFontResourceA(LPCSTR);
;;int WINAPI AddFontResourceW(LPCWSTR);
;;#ifdef UNICODE
;;#define AddFontResource  AddFontResourceW
;;#else
;;#define AddFontResource  AddFontResourceA
;;#endif // !UNICODE
;;
;;int   WINAPI AddFontModule(HMODULE);
;;BOOL  WINAPI AnimatePalette(HPALETTE, UINT, UINT, CONST PALETTEENTRY *);
;;BOOL  WINAPI Arc(HDC, int, int, int, int, int, int, int, int);
;;BOOL  WINAPI BitBlt(HDC, int, int, int, int, HDC, int, int, DWORD);
;;BOOL  WINAPI CancelDC(HDC);
;;BOOL  WINAPI Chord(HDC, int, int, int, int, int, int, int, int);
;;HMETAFILE  WINAPI CloseMetaFile(HDC);
;;int     WINAPI CombineRgn(HRGN, HRGN, HRGN, int);
;;HMETAFILE WINAPI CopyMetaFileA(HMETAFILE, LPSTR);
;;HMETAFILE WINAPI CopyMetaFileW(HMETAFILE, LPWSTR);
;;#ifdef UNICODE
;;#define CopyMetaFile  CopyMetaFileW
;;#else
;;#define CopyMetaFile  CopyMetaFileA
;;#endif // !UNICODE
;;HBITMAP WINAPI CreateBitmap(int, int, UINT, UINT, CONST VOID *);
;;HBITMAP WINAPI CreateBitmapIndirect(LPBITMAP);
;;HBRUSH  WINAPI CreateBrushIndirect(LPLOGBRUSH);
;;HBITMAP WINAPI CreateCompatibleBitmap(HDC, int, int);
;;HBITMAP WINAPI CreateDiscardableBitmap(HDC, int, int);
;;HDC     WINAPI CreateCompatibleDC(HDC);
;;HDC     WINAPI CreateDCA(LPCSTR, LPCSTR , LPCSTR , CONST DEVMODEA FAR *);
;;HDC     WINAPI CreateDCW(LPCWSTR, LPCWSTR , LPCWSTR , CONST DEVMODEW FAR *);
;;#ifdef UNICODE
;;#define CreateDC  CreateDCW
;;#else
;;#define CreateDC  CreateDCA
;;#endif // !UNICODE
;;HBITMAP WINAPI CreateDIBitmap(HDC, LPBITMAPINFOHEADER, DWORD, CONST BYTE *, LPBITMAPINFO, UINT);
;;HBITMAP WINAPI CreateDIBSection(HDC, LPBITMAPINFO, DWORD, DWORD);
;;HBRUSH  WINAPI CreateDIBPatternBrush(HGLOBAL, UINT);
;;HBRUSH	WINAPI CreateDIBPatternBrushPt(LPVOID, UINT);
;;HRGN    WINAPI CreateEllipticRgn(int, int, int, int);
;;HRGN    WINAPI CreateEllipticRgnIndirect(CONST RECT *);
;;HFONT   WINAPI CreateFontIndirectA(CONST LOGFONTA *);
;;HFONT   WINAPI CreateFontIndirectW(CONST LOGFONTW *);
;;#ifdef UNICODE
;;#define CreateFontIndirect  CreateFontIndirectW
;;#else
;;#define CreateFontIndirect  CreateFontIndirectA
;;#endif // !UNICODE
;;HFONT   WINAPI CreateFontA(int, int, int, int, int, DWORD,
;;                             DWORD, DWORD, DWORD, DWORD, DWORD,
;;                             DWORD, DWORD, LPCSTR);
;;HFONT   WINAPI CreateFontW(int, int, int, int, int, DWORD,
;;                             DWORD, DWORD, DWORD, DWORD, DWORD,
;;                             DWORD, DWORD, LPCWSTR);
;;#ifdef UNICODE
;;#define CreateFont  CreateFontW
;;#else
;;#define CreateFont  CreateFontA
;;#endif // !UNICODE
;;
;;HBRUSH  WINAPI CreateHatchBrush(int, COLORREF);
;;HDC     WINAPI CreateICA(LPCSTR, LPCSTR , LPCSTR , CONST DEVMODEA FAR *);
;;HDC     WINAPI CreateICW(LPCWSTR, LPCWSTR , LPCWSTR , CONST DEVMODEW FAR *);
;;#ifdef UNICODE
;;#define CreateIC  CreateICW
;;#else
;;#define CreateIC  CreateICA
;;#endif // !UNICODE
;;HDC     WINAPI CreateMetaFileA(LPCSTR);
;;HDC     WINAPI CreateMetaFileW(LPCWSTR);
;;#ifdef UNICODE
;;#define CreateMetaFile  CreateMetaFileW
;;#else
;;#define CreateMetaFile  CreateMetaFileA
;;#endif // !UNICODE
;;HPALETTE WINAPI CreatePalette(CONST LOGPALETTE *);
;;HPEN    WINAPI CreatePen(int, int, COLORREF);
;;HPEN    WINAPI CreatePenIndirect(LPLOGPEN);
;;HRGN    WINAPI CreatePolyPolygonRgn(CONST POINT *, CONST INT *, int, int);
;;HBRUSH  WINAPI CreatePatternBrush(HBITMAP);
;;HRGN    WINAPI CreateRectRgn(int, int, int, int);
;;HRGN    WINAPI CreateRectRgnIndirect(CONST RECT *);
;;HRGN    WINAPI CreateRoundRectRgn(int, int, int, int, int, int);
;;BOOL    WINAPI CreateScalableFontResourceA(DWORD, LPCSTR, LPCSTR, LPSTR);
;;BOOL    WINAPI CreateScalableFontResourceW(DWORD, LPCWSTR, LPCWSTR, LPWSTR);
;;#ifdef UNICODE
;;#define CreateScalableFontResource  CreateScalableFontResourceW
;;#else
;;#define CreateScalableFontResource  CreateScalableFontResourceA
;;#endif // !UNICODE
;;HBRUSH  WINAPI CreateSolidBrush(COLORREF);
;;
;;BOOL WINAPI DeleteDC(HDC);
;;BOOL WINAPI DeleteMetaFile(HMETAFILE);
;;BOOL WINAPI DeleteObject(HGDIOBJ);
;;
;;/* define types of pointers to ExtDeviceMode() and DeviceCapabilities()
;; * functions for Win 3.1 compatibility
;; */
;;
;;typedef UINT   (CALLBACK* LPFNDEVMODE)(HWND, HMODULE, LPDEVMODE, LPSTR, LPSTR, LPDEVMODE, LPSTR, UINT);
;;
;;typedef DWORD  (CALLBACK* LPFNDEVCAPS)(LPSTR, LPSTR, UINT, LPSTR, LPDEVMODE);
;;
;;/* mode selections for the device mode function */
(define-integrable DM_UPDATE           1)
(define-integrable DM_COPY             2)
(define-integrable DM_PROMPT           4)
(define-integrable DM_MODIFY           8)

(define-integrable DM_IN_BUFFER        DM_MODIFY)
(define-integrable DM_IN_PROMPT        DM_PROMPT)
(define-integrable DM_OUT_BUFFER       DM_COPY)
(define-integrable DM_OUT_DEFAULT      DM_UPDATE)

;;/* device capabilities indices */
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

;;int  WINAPI DeviceCapabilitiesExA(LPCSTR, LPCSTR, LPCSTR, int,
;;                                  LPCSTR, LPDEVMODEA );
;;int  WINAPI DeviceCapabilitiesExW(LPCWSTR, LPCWSTR, LPCWSTR, int,
;;                                  LPCWSTR, LPDEVMODEW );
;;#ifdef UNICODE
;;#define DeviceCapabilitiesEx  DeviceCapabilitiesExW
;;#else
;;#define DeviceCapabilitiesEx  DeviceCapabilitiesExA
;;#endif // !UNICODE
;;BOOL  WINAPI Ellipse(HDC, int, int, int, int);
;;
;;int  WINAPI EnumFontFamiliesA(HDC, LPCSTR, FONTENUMPROC, LPARAM);
;;int  WINAPI EnumFontFamiliesW(HDC, LPCWSTR, FONTENUMPROC, LPARAM);
;;#ifdef UNICODE
;;#define EnumFontFamilies  EnumFontFamiliesW
;;#else
;;#define EnumFontFamilies  EnumFontFamiliesA
;;#endif // !UNICODE
;;int  WINAPI EnumFontsA(HDC, LPCSTR,  FONTENUMPROC, LPARAM);
;;int  WINAPI EnumFontsW(HDC, LPCWSTR,  FONTENUMPROC, LPARAM);
;;#ifdef UNICODE
;;#define EnumFonts  EnumFontsW
;;#else
;;#define EnumFonts  EnumFontsA
;;#endif // !UNICODE
;;
;;#ifdef STRICT
;;int  WINAPI EnumObjects(HDC, int, GOBJENUMPROC, LPARAM);
;;#else
;;int  WINAPI EnumObjects(HDC, int, GOBJENUMPROC, LPVOID);
;;#endif
;;
;;BOOL WINAPI EqualRgn(HRGN, HRGN);
;;int  WINAPI Escape(HDC, int, int, LPCSTR, LPVOID);
;;int  WINAPI ExtEscape(HDC, int, int, LPCSTR, int, LPSTR);
;;int  WINAPI DrawEscape(HDC, int, int, LPCSTR);
;;int  WINAPI ExcludeClipRect(HDC, int, int, int, int);
;;HRGN WINAPI ExtCreateRegion(LPXFORM, DWORD, LPRGNDATA);
;;BOOL  WINAPI ExtFloodFill(HDC, int, int, COLORREF, UINT);
;;BOOL   WINAPI FillRgn(HDC, HRGN, HBRUSH);
;;BOOL   WINAPI FloodFill(HDC, int, int, COLORREF);
;;BOOL   WINAPI FrameRgn(HDC, HRGN, HBRUSH, int, int);
;;int   WINAPI GetROP2(HDC);
;;BOOL  WINAPI GetAspectRatioFilterEx(HDC, LPSIZE);
;;COLORREF WINAPI GetBkColor(HDC);
;;int   WINAPI GetBkMode(HDC);
;;LONG  WINAPI GetBitmapBits(HBITMAP, LONG, LPVOID);
;;BOOL  WINAPI GetBitmapDimensionEx(HBITMAP, LPSIZE);
;;UINT  WINAPI GetBoundsRect(HDC, LPRECT, UINT);
;;
;;BOOL  WINAPI GetBrushOrgEx(HDC, LPPOINT);
;;
;;BOOL  WINAPI GetCharWidthA(HDC, UINT, UINT, LPINT);
;;BOOL  WINAPI GetCharWidthW(HDC, UINT, UINT, LPINT);
;;#ifdef UNICODE
;;#define GetCharWidth  GetCharWidthW
;;#else
;;#define GetCharWidth  GetCharWidthA
;;#endif // !UNICODE
;;BOOL  WINAPI GetCharWidth32A(HDC, UINT, UINT, LPINT);
;;BOOL  WINAPI GetCharWidth32W(HDC, UINT, UINT, LPINT);
;;#ifdef UNICODE
;;#define GetCharWidth32  GetCharWidth32W
;;#else
;;#define GetCharWidth32  GetCharWidth32A
;;#endif // !UNICODE
;;BOOL  APIENTRY GetCharWidthFloatA(HDC, UINT, UINT, PFLOAT);
;;BOOL  APIENTRY GetCharWidthFloatW(HDC, UINT, UINT, PFLOAT);
;;#ifdef UNICODE
;;#define GetCharWidthFloat  GetCharWidthFloatW
;;#else
;;#define GetCharWidthFloat  GetCharWidthFloatA
;;#endif // !UNICODE
;;
;;BOOL  APIENTRY GetCharABCWidthsA(HDC, UINT, UINT, LPABC);
;;BOOL  APIENTRY GetCharABCWidthsW(HDC, UINT, UINT, LPABC);
;;#ifdef UNICODE
;;#define GetCharABCWidths  GetCharABCWidthsW
;;#else
;;#define GetCharABCWidths  GetCharABCWidthsA
;;#endif // !UNICODE
;;BOOL  APIENTRY GetCharABCWidthsFloatA(HDC, UINT, UINT, LPABCFLOAT);
;;BOOL  APIENTRY GetCharABCWidthsFloatW(HDC, UINT, UINT, LPABCFLOAT);
;;#ifdef UNICODE
;;#define GetCharABCWidthsFloat  GetCharABCWidthsFloatW
;;#else
;;#define GetCharABCWidthsFloat  GetCharABCWidthsFloatA
;;#endif // !UNICODE
;;
;;int   WINAPI GetClipBox(HDC, LPRECT);
;;int   WINAPI GetClipRgn(HDC, HRGN);
;;int   WINAPI GetMetaRgn(HDC, HRGN);
;;HGDIOBJ WINAPI GetCurrentObject(HDC, UINT);
;;BOOL  WINAPI GetCurrentPositionEx(HDC, LPPOINT);
;;int   WINAPI GetDeviceCaps(HDC, int);
;;int   WINAPI GetDIBits(HDC, HBITMAP, UINT, UINT, LPVOID, LPBITMAPINFO, UINT);
;;DWORD WINAPI GetFontData(HDC, DWORD, DWORD, LPVOID, DWORD);
;;DWORD WINAPI GetGlyphOutline(HDC, UINT, UINT, LPGLYPHMETRICS, DWORD, LPVOID, CONST MAT2 *);
;;int   WINAPI GetGraphicsMode(HDC);
;;int   WINAPI GetMapMode(HDC);
;;UINT  WINAPI GetMetaFileBitsEx(HMETAFILE, UINT, LPVOID);
;;HMETAFILE   WINAPI GetMetaFileA(LPCSTR);
;;HMETAFILE   WINAPI GetMetaFileW(LPCWSTR);
;;#ifdef UNICODE
;;#define GetMetaFile  GetMetaFileW
;;#else
;;#define GetMetaFile  GetMetaFileA
;;#endif // !UNICODE
;;COLORREF WINAPI GetNearestColor(HDC, COLORREF);
;;UINT  WINAPI GetNearestPaletteIndex(HPALETTE, COLORREF);
;;DWORD WINAPI GetObjectType(HGDIOBJ h);
;;
;;#ifndef NOTEXTMETRIC
;;
;;UINT APIENTRY GetOutlineTextMetricsA(HDC, UINT, LPOUTLINETEXTMETRICA);
;;UINT APIENTRY GetOutlineTextMetricsW(HDC, UINT, LPOUTLINETEXTMETRICW);
;;#ifdef UNICODE
;;#define GetOutlineTextMetrics  GetOutlineTextMetricsW
;;#else
;;#define GetOutlineTextMetrics  GetOutlineTextMetricsA
;;#endif // !UNICODE
;;
;;#endif /* NOTEXTMETRIC */
;;
;;UINT  WINAPI GetPaletteEntries(HPALETTE, UINT, UINT, LPPALETTEENTRY);
;;COLORREF WINAPI GetPixel(HDC, int, int);
;;int   WINAPI GetPolyFillMode(HDC);
;;BOOL  WINAPI GetRasterizerCaps(LPRASTERIZER_STATUS, UINT);
;;DWORD WINAPI GetRegionData(HRGN, DWORD, LPRGNDATA);
;;int   WINAPI GetRgnBox(HRGN, LPRECT);
;;HGDIOBJ WINAPI GetStockObject(int);
;;int   WINAPI GetStretchBltMode(HDC);
;;UINT  WINAPI GetSystemPaletteEntries(HDC, UINT, UINT, LPPALETTEENTRY);
;;UINT  WINAPI GetSystemPaletteUse(HDC);
;;int   WINAPI GetTextCharacterExtra(HDC);
;;UINT  WINAPI GetTextAlign(HDC);
;;COLORREF WINAPI GetTextColor(HDC);
;;
;;BOOL  APIENTRY GetTextExtentPointA(
;;                    HDC,
;;                    LPCSTR,
;;                    int,
;;                    LPSIZE
;;                    );
;;BOOL  APIENTRY GetTextExtentPointW(
;;                    HDC,
;;                    LPCWSTR,
;;                    int,
;;                    LPSIZE
;;                    );
;;#ifdef UNICODE
;;#define GetTextExtentPoint  GetTextExtentPointW
;;#else
;;#define GetTextExtentPoint  GetTextExtentPointA
;;#endif // !UNICODE
;;
;;BOOL  APIENTRY GetTextExtentPoint32A(
;;                    HDC,
;;                    LPCSTR,
;;                    int,
;;                    LPSIZE
;;                    );
;;BOOL  APIENTRY GetTextExtentPoint32W(
;;                    HDC,
;;                    LPCWSTR,
;;                    int,
;;                    LPSIZE
;;                    );
;;#ifdef UNICODE
;;#define GetTextExtentPoint32  GetTextExtentPoint32W
;;#else
;;#define GetTextExtentPoint32  GetTextExtentPoint32A
;;#endif // !UNICODE
;;
;;BOOL  APIENTRY GetTextExtentExPointA(
;;                    HDC,
;;                    LPCSTR,
;;                    int,
;;                    int,
;;                    LPINT,
;;                    LPINT,
;;                    LPSIZE
;;                    );
;;BOOL  APIENTRY GetTextExtentExPointW(
;;                    HDC,
;;                    LPCWSTR,
;;                    int,
;;                    int,
;;                    LPINT,
;;                    LPINT,
;;                    LPSIZE
;;                    );
;;#ifdef UNICODE
;;#define GetTextExtentExPoint  GetTextExtentExPointW
;;#else
;;#define GetTextExtentExPoint  GetTextExtentExPointA
;;#endif // !UNICODE
;;
;;BOOL  WINAPI GetViewportExtEx(HDC, LPSIZE);
;;BOOL  WINAPI GetViewportOrgEx(HDC, LPPOINT);
;;BOOL  WINAPI GetWindowExtEx(HDC, LPSIZE);
;;BOOL  WINAPI GetWindowOrgEx(HDC, LPPOINT);
;;
;;int  WINAPI IntersectClipRect(HDC, int, int, int, int);
;;BOOL WINAPI InvertRgn(HDC, HRGN);
;;BOOL WINAPI LineDDA(int, int, int, int, LINEDDAPROC, LPARAM);
;;BOOL WINAPI LineTo(HDC, int, int);
;;BOOL WINAPI MaskBlt(HDC, int, int, int, int,
;;              HDC, int, int, HBITMAP, int, int, DWORD);
;;BOOL WINAPI PlgBlt(HDC, LPPOINT, HDC, int, int, int,
;;                     int, HBITMAP, int, int);
;;
;;int  WINAPI OffsetClipRgn(HDC, int, int);
;;int  WINAPI OffsetRgn(HRGN, int, int);
;;BOOL WINAPI PatBlt(HDC, int, int, int, int, DWORD);
;;BOOL WINAPI Pie(HDC, int, int, int, int, int, int, int, int);
;;BOOL WINAPI PlayMetaFile(HDC, HMETAFILE);
;;BOOL WINAPI PaintRgn(HDC, HRGN);
;;BOOL WINAPI PolyPolygon(HDC, CONST POINT *, LPINT, int);
;;BOOL WINAPI PtInRegion(HRGN, int, int);
;;BOOL WINAPI PtVisible(HDC, int, int);
;;BOOL WINAPI RectInRegion(HRGN, CONST RECT *);
;;BOOL WINAPI RectVisible(HDC, CONST RECT *);
;;BOOL WINAPI Rectangle(HDC, int, int, int, int);
;;BOOL WINAPI RestoreDC(HDC, int);
;;HDC  WINAPI ResetDCA(HDC, CONST DEVMODEA *);
;;HDC  WINAPI ResetDCW(HDC, CONST DEVMODEW *);
;;#ifdef UNICODE
;;#define ResetDC  ResetDCW
;;#else
;;#define ResetDC  ResetDCA
;;#endif // !UNICODE
;;UINT WINAPI RealizePalette(HDC);
;;BOOL WINAPI RemoveFontModule(HMODULE);
;;BOOL WINAPI RemoveFontResourceA(LPSTR);
;;BOOL WINAPI RemoveFontResourceW(LPWSTR);
;;#ifdef UNICODE
;;#define RemoveFontResource  RemoveFontResourceW
;;#else
;;#define RemoveFontResource  RemoveFontResourceA
;;#endif // !UNICODE
;;BOOL  WINAPI RoundRect(HDC, int, int, int, int, int, int);
;;BOOL WINAPI ResizePalette(HPALETTE, UINT);
;;
;;int  WINAPI SaveDC(HDC);
;;int  WINAPI SelectClipRgn(HDC, HRGN);
;;int  WINAPI ExtSelectClipRgn(HDC, HRGN, int);
;;int  WINAPI SetMetaRgn(HDC);
;;HGDIOBJ WINAPI SelectObject(HDC, HGDIOBJ);
;;HPALETTE WINAPI SelectPalette(HDC, HPALETTE, BOOL);
;;COLORREF WINAPI SetBkColor(HDC, COLORREF);
;;int   WINAPI SetBkMode(HDC, int);
;;LONG  WINAPI SetBitmapBits(HBITMAP, DWORD, CONST VOID *);
;;
;;UINT  WINAPI SetBoundsRect(HDC, CONST RECT *, UINT);
;;int   WINAPI SetDIBits(HDC, HBITMAP, UINT, UINT, CONST VOID *, LPBITMAPINFO, UINT);
;;int   WINAPI SetDIBitsToDevice(HDC, int, int, DWORD, DWORD, int, int, UINT, UINT, LPVOID, LPBITMAPINFO, UINT);
;;DWORD WINAPI SetMapperFlags(HDC, DWORD);
;;int   WINAPI SetGraphicsMode(HDC hdc, int iMode);
;;int   WINAPI SetMapMode(HDC, int);
;;HMETAFILE   WINAPI SetMetaFileBitsEx(UINT, LPBYTE);
;;UINT  WINAPI SetPaletteEntries(HPALETTE, UINT, UINT, CONST PALETTEENTRY *);
;;COLORREF WINAPI SetPixel(HDC, int, int, COLORREF);
;;BOOL   WINAPI SetPixelV(HDC, int, int, COLORREF);
;;int   WINAPI SetPolyFillMode(HDC, int);
;;BOOL   WINAPI StretchBlt(HDC, int, int, int, int, HDC, int, int, int, int, DWORD);
;;BOOL   WINAPI SetRectRgn(HRGN, int, int, int, int);
;;int   WINAPI StretchDIBits(HDC, int, int, int, int, int, int, int, int, CONST
;;        VOID *, LPBITMAPINFO, UINT, DWORD);
;;int   WINAPI SetROP2(HDC, int);
;;int   WINAPI SetStretchBltMode(HDC, int);
;;UINT  WINAPI SetSystemPaletteUse(HDC, UINT);
;;int   WINAPI SetTextCharacterExtra(HDC, int);
;;COLORREF WINAPI SetTextColor(HDC, COLORREF);
;;UINT  WINAPI SetTextAlign(HDC, UINT);
;;BOOL  WINAPI SetTextJustification(HDC, int, int);
;;BOOL  WINAPI UpdateColors(HDC);
;;
;;#ifndef NOMETAFILE
;;
;;BOOL  WINAPI PlayMetaFileRecord(HDC, LPHANDLETABLE, LPMETARECORD, UINT);
;;typedef int (CALLBACK* MFENUMPROC)(HDC, HANDLETABLE FAR*, METARECORD FAR*, int, LPARAM);
;;BOOL  WINAPI EnumMetaFile(HDC, HMETAFILE, MFENUMPROC, LPARAM);
;;
;;typedef int (CALLBACK* ENHMFENUMPROC)(HDC, HANDLETABLE FAR*, ENHMETARECORD FAR*, int, LPARAM);
;;
;;// Enhanced Metafile Function Declarations
;;
;;HENHMETAFILE WINAPI CloseEnhMetaFile(HDC);
;;HENHMETAFILE WINAPI CopyEnhMetaFileA(HENHMETAFILE, LPSTR);
;;HENHMETAFILE WINAPI CopyEnhMetaFileW(HENHMETAFILE, LPWSTR);
;;#ifdef UNICODE
;;#define CopyEnhMetaFile  CopyEnhMetaFileW
;;#else
;;#define CopyEnhMetaFile  CopyEnhMetaFileA
;;#endif // !UNICODE
;;HDC   WINAPI CreateEnhMetaFileA(HDC, LPSTR, LPRECT, LPSTR);
;;HDC   WINAPI CreateEnhMetaFileW(HDC, LPWSTR, LPRECT, LPWSTR);
;;#ifdef UNICODE
;;#define CreateEnhMetaFile  CreateEnhMetaFileW
;;#else
;;#define CreateEnhMetaFile  CreateEnhMetaFileA
;;#endif // !UNICODE
;;BOOL  WINAPI DeleteEnhMetaFile(HENHMETAFILE);
;;BOOL  WINAPI EnumEnhMetaFile(HDC, HENHMETAFILE, ENHMFENUMPROC, LPVOID, LPRECT);
;;HENHMETAFILE  WINAPI GetEnhMetaFileA(LPSTR);
;;HENHMETAFILE  WINAPI GetEnhMetaFileW(LPWSTR);
;;#ifdef UNICODE
;;#define GetEnhMetaFile  GetEnhMetaFileW
;;#else
;;#define GetEnhMetaFile  GetEnhMetaFileA
;;#endif // !UNICODE
;;UINT  WINAPI GetEnhMetaFileBits(HENHMETAFILE, UINT, LPBYTE);
;;UINT  WINAPI GetEnhMetaFileDescriptionA(HENHMETAFILE, UINT, LPSTR );
;;UINT  WINAPI GetEnhMetaFileDescriptionW(HENHMETAFILE, UINT, LPWSTR );
;;#ifdef UNICODE
;;#define GetEnhMetaFileDescription  GetEnhMetaFileDescriptionW
;;#else
;;#define GetEnhMetaFileDescription  GetEnhMetaFileDescriptionA
;;#endif // !UNICODE
;;UINT  WINAPI GetEnhMetaFileHeader(HENHMETAFILE, UINT, LPENHMETAHEADER );
;;UINT  WINAPI GetEnhMetaFilePaletteEntries(HENHMETAFILE, UINT, LPPALETTEENTRY );
;;UINT  WINAPI GetWinMetaFileBits(HENHMETAFILE, UINT, LPBYTE, INT, HDC);
;;BOOL  WINAPI PlayEnhMetaFile(HDC, HENHMETAFILE, LPRECT);
;;BOOL  WINAPI PlayEnhMetaFileRecord(HDC, LPHANDLETABLE, LPENHMETARECORD, UINT);
;;HENHMETAFILE  WINAPI SetEnhMetaFileBits(UINT, LPBYTE);
;;HENHMETAFILE  WINAPI SetWinMetaFileBits(UINT, LPBYTE, HDC, LPMETAFILEPICT);
;;BOOL  WINAPI GdiComment(HDC, UINT, LPBYTE);
;;
;;#endif  /* NOMETAFILE */
;;
;;#ifndef NOTEXTMETRIC
;;
;;BOOL WINAPI GetTextMetricsA(HDC, LPTEXTMETRICA);
;;BOOL WINAPI GetTextMetricsW(HDC, LPTEXTMETRICW);
;;#ifdef UNICODE
;;#define GetTextMetrics  GetTextMetricsW
;;#else
;;#define GetTextMetrics  GetTextMetricsA
;;#endif // !UNICODE
;;
;;#endif
;;
;;/* new GDI */
;;BOOL WINAPI AngleArc(HDC, int, int, DWORD, FLOAT, FLOAT);
;;BOOL WINAPI PolyPolyline(HDC, LPPOINT, LPDWORD, DWORD);
;;BOOL WINAPI GetWorldTransform(HDC, LPXFORM);
;;BOOL WINAPI SetWorldTransform(HDC, LPXFORM);
;;BOOL WINAPI ModifyWorldTransform(HDC, LPXFORM , DWORD);
;;BOOL WINAPI CombineTransform(LPXFORM, LPXFORM, LPXFORM);
;;
;;/* Flags value for COLORADJUSTMENT */
(define-integrable CA_NEGATIVE                 #x0001)
(define-integrable CA_LOG_FILTER               #x0002)
;;
;;/* IlluminantIndex values */
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
;;/* Min and max for RedGamma, GreenGamma, BlueGamma */
(define-integrable RGB_GAMMA_MIN               02500)  ;(WORD)02500
(define-integrable RGB_GAMMA_MAX               65000)
;;
;;/* Min and max for ReferenceBlack and ReferenceWhite */
(define-integrable REFERENCE_WHITE_MIN         6000)	;(WORD)6000
(define-integrable REFERENCE_WHITE_MAX         10000)	;(WORD)10000
(define-integrable REFERENCE_BLACK_MIN         0)	;(WORD)0
(define-integrable REFERENCE_BLACK_MAX         4000)	;(WORD)4000
;;
;;/* Min and max for Contrast, Brightness, Colorfulness, RedGreenTint */
(define-integrable COLOR_ADJ_MIN               -100)	;(SHORT)-100
(define-integrable COLOR_ADJ_MAX               100)	;(SHORT)100
;;
;;typedef struct  tagCOLORADJUSTMENT {
;;    WORD   caSize;
;;    WORD   caFlags;
;;    WORD   caIlluminantIndex;
;;    WORD   caRedGamma;
;;    WORD   caGreenGamma;
;;    WORD   caBlueGamma;
;;    WORD   caReferenceBlack;
;;    WORD   caReferenceWhite;
;;    SHORT  caContrast;
;;    SHORT  caBrightness;
;;    SHORT  caColorfulness;
;;    SHORT  caRedGreenTint;
;;} COLORADJUSTMENT, *PCOLORADJUSTMENT, FAR *LPCOLORADJUSTMENT;
;;
;;BOOL WINAPI SetColorAdjustment(HDC, LPCOLORADJUSTMENT);
;;BOOL WINAPI GetColorAdjustment(HDC, LPCOLORADJUSTMENT);
;;HPALETTE WINAPI CreateHalftonePalette(HDC);
;;
;;#ifdef STRICT
;;typedef BOOL (CALLBACK* ABORTPROC)(HDC, int);
;;#else
;;typedef FARPROC ABORTPROC;
;;#endif
;;
;;typedef struct _DOCINFOA {
;;    int     cbSize;
;;    LPCSTR   lpszDocName;
;;    LPCSTR   lpszOutput;
;;} DOCINFOA, *LPDOCINFOA;
;;typedef struct _DOCINFOW {
;;    int     cbSize;
;;    LPCWSTR  lpszDocName;
;;    LPCWSTR  lpszOutput;
;;} DOCINFOW, *LPDOCINFOW;
;;#ifdef UNICODE
;;typedef DOCINFOW DOCINFO;
;;typedef LPDOCINFOW LPDOCINFO;
;;#else
;;typedef DOCINFOA DOCINFO;
;;typedef LPDOCINFOA LPDOCINFO;
;;#endif // UNICODE
;;
;;int WINAPI StartDocA(HDC, LPDOCINFOA);
;;int WINAPI StartDocW(HDC, LPDOCINFOW);
;;#ifdef UNICODE
;;#define StartDoc  StartDocW
;;#else
;;#define StartDoc  StartDocA
;;#endif // !UNICODE
;;int WINAPI EndDoc(HDC);
;;int WINAPI StartPage(HDC);
;;int WINAPI EndPage(HDC);
;;int WINAPI AbortDoc(HDC);
;;int WINAPI SetAbortProc(HDC, ABORTPROC);
;;
;;BOOL WINAPI AbortPath(HDC);
;;BOOL WINAPI ArcTo(HDC, int, int, int, int, int, int,int, int);
;;BOOL WINAPI BeginPath(HDC);
;;BOOL WINAPI CloseFigure(HDC);
;;BOOL WINAPI EndPath(HDC);
;;BOOL WINAPI FillPath(HDC);
;;BOOL WINAPI FlattenPath(HDC);
;;int  WINAPI GetPath(HDC, LPPOINT, LPBYTE, int);
;;HRGN WINAPI PathToRegion(HDC);
;;BOOL WINAPI PolyDraw(HDC, LPPOINT, LPBYTE, int);
;;BOOL WINAPI SelectClipPath(HDC, int);
;;int  WINAPI SetArcDirection(HDC, int);
;;BOOL WINAPI SetMiterLimit(HDC, FLOAT, PFLOAT);
;;BOOL WINAPI StrokeAndFillPath(HDC);
;;BOOL WINAPI StrokePath(HDC);
;;BOOL WINAPI WidenPath(HDC);
;;HPEN WINAPI ExtCreatePen(DWORD, DWORD, LPLOGBRUSH, DWORD, LPDWORD);
;;BOOL WINAPI GetMiterLimit(HDC, PFLOAT);
;;int  WINAPI GetArcDirection(HDC);
;;
;;int   WINAPI GetObjectA(HGDIOBJ, int, LPVOID);
;;int   WINAPI GetObjectW(HGDIOBJ, int, LPVOID);
;;#ifdef UNICODE
;;#define GetObject  GetObjectW
;;#else
;;#define GetObject  GetObjectA
;;#endif // !UNICODE
;;BOOL  WINAPI MoveToEx(HDC, int, int, LPPOINT);
;;BOOL  WINAPI TextOutA(HDC, int, int, LPCSTR, int);
;;BOOL  WINAPI TextOutW(HDC, int, int, LPCWSTR, int);
;;#ifdef UNICODE
;;#define TextOut  TextOutW
;;#else
;;#define TextOut  TextOutA
;;#endif // !UNICODE
;;BOOL  WINAPI ExtTextOutA(HDC, int, int, UINT, CONST RECT *, LPCSTR, UINT, LPINT);
;;BOOL  WINAPI ExtTextOutW(HDC, int, int, UINT, CONST RECT *, LPCWSTR, UINT, LPINT);
;;#ifdef UNICODE
;;#define ExtTextOut  ExtTextOutW
;;#else
;;#define ExtTextOut  ExtTextOutA
;;#endif // !UNICODE
;;BOOL  WINAPI PolyTextOutA(HDC, POLYTEXTA *, int);
;;BOOL  WINAPI PolyTextOutW(HDC, POLYTEXTW *, int);
;;#ifdef UNICODE
;;#define PolyTextOut  PolyTextOutW
;;#else
;;#define PolyTextOut  PolyTextOutA
;;#endif // !UNICODE
;;
;;HRGN  WINAPI CreatePolygonRgn(CONST POINT *, int, int);
;;BOOL  WINAPI DPtoLP(HDC, LPPOINT, int);
;;BOOL  WINAPI LPtoDP(HDC, LPPOINT, int);
;;BOOL  WINAPI Polygon(HDC, CONST POINT *, int);
;;BOOL  WINAPI Polyline(HDC, CONST POINT *, int);
;;
;;BOOL  WINAPI PolyBezier(HDC, LPPOINT, DWORD);
;;BOOL  WINAPI PolyBezierTo(HDC, LPPOINT, DWORD);
;;BOOL  WINAPI PolylineTo(HDC, LPPOINT, DWORD);
;;
;;BOOL  WINAPI SetViewportExtEx(HDC, int, int, LPSIZE);
;;BOOL  WINAPI SetViewportOrgEx(HDC, int, int, LPPOINT);
;;BOOL  WINAPI SetWindowExtEx(HDC, int, int, LPSIZE);
;;BOOL  WINAPI SetWindowOrgEx(HDC, int, int, LPPOINT);
;;
;;BOOL  WINAPI OffsetViewportOrgEx(HDC, int, int, LPPOINT);
;;BOOL  WINAPI OffsetWindowOrgEx(HDC, int, int, LPPOINT);
;;BOOL  WINAPI ScaleViewportExtEx(HDC, int, int, int, int, LPSIZE);
;;BOOL  WINAPI ScaleWindowExtEx(HDC, int, int, int, int, LPSIZE);
;;BOOL  WINAPI SetBitmapDimensionEx(HBITMAP, int, int, LPSIZE);
;;BOOL  WINAPI SetBrushOrgEx(HDC, int, int, LPPOINT);
;;
;;int   WINAPI GetTextFaceA(HDC, int, LPSTR);
;;int   WINAPI GetTextFaceW(HDC, int, LPWSTR);
;;#ifdef UNICODE
;;#define GetTextFace  GetTextFaceW
;;#else
;;#define GetTextFace  GetTextFaceA
;;#endif // !UNICODE
;;
;;#define FONTMAPPER_MAX 10
;;
;;HFONT  WINAPI ExtCreateFontIndirectA(LPEXTLOGFONTA);
;;HFONT  WINAPI ExtCreateFontIndirectW(LPEXTLOGFONTW);
;;#ifdef UNICODE
;;#define ExtCreateFontIndirect  ExtCreateFontIndirectW
;;#else
;;#define ExtCreateFontIndirect  ExtCreateFontIndirectA
;;#endif // !UNICODE
;;typedef struct tagKERNINGPAIR {
;;   WORD wFirst;
;;   WORD wSecond;
;;   int  iKernAmount;
;;} KERNINGPAIR, *LPKERNINGPAIR;
;;
;;DWORD WINAPI GetKerningPairs(HDC, DWORD, LPKERNINGPAIR);
;;
;;DWORD WINAPI GetDCOrg(HDC);
;;BOOL  WINAPI FixBrushOrgEx(HDC,int,int,LPPOINT);
;;BOOL  WINAPI UnrealizeObject(HGDIOBJ);
;;
;;BOOL  WINAPI GdiFlush();
;;DWORD WINAPI GdiSetBatchLimit(DWORD);
;;DWORD WINAPI GdiGetBatchLimit();
;;
;;#ifndef NOMETAFILE
;;
;;// Enhanced metafile constants.
;;
;;#define ENHMETA_SIGNATURE	0x464D4520
;;
;;// Stock object flag used in the object handle index in the enhanced
;;// metafile records.
;;// E.g. The object handle index (META_STOCK_OBJECT | BLACK_BRUSH)
;;// represents the stock object BLACK_BRUSH.
;;
;;#define ENHMETA_STOCK_OBJECT	0x80000000
;;
;;// Enhanced metafile record types.
;;
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

(define-integrable EMR_MIN				1)
(define-integrable EMR_MAX				97)

;;// Base record type for the enhanced metafile.
;;
;;typedef struct tagEMR
;;{
;;    DWORD   iType;		// Enhanced metafile record type
;;    DWORD   nSize;		// Length of the record in bytes.
;;				// This must be a multiple of 4.
;;} EMR, *PEMR;
;;
;;// Base text record type for the enhanced metafile.
;;
;;typedef struct tagEMRTEXT
;;{
;;    POINTL  ptlReference;
;;    DWORD   nChars;
;;    DWORD   offString;		// Offset to the string
;;    DWORD   fOptions;
;;    RECTL   rcl;
;;    DWORD   offDx;		// Offset to the inter-character spacing array.
;;				// This is always given.
;;} EMRTEXT, *PEMRTEXT;
;;
;;// Record structures for the enhanced metafile.
;;
;;typedef struct tagABORTPATH
;;{
;;    EMR     emr;
;;} EMRABORTPATH,      *PEMRABORTPATH,
;;  EMRBEGINPATH,      *PEMRBEGINPATH,
;;  EMRENDPATH,        *PEMRENDPATH,
;;  EMRCLOSEFIGURE,    *PEMRCLOSEFIGURE,
;;  EMRFLATTENPATH,    *PEMRFLATTENPATH,
;;  EMRWIDENPATH,      *PEMRWIDENPATH,
;;  EMRSETMETARGN,     *PEMRSETMETARGN,
;;  EMRSAVEDC,         *PEMRSAVEDC,
;;  EMRREALIZEPALETTE, *PEMRREALIZEPALETTE;
;;
;;typedef struct tagEMRSELECTCLIPPATH
;;{
;;    EMR     emr;
;;    DWORD   iMode;
;;} EMRSELECTCLIPPATH,    *PEMRSELECTCLIPPATH,
;;  EMRSETBKMODE,         *PEMRSETBKMODE,
;;  EMRSETMAPMODE,        *PEMRSETMAPMODE,
;;  EMRSETPOLYFILLMODE,   *PEMRSETPOLYFILLMODE,
;;  EMRSETROP2,           *PEMRSETROP2,
;;  EMRSETSTRETCHBLTMODE, *PEMRSETSTRETCHBLTMODE,
;;  EMRSETTEXTALIGN,      *PEMRSETTEXTALIGN;
;;
;;typedef struct tagEMRSETMITERLIMIT
;;{
;;    EMR     emr;
;;    FLOAT   eMiterLimit;
;;} EMRSETMITERLIMIT, *PEMRSETMITERLIMIT;
;;
;;typedef struct tagEMRRESTOREDC
;;{
;;    EMR     emr;
;;    LONG    iRelative;		// Specifies a relative instance
;;} EMRRESTOREDC, *PEMRRESTOREDC;
;;
;;typedef struct tagEMRSETARCDIRECTION
;;{
;;    EMR     emr;
;;    DWORD   iArcDirection;	// Specifies the arc direction in the
;;				// advanced graphics mode.
;;} EMRSETARCDIRECTION, *PEMRSETARCDIRECTION;
;;
;;typedef struct tagEMRSETMAPPERFLAGS
;;{
;;    EMR     emr;
;;    DWORD   dwFlags;
;;} EMRSETMAPPERFLAGS, *PEMRSETMAPPERFLAGS;
;;
;;typedef struct tagEMRSETTEXTCOLOR
;;{
;;    EMR     emr;
;;    COLORREF crColor;
;;} EMRSETBKCOLOR,   *PEMRSETBKCOLOR,
;;  EMRSETTEXTCOLOR, *PEMRSETTEXTCOLOR;
;;
;;typedef struct tagEMRSELECTOBJECT
;;{
;;    EMR     emr;
;;    DWORD   ihObject;		// Object handle index
;;} EMRSELECTOBJECT, *PEMRSELECTOBJECT,
;;  EMRDELETEOBJECT, *PEMRDELETEOBJECT;
;;
;;typedef struct tagEMRSELECTPALETTE
;;{
;;    EMR     emr;
;;    DWORD   ihPal;		// Palette handle index, background mode only
;;} EMRSELECTPALETTE, *PEMRSELECTPALETTE;
;;
;;typedef struct tagEMRRESIZEPALETTE
;;{
;;    EMR     emr;
;;    DWORD   ihPal;		// Palette handle index
;;    DWORD   cEntries;
;;} EMRRESIZEPALETTE, *PEMRRESIZEPALETTE;
;;
;;typedef struct tagEMRSETPALETTEENTRIES
;;{
;;    EMR     emr;
;;    DWORD   ihPal;		// Palette handle index
;;    DWORD   iStart;
;;    DWORD   cEntries;
;;    PALETTEENTRY aPalEntries[1];// The peFlags fields do not contain any flags
;;} EMRSETPALETTEENTRIES, *PEMRSETPALETTEENTRIES;
;;
;;typedef struct tagEMRSETCOLORADJUSTMENT
;;{
;;    EMR     emr;
;;    COLORADJUSTMENT ColorAdjustment;
;;} EMRSETCOLORADJUSTMENT, *PEMRSETCOLORADJUSTMENT;
;;
;;typedef struct tagEMRGDICOMMENT
;;{
;;    EMR     emr;
;;    DWORD   cbData;		// Size of data in bytes
;;    BYTE    Data[1];
;;} EMRGDICOMMENT, *PEMRGDICOMMENT;
;;
;;typedef struct tagEMREOF
;;{
;;    EMR     emr;
;;    DWORD   nPalEntries;	// Number of palette entries
;;    DWORD   offPalEntries;	// Offset to the palette entries
;;    DWORD   nSizeLast;		// Same as nSize and must be the last DWORD
;;				// of the record.  The palette entries,
;;				// if exist, precede this field.
;;} EMREOF, *PEMREOF;
;;
;;typedef struct tagEMRLINETO
;;{
;;    EMR     emr;
;;    POINTL  ptl;
;;} EMRLINETO,   *PEMRLINETO,
;;  EMRMOVETOEX, *PEMRMOVETOEX;
;;
;;typedef struct tagEMROFFSETCLIPRGN
;;{
;;    EMR     emr;
;;    POINTL  ptlOffset;
;;} EMROFFSETCLIPRGN, *PEMROFFSETCLIPRGN;
;;
;;typedef struct tagEMRFILLPATH
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;} EMRFILLPATH,          *PEMRFILLPATH,
;;  EMRSTROKEANDFILLPATH, *PEMRSTROKEANDFILLPATH,
;;  EMRSTROKEPATH,        *PEMRSTROKEPATH;
;;
;;typedef struct tagEMREXCLUDECLIPRECT
;;{
;;    EMR     emr;
;;    RECTL   rclClip;
;;} EMREXCLUDECLIPRECT,   *PEMREXCLUDECLIPRECT,
;;  EMRINTERSECTCLIPRECT, *PEMRINTERSECTCLIPRECT;
;;
;;typedef struct tagEMRSETVIEWPORTORGEX
;;{
;;    EMR     emr;
;;    POINTL  ptlOrigin;
;;} EMRSETVIEWPORTORGEX, *PEMRSETVIEWPORTORGEX,
;;  EMRSETWINDOWORGEX,   *PEMRSETWINDOWORGEX,
;;  EMRSETBRUSHORGEX,    *PEMRSETBRUSHORGEX;
;;
;;typedef struct tagEMRSETVIEWPORTEXTEX
;;{
;;    EMR     emr;
;;    SIZEL   szlExtent;
;;} EMRSETVIEWPORTEXTEX, *PEMRSETVIEWPORTEXTEX,
;;  EMRSETWINDOWEXTEX,   *PEMRSETWINDOWEXTEX;
;;
;;typedef struct tagEMRSCALEVIEWPORTEXTEX
;;{
;;    EMR     emr;
;;    LONG    xNum;
;;    LONG    xDenom;
;;    LONG    yNum;
;;    LONG    yDenom;
;;} EMRSCALEVIEWPORTEXTEX, *PEMRSCALEVIEWPORTEXTEX,
;;  EMRSCALEWINDOWEXTEX,   *PEMRSCALEWINDOWEXTEX;
;;
;;typedef struct tagEMRSETWORLDTRANSFORM
;;{
;;    EMR     emr;
;;    XFORM   xform;
;;} EMRSETWORLDTRANSFORM, *PEMRSETWORLDTRANSFORM;
;;
;;typedef struct tagEMRMODIFYWORLDTRANSFORM
;;{
;;    EMR     emr;
;;    XFORM   xform;
;;    DWORD   iMode;
;;} EMRMODIFYWORLDTRANSFORM, *PEMRMODIFYWORLDTRANSFORM;
;;
;;typedef struct tagEMRSETPIXELV
;;{
;;    EMR     emr;
;;    POINTL  ptlPixel;
;;    COLORREF crColor;
;;} EMRSETPIXELV, *PEMRSETPIXELV;
;;
;;typedef struct tagEMREXTFLOODFILL
;;{
;;    EMR     emr;
;;    POINTL  ptlStart;
;;    COLORREF crColor;
;;    DWORD   iMode;
;;} EMREXTFLOODFILL, *PEMREXTFLOODFILL;
;;
;;typedef struct tagEMRELLIPSE
;;{
;;    EMR     emr;
;;    RECTL   rclBox;		// Inclusive-inclusive bounding rectangle
;;} EMRELLIPSE,  *PEMRELLIPSE,
;;  EMRRECTANGLE, *PEMRRECTANGLE;
;;
;;typedef struct tagEMRROUNDRECT
;;{
;;    EMR     emr;
;;    RECTL   rclBox;		// Inclusive-inclusive bounding rectangle
;;    SIZEL   szlCorner;
;;} EMRROUNDRECT, *PEMRROUNDRECT;
;;
;;typedef struct tagEMRARC
;;{
;;    EMR     emr;
;;    RECTL   rclBox;		// Inclusive-inclusive bounding rectangle
;;    POINTL  ptlStart;
;;    POINTL  ptlEnd;
;;} EMRARC,   *PEMRARC,
;;  EMRARCTO, *PEMRARCTO,
;;  EMRCHORD, *PEMRCHORD,
;;  EMRPIE,   *PEMRPIE;
;;
;;typedef struct tagEMRANGLEARC
;;{
;;    EMR     emr;
;;    POINTL  ptlCenter;
;;    DWORD   nRadius;
;;    FLOAT   eStartAngle;
;;    FLOAT   eSweepAngle;
;;} EMRANGLEARC, *PEMRANGLEARC;
;;
;;typedef struct tagEMRPOLYLINE
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   cptl;
;;    POINTL  aptl[1];
;;} EMRPOLYLINE,     *PEMRPOLYLINE,
;;  EMRPOLYBEZIER,   *PEMRPOLYBEZIER,
;;  EMRPOLYGON,      *PEMRPOLYGON,
;;  EMRPOLYBEZIERTO, *PEMRPOLYBEZIERTO,
;;  EMRPOLYLINETO,   *PEMRPOLYLINETO;
;;
;;typedef struct tagEMRPOLYLINE16
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   cpts;
;;    POINTS  apts[1];
;;} EMRPOLYLINE16,     *PEMRPOLYLINE16,
;;  EMRPOLYBEZIER16,   *PEMRPOLYBEZIER16,
;;  EMRPOLYGON16,      *PEMRPOLYGON16,
;;  EMRPOLYBEZIERTO16, *PEMRPOLYBEZIERTO16,
;;  EMRPOLYLINETO16,   *PEMRPOLYLINETO16;
;;
;;typedef struct tagEMRPOLYDRAW
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   cptl;		// Number of points
;;    POINTL  aptl[1];		// Array of points
;;    BYTE    abTypes[1];		// Array of point types
;;} EMRPOLYDRAW, *PEMRPOLYDRAW;
;;
;;typedef struct tagEMRPOLYDRAW16
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   cpts;		// Number of points
;;    POINTS  apts[1];		// Array of points
;;    BYTE    abTypes[1];		// Array of point types
;;} EMRPOLYDRAW16, *PEMRPOLYDRAW16;
;;
;;typedef struct tagEMRPOLYPOLYLINE
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   nPolys;		// Number of polys
;;    DWORD   cptl;       	// Total number of points in all polys
;;    DWORD   aPolyCounts[1];	// Array of point counts for each poly
;;    POINTL  aptl[1];		// Array of points
;;} EMRPOLYPOLYLINE, *PEMRPOLYPOLYLINE,
;;  EMRPOLYPOLYGON,  *PEMRPOLYPOLYGON;
;;
;;typedef struct tagEMRPOLYPOLYLINE16
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   nPolys;		// Number of polys
;;    DWORD   cpts;       	// Total number of points in all polys
;;    DWORD   aPolyCounts[1];	// Array of point counts for each poly
;;    POINTS  apts[1];		// Array of points
;;} EMRPOLYPOLYLINE16, *PEMRPOLYPOLYLINE16,
;;  EMRPOLYPOLYGON16,  *PEMRPOLYPOLYGON16;
;;
;;typedef struct tagEMRINVERTRGN
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   cbRgnData;		// Size of region data in bytes
;;    BYTE    RgnData[1];
;;} EMRINVERTRGN, *PEMRINVERTRGN,
;;  EMRPAINTRGN,  *PEMRPAINTRGN;
;;
;;typedef struct tagEMRFILLRGN
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   cbRgnData;		// Size of region data in bytes
;;    DWORD   ihBrush;		// Brush handle index
;;    BYTE    RgnData[1];
;;} EMRFILLRGN, *PEMRFILLRGN;
;;
;;typedef struct tagEMRFRAMERGN
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   cbRgnData;		// Size of region data in bytes
;;    DWORD   ihBrush;		// Brush handle index
;;    SIZEL   szlStroke;
;;    BYTE    RgnData[1];
;;} EMRFRAMERGN, *PEMRFRAMERGN;
;;
;;typedef struct tagEMREXTSELECTCLIPRGN
;;{
;;    EMR     emr;
;;    DWORD   cbRgnData;		// Size of region data in bytes
;;    DWORD   iMode;
;;    BYTE    RgnData[1];
;;} EMREXTSELECTCLIPRGN, *PEMREXTSELECTCLIPRGN;
;;
;;typedef struct tagEMREXTTEXTOUTA
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   iGraphicsMode;	// Current graphics mode
;;    FLOAT   exScale;            // X and Y scales from Page units to .01mm units
;;    FLOAT   eyScale;            //   if graphics mode is GM_COMPATIBLE.
;;    EMRTEXT emrtext;		// This is followed by the string and spacing
;;				// array
;;} EMREXTTEXTOUTA, *PEMREXTTEXTOUTA,
;;  EMREXTTEXTOUTW, *PEMREXTTEXTOUTW;
;;
;;typedef struct tagEMRPOLYTEXTOUTA
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    DWORD   iGraphicsMode;	// Current graphics mode
;;    FLOAT   exScale;            // X and Y scales from Page units to .01mm units
;;    FLOAT   eyScale;            //   if graphics mode is GM_COMPATIBLE.
;;    LONG    cStrings;
;;    EMRTEXT aemrtext[1];	// Array of EMRTEXT structures.  This is
;;				// followed by the strings and spacing arrays.
;;} EMRPOLYTEXTOUTA, *PEMRPOLYTEXTOUTA,
;;  EMRPOLYTEXTOUTW, *PEMRPOLYTEXTOUTW;
;;
;;typedef struct tagEMRBITBLT
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    LONG    xDest;
;;    LONG    yDest;
;;    LONG    cxDest;
;;    LONG    cyDest;
;;    DWORD   dwRop;
;;    LONG    xSrc;
;;    LONG    ySrc;
;;    XFORM   xformSrc;		// Source DC transform
;;    COLORREF crBkColorSrc;	// Source DC BkColor in RGB
;;    DWORD   iUsageSrc;		// Source bitmap info color table usage
;;				// (DIB_RGB_COLORS)
;;    DWORD   offBmiSrc;		// Offset to the source BITMAPINFO structure
;;    DWORD   cbBmiSrc;		// Size of the source BITMAPINFO structure
;;    DWORD   offBitsSrc;		// Offset to the source bitmap bits
;;    DWORD   cbBitsSrc;		// Size of the source bitmap bits
;;} EMRBITBLT, *PEMRBITBLT;
;;
;;typedef struct tagEMRSTRETCHBLT
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    LONG    xDest;
;;    LONG    yDest;
;;    LONG    cxDest;
;;    LONG    cyDest;
;;    DWORD   dwRop;
;;    LONG    xSrc;
;;    LONG    ySrc;
;;    XFORM   xformSrc;		// Source DC transform
;;    COLORREF crBkColorSrc;	// Source DC BkColor in RGB
;;    DWORD   iUsageSrc;		// Source bitmap info color table usage
;;				// (DIB_RGB_COLORS)
;;    DWORD   offBmiSrc;		// Offset to the source BITMAPINFO structure
;;    DWORD   cbBmiSrc;		// Size of the source BITMAPINFO structure
;;    DWORD   offBitsSrc;		// Offset to the source bitmap bits
;;    DWORD   cbBitsSrc;		// Size of the source bitmap bits
;;    LONG    cxSrc;
;;    LONG    cySrc;
;;} EMRSTRETCHBLT, *PEMRSTRETCHBLT;
;;
;;typedef struct tagEMRMASKBLT
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    LONG    xDest;
;;    LONG    yDest;
;;    LONG    cxDest;
;;    LONG    cyDest;
;;    DWORD   dwRop;
;;    LONG    xSrc;
;;    LONG    ySrc;
;;    XFORM   xformSrc;		// Source DC transform
;;    COLORREF crBkColorSrc;	// Source DC BkColor in RGB
;;    DWORD   iUsageSrc;		// Source bitmap info color table usage
;;				// (DIB_RGB_COLORS)
;;    DWORD   offBmiSrc;		// Offset to the source BITMAPINFO structure
;;    DWORD   cbBmiSrc;		// Size of the source BITMAPINFO structure
;;    DWORD   offBitsSrc;		// Offset to the source bitmap bits
;;    DWORD   cbBitsSrc;		// Size of the source bitmap bits
;;    LONG    xMask;
;;    LONG    yMask;
;;    DWORD   iUsageMask;		// Mask bitmap info color table usage
;;    DWORD   offBmiMask;		// Offset to the mask BITMAPINFO structure if any
;;    DWORD   cbBmiMask;		// Size of the mask BITMAPINFO structure if any
;;    DWORD   offBitsMask;	// Offset to the mask bitmap bits if any
;;    DWORD   cbBitsMask;		// Size of the mask bitmap bits if any
;;} EMRMASKBLT, *PEMRMASKBLT;
;;
;;typedef struct tagEMRPLGBLT
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    POINTL  aptlDest[3];
;;    LONG    xSrc;
;;    LONG    ySrc;
;;    LONG    cxSrc;
;;    LONG    cySrc;
;;    XFORM   xformSrc;		// Source DC transform
;;    COLORREF crBkColorSrc;	// Source DC BkColor in RGB
;;    DWORD   iUsageSrc;		// Source bitmap info color table usage
;;				// (DIB_RGB_COLORS)
;;    DWORD   offBmiSrc;		// Offset to the source BITMAPINFO structure
;;    DWORD   cbBmiSrc;		// Size of the source BITMAPINFO structure
;;    DWORD   offBitsSrc;		// Offset to the source bitmap bits
;;    DWORD   cbBitsSrc;		// Size of the source bitmap bits
;;    LONG    xMask;
;;    LONG    yMask;
;;    DWORD   iUsageMask;		// Mask bitmap info color table usage
;;    DWORD   offBmiMask;		// Offset to the mask BITMAPINFO structure if any
;;    DWORD   cbBmiMask;		// Size of the mask BITMAPINFO structure if any
;;    DWORD   offBitsMask;	// Offset to the mask bitmap bits if any
;;    DWORD   cbBitsMask;		// Size of the mask bitmap bits if any
;;} EMRPLGBLT, *PEMRPLGBLT;
;;
;;typedef struct tagEMRSETDIBITSTODEVICE
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    LONG    xDest;
;;    LONG    yDest;
;;    LONG    xSrc;
;;    LONG    ySrc;
;;    LONG    cxSrc;
;;    LONG    cySrc;
;;    DWORD   offBmiSrc;		// Offset to the source BITMAPINFO structure
;;    DWORD   cbBmiSrc;		// Size of the source BITMAPINFO structure
;;    DWORD   offBitsSrc;		// Offset to the source bitmap bits
;;    DWORD   cbBitsSrc;		// Size of the source bitmap bits
;;    DWORD   iUsageSrc;		// Source bitmap info color table usage
;;    DWORD   iStartScan;
;;    DWORD   cScans;
;;} EMRSETDIBITSTODEVICE, *PEMRSETDIBITSTODEVICE;
;;
;;typedef struct tagEMRSTRETCHDIBITS
;;{
;;    EMR     emr;
;;    RECTL   rclBounds;		// Inclusive-inclusive bounds in device units
;;    LONG    xDest;
;;    LONG    yDest;
;;    LONG    xSrc;
;;    LONG    ySrc;
;;    LONG    cxSrc;
;;    LONG    cySrc;
;;    DWORD   offBmiSrc;		// Offset to the source BITMAPINFO structure
;;    DWORD   cbBmiSrc;		// Size of the source BITMAPINFO structure
;;    DWORD   offBitsSrc;		// Offset to the source bitmap bits
;;    DWORD   cbBitsSrc;		// Size of the source bitmap bits
;;    DWORD   iUsageSrc;		// Source bitmap info color table usage
;;    DWORD   dwRop;
;;    LONG    cxDest;
;;    LONG    cyDest;
;;} EMRSTRETCHDIBITS, *PEMRSTRETCHDIBITS;
;;
;;typedef struct tagEMREXTCREATEFONTINDIRECTW
;;{
;;    EMR     emr;
;;    DWORD   ihFont;		// Font handle index
;;    EXTLOGFONTW	elfw;
;;} EMREXTCREATEFONTINDIRECTW, *PEMREXTCREATEFONTINDIRECTW;
;;
;;typedef struct tagEMRCREATEPALETTE
;;{
;;    EMR     emr;
;;    DWORD   ihPal;		// Palette handle index
;;    LOGPALETTE lgpl;		// The peFlags fields in the palette entries
;;				// do not contain any flags
;;} EMRCREATEPALETTE, *PEMRCREATEPALETTE;
;;
;;typedef struct tagEMRCREATEPEN
;;{
;;    EMR     emr;
;;    DWORD   ihPen;		// Pen handle index
;;    LOGPEN  lopn;
;;} EMRCREATEPEN, *PEMRCREATEPEN;
;;
;;typedef struct tagEMREXTCREATEPEN
;;{
;;    EMR     emr;
;;    DWORD   ihPen;		// Pen handle index
;;    DWORD   offBmi;		// Offset to the BITMAPINFO structure if any
;;    DWORD   cbBmi;		// Size of the BITMAPINFO structure if any
;;				// The bitmap info is followed by the bitmap
;;				// bits to form a packed DIB.
;;    DWORD   offBits;		// Offset to the brush bitmap bits if any
;;    DWORD   cbBits;		// Size of the brush bitmap bits if any
;;    EXTLOGPEN elp;		// The extended pen with the style array.
;;} EMREXTCREATEPEN, *PEMREXTCREATEPEN;
;;
;;typedef struct tagEMRCREATEBRUSHINDIRECT
;;{
;;    EMR     emr;
;;    DWORD   ihBrush;		// Brush handle index
;;    LOGBRUSH lb;		// The style must be BS_SOLID, BS_HOLLOW,
;;				// BS_NULL or BS_HATCHED.
;;} EMRCREATEBRUSHINDIRECT, *PEMRCREATEBRUSHINDIRECT;
;;
;;typedef struct tagEMRCREATEMONOBRUSH
;;{
;;    EMR     emr;
;;    DWORD   ihBrush;		// Brush handle index
;;    DWORD   iUsage;		// Bitmap info color table usage
;;    DWORD   offBmi;		// Offset to the BITMAPINFO structure
;;    DWORD   cbBmi;		// Size of the BITMAPINFO structure
;;    DWORD   offBits;		// Offset to the bitmap bits
;;    DWORD   cbBits;		// Size of the bitmap bits
;;} EMRCREATEMONOBRUSH, *PEMRCREATEMONOBRUSH;
;;
;;typedef struct tagEMRCREATEDIBPATTERNBRUSHPT
;;{
;;    EMR     emr;
;;    DWORD   ihBrush;		// Brush handle index
;;    DWORD   iUsage;		// Bitmap info color table usage
;;    DWORD   offBmi;		// Offset to the BITMAPINFO structure
;;    DWORD   cbBmi;		// Size of the BITMAPINFO structure
;;				// The bitmap info is followed by the bitmap
;;				// bits to form a packed DIB.
;;    DWORD   offBits;		// Offset to the bitmap bits
;;    DWORD   cbBits;		// Size of the bitmap bits
;;} EMRCREATEDIBPATTERNBRUSHPT, *PEMRCREATEDIBPATTERNBRUSHPT;
;;
;;#endif  /* NOMETAFILE */
;;
;;#endif /* NOGDI */
;;
;;#ifdef __cplusplus
;;}
;;#endif
;;
;;#endif /* _WINGDI_ */
