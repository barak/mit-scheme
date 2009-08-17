/* -*-C-*-

$Id: e3c4130f787a91b57c9a54fc704a0e393eef0fab $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

*/


/* This software was derived from the following:  */

/******************************************************************************\
*       This is a part of the Microsoft Source Code Samples. 
*       Copyright (C) 1993 Microsoft Corporation.
*       All rights reserved. 
*       This source code is only intended as a supplement to 
*       Microsoft Development Tools and/or WinHelp documentation.
*       See these sources for detailed information regarding the 
*       Microsoft samples programs.
\******************************************************************************/

/*******************************************************************************
 *                                                                             *
 *  MODULE      : DIB.C                                                        *
 *                                                                             *
 *  DESCRIPTION : Routines for dealing with Device Independent Bitmaps.        *
 *                                                                             *
 *  FUNCTIONS   : OpenDIB()           - Opens DIB file and creates a memory DIB*
 *                                                                             *
 *                WriteDIB()          - Writes a global handle in CF_DIB format*
 *                                      to a file.                             *
 *                                                                             *
 *                CreateDIB()         - Creates an empty DIB of given size     *
 *                                                                             *
 *                DeleteDIB()         - Deletes the DIB                        *
 *                                                                             *
 *                DibInfo()           - Retrieves the info. block associated   *
 *                                      with a CF_DIB format memory block.     *
 *                                                                             *
 *                CreateBIPalette()   - Creates a GDI palette given a pointer  *
 *                                      to a BITMAPINFO structure.             *
 *                                                                             *
 *                CreateDibPalette()  - Creates a GDI palette given a HANDLE   *
 *                                      to a BITMAPINFO structure.             *
 *                                                                             *
 *                ReadDibBitmapInfo() - Reads a file in DIB format and returns *
 *                                      a global handle to it's BITMAPINFO     *
 *                                                                             *
 *                DibWidth()                                                   *
 *                                                                             *
 *                DibHeight()                                                  *
 *                                                                             *
 *                PaletteSize()       - Calculates the palette size in bytes   *
 *                                      of given DIB                           *
 *                                                                             *
 *                DibNumColors()      - Determines the number of colors in DIB *
 *                                                                             *
 *                BitmapFromDib()     - Creates a DDB given a global handle to *
 *                                      a block in CF_DIB format.              *
 *                                                                             *
 *                DibFromBitmap()     - Creates a DIB repr. the DDB passed in. *
 *                                                                             *
 *                DrawBitmap()        - Draws a bitmap at specified position   *
 *                                      in the DC.                             *
 *                                                                             *
 *                DibBlt()            - Draws a bitmap in CIF_DIB format using *
 *                                      SetDIBitsToDevice()                    *
 *                                                                             *
 *                StretchDibBlt()     - Draws a bitmap in CIF_DIB format using *
 *                                      StretchDIBits()                        *
 *                                                                             *
 *                CopyBitmap()           -  Copies given bitmap to another.    *
 *                                                                             *
 *                CropBitmap()           -  Crops a bitmap to the given size.  *
 *                                                                             *
 *                DIBSetPixelsUnaligned - copy bytes into DIB bitmap area      *
 *                                                                             *
 *                lread()             - Private routine to read more than 64k  *
 *                                                                             *
 *                lwrite()            - Private routine to write more than 64k *
 *                                                                             *
 *******************************************************************************/

#include <windows.h>
#include "dibutils.h"
static   HCURSOR hcurSave;

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :OpenDIB(LPSTR szFile)                                       *
 *                                                                          *
 *  PURPOSE    :Open a DIB file and create a MEMORY DIB, a memory handle    *
 *              containing BITMAPINFO, palette data and the bits.           *
 *                                                                          *
 *  RETURNS    :A handle to the DIB.                                        *
 *                                                                          *
 ****************************************************************************/
HANDLE FAR WINAPI
OpenDIB (LPSTR szFile)
{
    HFILE               fh;
    BITMAPINFOHEADER    bi;
    LPBITMAPINFOHEADER  lpbi;
    DWORD               dwLen = 0;
    DWORD               dwBits;
    HANDLE              hdib;
    HANDLE              h;
    OFSTRUCT            of;

    /* Open the file and read the DIB information */
    fh = OpenFile(szFile, &of, (UINT)OF_READ);
    if (fh == -1)
        return NULL;

    hdib = ReadDibBitmapInfo(fh);
    if (!hdib)
        return NULL;
    DibInfo(hdib,&bi);

    /* Calculate the memory needed to hold the DIB */
    dwBits = bi.biSizeImage;
    dwLen  = bi.biSize + (DWORD)PaletteSize (&bi) + dwBits;

    /* Try to increase the size of the bitmap info. buffer to hold the DIB */
    h = GlobalReAlloc(hdib, dwLen, GHND);
    if (!h){
        GlobalFree(hdib);
        hdib = NULL;
    }
    else
        hdib = h;

    /* Read in the bits */
    if (hdib){

        lpbi = (VOID FAR *)GlobalLock(hdib);
        lread(fh, (LPSTR)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi), dwBits);
        GlobalUnlock(hdib);
    }
    _lclose(fh);

    return hdib;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : WriteDIB(LPSTR szFile,HANDLE hdib)                         *
 *                                                                          *
 *  PURPOSE    : Write a global handle in CF_DIB format to a file.          *
 *                                                                          *
 *  RETURNS    : TRUE  - if successful.                                     *
 *               FALSE - otherwise                                          *
 *                                                                          *
 ****************************************************************************/
BOOL FAR WINAPI
WriteDIB (LPSTR szFile, HANDLE hdib)
{
    BITMAPFILEHEADER    hdr;
    LPBITMAPINFOHEADER  lpbi;
    HFILE               fh;
    OFSTRUCT            of;

    if (!hdib)
        return FALSE;

    fh = OpenFile(szFile, &of, (UINT)OF_CREATE|OF_READWRITE);
    if (fh == -1)
        return FALSE;

    lpbi = (VOID FAR *)GlobalLock (hdib);

    /* Fill in the fields of the file header */
    hdr.bfType          = BFT_BITMAP;
    hdr.bfSize          = GlobalSize (hdib) + SIZEOF_BITMAPFILEHEADER_PACKED;
    hdr.bfReserved1     = 0;
    hdr.bfReserved2     = 0;
    hdr.bfOffBits       = (DWORD) (SIZEOF_BITMAPFILEHEADER_PACKED + lpbi->biSize +
                          PaletteSize(lpbi));

    /* Write the file header */
#ifdef  FIXDWORDALIGNMENT
    _lwrite(fh, (LPSTR)&hdr, (UINT)(SIZEOF_BITMAPFILEHEADER_PACKED));
#else
        WriteMapFileHeaderandConvertFromDwordAlignToPacked(fh, &hdr);
#endif

        /* this struct already DWORD aligned!*/
    /* Write the DIB header and the bits */
    lwrite (fh, (LPSTR)lpbi, GlobalSize (hdib));

    GlobalUnlock (hdib);
    _lclose(fh);
    return TRUE;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DeleteDIB(HANDLE hdib)                                     *
 *                                                                          *
 *  PURPOSE    : Frees storage associated with DIB                          *
 *               format memory block.                                       *
 *                                                                          *
 *  RETURNS    : TRUE  - if successful.                                     *
 *               FALSE - otherwise                                          *
 *                                                                          *
 ****************************************************************************/
BOOL FAR WINAPI
DeleteDIB (HANDLE hdib)
{
    if (hdib)
      return  GlobalFree (hdib) == 0;
    else
      return  FALSE;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibInfo(HANDLE hbi,LPBITMAPINFOHEADER lpbi)                *
 *                                                                          *
 *  PURPOSE    : Retrieves the DIB info associated with a CF_DIB            *
 *               format memory block.                                       *
 *                                                                          *
 *  RETURNS    : TRUE  - if successful.                                     *
 *               FALSE - otherwise                                          *
 *                                                                          *
 ****************************************************************************/
BOOL DibInfo (
    HANDLE hbi,
    LPBITMAPINFOHEADER lpbi)
{
    if (hbi){
        *lpbi = *(LPBITMAPINFOHEADER)GlobalLock (hbi);

        /* fill in the default fields */
        if (lpbi->biSize != sizeof (BITMAPCOREHEADER)){
            if (lpbi->biSizeImage == 0L)
                                lpbi->biSizeImage = WIDTHBYTES(lpbi->biWidth*lpbi->biBitCount) * lpbi->biHeight;

            if (lpbi->biClrUsed == 0L)
                                lpbi->biClrUsed = DibNumColors (lpbi);
    }
        GlobalUnlock (hbi);
        return TRUE;
    }
    return FALSE;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : CreateBIPalette(LPBITMAPINFOHEADER lpbi)                   *
 *                                                                          *
 *  PURPOSE    : Given a Pointer to a BITMAPINFO struct will create a       *
 *               a GDI palette object from the color table.                 *
 *                                                                          *
 *  RETURNS    : A handle to the palette.                                   *
 *                                                                          *
 ****************************************************************************/
HPALETTE CreateBIPalette (LPBITMAPINFOHEADER lpbi)
{
    LOGPALETTE          *pPal;
    HPALETTE            hpal = NULL;
    WORD                nNumColors;
    BYTE                red;
    BYTE                green;
    BYTE                blue;
    WORD                i;
    RGBQUAD        FAR *pRgb;

    if (!lpbi)
        return NULL;

    if (lpbi->biSize != sizeof(BITMAPINFOHEADER))
        return NULL;

    /* Get a pointer to the color table and the number of colors in it */
    pRgb = (RGBQUAD FAR *)((LPSTR)lpbi + (WORD)lpbi->biSize);
    nNumColors = DibNumColors(lpbi);

    if (nNumColors){
        /* Allocate for the logical palette structure */
        pPal = (LOGPALETTE*)LocalAlloc(LPTR,sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));
        if (!pPal)
            return NULL;

        pPal->palNumEntries = nNumColors;
        pPal->palVersion    = PALVERSION;

        /* Fill in the palette entries from the DIB color table and
         * create a logical color palette.
         */
        for (i = 0; i < nNumColors; i++){
            pPal->palPalEntry[i].peRed   = pRgb[i].rgbRed;
            pPal->palPalEntry[i].peGreen = pRgb[i].rgbGreen;
            pPal->palPalEntry[i].peBlue  = pRgb[i].rgbBlue;
            pPal->palPalEntry[i].peFlags = (BYTE)0;
        }
        hpal = CreatePalette(pPal);
        LocalFree((HANDLE)pPal);
    }
    else if (lpbi->biBitCount == 24){
        /* A 24 bitcount DIB has no color table entries so, set the number of
         * to the maximum value (256).
         */
        nNumColors = MAXPALETTE;
        pPal = (LOGPALETTE*)LocalAlloc(LPTR,sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));
        if (!pPal)
            return NULL;

        pPal->palNumEntries = nNumColors;
        pPal->palVersion    = PALVERSION;

        red = green = blue = 0;

        /* Generate 256 (= 8*8*4) RGB combinations to fill the palette
         * entries.
         */
        for (i = 0; i < pPal->palNumEntries; i++){
            pPal->palPalEntry[i].peRed   = red;
            pPal->palPalEntry[i].peGreen = green;
            pPal->palPalEntry[i].peBlue  = blue;
            pPal->palPalEntry[i].peFlags = (BYTE)0;

            if (!(red += 32))
                if (!(green += 32))
                    blue += 64;
        }
        hpal = CreatePalette(pPal);
        LocalFree((HANDLE)pPal);
    }
    return hpal;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : CreateDibPalette(HANDLE hbi)                               *
 *                                                                          *
 *  PURPOSE    : Given a Global HANDLE to a BITMAPINFO Struct               *
 *               will create a GDI palette object from the color table.     *
 *               (BITMAPINFOHEADER format DIBs only)                        *
 *                                                                          *
 *  RETURNS    : A handle to the palette.                                   *
 *                                                                          *
 ****************************************************************************/
HPALETTE CreateDibPalette (HANDLE hbi)
{
    HPALETTE hpal;

    if (!hbi)
        return NULL;
    hpal = CreateBIPalette((LPBITMAPINFOHEADER)GlobalLock(hbi));
    GlobalUnlock(hbi);
    return hpal;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : ReadDibBitmapInfo(int fh)                                  *
 *                                                                          *
 *  PURPOSE    : Will read a file in DIB format and return a global HANDLE  *
 *               to it's BITMAPINFO.  This function will work with both     *
 *               "old" (BITMAPCOREHEADER) and "new" (BITMAPINFOHEADER)      *
 *               bitmap formats, but will always return a "new" BITMAPINFO  *
 *                                                                          *
 *  RETURNS    : A handle to the BITMAPINFO of the DIB in the file.         *
 *                                                                          *
 ****************************************************************************/
HANDLE ReadDibBitmapInfo (INT fh)
{
    DWORD     off;
    HANDLE    hbi = NULL;
    INT       size;
    INT       i;
    WORD      nNumColors;

    RGBQUAD FAR       *pRgb;
    BITMAPINFOHEADER   bi;
    BITMAPCOREHEADER   bc;
    LPBITMAPINFOHEADER lpbi;
    BITMAPFILEHEADER   bf;
    DWORD              dwWidth = 0;
    DWORD              dwHeight = 0;
    WORD               wPlanes, wBitCount;

    if (fh == -1)
        return NULL;
#ifdef FIXDWORDALIGNMENT
    /* Reset file pointer and read file header */
    off = _llseek(fh, 0L, (UINT)SEEK_CUR);
    if ((SIZEOF_BITMAPFILEHEADER_PACKED)  != _lread(fh, (LPSTR)&bf, (UINT)sizeof (SIZEOF_BITMAPFILEHEADER_PACKED)))
        return FALSE;
#else
        ReadBitMapFileHeaderandConvertToDwordAlign(fh, &bf, &off);
        /* at this point we have read the file into bf*/
#endif

    /* Do we have a RC HEADER? */
    if (!ISDIB (bf.bfType)) {    
        bf.bfOffBits = 0L;               
                _llseek(fh, off, (UINT)SEEK_SET); /*seek back to beginning of file*/
    }
    if (sizeof (bi) != _lread(fh, (LPSTR)&bi, (UINT)sizeof(bi)))
        return FALSE;

    nNumColors = DibNumColors (&bi);

    /* Check the nature (BITMAPINFO or BITMAPCORE) of the info. block
     * and extract the field information accordingly. If a BITMAPCOREHEADER,
     * transfer it's field information to a BITMAPINFOHEADER-style block
     */
    switch (size = (INT)bi.biSize){
        case sizeof (BITMAPINFOHEADER):
            break;

        case sizeof (BITMAPCOREHEADER):

            bc = *(BITMAPCOREHEADER*)&bi;

            dwWidth   = (DWORD)bc.bcWidth;
            dwHeight  = (DWORD)bc.bcHeight;
            wPlanes   = bc.bcPlanes;
            wBitCount = bc.bcBitCount;

        bi.biSize           = sizeof(BITMAPINFOHEADER);
            bi.biWidth              = dwWidth;
            bi.biHeight             = dwHeight;
            bi.biPlanes             = wPlanes;
            bi.biBitCount           = wBitCount;

            bi.biCompression        = BI_RGB;
            bi.biSizeImage          = 0;
            bi.biXPelsPerMeter      = 0;
            bi.biYPelsPerMeter      = 0;
            bi.biClrUsed            = nNumColors;
            bi.biClrImportant       = nNumColors;

            _llseek(fh, (LONG)sizeof (BITMAPCOREHEADER) - sizeof (BITMAPINFOHEADER), (UINT)SEEK_CUR);
            break;

        default:
            /* Not a DIB! */
            return NULL;
    }

    /*  Fill in some default values if they are zero */
    if (bi.biSizeImage == 0){
        bi.biSizeImage = WIDTHBYTES ((DWORD)bi.biWidth * bi.biBitCount)
                         * bi.biHeight;
    }
    if (bi.biClrUsed == 0)
        bi.biClrUsed = DibNumColors(&bi);

    /* Allocate for the BITMAPINFO structure and the color table. */
    hbi = GlobalAlloc (GHND, (LONG)bi.biSize + nNumColors * sizeof(RGBQUAD));
    if (!hbi)
        return NULL;
    lpbi = (VOID FAR *)GlobalLock (hbi);
    *lpbi = bi;

    /* Get a pointer to the color table */
    pRgb = (RGBQUAD FAR *)((LPSTR)lpbi + bi.biSize);
    if (nNumColors){
        if (size == sizeof(BITMAPCOREHEADER)){
            /* Convert a old color table (3 byte RGBTRIPLEs) to a new
             * color table (4 byte RGBQUADs)
             */
            _lread(fh, (LPSTR)pRgb, (UINT)nNumColors * sizeof(RGBTRIPLE));

            for (i = nNumColors - 1; i >= 0; i--){
                RGBQUAD rgb;

                rgb.rgbRed      = ((RGBTRIPLE FAR *)pRgb)[i].rgbtRed;
                rgb.rgbBlue     = ((RGBTRIPLE FAR *)pRgb)[i].rgbtBlue;
                rgb.rgbGreen    = ((RGBTRIPLE FAR *)pRgb)[i].rgbtGreen;
                rgb.rgbReserved = (BYTE)0;

                pRgb[i] = rgb;
            }
        }
        else
            _lread(fh, (LPSTR)pRgb, (UINT)nNumColors * sizeof(RGBQUAD));
    }

    if (bf.bfOffBits != 0L){
        _llseek(fh, off + bf.bfOffBits, (UINT)SEEK_SET);
        }
    GlobalUnlock(hbi);
    return hbi;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  DibHeight(HANDLE hdib)                                     *
 *                                                                          *
 *  RETURNS    :  Height of DIB in pixels                                    *
 *                                                                          *
 ****************************************************************************/
DWORD FAR WINAPI
DibHeight (HANDLE hdib)
{
    LPBITMAPINFOHEADER lpbmi;
    LPBITMAPCOREHEADER lpbci;
    DWORD  size;

    if (!hdib)
      return 0;

    lpbmi      = (LPBITMAPINFOHEADER) GlobalLock(hdib);
    lpbci      = (LPBITMAPCOREHEADER) lpbmi;

    if (lpbmi->biSize == sizeof(BITMAPINFOHEADER))
      size = lpbmi->biHeight;
    else
      size = (DWORD) lpbci->bcHeight;

    GlobalUnlock (hdib);
    return  size;    
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  DibWidth(HANDLE hdib)                                     *
 *                                                                          *
 *  RETURNS    :  Width of DIB in pixels                                    *
 *                                                                          *
 ****************************************************************************/
DWORD FAR WINAPI
DibWidth (HANDLE hdib)
{
    LPBITMAPINFOHEADER lpbmi;
    LPBITMAPCOREHEADER lpbci;
    DWORD size;

    if (!hdib)
      return  0;

    lpbmi      = (LPBITMAPINFOHEADER) GlobalLock(hdib);
    lpbci      = (LPBITMAPCOREHEADER) lpbmi;

    if (lpbmi->biSize == sizeof(BITMAPINFOHEADER))
      size = lpbmi->biWidth;
    else
      size = (DWORD) lpbci->bcWidth;

    GlobalUnlock (hdib);
    return  size;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  DIBSetPixeslUnaligned (HANDLE hdib, BYTE *bytes)          *
 *                                                                          *
 ****************************************************************************/
BOOL FAR WINAPI
DIBSetPixelsUnaligned (HANDLE hdib, BYTE *bytes)
{
    LPBITMAPINFOHEADER lpbmi;
    LPBITMAPCOREHEADER lpbci;
    DWORD width, height;

    if (!hdib)
      return  FALSE;

    lpbmi      = (LPBITMAPINFOHEADER) GlobalLock(hdib);
    lpbci      = (LPBITMAPCOREHEADER) lpbmi;

    if (lpbmi->biSize == sizeof(BITMAPINFOHEADER))
    {
      width  = lpbmi->biWidth;
      height = lpbmi->biHeight;
    }
    else
    {
      width  = (DWORD) lpbci->bcWidth;
      height = (DWORD) lpbci->bcHeight;
    }

    {
      BYTE *dest_area =(LPSTR)lpbmi + (WORD)lpbmi->biSize + PaletteSize(lpbmi);
      BYTE *src_area = bytes;
      long scanline = height-1;
      long aligned_width = (width+3)&0xfffffffc;

      for (scanline = height-1; scanline>=0; scanline--)
      {
	BYTE *src = src_area + width*(height-scanline-1);
	BYTE *dest = dest_area + aligned_width * scanline;
	long  column = width;
	*(DWORD*)(dest+aligned_width-sizeof(DWORD)) = 0;  // zero-fill end word
      loop:
	if (column>=8)
	{
	  ((DWORD*)dest)[0] = ((DWORD*)src)[0];
	  ((DWORD*)dest)[1] = ((DWORD*)src)[1];
	  column -= 2*sizeof(DWORD);
	  src    += 2*sizeof(DWORD);
	  dest   += 2*sizeof(DWORD);
	  goto loop;
	}
	else if (column>0)
	{
	  column -= 1;
	  *dest++ = *src++;
	  goto loop;
	}
      }
    }

    GlobalUnlock (hdib);
    return  TRUE;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  PaletteSize(VOID FAR * pv)                                *
 *                                                                          *
 *  PURPOSE    :  Calculates the palette size in bytes. If the info. block  *
 *                is of the BITMAPCOREHEADER type, the number of colors is  *
 *                multiplied by 3 to give the palette size, otherwise the   *
 *                number of colors is multiplied by 4.                      *
 *                                                                          *
 *  RETURNS    :  Palette size in number of bytes.                          *
 *                                                                          *
 ****************************************************************************/
WORD PaletteSize (VOID FAR * pv)
{
    LPBITMAPINFOHEADER lpbi;
    WORD               NumColors;

    lpbi      = (LPBITMAPINFOHEADER)pv;
    NumColors = DibNumColors(lpbi);

    if (lpbi->biSize == sizeof(BITMAPCOREHEADER))
        return (WORD)(NumColors * sizeof(RGBTRIPLE));
    else
        return (WORD)(NumColors * sizeof(RGBQUAD));
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibNumColors(VOID FAR * pv)                                *
 *                                                                          *
 *  PURPOSE    : Determines the number of colors in the DIB by looking at   *
 *               the BitCount filed in the info block.                      *
 *                                                                          *
 *  RETURNS    : The number of colors in the DIB.                           *
 *                                                                          *
 ****************************************************************************/
WORD DibNumColors (VOID FAR * pv)
{
    INT                 bits;
    LPBITMAPINFOHEADER  lpbi;
    LPBITMAPCOREHEADER  lpbc;

    lpbi = ((LPBITMAPINFOHEADER)pv);
    lpbc = ((LPBITMAPCOREHEADER)pv);

    /*  With the BITMAPINFO format headers, the size of the palette
     *  is in biClrUsed, whereas in the BITMAPCORE - style headers, it
     *  is dependent on the bits per pixel ( = 2 raised to the power of
     *  bits/pixel).
     */
    if (lpbi->biSize != sizeof(BITMAPCOREHEADER)){
        if (lpbi->biClrUsed != 0)
            return (WORD)lpbi->biClrUsed;
        bits = lpbi->biBitCount;
    }
    else
        bits = lpbc->bcBitCount;

    switch (bits){
        case 1:
                return 2;
        case 4:
                return 16;
        case 8:
                return 256;
        default:
                /* A 24 bitcount DIB has no color table */
                return 0;
    }
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibFromBitmap()                                            *
 *                                                                          *
 *  PURPOSE    : Will create a global memory block in DIB format that       *
 *               represents the Device-dependent bitmap (DDB) passed in.    *
 *                                                                          *
 *  RETURNS    : A handle to the DIB                                        *
 *                                                                          *
 ****************************************************************************/
HANDLE FAR WINAPI
DibFromBitmap (HBITMAP hbm, DWORD biStyle, WORD biBits, HPALETTE hpal)
{
    BITMAP               bm;
    BITMAPINFOHEADER     bi;
    BITMAPINFOHEADER FAR *lpbi;
    DWORD                dwLen;
    HANDLE               hdib;
    HANDLE               h;
    HDC                  hdc;

    if (!hbm)
        return NULL;

    if (hpal == NULL)
        hpal = GetStockObject(DEFAULT_PALETTE);

    GetObject(hbm,sizeof(bm),(LPSTR)&bm);

    if (biBits == 0)
        biBits =  bm.bmPlanes * bm.bmBitsPixel;

    bi.biSize               = sizeof(BITMAPINFOHEADER);
    bi.biWidth              = bm.bmWidth;
    bi.biHeight             = bm.bmHeight;
    bi.biPlanes             = 1;
    bi.biBitCount           = biBits;
    bi.biCompression        = biStyle;
    bi.biSizeImage          = 0;
    bi.biXPelsPerMeter      = 0;
    bi.biYPelsPerMeter      = 0;
    bi.biClrUsed            = 0;
    bi.biClrImportant       = 0;

    dwLen  = bi.biSize + PaletteSize(&bi);

    hdc = GetDC(NULL);
    hpal = SelectPalette(hdc,hpal,FALSE);
         RealizePalette(hdc);

    hdib = GlobalAlloc(GHND,dwLen);

    if (!hdib){
        SelectPalette(hdc,hpal,FALSE);
        ReleaseDC(NULL,hdc);
        return NULL;
    }

    lpbi = (VOID FAR *)GlobalLock(hdib);

    *lpbi = bi;

    /*  call GetDIBits with a NULL lpBits param, so it will calculate the
     *  biSizeImage field for us
     */
    GetDIBits(hdc, hbm, 0L, (DWORD)bi.biHeight,
        (LPBYTE)NULL, (LPBITMAPINFO)lpbi, (DWORD)DIB_RGB_COLORS);

    bi = *lpbi;
    GlobalUnlock(hdib);

    /* If the driver did not fill in the biSizeImage field, make one up */
    if (bi.biSizeImage == 0){
        bi.biSizeImage = WIDTHBYTES((DWORD)bm.bmWidth * biBits) * bm.bmHeight;

        if (biStyle != BI_RGB)
            bi.biSizeImage = (bi.biSizeImage * 3) / 2;
    }

    /*  realloc the buffer big enough to hold all the bits */
    dwLen = bi.biSize + PaletteSize(&bi) + bi.biSizeImage;
    if (h = GlobalReAlloc(hdib,dwLen,0))
        hdib = h;
    else{
        GlobalFree(hdib);
        hdib = NULL;

        SelectPalette(hdc,hpal,FALSE);
        ReleaseDC(NULL,hdc);
        return hdib;
    }

    /*  call GetDIBits with a NON-NULL lpBits param, and actualy get the
     *  bits this time
     */
    lpbi = (VOID FAR *)GlobalLock(hdib);

    if (GetDIBits( hdc,
                   hbm,
                   0L,
                   (DWORD)bi.biHeight,
                   (LPBYTE)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi),
                   (LPBITMAPINFO)lpbi, (DWORD)DIB_RGB_COLORS) == 0){
         GlobalUnlock(hdib);
         hdib = NULL;
         SelectPalette(hdc,hpal,FALSE);
         ReleaseDC(NULL,hdc);
         return NULL;
    }

    bi = *lpbi;
    GlobalUnlock(hdib);

    SelectPalette(hdc,hpal,FALSE);
    ReleaseDC(NULL,hdc);
    return hdib;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : CreateDib()                                                *
 *                                                                          *
 *  PURPOSE    : Will create a global memory block in DIB format.           *
 *                                                                          *
 *  RETURNS    : A handle to the DIB                                        *
 *                                                                          *
 ****************************************************************************/
HANDLE FAR WINAPI
CreateDIB (int width, int height, DWORD biStyle, WORD biBits, HPALETTE hpal)
{
    BITMAPINFOHEADER     bi;
    BITMAPINFO      FAR *lpbi;
    DWORD                dwLen;
    HANDLE               hdib;
    HANDLE               h;
    PALETTEENTRY         paletteentry[256];
    UINT                 palettesize;
    UINT                 i;

    if (hpal == NULL)
        hpal = GetStockObject(DEFAULT_PALETTE);

    bi.biSize               = sizeof(BITMAPINFOHEADER);
    bi.biWidth              = width;
    bi.biHeight             = height;
    bi.biPlanes             = 1;
    bi.biBitCount           = biBits;
    bi.biCompression        = biStyle;
    bi.biSizeImage          = 0;
    bi.biXPelsPerMeter      = 0;
    bi.biYPelsPerMeter      = 0;
    bi.biClrUsed            = 0;
    bi.biClrImportant       = 0;

    dwLen  = bi.biSize + PaletteSize(&bi);

    hdib = GlobalAlloc(GHND,dwLen);

    if (!hdib)
        return NULL;

    /* Invent a biSizeImage field value */
    bi.biSizeImage = WIDTHBYTES((DWORD)width * biBits) * height;

    if (biStyle != BI_RGB)
      bi.biSizeImage = (bi.biSizeImage * 3) / 2;


    lpbi = (VOID FAR *)GlobalLock(hdib);

    /* query the palette and copy its colors */
    palettesize = GetPaletteEntries (hpal, 0, 256, paletteentry);
    for (i = 0; i<palettesize; i++)
    {
      lpbi->bmiColors[i].rgbRed   = paletteentry[i].peRed;
      lpbi->bmiColors[i].rgbGreen = paletteentry[i].peGreen;
      lpbi->bmiColors[i].rgbBlue  = paletteentry[i].peBlue;
      lpbi->bmiColors[i].rgbReserved = 0;
    }

    lpbi->bmiHeader = bi;

    GlobalUnlock(hdib);

    /*  realloc the buffer big enough to hold all the bits */
    dwLen = bi.biSize + PaletteSize(&bi) + bi.biSizeImage;
    if (h = GlobalReAlloc(hdib,dwLen,GMEM_ZEROINIT))
        hdib = h;
    else{
        GlobalFree(hdib);
        return  NULL;;
    }

    return hdib;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : BitmapFromDib(HANDLE hdib, HPALETTE hpal)                  *
 *                                                                          *
 *  PURPOSE    : Will create a DDB (Device Dependent Bitmap) given a global *
 *               handle to a memory block in CF_DIB format                  *
 *                                                                          *
 *  RETURNS    : A handle to the DDB.                                       *
 *                                                                          *
 ****************************************************************************/
HBITMAP FAR WINAPI
BitmapFromDib (HANDLE hdib, HPALETTE hpal)
{
    LPBITMAPINFOHEADER  lpbi;
    HPALETTE            hpalT;
    HDC                 hdc;
    HBITMAP             hbm;

    StartWait();

    if (!hdib)
        return NULL;

    lpbi = (VOID FAR *)GlobalLock(hdib);

    if (!lpbi)
        return NULL;

    hdc = GetDC(NULL);

    if (hpal){
        hpalT = SelectPalette(hdc,hpal,FALSE);
        RealizePalette(hdc);     // GDI Bug...????
    }

    hbm = CreateDIBitmap(hdc,
                (LPBITMAPINFOHEADER)lpbi,
                (LONG)CBM_INIT,
                (LPSTR)lpbi + lpbi->biSize + PaletteSize(lpbi),
                (LPBITMAPINFO)lpbi,
                DIB_RGB_COLORS );

    if (hpal)
        SelectPalette(hdc,hpalT,FALSE);

    ReleaseDC(NULL,hdc);
    GlobalUnlock(hdib);

    EndWait();

    return hbm;
}
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DrawBitmap(HDC hdc, int x, int y, HBITMAP hbm, DWORD rop)  *
 *                                                                          *
 *  PURPOSE    : Draws bitmap <hbm> at the specifed position in DC <hdc>    *
 *                                                                          *
 *  RETURNS    : Return value of BitBlt()                                   *
 *                                                                          *
 ****************************************************************************/
BOOL DrawBitmap (
    HDC    hdc,
    INT    x,
    INT    y,
    HBITMAP    hbm,
    DWORD          rop)
{
    HDC       hdcBits;
    BITMAP    bm;
//    HPALETTE  hpalT;  
    BOOL      f;

    if (!hdc || !hbm)
        return FALSE;

    hdcBits = CreateCompatibleDC(hdc);
    GetObject(hbm,sizeof(BITMAP),(LPSTR)&bm);
    SelectObject(hdcBits,hbm);
    f = BitBlt(hdc,0,0,bm.bmWidth,bm.bmHeight,hdcBits,0,0,rop);
    DeleteDC(hdcBits);

    return f;
        UNREFERENCED_PARAMETER(y);
        UNREFERENCED_PARAMETER(x);
}
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibBlt( HDC hdc,                                           *
 *                       int x0, int y0,                                    *
 *                       int dx, int dy,                                    *
 *                       HANDLE hdib,                                       *
 *                       int x1, int y1,                                    *
 *                       LONG rop)                                          *
 *                                                                          *
 *  PURPOSE    : Draws a bitmap in CF_DIB format, using SetDIBits to device.*
 *               taking the same parameters as BitBlt().                    *
 *                                                                          *
 *  RETURNS    : TRUE  - if function succeeds.                              *
 *               FALSE - otherwise.                                         *
 *                                                                          *
 ****************************************************************************/
BOOL FAR WINAPI
DibBlt (HDC    hdc,
	INT    x0,
	INT    y0,
	INT    dx,
	INT    dy,
	HANDLE hdib,
	INT    x1,
	INT    y1,
	LONG   rop)
{
    LPBITMAPINFOHEADER   lpbi;
//    HPALETTE           hpal,hpalT;
    LPSTR                pBuf;
//    HDC                hdcMem;
//    HBITMAP            hbm,hbmT;

    if (!hdib)
        return PatBlt(hdc,x0,y0,dx,dy,rop);

    lpbi = (VOID FAR *)GlobalLock(hdib);

    if (!lpbi)
        return FALSE;

    pBuf = (LPSTR)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi);
    SetDIBitsToDevice (hdc, x0, y0, dx, dy,
                       x1,y1,
                       x1,
                       dy,
                       pBuf, (LPBITMAPINFO)lpbi,
                       DIB_RGB_COLORS );

    GlobalUnlock(hdib);
    return TRUE;
}
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : StretchDibBlt( HDC hdc,                                    *
 *                              int x, int y,                               *
 *                              int dx, int dy,                             *
 *                              HANDLE hdib,                                *
 *                              int x0, int y0,                             *
 *                              int dx0, int dy0,                           *
 *                              LONG rop)                                   *
 *                                                                          *
 *  PURPOSE    : Draws a bitmap in CF_DIB format, using StretchDIBits()     *
 *               taking the same parameters as StretchBlt().                *
 *                                                                          *
 *  RETURNS    : TRUE  - if function succeeds.                              *
 *               FALSE - otherwise.                                         *
 *                                                                          *
 ****************************************************************************/
BOOL StretchDibBlt (
    HDC hdc,
    INT x,
    INT y,
    INT dx,
    INT dy,
    HANDLE hdib,
    INT x0,
    INT y0,
    INT dx0,
    INT dy0,
    LONG rop)

{
    LPBITMAPINFOHEADER lpbi;
    LPSTR        pBuf;
    BOOL         f;

    if (!hdib)
        return PatBlt(hdc,x,y,dx,dy,rop);

    lpbi = (VOID FAR *)GlobalLock(hdib);

    if (!lpbi)
        return FALSE;

    pBuf = (LPSTR)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi);

    f = StretchDIBits ( hdc,
                        x, y,
                        dx, dy,
                        x0, y0,
                        dx0, dy0,
                        pBuf, (LPBITMAPINFO)lpbi,
                        DIB_RGB_COLORS,
                        rop);

    GlobalUnlock(hdib);
    return f;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : CopyBitmap (HBITMAP hbm)                                   *
 *                                                                          *
 *  PURPOSE    : Copies the given bitmap to another.                        *
 *                                                                          *
 *  RETURNS    : A handle to the new bitmap.                                *
 *                                                                          *
 ****************************************************************************/
HBITMAP FAR WINAPI
CopyBitmap (HBITMAP hbm)
{
    BITMAP  bm;

    if (!hbm)
         return NULL;

    GetObject (hbm, sizeof(BITMAP), (LPSTR)&bm);

    return CropBitmap (hbm, 0, 0, bm.bmWidth, bm.bmHeight);
}
/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  CropBitmap (hbm, left, top, right, bottom)                *
 *                                                                          *
 *  PURPOSE    :  Crops a bitmap to a new size specified by the lprect      *
 *                parameter.                                                *
 *                                                                          *
 *  RETURNS    :  A handle to the new bitmap.                               *
 *                                                                          *
 ****************************************************************************/
HBITMAP FAR WINAPI
CropBitmap (HBITMAP hbm, int left, int top, int right, int bottom)
{
    HDC     hMemDCsrc;
    HDC     hMemDCdst;
    HDC     hdc;
    HBITMAP hNewBm;
    BITMAP  bm;
    INT     dx,dy;

    if (!hbm)
         return NULL;

    hdc = GetDC (NULL);
    hMemDCsrc = CreateCompatibleDC (hdc);
    hMemDCdst = CreateCompatibleDC (hdc);

    GetObject (hbm, sizeof(BITMAP), (LPSTR)&bm);
    dx = right  - left;
    dy = bottom - top;

    /*hNewBm = +++CreateBitmap - Not Recommended(use CreateDIBitmap)+++ (dx, dy, bm.bmPlanes, bm.bmBitsPixel, NULL);*/
    hNewBm = CreateBitmap(dx, dy, bm.bmPlanes, bm.bmBitsPixel, NULL);
    if (hNewBm){
        SelectObject (hMemDCsrc, hbm);
        SelectObject (hMemDCdst, hNewBm);

        BitBlt (hMemDCdst,
                0,
                0,
                dx,
                dy,
                hMemDCsrc,
                left,
                top,
                SRCCOPY);
    }

    ReleaseDC (NULL,hdc);
    DeleteDC (hMemDCsrc);
    DeleteDC (hMemDCdst);
    return hNewBm;
}


 /************* PRIVATE ROUTINES TO READ/WRITE MORE THAN 64K ***************/
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : lread(int fh, VOID FAR *pv, DWORD ul)                      *
 *                                                                          *
 *  PURPOSE    : Reads data in steps of 32k till all the data has been read.*
 *                                                                          *
 *  RETURNS    : 0 - If read did not proceed correctly.                     *
 *               number of bytes read otherwise.                            *
 *                                                                          *
 ****************************************************************************/
DWORD PASCAL lread (
    INT       fh,
    VOID FAR      *pv,
    DWORD             ul)
{
    DWORD     ulT = ul;
    BYTE HUGE_T *hp = pv;

    while (ul > (DWORD)MAXREAD) {
        if (_lread(fh, (LPSTR)hp, (UINT)MAXREAD) != MAXREAD)
                return 0;
        ul -= MAXREAD;
        hp += MAXREAD;
    }
    if (_lread(fh, (LPSTR)hp, (UINT)ul) != (UINT)ul)
        return 0;
    return ulT;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : lwrite(int fh, VOID FAR *pv, DWORD ul)                     *
 *                                                                          *
 *  PURPOSE    : Writes data in steps of 32k till all the data is written.  *
 *                                                                          *
 *  RETURNS    : 0 - If write did not proceed correctly.                    *
 *               number of bytes written otherwise.                         *
 *                                                                          *
 ****************************************************************************/
DWORD PASCAL lwrite (
    INT      fh,
    VOID FAR     *pv,
    DWORD            ul)
{
    DWORD     ulT = ul;
    BYTE HUGE_T *hp = pv;

    while (ul > MAXREAD) {
        if (_lwrite(fh, (LPSTR)hp, (UINT)MAXREAD) != MAXREAD)
                return 0;
        ul -= MAXREAD;
        hp += MAXREAD;
    }
    if (_lwrite(fh, (LPSTR)hp, (UINT)ul) != (UINT)ul)
        return 0;                 

    return ulT;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : ReadBitMapFileHeaderandConvertToDwordAlign(HFILE fh, LPBITMAPFILEHEADER pbf)
 *                                                                          *
 *  PURPOSE    : read file header (which is packed) and convert into unpacked BITMAPFILEHEADER strucutre
 *                                                                          *
 *  RETURNS    : VOID
 *                                                                          *
 ****************************************************************************/

VOID ReadBitMapFileHeaderandConvertToDwordAlign(HFILE fh, LPBITMAPFILEHEADER pbf, LPDWORD lpdwoff)
{
        DWORD off;

        off = _llseek(fh, 0L, (UINT) SEEK_CUR);
        *lpdwoff = off;

/*              BITMAPFILEHEADER STRUCUTURE is as follows 
 *              BITMAPFILEHEADER
 *              WORD    bfType 
 >          ....                  <     add WORD if packed here!
 *              DWORD   bfSize 
 *              WORD    bfReserved1
 *              WORD    bfReserved2
 *              DWORD   bfOffBits 
 *                      This is the packed format, unpacked adds a WORD after bfType
 */

        /* read in bfType*/
        _lread(fh, (LPSTR) &pbf->bfType, sizeof(WORD));
        /* read in last 3 dwords*/
        _lread(fh, (LPSTR) &pbf->bfSize, sizeof(DWORD) * 3);

}



/****************************************************************************
 *                                                                          *
 *  FUNCTION   : WriteMapFileHeaderandConvertFromDwordAlignToPacked(HFILE fh, LPBITMAPFILEHEADER pbf)
 *                                                                          *
 *  PURPOSE    : write header structure (which NOT packed) and write it PACKED
 *                                                                          *
 *  RETURNS    : VOID
 *                                                                          *
 ****************************************************************************/

VOID WriteMapFileHeaderandConvertFromDwordAlignToPacked(HFILE fh, LPBITMAPFILEHEADER pbf)
{

        /* write bfType*/
    _lwrite(fh, (LPSTR)&pbf->bfType, (UINT)sizeof (WORD));
        /* now pass over extra word, and only write next 3 DWORDS!*/
        _lwrite(fh, (LPSTR)&pbf->bfSize, sizeof(DWORD) * 3);
}
