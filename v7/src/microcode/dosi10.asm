;;; -*-Midas-*-
;;;
;;; $Id: dosi10.asm,v 1.3 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1992, 1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;

.386
.model tiny
	.code

FRAME equ 8

;;
;; STACK MAP: (after initial 'PUSH EBP')
;;
;; (EBP+FRAME)+24          unsigned long pString (segment16:offset16 format)
;; (EBP+FRAME)+20          unsigned long lRow
;; (EBP+FRAME)+16          unsigned long lColumn
;; (EBP+FRAME)+12          unsigned long lCharCount
;; (EBP+FRAME)+08          unsigned long lSingleAttribute
;; (EBP+FRAME)+04          unsigned long lPageNumber
;; (EBP+FRAME)+00          unsigned long lAttributeMode
;;	--------------------------------
;; (EBP+FRAME)-04          EIP return address
;; (EBP+FRAME)-08          Old EBP
;;

  public _asm_bios__write_string_attr
_asm_bios__write_string_attr proc near
  push EBP
  mov EBP,ESP

  lea ESP,-20[ESP]

  push EBX
  push ESI
  push ES

  mov EAX,04[EBP+FRAME]  ;; BH = iPageNumber
  mov BH,AL
  mov EAX,08[EBP+FRAME]  ;; BL = iSingleAttribute
  mov BL,AL
  mov ECX,12[EBP+FRAME]  ;; CX = iCharCount
  mov EAX,16[EBP+FRAME]  ;; DL = iColumn
  mov DL,AL
  mov EAX,20[EBP+FRAME]  ;; DH = iRow
  mov DH,AL
  mov EAX,24[EBP+FRAME]  ;; ES = pString segment
  ror EAX,16
  mov WORD PTR -18[EBP],10h
  mov -16[EBP],AX
  mov -14[EBP],AX
  mov -12[EBP],AX
  mov -10[EBP],AX
  mov EAX,00[EBP+FRAME]  ;; AL = iAttributeMode
  mov AH,13h             ;; AH = 0x13
  mov -08[EBP],EAX
  mov -04[EBP],EDX

  push ebp
  mov ESI,24[EBP+FRAME]  ;; BP = pString offset
  lea edx,-18[EBP]
  mov BP,SI
  mov AX,2511h
  int 21h
  pop ebp

  pop ES
  pop ESI
  pop EBX

  mov ESP,EBP

  pop EBP
  ret
_asm_bios__write_string_attr endp

;;;; Example test program:
;;;;  386i10t.exe: 386i10t.c 386i10.asm
;;;;    $(CC) $(CFLAGS) -L/m -o386i10t.exe 386i10.obj 386i10t.obj $(FLASHTEK)
;;;;
;;;; #include "386int10.h"
;;;;
;;;; unsigned long RealModeBufferParagraph = 0;
;;;; char *pRealModeBuffer = NULL;
;;;;
;;;; void bios_write_string_attributed(char *pString, int iColumn,
;;;;   int iRow, int iSingleAttribute)
;;;; {
;;;;   strcpy(pRealModeBuffer,pString);
;;;;   asm_bios__write_string_attr(1,0,iSingleAttribute,strlen(pString),
;;;;     iColumn,iRow,((RealModeBufferParagraph << 16) + 0));
;;;; }
;;;;
;;;; static char *lpszStatic = "This is a static string.";
;;;;
;;;; main()
;;;; {
;;;;   char *lpszAuto = "This is an auto variable string.";
;;;;
;;;;   {
;;;;     union REGS rIn;
;;;;     union REGS rOut;
;;;;
;;;;     rIn.h.ah = 0x48;
;;;;     rIn.x.bx = 256;
;;;;     int86(0x21,&rIn,&rOut);
;;;;       /* Ought to check for success, carry flag cleared if successful */
;;;;     if (rOut.x.flags & 0x1)
;;;;       {
;;;;         printf("The attempt to allocate real-mode memory failed.\n");
;;;;         exit(1);
;;;;       }
;;;;     else
;;;;       {
;;;;         printf("The attempt to allocate real-mode memory appears successful.\n");
;;;;       }
;;;;     pRealModeBuffer = (char *) rOut.e.ebx;
;;;;     RealModeBufferParagraph = rOut.x.ax;
;;;;     printf("RealModeBufferParagraph = %lx",RealModeBufferParagraph);
;;;;   }
;;;;
;;;;   printf("Hello, world.\n");
;;;;
;;;;   bios_write_string_attributed(lpszStatic,0,0,112);
;;;;   bios_write_string_attributed(lpszAuto,0,1,112);
;;;;   bios_write_string_attributed("This is an unnamed string literal.\n",0,2,112);
;;;;
;;;;   printf("Good-bye, world.\n");
;;;;
;;;; }
;;;;

end
