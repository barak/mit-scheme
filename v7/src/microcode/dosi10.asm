;;; -*-Midas-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosi10.asm,v 1.1 1992/07/28 18:12:49 jinx Exp $
;;;
;;;	Copyright (c) 1992 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
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
