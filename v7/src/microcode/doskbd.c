/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/doskbd.c,v 1.6 1992/05/28 19:08:24 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
MIT in each case. */

/* These flags determine how the code will behave. */

#define DOSX_USE_INT_INTERCEPT
#define DOSX_RM_HANDLER_UNTOUCHED
#define DOSX_PM_HANDLER_UNTOUCHED
/* #define DOSX_RM_HANDLER_REAL */
#define DPMI_RM_HANDLER_REAL
#define DPMI_PM_HANDLER_UNTOUCHED

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <bios.h>
#include <dos.h>
#include <int.h>
#include "msdos.h"

#ifdef getDS
#undef getDS
#endif

#include "dossys.h"
#include "dosinsn.h"
#include "doskbd.h"

#ifndef ELOOP
#  define ELOOP 2000
#endif

#ifndef EFAULT
#  define EFAULT 2001
#endif

/* Tables mapping scan codes to ASCII characters.
   Entries with NULL (\0) should not be mapped by the
   Scheme keyboard ISR.  Let the default handler map them.
 */

static unsigned char
shifted_scan_code_to_ascii[] =
{ 
	'\0',		/* 0 */
	'\033',		/* 1 */
	'!',		/* 2 */
	'@',		/* 3 */
	'#',		/* 4 */
	'$',		/* 5 */
	'%',		/* 6 */
	'^',		/* 7 */
	'&',		/* 8 */
	'*',		/* 9 */
	'(',		/* 10 */
	')',		/* 11 */
	'_',		/* 12 */
	'+',		/* 13 */
	'\177',		/* 14 */
	'\t',		/* 15 */
	'Q',		/* 16 */
	'W',		/* 17 */
	'E',		/* 18 */
	'R',		/* 19 */
	'T',		/* 20 */
	'Y',		/* 21 */
	'U',		/* 22 */
	'I',		/* 23 */
	'O',		/* 24 */
	'P',		/* 25 */
	'{',		/* 26 */
	'}',		/* 27 */
	'\r',		/* 28 */
	'\0',		/* 29 */
	'A',		/* 30 */
	'S',		/* 31 */
	'D',		/* 32 */
	'F',		/* 33 */
	'G',		/* 34 */
	'H',		/* 35 */
	'J',		/* 36 */
	'K',		/* 37 */
	'L',		/* 38 */
	':',		/* 39 */
	'\"',		/* 40 */
	'~',		/* 41 */
	'\0',		/* 42 */
	'|',		/* 43 */
	'Z',		/* 44 */
	'X',		/* 45 */
	'C',		/* 46 */
	'V',		/* 47 */
	'B',		/* 48 */
	'N',		/* 49 */
	'M',		/* 50 */
	'<',		/* 51 */
	'>',		/* 52 */
	'?',		/* 53 */
	'\0',		/* 54 */
	'\0',		/* 55 */
	'\0',		/* 56 */
	' '		/* 57 */
  };
    
static unsigned char
unshifted_scan_code_to_ascii[] =
{
	'\0',		/* 0 */
	'\033',		/* 1 */
	'1',		/* 2 */
	'2',		/* 3 */
	'3',		/* 4 */
	'4',		/* 5 */
	'5',		/* 6 */
	'6',		/* 7 */
	'7',		/* 8 */
	'8',		/* 9 */
	'9',		/* 10 */
	'0',		/* 11 */
	'-',		/* 12 */
	'=',		/* 13 */
	'\177',		/* 14 */
	'\t',		/* 15 */
	'q',		/* 16 */
	'w',		/* 17 */
	'e',		/* 18 */
	'r',		/* 19 */
	't',		/* 20 */
	'y',		/* 21 */
	'u',		/* 22 */
	'i',		/* 23 */
	'o',		/* 24 */
	'p',		/* 25 */
	'[',		/* 26 */
	']',		/* 27 */
	'\r',		/* 28 */
	'\0',		/* 29 */
	'a',		/* 30 */
	's',		/* 31 */
	'd',		/* 32 */
	'f',		/* 33 */
	'g',		/* 34 */
	'h',		/* 35 */
	'j',		/* 36 */
	'k',		/* 37 */
	'l',		/* 38 */
	';',		/* 39 */
	'\'',		/* 40 */
	'`',		/* 41 */
	'\0',		/* 42 */
	'\\',		/* 43 */
	'z',		/* 44 */
	'x',		/* 45 */
	'c',		/* 46 */
	'v',		/* 47 */
	'b',		/* 48 */
	'n',		/* 49 */
	'm',		/* 50 */
	',',		/* 51 */
	'.',		/* 52 */
	'/',		/* 53 */
	'\0',		/* 54 */
	'\0',		/* 55 */
	'\0',		/* 56 */
	' '		/* 57 */
  };

static unsigned char modifier_mask = 0x4f;

union RM_address
{
  unsigned fp;
  struct
    {
      unsigned short off;
      unsigned short seg;
    } x;
};

dos_boolean
under_QEMM_386_p (void)
{
  unsigned int i;
  union REGS iregs, oregs;

  iregs.h.al = 0x01;
  iregs.x.bx = 0x5145;
  iregs.x.cx = 0x4d4d;
  iregs.x.dx = 0x3432;

  for (i = 0xc0; i <= 0xff; i++)
  {
    iregs.h.ah = i;
    int86 (0x2f, &iregs, &oregs);
    if (oregs.x.bx == 0x4f4b)
      return (dos_true);
  }
  return (dos_false);
}

dos_boolean
under_DPMI_p (void)
{
  union REGS regs;
  
  regs.e.eax = 0x1686;
  int86 (0x2f, &regs, &regs);
  return (regs.x.ax == 0);
}

static void
normalize_RM_address (union RM_address * addr)
{
  if (addr->x.off > 0xf)
  {
    addr->x.seg += (addr->x.off >> 4);
    addr->x.off = (addr->x.off & 0xf);
  }
  return;
}

static dos_boolean
install_kbd_hook_p (char * var_name)
{
  extern int strcmp_ci (char *, char *);
  char * envvar = (DOS_getenv (var_name));

  if ((envvar != NULL) && ((strcmp_ci (envvar, "false")) == 0))
    return (dos_false);
  else
    return (dos_true);
}

/*
   We would like to use Zortech's int_intercept with the following
   routine under DOSX (or Phar Lap).

   Unfortunately, it does not work under QEMM386 or under MS Windows 3.1.
   The real-mode call-back routine is apparently just plain broken.

   In addition, bypassing DOSX under DPMI 0.9 and using DPMI's
   real-mode call backs does not work consistently, and the keyboard
   interrupt happens only in real mode since we are capturing the
   interrupt DOS uses to tell the BIOS that a scan code has arrived
   after doing the handshake with the keyboard device itself.

   Thus, under DPMI, we install our own hard-coded real-mode keyboard
   driver and don't bother with a protected mode handler.  All the
   code is here in case it can be turned on in the future, perhaps for
   a different DOS Extender or a new version of DPMI.

   Apparently telling DOSX to install a protected mode handler and
   making it reflect real mode interrupts to protected mode does not
   work consistently under QEMM386 either, thus we are now installing
   a real-mode handler no matter what, although using different
   mechanisms under DPMI and not DPMI.
 */

#ifdef DOSX_USE_INT_INTERCEPT

#define PC_KBD_ALT_MASK			0x8
#define PC_KBD_CTRL_MASK		0x4
#define PC_KBD_SHIFT_MASK		0x3
#define PC_KBD_CAPSL_MASK		0x40

#define DOS_HOOK_TRANSLATE_KEYSTROKE	0x4f
#define DOS_KBD_FUNC_RECORD_KEYSTROKE	0x5

int
bios_keyboard_handler(struct INT_DATA *pd)
{
  unsigned char scan_code, chord, ascii;
  union REGS regs;

  if (pd->regs.h.ah != DOS_HOOK_TRANSLATE_KEYSTROKE)
    return (INTERRUPT_CHAIN_NEXT);

  scan_code = (pd->regs.h.al);
  if (scan_code >= (sizeof (shifted_scan_code_to_ascii)))
    return (INTERRUPT_CHAIN_NEXT);

  chord = ((bioskey (_KEYBRD_SHIFTSTATUS)) & modifier_mask);

  if ((chord == 0) || (chord == PC_KBD_ALT_MASK))
    ascii = ((int) unshifted_scan_code_to_ascii[scan_code]);
  else
    ascii = ((int) shifted_scan_code_to_ascii[scan_code]);

  if (ascii == 0)
    return (INTERRUPT_CHAIN_NEXT);
  if ((chord & PC_KBD_CTRL_MASK) != 0)
    ascii &= ~0x60;			/* Controlify */
  if (chord & PC_KBD_ALT_MASK)
    ascii |= 0x80;			/* Metafy */
  if (ascii == 0360)
    return (INTERRUPT_CHAIN_NEXT);	/* Problems with M-p */
  if ((ascii == 0200) || (ascii == 0))
    scan_code = 3;			/* Problems with C-Space */

  /* Insert metafied char in bios buffer. */
  regs.h.ah = DOS_KBD_FUNC_RECORD_KEYSTROKE;
  regs.h.ch = scan_code;
  regs.h.cl = ascii;
  int86 (DOS_INTVECT_KEYBOARD_REQUEST, &regs, &regs);

  pd->regs.e.flags &= ~1;		/* clear CF, scan code ignored! */
  return (INTERRUPT_RETURN);
}
#endif /* DOSX_USE_INT_INTERCEPT */

static void
DPMI_PM_getvector (unsigned vecnum, unsigned * eip, unsigned * cs)
{
  union REGS regs;
  
  regs.x.ax = 0x204;
  regs.h.bl = (vecnum & 0xff);
  int86 (0x31, &regs, &regs);
  * eip = regs.e.edx;
  * cs = ((unsigned) regs.x.cx);
  return;
}

static int
DPMI_PM_setvector (unsigned vecnum, unsigned eip, unsigned cs)
{
  union REGS regs;
  
  regs.x.ax = 0x205;
  regs.h.bl = (vecnum & 0xff);
  regs.e.edx = eip;
  regs.x.cx = ((unsigned short) cs);
  int86 (0x31, &regs, &regs);
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}

static void
DPMI_RM_getvector (unsigned vecnum, unsigned short * ip, unsigned short * cs)
{
  union REGS regs;
  
  regs.x.ax = 0x200;
  regs.h.bl = (vecnum & 0xff);
  int86 (0x31, &regs, &regs);
  * ip = regs.x.dx;
  * cs = regs.x.cx;
  return;
}

static int
DPMI_RM_setvector (unsigned vecnum, unsigned short ip, unsigned short cs)
{
  union REGS regs;
  
  regs.x.ax = 0x201;
  regs.h.bl = (vecnum & 0xff);
  regs.x.cx = cs;
  regs.x.dx = ip;
  int86 (0x31, &regs, &regs);
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}

#ifdef DPMI_RM_HANDLER_PROTECTED

struct DPMI_RM_REGS
{
  unsigned long edi;		/* 0 */
  unsigned long esi;		/* 4 */
  unsigned long ebp;		/* 8 */
  unsigned long esp;		/* 12 */
  unsigned long ebx;		/* 16 */
  unsigned long edx;		/* 20 */
  unsigned long ecx;		/* 24 */
  unsigned long eax;		/* 28 */
  unsigned short flags;		/* 30 */
  unsigned short es;		/* 32 */
  unsigned short ds;		/* 34 */
  unsigned short fs;		/* 36 */
  unsigned short gs;		/* 38 */
  unsigned short ip;		/* 40 */
  unsigned short cs;		/* 42 */
  unsigned short sp;		/* 44 */
  unsigned short ss;		/* 48 */
  unsigned short pad;		/* 50 */
  unsigned long old_vector_ip;	/* 52 */
  unsigned long old_vector_cs;	/* 56 */
};

static int
DPMI_allocate_RM_call_back (unsigned short * cb_ip,
			    unsigned short * cb_cs,
			    unsigned eip, unsigned cs,
			    unsigned RM_regs, unsigned ds)
{
  union REGS regs;
  struct SREGS sregs;
  
  segread (& sregs);
  regs.x.ax = 0x303;
  regs.e.esi = eip;
  sregs.ds = cs;
  regs.e.edi = RM_regs;
  sregs.es = ds;

  int86x (0x31, &regs, &regs, &sregs);
  * cb_ip = regs.x.dx;
  * cb_cs = regs.x.cx;
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}

static int
DPMI_free_RM_call_back (unsigned short cb_ip, unsigned short cb_cs)
{
  union REGS regs;
  
  regs.x.ax = 0x304;
  regs.x.cx = cb_cs;
  regs.x.dx = cb_ip;
  int86 (0x31, &regs, &regs);
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}

#endif /* DPMI_RM_HANDLER_PROTECTED */

#ifdef DPMI_RM_HANDLER_REAL

static int
DPMI_allocate_DOS_block (unsigned short size,
			 unsigned short * rm_seg,
			 unsigned short * pm_sel)
{
  union REGS regs;
  
  regs.x.ax = 0x100;
  regs.x.bx = ((((unsigned) size) + 15) >> 4);	/* paragraphs */
  int86 (0x31, & regs, & regs);
  * rm_seg = regs.x.ax;
  * pm_sel = regs.x.dx;
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}

static int
DPMI_free_DOS_block (unsigned short selector)
{
  union REGS regs;
  
  regs.x.ax = 0x101;
  regs.x.dx = selector;
  int86 (0x31, & regs, & regs);
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}

#endif /* DPMI_RM_HANDLER_REAL */

#ifdef DOSX_RM_HANDLER_REAL

static int
DOSX_allocate_DOS_block (unsigned short size,
			 unsigned short * rm_seg)
{
  union REGS regs;
#if 0
  
  regs.x.ax = 0x25c0;
  regs.x.bx = ((((unsigned) size) + 15) >> 4);	/* paragraphs */
  int86 (0x21, & regs, & regs);
  if ((regs.e.flags & 1) == 0)
  {
    * rm_seg = regs.x.ax;
    return (DOS_SUCCESS);
  }
  if (regs.x.ax == 0x8)
    errno = ENOMEM;
  else
    errno = EFAULT;
  return (DOS_FAILURE);

#else /* not 0 */

  regs.h.ah = 0x48;
  regs.x.bx = ((((unsigned) size) + 15) >> 4);	/* paragraphs */
  int86 (0x21, & regs, & regs);
  * rm_seg = regs.x.ax;
  if ((regs.e.flags & 1) != 0)
  {
    errno = ENOMEM;
    return (DOS_FAILURE);
  }
  return (DOS_SUCCESS);

#endif /* 0 */
}

static int
DOSX_free_DOS_block (unsigned short seg)
{
  union REGS regs;
#if 0
  
  regs.x.ax = 0x25c1;
  regs.x.cx = seg;
  int86 (0x21, & regs, & regs);

#else /* not 0 */

  struct SREGS sregs;

  regs.h.ah = 0x49;
  segread (&sregs);
  sregs.es = seg;
  int86x (0x21, & regs, & regs, & sregs);

#endif /* 0 */
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}

#endif /* DOSX_RM_HANDLER_REAL */

#ifndef DOSX_PM_HANDLER_UNTOUCHED

static void
DOSX_PM_getvector (unsigned vecnum, unsigned * eip, unsigned * cs)
{
  union REGS regs;
  struct SREGS sregs;
  
  regs.x.ax = 0x2502;
  regs.h.cl = (vecnum & 0xff);
  segread (&sregs);
  int86x (0x21, &regs, &regs, &sregs);
  * eip = regs.e.ebx;
  * cs = ((unsigned) sregs.es);
  return;
}

static void
DOSX_installvector (unsigned vecnum, unsigned eip, unsigned cs)
{
  union REGS regs;
  struct SREGS sregs;
  
  regs.x.ax = 0x2506;
  regs.h.cl = (vecnum & 0xff);
  regs.e.edx = eip;
  segread (&sregs);
  sregs.ds = cs;
  int86x (0x21, &regs, &regs, &sregs);
  return;
}

static void
DOSX_restore_vector (unsigned vecnum, unsigned eip,
		     unsigned cs, unsigned rmode)
{
  union REGS regs;
  struct SREGS sregs;
  
  segread (&sregs);
  sregs.ds = cs;
  regs.e.edx = eip;
  regs.e.ebx = rmode;
  regs.x.ax = 0x2507;
  regs.h.cl = (vecnum & 0xff);
  int86x (0x21, &regs, &regs, &sregs);
  return;
}

#endif /* DOSX_PM_HANDLER_UNTOUCHED */

#if (!(defined(DOSX_RM_HANDLER_UNTOUCHED) && defined(DOSX_PM_HANDLER_UNTOUCHED)))

static void
DOSX_RM_getvector (unsigned vecnum, unsigned * vector)
{
  union REGS regs;

  regs.x.ax = 0x2503;
  regs.h.cl = (vecnum & 0xff);
  int86 (0x21, &regs, &regs);
  * vector = regs.e.ebx;
  return;
}

#endif /* !(DOSX_RM_HANDLER_UNTOUCHED && DOSX_PM_HANDLER_UNTOUCHED) */

#ifndef DOSX_RM_HANDLER_UNTOUCHED

static int
DOSX_RM_setvector (unsigned vecnum, unsigned rm_address)
{
  union REGS regs;
  
  regs.x.ax = 0x2505;
  regs.h.cl = (vecnum & 0xff);
  regs.e.ebx = rm_address;
  int86 (0x31, &regs, &regs);
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}

#if 0
static int
DOSX_convert_PM_to_RM_address (unsigned short sel, unsigned offset,
			       unsigned length, unsigned * rm_address)
{
  union REGS regs;
  struct SREGS sregs;

  segread (&sregs);
  sregs.es = sel;
  regs.e.ebx = offset;
  regs.e.ecx = length;
  regs.e.eax = 0x250F;
  int86x (0x21, &regs, &regs, &sregs);
  * rm_address = regs.e.ecx;
  return (((regs.e.flags & 1) == 0) ? DOS_SUCCESS : DOS_FAILURE);
}
#endif /* 0 */

#endif /* DOSX_RM_HANDLER_UNTOUCHED */

static unsigned 
  old_PM_vector_eip, 
  old_PM_vector_cs;

static union RM_address old_RM_vector;

static void
  * scheme_PM_vector = ((void *) NULL),
  * scheme_RM_vector = ((void *) NULL);

#if (!defined (DOSX_PM_HANDLER_UNTOUCHED)) || (!defined (DPMI_PM_HANDLER_UNTOUCHED))
static void *
make_PM_trampoline (void (* hook) (void))
{
  void * trampoline;
  INSN_DECLS ();

  trampoline = (malloc (TRAMP_SIZE (6)));
  if (trampoline != ((void *) NULL))
  {
    INIT_INSNS (trampoline);
    PUSH_INSN (old_PM_vector_cs);
    PUSH_INSN (old_PM_vector_eip);
    PUSH_INSN (& modifier_mask);
    PUSH_INSN (unshifted_scan_code_to_ascii);
    PUSH_INSN (shifted_scan_code_to_ascii);
    PUSH_INSN (getDS ());
    JMP_INSN (hook);
    HLT_INSNS (6);
  }
  return (trampoline);
}
#endif /* !DOSX_PM_HANDLER_UNTOUCHED || !DPMI_PM_HANDLER_UNTOUCHED */

#ifdef DPMI_RM_HANDLER_PROTECTED
static void *
make_RM_trampoline (void (* hook) (void))
{
  void * trampoline;
  INSN_DECLS ();

  trampoline = (malloc (TRAMP_SIZE (6)));
  if (trampoline != ((void *) NULL))
  {
    INIT_INSNS (trampoline);
    PUSH_INSN (old_RM_vector.x.seg);
    PUSH_INSN (old_RM_vector.x.off);
    PUSH_INSN (& modifier_mask);
    PUSH_INSN (unshifted_scan_code_to_ascii);
    PUSH_INSN (shifted_scan_code_to_ascii);
    PUSH_INSN (getDS ());
    JMP_INSN (hook);
    HLT_INSNS (6);
  }
  return (trampoline);
}
#endif /* DPMI_RM_HANDLER_PROTECTED */

#ifdef DPMI_RM_HANDLER_PROTECTED
  static union RM_address DPMI_RM_call_back;
  static void * DPMI_RM_regs = ((void *) NULL);
#endif /* DPMI_RM_HANDLER_PROTECTED */

#ifdef DPMI_RM_HANDLER_REAL
  static unsigned short DPMI_RM_selector = 0;
#endif /* DPMI_RM_HANDLER_REAL */

static char * DPMI_env_var = "MITSCHEME_DPMI_EXT_KBD";

static int
DPMI_restore_kbd_hook (void)
{
  if (!(install_kbd_hook_p (DPMI_env_var)))
    return (DOS_FAILURE);

#ifdef DPMI_RM_HANDLER_REAL
  if (DPMI_RM_selector != 0)
  {
    if (((DPMI_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
			     old_RM_vector.x.off,
			     old_RM_vector.x.seg))
	 != DOS_SUCCESS)
	|| ((DPMI_free_DOS_block (DPMI_RM_selector)) != DOS_SUCCESS))
    {
      errno = EACCES;
      return (DOS_FAILURE);
    }
    DPMI_RM_selector = 0;
    free (scheme_RM_vector);
    scheme_RM_vector = ((void *) NULL);
  }
#endif /* #ifdef DPMI_RM_HANDLER_REAL */

#ifdef DPMI_RM_HANDLER_PROTECTED
  if (scheme_RM_vector != ((void *) NULL))
  {
    if (((DPMI_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
			     old_RM_vector.x.off,
			     old_RM_vector.x.seg))
	 != DOS_SUCCESS)
	|| ((DPMI_free_RM_call_back (DPMI_RM_call_back.x.off,
				     DPMI_RM_call_back.x.seg))
	    != DOS_SUCCESS))
    {
      errno = EACCES;
      return (DOS_FAILURE);
    }
    free (DPMI_RM_regs);
    free (scheme_RM_vector);
    scheme_RM_vector = ((void *) NULL);
  }
#endif /* DPMI_RM_HANDLER_PROTECTED */

#ifndef DPMI_PM_HANDLER_UNTOUCHED
  if (scheme_PM_vector != ((void *) NULL))
  {
    if ((DPMI_PM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
			    old_PM_vector_eip,
			    old_PM_vector_cs))
	!= DOS_SUCCESS)
    {
      errno = EACCES;
      return (DOS_FAILURE);
    }
    free (scheme_PM_vector);
    scheme_PM_vector = ((void *) NULL);
  }
#endif /* DPMI_PM_HANDLER_UNTOUCHED */

  return (DOS_SUCCESS);
}

#if defined(DPMI_RM_HANDLER_REAL) || defined(DOSX_RM_HANDLER_REAL)

unsigned char RM_handler_pattern[] =
{
			/*  chain:					*/
0x9d,			/* 0	popf					*/
0xea,0,0,0,0,		/* 1	jmpf	next_in_chain			*/
			/*  kbd_isr:					*/
0x9c,			/* 6	pushf					*/
0x80,0xfc,0x4f,		/* 7	cmp	ah,4fh				*/
0x75,0xf4,		/* a	jne	chain				*/
0x3c,0x39,		/* c	cmp	al,39h				*/
0x77,0xf0,		/* e	ja	chain				*/
0x53,			/* 10	push	bx	; Preserve bx		*/
0x50,			/* 11	push	ax	; Preserve scan code	*/
0xb4,2,			/* 12	mov	ah,2h				*/
0xcd,0x16,		/* 14	int	16h	; Get modifier bits	*/
0x2e,0x22,6,0xf4,0,	/* 16	and	al,cs:modifier mask		*/
0x5b,			/* 1b	pop	bx	; Get scan code		*/
0x53,			/* 1c	push	bx				*/
0x81,0xe3,0x3f,0,	/* 1d	and	bx,3fh	; Drop fncn		*/
0x3c,8,			/* 21	cmp	al,8h	; Only meta bit set?	*/
0x74,0xb,		/* 23	je	do_unshifted			*/
0x3c,0,			/* 25	cmp	al,0	; No modifier bits set? */
0x74,7,			/* 27	je	do_unshifted			*/
			/*  do_shifted:					*/
0x2e,0x8a,0x9f,0x80,0,	/* 29	mov	bl,cs:shifted_table[bx]		*/
0xeb,5,			/* 2e	jmp	merge				*/
			/*  do_unshifted:				*/
0x2e,0x8a,0x9f,0xba,0,	/* 30	mov	bl,cs:unshifted_table[bx]	*/
			/*  merge:					*/
0x80,0xfb,0,		/* 35	cmp	bl,0	; No translation?	*/
0x74,0x37,		/* 38	je	abort_translation		*/
0x0f,0xba,0xe0,2,	/* 3a	bt	al,2h	; Control set?		*/
0x73,3,			/* 3e	jnc	after_ctrl			*/
0x80,0xe3,0x9f,		/* 40	and	bl,09fh ; controlify		*/
			/*  after_ctrl:					*/
0x0f,0xba,0xe0,3,	/* 43	bt	al,3h	; Alt set?		*/
0x73,3,			/* 47	jnc	after_meta			*/
0x80,0xcb,0x80,		/* 49	or	bl,080h ; metify		*/
			/*  after_meta:					*/
0x80,0xfb,0xf0,		/* 4c   cmp	bl,0f0h ; M-p ?			*/
0x74,0x20,		/* 4f	je	abort_translation		*/
0x58,			/* 51	pop	ax				*/
0x51,			/* 52	push	cx	; Preserve cx		*/
0x50,			/* 53	push	ax				*/
0x8a,0xe8,		/* 54	mov	ch,al	; Scan code		*/
0x80,0xfb,0,		/* 56	cmp	bl,0	; C-Space?		*/
0x75,2,			/* 59   jne	after_ctrl_space		*/
0xb5,3,			/* 5b	mov	ch,3	; Fudge scan code	*/
			/*  after_ctrl_space:				*/
0x8a,0xcb,		/* 5d	mov	cl,bl	; ASCII value		*/
0xb4,5,			/* 5f	mov	ah,05h	; fcn. number		*/
0xcd,0x16,		/* 61	int	16h	; Record keystroke	*/
0x58,			/* 63	pop	ax	; Restore registers	*/
0x59,			/* 64	pop	cx				*/
0x5b,			/* 65	pop	bx				*/
0x55,			/* 66	push	bp				*/
0x8b,0xec,		/* 67	mov	bp,sp				*/
0x80,0x66,8,0xfe,	/* 69	and	8[bp],0feh  ; clc iret's flags	*/
0x5d,			/* 6d	pop	bp				*/
0x9d,			/* 6e	popf					*/
0xf8,			/* 6f	clc					*/
0xcf,			/* 70	iret					*/
			/*  abort_translation:				*/
0x58,			/* 71	pop	ax				*/
0x5b,			/* 72	pop	bx				*/
0xeb,0x8b		/* 73	jmp	chain				*/
			/* 75	PAD					*/
};

#define PATTERN_SIZE		0x75
#define PADDED_PATTERN_SIZE	0x80
#define PATTERN_CHAIN_OFFSET	2
#define PATTERN_START_OFFSET	6
#define RM_ISR_TABLE_SIZE	0x3a
#define RM_ISR_TOTAL_SIZE					\
  (PADDED_PATTERN_SIZE + (2 * RM_ISR_TABLE_SIZE) + 1)
#define RM_ISR_MASK_OFFSET	(RM_ISR_TOTAL_SIZE - 1)

static void *
make_RM_handler (void)
{
  unsigned char * copy;
  unsigned short * wordptr;

  if (((sizeof (RM_handler_pattern)) != PATTERN_SIZE)
      || ((sizeof (shifted_scan_code_to_ascii)) != RM_ISR_TABLE_SIZE)
      || ((sizeof (unshifted_scan_code_to_ascii)) != RM_ISR_TABLE_SIZE)
      || (RM_ISR_MASK_OFFSET != 0xf4))
  {
    fprintf (stderr, "make_RM_handler: Inconsistent sizes!\n");
    fprintf (stderr, "	   PATTERN_SIZE = %d\n", PATTERN_SIZE);
    fprintf (stderr, "and (sizeof (RM_handler_pattern)) = %d\n",
	     (sizeof (RM_handler_pattern)));
    fprintf (stderr, "	   RM_ISR_TABLE_SIZE = %d\n",
	     RM_ISR_TABLE_SIZE);

    fprintf (stderr, "and (sizeof (shifted_scan_code_to_ascii)) = %d\n",
	     (sizeof (shifted_scan_code_to_ascii)));
    fprintf (stderr, "and (sizeof (unshifted_scan_code_to_ascii)) = %d\n",
	     (sizeof (unshifted_scan_code_to_ascii)));
    fprintf (stderr, "	   RM_ISR_MASK_OFFSET  = 0x%x <> 0xf4",
	     RM_ISR_MASK_OFFSET);
    errno = EFAULT;
    return ((void *) NULL);
  }

  copy = ((unsigned char *) (malloc (RM_ISR_TOTAL_SIZE)));
  if (copy == ((unsigned char *) NULL))
    return ((void *) NULL);

  memcpy (copy, RM_handler_pattern, (sizeof (RM_handler_pattern)));
  memcpy ((copy + PADDED_PATTERN_SIZE),
	  shifted_scan_code_to_ascii,
	  RM_ISR_TABLE_SIZE);
  memcpy ((copy + PADDED_PATTERN_SIZE + RM_ISR_TABLE_SIZE),
	  unshifted_scan_code_to_ascii,
	  RM_ISR_TABLE_SIZE);

  wordptr = ((unsigned short *) (copy + PATTERN_CHAIN_OFFSET));
  * wordptr++ = old_RM_vector.x.off;
  * wordptr = old_RM_vector.x.seg;
  * (copy + RM_ISR_MASK_OFFSET) = modifier_mask;

  return ((void *) copy);
}

#endif /* DPMI_RM_HANDLER_REAL || DOSX_RM_HANDLER_REAL */

static int
DPMI_install_kbd_hook (void)
{
  if (!(install_kbd_hook_p (DPMI_env_var)))
    return (DOS_FAILURE);

#ifndef DPMI_PM_HANDLER_UNTOUCHED

  DPMI_PM_getvector (DOS_INTVECT_SYSTEM_SERVICES,
		     & old_PM_vector_eip,
		     & old_PM_vector_cs);

  {
    extern void DPMI_PM_scheme_system_isr (void);
    void * PM_trampoline;

    PM_trampoline = (make_PM_trampoline (DPMI_PM_scheme_system_isr));
    if (PM_trampoline == ((void *) NULL))
    {
      errno = ENOMEM;
      return (DOS_FAILURE);
    }
    if ((DPMI_PM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
			    ((unsigned) PM_trampoline),
			    (getCS ())))
	!= DOS_SUCCESS)
    {
      errno = EACCES;
      free (PM_trampoline);
      return (DOS_FAILURE);
    }
    scheme_PM_vector = PM_trampoline;
  }

#endif /* DPMI_PM_HANDLER_UNTOUCHED */

#ifndef DPMI_RM_HANDLER_UNTOUCHED

  DPMI_RM_getvector (DOS_INTVECT_SYSTEM_SERVICES,
		     & old_RM_vector.x.off,
		     & old_RM_vector.x.seg);

#  ifdef DPMI_RM_HANDLER_PROTECTED

  {
    extern void DPMI_RM_scheme_system_isr (void);
    struct DPMI_RM_REGS * RM_regs;
    union RM_address RM_call_back;
    void * RM_trampoline;

    RM_regs = ((struct DPMI_RM_REGS *)
	       (malloc (sizeof (struct DPMI_RM_REGS))));
    if (RM_regs == ((struct DPMI_RM_REGS *) NULL))
    {
      DPMI_restore_kbd_hook ();
      errno = ENOMEM;
      return (DOS_FAILURE);
    }

    RM_regs->ss = 0;
    RM_regs->sp = 0;
    RM_regs->old_vector_ip = (old_RM_vector.x.off);
    RM_regs->old_vector_cs = (old_RM_vector.x.seg);

    RM_trampoline = (make_RM_trampoline (DPMI_RM_scheme_system_isr));
    if (RM_trampoline == ((void *) NULL))
    {
      free (RM_regs);
      DPMI_restore_kbd_hook ();
      errno = ENOMEM;
      return (DOS_FAILURE);
    }

    if (((DPMI_allocate_RM_call_back (& RM_call_back.x.off,
				      & RM_call_back.x.seg,
				      ((unsigned) RM_trampoline),
				      ((unsigned) (getCS ())),
				      ((unsigned) RM_regs),
				      ((unsigned) (getDS ()))))
	 != DOS_SUCCESS)
	|| ((DPMI_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
				RM_call_back.x.off,
				RM_call_back.x.seg))
	    != DOS_SUCCESS))
    {
      free (RM_trampoline);
      free (RM_regs);
      DPMI_restore_kbd_hook ();
      errno = EACCES;
      return (DOS_FAILURE);
    }
    scheme_RM_vector = RM_trampoline;
    DPMI_RM_regs = ((void *) RM_regs);
    DPMI_RM_call_back = RM_call_back;
  }

#  else  /* not DPMI_RM_HANDLER_PROTECTED = DPMI_RM_HANDLER_REAL */

  {
    void * RM_handler;
    unsigned short real_mode_segment;
    unsigned short prot_mode_selector;

    RM_handler = (make_RM_handler ());
    if (RM_handler == ((void *) NULL))
    {
      int saved_errno = errno;

      DPMI_restore_kbd_hook ();
      errno = saved_errno;
      return (DOS_FAILURE);
    }

    if ((DPMI_allocate_DOS_block (RM_ISR_TOTAL_SIZE,
				  & real_mode_segment,
				  & prot_mode_selector))
	!= DOS_SUCCESS)
    {
      free (RM_handler);
      DPMI_restore_kbd_hook ();
      errno = ENOMEM;
      return (DOS_FAILURE);
    }

    farcpy (0, prot_mode_selector,
	    ((unsigned) RM_handler), (getDS ()),
	    RM_ISR_TOTAL_SIZE);

    if ((DPMI_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
			    PATTERN_START_OFFSET,
			    real_mode_segment))
	!= DOS_SUCCESS)
    {
      DPMI_free_DOS_block (prot_mode_selector);
      free (RM_handler);
      DPMI_restore_kbd_hook ();
      errno = EACCES;
      return (DOS_FAILURE);
    }

    DPMI_RM_selector = prot_mode_selector;
    scheme_RM_vector = RM_handler;		/* Kludge! */
  }

#  endif /* not DPMI_RM_HANDLER_PROTECTED */
#endif /* DPMI_RM_HANDLER_UNTOUCHED */
  return (DOS_SUCCESS);
}

#ifdef DOSX_RM_HANDLER_REAL
  static unsigned short DOSX_RM_segment = 0;
#endif /* DOSX_RM_HANDLER_REAL */
#ifdef DOSX_USE_INT_INTERCEPT
  static unsigned char kludge;
#endif /* DOSX_USE_INT_INTERCEPT */

static char * DOSX_env_var = "MITSCHEME_DOSX_EXT_KBD";

static int
DOSX_install_kbd_hook (void)
{
  if (!(install_kbd_hook_p (DOSX_env_var)))
    return (DOS_FAILURE);
  
#ifdef DOSX_USE_INT_INTERCEPT
  {
    int_intercept (DOS_INTVECT_SYSTEM_SERVICES, 
		   bios_keyboard_handler, 
		   256);

    scheme_PM_vector = ((void *) & kludge);
  }
#else /* not DOSX_USE_INT_INTERCEPT */
#ifndef DOSX_PM_HANDLER_UNTOUCHED
  {
    extern void DOSX_scheme_system_isr (void);
    void * trampoline;

    DOSX_PM_getvector (DOS_INTVECT_SYSTEM_SERVICES,
		       & old_PM_vector_eip,
		       & old_PM_vector_cs);
    DOSX_RM_getvector (DOS_INTVECT_SYSTEM_SERVICES,
		       & old_RM_vector.fp);

    trampoline = (make_PM_trampoline (DOSX_scheme_system_isr));
    if (trampoline == ((void *) NULL))
      return (DOS_FAILURE);

    DOSX_installvector (DOS_INTVECT_SYSTEM_SERVICES,
			((unsigned) trampoline),
			((unsigned) (getCS ())));

    scheme_PM_vector = trampoline;
  }
#endif /* DOSX_PM_HANDLER_UNTOUCHED */

#ifdef DOSX_RM_HANDLER_REAL
  {
    void * RM_handler;
    union RM_address new_handler;

    DOSX_RM_getvector (DOS_INTVECT_SYSTEM_SERVICES,
		       ((unsigned *) & old_RM_vector));

    RM_handler = (make_RM_handler ());
    if (RM_handler == ((void *) NULL))
      return (DOS_FAILURE);

#if 0

    if ((DOSX_convert_PM_to_RM_address ((getDS ()), ((unsigned) RM_handler),
					RM_ISR_TOTAL_SIZE,
					((unsigned *) & new_handler)))
	!= DOS_SUCCESS)
    {
      int saved_errno = errno;

      fflush (stdout);
      free (RM_handler);
      errno = saved_errno;
      return (DOS_FAILURE);
    }
    
    if ((new_handler.x.off & 0xf) != 0)
    {
      fflush (stdout);
      free (RM_handler);
      errno = EFAULT;
      return (DOS_FAILURE);
    }

    normalize_RM_address (& new_handler);

    if ((DOSX_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
			    ((unsigned) new_handler)))
	!= DOS_SUCCESS)
    {
      fflush (stdout);
      free (RM_handler);
      errno = EFAULT;
      return (DOS_FAILURE);
    }

#else /* not 0 */
    
    if ((DOSX_allocate_DOS_block (RM_ISR_TOTAL_SIZE, &new_handler.x.seg))
	!= DOS_SUCCESS)
    {
      int saved_errno = errno;

      free (RM_handler);
      errno = saved_errno;
      return (DOS_FAILURE);
    }
    new_handler.x.off = 0;

    /* This assumes that the bottom 1 Mb of memory is mapped to the DOS
       memory, so it can be accessed directly.
     */

    memcpy (((void *) ((unsigned long) new_handler.x.seg << 4)),
	    RM_handler,
	    RM_ISR_TOTAL_SIZE);

    if ((DOSX_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES, new_handler.fp))
	!= DOS_SUCCESS)
    {
      DOSX_free_DOS_block (new_handler.x.seg);
      fflush (stdout);
      free (RM_handler);
      errno = EFAULT;
      return (DOS_FAILURE);
    }
    DOSX_RM_segment = new_handler.x.seg;

#endif /* 0 */

    scheme_RM_vector = RM_handler;
  }

#endif /* DOSX_PM_HANDLER_UNTOUCHED */
#endif /* DOSX_USE_INT_INTERCEPT */
  return (DOS_SUCCESS);
}

static int
DOSX_restore_kbd_hook (void)
{
  if (!(install_kbd_hook_p (DOSX_env_var)))
    return (DOS_FAILURE);

#ifdef DOSX_USE_INT_INTERCEPT

  (void) int_restore (DOS_INTVECT_SYSTEM_SERVICES);
  scheme_PM_vector = ((void *) NULL);
  
#else /* not DOSX_USE_INT_INTERCEPT */
#ifndef DOSX_PM_HANDLER_UNTOUCHED

  DOSX_restore_vector (DOS_INTVECT_SYSTEM_SERVICES,
		       old_PM_vector_eip, 
		       old_PM_vector_cs,
		       old_RM_vector.fp);

  free (scheme_PM_vector);
  scheme_PM_vector = ((void *) NULL);

#endif /* DOSX_PM_HANDLER_UNTOUCHED */

#ifdef DOSX_RM_HANDLER_REAL

  if ((DOSX_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
			  ((unsigned) old_RM_vector)))
      != DOS_SUCCESS)
    return (DOS_FAILURE);

#if 1

  if ((DOSX_free_DOS_block (DOSX_RM_segment)) != DOS_SUCCESS)
    return (DOS_FAILURE);
  DOSX_RM_segment = 0;

#endif /* 1 */

  free (scheme_RM_vector);
  scheme_RM_vector = ((void *) NULL);

#endif /* DOSX_RM_HANDLER_REAL */
#endif /* DOSX_USE_INT_INTERCEPT */
  return (DOS_SUCCESS);
}

int
dos_install_kbd_hook (void)
{
  if (scheme_PM_vector != ((void *) NULL))
  {
    errno = ELOOP;
    return (DOS_FAILURE);
  }
  if (under_DPMI_p ())
    return (DPMI_install_kbd_hook ());
  else
    return (DOSX_install_kbd_hook ());
}

int
dos_restore_kbd_hook (void)
{
  if ((scheme_PM_vector == ((void *) NULL))
      && (scheme_RM_vector == ((void *) NULL)))
    return (DOS_SUCCESS);
  else if (!under_DPMI_p ())
  {
    if ((DOSX_restore_kbd_hook ()) != DOS_SUCCESS)
      return (DOS_FAILURE);
  }
  else if ((DPMI_restore_kbd_hook ()) != DOS_SUCCESS)
    return (DOS_FAILURE);

  if (scheme_PM_vector != ((void *) NULL))
  {
    free (scheme_PM_vector);
    scheme_PM_vector = ((void *) NULL);
  }
  if (scheme_RM_vector != ((void *) NULL))
  {
    free (scheme_RM_vector);
    scheme_RM_vector = ((void *) NULL);
  }
  return (DOS_SUCCESS);
}

unsigned char
dos_set_kbd_modifier_mask (unsigned char new_mask)
{
  unsigned char old_mask = modifier_mask;

  modifier_mask = new_mask;

#ifdef DPMI_RM_HANDLER_REAL

  if (DPMI_RM_selector != 0)
    farcpy (RM_ISR_MASK_OFFSET, DPMI_RM_selector, 
	    ((unsigned) (& modifier_mask)), (getDS ()),
	    1);

#endif /* DPMI_RM_HANDLER_REAL */

#ifdef DOSX_RM_HANDLER_REAL

  if (DOSX_RM_segment != 0)
    (* ((unsigned char *)
	((((unsigned long) DOSX_RM_segment) << 4) + RM_ISR_MASK_OFFSET)))
      = modifier_mask;

#endif /* DOSX_RM_HANDLER_REAL */

  return (old_mask);
}

extern int EXFUN (dos_set_kbd_translation,
		  (unsigned, unsigned, unsigned char));

#ifndef PADDED_PATTERN_SIZE
#  define PADDED_PATTERN_SIZE 0
#endif

#ifndef RM_ISR_TABLE_SIZE
#  define RM_ISR_TABLE_SIZE 0
#endif

int
dos_set_kbd_translation (unsigned shift_p,
			 unsigned scan_code,
			 unsigned char new)
{
  unsigned char old;
  unsigned char * table;
  unsigned offset;

  if (scan_code >= (sizeof (shifted_scan_code_to_ascii)))
    return (-1);

  if (shift_p != 0)
  {
    table = &shifted_scan_code_to_ascii[0];
    offset = PADDED_PATTERN_SIZE;
  }
  else
  {
    table = &unshifted_scan_code_to_ascii[0];
    offset = (PADDED_PATTERN_SIZE + RM_ISR_TABLE_SIZE);
  }
  old = table[scan_code];
  table[scan_code] = new;
			       

#ifdef DPMI_RM_HANDLER_REAL

  if (DPMI_RM_selector != 0)
    farcpy ((offset + scan_code),
	    DPMI_RM_selector,
	    ((unsigned) (& table[scan_code])),
	    (getDS ()),
	    1);

#endif /* DPMI_RM_HANDLER_REAL */

#ifdef DOSX_RM_HANDLER_REAL

  if (DOSX_RM_segment != 0)
    (* ((unsigned char *)
	((((unsigned long) DOSX_RM_segment) << 4) + offset)))
      = new;

#endif /* DOSX_RM_HANDLER_REAL */

  return (old);
}
