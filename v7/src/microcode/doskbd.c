/* -*-C-*-
$Id: doskbd.c,v 1.9 1992/09/06 16:24:03 jinx Exp $

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

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <bios.h>
#include <dos.h>
#include <int.h>
#include "msdos.h"

/* These flags determine how the code will behave. */

#define DOSX_USE_INT_INTERCEPT
#define DOSX_RM_HANDLER_UNTOUCHED
#define DOSX_PM_HANDLER_UNTOUCHED
/* #define DOSX_RM_HANDLER_REAL */
#define DPMI_RM_HANDLER_REAL
#define DPMI_PM_HANDLER_UNTOUCHED

#ifdef getDS
#undef getDS
#endif

#ifdef getCS
#undef getCS
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

/* These depend on the C compiler (Zortech) allocating them contiguosly. */

extern unsigned char scan_code_tables_start[];
extern unsigned char scan_code_tables_end[];

/* Tables mapping scan codes to ASCII characters.
   Entries with NULL (\0) should not be mapped by the
   Scheme keyboard ISR.  Let the default handler map them.
 */

unsigned char
scan_code_tables_start[] = "foo";

static unsigned char modifier_mask = 0x4f;

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
caps_scan_code_to_ascii[] =
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
	'[',		/* 26 */
	']',		/* 27 */
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
	';',		/* 39 */
	'\'',		/* 40 */
	'`',		/* 41 */
	'\0',		/* 42 */
	'\\',		/* 43 */
	'Z',		/* 44 */
	'X',		/* 45 */
	'C',		/* 46 */
	'V',		/* 47 */
	'B',		/* 48 */
	'N',		/* 49 */
	'M',		/* 50 */
	',',		/* 51 */
	'.',		/* 52 */
	'/',		/* 53 */
	'\0',		/* 54 */
	'\0',		/* 55 */
	'\0',		/* 56 */
	' '		/* 57 */
  };

unsigned char
scan_code_tables_end[] = "bar";

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

  if ((envvar != NULL) && ((strcmp_ci (envvar, "true")) == 0))
    return (dos_true);
  else
    return (dos_false);
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
bios_keyboard_handler (struct INT_DATA * pd)
{
  unsigned char scan_code, chord, ascii, * table;
  union REGS regs;

  if (pd->regs.h.ah != DOS_HOOK_TRANSLATE_KEYSTROKE)
    return (INTERRUPT_CHAIN_NEXT);

  scan_code = (pd->regs.h.al);

  /* All the tables are assumed to be the same length. */

  if (scan_code >= (sizeof (shifted_scan_code_to_ascii)))
    return (INTERRUPT_CHAIN_NEXT);

  chord = ((bioskey (_KEYBRD_SHIFTSTATUS)) & modifier_mask);

  if ((chord & (PC_KBD_CTRL_MASK | PC_KBD_SHIFT_MASK)) != 0)
    table = &shifted_scan_code_to_ascii[0];
  else if ((chord & PC_KBD_CAPSL_MASK) != 0)
    table = &caps_scan_code_to_ascii[0];
  else
    table = &unshifted_scan_code_to_ascii[0];

  ascii = table[scan_code];

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

  trampoline = (malloc (TRAMP_SIZE (7)));
  if (trampoline != ((void *) NULL))
  {
    INIT_INSNS (trampoline);
    PUSH_INSN (old_PM_vector_cs);
    PUSH_INSN (old_PM_vector_eip);
    PUSH_INSH (caps_scan_code_to_ascii);
    PUSH_INSN (shifted_scan_code_to_ascii);
    PUSH_INSN (unshifted_scan_code_to_ascii);
    PUSH_INSN (& modifier_mask);
    PUSH_INSN (getDS ());
    JMP_INSN (hook);
    HLT_INSNS (7);
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

  trampoline = (malloc (TRAMP_SIZE (7)));
  if (trampoline != ((void *) NULL))
  {
    INIT_INSNS (trampoline);
    PUSH_INSN (old_RM_vector.x.seg);
    PUSH_INSN (old_RM_vector.x.off);
    PUSH_INSH (caps_scan_code_to_ascii);
    PUSH_INSN (shifted_scan_code_to_ascii);
    PUSH_INSN (unshifted_scan_code_to_ascii);
    PUSH_INSN (& modifier_mask);
    PUSH_INSN (getDS ());
    JMP_INSN (hook);
    HLT_INSNS (7);
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

static unsigned tables_offset = 0;

#define PATTERN_MODIFIER_OFFSET		0
#define PATTERN_UNSHIFTED_PTR_OFFSET	2
#define PATTERN_SHIFTED_PTR_OFFSET	4
#define PATTERN_CAPS_PTR_OFFSET		6
#define PATTERN_CHAIN_OFFSET		10
#define PATTERN_START_OFFSET		14

#define RM_ISR_MASK_OFFSET		PATTERN_MODIFIER_OFFSET
									
static void *
make_RM_handler (unsigned * size, unsigned * offset, unsigned * delta)
{
  extern void RM_keyboard_pattern_start (void);
  extern void RM_keyboard_pattern_end (void);
  unsigned long pattern_start, start_offset;
  unsigned long pattern_size, total_size;
  unsigned short * wordptr;
  unsigned char * copy;
  union REGS regs;

  regs.x.ax = 0x2509;
  int86 (0x21, &regs, &regs);
  start_offset = ((unsigned long) RM_keyboard_pattern_start);
  pattern_start = ((((unsigned long) regs.x.bx) << 4) + start_offset);

  pattern_size = (((unsigned long) RM_keyboard_pattern_end) - start_offset);
  total_size = (pattern_size
		+ (sizeof (unshifted_scan_code_to_ascii))
		+ (sizeof (shifted_scan_code_to_ascii))
		+ (sizeof (caps_scan_code_to_ascii)));

  copy = ((unsigned char *) (malloc (total_size)));
  if (copy == ((unsigned char *) NULL))
    return ((void *) NULL);

  farcpy (((unsigned) copy), (getDS ()),
	  ((unsigned) pattern_start), (regs.e.edx >> 16),
	  pattern_size);

  if (copy[PATTERN_START_OFFSET] != ((unsigned char) 0x9c))
  {
    fprintf (stderr, "make_RM_handler: Bad pattern!\n");
    fprintf (stderr, "\tpattern_start = 0x%lx, pattern_size = %d.\n",
	     pattern_start, pattern_size);
    fprintf (stderr, "\tselector = 0x%x; segment = 0x%x; start_offset = 0x%x.\n",
	     (regs.e.edx >> 16), regs.x.bx, start_offset);
    free (copy);
    return ((void *) NULL);
  }

  memcpy ((copy + pattern_size),
	  unshifted_scan_code_to_ascii,
	  (sizeof (unshifted_scan_code_to_ascii)));

  memcpy ((copy + (pattern_size + (sizeof (unshifted_scan_code_to_ascii)))),
	  shifted_scan_code_to_ascii,
	  (sizeof (shifted_scan_code_to_ascii)));

  memcpy ((copy + (pattern_size + ((sizeof (unshifted_scan_code_to_ascii))
				   + (sizeof (shifted_scan_code_to_ascii))))),
	  caps_scan_code_to_ascii,
	  (sizeof (caps_scan_code_to_ascii)));

  copy[PATTERN_MODIFIER_OFFSET] = modifier_mask;
  wordptr = ((unsigned short *) (copy + PATTERN_UNSHIFTED_PTR_OFFSET));
  * wordptr = (pattern_size + start_offset);
  wordptr = ((unsigned short *) (copy + PATTERN_SHIFTED_PTR_OFFSET));
  * wordptr = ((pattern_size + (sizeof (unshifted_scan_code_to_ascii)))
	       + start_offset);
  wordptr = ((unsigned short *) (copy + PATTERN_CAPS_PTR_OFFSET));
  * wordptr = ((pattern_size + ((sizeof (unshifted_scan_code_to_ascii))
				+ (sizeof (shifted_scan_code_to_ascii))))
	       + start_offset);
  wordptr = ((unsigned short *) (copy + PATTERN_CHAIN_OFFSET));
  * wordptr++ = old_RM_vector.x.off;
  * wordptr = old_RM_vector.x.seg;

  * delta = start_offset;
  * size = total_size;
  * offset = PATTERN_START_OFFSET;
  tables_offset = pattern_size;
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
    unsigned handler_size, entry_offset, relocation;
    unsigned short prot_mode_selector;
    unsigned short real_mode_segment;
    unsigned long base_addr;

    RM_handler = (make_RM_handler (& handler_size, & entry_offset, & relocation));
    if (RM_handler == ((void *) NULL))
    {
      int saved_errno = errno;

      DPMI_restore_kbd_hook ();
      errno = saved_errno;
      return (DOS_FAILURE);
    }

    if ((DPMI_allocate_DOS_block (handler_size,
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
	    handler_size);

    base_addr = ((((unsigned long) real_mode_segment) << 4) - relocation);

    if ((DPMI_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES,
			    (entry_offset + relocation),
			    (base_addr >> 4)))
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

static void
DPMI_set_modifier_mask (unsigned char new_mask)
{
#ifdef DPMI_RM_HANDLER_REAL

  if (DPMI_RM_selector != 0)
    farcpy (RM_ISR_MASK_OFFSET, DPMI_RM_selector, 
	    ((unsigned) (& new_mask)), (getDS ()),
	    1);

#endif /* DPMI_RM_HANDLER_REAL */
  return;
}

static void
DPMI_set_kbd_translation (unsigned table,
			  unsigned scan_code,
			  unsigned char new)
{
#ifdef DPMI_RM_HANDLER_REAL

  int offset = tables_offset;

  switch (table)
  {
    case 2:
      offset += (sizeof (shifted_scan_code_to_ascii));

    case 1:
      offset += (sizeof (unshifted_scan_code_to_ascii));

    default:
      break;
  }

  if (DPMI_RM_selector != 0)
    farcpy ((scan_code + tables_offset),
	    DPMI_RM_selector,
	    ((unsigned) (& new)),
	    (getDS ()),
	    1);

#endif /* DPMI_RM_HANDLER_REAL */

  return;
}

dos_boolean
under_DPMI_p (void)
{
  union REGS regs;
  
  regs.e.eax = 0x1686;
  int86 (0x2f, &regs, &regs);
  return (regs.x.ax == 0);
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
    unsigned handler_size, entry_offset, relocation;
    unsigned long base_addr;

    DOSX_RM_getvector (DOS_INTVECT_SYSTEM_SERVICES,
		       ((unsigned *) & old_RM_vector));

    RM_handler = (make_RM_handler (& handler_size, & entry_offset, & relocation));
    if (RM_handler == ((void *) NULL))
      return (DOS_FAILURE);

#if 0

    if ((DOSX_convert_PM_to_RM_address ((getDS ()), ((unsigned) RM_handler),
					handler_size,
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

    base_addr = (((new_handler.x.seg << 4) + (new_handler.x.off))
		 - relocation);
    new_handler.x.seg = (base_addr >> 4);
    new_handler.x.off = (base_addr & 0xf);
    new_handler.x.off += (relocation + entry_offset);

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
    
    if ((DOSX_allocate_DOS_block (handler_size, &new_handler.x.seg))
	!= DOS_SUCCESS)
    {
      int saved_errno = errno;

      free (RM_handler);
      errno = saved_errno;
      return (DOS_FAILURE);
    }

    /* This assumes that the bottom 1 Mb of memory is mapped to the DOS
       memory, so it can be accessed directly.
     */

    memcpy (((void *) ((unsigned long) new_handler.x.seg << 4)),
	    RM_handler,
	    handler_size);

    DOSX_RM_segment = new_handler.x.seg;

    base_addr = ((new_handler.x.seg << 4) - relocation)
    new_handler.x.seg = (base_addr >> 4);
    new_handler.x.off = (base_addr & 0xf);
    new_handler.x.off += (relocation + entry_offset);

    if ((DOSX_RM_setvector (DOS_INTVECT_SYSTEM_SERVICES, new_handler.fp))
	!= DOS_SUCCESS)
    {
      DOSX_free_DOS_block (DOSX_RM_segment);
      DOSX_RM_segment = 0;
      fflush (stdout);
      free (RM_handler);
      errno = EFAULT;
      return (DOS_FAILURE);
    }

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

static void
DOSX_set_modifier_mask (unsigned char new_mask)
{
#ifdef DOSX_RM_HANDLER_REAL

  if (DOSX_RM_segment != 0)
    (* ((unsigned char *)
	((((unsigned long) DOSX_RM_segment) << 4) + RM_ISR_MASK_OFFSET)))
      = new_mask;

#endif /* DOSX_RM_HANDLER_REAL */
  return;
}

static void
DOSX_set_kbd_translation (unsigned table,
			  unsigned scan_code,
			  unsigned char new)
{
#ifdef DOSX_RM_HANDLER_REAL

  int offset = tables_offset;

  switch (table)
  {
    case 2:
      offset += (sizeof (shifted_scan_code_to_ascii));

    case 1:
      offset += (sizeof (unshifted_scan_code_to_ascii));

    default:
      break;
  }

  if (DOSX_RM_segment != 0)
    (* ((unsigned char *)
	((((unsigned long) DOSX_RM_segment) << 4) + (scan_code + offset))))
      = new;

#endif /* DOSX_RM_HANDLER_REAL */
  return;
}

static dos_boolean
under_DOSX_p (void)
{
  return (dos_true);
}

/* Zortech's int_intercept does not work consistently with X32.
   Here is alternative lower-level code.
 */

extern dos_boolean EXFUN (under_X32_p, (void));

static int
X32_install_kbd_hook (void)
{
  extern int EXFUN (X32_int_intercept, (unsigned, void (*) (void), PTR));
  extern void EXFUN (X32_keyboard_interrupt, (void));
  extern PTR X32_kbd_interrupt_pointers[];
  extern int X32_kbd_interrupt_previous;

  X32_kbd_interrupt_pointers[0] = ((PTR) &modifier_mask);
  X32_kbd_interrupt_pointers[1] = ((PTR) &unshifted_scan_code_to_ascii[0]);
  X32_kbd_interrupt_pointers[2] = ((PTR) &shifted_scan_code_to_ascii[0]);
  X32_kbd_interrupt_pointers[3] = ((PTR) &caps_scan_code_to_ascii[0]);

  if ((X32_int_intercept (DOS_INTVECT_SYSTEM_SERVICES,
			  X32_keyboard_interrupt,
			  ((PTR) &X32_kbd_interrupt_previous)))
      != 0)
    return (DOS_FAILURE);
  return (DOS_SUCCESS);
}

static int
X32_restore_kbd_hook (void)
{
  extern int EXFUN (X32_interrupt_restore, (unsigned));

  if ((X32_interrupt_restore (DOS_INTVECT_SYSTEM_SERVICES)) != 0)
    return (DOS_FAILURE);
  return (DOS_SUCCESS);
}

static void
X32_set_modifier_mask (unsigned char new_mask)
{
  return;
}

static void
X32_set_kbd_translation (unsigned table,
			 unsigned scan_code,
			 unsigned char new)
{
  return;
}

struct keyboard_method_s
{
  dos_boolean (* present) (void);
  int (* install) (void);
  int (* restore) (void);
  void (* set_modifier_mask) (unsigned char);
  void (* set_kbd_translation) (unsigned, unsigned, unsigned char);
};

static struct keyboard_method_s keyboard_methods[] =
{
  {
    under_DPMI_p,
    DPMI_install_kbd_hook,
    DPMI_restore_kbd_hook,
    DPMI_set_modifier_mask,
    DPMI_set_kbd_translation
  },
  {
    under_X32_p,
    X32_install_kbd_hook,
    X32_restore_kbd_hook,
    X32_set_modifier_mask,
    X32_set_kbd_translation
  },
  {
    under_DOSX_p,
    DOSX_install_kbd_hook,
    DOSX_restore_kbd_hook,
    DOSX_set_modifier_mask,
    DOSX_set_kbd_translation    
  }
};

struct keyboard_method_s *
  installed_keyboard_method = ((struct keyboard_method_s *) NULL);

#define N_KEYBOARD_METHODS						\
  ((sizeof (keyboard_methods)) / (sizeof (struct keyboard_method_s)))

int
dos_install_kbd_hook (void)
{
  int i, result;

  for (i = 0; i < N_KEYBOARD_METHODS; i++)
  {
    if ((* (keyboard_methods[i].present)) ())
    {
      result = ((* (keyboard_methods[i].install)) ());
      if (result == DOS_SUCCESS)
	installed_keyboard_method = &keyboard_methods[i];
      return (result);
    }
  }
  return (DOS_FAILURE);
}

int
dos_restore_kbd_hook (void)
{
  int result;

  if (installed_keyboard_method == ((struct keyboard_method_s *) NULL))
    return (DOS_SUCCESS);
  
  result = (* (installed_keyboard_method->restore)) ();
  if (result == DOS_SUCCESS)
    installed_keyboard_method = ((struct keyboard_method_s *) NULL);
  return (result);
}

unsigned char
dos_set_kbd_modifier_mask (unsigned char new_mask)
{
  unsigned char old_mask = modifier_mask;

  modifier_mask = new_mask;

  if (installed_keyboard_method != ((struct keyboard_method_s *) NULL))
    (* (installed_keyboard_method->set_modifier_mask)) (modifier_mask);

  return (old_mask);
}

extern int EXFUN (dos_set_kbd_translation,
		  (unsigned, unsigned, unsigned char));

int
dos_set_kbd_translation (unsigned which_table,
			 unsigned scan_code,
			 unsigned char new)
{
  unsigned char old;
  unsigned char * table;

  if (scan_code >= (sizeof (shifted_scan_code_to_ascii)))
    return (-1);

  switch (which_table)
  {
    case 0:
    default:
      table = &unshifted_scan_code_to_ascii[0];
      break;

    case 1:
      table = &shifted_scan_code_to_ascii[0];
      break;

    case 2:
      table = &caps_scan_code_to_ascii[0];
      break;
  }

  old = table[scan_code];
  table[scan_code] = new;

  if (installed_keyboard_method != ((struct keyboard_method_s *) NULL))
    (* (installed_keyboard_method->set_kbd_translation))
      (which_table, scan_code, new);

  return ((int) old);
}
