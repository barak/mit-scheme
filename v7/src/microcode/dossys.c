/* -*-C-*-

$Id: dossys.c,v 1.3 1992/09/17 13:31:53 jinx Exp $

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

#include <dos.h>
#include <stdio.h>
#include "dossys.h"

int 
dos_keyboard_input_available_p (void)
{
  union REGS inregs, outregs;
  
  inregs.h.ah = 0x0B;
  intdos (&inregs, &outregs);
  return (outregs.h.al != 0);
}

unsigned char 
dos_get_keyboard_character (void)
{
  union REGS inregs, outregs;
  
  inregs.h.ah = 0x07;
  intdos (&inregs, &outregs);
  return ((unsigned char) (outregs.h.al));
}

int 
dos_poll_keyboard_character (unsigned char * result)
{
  union REGS inregs, outregs;
  
  inregs.h.ah = 0x06;
  inregs.h.dl = 0xFF;
  intdos (&inregs, &outregs);
  *result = ((unsigned char) (outregs.h.al));
  return ((outregs.x.flags & 0x40) == 0);
}

void 
dos_console_write_character (unsigned char character)
{
  union REGS inregs, outregs;
  
  inregs.h.ah = 0x06;
  inregs.h.dl = character;
  intdos (&inregs, &outregs);
  return;
}

int 
dos_console_write (void * vbuffer, size_t nsize)
{
  union REGS inregs, outregs;
  unsigned char * buffer = vbuffer;
  int i;  
  
  for (inregs.h.ah = 0x06, i=0; i < nsize; i++)
  {
    inregs.h.dl = buffer[i];
    intdos (&inregs, &outregs);
  }
  return (nsize);
}

/* DOS I/O functions using handles */

handle_t 
dos_open_file_with_handle (unsigned char * name, int mode)
{
  union REGS inregs, outregs;
  struct SREGS segregs;
  
  inregs.e.edx = ((unsigned long) name);
  segread (&segregs);
  inregs.h.ah = 0x3D;
  inregs.h.al = mode;
  intdosx (&inregs, &outregs, &segregs);
  return ((outregs.x.cflag) ? DOS_FAILURE : ((unsigned int) outregs.x.ax));
}

int 
dos_close_file_with_handle (handle_t handle)
{
  union REGS inregs, outregs;
  
  inregs.x.bx = handle;
  inregs.h.al = 0x3E;
  intdos (&inregs, &outregs);
  return ((outregs.x.cflag) ? DOS_FAILURE : DOS_SUCCESS);
}

int 
dos_read_file_with_handle (handle_t handle, void * buffer, size_t nbytes)
{
  union REGS inregs, outregs;
  struct SREGS segregs;
  
  inregs.x.bx = handle;  
  inregs.e.edx = ((unsigned long) buffer);
  inregs.e.ecx = nbytes;
  segread (&segregs);
  inregs.h.ah = 0x3F;
  intdosx (&inregs, &outregs, &segregs);
  return ((outregs.x.cflag) ? DOS_FAILURE : outregs.e.eax);
}

int 
dos_write_file_with_handle (handle_t handle, void * buffer, size_t nbytes)
{
  union REGS inregs, outregs;
  struct SREGS segregs;
  
  inregs.x.bx = handle;
  inregs.e.edx = (unsigned long) buffer;
  inregs.e.ecx = nbytes;
  segread (&segregs);
  inregs.h.ah = 0x40;
  intdosx (&inregs, &outregs, &segregs);
  return ((outregs.x.cflag) ? DOS_FAILURE : outregs.e.eax);
}
  
int 
dos_get_device_status_with_handle (handle_t handle)
{ 
  union REGS inregs, outregs;
  
  inregs.x.bx = handle;
  inregs.x.ax = 0x4400;
  intdos (&inregs, &outregs);
  return ((outregs.x.cflag) ? DOS_FAILURE : ((unsigned int) outregs.x.dx));
}
  
int 
dos_set_device_status_with_handle (handle_t handle, int mode)
{ 
  int original_mode;
  union REGS inregs, outregs;
  
  original_mode = dos_get_device_status_with_handle(handle);
  if (original_mode == DOS_FAILURE)
    return (DOS_FAILURE);
  inregs.x.dx = mode;
  inregs.x.bx = handle;
  inregs.x.ax = 0x4401;
  intdos (&inregs, &outregs);
  return ((outregs.x.cflag) ? DOS_FAILURE : original_mode);
}  

void 
dos_get_version (version_t *version_number)
{
  union REGS inregs, outregs;

  /* Use old style version number because we may be running below DOS 5.0 */
  inregs.h.al = 0x01;
  inregs.h.ah = 0x30;
  intdos (&inregs, &outregs);
  version_number -> major = outregs.h.al;
  version_number -> minor = outregs.h.ah;
  if ((version_number -> major) >= 5)
  { /* Get the real version. */
    inregs.x.ax = 0x3306;
    intdos (&inregs, &outregs);
    version_number -> major = outregs.h.bl;
    version_number -> minor = outregs.h.bh;
  }
  return;
}

void 
dos_reset_drive (void)
{
  union REGS inregs, outregs;
  
  inregs.h.al = 0x0d;
  intdos (&inregs, &outregs);
  return;
}

int 
dos_set_verify_flag (int verify_p)
{
  union REGS inregs, outregs;
  int old_flag;
  
  inregs.h.ah = 0x54;
  intdos(&inregs, &outregs);
  old_flag = outregs.h.al;
  inregs.h.al = (verify_p) ? 1 : 0;
  inregs.h.ah = 0x2E;
  intdos(&inregs, &outregs);
  return (old_flag);
}

int 
dos_set_ctrl_c_check_flag (int check_p)
{
  union REGS inregs, outregs;
  int old_flag;
  
  inregs.x.ax = 0x3300;
  intdos(&inregs, &outregs);
  old_flag = outregs.h.dl;
  inregs.h.dl = (check_p) ? 1 : 0;
  inregs.x.ax = 0x3301;
  intdos(&inregs, &outregs);
  return (old_flag);
}

int 
dos_rename_file (const char * old, const char * new)
{
  union REGS inregs, outregs;
  struct SREGS segregs;
  
  inregs.e.edx = (unsigned long) old;
  inregs.e.edi = (unsigned long) new;
  segread(&segregs);
  segregs.es = segregs.ds;
  inregs.h.ah = 0x56;
  intdosx(&inregs, &outregs, &segregs);
  if (outregs.x.cflag)
    return (DOS_FAILURE);
  else
    return (DOS_SUCCESS);
}

int 
dos_get_machine_name (char * name)
{
  union REGS inregs, outregs;
  struct SREGS segregs;
  
  inregs.e.edx = ((unsigned long) name);
  segregs.ds = getDS();
  inregs.x.ax = 0x5E00;
  intdosx(&inregs, &outregs, &segregs);
  if ((outregs.x.cflag) || (outregs.h.ch == 0))
    return (DOS_FAILURE);
  else
    return (outregs.h.cl);
}

int 
dos_drive_letter_to_number (char letter)
{
  if (letter == '\0')
    return 0;
  else if ((letter >= 'a')&&(letter <= 'z'))
    return ((letter - 'a') + 1);
  else if ((letter >= 'A')&&(letter <= 'Z'))
    return ((letter - 'A') + 1);
  else
    return (-1);
}

char 
dos_drive_number_to_letter (int number)
{
  if ((number >= 1)&&(number <= 26))
    return ('A' + (number - 1));
  else
    return ('\0');
}

int 
dos_set_default_drive (int drive_number)
{
  union REGS inregs, outregs;
  
  if (drive_number > 0)
  {	
    inregs.h.dl = drive_number - 1;
    inregs.h.ah = 0x0E;
    intdos(&inregs, &outregs);
  }
  return (DOS_SUCCESS);
}
    
int
dos_get_default_drive (int drive_number)
{
  union REGS inregs, outregs;
  
  inregs.h.ah = 0x19;
  intdos (&inregs, &outregs);
  return (outregs.h.al + 1);
}

dos_boolean 
dos_pathname_as_filename (char * name, char * buffer)
{ /* Returns whether directory encountered is top level */
  unsigned int end_index = strlen(name) - 1;

  /* The runtime system comes down with a name that has a back slash
     at the end.  This will choke DOS.
   */
  strcpy (buffer, name);
  if ((end_index >= 0) && (buffer[end_index] == '\\'))
  { /* Name is indeed a directory */
    if (end_index == 0) /* if only one char, name is top */
      return (dos_true);
    else
    {
      if (buffer[end_index-1] == ':') /* Preceded by drive letter, top */
	return (dos_true);
      else
      {
	buffer[end_index] = '\0';
	return (dos_false);
      }
    }
  }
  else
    return (dos_false);
}

int 
dos_split_filename (char * name, char * device, char * filename)
{ 
  unsigned start;
  int drive_number;
  
  if ((strlen(name) >= 2) && (name[1] == ':'))
  {
    device[0] = name[0], device[1] = name[1], device[2] = '\0';
    drive_number = dos_drive_letter_to_number(name[0]);
    start = 2;
  }
  else
  {
    device[0] = '\0';
    drive_number = 0;
    start = 0;
  }
  dos_pathname_as_filename (&name[start], filename);
  return (drive_number);
}

/* The following code should work at least under X32, Zortech's DOSX,
   and Phar Lap 386/DOSX. 
*/

extern int DOS_canonicalize_filename (char *, char *);
extern unsigned long RealModeBufferParagraph;
extern char *pRealModeBuffer;

#pragma ZTC align 1

struct rm_interrupt
{
  unsigned short intno;
  unsigned short ds;
  unsigned short es;
  unsigned short fs;
  unsigned short gs;
  unsigned long eax;
  unsigned long edx;
};

#pragma ZTC align

#define RETURN_BUFFER_SIZE 128

int
DOS_canonicalize_filename (char * aliased, char * direct)
{
  struct rm_interrupt intrpt;
  struct SREGS sregs;
  union REGS regs;

  if (pRealModeBuffer == NULL)
    return (-1);
  strcpy ((pRealModeBuffer + RETURN_BUFFER_SIZE), aliased);
  segread (&sregs);
  /* Int 21h, ah = 60h: Canonicalize filename or path. */
  intrpt.intno = 0x21;
  intrpt.eax = 0x6000;
  intrpt.ds  = (RealModeBufferParagraph + (RETURN_BUFFER_SIZE >> 4));
  intrpt.es  = RealModeBufferParagraph;
  regs.e.esi = 0;
  regs.e.edi = 0;
  regs.e.edx = ((unsigned) &intrpt);
  /* Int 21h, ax = 2511h: Issue real mode interrupt. */
  regs.x.ax = 0x2511;
  int86x (0x21, &regs, &regs, &sregs);
  if (regs.e.cflag != 0)
    return (-1);
  strncpy (direct, pRealModeBuffer, RETURN_BUFFER_SIZE);
  return (0);
}

extern void DOS_initialize_real_mode (void);
unsigned long RealModeBufferParagraph = 0;
char *pRealModeBuffer = NULL;

void
DOS_initialize_real_mode (void)
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x48;
  rIn.x.bx = 256;
  int86 (0x21, &rIn, &rOut);
  if (rOut.e.cflag == 0)
  {
    pRealModeBuffer = ((char *) rOut.e.ebx);
    RealModeBufferParagraph = rOut.x.ax;
  }
  return;
}
