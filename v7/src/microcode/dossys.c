/* -*-C-*-

$Id: dossys.c,v 1.4 1992/10/07 06:23:33 jinx Exp $

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
#include "msdos.h"
#include "dossys.h"

#ifdef UNUSED
int 
dos_keyboard_input_available_p (void)
{
  union REGS regs;
  
  regs.h.ah = 0x0B;
  intdos (&regs, &regs);
  return (regs.h.al != 0);
}

unsigned char 
dos_get_keyboard_character (void)
{
  union REGS regs;
  
  regs.h.ah = 0x07;
  intdos (&regs, &regs);
  return ((unsigned char) (regs.h.al));
}
#endif /* UNUSED */

int 
dos_poll_keyboard_character (unsigned char * result)
{
  union REGS regs;
  
  regs.h.ah = 0x06;
  regs.h.dl = 0xFF;
  intdos (&regs, &regs);
  *result = ((unsigned char) (regs.h.al));
  return ((regs.x.flags & 0x40) == 0);
}

void
dos_console_write_character (unsigned char character)
{
  union REGS regs;
  
  regs.h.ah = 0x06;
  regs.h.dl = character;
  intdos (&regs, &regs);
  return;
}

int 
dos_console_write (void * vbuffer, size_t nsize)
{
  unsigned char * buffer = vbuffer;
  union REGS inregs, outregs;
  int i;  
  
  inregs.h.ah = 0x06;
  for (i = 0; i < nsize; i++)
  {
    inregs.h.dl = buffer[i];
    intdos (&inregs, &outregs);
  }
  return (nsize);
}

/* DOS I/O functions using handles */

#ifdef UNUSED
handle_t 
dos_open_file_with_handle (unsigned char * name, int mode)
{
  union REGS regs;
  struct SREGS segregs;
  
  regs.e.edx = ((unsigned long) name);
  segread (&segregs);
  regs.h.ah = 0x3D;
  regs.h.al = mode;
  intdosx (&regs, &regs, &segregs);
  return ((regs.x.cflag) ? DOS_FAILURE : ((unsigned int) regs.x.ax));
}

int 
dos_close_file_with_handle (handle_t handle)
{
  union REGS regs;
  
  regs.x.bx = handle;
  regs.h.al = 0x3E;
  intdos (&regs, &regs);
  return ((regs.x.cflag) ? DOS_FAILURE : DOS_SUCCESS);
}

int 
dos_read_file_with_handle (handle_t handle, void * buffer, size_t nbytes)
{
  union REGS regs;
  struct SREGS segregs;
  
  regs.x.bx = handle;  
  regs.e.edx = ((unsigned long) buffer);
  regs.e.ecx = nbytes;
  segread (&segregs);
  regs.h.ah = 0x3F;
  intdosx (&regs, &regs, &segregs);
  return ((regs.x.cflag) ? DOS_FAILURE : regs.e.eax);
}

int 
dos_write_file_with_handle (handle_t handle, void * buffer, size_t nbytes)
{
  union REGS regs;
  struct SREGS segregs;
  
  regs.x.bx = handle;
  regs.e.edx = (unsigned long) buffer;
  regs.e.ecx = nbytes;
  segread (&segregs);
  regs.h.ah = 0x40;
  intdosx (&regs, &regs, &segregs);
  return ((regs.x.cflag) ? DOS_FAILURE : regs.e.eax);
}
  
int 
dos_get_device_status_with_handle (handle_t handle)
{ 
  union REGS regs;
  
  regs.x.bx = handle;
  regs.x.ax = 0x4400;
  intdos (&regs, &regs);
  return ((regs.x.cflag) ? DOS_FAILURE : ((unsigned int) regs.x.dx));
}
  
int 
dos_set_device_status_with_handle (handle_t handle, int mode)
{ 
  int original_mode;
  union REGS regs;
  
  original_mode = dos_get_device_status_with_handle(handle);
  if (original_mode == DOS_FAILURE)
    return (DOS_FAILURE);
  regs.x.dx = mode;
  regs.x.bx = handle;
  regs.x.ax = 0x4401;
  intdos (&regs, &regs);
  return ((regs.x.cflag) ? DOS_FAILURE : original_mode);
}  
#endif /* UNUSED */

void 
dos_get_version (version_t *version_number)
{
  union REGS regs;

  /* Use old style version number because we may be running below DOS 5.0 */
  regs.h.al = 0x01;
  regs.h.ah = 0x30;
  intdos (&regs, &regs);
  version_number -> major = regs.h.al;
  version_number -> minor = regs.h.ah;
  if ((version_number -> major) >= 5)
  { /* Get the real version. */
    regs.x.ax = 0x3306;
    intdos (&regs, &regs);
    version_number -> major = regs.h.bl;
    version_number -> minor = regs.h.bh;
  }
  return;
}

#ifdef UNUSED
void 
dos_reset_drive (void)
{
  union REGS regs;
  
  regs.h.al = 0x0d;
  intdos (&regs, &regs);
  return;
}

int 
dos_set_verify_flag (int verify_p)
{
  union REGS regs;
  int old_flag;
  
  regs.h.ah = 0x54;
  intdos (&regs, &regs);
  old_flag = regs.h.al;
  regs.h.al = ((verify_p) ? 1 : 0);
  regs.h.ah = 0x2E;
  intdos (&regs, &regs);
  return (old_flag);
}
#endif /* UNUSED */

int 
dos_set_ctrl_c_check_flag (int check_p)
{
  union REGS regs;
  int old_flag;
  
  regs.x.ax = 0x3300;
  intdos (&regs, &regs);
  old_flag = regs.h.dl;
  regs.h.dl = ((check_p) ? 1 : 0);
  regs.x.ax = 0x3301;
  intdos (&regs, &regs);
  return (old_flag);
}

int 
dos_rename_file (const char * old, const char * new)
{
  union REGS regs;
  struct SREGS segregs;
  
  regs.e.edx = ((unsigned long) old);
  regs.e.edi = ((unsigned long) new);
  segread (&segregs);
  segregs.es = segregs.ds;
  regs.h.ah = 0x56;
  intdosx (&regs, &regs, &segregs);
  if (regs.x.cflag)
    return (DOS_FAILURE);
  else
    return (DOS_SUCCESS);
}

#ifdef UNUSED
int 
dos_get_machine_name (char * name)
{
  union REGS regs;
  struct SREGS segregs;
  
  regs.e.edx = ((unsigned long) name);
  segread (&segregs);
  regs.x.ax = 0x5E00;
  intdosx (&regs, &regs, &segregs);
  if ((regs.x.cflag) || (regs.h.ch == 0))
    return (DOS_FAILURE);
  else
    return (regs.h.cl);
}
#endif /* UNUSED */

int 
dos_drive_letter_to_number (char letter)
{
  if (letter == '\0')
    return 0;
  else if ((letter >= 'a') && (letter <= 'z'))
    return ((letter - 'a') + 1);
  else if ((letter >= 'A') && (letter <= 'Z'))
    return ((letter - 'A') + 1);
  else
    return (-1);
}

#ifdef UNUSED
char 
dos_drive_number_to_letter (int number)
{
  if ((number >= 1) && (number <= 26))
    return ('A' + (number - 1));
  else
    return ('\0');
}
#endif /* UNUSED */

int 
dos_set_default_drive (int drive_number)
{
  union REGS regs;
  
  if (drive_number > 0)
  {	
    regs.h.dl = (drive_number - 1);
    regs.h.ah = 0x0E;
    intdos (&regs, &regs);
  }
  return (DOS_SUCCESS);
}
    
#ifdef UNUSED
int
dos_get_default_drive (int drive_number)
{
  union REGS regs;
  
  regs.h.ah = 0x19;
  intdos (&regs, &regs);
  return ((regs.h.al) + 1);
}
#endif /* UNUSED */

dos_boolean 
dos_pathname_as_filename (char * name, char * buffer)
{ /* Returns whether directory encountered is top level */
  unsigned int end_index = ((strlen (name)) - 1);

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
  intdosx (&regs, &regs, &sregs);
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
  union REGS regs;

  regs.h.ah = 0x48;
  regs.x.bx = 256;
  intdos (&regs, &regs);
  if (regs.e.cflag == 0)
  {
    pRealModeBuffer = ((char *) regs.e.ebx);
    RealModeBufferParagraph = regs.x.ax;
  }
  return;
}
