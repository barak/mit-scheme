/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "ux.h"
#include "osfs.h"
#include "osfile.h"
#include "osio.h"

/* Don't use statfs on unknown systems.  It is necessary to enumerate
   the set of interesting results for the support to be useful.  */
#if ! ((defined (__linux__)) || (defined (__HPUX__)))
#  undef HAVE_STATFS
#endif

#ifdef HAVE_STATFS
#  ifdef HAVE_SYS_VFS_H
     /* GNU/Linux */
#    include <sys/vfs.h>
#  else
#    ifdef HAVE_SYS_MOUNT_H
       /* FreeBSD */
#      include <sys/param.h>
#      include <sys/mount.h>
#    endif
#  endif
#  ifdef __linux__
/* The following superblock magic constants are taken from the kernel
   headers for Linux 2.0.33.  We use these rather than reading the
   header files, because the Linux kernel header files have
   definitions that conflict with those of glibc2.  These constants
   are unlikely to be changed, so this ought to be safe.  */
#    ifndef AFFS_SUPER_MAGIC
#      define AFFS_SUPER_MAGIC 0xadff
#    endif
#    ifndef CIFS_MAGIC_NUMBER
#      define CIFS_MAGIC_NUMBER 0xFF534D42
#    endif
#    ifndef COH_SUPER_MAGIC
#      define COH_SUPER_MAGIC 0x012FF7B7
#    endif
#    ifndef EXT_SUPER_MAGIC
#      define EXT_SUPER_MAGIC 0x137D
#    endif
#    ifndef EXT2_SUPER_MAGIC
#      define EXT2_SUPER_MAGIC 0xEF53
#    endif
#    ifndef HPFS_SUPER_MAGIC
#      define HPFS_SUPER_MAGIC 0xf995e849
#    endif
#    ifndef ISOFS_SUPER_MAGIC
#      define ISOFS_SUPER_MAGIC 0x9660
#    endif
#    ifndef MINIX_SUPER_MAGIC
#      define MINIX_SUPER_MAGIC 0x137F
#    endif
#    ifndef MINIX_SUPER_MAGIC2
#      define MINIX_SUPER_MAGIC2 0x138F
#    endif
#    ifndef MINIX2_SUPER_MAGIC
#      define MINIX2_SUPER_MAGIC 0x2468
#    endif
#    ifndef MINIX2_SUPER_MAGIC2
#      define MINIX2_SUPER_MAGIC2 0x2478
#    endif
#    ifndef MSDOS_SUPER_MAGIC
#      define MSDOS_SUPER_MAGIC 0x4d44
#    endif
#    ifndef NCP_SUPER_MAGIC
#      define NCP_SUPER_MAGIC 0x564c
#    endif
#    ifndef NFS_SUPER_MAGIC
#      define NFS_SUPER_MAGIC 0x6969
#    endif
#    ifndef NTFS_SUPER_MAGIC
#      define NTFS_SUPER_MAGIC 0x5346544E
#    endif
#    ifndef PROC_SUPER_MAGIC
#      define PROC_SUPER_MAGIC 0x9fa0
#    endif
#    ifndef SMB_SUPER_MAGIC
#      define SMB_SUPER_MAGIC 0x517B
#    endif
#    ifndef SYSV2_SUPER_MAGIC
#      define SYSV2_SUPER_MAGIC 0x012FF7B6
#    endif
#    ifndef SYSV4_SUPER_MAGIC
#      define SYSV4_SUPER_MAGIC 0x012FF7B5
#    endif
#    ifndef XENIX_SUPER_MAGIC
#      define XENIX_SUPER_MAGIC 0x012FF7B4
#    endif
#    ifndef _XIAFS_SUPER_MAGIC
#      define _XIAFS_SUPER_MAGIC 0x012FD16D
#    endif
#  endif
#endif

#ifndef FILE_TOUCH_OPEN_TRIES
#  define FILE_TOUCH_OPEN_TRIES 5
#endif

#define STAT_SYSTEM_CALL(name, expression, if_failure)	\
  do {							\
    while ((expression) < 0)				\
      {							\
	if ((errno == ENOENT) || (errno == ENOTDIR))	\
	  if_failure;					\
	if (errno != EINTR)				\
	  error_system_call (errno, name);		\
	deliver_pending_interrupts ();			\
      }							\
  } while (0)

void
UX_read_fd_status (int fd, struct stat * s)
{
  STD_VOID_SYSTEM_CALL (syscall_fstat, (UX_fstat (fd, s)));
}

int
UX_read_file_status (const char * filename, struct stat * s)
{
  STAT_SYSTEM_CALL (syscall_lstat, (UX_lstat (filename, s)), return (0));
  return (1);
}

int
UX_read_file_status_indirect (const char * filename, struct stat * s)
{
  STAT_SYSTEM_CALL (syscall_stat, (UX_stat (filename, s)), return (0));
  return (1);
}

enum file_existence
OS_file_existence_test (const char * name)
{
  struct stat s;
  if (!UX_read_file_status (name, (&s)))
    return (file_doesnt_exist);
#ifdef HAVE_SYMLINK
  if (((s . st_mode) & S_IFMT) == S_IFLNK)
    {
      if (UX_read_file_status_indirect (name, (&s)))
	return (file_does_exist);
      else
	return (file_is_link);
    }
#endif
  return (file_does_exist);
}

enum file_existence
OS_file_existence_test_direct (const char * name)
{
  struct stat s;
  if (!UX_read_file_status (name, (&s)))
    return (file_doesnt_exist);
#ifdef HAVE_SYMLINK
  if (((s . st_mode) & S_IFMT) == S_IFLNK)
    return (file_is_link);
#endif
  return (file_does_exist);
}

#ifndef S_ISREG
#  define S_ISREG(mode) (((mode) & S_IFREG) != 0)
#endif
#ifndef S_ISDIR
#  define S_ISDIR(mode) (((mode) & S_IFDIR) != 0)
#endif
#ifndef S_ISLNK
#  define S_ISLNK(mode) (((mode) & S_IFLNK) != 0)
#endif
#ifndef S_ISCHR
#  define S_ISCHR(mode) (((mode) & S_IFCHR) != 0)
#endif
#ifndef S_ISBLK
#  define S_ISBLK(mode) (((mode) & S_IFBLK) != 0)
#endif
#ifndef S_ISFIFO
#  define S_ISFIFO(mode) (((mode) & S_IFIFO) != 0)
#endif
#ifndef S_ISSOCK
#  define S_ISSOCK(mode) (((mode) & S_IFSOCK) != 0)
#endif

#define COMPUTE_FILE_TYPE(proc, name)					\
{									\
  struct stat s;							\
  if (!proc ((name), (&s)))						\
    return (file_type_nonexistent);					\
  else if (S_ISREG (s . st_mode))					\
    return (file_type_regular);						\
  else if (S_ISDIR (s . st_mode))					\
    return (file_type_directory);					\
  else if (S_ISLNK (s . st_mode))					\
    return (file_type_unix_symbolic_link);				\
  else if (S_ISCHR (s . st_mode))					\
    return (file_type_unix_character_device);				\
  else if (S_ISBLK (s . st_mode))					\
    return (file_type_unix_block_device);				\
  else if (S_ISFIFO (s . st_mode))					\
    return (file_type_unix_fifo);					\
  else if (S_ISSOCK (s . st_mode))					\
    return (file_type_unix_stream_socket);				\
  else									\
    return (file_type_unknown);						\
}

enum file_type
OS_file_type_direct (const char * name)
  COMPUTE_FILE_TYPE (UX_read_file_status, name)

enum file_type
OS_file_type_indirect (const char * name)
  COMPUTE_FILE_TYPE (UX_read_file_status_indirect, name)

const char *
UX_file_system_type (const char * name)
{
#ifdef HAVE_STATFS
  struct statfs s;
  STAT_SYSTEM_CALL (syscall_statfs, (UX_statfs (name, (&s))), return (0));

#ifdef __linux__
  switch (s . f_type)
    {
    case CIFS_MAGIC_NUMBER:	return ("cifs");
    case COH_SUPER_MAGIC:	return ("coherent");
    case EXT_SUPER_MAGIC:	return ("ext");
    case EXT2_SUPER_MAGIC:	return ("ext2");
    case HPFS_SUPER_MAGIC:	return ("hpfs");
    case ISOFS_SUPER_MAGIC:	return ("iso9660");
    case MINIX_SUPER_MAGIC:	return ("minix1");
    case MINIX_SUPER_MAGIC2:	return ("minix1-30");
    case MINIX2_SUPER_MAGIC:	return ("minix2");
    case MINIX2_SUPER_MAGIC2:	return ("minix2-30");
    case MSDOS_SUPER_MAGIC:	return ("fat");
    case NCP_SUPER_MAGIC:	return ("ncp");
    case NFS_SUPER_MAGIC:	return ("nfs");
    case NTFS_SUPER_MAGIC:	return ("ntfs");
    case PROC_SUPER_MAGIC:	return ("proc");
    case SMB_SUPER_MAGIC:	return ("smb");
    case SYSV2_SUPER_MAGIC:	return ("sysv2");
    case SYSV4_SUPER_MAGIC:	return ("sysv4");
    case XENIX_SUPER_MAGIC:	return ("xenix");
    case _XIAFS_SUPER_MAGIC:	return ("xiafs");
    }
#endif /* __linux__ */

#ifdef __HPUX__
  switch ((s . f_fsid) [1])
    {
    case MOUNT_UFS:		return ("ufs");
    case MOUNT_NFS:		return ("nfs");
    case MOUNT_CDFS:		return ("iso9660");
    }
#endif /* __HPUX__ */
#endif /* HAVE_STATFS */

  return (0);
}

int
OS_file_directory_p (const char * name)
{
  struct stat s;
  return
    ((UX_read_file_status_indirect (name, (&s)))
     && (((s . st_mode) & S_IFMT) == S_IFDIR));
}

const char *
OS_file_soft_link_p (const char * name)
{
#ifdef HAVE_SYMLINK
  struct stat s;
  if (! ((UX_read_file_status (name, (&s)))
	 && (((s . st_mode) & S_IFMT) == S_IFLNK)))
    return (0);
  {
    int scr;
    int buffer_length = 100;
    char * buffer = (UX_malloc (buffer_length));
    if (buffer == 0)
      error_system_call (ENOMEM, syscall_malloc);
    while (1)
      {
	STD_UINT_SYSTEM_CALL
	  (syscall_readlink, scr, (UX_readlink (name, buffer, buffer_length)));
	if (scr < buffer_length)
	  break;
	buffer_length *= 2;
	buffer = (UX_realloc (buffer, buffer_length));
	if (buffer == 0)
	  error_system_call (ENOMEM, syscall_realloc);
      }
    (buffer[scr]) = '\0';
    return ((const char *) buffer);
  }
#else
  return (0);
#endif
}

int
OS_file_access (const char * name, unsigned int mode)
{
  return ((UX_access (name, mode)) == 0);
}

void
OS_file_remove (const char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_unlink, (UX_unlink (name)));
}

void
OS_file_remove_link (const char * name)
{
  struct stat s;
  if ((UX_read_file_status (name, (&s)))
      && ((((s . st_mode) & S_IFMT) == S_IFREG)
#ifdef HAVE_SYMLINK
	  || (((s . st_mode) & S_IFMT) == S_IFLNK)
#endif
	  ))
    UX_unlink (name);
}

void
OS_file_link_hard (const char * from_name, const char * to_name)
{
  STD_VOID_SYSTEM_CALL (syscall_link, (UX_link (from_name, to_name)));
}

void
OS_file_link_soft (const char * from_name, const char * to_name)
{
#ifdef HAVE_SYMLINK
  STD_VOID_SYSTEM_CALL (syscall_symlink, (UX_symlink (from_name, to_name)));
#else
  error_unimplemented_primitive ();
#endif
}

void
OS_file_rename (const char * from_name, const char * to_name)
{
  STD_VOID_SYSTEM_CALL (syscall_rename, (UX_rename (from_name, to_name)));
}

#ifndef FILE_COPY_BUFFER_LENGTH
#define FILE_COPY_BUFFER_LENGTH 8192
#endif

void
OS_file_copy (const char * from_name, const char * to_name)
{
  Tchannel src, dst;
  off_t src_len, len;
  char buffer [FILE_COPY_BUFFER_LENGTH];
  long nread, nwrite;

  src = (OS_open_input_file (from_name));
  OS_channel_close_on_abort (src);
  dst = (OS_open_output_file (to_name));
  OS_channel_close_on_abort (dst);
  src_len = (OS_file_length (src));
  len = (sizeof (buffer));
  while (src_len > 0)
    {
      if (src_len < len)
	len = src_len;
      nread = (OS_channel_read (src, buffer, len));
      if (nread < 0)
	error_system_call (errno, syscall_read);
      else if (nread == 0)
	break;
      nwrite = (OS_channel_write (dst, buffer, nread));
      if (nwrite < 0)
	error_system_call (errno, syscall_write);
      else if (nwrite < nread)
	error_system_call (ENOSPC, syscall_write);
      src_len -= nread;
    }
  OS_channel_close (src);
  OS_channel_close (dst);
}

void
OS_directory_make (const char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_mkdir, (UX_mkdir (name, MODE_DIR)));
}

void
OS_directory_delete (const char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_rmdir, (UX_rmdir (name)));
}

static void protect_fd (int fd);

int
OS_file_touch (const char * filename)
{
  int fd;
  transaction_begin ();
  {
    unsigned int count = 0;
    while (1)
      {
	count += 1;
	/* Use O_EXCL to prevent overwriting existing file. */
	fd = (UX_open (filename, (O_RDWR | O_CREAT | O_EXCL), MODE_REG));
	if (fd >= 0)
	  {
	    protect_fd (fd);
	    transaction_commit ();
	    return (1);
	  }
	if (errno == EEXIST)
	  {
	    fd = (UX_open (filename, O_RDWR, MODE_REG));
	    if (fd >= 0)
	      {
		protect_fd (fd);
		break;
	      }
	    else if ((errno == ENOENT)
#ifdef ESTALE
		     || (errno == ESTALE)
#endif
		     )
	      continue;
	  }
	if (count >= FILE_TOUCH_OPEN_TRIES)
	  error_system_call (errno, syscall_open);
      }
  }
  {
    struct stat file_status;
    STD_VOID_SYSTEM_CALL (syscall_fstat, (UX_fstat (fd, (&file_status))));
    if (((file_status . st_mode) & S_IFMT) != S_IFREG)
      return (-1);
    /* CASE 3: file length of 0 needs special treatment. */
    if ((file_status . st_size) == 0)
      {
	char buf [1];
	(buf[0]) = '\0';
	STD_VOID_SYSTEM_CALL (syscall_write, (UX_write (fd, buf, 1)));
#ifdef HAVE_FTRUNCATE
	STD_VOID_SYSTEM_CALL (syscall_ftruncate, (UX_ftruncate (fd, 0)));
	transaction_commit ();
#else
	transaction_commit ();
	/* FIXME: Need to check for EINTR.  */
	fd = (UX_open (filename, (O_WRONLY | O_TRUNC), MODE_REG));
	if (fd >= 0)
	  (void) UX_close (fd);
#endif
	return (0);
      }
  }
  /* CASE 4: read, then write back the first byte in the file. */
  {
    char buf [1];
    int scr;
    STD_UINT_SYSTEM_CALL (syscall_read, scr, (UX_read (fd, buf, 1)));
    if (scr > 0)
      {
	STD_VOID_SYSTEM_CALL (syscall_lseek, (UX_lseek (fd, 0, SEEK_SET)));
	STD_VOID_SYSTEM_CALL (syscall_write, (UX_write (fd, buf, 1)));
      }
  }
  transaction_commit ();
  return (0);
}

static void
protect_fd_close (void * ap)
{
  (void) UX_close (* ((int *) ap));
}

static void
protect_fd (int fd)
{
  int * p = (dstack_alloc (sizeof (int)));
  (*p) = fd;
  transaction_record_action (tat_always, protect_fd_close, p);
}

static DIR ** directory_pointers;
static unsigned int n_directory_pointers;

static void
close_all_directories (void)
{
  DIR ** scan = directory_pointers;
  DIR ** end = (scan + n_directory_pointers);
  while (scan < end)
    {
      if ((*scan) != 0)
	{
	  closedir (*scan);
	  (*scan) = 0;
	}
      scan += 1;
    }
  directory_pointers = 0;
  n_directory_pointers = 0;
}

void
UX_initialize_directory_reader (void)
{
  directory_pointers = 0;
  n_directory_pointers = 0;
  add_reload_cleanup (close_all_directories);
}

static unsigned int
allocate_directory_pointer (DIR * pointer)
{
  if (n_directory_pointers == 0)
    {
      DIR ** pointers = ((DIR **) (UX_malloc ((sizeof (DIR *)) * 4)));
      if (pointers == 0)
	error_system_call (ENOMEM, syscall_malloc);
      directory_pointers = pointers;
      n_directory_pointers = 4;
      {
	DIR ** scan = directory_pointers;
	DIR ** end = (scan + n_directory_pointers);
	(*scan++) = pointer;
	while (scan < end)
	  (*scan++) = 0;
      }
      return (0);
    }
  {
    DIR ** scan = directory_pointers;
    DIR ** end = (scan + n_directory_pointers);
    while (scan < end)
      if ((*scan++) == 0)
	{
	  (*--scan) = pointer;
	  return (scan - directory_pointers);
	}
  }
  {
    unsigned int result = n_directory_pointers;
    unsigned int n_pointers = (2 * n_directory_pointers);
    DIR ** pointers =
      ((DIR **)
       (UX_realloc (((void *) directory_pointers),
		    ((sizeof (DIR *)) * n_pointers))));
    if (pointers == 0)
      error_system_call (ENOMEM, syscall_realloc);
    {
      DIR ** scan = (pointers + result);
      DIR ** end = (pointers + n_pointers);
      (*scan++) = pointer;
      while (scan < end)
	(*scan++) = 0;
    }
    directory_pointers = pointers;
    n_directory_pointers = n_pointers;
    return (result);
  }
}

#define REFERENCE_DIRECTORY(index) (directory_pointers[(index)])
#define DEALLOCATE_DIRECTORY(index) ((directory_pointers[(index)]) = 0)

int
OS_directory_valid_p (unsigned int index)
{
  return
    ((index < n_directory_pointers)
     && ((REFERENCE_DIRECTORY (index)) != 0));
}

unsigned int
OS_directory_open (const char * name)
{
  DIR * pointer;
  STD_PTR_SYSTEM_CALL (syscall_opendir, pointer, (opendir (name)));
  /* FIXME: This leaks a directory pointer if malloc or realloc fails
     here.  Avoiding this safely with a transaction is unfortunately
     not straightforward.  */
  return (allocate_directory_pointer (pointer));
}

const char *
OS_directory_read (unsigned int index)
{
  struct dirent * entry = (readdir (REFERENCE_DIRECTORY (index)));
  return ((entry == 0) ? 0 : (entry -> d_name));
}

const char *
OS_directory_read_matching (unsigned int index, const char * prefix)
{
  DIR * pointer = (REFERENCE_DIRECTORY (index));
  unsigned int n = (strlen (prefix));
  while (1)
    {
      struct dirent * entry = (readdir (pointer));
      if (entry == 0)
	return (0);
      if ((strncmp (prefix, (entry -> d_name), n)) == 0)
	return (entry -> d_name);
    }
}

void
OS_directory_close (unsigned int index)
{
  closedir (REFERENCE_DIRECTORY (index));
  DEALLOCATE_DIRECTORY (index);
}
