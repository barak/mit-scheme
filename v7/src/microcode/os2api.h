/* -*-C-*-

$Id: os2api.h,v 1.2 1994/12/19 22:30:46 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

#ifndef SCM_OS2API_H
#define SCM_OS2API_H

#define XTD_API_CALL(proc, args, if_error)				\
{									\
  while (1)								\
    {									\
      APIRET rc = (proc args);						\
      if (rc == NO_ERROR)						\
	break;								\
      if (rc != ERROR_INTERRUPT)					\
	{								\
	  if_error;							\
	  OS2_error_system_call (rc, syscall_ ## proc);			\
	}								\
    }									\
}

#define STD_API_CALL(proc, args) XTD_API_CALL (proc, args, {})

#ifdef CLOSED_API_CALLS

extern APIRET dos_async_timer (ULONG, HSEM, PHTIMER);
extern APIRET dos_close (HFILE);
extern APIRET dos_close_event_sem (HEV);
extern APIRET dos_close_mutex_sem (HMTX);
extern APIRET dos_close_queue (HQUEUE);
extern APIRET dos_create_dir (PSZ, PEAOP2);
extern APIRET dos_create_event_sem (PSZ, PHEV, ULONG, BOOL32);
extern APIRET dos_create_mutex_sem (PSZ, PHMTX, ULONG, BOOL32);
extern APIRET dos_create_pipe (PHFILE, PHFILE, ULONG);
extern APIRET dos_create_queue (PHQUEUE, ULONG, PSZ);
extern APIRET dos_create_thread (PTID, PFNTHREAD, ULONG, ULONG, ULONG);
extern APIRET dos_delete (PSZ);
extern APIRET dos_delete_dir (PSZ);
extern void   dos_exit (ULONG, ULONG);
extern APIRET dos_find_close (HDIR);
extern APIRET dos_find_first (PSZ, PHDIR, ULONG, PVOID, ULONG, PULONG, ULONG);
extern APIRET dos_find_next (HDIR, PVOID, ULONG, PULONG);
extern APIRET dos_get_info_blocks (PTIB *, PPIB *);
extern APIRET dos_get_message (PCHAR *, ULONG, PCHAR, ULONG, ULONG, PSZ, PULONG);
extern APIRET dos_kill_thread (TID);
extern APIRET dos_move (PSZ, PSZ);
extern APIRET dos_open (PSZ, PHFILE, PULONG, ULONG, ULONG, ULONG, ULONG, PEAOP2);
extern APIRET dos_post_event_sem (HEV);
extern APIRET dos_query_current_dir (ULONG, PBYTE, PULONG);
extern APIRET dos_query_current_disk (PULONG, PULONG);
extern APIRET dos_query_file_info (HFILE, ULONG, PVOID, ULONG);
extern APIRET dos_query_fs_attach (PSZ, ULONG, ULONG, PFSQBUFFER2, PULONG);
extern APIRET dos_query_fs_info (ULONG, ULONG, PVOID, ULONG);
extern APIRET dos_query_h_type (HFILE, PULONG, PULONG);
extern APIRET dos_query_n_p_h_state (HPIPE, PULONG);
extern APIRET dos_query_path_info (PSZ, ULONG, PVOID, ULONG);
extern APIRET dos_query_sys_info (ULONG, ULONG, PVOID, ULONG);
extern APIRET dos_read (HFILE, PVOID, ULONG, PULONG);
extern APIRET dos_read_queue (HQUEUE, PREQUESTDATA, PULONG, PPVOID, ULONG, BOOL32, PBYTE, HEV);
extern APIRET dos_release_mutex_sem (HMTX);
extern APIRET dos_request_mutex_sem (HMTX, ULONG);
extern APIRET dos_reset_event_sem (HEV, PULONG);
extern APIRET dos_scan_env (PSZ, PSZ *);
extern APIRET dos_set_current_dir (PSZ);
extern APIRET dos_set_default_disk (ULONG);
extern APIRET dos_set_file_ptr (HFILE, LONG, ULONG, PULONG);
extern APIRET dos_set_file_size (HFILE, ULONG);
extern APIRET dos_set_path_info (PSZ, ULONG, PVOID, ULONG, ULONG);
extern APIRET dos_start_timer (ULONG, HSEM, PHTIMER);
extern APIRET dos_stop_timer (PHTIMER);
extern APIRET dos_wait_event_sem (HEV, ULONG);
extern APIRET dos_write (HFILE, PVOID, ULONG, PULONG);
extern APIRET dos_write_queue (HQUEUE, ULONG, ULONG, PVOID, ULONG);
extern APIRET kbd_char_in (PKBDKEYINFO, USHORT, HKBD);
extern APIRET vio_wrt_tty (PCH, USHORT, HVIO);

#else /* not CLOSED_API_CALLS */

#define dos_async_timer		DosAsyncTimer
#define dos_close		DosClose
#define dos_close_event_sem	DosCloseEventSem
#define dos_close_mutex_sem	DosCloseMutexSem
#define dos_close_queue		DosCloseQueue
#define dos_create_dir		DosCreateDir
#define dos_create_event_sem	DosCreateEventSem
#define dos_create_mutex_sem	DosCreateMutexSem
#define dos_create_pipe		DosCreatePipe
#define dos_create_queue	DosCreateQueue
#define dos_create_thread	DosCreateThread
#define dos_delete		DosDelete
#define dos_delete_dir		DosDeleteDir
#define dos_exit		DosExit
#define dos_find_close		DosFindClose
#define dos_find_first		DosFindFirst
#define dos_find_next		DosFindNext
#define dos_get_info_blocks	DosGetInfoBlocks
#define dos_get_message		DosGetMessage
#define dos_kill_thread		DosKillThread
#define dos_move		DosMove
#define dos_open		DosOpen
#define dos_post_event_sem	DosPostEventSem
#define dos_query_current_dir	DosQueryCurrentDir
#define dos_query_current_disk	DosQueryCurrentDisk
#define dos_query_file_info	DosQueryFileInfo
#define dos_query_fs_attach	DosQueryFSAttach
#define dos_query_fs_info	DosQueryFSInfo
#define dos_query_h_type	DosQueryHType
#define dos_query_n_p_h_state	DosQueryNPHState
#define dos_query_path_info	DosQueryPathInfo
#define dos_query_sys_info	DosQuerySysInfo
#define dos_read		DosRead
#define dos_read_queue		DosReadQueue
#define dos_release_mutex_sem	DosReleaseMutexSem
#define dos_request_mutex_sem	DosRequestMutexSem
#define dos_reset_event_sem	DosResetEventSem
#define dos_scan_env		DosScanEnv
#define dos_set_current_dir	DosSetCurrentDir
#define dos_set_default_disk	DosSetDefaultDisk
#define dos_set_file_ptr	DosSetFilePtr
#define dos_set_file_size	DosSetFileSize
#define dos_set_path_info	DosSetPathInfo
#define dos_start_timer		DosStartTimer
#define dos_stop_timer		DosStopTimer
#define dos_wait_event_sem	DosWaitEventSem
#define dos_write		DosWrite
#define dos_write_queue		DosWriteQueue
#define kbd_char_in		KbdCharIn
#define vio_wrt_tty		VioWrtTTY

#define syscall_dos_async_timer		syscall_DosAsyncTimer
#define syscall_dos_close		syscall_DosClose
#define syscall_dos_close_event_sem	syscall_DosCloseEventSem
#define syscall_dos_close_mutex_sem	syscall_DosCloseMutexSem
#define syscall_dos_close_queue		syscall_DosCloseQueue
#define syscall_dos_create_dir		syscall_DosCreateDir
#define syscall_dos_create_event_sem	syscall_DosCreateEventSem
#define syscall_dos_create_mutex_sem	syscall_DosCreateMutexSem
#define syscall_dos_create_pipe		syscall_DosCreatePipe
#define syscall_dos_create_queue	syscall_DosCreateQueue
#define syscall_dos_create_thread	syscall_DosCreateThread
#define syscall_dos_delete		syscall_DosDelete
#define syscall_dos_delete_dir		syscall_DosDeleteDir
#define syscall_dos_exit		syscall_DosExit
#define syscall_dos_find_close		syscall_DosFindClose
#define syscall_dos_find_first		syscall_DosFindFirst
#define syscall_dos_find_next		syscall_DosFindNext
#define syscall_dos_get_info_blocks	syscall_DosGetInfoBlocks
#define syscall_dos_get_message		syscall_DosGetMessage
#define syscall_dos_kill_thread		syscall_DosKillThread
#define syscall_dos_move		syscall_DosMove
#define syscall_dos_open		syscall_DosOpen
#define syscall_dos_post_event_sem	syscall_DosPostEventSem
#define syscall_dos_query_current_dir	syscall_DosQueryCurrentDir
#define syscall_dos_query_current_disk	syscall_DosQueryCurrentDisk
#define syscall_dos_query_file_info	syscall_DosQueryFileInfo
#define syscall_dos_query_fs_attach	syscall_DosQueryFSAttach
#define syscall_dos_query_fs_info	syscall_DosQueryFSInfo
#define syscall_dos_query_h_type	syscall_DosQueryHType
#define syscall_dos_query_n_p_h_state	syscall_DosQueryNPHState
#define syscall_dos_query_path_info	syscall_DosQueryPathInfo
#define syscall_dos_query_sys_info	syscall_DosQuerySysInfo
#define syscall_dos_read		syscall_DosRead
#define syscall_dos_read_queue		syscall_DosReadQueue
#define syscall_dos_release_mutex_sem	syscall_DosReleaseMutexSem
#define syscall_dos_request_mutex_sem	syscall_DosRequestMutexSem
#define syscall_dos_reset_event_sem	syscall_DosResetEventSem
#define syscall_dos_scan_env		syscall_DosScanEnv
#define syscall_dos_set_current_dir	syscall_DosSetCurrentDir
#define syscall_dos_set_default_disk	syscall_DosSetDefaultDisk
#define syscall_dos_set_file_ptr	syscall_DosSetFilePtr
#define syscall_dos_set_file_size	syscall_DosSetFileSize
#define syscall_dos_set_path_info	syscall_DosSetPathInfo
#define syscall_dos_start_timer		syscall_DosStartTimer
#define syscall_dos_stop_timer		syscall_DosStopTimer
#define syscall_dos_wait_event_sem	syscall_DosWaitEventSem
#define syscall_dos_write		syscall_DosWrite
#define syscall_dos_write_queue		syscall_DosWriteQueue
#define syscall_kbd_char_in		syscall_KbdCharIn
#define syscall_vio_wrt_tty		syscall_VioWrtTTY
#define syscall_VIO16WRTTTY		syscall_VioWrtTTY

#endif /* not CLOSED_API_CALLS */

#endif /* SCM_OS2API_H */
