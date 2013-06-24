
(define open-clipboard
  (windows-procedure (open-clipboard (hwnd hwnd))
    bool user32.dll "OpenClipboard"))

(define close-clipboard
  (windows-procedure (close-clipboard) bool user32.dll "CloseClipboard"))

(define set-clipboard-data
  (windows-procedure (set-clipboard-data (format uint) (hdata handle))
    handle user32.dll "SetClipboardData"))

(define get-clipboard-data
  (windows-procedure (get-clipboard-data (format uint))
    handle user32.dll "GetClipboardData"))

(define global-alloc
  (windows-procedure (global-alloc (fuFlags uint) (cbBytes dword))
    handle kernel32.dll "GlobalAlloc"))

(define global-lock
  (windows-procedure (global-lock (hglbMem handle))
    uint kernel32.dll "GlobalLock"))

(define global-unlock
  (windows-procedure (global-unlock (hglbMem handle))
    bool kernel32.dll "GlobalUnlock"))

(define global-size
  (windows-procedure (global-size (hglbMem handle))
    dword kernel32.dll "GlobalSize"))

(define copy-memory
  (windows-procedure (copy-memory (destination unchecked) (source unchecked)
				  (length dword))
    bool kernel32.dll "RtlMoveMemory"))

(define (clipboard-set! s)
  (let ((clip? (open-clipboard 0)))
    (and clip?
	 (let* ((len  (+ (string-length s) 1))
		(mem  (global-alloc #x2002 #|= GMEM_MOVEABLE + GMEM_DDESHARE|#
				    len)))
	   (if (= mem 0)
	       #F
	       (let ((ptr  (global-lock mem)))
		 (if (= ptr 0)
		     #F
		     (begin
		       (copy-memory ptr s len)
		       (global-unlock mem)
		       (set-clipboard-data CF_TEXT mem)
		       (close-clipboard)))))))))

(define (clipboard-ref)
  (let ((clip? (open-clipboard 0)))
    (and clip?
	 (let* ((mem  (get-clipboard-data CF_TEXT)))
	   (if (= mem 0)
	       'empty
	       (let* ((maxlen (global-size mem))
		      (s      (string-allocate maxlen))
		      (ptr    (global-lock mem)))
		 (copy-memory s ptr maxlen)
		 (global-unlock mem)
		 (close-clipboard)
		 (let ((end (vector-8b-find-next-char s 0 maxlen 0)))
		   (set-string-length! s end))
		 s))))))