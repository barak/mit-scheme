; These get overriden when TK is loaded
(define (tk-doevents) 'tk-doevents)
(define (tk-init dsp) 'tk-init)

(with-working-directory-pathname
 "/scheme/8.0/700/swat2/scheme"
 (lambda ()
   ; Dynamically load the microcode
   (load "../c/scxl")
   (load "../c/uitk")
   
   ; And now the Scheme level
   ;;(load "scc-macros")
   ;;(load "uitk-macros")
   (load "control-floating-errors")
   (load "structures")
   (load "structures2")
   (load "generics")
   (load "uitk")
   (load "xlibCONSTANTS")
   (load "mit-xlib")
   (load "tk-mit")
   (load "mit-xhooks")
   (load "widget-mit")
   (load "baseobj")
   (load "widget")
   (load "geometry")
   (load "simple")
   (load "canvas")
   (load "menu")
   (load "text")
   ;;(load "rtest")
   ;;(load "btest")
   ))
