;; $Id$
;;
;; This file may be used to produce png files from geda schematics from
;; the command line. Typical usage is:
;;
;;   gschem -z -o mysch.png -r /path/to/this/file/image.scm mysch.sch
;;
;; The schematic in "mysch.sch" will be exported to the file "mysch.png"
;;
;; The image output file extension can be bmp, png, jpeg, jpg, pdf, tif,
;; or tiff.
;;

(image-size 1024 768)
;(image-size 3200 2400)

(image-color "enabled")
;(image-color "disabled")

;(invert-images "enabled")
(invert-images "disabled")

(anti-aliasing "subpixel")

; You need call this after you call any rc file function
(gschem-use-rc-values)

; filename is specified on the command line
(gschem-png-image "output.png")

(gschem-exit)
