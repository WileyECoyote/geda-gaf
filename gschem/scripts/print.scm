;; $Id$
;;
;; This file may be used to print geda schematics from the
;; command line. Typical usage is:
;;
;;   gschem -z -o mysch.ps -r /path/to/this/file/print.scm mysch.sch
;;
;; The schematic in "mysch.sch" will be printed to the file "mysch.ps"
;;
;; Notes:
;;
;;   1. If the -o option is not used then the output is written to the
;;      file ./output.pdf
;;
;;   2. For postscript output the -o option must be used with a filename
;;      with the ".ps" file extension as shown in the example above.
;;
;;   3. Currently, "portrait" orientation is ignored for pdf outputs. If
;;      you require a "portrait" pdf output please use gaf export.
;;
;;   4. Currently, outputs for .sym files cannot be generated using this
;;      script, but .sym outputs can be generated from within the gschem
;;      gui.
;;
;;   5. Uncomment keywords below to override defaults when printing from
;;      the command line.
;;

;(output-capstyle "square")
;(output-capstyle "round")
;(output-capstyle "butt")

;(output-color "enabled")
;(output-color "disable")

;(output-extents "extents")
;(output-extents "extents no margins")

;(output-orientation "landscape")
;(output-orientation "portrait")

;; You need to call gschem-use-rc-values if something above was uncommented
;; that is also not the current setting. The current settings can vary by
;; installation, user or even project depending on the contents of gschemrc,
;; gafrc and other configuration files.
;(gschem-use-rc-values)

;; The gschem-output-type is determined from the filename specified on the
;; command line. If no filename was specified then the type will be "pdf".
(if (string=? (gschem-output-type) "ps")
    (gschem-postscript "output.ps")
    (gschem-pdf "output.pdf"))

(gschem-exit)
