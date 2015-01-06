;; =================================================================
;;
;;                     gschem Export menus
;;
;; =================================================================
;;
;; File:  gschem-export-menu.scm  by Wiley Hill
;;
;; (1) Put this file in /usr/share/gEDA/scheme/ directory.
;; (2) Add the following line (without the semi-colon) at the end of
;;     system-gschemrc file, normally under /usr/share/gEDA/ folder:
;;
;     (load-from-path "gschem/gschem-export-menu.scm")
;;


(use-modules (geda page)
             (gschem action))

;; =================================================================


;; ==================================================================
(define export:menu-items
;;
;;    menu item name       menu action            menu stock icon menu       Menu Item Tooltip
;;
  '(("_Symbol"            export-symbol          "geda-inbed"                 ,(N_ "Export embed symbol"))
    ("_Picture"           export-picture         "gtk-orientation-portrait"   ,(N_ "Export embed picture"))
    ("SEPARATOR"                #f                     #f)
  )
)
;(add-menu (N_ "_File")       file-menu-items)
(add-menu "_File/_Export" export:menu-items)
;; ==================================================================
;;

