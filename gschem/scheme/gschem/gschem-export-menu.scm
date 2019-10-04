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
             (gschem action)
             (gschem window))

;; =================================================================
(define export:geda2xml "geda2xml")

;; =================================================================
;; -----------------------------------------------------------------
;; This allows checking if document has been modified.
(define (export:check-saved)
  (if (page-dirty? (active-page))
      (let* ((response (gschem-confirm-cancel (string-append
                        "Save " (get-active-filename) " first?\n"))))
        (if (= response 1)
            (gschem-save-file)
            (if (= response 0)
              #t
              #f
            )
        )
      )
      #t
  )
)

;; ------------------------- export:xml ------------------------------
(define (export:xml)
  (if (export:check-saved)
    (let  ((fin (get-active-filename)))
      (system (string-append export:geda2xml " -f " fin " "))
    )
  )
)

;; ==================================================================
(define export:menu-items
;;
;;    menu item name       menu action            menu stock icon menu       Menu Item Tooltip
;;
  `(("_XML"               export:xml             "geda-xml"                    "Export document to XML file")
    ("SEPARATOR"                #f                     #f)
    ("_Symbol"            export-symbol          "geda-inbed"                 ,(N_ "Export embedded symbol"))
    ("_Picture"           export-picture         "gtk-orientation-portrait"   ,(N_ "Export embedded picture"))

  )
)
;(add-menu (N_ "_File")       file-menu-items)
(add-menu (N_ "_File/_Export") export:menu-items)
;; ==================================================================
;;
