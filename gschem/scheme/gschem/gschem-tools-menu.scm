;; =================================================================
;;
;;                     gschem Tools menus
;;
;; =================================================================
;;
;; File:  gschem-tools-menu.scm  by Paul Tan
;;
;; (1) Put this file in /usr/share/gEDA/scheme/ directory.
;; (2) Add the following line (without the semi-colon) to the file
;;     system-gschemrc file, normally under /usr/share/gEDA/ folder:
;;
;     (load-from-path "gschem/gschem-tools-menu.scm")


(use-modules (geda page)
             (gschem action)
             (gschem window))

;; =================================================================
;; Define your favorite editor here
;(define tools:editor "emacs")
(define tools:editor "gedit")
;(define tools:editor "kwrite")
;(define tools:editor "xfwrite")
;(define tools:editor "notepad")

;; Define your favorite git client here
;(define tools:gitclient "git-cola")
;(define tools:gitclient "gitg")
;(define tools:gitclient "gitk")
(define tools:gitclient "git gui")

;; Define your preferred BOM backend here
;(define tools:bom "bom")
;(define tools:bom "bom2")
(define tools:bom "partslist1")
;(define tools:bom "partslist2")
;(define tools:bom "partslist3")

;; Define a spice back-end Options
(define spice-back-end "spice")
(define sdb-back-end   "spice-sdb -q -O sort_mode")
(define anise-back-end "spice-anise -q -O include_mode -O sort_mode -O embed_mode")
(define noqsi-back-end "spice-noqsi")

;; ======================== Utilities code  ========================
;; Get the current input schematic/sym filepath when called
(define (tools:ifpath) (get-active-filename))

(define (tools:ifdir)
  (substring (tools:ifpath) 0
        (string-rindex (tools:ifpath) #\/)))

(define (tools:ifname)
  (substring (tools:ifpath)
    (+ (string-rindex (tools:ifpath) separator-char) 1)))

(define (tools:ifbase)
  (substring (tools:ifname) 0
    (string-rindex (tools:ifname) #\.)))

(define (tools:ifext)
  (substring (tools:ifpath)
    (+ (string-rindex (tools:ifpath) #\.) 1)))

;; -----------------------------------------------------------------
;; This allows checking of schematic/sym file extention,
;; in case some tools need symbol file as input.
(define (tools:check-file chk-ext)
  (if (not (string-ci=? (tools:ifext) chk-ext))
    (begin
      (gschem-msg (string-append
          "Error: \n"
          " Please make sure the input file extention is ." chk-ext))
      #f)
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
)

;; -----------------------------------------------------------------
;; This allows checking of schematic/sym file has been modified.
(define (tools:check-saved)
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

;; ---------------- tools:open-editor? ------------------------------
(define (tools:open-editor? filename)
  (if (gschem-confirm (string-append
        "Generated  " filename ".\n\n"
        "Open " filename " with " tools:editor "?\n"
        "Click Cancel if you do not want to open " filename ".\n"))
    (system (string-append tools:editor " " filename))
  )
)

;; ----------------- tools:check-symbol ----------------------------
(define (tools:check-symbol)
  (let  ((fout (string-append (tools:ifname) "-gsymcheck.log")))
    (if (tools:check-file "sym")
      (if (not (equal? 0
          (status:exit-val (system (string-append "gsymcheck -vv " (tools:ifpath) " >" fout)))))
        (tools:open-editor? fout)
      )
    )
  )
)

;; ----------------- tools:sch-netlist-0 ---------------------------
;; Can call this template code, if needed form is:
;; gnetlist -g [netlist-type] -o filebase[foutext] inputfilename
;;
(define (tools:sch-netlist-0 netlist-type foutext)
  (let* ( (fout (string-append (tools:ifbase) foutext)))
    ;; Make sure the file opened in gschem is .sch instead of .sym file
    (if (tools:check-file "sch")
      (begin
        (system (string-append
            "gnetlist -g " netlist-type " -o " fout " " (tools:ifpath)))
        (tools:open-editor? fout)
))))

;; ==================================================================

;; ----------------- tools:open-editor -------------------------------
(define (tools:open-editor)
  (system (string-append tools:editor " &")))

;; ----------------- tools:run-drc ----------------------------
(define (tools:run-drc)
  (let  ((fout (string-append (tools:ifbase) "-drc.txt")))
    (if (tools:check-file "sch")
      (begin
        (system (string-append "gnetlist -g drc -o " fout " " (tools:ifpath)))
        (tools:open-editor? fout)
      )
    )
  )
)
;; ----------------- tools:run-drc2 ----------------------------
(define (tools:run-drc2)
  (let	((fout (string-append (tools:ifbase) "-drc2.txt")))
    (if (tools:check-file "sch")
      (begin
        (system (string-append "gnetlist -g drc2 -o " fout " " (tools:ifpath)))
        (tools:open-editor? fout)
      )
    )
  )
)

;; ----------------- tools:open-gitclient -----------------------------
(define (tools:open-gitclient)
  (if (tools:check-saved)
      (system (string-append tools:gitclient))
  )
)

;; ----------------- tools:run-bom ----------------------------
(define (tools:run-bom)
  (let  ((fout   (string-append (tools:ifbase) "-bom.csv")))
    (if (tools:check-file "sch")
      (begin
        (system (string-append "gnetlist -g " tools:bom " -o " fout " " (tools:ifpath)))
      )
    )
  )
)

;; ==================================================================

;; ----------------- tools:geda-netlist -------------------------------
(define (tools:geda-netlist)
    (tools:sch-netlist-0 "geda" "-geda.net"))

;; ------------------- tools:spice-netlist ----------------------------
(define (tools:spice-netlist)
    (tools:sch-netlist-0 spice-back-end ".cir"))

;; ----------------- tools:spice-sdb-netlist --------------------------
(define (tools:spice-sdb-netlist)
    (tools:sch-netlist-0 sdb-back-end ".cir"))

;; ----------------- tools:spice-anise-netlist ------------------------
(define (tools:spice-anise-netlist)
    (tools:sch-netlist-0 anise-back-end ".cir"))

;; ----------------- tools:spice-noqsi-netlist ------------------------
(define (tools:spice-noqsi-netlist)
    (tools:sch-netlist-0 noqsi-back-end ".cir"))

;; ----------------- tools:verilog-netlist ----------------------------
(define (tools:verilog-netlist)
  (tools:sch-netlist-0 "verilog" ".v"))

;; ----------------- tools:vhdl-sch-netlist ---------------------------
(define (tools:vhdl-sch-netlist)
    (tools:sch-netlist-0 "vhdl" ".vhd"))

;; ----------------- tools:gnet_hier_verilog  -----------------------
(define (tools:gnet_hier_verilog)
  (let  ((fout  (string-append (tools:ifbase) ".v")))
    (if (tools:check-file "sch")
      (begin
        (system (string-append "gnet_hier_verilog " (tools:ifpath)))
        (tools:open-editor? fout)
))))

(map-keys "T E"    "tools:open-editor")
(map-keys "T S"    "tools:check-symbol")
(map-keys "T T"    "tools-translate")
(map-keys "T V"    "tools:open-gitclient")

(map-icon "geda-text-editor" "tools:open-editor")

;; ==================================================================
(define tools:menu-items
;;
;;    menu item name             menu action             menu stock icon menu    Menu Item Tooltip
;;
  `(
    (,(N_ "_Symbols")            #f                          "geda-component"     "Tool for symbol files")
    ("SEPARATOR"                #f                     #f)
    (,(N_ "_Open Editor")         tools:open-editor          "geda-text-editor"   "Open text editor")

    (,(N_ "Run DRC 1")            tools:run-drc              "geda-check-grn"     "Launch design rule checker 1")
    (,(N_ "Run DRC 2")            tools:run-drc2             "geda-check-org"     "Launch design rule checker 2")
    (,(N_ "Version Control")      tools:open-gitclient       "git-logo"           "Launch version system")
    (,(N_ "Bill of Materials")    tools:run-bom              "gaf-bom"            "Generate a BOM")

    ("SEPARATOR"                #f                     #f)
    (,(N_ "gEDA netlist")         tools:geda-netlist         "gschem-net")
    (,(N_ "S_pice netlist")     #f                           "geda-wave"          "Generate spice netlists")
    ("SEPARATOR"                #f                     #f)
    (,(N_ "VHDL netlist")         tools:vhdl-sch-netlist     "geda-simulate")
    (,(N_ "Verilog netlist")      tools:verilog-netlist      "geda-verilog-blue")
    (,(N_ "hierarchy Verilog")    tools:gnet_hier_verilog    "geda-verilog-grn")
  )
)

;; ==================================================================
(define tools:symbol-items
;;
;;    menu item name       menu action            menu stock icon menu       Menu Item Tooltip
;;
  `(
    (,(N_ "Symbol _Translate...") tools-translate          "gtk-convert"      ,(N_ "Reset the X-Y Zero point"))
    (,(N_ "Check Symbol")         tools:check-symbol       "geda-inspect-grn" ,(N_ "Run gsymcheck"))
    ("SEPARATOR"                  #f                     #f)
    (,(N_ "Update Component")     tools-update             "gtk-refresh"      ,(N_ "Reload definition of selected component"))
  )
)

(define tools:spice-items
;;
;;    menu item name       menu action            menu stock icon menu       Menu Item Tooltip
;;
  `(
    ("Spice netlist"        tools:spice-netlist        "geda-spectrum"    "Generate spice netlists")
    ("SDB netlist"          tools:spice-sdb-netlist    "geda-wave"        "Generate spice netlists using SDB backend")
    ("Anise netlist"        tools:spice-anise-netlist  "geda-waves"       "Generate spice netlists using the Anise backend")
    ("NoSqi netlist"        tools:spice-noqsi-netlist  "geda-sinx"        "Generate spice netlists using the NoSqi backend")
  )
)

(add-menu (N_ "_Tools") tools:menu-items)
(add-menu (N_ "_Tools/_Symbols") tools:symbol-items)
(add-menu (N_ "_Tools/S_pice netlist") tools:spice-items)

;; ==================================================================
