;; =================================================================
;;
;;                     gschem Tools menus
;;
;; =================================================================
;;
;; File:  gsch-tools-menu.scm  by Paul Tan
;;
;; (1) Put this file in /usr/share/gEDA/scheme/ directory.
;; (2) Add the following line (without the semi-colon) at the end of
;;     system-gschemrc file, normally under /usr/share/gEDA/ folder:
;;
;     (load-from-path "gschem-tools-menu.scm")
;;


(use-modules (geda page)
             (gschem action))

;; =================================================================
;; Define your favorite editor here
;(define tools:editor "emacs")
;(define tools:editor "kwrite")
(define tools:editor "gedit")

;; ======================== Utilities code  ========================
;; Get the current input schematic/sym filepath when called
(define (tools:ifpath) (get-selected-filename))

(define (tools:ifdir)
  (substring (tools:ifpath) 0
        (string-rindex (tools:ifpath) #\/)))

(define (tools:ifname)
  (substring (tools:ifpath)
    (+ (string-rindex (tools:ifpath) #\/) 1)))

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
                        "Save " (get-selected-filename) " first?\n"))))
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

;; ---------------- tools:open-editor? ------------------------------
(define (tools:open-editor? filename)
  (if (gschem-confirm (string-append
        "Generated  " filename ".\n\n"
        "Open " filename " with " tools:editor "?\n"
        "Click Cancel if you do not want to open " filename ".\n"))
    (system (string-append tools:editor " " filename))
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

;; ----------------- tools:run-drc2 ----------------------------
(define (tools:run-drc2)
  (let	((fout   (string-append (tools:ifbase) "_drc2.txt")))
    (if (tools:check-file "sch")
      (begin
        (system (string-append "gnetlist -g drc2 -o " fout " " (tools:ifpath)))
        (tools:open-editor? fout)
      )
    )
  )
)

;; ----------------- tools:geda-netlist -------------------------------
(define (tools:geda-netlist)
    (tools:sch-netlist-0 "geda" ".geda"))

;; ----------------- tools:spice-sdb-netlist --------------------------
(define (tools:spice-sdb-netlist)
    (tools:sch-netlist-0 "spice-sdb" ".cir"))

;; ----------------- tools:spice-noqsi-netlist ------------------------
(define (tools:spice-noqsi-netlist)
    (tools:sch-netlist-0 "spice-noqsi" ".cir"))

;; ----------------- tools:verilog-netlist ----------------------------
(define (tools:verilog-netlist)
  (tools:sch-netlist-0 "verilog" ".v"))

;; ----------------- tools:vhdl-sch-netlist ---------------------------
(define (tools:vhdl-sch-netlist)
    (tools:sch-netlist-0 "vhdl" ".vhd"))

;; ----------------- tools:gnet_hier_verilog  -----------------------
(define (tools:gnet_hier_verilog)
  (let  ((fout  (string-append (tools:ifbase) ".v")))
    (if (tools:check-ext "sch")
      (begin
        (system (string-append "gnet_hier_verilog " (tools:ifpath)))
        (tools:open-editor? fout)
))))
;; ==================================================================
(define tools:menu-items
;;
;;    menu item name       menu action             menu stock icon menu       Menu Item Tooltip
;;
  '(("SEPARATOR"                #f                     #f)
    ("_Open Editor"         tools:open-editor          "geda-text-editor"   "Open text editor")
    ("Run DRC2"             tools:run-drc2             #f)
    ("SEPARATOR"                #f                     #f)
    ("gEDA netlist"         tools:geda-netlist         "gschem-net")
    ("Spice-sdb netlist"    tools:spice-sdb-netlist    #f)
    ("Spice-noqsi netlist"  tools:spice-noqsi-netlist  #f)
    ("SEPARATOR"                #f                     #f)
    ("Verilog netlist"      tools:verilog-netlist      #f)
    ("VHDL netlist"         tools:vhdl-sch-netlist     #f)
    ("SEPARATOR"                #f                     #f)
    ("gnet_hier_verilog"    tools:gnet_hier_verilog    #f)
  )
)

(add-menu "_Tools" tools:menu-items)
;; ==================================================================
;;

