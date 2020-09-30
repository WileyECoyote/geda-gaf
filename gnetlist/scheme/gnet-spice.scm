; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA, <http://www.gnu.org/licenses/>.

;; --------------------------------------------------------------------------
;;
;; SPICE netlist backend written by S. Gieltjes starts here
;;

;; The following is needed to make guile 1.8.x happy.
(use-modules (ice-9 rdelim))

;; Common functions for the `spice' and `spice-sdb' backends
(load-from-path "spice-common.scm")

;;  write mos transistor
;;
(define spice:write-mos-transistor
  (lambda (package)
    (spice:write-one-component package)
            ;; create list of attributes which can be attached to a mosfet
    (let ((attrib-list (list "l" "w" "as" "ad" "pd" "ps" "nrd" "nrs" "temp" "ic")))
      (spice:write-list-of-attributes package attrib-list))
            ;; write the off attribute separately
    (let ((off-value (get-package-attribute package "off")))
      (cond ((string=? off-value "#t") (display " off"))
            ((string=? off-value "1" ) (display " off"))))
    (newline)))


;;
;; Include a file
;;
(define spice:write-include
  (lambda (package)
    (display (string-append package " " (spice:component-value package) "\n"))))


;;----------------------------------------------------------
;; Include a spice model (instantiated as a model box on the schematic)
;;  Two types of model can be included:
;;  1.  An embedded model, which is a one- or multi-line string held in the attribute "model".
;;      In this case, the following attributes are mandatory:
;;      --  model (i.e. list of parameter=value strings)
;;      --  model-name
;;      --  type
;;      In this case, the function creates and formats the correct spice model line(s).
;;  2.  A model held in a file whose name is held in the attribute "file"
;;      In this case, the following attribute are mandatory:
;;      --  file (i.e. list of parameter=value strings)
;;      In this case, the function just opens the file and dumps the contents
;;      into the netlist.
;;----------------------------------------------------------
(define spice:write-model
  (lambda (package)
             ;; Collect variables used in creating spice code
        (let ((model-name (get-package-attribute package "model-name"))
              (model-file (get-package-attribute package "file"))
              (model (get-package-attribute package "model"))
              (type (get-package-attribute package "type"))
             )   ;; end of local assignments

          ;; Now, depending upon what combination of model, model-file, and model-name
          ;; exist (as described above) write out lines into spice netlist.
          (cond
             ;; one model and model name exist
           ( (not (or (string=? model "unknown") (string=? model-name "unknown")))
             (display (string-append ".MODEL " model-name " " type " (" model ")\n")) )

             ;; model file exists
           ( (not (or (string=? model-file "unknown") ))
             (spice:insert-text-file model-file)   ;; don't write it out -- it's handled after the second pass.
           )

          )  ;; close of cond
        ) ;; close of let
    ) ;; close of lambda
) ;; close of define

;;
;; write the refdes, the net name connected to pin# and the component value. No extra attributes.
;;
(define spice:write-one-component
  (lambda (package)
    (display (string-append package " "))
        ;; write net names, slotted components not implemented
    (spice:write-net-names-on-component package)
        ;; write component value, if components have a label "value=#"
        ;; what if a component has no value label, currently unknown is written
    (display (spice:component-value package))))


;;
;; Looks for device labels to be written before the netlist body,
;; Currently, only device=model is supported.
;;
(define spice:write-prologue
  (lambda (ls)
     (if (not (null? ls))
      (let ((package (car ls)))                           ;; search for model device labels
        (if (string=? (get-device package) "model")
            (spice:write-model package))
        (spice:write-prologue (cdr ls)) ))))


;;
;; write the refdes, to the pin# connected net and component value and optional extra attributes
;; check if the component is a special spice component
;;
(define spice:write-netlist
  (lambda (ls)
     (if (not (null? ls))
      (let ((package (car ls)))                           ;; search for specific device labels
        (cond
          ( (string=? (get-device package) "SPICE-ccvs")
              (spice:write-ccvs package))
          ( (string=? (get-device package) "SPICE-cccs")
              (spice:write-cccs package))
          ( (string=? (get-device package) "SPICE-vcvs")
              (spice:write-vcvs package))
          ( (string=? (get-device package) "SPICE-vccs")
              (spice:write-vccs package))
          ( (string=? (get-device package) "SPICE-nullor")
              (spice:write-nullor package))
          ( (string=? (get-device package) "PMOS_TRANSISTOR")
              (spice:write-mos-transistor package))
          ( (string=? (get-device package) "NMOS_TRANSISTOR")
              (spice:write-mos-transistor package))
          ( (string=? (get-device package) "include")
              (spice:write-include package))
          ( (string=? (get-device package) "model")
              'nothing)                                  ;; Was handled by write-prologue
          ( else (spice:write-one-component package)
               (newline)))
        (spice:write-netlist (cdr ls)) ))))


;;
;; Spice netlist header
;;
(define (spice:write-top-header)
  (display "* Spice netlister for gnetlist\n"))


;;
;; Write the .END line
;;
(define (spice:write-bottom-footer)
  (display ".END")
  (newline))

;;
;; Spice netlist generation
;;
(define (spice output-filename)
  (set-current-output-port (output-port output-filename))
  (spice:write-top-header)
  (spice:write-prologue packages)
  (spice:write-netlist packages)
  (spice:write-bottom-footer)
  (close-output-port (current-output-port)))


;; SPICE netlist backend written by S. Gieltjes ends here
;;
;; --------------------------------------------------------------------------
