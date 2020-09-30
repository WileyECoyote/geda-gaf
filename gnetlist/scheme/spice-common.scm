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
;;
;; --------------------------------------------------------------------------
;; Common functions for the SPICE netlist backends `spice' and `spice-sdb'.
;; By S. Gieltjes and others.
;; --------------------------------------------------------------------------

;;--------------------------------------------------------------------------
;; Given a filename, open the file, get the contents, and dump the contents
;; into the current outpout port, aka spice file.
;; Calling form is "(insert-text-file filename)"
;; The function opens filename, but assumes that output-file is stdout
;;
;; This function can be to include spice models referenced in schematic files
;; into the netlist.  Note that it doesn't check the correctness of the spice
;; code in the file -- you're on your own!
;;---------------------------------------------------------------------------
(define spice:insert-text-file
  (lambda (filename)
    (if (file-exists? filename)
    (let ((model-file (open-input-file filename)) )
      (display (string-append "*vvvvvvvv  Included SPICE from " filename " vvvvvvvv\n"))
      (let while ((model-line (read-line model-file)))
          (if (not (eof-object? model-line))
                   (begin
                     (display (string-append model-line "\n"))
                     (while (read-line model-file))
                   )  ;; end of inner begin
          ) ;; end of if
        )  ;; end of inner let
        (close-port model-file)
        (display (string-append "*^^^^^^^^  End of included SPICE from " filename " ^^^^^^^^\n*\n"))
     ) ;; end of outer let
    (begin
      (message (string-append "ERROR: File '" filename "' not found.\n"))
      (primitive-exit 1))
    )
  )
)

;;--------------------------------------------------------------------------
;; handle-spice-file:  This wraps insert-text-file.
;; Calling form: (handle-spice-file file-name)
;; The wrapper check if "include_mode" was passed to the backend using the
;; -O option. If found, insert-text-file calls to stick the file's contents
;; into the SPICE netlist, otherwise it just writes a .INCLUDE card with the
;; file name.
;;--------------------------------------------------------------------------
(define spice:handle-spice-file
  (lambda (file-name)
    (debug-spew (string-append "Handling spice model file " file-name "\n"))
    (if (member "include_mode" (get-backend-arguments))
        (display (string-append ".INCLUDE " file-name "\n"))   ;; flag found: just print out .INCLUDE card
        (spice:insert-text-file file-name)                     ;; flag not found: invoke insert-text-file
    ) ;; endif was a member
) )


;;---------------------------------------------------------------------
;; write netnames connected to pin-a and pin-b
;;   (currently used by the controlled sources (e, g, f and h)
;;---------------------------------------------------------------------
(define spice:write-two-pin-names
  (lambda (package pin-a pin-b)
    (display (string-append
      (car (spice:get-net package (gnetlist:get-attribute-by-pinseq package pin-a "pinnumber"))) " "))
    (display (string-append
      (car (spice:get-net package (gnetlist:get-attribute-by-pinseq package pin-b "pinnumber"))) " "))))


;;--------------------------------------------------------------------
;; Given a refdes returns the device associated nets(s) ordered by
;; their pin number,
;; what when not defined?
;;      problem is slotted components e.g. ../examples/singlenet_1.sch
;;--------------------------------------------------------------------
(define (spice:write-net-names-on-component refdes)
  (do ((i 1 (1+ i)))
      ((> i  (length (gnetlist:get-pins refdes))))
    (let ((pin-name (number->string i)))
      (display (car (spice:get-net refdes (gnetlist:get-attribute-by-pinseq refdes pin-name "pinnumber"))))
      (write-char #\space))))


;;----------------------------------------------------------------
;; write a current controlled voltage source and implement the necessary
;;   current measuring voltage source
;;----------------------------------------------------------------
(define spice:write-ccvs
  (lambda (package)
    ( begin
      (display "* begin ccvs expansion, h<name>\n")
          ;; implement the controlled current source
          ;; the user should create the refdes label begining with a h
      (display (string-append package " "))
      (spice:write-two-pin-names package "1" "2")
      (display (string-append "Vsense_" package  " " (spice:component-value package) "\n" ))
          ;; implement the current measuring voltage source
      (display (string-append "Vsense_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
          ;; now it is possible to leave the output voltage source unconnected
          ;; i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " "))
      (spice:write-two-pin-names package "1" "2")
      (display "dc 0\n")
      (display "* end ccvs expansion\n"))))


;;-----------------------------------------------------------------------
;; write a current controlled current source and implement the necessary
;;   current measuring voltage source
;;-----------------------------------------------------------------------
(define spice:write-cccs
  (lambda (package)
    ( begin
      (display "* begin cccs expansion, f<name>\n")
          ;; implement the controlled current source
          ;; the user should create the refdes label begining with a f
      (display (string-append package " "))
      (spice:write-two-pin-names package "1" "2")
      (display (string-append "Vsense_" package " " (get-package-attribute package "value") "\n" ))
          ;; implement the current measuring voltage source
      (display (string-append "Vsense_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
      (display "* end cccs expansion\n"))))


;;-------------------------------------------------------------------------
;; write a voltage controlled current source and implement the necessary
;;   voltage measuring current source
;;-------------------------------------------------------------------------
(define spice:write-vccs
  (lambda (package)
    ( begin
      (display "* begin vccs expansion, g<name>\n")
          ;; implement the controlled current source
          ;; the user should create a refdes label beginning with a g
      (display (string-append package " "))
      (spice:write-net-names-on-component package)
      (display (string-append (spice:component-value package) "\n"))
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "IMeasure_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
      (display "* end vccs expansion\n"))))


;;------------------------------------------------------------------------
;; write a voltage controlled voltage source and implement the necessary
;;   voltage measuring current source
;;------------------------------------------------------------------------
(define spice:write-vcvs
  (lambda (package)
    ( begin
      (display "* begin vcvs expansion, e<name>\n")
          ;; implement the controlled voltage source
          ;; the user should create a refdes label beginning with an e
      (display (string-append package " "))
      (spice:write-net-names-on-component package)
      (display (string-append (get-package-attribute package "value") "\n" ))
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "Isense_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
          ;; with an output current source it is possible to leave the output voltage source
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " "))
      (spice:write-two-pin-names package "1" "2")
      (display "dc 0\n")
      (display "* end vcvs expansion\n"))))


;;--------------------------------------------------------------------------
;; Create a nullor, make sure it consists of a voltage controlled source
;;--------------------------------------------------------------------------
(define spice:write-nullor
  (lambda (package)
    (let ((value (get-package-attribute package "value")))
      (display "* begin nullor expansion, e<name>\n")
          ;; implement the controlled voltage source
      (display (string-append "E_" package " "))
      (spice:write-net-names-on-component package)
      (display (string-append (if (string=? value "unknown") "1000Meg" value) "\n"))
          ;; implement the voltage measuring current source
          ;; imagine yourself copying the voltage of a voltage source with an internal
          ;; impedance, spice starts complaining about unconnected nets if this current
          ;; source is not here.
      (display (string-append "IMeasure_" package " "))
      (spice:write-two-pin-names package "3" "4")
      (display "dc 0\n")
          ;; with an output current source it is possible to leave the output voltage source
          ;; unconnected i.e. spice won't complain about unconnected nodes
      (display (string-append "IOut_" package " "))
      (spice:write-two-pin-names package "1" "2")
      (display "dc 0\n")
      (display "* end of nullor expansion\n"))))


;;-------------------------------------------------------------------
;; write all listed and available attributes in the form of <variable>=<value>
;;-------------------------------------------------------------------
(define spice:write-list-of-attributes
  (lambda (package attrib-list)
    (if (not (null? attrib-list))
      (let ((attrib (get-package-attribute package (car attrib-list))))
            ; Is it possible to make no differentiation between upper and lower case?
            ; That relieves you of mixed case forms e.g. As, AS, as..., they are the
            ; same attributes, spice3f5 is case insensitive.  And other spice versions?
        (if (not (string=? attrib "unknown"))
          (display (string-append  " " (car attrib-list) "=" attrib)))
        (spice:write-list-of-attributes package (cdr attrib-list))))))


;;-----------------------------------------------------------
;; Given a refdes, returns the device attribute "value" as string
;; Used when "value" is a mandatory attribute.
;; Returns "<no valid attribute . . .>" if not available.
;;-----------------------------------------------------------
(define spice:component-value
  (lambda (package)
    (let ((value (get-package-attribute package "value")))
      (if (not (string=? value "unknown"))
        value
        "<No valid value attribute found>"))))

;;-----------------------------------------------------------
;; gnet-spice replacement of get-nets, a net labeled "GND" becomes 0
;;-----------------------------------------------------------
(define spice:get-net
  (lambda (refdes pin-name)
    (let ((net-name (get-nets refdes pin-name)))
      (cond ((string=? (car net-name) "GND") (cons "0" #t))
            (else                            (cons (car net-name) #t))))))
