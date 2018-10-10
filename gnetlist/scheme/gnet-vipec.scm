; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;
;;; Copyright (C) 1998-2015 Ales Hvezda
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

(define vipec:analysis-templates
   (list
      (cons
         (cons "VIPEC" " ")
         (list
            (list "value" "R=" #t)))
))

(define vipec:component-templates
   (list
      (cons
         (cons "RESISTOR" "RES")
         (list
            (list "value" "R=" #t "use value attrib for resistance")))
      (cons
         (cons "INDUCTOR" "IND")
         (list
            (list "value" "L=" #t "use value attrib for inductance")
            (list "Q" "Q=" #f)))
      (cons
         (cons "CAPACITOR" "CAP")
         (list
            (list "value" "C=" #t "use value attrib for capacitance")))
      (cons
         (cons "TLIN" "TLIN")
         (list
            (list "Z" "Z=" #t 50)
            (list "length" "E=" #t "length attrib for length")
            (list "F" "F=" #t "F attrib for frequency")))
      (cons
         (cons "CLIN" "CLIN")
         (list
            (list "ZE" "ZE=" #t)
            (list "ZO" "ZO=" #t)
            (list "E" "E=" #t)
            (list "F" "F=" #t)))
      (cons
         (cons "SPARAMBLOCK" "BLOCK")
         (list
            (list "filename" "" #t "filename attrib for sparams")))
))

(define vipec:get-template
   (lambda (templates device)
      (if (not (null? templates))
         (if (string=? device (car (car (car templates))))
            (car templates)
            (vipec:get-template (cdr templates) device))
         (begin
            (message "Template not found   ")
            (message device)
            (message "\n")
            (cons (cons device "error") '())))))

(define (vipec:write-net-name-of-node uref number-of-pin netnumbers)
  (do ((i 1 (1+ i)))
      ((> i number-of-pin))
    (let ((pin-name (number->string i)))
      (display (get-net-number (car (get-nets uref (gnetlist:get-attribute-by-pinseq uref pin-name "pinnumber"))) netnumbers))
      (write-char #\space))))

(define vipec:write-attribs
   (lambda (package attribs term)
      (if (not (null? attribs))
         (let ((attrib (car attribs))
               (value (get-package-attribute package (car(car attribs)))))
            (if (not (string=? value "unknown"))
               (begin
                  (display (cadr attrib))
                  (display value)
                  (display term))
               (if (and (caddr attrib)(not (null? (cdddr attrib))))
                  (begin
                     (display (cadr attrib))
                     (display (cadddr attrib))
                     (display term))))
         (vipec:write-attribs package (cdr attribs) term)))))

(define vipec:write-gen-component
   (lambda (package netnumbers)
      (let ((template (vipec:get-template vipec:component-templates (get-device package))))
         (display "\t")
         (display (cdr (car template)))
         (display "\t")
         (vipec:write-net-name-of-node package
            (length (gnetlist:get-pins package)) netnumbers)
         (vipec:write-attribs package (cdr template) "\t")
         (display (string-append "\t% " package))
         (newline))))

(define vipec:component-writing
   (lambda (ls netnumbers)
      (if (not (null? ls))
         (let ((package (car ls))
               (device (get-device (car ls))))
            (cond
               ((string=? device "VIPEC") #t)
               ((string=? device "SMITH") #t)
               ((string=? device "GRID") #t)
               (else (vipec:write-gen-component package netnumbers)))
            (vipec:component-writing (cdr ls) netnumbers)))))

(define vipec:misc-components
   (lambda (netnumbers)
      (display "\tDEF2P\t")
      (display (get-net-number "PORT1" netnumbers))
      (display "  ")
      (display (get-net-number "PORT2" netnumbers))
      (display "\n\tTERM\t50 50\n")))

(define (vipec:header)
  (display "% ViPEC RF Netlister\n")
  (display "% Written by Matthew Ettus\n")
  (display "% Based on code by Bas Gieltjes\n"))

(define vipec:analysis-block
   (lambda (packages)
      (if (not (null? packages))
         (begin
            (if (string=? (get-device (car packages)) "VIPEC")
               (let ((template (vipec:get-template vipec:analysis-templates "VIPEC")))
                  (vipec:write-attribs (car packages) (cdr template) "\n")
                  (newline)))
            (vipec:analysis-block (cdr packages))))))

(define (vipec output-filename)
  (set-current-output-port (output-port output-filename))
  (let ((netnumbers (number-nets all-unique-nets 1)))
     (vipec:header)
     (display "CKT\n")
     (vipec:component-writing packages netnumbers)
     (vipec:misc-components netnumbers)
     (newline)
     (vipec:analysis-block packages)
     )
  (close-output-port (current-output-port)))
