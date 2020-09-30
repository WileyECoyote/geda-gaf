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

;; PADS netlist format

;; This procedure takes a net name as determined by gnetlist and
;; modifies it to be a valid pads net name.
;;
(define pads:map-net-names
  (lambda (net-name)
    (let ((net-alias net-name)
          )
      ;; Convert to all upper case because Pads seems
      ;; to do that internally anyway and we'd rather do
      ;; it here to catch shorts created by not preserving
      ;; case.  Plus we can eliminate lots of ECO changes
      ;; that will show up during backannotation.
      (string-upcase net-alias)
      )
    )
  )

;; This procedure takes a refdes as determined by gnetlist and
;; modifies it to be a valid pads refdes.
;;
(define pads:map-refdes
  (lambda (refdes)
    (let ((refdes-alias refdes)
          )
      ;; Convert to all upper case because Pads seems
      ;; to do that internally anyway and we'd rather do
      ;; it here to catch name clashes created by not preserving
      ;; case.
      (string-upcase refdes-alias)
      )
    )
  )

(define pads:components
   (lambda (packages)
      (if (not (null? packages))
         (begin
            (let ((pattern (get-package-attribute (car packages)
                                                           "pattern"))
            ;; The above pattern should stay as "pattern" and not "footprint"
                  (package (car packages)))
               (if (not (string=? pattern "unknown"))
                  (display pattern))

               ;; print out the refdes with aliasing
               (display (netlist:alias-refdes package))

               (write-char #\tab)
               (display (get-package-attribute package "footprint"))
               (display "\r\n"))
            (pads:components (cdr packages))))))

(define (pads:display-connections nets)
  (let ((k ""))
    (for-each (lambda (in-string)
                (set! k (string-append k in-string)))
              (map (lambda (net)
                     (string-append " " (netlist:alias-refdes (car net)) "." (car (cdr net))))
                   nets))
    (string-append k "\r\n")))


; This function is replaced with the above one. Due to non existent
; verification, this function is left commented out.
; /spe, 2002-01-08
;(define (pads:display-connections nets)
;  (if (not (null? nets))
;      (string-append " " (car (car nets)) "." (car (cdr (car nets)))
;       (pads:display-connections (cdr nets)))
;      "\r\n"))



(define pads:write-net
   (lambda (netnames)
      (if (not (null? netnames))
         (let ((netname (car netnames)))
            (display "*SIGNAL* ")
            (display (netlist:alias-net netname))
            (display "\r\n")
            (display (wrap
                      (pads:display-connections
                       (get-all-connections netname))
                      78
                      "")
                    )
            (pads:write-net (cdr netnames))))))

(define (pads output-filename)
  (set-current-output-port (output-port output-filename))
  ;; initialize the net-name aliasing
  (netlist:build-net-aliases pads:map-net-names all-unique-nets)

  ;; initialize the refdes aliasing
  (netlist:build-refdes-aliases pads:map-refdes packages)

  ;; print out the header
  (display "!PADS-POWERPCB-V3.0-MILS!\r\n")
  (display "\r\n*PART*\r\n")

  ;; print out the parts
  (pads:components packages)

  ;; print out the net information
  (display "\r\n*NET*\r\n")
  (pads:write-net all-unique-nets)

  ;; print out the footer
  (display "\r\n*END*\r\n")
  (close-output-port (current-output-port)))
