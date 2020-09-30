; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
;;;
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
;; DRC backend written by Matt Ettus starts here
;;

;; DRC rules format:  (list (part rules) (net rules) (pin rules))
;; Part rules:  List of predicates of one variable, uref
;; Net rules:  List of predicates of one variable, net name
;; Pin Rules:  List of predicates of 2 variables, uref and pin number

(define drc:parseconfig
  (lambda (port)
    (let ((read-from-file (read port)))
      (if (not (eof-object? read-from-file))
          (cons (symbol->string read-from-file) (drc:parseconfig port))
          '()))))

(define drc:attriblist
  ((lambda (filename)
     (if (file-exists? filename)
       (drc:parseconfig (open-input-file filename))
       (let ((msg (string-append "ERROR: Attribute file '" filename "' not found.\n")))
        (message msg)
        (close-output-port (current-output-port))
        (primitive-exit 1)
       )
     )
   )
   "attribs"))

(define (drc output-filename)
  (set-current-output-port (output-port output-filename))
  (drc:device-rules drc:attriblist packages)
  (drc:net-rules all-unique-nets)
  (drc:pin-rules packages)
  (close-output-port (current-output-port)))


(define drc:net-rules
  (lambda(nets)
    (cond
      ((null? nets) #t)
      ((null? (get-all-connections (car nets)))
          (begin
            (display "Net ")
            (display (car nets))
            (display " has no connected pins\n")
            (drc:net-rules (cdr nets))
            #f))
      ((null? (cdr (get-all-connections (car nets))))
          (begin
            (display "Net ")
            (display (car nets))
            (display " has only 1 connected pin\n")
            (drc:net-rules (cdr nets))
            #f))
      (#t (drc:net-rules (cdr nets))))))

(define drc:pin-rules
  (lambda(packages)
    #t))

;; Probably belongs in bom-common.scm
(define (drc:no-bom-package? package)
    (string=? "1" (get-package-attribute package "nobom")))

(define drc:device-rules
  (lambda (attriblist packages)
    (if (not (null? packages))
      (let ((package (car packages)))
        (if (not (drc:no-bom-package? package))
          (drc:has-attributes? attriblist package))
        (drc:device-rules attriblist (cdr packages))
      ))))

(define drc:has-attributes?
  (lambda (attriblist uref)
    (if (not (null? attriblist))
      (begin
        (if (string=? "unknown" (get-package-attribute uref (car attriblist)))
          (begin
            (display uref)
            (display " Does not have attribute: ")
            (display (car attriblist))
            (newline)))
        (drc:has-attributes? (cdr attriblist) uref)))))


;;
;; DRC backend written by Matt Ettus ends here
;;
;; --------------------------------------------------------------------------
