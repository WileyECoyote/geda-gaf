; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;
;;; Copyright (C) 2006-2015 John P. Doty
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

;;  Calay format (modified from Ales's gnet-PCB.scm by jpd)
;;  Netname translation cleaned up at Dan McMahill'suggestion -jpd

(define (calay:display-connections nets)
  (let ((k ""))
    (for-each (lambda (in-string)
                (set! k (string-append k in-string)))
              (map (lambda (net)
                     (string-append " " (car net) "(" (car (cdr net)) ")"))
                   nets))
    (string-append k ";\n")))

;;
;; Wrap a string into lines no longer than wrap-length
;; (from Stefan Petersen)
;; (Modified for Calay format by jpd)
(define (calay:wrap string-to-wrap wrap-length)
  (if (> wrap-length (string-length string-to-wrap))
      string-to-wrap ; Last snippet of string
      (let ((pos (string-rindex string-to-wrap #\space 0 wrap-length)))
        (cond ((not pos)
               (display "Could not wrap string at requested position\n")
               " Wrap error!")
              (else
               (string-append
                (substring string-to-wrap 0 pos)
                ",\n          "
                (calay:wrap (substring string-to-wrap (+ pos 1)) wrap-length)))))))

;; Translate netnames
;; For the nonce, this just turns "_" into "-"
;;
(define (calay:translate string-to-translate)
  (let ((pos (string-index string-to-translate #\_)))
    (if pos (calay:translate (string-append (substring string-to-translate 0
    pos) "-" (substring string-to-translate (+ 1 pos)))) string-to-translate)))

(define (calay:write-net netnames)
  (if (not (null? netnames))
      (let ((netname (car netnames)))
        (display "/")
        (display (netlist:alias-net netname))
        (display "\t")
        (display (calay:wrap (calay:display-connections
          (get-all-connections netname)) 66))
        (calay:write-net (cdr netnames)))))


(define (calay output-filename)
  (set-current-output-port (output-port output-filename))
  (netlist:build-net-aliases calay:translate netlist:all-unique-nets)
  (calay:write-net netlist:all-unique-nets)
  (close-output-port (current-output-port)))
