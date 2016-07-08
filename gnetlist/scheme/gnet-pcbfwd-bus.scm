;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;;; Copyright (C) 1998-2008 Ales Hvezda
;;; Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA, <http://www.gnu.org/licenses/>.

;; PCB forward annotation script
;;
;; This is a replacement for gnet-pcbfwd.scm which PCB's File->Import
;; uses. It expands lists in netnames and pin numbers like this:
;;
;; 4-7 becomes 4,5,6,7
;; 7-4 becomes 7,6,5,4
;;
;; -then-
;;
;; sym[1,2,3] becomes sym1,sym2,sym3
;;
;; then assignments between lists of netnames to lists of pinnumbers
;; are matched up one-to-one and assignments between one netname and
;; a list of pinnumbers are matched up one-to-many.
;;
;; Likewise for pin numbers and pin labels.
;;
;; The net result of this is that you can assign a net named "nBL,A[8-2]"
;; to a pin labelled "A[0-7]" and numbered "1-4,10-7" and they'll all get
;; hooked up as appropriate.
;;
;; You can also have a pin named "GND" and numbered "1,15,18" connected
;; to net "GND" and it will connect all three pins to the one net.
;;
;; Constructive feedback welcome!
;;
;; DJ

(use-modules (ice-9 regex))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

;; This is a list of attributes which are propogated to the pcb
;; elements.  Note that refdes, value, and footprint need not be
;; listed here.
(define pcbfwd:element-attrs
  '("device"
    "manufacturer"
    "manufacturer_part_number"
    "vendor"
    "vendor_part_number"
    ))

(define (pcbfwd:quote-string s)
  (string-append "\""
		 (regexp-substitute/global #f "\"" s 'pre "\\\"" 'post)
		 "\"")
  )

(define (pcbfwd:irange n1 n2)
  (cond
   ((= n1 n2) (number->string n1))
   ((< n1 n2) (string-append (number->string n1) "," (pcbfwd:irange (+ n1 1) n2)))
   ((> n1 n2) (string-append (number->string n1) "," (pcbfwd:irange (- n1 1) n2)))
   )
  )

(define (pcbfwd:join delim s)
  (if (null? (cdr s))
      (car s)
      (string-append (car s) delim (pcbfwd:join delim (cdr s)))
      )
  )

;; Look for text like 1-4 and replace it with 1,2,3,4
(define (pcbfwd:expand-range s)
  (let ((m (string-match "([0-9]+)-([0-9]+)" s)))
    (if m
	(begin
	  (string-append
	   (match:prefix m)
	   (pcbfwd:irange (string->number (match:substring m 1))
			  (string->number (match:substring m 2)))
	   (pcbfwd:expand-range (match:suffix m))
	   )
	  )
	s
	)
    )
  )

;; Look for text like A[1,2,3] and replace it with A1,A2,A3.
(define (pcbfwd:expand-bus1 s)
  (let ((m (string-match "([^][,]+)\\[([^]]+)\\]" s)))
    (if m
	(string-append
	 (match:prefix m)
	 (pcbfwd:join "," (map (lambda (x)
		(string-append (match:substring m 1) x))
	      (map match:substring (list-matches "[^,]+" (match:substring m 2)))
	      ))
	 (pcbfwd:expand-bus1 (match:suffix m))
	 )
	s
	)
    )
  )

(define (pcbfwd:split-bus s)
  (map match:substring (list-matches "[^,]+" s))
  )

(define (pcbfwd:expand-bus s)
  (set! s (pcbfwd:expand-range s))
  (set! s (pcbfwd:expand-bus1 s))
  (set! s (pcbfwd:split-bus s))
  s
  )

(define (pcbfwd:each-pin1 net refdes pin port)
  (format port "Netlist(Add,~a,~a-~a)\n" net refdes pin)
  )

(define (pcbfwd:each-pin net pins port)
  (if (not (null? pins))
      (let* ((pin (car pins))
	     (refdes (car pin))
	     (pinnum (car (cdr pin)))
	     (netlist (pcbfwd:expand-bus net))
	     (pinlist (pcbfwd:expand-bus pinnum))
	     )

	(cond
	 ((= (length netlist) (length pinlist))
	  (map (lambda (n p) (pcbfwd:each-pin1 n refdes p port))
	       netlist pinlist)
	  )

	 ((= (length netlist) 1)
	  (map (lambda (p) (pcbfwd:each-pin1 (car netlist) refdes p port))
	       pinlist)
	  )

	 (
	  (pretty-print netlist)
	  (pretty-print pinlist)
	  (error (string-append "Bus size mismatch: "
				(number->string (length netlist))
				" vs "
				(number->string (length pinlist))
				", "
				net " vs " pinnum
				))
	  )
	 )

	(pcbfwd:each-pin net (cdr pins) port))))

(define (pcbfwd:each-net netnames port)
  (if (not (null? netnames))
      (let ((netname (car netnames)))
	(pcbfwd:each-pin netname (gnetlist:get-all-connections netname) port)
	(pcbfwd:each-net (cdr netnames) port))))

(define (pcbfwd:each-attr refdes attrs port)
  (if (not (null? attrs))
      (let ((attr (car attrs)))
	(format port "ElementSetAttr(~a,~a,~a)~%"
		(pcbfwd:quote-string refdes)
		(pcbfwd:quote-string attr)
		(pcbfwd:quote-string (gnetlist:get-package-attribute refdes attr)))
	(pcbfwd:each-attr refdes (cdr attrs) port))))

(define (pcbfwd:component_pins1 package num name port)
  (display "ChangePinName(" port)
  (display (pcbfwd:quote-string package) port)
  (display ", " port)
  (display num port)
  (display ", " port)
  (display (pcbfwd:quote-string name) port)
  (display ")\n" port)
  )

;; write out the pins for a particular component
(define pcbfwd:component_pins
  (lambda (port package pins)
    (if (and (not (null? package)) (not (null? pins)))
	(begin
	  (let (
		(pin (car pins))
		(label #f)
		(pinnum #f)
		(numlist #f)
		(labellist #f)
		)
	    (set! pinnum (gnetlist:get-attribute-by-pinnumber package pin "pinnumber"))
	    (set! label (gnetlist:get-attribute-by-pinnumber package pin "pinlabel"))
	    (if (string=? label "unknown")
		(set! label pinnum)
		)

	    (set! numlist (pcbfwd:expand-bus pinnum))
	    (set! labellist (pcbfwd:expand-bus label))

	    (cond
	     ((= (length numlist) (length labellist))
	      (map (lambda (n l) (pcbfwd:component_pins1 package n l port))
		   numlist labellist)
	      )

	     ((= 1 (length labellist))
	      (map (lambda (l) (pcbfwd:component_pins1 package (car numlist) l port))
		   labellist)
	      )
	     )
	    )

	  (pcbfwd:component_pins port package (cdr pins))
	  )
	)
    )
  )

(define (pcbfwd:each-element elements port)
  (if (not (null? elements))
      (let* ((refdes (car elements))
	     (value (gnetlist:get-package-attribute refdes "value"))
	     (footprint (gnetlist:get-package-attribute refdes "footprint"))
	     )

	(format port "ElementList(Need,~a,~a,~a)~%"
		(pcbfwd:quote-string refdes)
		(pcbfwd:quote-string footprint)
		(pcbfwd:quote-string value))
	(pcbfwd:each-attr refdes pcbfwd:element-attrs port)
	(pcbfwd:component_pins port refdes (gnetlist:get-pins refdes))

	(pcbfwd:each-element (cdr elements) port))))

(define (pcbfwd-bus output-filename)
  (let ((port (open-output-file output-filename)))
    (format port "Netlist(Freeze)\n")
    (format port "Netlist(Clear)\n")
    (pcbfwd:each-net (gnetlist:get-all-unique-nets "dummy") port)
    (format port "Netlist(Sort)\n")
    (format port "Netlist(Thaw)\n")
    (format port "ElementList(Start)\n")
    (pcbfwd:each-element packages port)
    (format port "ElementList(Done)\n")
    (close-output-port port)))

