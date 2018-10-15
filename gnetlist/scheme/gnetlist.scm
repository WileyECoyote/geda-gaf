; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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

(use-modules (srfi srfi-1)
             (geda deprecated)
             (geda log))

;; The real problem is that this module has the same name as gnetlist.c
;; so I do not know how to modularize, so here is my feeble attempt to
;; hack-around:
(define-public get-packages                gnetlist:get-packages)
(define-public get-backend-arguments       gnetlist:get-backend-arguments)
(define-public get-non-unique-packages     gnetlist:get-non-unique-packages)
(define-public get-pins                    gnetlist:get-pins)
(define-public get-all-nets                gnetlist:get-all-nets)
(define-public get-all-unique-nets         gnetlist:get-all-unique-nets)
(define-public get-all-connections         gnetlist:get-all-connections)
(define-public get-nets                    gnetlist:get-nets)
(define-public get-pins-nets               gnetlist:get-pins-nets)
(define-public get-all-package-attributes  gnetlist:get-all-package-attributes)
(define-public get-toplevel-attribute      gnetlist:get-toplevel-attribute)
(define-public get-renamed-nets            gnetlist:get-renamed-nets)
(define-public get-attribute-by-pinseq     gnetlist:get-attribute-by-pinseq)
(define-public get-attribute-by-pinnumber  gnetlist:get-attribute-by-pinnumber)
(define-public vams-get-package-attributes gnetlist:vams-get-package-attributes)
(define-public graphical-net-objs-attrib   gnetlist:graphical-net-objs-attrib)
(define-public get-input-files             gnetlist:get-input-files)
(define-public get-verbosity               gnetlist:get-verbosity)
(define-public get-version                 gnetlist:get-version)

;;----------------------------------------------------------------------
;; Functions to support processing command-line arguments.
;;----------------------------------------------------------------------

;;---------------------------------------------------------------
;;  debug-spew
;;  Wrapper which spews debug messages if -v flag is set, otherwise
;;  does nothing.
;;  Calling form:  (debug-spew "verbose debug text")
;;--------------------------------------------------------------
(define-public debug-spew
  (lambda (debug-string)
    (if (= 1 (gnetlist:get-verbosity))
        (message debug-string)
)))

;; Yields a list of lists of command line flags and values. Each flag
;; must be known to the gnetlist front end. For example, the "--nomunge"
;; flag will yield ("nomunge_mode" #t).
(define-public (gnetlist:get-calling-flags)
  "Returns a list of `-O' arguments in the form:

  ((ARGUMENT #t) ...)"
  (map (lambda (x) (list x #t)) (gnetlist:get-backend-arguments))
)

;;---------------------------------------------------------------
;; calling-flag?
;;   Returns #t or #f depending upon the corresponding flag
;;   was set in the calling flags given to gnetlist.
;;   9.7.2003 -- SDB.
;;---------------------------------------------------------------
(define-public calling-flag?
  (lambda (searched-4-flag calling-flag-list)

    (if (null? calling-flag-list)
          '#f                                             ;; return #f if null list -- sort_mode not found.
          (let* ((calling-pair (car calling-flag-list))   ;; otherwise look for sort_mode in remainder of list.
                 (calling-flag (car calling-pair))
                 (flag-value   (cadr calling-pair)))

            ;; (display (string-append "examining calling-flag = " calling-flag "\n" ))
            ;; (display (string-append "flag-value = " (if flag-value "true" "false") "\n" ))

            (if (string=? calling-flag searched-4-flag)
                flag-value                                                 ;; return flag-value if sort_mode found
                (calling-flag? searched-4-flag (cdr calling-flag-list))    ;; otherwise recurse until sort_mode is found
            )  ;; end if
          )  ;; end of let*
     )  ;; end of if (null?
))

(define-public (error-no-input-file)
  (display "ERROR: No schematics files specified for processing.\n")
  (display "Run `gnetlist --help' for more information.\n")
)

(define-public have-input-file?
 (if (= 0 (length (get-input-files))) #f #t)
)

(define-public help-flag?
  (calling-flag? "help" (gnetlist:get-calling-flags))
)

(define-public version-flag?
  (calling-flag? "version" (gnetlist:get-calling-flags))
)

;;-------------  End of command line flag functions ----------------

;; Support functions

;;  This fcn should behave exactly the same as C's strncmp fcn.
;;  It compares two strings from the start up to a user-defined end
;;  char count.  It also checks that the string compare was successful through
;;  the end char count (i.e. that both strings are >= "end").  This
;;  guards against returning #t when comparing "unconnected_pin-23" to "unc"
;;  (over 15 chars).
;;  I needed to write this because substring chokes when the string arg is
;;  shorter than the end arg.
;;  1.4.2006 -- SDB.
(define-public strncmp?
  (lambda (string1 string2 end)
    (and
     (string-ci=? (substring string1 0 (min end (string-length string1)))
		  (substring string2 0 (min end (string-length string2))))
     (>= (min (string-length string1) (string-length string2)) end)
    )
  )
)

;;  This fcn returns the first len characters of the string str.  If
;;  str has less than len characters, it returns the whole string
;;  (but doesn't choke)
(define-public safe-string-head
  (lambda (str len)
    (substring str 0 (min len (string-length str)))
  )
)

;;; Default resolver: Returns the first valid (non-#F) value from
;;; VALUES, or #F, if there is no valid attribute value. If any
;;; other valid value in the list is different, yields a warning
;;; reporting REFDES of affected symbol instances and attribute
;;; NAME.
(define-public (unique-attribute refdes name values)
  (let ((values (filter-map identity values)))
    (and (not (null? values))
         (let ((value (car values)))
           (or (every (lambda (x) (equal? x value)) values)
               (format (current-error-port) "\
Possible attribute conflict for refdes: ~A
name: ~A
values: ~A
" refdes name values))
      value))))

(define-public (gnetlist:get-package-attribute refdes name)
  "Return the value associated with attribute NAME on package
identified by REFDES.

It actually computes a single value from the full list of values
produced by 'get-all-package-attributes' as that list is
passed through 'unique-attribute'.

The default behavior is to return the value associated with the
first symbol instance for REFDES having the attribute NAME. If
some of the instances of REFDES have different value for NAME, it
prints a warning."
  (let* ((values (gnetlist:get-all-package-attributes refdes name))
         (value (unique-attribute refdes name values)))
    (or value "unknown")))

(define (gnetlist:get-slots refdes)
  "Return a sorted list of slots used by package REFDES.

get-slots collects the slot attribute values of each symbol instance
of REFDES. As a result, slots may be repeated in the returned list."
  (sort-list!
   (filter-map
    (lambda (slot)
      (if slot
          ;; convert string attribute value to number
          (or (string->number slot)
              ;; conversion failed, invalid slot, ignore value
              (begin
                (format (current-error-port)
                        "Uref ~a: Bad slot number: ~a.\n" refdes slot)
                #f))
          ;; no slot attribute, assume slot number is 1
          1))
    (gnetlist:get-all-package-attributes refdes "slot"))
   <))

(define-public (gnetlist:get-unique-slots refdes)
  "Return a sorted list of unique slots used by package REFDES."
  (delete-duplicates! (gnetlist:get-slots refdes)))

;;
;; Given a uref, returns the device attribute value (unknown if not defined)
;;
(define get-device
   (lambda (package)
      (get-package-attribute package "device")))

;; Shorthand for get component values
(define get-value
   (lambda (package)
      (get-package-attribute package "value")))

(define get-component-text
   (lambda (package)
      (let ((value (get-package-attribute package "value"))
            (label (get-package-attribute package "label"))
            (device (get-package-attribute package "device")))
         (if (not (string=? "unknown" value))
            value
            (if (not (string=? "unknown" label))
               label
               device)))))


;; return all pins for a particular package
(define pins
   (lambda (package)
      (gnetlist:get-pins package)))

;; this is really crude, but I'm tired... :)
(define display-nl
   (lambda (list)
      (display list)
      (newline)))


;; ah.. wonder what use this is...
(define display-pin
   (lambda (pin-list)
      (for-each display-nl pin-list)))


;; ha. I'm playing with scheme here.. don't mind me
(define display-all-pins
   (lambda ()
      (for-each display-pin all-pins)))


;; another misc function
(define print-packages
   (lambda (plist)
      (for-each display-nl plist)))

;; ETTUS
;; find-device
;; Usage:  (find-device packages devicename)
;; Returns the first package which matches the devicename
(define-public find-device
   (lambda (components devicename)
      (if (not (null? components))
         (if (string=? devicename (get-device (car components)))
            (car components)
            (find-device (cdr components) devicename)))))


;; ETTUS
;; find-devices
;; Usage:  (find-devices packages devicename '())
;; Returns a list of packages which match the device name
(define find-devices
   (lambda (components devicename list)
      (if (not (null? components))
         (if (string=? devicename (get-device (car components)))
            (find-devices (cdr components)
                                devicename
                                (cons (car components) list))
            (find-devices (cdr components)
                                devicename
                                list))
         list)))

;; ETTUS
;; contains?
;; Usage (contains? list item)
;; True if the list contains the item, according to string=?
(define-public contains?
   (lambda (ls item)
      (cond
         ((null? ls) #f)
         ((string=? item (car ls)) #t)
         (#t (contains? (cdr ls) item)))))

;; ETTUS
;; Usage: (number-nets all-unique-nets 1)
;; Returns a list of pairs of form (netname . number)
(define-public (number-nets nets number)
  (define (number-nets-impl in i out)
    (if (null? in)
        (reverse! out) ; Return value
        (let ((netname (car in)))
          (if (string=? "GND" netname)
              (number-nets-impl (cdr in) i (cons (cons netname 0) out))
              (number-nets-impl (cdr in) (1+ i) (cons (cons netname i) out))))))
  (number-nets-impl nets number '()))

;; ETTUS
;; Usage: (get-net-number netname numberlist)
;; numberlist should be from (number-nets) above
;; Returns the number corresponding to the net
(define-public get-net-number
   (lambda (netname numberlist)
      (if (not (null? numberlist))
         (if (string=? netname (car (car numberlist)))
            (cdr (car numberlist))
            (get-net-number netname (cdr numberlist))))))

;;
;; Useful output functions contributed by Andrew Bardsley
;;
(define-public (print-to-port port . l)
    (for-each (lambda (elem) (display elem port)) l))

(define-public (print . l)
    (apply print-to-port (cons (current-output-port) l)))

;;
;; Wrap a string into lines no longer than wrap-length
;; wrap-char is put on the end-of-the-wrapped-line, before the return
;; (from Stefan Petersen)
(define-public (wrap string-to-wrap wrap-length wrap-char)
  (if (> wrap-length (string-length string-to-wrap))
      string-to-wrap ; Last snippet of string
      (let ((pos (string-rindex string-to-wrap #\space 0 wrap-length)))
	(cond ((not pos)
	       (display "Could not wrap string at requested position\n")
	       " Wrap error!")
	      (else
	       (string-append
		(substring string-to-wrap 0 pos)
		wrap-char
		"\n "
		(wrap (substring string-to-wrap (+ pos 1)) wrap-length wrap-char)))))))

;; example use
; (define (run-test test-string wrap-len)
;   (display (string-append "Wrapping \"" test-string "\" into "))
;   (display wrap-len)
;   (newline)
;   (display (wrap test-string wrap-len " \\"))
;   (newline)
;   (newline))

; (run-test "one two three four five six seven eight nine ten" 5)
; (run-test "one two three four five six seven eight nine ten" 10)
; (run-test "one two three four five six seven eight nine ten" 20)

;; determine the uref to use for a particular OBJECT
(define (gnetlist:get-uref object)
  ; Returns first value of first attrib found with given name, or #f.
  (define (attrib-first-value object name)
    (let ((attrib-lst (get-attrib-value-by-attrib-name object name)))
      (if (null? attrib-lst) #f (car attrib-lst))))
  ; Handler if we find uref=
  (define (handle-uref value)
    (simple-format (current-output-port)
                   "WARNING: Found uref=~A" value)
    (newline)
    (simple-format (current-output-port)
                   "uref= is deprecated, please use refdes=~A" value)
    (newline)
    value)

  ; Actually find attribute: check refdes, then uref, then return #f.
  (cond
   ((attrib-first-value object "refdes") => (lambda (x) x))
   ((attrib-first-value object "uref") => handle-uref)
   (else #f)))

;; define the default handler for get-uref
;;(define get-uref gnetlist:get-uref)

(define-public (gnetlist:get-command-line)
  "Return the command line used to invoke the program."
  (string-join (program-arguments)))

(define-public (stdout? output-filename)
  (string=? output-filename "-"))

;; If the output file name is "-", use stdout instead
(define-public (output-port output-filename)
  (if (stdout? output-filename)
    (current-output-port)
    (open-output-file output-filename)))

;; Where to output messages for the user
(define-public message-port (current-error-port))

;; Procedure to output messages to message-port
(define-public (message output-string)
  (display output-string message-port))

;; Procedure to output messages to geda log
(define-public (write-log message-string)
  (log! 'message message-string))