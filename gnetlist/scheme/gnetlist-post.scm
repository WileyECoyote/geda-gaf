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

;; get all packages for a particular schematic page could eventually
;; placeholder will be either the hierarchical level or something
;; of the sort
(define packages
  (gnetlist:get-packages "placeholder"))

;; return a list of all unique the nets in the design
(define all-unique-nets
  (gnetlist:get-all-unique-nets "placeholder"))

;; return a list of all the nets in the design
;; Might return duplicates
(define all-nets
  (gnetlist:get-all-nets "placeholder"))

;; not very useful, but amusing
(define all-pins
   (map gnetlist:get-pins packages))

(define renamed-nets
  (gnetlist:get-renamed-nets "placeholder"))

;;
;; Functions for dealing with naming requirements for different
;; output netlist formats which may be more restrictive than
;; gEDA's internals.
;;

;; These will become hash tables which provide the mapping
;; from gEDA net name to netlist net name and from netlist
;; net name to gEDA net name.
(define netlist:net-hash-forward (make-hash-table  (length all-nets)))
(define netlist:net-hash-reverse (make-hash-table  (length all-nets)))

;; These will become hash tables which provide the mapping
;; from gEDA refdes to netlist refdes and from netlist
;; refdes to gEDA refdes.
(define netlist:refdes-hash-forward (make-hash-table  (length packages)))
(define netlist:refdes-hash-reverse (make-hash-table  (length packages)))

;; build the hash tables with the net name mappings and
;; while doing so, check for any shorts which are created
;; by modifying the netnames.  If a short occurs, error out
;; with a descriptive message.
;;
;; This function should be called as one of the first steps
;; in a netlister which needs to alias nets.
(define netlist:build-net-aliases
  (lambda (mapfn nets)
    (if (not (null? nets))
        (begin
          (let ( (net (car nets))
                 (alias (mapfn (car nets)))
                 )

            (if (hash-ref netlist:net-hash-reverse alias)
                (begin
                  (message "***** ERROR *****\n")
                  (message "There is a net name collision!\n")
                  (message "The net called \"")
                  (message net)
                  (message "\" will be remapped\nto \"")
                  (message alias)
                  (message "\" which is already used\n")
                  (message "by the net called \"")
                  (message (hash-ref netlist:net-hash-reverse alias))
                  (message "\".\n")
                  (message "This may be caused by netname attributes colliding with other netnames\n")
                  (message "due to truncation of the name, case insensitivity, or\n")
                  (message "other limitations imposed by this netlist format.\n")
                  (error)
                  )
                )
            (hash-create-handle! netlist:net-hash-forward net   alias)
            (hash-create-handle! netlist:net-hash-reverse alias net  )
            (netlist:build-net-aliases mapfn (cdr nets))
            )
          )
        )
    )
  )

;; build the hash tables with the refdes mappings and
;; while doing so, check for any name clashes which are created
;; by modifying the refdes's.  If a name clash occurs, error out
;; with a descriptive message.
;;
;; This function should be called as one of the first steps
;; in a netlister which needs to alias refdes's.
(define netlist:build-refdes-aliases
  (lambda (mapfn refdeses)
    (if (not (null? refdeses))
        (begin
          (let ( (refdes (car refdeses))
                 (alias (mapfn (car refdeses)))
                 )

            (if (hash-ref netlist:refdes-hash-reverse alias)
                (begin
                  (message "***** ERROR *****\n")
                  (message "There is a refdes name collision!\n")
                  (message "The refdes \"")
                  (message refdes)
                  (message "\" will be mapped\nto \"")
                  (message alias)
                  (message "\" which is already used\n")
                  (message "by \"")
                  (message (hash-ref netlist:refdes-hash-reverse alias))
                  (message "\".\n")
                  (message "This may be caused by refdes attributes colliding with others\n")
                  (message "due to truncation of the refdes, case insensitivity, or\n")
                  (message "other limitations imposed by this netlist format.\n")
                  (error)
                  )
                )
            (hash-create-handle! netlist:refdes-hash-forward refdes alias)
            (hash-create-handle! netlist:refdes-hash-reverse alias  refdes  )
            (netlist:build-refdes-aliases mapfn (cdr refdeses))
            )
          )
        )
    )
  )

;; convert a gEDA netname into an output netlist net name
(define netlist:alias-net
  (lambda (net)
    (hash-ref netlist:net-hash-forward net)
    )
)

;; convert a gEDA refdes into an output netlist refdes
(define netlist:alias-refdes
  (lambda (refdes)
    (hash-ref netlist:refdes-hash-forward refdes)
    )
)

;; convert an output netlist net name into a gEDA netname
(define netlist:unalias-net
  (lambda (net)
    (hash-ref netlist:net-hash-reverse net)
    )
)

;; convert an output netlist refdes into a gEDA refdes
(define netlist:unalias-refdes
  (lambda (refdes)
    (hash-ref netlist:refdes-hash-reverse refdes)
    )
)

