;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;;
;; Copyright (C) 2010-2016 Peter Brett
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
;; <http://www.gnu.org/licenses/>
;;

;; This file contains Scheme API features, and should be renamed!

(define-module (gschem deprecated)

  #:use-module (gschem core gettext)

  #:use-module (geda   attrib)
  #:use-module (geda   deprecated)
  #:use-module (geda   object)
  #:use-module (geda   page)
  #:use-module (gschem window)
  #:use-module (gschem hook)
  #:use-module (gschem selection)
  #:use-module (gschem attrib)

  #:use-module (srfi srfi-1))

;; add-attribute-to-object object name value visible show
;;
;; Add an attribute "name=value" to object.  If visible is #f, the new
;; attribute will be invisible.  show should be a list containing one
;; or both of the strings "name and "value" (if neither is specified,
;; both are assumed).
;;
;; See also add-attrib! in the (gschem attrib) module.
(define-public (add-attribute-to-object object name value visible show color)
  (add-attrib! object name value visible
               (let ((n (member "name" show))
                     (v (member "value" show)))
                 (cond
                  ((and n v) 'both)
                  (n 'name)
                  (v 'value)
                  (else 'both)))
               color)) ;; Default

;; set-attribute-text-properties! attrib color size alignment
;;                                rotation x y
;;
;; Sets several parameters of the text object attrib.  x and y are the
;; coordinates of the text anchor.  color is the colormap index of the
;; color with which to draw the text.  size is the font size, and
;; angle is the angle at which to draw the text.
;;
;; All of the former parameters may be set to -1 to leave the current
;; value unchanged.
;;
;; alignment should be one of the following strings:
;;
;;    "Lower Left"
;;    "Middle Left"
;;    "Upper Left"
;;    "Lower Middle"
;;    "Middle Middle"
;;    "Upper Middle"
;;    "Lower Right"
;;    "Middle Right"
;;    "Upper Right"
;;
;; or the empty string "" to leave the alignment unchanged.
(define-public (set-attribute-text-properties!
                  attrib color size alignment rotation x y)
  (set-text! attrib
             ;; anchor
             (let ((anchor (text-anchor attrib)))
               (cons (if (= x -1) (car anchor) x)
                     (if (= y -1) (cdr anchor) y)))
             ;; align
             (or
              (assoc-ref
               '(("Lower Left" . lower-left)
                 ("Middle Left" . middle-left)
                 ("Upper Left" . upper-left)
                 ("Lower Middle" . lower-center)
                 ("Middle Middle" . middle-center)
                 ("Upper Middle" . upper-center)
                 ("Lower Right" . lower-right)
                 ("Middle Right" . middle-right)
                 ("Upper Right" . upper-right))
               alignment)
              (and (string=? "" alignment) (text-align attrib))
              (error (_ "Invalid text alignment ~A.") alignment))
             ;; angle
             (if (= rotation -1) (text-angle attrib) rotation)
             ;; string
             (text-string attrib)
             ;; size
             (if (= size -1) (text-size attrib) size)
             ;; visible
             (text-visible? attrib)
             ;; show
             (text-attribute-mode attrib)))

;; add-component-at-xy page basename x y angle selectable mirror
;;
;; Adds the component called basename from the component library to a
;; page, at the coordinates (x, y) and rotated by the given angle.  If
;; selectable is false, the component will be locked.  If mirror is
;; true, the component will be mirrored.  If the requested basename
;; cannot be found in the library, returns #f.
(define-public (add-component-at-xy page basename x y angle selectable mirror)
  (if (or (null? basename) (not basename) (string=? basename ""))
      #f
      (let ((C (make-component/library basename
                                       (cons x y) angle mirror
                                       (not selectable))))
        (and C
             (page-append! page C)
             (run-hook add-objects-hook (cons C (promote-attribs! C)))))))

;; set-attribute-value! attrib value
;;
;; Set the value part of the text object attrib.
(define-public (set-attribute-value! attrib value)
  (let ((params (text-info attrib))
        (name-value (parse-attrib attrib)))
    (list-set! params 3 (simple-format #f "~A=~A" (car name-value) value))
    (apply set-text! attrib params)))

;; get-objects-in-page page
;;
;; Get the contents of page, in reverse order.
(define-public (get-objects-in-page page)
  (reverse! (page-contents page)))

;; get-current-page
;;
;; Return the page which currently has focus in gschem.
(define-public get-current-page active-page)

;; get-object-pins object
;;
;; Return the pin objects from a component's contents, in reverse
;; order, or the empty list if object is not a component.
(define-public (get-object-pins object)
  (if (component? object)
      (reverse! (filter! pin? (component-contents object)))
      '()))

;; get-object-bounds object exclude-attribs exclude-types
;;
;; Return the bounds of an object, excluding attributes with
;; particular names or certain object types.
;;
;; The exclude-attribs should be a list of attribute names to be
;; omitted, as strings. If the special string "all" appears in the
;; list, all attributes are excluded.
;;
;; The exclude-types should be a list of single-character strings
;; containing object type characters (as returned by the deprecated
;; get-object-type function).
;;
;; Note that attributes attached to pins (but not attached to anything
;; else) are included in the bounds.
;;
;; The bounds are returned in the form:
;;
;;   ((left . right) . (bottom . top))
;;
;; N.b. that this is a different form to that returned by
;; object-bounds, so you can't use fold-bounds directly with bounds
;; returned by this function.
(define-public (get-object-bounds object exclude-attribs exclude-types)
  (define no-attribs (member "all" exclude-attribs))

  (define (exclude? object)
    (or
     ;; Is it an excluded type?
     (member (string (get-object-type object)) exclude-types)
     ;; Is it invisible text?
     (and (text? object) (not (text-visible? object)))
     ;; Is it an excluded attribute?
     (and (attribute? object)
          (or no-attribs
              (member (attrib-name object) exclude-attribs)))))

  (define (excluding-bounds object)
    (cond
     ;; If it's excluded, no bounds!
     ((exclude? object) #f)
     ;; If it's a component, recurse
     ((component? object)
      (fold fold-bounds #f
            (map excluding-bounds (component-contents object))))
     ;; If it's a pin, include its attributes
     ((pin? object)
      (fold fold-bounds #f
            (cons (object-bounds object)
                  (map excluding-bounds (object-attribs object)))))
     ;; Otherwise, just return the object bounds
     (else (object-bounds object))))

  (let ((bounds (excluding-bounds object)))
    (if bounds
        ;; Re-arrange the bounds into the format expected
        (cons (cons (caar bounds) (cadr bounds))
              (cons (cddr bounds) (cdar bounds)))
        ;; Stupid default
        '((1000000 . 0) . (1000000 . 0))))
)

;; get-pin-ends pin
;;
;; Return the coordinates of the endpoints of a pin, in the format:
;;
;;   ((x1 . y1) x2 . y2)
(define-public (get-pin-ends pin)
  (let ((params (line-info pin)))
    (cons (list-ref params 0) (list-ref params 1))))

;; get-selected-filename
;;
;; Returns the filename associated with the active page in the current
;; gschem window.
(define-public (get-selected-filename)
  (page-filename (active-page)))

;; get-selected-component-attributes
;;
;; Returns a list of all selected text object strings, with duplicate
;; values removed (i.e. the name is pretty much unrelated to the
;; behaviour).  Really, seriously, just don't use this in new code.
(define-public (get-selected-component-attributes)
  (delete-duplicates!
   (map text-string (filter! text? (page-selection (active-page))))))

;;;; Old-style hooks

;; Adds a function to src-hook.  The function is called with a single
;; argument, lst, which should be a list of objects.  For each member
;; of lst which matches filter?, the function calls tgt-hook with that
;; object as the argument.
(define (add-hook!/filter src-hook tgt-hook filter?)
  (add-hook! src-hook
    (lambda (lst)
      (if (not (hook-empty? tgt-hook))
          (for-each
           (lambda (obj)
             (if (filter? obj)
                 (run-hook tgt-hook obj)))
           lst)))))

;; Adds a function to src-hook. The function is called with a single
;; argument, lst, which should be a list of objects.  For each member
;; of lst which matches filter?, the function calls tgt-hook with a
;; list of all attributes (attached and inherited) of that object.
(define (add-hook!/full-attribs src-hook tgt-hook filter?)
  (add-hook! src-hook
    (lambda (lst)
      (if (not (hook-empty? tgt-hook))
          (for-each
           (lambda (obj)
             (if (filter? obj)
                 (run-hook tgt-hook
                           (append! (object-attribs obj)
                                    (inherited-attribs obj)))))
           lst)))))

;; add-component-hook:
;;
;; Called when a new component is added to the page (not copied etc).
;; Argument is a list of all attributes (inherited & promoted) of the
;; component.  Differs from the classic behaviour in that it is *not*
;; also called once for each promoted attribute with the empty list as
;; the argument.
(define-public add-component-hook (make-hook 1))
(add-hook!/full-attribs add-objects-hook add-component-hook component?)

;; add-component-object-hook:
;;
;; Called when a new component is added to the page (not copied etc).
;; Called once with the component itself as the argument, and once for
;; each promoted attribute, with the attribute as the argument.
(define-public add-component-object-hook (make-hook 1))
(add-hook! add-objects-hook
 (lambda (lst)
   (if (not (hook-empty? add-component-object-hook))
       (for-each
        (lambda (obj)
          (define (run x) (run-hook add-component-object-hook x))
          (if (component? obj)
              (begin (run obj) (for-each run (object-attribs obj)))))
        lst))))

;; add-attribute-hook
;;
;; Called each time an attribute is added to something.  Argument is
;; the thing that had an attribute added.  The behaviour here emulates
;; the classic behaviour as closely as possible -- it doesn't run the
;; hook on explicit attribute attachment operations (via
;; "Attributes->Attach"), but does run when an individual attribute is
;; created and simultaneously attached to something.
(define-public add-attribute-hook (make-hook 1))
(add-hook! add-objects-hook
  (lambda (lst)
    (if (and (not (hook-empty? add-attribute-hook)) (= 1 (length lst)))
        (let* ((attrib (car lst))
               (target (attrib-attachment attrib)))
          (if (and (attribute? attrib) target)
              (run-hook add-attribute-hook target))))))

;; add-pin-hook
;;
;; Called each time a pin is added to the schematic.  Argument is the
;; pin itself.
(define-public add-pin-hook (make-hook 1))
(add-hook!/filter add-objects-hook add-pin-hook pin?)

;; mirror-component-object-hook
;;
;; Called for each component in the selection when a mirror operation
;; is carried out.  The argument is the component itself.
(define-public mirror-component-object-hook (make-hook 1))
(add-hook!/filter mirror-objects-hook mirror-component-object-hook component?)

;; mirror-pin-hook
;;
;; Same as mirror-component-object-hook, but for pins.
(define-public mirror-pin-hook (make-hook 1))
(add-hook!/filter mirror-objects-hook mirror-pin-hook pin?)

;; rotate-component-object-hook
;;
;; Called for each component in the selection when a rotate operation
;; is carried out (including using the middle mouse button during a
;; move operation, but excluding rotations during component
;; placement).  The argument is the component itself.
(define-public rotate-component-object-hook (make-hook 1))
(add-hook!/filter rotate-objects-hook rotate-component-object-hook component?)

;; rotate-pin-hook
;;
;; Same as rotate-component-object-hook, but for pins.
(define-public rotate-pin-hook (make-hook 1))
(add-hook!/filter rotate-objects-hook rotate-pin-hook pin?)

;; copy-component-hook:
;;
;; Called each time a component is copied into the schematic.
;; Argument is a list off all attributes (inherited & promoted) of the
;; component.  Differs from classic behaviour in that it is called on
;; pasting from buffers and the clipboard, in addition to "Edit->Copy
;; Mode" and "Edit->Multiple Copy Mode".
(define-public copy-component-hook (make-hook 1))
(add-hook!/full-attribs copy-objects-hook copy-component-hook component?)

;; move-component-hook:
;;
;; Called each time a component is moved in the schematic.
;; Argument is as copy-component-hook.
(define-public move-component-hook (make-hook 1))
(add-hook!/full-attribs move-objects-hook move-component-hook component?)

;; deselect-component-hook:
;;
;; Called each time a component is removed from the selection,
;; except if the selection is cleared entirely.  Argument is
;; as select-component-hook.
(define-public deselect-component-hook (make-hook 1))
(add-hook!/full-attribs deselect-objects-hook deselect-component-hook
                        component?)

;; deselect-net-hook:
;;
;; Called each time a net segment (n.b. *not* bus segment) is added to
;; the selection. Argument is a list of all attributes of the
;; net.
(define-public deselect-net-hook (make-hook 1))
(add-hook!/full-attribs deselect-objects-hook deselect-net-hook net?)

;; deselect-all-hook:
;;
;; Called with the empty list as the argument each time the
;; selection is emptied, even if the selection is already
;; empty.
(define-public deselect-all-hook (make-hook 1))
(add-hook! deselect-objects-hook
  (lambda (arg)
    (if (and (not (null? deselect-all-hook))
             (null? (page-selection (active-page))))
        (run-hook deselect-all-hook '()))))

;; select-component-hook:
;;
;; Called each time a component is added to the selection.
;; Argument is a list of all attributes (inherited & promoted)
;; of the component.
(define-public select-component-hook (make-hook 1))
(add-hook!/full-attribs select-objects-hook select-component-hook
                        component?)

;; select-net-hook:
;;
;; Called each time a net segment (n.b. *not* bus segment) is
;; added to the selection.  Argument is the empty list.
(define-public select-net-hook (make-hook 1))
(add-hook!/full-attribs select-objects-hook select-net-hook net?)
