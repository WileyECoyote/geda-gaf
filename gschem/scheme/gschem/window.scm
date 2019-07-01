;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;;
;; Copyright (C) 2010-2015 Peter Brett <peter@peter-b.co.uk>
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

(define-module (gschem window)

  ; Import C procedures
  #:use-module (gschem core window)

  #:use-module (geda page)
  #:re-export (close-page!))

(define-public active-page %active-page)
(define-public set-active-page! %set-active-page!)
(define-public pointer-position %pointer-position)

;; get-active-filename
;;
;; Returns the filename associated with the active page in the current
;; gschem window.
(define-public (get-active-filename)
  (page-filename (active-page)))

(define-public (snap-point point)
  (%snap-point (car point) (cdr point)))
