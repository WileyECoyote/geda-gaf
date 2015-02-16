;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;;
;; Copyright (C) 2010-2015 Peter Brett
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

(define-module (gschem hook)

  ;; Import C definitions
  #:use-module (gschem core hook))

(define-public action-property-hook  %action-property-hook)

(define-public add-objects-hook      %add-objects-hook)

(define-public attach-attribs-hook   %attach-attribs-hook)

(define-public bind-keys-hook        %bind-keys-hook)

(define-public copy-objects-hook     %copy-objects-hook)

(define-public deselect-objects-hook %deselect-objects-hook)

(define-public detach-attribs-hook   %detach-attribs-hook)

(define-public mirror-objects-hook   %mirror-objects-hook)

(define-public move-objects-hook     %move-objects-hook)

(define-public new-page-hook         %new-page-hook)

(define-public paste-objects-hook    %paste-objects-hook)

(define-public remove-objects-hook   %remove-objects-hook)

(define-public rotate-objects-hook   %rotate-objects-hook)

(define-public select-objects-hook   %select-objects-hook)

