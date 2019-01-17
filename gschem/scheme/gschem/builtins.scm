;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture - Scheme API
;;
;; Copyright (C) 2013-2014 Peter Brett <peter@peter-b.co.uk>
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (gschem builtins)
  #:use-module (geda object)
  #:use-module (gschem core gettext)
  #:use-module (gschem core builtins)
  #:use-module (gschem action)
  #:use-module (gschem gschemdoc)
  #:use-module (gschem selection)
  #:use-module (gschem window)
  #:use-module (srfi srfi-1))

(or (defined? 'define-syntax)
    (use-modules (ice-9 syncase)))

(define-syntax define-action-public
  (syntax-rules ()
    ((_ (name . args) . forms)
     (begin
       (define-action (name . args) . forms)
       (export name)))))

;; -------------------------------------------------------------------
;;;; Special actions

(define-action-public (&cancel #:label (_ "Cancel"))
  (%cancel))

;; -------------------------------------------------------------------
;;;; File menu actions

(define-action-public (&file-new #:label (_ "New File") #:icon "gtk-new")
  (%file-new))

(define-action-public (&file-open #:label (_ "Open File") #:icon "gtk-open")
  (%file-open))

(define-action-public (&file-save #:label (_ "Save") #:icon "gtk-save")
  (%file-save))

(define-action-public (&file-save-as #:label (_ "Save As") #:icon "gtk-save-as")
  (%file-save-as))

(define-action-public (&file-save-all #:label (_ "Save All") #:icon "gtk-save")
  (%file-save-all))

(define-action-public (&file-save-modified #:label (_ "Save Modified") #:icon "gtk-save")
  (%file-save-all))

(define-action-public (&file-print #:label (_ "Print") #:icon "gtk-print")
  (%file-print))

(define-action-public (&file-write-image #:label (_ "Export Image") #:icon "geda-save-image")
  (%file-write-image))

(define-action-public (&file-run-script #:label (_ "Run Script") #:icon "gtk-execute")
  (%file-run-script))

(define-action-public (&file-new-window #:label (_ "New Window") #:icon "window-new")
  (%file-new-window))

(define-action-public (&file-close #:label (_ "Close Window") #:icon "geda-close")
  (%file-close))

(define-action-public (&file-close-all #:label (_ "Close Window") #:icon "geda-close-all")
  (%file-close-all))

(define-action-public (&file-quit #:label (_ "Quit") #:icon "gtk-quit")
  (%file-quit))

(define-action-public (&export-symbol #:label (_ "Export Symbol") #:icon "geda-inbed")
  (%export-symbol))

(define-action-public (&export-picture #:label (_ "Export Picture") #:icon "gtk-orientation-portrait")
  (%export-picture))

;; -------------------------------------------------------------------
;;;; General editing actions

(define-action-public (&edit-undo #:label (_ "Undo") #:icon "edit-undo")
  (%edit-undo))

(define-action-public (&edit-redo #:label (_ "Redo") #:icon "edit-redo")
  (%edit-redo))

(define-action-public (&edit-delete #:label (_ "Delete") #:icon "gtk-delete")
  (%edit-delete))

(define-action-public (&edit-copy #:label (_ "Copy Mode") #:icon "geda-copy")
  (%edit-copy))

(define-action-public (&edit-move #:label (_ "Move Mode") #:icon "geda-move")
  (%edit-move))

(define-action-public (&edit-mcopy #:label (_ "Multiple Copy Mode") #:icon "geda-multi")
  (%edit-mcopy))

(define-action-public (&edit-mirror #:label (_ "Mirror Mode") #:icon "geda-mirror")
  (%edit-mirror))

(define-action-public (&edit-offset #:label (_ "Offset selection") #:icon "geda-offset")
  (%edit-offset))

(define-action-public (&edit-rotate-left #:label (_ "Rotate Mode") #:icon "geda-rotate")
  (%edit-rotate-left))

(define-action-public (&edit-rotate-right #:label (_ "Rotate Mode") #:icon "geda-rotate")
  (%edit-rotate-right))

(define-action-public (&edit-snap #:label (_ "Snap to Grid") #:icon "geda-snap")
  (%edit-snap))

(define-action-public (&edit-array #:label (_ "Create Array") #:icon "geda-array")
  (%edit-array))

(define-action-public (&edit-break #:label (_ "Edit...") #:icon "break")
  (%edit-break))

(define-action-public (&edit-extend #:label (_ "Edit...") #:icon "extend")
  (%edit-extend))

(define-action-public (&edit-arc #:label (_ "Edit Arc") #:icon "geda-arc-edit")
  (%edit-arc))

(define-action-public (&edit-attributes #:label (_ "Edit Object") #:icon "gtk-indent")
  (%edit-attributes))

(define-action-public (&edit-text #:label (_ "Edit Text") #:icon "gtk-edit")
  (%edit-text))

(define-action-public (&edit-color #:label (_ "Edit Color") #:icon "geda-display-color")
  (%edit-color))

(define-action-public (&edit-pin-type #:label (_ "Edit Pin Type") #:icon "geda-pin-type")
  (%edit-pin-type))

(define-action-public (&edit-linetype #:label (_ "Edit Line Width & Type") #:icon "geda-line-type")
  (%edit-linetype))

(define-action-public (&edit-filltype #:label (_ "Edit Fill Type") #:icon "geda-mesh")
  (%edit-filltype))

(define-action-public (&edit-slot #:label (_ "Choose Slot") #:icon "geda-slot")
  (%edit-slot))

(define-action-public (&edit-component #:label (_ "Choose Slot") #:icon "geda-component")
  (%edit-component))

;; -------------------------------------------------------------------
;;;; Select actions

(define-action-public (&edit-select #:label (_ "Select Mode") #:icon "gschem-select")
  (%edit-select))

(define-action-public (&edit-select-all #:label (_ "Select All") #:icon "gschem-select-all")
  (%edit-select-all))

(define-action-public (&edit-select-invert #:label (_ "Invert Selection") #:icon "gschem-invert")
  (%edit-select-invert))

(define-action-public (&edit-select-last #:label (_ "Select Last") #:icon "gtk-select-all")
  (%edit-select-last))

(define-action-public (&edit-deselect #:label (_ "Deselect mode") #:icon "gschem-unselect")
  (%edit-deselect))

(define-action-public (&edit-deselect-all #:label (_ "Deselect All")#:icon "gschem-invert")
  (%edit-deselect-all))

(define-action-public (&edit-lock #:label (_ "Lock") #:icon "geda-lock")
  (%edit-lock))

(define-action-public (&edit-unlock #:label (_ "Unlock") #:icon "geda-unlock")
  (%edit-unlock))

;; -------------------------------------------------------------------
;;;; Clipboard actions

(define-action-public (&clipboard-cut #:label (_ "Cut") #:icon "gtk-cut")
  (%clipboard-cut))

(define-action-public (&clipboard-copy #:label (_ "Copy") #:icon "gtk-copy")
  (%clipboard-copy))

(define-action-public (&clipboard-paste #:label (_ "Paste") #:icon "gtk-paste")
  (%clipboard-paste))

;; -------------------------------------------------------------------
;;;; View control actions

(define-action-public (&view-redraw #:label (_ "Redraw") #:icon "gtk-refresh")
  (%view-redraw))

(define-action-public (&view-pan #:label (_ "Pan") #:icon "geda-zoom-pan")
  (%view-pan))

(define-action-public (&view-pan-left #:label (_ "Pan Left"))
  (%view-pan-left))

(define-action-public (&view-pan-right #:label (_ "Pan Right"))
  (%view-pan-right))

(define-action-public (&view-pan-up #:label (_ "Pan Up"))
  (%view-pan-up))

(define-action-public (&view-pan-down #:label (_ "Pan Down"))
  (%view-pan-down))

(define-action-public (&view-zoom-box #:label (_ "Zoom Box"))
  (%view-zoom-box))

(define-action-public (&view-zoom-selected #:label (_ "Zoom Selection") #:icon "gtk-zoom-fit")
  (%view-zoom-selected))

(define-action-public (&view-zoom-extents #:label (_ "Zoom Extents") #:icon "gtk-zoom-fit")
  (%view-zoom-extents))

(define-action-public (&view-zoom-in #:label (_ "Zoom In") #:icon "gtk-zoom-in")
  (%view-zoom-in))

(define-action-public (&view-zoom-out #:label (_ "Zoom Out") #:icon "gtk-zoom-out")
  (%view-zoom-out))

(define-action-public (&view-zoom-all #:label (_ "Zoom All"))
  (%view-zoom-all))

(define-action-public (&view-zoom-to-mag #:label (_ "Zoom to Mag"))
  (%view-zoom-to-mag))

(define-action-public (&view-documentation #:label (_ "Documentation"))
  (%view-documentation))

(define-action-public (&view-show-hidden #:label (_ "Show/Hide Invisible Text"))
  (%view-show-hidden))

(define-action-public (&view-show-inherited #:label (_ "Show/Hide Inherited Attributes"))
  (%view-show-inherited))

(define-action-public (&view-show-nets #:label (_ "Show/Hide net names"))
  (%view-show-nets))

(define-action-public (&view-dark-colors #:label (_ "Dark Color Scheme"))
  (%view-dark-colors))

(define-action-public (&view-light-colors #:label (_ "Light Color Scheme"))
  (%view-light-colors))

(define-action-public (&view-bw-colors #:label (_ "Monochrome Color Scheme"))
  (%view-bw-colors))

;; -------------------------------------------------------------------
;;;; Page draw order actions

(define-action-public (&page-draw-after #:label (_ "Draw After"))
  (%page-draw-after))

(define-action-public (&page-draw-before #:label (_ "Draw Before"))
  (%page-draw-before))

(define-action-public (&page-draw-first #:label (_ "Draw First"))
  (%page-draw-first))

(define-action-public (&page-draw-last #:label (_ "Draw Last"))
  (%page-draw-last))

;; -------------------------------------------------------------------
;;;; Page-related actions

(define-action-public (&page-manager #:label (_ "Page Manager"))
  (%page-manager))

(define-action-public (&page-first #:label (_ "First Page") #:icon "gtk-go-up")
  (%page-first))

(define-action-public (&page-prev #:label (_ "Previous Page") #:icon "gtk-go-up")
  (%page-prev))

(define-action-public (&page-next #:label (_ "Next Page") #:icon "gtk-go-up")
  (%page-next))

(define-action-public (&page-up #:label (_ "Page Up") #:icon "gtk-go-up")
  (%page-up))

(define-action-public (&page-down #:label (_ "Page Down") #:icon "gtk-go-down")
  (%page-down))

(define-action-public (&page-last #:label (_ "Last Page") #:icon "gtk-go-up")
  (%page-last))

(define-action-public (&page-new #:label (_ "New Page") #:icon "gtk-go-up")
  (%page-new))

(define-action-public (&page-print #:label (_ "Print Page") #:icon "gtk-print")
  (%page-print))

(define-action-public (&page-revert #:label (_ "Revert Changes") #:icon "gtk-revert-to-saved")
  (%page-revert))

(define-action-public (&page-revert-all #:label (_ "Revert All") #:icon "gtk-revert-to-saved")
  (%page-revert-all))

(define-action-public (&page-discard #:label (_ "Close Page") #:icon "gtk-close")
  (%page-discard))

(define-action-public (&page-close #:label (_ "Discard Page") #:icon "gtk-close")
  (%page-close))

;; -------------------------------------------------------------------
;;;; Hierarchy actions

(define-action-public (&hierarchy-down-schematic #:label (_ "Down Schematic") #:icon "gtk-go-down")
  (%hierarchy-down-schematic))

(define-action-public (&hierarchy-down-symbol #:label (_ "Down Symbol") #:icon "gtk-goto-bottom")
  (%hierarchy-down-symbol))

(define-action-public (&hierarchy-up #:label (_ "Up Hierarchy") #:icon "gtk-go-up")
  (%hierarchy-up))

;; -------------------------------------------------------------------
;;;; Actions related to adding things

(define-action-public (&add-component #:label (_ "Add Component") #:icon "insert-symbol")
  (%add-component))

(define-action-public (&add-net #:label (_ "Add Net") #:icon "insert-net")
  (%add-net))

(define-action-public (&add-bus #:label (_ "Add Bus") #:icon "insert-bus")
  (%add-bus))

(define-action-public (&add-attribute #:label (_ "Add Attribute") #:icon "insert-attribute")
  (%add-attribute))

(define-action-public (&add-text #:label (_ "Add Text") #:icon "insert-text")
  (%add-text))

(define-action-public (&add-line #:label (_ "Add Line") #:icon "insert-line")
  (%add-line))

(define-action-public (&add-pin #:label (_ "Add Pin") #:icon "insert-pin")
  (%add-pin))

(define-action-public (&add-box #:label (_ "Add Box") #:icon "insert-box")
  (%add-box))

(define-action-public (&add-circle #:label (_ "Add Circle") #:icon "insert-circle")
  (%add-circle))

(define-action-public (&add-arc #:label (_ "Add Arc") #:icon "insert-arc")
  (%add-arc))

(define-action-public (&add-path #:label (_ "Add Path") #:icon "insert-path")
  (%add-path))

(define-action-public (&add-picture #:label (_ "Add Picture") #:icon "insert-image")
  (%add-picture))

;; -------------------------------------------------------------------
;;;; Session System actions

(define-action-public (&session-new #:label (_ "Create New Session") #:icon "insert-image")
  (%session-new))

(define-action-public (&session-open #:label (_ "Open a Session") #:icon "insert-image")
  (%session-open))

(define-action-public (&session-save #:label (_ "Save Session") #:icon "insert-image")
  (%session-save))

(define-action-public (&session-save-as #:label (_ "Save Session As") #:icon "insert-image")
  (%session-save-as))

(define-action-public (&session-manage #:label (_ "Manage Sessions") #:icon "insert-image")
  (%session-manage))

;; -------------------------------------------------------------------
;;;; Attribute actions

(define-action-public (&attributes-attach #:label (_ "Attach Attributes") #:icon "attribute-attach")
  (%attributes-attach))

(define-action-public (&attributes-detach #:label (_ "Detach Attributes") #:icon "attribute-detach")
  (%attributes-detach))

(define-action-public (&attributes-home #:label (_ "Reset Attribute Positions") #:icon "show-value")
  (%attributes-home))

(define-action-public (&attributes-show-value #:label (_ "Show Attribute Value") #:icon "show-value")
  (%attributes-show-value))

(define-action-public (&attributes-show-name #:label (_ "Show Attribute Name") #:icon "show-name")
  (%attributes-show-name))

(define-action-public (&attributes-show-both #:label (_ "Show Name & Value") #:icon "show-both")
  (%attributes-show-both))

(define-action-public (&attributes-visibility #:label (_ "Toggle Text Visibility"))
  (%attributes-visibility))

(define-action-public (&attributes-editor #:label (_ "Open Attributes Editor"))
  (%attributes-editor))

(define-action-public (&attributes-find-text #:label (_ "Find Specific Text") #:icon "gtk-find")
  (%attributes-find-text))

(define-action-public (&attributes-hide-text #:label (_ "Hide Specific Text"))
  (%attributes-hide-text))

(define-action-public (&attributes-show-text #:label (_ "Show Specific Text"))
  (%attributes-show-text))

;; -------------------------------------------------------------------
;;;; Tools actions
(define-action-public (&tools-autonumber #:label (_ "Autonumber Text"))
  (%tools-autonumber))

(define-action-public (&tools-show-console #:label (_ "Show Console Window"))
  (%tools-show-console))

(define-action-public (&tools-show-coordinates #:label (_ "Show Coordinates Window"))
  (%tools-show-coordinates))

(define-action-public (&tools-guile-path #:label (_ "Guile Path"))
  (%tools-guile-path))

(define-action-public (&tools-invoke-macro #:label (_ "Invoke Macro"))
  (%tools-invoke-macro))

(define-action-public (&tools-translate #:label (_ "Translate Symbol"))
  (%tools-translate))

(define-action-public (&tools-embed #:label (_ "Embed Component/Picture"))
  (%tools-embed))

(define-action-public (&tools-unembed #:label (_ "Unembed Component/Picture"))
  (%tools-unembed))

(define-action-public (&tools-update #:label (_ "Update Component") #:icon "gtk-refresh")
  (%tools-update))

;; -------------------------------------------------------------------
;;;; Configuration actions

(define-action-public (&options-grid-dots #:label (_ "Dots Grid Style"))
  (%options-grid-dot))

(define-action-public (&options-grid-mesh #:label (_ "Mesh Grid Style"))
  (%options-grid-mesh))

(define-action-public (&options-grid-off #:label (_ "Turn Grid Off"))
  (%options-grid-off))

(define-action-public (&options-cycle-grid #:label (_ "Cycle Grid Mode"))
  (%options-cycle-grid))

(define-action-public (&scale-up-snap-size #:label (_ "Increase Grid Spacing"))
  (%scale-up-snap-size))

(define-action-public (&scale-down-snap-size #:label (_ "Decrease Grid Spacing"))
  (%scale-down-snap-size))

(define-action-public (&options-snap-size #:label (_ "Set Snap Spacing"))
  (%options-snap-size))

(define-action-public (&options-snap-off #:label (_ "Turn Snap off"))
  (%options-snap-size))

(define-action-public (&options-snap-on #:label (_ "Turn Snap on"))
  (%options-snap-on))

(define-action-public (&options-cycle-snap #:label (_ "Cycle Snap Mode"))
  (%options-cycle-snap))

(define-action-public (&options-rubberband #:label (_ "Toggle Net Rubber Band"))
  (%options-rubberband))

(define-action-public (&options-magneticnet #:label (_ "Toggle Magnetic Nets"))
  (%options-magneticnet))

(define-action-public (&options-dragcanmove #:label (_ "Enable/Disable Drag move"))
  (%options-dragcanmove))

(define-action-public (&options-action-feedback #:label (_ "Toggle Outline Drawing"))
  (%options-action-feedback))

(define-action-public (&options-auto-pan #:label (_ "Enable/Disable Auto Panning"))
  (%options-auto-pan))

(define-action-public (&options-text-size #:label (_ "Set Default Text Size"))
  (%options-text-size))

(define-action-public (&options-show-settings #:label (_ "Open Preference Dialog"))
  (%options-show-settings))

;; -------------------------------------------------------------------
;;;; Documentation-related actions

(define-action-public
    (&help-show-manual #:label (_ "gEDA Manuals") #:icon "help-browser"
     #:tooltip (_ "View the front page of the gEDA documentation in a browser."))
  (show-wiki "geda:documentation"))

(define-action-public (&help-show-hotkeys #:label (_ "Show Hotkeys") #:icon "preferences-desktop-keyboard-shortcuts")
  (%help-show-hotkeys))

(define-action-public
    (&hierarchy-documentation #:label (_ "Component Documentation")
                              #:icon "symbol-datasheet"
                              #:tooltip (_ "View documentation for selected component"))

  "If a component is selected, search for and display corresponding
documentation in a browser or PDF viewer. If no documentation can be
found, shows a dialog with an error message."

  (catch 'misc-error

   (lambda ()
     (let ((component
            (any (lambda (obj) (and (component? obj) obj))
                 (page-selection (active-page)))))
       (and component (show-component-documentation component))))

   (lambda (key subr msg args . rest)
     (gschem-msg (string-append
                  (_ "Could not show documentation for selected component\n")
                  (apply format #f msg args))))))

(define-action-public
    (&help-guide #:label (_ "gschem User Guide") #:icon "gtk-help"
                 #:tooltip (_ "View the gschem User Guide in a browser."))
  (show-wiki "geda:gschem_ug"))


(define-action-public
    (&help-show-faq #:label (_ "gschem FAQ") #:icon "help-faq"
     #:tooltip (_ "Frequently Asked Questions about using gschem."))
  (show-wiki "geda:faq-gschem"))


(define-action-public
    (&help-show-wiki #:label (_ "gEDA wiki") #:icon "web-browser"
     #:tooltip (_ "View the front page of the gEDA wiki in a browser."))
  (show-wiki))


(define-action-public (&help-show-about #:label (_ "About gschem") #:icon "gtk-about")
  (%help-show-about))

;; Local Variables:
;; eval: (put 'define-action-public 'scheme-indent-function 1)
;; End:
