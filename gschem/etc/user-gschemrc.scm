;-*-Scheme-*-
;
; Init file for gschem
;

;
;  WARNING: Make a backup copy of this file before making any changes!
;

;  ;'s are comments
;  keywords are case sensitive (guile feature)
;  mode strings are case sensitive
;  colors are not case sensitive
;
;  If an integer is out-side of the valid range, the corresponding limit
;  will be used and a warning issued.
;
; In case of difficulties, start gschem from a shell, as oppose to starting
; from a GUI. Then look for error messages and double check the parameter(s)
; listed in the message. If there are multiple messages, be sure to scroll
; back and look for the "first" error as this is often the only problem.
;
; A common symptom that appears when there is a problem with this file is
; "no menus" and a sensitivity error on  non-existent menu item. This is
; indicative of a syntax error in this file. Maybe one of the easiest mistake
; is to use a colon instead of a semi-colon and no useful information is
; provided for locating the problem. Try using an Editor like gedit or Kate
; with search high-lighting. There are not that many colons in this file and
; none of them should be the first character on a line.
;
; gschem-version string
;
; Specifies the version of this file.  This number is used to make sure
; that the rc file is compatible with the version of gschem that is
; being run. The end user should *not* change this value.
;

(gschem-version "@DATE_VERSION@")

;
; Start of color section
;

; Load up a color scheme which has a dark (black) background. Comment out
; the first line and comment in the second line for a light (almost white)
; background. The dark background is the original look.
;
(primitive-load (build-path geda-rc-path "display-colormap-darkbg")) ; dark background
;(primitive-load (build-path geda-rc-path "gschem-colormap-lightbg")) ; light background
;(primitive-load (build-path geda-rc-path "gschem-colormap-bw")) ; light background, bw
;(primitive-load (build-path geda-rc-path "gschem-colormap-custom"))

;; Load up a color scheme for printing optimized for a white background.
; Comment out the second line and comment in the first line for a
; dark background. The dark background is the original look.
;
;(primitive-load (build-path geda-rc-path "print-colormap-darkbg")) ; dark background
(primitive-load (build-path geda-rc-path "print-colormap-lightbg")) ; light background

; image-color string
;
; Controls if images (png, jpg, etc.) are color (enabled) or black/white
; (disabled). If disabled images will be generated in black and white.
; When enabled, colorable image types will be generated using the active
; color map scheme, (which could be black/white).
;
;(image-color "enabled")
;(image-color "disabled")

; invert-images string
;
; When image-color is disabled, invert-images will black and white so that
; images are black-on-white when enabled (default). When disabled, generated
; images will be the same as the black and white screen image; white-on-
; black (like the old gschem style).
;(invert-images "enabled")
(invert-images "disabled")

;
; End of color section
;

;
; Start of mode related keywords
;

;BEGIN =================> Display Configuration <===================

;
; action-color integer
;
; Sets the color to use for status text while inside an action.
;
; Min value: 0, Max value: 29, Default value: 4
;
(action-color 4)

;
; anti-aliasing string
;
; Controls the anti-aliasing method or hinting when drawing object on the
; screen, excluding fonts. The "default" is Cairo's default, which is not
; defined. Gschem's default, if not option is uncommented is "subpixel".
; primarily effects the edge quality when rendering non-text objects.
;
;(anti-aliasing "default")
;(anti-aliasing "none")
;(anti-aliasing "gray")
;(anti-aliasing "subpixel")
;(anti-aliasing "fast")
;(anti-aliasing "good")
;(anti-aliasing "best")

;
; draw-grips string
;
; Controls if the editing grips are drawn when selecting objects
;
;(draw-grips "enabled")
;(draw-grips "disabled")

;
; draw-complex-grips string
;
; Controls if the editing grips are drawn when selecting complex
; objects when draw-grips is "enabled".
;
;(draw-complex-grips "enabled")
;(draw-complex-grips "disabled")

;
; grip-size integer
;
; Controls the size of grips in pixel units.
;
; Min value: 7, Max value: 30, Default value: 12
;
;(grip-size 12)

;BEGIN ----------------------> Grid Setup <-------------------------
;
; grid-mode string
;
; Controls displaying of the grid and grid type.
;
;(grid-mode "none")
;(grid-mode "dots")
;(grid-mode "mesh")

; dots-grid-minor-color 3 integers
;
; The dots-grid-minor-color specifies the color to use for the minor dots grid,
; The color is specified as three integers for red, green, and blue, respectively.
;
;  RED GREEN BLUE
;
;(dots-grid-minor-color 48830 48830 48830)
;(dots-grid-minor-color 5781 56202 17928)
(dots-grid-minor-color 42747 41944 3217)

; dots-grid-major-color 3 integers
;
; The dots-grid-major-color specifies the color to use for the major dots grid,
; The color is specified as three integers for red, green, and blue, respectively.
;
;  RED GREEN BLUE
;
;(dots-grid-major-color 48830 48830 48830)
;(dots-grid-major-color 22084 3125 63070)
(dots-grid-major-color 46956 4943 42960)

; dots-grid-dot-size integer
;
; The dots-grid-dot-size keyword controls the size of the grid dots in the
; dots grid display. The units are in pixels. The default (min) value of 1
; is the best performing as the grid dot size is rendered as a single pixel.
; Values of 2 and 3 are good values to try if the default grid dot size is
; too small for your tastes. Anything larger than 3 is probably too large.
;
;(dots-grid-dot-size 1)
;(dots-grid-dot-size 2)
;(dots-grid-dot-size 3)

; dots-grid-mode string
;
; The dots-grid-mode keyword controls the mode of the dotted grid, either
; variable or fixed. In the variable mode, the grid spacing changes
; depending on the zoom factor. In the fixed mode, the grid always
; represents the same number of units as the snap-spacing. You can
; control the density of the grid using the dots-grid-threshold.
;(dots-grid-mode "variable")
;(dots-grid-mode "fixed")

; dots-grid-threshold integer
;
; The dots-grid-threshold specifies the minimum number of pixels
; grid-spacing for the grid to be displayed. Using this parameter you can
; control the density of the displayed grid (smaller numbers will cause the
; grid to be drawn denser). This mode is only used when grid-mode is fixed.
;
(dots-grid-threshold 10)

; dots-grid-minor-alpha integer
;
; The dots-grid-minor-alpha specifies the percentage transparency to apply to
; the color used for minor grid lines when when using the dots grid. The alpha
; setting is dependent on the color choice, and requires some experimentation.
;
; min value: 0
; max value: 100
; default value: 30
;
;(dots-grid-minor-alpha 15)
(dots-grid-minor-alpha 30)

; dots-grid-major-alpha integer
;
; The dots-grid-major-alpha specifies the percentage transparency to apply to
; the color used for major grid lines when when using the dots grid. The alpha
; setting is dependent on the color choice, and requires some experimentation.
;
; min value: 0
; max value: 100
; default value: 40
;
;(dots-grid-major-alpha 20)
(dots-grid-major-alpha 50)

; mesh-line-width-factor integer
;
; The mesh-line-width-factor specifies the line width factor to apply when
; using the mesh grid.
;
(mesh-line-width-factor 118)

; mesh-grid-threshold integer
;
; The mesh-grid-threshold specifies the minimum line pitch for the grid to
; be displayed. This parameter can be used to control the maximum density
; of the displayed grid before the minor, and then major grid-lines are
; turned off.
;
(mesh-grid-threshold 10)

; mesh-grid-minor-alpha integer
;
; The mesh-grid-minor-alpha specifies the percentage transparency to apply to
; the color used for minor grid lines when when using the mesh grid. The alpha
; setting is dependent on the color choice, and requires some experimentation.
;
; min value: 0
; max value: 100
; default value: 30
;
;(mesh-grid-minor-alpha 15)
(mesh-grid-minor-alpha 27)

; mesh-grid-major-alpha integer
;
; The mesh-grid-major-alpha specifies the percentage transparency to apply to
; the color used for major grid lines when when using the mesh grid. The alpha
; setting is dependent on the color choice, and requires some experimentation.
;
; min value: 0
; max value: 100
; default value: 40
;
;(mesh-grid-major-alpha 20)
(mesh-grid-major-alpha 38)

; mesh-grid-minor-color 3 integers
;
; The mesh-grid-minor-color specifies the color to use for minor lines for the
; mesh grid. The color is specified as three integers for red, green, and blue,
; respectively.
;
; For the "white" values provided in set 1, try lower alpha setting, i.e. the
; 15/20 set. Some experimentation maybe required.
;
; Set 1 Minor: White
;(mesh-grid-minor-color 48830 48830 48830)
;
; Set 2 Minor: Blue
;(mesh-grid-minor-color 5781 56202 17928)
;
; Set 3 Minor: Magenta
(mesh-grid-minor-color 42747 41944 3217)

; mesh-grid-major-color 3 integers
;
; The mesh-grid-major-color specifies the color to use for major lines when
; using the mesh grid, see comments for mesh-grid-minor-color.
;
; Set 1 Major: White
;(mesh-grid-major-color 48830 48830 48830)
;
; Set 2 Major: Green
;(mesh-grid-major-color 22084 3125 63070)
;
; Set 3 Major: Magenta 3
(mesh-grid-major-color 52585 0 52585)

;END ------------------------> Grid Setup <-------------------------

; object-clipping string
;
; Determines if the object clipping code is executed or not
; This code does speed up redraws a bit. Possible options are enabled
; or disabled. Clipping should not normally be turned off, since some
; X servers don't handle clipping correctly.
;
(object-clipping "enabled")
;(object-clipping "disabled")

; window-size integer integer
;
; window-size width height
;
; Specifies the size of the drawing area window.  The width and height
; are specified in pixels and do not include the three menu bars and
; scrollbars (so the window will be larger than the specified
; measurements). Try to keep an aspect ratio of 1.333333 if at all possible.
; These numbers are NOT the true size of the window, but of the drawing area.
;
;(window-size 650 487)  ; Good size for 800x600
(window-size 900 650)  ; Good size for 1024x768
;(window-size 950 712)  ; Good size for 1152x864
;(window-size 1100 825) ; Good size for 1280x1024

; world-size width height border
;
; Specifies the size of the world and a border (in world space units)
; Be sure all inputs are reals (floats/doubles) and don't try to reverse
; the values to get a portrait mode.  Code to support that needs to be added
; The code that implements this automatically transforms the dimensions into
; the proper aspect ratio.  All units are in inches.
; This is not the paper size.  That is specified elsewhere.  End users should
; not change this at all.
;
;(world-size 60.0 45.0 1.0)
(world-size 120.0 90.0 1.0)
;(world-size 180.0 135.0 1.0)

;BEGIN -------------------> Display Zoom Setup <--------------------

; warp-cursor string
;
; Controls if the cursor is warped (or moved) when you zoom in and out.
; Some people find this forced cursor movement annoying.
;
;(warp-cursor "enabled")
(warp-cursor "disabled")

; zoom-gain integer
;
; Controls the percentage size increase when zooming into the page.
; Un-zooming uses the inverse factor such that a zoom in / zoom out
; pair will return the schematic to the same size.
;  E.g.
;    20% increment => x 1.2 original size when zooming in
;                  => x 1 / 1.2 x original size when zooming out
;
(zoom-gain 20)
;;(zoom-gain 50) ; Hard-coded behaviour up to version 1.5.0.20080706

; zoom-with-pan string
;
; Sets the zoom in and zoom out functions to pan the display and then zoom.
; Basically zoom in / out where the mouse pointer sits on the display.
; Comment out if you want the default mode.
;
(zoom-with-pan "enabled")
;(zoom-with-pan "disabled")

;END ------------------->  Display Zoom Setup  <--------------------
;END ===================> Display Configuration <===================

;BEGIN ==================> Set Log Configuration <==================

; logging string or integer
;
; Determines if the logging mechanism is enabled or disabled
; Possible options are enabled or disabled
;
; See log-destiny keyword for control over where the messages go.
;
(logging "enabled")
;(logging "disabled")

; log-destiny string
;
; Specifies where log message go during run time.
; Possible options are;
;      console_window  The console window (if it's visible)
;      tty             The stdout of the terminal where gschem was run from
;      both            Both of the above locations
; Message are always written to the log file (unless logging is disabled)
; by the above keyword.
;
;
(log-destiny "console_window")
;(log-destiny "tty")
;(log-destiny "both")
;
; Controls if the console window is mapped when gschem is started up
; Possible options;
;       startup - opened up when gschem starts
;       later   - NOT opened up when gschem starts
;                 (can be opened by Options/Show console Window)
;
;(console-window "enabled")
;(console-window "disabled")

; console-window-type string
;
; Controls if the console window is a transient or if it is decorated
; as a normal window (this is dependant on the window manager doing decoration
; right)
;
; Possible options;
;       decorated       - console window is a normal decorated window
;       transient       - console window is a transient dialog box, typically
;                         not decorated by the window manager
;
(console-window-type "decorated")
;(console-window-type "transient")

;END ====================> Log Configuration <======================

;BEGIN ================> Miscellaneous Options <====================

; action-feedback-mode string
;
; Set the default action feedback mode (for copy/move/component place).
; Set to outline to get an outline of the selection.
; Set to boundingbox to get a bounding box of the selection.
; For a fast machines with fast video use outline (it looks good).
; For a slow machine use boundingbox; it is much faster.
; Comment out if you want the default mode.
;
(action-feedback-mode "outline")
;(action-feedback-mode "boundingbox")

; add-attribute-offset integer
;
; This has not been implemented/debugged yet.
;
; Controls a offset which is added to the location of text items that are
; added to an object as an attribute. This offset is added when the following
; conditions occur;
;
;  1) Add/Attribute... has been invoked via the hotkey
;  2) It is the "netname" attribute being added
;  3) It is being attached to a horizontal or vertical net segment
;  4) The initial mouse position is at or near the actual net (with one
;     grid unit).
;
; If these four conditions are not met, then this offset is not added.
(add-attribute-offset 50)

; auto-load-last string
;
; Determines if the newly placed components are embedded in the schematic
; or if only the filename is specified and the component is searched for
; instead.  If it is enabled, then all new components will be embedded
; othewise they are not embedded. This can be controlled on the fly during
; runtime with the "Embed Component" checkbox on the select component dialog
; box
;
(auto-load-last "enabled")
;(auto-load-last "disabled")

; autosave interval
;
; Controls if a backup copy is made every "interval" seconds.
; Note that the backup copy is made when you make some change to the schematic,
; and there were more than "interval" seconds from the last autosave.
; Autosaving will not be allowed if setting it to zero.
;(auto-save-interval 180)

; Attribute autoplacement grid
(define autoplace-attributes-grid 50)

; component-dialog-attributes stringlist
;
; Sets a list of attributs that are visible in the component select dialog.
; The attributes are sorted in the same order as they appear in the list.
; If the first list element is an asterisk "*", all attributes will be
; displayed in alphabetical order.
;
; Note 1. An empty list will disable the attribute view in the dialog.
; Note 2. To enable the list ALL of the semi-colons must be removed
;         from the first option!
;
(component-dialog-attributes '("device" "description" "footprint" "comment"
                               "net" "model" "model-name" "file" "value"
                               "numslots" "slotdef" "slot" "documentation"
                               "symversion" "author" "use-license"
                               "dist-license" ))
;(component-dialog-attributes '("*"))
;(component-dialog-attributes '())

; continue-component-place string
;
; If this enabled then multiple instances of the same component can be placed
; immediately without having to click on the name or Apply in the Component
; Place... dialog box.  If this is disabled then only one component can be
; placed (the user must then press Apply in the dialog box to place multiple
; instances of the same component)
;
(continue-component-place "enabled")
;(continue-component-place "disabled")

; embed-components string
;
; Determines if the newly placed components are embedded in the schematic
; or if only the filename is specified and the component is searched for
; instead.  If it is enabled, then all new components will be embedded
; othewise they are not embedded.  This can be controlled on the fly during
; runtime with the "Embed Component" checkbox on the select component dialog
; box
;
;(embed-components "enabled")
(embed-components "disabled")

;  enforce-hierarchy string
;
;  Controls if the movement between hierarchy levels (of the same underlying
;  schematics) is allowed or not.
;  If this is enabled, then the user cannot (without using the page manager)
;  move between hierarchy levels otherwise, if enabled, the user sees all
;  the hierarchy levels as being flat.
;
(enforce-hierarchy "enabled")
;(enforce-hierarchy "disabled")

; force-boundingbox string
;
; Controls if the entire bounding box of a symbol is used when figuring out
; whichend of the pin is considered the active port.  Enable this when
; gschem is guessing incorrectly.
;
;(force-boundingbox "enabled")
(force-boundingbox "disabled")

; keyboardpan-gain integer
;
; Controls how much the display pans when using the keyboard cursor keys.
; A larger value provides greater pan distance when pressing the cursor
; keys, while a smaller value provides a smoother, but smaller pan
; distance when moving the cursor keys.
(keyboardpan-gain 20)
;;(keyboardpan-gain 10)
;;(keyboardpan-gain 1)
;;(keyboardpan-gain 5)

; magnetic-net-mode string
;
; Controls the initial setting of the magnetic net mode. The magnetic
; net mode marks a possible connection that is close to the current
; cursor position
(magnetic-net-mode "enabled")
;(magnetic-net-mode "disabled")

; netconn-rubberband string
;
; Controls if net connections are maintained when you move a connecting
; component or net.
;
(netconn-rubberband "enabled")
;(netconn-rubberband "disabled")

; select-slack-pixels integer
;
; Controls how many pixels around an object can still be clicked as part of
; that object.
; A larger value gives greater ease in selecting small, or narrow objects.
(select-slack-pixels 10)
;;(select-slack-pixels 4)
;;(select-slack-pixels 0)
;;(select-slack-pixels 1)

; snap-size number
;
; Sets the default snap spacing at start-up of gschem.
;
(snap-size 100)

; sort-components-library string
;
; If this is enabled, then the component library will be sorted in
; alphanumeric order.  Bear in mind that this option is totally
; cosmetic, and will not alter the component search order (latest
; added gets scanned first).
;
;(sort-component-library "enabled")
(sort-component-library "disabled")

;END ==================> Miscellaneous Options <====================

;BEGIN =================> Nets and Routing Setup <==================

; net-consolidate string
;
;  Controls if the net consolidation code is used when schematics are read
;  in, written to disk, and when nets are being drawn (does not consolidate
;  when things are being copied or moved yet).  Net consolidation is the
;  connection of nets which can be combined into one.
;  Comment out if you want the default mode
;
(net-consolidate "enabled")
;(net-consolidate "disabled")

; net-endpoint-mode string
;
; Not fully implemented.
;(net-endpoint-mode "none")
;(net-endpoint-mode "empty")
(net-endpoint-mode "filledbox")

;  net-midpoint-mode string
;
; Not fully implemented.
;(net-midpoint-mode "none")
;(net-midpoint-mode "empty")
(net-midpoint-mode "filledbox")

;  net-direction-mode string
;
;  Controls if the net direction mode is used. This mode tries to guess
;  the best continuation direction of a L-shape net when adding a net.
;
(net-direction-mode "enabled")
;(net-direction-mode "disabled")

;  net-selection-mode string
;
; Controls how many net segments are selected when you click at a net
; If one of the enabled items is used, the selection state will toggle
; through all selection states. The mode defines the maximum search depth
; for the net selection mode
;
;(net-selection-mode "disabled")
(net-selection-mode "net")
;(net-selection-mode "all")

;BEGIN --------------------->  Net Ripper  <------------------------

; Bus ripper controls
; The following keywords control the auto bus ripper addition code
;
; bus-ripper-size     => Sets the size of the auto bus rippers.
; bus-ripper-type     => Sets the bus ripper type either a "component" or
;                        plain "net"
; bus-ripper-symname  => If above is set to component, specify the symbol name.
;                        The symbol must exist in a component library
; bus-ripper-rotation => Either "symmetric" or "non-symmetric".  This deals
;                        with how the bus ripper symbol is rotated when it
;                        is auto added to a schematic.
;

; The default bus ripper setup
(bus-ripper-size 200)
(bus-ripper-type "component")
(bus-ripper-symname "busripper-1.sym")
(bus-ripper-rotation "non-symmetric")

;END ------------------------>  Net Ripper  <-----------------------

;END ==================> Nets and Routing Setup <===================

;BEGIN ------------------------>  Styles  <-------------------------
;  bus-style string
;
;  Set to thin if you want thin buses.
;  Set to thick if you want thick buses.
;  This mode also determines what bus style gets printed
;
;(bus-style "none")
;(bus-style "thin")
(bus-style "thick")

; line-style string
;
; Set to none disable using line width styles.
; Set to thin if you want thin lines
; Set to thick if you want thick lines.
; This mode also determines what line style gets printed
;
(line-style "none")
;(line-style "thin")
;(line-style "thick")

;  net-style string
;
;  Set to thin if you want thin nets.
;  Set to thick if you want thick nets.
;  This mode also determines what net style gets printed
;
;(net-style "none")
(net-style "thin")
;(net-style "thick")

; pin-style string
;
; Set to none disable using line width styles, pins width will be
; based on the pin type, either net or bus.
; Set to thin if you want thin pins
; Set to thick if you want thick pins.
; This mode also determines what pin style gets printed
;
;(pin-style "none")
;(pin-style "thin")
(pin-style "thick")

; Line widths can be set for a given style using the appropriate
; keywords. While defaults are reasonable values, some installations
; may require different values for optimization. One example might
; be a large display operating in a high resolution mode, where
; 5 pixels may appear minute.
;
; Note: If the corresponding xxx-style is NOT used then the internal
; defaults are assigned, in which case: Nets and Pin will be the old
; "narrow" values, Lines will be an over-ridable THIN default, and
; Buses will be over-ridable THICK default. In situtations where
; "tweeking" is NOT required, all the keywords for Bus, Line, Net,
; and Pin styles and widths would be better left commented out.
;
; Or, if you feel a compelling urge, you could uncomment them all,
; along with the styles and just see what you get!
;
(thin-bus-width 15)
(thick-bus-width 30)

(thin-line-width 15)
(thick-line-width 30)

(thin-net-width 15)
(thick-net-width 25)

(thin-pin-width 15)
(thick-pin-width 30)

;END ------------------------->  Styles  <--------------------------

;BEGIN ==============> Pointer Device  Preferences <================

; fast-mousepan string
;
; Controls if text is drawn properly or if a simplified version (a line which
; represents the text string) is drawn during mouse pan. Drawing a simple
; line speeds up mousepan a lot for big schematics
;(fast-mousepan "enabled")
;(fast-mousepan "disabled")

; drag-can-move string
;
; If enabled, the drag movement over selected objects can move the objects.
;(drag-can-move "enabled")
;(drag-can-move "disabled")

; middle-button string
;
; Controls if the middle mouse button draws strokes, repeats the last
; command, does an action (move and copy (holding down the ALT key)
; are supported) on a single objects, or if it does the mouse panning.
;
;(middle-button "Stroke")
;(middle-button "Repeat")
;(middle-button "Action")
;(middle-button "Pan")
;(middle-button "Popup")

; mousepan-gain integer
;
; Controls how much the display pans when using mousepan.  A larger value
; provides greater pan distance when moving the mouse, while a smaller value
; provides a smoother, but smaller pan distance when moving the mouse.
;(mousepan-gain 1)
;;(mousepan-gain 5)
;;(mousepan-gain 10)

; scrollpan-steps non-zero integer
;
; Controls the number of scroll-wheel pan events required to traverse the
; viewed schematic area. Larger numbers mean more scroll steps are required
; to pan across the viewed area and giving finer control over positioning.
; scrollpan-steps can not be 0.
;
;(scrollpan-steps 8)
;;(scrollpan-steps 4) ; Hard-coded behaviour up to version 1.5.0.20080706

; scroll-wheel string
;
; Controls the binding of the mouse scroll wheel.
; "classic" style is the gschem default, where scrolling with no modifier
; key is mapped to zoom, + CTRL -> x-axis pan, + SHIFT -> y-axis pan.
; "gtk" style changes this behaviour to be like other GTK applications,
; which is awkward for normal people, if you are adnormal and want Gschem
; use no modifier-> y-axis pan, + CTRL -> zoom, + SHIFT -> x-axis pan.
; set the scroll-wheel value to "gtk", otherwise use the "classic"
;
;(scroll-wheel "gtk")
;;(scroll-wheel "classic")

; pointer-hscroll string
;
; Controls if horizontal scroll events are enabled for the pointing device.
; Pointers with a horizontal scroll feature integrated with a middle button
; and scroll wheel can have problems with generating a horizontal scroll
; events when attempting to press the middle button. This option allows dis-
; abling of pointer horizontal scroll events so that the integrated middle
; button can be utilized without interference. If the screen always seems to
; pan by itself when attempting to mouse-pan with the scroll-wheel button,
; then try setting this to "disabled".
;
; Enable/disable mouse horizontal scroll events:
;(pointer-hscroll "enabled")
;(pointer-hscroll "disabled")

; third-button string
;
; Controls if the third mouse button performs the popup ("popup") or
; if it does the mouse panning ("mousepan")
;
;(third-button "Popup")
;(third-button "Pan")

; third-button-cancel string
;
; Controls if the third mouse in mousepan mode cancels draw actions such as
; placing of a component or drawing of a primitive
;
(third-button-cancel "enabled")
;(third-button-cancel "disabled")

;END ================> Pointer Device  Preferences <================

;BEGIN ==================>  Printer Related  <======================

; page-size width height
;
; Specifies the size of the default paper size
; Be sure all inputs are reals (floats/doubles) and don't try to reverse
; the values to get a portrait mode.  Code to support that needs to be added
; The code that implements this automatically transforms the dimensions into
; the proper aspect ratio.  All units are in inches. (use output-orientation
; to get portrait mode)
;
(paper-size 11.0 8.5) ; letter
;(paper-size 14.0 8.5) ; legal
;(paper-size 8.5 5.5) ; statement
;(paper-size 17.0 11.0) ; tabloid
;(paper-size 11.0 17.0) ; ledger
;(paper-size 13.0 8.5) ; folio
;(paper-size 10.833 8.472) ; quarto
;(paper-size 14 10) ; 10x14
;(paper-size 10.0 7.5) ; executive
;(paper-size 11.0 8.5) ; A
;(paper-size 17.0 11.0) ; B
;(paper-size 22.0 17.0) ; C
;(paper-size 34.0 22.0) ; D
;(paper-size 44.0 34.0) ; E
;(paper-size 46.81 33.11) ; A0
;(paper-size 33.11 23.39) ; A1
;(paper-size 23.39 16.54) ; A2
;(paper-size 16.54 11.69) ; A3
;(paper-size 11.69 8.27) ; A4
;(paper-size 8.27 5.83) ; A5
;(paper-size 5.83 4.13) ; A6
;(paper-size 4.13 2.91) ; A7
;(paper-size 2.91 2.05) ; A8
;(paper-size 2.05 1.46) ; A9
;(paper-size 1.46 1.02) ; A10
;(paper-size 1.02 0.71) ; A11
;(paper-size 0.71 0.51) ; A12

; paper-sizes string width height
;
; Specifies which paper sizes are available for printing.
; The width and height parameters are in the order for landscape printing,
; so reversing them for portrait won't work just yet. (and that will be
; implemented differently.
; The default paper size is set above.  All units are in inches (forgiveness
; please).
;
; You MUST change this list in this file (gschem-systemrc) if you want
; any reordering changes to take effect.
;
(paper-sizes "Letter : 8.5 in x 11 in" 11.0 8.5)
(paper-sizes "Legal : 8.5 in x 14 in" 14.0 8.5)
(paper-sizes "Statement : 5.5 in x 8.5 in" 8.5 5.5)
(paper-sizes "Tabloid : 11 in x 17 in" 17.0 11.0)
(paper-sizes "Ledger : 17 in x 11 in" 11.0 17.0)
(paper-sizes "Folio : 8.5 in x 13 in" 13.0 8.5)
(paper-sizes "Quarto : 8.472 in x 10.833 in" 10.833 8.472)
(paper-sizes "10x14 : 10 in x 14 in " 14.0 10.0)
(paper-sizes "Executive : 7.5 x 10" 10.0 7.5)
(paper-sizes "A : 8.5 in x 11 in" 11.0 8.5)
(paper-sizes "B : 11 in x 17 in" 17.0 11.0)
(paper-sizes "C : 17 in x 22 in" 22.0 17.0)
(paper-sizes "D : 22 in x 34 in" 34.0 22.0)
(paper-sizes "E : 34 in x 44 in" 44.0 34.0)
(paper-sizes "A0  84.10 cm x 118.90 cm" 46.81 33.11)
(paper-sizes "A1  59.40 cm x 84.10 cm" 33.11 23.39)
(paper-sizes "A2  42.00 cm x 59.40 cm" 23.39 16.54)
(paper-sizes "A3  29.70 cm x 42.00 cm" 16.54 11.69)
(paper-sizes "A4  21.00 cm x 29.70 cm" 11.69 8.27)
(paper-sizes "A5  14.80 cm x 21.00 cm" 8.27 5.83)
(paper-sizes "A6  10.50 cm x 14.80 cm" 5.83 4.13)
(paper-sizes "A7  7.40 cm x 10.50 cm" 4.13 2.91)
(paper-sizes "A8  5.20 cm x 7.40 cm" 2.91 2.05)
(paper-sizes "A9  3.70 cm x 5.20 cm" 2.05 1.46)
(paper-sizes "A10 2.60 cm x 3.70 cm" 1.46 1.02)
(paper-sizes "A11 1.80 cm x 2.60 cm" 1.02 0.71)
(paper-sizes "A12 1.30 cm x 1.80 cm" 0.71 0.51)

; print-command string
;
; The command to send data to in order to print to a printer.  On most
; systems, this will be "lpr".
;
(print-command "lpr")

; output-extents string
;
; Controls what is actually printed
;       string is either "extents" or "extents no margins" or
;       "current window"
;
(output-extents "extents")
;(output-extents "extents no margins")
;(output-extents "current window")
;;; (output-extents "limits")  "limits" is considered deprecated and should
;;;                            not be used.

; output-orientation string
;
; Controls which way the output page is layed out (landscape or portrait)
;
;(output-orientation "portrait")
(output-orientation "landscape")

; output-color string
;
; Controls if output (postscript) is color (enabled) or black/white (disabled)
;
(output-color "disabled")
;(output-color "enabled")

; output-capstyle string
;
; Controls the capstyle at the end of lines in the postscript output
;
(output-capstyle "square")
;(output-capstyle "round")
;(output-capstyle "butt")

; setpagedevice-orientation string
;
; If enabled, puts a << /Orientation x >> setpagedevice into the postscript
; output.  x is either 1 for landscape or 0 for portrait.
;
(setpagedevice-orientation "disabled")
;(setpagedevice-orientation "enabled")

; setpagedevice-pagesize string
;
; If enabled, puts a << /PageSize XxY >> setpagedevice into the postscript
; output.  XxY is the size of the paper in points.
;
(setpagedevice-pagesize "disabled")
;(setpagedevice-pagesize "enabled")

;END ====================>  Printer Related  <======================

;BEGIN =================> General System Options <==================

;  file-preview string
;
;  Controls if the preview area in the File Open/Save As and Component
;  dialog boxes is enabled by default or not
;
(file-preview "enabled")
;(file-preview "disabled")

; handleboxes string
;
; Controls if the handleboxes (which contain the menu and toolbar) are
; visible or not.
;
(handleboxes "enabled")
;(handleboxes "disabled")

; raise-dialog-boxes-on-expose
;
; Controls if dialog boxes are raised whenever an expose event happens
; Default is disabled since gtk2 supposedly handles the raising of these
; dialogs correctly now.
;
;(raise-dialog-boxes-on-expose "enabled")
(raise-dialog-boxes-on-expose "disabled")

; save-ui-settings string
;
; Controls if settings are retained between sessions. Note this does not
; control saving of configurations variables, which can be saved in the
; Preferences Dialog. This feature saves the Main Window size and
; Position, the state of all tool bars, and menu settings.
;
(save-ui-settings "enabled")
;(save-ui-settings "disabled")

; show-full-path string
;
; Controls if application should display the full path in the file name or not.
; This is a libgeda settings.
;
(show-full-path "enabled")
;(show-full-path "disabled")

; toolbars string
;
; Controls if the toolbars are visible or not. If toolbars are not enabled
; here then toolbars can not enabled without restarting.
;
(toolbars "enabled")
;(toolbars "disabled")

; toolbars-mode string
;
; Controls if Icons, Text, or both are displayed on toolbars. This keyword
; will over-ride the settings in gtkrc's. This may be required in certain
; environments. gschem does not default this value so the setting will be
; determined by gtk if this keyword is not set. The last mode, "retention"
; will cause each toolbar to be restored with values preserved from the
; last shutdown when save-ui-settings was "enabled".
;
;(toolbars-mode "only-icons")
;(toolbars-mode "only-text")
;(toolbars-mode "show-both")
;(toolbars-mode "show-both-horiz")
(toolbars-mode "retention")

; show-toolbar-tips string
;
; Controls if tooltips are displayed for toolbars items. Tooltips on toolbars
; is also controlled by the toolbars-mode "retention" but can be over-ridden
; here by explicitly enabling or disabled. When over-ridden , the setting can
; still be changed in the menu for the current session but will revert to the
; choice here with each startup.
;
;(show-toolbar-tips "enabled")
;(show-toolbar-tips "disabled")

; untitled-name string
;
; Specify the default untitled basename (usually only used a startup time)
; And typically not changed at runtime
;
(untitled-name "untitled")

;BEGIN ===================> Scrollbar Options <=====================

; scrollbars string
;
; Controls if the scrollbars are enabled or disabled. If you disable
; the scrollbars, you will not be able to use the scroll wheel on your
; mouse. See scrollbars-visible.
;
;(scrollbars "enabled")
;(scrollbars "disabled")

; scrollbar-update string
;
; Specifies the behavior of the scrollbars in the main window.
;    continuous - display is redrawn as you move the scrollbar
;    delayed - display is redrawn once you stop moving the scrollbar
; Default is continuous
;
;(scrollbar-update "continuous")
;(scrollbar-update "delayed")

; scrollbars-visible string
;
; Controls if the scrollbars are displayed (enabled) or not (disabled)
; If scrollbars-visible is disabled, scrollbars will not be displade
; scroll wheel on the pointer will still be functional.
;
;(scrollbars-visible "enabled")
;(scrollbars-visible "disabled")


;END =====================> Scrollbar Options <=====================
;END =======================> System Options <======================

;BEGIN =================> Set Text Configuration <==================
;
; Sets the default caps style used for the input of text
; lower specifies that all inputed text is in lowercase
; upper specifies that all inputed text is in uppercase
; both specifies that all inputed text is used as is (no case conversion)
;
;(text-case "lower")
;(text-case "upper")
;(text-case "both")

; text-display-zoomfactor integer
;
; Sets the zoom factor at which text is drawn as a box. When the current
; Zoom is at or above this value, text will be drawn as boxes if text-feed
; back is set to "only-when-readable". This parameter is not valid when
; text-feedback is set to "always".
;
;(text-display-zoomfactor 21)

; text-feedback string
;
; Controls if text is rendered when doing an xor action (like copy/move)
; or when the zoom is above the zoom threashold specified by text-display
; -zoomfactor. Comment out if you want the default mode.
;
;(text-feedback "only-when-readable")
(text-feedback "always")

; text-origin-marker string
;
; Controls if the text origin markers are displayed (or not)
;
;(text-origin-marker "enabled")
;(text-origin-marker "disabled")

; text-marker-size integer
;
; Sets the default size of the text marker, this value is ignored
; when the text-origin-marker is disabled.
;
;(text-marker-size 15)

; text-marker-threshold
;
; Sets the default text marker distance threshold. The setting is
; inversely proportional to 10 times the marker distance value at
; which point text markers are drawn. Larger values result in the
; markers being draw at lower magnification. Use smaller values to
; surpress drawing of markers until the desired magnification level
; is reached.
;
;(text-marker-threshold 20)

; text-size integer
;
; Sets the default text size.
;
;(text-size 10)

;END ====================> Text Configuration <=====================

;BEGIN ==================>  Undo Sub-System  <======================

; undo-control string
;
; Controls if the undo is enabled or not
;
;(undo-control "enabled")
;(undo-control "disabled")

; undo-levels number
;
; Determines the number of levels of undo.  Basically this number decides
; how many backup schematics are saved on disk.
;
;(undo-levels 20)

; undo-type string
;
; Controls which kind of undo is used.  The default is to use the disk as
; the storing medium (ie after every action the undo information is stored
; to disk).  The other mechanism uses only memory.  The disk mechanism is
; nice because you get undo-level number of backups of the schematic written
; to disk as backups so you should never lose a schematic due to a crash,
; the memory type is faster doesn't clutter the file system with stranded
; file in the event of a crash.
;
;(undo-type "disk")
;(undo-type "memory")

; undo-panzoom string
;
; Controls if pan or zoom commands are saved in the undo list.  If this
; is enabled then a pan or zoom command will be considered a command and
; can be undone.  If this is false, then panning and zooming is not saved
; in the undo list and cannot be undone.
;
;(undo-panzoom "enabled")
;(undo-panzoom "disabled")

; undo-preserve string
;
; Controls if after the undo operation is performed whether to restore
; the viewport to the values prior to the operation. When undo-panzoom
; is disabled this has the effect of preserving views when undo'ing.
; Note this does not "restore" the view to when the operation being undone
; was performed unless the undo-panzoom is enabled. This means that if
; undo-panzoom is "disabled" and "undo-preserve" is enabled, operations
; may be undone that are not in the current view.
; (User familar with gschem before this feature was implemented that
; disabled undo-panzoom, should try setting both to enabled)
;
;(undo-preserve "disabled")
;(undo-preserve "enabled")


;END ====================>  Undo Sub-System  <======================

; reset-componet-library
;
; When reset-component-library is executed, then all known component library
; paths are erased.  This is useful if the user wants to override all the
; system provided paths and provide his/her own set.  Normally this is not
; commented in.
;
; (reset-component-library)

;----------------------------------------------------------------------------
;
; End of mode related keywords
;

;
; Start of hooks
;
(use-modules (gschem deprecated) (gschem hook))

;; Uncomment this scheme code if you want automatic numbering when
;; placing new component and copying components.
;
;(load-from-path "auto-uref.scm")
;(add-hook! add-component-hook auto-uref)
;(add-hook! copy-component-hook auto-uref)

;; Define value of page-offset for auto number on insert.
;; Refdeses will be numbered from integer multiples of page-offset,
;; depending on the lowest refdes value found on the page.
;; If lowest value is 323 and page offset is 100, then next refdes
;; will be 301.
;; Setting to 0 disables the feature.
;
;(auto-uref-set-page-offset 100)

; Define default pin attributes
; Attributes:
;   - Attribute name.
;   - Value of the attribute.
;   - Visibility: #t (visible) or #f (hidden).
;   - Show_list:  a list containing what to show, using
;                 elements like "name" or "value", or an empty list.
(define default-pin-attributes
       '(("pintype"   "pas"     #f ()        5)
         ("pinlabel"  "unknown" #t ("value") 9)
         ("pinnumber" "0"       #t ("value") 5)
         ("pinseq"    "0"       #f ()        5)))

; Convert a character into a string
(define char2str
  (lambda (char)
    (list->string (list char))))

; Load the default position of attributes, for attribute autoplacing
; functions.
;(load-from-path "default-attrib-positions.scm")

; Adds the default pin attributes to each newly placed pin.
;(define (add-default-pin-attributes object)
;  (for-each
;    (lambda (a)
;      (apply add-attribute-to-object object a)) default-pin-attributes))

; Comment in this hook to automatically add the default attributes to
; each newly placed pin
;(add-hook! add-pin-hook add-default-pin-attributes)

; Comment in this to load the functions to place the attributes automatically.
;(load-from-path "auto-place-attribs.scm")

; Autoplace pin text attributes hook.
; Comment in these if you want the pin attributes to be automatically placed.
; There are different hooks for situations like adding a new pin and rotating
; or mirroring an existing one.
; The #t at the end means that function is appended to the end of the hook.
;(add-hook! add-pin-hook (lambda (pin) (autoplace-pin-attributes pin )) #t)
;(add-hook! rotate-pin-hook (lambda (pin) (autoplace-pin-attributes pin )) #t)
;(add-hook! mirror-pin-hook (lambda (pin) (autoplace-pin-attributes pin )) #t)

; Autoplace component/net/buses text attributes hook.
; Comment in these if you want the component attributes to be
; automatically placed.
; There are different hooks for situations like adding a new pin, rotating
; or mirroring an existing one, adding a new attribute or a new component.
; The #t at the end means that function is appended to the end of the hook.
;(add-hook! add-component-object-hook (lambda (object)
;       (autoplace-object-attributes object)) #t)
;(add-hook! rotate-component-object-hook (lambda (object)
;       (autoplace-object-attributes object)) #t)
;(add-hook! mirror-component-object-hook (lambda (object)
;       (autoplace-object-attributes object)) #t)
;(add-hook! add-attribute-hook (lambda (object)
;       (autoplace-object-attributes object)) #t)
;(add-hook! complex-place-list-changed-hook (lambda (object)
;         (autoplace-object-attributes object)) #t)

; Autoplace netname= attribute hook. This autoplaces netname attributes or
; causes seg-faults if either the load-from-path "default-attrib-positions"
; and load-from-path "auto-place-attribs" above are commented out!
;(load-from-path "auto-place-netname.scm")
;(add-hook! add-objects-hook place-netname-attribute-handler)

;; Automatically place a titleblock (or other components) when creating
;; a new page.
;; Comment in these lines if you want gschem to automatically place a titleblock
;; when you create a new _empty_ page.
;; Users can customize the default titleblock by adding the following line
;; (without the semi-colons at the beginning) to the gschemrc file;
;; (define default-titleblock "title-B.sym")
;; Change "title-B.sym" to the name of your preferred titleblock!
;
;; If you do not want a titleblock to be added automatically, then add one of
;; the following lines to your gschemrc file (without the semicolon).
;; There are several ways, so just choose one;
;;   (define default-titleblock "")
;;   (define default-titleblock '())
;;   (define default-titleblock #f)
;
;(define default-titleblock "title-B.sym")

; Load the regular expressions module
(if (provided? 'regex)
    (use-modules (ice-9 regex))
    (display "Your Guile installation doesn't provide the regex module.\n"))

;(add-hook! (@ (gschem hook) new-page-hook) (lambda (page)
;   ; Only place the titleblock if there are no objects in the page
;   ; and the page filename ends in ".sym".
;   (if (and (null? (get-objects-in-page page))
;            ; If the guile installation doesn't provide the regex module,
;            ; don't care about the page filename.
;            (if (provided? 'regex)
;                (not (string-match ".*\\.[sS][yY][mM]"
;                                   (get-page-filename page)))
;                #t))
;;      Syntax             Symbol name        X   Y    angle selectable mirrored
;      (add-component-at-xy page default-titleblock 40000 40000   0       #f       #f))

   ;; After adding titleblock, reset page to mark as unchanged.
;   ((@ (geda page) set-page-dirty!) (active-page) #f))
;           #t)

; Evaluate an expression entered in the magic-colon text box.
; In 20 years this might dispatch to an interpreter for some other language.
(define (invoke-macro s-expr)
  (gschem-log (format #f "~s\n" (eval-string-protected s-expr))))

;
; End of hooks
;

;
; Start of path related keywords
;

; attribute-name string
;
; Specifies the default attributes which are presented to the user in the
; "Add Attribute" dialog box.
; The main purpose of this keyword is to allow the user to add any attributes
; which should be in this dialog box.
; Some of these names are specific for symbols while others are for general
; components or nets.  The attribute names are case sensitive. (change this?)
;
; The order of the attribute-name keywords determines the order they
; are displayed.
;
(attribute-name "netname")
(attribute-name "footprint")
(attribute-name "value")
(attribute-name "refdes")
(attribute-name "source")
(attribute-name "model-name")
(attribute-name "model")
(attribute-name "net")
(attribute-name "device")
(attribute-name "pinnumber")
(attribute-name "pinseq")
(attribute-name "pintype")
(attribute-name "pinlabel")
(attribute-name "numslots")
(attribute-name "slot")
(attribute-name "slotdef")
(attribute-name "graphical")
(attribute-name "description")
(attribute-name "documentation")
(attribute-name "symversion")
(attribute-name "comment")
(attribute-name "author")
(attribute-name "dist-license")
(attribute-name "use-license")
(attribute-name "file")

;
; End of path related keywords
;

;; Comment in this scheme code if you want to link with pcb
;;
;; Please note that the code in pcb.scm is still highly experimental
;; and there are known (and easy) ways to crash pcb and/or gschem with this code.
;; The short answer is neither program likes a pipe to break.
;;
; (load-from-path "pcb.scm")

