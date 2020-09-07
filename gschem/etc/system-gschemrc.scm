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
; sensitivity errors on  non-existent menu item. This is indicative of a
; syntax error in this file. Maybe one of the easiest mistake is to use a
; colon instead of a semi-colon and no useful information is provided for
; locating the problem. Try using an Editor like gedit or Kate with search
; high-lighting. There are not that many colons in this file and none of
; them should be the first character on a line.
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
;(primitive-load (build-path geda-rc-path "display-colormap-lightbg")) ; light background
;(primitive-load (build-path geda-rc-path "display-colormap-bw")) ; light background, bw
;(primitive-load (build-path geda-rc-path "display-colormap-custom"))

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
; color map scheme, which could be black/white.
;
;(image-color "enabled")
;(image-color "disabled")

; invert-images string
;
; When image-color is disabled, invert-images will black and white so that
; images are black-on-white when enabled (default). When disabled, generated
; images will be the same as the black and white screen image; white-on-
; black (like the old gschem style).
;
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
; defined. Gschem's default, if no option is uncommented is "subpixel".
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
; Controls if the editing grips are drawn when selecting objects.
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
(draw-complex-grips "disabled")

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
; The grid-mode keyword sets the default grid mode that will be active
; at startup.
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
(dots-grid-minor-color 5781 56202 17928)
;(dots-grid-minor-color 42747 41944 3217)

; dots-grid-major-color 3 integers
;
; The dots-grid-major-color specifies the color to use for the major dots grid,
; The color is specified as three integers for red, green, and blue, respectively.
;
;  RED GREEN BLUE
;
(dots-grid-major-color 48830 48830 48830)
;(dots-grid-major-color 22084 3125 63070)
;(dots-grid-major-color 46956 4943 42960)

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
(dots-grid-major-alpha 45)

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
(mesh-grid-minor-alpha 30)

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
; X servers do not handle clipping correctly.
;
;(object-clipping "enabled")
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
;(window-size 900 650)  ; Good size for 1024x768
(window-size 950 712)  ; Good size for 1152x864
;(window-size 1100 825) ; Good size for 1280x1024

; world-size width height border
;
; Specifies the size of the world and a border (in world space units)
; Be sure all inputs are reals (floats/doubles) and don't try to reverse
; the values to get a portrait mode. The values will automatically be
; transformed into the proper aspect ratio. All units are in inches.
; This is not the paper size, which is specified elsewhere. End users
; should not normally change these values.
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
(console-window "enabled")
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

; Attribute autoplacement grid
(define autoplace-attributes-grid 50)

; auto-load-last string
;
; Determines if the last document is automatially loaded if no document is
; specified when gschem is started.
;
(auto-load-last "enabled")
;(auto-load-last "disabled")

; auto-load-last string
;
; Determines if the auto-panning is performed while drawing nets and buses.
;
(auto-pan "enabled")
;(auto-pan "disabled")

; auto-pan-step integer
;
; The number of pixels per pan events during auto-pan events.
; Larger numbers mean more scroll step.
;
;;(auto-pan-step 0)
;;(auto-pan-step  8)
(auto-pan-step 16)

; autosave interval
;
; Controls if a backup copy is made every "interval" seconds.
; Note that the backup copy is made when you make some change to the schematic,
; and there were more than "interval" seconds from the last autosave.
; Autosaving will not be allowed if setting it to zero.
;(auto-save-interval 180)

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
;(continue-component-place "enabled")
;(continue-component-place "disabled")

; embed-components string
;
; Determines if the newly placed components are embedded in the schematic
; or if only the filename is specified and the component is searched for
; instead.  If it is enabled, then all new components will be embedded
; othewise they are not embedded.  This is normally controlled on the fly
; during runtime with the "Embed Component" option on the select component
; dialog box. Setting here
;
;(embed-components "enabled")
;(embed-components "disabled")

;  enforce-hierarchy string
;
;  Controls if the movement between hierarchy levels (of the same underlying
;  schematics) is allowed or not.
;  If this is enabled, then the user cannot (without using the page manager)
;  move between hierarchy levels otherwise, if enabled, the user sees all
;  the hierarchy levels as being flat.
;
;(enforce-hierarchy "enabled")
;(enforce-hierarchy "disabled")

;  hierarchy-up-close string
;
;  Controls if documents are closed during ascension of hierarchy levels.
;  If set to "both", the underlying schematic and symbols will be closed
;  during ascension. If set to "sym", only symbols will be closed during
;  ascension.  If set to "none", no douments will be closed when ascending
;  hierarchy levels.
;
(hierarchy-up-close "none")
;(hierarchy-up-close "sym")
;(hierarchy-up-close "both")

; force-boundingbox string
;
; Controls if the entire bounding box of a symbol is used when figuring out
; whichend of the pin is considered the active port.  Enable this when
; gschem is guessing incorrectly.
;
;(force-boundingbox "enabled")
;(force-boundingbox "disabled")

; keyboardpan-gain integer
;
; Controls how much the display pans when using the keyboard cursor keys.
; A larger value provides greater pan distance when pressing the cursor
; keys, while a smaller value provides a smoother, but smaller pan
; distance when moving the cursor keys.
;(keyboardpan-gain 20)
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
;(netconn-rubberband "enabled")
;(netconn-rubberband "disabled")

; select-slack-pixels integer
;
; Controls how many pixels around an object can still be clicked as part of
; that object.
; A larger value gives greater ease in selecting small, or narrow objects.
;(select-slack-pixels 10)
;;(select-slack-pixels 4)
;;(select-slack-pixels 0)
;;(select-slack-pixels 1)

; snap-size number
;
; Sets the default snap spacing at start-up of gschem. This is generally
; handy to leave enabled at a system level, so gschem does not start-up
; with an odd snap size.
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
;(sort-component-library "disabled")

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
; are supported) on a single objects, does the mouse panning, or if
; it displays a contect popup menu.
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
;  dialog boxes is enabled by default.
;
(file-preview "enabled")
;(file-preview "disabled")

; handleboxes string
;
; Controls if the handleboxes (which contain the menu and toolbars) are
; enabled or not.
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
;(show-full-path "enabled")
(show-full-path "disabled")

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
; And typically not changed at runtime.
;
; This is a libgeda settings.
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
; If scrollbars-visible is disabled, scrollbars will not be displayed
; but scroll wheel on the pointer will still be functional.
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
; or when the zoom is above the zoom threshold specified by text-display
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
;(text-marker-size 16)

; text-marker-threshold
;
; Sets the default text marker distance threshold. The setting is
; inversely proportional to 10 times the marker distance value at
; which point text markers are drawn. Larger values result in the
; markers being draw at lower magnification. Use smaller values to
; surpress drawing of markers until the desired magnification level
; is reached.
;
;(text-marker-threshold 12)

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
; in the undo list and cannot be undone.  Note, the current viewport
; information is saved for every command, so the display will change to the
; viewport before a command is executed.
;
;(undo-panzoom "enabled")
;(undo-panzoom "disabled")

; undo-preserve string
;
; Controls if after the undo operation is performed whether to restore
; the viewport to the values prior to the operation. When undo-panzoom
; is disabled this has the effect of preserving views when undo'ing.
; If undo-panzoom is enabled this keyword has no effect.
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
(load-from-path "auto-uref.scm")
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
(load-from-path "default-attrib-positions.scm")

; Adds the default pin attributes to each newly placed pin.
(define (add-default-pin-attributes object)
  (for-each
    (lambda (a)
      (apply add-attribute-to-object object a)) default-pin-attributes))

; Comment in this hook to automatically add the default attributes to
; each newly placed pin
(add-hook! add-pin-hook add-default-pin-attributes)

; Comment in this to load the functions to place the attributes automatically.
(load-from-path "auto-place-attribs.scm")

(define (reset-attribute-positions object)
  (autoplace-object-attributes object))

; Autoplace pin text attributes hook.
; Comment in these if you want the pin attributes to be automatically placed.
; There are different hooks for situations like adding a new pin and rotating
; or mirroring an existing one. load-from-path "default-attrib-positions" and
; load-from-path "auto-place-attribs" above must NOT be commented out!
; The #t at the end means that function is appended to the end of the hook.
(add-hook! add-pin-hook (lambda (pin) (autoplace-pin-attributes pin )) #t)
;(add-hook! rotate-pin-hook (lambda (pin) (autoplace-pin-attributes pin )) #t)
;(add-hook! mirror-pin-hook (lambda (pin) (autoplace-pin-attributes pin )) #t)

; Autoplace component/net/buses text attributes hook.
; Uncomment in these if you want the component attributes to be
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

; Autoplace netname= attribute hook. This autoplaces netname attributes.
(load-from-path "auto-place-netname.scm")
(add-hook! add-objects-hook place-netname-attribute-handler)

;; Automatically place a titleblock (or other components) when creating
;; a new page.
;; Uncomment in these lines if you want gschem to automatically place a titleblock
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
;   (if (and (null? (page-contents page))
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
; components or nets. The attribute names are case sensitive. (change this?)
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

;
; Start of stroke related keywords
;

;
; This section defines associations between a stroke sequence and a
; guile function which is executed when the stroke is drawn in the
; gschem window
;
; Strokes are defined as follows;
;
; 1  2  3
;
; 4  5  6
;
; 7  8  9
;
; The sequence of number such as "852" specify how the stroke is drawn.
; Sequence "852" happens to be a vertical line drawn from the bottom going
; up.
;
; Please see the libstroke documentation for further information on the
; stroke description.
;
; For the most part I went a little overboard on the stroke defs, you
; probably can get away with many less stroke defs, but I'm a very
; sloppy stroke drawing person.
;
; Be careful here, strokes is a rather large list, and make sure you
; maintain proper ( and )'s.
;

(define strokes
; Letter L for line
  '(("14789" . add-line)

; Letter i for Insert component (without dot)
("852" . add-component)
("258" . add-component)

; Letter C for copy
("3214789" . edit-copy)
("214789" . edit-copy)
("21489" . edit-copy)
("32478" . edit-copy)

; Letter D for delete
("14786321" . edit-delete)
("14789621" . edit-delete)
("147896321" . edit-delete)
("15896321" . edit-delete)
("257896321" . edit-delete)
("25896321" . edit-delete)
("4789621" . edit-delete)
("741236987" . edit-delete)

; Letter B for Box
("7417654456987" . add-box)

; Letter E for edit
("563214789" . edit-attributes)
("53214789" . edit-attributes)
("5321478" . edit-attributes)
("5214789" . edit-attributes)
("521478" . edit-attributes)
("453214789" . edit-attributes)
("45321478" . edit-attributes)
("456321478" . edit-attributes)
("456214789" . edit-attributes)
("45621478" . edit-attributes)

; Letter N for net
("415963" . add-net)
("7414863" . add-net)
("74148963" . add-net)
("74158963" . add-net)
("7415963" .  add-net)
("7418963" .  add-net)

; Letter M for move
("741236963" . edit-move)
("7412572369" . edit-move)
("7412575369" . edit-move)
("741258369" . edit-move)
("74125852369" . edit-move)
("7412585369" . edit-move)
("74125863" . edit-move)
("74126963" . edit-move)
("741475369" . edit-move)
("7414785369" . edit-move)
("74148369" . edit-move)
("7414852369" . edit-move)
("741485369" . edit-move)
("74148669" . edit-move)
("741552369" . edit-move)
("741575369" . edit-move)
("7415852369" . edit-move)
("741585369" . edit-move)
("74185369" . edit-move)
("74255369" . edit-move)
("7425852369" . edit-move)
("742585369" . edit-move)
("7426963" . edit-move)
("74585369" . edit-move)

; Capital Letter O for Open
("12369874" . file-open)
("74123698" . file-open)

; Letter S for save
("2145987" . edit-select )
("215987" . edit-select )
("2156987" . edit-select )
("21256987" . edit-select )
("3215987" . edit-select )
("32156987" . edit-select )
("32148987" . edit-select )
("32145987" . edit-select )

; Letter U for undo
("1478963" . edit-undo )

; Letter Z for zoom window
("125789" . view-zoom-box)
("1254789" . view-zoom-box)
("1235789" . view-zoom-box)
("2354789" . view-zoom-box)
("2324789" . view-zoom-box)
("12354789" . view-zoom-box)
("12324789" . view-zoom-box)
("12365789" . view-zoom-box)
("1232789" . view-zoom-box)

))

;
; End of stroke related keywords
;

;
; Start of keymapping related keywords
;

;;;; Keymapping
;;
;; Everything is case-sensitive.  Any number of keys may be bound in
;; sequence, and each keystroke consists of a non-modifier key with
;; some number of modifiers applied.  Examples:
;;
;;  *(map-keys "F N" 'file-new)
;;
;;    The "New Window" command will be run when an <F> is typed,
;;    followed by an <A>.
;;
;;  *(map-keys "<Control><Shift>A" 'edit-deselect)
;;
;;    The "Deselect All" command will be run when the <Ctrl> and
;;    <Shift> keys are held down, and the <A> key is pressed.
;;
;;  *(map-keys "O <Shift>S" 'options-snapsize)
;;
;;    The "Snap Size" dialog box will be shown when an <O> is typed,
;;    followed by an <S> typed with the <Shift> key held down.
;;
;; Key names can be found in /usr/include/gtk-2.0/gdk/gdkkeysyms.h on
;; most Linux systems. For other systems, please see your platform
;; documentation, or see libgedauio/include/geda_keysyms.h
;;
;; Later keybindings override earlier ones.

(map-keys "A C" "add-component")
(map-keys "A A" "add-attribute")
(map-keys "A N" "add-net")
(map-keys "A U" "add-bus")
(map-keys "A T" "add-text")
(map-keys "A L" "add-line")
(map-keys "A H" "add-path")
(map-keys "A B" "add-box")
(map-keys "A I" "add-circle")
(map-keys "A R" "add-arc")
(map-keys "A P" "add-pin")
(map-keys "A G" "add-picture")

(map-keys "B A"        "attributes-attach")
(map-keys "B D"        "attributes-detach")
(map-keys "B N"        "attributes-show-name")
(map-keys "B V"        "attributes-show-value")
(map-keys "B B"        "attributes-show-both")
(map-keys "B T"        "attributes-visibility")
(map-keys "B F"        "attributes-find-text")
(map-keys "B H"        "attributes-hide-text")
(map-keys "B <Shift>H" "attributes-show-text")
(map-keys "B E"        "attributes-editor")
(map-keys "B R"        "attributes-home")

(map-keys "<Control>A"        "edit-select-all")
(map-keys "<Control><Shift>A" "edit-deselect")
(map-keys "<Control><Shift>I" "edit-select-invert")

;;(map-keys "B"          "add-box")
(map-keys "<Shift>B"   "add-bus")
(map-keys "C"          "edit-copy")
(map-keys "<Control>C" "clipboard-copy")
;(map-keys "D"          "edit-delete")

(map-keys "E A"        "edit-array")
(map-keys "E B"        "edit-break")
(map-keys "E C"        "edit-copy")
(map-keys "E D"        "edit-delete")
(map-keys "E <Shift>A" "edit-attributes")
(map-keys "E Y"        "edit-mcopy")
(map-keys "E X"        "edit-text")
(map-keys "E M"        "edit-move")
(map-keys "E N"        "edit-snap")
(map-keys "E P"        "edit-extend")
(map-keys "E R"        "edit-rotate-left")
(map-keys "E T"        "edit-rotate-right")
(map-keys "E O"        "edit-offset")
(map-keys "E I"        "edit-mirror")
(map-keys "E <Shift>C" "edit-color")
(map-keys "E <Shift>P" "edit-pintype")
(map-keys "E <Shift>L" "edit-linetype")
(map-keys "E <Shift>F" "edit-filltype")
(map-keys "E <Shift>N" "edit-component")
(map-keys "E <Shift>S" "edit-slot")

(map-keys "E <Shift>R" "edit-redo")
(map-keys "E <Shift>U" "edit-undo")

(map-keys "F N"        "file-new")
(map-keys "F W"        "file-new-window")
(map-keys "F O"        "file-open")
(map-keys "F S"        "file-save")
(map-keys "F C"        "file-close")
(map-keys "F A"        "file-save-as")
(map-keys "F L"        "file-save-all")
(map-keys "F M"        "file-save-modified")
(map-keys "F P"        "file-print")
(map-keys "F R"        "page-revert")
(map-keys "F V"        "page-revert-all")
(map-keys "F I"        "file-write-image")
(map-keys "F F"        "file-write-pdf")
(map-keys "F T"        "file-run-script")
(map-keys "<Shift>C A" "file-close-all")
(map-keys "F Q"        "file-quit")

(map-keys "<Control>F" "attributes-find-text")

(map-keys "H A"        "help-show-about")
(map-keys "H C"        "hierarchy-documentation")
(map-keys "H F"        "help-show-faq")
(map-keys "H G"        "help-show-geda")
;;(map-keys "H H"        "help-show-faq")
(map-keys "H M"        "help-show-manual")
(map-keys "H K"        "help-show-hotkeys")
(map-keys "H W"        "help-show-wiki")

(map-keys "<Shift>H D" "hierarchy-down-schematic")
(map-keys "<Shift>H S" "hierarchy-down-symbol")
(map-keys "<Shift>H U" "hierarchy-up")

(map-keys "I"          "add-component")
(map-keys "L"          "add-line")
(map-keys "M"          "edit-move")
(map-keys "N"          "add-net")

(map-keys "O A"        "options-auto-pan")
(map-keys "O B"        "options-action-feedback")
(map-keys "O D"        "options-dragcanmove")
(map-keys "O G"        "options-cycle-grid")
(map-keys "O S"        "options-cycle-snap")
(map-keys "O R"        "options-rubberband")
(map-keys "O M"        "options-magneticnet")
(map-keys "O <Shift>S" "options-snap-size")
(map-keys "O T"        "options-show-text-size")
(map-keys "O P"        "options-show-settings")

(map-keys "P C"        "page-close")
(map-keys "P D"        "page-down")
(map-keys "P F"        "page-first")
(map-keys "P L"        "page-last")
(map-keys "P M"        "page-manager")
(map-keys "P N"        "page-next")
(map-keys "P P"        "page-prev")
(map-keys "P R"        "page-revert")
(map-keys "P U"        "page-up")
(map-keys "P W"        "page-new")

(map-keys "P <Shift>D" "page-discard")
(map-keys "P <Shift>P" "page-print")

(map-keys "<Alt>Q"     "file-quit")
(map-keys "R"          "view-redraw")
(map-keys "<Shift>R"   "edit-redo")

;(map-keys "S"          "edit-select")
(map-keys "<Shift>S G" "edit-snap")

(map-keys "S S"        "edit-select")
(map-keys "S T"        "edit-select-last")
(map-keys "S L"        "edit-lock")
(map-keys "S U"        "edit-unlock")

(map-keys "T N"            "tools-autonumber")
(map-keys "T C"            "tools-show-console")
(map-keys "T G"            "tools-guile-path")
(map-keys "T O"            "tools-show-coordinates")
;; see scheme/gschem/gschem-tools-menu.scm
(map-keys "T <Shift>colon" "tools-invoke-macro")
(map-keys "T E"            "tools-embed")
(map-keys "T U"            "tools-unembed")
(map-keys "T P"            "tools-update")
;(map-keys "T <Shift>R"     "tools-repl")

(map-keys "U"             "edit-undo")
(map-keys "<Shift>U"      "edit-undo")

(map-keys "D A"           "page-draw-after")
(map-keys "D B"           "page-draw-before")
(map-keys "D F"           "page-draw-first")
(map-keys "D L"           "page-draw-last")

(map-keys "V A"           "view-zoom-all")
(map-keys "V B"           "view-zoom-box")
(map-keys "V E"           "view-zoom-extents")
(map-keys "V S"           "view-zoom-selected")
(map-keys "V I"           "view-zoom-in")
(map-keys "V O"           "view-zoom-out")
(map-keys "V P"           "view-pan")
(map-keys "V R"           "view-redraw")
(map-keys "V D"           "view-documentation")
(map-keys "V H"           "view-show-hidden")
(map-keys "V T"           "view-show-inherited")
(map-keys "V M"           "view-zoom-to-mag")
(map-keys "V N"           "view-show-nets")
(map-keys "V K"           "view-dark-colors")
(map-keys "V L"           "view-light-colors")
(map-keys "V W"           "view-bw-colors")

(map-keys "<Control>V"    "clipboard-paste")
(map-keys "W"             "view-zoom-box")
(map-keys "X"             "view-pan")
(map-keys "<Control>X"    "clipboard-cut")

(map-keys "Y C 1"         "buffer-copy1")
(map-keys "Y U 1"         "buffer-cut1")
(map-keys "Y P 1"         "buffer-paste1")
(map-keys "Y C 2"         "buffer-copy2")
(map-keys "Y U 2"         "buffer-cut2")
(map-keys "Y P 2"         "buffer-paste2")
(map-keys "Y C 3"         "buffer-copy3")
(map-keys "Y U 3"         "buffer-cut3")
(map-keys "Y P 3"         "buffer-paste3")
(map-keys "Y C 4"         "buffer-copy4")
(map-keys "Y U 4"         "buffer-cut4")
(map-keys "Y P 4"         "buffer-paste4")
(map-keys "Y C 5"         "buffer-copy5")
(map-keys "Y U 5"         "buffer-cut5")
(map-keys "Y P 5"         "buffer-paste5")

(map-keys "<Control>Y"    "edit-redo")
(map-keys "Z"             "view-zoom-in")
(map-keys "<Shift>Z"      "view-zoom-out")
(map-keys "<Control>Z"    "edit-undo")

(map-keys "Escape"        "cancel")
(map-keys "bracketright"  "scale-up-snap-size")
(map-keys "bracketleft"   "scale-down-snap-size")
(map-keys "Left"          "view-pan-left")
(map-keys "Right"         "view-pan-right")
(map-keys "Up"            "view-pan-up")
(map-keys "Down"          "view-pan-down")
(map-keys "period"        "repeat-last")
(map-keys "colon"         "tools-invoke-macro")

(map-keys "minus"         "view-zoom-out")
(map-keys "plus"          "view-zoom-in")

(map-keys "Delete"        "edit-delete")
(map-keys "greater"       "page-down")
(map-keys "Page_Down"     "page-down")
(map-keys "less"          "page-up")
(map-keys "Page_Up"       "page-up")

(map-keys "<Alt>Left"     "page-up")
(map-keys "<Alt>Right"    "page-down")

(map-keys "F1"            "help-show-manual")
(map-keys "F2"            "tools-show-console")
(map-keys "F3"            "options-magneticnet")
(map-keys "F4"            "edit-attributes")
(map-keys "F5"            "page-revert")
;(map-keys "F6"            "options-cycle-coord")
(map-keys "F7"            "options-cycle-grid")
;(map-keys "F8"            "options-toggle-ortho")
(map-keys "F9"            "options-cycle-snap")
(map-keys "F11"           "options-rubberband")
(map-keys "F12"           "edit-component")

(map-keys "<Shift>F5"     "tools-update")

; Map icons to actions
(map-icon "gtk-new"                    "file-new")
(map-icon "gtk-open"                   "file-open")
(map-icon "gtk-save"                   "file-save")
(map-icon "gtk-save-as"                "file-save-as")
(map-icon "gtk-save"                   "file-save-all")
(map-icon "gtk-save"                   "file-save-modified")
;(map-icon "gtk-print"                  "file-print")
(map-icon "gschem-print-document"      "file-print")
(map-icon "gaf-pdf"                    "file-write-pdf")
(map-icon "geda-save-image"            "file-write-image")

(map-icon "gtk-execute"                "file-run-script")
;;(map-icon "gtk-close"                 "file-close")
(map-icon "geda-close"                 "file-close")
(map-icon "geda-close-all"             "file-close-all")
(map-icon "gtk-quit"                   "file-quit")

(map-icon "geda-undo"                  "edit-undo")
(map-icon "geda-redo"                  "edit-redo")
;;(map-icon "edit-undo"                  "edit-undo")
;;(map-icon "edit-redo"                  "edit-redo")

(map-icon "gtk-cut"                    "clipboard-cut")
(map-icon "gtk-copy"                   "clipboard-copy")
(map-icon "gtk-paste"                  "clipboard-paste")
(map-icon "gtk-delete"                 "edit-delete")

(map-icon "geda-copy"                  "edit-copy")
(map-icon "geda-multi"                 "edit-mcopy")
(map-icon "geda-mirror"                "edit-mirror")
(map-icon "geda-move"                  "edit-move")
(map-icon "geda-offset"                "edit-offset")
(map-icon "geda-rotate-left"           "edit-rotate-left")
(map-icon "geda-snap"                  "edit-snap")

(map-icon "gschem-array"               "edit-array")
(map-icon "break"                      "edit-break")
(map-icon "extend"                     "edit-extend")

(map-icon "gtk-indent"                 "edit-attributes")
(map-icon "gtk-edit"                   "edit-text" )
;(map-icon "gtk-select-color"           "edit-color")
(map-icon "geda-display-color"         "edit-color")
(map-icon "geda-pin-type"              "edit-pintype")
(map-icon "geda-line-type"             "edit-linetype")
(map-icon "geda-mesh"                  "edit-filltype")
(map-icon "geda-properties"            "edit-component")
(map-icon "geda-slot"                  "edit-slot")

(map-icon "gschem-select"              "edit-select")
(map-icon "gschem-unselect"            "edit-deselect")
(map-icon "gschem-select-all"          "edit-select-all")
(map-icon "geda-unselect-all"          "edit-deselect-all")
(map-icon "gschem-invert"              "edit-select-invert")

(map-icon "gtk-copy"                   "buffer-copy1")
(map-icon "gtk-copy"                   "buffer-copy2")
(map-icon "gtk-copy"                   "buffer-copy3")
(map-icon "gtk-copy"                   "buffer-copy4")
(map-icon "gtk-copy"                   "buffer-copy5")

(map-icon "gtk-cut"                    "buffer-cut1")
(map-icon "gtk-cut"                    "buffer-cut2")
(map-icon "gtk-cut"                    "buffer-cut3")
(map-icon "gtk-cut"                    "buffer-cut4")
(map-icon "gtk-cut"                    "buffer-cut5")

(map-icon "gtk-paste"                  "buffer-paste1")
(map-icon "gtk-paste"                  "buffer-paste2")
(map-icon "gtk-paste"                  "buffer-paste3")
(map-icon "gtk-paste"                  "buffer-paste4")
(map-icon "gtk-paste"                  "buffer-paste5")

(map-icon "geda-lock"                  "edit-lock")
(map-icon "geda-unlock"                "edit-unlock")

(map-icon "geda-zoom-pan"              "view-pan")
(map-icon "geda-view-redraw"           "view-redraw")
;(map-icon "gtk-refresh"                "view-redraw")

;(map-icon "gtk-fullscreen"             "view-zoom-all")
(map-icon "geda-zoom-limits"           "view-zoom-all")
(map-icon "geda-zoom-box"              "view-zoom-box")
(map-icon "geda-zoom-extents"          "view-zoom-extents")
(map-icon "geda-zoom-in"               "view-zoom-in")
(map-icon "geda-zoom-out"              "view-zoom-out")
(map-icon "geda-zoom-mag"              "view-zoom-to-mag")
(map-icon "geda-zoom-selection"        "view-zoom-selected")

;(map-icon "zoom-limits"               "view-zoom-all")
;(map-icon "zoom-fit"                  "view-zoom-box")
;(map-icon "zoom-extents"              "view-zoom-extents")
;(map-icon "zoom-in"                   "view-zoom-in")
;(map-icon "zoom-out"                  "view-zoom-out")
;(map-icon "zoom-mag"                  "view-zoom-to-mag")
;(map-icon "zoom-selection"            "view-zoom-selected")

(map-icon "gaf-see-notes"              "view-documentation")
;;(map-icon "gtk-index"                  "view-documentation")
(map-icon "show-hidden"                "view-show-hidden")
(map-icon "show-inherited"             "view-show-inherited")
(map-icon "geda-show-nets"             "view-show-nets")
;(map-icon "show-netnames"              "view-show-nets")

;(map-icon #f                          "view-dark-colors")
;(map-icon #f                          "view-light-colors")
;(map-icon #f                          "view-bw-colors")

;(map-icon "gtk-properties"             "page-manager")
(map-icon "gschem-page-man"               "page-manager")

(map-icon "gtk-goto-first"             "page-first")
(map-icon "gtk-go-back"                "page-prev")
(map-icon "gtk-go-forward"             "page-next")
(map-icon "gtk-go-up"                  "page-up")
(map-icon "gtk-go-down"                "page-down")
(map-icon "gtk-goto-last"              "page-last")
(map-icon "gtk-new"                    "page-new")

(map-icon "gschem-print-document"      "page-print")
;(map-icon "gtk-print"                  "page-print")

(map-icon "gtk-revert-to-saved"        "page-revert")
(map-icon "gtk-revert-to-saved"        "page-revert-all")
(map-icon "gtk-close"                  "page-close")
(map-icon "gtk-discard"                "page-discard" )
(map-icon "gtk-sort-ascending"         "page-draw-before")
(map-icon "gtk-sort-descending"        "page-draw-after")
(map-icon "gtk-sort-ascending"         "page-draw-last")
(map-icon "gtk-sort-descending"        "page-draw-first")

;(map-icon "gaf-hierarchy-sch"          "hierarchy-down-schematic")
;(map-icon "gaf-hierarchy-sym"          "hierarchy-down-symbol")
;(map-icon "gaf-hierarchy-up"           "hierarchy-up")
(map-icon "gtk-go-down"                "hierarchy-down-schematic")
(map-icon "gtk-goto-bottom"            "hierarchy-down-symbol")
(map-icon "gtk-go-up"                  "hierarchy-up")

;(map-icon "gtk-info"                  "hierarchy-documentation")
;(map-icon "gtk-indent"                "hierarchy-documentation")
(map-icon "gtk-index"                  "hierarchy-documentation")

;(map-icon "gschem-comp"               "add-component")
;(map-icon "gschem-transistor"         "add-component")
(map-icon "geda-component"             "add-component")

(map-icon "gschem-net"                 "add-net")
(map-icon "gschem-bus"                 "add-bus")
(map-icon "gaf-add-attribute"          "add-attribute")
;(map-icon "insert-attribute"           "add-attribute")
(map-icon "gschem-text"                "add-text")
;(map-icon "gtk-bold"                   "add-text")
(map-icon "geda-arc"                   "add-arc")
(map-icon "geda-box"                   "add-box")
(map-icon "geda-circle"                "add-circle")
(map-icon "geda-line"                  "add-line")
(map-icon "geda-path"                  "add-path")
(map-icon "gtk-orientation-portrait"   "add-picture")
(map-icon "geda-pin"                   "add-pin")

(map-icon "gtk-go-down"                "session-new")
(map-icon "geda-open-recent"           "session-open")
(map-icon "gtk-info"                   "session-save")
(map-icon "geda-open-recent"           "session-save-as")
(map-icon "gtk-properties"             "session-manage")

(map-icon "gaf-demote"                 "attributes-detach")
(map-icon "gaf-promote"                "attributes-attach")
;(map-icon "geda-demote"                "attributes-attach")
;(map-icon "geda-promote"               "attributes-detach")
;(map-icon "gtk-go-up"                  "attributes-attach")
;(map-icon "gtk-go-down"                "attributes-detach")

;(map-icon "show-value"                "attributes-show-value")
;(map-icon "show-name"                 "attributes-show-name")
;(map-icon "show-both"                 "attributes-show-both")
(map-icon "geda-value"                 "attributes-show-value")
(map-icon "geda-name-tag"              "attributes-show-name")
(map-icon "geda-name-value"            "attributes-show-both")
(map-icon "attribute-visibility"       "attributes-visibility")
(map-icon "attribute-reset"            "attributes-home")
(map-icon "gtk-find-and-replace"       "attributes-find-text")
(map-icon "gtk-clear"                  "attributes-hide-text")
;(map-icon  #f                         "attributes-show-text")
(map-icon  "gtk-indent"                "attributes-editor")

(map-icon "geda-autonum-blue"          "tools-autonumber")
;(map-icon #f                          "tools-show-console")
;(map-icon #f                          "tools-show-coordinates")
(map-icon "gtk-execute"                "tools-invoke-macro")
(map-icon "guile-logo"                 "tools-guile-path")
;(map-icon "gtk-convert"                "tools-translate")
(map-icon "geda-translate"             "tools-translate")

(map-icon "gtk-refresh"                "tools-update")
(map-icon "geda-inbed"                 "tools-embed")
(map-icon "geda-bed"                   "tools-unembed")

(map-icon "gtk-jump-to"                "options-cycle-grid")

(map-icon "gtk-goto-top"               "scale-up-snap-size")
(map-icon "gtk-goto-bottom"            "scale-down-snap-size")
(map-icon "geda-magnet"                "options-snap-size")

;(map-icon #f                          "options-show-text-size")
;(map-icon "gaf-tools"                  "options-show-settings")
(map-icon "geda-tools"                 "options-show-settings")
;(map-icon "gtk-preferences"            "options-show-settings")

(map-icon "gtk-help"                   "help-show-manual")
(map-icon "keyboard-shortcuts"         "help-show-hotkeys")
(map-icon "help-faq"                   "help-show-faq")

(map-icon "symbol-datasheet"           "help-show-geda")
(map-icon "web-browser"                "help-show-wiki")
(map-icon "gtk-about"                  "help-show-about")

; Definitions for the top pull down menu bar
;
; The "menu item name" is the name of the item as it will appear in the menu
; The "menu action" is the scheme function which is executed when the item
; is selected off of the menu.
; The hotkeys which are displayed are defined by the global-keymap. Actions
; can have several hotkeys, but the displayed keys are the last ones found.
;
; The SEPARATOR keyword is case sensitive and adds a separator into the menu.
;

(or (defined? 'define-syntax)
    (use-modules (ice-9 syncase)))

(define T_
  (lambda (text)
    (string-append "Toggle " text)))

;; Define a no-op macro for flagging strings as translatable.
(define-syntax N_
  (syntax-rules ()
    ((N_ expr) expr)))

(define file-menu-items
;;
;;      menu item name             menu action          menu icon name             Menu Item Tooltip
;;
     `( (,(N_ "_New")              file-new             "gtk-new"             ,(N_ "Create a new file" ))
        (,(N_ "_Open...")          file-open            "gtk-open"            ,(N_ "Open an existing schematic or symbol file"))
        (,(N_ "Open Recen_t")      #f                   "geda-open-recent"    ,(N_ "Open recently accessed schematic or symbol files"))
       ;(,(N_ "New Window")        file-new-window      "window-new"          ,(N_ "Create a new window"))

        ("SEPARATOR"               #f                   #f)
        (,(N_ "_Save")             file-save            "gtk-save"            ,(N_ "Save the current document"))
        (,(N_ "Save _As...")       file-save-as         "gtk-save-as"         ,(N_ "Save the current document to a new name or location"))
        (,(N_ "Save A_ll")         file-save-all        "gtk-save"            ,(N_ "Save all open documents"))
        (,(N_ "Save _Modified")    file-save-modified   "gtk-save"            ,(N_ "Save all modified documents"))
        (,(N_ "_Revert")           page-revert          "gtk-revert-to-saved" ,(N_ "Discard changes and reload the current document"))
        (,(N_ "Re_vert All")       page-revert-all      "gtk-revert-to-saved" ,(N_ "Discard changes and reload all open documents"))

        ("SEPARATOR"               #f                   #f)
        (,(N_ "_Print...")         file-print           #f                    ,(N_ "Open the Print Dialog"))
        (,(N_ "_Export")           #f                   "gtk-save-as"         ,(N_ "Export options"))
        (,(N_ "Write p_df")        file-write-pdf       #f                    ,(N_ "Create PDF document"))
        (,(N_ "Write _image...")   file-write-image     "geda-save-image"     ,(N_ "Export image"))

        ("SEPARATOR"               #f                   #f)
        (,(N_ "E_xecute Script...")file-run-script      "gtk-execute"         ,(N_ "Execute a script file"))

        ("SEPARATOR"               #f                   #f)
        (,(N_ "_Close")            file-close           "gtk-close"           ,(N_ "Close the current document"))
        (,(N_ "Close All")         file-close-all       "geda-close-all"      ,(N_ "Close all open documents"))
        (,(N_ "_Quit")             file-quit            "gtk-quit"            ,(N_ "Quit gschem and exit"))
      )
)

(define edit-menu-items
;;
;;      menu item name              menu action            menu icon name          Menu Item Tooltip
;;
     `( (,(N_ "_Undo")              edit-undo             #f                  ,(N_ "Undo the last operation"))
        (,(N_ "_Redo")              edit-redo             #f                  ,(N_ "Redo the last undo"))

        ("SEPARATOR"               #f                     #f)
        (,(N_ "Cu_t clipboard")     clipboard-cut          "gtk-cut"          ,(N_ "Cut the current selection to the system clipboard"))
        (,(N_ "_Copy clipboard")    clipboard-copy         "gtk-copy"         ,(N_ "Copy the current selection to the system clipboard"))
        (,(N_ "_Paste clipboard")   clipboard-paste        "gtk-paste"        ,(N_ "Paste the contents of the system clipboard"))
        (,(N_ "_Delete")            edit-delete            "gtk-delete"       ,(N_ "Delete the current selection" ))

        ("SEPARATOR"               #f                     #f)
        (,(N_ "C_opy")              edit-copy              "geda-copy"        ,(N_ "Copy selection"))
        (,(N_ "_Multiple Copy")     edit-mcopy             "geda-multi"       ,(N_ "Make multiple copies of selection"))
        (,(N_ "M_irror")            edit-mirror            "geda-mirror"      ,(N_ "Mirror an object about a point"))
        (,(N_ "Mo_ve")              edit-move              "geda-move"        ,(N_ "Move selection"))
        (,(N_ "O_ffset")            edit-offset            #f                 ,(N_ "Offset the selected objects"))
        (,(N_ "Rotate _90")         edit-rotate-left       #f                 ,(N_ "Rotate the current selection about a point"))
        (,(N_ "Snap to _grid")      edit-snap              "geda-snap"        ,(N_ "Snap selection to current grid"))

        ("SEPARATOR"               #f                     #f)
        (,(N_ "_Array")             edit-array            #f                  ,(N_ "Create and array of objects"))
        (,(N_ "_Break")             edit-break            #f                  ,(N_ "Break an object into separate objects"))
        (,(N_ "E_xtend")            edit-extend           #f                  ,(N_ "Project linear objects to other objects"))

        ("SEPARATOR"               #f                     #f)
        (,(N_ "_Edit...")           edit-attributes       #f                  ,(N_ "Edit Object Attributes"))
        (,(N_ "E_dit Text...")      edit-text              "gtk-edit"         ,(N_ "Open the Text Editor Dialog"))
        (,(N_ "Co_lor...")           edit-color           #f                  ,(N_ "Open the Color Editor Dialog"))
        (,(N_ "Edit Pi_n...")          edit-pintype        "geda-pin-type"    ,(N_ "Open the Pin Type Dialog"))
        (,(N_ "Line _Width & Type...") edit-linetype       "geda-line-type"   ,(N_ "Open the Line Editor Dialog"))
        (,(N_ "Fill T_ype...")         edit-filltype       "geda-mesh"        ,(N_ "Open the Fill Editor Dialog"))
        (,(N_ "Edit Component...")    edit-component       "geda-properties"  ,(N_ "Open the Component Editor Dialog"))
        (,(N_ "_Slot...")             edit-slot            "geda-slot"        ,(N_ "Open the Slot Editor Dialog"))
      )
)

(define select-menu-items
     `( (,(N_ "Select Mode")        edit-select            "gschem-select"       ,(N_ "Activate Select mode"))
        (,(N_ "Deselect Mode")      edit-deselect          "gschem-unselect"     ,(N_ "Activate Deselect mode"))
        (,(N_ "Select All")         edit-select-all        "gschem-select-all"   ,(N_ "Select all objects"))
        (,(N_ "Deselect All")       edit-deselect-all      "geda-unselect-all"   ,(N_ "Unselect everything"))
        (,(N_ "_Invert Selection")  edit-select-invert     "gschem-invert"       ,(N_ "Invert the current selection set"))
        ("SEPARATOR"               #f                     #f)

        (,(N_ "Copy into 1")        buffer-copy1           "gtk-copy"            ,(N_ "Copy selection to first auxiliary buffer"))
        (,(N_ "Copy into 2")        buffer-copy2           "gtk-copy"            ,(N_ "Copy selection to second auxiliary buffer"))
        (,(N_ "Copy into 3")        buffer-copy3           "gtk-copy"            ,(N_ "Copy selection to third auxiliary buffer"))
        (,(N_ "Copy into 4")        buffer-copy4           "gtk-copy"            ,(N_ "Copy selection to forth auxiliary buffer"))
        (,(N_ "Copy into 5")        buffer-copy5           "gtk-copy"            ,(N_ "Copy selection to fifth auxiliary buffer"))
        (,(N_ "Cut into 1")         buffer-cut1            "gtk-cut"             ,(N_ "Cut selection to first auxiliary buffer"))
        (,(N_ "Cut into 2")         buffer-cut2            "gtk-cut"             ,(N_ "Cut selection to second auxiliary buffer"))
        (,(N_ "Cut into 3")         buffer-cut3            "gtk-cut"             ,(N_ "Cut selection to third auxiliary buffer"))
        (,(N_ "Cut into 4")         buffer-cut4            "gtk-cut"             ,(N_ "Cut selection to forth auxiliary buffer"))
        (,(N_ "Cut into 5")         buffer-cut5            "gtk-cut"             ,(N_ "Cut selection to fifth auxiliary buffer"))
        (,(N_ "Paste from 1")       buffer-paste1          "gtk-paste"           ,(N_ "Insert contents of the first auxiliary buffer"))
        (,(N_ "Paste from 2")       buffer-paste2          "gtk-paste"           ,(N_ "Insert contents of the second auxiliary buffer"))
        (,(N_ "Paste from 3")       buffer-paste3          "gtk-paste"           ,(N_ "Insert contents of the third auxiliary buffer"))
        (,(N_ "Paste from 4")       buffer-paste4          "gtk-paste"           ,(N_ "Insert contents of the forth auxiliary buffer"))
        (,(N_ "Paste from 5")       buffer-paste5          "gtk-paste"           ,(N_ "Insert contents of the fifth auxiliary buffer"))

        ("SEPARATOR"               #f                     #f)
        (,(N_ "Lock")               edit-lock              "geda-lock"        ,(N_ "Lock selected objects"))
        (,(N_ "Unlock")             edit-unlock            "geda-unlock"      ,(N_ "Unlock selected objects"))
      )
)

;; If you prefer, the buffer menu items can be under a separate top-level menu,
;; to do this uncomment all of this section and, though not actually required,
;; comment-out the same items under select-menu-items and uncomment the "_Buffer"
;; top-level menu near the end of the menu section.
;(define buffer-menu-items
;
;      menu item name                    menu action             menu icon name             Menu Item Tooltip
;
;     `( (,(N_ "Copy into 1")    buffer-copy1            "gtk-copy"  ,(N_ "Copy selection to first auxiliary buffer"))
;        (,(N_ "Copy into 2")    buffer-copy2            "gtk-copy"  ,(N_ "Copy selection to second auxiliary buffer"))
;        (,(N_ "Copy into 3")    buffer-copy3            "gtk-copy"  ,(N_ "Copy selection to third auxiliary buffer"))
;        (,(N_ "Copy into 4")    buffer-copy4            "gtk-copy"  ,(N_ "Copy selection to forth auxiliary buffer"))
;        (,(N_ "Copy into 5")    buffer-copy5            "gtk-copy"  ,(N_ "Copy selection to fifth auxiliary buffer"))
;        (,(N_ "Cut into 1")     buffer-cut1             "gtk-cut"   ,(N_ "Cut selection to first auxiliary buffer"))
;        (,(N_ "Cut into 2")     buffer-cut2             "gtk-cut"   ,(N_ "Cut selection to second auxiliary buffer"))
;        (,(N_ "Cut into 3")     buffer-cut3             "gtk-cut"   ,(N_ "Cut selection to third auxiliary buffer"))
;        (,(N_ "Cut into 4")     buffer-cut4             "gtk-cut"   ,(N_ "Cut selection to forth auxiliary buffer"))
;        (,(N_ "Cut into 5")     buffer-cut5             "gtk-cut"   ,(N_ "Cut selection to fifth auxiliary buffer"))
;        (,(N_ "Paste from 1")   buffer-paste1           "gtk-paste" ,(N_ "Insert contents of the first auxiliary buffer"))
;        (,(N_ "Paste from 2")   buffer-paste2           "gtk-paste" ,(N_ "Insert contents of the second auxiliary buffer"))
;        (,(N_ "Paste from 3")   buffer-paste3           "gtk-paste" ,(N_ "Insert contents of the third auxiliary buffer"))
;        (,(N_ "Paste from 4")   buffer-paste4           "gtk-paste" ,(N_ "Insert contents of the forth auxiliary buffer"))
;        (,(N_ "Paste from 5")   buffer-paste5           "gtk-paste" ,(N_ "Insert contents of the fifth auxiliary buffer"))
;      )
;)

(define view-menu-items
;;
;;      menu item name               menu action             menu icon name              Menu Item Tooltip
;;
     `(
        (,(N_ "_Redraw")              view-redraw           #f               ,(N_ "Redraw the current window"))
        (,(N_ "_Pan")                 view-pan              #f               ,(N_ "Activate Panning"))
        (,(N_ "Pre_vious")            view-pan              #f               ,(N_ "View previous"))

        (,(N_ "Zoom _All")            view-zoom-all         #f               ,(N_ "Zoom to the limits of the drawing area"))
        (,(N_ "Zoom _Box")            view-zoom-box         #f               ,(N_ "Zoom to a Windowed region"))
        (,(N_ "Zoom _Extents")        view-zoom-extents     #f               ,(N_ "Zoom to the extents of the drawing"))
        (,(N_ "Zoom _In")             view-zoom-in          #f               ,(N_ "Increase the Zoom magnification"))
        (,(N_ "Zoom _Out")            view-zoom-out         #f               ,(N_ "Decrease the Zoom magnification"))
        (,(N_ "Zoom _Mag")            view-zoom-to-mag      #f               ,(N_ "Zoom to a specified level"))
        (,(N_ "Zoom _Selection")      view-zoom-selected    #f               ,(N_ "Zoom to selected objects"))

        ("SEPARATOR"                #f                      #f)
        (,(N_ "D_ocumentation...")    view-documentation    #f               ,(N_ "View component documentation"))
        (,(N_ "Show/Hide Inv Text")   view-show-hidden      #f               ,(N_ "Toggle hidden text attributes"))
        (,(N_ "Show/Hide Inheri_ted") view-show-inherited   #f               ,(N_ "Toggle inherited text attributes"))
        (,(N_ "Show/Hide Net Names")  view-show-nets        #f               ,(N_ "Toggle hidden net name attributes"))

        ("SEPARATOR"                #f                      #f)
        (,(N_ "_Dark color scheme")   view-dark-colors      #f               ,(N_ "Set the color map to the Dark set"))
        (,(N_ "_Light color scheme")  view-light-colors     #f               ,(N_ "Set the color map to the Light set"))
        (,(N_ "B_W color scheme")     view-bw-colors        #f               ,(N_ "Set the color map to Black and White"))
      )
)

(define page-menu-items
;;
;;      menu item name             menu action              menu icon name             Menu Item Tooltip
;;
     `(
        (,(N_ "_Draw Order")       #f                      "gtk-index"            ,(N_ "Change the order objects are drawn"))
        ("SEPARATOR"               #f                      #f)

        (,(N_ "_Manager...")       page-manager            "gschem-pages"         ,(N_ "Open the Page Manager"))
        (,(N_ "_First")            page-first              "gtk-goto-first"       ,(N_ "Goto the first page"))
        (,(N_ "_Previous")         page-prev               "gtk-go-back"          ,(N_ "Switch to the previous page"))
        (,(N_ "_Next")             page-next               "gtk-go-forward"       ,(N_ "Switch to the next page"))
        (,(N_ "_Up")               page-up                 "gtk-go-up"            ,(N_ "Go up one page"))
        (,(N_ "_Down")             page-down               "gtk-go-down"          ,(N_ "Go down one page"))
        (,(N_ "_Last")             page-last               "gtk-goto-last"        ,(N_ "Goto the last page"))
        (,(N_ "Ne_w")              page-new                "gtk-new"              ,(N_ "Create a new Page"))

        ("SEPARATOR"              #f                      #f)
        (,(N_ "_Close")            page-close              "gtk-close"            ,(N_ "Close the current page"))
        (,(N_ "_Revert")           page-revert             "gtk-revert-to-saved"  ,(N_ "Discard changes and reload the current documents"))
        (,(N_ "D_iscard")          page-discard            "gtk-discard"          ,(N_ "Close the current page without saving"))
        (,(N_ "Prin_t")            page-print              "gtk-print"            ,(N_ "Print the current page"))

        ("SEPARATOR"              #f                       #f)
        (,(N_ "Down _Schematic")   hierarchy-down-schematic #f                 ,(N_ "Descend down in the schematic hierarchy"))
        (,(N_ "Down S_ymbol")      hierarchy-down-symbol    #f                 ,(N_ "Descend down in the symbol hierarchy"))
        (,(N_ "_Hierarchy Up")     hierarchy-up             #f                 ,(N_ "ascend up in the schematic hierarchy"))
      )
)

(define add-menu-items
;;
;;      menu item name             menu action              menu icon name         Menu Item Tooltip
;;
     `( (,(N_ "_Component...")     add-component           "geda-component"   ,(N_ "Insert a symbol from the component library"))
        (,(N_ "_Net")              add-net                 "gschem-net"       ,(N_ "Add net"))
        (,(N_ "B_us")              add-bus                 "gschem-bus"       ,(N_ "Add bus"))
        (,(N_ "_Attribute...")     add-attribute           #f                 ,(N_ "Add attribute"))
        (,(N_ "_Text...")          add-text                #f                 ,(N_ "Add text"))

        ("SEPARATOR"              #f                       #f)
        (,(N_ "A_rc")              add-arc                 "geda-arc"         ,(N_ "Create an arc"))
        (,(N_ "_Box")              add-box                 "geda-box"         ,(N_ "Add a box"))
        (,(N_ "C_ircle")           add-circle              "geda-circle"      ,(N_ "Add a circle"))
        (,(N_ "_Line")             add-line                "geda-line"        ,(N_ "Add a line"))
        (,(N_ "Pat_h")             add-path                "geda-path"        ,(N_ "Add a path"))
        (,(N_ "Pictu_re...")       add-picture             "gtk-orientation-portrait" ,(N_ "Insert an image into the current document"))
        (,(N_ "_Pin")              add-pin                 "geda-pin"                 ,(N_ "Add a pin"))
      )
)

(define sessions-menu-items
;;
;;      menu item name            menu action              menu icon name           Menu Item Tooltip
;;
     `( (,(N_ "_New")             session-new              "gtk-go-down"       ,(N_ "Create a new session"))
        (,(N_ "_Open...")         session-open             "geda-open-recent"  ,(N_ "Launch the open Session dialog"))
        (,(N_ "_Restore")         #f                       "gtk-go-up"         ,(N_ "Restore an existing Session"))
        ("SEPARATOR"              #f                      #f)
        (,(N_ "_Save")            session-save             "gtk-info"          ,(N_ "Save the current Session"))
        (,(N_ "Save _As...")      session-save-as          "geda-open-recent"  ,(N_ "Save the current Session to another name"))
        ("SEPARATOR"              #f                      #f)
        (,(N_ "_Manage...")       session-manage           "gtk-properties"    ,(N_ "Open the Session Manager dialog"))
      )
)

(define attributes-menu-items
;;
;;      menu item name             menu action                   menu icon name                Menu Item Tooltip
;;
     `( (,(N_ "_Attach")           attributes-attach             #f            ,(N_ "Attach selected attributes to symbol"))
        (,(N_ "_Detach")           attributes-detach             #f            ,(N_ "Detach selected attributes from a symbol"))
        (,(N_ "Show _Value")       attributes-show-value         #f            ,(N_ "Set selected value visible"))
        (,(N_ "Show _Name")        attributes-show-name          #f            ,(N_ "Set selected name visible"))
        (,(N_ "Show _Both")        attributes-show-both          #f            ,(N_ "Set selected name and value visible"))
        ("SEPARATOR"                    #f                       #f)
        (,(N_ "_Toggle Visibility")      attributes-visibility   #f            ,(N_ "Toggle attribute visibility"))
        (,(N_ "_Reset Position")         attributes-home         #f            ,(N_ "Restore attribute positions and orientation"))

        (,(N_ "_Find Specific Text...")  attributes-find-text    #f            ,(N_ "Find an attribute"))
        (,(N_ "_Hide Specific Text...")  attributes-hide-text    #f            ,(N_ "Hide selected attribute"))
        (,(N_ "_Show Specific Text...")  attributes-show-text    #f            ,(N_ "Show a specific attribute"))
        ("SEPARATOR"                    #f                       #f)
        (,(N_ "Attribute _Editor...")    attributes-editor       "gtk-indent"  ,(N_ "Open the Attributes Editor Dialog"))
      )
)

(define tools-menu-items
;;
;;      menu item name                    menu action            menu icon name            Menu Item Tooltip
;;
     `( (,(N_ "A_utonumber Text...")      tools-autonumber       #f                 ,(N_ "Open Auto Number dialog"))
        (,(N_ "Show _Console Window...")  tools-show-console     #f                 ,(N_ "Display the console window"))
        (,(N_ "Show Coord _Window...")    tools-show-coordinates #f                 ,(N_ "Display coordinates"))

        ("SEPARATOR"                      #f                     #f)
        (,(N_ "Invoke Macro")             tools-invoke-macro     "gtk-execute"      ,(N_ "Invoke a macro"))
        (,(N_ "G_uile Path...")           tools-guile-path       "guile-logo"       ,(N_ "View or manage guile path"))
        ;;(,(N_ "REPL...")                  tools-repl             "gtk-execute"      ,(N_ "open a REPL"))

        ("SEPARATOR"                      #f                     #f)
        (,(N_ "Embed Component/Picture")   tools-embed           "geda-inbed"       ,(N_ "Embed a component or image object"))
        (,(N_ "Unembed Component/Picture") tools-unembed         "geda-bed"         ,(N_ "Unembed a component or image object"))
      )
)

(define options-menu-items
;;
;;      menu item name                    menu action             menu icon name             Menu Item Tooltip
;;
     `( (,(N_ "Cycle _grid styles")       options-cycle-grid       #f ,(N_ "Cycle grid between Dot, Mesh and Off"))
        (,(N_ "Scale _up Grid Spacing")   scale-up-snap-size       #f ,(N_ "Increase the snap size"))
        (,(N_ "Scale _down Grid Spacing") scale-down-snap-size     #f ,(N_ "Decrease the snap size"))
        (,(N_ "S_nap Grid Spacing...")    options-snap-size        #f ,(N_ "Adjust snap size"))

        ("SEPARATOR"                     #f                        #f)
        (,(T_ (N_ "_Snap On-Off"))        options-cycle-snap       #f ,(N_ "Toggle the object snap mode"))
        (,(T_ (N_ "_Rubberband"))         options-rubberband       #f ,(N_ "Toggle rubberband net mode"))
        (,(T_ (N_ "_Magnetic Net"))       options-magneticnet      #f ,(N_ "Toggle magnetic net mode"))
        (,(T_ (N_ "_Drag Move"))          options-dragcanmove      #f ,(N_ "Toggle Drag-can-Move mode"))
        (,(T_ (N_ "_Outline-Box"))        options-action-feedback  #f ,(N_ "Toggle action feedback"))
        (,(T_ (N_ "_Auto-Pan"))           options-auto-pan         #f ,(N_ "Toggle auto panning mode"))

        ("SEPARATOR"                     #f                        #f)
        (,(N_ "_Text Size...")            options-show-text-size   #f                ,(N_ "Open the Text Size settings"))
        (,(N_ "_Preferences...")          options-show-settings    "gtk-preferences" ,(N_ "Open the Preferences dialog"))
      )
)

(define help-menu-items
;;
;;      menu item name                  menu action               menu hotkey action        menu stock icon
;;
     `(
        (,(N_ "Gschem Guide")           help-show-manual          "gtk-help")
        (,(N_ "_Hotkeys...")            help-show-hotkeys         "keyboard-shortcuts")
        (,(N_ "gschem _FAQ...")         help-show-faq             "help-faq")
        ("SEPARATOR"                   #f                         #f)
        (,(N_ "Component D_ocumentation...")hierarchy-documentation  #f                 ,(N_ "Find component documentation"))
        ("SEPARATOR"                   #f                         #f)
        (,(N_ "gEDA Docu_mentation...") help-show-geda            "symbol-datasheet")
        (,(N_ "gEDA _Wiki...")          help-show-wiki            "web-browser")
        (,(N_ "_About...")              help-show-about           "gtk-about")
      )
)

(define draw-order-menu-items
;;
;;    menu item name         menu action           menu icon                   Menu Item Tooltip
;;
  `((,(N_ "_Lower Behind")    page-draw-before     "gtk-sort-ascending"        ,(N_ "Draw objects before another object"))
    (,(N_"_Raise Above")      page-draw-after      "gtk-sort-descending"       ,(N_ "Draw object after another object"))
    ("SEPARATOR"             #f                    #f)
    (,(N_"Bring to _Top")     page-draw-last       "gtk-sort-ascending"        ,(N_ "Raise objects to top of drawing order"))
    (,(N_"Move to _Bottom")   page-draw-first      "gtk-sort-descending"       ,(N_ "Lower to bottom of the drawing order"))
  )
)

;
; Now actually add the menus.  The order here defines the order in which
; the menus appear in the top menu bar.
;
(add-menu (N_ "_File")       file-menu-items)
(add-menu (N_ "_Edit")       edit-menu-items)
(add-menu (N_ "_Select")     select-menu-items)
;(add-menu (N_ "_Buffer")     buffer-menu-items)
(add-menu (N_ "_View")       view-menu-items)
(add-menu (N_ "_Page")       page-menu-items)
(add-menu (N_ "_Add")        add-menu-items)
(add-menu (N_ "Sessio_ns")   sessions-menu-items)
(add-menu (N_ "Attri_butes") attributes-menu-items)
(add-menu (N_ "_Tools")      tools-menu-items)

;; The add the sub-menu items under the Draw Order menu item
(add-menu (N_ "_Page/_Draw Order") draw-order-menu-items)

(load-from-path "gschem/gschem-export-menu.scm")

;; Add Paul Tan's tools utility menu
(load-from-path "gschem/gschem-tools-menu.scm")

(add-menu (N_ "_Options") options-menu-items)
(add-menu (N_ "_Help") help-menu-items)

;
; End of keymapping related keywords
;

;;
;; Major modes
;;

;; Comment in this scheme code if you want to link with pcb
;;
;; Please note that the code in pcb.scm is still highly experimental
;; and there are known (and easy) ways to crash pcb and/or gschem with this code.
;; The short answer is neither program likes a pipe to break.
;;
; (load-from-path "pcb.scm")
