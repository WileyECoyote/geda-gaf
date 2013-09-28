
#define QUOTE_SYMBOL(symbol) #symbol
#define ACTION(...) QUOTE_SYMBOL(__VA_ARGS__)

#define FILE_NEW         file-new
#define FILE_NEW_WINDOW  file-new-window
#define FILE_OPEN        file-open
#define FILE_SAVE        file-save
#define FILE_SAVE_AS     file-save-as
#define FILE_SAVE_ALL    file-save-all
#define FILE_PRINT       file-print
#define FILE_WRITE_IMAGE file-write-image
#define FILE_WRITE_PDF   write-pdf
#define FILE_RUN_SCRIPT  file-run-script
#define FILE_CLOSE       file-close
#define FILE_QUIT        file-quit

#define EDIT_UNDO        edit-undo
#define EDIT_REDO        edit-redo
#define EDIT_CB_CUT      clipboard-cut
#define EDIT_CB_COPY     clipboard-copy
#define EDIT_BUF_cut     clipboard-cut
#define EDIT_BUF_copy    clipboard-copy
#define EDIT_BUF_paste   clipboard-paste
#define EDIT_CB_PASTE    clipboard-paste
#define EDIT_DELETE      edit-delete
#define EDIT_SELECT      edit-select
#define EDIT_SELECT_ALL  edit-select-all
#define EDIT_INVERT      edit-select-invert
#define EDIT_DESELECT     edit-deselect
#define EDIT_DESELECT_ALL edit-deselect-all

#define EDIT_COPY        edit-copy
#define EDIT_MCOPY       edit-mcopy
#define EDIT_MOVE        edit-move
#define EDIT_ROTATE      edit-rotate
#define EDIT_MIRROR      edit-mirror
#define EDIT_ARC         edit-arc
#define EDIT_ATTRIB      edit-attributes
#define EDIT_TEXT        edit-text
#define EDIT_SLOT        edit-slot
#define EDIT_COLOR       edit-color
#define EDIT_PIN         edit-pintype
#define EDIT_LINE        edit-linetype
#define EDIT_FILL        edit-filltype
#define EDIT_TRANSLATE   edit-translate
#define EDIT_LOCK        edit-lock
#define EDIT_UNLOCK      edit-unlock
#define EDIT_MACRO       edit-invoke-macro
#define EDIT_EMBED       edit-embed
#define EDIT_UNEMBED     edit-unembed
#define EDIT_UPDATE      edit-update

#define VIEW_REDRAW      view-redraw
#define VIEW_PAN         view-pan
#define VIEW_BOX         view-zoom-box
#define VIEW_SELECTED    view-zoom-selected
#define VIEW_EXTENTS     view-zoom-extents
#define VIEW_ZOOM_IN     view-zoom-in
#define VIEW_ZOOM_OUT    view-zoom-out
#define VIEW_ZOOM_ALL    view-zoom-all
#define VIEW_DOCUMENT    view-documentation
#define VIEW_HIDDEN      view-show-hidden
#define VIEW_NETS        view-show-nets
#define VIEW_DARK        view-dark-colors
#define VIEW_LIGHT       view-light-colors
#define VIEW_BLACK_WHITE view-bw-colors

#define PAGE_MANAGER     page-manager
#define PAGE_PREV        page-prev
#define PAGE_NEXT        page-next
#define PAGE_NEW         page-new
#define PAGE_PRINT       page-print
#define PAGE_REVERT      page-revert
#define PAGE_CLOSE       page-close
#define PAGE_DISCARD     page-discard

#define ADD_COMPONENT    add-component
#define ADD_NET          add-net
#define ADD_BUS          add-bus
#define ADD_ATTRIB       add-attribute
#define ADD_TEXT         add-text
#define ADD_LINE         add-line
#define ADD_PATH         add-path
#define ADD_BOX          add-box
#define ADD_CIRCLE       add-circle
#define ADD_ARC          add-arc
#define ADD_PIN          add-pin
#define ADD_PICTURE      add-picture

#define DOWN_SCHEMATIC   hierarchy-down-schematic
#define DOWN_SYMBOL      hierarchy-down-symbol
#define HIERARCHY_UP     hierarchy-up

#define ATTRIB_ATTACH    attributes-attach
#define ATTRIB_DETACH    attributes-detach
#define ATTRIB_VALUE     attributes-show-value
#define ATTRIB_NAME      attributes-show-name
#define ATTRIB_BOTH         attributes-show-both
#define ATTRIB_VISIBILITY   attributes-visibility

#define ATTRIB_FIND      attributes-find-text
#define ATTRIB_HIDE      attributes-hide-text
#define ATTRIB_SHOW      attributes-show-text
#define ATTRIB_EDIT      attributes-editor
#define ATTRIB_AUTONUM   attributes-autonumber

/* Grid */
#define OPT_GRID_DOT     options-grid-dot
#define OPT_GRID_MESH    options-grid-mesh
#define OPT_GRID_OFF     options-grid-off

/* Toggles */
#define OPT_CYLCE_GRID   options-cycle-grid
#define OPT_SNAP_UP      scale-up-snap-size
#define OPT_SNAP_DOWN    scale-down-snap-size
#define OPT_SNAP_SIZE    options-snap-size
#define OPT_SNAP_OFF     options-snap-off
#define OPT_SNAP_ON      options-snap-on

/* Toggles */
#define CYCLE_SNAP       options-cycle-snap
#define TOGGLE_FEEDBACK  options-action-feedback
#define TOGGLE_RUBBER    options-rubberband
#define TOGGLE_MAGNETIC  options-magneticnet

#define OPT_CONSOLE      options-show-console
#define OPT_COORDINATES  options-show-coordinates
#define OPT_TEXT_SIZE    options-show-text-size
#define OPT_SETTINGS     options-show-settings

#define HELP_MANUAL      help-show-manual
#define HELP_HOTKEYS     help-show-hotkeys
#define HELP_FAQ         help-show-faq
#define HELP_GEDA        help-show-geda
#define HELP_WIKI        help-show-wiki
#define HELP_ABOUT       help-show-about
