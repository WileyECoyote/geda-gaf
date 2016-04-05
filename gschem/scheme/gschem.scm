;;; gEDA - GPL Electronic Design Automation
;;; gschem - gEDA Schematic Capture
;;; Copyright (C) 1998-2014 Ales Hvezda
;;; Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

(use-modules (gschem keymap)
             (gschem selection)
             (gschem window)
             (gschem gschemdoc)
             (geda object)
             (srfi srfi-1))

;; Define an eval-in-current module procedure
(define (eval-cm expr) (eval expr (current-module)))

(define current-keys '())

(define %global-keymap (make-keymap))
(define current-keymap %global-keymap)

;; Set a global keybinding
(define (global-set-key key binding)
  (bind-keys! %global-keymap key binding))

;; Called from C code to evaluate keys.
(define (press-key key)
  (eval-pressed-key current-keymap key))

;; Function for resetting current key sequence
(define (reset-keys) (set! current-keys '()) #f)

;; Does the work of evaluating a key.  Adds the key to the current key
;; sequence, then looks up the key sequence in the current keymap.  If
;; the key sequence resolves to an action, calls the action.  If the
;; key sequence can be resolved to an action, returns #t; if it
;; resolves to a keymap (i.e. it's a prefix key), returns the "prefix"
;; symbol; otherwise, returns #f.  If the key is #f, clears the
;; current key sequence.
(define (eval-pressed-key keymap key)
  (if key
    (begin
      ;; Add key to current key sequence
      (set! current-keys (cons key current-keys))
      (let* ((keys (list->vector (reverse current-keys)))
             (bound (lookup-keys keymap keys)))
        (cond
         ;; Keys are a prefix -- do nothing successfully
         ((keymap? bound) 'prefix)
         ;; Keys are bound to something -- reset current key
         ;; sequence, then try to run the action
         (bound (begin
                  (reset-keys)
                  (eval-keymap-action bound)
         )      )
         ;; No binding
         (else (reset-keys))
    ) ) )
    (reset-keys)
) )

;; Evaluates a keymap action.  A keymap action is expected to be a
;; symbol naming a thunk variable in the current module.
(define (eval-keymap-action action)
  (define (invalid-action-error)
    (gschem-msg (string-append "Invalid action for keybinding:\n\n" (symbol->string action)))
  )
  (if (symbol? action)
    (let ((proc (false-if-exception (eval-cm action))))
      (if (thunk? proc)
          (begin
            (proc)
            #t)
          (if (procedure? proc)
              (begin
                (proc (symbol->string action))
                 #t)
              (invalid-action-error)
          )
      )
   )
   (invalid-action-error)   ;; action is not a symbols, so fail
  )
)

(define (eval-stroke stroke)
  (let ((action (assoc stroke strokes)))
    (cond ((not action)
;          (display "No such stroke\n")
;          (display stroke)
            "")
          (else
;           (display "Scheme found action ")
;           (display action)
;           (display "\n")
           (symbol->string (cdr action))))))

;; Search the global keymap for a particular symbol and return the
;; keys which execute this hotkey, as a string suitable for display to
;; the user. This is not used.
(define (find-key action)
  (let ((keys (lookup-binding %global-keymap action)))
    (and keys (keys->display-string keys))))

;; Printing out current key bindings for gEDA (gschem)
(define (dump-global-keymap)
  (dump-keymap %global-keymap))

(define (dump-keymap keymap)

  (define lst '())

  (define (binding->entry prefix key binding)
    (let ((keys (list->vector (reverse (cons key prefix)))))
      (set! lst (cons (cons (symbol->string binding)
                            (keys->display-string keys))
                      lst))))

  (define (build-dump! km prefix)
    (keymap-for-each
     (lambda (key binding)
       (cond
        ((symbol? binding)
         (binding->entry prefix key binding))
        ((keymap? binding)
         (build-dump! binding (cons key prefix)))
        (else (error "Invalid action ~S bound to ~S"
                     binding (list->vector (reverse (cons key prefix)))))))
     km))

  (build-dump! keymap '())
  lst)

;;;; Documentation-related actions

(define (hierarchy-documentation)
  "hierarchy-documentation

If a component is selected, search for and display corresponding
documentation in a browser or PDF viewer. If no documentation can be
found, shows a dialog with an error message."
  (catch
   'misc-error
   (lambda ()
     (let ((component
            (any (lambda (obj) (and (component? obj) obj))
                 (page-selection (active-page)))))
       (and component (show-component-documentation component))))
   (lambda (key subr msg args . rest)
     (gschem-msg (string-append
                  "Could not show documentation for selected component:\n\n"
                  (apply format #f msg args))))))

(define (tools-repl)
  (start-repl-in-background-terminal)
)

(define (zoom-sw)
  (show-wiki "geda:documentation")
)

;;(define (help-show-manual)
;;  "help-manual
;;  Display the front page of the gEDA manuals in a browser."
;;  (show-wiki "geda:documentation"))
;;(define (help-show-faq)
;;  "help-faq
;;  Display the gschem Frequently Asked Questions in a browser."
;;  (show-wiki "geda:faq-gschem"))

;;(define (help-show-wiki)
;;  "help-faq
;;  Display the gEDA wiki in a browser."
;;  (show-wiki))

