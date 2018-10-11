; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist

;; This backend provides an interactive shell intended
;; for development and debugging.

(use-modules (ice-9 readline))
(activate-readline)

(define (shell output-filename)

    (set-repl-prompt! "gnetlist>")
    (scm-style-repl)
)

