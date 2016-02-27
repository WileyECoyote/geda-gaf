; Test Scheme procedures for accessing host operating system
; information.

(use-modules (unit-test)
             (geda os)
             (srfi srfi-1))

(begin-test 'platform
  (assert-true (every symbol? (platform))))

(begin-test 'separators
  (assert-true (char? separator-char))
  (assert-true (string? separator))
  (assert-equal 1 (string-length separator))

  (assert-true (char? path-separator-char))
  (assert-true (string? path-separator))
  (assert-equal 1 (string-length path-separator))

  (assert-true (separator-char? separator-char)))

(begin-test 'sys-directories
  (assert-true (every string? (sys-config-dirs)))
  (assert-true (every string? (sys-data-dirs))))

(begin-test 'user-directories
  (assert-true (every string? (user-config-dir)))
  (assert-true (every string? (user-data-dir))))