;; Test routines defined in src/guile/g_rc.c.

;; Check bitmap-directory returns value
(begin-test 'bitmap-directory/getter
  (let ((path (bitmap-directory)))
    (assert-equal #f path))
  (bitmap-directory ".")
  (let ((path (bitmap-directory)))
    (assert-equal "." path))
)
