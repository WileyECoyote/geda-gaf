;; Test routines defined in src/guile/g_rc.c.

(begin-test 'attribute-promotion/getter
  (let ((mode (attribute-promotion)))
    (assert-equal #t mode)))

(begin-test 'promote-invisible/getter
  (let ((mode (promote-invisible)))
    (assert-equal #f mode)))

(begin-test 'keep-invisible/getter
  (let ((mode (keep-invisible)))
    (assert-equal #t mode)))

;; Check bitmap-directory returns value
(begin-test 'bitmap-directory/getter
  (let ((path (bitmap-directory)))
    (assert-equal #f path))
  (bitmap-directory ".")
  (let ((path (bitmap-directory)))
    (assert-equal "." path))
)
