;; Test Scheme procedures related to pages.

(use-modules (unit-test))
(use-modules (geda page))
(use-modules (geda object))

(begin-test 'page-remove
  (let ((E   (make-page "/test/page/E"))
        (F   (make-page "/test/page/F"))
        (TC2 (make-component "test component" '(1 . 2) 0 #t #f))
        (L4  (make-line '(0 . 0) '(2 . 0)))
        (L5  (make-line '(0 . 0) '(0 . 2)))
        (L6  (make-line '(1 . 0) '(2 . 2))))

    (dynamic-wind ; Make sure pages are cleaned up
        (lambda () #f)
        (lambda ()
          (page-append! E L4)
          (assert-equal E (page-remove! E L4))
          (assert-equal '() (page-contents E))
          (assert-equal E (page-remove! E L4))
          (assert-equal F (page-remove! F L4))

          (page-append! E L4 L5)
          (assert-equal E (page-remove! E L4))
          (assert-equal (list L5) (page-contents E))

          (assert-thrown 'object-state (page-remove! F L5))

          (component-append! TC2 L6)
          (assert-thrown 'object-state (page-remove! E L6))
        )

        (lambda ()
          (close-page! E)
          (close-page! F)
        )
    )
  )
)