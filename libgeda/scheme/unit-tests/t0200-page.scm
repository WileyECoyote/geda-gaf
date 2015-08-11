;; Test Scheme procedures related to pages.

(use-modules (unit-test))
(use-modules (geda page))
(use-modules (geda object))

(begin-test 'page-basic
   (let ((A (make-page "/test/page/A"))
         (B (make-page "/test/page/B")))

         (assert-true (page? A))

         (assert-equal "/test/page/A" (page-filename A))
         (assert-equal (list A B) (active-pages))

         (assert-equal A (set-page-filename! A "/test/page/G"))
         (assert-equal "/test/page/G" (page-filename A))

         (close-page! A)
         (assert-equal (list B) (active-pages))
         (close-page! B)
  )
)

(begin-test 'page-append
  (let ((C  (make-page "/test/page/C"))
        (D  (make-page "/test/page/D"))
        (L1 (make-line '(0 . 0) '(1 . 2)))
        (L2 (make-line '(0 . 1) '(2 . 2))))

    (begin ; Make sure pages are cleaned up
        (lambda () #f)
        (lambda ()
          (assert-equal '() (page-contents C))

          (assert-equal C (page-append! C L1))
          (assert-equal (list L1) (page-contents C))

          (assert-equal C (page-append! C L1 L2))
          (assert-equal (list L1 L2) (page-contents C))

          (assert-thrown 'object-state (page-append! D L1))

          (assert-thrown 'object-state
            (let* ((TC1 (make-component "test component" '(1 . 2) 0 #t #f))
                   (L3 (make-line '(1 . 0) '(2 . 2))))
              (component-append! TC1 L3)
              (page-append! C L3))))

        (lambda ()
          (close-page! C)
          (close-page! D)
        )
    )
  )
)
