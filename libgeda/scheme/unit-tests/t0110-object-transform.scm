;; Test Scheme procedures for transforming objects

(use-modules (unit-test))
(use-modules (geda object))

(begin-test 'translate-objects!
  (let ((C (make-component "test component" '(1 . 2) 0 #t #f))
        (a (make-line '(1 . 2) '(3 . 4)))
        (b (make-line '(1 . 2) '(3 . 4))))

    ;; Translate nothing
    (assert-equal '() (translate-objects! '(1 . 2)))

    ;; Translate a line
    (assert-equal (list a) (translate-objects! '(1 . 2) a))
    (assert-equal '(2 . 4) (line-start a))
    (assert-equal '(4 . 6) (line-end a))

    ;; Translate a component
    (component-append! C b)
    (assert-equal (list C) (translate-objects! '(1 . 2) C))
    (assert-equal '(2 . 4) (component-position C))
    (assert-equal '(2 . 4) (line-start b))
    (assert-equal '(4 . 6) (line-end b))

    ;; Translate multiple objects
    (assert-equal (list a C) (translate-objects! '(-1 . -2) a C))
    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal '(1 . 2) (component-position C))
    (assert-equal '(1 . 2) (line-start b))
    (assert-equal '(3 . 4) (line-end b)) ))

(begin-test 'rotate-objects!
  (let ((C (make-component "test component" '(1 . 2) 0 #t #f))
        (a (make-line '(1 . 2) '(3 . 4)))
        (b (make-line '(1 . 2) '(3 . 4))))

    ;; Rotate nothing
    (assert-equal '() (rotate-objects! '(1 . 2) 90))

    ;; Rotate a line
    (assert-equal (list a) (rotate-objects! '(1 . 2) 90 a))
    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(-1 . 4) (line-end a))

    ;; Rotate a component
    (component-append! C b)
    (assert-equal (list C) (rotate-objects! '(1 . 2) -270 C))
    (assert-equal '(1 . 2) (component-position C))
    (assert-equal 90 (component-angle C))
    (assert-equal '(1 . 2) (line-start b))
    (assert-equal '(-1 . 4) (line-end b))

    ;; Rotate multiple objects
    (assert-equal (list a C) (rotate-objects! '(1 . 2) -90 a C))
    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-equal 0 (component-angle C))
    (assert-equal '(1 . 2) (line-start b))
    (assert-equal '(3 . 4) (line-end b)) ))

(begin-test 'mirror-objects!
  (let ((C (make-component "test component" '(1 . 2) 0 #f #f))
        (a (make-line '(1 . 2) '(3 . 4)))
        (b (make-line '(1 . 2) '(3 . 4))))

    ;; Mirror nothing
    (assert-equal '() (mirror-objects! 2))

    ;; Mirror a line
    (assert-equal (list a) (mirror-objects! 2 a))
    (assert-equal '(3 . 2) (line-start a))
    (assert-equal '(1 . 4) (line-end a))

    ;; Mirror a component
    (component-append! C b)
    (assert-equal (list C) (mirror-objects! 2 C))
    (assert-equal '(3 . 2) (component-position C))
    (assert-true (component-mirror? C))
    (assert-equal '(3 . 2) (line-start b))
    (assert-equal '(1 . 4) (line-end b))

    ;; Mirror multiple objects
    (assert-equal (list a C) (mirror-objects! 2 a C))
    (assert-equal '(1 . 2) (line-start a))
    (assert-equal '(3 . 4) (line-end a))
    (assert-true (not (component-mirror? C)))
    (assert-equal '(1 . 2) (line-start b))
    (assert-equal '(3 . 4) (line-end b)) ))

;; New symbol library with one component containing only one
;; line primitive
(component-library-funcs
  (lambda ()       ; list-symbol-names function
    '("line.sym"))
  (lambda (name)   ; get-symbol-by-name function
    (let ((page (make-page "/test/page/line")))
      (page-append! page (make-line '(1 . 2) '(3 . 4)))
      (let ((s (page->string page)))
        (close-page! page)
        s)))
  "Test symbols"   ; Library name
  )

;; Test the 'set-component!' procedure (the 'page-append!' procedure also
;; invokes it). This test includes testing of mirroring and rotation of a
;; component's primitives.

(begin-test 'component/library-transform
  (let ((P (make-page "/test/page/A"))
        (C (make-component/library "line.sym" '(0 . 0) 90 #f #f)))

    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (page-append! P C)

        (assert-equal '(-2 . 1) (line-start (car (component-contents C))))
        (assert-equal '(-4 . 3) (line-end   (car (component-contents C))))

        (set-component! C '(0 . 0) 90 #t #f)

        (assert-equal '(-2 . -1) (line-start (car (component-contents C))))
        (assert-equal '(-4 . -3) (line-end   (car (component-contents C))))
        )
      (lambda ()
        (close-page! P)))
    ))

;; Clear component library again
(reset-component-library)