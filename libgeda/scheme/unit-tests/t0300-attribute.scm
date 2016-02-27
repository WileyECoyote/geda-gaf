;; Test Scheme procedures related to attributes.

(use-modules (unit-test))
(use-modules (geda attrib))
(use-modules (geda page))
(use-modules (geda object))

(begin-test 'parse-attrib
  (let ((good (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
        (bad (make-text '(1 . 2) 'lower-left 0 "name value" 10 #t 'both)))

    (assert-equal #t (attribute? good))
    (assert-equal #f (attribute? bad))

    (assert-equal "name" (attrib-name good))
    (assert-equal "value" (attrib-value good))
    (assert-equal (cons (attrib-name good) (attrib-value good))
                  (parse-attrib good))

    (assert-thrown 'attribute-format (parse-attrib bad))
    (assert-thrown 'attribute-format (attrib-name bad))
    (assert-thrown 'attribute-format (attrib-value bad)) ))

(begin-test 'attach-attrib
  (let ((C (make-component "testcomponent1" '(0 . 0) 0 #f #f))
        (D (make-component "testcomponent2" '(0 . 0) 0 #f #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (q (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
        (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both))
        (z (make-text '(0 . 0) 'lower-left 0 "name=z" 10 #t 'both)))

    ;; Attach attribute outside component or page
    (assert-thrown 'object-state (attach-attribs! C x))
    (assert-equal '() (object-attribs C))
    (assert-true (not (attrib-attachment x)))

    ;; Populate components
    (component-append! C p q x y)
    (component-append! D z)

    ;; Attach attribute to object in same component
    (assert-equal p (attach-attribs! p x))
    (assert-equal (list x) (object-attribs p))
    (assert-equal p (attrib-attachment x))

    ;; Attach attribute twice
    (assert-equal p (attach-attribs! p x))
    (assert-equal (list x) (object-attribs p))
    (assert-equal p (attrib-attachment x))

    ;; Attach attribute which is already attached, within same
    ;; component
    (assert-thrown 'object-state (attach-attribs! q x))

    ;; Attach attribute to attached attribute, within same component
    (assert-thrown 'object-state (attach-attribs! x y))

    ;; Attach attribute to object in different component
    (assert-thrown 'object-state (attach-attribs! p z))
    (assert-equal (list x) (object-attribs p))
    (assert-true (not (attrib-attachment z)))

    ;; Attach internal attribute to containing component
    (assert-thrown 'object-state (attach-attribs! D z))
    (assert-equal '() (object-attribs D))
    (assert-true (not (attrib-attachment z)))

    ;; Attach attribute in component to floating object
    (assert-thrown 'object-state (attach-attribs! C z))
    (assert-equal '() (object-attribs C))
    (assert-true (not (attrib-attachment z)))

    ;; Attach floating attribute to object in component
    (component-remove! D z)
    (assert-thrown 'object-state (attach-attribs! p z))
    (assert-equal (list x) (object-attribs p))
    (assert-true (not (attrib-attachment z)))

    ;; Attach multiple attributes
    (assert-equal p (attach-attribs! p y))
    (assert-equal (list x y) (object-attribs p))
    (assert-equal p (attrib-attachment y))
  )
)

(begin-test 'inherited-attribs
  (let ((C (make-component "test component" '(1 . 2) 0 #t #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
        (y (make-text '(1 . 2) 'lower-left 0 "name=y" 10 #t 'both)))

    (assert-equal '() (inherited-attribs p))
    (assert-equal '() (inherited-attribs C))

    ;; Set up component
    (component-append! C p x y)

    (assert-equal (list x y) (inherited-attribs C))

    (attach-attribs! p x)

    (assert-equal (list y) (inherited-attribs C))))

(begin-test 'attrib-inherited?
  (let* ((P (make-page "/test/page/1"))
         (A (make-component "test component" '(0 . 0) 0 #t #f))
         (p (make-net-pin '(0 . 0) '(100 . 0)))
         (w (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
         (x (make-text '(1 . 2) 'lower-left 0 "name=x" 10 #t 'both))
         (y (make-text '(1 . 2) 'lower-left 0 "name=y" 10 #t 'both))
         (z (make-text '(1 . 2) 'lower-left 0 "name=z" 10 #t 'both)))

    (page-append! P A w x)
    (attach-attribs! A x)
    (component-append! A p y z)
    (attach-attribs! p y)

    (assert-true (not (attrib-inherited? w)))
    (assert-true (not (attrib-inherited? x)))
    (assert-true (not (attrib-inherited? y)))
    (assert-true (attrib-inherited? z))))

(begin-test 'set-attrib-value!
  (let ((a (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both)))
    (set-attrib-value! a "foo")
    (assert-equal "name" (attrib-name a))
    (assert-equal "foo" (attrib-value a))))

(begin-test 'detach-attrib
  (let ((page (make-page "/test/page/1"))
        (pin1 (make-net-pin '(0 . 0) '(100 . 0)))
        (pin2 (make-net-pin '(0 . 100) '(100 . 100)))
        (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))

    (page-append! page pin1 pin2 x)

    ;; Detach when already detached
    (assert-equal pin1 (detach-attribs! pin1 x))

    (attach-attribs! pin1 x)

    (assert-thrown 'object-state
      (detach-attribs! pin2 x))

    (assert-equal pin1 (detach-attribs! pin1 x))
    (assert-equal '() (object-attribs pin1)) ))

(begin-test 'attach-attrib/page
  (let ((P (make-page "/test/page/A"))
        (Q (make-page "/test/page/A"))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
        (y (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
        (z (make-text '(1 . 2) 'lower-left 0 "name=value" 10 #t 'both))
        (C (make-component "test component" '(1 . 2) 0 #t #f)))

    (dynamic-wind ; Make sure pages are cleaned up
        (lambda () #f)
        (lambda ()
          ; Populate pages
          (page-append! P x C)
          (component-append! C p y)

          (page-append! Q z)

          ; Attach attribute to component in same page
          (attach-attribs! C x)
          (assert-equal (list x) (object-attribs C))
          (assert-equal C (attrib-attachment x))

          ; Remove stuff from page
          (assert-thrown 'object-state (page-remove! P x))
          (assert-thrown 'object-state (page-remove! P C))

          ; Attach attribute to component in different page
          (assert-thrown 'object-state (attach-attribs! C z))

          ; Attach attribute to pin in component in page
          (attach-attribs! p y)
          (assert-equal (list y) (object-attribs p))
          (assert-equal p (attrib-attachment y))

          ; Remove stuff from component in page
          (assert-thrown 'object-state (component-remove! C p))
          (assert-thrown 'object-state (component-remove! C y)) )
        (lambda ()
          (close-page! P)
          (close-page! Q) ))

))
