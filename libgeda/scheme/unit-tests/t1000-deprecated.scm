;; Test deprecated procedures from legacy Scheme API.

(use-modules (unit-test))
(use-modules (geda deprecated))
(use-modules (geda object))
(use-modules (geda attrib))
(use-modules (geda page))

(begin-test 'calcule-new-attrib-bounds
  ; Can't actually test this procedure in libgeda only, due to the
  ; absence of a function for calculating text bounds.
  #f)

(begin-test 'get-attribute-bounds
  ; Can't actually test this procedure in libgeda only, due to the
  ; absence of a function for calculating text bounds.
  #f)

(begin-test 'get-object-attributes
  (let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
        (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both)))

    (for-each (lambda (o) (component-append! C o)) (list p x y))
    (attach-attribs! p x y)

    (assert-equal (list y x) (get-object-attributes p))))

(begin-test 'get-attrib-value-by-attrib-name
  (let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (x (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both))
        (y (make-text '(0 . 0) 'lower-left 0 "name=y" 10 #t 'both))
        (z (make-text '(0 . 0) 'lower-left 0 "bork=z" 10 #t 'both)))

    (for-each (lambda (o) (component-append! C o)) (list p x y z))
    (attach-attribs! p x y z)

    (assert-equal (list "y" "x") (get-attrib-value-by-attrib-name p "name"))

    ;;WEH The next test is not a valid test because set-text will set the would
    ;;be attribute to be a text object but is still in the object's attribute
    ;;list, e.g. is set-text- not set-attribute. There is no automatic attribute
    ;;demotion mechanism in Libgeda.

    ; make an invalid atribute
    (set-text! y '(0 . 0) 'lower-left 0 "name=" 10 #t 'both)
    (assert-equal (list "x") (get-attrib-value-by-attrib-name p "name"))
  )
)

(begin-test 'get-object-type
  (let ((C (make-component "testcomponent" '(0 . 0) 0 #f #f))
        (p (make-net-pin '(0 . 0) '(100 . 0)))
        (t (make-text '(0 . 0) 'lower-left 0 "name=x" 10 #t 'both)))

    ; Obviously not exhaustive
    (assert-equal OBJ_COMPLEX (get-object-type C))
    (assert-equal OBJ_PIN (get-object-type p))
    (assert-equal OBJ_TEXT (get-object-type t))))

(begin-test 'get-line-width
  (let ((p (make-net-pin '(0 . 0) '(100 . 0))))

    ; In order to pass the numeric value on the left must be equal to
    ; the DEFAULT_THICK_LINE_WIDTH width in defines.h
    (assert-equal 30 (get-line-width p))))

(define P (make-page "/test/page/A"))

(begin-test 'get-page-filename
  (assert-equal "/test/page/A" (get-page-filename P)))

(begin-test 'set-page-filename
  (set-page-filename P "/test/page/B")
  (assert-equal "/test/page/B" (page-filename P)))

(close-page! P)
