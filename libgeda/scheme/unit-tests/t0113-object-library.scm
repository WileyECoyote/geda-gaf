;; Test Scheme procedures related to component objects.

(use-modules (unit-test))
(use-modules (geda object))
(use-modules (geda page))

;; Set up component library, making blatant assumptions about the
;; directory layout. WEH 04/03/14: Hacked to get around distchecks
(let ((norm (string-join (list (getenv "srcdir") "../../symbols/passive/resistor") "/"))
      (dist (string-join (list (getenv "srcdir") "../../../symbols/passive/resistor") "/"))
     )
       (if (access? dist R_OK)
           (component-library dist "Basic devices")
           (component-library norm "Basic devices")))

(begin-test 'component/library
  (let ( (A (make-component/library "resistor-1.sym" '(1 . 2) 0 #t #f))
         (B (make-component/library "invalid-component-name" '(1 . 2) 0 #t #f)))

    (assert-true A)
    (assert-equal '(1 . 2) (component-position A))
    (assert-equal 0 (component-angle A))
    (assert-true (component-mirror? A))
    (assert-true (not (component-locked? A)))

    (assert-equal "resistor-1.sym" (component-basename A))

    (assert-true (not (null? (component-contents A))))

    (assert-true (not B)))
)

;; Clear component library again
(reset-component-library)


