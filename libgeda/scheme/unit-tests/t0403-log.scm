;; Test Scheme procedures related to box objects.

(use-modules (unit-test))
(use-modules (geda log))

(begin-test 'log
  (let* ((m1 "The quick brown fox")
         (m2 "jumping over the lazy dogs back"))

    (log! 'critical "~A was critically injured ~A" m1 m2)

    (assert-equal #f (string? (log-read)))

    (log-open "tests")

    (log! 'critical "~A was critically injured ~A" m1 m2)

    (assert-equal #t (string? (log-read)))

    (log! 'warning "~A got a warning for ~A" m1 m2)

    (log! 'message "~A sent a message ~A" m1 m2)

    (log! 'info "~A was ~A without info" m1 m2)

    (log! 'debug "~A found a debug ~A" m1 m2)

    ;;(let ((str (log-read)))
    ;;   (assert-true (string? str))
    ;;)

    (log-close)
))
