;; Test Scheme procedures defined in geda.scm.  Makes blatant
;; assumptions about the current directory.  Oh well.

(use-modules (unit-test) (geda os))

(load-from-path "geda.scm")

(begin-test 'build-path
 (let* ((path1 (string-append "prefix" separator "suffix"))
        (path2 (string-append "path" separator "to" separator "a" separator "directory")))

       (assert-equal path1 (build-path "prefix" "suffix"))
       (assert-equal path2 (build-path "path" "to" "a" "directory"))))

(begin-test 'regular-file?
 (assert-true (regular-file? "Makefile"))
 (assert-true (not (regular-file? "."))))

(begin-test 'directory?
 (assert-true (directory? "."))
 (assert-true (not (directory? "Makefile"))))

(begin-test 'has-suffix?
 (assert-true (has-suffix? "unit-test.scm" ".scm"))
 (assert-true (not (has-suffix? "Makefile" ".scm"))))
