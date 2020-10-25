(defpackage myemacs/tests/main
  (:use :cl
        :myemacs
        :rove))
(in-package :myemacs/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :myemacs)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
