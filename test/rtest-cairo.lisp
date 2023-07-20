(defpackage :cairo-test
  (:use :fiveam :common-lisp)
  (:import-from :cffi)
  (:import-from :cairo)
  (:export #:run!
           #:cairo-suite))

(in-package :cairo-test)

(def-suite cairo-suite)
(in-suite cairo-suite)

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(defun approx-equal (x y &optional (eps-factor 1.0d-1))
  (or (< (abs (- x y)) eps-factor)
      (< (abs (- x y)) (* eps-factor (max (abs x) (abs y))))))

;; Ensure directory for the output of test results
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-directories-exist
      (asdf:system-relative-pathname :cl-cffi-cairo "test/out/")))

;; Get the pathname for a file in the testsuite
(defun sys-path (filename &optional (system :cl-cffi-cairo))
  (asdf:system-relative-pathname system
                                 (concatenate 'string "test/" filename)))

;;; --- 2023-1-26 --------------------------------------------------------------
