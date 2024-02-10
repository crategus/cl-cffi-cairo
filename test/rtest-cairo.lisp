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

;; Utility function for the testsuite
(defun flatten (tree)
  (let (lst)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree lst)))))
      (traverse tree))
    (nreverse lst)))

;; Draw function used for the tests
(defun draw-stroke (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (setf (cairo:line-width context) 0.1)
  (cairo:set-source-rgb context 1.0 0.0 0.0)
  (cairo:rectangle context 0.25 0.25 0.5 0.5)
  (cairo:stroke context)
  (cairo:restore context))

;;; 2024-1-14
