(defpackage :cairo-test
  (:use :fiveam :common-lisp)
  (:import-from :cffi  #:with-foreign-object
                       #:with-foreign-objects)
  (:import-from :cairo #:with-cairo-context
                       #:with-cairo-image-surface
                       #:with-cairo-context-for-image-surface
                       #:with-cairo-toy-font-face)
  (:export #:run!
           #:cairo-suite))

(in-package :cairo-test)

(def-suite cairo-suite)
(in-suite cairo-suite)

;;; --- 2023-1-7 ---------------------------------------------------------------
