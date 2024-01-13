(defpackage :cairo-example
  (:use :common-lisp)
  (:import-from :cairo)
  (:export #:run-example

           #:cairo-draw-caps
           #:cairo-draw-dashes
           #:cairo-draw-joins

           #:cairo-draw-stroke
           #:cairo-draw-fill
           #:cairo-draw-text
           #:cairo-draw-paint
           #:cairo-draw-mask
           #:cairo-draw-source-rgba
           #:cairo-draw-source-gradient
           #:cairo-draw-path
           #:cairo-draw-dash
           #:cairo-draw-logo
           #:cairo-draw-logo-translate

           #:cairo-draw-text-centered
           #:cairo-draw-text-glyph
           #:cairo-draw-text-gradient
           #:cairo-draw-text-shaded
           #:cairo-draw-text-soulmate

           #:demo-png-image
           #:demo-png-image-draw
           #:demo-png-image-for-data
           #:demo-png-image-from-png

           #:demo-svg-simple
           #:demo-svg-draw

           #:demo-pdf-simple
           #:demo-pdf-draw
           #:demo-pdf-draw-multipage
           ))

(in-package :cairo-example)

;; Ensure directory for the output of test results
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-directories-exist
      (asdf:system-relative-pathname :cairo-example "out/")))

;; Get the pathname for a file in the example directory
(defun sys-path (filename &optional (system :cairo-example))
  (asdf:system-relative-pathname system filename))

;;; --- 2024-1-12 --------------------------------------------------------------
