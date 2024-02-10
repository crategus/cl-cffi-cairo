(defpackage :cairo-example
  (:use :common-lisp)
  (:import-from :cairo)
  (:export #:run-example

           #:draw-arc
           #:draw-arc-negative
           #:draw-clip
           #:draw-clip-image
           #:draw-curve-to
           #:draw-dash
           #:draw-fill-preserve
           #:draw-fill-style
           #:draw-gradient
           #:draw-image
           #:draw-image-pattern
           #:draw-multi-caps
           #:draw-rounded-rectangle
           #:draw-line-cap
           #:draw-line-join
           #:draw-text
           #:draw-text-align-center
           #:draw-text-extents

           #:draw-mesh-coons-patch
           #:draw-mesh-gouraud-triangle-patch

           #:cairo-draw-caps
           #:cairo-draw-dashes
           #:cairo-draw-joins

           #:cairo-draw-stroke
           #:cairo-draw-fill
           #:cairo-draw-text-letter
           #:cairo-draw-paint
           #:cairo-draw-mask
           #:cairo-draw-source-rgba
           #:cairo-draw-source-gradient
           #:cairo-draw-path
           #:cairo-draw-logo
           #:cairo-draw-logo-translate

           #:cairo-draw-text-centered
           #:cairo-draw-text-glyph
           #:cairo-draw-text-gradient
           #:cairo-draw-text-shaded
           #:cairo-draw-text-soulmate

           ;; More draw functions
           #:draw-path-glyph

           #:demo-png-image
           #:demo-png-image-draw
           #:demo-png-image-for-data
           #:demo-png-image-from-png

           #:demo-svg-simple
           #:demo-svg-draw

           #:demo-pdf-simple
           #:demo-pdf-draw
           #:demo-pdf-draw-multipage

           #:script-draw
           #:ps-draw
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
