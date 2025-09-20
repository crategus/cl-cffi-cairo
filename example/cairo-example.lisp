(defpackage :cairo-example
  (:use :common-lisp)
  (:import-from :cairo)
  (:export #:draw-arc
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

           #:draw-caps
           #:draw-dashes
           #:draw-joins

           #:draw-stroke
           #:draw-fill
           #:draw-text-letter
           #:draw-paint
           #:draw-mask
           #:draw-source-rgba
           #:draw-source-gradient
           #:draw-path
           #:draw-logo
           #:draw-logo-translate

           #:draw-text-centered
           #:draw-text-glyph
           #:draw-text-gradient
           #:draw-text-shaded
           #:draw-text-soulmate

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

           #:demo-script-draw
           #:demo-ps-draw
           ))

(in-package :cairo-example)

;; Ensure directory for the output of examples
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-directories-exist
      (asdf:system-relative-pathname :cairo-example "out/")))

;; Get pathname for a file in the example directory
(defun sys-path (file &optional (system :cairo-example))
  (asdf:system-relative-pathname system file))

;;; 2025-09-20
