(defpackage :cairo-example
  (:use :common-lisp)
  (:import-from :cairo #:with-cairo-context
                       #:with-cairo-context-for-image-surface
                       #:with-cairo-image-surface)
  (:export #:run-example

           #:draw-caps
           #:draw-dashes
           #:draw-joins

           #:draw-stroke
           #:draw-fill
           #:draw-text
           #:draw-paint
           #:draw-mask
           #:draw-source-rgba
           #:draw-source-gradient
           #:draw-path
           #:draw-dash
           #:draw-logo
           #:draw-logo-translate

           #:draw-text-centered
           #:draw-text-glyph
           #:draw-text-gradient
           #:draw-text-shaded
           #:draw-text-soulmate

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
      (asdf:system-relative-pathname :cl-cffi-cairo "example/out/")))

;; Get the pathname for a file in the example directory
(defun sys-path (filename &optional (system :cl-cffi-cairo))
  (asdf:system-relative-pathname system
                                 (concatenate 'string "example/" filename)))

;;; --- 2023-1-26 --------------------------------------------------------------
