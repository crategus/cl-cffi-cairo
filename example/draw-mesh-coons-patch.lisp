;;;; Draw Coons patch
;;;;
;;;; 2024-2-2

(in-package :cairo-example)

(defun draw-mesh-coons-patch (context width height)
  (let* ((size 256)
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size))))
         (pattern (cairo:pattern-create-mesh)))

    ;; Save the context
    (cairo:save context)

    ;; Draw a Coons pattern on the background of the drawing area
    (cairo:mesh-pattern-begin-patch pattern)

    (cairo:mesh-pattern-move-to pattern 0 0)
    (cairo:mesh-pattern-curve-to pattern 30 -30 60 30 100 0)
    (cairo:mesh-pattern-curve-to pattern 60 30 130 60 100 100)
    (cairo:mesh-pattern-curve-to pattern 60 70 30 130 0 100)
    (cairo:mesh-pattern-curve-to pattern 30 70 -30 30 0 0)

    (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 3 1 1 0)

    (cairo:mesh-pattern-end-patch pattern)

    ;; Scale and translate the pattern
    (cairo:with-matrix (matrix 0.5 0 0 0.5 -15 -15)
      (setf (cairo:pattern-matrix pattern) matrix)

      ;; Scale and translate the context
      (cairo:translate context tx ty)
      (cairo:scale context scale scale)

      (cairo:rectangle context 0 0 size size)
      (setf (cairo:source context) pattern)
      (cairo:fill-preserve context)
      (cairo:set-source-rgb context 1.0 0.0 0.0)
      (cairo:stroke context)
      (cairo:pattern-destroy pattern)

      ;; Restore the context
      (cairo:restore context))))
