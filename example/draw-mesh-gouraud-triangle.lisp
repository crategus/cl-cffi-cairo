;;;; Draw Gouraud Triangle patch
;;;;
;;;; 2024-2-2

(in-package :cairo-example)

(defun draw-mesh-gouraud-triangle-patch (context width height)
  (let* ((size 256)
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size))))
         (pattern (cairo:pattern-create-mesh)))

    ;; Save the context
    (cairo:save context)

    ;; Add a Gouraud-shaded triangle
    (cairo:mesh-pattern-begin-patch pattern)

    (cairo:mesh-pattern-move-to pattern 0 30)
    (cairo:mesh-pattern-line-to pattern 30 60)
    (cairo:mesh-pattern-line-to pattern 30  0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
    (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)

    (cairo:mesh-pattern-end-patch pattern)

    (cairo:with-matrix (matrix 0.25 0 0 0.25 -15 -2)
      (setf (cairo:pattern-matrix pattern) matrix)

      (cairo:translate context tx ty)
      (cairo:scale context scale scale)

      (cairo:rectangle context 0 0 size size)
      (setf (cairo:source context) pattern)
      (cairo:fill-preserve context)
      (cairo:set-source-rgb context 1.0 0.0 0.0)
      (setf (cairo:hairline context) t)
      (cairo:stroke context)

      ;; Destroy the pattern and restore the context
      (cairo:pattern-destroy pattern)
      (cairo:restore context))))
