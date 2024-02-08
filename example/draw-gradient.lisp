;;;; Draw gradient
;;;;
;;;; 2024-1-21

(in-package :cairo-example)

(defun draw-gradient (context width height)
  (let* ((size 256)
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size))))
         (pattern1 (cairo:pattern-create-linear 0.0 0.0 0.0 256.0))
         (pattern2 (cairo:pattern-create-radial 115.2 102.4  25.6
                                                102.4 102.4 128.0)))
    ;; Save the context
    (cairo:save context)

    ;; Draw a linear pattern on the background of the drawing area
    (cairo:pattern-add-color-stop-rgba pattern1 1.0 0.0 0.0 0.0 1.0)
    (cairo:pattern-add-color-stop-rgba pattern1 0.0 1.0 1.0 1.0 1.0)
    (cairo:rectangle context 0 0 width height)
    (setf (cairo:source context) pattern1)
    (cairo:fill context)
    (cairo:pattern-destroy pattern1)
    ;; Scale and translate the context
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)
    ;; Draw a radial pattern on an arc
    (cairo:pattern-add-color-stop-rgba pattern2 0.0 1.0 1.0 1.0 1.0)
    (cairo:pattern-add-color-stop-rgba pattern2 1.0 0.0 0.0 0.0 1.0)
    (setf (cairo:source context) pattern2)
    (cairo:arc context 128.0 128.0 76.8 0.0 (* 2.0 pi))
    (cairo:fill context)
    (cairo:pattern-destroy pattern2)
    ;; Restore the context
    (cairo:restore context)))
