;;;; Draw a curve
;;;;
;;;; 2024-2-8

(in-package :cairo-example)

(defun draw-curve-to (context width height)
  (let* (;; Parameters to scale and translate the context
         (size 256) ; User space is 256 x 256 rectangle
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size))))
         ;; Make the linewidth a fraction of size
         (linewidth (* 0.01 size))
         ;; Coordinates of the curve
         (x   25.6) (y  128.0)
         (x1 102.4) (y1 230.4)
         (x2 153.6) (y2  25.6)
         (x3 230.4) (y3 128.0))
    ;; Save the context
    (cairo:save context)
    ;; Translate and scale the context
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)
    ;; Draw the curve
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (setf (cairo:line-width context) linewidth)
    (cairo:move-to context x y)
    (cairo:curve-to context x1 y1 x2 y2 x3 y3)
    (cairo:stroke context)
    ;; Draw helper lines
    (cairo:set-source-rgba context 1.0 0.0 0.0 1.0)
    (setf (cairo:hairline context) t)
    (cairo:move-to context x y)
    (cairo:line-to context x1 y1)
    (cairo:move-to context x2 y2)
    (cairo:line-to context x3 y3)
    (cairo:stroke context)
    ;; Restore the context
    (cairo:restore context)))
