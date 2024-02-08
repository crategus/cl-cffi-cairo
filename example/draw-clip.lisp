;;;; Cairo clip
;;;;
;;;; 2024-1-21

(in-package :cairo-example)

(defun draw-clip (context width height)
  (let* ((xcenter (/ width 2.0))
         (ycenter (/ height 2.0))
         (radius (* 0.75 (min xcenter ycenter)))
         (linewidth (* 0.05 radius)))
    ;; Save the context
    (cairo:save context)
    ;; Clip drawing to an arc
    (cairo:arc context xcenter ycenter radius 0 (* 2.0 pi))
    (cairo:clip context)
    ;; Draw a black and filled rectangle in the drawing area which is clipped
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (cairo:new-path context)
    (cairo:rectangle context 0.0 0.0 width height)
    (cairo:fill context)
    ;; Draw two lines from the corners of the drawing area which are clipped
    (cairo:set-source-rgb context 0.0 0.0 1.0)
    (setf (cairo:line-width context) linewidth)
    (cairo:move-to context 0.0 0.0)
    (cairo:line-to context width height)
    (cairo:move-to context 0.0 height)
    (cairo:line-to context width 0.0)
    (cairo:stroke context)
    ;; Restore context
    (cairo:restore context)))
