;;;; Cairo drawing an negative arc
;;;;
;;;; 2024-1-21

(in-package :cairo-example)

(defun draw-arc-negative (context width height)
  (let* ((xcenter (/ width 2.0))
         (ycenter (/ height 2.0))
         (radius (* 0.75 (min xcenter ycenter))) ; 75 % of size
         (angle1 (* 45.0 (/ pi 180.0)))
         (angle2 (* 180.0 (/ pi 180.0)))
         (linewidth (* 0.03 radius))) ; 3 % of radius
    ;; Save the context
    (cairo:save context)
    ;; Draw a negative arc
    (setf (cairo:line-width context) linewidth)
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (cairo:arc-negative context xcenter ycenter radius angle1 angle2)
    (cairo:stroke context)
    ;; Draw red helping lines in hairline style
    (cairo:set-source-rgba context 1.0 0.0 0.0 1.0)
    (setf (cairo:hairline context) t)
    ;; The undrawn part of the arc
    (cairo:arc context xcenter ycenter radius angle1 angle2)
    (cairo:stroke context)
    ;; A filled circle for the center
    (cairo:arc context xcenter ycenter linewidth 0.0 (* 2.0 pi))
    (cairo:fill context)
    ;; Line from the first angle to the center
    (cairo:arc context xcenter ycenter radius angle1 angle1)
    (cairo:line-to context xcenter ycenter)
    ;; Line from the second angle to the center
    (cairo:arc context xcenter ycenter radius angle2 angle2)
    (cairo:line-to context xcenter ycenter)
    (cairo:stroke context)
    ;; Restore the context
    (cairo:restore context)))
