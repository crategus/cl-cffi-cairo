;;;; Cairo Drawing Line Dashes
;;;;
;;;; Each line can be drawn with a different pen dash. It defines the style of
;;;; the line. The dash is used by the cairo:context-stroke function. The dash
;;;; pattern is specified by the (setf cairo:context-dash) function. The pattern
;;;; is set by the dash list, which is a list of positive floating point values.
;;;; They set the on and off parts of the dash pattern. If the list is empty,
;;;; the dashing is disabled. If it is 1, a symmetric pattern is asumed with
;;;; alternating on and off portions of the size specified by the single value
;;;; in dashes.
;;;;
;;;; Last update: 2025-09-20

(in-package :cairo-example)

(defun draw-dashes (context width height)
  (let* ((offset (truncate (/ height 4)))
         (border (truncate (/ width 10))))
    ;; Paint a white background
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Draw in black ink.
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    ;; Set the line width
    (setf (cairo:line-width context) 2.5)
    ;; First line.
    (setf (cairo:dash context 0) '(4.0 21.0 2.0))
    (cairo:move-to context border offset)
    (cairo:line-to context (- width border) offset)
    (cairo:stroke context)
    ;; Second line.
    (setf (cairo:dash context 1) '(14.0 6.0))
    (cairo:move-to context border (* 2 offset))
    (cairo:line-to context (- width border) (* 2 offset))
    (cairo:stroke context)
    ;; Third line.
    (setf (cairo:dash context 0) '(2.0))
    (cairo:move-to context border (* 3 offset))
    (cairo:line-to context (- width border) (* 3 offset))
    (cairo:stroke context)))
