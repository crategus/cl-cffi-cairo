;;;; Cairo Stroke
;;;;
;;;; The <tt>cairo:stroke</tt> operation takes a virtual pen along the path.
;;;; It allows the source to transfer through the mask in a thin (or thick) line
;;;; around the path, according to the pen's line width, dash style, and line
;;;; caps.
;;;;
;;;; 2025-09-20

(in-package :cairo-example)

(defun draw-stroke (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (setf (cairo:line-width context) 0.1)
  (cairo:set-source-rgb context 1.0 0.0 0.0)
  (cairo:rectangle context 0.25 0.25 0.5 0.5)
  (cairo:stroke context)
  (cairo:restore context))
