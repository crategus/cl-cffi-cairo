(in-package :cairo-example)

(defun cairo-draw-paint (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 0.0 0.0 0.0)
  (cairo:paint-with-alpha context 0.5d0)
  (cairo:restore context))

;;; --- 2023-2-12 --------------------------------------------------------------
