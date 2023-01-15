(in-package :cairo-example)

(defun draw-path (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0d0 1.0d0 1.0d0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (setf (cairo:line-width context) 0.01d0)
  (cairo:set-source-rgb context 1.0d0 0.0d0 0.0d0)
  (cairo:move-to context 0.25 0.25)
  (cairo:line-to context 0.5 0.375)
  (cairo:rel-line-to context 0.25 -0.125)
  (cairo:arc context 0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 pi) (* 0.25 pi))
  (cairo:rel-curve-to context -0.25 -0.125 -0.25 0.125 -0.5 0)
  (cairo:close-path context)
  (cairo:stroke context)
  (cairo:restore context))

;;; --- 2023-1-14 --------------------------------------------------------------
