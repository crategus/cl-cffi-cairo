(in-package :cairo-example)

(defun draw-source-rgba (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 0 0 0)
  (cairo:move-to context 0 0)
  (cairo:line-to context 1 1)
  (cairo:move-to context 1 0)
  (cairo:line-to context 0 1)
  (setf (cairo:line-width context) 0.2)
  (cairo:stroke context)
  (cairo:rectangle context 0 0 0.5 0.5)
  (cairo:set-source-rgba context 1 0 0 0.80)
  (cairo:fill context)
  (cairo:rectangle context 0 0.5 0.5 0.5)
  (cairo:set-source-rgba context 0 1 0 0.60)
  (cairo:fill context)
  (cairo:rectangle context 0.5 0 0.5 0.5)
  (cairo:set-source-rgba context 0 0 1 0.40)
  (cairo:fill context)
  (cairo:restore context))

;;; --- 2023-1-14 --------------------------------------------------------------
