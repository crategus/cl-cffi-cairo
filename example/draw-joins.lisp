;;;; Cairo Drawing Line Joins
;;;;
;;;; Last update: 2025-09-20

(in-package :cairo-example)

(defun draw-joins (context width height)
  (let* ((offset (truncate (/ height 5)))
         (border (truncate (/ width 10)))
         (line-width (truncate (/ height 10))))
    ;; Paint a white background
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Set RGB color
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    ;; Set the line width
    (setf (cairo:line-width context) line-width)
    ;; First angle
    (setf (cairo:line-join context) :miter)
    (cairo:move-to context border (- height border))
    (cairo:line-to context border border)
    (cairo:line-to context (- width border) border)
    (cairo:stroke context)
    ;; Second angle
    (setf (cairo:line-join context) :bevel)
    (cairo:move-to context (+ border offset) (- height border))
    (cairo:line-to context (+ border offset) (+ border offset))
    (cairo:line-to context (- width border) (+ border offset))
    (cairo:stroke context)
    ;; Third angle
    (setf (cairo:line-join context) :round)
    (cairo:move-to context (+ border (* 2 offset)) (- height border))
    (cairo:line-to context (+ border (* 2 offset)) (+ border (* 2 offset)))
    (cairo:line-to context (- width border) (+ border (* 2 offset)))
    (cairo:stroke context)))
