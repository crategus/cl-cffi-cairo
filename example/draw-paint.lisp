;;;; Cairo Paint
;;;;
;;;; The <tt>cairo:paint</tt> operation uses a mask that transfers the entire
;;;; source to the destination. Some people consider this an infinitely large
;;;; mask, and others consider it no mask; the result is the same. The related
;;;; operation <tt>cairo:paint-with-alpha</tt> similarly allows transfer of the
;;;; full source to destination, but it transfers only the provided percentage
;;;; of the color.
;;;;
;;;; 2025-1-16

(in-package :cairo-example)

(defun cairo-draw-paint (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 1.0 0.0 0.0)
  (cairo:paint-with-alpha context 0.5)
  (cairo:restore context))
