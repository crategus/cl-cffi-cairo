;;;; Cairo drawing centered Text
;;;;
;;;; Last update: 2025-09-20

(in-package :cairo-example)

(defun cairo-draw-text-centered (context width height)
  ;; Paint a white background
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Set the color.
  (cairo:set-source-rgb context 0.1 0.1 0.1)
  ;; Select the font face
  (cairo:select-font-face context "Courier" :weight :bold)
  ;; Specify the font size
  (cairo:set-font-size context 60)
  (multiple-value-bind (xbearing ybearing twidth theight xadvance yadvance)
      (cairo:text-extents context "Crategus")
    (declare (ignore xbearing ybearing theight xadvance yadvance))
    ;; Display text on the drawing area
    (cairo:move-to context (- (/ width 2) (/ twidth 2)) (/ height 2))
    (cairo:show-text context "Crategus")))
