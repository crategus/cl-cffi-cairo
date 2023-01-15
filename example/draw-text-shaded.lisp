(in-package :cairo-example)

(defun draw-text-shaded (context width height)
  ;; Paint a white background
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Select the font face
  (cairo:select-font-face context "Serif" :weight :bold)
  ;; Specify the font size
  (cairo:set-font-size context (truncate (/ height 5)))
  (multiple-value-bind (xbearing ybearing twidth theight xadvance yadvance)
      (cairo:text-extents context "Crategus")
    (declare (ignore xbearing ybearing theight xadvance yadvance))
    ;; Display text on the drawing area
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (cairo:move-to context (- (/ width 2) (/ twidth 2)) (/ height 2))
    (cairo:show-text context "Crategus")
    ;; Display text on the drawing area
    (cairo:set-source-rgb context 0.5 0.5 0.5)
    (cairo:move-to context (+ 3 (- (/ width 2) (/ twidth 2)))
                           (+ 3 (/ height 2)))
    (cairo:show-text context "Crategus")))

;;; --- 2023-1-15 --------------------------------------------------------------
