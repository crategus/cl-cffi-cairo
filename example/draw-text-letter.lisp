(in-package :cairo-example)

(defun cairo-draw-text-letter (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 0.0 0.0 0.0)
  (cairo:select-font-face context "Georgia" :weight :bold)
  (cairo:set-font-size context 1.2)
  (multiple-value-bind (xbearing ybearing twidth theight xadvance yadvance)
      (cairo:text-extents context "a")
    (declare (ignore xadvance yadvance))
    (cairo:move-to context
                   (- 0.5 (/ twidth 2) xbearing)
                   (- 0.5 (/ theight 2) ybearing))
     (cairo:show-text context "a"))
  (cairo:restore context))

;;; --- 2023-2-12 --------------------------------------------------------------
