(in-package :cairo-example)

(defun cairo-draw-text-gradient (context width height)
  (let* ((font-size (truncate (/ height 5)))
         (pattern (cairo:pattern-create-linear 0 15 0 (* 0.8 font-size))))
    ;; Paint a dark background
    (cairo:set-source-rgb context 0.2 0.2 0.2)
    (cairo:paint context)
    ;; Select the font face
    (cairo:select-font-face context "Serif" :slant :italic :weight :bold)
    ;; Specify the font size
    (cairo:set-font-size context font-size)

    (setf (cairo:pattern-extend pattern) :repeat)
    (cairo:pattern-add-color-stop-rgb pattern 0.0 1.0 0.6 0.0)
    (cairo:pattern-add-color-stop-rgb pattern 0.5 1.0 0.3 0.0)

    (multiple-value-bind (xbearing ybearing twidth theight xadvance yadvance)
        (cairo:text-extents context "Crategus")
      (declare (ignore xbearing ybearing theight xadvance yadvance))
      ;; Display text on the drawing area
      (cairo:move-to context (- (/ width 2) (/ twidth 2)) (/ height 2))
      (cairo:text-path context "Crategus")
      (setf (cairo:source context) pattern)
      (cairo:fill context))))

;;; --- 2023-2-12 --------------------------------------------------------------
