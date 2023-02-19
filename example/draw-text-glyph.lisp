(in-package :cairo-example)

(defun cairo-draw-text-glyph (context width height)
  (let* ((font-size (min (truncate (- (/ height 20) 5))
                         (truncate (- (/ width 36) 5))))
         (glyphs nil))
    ;; Paint a white background
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Set the color
    (cairo:set-source-rgb context 0.1 0.1 0.1)
    ;; Select the font face
    (cairo:select-font-face context "Purisa")
    ;; Specify the font size
    (cairo:set-font-size context font-size)
    ;; Make a list of 20 * 35 glyphs
    (loop for y from 0 below 20
          do (loop for x from 0 below 35
                   do (push (list (+ x (* y 20))
                                  (+ font-size (* x (+ 5 font-size)))
                                  (+ font-size (* y (+ 5 font-size))))
                            glyphs)))
    (setf glyphs (reverse glyphs))
    ;; Print the glyps extents
    (multiple-value-bind (xbearing ybearing twidth theight xadvance yadvance)
        (cairo:glyph-extents context glyphs)
      (format t "~&bearing ~a ~a~%" xbearing ybearing)
      (format t "~&size ~a ~a~%" twidth theight)
      (format t "~&advance ~a ~a~%" xadvance yadvance))
    ;; Show the list of glyphs
    (cairo:show-glyphs context glyphs)))

;;; --- 2023-2-17 --------------------------------------------------------------
