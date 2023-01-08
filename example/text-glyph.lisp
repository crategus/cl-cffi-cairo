(in-package :cairo-example)

(defun drawing-text-glyph (widget cr width height)
  (declare (ignore widget))
  (let* ((font-size (min (truncate (- (/ height 20) 5))
                         (truncate (- (/ width 36) 5))))
         (glyphs nil)
         (extents nil))
    ;; Set the color.
    (cairo:set-source-rgb cr 0.1 0.1 0.1)
    ;; Select the font face
    (cairo:select-font-face cr "Purisa")
    ;; Specify the font size
    (cairo:set-font-size cr font-size)
    ;; Make a list of 20 * 35 glyphs
    (loop for y from 0 below 20
          do (loop for x from 0 below 35
                   do (push (list (+ x (* y 20))
                                  (+ font-size (* x (+ 5 font-size)))
                                  (+ font-size (* y (+ 5 font-size))))
                            glyphs)))
    (setf glyphs (reverse glyphs))

    ;; Print the glyps extents
    (setf extents (cairo:glyph-extents cr glyphs))
    (format t "~&bearing ~a ~a~%" (cairo:text-extents-x-bearing extents)
                                  (cairo:text-extents-y-bearing extents))
    (format t "~&size ~a ~a~%"    (cairo:text-extents-width extents)
                                  (cairo:text-extents-height extents))
    (format t "~&advance ~a ~a~%" (cairo:text-extents-x-advance extents)
                                  (cairo:text-extents-y-advance extents))
    ;; Show the list of glyphs
    (cairo:show-glyphs cr glyphs)))

(defun demo-text-glyph (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Text Glyph"
                                :default-width 800
                                :default-height 480)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area #'drawing-text-glyph)
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
