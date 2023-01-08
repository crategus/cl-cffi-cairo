(in-package :cairo-example)

(defun drawing-text-gradient (widget cr width height)
  (declare (ignore widget))
  (let* ((font-size (truncate (/ height 5)))
         (pattern (cairo:pattern-create-linear 0 15 0 (* 0.8 font-size))))
    ;; Set the color.
    (cairo:set-source-rgb cr 0.2 0.2 0.2)
    (cairo:paint cr)
    ;; Select the font face
    (cairo:select-font-face cr "Serif" :slant :italic :weight :bold)
    ;; Specify the font size
    (cairo:set-font-size cr font-size)

    (setf (cairo:pattern-extend pattern) :repeat)
    (cairo:pattern-add-color-stop-rgb pattern 0.0 1.0 0.6 0.0)
    (cairo:pattern-add-color-stop-rgb pattern 0.5 1.0 0.3 0.0)

    (let* ((extents (cairo:text-extents cr "Crategus"))
           (text-width (cairo:text-extents-width extents)))

      ;; Display text on the drawing area
      (cairo:move-to cr (- (/ width 2) (/ text-width 2)) (/ height 2))
      (cairo:text-path cr "Crategus")

      (setf (cairo:source cr) pattern)
      (cairo:fill cr))))

(defun demo-text-gradient (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Text Gradient"
                                :default-width 400
                                :default-height 300)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area #'drawing-text-gradient)
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
