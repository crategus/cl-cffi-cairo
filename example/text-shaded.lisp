(in-package :cairo-example)

(defun drawing-text-shaded (widget cr width height)
  (declare (ignore widget))
  ;; Select the font face
  (cairo:select-font-face cr "Serif" :weight :bold)
  ;; Specify the font size
  (cairo:set-font-size cr (truncate (/ height 5)))
  (let* ((extents (cairo:text-extents cr "Crategus"))
         (text-width (cairo:text-extents-width extents)))
    ;; Display text on the drawing area
    (cairo:set-source-rgb cr 0.0 0.0 0.0)
    (cairo:move-to cr (- (/ width 2) (/ text-width 2)) (/ height 2))
    (cairo:show-text cr "Crategus")
    ;; Display text on the drawing area
    (cairo:set-source-rgb cr 0.5 0.5 0.5)
    (cairo:move-to cr (+ 3 (- (/ width 2) (/ text-width 2)))
                      (+ 3 (/ height 2)))
    (cairo:show-text cr "Crategus")))

(defun demo-text-shaded (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Text Shaded"
                                :default-width 400
                                :default-height 300)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area #'drawing-text-shaded)
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
