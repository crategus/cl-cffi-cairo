(in-package :cairo-example)

(defun drawing-text-centered (widget cr width height)
  (declare (ignore widget))
  ;; Set the color.
  (cairo:set-source-rgb cr 0.1 0.1 0.1)
  ;; Select the font face
  (cairo:select-font-face cr "Courier" :weight :bold)
  ;; Specify the font size
  (cairo:set-font-size cr 60)
  (let* ((extents (cairo:text-extents cr "Crategus"))
         (text-width (cairo:text-extents-width extents)))
    ;; Display text on the drawing area
    (cairo:move-to cr (- (/ width 2) (/ text-width 2)) (/ height 2))
    (cairo:show-text cr "Crategus")))

(defun demo-text-centered (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Text Centered"
                                :default-width 400
                                :default-height 300)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area #'drawing-text-centered)
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
