;;;; Cairo Drawing Line Caps

(in-package :cairo-example)

(defun drawing-caps (widget cr width height)
  (declare (ignore widget))
  (let* ((offset (truncate (/ height 4)))
         (border (truncate (/ width 5)))
         (line-width (truncate (/ height 10))))
    ;; Draw in black ink.
    (cairo:set-source-rgb cr 0.0 0.0 0.0)
    ;; Set the line width
    (setf (cairo:line-width cr) line-width)
    ;; First line with butt caps
    (setf (cairo:line-cap cr) :butt)
    (cairo:move-to cr border offset)
    (cairo:line-to cr (- width border) offset)
    (cairo:stroke cr)
    ;; Second line with round caps.
    (setf (cairo:line-cap cr) :round)
    (cairo:move-to cr border (* 2 offset))
    (cairo:line-to cr (- width border) (* 2 offset))
    (cairo:stroke cr)
    ;; Third line with square caps.
    (setf (cairo:line-cap cr) :square)
    (cairo:move-to cr border (* 3 offset))
    (cairo:line-to cr (- width border) (* 3 offset))
    (cairo:stroke cr)
    ;; Helper lines to show the line length.
    (cairo:set-source-rgb cr 1.0 0.0 0.0)
    (setf (cairo:line-width cr) 1.0)
    ;; Line on the left side.
    (cairo:move-to cr border (- offset line-width))
    (cairo:line-to cr border (+ (* 3 offset) line-width))
    (cairo:stroke cr)
    ;; Two lines on the right side.
    (cairo:move-to cr (- width border) (- offset line-width))
    (cairo:line-to cr (- width border) (+ (* 3 offset) line-width))
    (cairo:stroke cr)

    (cairo:move-to cr (+ (- width border) (/ line-width 2))
                      (- offset line-width))
    (cairo:line-to cr (+ (- width border) (/ line-width 2))
                      (+ (* 3 offset) line-width))
    (cairo:stroke cr)))

(defun demo-drawing-caps (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Drawing Caps"
                                :default-width 400
                                :default-height 300)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area #'drawing-caps)
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
