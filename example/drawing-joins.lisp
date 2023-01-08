;;;; Cairo Drawing Line Caps

(in-package :cairo-example)

(defun drawing-joins (widget cr width height)
  (declare (ignore widget))
  (let* ((offset (truncate (/ height 5)))
         (border (truncate (/ width 10)))
         (line-width (truncate (/ height 10))))
    ;; Set RGB color.
    (cairo:set-source-rgb cr 0.0 0.0 0.0)
    ;; Set the line width
    (setf (cairo:line-width cr) line-width)
    ;; First angle
    (setf (cairo:line-join cr) :miter)
    (cairo:move-to cr border (- height border))
    (cairo:line-to cr border border)
    (cairo:line-to cr (- width border) border)
    (cairo:stroke cr)
    ;; Second angle
    (setf (cairo:line-join cr) :bevel)
    (cairo:move-to cr (+ border offset) (- height border))
    (cairo:line-to cr (+ border offset) (+ border offset))
    (cairo:line-to cr (- width border) (+ border offset))
    (cairo:stroke cr)
    ;; Third angle
    (setf (cairo:line-join cr) :round)
    (cairo:move-to cr (+ border (* 2 offset)) (- height border))
    (cairo:line-to cr (+ border (* 2 offset)) (+ border (* 2 offset)))
    (cairo:line-to cr (- width border) (+ border (* 2 offset)))
    (cairo:stroke cr)))

(defun demo-drawing-joins (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                               :child area
                               :application application
                               :title "Cairo Drawing Caps"
                               :default-width 300
                               :default-height 300)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area #'drawing-joins)
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
