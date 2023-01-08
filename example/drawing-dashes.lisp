;;;; Cairo Drawing Line Dashes
;;;;
;;;; Each line can be drawn with a different pen dash. It defines the style of
;;;; the line. The dash is used by the cairo:context-stroke function. The dash
;;;; pattern is specified by the (setf cairo:context-dash) function. The pattern
;;;; is set by the dash list, which is a list of positive floating point values.
;;;; They set the on and off parts of the dash pattern. If the list is empty,
;;;; the dashing is disabled. If it is 1, a symmetric pattern is asumed with
;;;; alternating on and off portions of the size specified by the single value
;;;; in dashes.

(in-package :cairo-example)

(defun drawing-dashes (widget cr width height)
  (declare (ignore widget))
  (let* ((offset (truncate (/ height 4)))
         (border (truncate (/ width 10))))
    ;; Draw in black ink.
    (cairo:set-source-rgb cr 0.0 0.0 0.0)
    ;; Set the line width
    (setf (cairo:line-width cr) 2.5)
    ;; First line.
    (setf (cairo:dash cr 0) '(4.0 21.0 2.0))
    (cairo:move-to cr border offset)
    (cairo:line-to cr (- width border) offset)
    (cairo:stroke cr)
    ;; Second line.
    (setf (cairo:dash cr 1) '(14.0 6.0))
    (cairo:move-to cr border (* 2 offset))
    (cairo:line-to cr (- width border) (* 2 offset))
    (cairo:stroke cr)
    ;; Third line.
    (setf (cairo:dash cr 0) '(2.0))
    (cairo:move-to cr border (* 3 offset))
    (cairo:line-to cr (- width border) (* 3 offset))
    (cairo:stroke cr)))

(defun demo-drawing-dashes (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Drawing Dashes"
                                :default-width 400
                                :default-height 300)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area #'drawing-dashes)
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
