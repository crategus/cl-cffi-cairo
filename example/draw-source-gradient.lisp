;;;; Cairo Source Gradient
;;;;
;;;; Gradients describe a progression of colors by setting a start and stop
;;;; reference location and a series of "stops" along the way. Linear gradients
;;;; are built from two points which pass through parallel lines to define the
;;;; start and stop locations. Radial gradients are also built from two points,
;;;; but each has an associated radius of the circle on which to define the
;;;; start and stop locations. Stops are added to the gradient with the
;;;; <tt>cairo:add-color-stop-rgb</tt> and <tt>cairo:add-color-stop-rgba</tt>
;;;; functions which take a color like the <tt>cairo:set-source-rgb</tt>
;;;; function, as well as an offset to indicate where it lies between the
;;;; reference locations. The colors between adjacent stops are averaged over
;;;; space to form a smooth blend. Finally, the behavior beyond the reference
;;;; locations can be controlled with the <tt>cairo:pattern-extend</tt>
;;;; function.
;;;;
;;;; 2025-09-20

(in-package :cairo-example)

(defun draw-source-gradient (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (let ((radpat (cairo:pattern-create-radial 0.25 0.25 0.10
                                             0.50 0.50 0.50))
        (linpat (cairo:pattern-create-linear 0.25 0.35 0.75 0.65)))
    (cairo:pattern-add-color-stop-rgb radpat 0.00 1.00 0.80 0.80)
    (cairo:pattern-add-color-stop-rgb radpat 1.00 0.90 0.00 0.00)
    (dotimes (i 9)
      (dotimes (j 9)
        (cairo:rectangle context
                         (- (/ (1+ i) 10.0) 0.04)
                         (- (/ (1+ j) 10.0) 0.04)
                         0.08
                         0.08)))
    (setf (cairo:source context) radpat)
    (cairo:fill context)
    (cairo:pattern-add-color-stop-rgba linpat 0.00 1.0 1.0 1.0 0.0)
    (cairo:pattern-add-color-stop-rgba linpat 0.25 0.0 1.0 0.0 0.5)
    (cairo:pattern-add-color-stop-rgba linpat 0.50 1.0 1.0 1.0 0.0)
    (cairo:pattern-add-color-stop-rgba linpat 0.75 0.0 0.0 1.0 0.5)
    (cairo:pattern-add-color-stop-rgba linpat 1.00 1.0 1.0 1.0 0.0)
    (cairo:rectangle context 0.0 0.0 1.0 1.0)
    (setf (cairo:source context) linpat)
    (cairo:fill context))
  (cairo:restore context))
