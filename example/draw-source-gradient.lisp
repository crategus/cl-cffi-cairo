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

;;; --- 2023-1-14 --------------------------------------------------------------
