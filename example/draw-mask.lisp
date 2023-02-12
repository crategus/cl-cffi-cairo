(in-package :cairo-example)

(defun cairo-draw-mask (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (let ((linpat (cairo:pattern-create-linear 0.0 0.0 1.0 1.0))
        (radpat (cairo:pattern-create-radial 0.5 0.5 0.25
                                             0.5 0.5 0.75)))
    (cairo:pattern-add-color-stop-rgb linpat 0 0 0.3 0.8)
    (cairo:pattern-add-color-stop-rgb linpat 1 0 0.8 0.3)
    (cairo:pattern-add-color-stop-rgba radpat 0 0 0 0 1)
    (cairo:pattern-add-color-stop-rgba radpat 0.5 0 0 0 0)
    (setf (cairo:source context) linpat)
    (cairo:mask context radpat)
    (cairo:pattern-destroy linpat)
    (cairo:pattern-destroy radpat))
  (cairo:restore context))

;;; --- 2023-2-12 --------------------------------------------------------------
