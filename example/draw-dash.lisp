(in-package :cairo-example)

(defun cairo-draw-dash (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (let* ((scale 300)
         (dashes (list (/ 50.0 scale)
                       (/ 10.0 scale)
                       (/ 10.0 scale)
                       (/ 10.0 scale)))
         (offset (/ -50.0 scale)))
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (setf (cairo:dash context offset) dashes)
    (setf (cairo:line-width context) (/ 10.0 scale))
    (cairo:move-to context (/ 128.0 scale) (/ 25.6 scale))
    (cairo:line-to context (/ 230.4 scale) (/ 230.4 scale))
    (cairo:rel-line-to context (/ -102.4 scale) 0.0)
    (cairo:curve-to context (/ 51.2 scale)
                            (/ 230.4 scale)
                            (/ 51.2 scale)
                            (/ 128.0 scale)
                            (/ 128.0 scale)
                            (/ 128.0 scale))
    (cairo:stroke context))
  (cairo:restore context))

;;; --- 2023-2-12 --------------------------------------------------------------
