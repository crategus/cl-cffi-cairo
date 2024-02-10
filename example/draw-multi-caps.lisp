(in-package :cairo-example)

(defun draw-multi-caps (context width height)
  (let* ((size 256)
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size)))))
    (cairo:save context)
    ;; Translate and scale the context
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)
    ;; First line
    (cairo:move-to context  50.0 75.0)
    (cairo:line-to context 200.0 75.0)
    ;; Second line
    (cairo:move-to context  50.0 125.0)
    (cairo:line-to context 200.0 125.0)
    ;; Third line
    (cairo:move-to context  50.0 175.0)
    (cairo:line-to context 200.0 175.0)
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    ;; Stroke the lines
    (setf (cairo:line-width context) 30.0)
    (setf (cairo:line-cap context) :round)
    (cairo:stroke context)
    (cairo:restore context)))
