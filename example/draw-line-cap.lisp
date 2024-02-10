(in-package :cairo-example)

(defun draw-line-cap (context width height)
  (let* ((size 256)
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size)))))
    (cairo:save context)
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)

    (cairo:set-source-rgb context 0.0 0.0 0.0)

    (setf (cairo:line-width context) 30)
    (setf (cairo:line-cap context) :butt)
    (cairo:move-to context 64.0 50.0)
    (cairo:line-to context 64.0 200.0)
    (cairo:stroke context)

    (setf (cairo:line-width context) 30)
    (setf (cairo:line-cap context) :round)
    (cairo:move-to context 128.0 50.0)
    (cairo:line-to context 128.0 200.0)
    (cairo:stroke context)

    (setf (cairo:line-width context) 30)
    (setf (cairo:line-cap context) :square)
    (cairo:move-to context 192.0 50.0)
    (cairo:line-to context 192.0 200.0)
    (cairo:stroke context)

    ;; Draw helping lines
    (cairo:set-source-rgb context 1.0 0.2 0.2)
    (setf (cairo:line-width context) 2.56)
    (cairo:move-to context 64.0 50.0)
    (cairo:line-to context 64.0 200.0)
    (cairo:move-to context 128.0 50.0)
    (cairo:line-to context 128.0 200.0)
    (cairo:move-to context 192.0 50.0)
    (cairo:line-to context 192.0 200.0)
    (cairo:stroke context)

    (cairo:restore context)))
