(in-package :cairo-example)

(defun draw-line-join (context width height)
  (let* ((size 256)
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size))))
        )

    (cairo:save context)
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)

    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (setf (cairo:line-width context) 40.96)

    (cairo:move-to context 76.8 84.48)
    (cairo:rel-line-to context 51.2 -51.2)
    (cairo:rel-line-to context 51.2  51.2)
    (setf (cairo:line-join context) :miter)
    (cairo:stroke context)

    (cairo:move-to context 76.8 161.28)
    (cairo:rel-line-to context 51.2 -51.2)
    (cairo:rel-line-to context 51.2  51.2)
    (setf (cairo:line-join context) :bevel)
    (cairo:stroke context)

    (cairo:move-to context 76.8 238.08)
    (cairo:rel-line-to context 51.2 -51.2)
    (cairo:rel-line-to context 51.2  51.2)
    (setf (cairo:line-join context) :round)
    (cairo:stroke context)

    (cairo:restore context)))
