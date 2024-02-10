;;;; Draw a dashed curve
;;;;
;;;; 2024-1-21

(in-package :cairo-example)

(defun draw-dash (context width height)
  (let* (;; Parameters to scale and translate the context
         (size 256)
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size))))
         ;; Parameters that define the dashes
         (dashes (list 50.0 10.0 10.0 10.0))
         (offset -50.0)
         (linewidth (* 0.03 size)))
    ;; Save the context
    (cairo:save context)
    ;; Translate and scale the context
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)
    ;; Draw a dashed curve
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (setf (cairo:dash context offset) dashes)
    (setf (cairo:line-width context) linewidth)
    (cairo:move-to context 128.0 25.6)
    (cairo:line-to context 230.4 230.4)
    (cairo:rel-line-to context -102.4 0.0)
    (cairo:curve-to context 51.2 230.4 51.2 128.0 128.0 128.0)
    (cairo:stroke context))
    ;; Restore the context
    (cairo:restore context))
