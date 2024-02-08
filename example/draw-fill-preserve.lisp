;;;; Draw with Fill Preserve
;;;;
;;;; 2024-2-8

(in-package :cairo-example)

(defun draw-fill-preserve (context width height)
  (let* (;; Parameters to scale and translate the context
         (w 243.2)
         (h 256.0)
         (size (max w h))
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale w))))
         (ty (* 0.5 (- height (* scale h))))
         (linewidth (* 0.03 size)))
    ;; Save the context
    (cairo:save context)
    ;; Translate and scale the context
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)
    ;; Draw helping rectangles (used to check scaling and translating)
    (cairo:set-source-rgba context 1.0 0.0 0.0 1.0)
    (setf (cairo:hairline context) t)
    (cairo:rectangle context 12.8 25.6 217.6 204.8)
    (cairo:rectangle context  0.0  0.0 w h)
    (cairo:stroke context)
    (setf (cairo:hairline context) nil)
    ;; Draw the first path
    (cairo:move-to context 128.0 25.6)
    (cairo:line-to context 230.4 230.4)
    (cairo:rel-line-to context -102.4 0.0)
    (cairo:curve-to context 51.2 230.4
                            51.2 128.0
                            128.0 128.0)
    (cairo:close-path context)
    ;; Draw a second path
    (cairo:move-to context 64.0 25.6)
    (cairo:rel-line-to context 51.2 51.2)
    (cairo:rel-line-to context -51.2 51.2)
    (cairo:rel-line-to context -51.2 -51.2)
    (cairo:close-path context)
    ;; Fill and stroke the paths
    (setf (cairo:line-width context) linewidth)
    (cairo:set-source-rgb context 0.0 0.0 1.0)
    (cairo:fill-preserve context)
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (cairo:stroke context)
    ;; Restore the context
    (cairo:restore context)))
