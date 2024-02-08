;;;; Draw with fill style
;;;;
;;;; 2024-1-21

(in-package :cairo-example)

(defun draw-fill-style (context width height)
  (let* (;; Parameters to scale and translate the context
         (w 256)
         (h 244)
         (size (max w h))
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale w))))
         (ty (* 0.5 (- height (* scale h))))
         (linewidth (* 0.02 size)))
    ;; Save the context
    (cairo:save context)
    ;; Translate and scale the context
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)
    ;; Set linewidth for the paths
    (setf (cairo:line-width context) linewidth)
    ;; First rectangle and two arcs
    (cairo:rectangle context 12 12 232 70)
    (cairo:new-sub-path context)
    (cairo:arc context 64 64 40 0 (* 2 pi))
    (cairo:new-sub-path context)
    (cairo:arc-negative context 192 64 40 0 (* -2 pi))
    ;; Fill with :even-odd style and stroke
    (setf (cairo:fill-rule context) :even-odd)
    (cairo:set-source-rgb context 0 1.0 0)
    (cairo:fill-preserve context)
    (cairo:set-source-rgb context 0 0 0)
    (cairo:stroke context)
    ;; Second rectangle and two arcs
    (cairo:translate context 0 128)
    (cairo:rectangle context 12 12 232 70)
    (cairo:new-sub-path context)
    (cairo:arc context 64 64 40 0 (* 2 pi))
    (cairo:new-sub-path context)
    (cairo:arc-negative context 192 64 40 0 (* -2 pi))
    ;; Fill with :winding style and stroke
    (setf (cairo:fill-rule context) :winding)
    (cairo:set-source-rgb context 0 0 1.0)
    (cairo:fill-preserve context)
    (cairo:set-source-rgb context 0 0 0)
    (cairo:stroke context)
    ;; Restore the context
    (cairo:restore context)))
