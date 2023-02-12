;;;; Cairo Drawing Line Caps

(in-package :cairo-example)

(defun cairo-draw-caps (context width height)
  (let* ((offset (truncate (/ height 4)))
         (border (truncate (/ width 5)))
         (line-width (truncate (/ height 10))))
    (cairo:save context)
    ;; Paint a white background
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Draw in black ink
    (cairo:set-source-rgb context 0.0 0.0 0.0)
    ;; Set the line width
    (setf (cairo:line-width context) line-width)
    ;; First line with butt caps
    (setf (cairo:line-cap context) :butt)
    (cairo:move-to context border offset)
    (cairo:line-to context (- width border) offset)
    (cairo:stroke context)
    ;; Second line with round caps
    (setf (cairo:line-cap context) :round)
    (cairo:move-to context border (* 2 offset))
    (cairo:line-to context (- width border) (* 2 offset))
    (cairo:stroke context)
    ;; Third line with square caps
    (setf (cairo:line-cap context) :square)
    (cairo:move-to context border (* 3 offset))
    (cairo:line-to context (- width border) (* 3 offset))
    (cairo:stroke context)
    ;; Helper lines to show the line length
    (cairo:set-source-rgb context 1.0 0.0 0.0)
    (setf (cairo:line-width context) 1.0)
    ;; Line on the left side
    (cairo:move-to context border (- offset line-width))
    (cairo:line-to context border (+ (* 3 offset) line-width))
    (cairo:stroke context)
    ;; Two lines on the right side
    (cairo:move-to context (- width border) (- offset line-width))
    (cairo:line-to context (- width border) (+ (* 3 offset) line-width))
    (cairo:stroke context)
    (cairo:move-to context (+ (- width border) (/ line-width 2))
                           (- offset line-width))
    (cairo:line-to context (+ (- width border) (/ line-width 2))
                           (+ (* 3 offset) line-width))
    (cairo:stroke context)
    (cairo:restore context)))

;;; --- 2023-2-12 --------------------------------------------------------------
