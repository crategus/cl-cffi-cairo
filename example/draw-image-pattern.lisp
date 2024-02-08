;;;; Draw image pattern
;;;;
;;;; 2024-1-21

;; TODO: Use the cairo:with-matrix macro

(in-package :cairo-example)

(defun draw-image-pattern (context width height)
  (let* ((path (sys-path "resource/ducky.png"))
         (image (cairo:image-surface-create-from-png path))
         (w (cairo:image-surface-width image))
         (h (cairo:image-surface-height image))
         ;; Create a pattern for an image surface
         (pattern (cairo:pattern-create-for-surface image))
         ;; Parameters to scale and translate the context
         (size  (max w h))
         (scale (* 0.70 (/ (min width height) size)))) ; reduce scale to 70 %
    ;; Save the context
    (cairo:save context)
    ;; Translate, rotate and scale the context
    (cairo:translate context (* 0.5 width) (* 0.5 height))
    (cairo:rotate context (/ pi 4))
    (cairo:scale context scale scale)
    (cairo:translate context (* -0.5 w) (* -0.5 h))
    ;; Create and setup a transformation matrix for the pattern
    (cffi:with-foreign-object (matrix '(:struct cairo:matrix-t))
      ;; Set the transformation matrix for the pattern
      (cairo:matrix-init-scale matrix (* 5.0 (/ w size)) (* 5.0 (/ h size)))
      (setf (cairo:pattern-matrix pattern) matrix)
      ;; Fill a rectangle with the pattern
      (setf (cairo:pattern-extend pattern) :repeat)
      (setf (cairo:source context) pattern)
      (cairo:rectangle context 0.0 0.0 size size)
      (cairo:fill context)
      ;; Destroy pattern and image surface
      (cairo:pattern-destroy pattern)
      (cairo:surface-destroy image))
    ;; Restore context
    (cairo:restore context)))
