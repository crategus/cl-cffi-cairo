;;;; Draw image
;;;;
;;;; 2024-1-21

(in-package :cairo-example)

(defun draw-image (context width height)
  (let* (;; Load an image from a path
         (path (sys-path "resource/ducky.png"))
         (image (cairo:image-surface-create-from-png path))
         ;; Get the size of the image
         (w (cairo:image-surface-width image))
         (h (cairo:image-surface-height image))
         ;; Parameters to scale and translate the context
         (size  (max w h))
         (scale (* 0.75 (/ (min width height) size)))) ; reduce scale to 75 %
    ;; Save the context
    (cairo:save context)
    ;; Translate to the center of the drawing area
    (cairo:translate context (* 0.5 width) (* 0.5 height))
    ;; Rotate around the center of the drawing area
    (cairo:rotate context (/ pi 4))
    ;; Scale the context to user coordinates
    (cairo:scale context scale scale)
    ;; Reverse the translation in user coordinates
    (cairo:translate context (* -0.5 w) (* -0.5 h))
    ;; Paint the image surface
    (cairo:set-source-surface context image 0.0 0.0)
    (cairo:paint context)
    ;; Destroy the image surface
    (cairo:surface-destroy image)
    ;; Restore the context
    (cairo:restore context)))
