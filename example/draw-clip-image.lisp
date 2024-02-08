;;;; Cairo clip image
;;;;
;;;; 2024-2-8

(in-package :cairo-example)

(defun draw-clip-image (context width height)
  (let* (;; Load an image from a path
         (path (sys-path "resource/ducky.png"))
         (image (cairo:image-surface-create-from-png path))
         ;; Get the size of the image
         (w (cairo:image-surface-width image))
         (h (cairo:image-surface-height image))
         ;; Parameters to scale and translate the context
         (size (max w h))
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale w))))
         (ty (* 0.5 (- height (* scale h)))))
    ;; Save the context
    (cairo:save context)
    ;; Translate first, then scale the context, the order is important
    (cairo:translate context tx ty)
    (cairo:scale context scale scale)
    ;; Clip with an arc
    (cairo:arc context (/ w 2.0) (/ h 2.0) (/ size 4.0) 0.0 (* 2.0 pi))
    (cairo:clip context)
    ;; Paint image on the context
    (cairo:new-path context)
    (cairo:set-source-surface context image 0 0)
    (cairo:paint context)
    ;; Destroy surface and restore context
    (cairo:surface-destroy image)
    (cairo:restore context)))
