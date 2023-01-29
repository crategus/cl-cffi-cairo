;;;; A simple console application that will create a PNG image from an image.

(in-package :cairo-example)

(defun demo-png-image-from-png ()
  (let* (;; Create an image surface from PNG file and a Cairo context.
         (pathname (sys-path "resource/ducky.png"))
         (surface (cairo:image-surface-create-from-png pathname))
         (context (cairo:create surface)))
    (when (not (eq :success (cairo:surface-status surface)))
      (error "Error loading the file ~a.~%" pathname))
    ;; Draw in black ink
    (cairo:set-source-rgba context 0.0 0.0 0.0 1.0)
    ;; Choose a font type and set its size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18.0)
    ;; Move to a position within the image and draw the text
    (cairo:move-to context 10.0 50.0)
    (cairo:show-text context "Cario drawing to an image surface from PNG file.")
    ;; Create and save the PNG image
    (cairo:surface-write-to-png surface
                                (sys-path "out/image-from-png.png"))
    ;; Free the resources
    (cairo:destroy context)
    (cairo:surface-destroy surface)))

;;; --- 2023-1-26 --------------------------------------------------------------
