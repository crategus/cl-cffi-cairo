;;;; A simple console application that will create a PNG image for data.

(in-package :cairo-example)

(defun demo-png-image-for-data ()
  (let* ((height 300)
         (width 500)
         (stride (cairo:format-stride-for-width :argb32 width))
         (data (g:malloc (* height stride)))
         (pathname (sys-path "out/image-for-data.png"))
         ;; Create an image surface for data and a Cairo context.
         (surface (cairo:image-surface-create-for-data data
                                                       :argb32
                                                       width height stride))
         (context (cairo:create surface)))
    ;; Clear surface
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Draw in black ink
    (cairo:set-source-rgba context 0.0 0.0 0.0 1.0)
    ;; Choose a font type and set its size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 20.0)
    ;; Move to a position within the image and draw the text
    (cairo:move-to context 10.0 50.0)
    (cairo:show-text context "Cario drawing to an image surface for data.")
    ;; Create and save the PNG image
    (cairo:surface-write-to-png surface pathname)
    ;; Free the resources
    (cairo:destroy context)
    (cairo:surface-destroy surface)
    (g:free data)))

;;; --- 2023-1-26 --------------------------------------------------------------
