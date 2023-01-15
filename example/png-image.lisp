;;;; A simple console application that will create a PNG image.

(in-package :cairo-example)

(defun demo-png-image ()
  (with-cairo-context-for-image-surface (context :argb32 400 300)
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
    (cairo:show-text context "Cario drawing to an image surface.")
    ;; Create and save the PNG image
    (format t "Write image to ~a~%" (sys-path "example/out/image.png"))
    (cairo:surface-write-to-png (cairo:target context)
                                (sys-path "example/out/image.png"))))

;;; --- 2023-1-14 --------------------------------------------------------------
