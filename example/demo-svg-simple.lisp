;;;; Use the Cairo library to create a simple SVG file.

(in-package :cairo-example)

(defun demo-svg-simple ()
  (let* (;; Create SVG surface and Cairo context
         (path (sys-path "out/svg-simple.svg"))
         (surface (cairo:svg-surface-create path 400 300))
         (context (cairo:create surface)))
    ;; Clear surface
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Draw in blue ink
    (cairo:set-source-rgba context 0 0 1.0 1.0)
    ;; Choose font type and set its size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 20.0)
    ;; Move to a position within the image and draw the text
    (cairo:move-to context 20.0 70.0)
    (cairo:show-text context "Cario drawing to a SVG surface.")
    ;; Free resources
    (cairo:surface-destroy surface)
    (cairo:destroy context)))

;;; 2025-09-21
