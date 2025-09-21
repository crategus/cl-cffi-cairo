;;;; Use the Cairo library to create a simple PDF file.

(in-package :cairo-example)

(defun demo-pdf-simple ()
  (let* (;; Create PDF surface and Cairo context.
         (path (sys-path "out/pdf-simple.pdf"))
         (surface (cairo:pdf-surface-create path 504.0 648.0))
         (context (cairo:create surface)))
    ;; Draw in blue ink.
    (cairo:set-source-rgba context 0 0 1.0 1.0)
    ;; Choose font type and set its size.
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 20.0)
    ;; Move to a position within the image and draw the text.
    (cairo:move-to context 20.0 70.0)
    (cairo:show-text context "Cario drawing to a PDF surface.")
    ;; Finish rendering of the PDF file.
    (cairo:show-page context)
    ;; Clean resources.
    (cairo:surface-destroy surface)
    (cairo:destroy context)))

;;; 2025-09-21
