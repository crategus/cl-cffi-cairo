(in-package :cairo-example)

(defun demo-pdf-draw (&optional (drawfunc #'draw-stroke))
  (let* (;; Create a PDF surface and a Cairo context.
         (pathname (sys-path "example/out/pdf-draw.pdf"))
         (width 504.0)
         (height 648.0)
         (surface (cairo:pdf-surface-create pathname width height))
         (context (cairo:create surface)))

    (funcall drawfunc context width height)
    (cairo:show-page context)

    ;; Clean the resources.
    (cairo:surface-destroy surface)
    (cairo:destroy context)))

;;; --- 2023-1-14 --------------------------------------------------------------
