(in-package :cairo-example)

(defun demo-svg-draw (&optional (drawfunc #'cairo-draw-stroke))
  (let* (;; Create a SVG surface and a Cairo context.
         (path (sys-path "out/svg-draw.svg"))
         (width 400)
         (height 400)
         (surface (cairo:svg-surface-create path width height))
         (context (cairo:create surface)))
    (funcall drawfunc context width height)
    (cairo:surface-show-page surface)
    ;; Free the resources
    (cairo:surface-destroy surface)
    (cairo:destroy context)))

;;; --- 2023-2-12 --------------------------------------------------------------
