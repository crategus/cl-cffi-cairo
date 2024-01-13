(in-package :cairo-example)

(defun demo-script-draw (&optional (drawfunc #'cairo-draw-stroke))
  (let ((path (sys-path "out/script-draw.script"))
        (width 400)
        (height 400))
    (cairo:with-script-surface (surface path :color width height)
      (cairo:with-context (context surface)
        (funcall drawfunc context width height)
        (cairo:surface-show-page surface)))))

;;; --- 2024-1-12 --------------------------------------------------------------
