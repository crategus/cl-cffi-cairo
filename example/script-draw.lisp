(in-package :cairo-example)

(defun demo-script-draw (&optional (drawfunc #'cairo-draw-stroke))
  (let ((path (sys-path "out/script-draw.script"))
        (width 400)
        (height 400))
    (cairo:with-cairo-script-surface (surface path :color width height)
      (cairo:with-cairo-context (context surface)
        (funcall drawfunc context width height)
        (cairo:surface-show-page surface)))))

;;; --- 2023-7-21 --------------------------------------------------------------
