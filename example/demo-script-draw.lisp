(in-package :cairo-example)

(defun demo-script-draw (drawfunc &key (width 400) (height 300))
  (let ((path (sys-path "out/script-draw.script")))
    (cairo:with-script-surface (surface path :color width height)
      (cairo:with-context (context surface)
        (funcall drawfunc context width height)
        (cairo:surface-show-page surface)))))

;;; 2025-09-20
