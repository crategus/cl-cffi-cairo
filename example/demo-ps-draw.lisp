(in-package :cairo-example)

(defun demo-ps-draw (drawfunc &key (width 400) (height 300))
  (let ((path (sys-path "out/ps-draw.ps")))
    (cairo:with-ps-surface (surface path width height)
      (cairo:with-context (context surface)
        (funcall drawfunc context width height)
        (cairo:surface-show-page surface)))))

;;; 2025-09-20
