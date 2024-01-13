(in-package :cairo-example)

(defun demo-png-image-draw (&optional (drawfunc #'cairo-draw-stroke))
  (let ((width 400) (height 300))
    (cairo:with-context-for-image-surface (context :argb32 width height)

      (funcall drawfunc context width height)
      (cairo:show-page context)

      ;; Create and save the PNG image
      (format t "Write image to ~a~%" (sys-path "out/image-draw.png"))
      (cairo:surface-write-to-png (cairo:target context)
                                  (sys-path "out/image-draw.png")))))

;;; --- 2024-1-12 --------------------------------------------------------------
