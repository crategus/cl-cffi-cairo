(in-package :cairo-example)

(defun demo-png-image-draw (&optional (drawfunc #'draw-stroke))
  (let ((width 400) (height 300))
    (with-cairo-context-for-image-surface (context :argb32 width height)

      (funcall drawfunc context width height)
      (cairo:show-page context)

      ;; Create and save the PNG image
      (format t "Write image to ~a~%" (sys-path "example/out/image-draw.png"))
      (cairo:surface-write-to-png (cairo:target context)
                                  (sys-path "example/out/image-draw.png")))))

;;; --- 2023-1-14 --------------------------------------------------------------
