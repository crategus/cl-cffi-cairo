(in-package :cairo-example)

(defun demo-png-image-draw (drawfunc &key (width 400) (height 300))
  (cairo:with-context-for-image-surface (context :argb32 width height)
    ;; Paint a white background
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Call the draw function
    (funcall drawfunc context width height)
    ;; Create and save the PNG image
    (format t "Write image to ~a~%" (sys-path "out/image-draw.png"))
    (cairo:surface-write-to-png (cairo:target context)
                                (sys-path "out/image-draw.png"))))

;;; 2024-2-8
