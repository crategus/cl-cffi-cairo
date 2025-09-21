(in-package :cairo-example)

(defun demo-png-image-draw (&optional (drawfunc 'draw-stroke)
                                      (width 400)
                                      (height nil))
  (let ((func (and (symbolp drawfunc)
                   (fboundp drawfunc)
                   (symbol-function drawfunc))))
    (if func
        (let* ((funcname (string-downcase (symbol-name drawfunc)))
               (path (sys-path (concatenate 'string "out/" funcname ".png")))
               (height (or height width)))
          (format t "~&Drawing PNG : ~a (~a x ~a size)~%" drawfunc width height)
          (cairo:with-context-for-image-surface (context :argb32 width height)
            ;; Paint white background
            (cairo:set-source-rgb context 1.0 1.0 1.0)
            (cairo:paint context)
            ;; Call draw function
            (funcall func context width height)
            ;; Create and save PNG image
            (format t "Save PNG to : ~a~%" path)
            (cairo:surface-write-to-png (cairo:target context) path)))
        (format t "Draw function ~a is unknown~%" drawfunc))))

;;; 2025-09-21
