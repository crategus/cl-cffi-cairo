(in-package :cairo-example)

(defun demo-svg-draw  (&optional (drawfunc 'draw-stroke)
                                 (width 400)
                                 (height nil))
  (let ((func (and (symbolp drawfunc)
                   (fboundp drawfunc)
                   (symbol-function drawfunc))))
    (unless func
      (format t "Draw function ~a is unknown~%" drawfunc)
      (return-from demo-svg-draw))
    (let* ((funcname (string-downcase (symbol-name drawfunc)))
           (path (sys-path (concatenate 'string "out/" funcname ".svg")))
           (height (or height width))
           (surface (cairo:svg-surface-create path width height))
           (context (cairo:create surface)))
      (format t "~&Drawing SVG : ~a (~a x ~a size)~%" drawfunc width height)
      (format t "Save SVG to : ~a~%" path)
      ;; Call draw function
      (funcall drawfunc context width height)
      (cairo:surface-show-page surface)
      ;; Free resources
      (cairo:surface-destroy surface)
      (cairo:destroy context))))

;;; 2025-09-21
