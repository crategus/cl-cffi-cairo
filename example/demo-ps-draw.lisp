(in-package :cairo-example)

(defun demo-ps-draw (&optional (drawfunc 'draw-stroke)
                               (width 400)
                               (height nil))
  (let ((func (and (symbolp drawfunc)
                   (fboundp drawfunc)
                   (symbol-function drawfunc))))
    (if func
        (let* ((funcname (string-downcase (symbol-name drawfunc)))
               (path (sys-path (concatenate 'string "out/" funcname ".ps")))
               (height (or height width)))
          (format t "~&Drawing PS : ~a (~a x ~a size)~%" drawfunc width height)
          (cairo:with-ps-surface (surface path width height)
            (cairo:with-context (context surface)
              (format t "Save PS to : ~a~%" path)
              (funcall drawfunc context width height)
              (cairo:surface-show-page surface))))
        (format t "Draw function ~a is unknown~%" drawfunc))))

;;; 2025-09-21
