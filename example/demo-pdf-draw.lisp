(in-package :cairo-example)

(defun demo-pdf-draw (&optional (drawfunc 'draw-stroke)
                                (width 400)
                                (height nil))
  (let ((height (or height width))
        (func (and (symbolp drawfunc) (fboundp drawfunc))))
    (format t "~&Draw function : ~a (~a x ~a size)~%" drawfunc width height)
    (if func
        (let* ((funcname (string-downcase (symbol-name drawfunc)))
               (path (sys-path (concatenate 'string "out/" funcname ".pdf"))))
          (cairo:with-context-for-pdf-surface (context path width height)
            (format t "Save PDF to   : ~a~%" path)
            (funcall func context width height)
            (cairo:show-page context)))
        (format t "Draw function ~a is unknown~%" drawfunc))))

;;; 2025-09-20
