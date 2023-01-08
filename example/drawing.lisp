(in-package :cairo-example)

(defun demo-drawing (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Drawing"
                                :default-width 400
                                :default-height 300)))
    ;; Set a drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore width height))
          (let* ((context (gtk:widget-style-context widget))
                 (color (gtk:style-context-color context)))
              ;; Draw in the default foreground color
              (cairo:set-source-rgb cr (gdk:rgba-red color)
                                       (gdk:rgba-green color)
                                       (gdk:rgba-blue color))
              ;; Choose a font type and set its size.
              (cairo:select-font-face cr "Sans")
              (cairo:set-font-size cr 20.0)
              ;; Move to a position within the context and draw the text.
              (cairo:move-to cr 10.0 50.0)
              (cairo:show-text cr "Cairo drawing to a widget."))))
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
