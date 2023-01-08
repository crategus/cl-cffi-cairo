(in-package :cairo-example)

(defun demo-drawing-lines (&optional application)
  (let* ((path '((10 20) (30 40)))
         (area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Drawing Lines"
                                :default-width 400
                                :default-height 300))
         (gesture (make-instance 'gtk:gesture-click
                                 :button 0)))
    ;; Set a drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget width height))
            (cairo:set-source-rgb cr 0.0 0.0 1.0)
            (setf (cairo:line-width cr) 0.5)
            (loop for (x1 y1) in path
                  do (loop for (x2 y2) in path
                           do (cairo:move-to cr x1 y1)
                              (cairo:line-to cr x2 y2)))
            (cairo:stroke cr)
            (setf path nil)))
    ;; Add the controller to the drawing area
    (gtk:widget-add-controller area gesture)
    ;; Create and run a color chooser dialog to select a background color
    (g:signal-connect gesture "pressed"
        (lambda (gesture n x y)
          (declare (ignore n))
          (format t "button clicked at ~a ~a~%" x y)
          (format t "button clicked is ~a~%" 
                    (gtk:gesture-single-current-button gesture))
          (case (gtk:gesture-single-current-button gesture)
            (1 (push (list x y) path)
               (format t "path is ~a~%" path))
            (3 (format t "button ~a is clicked.~%" 3)
               (gtk:widget-queue-draw (gtk:event-controller-widget gesture))))))

    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
