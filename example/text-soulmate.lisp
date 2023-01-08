(in-package :cairo-example)

(defun drawing-text-soulmate (widget cr width height)
  (declare (ignore widget width height))
  ;; Set the color.
  (cairo:set-source-rgb cr 0.1 0.1 0.1)
  ;; Select the font face
  (cairo:select-font-face cr "Purisa" :weight :bold)
  ;; Specify the font size
  (cairo:set-font-size cr 13)
  ;; Display text on the drawing area
  (cairo:move-to cr 20 30)
  (cairo:show-text cr "Most relationships seem so transitory")
  (cairo:move-to cr 20 60)
  (cairo:show-text cr "They're all good but not the permanent one")

  (cairo:move-to cr 20 120)
  (cairo:show-text cr "Who doesn't long for someone to hold")

  (cairo:move-to cr 20 150)
  (cairo:show-text cr "Who knows how to love you without being told")
  (cairo:move-to cr 20 180)
  (cairo:show-text cr "Somebody tell me why I'm on my own")
  (cairo:move-to cr 20 210)
  (cairo:show-text cr "If there's a soulmate for everyone"))

(defun demo-text-soulmate (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :child area
                                :application application
                                :title "Cairo Text Soulmate"
                                :default-width 400
                                :default-height 300)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area #'drawing-text-soulmate)
    ;; Show the window.
    (gtk:widget-show window)))

;;; 2022-12-20
