(in-package :cairo-example)

;; Class egg-clock-face is a subclass of a GtkDrawingArea

(defclass egg-clock-face (gtk:drawing-area)
  ((time :initarg :time
         :initform (multiple-value-list (get-decoded-time))
         :accessor egg-clock-face-time))
  (:metaclass gobject-class))

(defmethod initialize-instance :after
    ((clock egg-clock-face) &key &allow-other-keys)
  ;; A timeout source for the time
  (g:timeout-add 1000
                 (lambda ()
                   (setf (egg-clock-face-time clock)
                         (multiple-value-list (get-decoded-time)))
                   (gtk:widget-queue-draw clock)
                   +g-source-continue+))

  ;; Set a drawing function
  (gtk:drawing-area-set-draw-func clock
    (lambda (widget cr width height)
      (declare (ignore widget))
      ;; Clear surface
      (cairo:set-source-rgb cr 1.0 1.0 1.0)
      (cairo:paint cr)
      (let* ((x (/ width 2))
             (y (/ height 2))
             (radius (- (min x y) 12)))
        ;; Clock back
        (cairo:arc cr x y radius 0 (* 2 pi))
        (cairo:set-source-rgb cr 1 1 1)
        (cairo:fill-preserve cr)
        (cairo:set-source-rgb cr 0 0 0)
        (cairo:stroke cr)
        ;; Clock ticks
        (let ((inset 0.0)
              (angle 0.0))
          (dotimes (i 12)
            (cairo:save cr)
            (setf angle (/ (* i pi) 6))
            (if (eql 0 (mod i 3))
                (setf inset (* 0.2 radius))
                (progn
                  (setf inset (* 0.1 radius))
                  (setf (cairo:line-width cr)
                        (* 0.5 (cairo:line-width cr)))))
            (cairo:move-to cr
                           (+ x (* (- radius inset) (cos angle)))
                           (+ y (* (- radius inset) (sin angle))))
            (cairo:line-to cr
                           (+ x (* radius (cos angle)))
                           (+ y (* radius (sin angle))))
            (cairo:stroke cr)
            (cairo:restore cr)))
        (let ((seconds (first (egg-clock-face-time clock)))
              (minutes (second (egg-clock-face-time clock)))
              (hours (third (egg-clock-face-time clock))))
          ;; Hour hand: The hour hand is rotated 30 degrees (pi/6 r) per hour
          ;; + 1/2 a degree (pi/360 r) per minute
          (let ((hours-angle (* (/ pi 6) hours))
                (minutes-angle (* (/ pi 360) minutes)))
            (cairo:save cr)
            (setf (cairo:line-width cr)
                  (* 2.5 (cairo:line-width cr)))
            (cairo:move-to cr x y)
            (cairo:line-to cr
                           (+ x (* (/ radius 2)
                                   (sin (+ hours-angle minutes-angle))))
                           (+ y (* (/ radius 2)
                                   (- (cos (+ hours-angle minutes-angle))))))
            (cairo:stroke cr)
            (cairo:restore cr))
          ;; Minute hand: The minute hand is rotated 6 degrees (pi/30 r)
          ;; per minute
          (let ((angle (* (/ pi 30) minutes)))
            (cairo:move-to cr x y)
            (cairo:line-to cr
                           (+ x (* radius 0.75 (sin angle)))
                           (+ y (* radius 0.75 (- (cos angle)))))
            (cairo:stroke cr))
          ;; Seconds hand: Operates identically to the minute hand
          (let ((angle (* (/ pi 30) seconds)))
            (cairo:save cr)
            (cairo:set-source-rgb cr 1 0 0)
            (cairo:move-to cr x y)
            (cairo:line-to cr (+ x (* radius 0.7 (sin angle)))
                              (+ y (* radius 0.7 (- (cos angle)))))
            (cairo:stroke cr)
            (cairo:restore cr))))
      t)))

(defun demo-cairo-clock (&optional application)
  (let* ((clock (make-instance 'egg-clock-face))
         (window (make-instance 'gtk:window
                                :child clock
                                :application application
                                :title "Demo Cairo Clock"
                                :default-width 250
                                :default-height 250)))
    (gtk:widget-show window)))

;;; 2022-12-20
