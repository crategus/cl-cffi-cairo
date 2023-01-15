(in-package :cairo-example)

(defun draw-logo1 (context)
    (cairo:save context)
    (cairo:set-source-rgb context 1 0 0)
    (setf (cairo:line-width context) 10)
    (cairo:new-sub-path context)
    (cairo:arc context 0 0 25 0 (* 2 pi))
    (cairo:stroke context)
    (cairo:set-source-rgb context 0 0 0.6)
    (cairo:rectangle context -42.5 -7.5 85 15)
    (cairo:fill context)
    (cairo:set-source-rgb context 1 1 1)
    (cairo:move-to context -41 4)
    (cairo:show-text context "UNDERGROUND")
    (cairo:restore context))

(defun draw-logo (context width height)
  (let ((xcenter (/ width 2)) (ycenter (/ height 2)))
    ;; Clear surface
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Translate and scale the logo
    (cairo:translate context xcenter ycenter)
    (cairo:scale context 4.0 4.0)
    (draw-logo1 context)))

(defun draw-logo-translate (context width height)
  (let ((xcenter (/ width 2)) (ycenter (/ height 2)) (shift 150))
    ;; Clear surface
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    (cairo:translate context (- xcenter (/ shift 2)) (- ycenter (/ shift 2)))
    (cairo:scale context 1.0 1.0)
    (dotimes (i 4)
      (draw-logo1 context)
      (cairo:translate context shift 0)
      (cairo:rotate context (/ pi 2)))))

;;; --- 2023-1-14 --------------------------------------------------------------
