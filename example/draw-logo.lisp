(in-package :cairo-example)

(defun draw-logo1 (context)
  (let ((text "Ägypten"))
    (cairo:save context)
    ;; The drawing is done in a rectangle of 85 x 85 pixels. We scale this
    ;; rectangle from -1 to 1 for the x and y axes. The center of the circle
    ;; has the coordinates (0,0).
    (cairo:scale context (/ 2 85) (/ 2 85))
    ;; Draw the circle with a line width of 10 pixel in red color
    (cairo:set-source-rgb context 1 0 0)
    (setf (cairo:line-width context) 10)
    (cairo:new-sub-path context)
    (cairo:arc context 0 0 25 0 (* 2 pi))
    (cairo:stroke context)
    ;; Draw the rectangle of size 85 x 15 pixel in blue color
    (cairo:set-source-rgb context 0 0 0.6)
    (cairo:rectangle context -42.5 -7.5 85 15)
    (cairo:fill context)
    ;; Draw centered text in the rectangle
    (multiple-value-bind (xbearing ybearing w h xadvance yadvance)
        (cairo:text-extents context text)
      (declare (ignore xadvance yadvance))
      (let (x y)
        ;; Calculate the starting point from the text exents
        (setf x (- (+ (/ w 2) xbearing)))
        (setf y (- (+ (/ h 2) ybearing)))
        ;; Draw the text in white color
        (cairo:set-source-rgb context 1 1 1)
        (cairo:move-to context x y)
        (cairo:show-text context text)))
    (cairo:restore context)))

(defun cairo-draw-logo (context width height)
  (let ((xcenter (/ width 2)) (ycenter (/ height 2))
        (scale (min (/ width 2) (/ height 2))))
    ;; Clear surface
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Translate and scale the logo
    (cairo:translate context xcenter ycenter)
    (cairo:scale context scale scale)
    (draw-logo1 context)))

(defun cairo-draw-logo-translate (context width height)
  (let ((xcenter (/ width 2)) (ycenter (/ height 2)) (shift 2)
        (scale (/ (min (/ width 2) (/ height 2)) 2)))
    ;; Clear surface
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)

    (cairo:translate context (/ xcenter 2)  (/ ycenter 2))
    (cairo:scale context scale scale)
    (dotimes (i 4)
      (draw-logo1 context)
      (cairo:translate context shift 0)
      (cairo:rotate context (/ pi 2)))))

;;; 2025-1-27
