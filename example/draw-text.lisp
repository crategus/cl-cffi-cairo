(in-package :cairo-example)

(defun draw-text (context width height)
  (let* ((w 340) (h 280)
         (size (max w h))
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale w))))
         (ty (* 0.5 (- height (* scale h)))))

    (cairo:save context)

    (cairo:translate context tx ty)
    (cairo:scale context scale scale)

    (cairo:select-font-face context "cursive" :slant :normal :weight :bold)
    (cairo:set-font-size context 60)
    (cairo:set-source-rgb context (/ 205 255) (/ 102 255) 0.0)

    (cairo:move-to context 25 135)
    (cairo:show-text context "Hello")

    (cairo:move-to context 70 200)
    (cairo:text-path context "Crategus")

    (cairo:set-source-rgb context (/ 255 255) (/ 140 255) 0.0)
    (cairo:fill-preserve context)

    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (setf (cairo:line-width context) 2.56)
    (cairo:stroke context)

    ;; Draw two small arcs
    (cairo:set-source-rgba context 1 0.2 0.2 0.6)
    (cairo:arc context 25 135 5.12 0 (* 2 pi))
    (cairo:close-path context)
    (cairo:arc context 70 200 5.12 0 (* 2 pi))
    (cairo:fill context)

    (cairo:restore context)))
