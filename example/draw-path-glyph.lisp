(in-package :cairo-example)

(defun draw-path-glyph (context width height)

  (let* ((w 480)
         (h 240)
         (size (max w h))
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale w))))
         (ty (* 0.5 (- height (* scale h))))

         (str "Ägypten")
         (x 25) (y 150)
        )
    (cairo:save context)

    (cairo:translate context tx ty)
    (cairo:scale context scale scale)

    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (cairo:select-font-face context "Sans" :slant :normal :weight :normal)
    (cairo:set-font-size context 100)

    (cairo:move-to context x y)
    (cairo:show-text context str)

    (cairo:new-path context)
    (cairo:glyph-path context '((16  50 250) (17 100 250) (18 150 250)
                                (19 200 250) (20 250 250) (21 300 250)
                               ))
    (cairo:stroke context)

    (cairo:restore context)
    ))
