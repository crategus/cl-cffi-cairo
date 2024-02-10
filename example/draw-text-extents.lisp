(in-package :cairo-example)

(defun draw-text-extents (context width height)
  (let* ((str "Ägypten")
         (x 25) (y 120)
         (size 250)
         (scale (/ (min width height) size))
         (tx (* 0.5 (- width (* scale size))))
         (ty (* 0.5 (- height (* scale size)))))
    (cairo:save context)

    (cairo:translate context tx ty)
    (cairo:scale context scale scale)

    (cairo:set-source-rgb context 0.0 0.0 0.0)
    (cairo:select-font-face context "Sans" :slant :normal :weight :normal)
    (cairo:set-font-size context 50)

    (cairo:move-to context x y)
    (cairo:show-text context str)

    ;; Draw helping lines
    (cairo:set-source-rgba context 1 0.2 0.2 0.6)
    (setf (cairo:hairline context) t)

    (cairo:arc context x y 3 0 (* 2 pi))
    (cairo:fill context)

    (multiple-value-bind (xbearing ybearing w h xadvance yadvance)
        (cairo:text-extents context str)
      (format t "        x : ~a~%" x)
      (format t "        y : ~a~%" y)
      (format t "        w : ~a~%" w)
      (format t "        h : ~a~%" h)
      (format t " xbearing : ~a~%" xbearing)
      (format t " ybearing : ~a~%" ybearing)
      (format t " xadvance : ~a~%" xadvance)
      (format t " yadvance : ~a~%" yadvance)

      (cairo:move-to context x (+ y h ybearing))
      (cairo:rel-line-to context 0 (- h))
      (cairo:rel-line-to context xadvance 0)
      (cairo:rel-line-to context 0 h)
      (cairo:rel-line-to context (- xadvance) 0)
      (cairo:stroke context))

    (cairo:restore context)))
