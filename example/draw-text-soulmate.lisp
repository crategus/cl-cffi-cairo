(in-package :cairo-example)

(defun draw-text-soulmate (context width height)
  (declare (ignore width height))
  ;; Paint a white background
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Set the color
  (cairo:set-source-rgb context 0.1 0.1 0.1)
  ;; Select the font face
  (cairo:select-font-face context "Purisa" :weight :bold)
  ;; Specify the font size
  (cairo:set-font-size context 13)
  ;; Display text on the drawing area
  (cairo:move-to context 20 30)
  (cairo:show-text context "Most relationships seem so transitory")
  (cairo:move-to context 20 60)
  (cairo:show-text context "They're all good but not the permanent one")

  (cairo:move-to context 20 120)
  (cairo:show-text context "Who doesn't long for someone to hold")

  (cairo:move-to context 20 150)
  (cairo:show-text context "Who knows how to love you without being told")
  (cairo:move-to context 20 180)
  (cairo:show-text context "Somebody tell me why I'm on my own")
  (cairo:move-to context 20 210)
  (cairo:show-text context "If there's a soulmate for everyone"))

;;; --- 2023-1-14 --------------------------------------------------------------
