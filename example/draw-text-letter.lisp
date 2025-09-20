;;;; Cairo Text Letter
;;;;
;;;; The <tt>cairo:show-text</tt> operation forms the mask from text. It may be
;;;; easier to think of the <tt>cairo:show-text</tt> operation as a shortcut for
;;;; creating a path with the <tt>cairo:text-path</tt> function and then using
;;;; the <tt>cairo:fill</tt> function to transfer it. Be aware the
;;;; <tt>cairo:show-text</tt> function caches glyphs so is much more efficient
;;;; if you work with a lot of text.
;;;;
;;;; Last update: 2025-09-20

(in-package :cairo-example)

(defun draw-text-letter (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 0.0 0.0 0.0)
  (cairo:select-font-face context "Georgia" :weight :bold)
  (cairo:set-font-size context 1.2)
  (multiple-value-bind (xbearing ybearing twidth theight xadvance yadvance)
      (cairo:text-extents context "a")
    (declare (ignore xadvance yadvance))
    (cairo:move-to context
                   (- 0.5 (/ twidth 2) xbearing)
                   (- 0.5 (/ theight 2) ybearing))
     (cairo:show-text context "a"))
  (cairo:restore context))
