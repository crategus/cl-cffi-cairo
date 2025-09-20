;;;; Cairo Fill
;;;;
;;;; The <tt>cairo:fill</tt> operation uses the path like the lines of a
;;;; coloring book, and allows the source through the mask within the hole whose
;;;; boundaries are the path. For complex paths (paths with multiple closed
;;;; sub-paths—like a donut—or paths that self-intersect) this is influenced by
;;;; the fill rule. Note that while stroking the path transfers the source for
;;;; half of the line width on each side of the path, filling a path fills
;;;; directly up to the edge of the path and no further.
;;;;
;;;; Last update: 2025-09-20

(in-package :cairo-example)

(defun draw-fill (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0 1.0 1.0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (cairo:set-source-rgb context 1.0 0.0 0.0)
  (cairo:rectangle context 0.25 0.25 0.5 0.5)
  (cairo:fill context)
  (cairo:restore context))
