;;;; Cairo Path
;;;;
;;;; Cairo always has an active path. If you call the <tt>cairo:stroke</tt> it
;;;; will draw the path with your line settings. If you call the
;;;; <tt>cairo:fill</tt> function it will fill the inside of the path.
;;;;
;;;; <b>Moving</b>
;;;;
;;;; Cairo uses a connect-the-dots style system when creating paths. Start at 1,
;;;; draw a line to 2, then 3, and so forth. When you start a path, or when you
;;;; need to start a new sub-path, you want it to be like point 1: it has
;;;; nothing connecting to it. For this, use the <tt>cairo:move-to</tt>
;;;; function. This sets the current reference point without making the path
;;;; connect the previous point to it. There is also a relative coordinate
;;;; variant, <tt>cairo:rel-move-to</tt>, which sets the new reference a
;;;; specified distance away from the current reference instead. After setting
;;;; your first reference point, use the other path operations which both update
;;;; the reference point and connect to it in some way.
;;;;
;;;; <b>Straight Lines</b>
;;;;
;;;; Whether with absolute coordinates <tt>cairo:line-to</tt> (extend the path
;;;; from the reference to this point), or relative coordinates
;;;; <tt>cairo:rel-line-to</tt> (extend the path from the reference this far in
;;;; this direction), the path connection will be a straight line. The new
;;;; reference point will be at the other end of the line.
;;;;
;;;; <b>Arc Lines</b>
;;;;
;;;; Arcs are parts of the outside of a circle. Unlike straight lines, the point
;;;; you directly specify is not on the path. Instead it is the center of the
;;;; circle that makes up the addition to the path. Both a starting and ending
;;;; point on the circle must be specified, and these points are connected
;;;; either clockwise by the <tt>cairo:arc</tt> function or counter-clockwise by
;;;; the <tt>cairo:arc-negative</tt> function. If the previous reference point
;;;; is not on this new curve, a straight line is added from it to where the arc
;;;; begins. The reference point is then updated to where the arc ends. There
;;;; are only absolute versions.
;;;;
;;;; <b>Curves</b>
;;;;
;;;; Curves in Cairo are cubic Bézier splines. They start at the current
;;;; reference point and smoothly follow the direction of two other points
;;;; (without going through them) to get to a third specified point. Like lines,
;;;; there are both absolute <tt>cairo:curve-to</tt> and relative
;;;; <tt>cairo:rel-curve-to</tt> versions. Note that the relative variant
;;;; specifies all points relative to the previous reference point, rather than
;;;; each relative to the preceding control point of the curve.
;;;;
;;;; <b>Close the path</b>
;;;;
;;;; Cairo can also close the path by drawing a straight line to the beginning
;;;; of the current sub-path. This straight line can be useful for the last edge
;;;; of a polygon, but is not directly useful for curve-based shapes. A closed
;;;; path is fundamentally different from an open path: it's one continuous path
;;;; and has no start or end. A closed path has no line caps for there is no
;;;; place to put them.
;;;;
;;;; 2025-09-20

(in-package :cairo-example)

(defun draw-path (context width height)
  (cairo:save context)
  ;; Clear surface
  (cairo:set-source-rgb context 1.0d0 1.0d0 1.0d0)
  (cairo:paint context)
  ;; Example is in 1.0 x 1.0 coordinate space
  (cairo:scale context width height)
  ;; Drawing code goes here
  (setf (cairo:line-width context) 0.01d0)
  (cairo:set-source-rgb context 1.0d0 0.0d0 0.0d0)
  (cairo:move-to context 0.25 0.25)
  (cairo:line-to context 0.5 0.375)
  (cairo:rel-line-to context 0.25 -0.125)
  (cairo:arc context 0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 pi) (* 0.25 pi))
  (cairo:rel-curve-to context -0.25 -0.125 -0.25 0.125 -0.5 0)
  (cairo:close-path context)
  (cairo:stroke context)
  (cairo:restore context))
