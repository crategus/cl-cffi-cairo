;;; ----------------------------------------------------------------------------
;;; cairo.recording-surface.lisp
;;;
;;; The documentation in this file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo library,
;;; see <http://cairographics.org>. The API documentation of the Lisp binding
;;; is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Recording Surfaces
;;;
;;;     Records all drawing operations
;;;
;;; Functions
;;;
;;;     cairo_recording_surface_create
;;;     cairo_recording_surface_ink_extents
;;;     cairo_recording_surface_get_extents
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo:with-recording-surface
;;; ----------------------------------------------------------------------------

(defmacro with-recording-surface ((surface content &rest args) &body body)
 #+liber-documentation
 "@version{2025-1-29}
  @syntax{(cairo:with-recording-surface (surface content) body) => result}
  @syntax{(cairo:with-recording-surface (surface content x y width height)
    body) => result}
  @argument[surface]{a newly allocated @symbol{cairo:surface-t} instance}
  @argument[content]{a @symbol{cairo:content-t} value}
  @argument[x]{a number coerced to a double float for the x coordinate}
  @argument[y]{a number coerced to a double float for the y coordinate}
  @argument[width]{a number coerced to a double float for the width in pixels}
  @argument[height]{a number coerced to a double float for the height in pixels}
  @begin{short}
    The @fun{cairo:with-recording-surface} macro allocates a new
    @symbol{cairo:surface-t} instance and executes the body that uses the Cairo
    surface.
  @end{short}
  After execution of the body the allocated memory for the Cairo surface is
  released. This macro calls the @fun{cairo:recording-surface-create}
  function to create the surface. If the optional @arg{x}, @arg{y},
  @arg{width}, @arg{height} arguments are given a bounded recording surface is
  created, otherwise the recording surface is unbounded. For an unbounded
  recording surface the @fun{cairo:recording-surface-extents} function returns
  a @code{nil} value.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:recording-surface-create}
  @see-function{cairo:recording-surface-extents}"
  (cond ((null args)
         ;; No arguments, create an unbounded surface
         `(let ((,surface (recording-surface-create ,content)))
            (unwind-protect
              (progn ,@body)
              (surface-destroy ,surface))))
        ((null (fifth args))
         ;; Four arguments for x, y, width, height
         (destructuring-bind (x y width height) args
           `(let ((,surface (recording-surface-create ,content
                                                      :x ,x
                                                      :y ,y
                                                      :width ,width
                                                      :height ,height)))
              (progn ,@body)
              (surface-destroy ,surface))))
        (t
         (error "Syntax error in CAIRO:WITH-RECORDING-SURFACE"))))

(export 'with-recording-surface)

;;; ----------------------------------------------------------------------------
;;; cairo:with-context-for-recording-surface
;;; ----------------------------------------------------------------------------

(defmacro with-context-for-recording-surface ((context &rest args) &body body)
 #+liber-documentation
 "@version{2025-1-29}
  @syntax{(cairo:with-context-for-recording-surface (context content) body)
    => result}
  @syntax{(cairo:with-context-for-recording-surface (context content x y
    width height) body) => result}
  @argument[context]{a @symbol{cairo:context-t} instance to create and
    initialize}
  @argument[content]{a @symbol{cairo:content-t} value}
  @argument[x]{a number coerced to a double float for the x coordinate}
  @argument[y]{a number coerced to a double float for the y coordinate}
  @argument[width]{a number coerced to a double float for the width in pixels}
  @argument[height]{a number coerced to a double float for the height in pixels}
  @begin{short}
    The @fun{cairo:with-context-for-recording-surface} macro allocates a new
    @symbol{cairo:context-t} instance, initializes the Cairo context with the
    given values and executes the body that uses the Cairo context.
  @end{short}
  After execution of the body the allocated memory for the Cairo surface is
  released. See the documentation of the @fun{cairo:recording-surface-create}
  and @fun{cairo:create} functions for more information about the initialization
  of the new Cairo context.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:format-t}
  @see-function{cairo:create}
  @see-function{cairo:image-surface-create}"
  (let ((surface (gensym)))
    `(with-recording-surface (,surface ,@args)
       (with-context (,context ,surface)
         (progn ,@body)))))

(export 'with-context-for-recording-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_recording_surface_create
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_recording_surface_create" %recording-surface-create)
    (:pointer (:struct surface-t))
  (content content-t)
  (extents (:pointer (:struct rectangle-t))))

(defun recording-surface-create (content &key (x 0.0d0 x-supplied-p)
                                              (y 0.0d0 y-supplied-p)
                                              (width 0.0d0 width-supplied-p)
                                              (height 0.0d0 height-supplied-p))
 #+liber-documentation
 "@version{2025-1-29}
  @argument[content]{a @symbol{cairo:content-t} value}
  @argument[x]{a number coerced to a double float for the x coordinate in
    pixels}
  @argument[y]{a number coerced to a double float for the y coordinate in
    pixels}
  @argument[width]{a number coerced to a double float for the width in pixels}
  @argument[height]{a number coerced to a double float for the height in pixels}
  @return{The newly created @symbol{cairo:surface-t} instance.}
  @begin{short}
    Creates a recording surface which can be used to record all drawing
    operations at the highest level, that is, the level of paint, mask, stroke,
    fill and text glyphs.
  @end{short}
  If at least one keyword argument is given a bounded recording surface is
  created, otherwise the recording surface is unbounded. The default values
  for the keyword arguments are 0.0d0.

  The recording surface can then be \"replayed\" against any target surface by
  using it as a source to drawing operations. The recording phase of the
  recording surface is careful to snapshot all necessary objects, paths,
  patterns, etc., in order to achieve accurate replay.

  The caller owns the surface and should call the @fun{cairo:surface-destroy}
  function when done with it.
  @see-symbol{cairo:surface-t}
  @see-macro{cairo:with-recording-surface}
  @see-function{cairo:surface-destroy}"
  (if (or x-supplied-p y-supplied-p width-supplied-p height-supplied-p)
      ;; Create a bounded surface
      (let ((x1 (coerce x 'double-float))
            (y1 (coerce y 'double-float))
            (width1 (coerce width 'double-float))
            (height1 (coerce height 'double-float)))
        (cffi:with-foreign-object (rect '(:struct rectangle-t))
          (cffi:with-foreign-slots ((x y width height) rect
                                                       (:struct rectangle-t))
            (setf x x1 y y1 width width1 height height1)
            (%recording-surface-create content rect))))
      ;; No keyword parameters, intialize an unbounded surface
      (%recording-surface-create content (cffi:null-pointer))))

(export 'recording-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_recording_surface_ink_extents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_recording_surface_ink_extents"
               %recording-surface-ink-extents) :void
  (surface (:pointer (:struct surface-t)))
  (x (:pointer :double))
  (y (:pointer :double))
  (width (:pointer :double))
  (height (:pointer :double)))

(defun recording-surface-ink-extents (surface)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{return}
    @arg{x} -- a double float with the x coordinate of the top-left of the ink
    bounding box @br{}
    @arg{y} -- a double float with the y coordinate of the top-left of the ink
    bounding box @br{}
    @arg{width} -- a double float with the width of the the ink bounding box
    @br{}
    @arg{height} -- a double float with the height of the the ink bounding box
  @end{return}
  @begin{short}
    Measures the extents of the operations stored within the recording surface.
  @end{short}
  This is useful to compute the required size of an image surface, or
  equivalent, into which to replay the full sequence of drawing operations.
  @see-symbol{cairo:surface-t}"
  (cffi:with-foreign-objects ((x :double) (y :double)
                              (width :double) (height :double))
    (%recording-surface-ink-extents surface x y width height)
    (values (cffi:mem-ref x :double)
            (cffi:mem-ref y :double)
            (cffi:mem-ref width :double)
            (cffi:mem-ref height :double))))

(export 'recording-surface-ink-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_recording_surface_get_extents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_recording_surface_get_extents" %recording-surface-extents)
    :bool
  (surface (:pointer (:struct surface-t)))
  (extents (:pointer (:struct rectangle-t))))

(defun recording-surface-extents (surface)
 #+liber-documentation
 "@version{2025-1-29}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{return}
    @arg{x} -- a double float with the x coordinate of the top-left of the
    bounding box @br{}
    @arg{y} -- a double float with the y coordinate of the top-left of the
    bounding box @br{}
    @arg{width} -- a double float with the width of the the bounding box @br{}
    @arg{height} -- a double float with the height of the the bounding box
  @end{return}
  @begin{short}
    Get the extents of the recording surface.
  @end{short}
  Returns @em{false} if the recorded surface is unbounded.
  @see-symbol{cairo:surface-t}"
  (cffi:with-foreign-object (rect '(:struct rectangle-t))
    (when (%recording-surface-extents surface rect)
      (cffi:with-foreign-slots ((x y width height) rect (:struct rectangle-t))
        (values x y width height)))))

(export 'recording-surface-extents)

;;; --- End of file cairo.recording-surface.lisp -------------------------------
