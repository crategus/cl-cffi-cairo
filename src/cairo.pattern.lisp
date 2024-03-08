;;; ----------------------------------------------------------------------------
;;; cairo.pattern.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2024 Dieter Kaiser
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; cairo_pattern_t
;;;
;;;     Sources for drawing
;;;
;;; Types and Values
;;;
;;;     cairo_pattern_t
;;;     cairo_extend_t
;;;     cairo_filter_t
;;;     cairo_pattern_type_t
;;;
;;; Functions
;;;
;;;     cairo_pattern_reference
;;;     cairo_pattern_get_reference_count
;;;     cairo_pattern_destroy
;;;     cairo_pattern_status
;;;     cairo_pattern_get_type
;;;
;;;     cairo_pattern_set_extend
;;;     cairo_pattern_get_extend
;;;     cairo_pattern_set_filter
;;;     cairo_pattern_get_filter
;;;     cairo_pattern_set_matrix
;;;     cairo_pattern_get_matrix
;;;
;;;     cairo_pattern_add_color_stop_rgb
;;;     cairo_pattern_add_color_stop_rgba
;;;     cairo_pattern_get_color_stop_count
;;;     cairo_pattern_get_color_stop_rgba
;;;
;;;     cairo_pattern_create_rgb
;;;     cairo_pattern_create_rgba
;;;     cairo_pattern_get_rgba
;;;     cairo_pattern_create_for_surface
;;;     cairo_pattern_get_surface
;;;     cairo_pattern_create_linear
;;;     cairo_pattern_get_linear_points
;;;     cairo_pattern_create_radial
;;;     cairo_pattern_get_radial_circles
;;;
;;;     cairo_pattern_create_mesh
;;;     cairo_mesh_pattern_begin_patch
;;;     cairo_mesh_pattern_end_patch
;;;     cairo_mesh_pattern_move_to
;;;     cairo_mesh_pattern_line_to
;;;     cairo_mesh_pattern_curve_to
;;;     cairo_mesh_pattern_set_control_point
;;;     cairo_mesh_pattern_set_corner_color_rgb
;;;     cairo_mesh_pattern_set_corner_color_rgba
;;;     cairo_mesh_pattern_get_patch_count
;;;     cairo_mesh_pattern_get_path
;;;     cairo_mesh_pattern_get_control_point
;;;     cairo_mesh_pattern_get_corner_color_rgba
;;;
;;;     cairo_pattern_set_user_data                        not implemented
;;;     cairo_pattern_get_user_data                        not implemented
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; enum cairo_extend_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum extend-t
  :none
  :repeat
  :reflect
  :pad)

#+liber-documentation
(setf (liber:alias-for-symbol 'extend-t)
      "CEnum"
      (liber:symbol-documentation 'extend-t)
 "@version{2024-2-15}
  @begin{short}
    The @symbol{cairo:extend-t} enumeration is used to describe how pattern
    color/alpha will be determined for areas \"outside\" the natural area, of
    the pattern for example, outside the surface bounds or outside the gradient
    geometry.
  @end{short}

  Mesh patterns are not affected by the extend mode.

  The default extend mode is @code{:none} for surface patterns and @code{:pad}
  for gradient patterns.
  @begin{pre}
(cffi:defcenum extend-t
  :none
  :repeat
  :reflect
  :pad)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Pixels outside of the source pattern are fully transparent.}
    @entry[:repeat]{The pattern is tiled by repeating.}
    @entry[:reflect]{The pattern is tiled by reflecting at the edges.}
    @entry[:pad]{Pixels outside of the pattern copy the closest pixel from the
      source.}
  @end{table}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-extend}")

(export 'extend-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_filter_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum filter-t
  :fast
  :good
  :best
  :nearest
  :bilinear
  :gaussian)

#+liber-documentation
(setf (liber:alias-for-symbol 'filter-t)
      "CEnum"
      (liber:symbol-documentation 'filter-t)
 "@version{2024-1-24}
  @begin{short}
    The @symbol{cairo:filter-t} enumeration is used to indicate what filtering
    should be applied when reading pixel values from patterns.
  @end{short}
  See the @fun{cairo:pattern-filter} function for indicating the desired filter
  to be used with a particular pattern.
  @begin{pre}
(cffi:defcenum filter-t
  :fast
  :good
  :best
  :nearest
  :bilinear
  :gaussian)
  @end{pre}
  @begin[code]{table}
    @entry[:fast]{A high-performance filter, with quality similar to
      @code{:nearest}.}
    @entry[:good]{A reasonable-performance filter, with quality similar to
      @code{:bilinear}.}
    @entry[:best]{The highest-quality available, performance may not be
      suitable for interactive use.}
    @entry[:nearest]{Nearest-neighbor filtering.}
    @entry[:bilinear]{Linear interpolation in two dimensions.}
    @entry[:gaussian]{This filter value is currently unimplemented, and should
      not be used in current code.}
  @end{table}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-filter}")

(export 'filter-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_pattern_type_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum pattern-type-t
  :solid
  :surface
  :linear
  :radial
  :mesh
  :raster-source)

#+liber-documentation
(setf (liber:alias-for-symbol 'pattern-type-t)
      "CEnum"
      (liber:symbol-documentation 'pattern-type-t)
 "@version{2024-2-15}
  @begin{short}
    The @symbol{cairo:pattern-type-t} enumeration is used to describe the type
    of a given pattern.
  @end{short}

  The type of a pattern is determined by the function used to create it. The
  @fun{cairo:pattern-create-rgb} and @fun{cairo:pattern-create-rgba} functions
  create @code{:solid} patterns. The remaining @fun{cairo:pattern-create}
  functions map to pattern types in obvious ways.

  The pattern type can be queried with the @fun{cairo:pattern-type} function.

  Most Cairo pattern functions can be called with a pattern of any type, though
  trying to change the extend or filter for a solid pattern will have no effect.
  A notable exception are the @fun{cairo:pattern-add-color-stop-rgb} and
  @fun{cairo:pattern-add-color-stop-rgba} functions which must only be called
  with gradient patterns, either @code{:linear} or @code{:radial}. Otherwise the
  pattern will be shutdown and put into an error state.
  @begin{pre}
(cffi:defcenum pattern-type-t
  :solid
  :surface
  :linear
  :radial
  :mesh
  :raster-source)
  @end{pre}
  @begin[code]{table}
    @entry[:solid]{The pattern is a solid (uniform) color. It may be opaque or
      translucent.}
    @entry[:surface]{The pattern is a based on a surface (an image).}
    @entry[:linear]{The pattern is a linear gradient.}
    @entry[:radial]{The pattern is a radial gradient.}
    @entry[:mesh]{The pattern is a mesh.}
    @entry[:raster-source]{The pattern is a user pattern providing raster data.}
  @end{table}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-create-rgb}
  @see-function{cairo:pattern-create-rgba}
  @see-function{cairo:pattern-type}
  @see-function{cairo:pattern-add-color-stop-rgb}
  @see-function{cairo:pattern-add-color-stop-rgba}")

(export 'pattern-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct pattern-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'pattern-t)
      "CStruct"
      (liber:symbol-documentation 'pattern-t)
 "@version{2024-2-15}
  @begin{short}
    A @symbol{cairo:pattern-t} structure represents a source when drawing onto
    a surface.
  @end{short}
  There are different subtypes of @symbol{cairo:pattern-t} structures, for
  different types of sources; for example, the @fun{cairo:pattern-create-rgb}
  function creates a pattern for a solid opaque color.

  Other than various @code{cairo:pattern-create-type} functions, some of the
  pattern types can be implicitly created using various
  @code{cario:set-source-type} functions, for example the
  @fun{cairo:set-source-rgb} function.

  The type of a pattern can be queried with the @fun{cairo:pattern-type}
  function.

  Memory management of the @symbol{cairo:pattern-t} structure is done with the
  @fun{cairo:pattern-reference} and @fun{cairo:pattern-destroy} functions.
  @see-function{cairo:pattern-create-rgb}
  @see-function{cairo:set-source-rgb}
  @see-function{cairo:pattern-reference}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo:pattern-type}")

(export 'pattern-t)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_reference ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_reference" pattern-reference)
    (:pointer (:struct pattern-t))
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @return{The referenced @symbol{cairo:pattern-t} instance.}
  @begin{short}
    Increases the reference count on @arg{pattern} by one.
  @end{short}
  This prevents @arg{pattern} from being destroyed until a matching call to
  the @fun{cairo:pattern-destroy} function is made.

  The number of references to a @symbol{cairo:pattern-t} can be get using the
  @fun{cairo:pattern-reference-count} function.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo:pattern-reference-count}"
  (pattern (:pointer (:struct pattern-t))))

(export 'pattern-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_reference_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_get_reference_count" pattern-reference-count)
    :uint
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @return{An unsigned integer with the current reference count of
    @arg{pattern}.}
  @begin{short}
    Returns the current reference count of @arg{pattern}.
  @end{short}
  If the instance is a \"nil\" pattern, 0 will be returned.
  @see-symbol{cairo:pattern-t}"
  (pattern (:pointer (:struct pattern-t))))

(export 'pattern-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_destroy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_destroy" pattern-destroy) :void
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @begin{short}
    Decreases the reference count on @arg{pattern} by one.
  @end{short}
  If the result is zero, then @arg{pattern} and all associated resources are
  freed. See the @fun{cairo:pattern-reference} function.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-reference}"
  (pattern (:pointer (:struct pattern-t))))

(export 'pattern-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_status ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_status" pattern-status) status-t
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @return{The @symbol{cairo:status-t} value.}
  @begin{short}
    Checks whether an error has previously occurred for this pattern.
  @end{short}
  Possible values are @code{:success}, @code{:no-memory},
  @code{:invalid-matrix}, @code{:pattern-type-mismatch}, or
  @code{:invalid-mesh-construction}.
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:status-t}"
  (pattern (:pointer (:struct pattern-t))))

(export 'pattern-status)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_get_type" pattern-type) pattern-type-t
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @return{The @symbol{cairo:pattern-type-t} value.}
  @begin{short}
    This function returns the type of a pattern.
  @end{short}
  See the @symbol{cairo:pattern-type-t} enumeration for available types.
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:pattern-type-t}"
  (pattern (:pointer (:struct pattern-t))))

(export 'pattern-type)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_extend ()
;;; cairo_pattern_set_extend ()
;;; ----------------------------------------------------------------------------

(defun (setf pattern-extend) (extend pattern)
  (cffi:foreign-funcall "cairo_pattern_set_extend"
                        (:pointer (:struct pattern-t)) pattern
                        extend-t extend
                        :void)
  extend)

(cffi:defcfun ("cairo_pattern_get_extend" pattern-extend) extend-t
 #+liber-documentation
 "@version{2024-1-24}
  @syntax{(cairo:pattern-extend pattern) => extend}
  @syntax{(setf (cairo:pattern-extend pattern) extend)}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[extend]{a @symbol{cairo:extend-t} value describing how the area
    outside of the pattern will be drawn}
  @begin{short}
    The @fun{cairo:pattern-extend} function gets the current extend mode for a
    pattern.
  @end{short}
  The @setf{cairo:pattern-extend} function sets the mode to be used for drawing
  outside the area of a pattern. See the @symbol{cairo:extend-t} enumeration for
  details on the semantics of each extend strategy.

  The default extend mode is @code{:none} for surface patterns and @code{:pad}
  for gradient patterns.
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:extend-t}"
  (pattern (:pointer (:struct pattern-t))))

(export 'pattern-extend)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_filter ()
;;; cairo_pattern_set_filter ()
;;; ----------------------------------------------------------------------------

(defun (setf pattern-filter) (filter pattern)
  (cffi:foreign-funcall "cairo_pattern_set_filter"
                        (:pointer (:struct pattern-t)) pattern
                        filter-t filter
                        :void)
  filter)

(cffi:defcfun ("cairo_pattern_get_filter" pattern-filter) filter-t
 #+liber-documentation
 "@version{2024-2-15}
  @syntax{(cairo:pattern-filter pattern) => filter}
  @syntax{(setf (cairo:filter-extend pattern) filter)}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[filter]{a @symbol{cairo:filter-t} value describing the filter to
    use for resizing the pattern}
  @begin{short}
    The @fun{cairo:pattern-filter} function gets the current filter for
    @arg{pattern}.
  @end{short}
  The @setf{cairo:pattern-filter} function sets the filter to be used for
  resizing when using the pattern. See the @symbol{cairo:filter-t} enumeration
  for details on each filter.

  Note that you might want to control filtering even when you do not have an
  explicit @symbol{cairo:pattern-t} instance, for example when using the
  @fun{cairo:set-source-surface} function. In these cases, it is convenient to
  use the @fun{cairo:source} function to get access to the pattern that Cairo
  creates implicitly. For example:
  @begin{pre}
(cairo:set-source-surface context image x y)
(setf (cairo:pattern-filter (cairo:source cr)) :nearest)
  @end{pre}
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:filter-t}"
  (pattern (:pointer (:struct pattern-t))))

(export 'pattern-filter)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_matrix ()
;;; cairo_pattern_set_matrix ()
;;; ----------------------------------------------------------------------------

(defun (setf pattern-matrix) (matrix pattern)
  (cffi:foreign-funcall "cairo_pattern_set_matrix"
                        (:pointer (:struct pattern-t)) pattern
                        (:pointer (:struct matrix-t)) matrix
                        :void)
  matrix)

(defun pattern-matrix (pattern matrix)
 #+liber-documentation
 "@version{2024-2-15}
  @syntax{(cairo:pattern-matrix pattern matrix) => matrix}
  @syntax{(setf (cairo:filter-matrix pattern) matrix)}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @begin{short}
    The @fun{cairo:pattern-matrix} function gets the pattern's transformation
    matrix.
  @end{short}
  The @setf{cairo:pattern-matrix} function sets the transformation matrix of
  the pattern to @arg{matrix}. This matrix is a transformation from user space
  to pattern space.

  When a pattern is first created it always has the identity matrix for its
  transformation matrix, which means that pattern space is initially identical
  to user space.

  Important: Please note that the direction of this transformation matrix is
  from user space to pattern space. This means that if you imagine the flow
  from a pattern to user space, and on to device space, then coordinates in
  that flow will be transformed by the inverse of the pattern matrix.

  For example, if you want to make a pattern appear twice as large as it does
  by default the correct code to use is:
  @begin{pre}
(cairo:matrix-init-scale matrix 0.5 0.5)
(setf (cairo:pattern-matrix pattern) matrix)
  @end{pre}
  Meanwhile, using values of 2.0 rather than 0.5 in the code above would cause
  the pattern to appear at half of its default size.

  Also, please note the discussion of the user-space locking semantics of the
  @fun{cairo:source} function.
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:source}"
  (cffi:foreign-funcall "cairo_pattern_get_matrix"
                        (:pointer (:struct pattern-t)) pattern
                        (:pointer (:struct matrix-t)) matrix
                        :void)
  matrix)

(export 'pattern-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_add_color_stop_rgb ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_add_color_stop_rgb" %pattern-add-color-stop-rgb)
    :void
  (pattern (:pointer (:struct pattern-t)))
  (offset :double)
  (red :double)
  (green :double)
  (blue :double))

(defun pattern-add-color-stop-rgb (pattern offset red green blue)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[offset]{a number with an offset in the range [0.0 .. 1.0]}
  @argument[red]{a number with the red component of color}
  @argument[green]{a number with the green component of color}
  @argument[blue]{a number with the blue component of color}
  @begin{short}
    Adds an opaque color stop to a gradient pattern.
  @end{short}
  The offset specifies the location along the gradient's control vector. For
  example, a linear control vector of the pattern is from @code{(x0,y0)} to
  @code{(x1,y1)} while a radial control vector of the pattern is from any point
  on the start circle to the corresponding point on the end circle.

  The color is specified in the same way as in the @fun{cairo:set-source-rgb}
  function.

  If two (or more) stops are specified with identical offset values, they will
  be sorted according to the order in which the stops are added, stops added
  earlier will compare less than stops added later. This can be useful for
  reliably making sharp color transitions instead of the typical blend.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a gradient pattern, e.g. a linear or
    radial pattern, then @arg{pattern} will be put into an error status with a
    status of @code{:pattern-type-mismatch}.
  @end{dictionary}
  @begin[Lisp implementation]{dictionary}
    The arguments are coerced to the @code{double-float} type before being
    passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:set-source-rgb}"
  (%pattern-add-color-stop-rgb pattern
                               (coerce offset 'double-float)
                               (coerce red 'double-float)
                               (coerce green 'double-float)
                               (coerce blue 'double-float)))

(export 'pattern-add-color-stop-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_add_color_stop_rgba ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_add_color_stop_rgba" %pattern-add-color-stop-rgba)
    :void
  (pattern (:pointer (:struct pattern-t)))
  (offset :double)
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defun pattern-add-color-stop-rgba (pattern offset red green blue alpha)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[offset]{a number with an offset in the range [0.0 .. 1.0]}
  @argument[red]{a number with the red component of color}
  @argument[green]{a number with the green component of color}
  @argument[blue]{a number with the blue component of color}
  @argument[alpha]{a number with the alpha component of color}
  @begin{short}
    Adds a translucent color stop to a gradient pattern.
  @end{short}
  The offset specifies the location along the control vector of the gradient.
  For example, a linear control vector of the gradient is from @code{(x0,y0)} to
  @code{(x1,y1)} while a radial control vector of the gradient is from any point
  on the start circle to the corresponding point on the end circle. The color is
  specified in the same way as in the @fun{cairo:set-source-rgba} function.

  If two or more stops are specified with identical offset values, they will
  be sorted according to the order in which the stops are added, stops added
  earlier will compare less than stops added later. This can be useful for
  reliably making sharp color transitions instead of the typical blend.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a gradient pattern, e.g. a linear or
    radial pattern, then @arg{pattern} will be put into an error status with a
    @code{:pattern-type-mismatch} status.
  @end{dictionary}
  @begin[Lisp implementation]{dictionary}
    The arguments are coerced to the @code{double-float} type before being
    passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:set-source-rgba}"
  (%pattern-add-color-stop-rgba pattern
                                (coerce offset 'double-float)
                                (coerce red 'double-float)
                                (coerce green 'double-float)
                                (coerce blue 'double-float)
                                (coerce alpha 'double-float)))

(export 'pattern-add-color-stop-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_color_stop_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_get_color_stop_count" %pattern-color-stop-count)
    status-t
  (pattern (:pointer (:struct pattern-t)))
  (count (:pointer :int)))

(defun pattern-color-stop-count (cr)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @return{The integer with the number of color stops.}
  @begin{short}
    Gets the number of color stops specified in the given gradient pattern.
  @end{short}
  Returns @code{nil} if @arg{pattern} is not a gradient pattern.
  @see-symbol{cairo:pattern-t}"
  (cffi:with-foreign-object (count :int)
    (when (eq :success (%pattern-color-stop-count cr count))
      (values (cffi:mem-ref count :int)))))

(export 'pattern-color-stop-count)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_color_stop_rgba ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_get_color_stop_rgba" %pattern-color-stop-rgba)
    status-t
  (pattern (:pointer (:struct pattern-t)))
  (index :int)
  (offset (:pointer :double))
  (red (:pointer :double))
  (green (:pointer :double))
  (blue (:pointer :double))
  (alpha (:pointer :double)))

(defun pattern-color-stop-rgba (pattern index)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[index]{an integer with the index of the stop to return data for}
  @begin{return}
    @arg{offset} -- a double float with the offset of the stop @br{}
    @arg{red} -- a double float red component of color @br{}
    @arg{green} -- a double float green component of color @br{}
    @arg{blue} -- a double float blue component of color @br{}
    @arg{alpha} -- a double float alpha component of color
  @end{return}
  @begin{short}
    Gets the color and offset information at the given index for a gradient
    pattern.
  @end{short}
  Returns @code{nil} if @arg{pattern} is not a gradient pattern of if
  @arg{index} is not valid for the given pattern.

  Values of index are 0 to 1 less than the number returned by the
  @fun{cairo:pattern-color-stop-count} function.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-color-stop-count}"
  (cffi:with-foreign-objects ((offset :double)
                              (red :double)
                              (green :double)
                              (blue :double)
                              (alpha :double))
    (when (eq :success
              (%pattern-color-stop-rgba pattern
                                        index
                                        offset
                                        red green blue alpha))
      (values (cffi:mem-ref offset :double)
              (cffi:mem-ref red :double)
              (cffi:mem-ref green :double)
              (cffi:mem-ref blue :double)
              (cffi:mem-ref alpha :double)))))

(export 'pattern-color-stop-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_rgb ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_create_rgb" %pattern-create-rgb)
    (:pointer (:struct pattern-t))
  (red :double)
  (green :double)
  (blue :double))

(defun pattern-create-rgb (red green blue)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[red]{a double float red component of the color}
  @argument[green]{a double float green component of the color}
  @argument[blue]{a double float blue component of the color}
  @begin{return}
    The newly created @symbol{cairo:pattern-t} instance if successful, or an
    error pattern in case of no memory.
  @end{return}
  @begin{short}
    Creates a new @symbol{cairo:pattern-t} instance corresponding to an opaque
    color.
  @end{short}
  The caller owns the returned object and should call the
  @fun{cairo:pattern-destroy} function when finished with it. This function will
  always return a valid pattern, but if an error occurred the pattern status
  will be set to an error. To inspect the status of a pattern use the
  @fun{cairo:pattern-status} function.

  The color components are floating point numbers in the range 0 to 1. If the
  values passed in are outside that range, they will be clamped.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo:pattern-status}"
  (%pattern-create-rgb (coerce red 'double-float)
                       (coerce green 'double-float)
                       (coerce blue 'double-float)))

(export 'pattern-create-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_rgba ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_create_rgba" %pattern-create-rgba)
    (:pointer (:struct pattern-t))
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defun pattern-create-rgba (red green blue alpha)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[red]{a double float red component of the color}
  @argument[green]{a double float green component of the color}
  @argument[blue]{a double float blue component of the color}
  @argument[alpha]{a double float alpha component of the color}
  @begin{return}
    The newly created @symbol{cairo:pattern-t} instance if successful, or an
    error pattern in case of no memory.
  @end{return}
  @begin{short}
    Creates a new @symbol{cairo:pattern-t} instance corresponding to a
    translucent color.
  @end{short}
  The caller owns the returned object and should call the
  @fun{cairo:pattern-destroy} function when finished with it. This function will
  always return a valid pattern, but if an error occurred the pattern status
  will be set to an error. To inspect the status of a pattern use the
  @fun{cairo:pattern-status} function.

  The color components are floating point numbers in the range 0 to 1. If the
  values passed in are outside that range, they will be clamped.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo:pattern-status}"
  (%pattern-create-rgba (coerce red 'double-float)
                        (coerce green 'double-float)
                        (coerce blue 'double-float)
                        (coerce alpha 'double-float)))

(export 'pattern-create-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_rgba ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_get_rgba" %pattern-rgba) status-t
  (pattern (:pointer (:struct pattern-t)))
  (red (:pointer :double))
  (green (:pointer :double))
  (blue (:pointer :double))
  (alpha (:pointer :double)))

(defun pattern-rgba (pattern)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @begin{return}
    @arg{red} -- a double float red component of color @br{}
    @arg{green} -- a double float green component of color @br{}
    @arg{blue} -- a double float blue component of color @br{}
    @arg{alpha} -- a double float alpha component of color
  @end{return}
  @begin{short}
    Gets the color for a solid color pattern.
  @end{short}
  Returns @code{nil} if the pattern ist not a solid color pattern.
  @see-symbol{cairo:pattern-t}"
  (cffi:with-foreign-objects ((red :double)
                              (green :double)
                              (blue :double)
                              (alpha :double))
      (when (eq :success (%pattern-rgba pattern red green blue alpha))
        (values (cffi:mem-ref red :double)
                (cffi:mem-ref green :double)
                (cffi:mem-ref blue :double)
                (cffi:mem-ref alpha :double)))))

(export 'pattern-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_for_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_create_for_surface" pattern-create-for-surface)
    (:pointer (:struct pattern-t))
 #+liber-documentation
 "@version{2024-2-15}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{return}
    The newly created @symbol{cairo:pattern-t} instance if successful, or an
    error pattern in case of no memory. The caller owns the returned object and
    should call the @fun{cairo:pattern-destroy} function when finished with it.
  @end{return}
  @begin{short}
    Create a new @symbol{cairo:pattern-t} instance for the given surface.
  @end{short}
  This function will always return a valid pattern, but if an error occurred the
  pattern status will be set to an error. To inspect the status of a pattern use
  the @fun{cairo:pattern-status} function.
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo:pattern-status}"
  (surface (:pointer (:struct surface-t))))

(export 'pattern-create-for-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_get_surface" %pattern-surface) status-t
  (pattern (:pointer (:struct pattern-t)))
  (surface (:pointer (:pointer (:struct surface-t)))))

(defun pattern-surface (pattern)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @return{The @symbol{cairo:surface-t} instance of the pattern.}
  @begin{short}
    Gets the surface of a surface pattern.
  @end{short}
  Returns @code{nil} if the pattern is not a surface pattern.

  The reference returned in @arg{surface} is owned by the pattern. The caller
  should call the @fun{cairo:surface-reference} function if the surface is to
  be retained.
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-reference}"
  (cffi:with-foreign-object (surface '(:pointer (:struct surface-t)))
    (when (eq :success (%pattern-surface pattern surface))
      (cffi:mem-ref surface :pointer))))

(export 'pattern-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_linear ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_create_linear" %pattern-create-linear)
    (:pointer (:struct pattern-t))
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defun pattern-create-linear (x0 y0 x1 y1)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[x0]{a number with the x coordinate of the start point}
  @argument[y0]{a number with the y coordinate of the start point}
  @argument[x1]{a number with the x coordinate of the end point}
  @argument[y1]{a number with the y coordinate of the end point}
  @begin{return}
    The newly created @symbol{cairo:pattern-t} instance if successful, or an
    error pattern in case of no memory. The caller owns the returned instance
    and should call the @fun{cairo:pattern-destroy} function when finished with
    it.
  @end{return}
  @begin{short}
    Create a new linear gradient @symbol{cairo:pattern-t} instance along the
    line defined by (@arg{x0}, @arg{y0}) and (@arg{x1}, @arg{y1}).
  @end{short}
  This function will always return a valid patttern, but if an error occurred
  the pattern status will be set to an error. To inspect the status of a pattern
  use the @fun{cairo:pattern-status} function.

  Before using the gradient pattern, a number of color stops should be defined
  using the @fun{cairo:pattern-add-color-stop-rgb} or
  @fun{cairo:pattern-add-color-stop-rgba} functions.
  @begin[Note]{dictionary}
    The coordinates here are in pattern space. For a new pattern, pattern
    space is identical to user space, but the relationship between the spaces
    can be changed with the @fun{cairo:pattern-matrix} function.
  @end{dictionary}
  @begin[Lisp implementation]{dictionary}
    The arguments are coerced to the @code{double-float} type before being
    passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo:pattern-status}
  @see-function{cairo:pattern-add-color-stop-rgb}
  @see-function{cairo:pattern-add-color-stop-rgba}
  @see-function{cairo:pattern-matrix}"
  (%pattern-create-linear (coerce x0 'double-float)
                          (coerce y0 'double-float)
                          (coerce x1 'double-float)
                          (coerce y1 'double-float)))

(export 'pattern-create-linear)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_linear_points ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_get_linear_points" %pattern-linear-points)
    status-t
  (pattern (:pointer (:struct pattern-t)))
  (x0 (:pointer :double))
  (y0 (:pointer :double))
  (x1 (:pointer :double))
  (y1 (:pointer :double)))

(defun pattern-linear-points (pattern)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @begin{return}
    @arg{x0} -- a double float x coordinate of the first point @br{}
    @arg{y0} -- a double float y coordinate of the first point @br{}
    @arg{y0} -- a double float x coordinate of the second point @br{}
    @arg{y1} -- a double float y coordinate of the second point
  @end{return}
  @begin{short}
    Gets the gradient endpoints for a linear gradient.
  @end{short}
  Returns @code{nil} if the pattern is not a linear gradient pattern.
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:status-t}"
  (cffi:with-foreign-objects ((x0 :double)
                              (y0 :double)
                              (x1 :double)
                              (y1 :double))

    (when (eq :success (%pattern-linear-points pattern x0 y0 x1 y1))
      (values (cffi:mem-ref x0 :double)
              (cffi:mem-ref y0 :double)
              (cffi:mem-ref x1 :double)
              (cffi:mem-ref y1 :double)))))

(export 'pattern-linear-points)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_radial ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_create_radial" %pattern-create-radial)
    (:pointer (:struct pattern-t))
  (x0 :double)
  (y0 :double)
  (radius0 :double)
  (x1 :double)
  (y1 :double)
  (radius1 :double))

(defun pattern-create-radial (x0 y0 r0 x1 y1 r1)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[x0]{a number with the x coordinate for the center of the start
    circle}
  @argument[y0]{a number with the y coordinate for the center of the start
    circle}
  @argument[r0]{a number with the radius of the start circle}
  @argument[x1]{a number with the x coordinate for the center of the end circle}
  @argument[y1]{a number with the y coordinate for the center of the end circle}
  @argument[r1]{a number with the radius of the end circle}
  @begin{return}
    The newly created @symbol{cairo:pattern-t} instance if successful, or an
    error pattern in case of no memory. The caller owns the returned object and
    should call the @fun{cairo:pattern-destroy} function when finished with it.
  @end{return}
  @begin{short}
    Creates a new radial gradient @symbol{cairo:pattern-t} instance between the
    two circles defined by (@arg{x0}, @arg{y0}, @arg{r0}) and (@arg{x1},
    @arg{y1}, @arg{r1}).
  @end{short}
  This function will always return a valid pattern, but if an error occurred the
  pattern status will be set to an error. To inspect the status of a pattern use
  the @fun{cairo:pattern-status} function.

  Before using the gradient pattern, a number of color stops should be defined
  using the @fun{cairo:pattern-add-color-stop-rgb} or
  @fun{cairo:pattern-add-color-stop-rgba} functions.
  @begin[Note]{dictionary}
    The coordinates here are in pattern space. For a new pattern, pattern
    space is identical to user space, but the relationship between the spaces
    can be changed with the @fun{cairo:pattern-matrix} function.
  @end{dictionary}
  @begin[Lisp implementation]{dictionary}
    The arguments are coerced to the @code{double-float} type before being
    passed to the C function.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo:pattern-status}
  @see-function{cairo:pattern-add-color-stop-rgb}
  @see-function{cairo:pattern-add-color-stop-rgba}
  @see-function{cairo:pattern-matrix}"
  (%pattern-create-radial (coerce x0 'double-float)
                          (coerce y0 'double-float)
                          (coerce r0 'double-float)
                          (coerce x1 'double-float)
                          (coerce y1 'double-float)
                          (coerce r1 'double-float)))

(export 'pattern-create-radial)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_radial_circles ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_get_radial_circles" %pattern-radial-circles)
    status-t
  (pattern (:pointer (:struct pattern-t)))
  (x0 (:pointer :double))
  (y0 (:pointer :double))
  (r0 (:pointer :double))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (r1 (:pointer :double)))

(defun pattern-radial-circles (pattern)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @begin{return}
    @arg{x0} -- a double float x coordinate of the center of the first circle
    @br{}
    @arg{y0} -- a double float y coordinate of the center of the first circle
    @br{}
    @arg{r0} -- a double float radius of the first circle  @br{}
    @arg{x1} -- a double float x coordinate of the center of the second circle
    @br{}
    @arg{y1} -- a double float y coordinate of the center of the second circle
    @br{}
    @arg{r1} -- a double float radius of the second circle
  @end{return}
  @begin{short}
    Gets the gradient endpoint circles for a radial gradient, each specified as
    a center coordinate and a radius.
  @end{short}
  Returns @code{nil} it the pattern is not a radial gradient pattern.
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:status-t}"
  (cffi:with-foreign-objects ((x0 :double) (y0 :double) (r0 :double)
                              (x1 :double) (y1 :double) (r1 :double))
    (when (eq :success (%pattern-radial-circles pattern x0 y0 r0 x1 y1 r1))
      (values (cffi:mem-ref x0 :double)
              (cffi:mem-ref y0 :double)
              (cffi:mem-ref r0 :double)
              (cffi:mem-ref x1 :double)
              (cffi:mem-ref y1 :double)
              (cffi:mem-ref r1 :double)))))

(export 'pattern-radial-circles)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_create_mesh ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pattern_create_mesh" pattern-create-mesh)
    (:pointer (:struct pattern-t))
 #+liber-documentation
 "@version{2024-2-15}
  @begin{return}
   The newly created @symbol{cairo:pattern-t} instance if successful, or an
   error pattern in case of no memory. The caller owns the returned object and
   should call the @fun{cairo:pattern-destroy} function when finished with it.
  @end{return}
  @begin{short}
    Create a new mesh pattern.
  @end{short}
  This function will always return a valid pattern, but if an error occurred the
  pattern status will be set to an error. To inspect the status of a pattern use
  the @fun{cairo:pattern-status} function.

  Mesh patterns are tensor-product patch meshes (type 7 shadings in PDF). Mesh
  patterns may also be used to create other types of shadings that are special
  cases of tensor-product patch meshes such as Coons patch meshes (type 6
  shading in PDF) and Gouraud-shaded triangle meshes (type 4 and 5 shadings in
  PDF).

  Mesh patterns consist of one or more tensor-product patches, which should be
  defined before using the mesh pattern. Using a mesh pattern with a partially
  defined patch as source or mask will put the context in an error status with
  a @code{:invalid-mesh-construction} value.

  A tensor-product patch is defined by 4 Bézier curves (side 0, 1, 2, 3) and
  by 4 additional control points @code{(P0, P1, P2, P3)} that provide further
  control over the patch and complete the definition of the tensor-product
  patch. The corner @code{C0} is the first point of the patch.

  Degenerate sides are permitted so straight lines may be used. A zero length
  line on one side may be used to create 3 sided patches.
  @begin{pre}
      C1     Side 1       C2
       +---------------+
       |               |
       |  P1       P2  |
       |               |
Side 0 |               | Side 2
       |               |
       |               |
       |  P0       P3  |
       |               |
       +---------------+
     C0     Side 3        C3
  @end{pre}
  Each patch is constructed by first calling the
  @fun{cairo:mesh-pattern-begin-patch} function, then the
  @fun{cairo:mesh-pattern-move-to} to specify the first point in the patch
  @code{(C0)}. Then the sides are specified with calls to the
  @fun{cairo:mesh-pattern-curve-to} and @fun{cairo:mesh-pattern-line-to}
  functions.

  The four additional control points @code{(P0, P1, P2, P3)} in a patch can be
  specified with the @fun{cairo:mesh-pattern-set-control-point} function.

  At each corner of the patch @code{(C0, C1, C2, C3)} a color may be specified
  with the @fun{cairo:mesh-pattern-set-corner-color-rgb} or
  @fun{cairo:mesh-pattern-set-corner-color-rgba} functions. Any corner whose
  color is not explicitly specified defaults to transparent black.

  A Coons patch is a special case of the tensor-product patch where the
  control points are implicitly defined by the sides of the patch. The default
  value for any control point not specified is the implicit value for a Coons
  patch, i.e. if no control points are specified the patch is a Coons patch.

  A triangle is a special case of the tensor-product patch where the control
  points are implicitly defined by the sides of the patch, all the sides are
  lines and one of them has length 0, i.e. if the patch is specified using
  just 3 lines, it is a triangle. If the corners connected by the 0-length
  side have the same color, the patch is a Gouraud-shaded triangle.

  Patches may be oriented differently to the above diagram. For example the
  first point could be at the top left. The diagram only shows the
  relationship between the sides, corners and control points. Regardless of
  where the first point is located, when specifying colors, corner 0 will
  always be the first point, corner 1 the point between side 0 and side 1 etc.

  Calling the @fun{cairo:mesh-pattern-end-patch} function completes the current
  patch. If less than 4 sides have been defined, the first missing side is
  defined as a line from the current point to the first point of the patch
  @code{(C0)} and the other sides are degenerate lines from @code{C0} to
  @code{C0}. The corners between the added sides will all be coincident with
  @code{C0} of the patch and their color will be set to be the same as the color
  of @code{C0}.

  Additional patches may be added with additional calls to the
  @fun{cairo:mesh-pattern-begin-patch} and @fun{cairo:mesh-pattern-end-patch}
  functions.

  When two patches overlap, the last one that has been added is drawn over
  the first one.

  When a patch folds over itself, points are sorted depending on their
  parameter coordinates inside the patch. The @code{v} coordinate ranges from 0
  to 1 when moving from side 3 to side 1. The @code{u} coordinate ranges from 0
  to 1 when going from side 0 to side 2. Points with higher @code{v} coordinate
  hide points with lower @code{v} coordinate. When two points have the same
  @code{v} coordinate, the one with higher @code{u} coordinate is above. This
  means that points nearer to side 1 are above points nearer to side 3. When
  this is not sufficient to decide which point is above (for example when both
  points belong to side 1 or side 3) points nearer to side 2 are above points
  nearer to side 0.

  For a complete definition of tensor-product patches, see the PDF
  specification (ISO32000), which describes the parametrization in detail.
  @begin[Note]{dictionary}
    The coordinates are always in pattern space. For a new pattern, pattern
    space is identical to user space, but the relationship between the spaces
    can be changed with the @fun{cairo:pattern-matrix} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
;; Add a Coons patch
(let ((pattern (cairo:pattern-create-mesh)))
  ...
  ;; Start the patch
  (cairo:mesh-pattern-begin-patch pattern)
  ;; Set the path
  (cairo:mesh-pattern-move-to pattern 0 0)
  (cairo:mesh-pattern-curve-to pattern 30 -30 60 30 100 0)
  (cairo:mesh-pattern-curve-to pattern 60 30 130 60 100 100)
  (cairo:mesh-pattern-curve-to pattern 60 70 30 130 0 100)
  (cairo:mesh-pattern-curve-to pattern 30 70 -30 30 0 0)
  ;; Set the colors
  (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
  (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
  (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)
  (cairo:mesh-pattern-set-corner-color-rgb pattern 3 1 1 0)
  ;; Stop the patch
  (cairo:mesh-pattern-end-patch pattern)
  ...
  )

;; Add a Gouraud-shaded triangle
(let ((pattern (cairo:pattern-create-mesh)))
  ...
  ;; Start the patch
  (cairo:mesh-pattern-begin-patch pattern)
  ;; Set the path
  (cairo:mesh-pattern-move-to pattern 100 100)
  (cairo:mesh-pattern-line-to patten 130 130)
  (cairo:mesh-pattern-line-to pattern 130  70)
  ;; Set the colors
  (cairo:mesh-pattern-set-corner-color-rgb pattern 0 1 0 0)
  (cairo:mesh-pattern-set-corner-color-rgb pattern 1 0 1 0)
  (cairo:mesh-pattern-set-corner-color-rgb pattern 2 0 0 1)
  ;; Stop the patch
  (cairo:mesh-pattern-end-patch pattern)
  ...
  )
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-destroy}
  @see-function{cairo:pattern-status}
  @see-function{cairo:mesh-pattern-begin-patch}
  @see-function{cairo:mesh-pattern-end-patch}
  @see-function{cairo:mesh-pattern-move-to}
  @see-function{cairo:mesh-pattern-curve-to}
  @see-function{cairo:mesh-pattern-line-to}
  @see-function{cairo:mesh-pattern-set-control-point}
  @see-function{cairo:mesh-pattern-set-corner-color-rgb}
  @see-function{cairo:mesh-pattern-set-corner-color-rgba}
  @see-function{cairo:pattern-matrix}")

(export 'pattern-create-mesh)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_begin_patch ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mesh_pattern_begin_patch" mesh-pattern-begin-patch) :void
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @begin{short}
    Begin a patch in a mesh pattern.
  @end{short}
  After calling this function, the patch shape should be defined with the
  @fun{cairo:mesh-pattern-move-to}, @fun{cairo:mesh-pattern-line-to}
  and @fun{cairo:mesh-pattern-curve-to} functions.

  After defining the patch, the @fun{cairo:mesh-pattern-end-patch} function
  must be called before using the pattern as a source or mask.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a mesh pattern then @arg{pattern} will
    be put into an error status with a @code{:pattern-type-mismatch} value. If
    @arg{pattern} already has a current patch, it will be put into an error
    status with a @code{:invalid-mesh-contstruction} value.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:status-t}
  @see-function{cairo:mesh-pattern-move-to}
  @see-function{cairo:mesh-pattern-line-to}
  @see-function{cairo:mesh-pattern-curve-to}"
  (pattern (:pointer (:struct pattern-t))))

(export 'mesh-pattern-begin-patch)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_end_patch ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mesh_pattern_end_patch" mesh-pattern-end-patch) :void
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @begin{short}
    Indicates the end of the current patch in a mesh pattern.
  @end{short}
  If the current patch has less than 4 sides, it is closed with a straight
  line from the current point to the first point of the patch as if the
  @fun{cairo:mesh-pattern-line-to} function was used.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a mesh pattern then @arg{pattern} will
    be put into an error status with a @code{:pattern-type-mismatch} value. If
    @arg{pattern} has no current patch or the current patch has no current
    point, @arg{pattern} will be put into an error status with a
    @code{:invalid-mesh-construction} value.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-symbol{cairo:status-t}
  @see-function{cairo:mesh-pattern-line-to}"
  (pattern (:pointer (:struct pattern-t))))

(export 'mesh-pattern-end-patch)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_move_to ()
;;; ----------------------------------------------------------------------------

(defun mesh-pattern-move-to (pattern x y)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[x]{a number coerced to a double float with the x coordinate of the
    new position}
  @argument[y]{a number coerced to a double float with the y coordinate of the
    new position}
  @begin{short}
    Define the first point of the current patch in a mesh pattern.
  @end{short}
  After this call the current point will be @arg{(x, y)}.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a mesh pattern then @arg{pattern} will
    be put into an error status with a @code{:pattern-type-mismatch} value. If
    @arg{pattern} has no current patch or the current patch already has at least
    one side, @arg{pattern} will be put into an error status with a
    @code{:invalid-mesh-construction} value.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}"
  (cffi:foreign-funcall "cairo_mesh_pattern_move_to"
                        (:pointer (:struct pattern-t)) pattern
                        :double (coerce x 'double-float)
                        :double (coerce y 'double-float)
                        :void))

(export 'mesh-pattern-move-to)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_line_to ()
;;; ----------------------------------------------------------------------------

(defun mesh-pattern-line-to (pattern x y)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[x]{a number coerced to a double float with the x coordinate of the
    end of the new line}
  @argument[y]{a number coerced to a double float with the y coordinate of the
    end of the new line}
  @begin{short}
    Adds a line to the current patch from the current point to position
    (@arg{x}, @arg{y}) in pattern-space coordinates.
  @end{short}

  If there is no current point before the call to the
  @fun{cairo:mesh-pattern-line-to} function this function will behave as
  @begin{pre}
(cairo:mesh-pattern-move-to pattern x y)
  @end{pre}
  After this call the current point will be (@arg{x}, @arg{y}).
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a mesh pattern then @arg{pattern} will
    be put into an error status with a @code{:pattern-type-mismatch} value. If
    @arg{pattern} has no current patch or the current patch already has 4 sides,
    @arg{pattern} will be put into an error status with a
    @code{:invalid-mesh-construction} value.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:mesh-pattern-move-to}"
  (cffi:foreign-funcall "cairo_mesh_pattern_line_to"
                        (:pointer (:struct pattern-t)) pattern
                        :double (coerce x 'double-float)
                        :double (coerce y 'double-float)
                        :void))

(export 'mesh-pattern-line-to)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_curve_to ()
;;; ----------------------------------------------------------------------------

(defun mesh-pattern-curve-to (pattern x1 y1 x2 y2 x3 y3)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[x1]{a number with the x coordinate of the first control point}
  @argument[y1]{a number with the y coordinate of the first control point}
  @argument[x2]{a number with the x coordinate of the second control point}
  @argument[y2]{a number with the y coordinate of the second control point}
  @argument[x3]{a number with the x coordinate of the end of the curve}
  @argument[y3]{a number with the y coordinate of the end of the curve}
  @begin{short}
    Adds a cubic Bézier spline to the current patch from the current point to
    position @arg{(x3, y3)} in pattern-space coordinates, using
    @arg{(x1, y1)} and @arg{(x2, y2)} as the control points.
  @end{short}
  The coordinates are coerced to the @code{double-float} type before being
  passed to the C function.

  If the current patch has no current point before the call to the
  @fun{cairo:mesh-pattern-curve-to} function, this function will behave as if
  preceded by a call
  @begin{pre}
(cairo:mesh-pattern-move-to pattern x1 y1)
  @end{pre}
  After this call the current point will be @arg{(x3, y3)}.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a mesh pattern then @arg{pattern} will
    be put into an error status with a @code{:pattern-type-mismatch} value. If
    @arg{pattern} has no current patch or the current patch already has 4 sides,
    @arg{pattern} will be put into an error status with a
    @code{:invalid-mesh-construction} value.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:mesh-pattern-move-to}"
  (cffi:foreign-funcall "cairo_mesh_pattern_curve_to"
                        (:pointer (:struct pattern-t)) pattern
                        :double (coerce x1 'double-float)
                        :double (coerce y1 'double-float)
                        :double (coerce x2 'double-float)
                        :double (coerce y2 'double-float)
                        :double (coerce x3 'double-float)
                        :double (coerce y3 'double-float)
                        :void))

(export 'mesh-pattern-curve-to)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_control_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mesh_pattern_get_control_point"
               %mesh-pattern-control-point) status-t
  (pattern (:pointer (:struct pattern-t)))
  (index :uint)
  (point :uint)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun mesh-pattern-control-point (pattern index point)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[index]{an unsigned integer with the patch number to return data for}
  @argument[point]{an unsigned integer with the control point number to return
    data for}
  @begin{return}
    @arg{x} -- a double float x coordinate of the control point @br{}
    @arg{y} -- a double float y coordinate of the control point
  @end{return}
  @begin{short}
    Gets the control point @arg{point} of patch @arg{index} for a mesh pattern.
  @end{short}
  Returns @code{nil} if @arg{index} or @arg{point} is not valid for
  @arg{pattern}.

  The @arg{index} argument can range 0 to 1 less than the number returned by
  the @fun{cairo:mesh-pattern-patch-count} function.

  Valid values for @arg{point} are from 0 to 3 and identify the control points
  as explained for the @fun{cairo:pattern-create-mesh} function.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:mesh-pattern-patch-count}
  @see-function{cairo:pattern-create-mesh}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (eq :success
              (%mesh-pattern-control-point pattern
                                           index
                                           point
                                           x y))
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'mesh-pattern-control-point)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_control_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mesh_pattern_set_control_point"
          %mesh-pattern-set-control-point) :void
  (pattern (:pointer (:struct pattern-t)))
  (index :uint)
  (x :double)
  (y :double))

(defun mesh-pattern-set-control-point (pattern point x y)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[point]{an unsigned integer with the control point to set the
    position for}
  @argument[x]{a number coerced to a double float with the x coordinate of the
    control point}
  @argument[y]{a number coerced to a double float with the y coordinate of the
    control point}
  @begin{short}
    Set an internal control point of the current patch.
  @end{short}
  Valid values for @arg{point} are from 0 to 3 and identify the control
  points as explained for the @fun{cairo:pattern-create-mesh} function.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a mesh pattern then @arg{pattern} will
    be put into an error status with a status of @code{:pattern-type-mismatch}.
    If @arg{point} is not valid, @arg{pattern} will be put into an error status
    with a status of @code{:invalid-index}. If @arg{pattern} has no current
    patch, @arg{pattern} will be put into an error status with a status of
    @code{:invalid-mesh-construction}.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:pattern-create-mesh}"
  (%mesh-pattern-set-control-point pattern
                                   point
                                   (coerce x 'double-float)
                                   (coerce y 'double-float)))

(export 'mesh-pattern-set-control-point)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_corner_color_rgba ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mesh_pattern_get_corner_color_rgba"
               %mesh-pattern-corner-color-rgba) status-t
  (pattern (:pointer (:struct pattern-t)))
  (index :uint)
  (corner :uint)
  (red (:pointer :double))
  (green (:pointer :double))
  (blue (:pointer :double))
  (alpha (:pointer :double)))

(defun mesh-pattern-corner-color-rgba (pattern index corner)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[index]{an unsigned integer with the patch number to return data for}
  @argument[corner]{an unsigned integer with the corner number to return data
    for}
  @begin{return}
    @arg{red} -- a double float red component of color @br{}
    @arg{green} -- a double float green component of color @br{}
    @arg{blue} -- a double float blue component of color @br{}
    @arg{alpha} -- a double float alpha component of color
  @end{return}
  @begin{short}
    Gets the color information in corner @arg{corner} of patch @arg{index} for
    a mesh pattern.
  @end{short}
  Returns @code{nil} if @arg{pattern} or @arg{index} and @arg{corner} are not
  valid for @arg{pattern}.

  The @arg{index} argument can range 0 to 1 less than the number returned by the
  @fun{cairo:mesh-pattern-patch-count} function.

  Valid values for @arg{corner} are from 0 to 3 and identify the corners as
  explained for the @fun{cairo:pattern-create-mesh} function.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:mesh-pattern-patch-count}
  @see-function{cairo:pattern-create-mesh}"
  (cffi:with-foreign-objects ((red :double)
                              (green :double)
                              (blue :double)
                              (alpha :double))
    (when (eq :success
              (%mesh-pattern-corner-color-rgba pattern
                                               index
                                               corner
                                               red
                                               green
                                               blue
                                               alpha))
      (values (cffi:mem-ref red :double)
              (cffi:mem-ref green :double)
              (cffi:mem-ref blue :double)
              (cffi:mem-ref alpha :double)))))

(export 'mesh-pattern-corner-color-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_corner_color_rgb ()
;;; ----------------------------------------------------------------------------

(defun mesh-pattern-set-corner-color-rgb (pattern index red green blue)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[corner]{an unsigned integer with the corner to set the color
    for}
  @argument[red]{a number coerced to a double float with the red component
    of color}
  @argument[green]{a number coerced to a double float with the green component
    of color}
  @argument[blue]{a number coerced to a double float with the blue component
    of color}
  @begin{short}
    Sets the color of a corner of the current patch in a mesh pattern.
  @end{short}
  The color is specified in the same way as for the @fun{cairo:set-source-rgb}
  function.

  Valid values for @arg{corner} are from 0 to 3 and identify the corners as
  explained for the @fun{cairo:pattern-create-mesh} function.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a mesh pattern then @arg{pattern} will
    be put into an error status with a @code{:pattern-type-mismatch} value. If
    @arg{corner} is not valid, @arg{pattern} will be put into an error status
    with a @code{:invalid-index} value. If @arg{pattern} has no current patch,
    @arg{pattern} will be put into an error status with a
    @code{:invalid-mesh-construction} value.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:set-source-rgb}
  @see-function{cairo:pattern-create-mesh}"
  (cffi:foreign-funcall "cairo_mesh_pattern_set_corner_color_rgb"
                        (:pointer (:struct pattern-t)) pattern
                        :uint index
                        :double (coerce red 'double-float)
                        :double (coerce green 'double-float)
                        :double (coerce blue 'double-float)
                        :void))

(export 'mesh-pattern-set-corner-color-rgb)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_set_corner_color_rgba ()
;;; ----------------------------------------------------------------------------

(defun mesh-pattern-set-corner-color-rgba (pattern index red green blue alpha)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[corner]{an unsigned integer with the corner to set the color for}
  @argument[red]{a number coerced to a double float with the red component
    of color}
  @argument[green]{a number coerced to a double float with the green component
    of color}
  @argument[blue]{a number coerced to a double float with the blue component
    of color}
  @argument[alpha]{a number coerced to a double float with the alpha component
    of color}
  @begin{short}
    Sets the color of a corner of the current patch in a mesh pattern.
  @end{short}
  The color is specified in the same way as for the @fun{cairo:set-source-rgba}
  function.

  Valid values for @arg{corner} are from 0 to 3 and identify the corners as
  explained for the @fun{cairo:pattern-create-mesh} function.
  @begin[Note]{dictionary}
    If the @arg{pattern} argument is not a mesh pattern then @arg{pattern} will
    be put into an error status with a @code{:pattern-type-mismatch} value. If
    @arg{corner} is not valid, @arg{pattern} will be put into an error status
    with a @code{:invalid-index} value. If @arg{pattern} has no current patch,
    @arg{pattern} will be put into an error status with a
    @code{:invalid-mesh-construction} value.
  @end{dictionary}
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:set-source-rgba}
  @see-function{cairo:pattern-create-mesh}"
  (cffi:foreign-funcall "cairo_mesh_pattern_set_corner_color_rgba"
                        (:pointer (:struct pattern-t)) pattern
                        :uint index
                        :double (coerce red 'double-float)
                        :double (coerce green 'double-float)
                        :double (coerce blue 'double-float)
                        :double (coerce alpha 'double-float)
                        :void))

(export 'mesh-pattern-set-corner-color-rgba)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_patch_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mesh_pattern_get_patch_count" %mesh-pattern-patch-count)
    status-t
  (pattern (:pointer (:struct pattern-t)))
  (count (:pointer :uint)))

(defun mesh-pattern-patch-count (pattern)
 #+liber-documentation
 "@version{2024-2-2}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @return{The unsigned integer with the number of patches.}
  @begin{short}
    Gets the number of patches specified in the given mesh pattern.
  @end{short}
  Returns @code{nil} if the @arg{pattern} argument is not a mesh pattern.

  The number only includes patches which have been finished by calling the
  @fun{cairo:mesh-pattern-end-patch} function. For example it will be 0 during
  the definition of the first patch.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:mesh-pattern-end-patch}"
  (cffi:with-foreign-object (count :uint)
    (when (eq :success (%mesh-pattern-patch-count pattern count))
      (cffi:mem-ref count :uint))))

(export 'mesh-pattern-patch-count)

;;; ----------------------------------------------------------------------------
;;; cairo_mesh_pattern_get_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_mesh_pattern_get_path" %mesh-pattern-path) :pointer
  (pattern (:pointer (:struct pattern-t)))
  (index :uint))

(defun mesh-pattern-path (pattern index)
 #+liber-documentation
 "@version{2024-2-15}
  @argument[pattern]{a @symbol{cairo:pattern-t} instance}
  @argument[index]{an unsigned integer with the patch number to return data
    for}
  @return{The path defining the patch as a list.}
  @begin{short}
    Gets the path defining the patch @arg{patch-num} for a mesh pattern.
  @end{short}
  Returns @code{nil} if the @arg{pattern} argument is not a mesh pattern or
  @arg{index} is not valid for @arg{pattern}.

  The @arg{index} argument can range 0 to 1 less than the number returned by
  the @fun{cairo:mesh-pattern-patch-count} function.
  @see-symbol{cairo:pattern-t}
  @see-function{cairo:mesh-pattern-patch-count}"
  (let (path)
    (when (eq :success
              (path-status (setf path (%mesh-pattern-path pattern index))))
      (path-data-to-list path))))

(export 'mesh-pattern-path)

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_set_user_data ()
;;;
;;; cairo_status_t
;;; cairo_pattern_set_user_data (cairo_pattern_t *pattern,
;;;                              const cairo_user_data_key_t *key,
;;;                              void *user_data,
;;;                              cairo_destroy_func_t destroy);
;;;
;;; Attach user data to pattern. To remove user data from a surface, call this
;;; function with the key that was used to set it and NULL for data.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_pattern_t
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the cairo_t is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pattern_get_user_data ()
;;;
;;; void *
;;; cairo_pattern_get_user_data (cairo_pattern_t *pattern,
;;;                              const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to pattern using the specified key.
;;; If no user data has been attached with the given key this function returns
;;; NULL.
;;;
;;; pattern :
;;;     a cairo_pattern_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     The user data previously attached or NULL.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.pattern.lisp -----------------------------------------
