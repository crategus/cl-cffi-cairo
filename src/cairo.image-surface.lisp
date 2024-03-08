;;; ----------------------------------------------------------------------------
;;; cairo.image-surface.lisp
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
;;; Image Surfaces
;;;
;;;     Rendering to memory buffers
;;;
;;; Types and Values
;;;
;;;     cairo_format_t                           --> cairo.surface.lisp
;;;
;;; Functions
;;;
;;;     cairo_image_surface_create
;;;     cairo_image_surface_create_for_data
;;;     cairo_image_surface_get_data
;;;     cairo_image_surface_get_format
;;;     cairo_image_surface_get_width
;;;     cairo_image_surface_get_height
;;;     cairo_image_surface_get_stride
;;;
;;;     cairo_format_stride_for_width
;;; ----------------------------------------------------------------------------

(in-package :cairo)

(defmacro with-image-surface ((surface &rest args) &body body)
 #+liber-documentation
 "@version{2024-2-12}
  @syntax{(cairo:with-image-surface (surface format width height) body) =>
    result}
  @argument[surface]{a @symbol{cairo:surface-t} instance to create and
    initialize}
  @argument[format]{a @symbol{cairo:format-t} value with the format of pixels
    in the surface to create}
  @argument[width]{an integer with the width of the surface, in pixels}
  @argument[height]{an integer with the height of the surface, in pixels}
  @begin{short}
    The @fun{cairo:with-image-surface} macro allocates a new
    @symbol{cairo:surface-t} instance, initializes the Cairo surface with the
    @arg{format}, @arg{width}, and @arg{height} values and executes the body
    that uses the Cairo surface.
  @end{short}
  After execution of the body the allocated memory for the Cairo surface is
  released. See the documentation of the @fun{cairo:image-surface-create}
  function for more information about the initialization of the new Cairo
  surface.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:format-t}
  @see-function{cairo:image-surface-create}"
  `(let ((,surface (image-surface-create ,@args)))
     (unwind-protect
       (progn ,@body)
       (surface-destroy ,surface))))

(export 'with-image-surface)

;;; ----------------------------------------------------------------------------

(defmacro with-context-for-image-surface ((context &rest args) &body body)
 #+liber-documentation
 "@version{2024-2-12}
  @syntax{(cairo:with-context-for-image-surface (context format width height)
    body) => result}
  @argument[context]{a @symbol{cairo:context-t} instance to create and
    initialize}
  @argument[format]{a @symbol{cairo:format-t} value with the format of pixels
    in the surface to create}
  @argument[width]{an integer with the width of the surface, in pixels}
  @argument[height]{an integer with the height of the surface, in pixels}
  @begin{short}
    The @fun{cairo:with-context-for-image-surface} macro allocates a new
    @symbol{cairo:context-t} instance for an image surface, initializes the
    Cairo context with the @arg{format}, @arg{width}, and @arg{height} values
    and executes the body that uses the Cairo context.
  @end{short}
  After execution of the body the allocated memory for the Cairo surface is
  released. See the documentation of the @fun{cairo:image-surface-create} and
  @fun{cairo:create} functions for more information about the initialization of
  the new Cairo context.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:format-t}
  @see-function{cairo:create}
  @see-function{cairo:image-surface-create}"
  (let ((surface (gensym)))
    `(with-image-surface (,surface ,@args)
       (with-context (,context ,surface)
         (progn ,@body)))))

(export 'with-context-for-image-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_image_surface_create" image-surface-create)
    (:pointer (:struct surface-t))
 #+liber-documentation
 "@version{2024-2-12}
  @argument[format]{a @symbol{cairo:format-t} value with the format of pixels
    in the surface to create}
  @argument[width]{an integer with the width of the surface, in pixels}
  @argument[height]{an integer with the height of the surface, in pixels}
  @begin{return}
    The newly created @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it.
  @end{return}
  @begin{short}
    Creates an image surface of the specified format and dimensions.
  @end{short}
  Initially the surface contents are all 0. Specifically, within each pixel,
  each color or alpha channel belonging to the format will be 0. The contents
  of bits within a pixel, but not belonging to the given format are undefined.

  This function always returns a valid surface, but it will return a \"nil\"
  surface if an error such as out of memory occurs. You can use the
  @fun{cairo:surface-status} function to check for this.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:format-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-status}"
  (format format-t)
  (width :int)
  (height :int))

(export 'image-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_create_for_data ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_image_surface_create_for_data"
               image-surface-create-for-data) (:pointer (:struct surface-t))
 #+liber-documentation
 "@version{2024-2-12}
  @argument[data]{a pointer to a buffer supplied by the application in which to
    write contents, this pointer must be suitably aligned for any kind of
    variable, for example, a pointer returned by @code{malloc}}
  @argument[format]{a @symbol{cairo:format-t} value with the format of pixels
    in the surface to create}
  @argument[width]{an integer with the width of the image to be stored in the
    buffer}
  @argument[height]{an integer with the height of the image to be stored in the
    buffer}
  @argument[stride]{an integer with the number of bytes between the start of
    rows in the buffer as allocated, this value should always be computed by
    the @fun{caro:format-stride-for-width} function before allocating the data
    buffer}
  @begin{return}
    The newly created @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid surface, but it will return
    a \"nil\" surface in the case of an error such as out of memory or an
    invalid stride value. In case of invalid stride value the error status of
    the returned surface will be @code{:invalid-stride}. You can use the
    @fun{cairo:surface-status} function to check for this.
  @end{return}
  @begin{short}
    Creates an image surface for the provided pixel data.
  @end{short}
  The output buffer must be kept around until the Cairo surface is destroyed or
  the @fun{cairo:surface-finish} function is called on the surface. The initial
  contents of @arg{data} will be used as the initial image contents. You must
  explicitly clear the buffer, using, for example, the @fun{cairo:rectangle}
  and @fun{cairo:fill} functions if you want it cleared.

  Note that the stride may be larger than @arg{width} x bytes per pixel to
  provide proper alignment for each pixel and row. This alignment is required
  to allow high-performance rendering within Cairo. The correct way to obtain a
  legal stride value is to call the @fun{cairo:format-stride-for-width} function
  with the desired format and maximum image width value, and then use the
  resulting stride value to allocate the data and to create the image surface.
  See the @fun{cairo:format-stride-for-width} function for example code.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:format-stride-for-width}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-status}
  @see-function{cairo:surface-finish}
  @see-function{cairo:rectangle}
  @see-function{cairo:fill}"
  (data :pointer)
  (format format-t)
  (width :int)
  (height :int)
  (stride :int))

(export 'image-surface-create-for-data)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_data ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_image_surface_get_data" %image-surface-data) :pointer
  (surface (:pointer (:struct surface-t))))

(defun image-surface-data (surface)
 #+liber-documentation
 "@version{2024-2-12}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{return}
    The pointer to the image data of this surface or @code{nil} if @arg{surface}
    is not an image surface, or if the @fun{cairo:surface-finish} function has
    been called.
  @end{return}
  @begin{short}
    Get the pointer to the data of the image surface, for direct inspection or
    modification.
  @end{short}
  A call to the @fun{cairo:surface-flush} function is required before accessing
  the pixel data to ensure that all pending drawing operations are finished. A
  call to the @fun{cairo:surface-mark-dirty} function is required after the
  data is modified.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-finish}
  @see-function{cairo:surface-flush}
  @see-function{cairo:surface-mark-dirty}"
  (let (data)
    (when (not (cffi:null-pointer-p (setf data (%image-surface-data surface))))
      data)))

(export 'image-surface-data)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_format ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_image_surface_get_format" image-surface-format)
    format-t
 #+liber-documentation
 "@version{2024-2-12}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @return{The @symbol{cairo:format-t} value with the format of the surface.}
  @begin{short}
    Get the format of the image surface.
  @end{short}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:format-t}"
  (surface (:pointer (:struct surface-t))))

(export 'image-surface-format)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_width ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_image_surface_get_width" image-surface-width) :int
 #+liber-documentation
 "@version{2024-2-12}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @return{The integer with the width of the surface in pixels.}
  @begin{short}
    Gets the width of the image surface in pixels.
  @end{short}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:image-surface-height}"
  (surface (:pointer (:struct surface-t))))

(export 'image-surface-width)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_height ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_image_surface_get_height" image-surface-height) :int
 #+liber-documentation
 "@version{2024-2-12}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @return{The integer with the height of the surface in pixels.}
  @begin{short}
    Gets the height of the image surface in pixels.
  @end{short}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:image-surface-width}"
  (surface (:pointer (:struct surface-t))))

(export 'image-surface-height)

;;; ----------------------------------------------------------------------------
;;; cairo_image_surface_get_stride ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_image_surface_get_stride" image-surface-stride) :int
 #+liber-documentation
 "@version{2024-2-12}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{return}
    The integer with the stride of the image surface in bytes, or 0 if
    @arg{surface} is not an image surface.
  @end{return}
  @begin{short}
    Get the stride of the image surface in bytes.
  @end{short}
  The stride is the distance in bytes from the beginning of one row of the
  image data to the beginning of the next row.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:format-stride-for-width}"
  (surface (:pointer (:struct surface-t))))

(export 'image-surface-stride)

;;; ----------------------------------------------------------------------------
;;; cairo_format_stride_for_width ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_format_stride_for_width" format-stride-for-width) :int
 #+liber-documentation
 "@version{2024-2-12}
  @argument[format]{a @symbol{cairo:format-t} value}
  @argument[width]{an integer with the desired width of an image surface to
    be created.}
  @begin{return}
    The integer with the appropriate stride to use given the desired format and
    width, or -1 if either the format is invalid or the width too large.
  @end{return}
  @begin{short}
    This function provides a stride value that will respect all Cairo alignment
    requirements of the accelerated image-rendering code within Cairo.
  @end{short}
  @begin[Example]{dictionary}
   Typical usage will be of the form:
   @begin{pre}
(let* ((height 150)
       (width 200)
       (stride (cairo:format-stride-for-width :argb32 width))
       (data (g:malloc (* height stride)))
       (surface (cairo:image-surface-create-for-data data
                                                     :argb32
                                                     width
                                                     height
                                                     stride)))
  ... )
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:format-t}"
  (format format-t)
  (width :int))

(export 'format-stride-for-width)

;;; --- End of file cairo.image-surface.lisp -----------------------------------
