;;; ----------------------------------------------------------------------------
;;; cairo.surface.lisp
;;;
;;; The documentation in this file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo library,
;;; see <http://cairographics.org>. The API documentation of the Lisp binding
;;; is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; cairo_surface_t
;;;
;;;     Base class for surfaces
;;;
;;; Types and Values
;;;
;;;     cairo_surface_t
;;;     cairo_content_t
;;;     cairo_surface_type_t
;;;
;;;     cairo_format_t                 <- cairo.image-surface.lisp
;;;
;;; Functions
;;;
;;;     cairo_surface_create_similar
;;;     cairo_surface_create_similar_image
;;;     cairo_surface_create_for_rectangle
;;;     cairo_surface_reference
;;;     cairo_surface_get_reference_count
;;;     cairo_surface_destroy
;;;     cairo_surface_status
;;;     cairo_surface_get_type
;;;     cairo_surface_finish
;;;     cairo_surface_flush
;;;     cairo_surface_get_device
;;;     cairo_surface_get_font_options
;;;     cairo_surface_get_content
;;;     cairo_surface_mark_dirty
;;;     cairo_surface_mark_dirty_rectangle
;;;     cairo_surface_set_device_offset
;;;     cairo_surface_get_device_offset
;;;     cairo_surface_get_device_scale
;;;     cairo_surface_set_device_scale
;;;     cairo_surface_set_fallback_resolution
;;;     cairo_surface_get_fallback_resolution
;;;
;;;     cairo_surface_set_user_data                        not implemented
;;;     cairo_surface_get_user_data                        not implemented
;;;
;;;     cairo_surface_copy_page
;;;     cairo_surface_show_page
;;;
;;;     cairo_surface_has_show_text_glyphs                 not implemented
;;;     cairo_surface_set_mime_data                        not implemented
;;;     cairo_surface_get_mime_data                        not implemented
;;;     cairo_surface_supports_mime_type                   not implemented
;;;     cairo_surface_map_to_image                         not implemented
;;;     cairo_surface_unmap_image                          not implemented
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_format_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum format-t
  (:invalid -1)
  (:argb32 0)
  (:rgb24 1)
  (:a8 2)
  (:a1 3)
  (:rgb16-565 4)
  (:rgb30 5)
  #+cairo-1-18
  (:rgb96f 6)
  #+cairo-1-18
  (:rgba128f 7))

#+liber-documentation
(setf (liber:alias-for-symbol 'format-t)
      "CEnum"
      (liber:symbol-documentation 'format-t)
 "@version{2025-1-18}
  @begin{declaration}
(cffi:defcenum format-t
  (:invalid -1)
  (:argb32 0)
  (:rgb24 1)
  (:a8 2)
  (:a1 3)
  (:rgb16-565 4)
  (:rgb30 5)
  (:rgb96f 6)
  (:rgba128f 7))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:invalid]{No such format exists or is supported.}
      @entry[:argb32]{Each pixel is a 32-bit quantity, with alpha in the upper
        8 bits, then red, then green, then blue. The 32-bit quantities are
        stored native-endian. Pre-multiplied alpha is used. That is, 50 %
        transparent red is @code{0x80800000}, not @code{0x80ff0000}.}
      @entry[:rgb24]{Each pixel is a 32-bit quantity, with the upper 8 bits
        unused. Red, Green, and Blue are stored in the remaining 24 bits in
        that order.}
      @entry[:a8]{Each pixel is a 8-bit quantity holding an alpha value.}
      @entry[:a1]{Each pixel is a 1-bit quantity holding an alpha value. Pixels
        are packed together into 32-bit quantities. The ordering of the bits
        matches the endianess of the platform. On a big-endian machine, the
        first pixel is in the uppermost bit, on a little-endian machine the
        first pixel is in the least-significant bit.}
      @entry[:rgb16-565]{Each pixel is a 16-bit quantity with red in the upper
        5 bits, then green in the middle 6 bits, and blue in the lower 5 bits.}
      @entry[:rgb30]{Like @code{:rgb24} but with 10 bpc.}
      @entry[:rgb96f]{3 floats, R, G, B. Since 1.18}
      @entry[:rgba128f]{4 floats, R, G, B, A. Since 1.18}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:format-t} enumeration is used to identify the memory
    format of image data.
  @end{short}
  @see-symbol{cairo:surface-t}")

(export 'format-t)

;;; ----------------------------------------------------------------------------
;;; cairo_content_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum content-t
  (:color #x1000)
  (:alpha #x2000)
  (:color-alpha #x3000))

#+liber-documentation
(setf (liber:alias-for-symbol 'content-t)
      "CEnum"
      (liber:symbol-documentation 'content-t)
 "@version{2025-1-18}
  @begin{declaration}
(cffi:defcenum content-t
  (:color #x1000)
  (:alpha #x2000)
  (:color-alpha #x3000))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:color]{The surface will hold color content only.}
      @entry[:alpha]{The surface will hold alpha content only.}
      @entry[:color-alpha]{The surface will hold color and alpha content.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:content-t} enumeration is used to describe the content
    that a surface will contain, whether color information, alpha information
    (translucence vs. opacity), or both.
  @end{short}
  @begin[Notes]{dictionary}
    The large values here are designed to keep @symbol{cairo:content-t} values
    distinct from @symbol{cairo:format-t} values so that the implementation can
    detect the error if users confuse the two types.
  @end{dictionary}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:format-t}")

(export 'content-t)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_type_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum surface-type-t
  :image
  :pdf
  :ps
  :xlib
  :xcb
  :glitz
  :quartz
  :win32
  :beos
  :directfb
  :svg
  :os2
  :win32-printing
  :quartz-image
  :script
  :qt
  :recording
  :vg
  :gl
  :drm
  :tee
  :xml
  :skia
  :subsurface
  :cogl)

#+liber-documentation
(setf (liber:alias-for-symbol 'surface-type-t)
      "CEnum"
      (liber:symbol-documentation 'surface-type-t)
 "@version{2025-1-18}
  @begin{declaration}
(cffi:defcenum surface-type-t
  :image
  :pdf
  :ps
  :xlib
  :xcb
  :glitz
  :quartz
  :win32
  :beos
  :directfb
  :svg
  :os2
  :win32-printing
  :quartz-image
  :script
  :qt
  :recording
  :vg
  :gl
  :drm
  :tee
  :xml
  :skia
  :subsurface
  :cogl)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:image]{The surface is of type image.}
      @entry[:pdf]{The surface is of type pdf.}
      @entry[:ps]{The surface is of type ps.}
      @entry[:xlib]{The surface is of type xlib.}
      @entry[:xcb]{The surface is of type xcb.}
      @entry[:glitz]{The surface is of type glitz. Deprecated 1.18: glitz
        support have been removed, this surface type will never be set by
        Cairo.}
      @entry[:quartz]{The surface is of type quartz.}
      @entry[:win32]{The surface is of type win32.}
      @entry[:beos]{The surface is of type beos. Deprecated 1.18: beos support
        have been removed, this surface type will never be set by Cairo.}
      @entry[:directfb]{The surface is of type directfb. Deprecated 1.18:
        directfb support have been removed, this surface type will never be set
        by Cairo.}
      @entry[:svg]{The surface is of type svg.}
      @entry[:os2]{The surface is of type os2. Deprecated 1.18: os2 support
        have been removed, this surface type will never be set by Cairo.}
      @entry[:win32-printing]{The surface is a win32 printing surface.}
      @entry[:quartz-image]{The surface is of type quartz_image.}
      @entry[:script]{The surface is of type script.}
      @entry[:qt]{The surface is of type Qt. Deprecated 1.18: Ot support have
        been removed, this surface type will never be set by Cairo.}
      @entry[:recording]{The surface is of type recording.}
      @entry[:vg]{The surface is a OpenVG surface. Deprecated 1.18: OpenVG
        support have been removed, this surface type will never be set by
        Cairo.}
      @entry[:gl]{The surface is of type OpenGL. Deprecated 1.18: OpenGL support
        have been removed, this surface type will never be set by Cairo.}
      @entry[:drm]{The surface is of type Direct Render Manager. Deprecated
        1.18: DRM support have been removed, this surface type will never be
        set by Cairo.}
      @entry[:tee]{The surface is of type 'tee' (a multiplexing surface).}
      @entry[:xml]{The surface is of type XML (for debugging).}
      @entry[:skia]{The surface is of type Skia. Deprecated 1.18: Skia support
        have been removed, this surface type will never be set by Cairo.}
      @entry[:subsurface]{The surface is a subsurface created with the
        @fun{cairo:surface-create-for-rectangle} function.}
      @entry[:cogl]{This surface is of type Cogl. Deprecated 1.18: Cogl support
        have been removed, this surface type will never be set by Cairo.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:surface-type-t} enumeration is used to describe the type
    of a given surface.
  @end{short}
  The surface types are also known as \"backends\" or \"surface backends\"
  within Cairo. The type of a surface is determined by the function used to
  create it, which will generally be of the form
  @code{cairo:type-surface-create}, though see the
  @fun{cairo:surface-create-similar} function as well. The surface type can be
  queried with the @fun{cairo:surface-type} function.

  The various @symbol{cairo:surface-t} functions can be used with surfaces of
  any type, but some backends also provide type-specific functions that must
  only be called with a surface of the appropriate type. These functions have
  names that begin with @code{cairo:type-surface} such as the
  @fun{cairo:image-surface-width} function.

  The behavior of calling a type-specific function with a surface of the wrong
  type is undefined.
  @begin[Notes]{dictionary}
    In the Lisp API support for the following surface types is currently
    available: @code{:image}, @code{:pdf}, @code{:ps}, @code{:svg},
    @code{:script}, @code{:recording}, and @code{:subsurface}.
  @end{dictionary}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-create-similar}
  @see-function{cairo:surface-type}
  @see-function{cairo:image-surface-width}
  @see-function{cairo:surface-create-for-rectangle}")

(export 'surface-type-t)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct surface-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'surface-t)
      "CStruct"
      (liber:symbol-documentation 'surface-t)
 "@version{2025-1-18}
  @begin{short}
    The @symbol{cairo:surface-t} structure represents an image, either as the
    destination of a drawing operation or as source when drawing onto another
    surface.
  @end{short}
  To draw to a @symbol{cairo:surface-t} instance, create a Cairo context with
  the surface as the target, using the @fun{cairo:create} function.

  There are different subtypes of a @symbol{cairo:surface-t} structure for
  different drawing backends. For example, the
  @fun{cairo:image-surface-create} function creates a bitmap image in memory.
  The type of a surface can be queried with the @fun{cairo:surface-type}
  function.

  The initial contents of a surface after creation depend upon the manner of
  its creation. If Cairo creates the surface and backing storage for the user,
  it will be initially cleared, for example, the
  @fun{cairo:image-surface-create} and @fun{cairo:surface-create-similar}
  functions. Alternatively, if the user passes in a reference to some backing
  storage and asks Cairo to wrap that in a @symbol{cairo:surface-t} structure,
  then the contents are not modified, for example, the
  @fun{cairo:image-surface-create-for-data} function.

  Memory management of a @symbol{cairo:surface-t} structure is done with the
  @fun{cairo:surface-reference} and @fun{cairo:surface-destroy} functions.
  @see-constructor{cairo:surface-create-similar}
  @see-constructor{cairo:surface-create-similar-image}
  @see-constructor{cairo:surface-create-for-rectangle}
  @see-function{cairo:create}
  @see-function{cairo:surface-type}
  @see-function{cairo:image-surface-create}
  @see-function{cairo:image-surface-create-for-data}
  @see-function{cairo:surface-reference}
  @see-function{cairo:surface-destroy}")

(export 'surface-t)

;;; ----------------------------------------------------------------------------
;;; cairo:with-surface
;;; ----------------------------------------------------------------------------

(defmacro with-surface ((surface &rest args) &body body)
 #+liber-documentation
 "@version{2025-1-18}
  @syntax{(cairo:with-surface (surface target content width height) body) =>
    result}
  @syntax{(cairo:with-surface (surface target format width height) body) =>
    result}
  @syntax{(cairo:with-surface (surface target x y width height) body) => result}
  @argument[surface]{a @symbol{cairo:surface-t} instance to create and
    initialize}
  @argument[target]{an existing @symbol{cairo:surface-t} instance used to
    select the backend of the new surface}
  @argument[content]{a @symbol{cairo:content-t} value for the content for the
    new surface}
  @argument[format]{a @symbol{cairo:format-t} value for the format of pixels
    in the surface to create}
  @argument[x]{a number coerced to a double float for the x origin of the
    subsurface from the top-left of the target surface, in device-space units}
  @argument[y]{a number coerced to a double float for the y origin of the
    subsurface from the top-left of the target surface, in device-space units}
  @argument[width]{a number coerced to a double float for the width of
    the subsurface, in device-space units}
  @argument[height]{a number coerced to a double float for the height of
    the subsurface, in device-space units}
  @begin{short}
    The @fun{cairo:with-surface} macro allocates a new @symbol{cairo:surface-t}
    instance, initializes the Cairo surface with the given values and executes
    the body that uses the Cairo surface.
  @end{short}
  After execution of the body the allocated memory for the Cairo surface is
  released. See the documentation of the @fun{cairo:surface-create-similar},
  @fun{cairo:surface-create-similar-image} and
  @fun{cairo:surface-create-for-rectangle} functions for more information about
  the initialization of the new Cairo surface.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:content-t}
  @see-symbol{cairo:format-t}
  @see-function{cairo:surface-create-similar}
  @see-function{cairo:surface-create-similar-image}
  @see-function{cairo:surface-create-for-rectangle}"
  (cond ((null (fifth args))
         (if (member (second args) '(:color :alpha :color-alpha) :test #'eq)
             `(let ((,surface (surface-create-similar ,@args)))
                (unwind-protect
                  (progn ,@body)
                  (surface-destroy ,surface)))
             `(let ((,surface (surface-create-similar-image ,@args)))
                (unwind-protect
                  (progn ,@body)
                  (surface-destroy ,surface)))))
        ((null (sixth args))
         `(let ((,surface (surface-create-for-rectangle ,@args)))
            (unwind-protect
              (progn ,@body)
              (surface-destroy ,surface))))
        (t
         (error "Syntax error in CAIRO:WITH-SURFACE"))))

(export 'with-surface)

;;; ----------------------------------------------------------------------------
;;; cairo:with-context-for-surface
;;; ----------------------------------------------------------------------------

(defmacro with-context-for-surface ((context &rest args) &body body)
 #+liber-documentation
 "@version{2025-1-18}
  @syntax{(cairo:with-context-for-surface (context target content width height)
    body) => result}
  @syntax{(cairo:with-context-for-surface (context target format width height)
    body) => result}
  @syntax{(cairo:with-context-for-surface (context target x y width height)
    body) => result}
  @argument[context]{a @symbol{cairo:context-t} instance to create and
    initialize}
  @argument[surface]{a @symbol{cairo:surface-t} instance to create and
    initialize}
  @argument[target]{an existing @symbol{cairo:surface-t} instance used to select
    the backend of the new surface}
  @argument[content]{a @symbol{cairo:content-t} value for the content for the
    new surface}
  @argument[format]{a @symbol{cairo:format-t} value for the format of pixels
    in the surface to create}
  @argument[x]{a number coerced to a double float for the x origin of the
    subsurface from the top-left of the target surface, in device-space units}
  @argument[y]{a number coerced to a double float for the y origin of
    the subsurface from the top-left of the target surface, in device-space
    units}
  @argument[width]{a number coerced to a double float for the width of
    the subsurface, in device-space units}
  @argument[height]{a number coerced to a double float for the height of
    the subsurface, in device-space units}
  @begin{short}
    The @fun{cairo:with-context-for-surface} macro allocates a new
    @symbol{cairo:context-t} instance for a surface, initializes the
    Cairo context with the given values and executes the body that uses the
    Cairo context.
  @end{short}
  After execution of the body the allocated memory for the Cairo context is
  released.

  The context is created with the @fun{cairo:create} function and destroyed
  with the @fun{cairo:destroy} function. The context uses a surface that is
  created with one of the @fun{cairo:surface-create-similar},
  @fun{cairo:surface-create-similar-image}, or
  @fun{cairo:surface-create-for-rectangle} functions. You can access the
  @symbol{cairo:surface-t} instance for the created context with the
  @fun{cairo:target} function. See also the @macro{cairo:with-surface} macro.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:content-t}
  @see-symbol{cairo:format-t}
  @see-function{cairo:create}
  @see-function{cairo:target}
  @see-function{cairo:surface-create-similar}
  @see-function{cairo:surface-create-similar-image}
  @see-function{cairo:surface-create-for-rectangle}"
  (let ((surface (gensym)))
    `(with-surface (,surface ,@args)
       (with-context (,context ,surface)
         (progn ,@body)))))

(export 'with-context-for-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_create_similar
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_create_similar" surface-create-similar)
    (:pointer (:struct surface-t))
 #+liber-documentation
 "@version{2025-1-18}
  @argument[target]{an existing @symbol{cairo:surface-t} instance used to select
    the backend of the new surface}
  @argument[content]{a @symbol{cairo:content-t} value for the content for the
    new surface}
  @argument[width]{an integer for the width of the new surface, in
    device-space units}
  @argument[height]{an integer for the height of the new surface, in
    device-space units}
  @begin{return}
    The newly allocated @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid surface, but it will return
    a \"nil\" surface if @arg{target} is already in an error state or any other
    error occurs.
  @end{return}
  @begin{short}
    Create a new surface that is as compatible as possible with an existing
    surface.
  @end{short}
  For example the new surface will have the same fallback resolution and font
  options as @arg{target}. Generally, the new surface will also use the same
  backend as @arg{target}, unless that is not possible for some reason. The type
  of the returned surface may be examined with the @fun{cairo:surface-type}
  function.

  Initially the surface contents are all 0 and transparent if contents have
  transparency, black otherwise.

  Use the @fun{cairo:surface-create-similar-image} function if you need an
  image surface which can be painted quickly to the target surface.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:content-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-create-similar-image}"
  (target (:pointer (:struct surface-t)))
  (content content-t)
  (width :int)
  (height :int))

(export 'surface-create-similar)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_create_similar_image
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_create_similar_image"
               surface-create-similar-image) (:pointer (:struct surface-t))
 #+liber-documentation
 "@version{2025-1-18}
  @argument[target]{an existing @symbol{cairo:surface-t} instance used to select
    the preference of the new surface}
  @argument[format]{a @symbol{cairo:format-t} value for the new surface}
  @argument[width]{an integer for the width of the new surface, in
    device-space units}
  @argument[height]{an integer for the height of the new surface, in
    device-space units}
  @begin{return}
    The newly allocated image @symbol{cairo:surface-t} instance. The caller owns
    the surface and should call the @fun{cairo:surface-destroy} function when
    done with it. This function always returns a valid surface, but it will
    return a \"nil\" surface if @arg{target} is already in an error state or
    any other error occurs.
  @end{return}
  @begin{short}
    Create a new image surface that is as compatible as possible for uploading
    to and the use in conjunction with an existing surface.
  @end{short}
  However, this surface can still be used like any normal image surface.

  Initially the surface contents are all 0 and transparent if contents have
  transparency, black otherwise.

  Use the @fun{cairo:surface-create-similar} function if you do not need an
  image surface.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:format-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-create-similar}"
  (target (:pointer (:struct surface-t)))
  (format format-t)
  (width :int)
  (height :int))

(export 'surface-create-similar-image)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_create_for_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_create_for_rectangle"
               %surface-create-for-rectangle) (:pointer (:struct surface-t))
  (target (:pointer (:struct surface-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun surface-create-for-rectangle (target x y width height)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[target]{an existing @symbol{cairo:surface-t} instance for which
    the subsurface will point to}
  @argument[x]{a number coerced to a double float for the x origin of the
    subsurface from the top-left of the target surface, in device-space units}
  @argument[y]{a number coerced to a double float for the y origin of
    the subsurface from the top-left of the target surface, in device-space
    units}
  @argument[width]{a number coerced to a double float for the width of
    the subsurface, in device-space units}
  @argument[height]{a number coerced to a double float for the height of
    the subsurface, in device-space units}
  @begin{return}
    The newly allocated @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid surface, but it will return
    \"nil\" surface if @arg{target} is already in an error state or any other
    error occurs.
  @end{return}
  @begin{short}
    Create a new surface that is a rectangle within the target surface.
  @end{short}
  All operations drawn to this surface are then clipped and translated onto
  the target surface. Nothing drawn via this subsurface outside of its bounds
  is drawn onto the target surface, making this a useful method for passing
  constrained child surfaces to library routines that draw directly onto the
  parent surface, that is with no further backend allocations, double buffering
  or copies.
  @begin[Notes]{dictionary}
    The semantics of subsurfaces have not been finalized yet unless the
    rectangle is in full device units, is contained within the extents of the
    target surface, and the target or device of the subsurface transforms are
    not changed.
  @end{dictionary}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-destroy}"
  (%surface-create-for-rectangle target
                                 (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 (coerce width 'double-float)
                                 (coerce height 'double-float)))

(export 'surface-create-for-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_reference
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_reference" surface-reference)
    (:pointer (:struct surface-t))
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @return{The referenced @symbol{cairo:surface-t} instance.}
  @begin{short}
    Increases the reference count on @arg{surface} by one.
  @end{short}
  This prevents @arg{surface} from being destroyed until a matching call to
  the @fun{cairo:surface-destroy} function is made.

  The number of references to a @symbol{cairo:surface-t} instance can be get
  using the @fun{cairo:surface-reference-count} function.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-reference-count}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_reference_count
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_get_reference_count" surface-reference-count)
    :uint
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{return}
    The current reference count of @arg{surface}. If the instance is a
    \"nil\" surface, 0 will be returned.
  @end{return}
  @begin{short}
    Returns the current reference count of @arg{surface}.
  @end{short}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-reference}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_destroy" surface-destroy) :void
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{short}
    Decreases the reference count on @arg{surface} by one.
  @end{short}
  If the result is zero, then @arg{surface} and all associated resources are
  freed. See the @fun{cairo:surface-reference} function.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-reference}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_status" surface-status) status-t
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @return{The @symbol{cairo:status-t} value for @arg{surface}.}
  @begin{short}
    Checks whether an error has previously occurred for this surface.
  @end{short}
  Possible values are @code{:success}, @code{:null-pointer}, @code{:no-memory},
    @code{:read-error}, @code{:invalid-content}, @code{:invalid-format}, or
    @code{:invalid-visual}.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:status-t}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-status)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_get_type" surface-type) surface-type-t
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @return{The @symbol{cairo:surface-type-t} value for @arg{surface}.}
  @begin{short}
    This function returns the type of the backend used to create the surface.
  @end{short}
  See the @symbol{cairo:surface-type-t} enumeration for available types.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:surface-type-t}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-type)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_finish" surface-finish) :void
 #+liber-documentation
 "@version{#2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{short}
    This function finishes the surface and drops all references to external
    resources.
  @end{short}
  For example, for the Xlib backend it means that Cairo will no longer access
  the drawable, which can be freed. After calling the @fun{cairo:surface-finish}
  function the only valid operations on a surface are getting and setting user,
  referencing and destroying, and flushing and finishing it. Further drawing to
  the surface will not affect the surface but will instead trigger a
  @code{:surface-finished} error.

  When the last call to the @fun{cairo:surface-destroy} function decreases the
  reference count to zero, Cairo will call the @fun{cairo:surface-finish}
  function if it has not been called already, before freeing the resources
  associated with the surface.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-finish}
  @see-function{cairo:surface-destroy}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-finish)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_flush
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_flush" surface-flush) :void
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{short}
    Do any pending drawing for the surface and also restore any temporary
    modifications Cairo has made to the state of the surface.
  @end{short}
  This function must be called before switching from drawing on the surface
  with Cairo to drawing on it directly with native APIs. If the surface does not
  support direct access, then this function does nothing.
  @see-symbol{cairo:surface-t}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-flush)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_device
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_get_device" %surface-device)
    (:pointer (:struct device-t))
  (surface (:pointer (:struct surface-t))))

(defun surface-device (surface)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @return{The @symbol{cairo:device-t} instance with the device for @arg{surface}
    or @code{nil} if the surface does not have an associated device.}
  @begin{short}
    This function returns the device for a surface.
  @end{short}
  See the @symbol{cairo:device-t} documentation.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:device-t}"
  (let (device)
    (when (not (cffi:null-pointer-p (setf device (%surface-device surface))))
      device)))

(export 'surface-device)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_font_options
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_get_font_options" %surface-font-options) :void
  (surface (:pointer (:struct surface-t)))
  (options (:pointer (:struct font-options-t))))

(defun surface-font-options (surface options)
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[options]{a @symbol{cairo:font-options-t} instance into which to
    store the retrieved options, all existing values are overwritten}
  @return{The @symbol{cairo:font-options-t} instance with the retrieved
    options.}
  @begin{short}
    Retrieves the default font rendering options for the surface.
  @end{short}
  This allows display surfaces to report the correct subpixel order for
  rendering on them, print surfaces to disable hinting of metrics and so forth.
  The result can then be used with the @fun{cairo:scaled-font-create} function.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:scaled-font-create}"
  (%surface-font-options surface options)
  options)

(export 'surface-font-options)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_content
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_get_content" surface-content) content-t
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @return{The @symbol{cairo:content-t} value with the content type of
    @arg{surface}.}
  @begin{short}
    This function returns the content type of @arg{surface} which indicates
    whether the surface contains color and/or alpha information.
  @end{short}
  See the @symbol{cairo:content-t} enumeration.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:content-t}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-content)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_mark_dirty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_mark_dirty" surface-mark-dirty) :void
 #+liber-documentation
 "@version{#2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{short}
    Tells Cairo that drawing has been done to @arg{surface} using means other
    than Cairo, and that Cairo should reread any cached areas.
  @end{short}
  Note that you must call the @fun{cairo:surface-flush} function before doing
  such drawing.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-flush}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-mark-dirty)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_mark_dirty_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_mark_dirty_rectangle"
               surface-mark-dirty-rectangle) :void
 #+liber-documentation
 "@version{#2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[x]{an integerfor the x coordinate of dirty rectangle}
  @argument[y]{an integer for the y coordinate of dirty rectangle}
  @argument[width]{an integer for the width of dirty rectangle}
  @argument[height]{an integer for the height of dirty rectangle}
  @begin{short}
    Like the @fun{cairo:surface-mark-dirty} function, but drawing has been done
    only to the specified rectangle, so that Cairo can retain cached contents
    for other parts of the surface.
  @end{short}
  Any cached clip set on the surface will be reset by this function, to make
  sure that future Cairo calls have the clip set that they expect.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-mark-dirty}"
  (surface (:pointer (:struct surface-t)))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'surface-mark-dirty-rectangle)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_set_device_offset
;;; cairo_surface_get_device_offset
;;; ----------------------------------------------------------------------------

(defun (setf surface-device-offset) (offset surface)
  (destructuring-bind (xoffset yoffset) offset
    (setf xoffset (coerce xoffset 'double-float))
    (setf yoffset (coerce yoffset 'double-float))
    (cffi:foreign-funcall "cairo_surface_set_device_offset"
                          (:pointer (:struct surface-t)) surface
                          :double xoffset
                          :double yoffset
                          :void)
    (values xoffset yoffset)))

(cffi:defcfun ("cairo_surface_get_device_offset" %surface-device-offset) :void
  (surface (:pointer (:struct surface-t)))
  (xoffset (:pointer :double))
  (yoffset (:pointer :double)))

(defun surface-device-offset (surface)
 #+liber-documentation
 "@version{2025-1-18}
  @syntax{(cairo:surface-device-offset surface) => xoffset, yoffset}
  @syntax{(setf (cairo:surface-device-offset surface) (list xoffset yoffset))}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[xoffset]{a number coerced to a double float for the offset in
    the x direction, in device units}
  @argument[yoffset]{a number coerced to a double float for the offset in
    the y direction, in device units}
  @begin{short}
    The @fun{cairo:surface-device-offset} functions returns the device offset.
  @end{short}
  The @setf{cairo:surface-device-offset} function sets an offset that is added
  to the device coordinates determined by the current transformation matrix CTM
  when drawing to @arg{surface}.

  One use case for this function is when we want to create a
  @symbol{cairo:surface-t} instance that redirects drawing for a portion of an
  onscreen surface to an offscreen surface in a way that is completely
  invisible to the user of the Cairo API. Setting a transformation via the
  @fun{cairo:translate} function is not sufficient to do this, since functions
  like the @fun{cairo:device-to-user} function will expose the hidden offset.

  Note that the offset affects drawing to the surface as well as using the
  surface in a source pattern.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:translate}
  @see-function{cairo:device-to-user}"
  (cffi:with-foreign-objects ((xoffset :double) (yoffset :double))
    (%surface-device-offset surface xoffset yoffset)
    (values (cffi:mem-ref xoffset :double)
            (cffi:mem-ref yoffset :double))))

(export 'surface-device-offset)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_device_scale
;;; cairo_surface_set_device_scale
;;; ----------------------------------------------------------------------------

(defun (setf surface-device-scale) (scale surface)
  (destructuring-bind (xscale yscale) scale
    (setf xscale (coerce xscale 'double-float))
    (setf yscale (coerce yscale 'double-float))
    (cffi:foreign-funcall "cairo_surface_set_device_scale"
                          (:pointer (:struct surface-t)) surface
                          :double xscale
                          :double yscale
                          :void)
    (values xscale yscale)))

(cffi:defcfun ("cairo_surface_get_device_scale" %surface-device-scale) :void
  (surface (:pointer (:struct surface-t)))
  (xscale (:pointer :double))
  (yscale (:pointer :double)))

(defun surface-device-scale (surface)
 #+liber-documentation
 "@version{2025-1-18}
  @syntax{(cairo:surface-device-scale surface) => xscale, yscale}
  @syntax{(setf (cairo:surface-device-scale surface) (list xscale yscale))}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[xscale]{a number coerced to a double float for the scale in
    the x direction, in device units}
  @argument[yscale]{a number coerced to a double float for the scale in
    the y direction, in device units}
  @begin{short}
    The @fun{cairo:surface-device-scale} function returns the device scale.
  @end{short}
  The @setf{cairo:surface-device-scale} function sets a scale that is multiplied
  to the device coordinates determined by the current transformation matrix CTM
  when drawing to @arg{surface}.

  One common use for this is to render to very high resolution display devices
  at a scale factor, so that code that assumes 1 pixel will be a certain size
  will still work. Setting a transformation via the @fun{cairo:scale} function
  is not sufficient to do this, since functions like the
  @fun{cairo:device-to-user} function will expose the hidden scale.

  Note that the scale affects drawing to the surface as well as using the
  surface in a source pattern.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:scale}
  @see-function{cairo:device-to-user}"
  (cffi:with-foreign-objects ((xscale :double) (yscale :double))
    (%surface-device-scale surface xscale yscale)
    (values (cffi:mem-ref xscale :double)
            (cffi:mem-ref yscale :double))))

(export 'surface-device-scale)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_fallback_resolution
;;; cairo_surface_set_fallback_resolution
;;; ----------------------------------------------------------------------------

(defun (setf surface-fallback-resolution) (pixels surface)
  (destructuring-bind (xpixels ypixels) pixels
    (setf xpixels (coerce xpixels 'double-float))
    (setf ypixels (coerce ypixels 'double-float))
    (cffi:foreign-funcall "cairo_surface_set_fallback_resolution"
                          (:pointer (:struct surface-t)) surface
                          :double xpixels
                          :double ypixels
                          :void)
    (values xpixels ypixels)))

(cffi:defcfun ("cairo_surface_get_fallback_resolution"
               %surface-fallback-resolution) :void
  (surface (:pointer (:struct surface-t)))
  (xpixels (:pointer :double))
  (ypixels (:pointer :double)))

(defun surface-fallback-resolution (surface)
 #+liber-documentation
 "@version{2025-1-18}
  @syntax{(cairo:surface-fallback-resolution surface) => xpixels, ypixels}
  @syntax{(setf (cairo:surface-fallback-resolution surface)
    (list xpixels ypixels))}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[xpixels]{a number coerced to a double float for the horizontal
    pixels per inch}
  @argument[ypixels]{a number coerced to a double float for the vertical
    pixels per inch}
  @begin{short}
    The @fun{cairo:surface-fallback-resolution} function returns the fallback
    resolution, or default fallback resolution if never set.
  @end{short}
  The @setf{cairo:surface-fallback-resolution} function sets the horizontal and
  vertical resolution for image fallbacks.

  When certain operations are not supported natively by a backend, Cairo will
  fallback by rendering operations to an image and then overlaying that image
  onto the output. For backends that are natively vector-oriented, this function
  can be used to set the resolution used for these image fallbacks. Larger
  values will result in more detailed images, but also larger file sizes.

  Some examples of natively vector-oriented backends are the ps, pdf, and svg
  backends.

  For backends that are natively raster-oriented, image fallbacks are still
  possible, but they are always performed at the native device resolution. So
  this function has no effect on those backends.

  The default fallback resoultion is 300 pixels per inch in both dimensions.
  @begin[Notes]{dictionary}
    The fallback resolution only takes effect at the time of completing a
    page with the @fun{cairo:show-page} or @fun{cairo:copy-page} functions so
    there is currently no way to have more than one fallback resolution in
    effect on a single page.
  @end{dictionary}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:show-page}
  @see-function{cairo:copy-page}"
  (cffi:with-foreign-objects ((xscale :double) (yscale :double))
    (%surface-fallback-resolution surface xscale yscale)
    (values (cffi:mem-ref xscale :double)
            (cffi:mem-ref yscale :double))))

(export 'surface-fallback-resolution)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_set_user_data
;;;
;;; Attach user data to surface.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_user_data
;;;
;;; Return user data previously attached to surface using the specified key.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_copy_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_copy_page" surface-copy-page) :void
 #+liber-documentation
 "@version{#2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{short}
    Emits the current page for backends that support multiple pages, but does
    not clear it, so that the contents of the current page will be retained for
    the next page.
  @end{short}
  Use the @fun{cairo:surface-show-page} function if you want to get an empty
  page after the emission.

  There is a convenience function for this that takes a @symbol{cairo:context-t}
  context, namely the @fun{cairo:copy-page} function.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:context-t}
  @see-function{cairo:surface-show-page}
  @see-function{cairo:copy-page}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-copy-page)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_show_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_surface_show_page" surface-show-page) :void
 #+liber-documentation
 "@version{2025-1-18}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{short}
    Emits and clears the current page for backends that support multiple pages.
  @end{short}
  Use the @fun{cairo:surface-copy-page} function if you do not want to clear
  the page.

  There is a convenience function that takes a @symbol{cairo:context-t}
  instance, namely the @fun{cairo:show-page} function.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-copy-page}
  @see-function{cairo:show-page}"
  (surface (:pointer (:struct surface-t))))

(export 'surface-show-page)

;;; ----------------------------------------------------------------------------
;;; cairo_surface_has_show_text_glyphs ()
;;;
;;; cairo_bool_t cairo_surface_has_show_text_glyphs (cairo_surface_t *surface);
;;;
;;; Returns whether the surface supports sophisticated cairo_show_text_glyphs()
;;; operations. That is, whether it actually uses the provided text and cluster
;;; data to a cairo_show_text_glyphs() call.
;;;
;;; Note: Even if this function returns FALSE, a cairo_show_text_glyphs()
;;; operation targeted at surface will still succeed. It just will act like a
;;; cairo_show_glyphs() operation. Users can use this function to avoid
;;; computing UTF-8 text and cluster mapping if the target surface does not use
;;; it.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; Returns :
;;;     TRUE if surface supports cairo_show_text_glyphs(), FALSE otherwise
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_set_mime_data ()
;;;
;;; cairo_status_t cairo_surface_set_mime_data (cairo_surface_t *surface,
;;;                                             const char *mime_type,
;;;                                             const unsigned char *data,
;;;                                             unsigned long  length,
;;;                                             cairo_destroy_func_t destroy,
;;;                                             void *closure);
;;;
;;; Attach an image in the format mime_type to surface. To remove the data from
;;; a surface, call this function with same mime type and NULL for data.
;;;
;;; The attached image (or filename) data can later be used by backends which
;;; support it (currently: PDF, PS, SVG and Win32 Printing surfaces) to emit
;;; this data instead of making a snapshot of the surface. This approach tends
;;; to be faster and requires less memory and disk space.
;;;
;;; The recognized MIME types are the following: CAIRO_MIME_TYPE_JPEG,
;;; CAIRO_MIME_TYPE_PNG, CAIRO_MIME_TYPE_JP2, CAIRO_MIME_TYPE_URI.
;;;
;;; See corresponding backend surface docs for details about which MIME types
;;; it can handle. Caution: the associated MIME data will be discarded if you
;;; draw on the surface afterwards. Use this function with care.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; mime_type :
;;;     the MIME type of the image data
;;;
;;; data :
;;;     the image data to attach to the surface
;;;
;;; length :
;;;     the length of the image data
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the surface is
;;;     destroyed or when new image data is attached using the same mime type.
;;;
;;; closure :
;;;     the data to be passed to the destroy notifier
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_get_mime_data ()
;;;
;;; void cairo_surface_get_mime_data (cairo_surface_t *surface,
;;;                                   const char *mime_type,
;;;                                   const unsigned char **data,
;;;                                   unsigned long *length);
;;;
;;; Return mime data previously attached to surface using the specified mime
;;; type. If no data has been attached with the given mime type, data is set
;;; NULL.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; mime_type :
;;;     the mime type of the image data
;;;
;;; data :
;;;     the image data to attached to the surface
;;;
;;; length :
;;;     the length of the image data
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_supports_mime_type ()
;;;
;;; cairo_bool_t cairo_surface_supports_mime_type (cairo_surface_t *surface,
;;;                                                const char *mime_type);
;;;
;;; Return whether surface supports mime_type.
;;;
;;; surface :
;;;     a cairo_surface_t
;;;
;;; mime_type :
;;;     the mime type
;;;
;;; Returns :
;;;     TRUE if surface supports mime_type, FALSE otherwise
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_map_to_image ()
;;;
;;; cairo_surface_t * cairo_surface_map_to_image
;;;                                      (cairo_surface_t *surface,
;;;                                       const cairo_rectangle_int_t *extents);
;;;
;;; Returns an image surface that is the most efficient mechanism for modifying
;;; the backing store of the target surface. The region retrieved may be limited
;;; to the extents or NULL for the whole surface
;;;
;;; Note, the use of the original surface as a target or source whilst it is
;;; mapped is undefined. The result of mapping the surface multiple times is
;;; undefined. Calling cairo_surface_destroy() or cairo_surface_finish() on the
;;; resulting image surface results in undefined behavior.
;;;
;;; surface :
;;;     an existing surface used to extract the image from
;;;
;;; extents :
;;;     limit the extraction to an rectangular region
;;;
;;; Returns :
;;;     a pointer to the newly allocated image surface. The caller must use
;;;     cairo_surface_unmap_image() to destroy this image surface. This function
;;;     always returns a valid pointer, but it will return a pointer to a "nil"
;;;     surface if other is already in an error state or any other error occurs.
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_surface_unmap_image ()
;;;
;;; void cairo_surface_unmap_image (cairo_surface_t *surface,
;;;                                 cairo_surface_t *image);
;;;
;;; Unmaps the image surface as returned from #cairo_surface_map_to_image().
;;;
;;; The content of the image will be uploaded to the target surface. Afterwards,
;;; the image is destroyed.
;;;
;;; Using an image surface which wasn't returned by cairo_surface_map_to_image()
;;; results in undefined behavior.
;;;
;;; surface :
;;;     the surface passed to cairo_surface_map_to_image().
;;;
;;; image :
;;;     the currently mapped image
;;;
;;; Since 1.12
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.surface.lisp -----------------------------------------
