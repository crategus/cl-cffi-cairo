;;; ----------------------------------------------------------------------------
;;; cairo.svg-surface.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 - 2023 Dieter Kaiser
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
;;; SVG Surfaces
;;;
;;;     Rendering SVG documents
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_SVG_SURFACE
;;;     cairo_svg_version_t
;;;     cairo_svg_unit_t
;;;
;;; Functions
;;;
;;;     cairo_svg_surface_create
;;;     cairo_svg_surface_create_for_stream
;;;     cairo_svg_surface_get_document_unit
;;;     cairo_svg_surface_set_document_unit
;;;     cairo_svg_surface_restrict_to_version
;;;     cairo_svg_get_versions
;;;     cairo_svg_version_to_string
;;;
;;; Description
;;;
;;; The SVG surface is used to render cairo graphics to SVG files and is a
;;; multi-page vector surface backend.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_SVG_SURFACE
;;;
;;; #define CAIRO_HAS_SVG_SURFACE 1
;;;
;;; Defined if the SVG surface backend is available. This macro can be used to
;;; conditionally compile backend-specific code.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_svg_version_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum svg-version-t
  :version-1-1
  :version-1-2)

#+liber-documentation
(setf (liber:alias-for-symbol 'svg-version-t)
      "CEnum"
      (liber:symbol-documentation 'svg-version-t)
 "@version{#2023-1-9}
  @begin{short}
    The @sym{cairo:svg-version-t} enumeration is used to describe the version
    number of the SVG specification that a generated SVG file will conform to.
  @end{short}
  @begin{pre}
(cffi:defcenum svg-version-t
  :version-1-1
  :version-1-2)
  @end{pre}
  @begin[code]{table}
    @entry[:version-1-1]{The version 1.4 of the SVG specification.}
    @entry[:version-1-2]{The version 1.5 of the SVG specification.}
  @end{table}
  @see-function{cairo:svg-version-to-string}")

(export 'svg-version-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_svg_unit_t
;;;
;;; CAIRO_SVG_UNIT_USER :
;;;     User unit, a value in the current coordinate system. If used in the root
;;;     element for the initial coordinate systems it corresponds to pixels.
;;;     (Since 1.16)
;;; CAIRO_SVG_UNIT_EM : The size of the element's font. (Since 1.16)
;;; CAIRO_SVG_UNIT_EX : The x-height of the element’s font. (Since 1.16)
;;; CAIRO_SVG_UNIT_PX : Pixels (1px = 1/96th of 1in). (Since 1.16)
;;; CAIRO_SVG_UNIT_IN : Inches (1in = 2.54cm = 96px). (Since 1.16)
;;; CAIRO_SVG_UNIT_CM : Centimeters (1cm = 96px/2.54). (Since 1.16)
;;; CAIRO_SVG_UNIT_MM : Millimeters (1mm = 1/10th of 1cm). (Since 1.16)
;;; CAIRO_SVG_UNIT_PT : Points (1pt = 1/72th of 1in). (Since 1.16)
;;; CAIRO_SVG_UNIT_PC : Picas (1pc = 1/6th of 1in). (Since 1.16)
;;; CAIRO_SVG_UNIT_PERCENT :
;;;     Percent, a value that is some fraction of another reference value.
;;;     (Since 1.16)
;;;
;;;
;;; CAIRO_SVG_UNIT_USER
;;; CAIRO_SVG_UNIT_EM
;;; CAIRO_SVG_UNIT_EX
;;; CAIRO_SVG_UNIT_PX
;;; CAIRO_SVG_UNIT_IN
;;; CAIRO_SVG_UNIT_CM
;;; CAIRO_SVG_UNIT_MM
;;; CAIRO_SVG_UNIT_PT
;;; CAIRO_SVG_UNIT_PC
;;; CAIRO_SVG_UNIT_PERCENT
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

(cffi:defcenum svg-unit-t
  :user
  :em
  :ex
  :px
  :in
  :cm
  :mm
  :pt
  :pc
  :percent)

#+liber-documentation
(setf (liber:alias-for-symbol 'svg-unit-t)
      "CEnum"
      (liber:symbol-documentation 'svg-unit-t)
 "@version{#2023-1-9}
  @begin{short}
    The @sym{cairo:svg-unit-t} enumeration is used to describe the units valid
    for coordinates and lengths in the SVG specification.
  @end{short}
  @begin{pre}
(cffi:defcenum svg-unit-t
  :user
  :em
  :ex
  :px
  :in
  :cm
  :mm
  :pt
  :pc
  :percent)
  @end{pre}
  @begin[code]{table}
    @entry[:user]{User unit, a value in the current coordinate system. If used
      in the root element for the initial coordinate systems it corresponds to
      pixels.}
    @entry[:em]{The size of the element's font.}
    @entry[:ex]{The x-height of the element’s font.}
    @entry[:px]{Pixels (1px = 1/96th of 1in).}
    @entry[:in]{Inches (1in = 2.54cm = 96px).}
    @entry[:cm]{Centimeters (1cm = 96px/2.54).}
    @entry[:mm]{Millimeters (1mm = 1/10th of 1cm).}
    @entry[:pt]{Points (1pt = 1/72th of 1in).}
    @entry[:pc]{Picas (1pc = 1/6th of 1in).}
    @entry[:percent]{Percent, a value that is some fraction of another
      reference value.}
  @end{table}
  @see-function{cairo:svg-surface-document-unit}")

(export 'svg-unit-t)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_create ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_svg_surface_create" %svg-surface-create)
    (:pointer (:struct surface-t))
  (path :string)
  (width :double)
  (height :double))

(defun svg-surface-create (path width height)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[path]{a namestring or pathname with a path for the SVG output (must
    be writable), @code{nil} may be used to specify no output, this will
    generate a SVG surface that may be queried and used as a source, without
    generating a temporary file}
  @argument[width]{a number coerced to a double float with the width of the
    surface, in points (1 point == 1/72.0 inch)}
  @argument[height]{a number coerced to a double float with the height of the
    surface, in points (1 point == 1/72.0 inch)}
  @return{A newly created @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid pointer, but it will return a
    pointer to a \"nil\" surface if an error such as out of memory occurs. You
    can use the @fun{cairo:surface-status} function to check for this.}
  @begin{short}
    Creates a SVG surface of the specified size in points to be written to
    @arg{path}.
  @end{short}
  The SVG surface backend recognizes the following MIME types for the data
  attached to a surface, see the @fun{cairo:surface-set-mime-data} function,
  when it is used as a source pattern for drawing on this surface:
  @code{CAIRO_MIME_TYPE_JPEG}, @code{CAIRO_MIME_TYPE_PNG}, and
  @code{CAIRO_MIME_TYPE_URI}. If any of them is specified, the SVG backend emits
  a href with the content of MIME data instead of a surface snapshot
  (PNG, Base64-encoded) in the corresponding image tag.

  The unofficial MIME type @code{CAIRO_MIME_TYPE_URI} is examined first. If
  present, the URI is emitted as is: assuring the correctness of URI is left to
  the client code.

  If @code{CAIRO_MIME_TYPE_URI} is not present, but @code{CAIRO_MIME_TYPE_JPEG}
  or @code{CAIRO_MIME_TYPE_PNG} is specified, the corresponding data is
  Base64-encoded and emitted.

  If @code{CAIRO_MIME_TYPE_UNIQUE_ID} is present, all surfaces with the same
  unique identifier will only be embedded once.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-status}"
  (%svg-surface-create (if path (namestring path) (cffi:null-pointer))
                       (coerce width 'double-float)
                       (coerce height 'double-float)))

(export 'svg-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_create_for_stream ()
;;;
;;; cairo_surface_t *
;;; cairo_svg_surface_create_for_stream (cairo_write_func_t write_func,
;;;                                      void *closure,
;;;                                      double width_in_points,
;;;                                      double height_in_points);
;;;
;;; Creates a SVG surface of the specified size in points to be written
;;; incrementally to the stream represented by write_func and closure .
;;;
;;; write_func :
;;;     a cairo_write_func_t to accept the output data, may be NULL to indicate
;;;     a no-op write_func . With a no-op write_func , the surface may be
;;;     queried or used as a source without generating any temporary files.
;;;
;;; closure :
;;;     the closure argument for write_func
;;;
;;; width_in_points :
;;;     width of the surface, in points (1 point == 1/72.0 inch)
;;;
;;; height_in_points :
;;;     height of the surface, in points (1 point == 1/72.0 inch)
;;;
;;; Returns :
;;;     a pointer to the newly created surface. The caller owns the surface and
;;;     should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a "nil" surface if an error such as out of memory occurs.
;;;     You can use cairo_surface_status() to check for this.
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_get_document_unit ()
;;;
;;; cairo_svg_unit_t
;;; cairo_svg_surface_get_document_unit (cairo_surface_t *surface);
;;;
;;;
;;; surface :
;;;     a SVG cairo_surface_t
;;;
;;; Returns :
;;;     the SVG unit of the SVG surface.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_set_document_unit ()
;;;
;;; void
;;; cairo_svg_surface_set_document_unit (cairo_surface_t *surface,
;;;                                      cairo_svg_unit_t unit);
;;;
;;;
;;; surface :
;;;     a SVG cairo_surface_t
;;;
;;; unit :
;;;     SVG unit
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

(defun (setf svg-surface-document-unit) (unit surface)
  (cffi:foreign-funcall "cairo_svg_surface_set_document_unit"
                        (:pointer (:struct surface-t)) surface
                        svg-unit-t unit
                        :void)
  unit)

(cffi:defcfun ("cairo_svg_surface_get_document_unit"
               svg-surface-document-unit) svg-unit-t
 #+liber-documentation
 "@version{#2023-1-9}
  @syntax[]{(svg-surface-document-unit surface) => unit}
  @syntax[]{(setf (svg-surface-document-unit surface) unit)}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[unit]{a @symbol{cairo:svg-unit-t} value}
  @begin{short}
    The @sym{cairo:svg-surface-document-unit} function gets the unit of the SVG
    surface.
  @end{short}
  If the surface passed as an argument is not a SVG surface, the function sets
  the error status to @code{CAIRO_STATUS_SURFACE_TYPE_MISMATCH} and returns
  @code{CAIRO_SVG_UNIT_USER}.

  The @sym{(setf cairo:svg-surface-document-unit)} function sets the unit for
  use with the specified width and height of the generated SVG file. See the
  @symbol{cairo:svg-unit-t} enumeration for a list of available unit values that
  can be used here.

  This function can be called at any time before generating the SVG file.

  However to minimize the risk of ambiguities it is recommended to call it
  before any drawing operations have been performed on the given surface, to
  make it clearer what the unit used in the drawing operations is. The simplest
  way to do this is to call this function immediately after creating the SVG
  surface.

  Note if this function is never called, the default unit for SVG documents
  generated by cairo will be @code{:pt}. This is for historical reasons.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:svg-unit-t}"
  (surface (:pointer (:struct surface-t))))

(export 'svg-surface-document-unit)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_restrict_to_version ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_svg_surface_restrict_to_version"
               svg-surface-restrict-to-version) :void
 #+liber-documentation
 "@version{#2023-1-9}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[version]{a @symbol{cairo:svg-version-t} value}
  @begin{short}
    Restricts the generated SVG file to @arg{version}.
  @end{short}
  See the @fun{cairo:svg-versions} function for a list of available version
  values that can be used here. This function should only be called before any
  drawing operations have been performed on the given surface. The simplest way
  to do this is to call this function immediately after creating the surface.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:svg-version-t}
  @see-function{cairo:svg-versions}"
  (surface (:pointer (:struct surface-t)))
  (version svg-version-t))

(export 'svg-surface-restrict-to-version)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_get_versions ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_svg_get_versions" %svg-versions) :void
  (versions :pointer)
  (num :pointer))

(defun svg-versions ()
 #+liber-documentation
 "@version{#2023-1-9}
  @return{A list of @symbol{cairo:svg-version-t} values with the supported
    versions.}
  @begin{short}
    Used to retrieve the list of supported versions.
  @end{short}
  See the @fun{cairo:svg-surface-restrict-to-version} function.
  @see-symbol{cairo:svg-version-t}
  @see-function{cairo:svg-surface-restrict-to-version}"
  (cffi:with-foreign-objects ((ptr :pointer) (num :int))
    (%svg-versions ptr num)
    (loop with versions = (cffi:mem-ref ptr :pointer)
          for count from 0 below (cffi:mem-ref num :int)
          collect (cffi:mem-aref versions 'svg-version-t count))))

(export 'svg-versions)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_version_to_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_svg_version_to_string" svg-version-to-string) :string
 #+liber-documentation
 "@version{#2023-1-9}
  @argument[version]{a @symbol{cairo:svg-version-t} value}
  @return{A string with the given @arg{version}.}
  @begin{short}
    Gets the string representation of the given version ID.
  @end{short}
  This function will return @code{nil} if @arg{version} is not valid. See the
  @fun{cairo:svg-versions} functions for a way to get the list of valid version
  IDs.
  @see-symbol{cairo:svg-version-t}
  @see-function{cairo:svg-versions}"
  (version svg-version-t))

(export 'svg-version-to-string)

;;; --- End of file cairo.svg-surface.lisp -------------------------------------
