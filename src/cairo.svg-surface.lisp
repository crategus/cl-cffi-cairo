;;; ----------------------------------------------------------------------------
;;; cairo.svg-surface.lisp
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
;;; SVG Surfaces
;;;
;;;     Rendering SVG documents
;;;
;;; Types and Values
;;;
;;;     cairo_svg_version_t
;;;     cairo_svg_unit_t
;;;
;;; Functions
;;;
;;;     cairo_svg_surface_create
;;;     cairo_svg_surface_create_for_stream                 not implemented
;;;     cairo_svg_surface_get_document_unit
;;;     cairo_svg_surface_set_document_unit
;;;     cairo_svg_surface_restrict_to_version
;;;     cairo_svg_get_versions
;;;     cairo_svg_version_to_string
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_version_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum svg-version-t
  :version-1-1
  :version-1-2)

#+liber-documentation
(setf (liber:alias-for-symbol 'svg-version-t)
      "CEnum"
      (liber:symbol-documentation 'svg-version-t)
 "@version{2025-09-01}
  @begin{declaration}
(cffi:defcenum svg-version-t
  :version-1-1
  :version-1-2)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:version-1-1]{The version 1.1 of the SVG specification.}
      @entry[:version-1-2]{The version 1.2 of the SVG specification.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{cairo:svg-version-t} enumeration is used to describe the version
    number of the SVG specification that a generated SVG file will conform to.
  @end{short}
  @see-function{cairo:svg-version-to-string}")

(export 'svg-version-t)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_unit_t
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
 "@version{2025-09-01}
  @begin{declaration}
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
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
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
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{cairo:svg-unit-t} enumeration is used to describe the units valid
    for coordinates and lengths in the SVG specification.
  @end{short}

  Since 1.16
  @see-function{cairo:svg-surface-document-unit}")

(export 'svg-unit-t)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_create
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_svg_surface_create" %svg-surface-create)
    (:pointer (:struct surface-t))
  (path :string)
  (width :double)
  (height :double))

(defun svg-surface-create (path width height)
 #+liber-documentation
 "@version{2025-09-01}
  @argument[path]{a namestring or pathname for a path for the SVG output (must
    be writable), @code{nil} may be used to specify no output, this will
    generate a SVG surface that may be queried and used as a source, without
    generating a temporary file}
  @argument[width]{a number coerced to a double float for the width of the
    surface, in points (1 point == 1/72.0 inch)}
  @argument[height]{a number coerced to a double float for the height of the
    surface, in points (1 point == 1/72.0 inch)}
  @begin{return}
    The newly created @sym{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid pointer, but it will return a
    pointer to a \"nil\" surface if an error such as out of memory occurs. You
    can use the @fun{cairo:surface-status} function to check for this.
  @end{return}
  @begin{short}
    Creates a SVG surface of the specified size in points to be written to
    @arg{path}.
  @end{short}
  The SVG surface backend recognizes the following MIME types for the data
  attached to a surface, see the @code{cairo_surface_set_mime_data()} function,
  when it is used as a source pattern for drawing on this surface:
  @code{CAIRO_MIME_TYPE_JPEG}, @code{CAIRO_MIME_TYPE_PNG}, and
  @code{CAIRO_MIME_TYPE_URI}. If any of them is specified, the SVG backend
  emits a href with the content of MIME data instead of a surface snapshot
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
;;; cairo_svg_surface_create_for_stream
;;;
;;; Creates a SVG surface of the specified size in points to be written
;;; incrementally to the stream represented by write_func and closure .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_svg_surface_get_document_unit
;;; cairo_svg_surface_set_document_unit
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
 "@version{#2025-09-02}
  @syntax{(cairo:svg-surface-document-unit surface) => unit}
  @syntax{(setf (cairo:svg-surface-document-unit surface) unit)}
  @argument[surface]{a @sym{cairo:surface-t} instance}
  @argument[unit]{a @sym{cairo:svg-unit-t} value}
  @begin{short}
    The @fun{cairo:svg-surface-document-unit} function gets the unit of the SVG
    surface.
  @end{short}
  If the surface passed as an argument is not a SVG surface, the function sets
  the error status to @code{CAIRO_STATUS_SURFACE_TYPE_MISMATCH} and returns
  @code{CAIRO_SVG_UNIT_USER}.

  The @setf{cairo:svg-surface-document-unit} function sets the unit for use
  with the specified width and height of the generated SVG file. See the
  @sym{cairo:svg-unit-t} enumeration for a list of available unit values that
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
;;; cairo_svg_surface_restrict_to_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_svg_surface_restrict_to_version"
               svg-surface-restrict-to-version) :void
 #+liber-documentation
 "@version{#2025-09-02}
  @argument[surface]{a @sym{cairo:surface-t} instance}
  @argument[version]{a @sym{cairo:svg-version-t} value}
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
;;; cairo_svg_get_versions
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_svg_get_versions" %svg-versions) :void
  (versions :pointer)
  (num :pointer))

(defun svg-versions ()
 #+liber-documentation
 "@version{#2025-09-01}
  @begin{return}
    The list of @sym{cairo:svg-version-t} values for the supported versions.
  @end{return}
  @begin{short}
    Used to retrieve the list of supported versions.
  @end{short}
  See the @fun{cairo:svg-surface-restrict-to-version} function.
  @see-symbol{cairo:svg-version-t}
  @see-function{cairo:svg-surface-restrict-to-version}"
  (cffi:with-foreign-objects ((ptr :pointer) (num :int))
    (%svg-versions ptr num)
    (iter (with versions = (cffi:mem-ref ptr :pointer))
          (for n from 0 below (cffi:mem-ref num :int))
          (collect (cffi:mem-aref versions 'svg-version-t n)))))

(export 'svg-versions)

;;; ----------------------------------------------------------------------------
;;; cairo_svg_version_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_svg_version_to_string" svg-version-to-string) :string
 #+liber-documentation
 "@version{#2025-09-02}
  @argument[version]{a @sym{cairo:svg-version-t} value}
  @return{The string for the given @arg{version}.}
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
