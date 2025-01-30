;;; ----------------------------------------------------------------------------
;;; cairo.pdf-surface.lisp
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
;;; PDF Surfaces
;;;
;;;     Rendering PDF documents
;;;
;;; Types and Values
;;;
;;;     CAIRO_HAS_PDF_SURFACE
;;;     CAIRO_PDF_OUTLINE_ROOT
;;;     cairo_pdf_outline_flags_t
;;;     cairo_pdf_metadata_t
;;;     cairo_pdf_version_t
;;;
;;; Functions
;;;
;;;     cairo_pdf_surface_create
;;;     cairo_pdf_surface_create_for_stream                 not implemented
;;;     cairo_pdf_surface_restrict_to_version
;;;     cairo_pdf_get_versions
;;;     cairo_pdf_version_to_string
;;;     cairo_pdf_surface_set_size
;;;     cairo_pdf_surface_add_outline
;;;     cairo_pdf_surface_set_metadata
;;;     cairo_pdf_surface_set_custom_metadata
;;;     cairo_pdf_surface_set_page_label
;;;     cairo_pdf_surface_set_thumbnail_size
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_outline_flags_t
;;; ----------------------------------------------------------------------------

(cffi:defbitfield pdf-outline-flags-t
  (:open 1)
  (:bold 2)
  (:italic 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'pdf-outline-flags-t)
      "Bitfield"
      (liber:symbol-documentation 'pdf-outline-flags-t)
 "@version{2025-1-13}
  @begin{declaration}
(cffi:defbitfield pdf-outline-flags-t
  (:open 1)
  (:bold 2)
  (:italic 4))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:open]{The outline item defaults to open in the PDF viewer.}
      @entry[:bold]{The outline item is displayed by the viewer in bold text.}
      @entry[:italic]{The outline item is displayed by the viewer in italic text.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:pdf-outline-flags-t} flags is used by the
    @fun{cairo:pdf-surface-add-outline} function to specify the attributes of
    an outline item.
  @end{short}
  @see-function{cairo:pdf-surface-add-outline}")

(export 'pdf-outline-flags-t)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_metadata_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum pdf-metadata-t
  :title
  :author
  :subject
  :keywords
  :creator
  :create-date
  :mod-date)

#+liber-documentation
(setf (liber:alias-for-symbol 'pdf-metadata-t)
      "CEnum"
      (liber:symbol-documentation 'pdf-metadata-t)
 "@version{2025-1-13}
  @begin{declaration}
(cffi:defcenum pdf-metadata-t
  :title
  :author
  :subject
  :keywords
  :creator
  :create-date
  :mod-date)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:title]{The document title.}
      @entry[:author]{The document author.}
      @entry[:subject]{The document subject.}
      @entry[:keywords]{The document keywords.}
      @entry[:creator]{The document creator.}
      @entry[:create-date]{The document creation date.}
      @entry[:mod-date]{The document modification date.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:pdf-metadata-t} enumeration is used by the
    @fun{cairo:pdf-surface-set-metadata} function to specify the metadata to
    set.
  @end{short}
  @see-function{cairo:pdf-surface-set-metadata}")

(export 'pdf-metadata-t)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_version_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum pdf-version-t
  :version-1-4
  :version-1-5
  #+cairo-1-18
  :version-1-6
  #+cairo-1-18
  :version-1-7)

#+liber-documentation
(setf (liber:alias-for-symbol 'pdf-version-t)
      "CEnum"
      (liber:symbol-documentation 'pdf-version-t)
 "@version{2025-1-13}
  @begin{declaration}
(cffi:defcenum pdf-version-t
  :version-1-4
  :version-1-5
  #+cairo-1-18
  :version-1-6
  #+cairo-1-18
  :version-1-7)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:version-1-4]{The version 1.4 of the PDF specification.}
      @entry[:version-1-5]{The version 1.5 of the PDF specification.}
      @entry[:version-1-6]{The version 1.6 of the PDF specification.}
      @entry[:version-1-7]{The version 1.7 of the PDF specification.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{cairo:pdf-version-t} enumeration is used to describe the version
    number of the PDF specification that a generated PDF file will conform to.
  @end{short}
  @see-function{cairo:pdf-version-to-string}")

(export 'pdf-version-t)

;;; ----------------------------------------------------------------------------
;;; cairo:with-pdf-surface
;;; ----------------------------------------------------------------------------

(defmacro with-pdf-surface ((surface path width height) &body body)
 #+liber-documentation
 "@version{2025-1-13}
  @syntax{(cairo:with-pdf-surface (surface path width height) body) => result}
  @argument[surface]{a PDF @symbol{cairo:surface-t} instance}
  @argument[path]{a path or namestring with a filename for the PDF output,
    @code{nil} may be used to specify no output, this will generate a PDF
    surface that may be queried and used as a source, without generating a
    temporary file}
  @argument[width]{a number coerced to a double float for the width of
    the surface, in points (1 point == 1/72 inch)}
  @argument[height]{a number coerced to a double float for the height of
    the surface, in points (1 point == 1/72 inch)}
  @begin{short}
    The @symbol{cairo:with-pdf-surface} macro allocates a new PDF
    @symbol{cairo:surface-t} instance with the given @arg{path}, @arg{width},
    and @arg{height} values and executes the body that uses the PDF surface.
  @end{short}
  After execution of the body the allocated memory for the PDF surface
  is released.
  @see-symbol{cairo:surface-t}"
  `(let ((,surface (pdf-surface-create ,path ,width ,height)))
     (unwind-protect
       (progn ,@body)
       (surface-destroy ,surface))))

(export 'with-pdf-surface)

;;; ----------------------------------------------------------------------------
;;; cairo:with-context-for-pdf-surface
;;; ----------------------------------------------------------------------------

(defmacro with-context-for-pdf-surface ((context path width height) &body body)
 #+liber-documentation
 "@version{2025-1-13}
  @syntax{(cairo:with-context-for-pdf-surface (context format width height)
    body) => result}
  @argument[context]{a @symbol{cairo:context-t} instance to create and
    initialize}
  @argument[path]{a path or namestring with a filename for the PDF output,
    @code{nil} may be used to specify no output, this will generate a PDF
    surface that may be queried and used as a source, without generating a
    temporary file}
  @argument[width]{a number coerced to a double float for the width of
    the surface, in points (1 point == 1/72 inch)}
  @argument[height]{a number coerced to a double float for the height of
    the surface, in points (1 point == 1/72 inch)}
  @begin{short}
    The @fun{cairo:with-context-for-pdf-surface} macro allocates a new
    @symbol{cairo:context-t} instance for a PDF surface, initializes the
    Cairo context with the @arg{path}, @arg{width}, and @arg{height} values
    and executes the body that uses the Cairo context.
  @end{short}
  After execution of the body the allocated memory for the Cairo context is
  released. See the documentation of the @fun{cairo:pdf-surface-create} and
  @fun{cairo:create} functions for more information about the initialization of
  the new Cairo context.
  @see-symbol{cairo:context-t}
  @see-function{cairo:create}
  @see-function{cairo:pdf-surface-create}"
  (let ((surface (gensym)))
    `(let ((,surface (pdf-surface-create ,path ,width ,height)))
       (with-context (,context ,surface)
         (progn
           (surface-destroy ,surface)
           ,@body)))))

(export 'with-context-for-pdf-surface)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_create
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_create" %pdf-surface-create)
    (:pointer (:struct surface-t))
  (filename :string)
  (width :double)
  (height :double))

(defun pdf-surface-create (path width height)
 #+liber-documentation
 "@version{2025-1-13}
  @argument[path]{a namestring or pathname with a path for the PDF output
    (must be writable), @code{nil} may be used to specify no output, this will
    generate a PDF surface that may be queried and used as a source, without
    generating a temporary file}
  @argument[width]{a double float for the width of the surface, in
    points (1 point == 1/72.0 inch)}
  @argument[height]{a double float for the height of the surface, in
    points (1 point == 1/72.0 inch)}
  @begin{return}
    The newly created @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid pointer, but it will return a
    pointer to a \"nil\" surface if an error such as out of memory occurs. You
    can use the @fun{cairo:surface-status} function to check for this.
  @end{return}
  @begin{short}
    Creates a PDF surface of the specified size in points to be written to
    @arg{filename}.
  @end{short}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-status}"
  (%pdf-surface-create (if path (namestring path) (cffi:null-pointer))
                       (coerce width 'double-float)
                       (coerce height 'double-float)))

(export 'pdf-surface-create)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_create_for_stream
;;;
;;; Creates a PDF surface of the specified size in points to be written
;;; incrementally to the stream represented by write_func and closure .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_restrict_to_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_restrict_to_version"
               pdf-surface-restrict-to-version) :void
 #+liber-documentation
 "@version{2025-1-13}
  @argument[surface]{a PDF @symbol{cairo:surface-t} instance}
  @argument[version]{a @symbol{cairo:pdf-version-t} value}
  @begin{short}
    Restricts the generated PDF file to the given @arg{version}.
  @end{short}
  See the @fun{cairo:pdf-versions} function for a list of available version
  values that can be used here. This function should only be called before any
  drawing operations have been performed on the given surface. The simplest way
  to do this is to call this function immediately after creating the surface.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:pdf-version-t}"
  (surface (:pointer (:struct surface-t)))
  (version pdf-version-t))

(export 'pdf-surface-restrict-to-version)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_get_versions
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_get_versions" %pdf-versions) :void
  (versions :pointer)
  (num :pointer))

(defun pdf-versions ()
 #+liber-documentation
 "@version{2025-1-13}
  @return{The list of @symbol{cairo:pdf-version-t} values with the supported
    versions.}
  @begin{short}
    Used to retrieve the list of supported versions.
  @end{short}
  See the @fun{cairo:pdf-surface-restrict-to-version} function.
  @see-symbol{cairo:pdf-version-t}
  @see-function{cairo:pdf-surface-restrict-to-version}"
  (cffi:with-foreign-objects ((ptr :pointer) (num :int))
    (%pdf-versions ptr num)
    (iter (with versions = (cffi:mem-ref ptr :pointer))
          (for n from 0 below (cffi:mem-ref num :int))
          (collect (cffi:mem-aref versions 'pdf-version-t n)))))

(export 'pdf-versions)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_version_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_version_to_string" pdf-version-to-string) :string
 #+liber-documentation
 "@version{2025-1-13}
  @argument[version]{a @symbol{cairo:pdf-version-t} value}
  @return{The string with the given @arg{version}.}
  @begin{short}
    Gets the string representation of the given version ID.
  @end{short}
  This function will return @code{nil} if @arg{version} is not valid. See the
  @fun{cairo:pdf-versions} function for a way to get the list of valid version
  IDs.
  @see-symbol{cairo:pdf-version-t}
  @see-function{cairo:pdf-versions}"
  (version pdf-version-t))

(export 'pdf-version-to-string)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_size
;;; ----------------------------------------------------------------------------

(defun pdf-surface-set-size (surface width height)
 #+liber-documentation
 "@version{2025-1-13}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[width]{a number coerced to a double float for the new surface
    width, in points (1 point == 1/72.0 inch)}
  @argument[height]{a number coerced to a double float for the new surface
    height, in points (1 point == 1/72.0 inch)}
  @begin{short}
    Changes the size of a PDF surface for the current (and subsequent) pages.
  @end{short}
  This function should only be called before any drawing operations have been
  performed on the current page. The simplest way to do this is to call this
  function immediately after creating the surface or immediately after
  completing a page with either the @fun{cairo:show-page} or
  @fun{cairo:copy-page} functions.
  @see-symbol{cairo:surface-t}
  @see-function{cairo:show-page}
  @see-function{cairo:copy-page}"
  (cffi:foreign-funcall "cairo_pdf_surface_set_size"
                        (:pointer (:struct surface-t)) surface
                        :double (coerce width 'double-float)
                        :double (coerce height 'double-float)
                        :void))

(export 'pdf-surface-set-size)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_add_outline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_add_outline" pdf-surface-add-outline) :int
 #+liber-documentation
 "@version{2025-1-13}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[parent]{an integer for the ID of the parent item of 0 if this is a
    top level item}
  @argument[utf8]{a string for the name of the outline}
  @argument[link]{a string for the link attributes specifying where this
    outline links to}
  @argument[flags]{a @symbol{cairo:pdf-outline-flags-t} value for the outline
    flags}
  @return{The integer with the ID for the added item.}
  @begin{short}
    Add an item to the document outline hierarchy with the name @arg{utf8}
    that links to the location specified by @arg{link}.
  @end{short}
  Link attributes have the same keys and values as the Link Tag, excluding the
  \"rect\" attribute. The item will be a child of the item with ID
  @arg{parent}. Use the 0 value as the parent ID of toplevel items.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:pdf-outline-flags-t}"
  (surface (:pointer (:struct surface-t)))
  (parent :int)
  (utf8 :string)
  (link :string)
  (flags pdf-outline-flags-t))

(export 'pdf-surface-add-outline)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_metadata
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_set_metadata" pdf-surface-set-metadata) :void
 #+liber-documentation
 "@version{2025-1-13}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[metadata]{a @symbol{cairo:pdf-metadata-t} value with the metadata
    item to set}
  @argument[utf8]{a string for the metadata}
  @begin{short}
    Set document metadata.
  @end{short}
  The @code{:create-date} and @code{:mod-date} values must be in ISO-8601
  format: @code{YYYY-MM-DDThh:mm:ss}. An optional timezone of the form
  \"[+/-]hh:mm@}\" or \"Z\" for UTC time can be appended. All other metadata
  values can be any UTF-8 string.
  @begin[Examples]{dictionary}
     @begin{pre}
(cairo:pdf-surface-set-metadata surface :title \"My Document\")
(cairo:pdf-surface-set-metadata surface :create-datea \"2015-12-31T23:59+02:00\")
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:pdf-metadata-t}"
  (surface (:pointer (:struct surface-t)))
  (metadata pdf-metadata-t)
  (utf8 :string))

(export 'pdf-surface-set-metadata)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_custom_metadata
;;; ----------------------------------------------------------------------------

#+cairo-1-18
(cffi:defcfun ("cairo_pdf_surface_set_custom_metadata"
               pdf-surface-set-custom-metadata) :void
 #+liber-documentation
 "@version{#2025-1-13}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[name]{a string for the name of the custom metadata item to set
    (UTF8)}
  @argument[value]{a string for the value of the metadata (UTF8)}
  @begin{short}
    Set custom document metadata.
  @end{short}
  The @arg{name} argument may be any string except for the following names
  reserved by PDF: \"Title\", \"Author\", \"Subject\", \"Keywords\",
  \"Creator\", \"Producer\", \"CreationDate\", \"ModDate\", \"Trapped\".

  If the @arg{value} argument is @code{nil} or an empty string, the name
  metadata will not be set.
  @begin[Examples]{dictionary}
    @begin{pre}
(cairo:pdf-surface-set-custom-metadata surface \"ISBN\" \"978-0123456789\")
    @end{pre}
  @end{dictionary}
  Since 1.18
  @see-symbol{cairo:surface-t}"
  (surface (:pointer (:struct surface-t)))
  (name :string)
  (value :string))

#+cairo-1-18
(export 'pdf-surface-set-custom-metadata)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_page_label
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_set_page_label" pdf-surface-set-page-label)
    :void
 #+liber-documentation
 "@version{2025-1-13}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[utf8]{a string for the page label}
  @begin{short}
    Sets the page label for the current page.
  @end{short}
  @see-symbol{cairo:surface-t}"
  (surface (:pointer (:struct surface-t)))
  (utf8 :string))

(export 'pdf-surface-set-page-label)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_thumbnail_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_set_thumbnail_size"
               pdf-surface-set-thumbnail-size) :void
 #+liber-documentation
 "@version{2025-1-13}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[width]{an integer for the thumbnail width}
  @argument[height]{an integer for the thumbnail height}
  @begin{short}
    Set the thumbnail image size for the current and all subsequent pages.
  @end{short}
  Setting a width or height of 0 disables thumbnails for the current and
  subsequent pages.
  @see-symbol{cairo:surface-t}"
  (surface (:pointer (:struct surface-t)))
  (width :int)
  (height :int))

(export 'pdf-surface-set-thumbnail-size)

;;; --- End of file cairo.pdf-surface.lisp -------------------------------------
