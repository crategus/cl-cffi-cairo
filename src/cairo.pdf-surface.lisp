;;; ----------------------------------------------------------------------------
;;; cairo.pdf-surface.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;     cairo_pdf_surface_create ()
;;;     cairo_pdf_surface_create_for_stream ()
;;;     cairo_pdf_surface_restrict_to_version ()
;;;     cairo_pdf_get_versions ()
;;;     cairo_pdf_version_to_string ()
;;;     cairo_pdf_surface_set_size ()
;;;     cairo_pdf_surface_add_outline ()
;;;     cairo_pdf_surface_set_metadata ()
;;;     cairo_pdf_surface_set_page_label ()
;;;     cairo_pdf_surface_set_thumbnail_size ()
;;;
;;; Description
;;;
;;; The PDF surface is used to render cairo graphics to Adobe PDF files and is
;;; a multi-page vector surface backend.
;;;
;;; The following mime types are supported: CAIRO_MIME_TYPE_JPEG,
;;; CAIRO_MIME_TYPE_JP2, CAIRO_MIME_TYPE_UNIQUE_ID, CAIRO_MIME_TYPE_JBIG2,
;;; CAIRO_MIME_TYPE_JBIG2_GLOBAL, CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID,
;;; CAIRO_MIME_TYPE_CCITT_FAX, CAIRO_MIME_TYPE_CCITT_FAX_PARAMS.
;;;
;;; JBIG2 Images
;;;
;;; JBIG2 data in PDF must be in the embedded format as described in ISO/IEC
;;; 11544. Image specific JBIG2 data must be in CAIRO_MIME_TYPE_JBIG2. Any
;;; global segments in the JBIG2 data (segments with page association field set
;;; to 0) must be in CAIRO_MIME_TYPE_JBIG2_GLOBAL. The global data may be shared
;;; by multiple images. All images sharing the same global data must set
;;; CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID to a unique identifier. At least one of the
;;; images must provide the global data using CAIRO_MIME_TYPE_JBIG2_GLOBAL. The
;;; global data will only be embedded once and shared by all JBIG2 images with
;;; the same CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID.
;;;
;;; CCITT Fax Images
;;;
;;; The CAIRO_MIME_TYPE_CCITT_FAX mime data requires a number of decoding
;;; parameters These parameters are specified using
;;; CAIRO_MIME_TYPE_CCITT_FAX_PARAMS.
;;;
;;; CAIRO_MIME_TYPE_CCITT_FAX_PARAMS mime data must contain a string of the form
;;; "param1=value1 param2=value2 ...".
;;;
;;; Columns : [required] An integer specifying the width of the image in pixels.
;;;
;;; Rows : [required] An integer specifying the height of the image in scan
;;; lines.
;;;
;;; K : [optional] An integer identifying the encoding scheme used. < 0 is 2
;;; dimensional Group 4, = 0 is Group3 1 dimensional, > 0 is mixed 1 and 2
;;; dimensional encoding. Default is 0.
;;;
;;; EndOfLine : [optional] If true end-of-line bit patterns are present. Default
;;; is false.
;;;
;;; EncodedByteAlign : [optional] If true the end of line is padded with 0 bits
;;; so the next line begins on a byte boundary. Default is false.
;;;
;;; EndOfBlock : [optional] If true the data contains an end-of-block pattern.
;;; Default is true.
;;;
;;; BlackIs1 : [optional] If true 1 bits are black pixels. Default is false.
;;;
;;; DamagedRowsBeforeError : [optional] An integer specifying the number of
;;; damages rows tolerated before an error occurs. Default is 0.
;;;
;;; Boolean values may be "true" or "false", or 1 or 0.
;;;
;;; These parameters are the same as the CCITTFaxDecode parameters in the
;;; PostScript Language Reference and Portable Document Format (PDF). Refer to
;;; these documents for further details.
;;;
;;; An example CAIRO_MIME_TYPE_CCITT_FAX_PARAMS string is:
;;;
;;; "Columns=10230 Rows=40000 K=1 EndOfLine=true EncodedByteAlign=1
;;; BlackIs1=false"
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; CAIRO_HAS_PDF_SURFACE
;;;
;;; #define CAIRO_HAS_PDF_SURFACE 1
;;;
;;; Defined if the PDF surface backend is available. This macro can be used to
;;; conditionally compile backend-specific code.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; CAIRO_PDF_OUTLINE_ROOT
;;;
;;; #define CAIRO_PDF_OUTLINE_ROOT 0
;;;
;;; The root outline item in cairo_pdf_surface_add_outline().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum cairo_pdf_outline_flags_t
;;; ----------------------------------------------------------------------------

(cffi:defbitfield pdf-outline-flags-t
  (:open 1)
  (:bold 2)
  (:italic 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'pdf-outline-flags-t)
      "Bitfield"
      (liber:symbol-documentation 'pdf-outline-flags-t)
 "@version{2023-1-9}
  @begin{short}
    The @sym{cairo:pdf-outline-flags-t} flags is used by the
    @fun{cairo:pdf-surface-add-outline} function to specify the attributes of
    an outline item.
  @end{short}
  These flags may be bitwise-or'd to produce any combination of flags.
  @begin{pre}
(cffi:defbitfield pdf-outline-flags-t
  (:open 1)
  (:bold 2)
  (:italic 4))
  @end{pre}
  @begin[code]{table}
    @entry[:open]{The outline item defaults to open in the PDF viewer.}
    @entry[:bold]{The outline item is displayed by the viewer in bold text.}
    @entry[:italic]{The outline item is displayed by the viewer in italic text.}
  @end{table}
  @see-function{cairo:pdf-surface-add-outline}")

(export 'pdf-outline-flags-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_pdf_metadata_t
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
 "@version{2023-1-9}
  @begin{short}
    The @sym{cairo:pdf-metadata-t} enumeration is used by the
    @fun{cairo:pdf-surface-set-metadata} function to specify the metadata to
    set.
  @end{short}
  @begin{pre}
(cffi:defcenum pdf-metadata-t
  :title
  :author
  :subject
  :keywords
  :creator
  :create-date
  :mod-date)
  @end{pre}
  @begin[code]{table}
    @entry[:title]{The document title.}
    @entry[:author]{The document author.}
    @entry[:subject]{The document subject.}
    @entry[:keywords]{The document keywords.}
    @entry[:creator]{The document creator.}
    @entry[:create-date]{The document creation date.}
    @entry[:mod-date]{The document modification date.}
  @end{table}
  @see-function{cairo:pdf-surface-set-metadata}")

(export 'pdf-metadata-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_pdf_version_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum pdf-version-t
  :version-1-4
  :version-1-5)

#+liber-documentation
(setf (liber:alias-for-symbol 'pdf-version-t)
      "CEnum"
      (liber:symbol-documentation 'pdf-version-t)
 "@version{2023-1-9}
  @begin{short}
    The @sym{cairo:pdf-version-t} enumeration is used to describe the version
    number of the PDF specification that a generated PDF file will conform to.
  @end{short}
  @begin{pre}
(cffi:defcenum pdf-version-t
  :version-1-4
  :version-1-5)
  @end{pre}
  @begin[code]{table}
    @entry[:version-1-4]{The version 1.4 of the PDF specification.}
    @entry[:version-1-5]{The version 1.5 of the PDF specification.}
  @end{table}
  @see-function{cairo:pdf-version-to-string}")

(export 'pdf-version-t)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_create ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_create" %pdf-surface-create)
    (:pointer (:struct surface-t))
  (filename :string)
  (width :double)
  (height :double))

(defun pdf-surface-create (path width height)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[path]{a namestring or pathname with a path for the PDF output
    (must be writable), @code{nil} may be used to specify no output, this will
    generate a PDF surface that may be queried and used as a source, without
    generating a temporary file}
  @argument[width]{a double float with the width of the surface, in points
    (1 point == 1/72.0 inch)}
  @argument[height]{a double float with the height of the surface, in points
    (1 point == 1/72.0 inch)}
  @begin{return}
    A pointer to the newly created surface. The caller owns the surface and
    should call the @fun{cairo:surface-destroy} function when done with it.
    This function always returns a valid pointer, but it will return a pointer
    to a \"nil\" surface if an error such as out of memory occurs. You can use
    the @fun{cairo:surface-status} function to check for this.
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
;;; cairo_pdf_surface_create_for_stream ()
;;;
;;; cairo_surface_t *
;;; cairo_pdf_surface_create_for_stream (cairo_write_func_t write_func,
;;;                                      void *closure,
;;;                                      double width_in_points,
;;;                                      double height_in_points);
;;;
;;; Creates a PDF surface of the specified size in points to be written
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
;;; cairo_pdf_surface_restrict_to_version ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_restrict_to_version"
               pdf-surface-restrict-to-version) :void
 #+liber-documentation
 "@version{2023-1-9}
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
;;; cairo_pdf_get_versions ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_get_versions" %pdf-versions) :void
  (versions :pointer)
  (num :pointer))

(defun pdf-versions ()
 #+liber-documentation
 "@version{2023-1-9}
  @return{A list of @symbol{cairo:pdf-version-t} values with the supported
    versions}
  @begin{short}
    Used to retrieve the list of supported versions.
  @end{short}
  See the @fun{cairo:pdf-surface-restrict-to-version} function.
  @see-symbol{cairo:pdf-version-t}
  @see-function{cairo:pdf-surface-restrict-to-version}"
  (cffi:with-foreign-objects ((ptr :pointer) (num :int))
    (%pdf-versions ptr num)
    (loop with versions = (cffi:mem-ref ptr :pointer)
          for count from 0 below (cffi:mem-ref num :int)
          collect (cffi:mem-aref versions 'pdf-version-t count))))

(export 'pdf-versions)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_version_to_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_version_to_string" pdf-version-to-string) :string
 #+liber-documentation
 "@version{2023-1-9}
  @argument[verion]{a @symbol{cairo:pdf-version-t} value}
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
;;; cairo_pdf_surface_set_size ()
;;; ----------------------------------------------------------------------------

(defun pdf-surface-set-size (surface width height)
 #+liber-documentation
 "@version{2023-1-9}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[width]{a number coerced to a double float with the new surface
    width, in points (1 point == 1/72.0 inch)}
  @argument[height]{a number coerced to a double float with the new surface
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
;;; cairo_pdf_surface_add_outline ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_add_outline" pdf-surface-add-outline) :int
 #+liber-documentation
 "@version{2023-1-9}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[parent]{an integer with the ID of the parent item of 0 if this is a
    top level item}
  @argument[utf8]{a string with the name of the outline}
  @argument[link]{a string with the link attributes specifying where this
    outline links to}
  @argument[flags]{a @symbol{cairo:pdf-outline-flgs-t} value with the outline
    flags}
  @return{An integer with the ID for the added item.}
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
;;; cairo_pdf_surface_set_metadata ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_set_metadata" pdf-surface-set-metadata) :void
 #+liber-documentation
 "@version{2023-1-9}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[metadata]{a @symbol{cairo:pdf-metadata-t} value with the metadata
    item to set}
  @argument[utf8]{a string with the metadata}
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
;;; cairo_pdf_surface_set_page_label ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_set_page_label" pdf-surface-set-page-label)
    :void
 #+liber-documentation
 "@version{2023-1-9}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[utf8]{a string with the page label}
  @begin{short}
    Sets the page label for the current page.
  @end{short}
  @see-symbol{cairo:surface-t}"
  (surface (:pointer (:struct surface-t)))
  (utf8 :string))

(export 'pdf-surface-set-page-label)

;;; ----------------------------------------------------------------------------
;;; cairo_pdf_surface_set_thumbnail_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_pdf_surface_set_thumbnail_size"
               pdf-surface-set-thumbnail-size) :void
 #+liber-documentation
 "@version{2023-1-9}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @argument[width]{an integer with the thumbnail width}
  @argument[height]{an integer with the thumbnail height}
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
