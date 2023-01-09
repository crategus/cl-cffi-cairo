;;; ----------------------------------------------------------------------------
;;; cairo.package.lisp
;;;
;;; The documentation of this file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(defpackage :cairo
  (:use :common-lisp)
  (:import-from :cffi #:defcfun
                      #:defctype
                      #:defcstruct
                      #:defcenum
                      #:defbitfield
                      #:with-foreign-object
                      #:with-foreign-objects
                      #:with-foreign-slots)
  (:shadow #:fill))

#+liber-documentation
(setf (documentation (find-package :cairo) t)
 "Cairo is a software library used to provide a vector graphics-based,
  device-independent API for software developers. It is designed to provide
  primitives for 2-dimensional drawing across a number of different backends.
  Cairo is designed to use hardware acceleration when available.

  This is the API documentation of a Lisp binding to Cairo.
  @begin[Drawing]{section}
    @begin[The Cairo drawing context]{subsection}
      The @symbol{cairo:context-t} structure is the main object used when
      drawing with Cairo. To draw with Cairo, you create a
      @symbol{cairo:context-t} instance, set the target surface, and drawing
      options for the @symbol{cairo:context-t} instance, create shapes with
      functions like the @fun{cairo:path-move-to} and @fun{cairo:path-line-to}
      functions, and then draw shapes with the @fun{cairo:stroke} or
      @fun{cairo:fill} functions.

      The @symbol{cairo:context-t} instance can be pushed to a stack via the
      @fun{cairo:save} function. They may then safely be changed, without
      losing the current state. Use the @fun{cairo:restore} function to restore
      to the saved state.
      @about-symbol{antialias-t}
      @about-symbol{fill-rule-t}
      @about-symbol{line-cap-t}
      @about-symbol{line-join-t}
      @about-symbol{operator-t}
      @about-symbol{rectangle-t}
      @about-symbol{rectangle-list-t}
      @about-symbol{context-t}
      @about-macro{with-cairo-context}
      @about-function{create}
      @about-function{reference}
      @about-function{destroy}
      @about-function{status}
      @about-function{save}
      @about-function{restore}
      @about-function{target}
      @about-function{push-group}
      @about-function{push-group-with-content}
      @about-function{pop-group}
      @about-function{pop-group-to-source}
      @about-function{group-target}
      @about-function{set-source-rgb}
      @about-function{set-source-rgba}
      @about-function{source}
      @about-function{set-source-surface}
      @about-function{antialias}
      @about-function{dash}
      @about-function{dash-count}
      @about-function{fill-rule}
      @about-function{line-cap}
      @about-function{line-join}
      @about-function{line-width}
      @about-function{miter-limit}
      @about-function{operator}
      @about-function{tolerance}
      @about-function{clip}
      @about-function{clip-preserve}
      @about-function{clip-extents}
      @about-function{in-clip}
      @about-function{reset-clip}
      @about-function{rectangle-list-destroy}
      @about-function{copy-clip-rectangle-list}
      @about-function{fill}
      @about-function{fill-preserve}
      @about-function{fill-extents}
      @about-function{in-fill}
      @about-function{mask}
      @about-function{mask-surface}
      @about-function{paint}
      @about-function{paint-with-alpha}
      @about-function{stroke}
      @about-function{stroke-preserve}
      @about-function{stroke-extents}
      @about-function{in-stroke}
      @about-function{copy-page}
      @about-function{show-page}
      @about-function{reference-count}
      @about-function{user-data}
    @end{subsection}
    @begin[Paths]{subsection}
      Creating paths and manipulating path data. Paths are the most basic
      drawing tools and are primarily used to implicitly generate simple masks.
      @about-symbol{path-data-type-t}
      @about-symbol{path-data-t}
      @about-function{path-data-header}
      @about-function{path-data-point}
      @about-function{header-data-type}
      @about-function{header-length}
      @about-symbol{path-t}
      @about-function{path-status}
      @about-function{path-data}
      @about-function{path-numdata}
      @about-function{copy-path}
      @about-function{copy-path-flat}
      @about-function{path-destroy}
      @about-function{append-path}
      @about-function{has-current-point}
      @about-function{current-point}
      @about-function{new-path}
      @about-function{new-sub-path}
      @about-function{close-path}
      @about-function{arc}
      @about-function{arc-negative}
      @about-function{curve-to}
      @about-function{line-to}
      @about-function{move-to}
      @about-function{rectangle}
      @about-function{glyph-path}
      @about-function{text-path}
      @about-function{rel-curve-to}
      @about-function{rel-line-to}
      @about-function{rel-move-to}
      @about-function{path-extents}
    @end{subsection}
    @begin[Pattern]{subsection}
      The @symbol{cairo:pattern-t} structure is the paint with which Cairo
      draws. The primary use of patterns is as the source for all Cairo drawing
      operations, although they can also be used as masks, that is, as the brush
      too. A Cairo pattern is created by using one of the many constructors, of
      the form @sym{cairo:pattern-create-type} or implicitly through the
      @sym{cairo:set-source-type} functions.
      @about-symbol{pattern-t}
      @about-symbol{extend-t}
      @about-symbol{filter-t}
      @about-symbol{pattern-type-t}
      @about-function{pattern-add-color-stop-rgb}
      @about-function{pattern-add-color-stop-rgba}
      @about-function{pattern-color-stop-count}
      @about-function{pattern-color-stop-rgba}
      @about-function{pattern-create-rgb}
      @about-function{pattern-create-rgba}
      @about-function{pattern-rgba}
      @about-function{pattern-create-for-surface}
      @about-function{pattern-surface}
      @about-function{pattern-create-linear}
      @about-function{pattern-linear-points}
      @about-function{pattern-create-radial}
      @about-function{pattern-radial-circles}
      @about-function{pattern-create-mesh}
      @about-function{mesh-pattern-begin-patch}
      @about-function{mesh-pattern-end-patch}
      @about-function{mesh-pattern-move-to}
      @about-function{mesh-pattern-line-to}
      @about-function{mesh-pattern-curve-to}
      @about-function{mesh-pattern-set-control-point}
      @about-function{mesh-pattern-set-corner-color-rgb}
      @about-function{mesh-pattern-set-corner-color-rgba}
      @about-function{mesh-pattern-patch-count}
      @about-function{mesh-pattern-path}
      @about-function{mesh-pattern-get-control-point}
      @about-function{mesh-pattern-get-corner-color-rgba}
      @about-function{pattern-reference}
      @about-function{pattern-destroy}
      @about-function{pattern-status}
      @about-function{pattern-extend}
      @about-function{pattern-filter}
      @about-function{pattern-matrix}
      @about-function{pattern-type}
      @about-function{pattern-reference-count}
      @about-function{pattern-user-data}
    @end{subsection}
    @begin[Regions]{subsection}
      Regions are a simple graphical data type representing an area of
      integer-aligned rectangles. They are often used on raster surfaces to
      track areas of interest, such as change or clip areas.
      @about-symbol{region-overlap-t}
      @about-symbol{region-t}
      @about-function{region-create}
      @about-function{region-create-rectangle}
      @about-function{region-create-rectangles}
      @about-function{region-copy}
      @about-function{region-reference}
      @about-function{region-destroy}
      @about-function{region-status}
      @about-function{region-extents}
      @about-function{region-num-rectangles}
      @about-function{region-rectangle}
      @about-function{region-is-empty}
      @about-function{region-contains-point}
      @about-function{region-contains-rectangle}
      @about-function{region-equal}
      @about-function{region-translate}
      @about-function{region-intersect}
      @about-function{region-intersect-rectangle}
      @about-function{region-subtract}
      @about-function{region-subtract-rectangle}
      @about-function{region-union}
      @about-function{region-union-rectangle}
      @about-function{region-xor}
      @about-function{region-xor-rectangle}
    @end{subsection}
    @begin[Transformations]{subsection}
      Manipulating the current transformation matrix. The current transformation
      matrix, CTM, is a two-dimensional affine transformation that maps all
      coordinates and other drawing instruments from the user space into the
      surface's canonical coordinate system, also known as the device space.
      @about-function{translate}
      @about-function{scale}
      @about-function{rotate}
      @about-function{transform}
      @about-function{matrix}
      @about-function{identity-matrix}
      @about-function{user-to-device}
      @about-function{user-to-device-distance}
      @about-function{device-to-user}
      @about-function{device-to-user-distance}
    @end{subsection}
    @begin[Text]{subsection}
      Rendering text and glyphs.

      The functions with text in their name form Cairo's toy text API. The toy
      API takes UTF-8 encoded text and is limited in its functionality to
      rendering simple left-to-right text with no advanced features. That means
      for example that most complex scripts like Hebrew, Arabic, and Indic
      scripts are out of question. No kerning or correct positioning of
      diacritical marks either. The font selection is pretty limited too and
      does not handle the case that the selected font does not cover the
      characters in the text. This set of functions are really that, a toy text
      API, for testing and demonstration purposes. Any serious application
      should avoid them.

      The functions with glyphs in their name form Cairo's low-level text API.
      The low-level API relies on the user to convert text to a set of glyph
      indexes and positions. This is a very hard problem and is best handled by
      external libraries, like the @code{pangocairo} library that is part of
      the Pango text layout and rendering library. Pango is available from the
      @url[http://www.pango.org/]{Pango library}.
      @about-symbol{glyph-t}
      @about-symbol{font-slant-t}
      @about-symbol{font-weight-t}
      @about-symbol{text-cluster-t}
      @about-symbol{text-cluster-flags-t}
      @about-function{select-font-face}
      @about-function{set-font-size}
      @about-function{set-font-matrix}
      @about-function{get-font-matrix}
      @about-function{font-options}
      @about-function{font-face}
      @about-function{scaled-font}
      @about-function{show-text}
      @about-function{show-glyphs}
      @about-function{show-text-glyphs}
      @about-function{font-extents}
      @about-function{text-extents}
      @about-function{glyph-extents}
      @about-macro{with-cairo-toy-font-face}
      @about-function{toy-font-face-create}
      @about-function{toy-font-face-family}
      @about-function{toy-font-face-slant}
      @about-function{toy-font-face-weight}
      @about-function{glyph-allocate}
      @about-function{glyph-free}
      @about-function{text-cluster-allocate}
      @about-function{text-cluster-free}
    @end{subsection}
    @begin[Raster Source]{subsection}
      Supplying arbitrary image data.
      @about-function{pattern-create-raster-source}
      @about-function{raster-source-pattern-set-callback-data}
      @about-function{raster-source-pattern-get-callback-data}
      @about-function{raster-source-pattern-set-acquire}
      @about-function{raster-source-pattern-get-acquire}
      @about-function{raster-source-pattern-set-snapshot}
      @about-function{raster-source-pattern-get-snapshot}
      @about-function{raster-source-pattern-set-copy}
      @about-function{raster-source-pattern-get-copy}
      @about-function{raster-source-pattern-set-finish}
      @about-function{raster-source-pattern-get-finish}
    @end{subsection}
    @begin[Tags and Links]{subsection}
      Hyperlinks and document structure.
      @see-symbol{CAIRO_TAG_DEST}
      @see-symbol{CAIRO_TAG_LINK}
      @see-function{tag-begin}
      @see-function{tag-end}
    @end{subsection}
  @end{section}
  @begin[Fonts]{section}
    @begin[Font Faces]{subsection}
      Base class for font faces. The @symbol{cairo:font-face-t} structure
      represents a particular font at a particular weight, slant, and other
      characteristic but no size, transformation, or size.

      Font faces are created using font-backend-specific constructors, typically
      of the form @code{cairo:backend-font-face-create}, or implicitly using the
      toy text API by way of the @fun{cairo:select-font-face} function. The
      resulting face can be accessed using the @fun{cairo:font-face} function.
      @about-symbol{font-face-t}
      @about-symbol{font-type-t}
      @about-function{font-face-reference}
      @about-function{font-face-destroy}
      @about-function{font-face-status}
      @about-function{font-face-type}
      @about-function{font-face-reference-count}
      @about-function{font-face-user-data}
    @end{subsection}
    @begin[Scaled Fonts]{subsection}
      Font face at particular size and options.

      The @symbol{cairo:scaled-font-t} structure represents a realization of a
      font face at a particular size and transformation and a certain set of
      font options.
      @about-symbol{font-extents-t}
      @about-function{font-extents-ascent}
      @about-function{font-extents-descent}
      @about-function{font-extents-height}
      @about-function{font-extents-max-x-advance}
      @about-function{font-extents-max-y-advance}
      @about-symbol{text-extents-t}
      @about-function{text-extents-x-bearing}
      @about-function{text-extents-y-bearing}
      @about-function{text-extents-width}
      @about-function{text-extents-height}
      @about-function{text-extents-x-advance}
      @about-function{text-extents-y-advance}
      @about-symbol{scaled-font-t}
      @about-function{scaled-font-create}
      @about-function{scaled-font-reference}
      @about-function{scaled-font-destroy}
      @about-function{scaled-font-status}
      @about-function{scaled-font-extents}
      @about-function{scaled-font-text-extents}
      @about-function{scaled-font-glyph-extents}
      @about-function{scaled-font-text-to-glyphs}
      @about-function{scaled-font-font-face}
      @about-function{scaled-font-font-options}
      @about-function{scaled-font-font-matrix}
      @about-function{scaled-font-ctm}
      @about-function{scaled-font-scale-matrix}
      @about-function{scaled-font-type}
      @about-function{scaled-font-reference-count}
      @about-function{scaled-font-user-data}
    @end{subsection}
    @begin[Font Options]{subsection}
      How a font should be rendered.

      The font options specify how fonts should be rendered. Most of the time
      the font options implied by a surface are just right and do not need any
      changes, but for pixel-based targets tweaking font options may result in
      superior output on a particular display.
      @about-symbol{font-options-t}
      @about-function{font-options-create}
      @about-function{font-options-copy}
      @about-function{font-options-destroy}
      @about-function{font-options-status}
      @about-function{font-options-merge}
      @about-function{font-options-hash}
      @about-function{font-options-equal}
      @about-function{font-options-antialias}
      @about-symbol{subpixel-order-t}
      @about-function{font-options-subpixel-order}
      @about-symbol{hint-style-t}
      @about-function{font-options-hint-style}
      @about-symbol{hint-metrics-t}
      @about-function{font-options-hint-metrics}
      @about-function{font-options-variations}
    @end{subsection}
    @begin[FreeType Fonts]{subsection}
      Font support for FreeType
    @end{subsection}
    @begin[Win32 Fonts]{subsection}
      Font support for Microsoft Windows
    @end{subsection}
    @begin[Quartz Fonts]{subsection}
      Font support via CGFont on OS X
    @end{subsection}
    @begin[User Fonts]{subsection}
      Font support with font data provided by the user
    @end{subsection}
  @end{section}
  @begin[Surfaces]{section}
    @begin[cairo_device_t]{subsection}
        Interface to underlying rendering system.
        @about-symbol{device-t}
        @about-symbol{device-type-t}
        @about-function{device-reference}
        @about-function{device-destroy}
        @about-function{device-status}
        @about-function{device-finish}
        @about-function{device-flush}
        @about-function{device-type}
        @about-function{device-reference-count}
        @about-function{device-user-data}
        @about-function{device-acquire}
        @about-function{device-release}
        @about-function{device-observer-elapsed}
        @about-function{device-observer-fill-elapsed}
        @about-function{device-observer-glyphs-elapsed}
        @about-function{device-observer-mask-elapsed}
        @about-function{device-observer-paint-elapsed}
        @about-function{device-observer-print}
        @about-function{device-observer-stroke-elapsed}
    @end{subsection}
    @begin[Cairo surfaces]{subsection}
      Base class for surfaces.

      A @symbol{cairo:surface-t} structure is the abstract type representing all
      different drawing targets that cairo can render to. The actual drawings
      are performed using a Cairo context.

      A Cairo surface is created by using backend-specific constructors,
      typically of the form @code{cairo:backend-surface-create}.

      Most surface types allow accessing the surface without using Cairo
      functions. If you do this, keep in mind that it is mandatory that you call
      the @fun{cairo:surface-flush} function before reading from or writing to
      the surface and that you must use the @fun{cairo:surface-mark-dirty}
      function after modifying it.

      @b{Example 1.} Directly modifying an image surface
      @begin{pre}
 void
 modify_image_surface (cairo_surface_t *surface)
 {
   unsigned char *data;
   int width, height, stride;

   // flush to ensure all writing to the image was done
   cairo_surface_flush (surface);

   // modify the image
   data = cairo_image_surface_get_data (surface);
   width = cairo_image_surface_get_width (surface);
   height = cairo_image_surface_get_height (surface);
   stride = cairo_image_surface_get_stride (surface);
   modify_image_data (data, width, height, stride);

   // mark the image dirty so Cairo clears its caches.
   cairo_surface_mark_dirty (surface);
 @}
      @end{pre}
      Note that for other surface types it might be necessary to acquire the
      surface's device first. See the @fun{cairo:device-acquire} function for a
      discussion of devices.

      @about-symbol{CAIRO_HAS_MIME_SURFACE}
      @about-symbol{CAIRO_MIME_TYPE_JP2}
      @about-symbol{CAIRO_MIME_TYPE_JPEG}
      @about-symbol{CAIRO_MIME_TYPE_PNG}
      @about-symbol{CAIRO_MIME_TYPE_URI}
      @about-symbol{CAIRO_MIME_TYPE_UNIQUE_ID}
      @about-symbol{surface-t}
      @about-symbol{content-t}
      @about-symbol{surface-type-t}
      @about-function{surface-create-similar}
      @about-function{surface-create-similar-image}
      @about-function{surface-create-for-rectangle}
      @about-function{surface-reference}
      @about-function{surface-destroy}
      @about-function{surface-status}
      @about-function{surface-finish}
      @about-function{surface-flush}
      @about-function{surface-device}
      @about-function{surface-font-options}
      @about-function{surface-content}
      @about-function{surface-mark-dirty}
      @about-function{surface-mark-dirty-rectangle}
      @about-function{surface-set-device-offset}
      @about-function{surface-get-device-offset}
      @about-function{surfacedevice-scale}
      @about-function{surface-fallback-resolution}
      @about-function{surface-type}
      @about-function{surface-reference-count}
      @about-function{surface-user-data}
      @about-function{surface-copy-page}
      @about-function{surface-show-page}
      @about-function{surface-has-show-text-glyphs}
      @about-function{surface-mime-data}
      @about-function{surface-supports-mime-type}
      @about-function{surface-map-to-image}
      @about-function{surface-unmap-image}
    @end{subsection}
    @begin[Image Surfaces]{subsection}
      Rendering to memory buffers.

      Image surfaces provide the ability to render to memory buffers either
      allocated by Cairo or by the calling code. The supported image formats
      are those defined in the @symbol{cairo:format-t} enumeration.
      @about-symbol{format-t}
      @about-function{format-stride-for-width}
      @about-macro{with-cairo-image-surface}
      @about-macro{with-cairo-context-for-image-surface}
      @about-function{image-surface-create}
      @about-function{image-surface-create-for-data}
      @about-function{image-surface-data}
      @about-function{image-surface-format}
      @about-function{image-surface-width}
      @about-function{image-surface-height}
      @about-function{image-surface-stride}
    @end{subsection}
    @begin[PDF Surfaces]{subsection}
      The PDF surface is used to render Cairo graphics to Adobe PDF files and
      is a multi-page vector surface backend.

      The following mime types are supported: @code{CAIRO_MIME_TYPE_JPEG},
      @code{CAIRO_MIME_TYPE_JP2}, @code{CAIRO_MIME_TYPE_UNIQUE_ID},
      @code{CAIRO_MIME_TYPE_JBIG2}, @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL},
      @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID}, @code{CAIRO_MIME_TYPE_CCITT_FAX},
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS}.

      JBIG2 Images

      JBIG2 data in PDF must be in the embedded format as described in ISO/IEC
      11544. Image specific JBIG2 data must be in @code{CAIRO_MIME_TYPE_JBIG2}.
      Any global segments in the JBIG2 data (segments with page association
      field set to 0) must be in @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL}. The global
      data may be shared by multiple images. All images sharing the same global
      data must set @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID} to a unique
      identifier. At least one of the images must provide the global data using
      @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL}. The global data will only be embedded
      once and shared by all JBIG2 images with the same
      @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID}.

      CCITT Fax Images

      The @code{CAIRO_MIME_TYPE_CCITT_FAX} mime data requires a number of
      decoding parameters These parameters are specified using
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS}.

      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS} mime data must contain a string of
      the form \"param1=value1 param2=value2 ...\".

      Columns : [required] An integer specifying the width of the image in
      pixels.

      Rows : [required] An integer specifying the height of the image in scan
      lines.

      K : [optional] An integer identifying the encoding scheme used. < 0 is 2
      dimensional Group 4, = 0 is Group3 1 dimensional, > 0 is mixed 1 and 2
      dimensional encoding. Default is 0.

      EndOfLine : [optional] If true end-of-line bit patterns are present.
      Default is false.

      EncodedByteAlign : [optional] If true the end of line is padded with 0
      bits so the next line begins on a byte boundary. Default is false.

      EndOfBlock : [optional] If true the data contains an end-of-block pattern.
      Default is true.

      BlackIs1 : [optional] If true 1 bits are black pixels. Default is false.

      DamagedRowsBeforeError : [optional] An integer specifying the number of
      damages rows tolerated before an error occurs. Default is 0.

      Boolean values may be \"true\" or \"false\", or 1 or 0.

      These parameters are the same as the CCITTFaxDecode parameters in the
      PostScript Language Reference and Portable Document Format (PDF). Refer
      to these documents for further details.

      An example @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS} string is:
      \"Columns=10230 Rows=40000 K=1 EndOfLine=true EncodedByteAlign=1
      BlackIs1=false\"
      @about-symbol{CAIRO_HAS_PDF_SURFACE}
      @about-symbol{CAIRO_PDF_OUTLINE_ROOT}
      @about-symbol{pdf-outline-flags-t}
      @about-symbol{pdf-metadata-t}
      @about-symbol{pdf-version-t}
      @about-function{pdf-surface-create}
      @about-function{pdf-surface-create-for-stream}
      @about-function{pdf-surface-restrict-to-version}
      @about-function{pdf-versions}
      @about-function{pdf-version-to-string}
      @about-function{pdf-surface-set-size}
      @about-function{pdf-surface-add-outline}
      @about-function{pdf-surface-set-metadata}
      @about-function{pdf-surface-set-page-label}
      @about-function{pdf-surface-set-thumbnail-size}
    @end{subsection}
    @begin[PNG Support]{subsection}
      Reading and writing PNG images.

      The PNG functions allow reading PNG images into image surfaces, and
      writing any surface to a PNG file.

      It is a toy API. It only offers very simple support for reading and
      writing PNG files, which is sufficient for testing and demonstration
      purposes. Applications which need more control over the generated PNG
      file should access the pixel data directly, using the
      @fun{cairo:image-surface-data} function or a backend-specific access
      function, and process it with another library, e.g. GdkPixbuf or
      @code{libpng}.
      @about-symbol{CAIRO_HAS_PNG_FUNCTIONS}
      @about-function{image-surface-create-from-png}
      @about-function{image-surface-create-from-png-stream}
      @about-function{surface-write-to-png}
      @about-function{surface-write-to-png-stream}
    @end{subsection}
    @begin[PostScript Surfaces]{subsection}
      The PostScript surface is used to render cairo graphics to Adobe
      PostScript files and is a multi-page vector surface backend.

      The following mime types are supported: @code{CAIRO_MIME_TYPE_JPEG},
      @code{CAIRO_MIME_TYPE_UNIQUE_ID}, @code{CAIRO_MIME_TYPE_CCITT_FAX},
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS}, @code{CAIRO_MIME_TYPE_CCITT_FAX},
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS}, @code{CAIRO_MIME_TYPE_EPS},
      @code{CAIRO_MIME_TYPE_EPS_PARAMS}.

      Source surfaces used by the PostScript surface that have a
      @code{CAIRO_MIME_TYPE_UNIQUE_ID} mime type will be stored in PostScript
      printer memory for the duration of the print job.
      @code{CAIRO_MIME_TYPE_UNIQUE_ID} should only be used for small frequently
      used sources.

      The @code{CAIRO_MIME_TYPE_CCITT_FAX} and
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS} mime types are documented in
      CCITT Fax Images.

      Embedding EPS files

      Encapsulated PostScript files can be embedded in the PS output by setting
      the CAIRO_MIME_TYPE_EPS mime data on a surface to the EPS data and
      painting the surface. The EPS will be scaled and translated to the extents
      of the surface the EPS data is attached to.

      The CAIRO_MIME_TYPE_EPS mime type requires the
      @code{CAIRO_MIME_TYPE_EPS_PARAMS} mime data to also be provided in order
      to specify the embeddding parameters. @code{CAIRO_MIME_TYPE_EPS_PARAMS}
      mime data must contain a string of the form \"bbox=[llx lly urx ury]\"
      that specifies the bounding box (in PS coordinates) of the EPS graphics.
      The parameters are: lower left x, lower left y, upper right x, upper
      right y. Normally the bbox data is identical to the %%BoundingBox data in
      the EPS file.
      @about-symbol{ps-level-t}
      @about-function{ps-surface-create}
      @about-function{ps-surface-create-for-stream}
      @about-function{ps-surface-restrict-to-level}
      @about-function{ps-levels}
      @about-function{ps-level-to-string}
      @about-function{ps-surface-eps}
      @about-function{ps-surface-set-size}
      @about-function{ps-surface-dsc-begin-setup}
      @about-function{ps-surface-dsc-begin-page-setup}
      @about-function{ps-surface-dsc-comment}
    @end{subsection}
    @begin[Recording Surfaces]{subsection}
      Records all drawing operations
    @end{subsection}
    @begin[Win32 Surfaces]{subsection}
      Microsoft Windows surface support
    @end{subsection}
    @begin[SVG Surfaces]{subsection}
      The SVG surface is used to render Cairo graphics to SVG files and is a
      multi-page vector surface backend.
      @about-symbol{svg-version-t}
      @about-symbol{svg-unit-t}
      @about-function{svg-surface-create}
      @about-function{svg-surface-create-for-stream}
      @about-function{svg-surface-document-unit}
      @about-function{svg-surface-restrict-to-version}
      @about-function{svg-versions}
      @about-function{svg-version-to-string}
    @end{subsection}
    @begin[Quartz Surfaces]{subsection}
      Rendering to Quartz surfaces
    @end{subsection}
    @begin[XCB Surfaces]{subsection}
      X Window System rendering using the XCB library
    @end{subsection}
    @begin[XLib Surfaces]{subsection}
      X Window System rendering using XLib
    @end{subsection}
    @begin[XLib-XRender Backend]{subsection}
      X Window System rendering using the X Render extension
    @end{subsection}
    @begin[Script Surfaces]{subsection}
      The script surface provides the ability to render to a native script that
      matches the cairo drawing model. The scripts can be replayed using tools
      under the @file{util/cairo-script} directory, or with
      @code{cairo-perf-trace}.
      @about-symbol{script-mode-t}
      @about-function{script-create}
      @about-function{script-create-for-stream}
      @about-function{script-from-recording-surface}
      @about-function{script-mode}
      @about-function{script-surface-create}
      @about-function{script-surface-create-for-target}
      @about-function{script-write-comment}
    @end{subsection}
  @end{section}
  @begin[Utilities]{section}
    @begin[Generic matrix operations]{subsection}
      Generic matrix operations.
      @about-symbol{matrix-t}
      @about-function{matrix-init}
      @about-function{matrix-init-identity}
      @about-function{matrix-init-translate}
      @about-function{matrix-init-scale}
      @about-function{matrix-init-rotate}
      @about-function{matrix-to-float}
      @about-function{matrix-translate}
      @about-function{matrix-scale}
      @about-function{matrix-rotate}
      @about-function{matrix-invert}
      @about-function{matrix-multiply}
      @about-function{matrix-transform-distance}
      @about-function{matrix-transform-point}
    @end{subsection}
    @begin[Error handling]{subsection}
      Decoding Cairo's status.

      Cairo uses a single status type to represent all kinds of errors. A
      status value of @code{:success} represents no error and has an integer
      value of zero. All other status values represent an error.

      Cairo's error handling is designed to be easy to use and safe. All major
      Cairo objects retain an error status internally which can be queried
      anytime by the users using @code{cairo*-status} calls. In the mean time,
      it is safe to call all Cairo functions normally even if the underlying
      object is in an error status. This means that no error handling code is
      required before or after each individual Cairo function call.

      @about-symbol{status-t}
      @about-function{status-to-string}
      @about-function{debug-reset-static-data}
    @end{subsection}
    @begin[Version Information]{subsection}
      Cairo provides the ability to examine the version at either compile-time
      or run-time and in both a human readable form as well as an encoded form
      suitable for direct comparison. Cairo also provides the
      @fun{cairo:version-encode} function to perform the encoding.
      @about-function{version-encode}
      @about-function{version}
      @about-function{version-string}
    @end{subsection}
    @begin[Types]{subsection}
      This section lists generic data types used in the cairo API.
      @about-symbol{bool-t}
      @about-symbol{user-data-key-t}
      @about-symbol{rectangle-int-t}
    @end{subsection}
  @end{section}")

;;; --- End of file cairo.package.lisp -----------------------------------------
