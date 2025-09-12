;;; ----------------------------------------------------------------------------
;;; cairo.package.lisp
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

(defpackage :cairo
  (:use :iterate :common-lisp)
  (:import-from :cffi)
  (:shadow #:fill))

(in-package :cairo)

#+sbcl
(when (and (find-package "SB-INT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :cairo) t)
 "Cairo is a software library used to provide a vector graphics-based,
  device-independent API for software developers. It is designed to provide
  primitives for 2-dimensional drawing across a number of different backends.
  Cairo is designed to use hardware acceleration when available. This is the API
  documentation of a Lisp binding to Cairo.
  @begin[Drawing]{section}
    @begin[The Cairo drawing context]{subsection}
      The @sym{cairo:context-t} structure is the main object used when drawing
      with Cairo. To draw with Cairo, you create a @sym{cairo:context-t}
      instance, set the target surface, and drawing options for the
      @sym{cairo:context-t} instance, create shapes with functions like the
      @fun{cairo:move-to} and @fun{cairo:line-to} functions, and then draw
      shapes with the @fun{cairo:stroke} or @fun{cairo:fill} functions.

      The @sym{cairo:context-t} instance can be pushed to a stack via the
      @fun{cairo:save} function. They may then safely be changed, without
      losing the current state. Use the @fun{cairo:restore} function to restore
      to the saved state.
    @end{subsection}
    @begin[Types and functions for Cairo drawing]{subsection}
      @about-symbol{antialias-t}
      @about-symbol{fill-rule-t}
      @about-symbol{line-cap-t}
      @about-symbol{line-join-t}
      @about-symbol{operator-t}
      @about-symbol{context-t}
      @about-macro{with-context}
      @about-function{create}
      @about-function{reference}
      @about-function{reference-count}
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
      @about-function{hairline}
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
      @about-function{user-data}
    @end{subsection}
    @begin[Paths]{subsection}
      @about-symbol{path-data-type-t}
      @about-symbol{path-data-t}
      @about-symbol{path-t}
      @about-function{path-status}
      @about-function{path-data}
      @about-function{path-numdata}
      @about-function{copy-path}
      @about-function{copy-path-flat}
      @about-function{path-destroy}
      @about-function{path-data-to-list}
      @about-function{append-path}
      @about-function{has-current-point}
      @about-function{current-point}
      @about-function{new-path}
      @about-function{new-sub-path}
      @about-function{close-path}
      @about-function{path-extents}
      @about-function{move-to}
      @about-function{rel-move-to}
      @about-function{line-to}
      @about-function{rel-line-to}
      @about-function{curve-to}
      @about-function{rel-curve-to}
      @about-function{rectangle}
      @about-function{arc}
      @about-function{arc-negative}
      @about-function{glyph-path}
      @about-function{text-path}
    @end{subsection}
    @begin[Introduction to pattern]{subsection}
      The @sym{cairo:pattern-t} structure is the paint with which Cairo draws.
      The primary use of patterns is as the source for all Cairo drawing
      operations, although they can also be used as masks, that is, as the brush
      too. A Cairo pattern is created by using one of the many constructors, of
      the form @sym{cairo:pattern-create-type} or implicitly through the
      @sym{cairo:set-source-type} functions.
    @end{subsection}
    @begin[Types and functions for pattern]{subsection}
      @about-symbol{extend-t}
      @about-symbol{filter-t}
      @about-symbol{pattern-type-t}
      @about-symbol{pattern-t}
      @about-function{pattern-reference}
      @about-function{pattern-reference-count}
      @about-function{pattern-destroy}
      @about-function{pattern-status}
      @about-function{pattern-type}
      @about-function{pattern-extend}
      @about-function{pattern-filter}
      @about-function{pattern-matrix}
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
      @about-function{mesh-pattern-control-point}
      @about-function{mesh-pattern-set-control-point}
      @about-function{mesh-pattern-corner-color-rgba}
      @about-function{mesh-pattern-set-corner-color-rgb}
      @about-function{mesh-pattern-set-corner-color-rgba}
      @about-function{mesh-pattern-patch-count}
      @about-function{mesh-pattern-path}
      @about-function{pattern-user-data}
    @end{subsection}
    @begin[Indroduction to Regions]{subsection}
      Regions are a simple graphical data type representing an area of
      integer aligned rectangles. They are often used on raster surfaces to
      track areas of interest, such as change or clip areas.
    @end{subsection}
    @begin[Types and functions for Regions]{subsection}
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
    @begin[Introduction to Transformations]{subsection}
      The current transformation matrix, CTM, is a two-dimensional affine
      transformation that maps all coordinates and other drawing instruments
      from the user space into the surface's canonical coordinate system, also
      known as the device space.
    @end{subsection}
    @begin[Functions for Transformations]{subsection}
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
    @begin[Introduction to Text]{subsection}
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
    @end{subsection}
    @begin[Types and functions for Text]{subsection}
      @about-symbol{font-slant-t}
      @about-symbol{font-weight-t}
      @about-symbol{text-cluster-t}
      @about-symbol{text-cluster-flags-t}
      @about-symbol{glyph-t}
      @about-function{select-font-face}
      @about-function{set-font-size}
      @about-function{font-matrix}
      @about-function{font-options}
      @about-function{font-face}
      @about-function{scaled-font}
      @about-function{show-text}
      @about-function{show-glyphs}
      @about-function{show-text-glyphs}
      @about-function{font-extents}
      @about-function{text-extents}
      @about-function{glyph-extents}
      @about-macro{with-toy-font-face}
      @about-function{toy-font-face-create}
      @about-function{toy-font-face-family}
      @about-function{toy-font-face-slant}
      @about-function{toy-font-face-weight}
      @about-function{glyph-allocate}
      @about-function{glyph-free}
      @about-function{text-cluster-allocate}
      @about-function{text-cluster-free}
    @end{subsection}
  @end{section}
  @begin[Fonts]{section}
    @begin[Indroduction to Font Faces]{subsection}
      Base class for font faces. The @sym{cairo:font-face-t} structure
      represents a particular font at a particular weight, slant, and other
      characteristic but no size, transformation, or size.

      Font faces are created using font-backend-specific constructors, typically
      of the form @code{cairo:backend-font-face-create}, or implicitly using the
      toy text API by way of the @fun{cairo:select-font-face} function. The
      resulting face can be accessed using the @fun{cairo:font-face} function.
    @end{subsection}
    @begin[Type and functions for Font Faces]{subsection}
      @about-symbol{font-type-t}
      @about-symbol{font-face-t}
      @about-function{font-face-reference}
      @about-function{font-face-reference-count}
      @about-function{font-face-destroy}
      @about-function{font-face-status}
      @about-function{font-face-type}
      @about-function{font-face-user-data}
    @end{subsection}
    @begin[Indroduction to Scaled Fonts]{subsection}
      The @sym{cairo:scaled-font-t} structure represents a realization of a font
      face at a particular size and transformation and a certain set of font
      options.
    @end{subsection}
    @begin[Types and functions for Scaled Fonts]{subsection}
      @about-symbol{font-extents-t}
      @about-symbol{text-extents-t}
      @about-symbol{scaled-font-t}
      @about-macro{with-scaled-font}
      @about-function{scaled-font-create}
      @about-function{scaled-font-reference}
      @about-function{scaled-font-reference-count}
      @about-function{scaled-font-destroy}
      @about-function{scaled-font-status}
      @about-function{scaled-font-type}
      @about-function{scaled-font-font-face}
      @about-function{scaled-font-font-options}
      @about-function{scaled-font-font-matrix}
      @about-function{scaled-font-ctm}
      @about-function{scaled-font-scale-matrix}
      @about-function{scaled-font-extents}
      @about-function{scaled-font-text-extents}
      @about-function{scaled-font-glyph-extents}
      @about-function{scaled-font-text-to-glyphs}
      @about-function{scaled-font-user-data}
    @end{subsection}
    @begin[Introduction to Font Options]{subsection}
      The font options specify how fonts should be rendered. Most of the time
      the font options implied by a surface are just right and do not need any
      changes, but for pixel-based targets tweaking font options may result in
      superior output on a particular display.
    @end{subsection}
    @begin[Types and functions for Font Options]{subsection}
      @about-symbol{subpixel-order-t}
      @about-symbol{hint-style-t}
      @about-symbol{hint-metrics-t}
      @about-symbol{color-mode-t}
      @about-symbol{font-options-t}
      @about-function{font-options-create}
      @about-function{font-options-copy}
      @about-function{font-options-destroy}
      @about-function{font-options-status}
      @about-function{font-options-merge}
      @about-function{font-options-hash}
      @about-function{font-options-equal}
      @about-function{font-options-antialias}
      @about-function{font-options-subpixel-order}
      @about-function{font-options-hint-style}
      @about-function{font-options-hint-metrics}
      @about-function{font-options-variations}
      @about-function{font-options-color-mode}
      @about-function{font-options-color-palette}
      @about-function{font-options-custom-palette-color}
    @end{subsection}
  @end{section}
  @begin[Surfaces]{section}
    @begin[Introduction to Cairo Devices]{subsection}
      Devices are the abstraction Cairo employs for the rendering system used by
      a @sym{cairo:surface-t} instance. You can get the device of a surface
      using the @fun{cairo:surface-device} function.

      Devices are created using custom functions specific to the rendering
      system you want to use. See the documentation for the surface types for
      those functions.

      An important function that devices fulfill is sharing access to the
      rendering system between Cairo and your application. If you want to access
      a device directly that you used to draw to with Cairo, you must first call
      the @fun{cairo:device-flush} function to ensure that Cairo finishes all
      operations on the device and resets it to a clean state.

      Cairo also provides the @fun{cairo:device-acquire} and
      @fun{cairo:device-release} functions to synchronize access to the
      rendering system in a multithreaded environment. This is done internally,
      but can also be used by applications.

      Putting this all together, a function that works with devices should look
      something like this:
      @begin{pre}
(defun my-device-modifying-function (device)
  (let (status)
    ;; Ensure the device is properly reset
    (cairo:device-flush device)
    ;; Try to aquire the device
    (unless (eq :success
                (setf status
                      (cairo:device-aquire device)))
      (warn \"Failed to aquire the device: ~a\" (cairo:status-to-string status))
      (return-from my-device-modifying-function))

    ;; Do the custom operations on the device here.
    ;; But do not call any Cairo functions that might acquire devices.

    ;; Release the device when done.
    (cairo:device-release device)))
      @end{pre}
      @b{Note:} Please refer to the documentation of each backend for additional
      usage requirements, guarantees provided, and interactions with existing
      surface API of the device functions for surfaces of that type.
    @end{subsection}
    @begin[Types and functions for Cairo Devices]{subsection}
      @about-symbol{device-type-t}
      @about-symbol{device-t}
      @about-function{device-reference}
      @about-function{device-reference-count}
      @about-function{device-destroy}
      @about-function{device-status}
      @about-function{device-type}
      @about-function{device-finish}
      @about-function{device-flush}
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
    @begin[Introduction to Cairo Surfaces]{subsection}
      The @sym{cairo:surface-t} structure is the abstract type representing all
      different drawing targets that cairo can render to. The actual drawings
      are performed using a Cairo context. A Cairo surface is created by using
      backend-specific constructors, typically of the form
      @code{cairo:backend-surface-create}.

      Most surface types allow accessing the surface without using Cairo
      functions. If you do this, keep in mind that it is mandatory that you call
      the @fun{cairo:surface-flush} function before reading from or writing to
      the surface and that you must use the @fun{cairo:surface-mark-dirty}
      function after modifying it.

      @b{Example.} Directly modifying an image surface
      @begin{pre}
(defun modify-image-surface (surface)
  (let ((data (cairo:image-surface-data surface))
        (width (cairo:image-surface-width surface))
        (height (cairo:image-surface-height surface))
        (stride (cairo:image-surface-stride surface)))

    ;; flush to ensure all writing to the image was done
    (cairo:surface-flush surface)

    ;; modify the image
    (modify-image-data data width height stride)

    ;; mark the image dirty so Cairo clears its caches
    (cairo:surface-mark-dirty surface)))
      @end{pre}
      Note that for other surface types it might be necessary to acquire the
      device of the surface first. See the @fun{cairo:device-acquire} function
      for a discussion of devices.
    @end{subsection}
    @begin[Functions and types for Cairo Surfaces]{subsection}
      @about-symbol{content-t}
      @about-symbol{surface-type-t}
      @about-symbol{surface-t}
      @about-macro{with-surface}
      @about-macro{with-context-for-surface}
      @about-function{surface-create-similar}
      @about-function{surface-create-similar-image}
      @about-function{surface-create-for-rectangle}
      @about-function{surface-reference}
      @about-function{surface-reference-count}
      @about-function{surface-destroy}
      @about-function{surface-status}
      @about-function{surface-type}
      @about-function{surface-finish}
      @about-function{surface-flush}
      @about-function{surface-device}
      @about-function{surface-font-options}
      @about-function{surface-content}
      @about-function{surface-mark-dirty}
      @about-function{surface-mark-dirty-rectangle}
      @about-function{surface-device-offset}
      @about-function{surface-device-scale}
      @about-function{surface-fallback-resolution}
      @about-function{surface-user-data}
      @about-function{surface-copy-page}
      @about-function{surface-show-page}
      @about-function{surface-has-show-text-glyphs}
      @about-function{surface-mime-data}
      @about-function{surface-supports-mime-type}
      @about-function{surface-map-to-image}
      @about-function{surface-unmap-image}
    @end{subsection}
    @begin[Introduction to Image Surfaces]{subsection}
      Image surfaces provide the ability to render to memory buffers either
      allocated by Cairo or by the calling code. The supported image formats
      are those defined in the @sym{cairo:format-t} enumeration.
    @end{subsection}
    @begin[Types and functions for Image Surfaces]{subsection}
      @about-symbol{format-t}
      @about-macro{with-image-surface}
      @about-macro{with-context-for-image-surface}
      @about-function{image-surface-create}
      @about-function{image-surface-create-for-data}
      @about-function{image-surface-data}
      @about-function{image-surface-format}
      @about-function{image-surface-width}
      @about-function{image-surface-height}
      @about-function{image-surface-stride}
      @about-function{format-stride-for-width}
    @end{subsection}
    @begin[Introduction to PDF Surfaces]{subsection}
      The PDF surface is used to render Cairo graphics to Adobe PDF files and
      is a multi-page vector surface backend.

      The following mime types are supported: @code{CAIRO_MIME_TYPE_JPEG},
      @code{CAIRO_MIME_TYPE_JP2}, @code{CAIRO_MIME_TYPE_UNIQUE_ID},
      @code{CAIRO_MIME_TYPE_JBIG2}, @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL},
      @code{CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID}, @code{CAIRO_MIME_TYPE_CCITT_FAX},
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS}.

      @subheading{JBIG2 Images}
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

      @subheading{CCITT Fax Images}
      The @code{CAIRO_MIME_TYPE_CCITT_FAX} mime data requires a number of
      decoding parameters These parameters are specified using
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS}.
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS} mime data must contain a string
      of the form @code{\"param1=value1 param2=value2 ...\"}.
      @begin[code]{table}
        @entry[Columns]{An integer specifying the width of the image
          in pixels. [required]}
        @entry[Rows]{An integer specifying the height of the image in scan
          lines. [required]}
        @entry[K]{An integer identifying the encoding scheme used. < 0 is 2
          dimensional Group 4, = 0 is Group 3 1 dimensional, > 0 is mixed 1
          and 2 dimensional encoding. Default is 0. [optional]}
        @entry[EndOfLine]{If true end-of-line bit patterns are present. Default
          is false. [optional]}
        @entry[EncodedByteAlign]{If true the end of line is padded with 0 bits
          so the next line begins on a byte boundary. Default is false.
          [optional]}
        @entry[EndOfBlock]{If true the data contains an end-of-block pattern.
          Default is true. [optional]}
        @entry[BlackIs1]{If true 1 bits are black pixels. Default is false.
           [optional]}
        @entry[DamagedRowsBeforeError]{An integer specifying the number of
          damages rows tolerated before an error occurs. Default is 0.
           [optional]}
      @end{table}
      Boolean values may be @code{\"true\"} or @code{\"false\"}, or 1 or 0.

      These parameters are the same as the CCITTFaxDecode parameters in the
      PostScript Language Reference and Portable Document Format (PDF). Refer
      to these documents for further details.

      An example @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS} string is:
      @begin{pre}
\"Columns=10230 Rows=40000 K=1 EndOfLine=true EncodedByteAlign=1 BlackIs1=false\"
      @end{pre}
    @end{subsection}
    @begin[Types and functions for PDF surfaces]{subsection}
      @about-symbol{pdf-outline-flags-t}
      @about-symbol{pdf-metadata-t}
      @about-symbol{pdf-version-t}
      @about-macro{with-pdf-surface}
      @about-macro{with-context-for-pdf-surface}
      @about-function{pdf-surface-create}
      @about-function{pdf-surface-restrict-to-version}
      @about-function{pdf-versions}
      @about-function{pdf-version-to-string}
      @about-function{pdf-surface-set-size}
      @about-function{pdf-surface-add-outline}
      @about-function{pdf-surface-set-metadata}
      @about-function{pdf-surface-set-custom-metadata}
      @about-function{pdf-surface-set-page-label}
      @about-function{pdf-surface-set-thumbnail-size}
    @end{subsection}
    @begin[Introduction to PNG Support]{subsection}
      The PNG functions allow reading PNG images into image surfaces, and
      writing any surface to a PNG file.

      It is a toy API. It only offers very simple support for reading and
      writing PNG files, which is sufficient for testing and demonstration
      purposes. Applications which need more control over the generated PNG
      file should access the pixel data directly, using the
      @fun{cairo:image-surface-data} function or a backend-specific access
      function, and process it with another library, for example GdkPixbuf or
      @code{libpng}.
    @end{subsection}
    @begin[Functions for PNG support]{subsection}
      @about-function{image-surface-create-from-png}
      @about-function{image-surface-create-from-png-stream}
      @about-function{surface-write-to-png}
      @about-function{surface-write-to-png-stream}
    @end{subsection}
    @begin[Indroduction to PostScript Surfaces]{subsection}
      The PostScript surface is used to render Cairo graphics to Adobe
      PostScript files and is a multi-page vector surface backend.

      The following mime types are supported: @code{CAIRO_MIME_TYPE_JPEG},
      @code{CAIRO_MIME_TYPE_UNIQUE_ID}, @code{CAIRO_MIME_TYPE_CCITT_FAX},
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS}, @code{CAIRO_MIME_TYPE_CCITT_FAX},
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS}, @code{CAIRO_MIME_TYPE_EPS},
      @code{CAIRO_MIME_TYPE_EPS_PARAMS}.

      Source surfaces used by the PostScript surface that have a
      @code{CAIRO_MIME_TYPE_UNIQUE_ID} mime type will be stored in PostScript
      printer memory for the duration of the print job. The
      @code{CAIRO_MIME_TYPE_UNIQUE_ID} mime type should only be used for small
      frequently used sources.

      The @code{CAIRO_MIME_TYPE_CCITT_FAX} and
      @code{CAIRO_MIME_TYPE_CCITT_FAX_PARAMS} mime types are documented in
      CCITT Fax Images.

      @subheading{Embedding EPS files}
      Encapsulated PostScript files can be embedded in the PS output by setting
      the @code{CAIRO_MIME_TYPE_EPS} mime data on a surface to the EPS data and
      painting the surface. The EPS will be scaled and translated to the extents
      of the surface the EPS data is attached to.

      The @code{CAIRO_MIME_TYPE_EPS} mime type requires the
      @code{CAIRO_MIME_TYPE_EPS_PARAMS} mime data to also be provided in order
      to specify the embeddding parameters. @code{CAIRO_MIME_TYPE_EPS_PARAMS}
      mime data must contain a string of the form
      @code{\"bbox=[llx lly urx ury]\"} that specifies the bounding box (in PS
      coordinates) of the EPS graphics. The parameters are: @code{lower left x},
      @code{lower left y}, @code{upper right x}, @code{upper right y}. Normally
      the @code{bbox} data is identical to the @code{%%BoundingBox} data in
      the EPS file.
    @end{subsection}
    @begin[Type and functions for PostScript Surfaces]{subsection}
      @about-symbol{ps-level-t}
      @about-macro{with-ps-surface}
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
    @begin[Indroduction to Recording Surfaces]{subsection}
      A recording surface is a surface that records all drawing operations at
      the highest level of the surface backend interface, that is, the level
      of paint, mask, stroke, fill, and text glyphs. The recording surface can
      then be \"replayed\" against any target surface by using it as a source
      surface.

      If you want to replay a surface so that the results in target will be
      identical to the results that would have been obtained if the original
      operations applied to the recording surface had instead been applied to
      the target surface, you can use code like this:
      @begin{pre}
(cairo:with-context (context target)
  (cairo:set-source-surface context recording-surface 0.0 0.0)
  (cairo:paint context)
  ...)
      @end{pre}
      A recording surface is logically unbounded, that is, it has no implicit
      constraint on the size of the drawing surface. However, in practice this
      is rarely useful as you wish to replay against a particular target surface
      with known bounds. For this case, it is more efficient to specify the
      target extents to the recording surface upon creation.

      The recording phase of the recording surface is careful to snapshot all
      necessary objects, paths, patterns, etc., in order to achieve accurate
      replay.
    @end{subsection}
    @begin[Functions for Recording Surfaces]{subsection}
      @about-macro{with-recording-surface}
      @about-macro{with-context-for-recording-surface}
      @about-function{recording-surface-create}
      @about-function{recording-surface-ink-extents}
      @about-function{recording-surface-extents}
    @end{subsection}
    @begin[Introduction to SVG Surfaces]{subsection}
      The SVG surface is used to render Cairo graphics to SVG files and is a
      multi-page vector surface backend.
    @end{subsection}
    @begin[Types and functions for SVG surfaces]{subsection}
      @about-symbol{svg-version-t}
      @about-symbol{svg-unit-t}
      @about-function{svg-surface-create}
      @about-function{svg-surface-create-for-stream}
      @about-function{svg-surface-document-unit}
      @about-function{svg-surface-restrict-to-version}
      @about-function{svg-versions}
      @about-function{svg-version-to-string}
    @end{subsection}
    @begin[Indroduction to Script Surfaces]{subsection}
      The script surface provides the ability to render to a native script that
      matches the Cairo drawing model. The scripts can be replayed using tools
      under the @file{util/cairo-script} directory, or with the
      @code{cairo-perf-trace} utility.
    @end{subsection}
    @begin[Types and functions for Script Surfaces]{subsection}
      @about-symbol{script-mode-t}
      @about-macro{with-script-surface}
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
      @about-symbol{matrix-t}
      @about-macro{with-matrix}
      @about-macro{with-matrices}
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
    @begin[Introduction to Error handling]{subsection}
      Cairo uses a single status type to represent all kinds of errors. A
      status value of @code{:success} represents no error and has an integer
      value of zero. All other status values represent an error.

      Cairo's error handling is designed to be easy to use and safe. All major
      Cairo objects retain an error status internally which can be queried
      anytime by the users using @code{cairo*-status} calls. In the mean time,
      it is safe to call all Cairo functions normally even if the underlying
      object is in an error status. This means that no error handling code is
      required before or after each individual Cairo function call.
    @end{subsection}
    @begin[Types and functions for Error handling]{subsection}
      @about-symbol{status-t}
      @about-function{status-to-string}
    @end{subsection}
    @begin[Introduction to Version Information]{subsection}
      Cairo provides the ability to examine the version at either compile-time
      or run-time and in both a human readable form as well as an encoded form
      suitable for direct comparison. Cairo also provides the
      @fun{cairo:version-encode} function to perform the encoding.
    @end{subsection}
    @begin[Functions for Version Information]{subsection}
      @about-function{version-encode}
      @about-function{version}
      @about-function{version-string}
    @end{subsection}
  @end{section}")

;;; --- End of file cairo.package.lisp -----------------------------------------
