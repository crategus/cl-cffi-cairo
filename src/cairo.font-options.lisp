;;; ----------------------------------------------------------------------------
;;; cairo.font-options.lisp
;;;
;;; The documentation in this file is taken from the Cairo Reference Manual
;;; Version 1.18 and modified to document the Lisp binding to the Cairo library,
;;; see <http://cairographics.org>. The API documentation of the Lisp binding
;;; is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; cairo_font_options_t
;;;
;;;     How a font should be rendered
;;;
;;; Types and Values
;;;
;;;     cairo_font_options_t
;;;     cairo_subpixel_order_t
;;;     cairo_hint_style_t
;;;     cairo_hint_metrics_t
;;;     cairo_color_mode_t                                 Since 1.18
;;;
;;; Functions
;;;
;;;     cairo_font_options_create
;;;     cairo_font_options_copy
;;;     cairo_font_options_destroy
;;;     cairo_font_options_status
;;;     cairo_font_options_merge
;;;     cairo_font_options_hash
;;;     cairo_font_options_equal
;;;
;;;     cairo_font_options_set_antialias
;;;     cairo_font_options_get_antialias
;;;     cairo_font_options_set_subpixel_order
;;;     cairo_font_options_get_subpixel_order
;;;     cairo_font_options_set_hint_style
;;;     cairo_font_options_get_hint_style
;;;     cairo_font_options_set_hint_metrics
;;;     cairo_font_options_get_hint_metrics
;;;     cairo_font_options_get_variations
;;;     cairo_font_options_set_variations
;;;     cairo_font_options_set_color_mode                  Since 1.18
;;;     cairo_font_options_get_color_mode                  Since 1.18
;;;     cairo_font_options_set_color_palette               Since 1.18
;;;     cairo_font_options_get_color_palette               Since 1.18
;;;     cairo_font_options_set_custom_palette_color        Since 1.18
;;;     cairo_font_options_get_custom_palette_color        Since 1.18
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_subpixel_order_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum subpixel-order-t
  :default
  :rgb
  :bgr
  :vrgb
  :vbgr)

#+liber-documentation
(setf (liber:alias-for-symbol 'subpixel-order-t)
      "CEnum"
      (liber:symbol-documentation 'subpixel-order-t)
 "@version{2025-09-21}
  @begin{declaration}
(cffi:defcenum subpixel-order-t
  :default
  :rgb
  :bgr
  :vrgb
  :vbgr)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:default]{Use the default subpixel order for the target device.}
      @entry[:rgb]{Subpixel elements are arranged horizontally with red at the
        left.}
      @entry[:bgr]{Subpixel elements are arranged horizontally with blue at the
        left.}
      @entry[:vrgb]{Subpixel elements are arranged vertically with red at the
        top.}
      @entry[:vbgr]{Subpixel elements are arranged vertically with blue at the
        top.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The subpixel order specifies the order of color elements within each pixel
    on the display device when rendering with an antialiasing mode of
    @val[cairo:antialias-t]{:subpixel}.
  @end{short}

  @see-symbol{cairo:font-options-t}
  @see-function{cairo:font-options-subpixel-order}")

(export 'subpixel-order-t)

;;; ----------------------------------------------------------------------------
;;; cairo_hint_style_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum hint-style-t
  :default
  :none
  :slight
  :medium
  :full)

#+liber-documentation
(setf (liber:alias-for-symbol 'hint-style-t)
      "CEnum"
      (liber:symbol-documentation 'hint-style-t)
 "@version{2025-09-01}
  @begin{declaration}
(cffi:defcenum hint-style-t
  :default
  :none
  :slight
  :medium
  :full)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:default]{Use the default hint style for font backend and target
        device.}
      @entry[:none]{Do not hint outlines.}
      @entry[:sligth]{Hint outlines slightly to improve contrast while retaining
        good fidelity to the original shapes.}
      @entry[:medium]{Hint outlines with medium strength giving a compromise
        between fidelity to the original shapes and contrast.}
      @entry[:full]{Hint outlines to maximize contrast.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Specifies the type of hinting to do on font outlines.
  @end{short}
  Hinting is the process of fitting outlines to the pixel grid in order to
  improve the appearance of the result. Since hinting outlines involves
  distorting them, it also reduces the faithfulness to the original outline
  shapes. Not all of the outline hinting styles are supported by all font
  backends.
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:font-options-hint-style}")

(export 'hint-style-t)

;;; ----------------------------------------------------------------------------
;;; cairo_hint_metrics_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum hint-metrics-t
  :default
  :off
  :on)

#+liber-documentation
(setf (liber:alias-for-symbol 'hint-metrics-t)
      "CEnum"
      (liber:symbol-documentation 'hint-metrics-t)
 "@version{2025-09-01}
  @begin{declaration}
(cffi:defcenum hint-metrics-t
  :default
  :off
  :on)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:default]{Hint metrics in the default manner for the font backend
        and target device.}
      @entry[:off]{Do not hint font metrics.}
      @entry[:on]{Hint font metrics.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Specifies whether to hint font metrics.
  @end{short}
  Hinting font metrics means quantizing them so that they are integers in
  device space. Doing this improves the consistency of letter and line spacing,
  however it also means that text will be laid out differently at different
  zoom factors.
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:font-options-hint-metrics}")

(export 'hint-metrics-t)

;;; ----------------------------------------------------------------------------
;;; cairo_color_mode_t
;;; ----------------------------------------------------------------------------

#+cairo-1-18
(cffi:defcenum color-mode-t
  :default
  :no-color
  :color)

#+(and cairo-1-18 liber-documentation)
(setf (liber:alias-for-symbol 'color-mode-t)
      "CEnum"
      (liber:symbol-documentation 'color-mode-t)
 "@version{2025-09-01}
  @begin{declaration}
(cffi:defcenum color-mode-t
  :default
  :no-color
  :color)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:default]{Use the default color mode for font backend and target
        device.}
      @entry[:no-color]{Disable rendering color glyphs. Glyphs are always
        rendered as outline glyphs.}
      @entry[:color]{Enable rendering color glyphs. If the font contains a color
        presentation for a glyph, and when supported by the font backend, the
        glyph will be rendered in color.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Specifies if color fonts are to be rendered using the color glyphs or
    outline glyphs.
  @end{short}
  Glyphs that do not have a color presentation, and non-color fonts are not
  affected by this font option.

  Since 1.18
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:font-options-color-mode}")

#+cairo-1-18
(export 'color-mode-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct font-options-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'font-options-t)
      "CStruct"
      (liber:symbol-documentation 'font-options-t)
 #+liber-documentation
 "@version{2025-09-02}
  @begin{short}
    The @sym{cairo:font-options-t} structure is an opaque structure holding all
    options that are used when rendering fonts.
  @end{short}
  Individual features of a @sym{cairo:font-options-t} instance can be set or
  accessed using accessor functions named
  @code{cairo:font-options-feature-name}, like the
  @fun{cairo:font-options-antialias} function.

  New features may be added to a @sym{cairo:font-options-t} structure in the
  future. For this reason, the @fun{cairo:font-options-copy},
  @fun{cairo:font-options-equal}, @fun{cairo:font-options-merge}, and
  @fun{cairo:font-options-hash} functions should be used to copy, check for
  equality, merge, or compute a hash value of @sym{cairo:font-options-t}
  instances.
  @see-function{cairo:font-options-copy}
  @see-function{cairo:font-options-equal}
  @see-function{cairo:font-options-merge}
  @see-function{cairo:font-options-hash}
  @see-function{cairo:font-options-antialias}")

(export 'font-options-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_create
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_options_create" font-options-create)
    (:pointer (:struct font-options-t))
 #+liber-documentation
 "@version{2025-09-02}
  @return{The newly allocated @sym{cairo:font-options-t} instance.}
  @begin{short}
    Allocates a new font options instance with all options initialized to
    default values.
  @end{short}
  Free with the @fun{cairo:font-options-destroy} function. This function always
  returns a valid pointer. If memory cannot be allocated, then a special error
  object is returned where all operations on the object do nothing. You can
  check for this with the @fun{cairo:font-options-status} function.
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:font-options-destroy}
  @see-function{cairo:font-options-status}")

(export 'font-options-create)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_options_copy" font-options-copy)
    (:pointer (:struct font-options-t))
 #+liber-documentation
 "@version{2025-09-02}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @return{The newly allocated @sym{cairo:font-options-t} instance.}
  @begin{short}
    Allocates a new font options object copying the option values from original.
  @end{short}
  Free with the @fun{cairo:font-options-destroy} function. This function always
  returns a valid pointer. If memory cannot be allocated, then a special error
  object is returned where all operations on the object do nothing. You can
  check for this with the @fun{cairo:font-options-status} function.
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:font-options-destroy}
  @see-function{cairo:font-options-status}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-copy)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_destroy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_options_destroy" font-options-destroy) :void
 #+liber-documentation
 "@version{2025-09-02}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @begin{short}
    Destroys a @sym{cairo:font-options-t} instance created with the
    @fun{cairo:font-options-create} or @fun{cairo:font-options-copy} function.
  @end{short}
  @see-class{cairo:font-options-t}
  @see-function{cairo:font-options-create}
  @see-function{cairo:font-options-copy}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_options_status" font-options-status) status-t
 #+liber-documentation
 "@version{2025-09-20}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @begin{return}
    The @val[cairo:status-t]{:success} or @val[cairo:status-t]{:no-memory}
    values of the @sym{cairo:status-t} enumeration.
  @end{return}
  @begin{short}
    Checks whether an error has previously occurred for this font options
    instance.
  @end{short}
  @see-symbol{cairo:font-options-t}
  @see-symbol{cairo:status-t}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-status)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_merge
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_options_merge" font-options-merge) :void
 #+liber-documentation
 "@version{2025-09-21}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[other]{another @sym{cairo:font-options-t} instance}
  @begin{short}
    Merges non-default options from @arg{other} into @arg{options}, replacing
    existing values.
  @end{short}
  This operation can be thought of as somewhat similar to compositing
  @arg{other} onto @arg{options} with the operation of
  @val[cairo:operator-t]{:over}.
  @see-symbol{cairo:font-options-t}
  @see-symbol{cairo:operator-t}"
  (options (:pointer (:struct font-options-t)))
  (other (:pointer (:struct font-options-t))))

(export 'font-options-merge)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_hash
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_options_hash" font-options-hash) :ulong
 #+liber-documentation
 "@version{2025-09-01}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @begin{return}
    The unsigned long integer for the hash value for the font options
    object.
  @end{return}
  @begin{short}
    Compute a hash for the font options instance.
  @end{short}
  This value will be useful when storing an object containing a
  @sym{cairo:font-options-t} instance in a hash table.
  @see-symbol{cairo:font-options-t}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-hash)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_options_equal" font-options-equal) :bool
 #+liber-documentation
 "@version{2025-09-01}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[other]{another @sym{cairo:font-options-t} instance}
  @begin{return}
    @em{True} if all fields of the two font options instances match. Note that
    this function will return @em{false} if either instance is in error.
  @end{return}
  @begin{short}
    Compares two font options instances for equality.
  @end{short}
  @see-symbol{cairo:font-options-t}"
  (options (:pointer (:struct font-options-t)))
  (other (:pointer (:struct font-options-t))))

(export 'font-options-equal)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_antialias
;;; cairo_font_options_set_antialias
;;; ----------------------------------------------------------------------------

(defun (setf font-options-antialias) (antialias options)
  (cffi:foreign-funcall "cairo_font_options_set_antialias"
                        (:pointer (:struct font-options-t)) options
                        antialias-t antialias
                        :void)
  antialias)

(cffi:defcfun ("cairo_font_options_get_antialias" font-options-antialias)
    antialias-t
 #+liber-documentation
 "@version{2025-09-02}
  @syntax{(cairo:font-options-antialias options) => antialias}
  @syntax{(setf (cairo:font-options-antialias options) antialias)}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[antialias]{a @sym{cairo:antialias-t} value}
  @begin{short}
    The @fun{cairo:font-options-antialias} function gets the antialiasing mode
    for the font options instance.
  @end{short}
  The @setf{cairo:font-options-antialias} function sets the antialiasing mode.
  This specifies the type of antialiasing to do when rendering text.
  @see-symbol{cairo:font-options-t}
  @see-symbol{cairo:antialias-t}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-antialias)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_subpixel_order
;;; cairo_font_options_set_subpixel_order
;;; ----------------------------------------------------------------------------

(defun (setf font-options-subpixel-order) (order options)
  (cffi:foreign-funcall "cairo_font_options_set_subpixel_order"
                        (:pointer (:struct font-options-t)) options
                        subpixel-order-t order
                        :void)
  order)

(cffi:defcfun ("cairo_font_options_get_subpixel_order"
               font-options-subpixel-order) subpixel-order-t
 #+liber-documentation
 "@version{2025-09-21}
  @syntax{(cairo:font-options-subpixel-order options) => order}
  @syntax{(setf (cairo:font-options-subpixel-order options) order)}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[order]{a @sym{cairo:subpixel-order-t} value}
  @begin{short}
    The @fun{cairo:font-options-subpixel-order} function gets the subpixel order
    for the font options instance.
  @end{short}
  The @setf{cairo:font-options-subpixel-order} function sets the subpixel order.
  The subpixel order specifies the order of color elements within each pixel on
  the display device when rendering with an antialiasing mode of
  @val[cairo:antialias-t]{:subpixel}. See the documentation for the
  @sym{cairo:subpixel-order-t} enumeration for full details.
  @see-symbol{cairo:font-options-t}
  @see-symbol{cairo:subpixel-order-t}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-subpixel-order)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_hint_style
;;; cairo_font_options_set_hint_style
;;; ----------------------------------------------------------------------------

(defun (setf font-options-hint-style) (style options)
  (cffi:foreign-funcall "cairo_font_options_set_hint_style"
                        (:pointer (:struct font-options-t)) options
                        hint-style-t style
                        :void)
  style)

(cffi:defcfun ("cairo_font_options_get_hint_style" font-options-hint-style)
    hint-style-t
 #+liber-documentation
 "@version{2025-09-02}
  @syntax{(cairo:font-options-hint-style options) => style}
  @syntax{(setf (cairo:font-options-hint-style options) style)}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[style]{a @sym{cairo:hint-style-t} value}
  @begin{short}
    The @fun{cairo:font-options-hint-style} function gets the hint style for
    font outlines for the font options instance.
  @end{short}
  The @setf{cairo:font-options-hint-style} function sets the hint style for font
  outlines for the font options instance. This controls whether to fit font
  outlines to the pixel grid, and if so, whether to optimize for fidelity or
  contrast. See the documentation for the @sym{cairo:hint-style-t} enumeration
  for full details.
  @see-symbol{cairo:font-options-t}
  @see-symbol{cairo:hint-style-t}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-hint-style)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_hint_metrics
;;; cairo_font_options_set_hint_metrics
;;; ----------------------------------------------------------------------------

(defun (setf font-options-hint-metrics) (metrics options)
  (cffi:foreign-funcall "cairo_font_options_set_hint_metrics"
                        (:pointer (:struct font-options-t)) options
                        hint-metrics-t metrics
                        :void)
  metrics)

(cffi:defcfun ("cairo_font_options_get_hint_metrics"
               font-options-hint-metrics) hint-metrics-t
 #+liber-documentation
 "@version{2025-09-02}
  @syntax{(cairo:font-options-hint-metrics options) => metrics}
  @syntax{(setf (cairo:font-options-hint-metrics options) metrics)}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[metrics]{a @sym{cairo:hint-metrics-t} value}
  @begin{short}
    The @fun{cairo:font-options-hint-metrics} function gets the metrics hinting
    mode for the font options instance.
  @end{short}
  The @setf{cairo:font-options-hint-metrics} function sets the metrics hinting
  mode for the font options instance. This controls whether metrics are
  quantized to integers in device units. See the documentation for
  the @sym{cairo:hint-metrics-t} enumeration for full details.
  @see-symbol{cairo:font-options-t}
  @see-symbol{cairo:hint-metrics-t}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-hint-metrics)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_variations
;;; cairo_font_options_set_variations
;;; ----------------------------------------------------------------------------

(defun (setf font-options-variations) (variations options)
  (cffi:foreign-funcall "cairo_font_options_set_variations"
                        (:pointer (:struct font-options-t)) options
                        :string (or variations (cffi:null-pointer))
                        :void)
  variations)

(cffi:defcfun ("cairo_font_options_get_variations" font-options-variations)
    :string
 #+liber-documentation
 "@version{2025-09-02}
  @syntax{(cairo:font-options-variations options) => variations}
  @syntax{(setf (cairo:font-options-variations options) variations)}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[variations]{a string for the font variations, or @code{nil}}
  @begin{short}
    The @fun{cairo:font-options-variations} function gets the OpenType font
    variations for the font options instance.
  @end{short}
  The @setf{cairo:font-options-variations} function sets the OpenType font
  variations for the font options instance. The returned string belongs to the
  font options instance and must not be modified. It is valid until either the
  font options instance is destroyed or the font variations in the font options
  instance is modified.

  Font variations are specified as a string with a format that is similar to the
  CSS font-variation-settings. The string contains a comma-separated list of
  axis assignments, which each assignment consists of a 4-character axis name
  and a value, separated by whitespace and optional equals sign.
  @begin[Examples]{dictionary}
    @begin{pre}
wght=200, wdth=140.5
wght 200, wdth 140.5
    @end{pre}
  @end{dictionary}
  @see-symbol{cairo:font-options-t}"
  (options (:pointer (:struct font-options-t))))

(export 'font-options-variations)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_color_mode
;;; cairo_font_options_set_color_mode
;;; ----------------------------------------------------------------------------

#+cairo-1-18
(defun (setf font-options-color-mode) (value options)
  (cffi:foreign-funcall "cairo_font_options_set_color_mode"
                        (:pointer (:struct font-options-t)) options
                        color-mode-t value
                        :void)
  value)

#+cairo-1-18
(cffi:defcfun ("cairo_font_options_get_color_mode" font-options-color-mode)
    color-mode-t
 #+liber-documentation
 "@version{2025-09-02}
  @syntax{(cairo:font-options-color-mode options) => mode}
  @syntax{(setf (cairo:font-options-color-mode options) mode)}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[mode]{a @sym{cairo:color-mode-t} value for the color mode for the
    font options instance}
  @begin{short}
    The @fun{cairo:font-options-color-mode} function gets the color mode for
    the font options instance.
  @end{short}
  The @setf{cairo:font-options-color-mode} function sets the color mode. This
  controls whether color fonts are to be rendered in color or as outlines. See
  the documentation for the @sym{cairo:color-mode-t} enumeration for full
  details.

  Since 1.18
  @see-symbol{cairo:font-options-t}
  @see-symbol{cairo:color-mode-t}"
  (options (:pointer (:struct font-options-t))))

#+cairo-1-18
(export 'font-options-color-mode)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_color_palette
;;; cairo_font_options_set_color_palette
;;; ----------------------------------------------------------------------------

#+cairo-1-18
(defun (setf font-options-color-palette) (value options)
  (cffi:foreign-funcall "cairo_font_options_set_color_palette"
                        (:pointer (:struct font-options-t)) options
                        :uint value
                        :void)
  value)

#+cairo-1-18
(cffi:defcfun ("cairo_font_options_get_color_palette"
               font-options-color-palette) :uint
 #+liber-documentation
 "@version{2025-09-02}
  @syntax{(cairo:font-options-color-palette options) => index}
  @syntax{(setf (cairo:font-options-color-palette options) index)}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[index]{an unsigned integer for the palette index}
  @begin{short}
    The @fun{cairo:font-options-color-palette} function gets the current
    OpenType color font palette for the font options instance.
  @end{short}
  The @setf{cairo:font-options-color-palette} function sets the OpenType font
  color palette. OpenType color fonts with a CPAL table may contain multiple
  palettes. The default color palette index is 0.

  If @arg{index} is invalid, the default palette is used.

  Individual colors within the palette may be overriden with the
  @fun{cairo:font-options-custom-palette-color} function.

  Since 1.18
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:font-options-custom-palette-color}"
  (options (:pointer (:struct font-options-t))))

#+cairo-1-18
(export 'font-options-color-palette)

;;; ----------------------------------------------------------------------------
;;; cairo_font_options_get_custom_palette_color
;;; cairo_font_options_set_custom_palette_color
;;; ----------------------------------------------------------------------------

#+cairo-1-18
(defun (setf font-options-custom-palette-color) (value options index)
  (destructuring-bind (red green blue alpha) value
    (setf red (coerce red 'double-float))
    (setf green (coerce green 'double-float))
    (setf blue (coerce blue 'double-float))
    (setf alpha (coerce alpha 'double-float))
    (cffi:foreign-funcall "cairo_font_options_set_custom_palette_color"
                          (:pointer (:struct font-options-t)) options
                          :uint index
                          :double red
                          :double green
                          :double blue
                          :double alpha
                          :void)
    (values red green blue alpha)))

#+cairo-1-18
(cffi:defcfun ("cairo_font_options_get_custom_palette_color"
               %font-options-custom-palette-color) status-t
  (options (:pointer (:struct font-options-t)))
  (index :uint)
  (red (:pointer :double))
  (green (:pointer :double))
  (blue (:pointer :double))
  (alpha (:pointer :double)))

#+cairo-1-18
(defun font-options-custom-palette-color (options index)
 #+liber-documentation
 "@version{2025-01-29}
  @syntax{(cairo:font-options-custom-palette-color options) => red, green,
    blue, alpha}
  @syntax{(setf (cairo:font-options-custom-palette-color options)
    (list red green blue alpha))}
  @argument[options]{a @sym{cairo:font-options-t} instance}
  @argument[index]{an unsigned integer for the index of the color to get}
  @argument[red]{a number for the red component of the color}
  @argument[green]{a number for the green component of the color}
  @argument[blue]{a number for the blue component of the color}
  @argument[alpha]{a number for the alpha component of the color}
  @begin{short}
    The @fun{cairo:font-options-custom-palette-color} function gets the custom
    palette color for the color index for the font options instance.
  @end{short}
  Returns @code{nil} if no custom color exists for the color index.

  The @setf{cairo:font-options-custom-palette-color} function sets a custom
  palette color for the font options instance. This overrides the palette color
  at the specified color index. This override is independent of the selected
  palette index and will remain in place even if the
  @fun{cairo:font-options-color-palette} function is called to change the
  palette index.

  It is only possible to override color indexes already in the font palette.

  Since 1.18
  @begin[Notes]{dictionary}
    The arguments are coerced to double floats before being passed to the
    foreign C functions.
  @end{dictionary}
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:font-options-color-palette}"
  (cffi:with-foreign-objects ((red :double) (green :double) (blue :double)
                              (alpha :double))
    (when (eq :sucess
              (%font-options-custom-palette-color options
                                                  index
                                                  red green blue
                                                  alpha))
      (values (cffi:mem-ref red :double)
              (cffi:mem-ref green :double)
              (cffi:mem-ref blue :double)
              (cffi:mem-ref alpha :double)))))

#+cairo-1-18
(export 'font-options-custom-palette-color)

;;; --- cairo.font-options.lisp ------------------------------------------------
