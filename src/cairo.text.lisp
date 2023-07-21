;;; ----------------------------------------------------------------------------
;;; cairo.text.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;; Text
;;;
;;;     Rendering text and glyphs
;;;
;;; Types and Values
;;;
;;;     cairo_glyph_t                            --> cairo.scaled-font.lisp
;;;     cairo_font_slant_t
;;;     cairo_font_weight_t
;;;     cairo_text_cluster_t
;;;     cairo_text_cluster_flags_t
;;;
;;; Functions
;;;
;;;     cairo_select_font_face
;;;     cairo_set_font_size
;;;     cairo_set_font_matrix
;;;     cairo_get_font_matrix
;;;     cairo_set_font_options
;;;     cairo_get_font_options
;;;     cairo_set_font_face
;;;     cairo_get_font_face
;;;     cairo_set_scaled_font
;;;     cairo_get_scaled_font
;;;     cairo_show_text
;;;     cairo_show_glyphs
;;;     cairo_show_text_glyphs
;;;     cairo_font_extents
;;;     cairo_text_extents
;;;     cairo_glyph_extents
;;;     cairo_toy_font_face_create
;;;     cairo_toy_font_face_get_family
;;;     cairo_toy_font_face_get_slant
;;;     cairo_toy_font_face_get_weight
;;;     cairo_glyph_allocate
;;;     cairo_glyph_free
;;;     cairo_text_cluster_allocate
;;;     cairo_text_cluster_free
;;;
;;; Description
;;;
;;;     The functions with text in their name form Cairo's toy text API. The toy
;;;     API takes UTF-8 encoded text and is limited in its functionality to
;;;     rendering simple left-to-right text with no advanced features. That
;;;     means for example that most complex scripts like Hebrew, Arabic, and
;;;     Indic scripts are out of question. No kerning or correct positioning of
;;;     diacritical marks either. The font selection is pretty limited too and
;;;     does not handle the case that the selected font does not cover the
;;;     characters in the text. This set of functions are really that, a toy
;;;     text API, for testing and demonstration purposes. Any serious
;;;     application should avoid them.
;;;
;;;     The functions with glyphs in their name form Cairo's low-level text API.
;;;     The low-level API relies on the user to convert text to a set of glyph
;;;     indexes and positions. This is a very hard problem and is best handled
;;;     by external libraries, like the pangocairo that is part of the Pango
;;;     text layout and rendering library. Pango is available from
;;;     http://www.pango.org/.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

(defmacro with-cairo-toy-font-face ((face &rest args) &body body)
 #+liber-documentation
 "@version{2023-7-21}
  @syntax[]{(with-cairo-toy-font-face (face family slant weight) body => result}
  @argument[face]{a newly allocated @symbol{cairo:font-face-t} instance}
  @argument[familiy]{a string with the font family name, encoded in UTF-8}
  @argument[slant]{a @symbol{cairo:font-slant-t} value with the slant for the
    font}
  @argument[weight]{a @symbol{cairo:font-weight-t} value with the weight for
    the font}
  @begin{short}
    The @sym{with-cairo-toy-font-face} macro allocates a new
    @symbol{cairo:font-face-t} instance and executes the body that uses the
    font face.
  @end{short}
  After execution of the body the allocated memory for the font face is
  released. See the @fun{cairo:toy-font-face-create} function for more
  information.
  @see-symbol{cairo:font-face-t}
  @see-symbol{cairo:font-slant-t}
  @see-symbol{cairo:font-weight-t}
  @see-function{cairo:toy-font-face-create}"
  `(let ((,face (toy-font-face-create ,@args)))
     (unwind-protect
       (progn ,@body)
       (font-face-destroy ,face))))

(export 'with-cairo-toy-font-face)

;;; ----------------------------------------------------------------------------
;;; enum cairo_font_slant_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum font-slant-t
  :normal
  :italic
  :oblique)

#+liber-documentation
(setf (liber:alias-for-symbol 'font-slant-t)
      "CEnum"
      (liber:symbol-documentation 'font-slant-t)
 "@version{2023-7-21}
  @short{Specifies variants of a font face based on their slant.}
  @begin{pre}
(cffi:defcenum font-slant-t
  :normal
  :italic
  :oblique)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Upright font style.}
    @entry[:italic]{Italic font style.}
    @entry[:oblique]{Oblique font style.}
  @end{table}
  @see-symbol{cairo:font-weight-t}
  @see-function{cairo:select-font-face}")

(export 'font-slant-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_font_weight_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum font-weight-t
  :normal
  :bold)

#+liber-documentation
(setf (liber:alias-for-symbol 'font-weight-t)
      "CEnum"
      (liber:symbol-documentation 'font-weight-t)
 "@version{2023-7-21}
  @short{Specifies variants of a font face based on their weight.}
  @begin{pre}
(cffi:defcenum font-slant-t
  :normal
  :bold)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Normal font weight.}
    @entry[:bold]{Bold font weight.}
  @end{table}
  @see-symbol{cairo:font-slant-t}
  @see-function{cairo:select-font-face}")

(export 'font-weight-t)

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_t
;;; ----------------------------------------------------------------------------

(cffi:defcstruct text-cluster-t
  (num-bytes :int)
  (num-glyphs :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-cluster-t)
      "CStruct"
      (liber:symbol-documentation 'text-cluster-t)
 "@version{#2021-12-12}
  @begin{short}
    The @sym{cairo:text-cluster-t} structure holds information about a single
    text cluster.
  @end{short}
  A text cluster is a minimal mapping of some glyphs corresponding to some
  UTF-8 text.

  For a cluster to be valid, both @arg{num-bytes} and @arg{num-glyphs} should
  be non-negative, and at least one should be non-zero. Note that clusters with
  zero glyphs are not as well supported as normal clusters. For example, PDF
  rendering applications typically ignore those clusters when PDF text is
  being selected.

  See the @fun{cairo:show-text-glyphs} function for how clusters are used in
  advanced text operations.
  @begin{pre}
(cffi:defcstruct text-cluster-t
  (num-bytes :int)
  (num-glyphs :int))
  @end{pre}
  @begin[code]{table}
    @entry[num-bytes]{The number of bytes of UTF-8 text covered by cluster.}
    @entry[num-glyphs]{The number of glyphs covered by cluster.}
  @end{table}
  @see-symbol{cairo:text-cluster-flags-t}
  @see-function{cairo:show-text-glyphs}")

(export 'text-cluster-t)

;;; ----------------------------------------------------------------------------
;;; enum cairo_text_cluster_flags_t
;;; ----------------------------------------------------------------------------

(cffi:defcenum text-cluster-flags-t
  (:backward 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-cluster-flags-t)
      "CEnum"
      (liber:symbol-documentation 'text-cluster-flags-t)
 "@version{#2021-12-12}
  @short{Specifies properties of a text cluster mapping.}
  @begin{pre}
(cffi:defcenum text-cluster-flags-t
  (:backward 1))
  @end{pre}
  @begin[code]{table}
    @entry[:backward]{The clusters in the cluster array map to glyphs in the
      glyph array from end to start.}
  @end{table}
  @see-symbol{cairo:text-cluster-t}")

(export 'text-cluster-flags-t)

;;; ----------------------------------------------------------------------------
;;; cairo_select_font_face ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_select_font_face" %select-font-face) :void
  (cr (:pointer (:struct context-t)))
  (family :string)
  (slant font-slant-t)
  (weight font-weight-t))

(defun select-font-face (cr family &key (slant :normal) (weight :normal))
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[family]{a string with the font family name, encoded in UTF-8}
  @argument[slant]{the slant of type @symbol{cairo:font-slant-t} for the font,
    default value is @code{:normal}}
  @argument[weight]{the weight of type @symbol{cairo:font-weight-t} for the
    font, default value is @code{:normal}}
  @begin{short}
    Selects a family and style of font from a simplified description as a
    family name, slant and weight.
  @end{short}
  Cairo provides no operation to list available family names on the system,
  this is a \"toy\", remember, but the standard CSS2 generic family names,
  \"serif\", \"sans-serif\", \"cursive\", \"fantasy\", \"monospace\", are
  likely to work as expected.

  If family starts with the string \"cairo:\", or if no native font backends
  are compiled in, Cairo will use an internal font family. The internal font
  family recognizes many modifiers in the family string, most notably, it
  recognizes the string \"monospace\". That is, the family name
  \"cairo:monospace\" will use the monospace version of the internal font
  family.

  For \"real\" font selection, see the font-backend-specific
  @code{cairo:font-face-create} functions for the font backend you are using.
  For example, if you are using the freetype-based @code{ft} font backend,
  see the @code{cairo:ft-font-face-create-for-ft-face} or
  @code{cairo:ft-font-face-create-for-pattern} functions. The resulting font
  face could then be used with the @fun{cairo:scaled-font-create} and
  @fun{cairo:scaled-font} functions.

  Similarly, when using the \"real\" font support, you can call directly into
  the underlying font system, such as fontconfig or freetype, for operations
  such as listing available fonts, etc.

  It is expected that most applications will need to use a more comprehensive
  font handling and text layout library, for example Pango, in conjunction
  with Cairo.

  If text is drawn without a call to the @sym{cairo:select-font-face} function,
  nor the @fun{cairo:font-face} function nor the @fun{cairo:scaled-font}
  function, the default family is platform-specific, but is essentially
  \"sans-serif\". Default slant is @code{:normal}, and default weight is
  @code{:normal}.

  This function is equivalent to a call to the @fun{cairo:toy-font-face-create}
  function followed by the @fun{cairo:font-face} function.
  @begin[Note]{dictionary}
    The @sym{cairo:select-font-face} function is part of what the Cairo
    designers call the \"toy\" text API. It is convenient for short demos
    and simple programs, but it is not expected to be adequate for serious
    text-using applications.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:font-slant-t}
  @see-symbol{cairo:font-weight-t}
  @see-function{cairo:scaled-font}
  @see-function{cairo:scaled-font-create}
  @see-function{cairo:toy-font-face-create}
  @see-function{cairo:font-face}"
  (%select-font-face cr family slant weight))

(export 'select-font-face)

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_set_font_size" %set-font-size) :void
  (cr (:pointer (:struct context-t)))
  (size :double))

(defun set-font-size (cr size)
 #+liber-documentation
 "@version{2023-1-14}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[size]{a number coerced to a double float with the new font size,
    in user space units}
  @begin{short}
    Sets the current font matrix to a scale by a factor of @arg{size},
    replacing any font matrix previously set with the @sym{cairo:set-font-size}
    or @fun{cairo:set-font-matrix} functions.
  @end{short}
  This results in a font size of @arg{size} user space units. More precisely,
  this matrix will result in the em-square of the font being a @arg{size} by
  @arg{size} square in user space.

  If text is drawn without a call to the @sym{cairo:set-font-size} function,
  nor the @fun{cairo:set-font-matrix} or @fun{cairo:scaled-font} functions,
  the default font size is 10.0.
  @see-symbol{cairo:context-t}
  @see-function{cairo:set-font-matrix}
  @see-function{cairo:scaled-font}"
  (%set-font-size cr (coerce size 'double-float)))

(export 'set-font-size)

;;; ----------------------------------------------------------------------------
;;; cairo_set_font_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_set_font_matrix" set-font-matrix) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance describing a transform
    to be applied to the current font}
  @begin{short}
    Sets the current font matrix to @arg{matrix}.
  @end{short}
  The font matrix gives a transformation from the design space of the font (in
  this space, the em-square is 1 unit by 1 unit) to user space. Normally, a
  simple scale is used, see the @fun{cairo:set-font-size} function, but a more
  complex font matrix can be used to shear the font or stretch it unequally
  along the two axes.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:set-font-size}"
  (cr (:pointer (:struct context-t)))
  (matrix (:pointer (:struct cairo:matrix-t))))

(export 'set-font-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_get_font_matrix" get-font-matrix) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance for the value of the
    matrix}
  @begin{short}
    Stores the current font matrix into @arg{matrix}.
  @end{short}
  See the @fun{cairo:set-font-matrix} function.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:set-font-matrix}"
  (cr (:pointer (:struct context-t)))
  (matrix (:pointer (:struct cairo:matrix-t))))

(export 'get-font-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_options ()
;;; cairo_set_font_options () -> font-options
;;; ----------------------------------------------------------------------------

(defun (setf font-options) (options cr)
  (cffi:foreign-funcall "cairo_set_font_options"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct font-options-t))
                          (if options options (cffi:null-pointer))
                        :void)
    options)

(cffi:defcfun ("cairo_get_font_options" %get-font-options) :void
  (cr (:pointer (:struct context-t)))
  (options (:pointer (:struct font-options-t))))

(defun font-options (cr)
 #+liber-documentation
 "@version{#2021-12-12}
  @syntax[]{(font-options cr) => options}
  @syntax[]{(setf (font-options cr) options)}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[options]{a @symbol{cairo:font-options-t} instance}
  @begin{short}
    The @sym{cairo:font-options} function retrieves font rendering options.
  @end{short}
  The @sym{(setf font-options)} function sets a set of custom font
  rendering options for the Cairo context.

  Note that the returned font options do not include any font options derived
  from the underlying surface. They are literally the font options passed to
  the @sym{(setf font-options)} function.

  Rendering font options are derived by merging these @arg{options} with the
  options derived from underlying surface. If the value in @arg{options} has a
  default value, like @code{:default}, then the value from the surface is used.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:font-options-t}"
  (cffi:with-foreign-object (options '(:struct font-options-t))
    (%get-font-options cr options)
    (unless (cffi:null-pointer-p options)
      options)))

(export 'font-options)

;;; ----------------------------------------------------------------------------
;;; cairo_get_font_face ()
;;; cairo_set_font_face () -> font-face
;;; ----------------------------------------------------------------------------

(defun (setf font-face) (face cr)
  (cffi:foreign-funcall "cairo_set_font_face"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct font-face-t)) face
                        :void)
  face)

(cffi:defcfun ("cairo_get_font_face" font-face) (:pointer (:struct font-face-t))
 #+liber-documentation
 "@version{#2021-12-12}
  @syntax[]{(cairo:font-face cr) => face}
  @syntax[]{(setf (cairo:font-face cr) face)}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[face]{a @symbol{cairo:font-face-t} instance, or @code{nil} to
    restore to the default font}
  @begin{short}
    The @sym{cairo:font-face} function gets the current font face for a Cairo
    context.
  @end{short}
  The @sym{(setf cairo:font-face)} function replaces the current
  @symbol{cairo:font-face-t} instance in the Cairo context with @arg{face}.

  The replaced font face in the Cairo context will be destroyed if there are
  no other references to it.

  This object is owned by Cairo. To keep a reference to it, you must call the
  @fun{cairo:font-face-reference} function. This function never returns NULL.
  If memory cannot be allocated, a special \"nil\" @symbol{cairo:font-face-t}
  instance will be returned on which the @fun{cairo:font-face-status} function
  returns @code{:no-memory}. Using this nil instance will cause its error state
  to propagate to other objects it is passed to, for example, calling the
  @fun{cairo:font-face} function with a nil font will trigger an error that
  will shutdown the Cairo context.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:font-face-t}
  @see-function{cairo:font-face-reference}
  @see-function{cairo:font-face-status}"
  (cr (:pointer (:struct context-t))))

(export 'font-face)

;;; ----------------------------------------------------------------------------
;;; cairo_get_scaled_font ()
;;; cairo_set_scaled_font () -> scaled-font
;;; ----------------------------------------------------------------------------

(defun (setf scaled-font) (font cr)
  (cffi:foreign-funcall "cairo_set_scaled_font"
                        (:pointer (:struct context-t)) cr
                        (:pointer (:struct scaled-font-t)) font
                        :void)
  font)

(cffi:defcfun ("cairo_get_scaled_font" scaled-font)
    (:pointer (:struct scaled-font-t))
 #+liber-documentation
 "@version{#2021-12-12}
  @syntax[]{(cairo:scaled-font cr) => font}
  @syntax[]{(setf (cairo:scaled-font cr) font)}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @begin{short}
    The @sym{cairo:scaled-font} function gets the current scaled font for the
    Cairo context.
  @end{short}
  The @sym{(setf cairo:scaled-font)} function replaces the current font face,
  font matrix, and font options in the Cairo context with those of the
  @symbol{cairo:scaled-font-t} instance.

  Except for some translation, the current CTM of the Cairo context should
  be the same as that of the @symbol{cairo:scaled-font-t} instance, which can
  be accessed using the @fun{cairo:scaled-font-get-ctm} function.

  This object is owned by Cairo. To keep a reference to it, you must call the
  @fun{cairo:scaled-font-reference} function. This function never returns NULL.
  If memory cannot be allocated, a special \"nil\" @symbol{cairo:scaled-font-t}
  instance will be returned on which the @fun{cairo:scaled-font-status} function
  returns @code{:no-memory}. Using this nil instance will cause its error state
  to propagate to other objects it is passed to, for example, calling the
  @fun{cairo:scaled-font} function with a nil font will trigger an error that
  will shutdown the Cairo context.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:scaled-font-t}
  @see-function{cairo:scaled-font-reference}
  @see-function{cairo:scaled-font-status}
  @see-function{cairo:scaled-font}"
  (cr (:pointer (:struct context-t))))

(export 'scaled-font)

;;; ----------------------------------------------------------------------------
;;; cairo_show_text ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_show_text" %show-text) :void
  (cr (:pointer (:struct context-t)))
  (utf8 :string))

(defun show-text (cr utf8)
 #+liber-documentation
 "@version{2023-1-15}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[utf8]{a string of text encoded in UTF-8, or @code{nil}}
  @begin{short}
    A drawing operator that generates the shape from a string of UTF-8
    characters, rendered according to the current font face, font size
    (font matrix), and font options.
  @end{short}

  This function first computes a set of glyphs for the string of text. The
  first glyph is placed so that its origin is at the current point. The origin
  of each subsequent glyph is offset from that of the previous glyph by the
  advance values of the previous glyph.

  After this call the current point is moved to the origin of where the next
  glyph would be placed in this same progression. That is, the current point
  will be at the origin of the final glyph offset by its advance values. This
  allows for easy display of a single logical string with multiple calls to
  the @sym{cairo:show-text} function.
  @begin[Note]{dictionary}
    The @sym{cairo:show-text} function is part of what the Cairo designers call
    the \"toy\" text API. It is convenient for short demos and simple programs,
    but it is not expected to be adequate for serious text-using applications.
    See the @fun{cairo:show-glyphs} function for the \"real\" text display API
    in Cairo.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-function{cairo:show-glyphs}"
  (%show-text cr (if utf8 utf8 (cffi:null-pointer))))

(export 'show-text)

;;; ----------------------------------------------------------------------------
;;; cairo_show_glyphs ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_show_glyphs" %show-glyphs) :void
  (cr (:pointer (:struct context-t)))
  (glyphs (:pointer (:struct glyph-t)))
  (num-glyphs :int))

(defun show-glyphs (cr glyphs)
 #+liber-documentation
 "@version{2023-1-15}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[glyphs]{a list of glyphs to show}
  @begin{short}
    A drawing operator that generates the shape from a list of glyphs,
    rendered according to the current font face, font size (font matrix), and
    font options.
  @end{short}
  @see-symbol{cairo:context-t}"
  (let ((num-glyphs (length glyphs)))
    (cffi:with-foreign-object (glyphs-ptr '(:struct glyph-t) num-glyphs)
      (loop for count from 0 below num-glyphs
            for glyph in glyphs
            for glyph-ptr = (cffi:mem-aptr glyphs-ptr '(:struct glyph-t) count)
            do (setf (cffi:foreign-slot-value glyph-ptr
                                              '(:struct glyph-t)
                                              'index)
                     (first glyph)
                     (cffi:foreign-slot-value glyph-ptr
                                              '(:struct glyph-t)
                                              'x)
                     (coerce (second glyph) 'double-float)
                     (cffi:foreign-slot-value glyph-ptr
                                              '(:struct glyph-t)
                                              'y)
                     (coerce (third glyph) 'double-float)))
      (%show-glyphs cr glyphs-ptr num-glyphs))))

(export 'show-glyphs)

;;; ----------------------------------------------------------------------------
;;; cairo_show_text_glyphs ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_show_text_glyphs" show-text-glyphs) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[utf8]{a string of text encoded in UTF-8}
  @argument[len]{an integer with the length of @arg{utf8} in bytes, or -1 if it
    is NUL-terminated}
  @argument[glyphs]{array of @symbol{cairo:glyph-t} to show}
  @argument[num-glyphs]{an integer with the number of glyphs to show}
  @argument[clusters]{array of @symbol{cairo:text-cluster-t} cluster mapping
    information}
  @argument[num-clusters]{an integer with the number of clusters in the mapping}
  @argument[flags]{@symbol{cairo:text-cluster-flags-t} cluster mapping flags}
  @begin{short}
    This operation has rendering effects similar to the @fun{cairo:show-glyphs}
    function but, if the target surface supports it, uses the provided text and
    cluster mapping to embed the text for the glyphs shown in the output.
  @end{short}
  If the target does not support the extended attributes, this function acts
  like the basic the @fun{cairo:show-glyphs} function as if it had been passed
  @arg{glyphs} and @arg{num-glyphs}.

  The mapping between UTF-8 and glyphs is provided by an array of clusters.
  Each cluster covers a number of text bytes and glyphs, and neighboring
  clusters cover neighboring areas of UTF-8 and glyphs. The clusters should
  collectively cover UTF-8 and glyphs in entirety.

  The first cluster always covers bytes from the beginning of UTF-8. If
  @arg{flags} do not have the @code{:backward} set, the first cluster also
  covers the beginning of glyphs, otherwise it covers the end of the glyphs
  array and following clusters move backward.

  See the @symbol{cairo:text-cluster-t} structure for constraints on valid
  clusters.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:glyph-t}
  @see-symbol{cairo:text-cluster-t}
  @see-symbol{cairo:text-cluster-flags-t}
  @see-function{cairo:show-glyphs}"
  (cr (:pointer (:struct context-t)))
  (utf8 :string)
  (len :int)
  (glyphs (:pointer (:struct glyph-t)))
  (num-glyphs :int)
  (clusters (:pointer (:struct text-cluster-t)))
  (num-clusters :int)
  (flags text-cluster-flags-t))

(export 'show-text-glyphs)

;;; ----------------------------------------------------------------------------
;;; cairo_font_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_font_extents" %font-extents) :void
  (cr (:pointer (:struct context-t)))
  (extents (:pointer (:struct font-extents-t))))

(defun font-extents (cr)
 #+liber-documentation
 "@version{2023-1-15}
  @syntax[]{(cairo:font-extents cr) => ascent, descent, height, max-x-advance,
    max-y-advance}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @return{The double float values of the @symbol{cairo:font-extents-t}
    instance.}
  @begin{short}
    Gets the font extents for the currently selected font.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:font-extents-t}"
  (cffi:with-foreign-object (extents '(:struct font-extents-t))
    (%font-extents cr extents)
    (cffi:with-foreign-slots ((ascent
                               descent
                               height
                               max-x-advance
                               max-y-advance) extents (:struct font-extents-t))
    (values ascent descent height max-x-advance max-y-advance))))

(export 'font-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_text_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_text_extents" %text-extents) :void
  (cr (:pointer (:struct context-t)))
  (utf8 :string)
  (extents (:pointer (:struct text-extents-t))))

(defun text-extents (cr utf8)
 #+liber-documentation
 "@version{2023-1-15}
  @syntax[]{(cairo:text-extents cr utf8) => x-bearing, y-bearing, width, height
    x-advance, y-advance}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[utf8]{a string of text encoded in UTF-8}
  @begin{return}
    The double float values of the @symbol{cairo:text-extents-t} instance with
    the extents of @arg{utf8}.
  @end{return}
  @begin{short}
    Gets the extents for a string of text.
  @end{short}
  The extents describe a user-space rectangle that encloses the \"inked\"
  portion of the text, as it would be drawn by the @fun{cairo:show-text}
  function. Additionally, the @arg{x-advance} and @arg{y-advance} values
  indicate the amount by which the current point would be advanced by the
  @fun{cairo:show-text} function.

  Note that whitespace characters do not directly contribute to the size of
  the rectangle @arg{width} and @arg{height} values. They do contribute
  indirectly by changing the position of non-whitespace characters. In
  particular, trailing whitespace characters are likely to not affect the size
  of the rectangle, though they will affect the @arg{x-advance} and
  @arg{y-advance} values.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:text-extents-t}
  @see-function{cairo:show-text}"
  (cffi:with-foreign-object (extents '(:struct text-extents-t))
    (%text-extents cr utf8 extents)
    (cffi:with-foreign-slots ((x-bearing
                               y-bearing
                               width
                               height
                               x-advance
                               y-advance) extents (:struct text-extents-t))
    (values x-bearing y-bearing width height x-advance y-advance))))

(export 'text-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_glyph_extents" %glyph-extents) :void
  (cr (:pointer (:struct context-t)))
  (glyphs (:pointer (:struct glyph-t)))
  (num-glyphs :int)
  (extents (:pointer (:struct text-extents-t))))

(defun glyph-extents (cr glyphs)
 #+liber-documentation
 "@version{2023-1-15}
  @syntax[]{(cairo:glyph-extents cr glyphs) => x-bearing, y-bearing, width,
    height, x-advance, y-advance}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[glyphs]{a list of glyphs of the form @code{'((index1 x1 y1)
    (index2 x2 y2) ...)}}
  @begin{return}
    The double float values of the @symbol{cairo:text-extents-t} instance with
    the extents of @arg{glyphs}.
  @end{return}
  @begin{short}
    Gets the extents for a list of glyphs.
  @end{short}
  The extents describe a user-space rectangle that encloses the \"inked\"
  portion of the glyphs, as they would be drawn by the @fun{cairo:show-glyphs}
  function. Additionally, the @arg{x-advance} and @arg{y-advance} values
  indicate the amount by which the current point would be advanced by the
  @fun{cairo:show-glyphs} function.

  Note that whitespace glyphs do not contribute to the size of the rectangle
  @arg{width} and @arg{height} values.
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:glyph-t}
  @see-symbol{cairo:text-extents-t}
  @see-function{cairo:show-glyphs}"
  (let ((num-glyphs (length glyphs)))
    (cffi:with-foreign-objects ((extents '(:struct text-extents-t))
                                (glyphs-ptr '(:struct glyph-t) num-glyphs))
      (loop for count from 0 below num-glyphs
            for glyph-ptr = (cffi:mem-aptr glyphs-ptr '(:struct glyph-t) count)
            for glyph in glyphs
            do (setf (cffi:foreign-slot-value glyph-ptr
                                              '(:struct glyph-t)
                                              'cairo::index)
                     (first glyph)
                     (cffi:foreign-slot-value glyph-ptr
                                              '(:struct glyph-t)
                                              'cairo::x)
                     (coerce (second glyph) 'double-float)
                     (cffi:foreign-slot-value glyph-ptr
                                              '(:struct glyph-t)
                                              'cairo::y)
                     (coerce (third glyph) 'double-float)))
      (%glyph-extents cr glyphs-ptr num-glyphs extents)
      (cffi:with-foreign-slots ((x-bearing
                                 y-bearing
                                 width
                                 height
                                 x-advance
                                 y-advance) extents (:struct text-extents-t))
      (values x-bearing y-bearing width height x-advance y-advance)))))

(export 'glyph-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_create ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_toy_font_face_create" toy-font-face-create)
    (:pointer (:struct font-face-t))
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[familiy]{a string with the font family name, encoded in UTF-8}
  @argument[slant]{a @symbol{cairo:font-slant-t} slant for the font}
  @argument[weight]{a @symbol{cairo:font-weight-t}weight for the font}
  @begin{return}
    A newly created @symbol{cairo:font-face-t} instance. Free with the
    @fun{cairo:font-face-destroy} function when you are done using it.
  @end{return}
  @begin{short}
    Creates a font face from a triplet of family, slant, and weight.
  @end{short}
  These font faces are used in implementation of the the Cairo \"toy\" font API.

  If @arg{family} is the zero-length string \"\", the platform-specific default
  family is assumed. The default family then can be queried using the
  @fun{cairo:toy-font-face-family} function.

  The @fun{cairo:select-font-face} function uses this to create font faces. See
  that function for limitations and other details of toy font faces.
  @see-symbol{cairo:font-face-t}
  @see-symbol{cairo:font-slant-t}
  @see-symbol{cairo:font-weight-t}
  @see-function{cairo:font-face-destroy}
  @see-function{cairo:toy-font-face-family}
  @see-function{cairo:select-font-face}"
  (family :string)
  (slant font-slant-t)
  (weight font-weight-t))

(export 'toy-font-face-create)

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_family ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_toy_font_face_get_family" toy-font-face-family) :string
 #+liber-documentation
 "@version{2023-7-21}
  @argument[face]{a @symbol{cairo:font-face-t} instance}
  @return{A string with the family name.}
  @begin{short}
    Gets the family name of a toy font face.
  @end{short}
  @see-symbol{cairo:font-face-t}"
  (face (:pointer (:struct font-face-t))))

(export 'toy-font-face-family)

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_slant ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_toy_font_face_get_slant" toy-font-face-slant) font-slant-t
 #+liber-documentation
 "@version{2023-7-21}
  @argument[face]{a @symbol{cairo:font-face-t} instance}
  @return{The @symbol{cairo:font-slant-t} slant value.}
  @short{Gets the slant a toy font face.}
  @see-symbol{cairo:font-face-t}
  @see-symbol{cairo:font-slant-t}"
  (face (:pointer (:struct font-face-t))))

(export 'toy-font-face-slant)

;;; ----------------------------------------------------------------------------
;;; cairo_toy_font_face_get_weight ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_toy_font_face_get_weight" toy-font-face-weight)
    font-weight-t
 #+liber-documentation
 "@version{2023-7-21}
  @argument[face]{a @symbol{cairo:font-face-t} instance}
  @return{The @symbol{cairo:font-weight-t} weight value.}
  @short{Gets the weight a toy font face.}
  @see-symbol{cairo:font-face-t}
  @see-symbol{cairo:font-weight-t}"
  (face (:pointer (:struct font-face-t))))

(export 'toy-font-face-weight)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_allocate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_glyph_allocate" glyph-allocate)
    (:pointer (:struct glyph-t))
 #+liber-documentation
 "@version{#2023-1-15}
  @argument[num]{an integer with the number of glyphs to allocate}
  @begin{return}
    The newly allocated array of @symbol{cairo:glyph-t} glyphs that should be
    freed using the @fun{cairo:glyph-free} function.
  @end{return}
  @begin{short}
    Allocates an array of @symbol{cairo:glyph-t} instances.
  @end{short}
  This function is only useful in implementations of
  @code{user-scaled-font-text-to-glyphs-func-t} function where the user needs
  to a allocate an array of glyphs that Cairo will free. For all other uses,
  user can use their own allocation method for glyphs.

  This function returns NULL if @arg{num} is not positive, or if out of
  memory. That means, the NULL return value signals out-of-memory only if
  @arg{num} was positive.
  @see-symbol{cairo:glyph-t}
  @see-function{cairo:glyph-free}"
  (num :int))

(export 'glyph-allocate)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_glyph_free" glyph-free) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[glyphs]{array of @symbol{cairo:glyph-t} glyphs to free, or NULL}
  @begin{short}
    Frees an array of @symbol{cairo:glyph-t} instances allocated using the
    @fun{cairo:glyph-allocate} function.
  @end{short}
  This function is only useful to free glyph array returned by the
  @fun{cairo:scaled-font-text-to-glyphs} function where Cairo returns an array
  of glyphs that the user will free. For all other uses, user can use their own
  allocation method for glyphs.
  @see-symbol{cairo:glyph-t}
  @see-function{cairo:glyph-allocate}
  @see-function{cairo:scaled-font-text-to-glyphs}"
  (glyphs (:pointer (:struct glyph-t))))

(export 'glyph-free)

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_allocate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_text_cluster_allocate" text-cluster-allocate)
    (:pointer (:struct text-cluster-t))
 #+liber-documentation
 "@version{#2023-1-15}
  @argument[num]{an integer with the number of text clusters to allocate}
  @begin{return}
    The newly allocated array of @symbol{cairo:text-cluster-t} text clusters
    that should be freed using the @fun{cairo:text-cluster-free} function.
  @end{return}
  @begin{short}
    Allocates an array of @symbol{cairo:text-cluster-t} instances.
  @end{short}
  This function is only useful in implementations of a
  @code{user-scaled-font-text-to-glyphs-func-t} function where the user needs to
  allocate an array of text clusters that Cairo will free. For all other uses,
  user can use their own allocation method for text clusters.

  This function returns NULL if @arg{num} is not positive, or if out
  of memory. That means, the NULL return value signals out-of-memory only if
  @arg{num} was positive.
  @see-symbol{cairo:text-cluster-t}
  @see-function{cairo:text-cluster-free}"
  (num :int))

(export 'text-cluster-allocate)

;;; ----------------------------------------------------------------------------
;;; cairo_text_cluster_free ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("cairo_text_cluster_free" text-cluster-free) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[clusters]{array of @symbol{cairo:text-cluster-t} text clusters to
    free, or NULL}
  @begin{short}
    Frees an array of @symbol{cairo:text-cluster-t} instances allocated using
    the @fun{cairo:text-cluster-allocate} function.
  @end{short}
  This function is only useful to free text cluster array returned by the
  @fun{cairo:scaled-font-text-to-glyphs} function where Cairo returns an array
  of text clusters that the user will free. For all other uses, user can use
  their own allocation method for text clusters.
  @see-symbol{cairo:text-cluster-t}
  @see-function{cairo:text-cluster-allocate}
  @see-function{cairo:scaled-font-text-to-glyphs}"
  (clusters (:pointer (:struct text-cluster-t))))

(export 'text-cluster-free)

;;; --- End of file cairo.text.lisp --------------------------------------------
