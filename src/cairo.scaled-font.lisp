;;; ----------------------------------------------------------------------------
;;; cairo.scaled-font.lisp
;;;
;;; The documentation of the file is taken from the Cairo Reference Manual
;;; Version 1.16 and modified to document the Lisp binding to the Cairo
;;; library. See <http://cairographics.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;;
;;; cairo_scaled_font_t
;;;
;;;     Font face at particular size and options
;;;
;;; Types and Values
;;;
;;;     cairo_glyph_t                            <-- cairo.text.lisp
;;;     cairo_scaled_font_t
;;;     cairo_font_extents_t
;;;     cairo_text_extents_t
;;;
;;; Functions
;;;
;;;     cairo_scaled_font_create
;;;     cairo_scaled_font_reference
;;;     cairo_scaled_font_destroy
;;;     cairo_scaled_font_status
;;;
;;;     cairo_scaled_font_extents
;;;     cairo_scaled_font_text_extents
;;;     cairo_scaled_font_glyph_extents
;;;     cairo_scaled_font_text_to_glyphs
;;;     cairo_scaled_font_get_font_face
;;;     cairo_scaled_font_get_font_options
;;;     cairo_scaled_font_get_font_matrix
;;;     cairo_scaled_font_get_ctm
;;;     cairo_scaled_font_get_scale_matrix
;;;     cairo_scaled_font_get_type
;;;     cairo_scaled_font_get_reference_count
;;;     cairo_scaled_font_set_user_data
;;;     cairo_scaled_font_get_user_data
;;;
;;; Description
;;;
;;;     cairo_scaled_font_t represents a realization of a font face at a
;;;     particular size and transformation and a certain set of font options.
;;; ----------------------------------------------------------------------------

(in-package :cairo)

;;; ----------------------------------------------------------------------------
;;; cairo_glyph_t
;;; ----------------------------------------------------------------------------

(defcstruct glyph-t
  (index :ulong)
  (x :double)
  (y :double))

#+liber-documentation
(setf (liber:alias-for-symbol 'glyph-t)
      "CStruct"
      (liber:symbol-documentation 'glyph-t)
 "@version{#2021-12-12}
  @begin{short}
    The @sym{cairo:glyph-t} structure holds information about a single glyph when
    drawing or measuring text.
  @end{short}
  A font is (in simple terms) a collection of shapes used to draw text. A glyph
  is one of these shapes. There can be multiple glyphs for a single character
  (alternates to be used in different contexts, for example), or a glyph can be
  a ligature of multiple characters. Cairo does not expose any way of converting
  input text into glyphs, so in order to use the Cairo interfaces that take
  arrays of glyphs, you must directly access the appropriate underlying font
  system.

  Note that the offsets given by x and y are not cumulative. When drawing or
  measuring text, each glyph is individually positioned with respect to the
  overall origin.
  @begin{pre}
(defcstruct glyph-t
  (index :ulong)
  (x :double)
  (y :double))
  @end{pre}
  @begin[code]{table}
    @entry[index]{Glyph index in the font. The exact interpretation of the
      glyph index depends on the font technology being used.}
    @entry[x]{The offset in the x direction between the origin used for drawing
      or measuring the string and the origin of this glyph.}
    @entry[y]{The offset in the y direction between the origin used for drawing
      or measuring the string and the origin of this glyph.}
  @end{table}
  @see-function{cairo:glyph-path}
  @see-function{cairo:show-glyphs}")

(export 'glyph-t)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_t
;;; ----------------------------------------------------------------------------

(defcstruct scaled-font-t)

#+liber-documentation
(setf (liber:alias-for-symbol 'scaled-font-t)
      "CStruct"
      (liber:symbol-documentation 'scaled-font-t)
 "@version{#2023-1-13}
  @begin{short}
    A @sym{cairo:scaled-font-t} structure is a font scaled to a particular size
    and device resolution.
  @end{short}
  A @sym{cairo:scaled-font-t} structure is most useful for low-level font usage
  where a library or application wants to cache a reference to a scaled font to
  speed up the computation of metrics.

  There are various types of scaled fonts, depending on the font backend they
  use. The type of a scaled font can be queried using the
  @fun{cairo:scaled-font-type} function.

  Memory management of the @sym{cairo:scaled-font-t} structure is done with the
  @fun{cairo:scaled-font-reference} and @fun{cairo:scaled-font-destroy}
  functions.
  @see-function{cairo:scaled-font-type}
  @see-function{cairo:scaled-font-reference}
  @see-function{cairo:scaled-font-destroy}")

(export 'scaled-font-t)

;;; ----------------------------------------------------------------------------
;;; cairo_font_extents_t
;;; ----------------------------------------------------------------------------

(defcstruct font-extents-t
  (ascent :double)
  (descent :double)
  (height :double)
  (max-x-advance :double)
  (max-y-advance :double))

#+liber-documentation
(setf (liber:alias-for-symbol 'font-extents-t)
      "CStruct"
      (liber:symbol-documentation 'font-extents-t)
 "@version{2023-1-15}
  @begin{short}
    The @sym{cairo:font-extents-t} structure stores metric information for a
    font.
  @end{short}
  Values are given in the current user-space coordinate system. Because font
  metrics are in user-space coordinates, they are mostly, but not entirely,
  independent of the current transformation matrix. If you call
  @code{(cairo:scale cr 2.0 2.0)}, text will be drawn twice as big, but the
  reported text extents will not be doubled. They will change slightly due to
  hinting, so you can not assume that metrics are independent of the
  transformation matrix), but otherwise will remain unchanged.
  @begin{pre}
(defcstruct font-extents-t
  (ascent :double)
  (descent :double)
  (height :double)
  (max-x-advance :double)
  (max-y-advance :double))
  @end{pre}
  @begin[code]{table}
    @entry[ascent]{The distance that the font extends above the baseline. Note
      that this is not always exactly equal to the maximum of the extents of
      all the glyphs in the font, but rather is picked to express the font
      designer's intent as to how the font should align with elements above it.}
    @entry[descent]{The distance that the font extends below the baseline. This
      value is positive for typical fonts that include portions below the
      baseline. Note that this is not always exactly equal to the maximum of
      the extents of all the glyphs in the font, but rather is picked to express
      the font designer's intent as to how the font should align with elements
      below it.}
    @entry[height]{The recommended vertical distance between baselines when
      setting consecutive lines of text with the font. This is greater than
      @code{ascent} + @code{descent} by a quantity known as the line spacing or
      external leading. When space is at a premium, most fonts can be set with
      only a distance of @code{ascent} + @code{descent} between lines.}
    @entry[max-x-advance]{The maximum distance in the x direction that the
      origin is advanced for any glyph in the font.}
    @entry[max-y-advance]{The maximum distance in the y direction that the
      origin is advanced for any glyph in the font. This will be zero for normal
      fonts used for horizontal writing. The scripts of East Asia are sometimes
      written vertically.}
  @end{table}
  @see-function{cairo:scale}
  @see-function{cairo:font-extents}")

(export 'font-extents-t)

;;; ----------------------------------------------------------------------------
;;; cairo_text_extents_t
;;; ----------------------------------------------------------------------------

(defcstruct text-extents-t
  (x-bearing :double)
  (y-bearing :double)
  (width :double)
  (height :double)
  (x-advance :double)
  (y-advance :double))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-extents-t)
      "CStruct"
      (liber:symbol-documentation 'text-extents-t)
 "@version{#2023-1-13}
  @begin{short}
    The @sym{cairo:text-extents-t} structure stores the extents of a single
    glyph or a string of glyphs in user-space coordinates.
  @end{short}
  Because text extents are in user-space coordinates, they are mostly, but not
  entirely, independent of the current transformation matrix. If you call
  @code{(scale cr 2.0 2.0)}, text will be drawn twice as big, but the
  reported text extents will not be doubled. They will change slightly due to
  hinting, so you can not assume that metrics are independent of the
  transformation matrix, but otherwise will remain unchanged.
  @begin{pre}
(defcstruct text-extents-t
  (x-bearing :double)
  (y-bearing :double)
  (width :double)
  (height :double)
  (x-advance :double)
  (y-advance :double))
  @end{pre}
  @begin[code]{table}
    @entry[x-bearing]{The horizontal distance from the origin to the leftmost
      part of the glyphs as drawn. Positive if the glyphs lie entirely to the
      right of the origin.}
    @entry[y-bearing]{The vertical distance from the origin to the topmost part
      of the glyphs as drawn. Positive only if the glyphs lie completely below
      the origin; will usually be negative.}
    @entry[width]{Width of the glyphs as drawn.}
    @entry[height]{Height of the glyphs as drawn.}
    @entry[x-advance]{Distance to advance in the x direction after drawing
      these glyphs.}
    @entry[y-advance]{Distance to advance in the y direction after drawing
      these glyphs. Will typically be zero except for vertical text layout as
      found in East-Asian languages.}
  @end{table}
  @see-function{cairo:text-extents}
  @see-function{cairo:scaled-font-text-extents}")

(export 'text-extents-t)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_create" scaled-font-create)
    (:pointer (:struct scaled-font-t))
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[face]{a @symbol{cairo:font-face-t} instance}
  @argument[matrix]{a @symbol{cairo:matrix-t} matrix with the font space to
    user space transformation for the font, in the simplest case of a N point
    font, this matrix is just a scale by N, but it can also be used to shear
    the font or stretch it unequally along the two axes, see the
    @fun{cairo:set-font-matrix} function}
  @argument[ctm]{a @symbol{cairo:matrix-t} matrix with the user to device
    transformation with which the font will be used}
  @argument[options]{a @symbol{cairo:font-options-t} value with the options to
    use when getting metrics for the font and rendering with it}
  @begin{return}
    A newly created @symbol{cairo:scaled-font-t} instance. Destroy with the
    @fun{cairo:scaled-font-destroy} function.
  @end{return}
  @begin{short}
    Creates a @symbol{cairo:scaled-font-t} instance from a font face and
    matrices that describe the size of the font and the environment in which it
    will be used.
  @end{short}
  @see-symbol{cairo:font-face-t}
  @see-symbol{cairo:matrix-t}
  @see-symbol{cairo:font-options-t}
  @see-function{cairo:set-font-matrix}"
  (face (:pointer (:struct font-face-t)))
  (matrix (:pointer (:struct matrix-t)))
  (ctm (:pointer (:struct matrix-t)))
  (options (:pointer (:struct font-options-t))))

(export 'scaled-font-create)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_reference ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_reference" scaled-font-reference)
    (:pointer (:struct scaled-font-t))
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance, may be NULL in which
    case this function does nothing}
  @return{The referenced @symbol{cairo:scaled-font-t} instance.}
  @begin{short}
    Increases the reference count on @arg{font} by one.
  @end{short}
  This prevents @arg{font} from being destroyed until a matching call to the
  @fun{cairo:scaled-font-destroy} function is made. The number of references
  to a @symbol{cairo:scaled-font-t} instance can be get using the
  @fun{cairo:scaled-font-reference-count} function.
  @see-symbol{cairo:scaled-font-t}
  @see-function{cairo:scaled-font-destroy}
  @see-function{cairo:scaled-font-reference-count}"
  (font (:pointer (:struct scaled-font-t))))

(export 'scaled-font-reference)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_destroy" scaled-font-destroy) :void
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @begin{short}
    Decreases the reference count on @arg{font} by one.
  @end{short}
  If the result is zero, then @arg{font} and all associated resources are freed.
  See the @fun{cairo:scaled-font-reference} function.
  @see-symbol{cairo:scaled-font-t}
  @see-function{cairo:scaled-font-reference}"
  (font (:pointer (:struct scaled-font-t))))

(export 'scaled-font-destroy)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_status ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_status" scaled-font-status) status-t
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @begin{return}
    @code{:success} or another error such as @code{:no-memory}.
  @end{return}
  @begin{short}
    Checks whether an error has previously occurred for this scaled font.
  @end{short}
  @see-symbol{cairo:scaled-font-t}"
  (font (:pointer (:struct scaled-font-t))))

(export 'scaled-font-status)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_extents" %scaled-font-extents) :void
  (font (:pointer (:struct scaled-font-t)))
  (extents (:pointer (:struct font-extents-t))))

(defun scaled-font-extents (font)
 #+liber-documentation
 "@version{2023-1-15}
  @syntax[]{cairo:scaled-font-extents font) => ascent, descent, height,
    max-x-advance, max-y-advance}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @return{The double float values of the @symbol{cairo:font-extents-t} instance
    with the extents of @arg{font}.}
  @begin{short}
    Gets the metrics for a scaled font.
  @end{short}
  @see-symbol{cairo:scaled-font-t}
  @see-symbol{cairo:font-extents-t}"
  (with-foreign-object (extents '(:struct font-extents-t))
    (%scaled-font-extents font extents)
    (with-foreign-slots ((ascent
                          descent
                          height
                          max-x-advance
                          max-y-advance) extents (:struct font-extents-t))
    (values ascent descent height max-x-advance max-y-advance))))

(export 'scaled-font-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_text_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_text_extents" %scaled-font-text-extents) :void
  (font (:pointer (:struct scaled-font-t)))
  (utf8 :string)
  (extents (:pointer (:struct text-extents-t))))

(defun scaled-font-text-extents (font utf8)
 #+liber-documentation
 "@version{2023-1-15}
  @syntax[]{cairo:scaled-font-text-extents font utf8) => x-bearing, y-bearing,
    width, height, x-advance, y-advance}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @argument[utf8]{a string of text, encoded in UTF-8}
  @return{The double float values of the @symbol{cairo:text-extents-t} instance
    with the extents of @arg{utf8}.}
  @begin{short}
    Gets the extents for a string of text.
  @end{short}
  The extents describe a user-space rectangle that encloses the \"inked\"
  portion of the text drawn at the origin (0,0), as it would be drawn by
  the @fun{cairo:show-text} function if the Cairo graphics state were set to
  the same @code{cairo:font-face}, @code{cairo:set-font-matrix}, @code{ctm},
  and @code{cairo:font-options} as @arg{font}. Additionally, the
  @arg{x-advance} and @arg{y-advance} values indicate the amount by which the
  current point would be advanced by the @fun{cairo:show-text} function.

  Note that whitespace characters do not directly contribute to the size of
  the rectangle @arg{width} and @arg{height} values. They do contribute
  indirectly by changing the position of non-whitespace characters. In
  particular, trailing whitespace characters are likely to not affect the size
  of the rectangle, though they will affect the @arg{x-advance} and
  @arg{y-advance} values.
  @see-symbol{cairo:scaled-font-t}
  @see-type{cairo:text-extents-t}
  @see-function{cairo:show-text}"
  (with-foreign-object (extents '(:struct text-extents-t))
    (%scaled-font-text-extents font utf8 extents)
    (with-foreign-slots ((x-bearing
                          y-bearing
                          width
                          height
                          x-advance
                          y-advance) extents (:struct text-extents-t))
    (values x-bearing y-bearing width height x-advance y-advance))))

(export 'scaled-font-text-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_glyph_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_glyph_extents" %scaled-font-glyph-extents) :void
  (font (:pointer (:struct scaled-font-t)))
  (glyphs :pointer) ; (:pointer (:struct glyph-t))
  (num :int)
  (extents (:pointer (:struct text-extents-t))))

(defun scaled-font-glyph-extents (font glyphs)
 #+liber-documentation
 "@version{2023-1-15}
  @syntax[]{(cairo:scaled-font-glyph-extents font glyphs) => x-bearing,
    y-bearing, width, height, x-advance, y-advance}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @argument[glyphs]{an foreign array of glyph IDs with x and y offsets}
  @return{The double float values of the @symbol{cairo:text-extents-t} instance
    with the extents of @arg{glyphs}.}
  @begin{short}
    Gets the extents for @arg{glyphs}.
  @end{short}
  The extents describe a user-space rectangle that encloses the \"inked\"
  portion of the glyphs, as they would be drawn by the @fun{cairo:show-glyphs}
  function if the Cairo graphics state were set to the same @code{font-face},
  @code{font-matrix}, @code{ctm}, and @code{font-options} as @arg{font}.
  Additionally, the @arg{x-advance} and @arg{y-advance} values indicate the
  amount by which the current point would be advanced by the
  @fun{cairo:show-glyphs} function.

  Note that whitespace glyphs do not contribute to the size of the rectangle
  @arg{width} and @arg{height} values.
  @see-symbol{cairo:scaled-font-t}
  @see-symbol{cairo:text-extents-t}
  @see-function{cairo:show-glyphs}"
  (let ((num-glyphs (length glyphs)))
    (with-foreign-objects ((extents '(:struct text-extents-t))
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
      (%scaled-font-glyph-extents font glyphs-ptr num-glyphs extents)
      (with-foreign-slots ((x-bearing
                            y-bearing
                            width
                            height
                            x-advance
                            y-advance) extents (:struct text-extents-t))
      (values x-bearing y-bearing width height x-advance y-advance)))))

(export 'scaled-font-glyph-extents)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_text_to_glyphs ()
;;; ----------------------------------------------------------------------------

;; TODO: Implementation of clusters is missing. Udate the documentation.

(defcfun ("cairo_scaled_font_text_to_glyphs" %scaled-font-text-to-glyphs)
    status-t
  (font (:pointer (:struct scaled-font-t)))
  (x :double)
  (y :double)
  (utf8 :string)
  (len :int)
  (glyphs :pointer)
  (num-glyphs :pointer)
  (clusters :pointer)
  (num-clusters :pointer)
  (cluster-flags :pointer))

(defun scaled-font-text-to-glyphs (font x y utf8)
 #+liber-documentation
 "@version{2023-1-15}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @argument[x]{a number coerced to a double float with the x position to place
    first glyph}
  @argument[y]{a number coerced to a double float with the y position to place
    first glyph}
  @argument[utf8]{a string of text encoded in UTF8}
  @return{The list of glyphs for @arg{utf8} upon sucess, or @code{nil} if the
    input values are wrong or if conversion failed. If the input values are
    correct but the conversion failed, the error status is set on @arg{font}.}
  @begin{short}
    Converts UTF8 text to a list of glyphs, optionally with cluster mapping,
    that can be used to render later using @arg{font}.
  @end{short}
  For details of how @arg{clusters}, @arg{num-clusters}, and @arg{cluster-flags}
  map input UTF8 text to the output glyphs see the @fun{cairo:show-text-glyphs}
  function.

  The output values can be readily passed to the @fun{cairo:show-text-glyphs}
  function, the @fun{cairo:show-glyphs} function, or related functions, assuming
  that the exact same @arg{font} is used for the operation.
  @begin[Lisp implementation]{dictionary}
    Currently, cluster mapping is not implemented.
  @end{dictionary}
  @see-symbol{cairo:scaled-font-t}
  @see-function{cairo:show-glyphs}
  @see-function{cairo:show-text-glyphs}"
  (with-foreign-objects ((num :int) (glyphs :pointer))
    (when (eq :success
              (%scaled-font-text-to-glyphs font
                                           (coerce x 'double-float)
                                           (coerce y 'double-float)
                                           utf8 -1 glyphs num
                                           (cffi:null-pointer)
                                           (cffi:null-pointer)
                                           (cffi:null-pointer)))
        (loop with glyph-ptr = (cffi:mem-ref glyphs :pointer)
              for count from 0 below (cffi:mem-ref num :int)
              for glyph = (cffi:mem-aptr glyph-ptr '(:struct glyph-t) count)
              collect (with-foreign-slots ((index x y) glyph (:struct glyph-t))
                        (list index x y))))))

(export 'scaled-font-text-to-glyphs)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_font_face ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_font_face" scaled-font-font-face)
    (:pointer (:struct font-face-t))
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @return{The @symbol{cairo:font-face-t} instance with which @arg{font} was
    created. This object is owned by Cairo. To keep a reference to it, you must
    call the @fun{cairo:scaled-font-reference} function.}
  @begin{short}
    Gets the font face that this scaled font uses.
  @end{short}
  This might be the font face passed to the @fun{cairo:scaled-font-create}
  function, but this does not hold true for all possible cases.
  @see-symbol{cairo:scaled-font-t}
  @see-symbol{cairo:font-face-t}
  @see-function{cairo:scaled-font-reference}
  @see-function{cairo:scaled-font-create}"
  (font (:pointer (:struct scaled-font-t))))

(export 'scaled-font-font-face)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_font_options ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_font_options" scaled-font-font-options) :void
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @argument[options]{a @symbol{cairo-font-options-t} instance for the
    return value}
  @begin{short}
    Stores the font options with which @arg{font} was created into
    @arg{options}.
  @end{short}
  @see-symbol{cairo:scaled-font-t}
  @see-symbol{cairo:font-options-t}"
  (font (:pointer (:struct scaled-font-t)))
  (options (:pointer (:struct font-options-t))))

(export 'scaled-font-font-options)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_font_matrix ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_font_matrix" scaled-font-font-matrix) :void
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance}
  @begin{short}
    Stores the font matrix with which @arg{font} was created into @arg{matrix}.
  @end{short}
  @see-symbol{cairo:scaled-font-t}
  @see-symbol{cairo:matrix-t}"
  (font (:pointer (:struct scaled-font-t)))
  (matrix (:pointer (:struct cairo:matrix-t))))

(export 'scaled-font-font-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_ctm ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_ctm" scaled-font-ctm) :void
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @argument[ctm]{a @symbol{cairo:matrix-t} instance for the return value for the
    CTM}
  @begin{short}
    Stores the CTM with which @arg{font} was created into @arg{ctm}.
  @end{short}
  Note that the translation offsets @code{(x0, y0)} of the CTM are ignored by
  the @fun{cairo:scaled-font-create} function. So, the matrix this function
  returns always has @code{0,0} as @code{x0,y0}.
  @see-symbol{cairo:scaled-font-t}
  @see-symbol{cairo:matrix-t}
  @see-function{cairo:scaled-font-create}"
  (font (:pointer (:struct scaled-font-t)))
  (ctm (:pointer (:struct cairo:matrix-t))))

(export 'scaled-font-ctm)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_scale_matrix ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_scale_matrix" scaled-font-scale-matrix) :void
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @argument[matrix]{a @symbol{cairo:matrix-t} instance for the matrix}
  @begin{short}
    Stores the scale matrix of @arg{font} into @arg{matrix}.
  @end{short}
  The scale matrix is product of the font matrix and the CTM associated with the
  scaled font, and hence is the matrix mapping from font space to device space.
  @see-symbol{cairo:scaled-font-t}
  @see-symbol{cairo:matrix-t}"
  (font (:pointer (:struct scaled-font-t)))
  (matrix (:pointer (:struct cairo:matrix-t))))

(export 'scaled-font-scale-matrix)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_type" scaled-font-type) font-type-t
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @return{The @symbol{cairo:font-type-t} value of @arg{font}.}
  @begin{short}
    This function returns the type of the backend used to create a scaled font.
  @end{short}
  See the @symbol{cairo:font-type-t} enumeration for available types. However,
  this function never returns the @code{:toy} value.
  @see-symbol{cairo:scaled-font-t}
  @see-symbol{cairo:font-type-t}"
  (font (:pointer (:struct scaled-font-t))))

(export 'scaled-font-type)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_reference_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("cairo_scaled_font_get_reference_count"
           scaled-font-reference-count) :uint
 #+liber-documentation
 "@version{#2023-1-13}
  @argument[font]{a @symbol{cairo:scaled-font-t} instance}
  @return{An integer with the current reference count of @arg{font}. If the
    object is a nil object, 0 will be returned.}
  @begin{short}
    Returns the current reference count of @arg{font}.
  @end{short}
  @see-symbol{cairo:scaled-font-t}"
  (font (:pointer (:struct scaled-font-t))))

(export 'scaled-font-reference-count)

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_set_user_data ()
;;;
;;; cairo_status_t
;;; cairo_scaled_font_set_user_data (cairo_scaled_font_t *scaled_font,
;;;                                  const cairo_user_data_key_t *key,
;;;                                  void *user_data,
;;;                                  cairo_destroy_func_t destroy);
;;;
;;; Attach user data to scaled_font. To remove user data from a surface, call
;;; this function with the key that was used to set it and NULL for data.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; key :
;;;     the address of a cairo_user_data_key_t to attach the user data to
;;;
;;; user_data :
;;;     the user data to attach to the cairo_scaled_font_t
;;;
;;; destroy :
;;;     a cairo_destroy_func_t which will be called when the cairo_t is
;;;     destroyed or when new user data is attached using the same key.
;;;
;;; Returns :
;;;     CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a slot could not be
;;;     allocated for the user data.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; cairo_scaled_font_get_user_data ()
;;;
;;; void *
;;; cairo_scaled_font_get_user_data (cairo_scaled_font_t *scaled_font,
;;;                                  const cairo_user_data_key_t *key);
;;;
;;; Return user data previously attached to scaled_font using the specified
;;; key. If no user data has been attached with the given key this function
;;; returns NULL.
;;;
;;; scaled_font :
;;;     a cairo_scaled_font_t
;;;
;;; key :
;;;     the address of the cairo_user_data_key_t the user data was attached to
;;;
;;; Returns :
;;;     the user data previously attached or NULL.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; --- End of file cairo.scaled-font.lisp -------------------------------------
