(in-package :cairo-test)

(def-suite cairo-scaled-font :in cairo-suite)
(in-suite cairo-scaled-font)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_scaled_font_t
;;;     cairo_font_extents_t
;;;     cairo_text_extents_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_scaled_font_create
;;;     cairo_scaled_font_reference
;;;     cairo_scaled_font_destroy
;;;     cairo_scaled_font_status

;;;     cairo_scaled_font_extents

(test scaled-font-extents
  (with-cairo-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (let ((font (cairo:scaled-font context)))
      (multiple-value-bind (ascent descent height max-x-advance max-y-advance)
          (cairo:scaled-font-extents font)
        ;; Check the values
        (is (approx-equal 17.0 ascent))
        (is (approx-equal  5.0 descent))
        (is (approx-equal 21.0 height))
        (is (approx-equal 34.0 max-x-advance))
        (is (approx-equal  0.0 max-y-advance))))))

;;;     cairo_scaled_font_text_extents

(test scaled-font-text-extents
  (with-cairo-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cairo:scaled-font-text-extents (cairo:scaled-font context) "Crategus")
      ;; Check the returned values
      (is (approx-equal   1.0 x-bearing))
      (is (approx-equal -13.0 y-bearing))
      (is (approx-equal  79.0 width))
      (is (approx-equal  17.0 height))
      (is (approx-equal  80.0 x-advance))
      (is (approx-equal   0.0 y-advance)))))

;;;     cairo_scaled_font_glyph_extents

(test scaled-font-glyph-extents
  (with-cairo-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (let ((font (cairo:scaled-font context)))
      (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
          (cairo:scaled-font-glyph-extents font '((36 10 20)))
      ;; Check the returned values
      (is (approx-equal  -0.0 x-bearing))
      (is (approx-equal  -8.0 y-bearing))
      (is (approx-equal   7.0 width))
      (is (approx-equal   8.0 height))
      (is (approx-equal   7.0 x-advance))
      (is (approx-equal   0.0 y-advance))))))

;;;     cairo_scaled_font_text_to_glyphs

(test scaled-font-text-to-glyphs
  (with-cairo-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (let ((font (cairo:scaled-font context)))
      (is (equal '((38  0.0d0 0.0d0)
                   (85 13.0d0 0.0d0)
                   (68 20.0d0 0.0d0)
                   (87 31.0d0 0.0d0)
                   (72 38.0d0 0.0d0)
                   (74 49.0d0 0.0d0)
                   (88 60.0d0 0.0d0)
                   (86 71.0d0 0.0d0))
                 (cairo:scaled-font-text-to-glyphs font 0 0 "Crategus"))))))

;;;     cairo_scaled_font_get_font_face
;;;     cairo_scaled_font_get_font_options
;;;     cairo_scaled_font_get_font_matrix
;;;     cairo_scaled_font_get_ctm
;;;     cairo_scaled_font_get_scale_matrix
;;;     cairo_scaled_font_get_type
;;;     cairo_scaled_font_get_reference_count
;;;     cairo_scaled_font_set_user_data
;;;     cairo_scaled_font_get_user_data

;;; --- 2023-1-15 --------------------------------------------------------------
