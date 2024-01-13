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

(test cairo-scaled-font-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (let ((font (cairo:scaled-font context)))
      (multiple-value-bind (ascent descent height max-x-advance max-y-advance)
          (cairo:scaled-font-extents font)
        ;; Check the values
        (is (typep ascent 'double-float))
        (is (typep descent 'double-float))
        (is (typep height 'double-float))
        (is (typep max-x-advance 'double-float))
        (is (typep max-y-advance 'double-float))))))

#+crategus
(test cairo-scaled-font-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (let ((font (cairo:scaled-font context)))
      (multiple-value-bind (ascent descent height max-x-advance max-y-advance)
          (cairo:scaled-font-extents font)
        ;; Check the values
        (is (approx-equal 20.0 ascent))
        (is (approx-equal  6.0 descent))
        (is (approx-equal 25.0 height))
        (is (approx-equal 51.0 max-x-advance))
        (is (approx-equal  0.0 max-y-advance))))))

#+windows
(test cairo-scaled-font-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (let ((font (cairo:scaled-font context)))
      (multiple-value-bind (ascent descent height max-x-advance max-y-advance)
          (cairo:scaled-font-extents font)
        ;; Check the values
        (is (approx-equal  17.0 ascent))
        (is (approx-equal   4.0 descent))
        (is (approx-equal  21.0 height))
        (is (approx-equal 252.0 max-x-advance))
        (is (approx-equal   0.0 max-y-advance))))))

;;;     cairo_scaled_font_text_extents

#-windows
(test cairo-scaled-font-text-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
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

#+windows
(test cairo-scaled-font-text-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cairo:scaled-font-text-extents (cairo:scaled-font context) "Crategus")
      ;; Check the returned values
      (is (approx-equal  -0.1 x-bearing))
      (is (approx-equal -13.0 y-bearing))
      (is (approx-equal  79.0 width))
      (is (approx-equal  18.9 height))
      (is (approx-equal  80.0 x-advance))
      (is (approx-equal   0.0 y-advance)))))

;;;     cairo_scaled_font_glyph_extents

(test cairo-scaled-font-glyph-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (let ((font (cairo:scaled-font context)))
      (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
          (cairo:scaled-font-glyph-extents font '((36 10 20)))
      ;; Check the returned values
      (is (typep x-bearing 'double-float))
      (is (typep y-bearing 'double-float))
      (is (typep width 'double-float))
      (is (typep height 'double-float))
      (is (typep x-advance 'double-float))
      (is (typep y-advance 'double-float))))))

#+crategus
(test cairo-scaled-font-glyph-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
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
      (is (approx-equal   6.0 x-advance))
      (is (approx-equal   0.0 y-advance))))))

#+windows
(test cairo-scaled-font-glyph-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (let ((font (cairo:scaled-font context)))
      (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
          (cairo:scaled-font-glyph-extents font '((36 10 20)))
      ;; Check the returned values
      (is (approx-equal  -1.0 x-bearing))
      (is (approx-equal  -8.2 y-bearing))
      (is (approx-equal   9.0 width))
      (is (approx-equal   9.2 height))
      (is (approx-equal   7.0 x-advance))
      (is (approx-equal   0.0 y-advance))))))

;;;     cairo_scaled_font_text_to_glyphs

(test cairo-scaled-font-text-to-glyphs
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (let ((font (cairo:scaled-font context)))
      (is (every #'numberp
                 (flatten
                   (cairo:scaled-font-text-to-glyphs font 0 0 "Crategus")))))))

;; TODO: Create a check with approx-equal
#+nil
(test cairo-scaled-font-text-to-glyphs
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (let ((font (cairo:scaled-font context)))
      (is (equal '((38  0.0d0 0.0d0)
                   (85 11.0d0 0.0d0)
                   (68 18.0d0 0.0d0)
                   (87 28.0d0 0.0d0)
                   (72 35.0d0 0.0d0)
                   (74 45.0d0 0.0d0)
                   (88 56.0d0 0.0d0)
                   (86 67.0d0 0.0d0))
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

;;; 2024-1-12
