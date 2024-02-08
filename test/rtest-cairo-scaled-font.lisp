(in-package :cairo-test)

(def-suite cairo-scaled-font-suite :in cairo-suite)
(in-suite cairo-scaled-font-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_scaled_font_t
;;;     cairo_font_extents_t
;;;     cairo_text_extents_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo:with-scaled-font

#-windows
(test cairo-with-scaled-font
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (is (string= "" (cairo:toy-font-face-family face)))
          (is (eq :normal (cairo:toy-font-face-slant face)))
          (is (eq :normal (cairo:toy-font-face-weight face))))))))

#+windows
(test cairo-with-scaled-font
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (is (string= "Arial" (cairo:toy-font-face-family face)))
          (is (eq :normal (cairo:toy-font-face-slant face)))
          (is (eq :normal (cairo:toy-font-face-weight face))))))))

;;;     cairo_scaled_font_create
;;;     cairo_scaled_font_reference
;;;     cairo_scaled_font_get_reference_count
;;;     cairo_scaled_font_destroy
;;;     cairo_scaled_font_status
;;;     cairo_scaled_font_get_type

#-windows
(test cairo-scaled-font-create
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm)
      (let* ((options (cairo:font-options-create))
             (font (cairo:scaled-font-create face matrix ctm options)))
        (is (eq :success (cairo:scaled-font-status font)))
        (is (eq :ft (cairo:scaled-font-type font)))
        (is (string= "" (cairo:toy-font-face-family face)))
        (is (eq :normal (cairo:toy-font-face-slant face)))
        (is (eq :normal (cairo:toy-font-face-weight face)))
        (is-false (cairo:scaled-font-reference nil))
        (is-false (cairo:scaled-font-reference (cffi:null-pointer)))
        ;; TODO: Do we have to destroy the font options?
        (is-false (cairo:font-options-destroy options))
        ;; TODO: Why do we have a reference count of 2?
        (is (= 2 (cairo:scaled-font-reference-count font)))
        (is-false (cairo:scaled-font-destroy font))))))

#+windows
(test cairo-scaled-font-create
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm)
      (let* ((options (cairo:font-options-create))
             (font (cairo:scaled-font-create face matrix ctm options)))
        (is (eq :success (cairo:scaled-font-status font)))
        (is (eq :dwrite (cairo:scaled-font-type font)))
        (is (string= "Arial" (cairo:toy-font-face-family face)))
        (is (eq :normal (cairo:toy-font-face-slant face)))
        (is (eq :normal (cairo:toy-font-face-weight face)))
        (is-false (cairo:scaled-font-reference nil))
        (is-false (cairo:scaled-font-reference (cffi:null-pointer)))
        ;; TODO: Do we have to destroy the font options?
        (is-false (cairo:font-options-destroy options))
        ;; TODO: Why do we have a reference count of 2?
        (is (= 2 (cairo:scaled-font-reference-count font)))
        (is-false (cairo:scaled-font-destroy font))))))

;;;     cairo_scaled_font_get_font_face

#-windows
(test cairo-scaled-font-font-face
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (is (cffi:pointer-eq face
                               (cairo:scaled-font-font-face font))))))))

#+windows
(test cairo-scaled-font-font-face
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (is (cffi:pointer-eq face
                               (cairo:scaled-font-font-face font))))))))

;;;     cairo_scaled_font_get_font_options

#-windows
(test cairo-scaled-font-font-options
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm)
      (let ((options (cairo:font-options-create))
            (options1 (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (is (cffi:pointer-eq options1
                               (cairo:scaled-font-font-options font options1)))
          (is-true (cairo:font-options-equal options options1))
          (is-false (cairo:font-options-destroy options1)))))))

#+windows
(test cairo-scaled-font-font-options
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm)
      (let ((options (cairo:font-options-create))
            (options1 (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (is (cffi:pointer-eq options1
                               (cairo:scaled-font-font-options font options1)))
          (is-true (cairo:font-options-equal options options1))
          (is-false (cairo:font-options-destroy options1)))))))

;;;     cairo_scaled_font_get_font_matrix
;;;     cairo_scaled_font_get_ctm
;;;     cairo_scaled_font_get_scale_matrix

#-windows
(test cairo-scaled-font-font-matrix/ctm/scale-matrix
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix matrix1 ctm ctm1 scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (is (cffi:pointer-eq matrix1
                               (cairo:scaled-font-font-matrix font matrix1)))
          (is (cffi:pointer-eq ctm1
                               (cairo:scaled-font-ctm font ctm1)))
          (is (cffi:pointer-eq scale
                               (cairo:scaled-font-scale-matrix font scale)))
)))))

#+windows
(test cairo-scaled-font-font-matrix/ctm/scale-matrix
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix matrix1 ctm ctm1 scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (is (cffi:pointer-eq matrix1
                               (cairo:scaled-font-font-matrix font matrix1)))
          (is (cffi:pointer-eq ctm1
                               (cairo:scaled-font-ctm font ctm1)))
          (is (cffi:pointer-eq scale
                               (cairo:scaled-font-scale-matrix font scale)))
)))))

;;;     cairo_scaled_font_extents

#-windows
(test cairo-scaled-font-extents.1
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (is (equal '(2.0d0 1.0d0 1.0d0 3.0d0 0.0d0)
                     (multiple-value-list (cairo:scaled-font-extents font)))))))))

#+windows
(test cairo-scaled-font-extents.1
  (cairo:with-toy-font-face (face "" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (is (every #'approx-equal
                     '(0.9d0 0.2d0 1.15d0 14.0d0 0.0d0)
                     (multiple-value-list (cairo:scaled-font-extents font)))))))))

#-windows
(test cairo-scaled-font-extents.2
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (is (equal '(2.0d0 1.0d0 1.0d0 3.0d0 0.0d0)
                     (multiple-value-list (cairo:scaled-font-extents font)))))))))

#+windows
(test cairo-scaled-font-extents.2
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (is (every #'approx-equal
                     '(0.9d0 0.2d0 1.15d0 14.0d0 0.0d0)
                     (multiple-value-list (cairo:scaled-font-extents font)))))))))

;;;     cairo_scaled_font_text_extents

#-windows
(test cairo-scaled-font-text-extents
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (multiple-value-bind (x-bearing y-bearing
                                width height
                                x-advance y-advance)
              (cairo:scaled-font-text-extents font "Crategus")
          ;; Check the returned values
          (is (approx-equal  0.00 x-bearing))
          (is (approx-equal -1.00 y-bearing))
          (is (approx-equal  6.00 width))
          (is (approx-equal  1.00 height))
          (is (approx-equal  5.00 x-advance))
          (is (approx-equal  0.00 y-advance))))))))

#+windows
(test cairo-scaled-font-text-extents
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (multiple-value-bind (x-bearing y-bearing
                                width height
                                x-advance y-advance)
              (cairo:scaled-font-text-extents font "Crategus")
          ;; Check the returned values
          (is (approx-equal -0.95 x-bearing))
          (is (approx-equal -1.73 y-bearing))
          (is (approx-equal  5.96 width))
          (is (approx-equal  2.94 height))
          (is (approx-equal  4.06 x-advance))
          (is (approx-equal  0.00 y-advance))))))))

;;;     cairo_scaled_font_glyph_extents

#-windows
(test cairo-scaled-font-glyph-extents
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (multiple-value-bind (x-bearing y-bearing
                                width height
                                x-advance y-advance)
              (cairo:scaled-font-glyph-extents font '((36 10 20)))
            ;; Check the returned values
            (is (approx-equal   0.0  x-bearing))
            (is (approx-equal  -1.00 y-bearing))
            (is (approx-equal   1.00 width))
            (is (approx-equal   1.00 height))
            (is (approx-equal   1.00 x-advance))
            (is (approx-equal   0.0  y-advance))))))))

#+windows
(test cairo-scaled-font-glyph-extents
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (multiple-value-bind (x-bearing y-bearing
                                width height
                                x-advance y-advance)
              (cairo:scaled-font-glyph-extents font '((36 10 20)))
            ;; Check the returned values
            (is (approx-equal  -1.0  x-bearing))
            (is (approx-equal  -1.72 y-bearing))
            (is (approx-equal   2.67 width))
            (is (approx-equal   2.72 height))
            (is (approx-equal   0.67 x-advance))
            (is (approx-equal   0.0  y-advance))))))))

;;;     cairo_scaled_font_text_to_glyphs

#-windows
(test cairo-scaled-font-text-to-glyphs.1
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (is (every #'numberp
                     (flatten
                       (cairo:scaled-font-text-to-glyphs font
                                                         0 0
                                                         "Crategus")))))))))

#+windows
(test cairo-scaled-font-text-to-glyphs.1
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (is (every #'numberp
                     (flatten
                       (cairo:scaled-font-text-to-glyphs font
                                                         0 0
                                                         "Crategus")))))))))

#-windows
(test cairo-scaled-font-text-to-glyphs.2
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :ft (cairo:scaled-font-type font)))
          (is (every #'approx-equal
                     '(38 0.0d0 0.0d0
                       85 1.0d0 0.0d0
                       68 1.0d0 0.0d0
                       87 2.0d0 0.0d0
                       72 2.0d0 0.0d0
                       74 3.0d0 0.0d0
                       88 4.0d0 0.0d0
                       86 5.0d0 0.0d0)
                     (flatten
                       (cairo:scaled-font-text-to-glyphs font
                                                         0 0
                                                         "Crategus")))))))))

#+windows
(test cairo-scaled-font-text-to-glyphs.2
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (cairo:with-matrices (matrix ctm scale)
      (let ((options (cairo:font-options-create)))
        (cairo:with-scaled-font (font face matrix ctm options)
          (is (eq :success (cairo:scaled-font-status font)))
          (is (eq :dwrite (cairo:scaled-font-type font)))
          (is (every #'approx-equal
                     '(38 0.0d0 0.0d0
                       85 0.72216796875d0 0.0d0
                       68 1.05517578125d0 0.0d0
                       87 1.611328125d0 0.0d0
                       72 1.88916015625d0 0.0d0
                       74 2.4453125d0 0.0d0
                       88 3.00146484375d0 0.0d0
                       86 3.5576171875d0 0.0d0)
                     (flatten
                       (cairo:scaled-font-text-to-glyphs font
                                                         0 0
                                                         "Crategus")))))))))

;;;     cairo_scaled_font_set_user_data
;;;     cairo_scaled_font_get_user_data

;;; 2024-1-12
