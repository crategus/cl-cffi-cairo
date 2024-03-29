(in-package :cairo-test)

(def-suite cairo-text-suite :in cairo-suite)
(in-suite cairo-text-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_glyph_t
;;;     cairo_font_slant_t
;;;     cairo_font_weight_t
;;;     cairo_text_cluster_t
;;;     cairo_text_cluster_flags_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo:with-toy-font-face

#-windows
(test cairo-with-toy-font-face.1
  (cairo:with-toy-font-face (face "" :normal :normal)
    (is (string= "" (cairo:toy-font-face-family face)))
    (is (eq :normal (cairo:toy-font-face-slant face)))
    (is (eq :normal (cairo:toy-font-face-weight face)))))

#-windows
(test cairo-with-toy-font-face.2
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (is (string= "Sans" (cairo:toy-font-face-family face)))
    (is (eq :normal (cairo:toy-font-face-slant face)))
    (is (eq :normal (cairo:toy-font-face-weight face)))))

#+windows
(test cairo-with-toy-font-face.1
  (cairo:with-toy-font-face (face "" :normal :normal)
    (is (string= "Arial" (cairo:toy-font-face-family face)))
    (is (eq :normal (cairo:toy-font-face-slant face)))
    (is (eq :normal (cairo:toy-font-face-weight face)))))

#+windows
(test cairo-with-toy-font-face.2
  (cairo:with-toy-font-face (face "Sans" :normal :normal)
    (is (string= "Sans" (cairo:toy-font-face-family face)))
    (is (eq :normal (cairo:toy-font-face-slant face)))
    (is (eq :normal (cairo:toy-font-face-weight face)))))

#-windows
(test with-toy-font-face.3
  (cairo:with-toy-font-face (face "" :italic :bold)
    (is (string= "" (cairo:toy-font-face-family face)))
    (is (eq :italic (cairo:toy-font-face-slant face)))
    (is (eq :bold (cairo:toy-font-face-weight face)))))

#+windows
(test with-toy-font-face.3
  (cairo:with-toy-font-face (face "" :italic :bold)
    (is (string= "Arial" (cairo:toy-font-face-family face)))
    (is (eq :italic (cairo:toy-font-face-slant face)))
    (is (eq :bold (cairo:toy-font-face-weight face)))))

;;;     cairo_select_font_face

#-windows
(test cairo-select-font-face
  (let (face font (options (cairo:font-options-create)))
    (cairo:with-context-for-recording-surface (context :color)
      ;; Select font face
      (is-false (cairo:select-font-face context "Courier" :weight :bold))

      ;; Check font face
      (setf face (cairo:font-face context))
      (is (eq :success (cairo:font-face-status face)))
      (is (eq :toy (cairo:font-face-type face)))

      ;; Check scaled font
      (setf font (cairo:scaled-font context))
      (is (cffi:pointer-eq face (cairo:scaled-font-font-face font)))
      (is (eq :success (cairo:scaled-font-status font)))
      (is (eq :ft (cairo:scaled-font-type font)))

      ;; Check font options
      (setf options (cairo:font-options context options))
      (is (cffi:pointer-eq options (cairo:scaled-font-font-options font options)))
      (is (eq :default (cairo:font-options-antialias options)))
      (is (eq :default (cairo:font-options-subpixel-order options)))
      (is (eq :default (cairo:font-options-hint-style options)))
      (is (eq :default (cairo:font-options-hint-metrics options)))
      (is-false (cairo:font-options-variations options))

      ;; Destroy font options
      (is-false (cairo:font-options-destroy options)))))

#+windows
(test cairo-select-font-face
  (let (face font (options (cairo:font-options-create)))
    (cairo:with-context-for-recording-surface (context :color)
      ;; Select font face
      (is-false (cairo:select-font-face context "Courier" :weight :bold))

      ;; Check font face
      (setf face (cairo:font-face context))
      (is (eq :success (cairo:font-face-status face)))
      (is (eq :toy (cairo:font-face-type face)))

      ;; Check scaled font
      (setf font (cairo:scaled-font context))
      (is (cffi:pointer-eq face (cairo:scaled-font-font-face font)))
      (is (eq :success (cairo:scaled-font-status font)))
      (is (eq :dwrite (cairo:scaled-font-type font)))

      ;; Check font options
      (setf options (cairo:font-options context options))
      (is (cffi:pointer-eq options (cairo:scaled-font-font-options font options)))
      (is (eq :default (cairo:font-options-antialias options)))
      (is (eq :default (cairo:font-options-subpixel-order options)))
      (is (eq :default (cairo:font-options-hint-style options)))
      (is (eq :default (cairo:font-options-hint-metrics options)))
      (is-false (cairo:font-options-variations options))

      ;; Destroy font options
      (is-false (cairo:font-options-destroy options)))))

;;;     cairo_set_font_size

(test cairo-set-font-size
  (cairo:with-matrix (matrix)
    (cairo:with-context-for-recording-surface (context :color)
      ;; Select font face
      (is-false (cairo:select-font-face context "Courier" :weight :bold))
      ;; Check font matrix
      (is (equal '(10.0d0 0.0d0 0.0d0 10.0d0 0.0d0 0.0d0)
                 (cairo:matrix-to-float (cairo:font-matrix context matrix))))
      ;; Set font size
      (is-false (cairo:set-font-size context 18))
      ;; Check font matrix again
      (is (equal '(18.0d0 0.0d0 0.0d0 18.0d0 0.0d0 0.0d0)
                 (cairo:matrix-to-float (cairo:font-matrix context matrix)))))))

;;;     cairo_set_font_matrix
;;;     cairo_get_font_matrix

(test cairo-font-matrix
  (cairo:with-matrix (matrix)
    (cairo:with-context-for-recording-surface (context :color)
      ;; Select font face
      (is-false (cairo:select-font-face context "Courier" :weight :bold))
      ;; Get font matrix
      (is (cffi:pointer-eq matrix (cairo:font-matrix context matrix)))
      (is (equal '(10.0d0 0.0d0 0.0d0 10.0d0 0.0d0 0.0d0)
                 (cairo:matrix-to-float (cairo:font-matrix context matrix))))
      ;; Set font matrix
      (is (cffi:pointer-eq matrix (cairo:matrix-scale matrix 2 2)))
      (is (cffi:pointer-eq matrix
                           (setf (cairo:font-matrix context) matrix)))
      ;; Check font matrix again
      (is (equal '(20.0d0 0.0d0 0.0d0 20.0d0 0.0d0 0.0d0)
                 (cairo:matrix-to-float (cairo:font-matrix context matrix)))))))

;;;     cairo_set_font_options
;;;     cairo_get_font_options

(test cairo-font-options
  (cairo:with-context-for-recording-surface (context :color)
    (let* ((options (cairo:font-options-create)))
      ;; Set values for font options
      (is (eq :good (setf (cairo:font-options-antialias options) :good)))
      (is (eq :rgb (setf (cairo:font-options-subpixel-order options) :rgb)))
      (is (eq :full (setf (cairo:font-options-hint-style options) :full)))
      (is (eq :on (setf (cairo:font-options-hint-metrics options) :on)))
      ;; Set font options on context
      (is (cffi:pointer-eq options (setf (cairo:font-options context) options)))
      ;; Get font options from context
      (is (cffi:pointer-eq options (cairo:font-options context options)))
      ;; Check font options
      (is (eq :good (cairo:font-options-antialias options)))
      (is (eq :rgb (cairo:font-options-subpixel-order options)))
      (is (eq :full (cairo:font-options-hint-style options)))
      (is (eq :on (cairo:font-options-hint-metrics options)))
      ;; Destroy font options
      (is-false (cairo:font-options-destroy options)))))

;;;     cairo_set_font_face
;;;     cairo_get_font_face

(test cairo-font-face
  (cairo:with-context-for-recording-surface (context :color)
    (cairo:with-toy-font-face (face "Sans" :normal :normal)
      (is (cffi:pointer-eq face (setf (cairo:font-face context) face)))
      (is (cffi:pointer-eq face (cairo:font-face context))))))

;;;     cairo_set_scaled_font
;;;     cairo_get_scaled_font

(test cairo-scaled-font
  (let ((options (cairo:font-options-create)))
    (cairo:with-matrices (matrix ctm)
      (cairo:with-toy-font-face (face "Sans" :normal :normal)
        (cairo:with-scaled-font (font face matrix ctm options)
          (cairo:with-context-for-recording-surface (context :color)

            (is (cffi:pointer-eq font (setf (cairo:scaled-font context) font)))
            ;; TODO: Does not return the same pointer, check this more closely
            (is (cffi:pointerp (cairo:scaled-font context)))))))))

;;;     cairo_show_text

(test cairo-show-text.1
  (cairo:with-context-for-recording-surface (context :color)
    (is-false (cairo:show-text context ""))
    (is-false (cairo:show-text context nil))
    (is-false (cairo:show-text context (cffi:null-pointer)))
    (is-false (cairo:show-text context "Ägypten"))))

(test cairo-show-text.2
  (cairo:with-context-for-image-surface (context :rgb24 300 200)
    ;; Draw background in white color
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Draw text in black ink
    (cairo:set-source-rgba context 0.0 0.0 0.0 1.0)
    ;; Choose a font type and set its size
    (cairo:with-toy-font-face (face "monospace" :normal :bold)
      (setf (cairo:font-face context) face)
      (cairo:set-font-size context 18.0)
      (cairo:move-to context 50 50)
      (is-false (cairo:show-text context "Ägypten"))
      (cairo:surface-write-to-png (cairo:target context)
                                  (sys-path "out/text.png")))))

;;;     cairo_show_glyphs

(test cairo-show-glyphs.1
  (cairo:with-context-for-image-surface (context :rgb24 300 200)
    (let* ((glyphs '((35 10 30) (36 30 30) (37 50 30)))
           (num-glyphs (length glyphs)))
      (cffi:with-foreign-object (glyphs-ptr '(:struct cairo:glyph-t) num-glyphs)
        (loop for count from 0 below num-glyphs
              for glyph in glyphs
              for glyph-ptr = (cffi:mem-aptr glyphs-ptr
                                             '(:struct cairo:glyph-t)
                                             count)
              do (setf (cffi:foreign-slot-value glyph-ptr
                                                '(:struct cairo:glyph-t)
                                                'cairo::index)
                       (first glyph)
                       (cffi:foreign-slot-value glyph-ptr
                                           '(:struct cairo:glyph-t)
                                           'cairo::x)
                       (coerce (second glyph) 'double-float)
                       (cffi:foreign-slot-value glyph-ptr
                                           '(:struct cairo:glyph-t)
                                           'cairo::y)
                       (coerce (third glyph) 'double-float)))
        ;; Clear surface
        (cairo:set-source-rgb context 1.0 1.0 1.0)
        (cairo:paint context)
        ;; Draw in black ink
        (cairo:set-source-rgba context 0.0 0.0 0.0 1.0)
        ;; Choose a font type and set its size
        (cairo:with-toy-font-face (face "monospace" :normal :bold)
          (setf (cairo:font-face context) face)
          (cairo:set-font-size context 18.0)
          ;; Show the array of glyphs
          (cairo::%show-glyphs context glyphs-ptr num-glyphs)
          ;; Create and save the PNG image
          (cairo:surface-write-to-png (cairo:target context)
                                      (sys-path "out/glyphs.png")))))))

(test cairo-show-glyphs.2
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    (let ((glyphs '((35 10 30) (36 30 30) (37 50 30))))
      ;; Clear surface
      (cairo:set-source-rgb context 1.0 1.0 1.0)
      (cairo:paint context)
      ;; Draw in black ink.
      (cairo:set-source-rgba context 0.0 0.0 0.0 1.0)
      ;; Choose a font type and set its size
      (cairo:with-toy-font-face (face "Sans" :normal :bold)
        (setf (cairo:font-face context) face)
        (cairo:set-font-size context 18.0)
        ;; Show the list of glyphs
        (cairo:show-glyphs context glyphs)
        ;; Create and save the PNG image
        (cairo:surface-write-to-png (cairo:target context)
                                    (sys-path "out/glyphs.png"))))))

;;;     cairo_show_text_glyphs

;;;     cairo_font_extents

(test cairo-font-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (ascent descent height max-x-advance max-y-advance)
        (cairo:font-extents context)
      ;; Check the values
      (is (typep ascent 'double-float))
      (is (typep descent 'double-float))
      (is (typep height 'double-float))
      (is (typep max-x-advance 'double-float))
      (is (typep max-y-advance 'double-float)))))

#+crategus
(test cairo-font-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (ascent descent height max-x-advance max-y-advance)
        (cairo:font-extents context)
      ;; Check the values
      (is (approx-equal 20.0 ascent))
      (is (approx-equal  6.0 descent))
      (is (approx-equal 25.0 height))
      (is (approx-equal 51.0 max-x-advance))
      (is (approx-equal  0.0 max-y-advance)))))

#+windows
(test cairo-font-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (ascent descent height max-x-advance max-y-advance)
        (cairo:font-extents context)
      ;; Check the values
      (is (approx-equal  17.0 ascent))
      (is (approx-equal   4.0 descent))
      (is (approx-equal  21.0 height))
      (is (approx-equal 252.0 max-x-advance))
      (is (approx-equal   0.0 max-y-advance)))))

;;;     cairo_text_extents

#-windows
(test cairo-text-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cairo:text-extents context "Crategus")
      ;; Check the returned values
      (is (approx-equal   1.0 x-bearing))
      (is (approx-equal -13.0 y-bearing))
      (is (approx-equal  79.0 width))
      (is (approx-equal  17.0 height))
      (is (approx-equal  80.0 x-advance))
      (is (approx-equal   0.0 y-advance)))))

#+windows
(test cairo-text-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cairo:text-extents context "Crategus")
      ;; Check the returned values
      (is (approx-equal  -0.1 x-bearing))
      (is (approx-equal -13.0 y-bearing))
      (is (approx-equal  79.0 width))
      (is (approx-equal  18.9 height))
      (is (approx-equal  80.0 x-advance))
      (is (approx-equal   0.0 y-advance)))))

;;;     cairo_glyph_extents

#-windows
(test cairo-glyph-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cairo:glyph-extents context '((36 10 20)))
    ;; Check the returned values
    (is (approx-equal  -0.0 x-bearing))
    (is (approx-equal -13.0 y-bearing))
    (is (approx-equal  13.0 width))
    (is (approx-equal  13.0 height))
    (is (approx-equal  12.0 x-advance))
    (is (approx-equal   0.0 y-advance)))))

#+windows
(test cairo-glyph-extents
  (cairo:with-context-for-image-surface (context :rgb24 400 300)
    ;; Set a font and a font size
    (cairo:select-font-face context "Sans")
    (cairo:set-font-size context 18)
    (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
        (cairo:glyph-extents context '((36 10 20)))
    ;; Check the returned values
    (is (approx-equal  -1.0 x-bearing))
    (is (approx-equal -13.0 y-bearing))
    (is (approx-equal  13.0 width))
    (is (approx-equal  14.9 height))
    (is (approx-equal  12.0 x-advance))
    (is (approx-equal   0.0 y-advance)))))

;;;     cairo_toy_font_face_create
;;;     cairo_toy_font_face_get_family
;;;     cairo_toy_font_face_get_slant
;;;     cairo_toy_font_face_get_weight

(test cairo-toy-font-face-create
  (cairo:with-toy-font-face (face "Sans" :italic :bold)
    (is (string= "Sans" (cairo:toy-font-face-family face)))
    (is (eq :italic (cairo:toy-font-face-slant face)))
    (is (eq :bold (cairo:toy-font-face-weight face)))))

;;;     cairo_glyph_allocate
;;;     cairo_glyph_free
;;;     cairo_text_cluster_allocate
;;;     cairo_text_cluster_free

;;; 2024-2-3
