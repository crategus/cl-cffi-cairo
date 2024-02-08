(in-package :cairo-test)

(def-suite cairo-font-options-suite :in cairo-suite)
(in-suite cairo-font-options-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_font_options_t
;;;     cairo_subpixel_order_t
;;;     cairo_hint_style_t
;;;     cairo_hint_metrics_t
;;;     cairo_color_mode_t                                 Since 1.18

;;; --- Functions --------------------------------------------------------------

;;;     cairo_font_options_create
;;;     cairo_font_options_status
;;;     cairo_font_options_destroy

(test cairo-font-options-create
  (let ((options (cairo:font-options-create)))
    (is (cffi:pointerp options))
    (is (eq :success (cairo:font-options-status options)))
    (is (eq :default (cairo:font-options-antialias options)))
    (is (eq :default (cairo:font-options-subpixel-order options)))
    (is (eq :default (cairo:font-options-hint-style options)))
    (is (eq :default (cairo:font-options-hint-metrics options)))
    (is-false (cairo:font-options-variations options))
    (is-false (cairo:font-options-destroy options))))

;;;     cairo_font_options_copy
;;;     cairo_font_options_equal

(test cairo-font-options-copy
  (let ((options (cairo:font-options-create))
        options1)
    (is (cffi:pointerp (setf options1 (cairo:font-options-copy options))))
    (is (eq :success (cairo:font-options-status options1)))
    (is (cairo:font-options-equal options options1))
    (is-false (cairo:font-options-destroy options))
    (is-false (cairo:font-options-destroy options1))))

;;;     cairo_font_options_merge

(test cairo-font-options-merge
  (let ((options (cairo:font-options-create))
        (other (cairo:font-options-create)))
    (is (eq :good (setf (cairo:font-options-antialias other) :good)))
    (is (eq :rgb (setf (cairo:font-options-subpixel-order other) :rgb)))
    (is (eq :full (setf (cairo:font-options-hint-style other) :full)))
    (is (eq :off (setf (cairo:font-options-hint-metrics other) :off)))
    ;; Merge the options
    (is-false (cairo:font-options-merge options other))
    ;; Check the merge
    (is (eq :good (cairo:font-options-antialias options)))
    (is (eq :rgb (cairo:font-options-subpixel-order options)))
    (is (eq :full (cairo:font-options-hint-style options)))
    (is (eq :off (cairo:font-options-hint-metrics options)))
    ;; Destroy the font options
    (is-false (cairo:font-options-destroy options))
    (is-false (cairo:font-options-destroy other))))

;;;     cairo_font_options_hash

(test cairo-font-options-hash
  (let ((options (cairo:font-options-create)))
    (is (= 0 (cairo:font-options-hash options)))
    ;; Set values
    (is (eq :good (setf (cairo:font-options-antialias options) :good)))
    (is (eq :rgb (setf (cairo:font-options-subpixel-order options) :rgb)))
    (is (eq :full (setf (cairo:font-options-hint-style options) :full)))
    (is (eq :off (setf (cairo:font-options-hint-metrics options) :off)))
    ;; Calculate the hash value
    (is (= 81941 (cairo:font-options-hash options)))
    (is-false (cairo:font-options-destroy options))))

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

(test cairo-font-options-variations
  (let ((options (cairo:font-options-create)))
    (is-false (cairo:font-options-variations options))
    (is (string= "wght 200, wdth 140.5"
                 (setf (cairo:font-options-variations options)
                       "wght 200, wdth 140.5")))
    (is (string= "wght 200, wdth 140.5"
                 (cairo:font-options-variations options)))
    (is-false (setf (cairo:font-options-variations options) nil))
    (is-false (cairo:font-options-variations options))
    (is-false (cairo:font-options-destroy options))))

;;;     cairo_font_options_set_color_mode                  Since 1.18
;;;     cairo_font_options_get_color_mode                  Since 1.18

(test cairo-font-options-color-mode
  (let ((options (cairo:font-options-create)))
    (is (eq :default (cairo:font-options-color-mode options)))
    (is (eq :color (setf (cairo:font-options-color-mode options) :color)))
    (is (eq :color (cairo:font-options-color-mode options)))
    (is-false (cairo:font-options-destroy options))))

;;;     cairo_font_options_set_color_palette               Since 1.18
;;;     cairo_font_options_get_color_palette               Since 1.18

(test cairo-font-options-color-palette
  (let ((options (cairo:font-options-create)))
    (is (= 0 (cairo:font-options-color-palette options)))
    (is (= 1 (setf (cairo:font-options-color-palette options) 1)))
    (is (= 1 (cairo:font-options-color-palette options)))
    (is-false (cairo:font-options-destroy options))))

;;;     cairo_font_options_set_custom_palette_color        Since 1.18
;;;     cairo_font_options_get_custom_palette_color        Since 1.18

;; TODO: Default font options have not a custom color palette

(test cairo-font-options-custom-palette-color
  (let ((options (cairo:font-options-create)))
    (is-false (cairo:font-options-custom-palette-color options 0))
    (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0)
               (multiple-value-list
                 (setf (cairo:font-options-custom-palette-color options 0)
                       '(1.0 0.0 0.0 1.0)))))
    (is-false (cairo:font-options-custom-palette-color options 0))
    (is-false (cairo:font-options-destroy options))))

;;; 2024-1-27
