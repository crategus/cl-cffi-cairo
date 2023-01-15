(in-package :cairo-test)

(def-suite cairo-context :in cairo-suite)
(in-suite cairo-context)

;;; --- Types and Values -------------------------------------------------------

;;;     cairo_antialias_t
;;;     cairo_fill_rule_t
;;;     cairo_line_cap_t
;;;     cairo_line_join_t
;;;     cairo_operator_t
;;;
;;;     cairo_rectangle_t
;;;     cairo_rectangle_list_t

;;;     cairo_t

;;; --- Functions --------------------------------------------------------------

;;;     cairo_create

(test create.1
  (let* ((surface (cairo:image-surface-create :rgb24 100 150))
         (context (cairo:create surface)))
    (is (cffi:pointerp context))
    (is (eq :success (cairo:status context)))
    (is-false (cairo:destroy context))
    (is-false (cairo:surface-destroy surface))))

(test create.2
  (with-cairo-image-surface (surface :rgb24 100 150)
    (let ((context (cairo:create surface)))
      (is (cffi:pointerp context))
      (is (eq :success (cairo:status context)))
      (is-false (cairo:destroy context)))))

(test create.3
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (cffi:pointerp context))
      (is (eq :success (cairo:status context))))))

;;;     cairo_reference
;;;     cairo_destroy

(test reference/destroy
  (with-cairo-image-surface (surface :rgb24 100 150)
    (let ((context (cairo:create surface)))
      (is (cffi:pointerp context))
      (is (eq :success (cairo:status context)))
      (is (= 1 (cairo:reference-count context)))
      (is (cffi:pointerp (cairo:reference context)))
      (is (= 2 (cairo:reference-count context)))
      (is-false (cairo:destroy context))
      (is (= 1 (cairo:reference-count context)))
      (is-false (cairo:destroy context))
      (is (= 0 (cairo:reference-count context))))))

;;;     cairo_status

(test status
  (with-cairo-image-surface (surface :rgb24 150 100)
    (with-cairo-context (context surface)
      (is (eq :success (cairo:status context))))))

;;;   cairo_save
;;;   cairo_restore

(test save/restore
  (with-cairo-image-surface (surface :rgb24 150 100)
    (with-cairo-context (context surface)
      (is-false (cairo:save context))
      (is-false (cairo:restore context)))))

;;;   cairo_get_target

(test target
  (with-cairo-image-surface (surface :rgb24 150 100)
    (with-cairo-context (context surface)
      (is (eq :success (cairo:status context)))
      (is (cffi:pointer-eq surface (cairo:target context))))))

;;;     cairo_push_group
;;;     cairo_push_group_with_content
;;;     cairo_pop_group
;;;     cairo_pop_group_to_source

;;;     cairo_get_group_target
;;;     cairo_set_source_rgb
;;;     cairo_set_source_rgba
;;;     cairo_set_source
;;;     cairo_set_source_surface
;;;     cairo_get_source

;;;     cairo_set_antialias
;;;     cairo_get_antialias

(test antialias
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (eq :default (cairo:antialias context)))
      (is (eq :none (setf (cairo:antialias context) :none)))
      (is (eq :none (cairo:antialias context)))
      (is (eq :gray (setf (cairo:antialias context) :gray)))
      (is (eq :gray (cairo:antialias context)))
      (is (eq :subpixel (setf (cairo:antialias context) :subpixel)))
      (is (eq :subpixel (cairo:antialias context)))
      (is (eq :fast (setf (cairo:antialias context) :fast)))
      (is (eq :fast (cairo:antialias context)))
      (is (eq :good (setf (cairo:antialias context) :good)))
      (is (eq :good (cairo:antialias context)))
      (is (eq :best (setf (cairo:antialias context) :best)))
      (is (eq :best (cairo:antialias context))))))

;;;     cairo_set_dash
;;;     cairo_get_dash_count
;;;     cairo_get_dash

(test dash
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      ;; Get default values
      (is (equal '(() 0.0d0)
                  (multiple-value-list (cairo:dash context))))
      (is (= 0 (cairo:dash-count context)))
      ;; Set dashes
      (is (equal '(1.0 2.0 3.0 4.0)
                 (setf (cairo:dash context 3) '(1.0 2.0 3.0 4.0))))
      (is (equal '((1.0d0 2.0d0 3.0d0 4.0d0) 3.0d0)
                 (multiple-value-list (cairo:dash context))))
      (is (= 4 (cairo:dash-count context)))
      ;; Clear dashes
      (is (equal '()
                 (setf (cairo:dash context 0) '())))
      (is (equal '(() 0.0d0)
                 (multiple-value-list (cairo:dash context))))
      (is (= 0 (cairo:dash-count context)))
      ;; Set an invalid dash
      (is (equal '(1.0 -2.0)
                 (setf (cairo:dash context 0) '(1.0 -2.0))))
      (is (eq :invalid-dash (cairo:status context))))))

;;;     cairo_set_fill_rule
;;;     cairo_get_fill_rule

(test fill-rule
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (eq :winding (cairo:fill-rule context)))
      (is (eq :even-odd (setf (cairo:fill-rule context) :even-odd)))
      (is (eq :even-odd (cairo:fill-rule context))))))

;;;     cairo_set_line_cap
;;;     cairo_get_line_cap

(test line-cap
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (eq :butt (cairo:line-cap context)))
      (is (eq :round (setf (cairo:line-cap context) :round)))
      (is (eq :round (cairo:line-cap context)))
      (is (eq :square (setf (cairo:line-cap context) :square)))
      (is (eq :square (cairo:line-cap context))))))

;;;     cairo_set_line_join
;;;     cairo_get_line_join

(test line-join
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (eq :miter (cairo:line-join context)))
      (is (eq :round (setf (cairo:line-join context) :round)))
      (is (eq :round (cairo:line-join context)))
      (is (eq :bevel (setf (cairo:line-join context) :bevel)))
      (is (eq :bevel (cairo:line-join context))))))

;;;     cairo_set_line_width
;;;     cairo_get_line_width

(test line-width.1
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (= 2.0d0 (cairo:line-width context)))
      (is (= 1.0d0 (setf (cairo:line-width context) 1.0)))
      (is (= 1.0d0 (cairo:line-width context))))))

(test line-width.2
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (= 0.5d0 (setf (cairo:line-width context) 1/2)))
      (is (= 0.5d0 (cairo:line-width context)))
      (is (= 1.0d0 (setf (cairo:line-width context) 1)))
      (is (= 1.0d0 (cairo:line-width context)))
      (is (= 2.0d0 (setf (cairo:line-width context) 2.0)))
      (is (= 2.0d0 (cairo:line-width context)))
      (is (= 3.0d0 (setf (cairo:line-width context) 3.0d0)))
      (is (= 3.0d0 (cairo:line-width context))))))

;;;     cairo_set_miter_limit
;;;     cairo_get_miter_limit

(test miter-limit
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (= 10.0d0 (cairo:miter-limit context)))
      (is (= 15.0d0 (setf (cairo:miter-limit context) 15.0)))
      (is (= 15.0d0 (cairo:miter-limit context))))))

;;;     cairo_set_operator
;;;     cairo_get_operator

(test operator
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (eq :over (cairo:operator context)))
      (is (eq :clear (setf (cairo:operator context) :clear)))
      (is (eq :clear (cairo:operator context))))))

;;;     cairo_set_tolerance
;;;     cairo_get_tolerance

(test tolerance
  (with-cairo-image-surface (surface :rgb24 100 150)
    (with-cairo-context (context surface)
      (is (= 0.1d0 (cairo:tolerance context)))
      (is (approx-equal 0.2d0 (setf (cairo:tolerance context) 0.2)))
      (is (approx-equal 0.2d0 (cairo:tolerance context))))))

;;;     cairo_clip
;;;     cairo_clip_preserve
;;;     cairo_clip_extents
;;;     cairo_in_clip
;;;     cairo_reset_clip
;;;     cairo_rectangle_list_destroy
;;;     cairo_copy_clip_rectangle_list
;;;     cairo_fill
;;;     cairo_fill_preserve
;;;     cairo_fill_extents
;;;     cairo_in_fill
;;;     cairo_mask
;;;     cairo_mask_surface
;;;     cairo_paint
;;;     cairo_paint_with_alpha
;;;     cairo_stroke
;;;     cairo_stroke_preserve
;;;     cairo_stroke_extents
;;;     cairo_in_stroke
;;;     cairo_copy_page
;;;     cairo_show_page

;;;     cairo_get_reference_count

(test reference-count
  (let* ((surface (cairo:image-surface-create :rgb24 100 150))
         (context (cairo:create surface)))
    (is (= 1 (cairo:reference-count context)))
    (is (cffi:pointer-eq context (cairo:reference context)))
    (is (= 2 (cairo:reference-count context)))
    (is-false (cairo:destroy context))
    (is (= 1 (cairo:reference-count context)))
    (is-false (cairo:destroy context))
    (is (= 0 (cairo:reference-count context)))))

;;;     cairo_set_user_data
;;;     cairo_get_user_data

;;; --- 2023-1-11 --------------------------------------------------------------
